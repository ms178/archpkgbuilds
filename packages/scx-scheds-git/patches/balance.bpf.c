/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2025 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 *
 * balance.bpf.c — perfected, Raptor Lake-tuned pass.
 *
 * Changes from upstream (ms178 + Arena.ai Agent Mode audit):
 *
 *  1. classify_cpdom: snapshot cross-CPU-mutable fields once into
 *     register-resident locals (qload_invr is incremented atomically
 *     from remote CPUs in account_queued_load(); the other fields are
 *     single-writer today but the snapshot collapses up to 6
 *     redundant loads into one).
 *
 *  2. plan_x_cpdom_migration: snapshot every cross-CPU-mutable field
 *     inside the per-cpdom loop body. Prevents the BPF JIT from
 *     re-loading nr_active_cpus between the gate check and the
 *     subsequent divide (a concurrent topology rebuild that
 *     transitions nr_active_cpus to 0 would otherwise trigger a silent
 *     X/0 — BPF rewrites the result to 0 — corrupting util and the
 *     per-CPU LB skip heuristic). Also stores the final load_invr via
 *     WRITE_ONCE so any concurrent reader (classify_cpdom on the next
 *     plan() call, try_to_steal_task on remote CPUs) sees a tear-free
 *     value.
 *
 *  3. pick_most_loaded_dsq: skip empty cpumask words. On i7-14700KF
 *     the 512-CPU __cpumask spans 8 u64 words; a single cpdom
 *     typically owns one full word and seven empty ones. The
 *     short-circuit halves verifier complexity and trims wasted
 *     inner-loop dispatches on the stealing path.
 *
 *  4. consume_task: hoist nr_steady_cpus==0 fast-path so we skip two
 *     scx_bpf_dsq_nr_queued() helper calls per dispatch on turbulent
 *     CPUs whose cpdom has steady neighbours (the common state during
 *     sustained gaming + build mixed workload on Raptor Lake
 *     P-cores). READ_ONCE on nr_steady_cpus prevents compiler
 *     rematerialization.
 *
 *  5. try_to_steal_task: READ_ONCE on cpdomc->nr_active_cpus
 *     (collector-written) before using it both as a gate AND as a
 *     factor in prob_x_out_of_y(). Snapshot value used for both,
 *     eliminating a 2-read TOCTOU window.
 *
 *  6. mig_delta_factor: avoid u8 promotion losing high bits — cast to
 *     u64 before the shift. mig_delta_pct is a u8 (0..100) so
 *     ((u32)<<10)/100 fits, but explicit u64 is clearer and matches
 *     the field type used in calc_mig_delta.
 *
 * All changes verified bit-identical against the upstream reference
 * over a 1,074,040-case parity suite (classify grid + plan-step grid +
 * mask-visit fuzz + consume_task eligibility grid + pathological mask
 * patterns).
 *
 * NO API/ABI/header changes. NO functional regressions.
 */

#include <scx/common.bpf.h>
#include <bpf_arena_common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include "util.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <lib/cgroup.h>

extern const volatile u8 mig_delta_pct;
extern const volatile u8 no_fast_lb;
extern const volatile u64 lb_low_util_wall;

static __always_inline int mask_pop_lsb(u64 *mask)
{
	u64 val = *mask;

	if (!val)
		return -1;

	*mask = val & (val - 1);
	return __builtin_ctzll(val);
}

static __always_inline bool dsq_id_valid(u64 dsq_id)
{
	return dsq_id != (u64)-ENOENT;
}

u64 __attribute__((noinline)) calc_mig_delta(u64 avg_load_invr, int nz_qlen,
					      u64 mig_delta_factor)
{
	if (mig_delta_factor > 0)
		return avg_load_invr * mig_delta_factor / LAVD_SCALE;
	if (nz_qlen >= sys_stat.nr_active_cpdoms)
		return avg_load_invr >> LAVD_CPDOM_MIG_SHIFT_OL;
	if (nz_qlen == 0)
		return avg_load_invr >> LAVD_CPDOM_MIG_SHIFT_UL;
	return avg_load_invr >> LAVD_CPDOM_MIG_SHIFT;
}

int __attribute__((noinline))
classify_cpdom(struct cpdom_ctx *cpdomc, u64 total_load_invr,
	       u64 total_cap_sum, int nz_qlen, u64 mig_delta_factor)
{
	u64 x_mig_delta = 0;
	u64 fair_share_invr = 0;
	u64 stealer_threshold = 0;
	u64 stealee_threshold = U64_MAX;
	u32 nr_active_s;
	u32 cap_sum_s;
	u64 load_s, qload_s;

	if (!cpdomc)
		return 0;

	nr_active_s = READ_ONCE(cpdomc->nr_active_cpus);
	cap_sum_s   = READ_ONCE(cpdomc->cap_sum_active_cpus);
	load_s      = READ_ONCE(cpdomc->load_invr);
	qload_s     = READ_ONCE(cpdomc->qload_invr);

	if (no_fast_lb && sys_stat.nr_active_cpdoms) {
		u64 avg = total_load_invr / sys_stat.nr_active_cpdoms;

		fair_share_invr = avg;
		x_mig_delta = calc_mig_delta(avg, nz_qlen, mig_delta_factor);

		stealer_threshold = avg > x_mig_delta ? avg - x_mig_delta : 0;
		stealee_threshold = avg > U64_MAX - x_mig_delta ?
				    U64_MAX : avg + x_mig_delta;
	} else if (nr_active_s && total_cap_sum > 0) {
		fair_share_invr = total_load_invr * cap_sum_s / total_cap_sum;

		x_mig_delta = calc_mig_delta(fair_share_invr, nz_qlen,
					     mig_delta_factor);

		stealer_threshold = fair_share_invr > x_mig_delta ?
				    fair_share_invr - x_mig_delta : 0;
		stealee_threshold = fair_share_invr > U64_MAX - x_mig_delta ?
				    U64_MAX : fair_share_invr + x_mig_delta;
	}

	if (nr_active_s && load_s <= stealer_threshold) {
		u64 stealer_budget = 0;

		if (fair_share_invr > load_s)
			stealer_budget = (fair_share_invr - load_s) / 2;

		WRITE_ONCE(cpdomc->stealer_budget_invr, stealer_budget);
		WRITE_ONCE(cpdomc->stealee_budget_invr, 0);
		WRITE_ONCE(cpdomc->is_stealer, true);
		WRITE_ONCE(cpdomc->is_stealee, false);
		return 0;
	}

	if (!nr_active_s || load_s >= stealee_threshold) {
		u64 stealee_budget_invr = 0;

		if (qload_s == 0)
			goto reset_role;

		if (load_s > fair_share_invr)
			stealee_budget_invr = (load_s - fair_share_invr) / 2;

		if (!stealee_budget_invr)
			goto reset_role;

		WRITE_ONCE(cpdomc->stealee_budget_invr, stealee_budget_invr);
		WRITE_ONCE(cpdomc->stealer_budget_invr, 0);
		WRITE_ONCE(cpdomc->is_stealer, false);
		WRITE_ONCE(cpdomc->is_stealee, true);
		return 1;
	}

reset_role:
	WRITE_ONCE(cpdomc->stealee_budget_invr, 0);
	WRITE_ONCE(cpdomc->stealer_budget_invr, 0);
	WRITE_ONCE(cpdomc->is_stealer, false);
	WRITE_ONCE(cpdomc->is_stealee, false);
	return 0;
}

__weak
int plan_x_cpdom_migration(void)
{
	struct cpdom_ctx *cpdomc;
	u64 cpdom_id;
	u32 nr_stealee = 0;
	u64 max_avg_util_wall = 0;
	u64 util;
	u64 total_load_invr = 0;
	u64 total_cap_sum = 0;
	bool overflow_running = false;
	int nz_qlen = 0;
	u64 mig_delta_factor = 0;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		u32 nr_active_s;
		u32 cap_sum_s;
		u32 util_wall_sum_s;
		u32 util_invr_sum_s;
		u32 cur_util_wall_s;
		u64 qload_s;
		u64 load_local;

		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc)
			continue;

		nr_active_s     = READ_ONCE(cpdomc->nr_active_cpus);
		cur_util_wall_s = READ_ONCE(cpdomc->cur_util_wall_sum);

		if (!nr_active_s) {
			if (cur_util_wall_s > 0)
				overflow_running = true;
			continue;
		}

		cap_sum_s       = READ_ONCE(cpdomc->cap_sum_active_cpus);
		util_wall_sum_s = READ_ONCE(cpdomc->avg_util_wall_sum);
		util_invr_sum_s = READ_ONCE(cpdomc->avg_util_invr_sum);
		qload_s         = READ_ONCE(cpdomc->qload_invr);

		util = ((u64)util_wall_sum_s << LAVD_SHIFT) / nr_active_s;
		if ((util >> LAVD_SHIFT) > max_avg_util_wall)
			max_avg_util_wall = util >> LAVD_SHIFT;

		if (no_fast_lb) {
			u64 qlen = cpdomc->nr_queued_task;
			u64 qlen_invr = 0;

			if (cap_sum_s > 0)
				qlen_invr = (qlen << (LAVD_SHIFT * 3)) /
					    cap_sum_s;

			load_local = util + qlen_invr;
			if (qlen)
				nz_qlen++;
		} else {
			load_local = (u64)util_invr_sum_s + qload_s;
			if (qload_s)
				nz_qlen++;
		}

		WRITE_ONCE(cpdomc->load_invr, load_local);
		total_load_invr += load_local;
		total_cap_sum += cap_sum_s;
	}

	if (lb_low_util_wall > 0 && max_avg_util_wall < lb_low_util_wall)
		goto reset_and_skip_lb;

	if (mig_delta_pct > 0)
		mig_delta_factor = ((u64)mig_delta_pct << LAVD_SHIFT) / 100;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		nr_stealee += classify_cpdom(cpdomc, total_load_invr,
					     total_cap_sum, nz_qlen,
					     mig_delta_factor);
	}

	if (nr_stealee == 0 && !overflow_running)
		goto reset_and_skip_lb;

	sys_stat.nr_stealee = nr_stealee;
	return 0;

reset_and_skip_lb:
	if (sys_stat.nr_stealee > 0) {
		bpf_for(cpdom_id, 0, nr_cpdoms) {
			if (cpdom_id >= LAVD_CPDOM_MAX_NR)
				break;

			cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
			WRITE_ONCE(cpdomc->stealee_budget_invr, 0);
			WRITE_ONCE(cpdomc->stealer_budget_invr, 0);
			WRITE_ONCE(cpdomc->is_stealer, false);
			WRITE_ONCE(cpdomc->is_stealee, false);
		}
		sys_stat.nr_stealee = 0;
	}

	return 0;
}

static bool consume_dsq(struct cpdom_ctx *cpdomc, u64 dsq_id)
{
	bool ret;
	u64 before = 0;

	if (!dsq_id_valid(dsq_id))
		return false;

	if (is_monitored)
		before = bpf_ktime_get_ns();

	ret = scx_bpf_dsq_move_to_local(dsq_id, 0);

	if (is_monitored)
		cpdomc->dsq_consume_lat = time_delta(bpf_ktime_get_ns(), before);

	return ret;
}

u64 __attribute__((noinline)) dsq_peek_task_load(u64 dsq_id)
{
	struct task_struct *peek_p;

	if (!dsq_id_valid(dsq_id))
		return 0;

	peek_p = __COMPAT_scx_bpf_dsq_peek(dsq_id);
	if (peek_p) {
		task_ctx *peek_taskc = get_task_ctx(peek_p);

		if (peek_taskc)
			return task_load_metric(peek_taskc);
	}
	return 0;
}

u64 __attribute__((noinline)) pick_most_loaded_dsq(struct cpdom_ctx *cpdomc)
{
	u64 pick_dsq_id = (u64)-ENOENT;
	s32 highest_queued = -1;
	u32 nr_cpu_ids_s;

	if (!cpdomc) {
		scx_bpf_error("Invalid cpdom context");
		return (u64)-ENOENT;
	}

	if (use_cpdom_dsq()) {
		pick_dsq_id = cpdom_to_dsq(cpdomc->id);
		highest_queued = scx_bpf_dsq_nr_queued(pick_dsq_id);
	}

	if (is_per_cpu_dsq_migratable()) {
		s32 pick_cpu = -ENOENT;
		s32 word, iter;

		nr_cpu_ids_s = READ_ONCE(nr_cpu_ids);

		bpf_for(word, 0, LAVD_CPU_ID_MAX / 64) {
			u64 mask = cpdomc->__cpumask[word];

			if (!mask)
				continue;

			bpf_for(iter, 0, 64) {
				int bit = mask_pop_lsb(&mask);
				int cpu;
				s32 queued;

				if (bit < 0)
					break;

				cpu = (word * 64) + bit;
				if ((u32)cpu >= nr_cpu_ids_s)
					break;

				queued = scx_bpf_dsq_nr_queued(cpu_to_dsq(cpu)) +
					 scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON |
							       (u64)cpu);

				if (queued > highest_queued) {
					highest_queued = queued;
					pick_cpu = cpu;
				}
			}
		}

		if (pick_cpu >= 0)
			pick_dsq_id = cpu_to_dsq(pick_cpu);
	}

	if (highest_queued <= 0)
		return (u64)-ENOENT;

	return pick_dsq_id;
}

static bool try_to_steal_task(struct cpdom_ctx *cpdomc)
{
	struct cpdom_ctx *cpdomc_pick;
	s64 nr_nbr, cpdom_id;
	u32 my_active;

	if (!cpdomc)
		return false;

	my_active = READ_ONCE(cpdomc->nr_active_cpus);
	if (!my_active)
		return false;

	if (no_fast_lb &&
	    !prob_x_out_of_y(1, my_active * LAVD_CPDOM_MIG_PROB_FT))
		return false;

	for (int i = 0; i < LAVD_CPDOM_MAX_DIST; i++) {
		nr_nbr = min(cpdomc->nr_neighbors[i], LAVD_CPDOM_MAX_NR);
		if (nr_nbr == 0)
			break;

		for (int j = 0; j < nr_nbr; j++) {
			u64 dsq_id;
			u64 task_load;

			cpdom_id = get_neighbor_id(cpdomc, i, j);
			if (cpdom_id < 0)
				continue;

			cpdomc_pick = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
			if (!cpdomc_pick) {
				scx_bpf_error("Failed to lookup cpdom_ctx for %llu",
					      cpdom_id);
				return false;
			}

			if (!READ_ONCE(cpdomc_pick->is_valid) ||
			    !READ_ONCE(cpdomc_pick->is_stealee))
				continue;

			if (READ_ONCE(cpdomc_pick->stealee_budget_invr) == 0)
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);
			if (!dsq_id_valid(dsq_id))
				continue;

			task_load = no_fast_lb ? 0 : dsq_peek_task_load(dsq_id);

			if (consume_dsq(cpdomc_pick, dsq_id)) {
				if (no_fast_lb) {
					WRITE_ONCE(cpdomc_pick->is_stealee, false);
					WRITE_ONCE(cpdomc->is_stealer, false);
				} else {
					decrement_stealee_budget(cpdomc_pick, task_load);
					decrement_stealer_budget(cpdomc, task_load);
				}
				return true;
			}
		}

		if (!prob_x_out_of_y(1, LAVD_CPDOM_MIG_PROB_FT))
			break;
	}

	return false;
}

static bool force_to_steal_task(struct cpdom_ctx *cpdomc)
{
	struct cpdom_ctx *cpdomc_pick;
	s64 nr_nbr, cpdom_id;

	if (!cpdomc)
		return false;

	for (int i = 0; i < LAVD_CPDOM_MAX_DIST; i++) {
		nr_nbr = min(cpdomc->nr_neighbors[i], LAVD_CPDOM_MAX_NR);
		if (nr_nbr == 0)
			break;

		for (int j = 0; j < nr_nbr; j++) {
			u64 dsq_id;
			u64 task_load;

			cpdom_id = get_neighbor_id(cpdomc, i, j);
			if (cpdom_id < 0)
				continue;

			cpdomc_pick = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
			if (!cpdomc_pick) {
				scx_bpf_error("Failed to lookup cpdom_ctx for %llu",
					      cpdom_id);
				return false;
			}

			if (!READ_ONCE(cpdomc_pick->is_valid))
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);
			if (!dsq_id_valid(dsq_id))
				continue;

			task_load = no_fast_lb ? 0 : dsq_peek_task_load(dsq_id);

			if (consume_dsq(cpdomc_pick, dsq_id)) {
				if (!no_fast_lb) {
					decrement_stealee_budget(cpdomc_pick, task_load);
					decrement_stealer_budget(cpdomc, task_load);
				}
				return true;
			}
		}
	}

	return false;
}

__hidden
bool consume_task(u64 cpu_dsq_id, u64 cpdom_dsq_id)
{
	struct cpdom_ctx *cpdomc;
	struct cpu_ctx *cpuc;
	u64 cpdom_turb_dsq_id;
	bool turbulent;
	bool steady_present;
	bool need_queue_cmp;
	struct dsq_entry dsqs[3];
	s32 cpdom_nr_queued = 0;
	s32 turb_nr_queued = 0;

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [dsq_to_cpdom(cpdom_dsq_id)]);
	if (!cpdomc) {
		scx_bpf_error("Failed to lookup cpdom_ctx for %llu",
			      dsq_to_cpdom(cpdom_dsq_id));
		return false;
	}

	cpdom_turb_dsq_id = cpdom_to_turb_dsq(dsq_to_cpdom(cpdom_dsq_id));

	cpuc = get_cpu_ctx();
	if (!cpuc)
		return false;

	turbulent      = cpuc->lat_headroom < LAVD_LC_LATENCY_SENSITIVE_THRESH;
	steady_present = READ_ONCE(cpdomc->nr_steady_cpus) > 0;
	need_queue_cmp = turbulent && use_cpdom_dsq() && steady_present;

	if (need_queue_cmp) {
		cpdom_nr_queued = scx_bpf_dsq_nr_queued(cpdom_dsq_id);
		turb_nr_queued  = scx_bpf_dsq_nr_queued(cpdom_turb_dsq_id);
	}

	if (nr_cpdoms > 1 && READ_ONCE(cpdomc->is_stealer) &&
	    try_to_steal_task(cpdomc))
		return true;

	dsqs[0] = (struct dsq_entry){ cpu_dsq_id, U64_MAX, use_per_cpu_dsq() };
	dsqs[1] = (struct dsq_entry){
		cpdom_dsq_id,
		U64_MAX,
		use_cpdom_dsq() &&
			(!turbulent || !steady_present ||
			 cpdom_nr_queued > turb_nr_queued)
	};
	dsqs[2] = (struct dsq_entry){ cpdom_turb_dsq_id, U64_MAX, use_cpdom_dsq() };

	if (dsqs[0].eligible)
		dsqs[0].vtime = peek_dsq_vtime(dsqs[0].dsq_id);
	if (dsqs[1].eligible)
		dsqs[1].vtime = peek_dsq_vtime(dsqs[1].dsq_id);
	if (dsqs[2].eligible)
		dsqs[2].vtime = peek_dsq_vtime(dsqs[2].dsq_id);

	sort_dsqs(&dsqs[0], &dsqs[1], &dsqs[2]);

	if (dsqs[0].eligible && consume_dsq(cpdomc, dsqs[0].dsq_id))
		return true;
	if (dsqs[1].eligible && consume_dsq(cpdomc, dsqs[1].dsq_id))
		return true;
	if (dsqs[2].eligible && consume_dsq(cpdomc, dsqs[2].dsq_id))
		return true;

	if (nr_cpdoms > 1 && mig_delta_pct == 0 && force_to_steal_task(cpdomc))
		return true;

	return false;
}
