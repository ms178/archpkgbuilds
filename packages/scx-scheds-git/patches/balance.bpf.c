/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2025 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
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

	if (!cpdomc)
		return 0;

	if (no_fast_lb && sys_stat.nr_active_cpdoms) {
		u64 avg = total_load_invr / sys_stat.nr_active_cpdoms;

		fair_share_invr = avg;
		x_mig_delta = calc_mig_delta(avg, nz_qlen, mig_delta_factor);

		stealer_threshold = avg > x_mig_delta ? avg - x_mig_delta : 0;
		stealee_threshold = avg > U64_MAX - x_mig_delta ?
				    U64_MAX : avg + x_mig_delta;
	} else if (cpdomc->nr_active_cpus && total_cap_sum > 0) {
		fair_share_invr = total_load_invr *
				  cpdomc->cap_sum_active_cpus /
				  total_cap_sum;

		x_mig_delta = calc_mig_delta(fair_share_invr, nz_qlen,
					     mig_delta_factor);

		stealer_threshold = fair_share_invr > x_mig_delta ?
				    fair_share_invr - x_mig_delta : 0;
		stealee_threshold = fair_share_invr > U64_MAX - x_mig_delta ?
				    U64_MAX : fair_share_invr + x_mig_delta;
	}

	if (cpdomc->nr_active_cpus &&
	    cpdomc->load_invr <= stealer_threshold) {
		u64 stealer_budget = 0;

		if (fair_share_invr > cpdomc->load_invr)
			stealer_budget = (fair_share_invr -
					  cpdomc->load_invr) / 2;

		WRITE_ONCE(cpdomc->stealer_budget_invr, stealer_budget);
		WRITE_ONCE(cpdomc->stealee_budget_invr, 0);
		WRITE_ONCE(cpdomc->is_stealer, true);
		WRITE_ONCE(cpdomc->is_stealee, false);
		return 0;
	}

	if (!cpdomc->nr_active_cpus ||
	    cpdomc->load_invr >= stealee_threshold) {
		u64 stealee_budget_invr = 0;

		if (cpdomc->qload_invr == 0)
			goto reset_role;

		if (cpdomc->load_invr > fair_share_invr)
			stealee_budget_invr = (cpdomc->load_invr -
					       fair_share_invr) / 2;

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
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc->nr_active_cpus) {
			if (cpdomc->cur_util_wall_sum > 0)
				overflow_running = true;
			continue;
		}

		util = (cpdomc->avg_util_wall_sum << LAVD_SHIFT) /
		       cpdomc->nr_active_cpus;
		if ((util >> LAVD_SHIFT) > max_avg_util_wall)
			max_avg_util_wall = util >> LAVD_SHIFT;

		if (no_fast_lb) {
			u64 qlen = cpdomc->nr_queued_task;
			u64 qlen_invr = 0;

			if (cpdomc->cap_sum_active_cpus > 0)
				qlen_invr = (qlen << (LAVD_SHIFT * 3)) /
					    cpdomc->cap_sum_active_cpus;

			cpdomc->load_invr = util + qlen_invr;
			if (qlen)
				nz_qlen++;
		} else {
			cpdomc->load_invr = cpdomc->avg_util_invr_sum +
					    cpdomc->qload_invr;
			if (cpdomc->qload_invr)
				nz_qlen++;
		}

		total_load_invr += cpdomc->load_invr;
		total_cap_sum += cpdomc->cap_sum_active_cpus;
	}

	if (lb_low_util_wall > 0 && max_avg_util_wall < lb_low_util_wall)
		goto reset_and_skip_lb;

	if (mig_delta_pct > 0)
		mig_delta_factor = (mig_delta_pct << LAVD_SHIFT) / 100;

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

		bpf_for(word, 0, LAVD_CPU_ID_MAX / 64) {
			u64 mask = cpdomc->__cpumask[word];

			bpf_for(iter, 0, 64) {
				int bit = mask_pop_lsb(&mask);
				int cpu;
				s32 queued;

				if (bit < 0)
					break;

				cpu = (word * 64) + bit;
				if (cpu >= (int)nr_cpu_ids)
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

	if (!cpdomc || !cpdomc->nr_active_cpus)
		return false;

	if (no_fast_lb &&
	    !prob_x_out_of_y(1, cpdomc->nr_active_cpus * LAVD_CPDOM_MIG_PROB_FT))
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

	turbulent = cpuc->lat_headroom < LAVD_LC_LATENCY_SENSITIVE_THRESH;
	if (turbulent && use_cpdom_dsq()) {
		cpdom_nr_queued = scx_bpf_dsq_nr_queued(cpdom_dsq_id);
		turb_nr_queued = scx_bpf_dsq_nr_queued(cpdom_turb_dsq_id);
	}

	if (nr_cpdoms > 1 && READ_ONCE(cpdomc->is_stealer) &&
	    try_to_steal_task(cpdomc))
		return true;

	dsqs[0] = (struct dsq_entry){ cpu_dsq_id, U64_MAX, use_per_cpu_dsq() };
	dsqs[1] = (struct dsq_entry){
		cpdom_dsq_id,
		U64_MAX,
		use_cpdom_dsq() &&
			(!turbulent ||
			 cpdom_nr_queued > turb_nr_queued ||
			 cpdomc->nr_steady_cpus == 0)
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
