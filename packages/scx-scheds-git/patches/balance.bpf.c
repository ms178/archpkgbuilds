/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2025 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 * Optimized for Intel Raptor Lake i7-14700KF hybrid architecture
 */

#include <scx/common.bpf.h>
#include <scx/bpf_arena_common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include "util.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <lib/cgroup.h>


extern const volatile u8	mig_delta_pct;

/*
 * Calculate migration delta based on system load.
 * noinline required: Reduces BPF verifier complexity per-function.
 */
u64 __attribute__((noinline)) calc_mig_delta(u64 avg_sc_load, int nz_qlen)
{
	/*
	 * Dynamic threshold adjustment based on queue pressure:
	 * - Overloaded (nz_qlen >= active_cpdoms): Aggressive migration
	 * - Underloaded (nz_qlen == 0): Conservative migration
	 * - Normal: Balanced migration
	 */
	if (nz_qlen >= sys_stat.nr_active_cpdoms)
		return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT_OL;
	if (nz_qlen == 0)
		return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT_UL;
	return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT;
}

/*
 * Plan cross-domain migration by identifying stealer and stealee domains.
 * __weak: Allows override for specialized hardware configurations.
 */
__weak
int plan_x_cpdom_migration(void)
{
	struct cpdom_ctx *cpdomc;
	u64 cpdom_id;
	u32 stealer_threshold, stealee_threshold, nr_stealee = 0;
	u64 avg_sc_load = 0, min_sc_load = U64_MAX, max_sc_load = 0;
	u64 x_mig_delta, util, qlen, sc_qlen;
	bool overflow_running = false;
	int nz_qlen = 0;

	/*
	 * Load balancing goals:
	 * 1) Equalize non-scaled CPU utilization (latency optimization)
	 * 2) Equalize scaled queue lengths (throughput optimization)
	 *
	 * For Raptor Lake: P-cores get higher capacity weighting,
	 * naturally attracting more work during high load.
	 */

	/*
	 * Phase 1: Calculate scaled load per active compute domain.
	 */
	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc->nr_active_cpus) {
			/*
			 * Overflow domain: Tasks running on inactive domain
			 * indicates work overflow - trigger rebalancing.
			 */
			if (cpdomc->cur_util_sum > 0) {
				overflow_running = true;
				cpdomc->sc_load = U32_MAX;
			} else {
				cpdomc->sc_load = 0;
			}
			continue;
		}

		/*
		 * Utilization calculation:
		 * - mig_delta_pct > 0: Use smoothed average (stable)
		 * - mig_delta_pct == 0: Use instantaneous (responsive)
		 */
		if (mig_delta_pct > 0)
			util = (cpdomc->avg_util_sum << LAVD_SHIFT) / cpdomc->nr_active_cpus;
		else
			util = (cpdomc->cur_util_sum << LAVD_SHIFT) / cpdomc->nr_active_cpus;

		qlen = cpdomc->nr_queued_task;
		/*
		 * Scaled queue length: Normalizes by domain capacity.
		 * Higher capacity domains (P-cores) get lower sc_qlen
		 * for same qlen, attracting more work.
		 */
		sc_qlen = (qlen << (LAVD_SHIFT * 3)) / cpdomc->cap_sum_active_cpus;
		cpdomc->sc_load = util + sc_qlen;
		avg_sc_load += cpdomc->sc_load;

		if (min_sc_load > cpdomc->sc_load)
			min_sc_load = cpdomc->sc_load;
		if (max_sc_load < cpdomc->sc_load)
			max_sc_load = cpdomc->sc_load;
		if (qlen)
			nz_qlen++;
	}

	if (sys_stat.nr_active_cpdoms)
		avg_sc_load /= sys_stat.nr_active_cpdoms;

	/*
	 * Phase 2: Determine stealer/stealee thresholds.
	 * Tighter thresholds under higher load.
	 */
	if (mig_delta_pct > 0) {
		u64 mig_delta_factor = (mig_delta_pct << LAVD_SHIFT) / 100;
		x_mig_delta = avg_sc_load * mig_delta_factor / LAVD_SCALE;
	} else {
		x_mig_delta = calc_mig_delta(avg_sc_load, nz_qlen);
	}

	stealer_threshold = avg_sc_load - x_mig_delta;
	stealee_threshold = avg_sc_load + x_mig_delta;

	/*
	 * Early exit: No overloaded domain, no stealing needed.
	 * [stealer_threshold ... avg ... max_sc_load ... stealee_threshold]
	 */
	if ((stealee_threshold > max_sc_load) && !overflow_running)
		return 0;

	/*
	 * Adjust threshold when overloaded domain exists but
	 * stealer_threshold is below minimum.
	 * [stealer_threshold ... min_sc_load ... avg ... stealee ... max]
	 */
	if ((stealee_threshold <= max_sc_load || overflow_running) &&
	    (stealer_threshold < min_sc_load))
		stealer_threshold = min_sc_load;

	/*
	 * Phase 3: Classify domains as stealer, stealee, or neutral.
	 */
	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);

		/* Under-loaded active domains: eligible to steal */
		if (cpdomc->nr_active_cpus &&
		    cpdomc->sc_load <= stealer_threshold) {
			WRITE_ONCE(cpdomc->is_stealer, true);
			WRITE_ONCE(cpdomc->is_stealee, false);
			continue;
		}

		/* Over-loaded or inactive domains: eligible for stealing from */
		if (!cpdomc->nr_active_cpus ||
		    cpdomc->sc_load >= stealee_threshold) {
			WRITE_ONCE(cpdomc->is_stealer, false);
			WRITE_ONCE(cpdomc->is_stealee, true);
			nr_stealee++;
			continue;
		}

		/* Neutral: neither stealing nor being stolen from */
		WRITE_ONCE(cpdomc->is_stealer, false);
		WRITE_ONCE(cpdomc->is_stealee, false);
	}

	sys_stat.nr_stealee = nr_stealee;

	return 0;
}

/*
 * Consume a task from the specified DSQ with latency tracking.
 */
static bool consume_dsq(struct cpdom_ctx *cpdomc, u64 dsq_id)
{
	bool ret;
	u64 before = 0;

	if (is_monitored)
		before = bpf_ktime_get_ns();

	ret = scx_bpf_dsq_move_to_local(dsq_id);

	if (is_monitored)
		cpdomc->dsq_consume_lat = time_delta(bpf_ktime_get_ns(), before);

	return ret;
}

/*
 * Optimized bit manipulation for CPU mask iteration.
 * Uses TZCNT instruction (1 cycle on Raptor Lake) instead of
 * cpumask_next_set_bit function call overhead.
 *
 * Returns: bit position (0-63) or -1 if no bits set.
 * Side effect: Clears the returned bit from mask.
 */
static __always_inline int mask_pop_lsb(u64 *mask)
{
	u64 val = *mask;

	if (!val)
		return -1;

	/* Clear lowest set bit: x & (x-1) */
	*mask = val & (val - 1);

	/* Count trailing zeros = position of lowest set bit */
	return __builtin_ctzll(val);
}

/*
 * Find the most loaded DSQ within a compute domain for work stealing.
 * noinline: Required for BPF verifier complexity management.
 */
u64 __attribute__((noinline)) pick_most_loaded_dsq(struct cpdom_ctx *cpdomc)
{
	u64 pick_dsq_id = -ENOENT;
	s32 highest_queued = -1;

	if (!cpdomc) {
		scx_bpf_error("Invalid cpdom context");
		return -ENOENT;
	}

	/*
	 * Strategy: Find DSQ with highest queued task count.
	 * For hybrid architectures, this naturally balances between
	 * P-core and E-core DSQs based on actual load.
	 */

	/* Check per-domain DSQ first */
	if (use_cpdom_dsq()) {
		pick_dsq_id = cpdom_to_dsq(cpdomc->id);
		highest_queued = scx_bpf_dsq_nr_queued(pick_dsq_id);
	}

	/* Check per-CPU DSQs if migratable */
	if (is_per_cpu_dsq_migratable()) {
		s32 pick_cpu = -ENOENT;
		s32 word;
		s32 iter;

		/*
		 * Iterate through CPU mask words (64 CPUs per word).
		 * For 14700KF: 28 threads = 1 word (bits 0-27 set).
		 */
		bpf_for(word, 0, LAVD_CPU_ID_MAX / 64) {
			u64 mask = cpdomc->__cpumask[word];

			/*
			 * Process each set bit using optimized pop.
			 * Early exit when mask exhausted.
			 */
			bpf_for(iter, 0, 64) {
				int bit = mask_pop_lsb(&mask);
				s32 queued;
				s32 cpu;

				if (bit < 0)
					break;

				cpu = (word * 64) + bit;
				if (cpu >= nr_cpu_ids)
					break;

				/*
				 * Count both regular per-CPU DSQ and
				 * LOCAL_ON DSQ (tasks pinned to this CPU).
				 */
				queued = scx_bpf_dsq_nr_queued(cpu_to_dsq(cpu)) +
					 scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpu);

				if (queued > highest_queued) {
					highest_queued = queued;
					pick_cpu = cpu;
				}
			}
		}

		if (pick_cpu >= 0)
			pick_dsq_id = cpu_to_dsq(pick_cpu);
	}

	return pick_dsq_id;
}

/*
 * Probabilistic task stealing from neighbor domains.
 * Uses distance-ordered traversal to prefer closer domains
 * (important for NUMA and hybrid cache topologies).
 */
static bool try_to_steal_task(struct cpdom_ctx *cpdomc)
{
	struct cpdom_ctx *cpdomc_pick;
	s64 nr_nbr, cpdom_id;

	/* Defensive null check + active domain requirement */
	if (!cpdomc || !cpdomc->nr_active_cpus)
		return false;

	/*
	 * Probabilistic gating: Prevents thundering herd when multiple
	 * CPUs become idle simultaneously. Only 1/N CPUs attempt stealing.
	 */
	if (!prob_x_out_of_y(1, cpdomc->nr_active_cpus * LAVD_CPDOM_MIG_PROB_FT))
		return false;

	/*
	 * Traverse neighbors by distance (cache topology aware).
	 * For Raptor Lake: P-core to E-core is distance 1 (shared L3).
	 */
	for (int dist = 0; dist < LAVD_CPDOM_MAX_DIST; dist++) {
		nr_nbr = min(cpdomc->nr_neighbors[dist], LAVD_CPDOM_MAX_NR);
		if (!nr_nbr)
			break;

		/*
		 * Optimized loop: Only iterate actual neighbor count,
		 * not LAVD_CPDOM_MAX_NR with early break.
		 */
		for (int idx = 0; idx < nr_nbr; idx++) {
			u64 dsq_id;

			cpdom_id = get_neighbor_id(cpdomc, dist, idx);
			if (cpdom_id < 0)
				continue;

			cpdomc_pick = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
			if (!cpdomc_pick) {
				scx_bpf_error("Failed to lookup cpdom_ctx for %llu", cpdom_id);
				return false;
			}

			/* Skip non-stealee or invalid domains */
			if (!READ_ONCE(cpdomc_pick->is_stealee) || !cpdomc_pick->is_valid)
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);

			/* Validate DSQ ID before consumption attempt */
			if ((s64)dsq_id < 0)
				continue;

			/*
			 * Successful steal: Mark both domains to prevent
			 * over-migration within this scheduling round.
			 */
			if (consume_dsq(cpdomc_pick, dsq_id)) {
				WRITE_ONCE(cpdomc_pick->is_stealee, false);
				WRITE_ONCE(cpdomc->is_stealer, false);
				return true;
			}
		}

		/*
		 * Exponential backoff for distant neighbors.
		 * Reduces cross-NUMA migration probability.
		 */
		if (!prob_x_out_of_y(1, LAVD_CPDOM_MIG_PROB_FT))
			break;
	}

	return false;
}

/*
 * Forced task stealing without probabilistic gating.
 * Used when local DSQs are empty and work is needed.
 */
static bool force_to_steal_task(struct cpdom_ctx *cpdomc)
{
	struct cpdom_ctx *cpdomc_pick;
	s64 nr_nbr, cpdom_id;

	if (!cpdomc)
		return false;

	/*
	 * Traverse all neighbors unconditionally.
	 * No probability check - CPU is idle and needs work.
	 */
	for (int dist = 0; dist < LAVD_CPDOM_MAX_DIST; dist++) {
		nr_nbr = min(cpdomc->nr_neighbors[dist], LAVD_CPDOM_MAX_NR);
		if (!nr_nbr)
			break;

		for (int idx = 0; idx < nr_nbr; idx++) {
			u64 dsq_id;

			cpdom_id = get_neighbor_id(cpdomc, dist, idx);
			if (cpdom_id < 0)
				continue;

			cpdomc_pick = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
			if (!cpdomc_pick) {
				scx_bpf_error("Failed to lookup cpdom_ctx for %llu", cpdom_id);
				return false;
			}

			if (!cpdomc_pick->is_valid)
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);

			/* Early validation avoids unnecessary consume attempt */
			if ((s64)dsq_id < 0)
				continue;

			if (consume_dsq(cpdomc_pick, dsq_id))
				return true;
		}
	}

	return false;
}

/*
 * Main task consumption entry point for dispatch path.
 * Orchestrates local consumption and cross-domain stealing.
 */
__hidden
bool consume_task(u64 cpu_dsq_id, u64 cpdom_dsq_id)
{
	struct cpdom_ctx *cpdomc;
	struct task_struct *p;
	u64 vtime = U64_MAX;

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [dsq_to_cpdom(cpdom_dsq_id)]);
	if (!cpdomc) {
		scx_bpf_error("Failed to lookup cpdom_ctx for %llu", dsq_to_cpdom(cpdom_dsq_id));
		return false;
	}

	/*
	 * Priority 1: Cross-domain stealing if we're a designated stealer.
	 * Probabilistic to prevent thundering herd.
	 */
	if (nr_cpdoms > 1 && READ_ONCE(cpdomc->is_stealer) &&
	    try_to_steal_task(cpdomc))
		goto x_domain_migration_out;

	/*
	 * Priority 2: Consume from local DSQs.
	 * When both per-CPU and per-domain DSQs are active,
	 * select task with lowest vtime for fairness.
	 */
	if (use_per_cpu_dsq() && use_cpdom_dsq()) {
		u64 dsq_id = cpu_dsq_id;
		u64 backup_dsq_id = cpdom_dsq_id;

		/* Peek CPU DSQ for vtime comparison */
		p = __COMPAT_scx_bpf_dsq_peek(cpu_dsq_id);
		if (p)
			vtime = p->scx.dsq_vtime;

		/* Compare with domain DSQ, prefer lower vtime */
		p = __COMPAT_scx_bpf_dsq_peek(cpdom_dsq_id);
		if (p && p->scx.dsq_vtime < vtime) {
			dsq_id = cpdom_dsq_id;
			backup_dsq_id = cpu_dsq_id;
		}

		/*
		 * Race handling: If preferred DSQ loses race,
		 * fall back to alternate DSQ to prevent stalls.
		 */
		if (consume_dsq(cpdomc, dsq_id))
			return true;
		if (consume_dsq(cpdomc, backup_dsq_id))
			return true;

	} else if (use_cpdom_dsq()) {
		if (consume_dsq(cpdomc, cpdom_dsq_id))
			return true;

	} else if (use_per_cpu_dsq()) {
		if (consume_dsq(cpdomc, cpu_dsq_id))
			return true;
	}

	/*
	 * Priority 3: Force stealing when local DSQs are empty.
	 * Disabled when mig_delta_pct is set to respect explicit thresholds.
	 */
	if (nr_cpdoms > 1 && mig_delta_pct == 0 && force_to_steal_task(cpdomc))
		goto x_domain_migration_out;

	return false;

x_domain_migration_out:
	return true;
}
