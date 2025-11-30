/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2025 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 * Optimized for Raptor Lake: Exposed stealing primitives for main.bpf.c
 */

/*
 * To be included to the main.bpf.c
 */

extern const volatile u8	mig_delta_pct;

u64 __attribute__ ((noinline)) calc_mig_delta(u64 avg_sc_load, int nz_qlen)
{
	/*
	 * Note that added "noinline" to make the verifier happy.
	 */
	if (nz_qlen >= sys_stat.nr_active_cpdoms)
		return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT_OL;
	if (nz_qlen == 0)
		return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT_UL;
	return avg_sc_load >> LAVD_CPDOM_MIG_SHIFT;
}

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

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc->nr_active_cpus) {
			if (cpdomc->cur_util_sum > 0) {
				overflow_running = true;
				cpdomc->sc_load = U32_MAX;
			}
			else
				cpdomc->sc_load = 0;
			continue;
		}

		if (mig_delta_pct > 0)
			util = (cpdomc->avg_util_sum << LAVD_SHIFT) / cpdomc->nr_active_cpus;
		else
			util = (cpdomc->cur_util_sum << LAVD_SHIFT) / cpdomc->nr_active_cpus;
		qlen = cpdomc->nr_queued_task;
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

	if (mig_delta_pct > 0) {
		u64 mig_delta_factor = (mig_delta_pct << LAVD_SHIFT) / 100;
		x_mig_delta = avg_sc_load * mig_delta_factor / LAVD_SCALE;
	} else {
		x_mig_delta = calc_mig_delta(avg_sc_load, nz_qlen);
	}
	stealer_threshold = avg_sc_load - x_mig_delta;
	stealee_threshold = avg_sc_load + x_mig_delta;

	if ((stealee_threshold > max_sc_load) && !overflow_running) {
		return 0;
	}
	if ((stealee_threshold <= max_sc_load || overflow_running) &&
	    (stealer_threshold < min_sc_load)) {
		stealer_threshold = min_sc_load;
	}

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);

		if (cpdomc->nr_active_cpus &&
		    cpdomc->sc_load <= stealer_threshold) {
			WRITE_ONCE(cpdomc->is_stealer, true);
			WRITE_ONCE(cpdomc->is_stealee, false);
			continue;
		}

		if (!cpdomc->nr_active_cpus ||
		    cpdomc->sc_load >= stealee_threshold) {
			WRITE_ONCE(cpdomc->is_stealer, false);
			WRITE_ONCE(cpdomc->is_stealee, true);
			nr_stealee++;
			continue;
		}

		WRITE_ONCE(cpdomc->is_stealer, false);
		WRITE_ONCE(cpdomc->is_stealee, false);
	}

	sys_stat.nr_stealee = nr_stealee;

	return 0;
}

/*
 * dsq_id: candidate DSQ to consume from, can be per-cpdom or per-cpu.
 */
static bool consume_dsq(struct cpdom_ctx *cpdomc, u64 dsq_id)
{
	bool ret;
	u64 before = 0;

	if (is_monitored)
		before = bpf_ktime_get_ns();

	/* Updated API: scx_bpf_dsq_move_to_local */
	ret = scx_bpf_dsq_move_to_local(dsq_id);

	if (is_monitored)
		cpdomc->dsq_consume_lat = time_delta(bpf_ktime_get_ns(), before);

	return ret;
}

static __always_inline int mask_pop_lsb(u64 *mask)
{
	u64 val = *mask;

	if (!val)
		return -1;

	*mask = val & (val - 1);
	return __builtin_ctzll(val);
}

u64 __attribute__((noinline)) pick_most_loaded_dsq(struct cpdom_ctx *cpdomc)
{
	u64 pick_dsq_id = -ENOENT;
	s32 highest_queued = -1;

	if (!cpdomc) {
		scx_bpf_error("Invalid cpdom context");
		return -ENOENT;
	}

	if (use_cpdom_dsq()) {
		pick_dsq_id = cpdom_to_dsq(cpdomc->id);
		highest_queued = scx_bpf_dsq_nr_queued(pick_dsq_id);
	}

	if (is_per_cpu_dsq_migratable()) {
		s32 pick_cpu = -ENOENT;
		s32 word;
		s32 iter;

		bpf_for(word, 0, LAVD_CPU_ID_MAX / 64) {
			u64 mask = cpdomc->__cpumask[word];

			bpf_for(iter, 0, 64) {
				int bit = mask_pop_lsb(&mask);
				s32 queued;
				s32 cpu;

				if (bit < 0)
					break;

				cpu = (word * 64) + bit;
				if (cpu >= nr_cpu_ids)
					break;

				/* Upstream update: Include SCX_DSQ_LOCAL_ON tasks in queued count */
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

static bool try_to_steal_task(struct cpdom_ctx *cpdomc)
{
	struct cpdom_ctx *cpdomc_pick;
	s64 nr_nbr, cpdom_id;

	if (!cpdomc || !cpdomc->nr_active_cpus)
		return false;

	if (!prob_x_out_of_y(1, cpdomc->nr_active_cpus * LAVD_CPDOM_MIG_PROB_FT))
		return false;

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
			if (!cpdomc_pick)
				return false;

			if (!READ_ONCE(cpdomc_pick->is_stealee) || !cpdomc_pick->is_valid)
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);
			if ((s64)dsq_id < 0)
				continue;

			if (consume_dsq(cpdomc_pick, dsq_id)) {
				WRITE_ONCE(cpdomc_pick->is_stealee, false);
				WRITE_ONCE(cpdomc->is_stealer, false);
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
			if (!cpdomc_pick || !cpdomc_pick->is_valid)
				continue;

			dsq_id = pick_most_loaded_dsq(cpdomc_pick);
			if ((s64)dsq_id < 0)
				continue;

			if (consume_dsq(cpdomc_pick, dsq_id))
				return true;
		}
	}

	return false;
}
