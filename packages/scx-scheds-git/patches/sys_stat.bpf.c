/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */

#include <scx/common.bpf.h>

#include "intf.h"
#include "lavd.bpf.h"
#include "power.bpf.h"

#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

extern bool CONFIG_NO_HZ_IDLE __kconfig __weak;

struct sys_stat		__weak	sys_stat;

const volatile u16	__weak lat_load_target_pct;
const volatile u8	__weak preempt_shift;

volatile u64		__weak performance_mode_ns;
volatile u64		__weak balanced_mode_ns;
volatile u64		__weak powersave_mode_ns;

extern const volatile u64	slice_min_ns;
extern const volatile u64	slice_max_ns;

extern volatile bool		__weak no_core_compaction;
extern volatile bool		__weak reinit_cpumask_for_performance;

const volatile bool	__weak is_autopilot_on;

int do_autopilot(void);

u32 calc_avg32(u32 old_val, u32 new_val);
u64 calc_avg(u64 old_val, u64 new_val);
int update_power_mode_time(void);

struct update_timer {
	struct bpf_timer timer;
};

struct {
	__uint(type, BPF_MAP_TYPE_ARRAY);
	__uint(max_entries, 1);
	__type(key, u32);
	__type(value, struct update_timer);
} update_timer SEC(".maps") __weak;

struct sys_stat_ctx {
	u64		now;
	u64		duration_wall;
	u64		duration_total_wall;
	u64		idle_total_wall;
	u64		compute_total_wall;
	u64		compute_total_invr;
	u64		tot_task_time_iwgt;
	u64		tsct_spike_invr;
	u64		nr_queued_task;
	s32		max_lat_cri;
	s32		avg_lat_cri;
	u64		sum_lat_cri;
	u32		nr_sched;
	u32		nr_preempt;
	u32		nr_perf_cri;
	u32		nr_lat_cri;
	u32		nr_x_migration;
	u32		nr_big;
	u32		nr_pc_on_big;
	u32		nr_lc_on_big;
	u64		min_perf_cri;
	u64		avg_perf_cri;
	u64		max_perf_cri;
	u64		sum_perf_cri;
	u32		thr_perf_cri;
	u32		cur_util_wall;
	u32		cur_util_invr;
};

static struct sys_stat_ctx ctx;

static __always_inline u64 lavd_nonzero_u64(u64 v)
{
	return v ? v : 1;
}

static __always_inline u32 lavd_scaled_util(u64 value, u64 duration)
{
	return (u32)((value << LAVD_SHIFT) / duration);
}

static __always_inline s32 lavd_lat_cri_thresh(s32 max_lat, s32 avg_lat, u8 shift)
{
	u32 s;

	if (max_lat <= avg_lat)
		return avg_lat;

	s = shift < 31 ? shift : 31;
	return max_lat - ((max_lat - avg_lat) >> s);
}

static __always_inline void lavd_decay_mode_time(volatile u64 *p)
{
	int i;

	for (i = 0; i < LAVD_MAX_RETRY; i++) {
		u64 old = READ_ONCE(*p);
		u64 sub = old >> 1;
		u64 new;

		if (!sub)
			return;

		new = old - sub;
		if (__sync_bool_compare_and_swap(p, old, new))
			return;
	}
}

static void init_sys_stat_ctx(void)
{
	struct sys_stat_ctx *c = &ctx;
	u64 last_update_clk = READ_ONCE(sys_stat.last_update_clk);

	__builtin_memset(c, 0, sizeof(*c));

	c->min_perf_cri = LAVD_SCALE;
	c->now = scx_bpf_now();
	c->duration_wall = lavd_nonzero_u64(time_delta(c->now, last_update_clk));

	WRITE_ONCE(sys_stat.last_update_clk, c->now);
}

static void collect_sys_stat(void)
{
	struct sys_stat_ctx *c = &ctx;
	struct cpdom_ctx *cpdomc;
	const bool use_cpdom_dsq_cached = use_cpdom_dsq();
	const bool use_per_cpu_dsq_cached = use_per_cpu_dsq();
	const bool can_boost_slice_cached = can_boost_slice();
	const bool nohz_idle = CONFIG_NO_HZ_IDLE;
	const u64 duration_wall = c->duration_wall;
	u64 cpdom_id;
	u64 compute_wall = 1;
	int cpu;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		int i, j, k;

		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc)
			continue;

		cpdomc->cur_util_wall_sum = 0;
		cpdomc->avg_util_wall_sum = 0;
		cpdomc->cur_util_invr_sum = 0;
		cpdomc->avg_util_invr_sum = 0;
		cpdomc->cur_steal_util_wall_sum = 0;
		cpdomc->avg_steal_util_wall_sum = 0;
		cpdomc->cur_steal_util_invr_sum = 0;
		cpdomc->avg_steal_util_invr_sum = 0;
		cpdomc->cur_dom_pinned_util_wall_sum = 0;
		cpdomc->avg_dom_pinned_util_wall_sum = 0;
		cpdomc->cur_dom_pinned_util_invr_sum = 0;
		cpdomc->avg_dom_pinned_util_invr_sum = 0;
		cpdomc->nr_queued_task = 0;
		cpdomc->util_sum_steady = 0;
		cpdomc->util_sum_turb = 0;
		cpdomc->cap_sum_steady = 0;
		cpdomc->cap_sum_turb = 0;
		cpdomc->nr_steady_cpus = 0;
		cpdomc->nr_turb_cpus = 0;

		if (use_cpdom_dsq_cached) {
			cpdomc->nr_queued_task =
				scx_bpf_dsq_nr_queued(cpdom_to_dsq(cpdom_id)) +
				scx_bpf_dsq_nr_queued(cpdom_to_turb_dsq(cpdom_id));
		}

		bpf_for(i, 0, LAVD_CPU_ID_MAX / 64) {
			u64 cpumask = cpdomc->__cpumask[i];

			bpf_for(k, 0, 64) {
				j = cpumask_next_set_bit(&cpumask);
				if (j < 0)
					break;

				cpu = (i * 64) + j;
				if (cpu >= nr_cpu_ids)
					break;

				cpdomc->nr_queued_task +=
					scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpu);

				if (use_per_cpu_dsq_cached)
					cpdomc->nr_queued_task +=
						scx_bpf_dsq_nr_queued(cpu_to_dsq(cpu));
			}
		}

		c->nr_queued_task += cpdomc->nr_queued_task;
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		u64 compute_invr;
		u64 cpuc_tot_task_time_invr;
		u64 irq_steal_wall;
		u64 irq_steal_invr;
		u64 task_wall;
		u64 rt_dl_time_invr;
		u64 now_task;
		u64 now_pelt;
		u64 delta_task;
		u64 delta_pelt;
		u64 cur_idle_wall = 0;
		u64 past_idle_wall;
		u64 dom_pinned_task_time_wall;
		u64 dom_pinned_task_time_invr;
		struct cpu_ctx *cpuc = get_cpu_ctx_id(cpu);
		int retry;

		if (!cpuc) {
			c->compute_total_wall = 0;
			break;
		}

		if (cpuc->nr_pinned_tasks || !can_boost_slice_cached ||
		    scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpuc->cpu_id)) {
			shrink_boosted_slice_remote(cpuc, c->now);
		}

		c->tot_task_time_iwgt += cpuc->tot_task_time_iwgt;
		cpuc->tot_task_time_iwgt = 0;

		for (retry = 0; retry < LAVD_MAX_RETRY; retry++) {
			u64 old_clk = cpuc->idle_start_clk;

			if (old_clk == 0 || time_after(old_clk, c->now))
				break;

			if (__sync_bool_compare_and_swap(&cpuc->idle_start_clk,
							 old_clk, c->now)) {
				cur_idle_wall = time_delta(c->now, old_clk);
				__sync_fetch_and_add(&cpuc->idle_total_wall,
						     cur_idle_wall);
				break;
			}
		}

		compute_wall = time_delta(duration_wall, cpuc->idle_total_wall);

		cpuc->steal_time_wall =
			time_delta(compute_wall, cpuc->tot_task_time_wall);
		cpuc->tot_task_time_wall = 0;

		dom_pinned_task_time_wall = cpuc->tot_dom_pinned_task_time_wall;
		cpuc->tot_dom_pinned_task_time_wall = 0;

		now_task = scx_clock_task(cpu);
		now_pelt = scx_clock_pelt(cpu);
		delta_task = time_delta(now_task, cpuc->prev_task_clk);

		if (nohz_idle && cur_idle_wall > 0) {
			u64 cap = scx_bpf_cpuperf_cap(cpu);
			u64 cur_idle_pelt = (cur_idle_wall * cap) >> LAVD_SHIFT;

			now_pelt += cur_idle_pelt;
		}

		delta_pelt = time_delta(now_pelt, cpuc->prev_pelt_clk);
		if (delta_pelt > compute_wall)
			delta_pelt = compute_wall;

		past_idle_wall = time_delta(cpuc->idle_total_wall, cur_idle_wall);
		task_wall = time_delta(delta_task, past_idle_wall);
		irq_steal_wall = time_delta(compute_wall, task_wall);

		if (task_wall > 0) {
			u64 perf_factor = (delta_pelt << LAVD_SHIFT) / task_wall;

			cpuc->avg_perf_factor =
				calc_avg(cpuc->avg_perf_factor, perf_factor);
			irq_steal_invr =
				conv_wall_to_invr_obs(irq_steal_wall,
						      delta_pelt,
						      task_wall);
		} else {
			irq_steal_invr =
				(irq_steal_wall * cpuc->avg_perf_factor) >>
				LAVD_SHIFT;
		}

		cpuc_tot_task_time_invr = cpuc->tot_task_time_invr;
		cpuc->tot_task_time_invr = 0;

		dom_pinned_task_time_invr = cpuc->tot_dom_pinned_task_time_invr;
		cpuc->tot_dom_pinned_task_time_invr = 0;

		rt_dl_time_invr = time_delta(delta_pelt, cpuc_tot_task_time_invr);
		cpuc->steal_time_invr = irq_steal_invr + rt_dl_time_invr;
		if (cpuc->steal_time_invr > compute_wall)
			cpuc->steal_time_invr = compute_wall;

		cpuc->cur_steal_util_wall =
			lavd_scaled_util(cpuc->steal_time_wall, duration_wall);
		cpuc->avg_steal_util_wall =
			calc_asym_avg(cpuc->avg_steal_util_wall,
				      cpuc->cur_steal_util_wall);

		cpuc->cur_steal_util_invr =
			lavd_scaled_util(cpuc->steal_time_invr, duration_wall);
		cpuc->avg_steal_util_invr =
			calc_asym_avg(cpuc->avg_steal_util_invr,
				      cpuc->cur_steal_util_invr);

		ravg_accumulate(&cpuc->avg_irq_steal_ravg,
				cpuc->cur_steal_util_invr,
				c->now,
				LAVD_RAVG_HALFLIFE_NS);

		{
			u64 avg_irq_fp =
				ravg_read(&cpuc->avg_irq_steal_ravg,
					  c->now,
					  LAVD_RAVG_HALFLIFE_NS);
			u32 avg_irq_val = (u32)(avg_irq_fp >> RAVG_FRAC_BITS);

			cpuc->lat_headroom =
				(avg_irq_val < LAVD_SCALE) ?
				(LAVD_SCALE - avg_irq_val) : 0;
		}

		cpuc->cur_util_wall =
			lavd_scaled_util(compute_wall, duration_wall);
		cpuc->avg_util_wall =
			calc_asym_avg(cpuc->avg_util_wall,
				      cpuc->cur_util_wall);

		compute_invr = cpuc_tot_task_time_invr + cpuc->steal_time_invr;
		if (compute_invr > compute_wall)
			compute_invr = compute_wall;

		cpuc->cur_util_invr =
			lavd_scaled_util(compute_invr, duration_wall);
		cpuc->avg_util_invr =
			calc_asym_avg(cpuc->avg_util_invr,
				      cpuc->cur_util_invr);

		if (dom_pinned_task_time_wall > compute_wall)
			dom_pinned_task_time_wall = compute_wall;
		if (dom_pinned_task_time_invr > compute_wall)
			dom_pinned_task_time_invr = compute_wall;

		cpuc->cur_dom_pinned_util_wall =
			lavd_scaled_util(dom_pinned_task_time_wall,
					 duration_wall);
		cpuc->avg_dom_pinned_util_wall =
			calc_asym_avg(cpuc->avg_dom_pinned_util_wall,
				      cpuc->cur_dom_pinned_util_wall);

		cpuc->cur_dom_pinned_util_invr =
			lavd_scaled_util(dom_pinned_task_time_invr,
					 duration_wall);
		cpuc->avg_dom_pinned_util_invr =
			calc_asym_avg(cpuc->avg_dom_pinned_util_invr,
				      cpuc->cur_dom_pinned_util_invr);

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (cpdomc) {
			cpdomc->cur_util_wall_sum += cpuc->cur_util_wall;
			cpdomc->avg_util_wall_sum += cpuc->avg_util_wall;
			cpdomc->cur_util_invr_sum += cpuc->cur_util_invr;
			cpdomc->avg_util_invr_sum += cpuc->avg_util_invr;
			cpdomc->cur_steal_util_wall_sum +=
				cpuc->cur_steal_util_wall;
			cpdomc->avg_steal_util_wall_sum +=
				cpuc->avg_steal_util_wall;
			cpdomc->cur_steal_util_invr_sum +=
				cpuc->cur_steal_util_invr;
			cpdomc->avg_steal_util_invr_sum +=
				cpuc->avg_steal_util_invr;
			cpdomc->cur_dom_pinned_util_wall_sum +=
				cpuc->cur_dom_pinned_util_wall;
			cpdomc->avg_dom_pinned_util_wall_sum +=
				cpuc->avg_dom_pinned_util_wall;
			cpdomc->cur_dom_pinned_util_invr_sum +=
				cpuc->cur_dom_pinned_util_invr;
			cpdomc->avg_dom_pinned_util_invr_sum +=
				cpuc->avg_dom_pinned_util_invr;
		}

		cpuc->prev_task_clk = now_task;
		cpuc->prev_pelt_clk = now_pelt;

		c->compute_total_invr += compute_invr;

		if (cpuc->cur_util_wall > LAVD_CC_UTIL_SPIKE)
			c->tsct_spike_invr += compute_invr;
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		struct bpf_cpumask *steady;
		struct cpdom_ctx *cpu_cpdomc;
		struct cpu_ctx *cpuc = get_cpu_ctx_id(cpu);

		if (!cpuc) {
			c->compute_total_wall = 0;
			break;
		}

		update_effective_capacity(cpuc);

		if (cpuc->big_core) {
			c->nr_big += cpuc->nr_sched;
			c->nr_pc_on_big += cpuc->nr_perf_cri;
			c->nr_lc_on_big += cpuc->nr_lat_cri;
		}

		c->nr_perf_cri += cpuc->nr_perf_cri;
		cpuc->nr_perf_cri = 0;

		c->nr_lat_cri += cpuc->nr_lat_cri;
		cpuc->nr_lat_cri = 0;

		c->nr_x_migration += cpuc->nr_x_migration;
		cpuc->nr_x_migration = 0;

		c->sum_lat_cri += cpuc->sum_lat_cri;
		cpuc->sum_lat_cri = 0;

		c->nr_sched += cpuc->nr_sched;
		cpuc->nr_sched = 0;

		c->nr_preempt += cpuc->nr_preempt;
		cpuc->nr_preempt = 0;

		if (cpuc->max_lat_cri > c->max_lat_cri)
			c->max_lat_cri = cpuc->max_lat_cri;
		cpuc->max_lat_cri = 0;

		if (have_little_core) {
			if (cpuc->min_perf_cri < c->min_perf_cri)
				c->min_perf_cri = cpuc->min_perf_cri;
			cpuc->min_perf_cri = LAVD_SCALE;

			if (cpuc->max_perf_cri > c->max_perf_cri)
				c->max_perf_cri = cpuc->max_perf_cri;
			cpuc->max_perf_cri = 0;

			c->sum_perf_cri += cpuc->sum_perf_cri;
			cpuc->sum_perf_cri = 0;
		}

		bpf_rcu_read_lock();

		steady = steady_cpumask;
		if (steady) {
			if (cpuc->lat_headroom >= LAVD_LC_LATENCY_SENSITIVE_THRESH)
				bpf_cpumask_set_cpu(cpu, steady);
			else
				bpf_cpumask_clear_cpu(cpu, steady);
		}

		bpf_rcu_read_unlock();

		cpu_cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (cpu_cpdomc) {
			if (cpuc->lat_headroom >= LAVD_LC_LATENCY_SENSITIVE_THRESH) {
				cpu_cpdomc->util_sum_steady += cpuc->util_est;
				cpu_cpdomc->cap_sum_steady += cpuc->max_capacity;
				cpu_cpdomc->nr_steady_cpus++;
			} else {
				cpu_cpdomc->util_sum_turb += cpuc->util_est;
				cpu_cpdomc->cap_sum_turb += cpuc->max_capacity;
				cpu_cpdomc->nr_turb_cpus++;
			}
		}

		c->idle_total_wall += cpuc->idle_total_wall;
		cpuc->idle_total_wall = 0;
	}
}

static void calc_sys_stat(void)
{
	struct sys_stat_ctx *c = &ctx;
	static int cnt;
	u64 avg_svc_time_iwgt = 0;
	u64 cur_util_invr;
	u64 scu_spike_invr;
	u64 cpdom_id;
	u64 nr_online = nr_cpus_onln;

	if (!nr_online)
		nr_online = 1;

	c->duration_total_wall =
		lavd_nonzero_u64(c->duration_wall * nr_online);
	c->compute_total_wall =
		time_delta(c->duration_total_wall, c->idle_total_wall);
	c->cur_util_wall =
		lavd_scaled_util(c->compute_total_wall,
				 c->duration_total_wall);

	cur_util_invr =
		(c->compute_total_invr << LAVD_SHIFT) /
		c->duration_total_wall;
	if (cur_util_invr > c->cur_util_wall)
		cur_util_invr = min(sys_stat.avg_util_invr, c->cur_util_wall);

	scu_spike_invr =
		(c->tsct_spike_invr << (LAVD_SHIFT - 1)) /
		c->duration_total_wall;
	c->cur_util_invr =
		min(cur_util_invr + scu_spike_invr, (u64)LAVD_SCALE);

	if (c->nr_sched == 0 || c->compute_total_wall == 0) {
		c->max_lat_cri = sys_stat.max_lat_cri;
		c->avg_lat_cri = sys_stat.avg_lat_cri;

		if (have_little_core) {
			c->min_perf_cri = sys_stat.min_perf_cri;
			c->max_perf_cri = sys_stat.max_perf_cri;
			c->avg_perf_cri = sys_stat.avg_perf_cri;
		}
	} else {
		c->avg_lat_cri = c->sum_lat_cri / c->nr_sched;

		if (have_little_core)
			c->avg_perf_cri = c->sum_perf_cri / c->nr_sched;
	}

	sys_stat.avg_util_wall =
		calc_asym_avg(sys_stat.avg_util_wall, c->cur_util_wall);
	sys_stat.avg_util_invr =
		calc_asym_avg(sys_stat.avg_util_invr, c->cur_util_invr);

	sys_stat.max_lat_cri =
		(__typeof__(sys_stat.max_lat_cri))
		calc_avg32((u32)sys_stat.max_lat_cri,
			   (u32)c->max_lat_cri);
	sys_stat.avg_lat_cri =
		(__typeof__(sys_stat.avg_lat_cri))
		calc_avg32((u32)sys_stat.avg_lat_cri,
			   (u32)c->avg_lat_cri);
	sys_stat.thr_lat_cri =
		(__typeof__(sys_stat.thr_lat_cri))
		lavd_lat_cri_thresh((s32)sys_stat.max_lat_cri,
				    (s32)sys_stat.avg_lat_cri,
				    preempt_shift);

	if (have_little_core) {
		sys_stat.min_perf_cri =
			(__typeof__(sys_stat.min_perf_cri))
			calc_avg32((u32)sys_stat.min_perf_cri,
				   (u32)c->min_perf_cri);
		sys_stat.avg_perf_cri =
			(__typeof__(sys_stat.avg_perf_cri))
			calc_avg32((u32)sys_stat.avg_perf_cri,
				   (u32)c->avg_perf_cri);
		sys_stat.max_perf_cri =
			(__typeof__(sys_stat.max_perf_cri))
			calc_avg32((u32)sys_stat.max_perf_cri,
				   (u32)c->max_perf_cri);
	}

	if (c->nr_sched > 0)
		avg_svc_time_iwgt = c->tot_task_time_iwgt / c->nr_sched;

	sys_stat.avg_svc_time_iwgt =
		calc_avg(sys_stat.avg_svc_time_iwgt, avg_svc_time_iwgt);
	sys_stat.nr_queued_task =
		calc_avg(sys_stat.nr_queued_task, c->nr_queued_task);

	if (cnt++ == LAVD_SYS_STAT_DECAY_TIMES) {
		cnt = 0;

		sys_stat.nr_sched >>= 1;
		sys_stat.nr_preempt >>= 1;
		sys_stat.nr_perf_cri >>= 1;
		sys_stat.nr_lat_cri >>= 1;
		sys_stat.nr_x_migration >>= 1;
		sys_stat.nr_big >>= 1;
		sys_stat.nr_pc_on_big >>= 1;
		sys_stat.nr_lc_on_big >>= 1;

		lavd_decay_mode_time(&performance_mode_ns);
		lavd_decay_mode_time(&balanced_mode_ns);
		lavd_decay_mode_time(&powersave_mode_ns);
	}

	sys_stat.nr_sched += c->nr_sched;
	sys_stat.nr_preempt += c->nr_preempt;
	sys_stat.nr_perf_cri += c->nr_perf_cri;
	sys_stat.nr_lat_cri += c->nr_lat_cri;
	sys_stat.nr_x_migration += c->nr_x_migration;
	sys_stat.nr_big += c->nr_big;
	sys_stat.nr_pc_on_big += c->nr_pc_on_big;
	sys_stat.nr_lc_on_big += c->nr_lc_on_big;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		struct cpdom_ctx *cpdomc;

		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc)
			continue;

		if (cpdomc->nr_turb_cpus == 0 || cpdomc->cap_sum_turb == 0) {
			cpdomc->vuln_thresh = 0;
		} else if (cpdomc->cap_sum_steady > 0) {
			u64 load_high =
				((u64)cpdomc->util_sum_steady << LAVD_SHIFT) /
				cpdomc->cap_sum_steady;
			u64 load_low =
				((u64)cpdomc->util_sum_turb << LAVD_SHIFT) /
				cpdomc->cap_sum_turb;
			u64 target =
				(load_high * lat_load_target_pct) / 100;

			if (load_low > target && cpdomc->vuln_thresh > 0)
				cpdomc->vuln_thresh--;
			else if (load_low < target &&
				 cpdomc->vuln_thresh < LAVD_VULN_THRESH_MAX)
				cpdomc->vuln_thresh++;
		}
	}

	update_power_mode_time();
}

static void calc_sys_time_slice(void)
{
	u64 nr_q = READ_ONCE(sys_stat.nr_queued_task);
	u64 nr_active = READ_ONCE(sys_stat.nr_active);
	u64 slice_wall;

	if (!nr_active)
		nr_active = 1;

	if (nr_q > 0) {
		slice_wall = (LAVD_TARGETED_LATENCY_NS * nr_active) / nr_q;
		slice_wall = clamp(slice_wall, slice_min_ns, slice_max_ns);
	} else {
		slice_wall = slice_max_ns;
	}

	sys_stat.slice_wall = calc_avg(sys_stat.slice_wall, slice_wall);
}

static int do_update_sys_stat(void)
{
	init_sys_stat_ctx();
	collect_sys_stat();
	calc_sys_stat();

	return 0;
}

__weak
int update_sys_stat(void)
{
	do_update_sys_stat();

	if (is_autopilot_on)
		do_autopilot();

	if (!no_core_compaction)
		do_core_compaction();

	if (reinit_cpumask_for_performance) {
		reinit_cpumask_for_performance = false;
		reinit_active_cpumask_for_performance();
	}

	calc_sys_time_slice();
	update_thr_perf_cri();

	if (nr_cpdoms > 1)
		plan_x_cpdom_migration();

	return 0;
}

static int update_timer_cb(void *map, int *key, struct bpf_timer *timer)
{
	int err;

	(void)map;
	(void)key;

	update_sys_stat();

	err = bpf_timer_start(timer, LAVD_SYS_STAT_INTERVAL_NS, 0);
	if (err)
		scx_bpf_error("Failed to arm update timer");

	return 0;
}

__weak
s32 init_sys_stat(u64 now)
{
	struct cpdom_ctx *cpdomc;
	struct bpf_timer *timer;
	u64 cpdom_id;
	u32 key = 0;
	int err;

	sys_stat.last_update_clk = now;
	sys_stat.nr_active = nr_cpus_onln ? nr_cpus_onln : 1;
	sys_stat.nr_active_cpdoms = 0;
	sys_stat.slice_wall = slice_max_ns;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (cpdomc && cpdomc->nr_active_cpus)
			sys_stat.nr_active_cpdoms++;
	}

	timer = bpf_map_lookup_elem(&update_timer, &key);
	if (!timer) {
		scx_bpf_error("Failed to lookup update timer");
		return -ESRCH;
	}

	bpf_timer_init(timer, &update_timer, CLOCK_BOOTTIME);
	bpf_timer_set_callback(timer, update_timer_cb);

	err = bpf_timer_start(timer, LAVD_SYS_STAT_INTERVAL_NS, 0);
	if (err) {
		scx_bpf_error("Failed to arm update timer");
		return err;
	}

	return 0;
}
