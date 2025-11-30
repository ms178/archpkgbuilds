/* SPDX-License-Identifier: GPL-2.0 */
/*
 * System statistics collection for LAVD scheduler.
 */

#include <scx/common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

struct sys_stat		__weak	sys_stat;
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
u64 calc_asym_avg(u64 old_val, u64 new_val);
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
	u64		duration;
	u64		duration_total;
	u64		idle_total;
	u64		compute_total;
	u64		tot_svc_time;
	u64		tot_sc_time;
	u64		tsct_spike;
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
	u32		cur_util;
	u32		cur_sc_util;
};

static void init_sys_stat_ctx(struct sys_stat_ctx *c)
{
	u64 last = READ_ONCE(sys_stat.last_update_clk);

	__builtin_memset(c, 0, sizeof(*c));

	c->min_perf_cri = LAVD_SCALE;
	c->now = scx_bpf_now();
	c->duration = time_delta(c->now, last);
	if (!c->duration)
		c->duration = 1;
	WRITE_ONCE(sys_stat.last_update_clk, c->now);
}

static void collect_sys_stat(struct sys_stat_ctx *c)
{
	struct cpdom_ctx *cpdomc;
	u64 cpdom_id, cpuc_tot_sc_time, compute;
	int cpu;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		int i, j, k;

		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (!cpdomc) {
			scx_bpf_error("cpdom_ctx %llu missing", cpdom_id);
			break;
		}

		cpdomc->cur_util_sum = 0;
		cpdomc->avg_util_sum = 0;
		cpdomc->nr_queued_task = 0;

		if (use_cpdom_dsq())
			cpdomc->nr_queued_task = scx_bpf_dsq_nr_queued(cpdom_to_dsq(cpdom_id));

		/* Upstream fix: Iterate CPUs unconditionally to count local DSQ load */
		bpf_for(i, 0, LAVD_CPU_ID_MAX / 64) {
			u64 cpumask = cpdomc->__cpumask[i];

			bpf_for(k, 0, 64) {
				j = cpumask_next_set_bit(&cpumask);
				if (j < 0)
					break;

				cpu = (i * 64) + j;
				if (cpu >= nr_cpu_ids)
					break;

				cpdomc->nr_queued_task += scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpu);

				if (use_per_cpu_dsq())
					cpdomc->nr_queued_task += scx_bpf_dsq_nr_queued(cpu_to_dsq(cpu));
			}
		}

		c->nr_queued_task += cpdomc->nr_queued_task;
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		struct cpu_ctx *cpuc = get_cpu_ctx_id(cpu);

		if (!cpuc)
			continue;

		if (cpuc->nr_pinned_tasks || !can_boost_slice() ||
		    scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpuc->cpu_id))
			shrink_boosted_slice_remote(cpuc, c->now);

		c->tot_svc_time += cpuc->tot_svc_time;
		cpuc->tot_svc_time = 0;

		cpuc_tot_sc_time = cpuc->tot_sc_time;

		if (c->duration)
			cpuc->cur_sc_util = (cpuc_tot_sc_time << LAVD_SHIFT) / c->duration;
		cpuc->avg_sc_util = calc_avg(cpuc->avg_sc_util, cpuc->cur_sc_util);

		c->tot_sc_time += cpuc_tot_sc_time;

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
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		struct cpu_ctx *cpuc = get_cpu_ctx_id(cpu);

		if (!cpuc)
			continue;

		cpuc_tot_sc_time = cpuc->tot_sc_time;

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

		for (int i = 0; i < LAVD_MAX_RETRY; i++) {
			u64 old_clk = cpuc->idle_start_clk;

			if (old_clk == 0 || time_after(old_clk, c->now))
				break;

			if (__sync_bool_compare_and_swap(&cpuc->idle_start_clk, old_clk, c->now)) {
				u64 dur = time_delta(c->now, old_clk);

				__sync_fetch_and_add(&cpuc->idle_total, dur);
				break;
			}
		}

		compute = time_delta(c->duration, cpuc->idle_total);
		if (c->duration)
			cpuc->cur_util = (compute << LAVD_SHIFT) / c->duration;
		cpuc->avg_util = calc_asym_avg(cpuc->avg_util, cpuc->cur_util);

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (cpdomc) {
			cpdomc->cur_util_sum += cpuc->cur_util;
			cpdomc->avg_util_sum += cpuc->avg_util;
		}

		c->idle_total += cpuc->idle_total;
		cpuc->idle_total = 0;

		if (cpuc->cur_util > LAVD_CC_UTIL_SPIKE)
			c->tsct_spike += cpuc_tot_sc_time;

		cpuc->tot_sc_time = 0;
	}
}

static void calc_sys_stat(struct sys_stat_ctx *c)
{
	static int cnt;
	u64 avg_svc_time = 0, cur_sc_util, scu_spike;

	c->duration_total = c->duration * (nr_cpus_onln ? nr_cpus_onln : 1);
	if (!c->duration_total)
		c->duration_total = 1;

	c->compute_total = time_delta(c->duration_total, c->idle_total);
	c->cur_util = (c->compute_total << LAVD_SHIFT) / c->duration_total;

	cur_sc_util = (c->tot_sc_time << LAVD_SHIFT) / c->duration_total;
	if (cur_sc_util > c->cur_util)
		cur_sc_util = min(sys_stat.avg_sc_util, c->cur_util);

	scu_spike = (c->tsct_spike << (LAVD_SHIFT - 1)) / c->duration_total;
	c->cur_sc_util = min(cur_sc_util + scu_spike, (u64)LAVD_SCALE);

	if (!c->nr_sched || !c->compute_total) {
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

	sys_stat.avg_util = calc_asym_avg(sys_stat.avg_util, c->cur_util);
	sys_stat.avg_sc_util = calc_asym_avg(sys_stat.avg_sc_util, c->cur_sc_util);
	sys_stat.max_lat_cri = calc_avg32(sys_stat.max_lat_cri, c->max_lat_cri);
	sys_stat.avg_lat_cri = calc_avg32(sys_stat.avg_lat_cri, c->avg_lat_cri);
	sys_stat.thr_lat_cri = sys_stat.max_lat_cri -
		((sys_stat.max_lat_cri - sys_stat.avg_lat_cri) >> preempt_shift);

	if (have_little_core) {
		sys_stat.min_perf_cri = calc_avg32(sys_stat.min_perf_cri, c->min_perf_cri);
		sys_stat.avg_perf_cri = calc_avg32(sys_stat.avg_perf_cri, c->avg_perf_cri);
		sys_stat.max_perf_cri = calc_avg32(sys_stat.max_perf_cri, c->max_perf_cri);
	}

	if (c->nr_sched)
		avg_svc_time = c->tot_svc_time / c->nr_sched;
	sys_stat.avg_svc_time = calc_avg(sys_stat.avg_svc_time, avg_svc_time);
	sys_stat.nr_queued_task = calc_avg(sys_stat.nr_queued_task, c->nr_queued_task);

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

		__sync_fetch_and_sub(&performance_mode_ns, performance_mode_ns / 2);
		__sync_fetch_and_sub(&balanced_mode_ns, balanced_mode_ns / 2);
		__sync_fetch_and_sub(&powersave_mode_ns, powersave_mode_ns / 2);
	}

	sys_stat.nr_sched += c->nr_sched;
	sys_stat.nr_preempt += c->nr_preempt;
	sys_stat.nr_perf_cri += c->nr_perf_cri;
	sys_stat.nr_lat_cri += c->nr_lat_cri;
	sys_stat.nr_x_migration += c->nr_x_migration;
	sys_stat.nr_big += c->nr_big;
	sys_stat.nr_pc_on_big += c->nr_pc_on_big;
	sys_stat.nr_lc_on_big += c->nr_lc_on_big;

	update_power_mode_time();
}

static void calc_sys_time_slice(void)
{
	u64 nr_q = sys_stat.nr_queued_task;
	u64 nr_active = sys_stat.nr_active ? sys_stat.nr_active : 1;
	u64 slice;

	if (nr_q)
		slice = (LAVD_TARGETED_LATENCY_NS * nr_active) / nr_q;
	else
		slice = slice_max_ns;

	slice = clamp(slice, slice_min_ns, slice_max_ns);
	sys_stat.slice = calc_avg(sys_stat.slice, slice);
}

static int do_update_sys_stat(void)
{
	struct sys_stat_ctx c;

	init_sys_stat_ctx(&c);
	collect_sys_stat(&c);
	calc_sys_stat(&c);

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
	sys_stat.slice = slice_max_ns;
	sys_stat.nr_active_cpdoms = 0;

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
