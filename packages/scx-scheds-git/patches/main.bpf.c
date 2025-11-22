/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: Latency-criticality Aware Virtual Deadline Scheduler
 * Optimized for Intel Raptor Lake (i7-14700KF)
 *
 * Changelog:
 * - Fixed: Speedometer 3.1 regression via Tiered Saturation.
 * - Fixed: "unused function" warning by integrating stealing logic.
 * - Fixed: "invalid DSQ ID" and API errors.
 */

#include <scx/common.bpf.h>
#include <scx/bpf_arena_common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <lib/cgroup.h>
#include <lib/sdt_task.h>
#include <lib/atq.h>

char _license[] SEC("license") = "GPL";

/* Globals */
static u64 cur_logical_clk = LAVD_DL_COMPETE_WINDOW;
static u64 cur_svc_time;

const volatile u64 slice_max_ns = LAVD_SLICE_MAX_NS_DFL;
const volatile u64 slice_min_ns = LAVD_SLICE_MIN_NS_DFL;
const volatile u64 pinned_slice_ns = 0;
const volatile u8 mig_delta_pct = 0;

static volatile u64 nr_cpus_big;
static pid_t lavd_pid;

/* Include sub-modules */
#include "util.bpf.c"
#include "idle.bpf.c"
#include "balance.bpf.c"
#include "lat_cri.bpf.c"

/*
 * Optimized Consumption Helper
 * 1. Tries local/shared DSQs.
 * 2. If empty, tries cross-domain WORK STEALING (via balance.bpf.c logic).
 * 3. Respects topology flags.
 */
static __always_inline bool consume_task_optimized(u64 cpu_dsq_id, u64 cpdom_dsq_id)
{
	/* 1. Check Local DSQ (Highest Locality) */
	if (use_per_cpu_dsq()) {
		if (scx_bpf_dsq_move_to_local(cpu_dsq_id))
			return true;
	}

	/* 2. Check Compute Domain DSQ */
	if (use_cpdom_dsq()) {
		if (scx_bpf_dsq_move_to_local(cpdom_dsq_id))
			return true;
	}

	/* 3. Work Stealing (Last Resort) */
	struct cpdom_ctx *cpdomc;
	u64 cpdom_id = dsq_to_cpdom(cpdom_dsq_id);

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
	if (!cpdomc) return false;

	/* Probabilistic Stealing */
	if (nr_cpdoms > 1 && READ_ONCE(cpdomc->is_stealer)) {
		if (try_to_steal_task(cpdomc))
			return true;
	}

	/* Force Stealing (if configured) */
	if (nr_cpdoms > 1 && mig_delta_pct == 0) {
		if (force_to_steal_task(cpdomc))
			return true;
	}

	return false;
}

static void advance_cur_logical_clk(struct task_struct *p)
{
	u64 vlc = READ_ONCE(p->scx.dsq_vtime);
	u64 clc = READ_ONCE(cur_logical_clk);
	u64 nr_queued, delta, new_clk;

	if (likely(vlc <= clc + 5000))
		return;

	nr_queued = READ_ONCE(sys_stat.nr_queued_task);
	nr_queued = nr_queued | ((nr_queued == 0) ? 1 : 0);

	if (likely(nr_queued == 1)) {
		delta = vlc - clc;
	} else if (nr_queued <= 2) {
		delta = (vlc - clc) >> 1;
	} else if (nr_queued <= 4) {
		delta = (vlc - clc) >> 2;
	} else if (nr_queued <= 8) {
		delta = (vlc - clc) >> 3;
	} else if (nr_queued <= 16) {
		delta = (vlc - clc) >> 4;
	} else {
		delta = (vlc - clc) / nr_queued;
	}

	delta = clamp(delta, 0, LAVD_SLICE_MAX_NS_DFL);
	new_clk = clc + delta;

	__sync_val_compare_and_swap(&cur_logical_clk, clc, new_clk);
}

static u64 calc_time_slice(task_ctx *taskc, struct cpu_ctx *cpuc)
{
	u64 slice, base_slice;
	u64 avg_runtime;

	if (unlikely(!taskc || !cpuc))
		return LAVD_SLICE_MAX_NS_DFL;

	base_slice = READ_ONCE(sys_stat.slice);
	avg_runtime = READ_ONCE(taskc->avg_runtime);

	if (unlikely(pinned_slice_ns && cpuc->nr_pinned_tasks)) {
		taskc->slice = min(pinned_slice_ns, base_slice);
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return taskc->slice;
	}

	if (likely(avg_runtime < base_slice)) {
		taskc->slice = base_slice;
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return base_slice;
	}

	if (!no_slice_boost) {
		if (can_boost_slice()) {
			slice = avg_runtime + LAVD_SLICE_BOOST_BONUS;
			slice = clamp(slice, slice_min_ns, LAVD_SLICE_BOOST_MAX);
			taskc->slice = slice;
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return slice;
		}

		if (unlikely(taskc->lat_cri > sys_stat.avg_lat_cri)) {
			u64 avg_lat_cri = READ_ONCE(sys_stat.avg_lat_cri);
			avg_lat_cri = avg_lat_cri | ((avg_lat_cri == 0) ? 1 : 0);
			u64 boost = (base_slice * taskc->lat_cri) / (avg_lat_cri + 1);

			slice = base_slice + boost;
			u64 cap = base_slice << 1;
			cap = (avg_runtime < cap) ? avg_runtime : cap;

			slice = clamp(slice, slice_min_ns, cap);
			taskc->slice = slice;
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return slice;
		}
	}

	taskc->slice = base_slice;
	reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
	return base_slice;
}

static void update_stat_for_running(struct task_struct *p,
									task_ctx *taskc,
									struct cpu_ctx *cpuc, u64 now)
{
	u64 wait_period, interval;
	struct cpu_ctx *prev_cpuc;

	if (have_scheduled(taskc)) {
		u64 last_quiescent = READ_ONCE(taskc->last_quiescent_clk);
		wait_period = time_delta(now, last_quiescent);
		interval = taskc->avg_runtime + wait_period;
		if (likely(interval > 0))
			taskc->run_freq = calc_avg_freq(taskc->run_freq, interval);
	}

	if (unlikely(is_monitored)) {
		u64 last_running = READ_ONCE(taskc->last_running_clk);
		taskc->resched_interval = time_delta(now, last_running);
	}

	taskc->prev_cpu_id = taskc->cpu_id;
	taskc->cpu_id = cpuc->cpu_id;

	reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP | LAVD_FLAG_IS_SYNC_WAKEUP);

	taskc->last_running_clk = now;
	taskc->last_measured_clk = now;

	reset_lock_futex_boost(taskc, cpuc);

	if (cpuc->max_lat_cri < taskc->lat_cri)
		cpuc->max_lat_cri = taskc->lat_cri;
	cpuc->sum_lat_cri += taskc->lat_cri;
	cpuc->nr_sched++;

	if (have_little_core) {
		if (cpuc->max_perf_cri < taskc->perf_cri)
			cpuc->max_perf_cri = taskc->perf_cri;
		if (cpuc->min_perf_cri > taskc->perf_cri)
			cpuc->min_perf_cri = taskc->perf_cri;
		cpuc->sum_perf_cri += taskc->perf_cri;
	}

	cpuc->flags = taskc->flags;
	cpuc->lat_cri = taskc->lat_cri;
	cpuc->running_clk = now;
	cpuc->est_stopping_clk = get_est_stopping_clk(taskc, now);

	if (is_lat_cri(taskc))
		cpuc->nr_lat_cri++;

	if (is_perf_cri(taskc))
		cpuc->nr_perf_cri++;

	prev_cpuc = get_cpu_ctx_id(taskc->prev_cpu_id);
	if (prev_cpuc && prev_cpuc->cpdom_id != cpuc->cpdom_id)
		cpuc->nr_x_migration++;

	reset_suspended_duration(cpuc);
}

static void account_task_runtime(struct task_struct *p,
								 task_ctx *taskc,
								 struct cpu_ctx *cpuc,
								 u64 now)
{
	u64 sus_dur, runtime, svc_time, sc_time;
	u64 weight;
	u64 last_measured;
	u64 tot_svc, tot_sc;

	sus_dur = get_suspended_duration_and_reset(cpuc);
	last_measured = READ_ONCE(taskc->last_measured_clk);

	if (now <= last_measured + sus_dur)
		return;

	runtime = now - last_measured - sus_dur;

	__builtin_prefetch((const void *)&cpuc->tot_svc_time, 1, 3);
	__builtin_prefetch((const void *)&cpuc->tot_sc_time, 1, 3);

	weight = READ_ONCE(p->scx.weight);
	weight = weight + (weight == 0);

	svc_time = runtime / weight;
	sc_time = scale_cap_freq(runtime, cpuc->cpu_id);

	tot_svc = READ_ONCE(cpuc->tot_svc_time);
	tot_sc = READ_ONCE(cpuc->tot_sc_time);

	WRITE_ONCE(cpuc->tot_svc_time, tot_svc + svc_time);
	WRITE_ONCE(cpuc->tot_sc_time, tot_sc + sc_time);

	taskc->acc_runtime += runtime;
	taskc->svc_time += svc_time;
	WRITE_ONCE(taskc->last_measured_clk, now);

	if (enable_cpu_bw && (p->pid != lavd_pid)) {
		struct cgroup *cgrp = bpf_cgroup_from_id(taskc->cgrp_id);
		if (cgrp) {
			scx_cgroup_bw_consume(cgrp, runtime);
			bpf_cgroup_release(cgrp);
		}
	}
}

static void update_stat_for_stopping(struct task_struct *p,
									 task_ctx *taskc,
									 struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();

	account_task_runtime(p, taskc, cpuc, now);

	taskc->avg_runtime = calc_avg(taskc->avg_runtime, taskc->acc_runtime);
	taskc->last_stopping_clk = now;

	if (unlikely(is_monitored)) {
		u64 last_running = READ_ONCE(taskc->last_running_clk);
		taskc->last_slice_used = time_delta(now, last_running);
	}

	taskc->lat_cri_waker = 0;

	if (READ_ONCE(cur_svc_time) < taskc->svc_time)
		WRITE_ONCE(cur_svc_time, taskc->svc_time);

	reset_lock_futex_boost(taskc, cpuc);
}

static void update_stat_for_refill(struct task_struct *p,
								   task_ctx *taskc,
								   struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();
	account_task_runtime(p, taskc, cpuc, now);
	taskc->avg_runtime = calc_avg(taskc->avg_runtime, taskc->acc_runtime);
}

s32 BPF_STRUCT_OPS(lavd_select_cpu, struct task_struct *p, s32 prev_cpu,
				   u64 wake_flags)
{
	struct pick_ctx ictx = {
		.p = p,
		.taskc = get_task_ctx(p),
		.prev_cpu = prev_cpu,
		.cpuc_cur = get_cpu_ctx(),
		.wake_flags = wake_flags,
	};
	bool found_idle = false;
	s32 cpu_id;

	if (unlikely(!ictx.taskc || !ictx.cpuc_cur))
		return prev_cpu;

	__builtin_prefetch(&ictx.taskc->lat_cri, 0, 3);
	__builtin_prefetch(&ictx.taskc->avg_runtime, 0, 3);

	if (prev_cpu >= 0) {
		struct cpu_ctx *cpuc_prev = get_cpu_ctx_id(prev_cpu);
		if (cpuc_prev && cpuc_prev->is_online && cpuc_prev->idle_start_clk != 0) {
			u64 now = scx_bpf_now();
			if (now - ictx.taskc->last_stopping_clk < 1500000ULL) {
				cpu_id = prev_cpu;
				found_idle = true;
				goto fast_path_idle;
			}
		}
	}

	if (wake_flags & SCX_WAKE_SYNC)
		set_task_flag(ictx.taskc, LAVD_FLAG_IS_SYNC_WAKEUP);
	else
		reset_task_flag(ictx.taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	cpu_id = pick_idle_cpu(&ictx, &found_idle);
	cpu_id = cpu_id >= 0 ? cpu_id : prev_cpu;

fast_path_idle:
	ictx.taskc->suggested_cpu_id = cpu_id;

	if (have_little_core && prev_cpu >= 0 && cpu_id >= 0) {
		u64 avg_perf = READ_ONCE(sys_stat.avg_perf_cri);
		struct cpu_ctx *cpuc_prev = get_cpu_ctx_id(prev_cpu);
		struct cpu_ctx *cpuc_new = get_cpu_ctx_id(cpu_id);

		if (cpuc_prev && cpuc_prev->is_online && cpuc_prev->big_core &&
			(!cpuc_new || !cpuc_new->big_core) &&
			(avg_perf == 0 || ictx.taskc->perf_cri >= avg_perf)) {
			cpu_id = prev_cpu;
			found_idle = false;
		}
	}

	if (found_idle) {
		struct cpu_ctx *cpuc;

		set_task_flag(ictx.taskc, LAVD_FLAG_IDLE_CPU_PICKED);

		cpuc = get_cpu_ctx_id(cpu_id);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu_id);
			goto out;
		}

		if (likely(!nr_queued_on_cpu(cpuc))) {
			p->scx.dsq_vtime = calc_when_to_run(p, ictx.taskc);
			p->scx.slice = LAVD_SLICE_MAX_NS_DFL;
			scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL, p->scx.slice, 0);
			goto out;
		}
	} else {
		reset_task_flag(ictx.taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}
out:
	return cpu_id;
}

static int cgroup_throttled(struct task_struct *p, task_ctx *taskc, bool put_aside)
{
	struct cgroup *cgrp;
	int ret, ret2;

	cgrp = bpf_cgroup_from_id(taskc->cgrp_id);
	if (!cgrp) {
		debugln("Failed to lookup a cgroup: %llu", taskc->cgrp_id);
		return -ESRCH;
	}

	ret = scx_cgroup_bw_throttled(cgrp);
	if ((ret == -EAGAIN) && put_aside) {
		ret2 = scx_cgroup_bw_put_aside(p, (u64)taskc, p->scx.dsq_vtime, cgrp);
		if (ret2) {
			bpf_cgroup_release(cgrp);
			return ret2;
		}
	}
	bpf_cgroup_release(cgrp);
	return ret;
}

void BPF_STRUCT_OPS(lavd_enqueue, struct task_struct *p, u64 enq_flags)
{
	struct cpu_ctx *cpuc, *cpuc_cur;
	s32 task_cpu, cpu = -ENOENT;
	bool is_idle = false;
	task_ctx *taskc;
	u64 dsq_id;

	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();
	if (unlikely(!taskc || !cpuc_cur)) {
		scx_bpf_error("Failed to lookup contexts in enqueue");
		return;
	}

	if (!(enq_flags & SCX_ENQ_REENQ)) {
		if (enq_flags & SCX_ENQ_WAKEUP)
			set_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
		else
			reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);

		p->scx.dsq_vtime = calc_when_to_run(p, taskc);
	}

	p->scx.slice = LAVD_SLICE_MIN_NS_DFL;

	task_cpu = scx_bpf_task_cpu(p);
	if (!__COMPAT_is_enq_cpu_selected(enq_flags)) {
		struct pick_ctx ictx = {
			.p = p,
			.taskc = taskc,
			.prev_cpu = task_cpu,
			.cpuc_cur = cpuc_cur,
			.wake_flags = 0,
		};

		cpu = pick_idle_cpu(&ictx, &is_idle);
		if (cpu < 0) {
			cpu = (task_cpu >= 0) ? task_cpu : cpuc_cur->cpu_id;
			is_idle = false;
		}
	} else {
		cpu = task_cpu;
		is_idle = test_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
		reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}

	cpuc = get_cpu_ctx_id(cpu);
	if (unlikely(!cpuc)) {
		cpu = cpuc_cur->cpu_id;
		cpuc = cpuc_cur;
	}
	taskc->suggested_cpu_id = cpu;
	taskc->cpdom_id = cpuc->cpdom_id;

	if (enable_cpu_bw && (p->pid != lavd_pid) &&
		(cgroup_throttled(p, taskc, true) == -EAGAIN)) {
		return;
	}

	if (is_pinned(p) && !test_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED)) {
		atomic_inc_u32(&cpuc->nr_pinned_tasks);
		set_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED);
	}

	if (is_idle && !nr_queued_on_cpu(cpuc)) {
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | cpu, p->scx.slice,
						   enq_flags);
	} else {
		dsq_id = get_target_dsq_id(p, cpuc);
		scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice,
								 p->scx.dsq_vtime, enq_flags);
	}

	if (is_idle) {
		scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
		return;
	}

	if (!no_preemption)
		try_find_and_kick_victim_cpu(p, taskc, cpu, cpdom_to_dsq(cpuc->cpdom_id));
}

static int enqueue_cb(struct task_struct __arg_trusted *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 dsq_id;
	s32 cpu;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("Failed to lookup a task context: %d", p->pid);
		return 0;
	}

	p->scx.dsq_vtime = calc_when_to_run(p, taskc);

	cpu = taskc->suggested_cpu_id;
	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return 0;
	}

	if (is_pinned(p) && !test_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED)) {
		atomic_inc_u32(&cpuc->nr_pinned_tasks);
		set_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED);
	}

	dsq_id = get_target_dsq_id(p, cpuc);
	scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice, p->scx.dsq_vtime, 0);

	return 0;
}

void BPF_STRUCT_OPS(lavd_dequeue, struct task_struct *p, u64 deq_flags)
{
	task_ctx *taskc;
	int ret;

	taskc = get_task_ctx(p);

	if (taskc && test_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED)) {
		s32 cpu = scx_bpf_task_cpu(p);
		struct cpu_ctx *cpuc = get_cpu_ctx_id(cpu);
		if (cpuc) {
			atomic_dec_u32(&cpuc->nr_pinned_tasks);
		}
		reset_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED);
	}

	if (!enable_cpu_bw)
		return;

	if (!taskc) {
		debugln("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}

	if ((ret = scx_cgroup_bw_cancel((u64)taskc)))
		debugln("Failed to cancel task %d with %d", p->pid, ret);
}

void BPF_STRUCT_OPS(lavd_dispatch, s32 cpu, struct task_struct *prev)
{
	struct bpf_cpumask *active, *ovrflw;
	u64 cpu_dsq_id, cpdom_dsq_id;
	task_ctx *taskc_prev = NULL;
	bool try_consume = false;
	struct task_struct *p;
	struct cpu_ctx *cpuc;
	int ret;

	cpuc = get_cpu_ctx_id(cpu);
	if (unlikely(!cpuc)) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_dsq_id = cpu_to_dsq(cpu);
	cpdom_dsq_id = cpdom_to_dsq(cpuc->cpdom_id);

	if (enable_cpu_bw && (ret = scx_cgroup_bw_reenqueue())) {
		scx_bpf_error("Failed to reenqueue backlogged tasks: %d", ret);
	}

	if (prev && (prev->scx.flags & SCX_TASK_QUEUED) &&
		is_lock_holder_running(cpuc))
		goto consume_prev;

	if (use_full_cpus())
		goto consume_out;

	bpf_rcu_read_lock();

	active = active_cpumask;
	ovrflw = ovrflw_cpumask;
	if (unlikely(!active || !ovrflw)) {
		scx_bpf_error("Failed to prepare cpumasks.");
		goto unlock_out;
	}

	if (bpf_cpumask_test_cpu(cpu, cast_mask(active)) ||
		bpf_cpumask_test_cpu(cpu, cast_mask(ovrflw))) {
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	if (use_per_cpu_dsq() && scx_bpf_dsq_nr_queued(cpu_dsq_id)) {
		bpf_cpumask_set_cpu(cpu, ovrflw);
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	if (prev) {
		if (is_pinned(prev)) {
			bpf_cpumask_set_cpu(cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		} else if (is_migration_disabled(prev)) {
			bpf_rcu_read_unlock();
			goto consume_out;
		}

		taskc_prev = get_task_ctx(prev);
		if (taskc_prev &&
			test_task_flag(taskc_prev, LAVD_FLAG_IS_AFFINITIZED) &&
			bpf_cpumask_test_cpu(cpu, prev->cpus_ptr) &&
			!bpf_cpumask_intersects(cast_mask(active), prev->cpus_ptr) &&
			!bpf_cpumask_intersects(cast_mask(ovrflw), prev->cpus_ptr)) {
			bpf_cpumask_set_cpu(cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		}
	}

	if (!use_cpdom_dsq())
		goto unlock_out;

	/* TIERED SATURATION LOGIC */
	u64 q_depth = scx_bpf_dsq_nr_queued(cpdom_dsq_id);
	u64 nr_cpus = nr_cpus_onln ? nr_cpus_onln : 1;
	bool is_saturated = q_depth > nr_cpus;
	bool is_emergency = q_depth > (nr_cpus << 1);

	if (!q_depth)
		goto unlock_out;

	bpf_for_each(scx_dsq, p, cpdom_dsq_id, 0) {
		task_ctx *taskc;
		s32 new_cpu;

		p = bpf_task_from_pid(p->pid);
		if (!p)
			break;

		if (is_pinned(p)) {
			new_cpu = scx_bpf_task_cpu(p);
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu(new_cpu, ovrflw);
				bpf_task_release(p);
				try_consume = true;
				break;
			}
			if (!bpf_cpumask_test_and_set_cpu(new_cpu, ovrflw))
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
			bpf_task_release(p);
			continue;
		} else if (is_migration_disabled(p)) {
			new_cpu = scx_bpf_task_cpu(p);
			if (new_cpu == cpu) {
				bpf_task_release(p);
				try_consume = true;
				break;
			}
			bpf_task_release(p);
			continue;
		}

		taskc = get_task_ctx(p);

		bool should_skip = true;

		if (taskc) {
			bool affinity_match = test_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
			bool mask_match = bpf_cpumask_intersects(cast_mask(active), p->cpus_ptr) ||
							  bpf_cpumask_intersects(cast_mask(ovrflw), p->cpus_ptr);

			if (affinity_match && !mask_match)
				should_skip = false;

			if (is_emergency) {
				should_skip = false;
			} else if (is_saturated) {
				if (cpuc->big_core) {
					should_skip = false;
				} else {
					if (taskc->perf_cri <= READ_ONCE(sys_stat.avg_perf_cri))
						should_skip = false;
				}
			}
		}

		if (should_skip) {
			bpf_task_release(p);
			continue;
		}

		new_cpu = find_cpu_in(p->cpus_ptr, cpuc);
		if (new_cpu >= 0) {
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu(new_cpu, ovrflw);
				bpf_task_release(p);
				try_consume = true;
				break;
			} else if (!bpf_cpumask_test_and_set_cpu(new_cpu, ovrflw))
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
		}
		bpf_task_release(p);
	}

unlock_out:
	bpf_rcu_read_unlock();

	if (!try_consume)
		return;

consume_out:
	if (consume_task_optimized(cpu_dsq_id, cpdom_dsq_id))
		return;

	if (prev && prev->scx.flags & SCX_TASK_QUEUED) {
consume_prev:
		taskc_prev = taskc_prev ?: get_task_ctx(prev);
		if (taskc_prev) {
			update_stat_for_refill(prev, taskc_prev, cpuc);

			if (enable_cpu_bw && (prev->pid != lavd_pid) &&
				(cgroup_throttled(prev, taskc_prev, false) == -EAGAIN))
				return;

			prev->scx.slice = calc_time_slice(taskc_prev, cpuc);

			if (is_lock_holder_running(cpuc))
				reset_lock_futex_boost(taskc_prev, cpuc);

			cpuc->flags = taskc_prev->flags;
			scx_bpf_dsq_insert(prev, SCX_DSQ_LOCAL, prev->scx.slice, 0);
		}
	}
}

void BPF_STRUCT_OPS(lavd_runnable, struct task_struct *p, u64 enq_flags)
{
	struct task_struct *waker;
	task_ctx *p_taskc, *waker_taskc;
	u64 now, interval;
	int i;

	p_taskc = get_task_ctx(p);
	if (!p_taskc) {
		scx_bpf_error("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}
	p_taskc->acc_runtime = 0;

	if (!(enq_flags & SCX_ENQ_WAKEUP))
		return;

	if (enq_flags & (SCX_ENQ_PREEMPT | SCX_ENQ_REENQ | SCX_ENQ_LAST))
		return;

	waker = bpf_get_current_task_btf();
	if ((p->real_parent != waker->real_parent))
		return;

	if (is_kernel_task(p) != is_kernel_task(waker))
		return;

	waker_taskc = get_task_ctx(waker);
	if (!waker_taskc)
		return;

	now = scx_bpf_now();
	interval = time_delta(now, READ_ONCE(waker_taskc->last_runnable_clk));
	if (interval >= LAVD_LC_WAKE_INTERVAL_MIN) {
		WRITE_ONCE(waker_taskc->wake_freq,
				   calc_avg_freq(waker_taskc->wake_freq, interval));
		WRITE_ONCE(waker_taskc->last_runnable_clk, now);
	}

	p_taskc->lat_cri_waker = waker_taskc->lat_cri;

	if (is_monitored) {
		p_taskc->waker_pid = waker->pid;
		for (i = 0; i < TASK_COMM_LEN && can_loop; i++)
			p_taskc->waker_comm[i] = waker->comm[i];
	}
}

void BPF_STRUCT_OPS(lavd_running, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now = scx_bpf_now();

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	if (p->scx.slice == SCX_SLICE_DFL)
		p->scx.dsq_vtime = READ_ONCE(cur_logical_clk);

	p->scx.slice = calc_time_slice(taskc, cpuc);
	advance_cur_logical_clk(p);
	update_stat_for_running(p, taskc, cpuc, now);
	update_cpuperf_target(cpuc);
	try_proc_introspec_cmd(p, taskc);
}

void BPF_STRUCT_OPS(lavd_tick, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now;

	if (__builtin_expect(!p, 0))
		return;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);

	if (__builtin_expect(!cpuc || !taskc, 0)) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();
	account_task_runtime(p, taskc, cpuc, now);

	if (enable_cpu_bw && (cgroup_throttled(p, taskc, false) == -EAGAIN)) {
		preempt_at_tick(p, cpuc);
		return;
	}

	if (cpuc->nr_pinned_tasks)
		shrink_slice_at_tick(p, cpuc, now);
}

void BPF_STRUCT_OPS(lavd_stopping, struct task_struct *p, bool runnable)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	update_stat_for_stopping(p, taskc, cpuc);
}

void BPF_STRUCT_OPS(lavd_quiescent, struct task_struct *p, u64 deq_flags)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now, interval;

	cpuc = get_cpu_ctx_task(p);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}
	cpuc->flags = 0;

	taskc = get_task_ctx(p);
	if (taskc && test_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED)) {
		atomic_dec_u32(&cpuc->nr_pinned_tasks);
		reset_task_flag(taskc, LAVD_FLAG_PINNED_COUNTED);
	}

	if (!(deq_flags & SCX_DEQ_SLEEP))
		return;

	if (!taskc) {
		scx_bpf_error("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();
	interval = time_delta(now, taskc->last_quiescent_clk);
	if (interval > 0) {
		taskc->wait_freq = calc_avg_freq(taskc->wait_freq, interval);
		taskc->last_quiescent_clk = now;
	}
}

static void cpu_ctx_init_online(struct cpu_ctx *cpuc, u32 cpu_id, u64 now)
{
	struct bpf_cpumask *cd_cpumask;

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_set_cpu(cpu_id, cd_cpumask);
	unlock_out:
	bpf_rcu_read_unlock();

	cpuc->flags = 0;
	cpuc->idle_start_clk = 0;
	cpuc->lat_cri = 0;
	cpuc->running_clk = 0;
	cpuc->est_stopping_clk = SCX_SLICE_INF;
	WRITE_ONCE(cpuc->online_clk, now);
	barrier();

	cpuc->is_online = true;
}

static void cpu_ctx_init_offline(struct cpu_ctx *cpuc, u32 cpu_id, u64 now)
{
	struct bpf_cpumask *cd_cpumask;

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_clear_cpu(cpu_id, cd_cpumask);
	unlock_out:
	bpf_rcu_read_unlock();

	cpuc->flags = 0;
	cpuc->idle_start_clk = 0;
	WRITE_ONCE(cpuc->offline_clk, now);
	cpuc->is_online = false;
	barrier();

	cpuc->lat_cri = 0;
	cpuc->running_clk = 0;
	cpuc->est_stopping_clk = SCX_SLICE_INF;
}

void BPF_STRUCT_OPS(lavd_cpu_online, s32 cpu)
{
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_online(cpuc, cpu, now);

	__sync_fetch_and_add(&nr_cpus_onln, 1);
	__sync_fetch_and_add(&total_capacity, cpuc->capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_cpu_offline, s32 cpu)
{
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_offline(cpuc, cpu, now);

	__sync_fetch_and_sub(&nr_cpus_onln, 1);
	__sync_fetch_and_sub(&total_capacity, cpuc->capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_update_idle, s32 cpu, bool idle)
{
	struct cpu_ctx *cpuc;
	u64 now = scx_bpf_now();
	u64 old_clk;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc)
		return;

	if (idle) {
		cpuc->idle_start_clk = now;
		reset_cpu_preemption_info(cpuc, false);
		return;
	}

	old_clk = cpuc->idle_start_clk;
	if (old_clk != 0 && now > old_clk) {
		if (__sync_bool_compare_and_swap(&cpuc->idle_start_clk, old_clk, 0))
			cpuc->idle_total += now - old_clk;
	}
}

void BPF_STRUCT_OPS(lavd_set_cpumask, struct task_struct *p,
					const struct cpumask *cpumask)
{
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	if (bpf_cpumask_weight(p->cpus_ptr) != nr_cpu_ids)
		set_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	set_on_core_type(taskc, cpumask);
}

void BPF_STRUCT_OPS(lavd_cpu_acquire, s32 cpu,
					struct scx_cpu_acquire_args *args)
{
	struct cpu_ctx *cpuc;
	u64 dur, scaled_dur;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	dur = time_delta(scx_bpf_now(), cpuc->cpu_release_clk);
	scaled_dur = scale_cap_freq(dur, cpu);
	cpuc->tot_sc_time += scaled_dur;

	cpuc->cpuperf_cur = scx_bpf_cpuperf_cur(cpu);
}

void BPF_STRUCT_OPS(lavd_cpu_release, s32 cpu,
					struct scx_cpu_release_args *args)
{
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}
	cpuc->flags = 0;

	reset_cpu_preemption_info(cpuc, true);
	scx_bpf_reenqueue_local();
	reset_cpuperf_target(cpuc);
	cpuc->cpu_release_clk = scx_bpf_now();
}

void BPF_STRUCT_OPS(lavd_enable, struct task_struct *p)
{
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	taskc->svc_time = READ_ONCE(cur_svc_time);
}

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init_task, struct task_struct *p,
							 struct scx_init_task_args *args)
{
	task_ctx *taskc;
	u64 now;
	int i;

	if (!p) {
		scx_bpf_error("NULL task_struct pointer received");
		return -ESRCH;
	}

	taskc = scx_task_alloc(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return -ENOMEM;
	}

	for (i = 0; i < sizeof(*taskc) && can_loop; i++)
		((char __arena *)taskc)[i] = 0;

	bpf_rcu_read_lock();
	if (bpf_cpumask_weight(p->cpus_ptr) != nr_cpu_ids)
		set_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	bpf_rcu_read_unlock();

	now = scx_bpf_now();
	taskc->last_runnable_clk = now;
	taskc->last_running_clk = now;
	taskc->last_stopping_clk = now;
	taskc->last_quiescent_clk = now;
	taskc->avg_runtime = sys_stat.slice;
	taskc->svc_time = sys_stat.avg_svc_time;
	taskc->pid = p->pid;
	taskc->cgrp_id = args->cgroup->kn->id;

	set_on_core_type(taskc, p->cpus_ptr);
	return 0;
}

s32 BPF_STRUCT_OPS(lavd_exit_task, struct task_struct *p,
				   struct scx_exit_task_args *args)
{
	scx_task_free(p);
	return 0;
}

static s32 init_cpdoms(u64 now)
{
	struct cpdom_ctx *cpdomc;
	int err;

	for (int i = 0; i < LAVD_CPDOM_MAX_NR; i++) {
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [i]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %d", i);
			return -ESRCH;
		}
		if (!cpdomc->is_valid)
			continue;

		if (use_cpdom_dsq()) {
			err = scx_bpf_create_dsq(cpdom_to_dsq(cpdomc->id),
									 cpdomc->numa_id);
			if (err) {
				scx_bpf_error("Failed to create a DSQ for cpdom %llu on NUMA node %d",
							  cpdomc->id, cpdomc->numa_id);
				return err;
			}
		}

		nr_cpdoms = i + 1;
	}

	return 0;
}

static int calloc_cpumask(struct bpf_cpumask **p_cpumask)
{
	struct bpf_cpumask *cpumask;
	cpumask = bpf_cpumask_create();
	if (!cpumask)
		return -ENOMEM;

	cpumask = bpf_kptr_xchg(p_cpumask, cpumask);
	if (cpumask)
		bpf_cpumask_release(cpumask);

	return 0;
}

static int init_cpumasks(void)
{
	const struct cpumask *online_cpumask;
	struct bpf_cpumask *active;
	int err = 0;

	bpf_rcu_read_lock();
	err = calloc_cpumask(&active_cpumask);
	active = active_cpumask;
	if (err || !active)
		goto out;

	online_cpumask = scx_bpf_get_online_cpumask();
	nr_cpus_onln = bpf_cpumask_weight(online_cpumask);
	bpf_cpumask_copy(active, online_cpumask);
	scx_bpf_put_cpumask(online_cpumask);

	err = calloc_cpumask(&ovrflw_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&turbo_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&big_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&little_cpumask);
	if (err)
		goto out;
	out:
	bpf_rcu_read_unlock();
	return err;
}

static s32 init_per_cpu_ctx(u64 now)
{
	struct cpu_ctx *cpuc;
	struct bpf_cpumask *turbo, *big, *little, *active, *ovrflw, *cd_cpumask;
	const struct cpumask *online_cpumask;
	struct cpdom_ctx *cpdomc;
	int cpu, i, j, k, err = 0;
	u64 cpdom_id;
	u32 sum_capacity = 0, big_capacity = 0;

	bpf_rcu_read_lock();
	online_cpumask = scx_bpf_get_online_cpumask();

	turbo = turbo_cpumask;
	big = big_cpumask;
	little = little_cpumask;
	active  = active_cpumask;
	ovrflw  = ovrflw_cpumask;
	if (!turbo || !big || !little || !active || !ovrflw) {
		scx_bpf_error("Failed to prepare cpumasks.");
		err = -ENOMEM;
		goto unlock_out;
	}

	one_little_capacity = LAVD_SCALE;
	bpf_for(cpu, 0, nr_cpu_ids) {
		if (cpu >= LAVD_CPU_ID_MAX)
			break;

		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			err = -ESRCH;
			goto unlock_out;
		}

		err = calloc_cpumask(&cpuc->tmp_a_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_o_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_l_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_i_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_t_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_t2_mask);
		if (err)
			goto unlock_out;

		err = calloc_cpumask(&cpuc->tmp_t3_mask);
		if (err)
			goto unlock_out;

		cpuc->cpu_id = cpu;
		cpuc->idle_start_clk = 0;
		cpuc->lat_cri = 0;
		cpuc->running_clk = 0;
		cpuc->est_stopping_clk = SCX_SLICE_INF;
		cpuc->online_clk = now;
		cpuc->offline_clk = now;
		cpuc->cpu_release_clk = now;
		cpuc->is_online = bpf_cpumask_test_cpu(cpu, online_cpumask);
		cpuc->capacity = cpu_capacity[cpu];
		cpuc->big_core = cpu_big[cpu];
		cpuc->turbo_core = cpu_turbo[cpu];
		cpuc->cpdom_poll_pos = cpu % LAVD_CPDOM_MAX_NR;
		cpuc->min_perf_cri = LAVD_SCALE;
		cpuc->futex_op = LAVD_FUTEX_OP_INVALID;

		sum_capacity += cpuc->capacity;

		if (cpuc->big_core) {
			nr_cpus_big++;
			big_capacity += cpuc->capacity;
			bpf_cpumask_set_cpu(cpu, big);
		}
		else {
			bpf_cpumask_set_cpu(cpu, little);
			have_little_core = true;
		}

		if (cpuc->turbo_core) {
			bpf_cpumask_set_cpu(cpu, turbo);
			have_turbo_core = true;
		}

		if (cpuc->capacity < one_little_capacity)
			one_little_capacity = cpuc->capacity;
	}
	default_big_core_scale = (big_capacity << LAVD_SHIFT) / sum_capacity;
	total_capacity = sum_capacity;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpdom_id]);
		if (!cpdomc || !cd_cpumask) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %llu", cpdom_id);
			err = -ESRCH;
			goto unlock_out;
		}
		if (!cpdomc->is_valid)
			continue;

		bpf_for(i, 0, LAVD_CPU_ID_MAX/64) {
			u64 cpumask = cpdomc->__cpumask[i];
			bpf_for(k, 0, 64) {
				j = cpumask_next_set_bit(&cpumask);
				if (j < 0)
					break;
				cpu = (i * 64) + j;
				cpuc = get_cpu_ctx_id(cpu);
				if (!cpuc) {
					scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
					err = -ESRCH;
					goto unlock_out;
				}
				cpuc->llc_id = cpdomc->llc_id;
				cpuc->cpdom_id = cpdomc->id;
				cpuc->cpdom_alt_id = cpdomc->alt_id;

				if (bpf_cpumask_test_cpu(cpu, online_cpumask)) {
					bpf_cpumask_set_cpu(cpu, cd_cpumask);
					cpdomc->nr_active_cpus++;
					cpdomc->cap_sum_active_cpus += cpuc->capacity;
				}
				cpdomc->nr_cpus++;
			}
		}
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			err = -ESRCH;
			goto unlock_out;
		}
		debugln("cpu[%d] capacity: %d, big_core: %d, turbo_core: %d, "
		"cpdom_id: %llu, alt_id: %llu",
		cpu, cpuc->capacity, cpuc->big_core, cpuc->turbo_core,
		cpuc->cpdom_id, cpuc->cpdom_alt_id);
	}

	unlock_out:
	scx_bpf_put_cpumask(online_cpumask);
	bpf_rcu_read_unlock();
	return err;
}

static int init_per_cpu_dsqs(void)
{
	struct cpdom_ctx *cpdomc;
	struct cpu_ctx *cpuc;
	int cpu, err = 0;

	bpf_for(cpu, 0, nr_cpu_ids) {
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			return -ESRCH;
		}

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %hhu", cpuc->cpdom_id);
			return -ESRCH;
		}

		if (is_smt_active && (cpu != get_primary_cpu(cpu)))
			continue;

		err = scx_bpf_create_dsq(cpu_to_dsq(cpu), cpdomc->numa_id);
		if (err) {
			scx_bpf_error("Failed to create a DSQ for cpu %d on NUMA node %d",
						  cpu, cpdomc->numa_id);
			return err;
		}
	}

	return 0;
}

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_cgroup_init, struct cgroup *cgrp,
							 struct scx_cgroup_init_args *args)
{
	int ret;

	if (!enable_cpu_bw)
		return 0;

	ret = scx_cgroup_bw_init(cgrp, args);
	if (ret)
		scx_bpf_error("Failed to init a cgroup: %d", ret);
	return ret;
}

void BPF_STRUCT_OPS(lavd_cgroup_exit, struct cgroup *cgrp)
{
	int ret;

	if (!enable_cpu_bw)
		return;

	ret = scx_cgroup_bw_exit(cgrp);
	if (ret)
		scx_bpf_error("Failed to exit a cgroup: %d", ret);
}

void BPF_STRUCT_OPS(lavd_cgroup_move, struct task_struct *p,
					struct cgroup *from, struct cgroup *to)
{
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc)
		scx_bpf_error("Failed to get a task context: %d", p->pid);
	taskc->cgrp_id = to->kn->id;
}

void BPF_STRUCT_OPS(lavd_cgroup_set_bandwidth, struct cgroup *cgrp,
					u64 period_us, u64 quota_us, u64 burst_us)
{
	int ret;

	if (!enable_cpu_bw)
		return;

	ret = scx_cgroup_bw_set(cgrp, period_us, quota_us, burst_us);
	if (ret)
		scx_bpf_error("Failed to set bandwidth of a cgroup: %d", ret);
}

int lavd_enqueue_cb(u64 ctx)
{
	task_ctx *taskc = (task_ctx *)ctx;
	struct task_struct *p;

	if (!enable_cpu_bw)
		return 0;

	if ((p = bpf_task_from_pid(taskc->pid))) {
		enqueue_cb(p);
		bpf_task_release(p);
	}
	return 0;
}
REGISTER_SCX_CGROUP_BW_ENQUEUE_CB(lavd_enqueue_cb);

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init)
{
	u64 now = scx_bpf_now();
	int err;

	err = init_cpdoms(now);
	if (err)
		return err;

	err = init_cpumasks();
	if (err)
		return err;

	err = init_per_cpu_ctx(now);
	if (err)
		return err;

	if (use_per_cpu_dsq()) {
		err = init_per_cpu_dsqs();
		if (err)
			return err;
	}

	err = init_sys_stat(now);
	if (err)
		return err;

	init_autopilot_caps();

	WRITE_ONCE(cur_logical_clk, LAVD_DL_COMPETE_WINDOW);
	WRITE_ONCE(cur_svc_time, 0);

	if (enable_cpu_bw) {
		struct scx_cgroup_bw_config bw_config = {
			.verbose = verbose > 2,
		};
		err = scx_cgroup_bw_lib_init(&bw_config);
	}

	lavd_pid = (u32)bpf_get_current_pid_tgid();

	return err;
}

void BPF_STRUCT_OPS(lavd_exit, struct scx_exit_info *ei)
{
	UEI_RECORD(uei, ei);
}

SCX_OPS_DEFINE(lavd_ops,
			   .select_cpu		= (void *)lavd_select_cpu,
			   .enqueue			= (void *)lavd_enqueue,
			   .dequeue			= (void *)lavd_dequeue,
			   .dispatch		= (void *)lavd_dispatch,
			   .runnable		= (void *)lavd_runnable,
			   .running			= (void *)lavd_running,
			   .tick			= (void *)lavd_tick,
			   .stopping		= (void *)lavd_stopping,
			   .quiescent		= (void *)lavd_quiescent,
			   .cpu_online		= (void *)lavd_cpu_online,
			   .cpu_offline		= (void *)lavd_cpu_offline,
			   .update_idle		= (void *)lavd_update_idle,
			   .set_cpumask		= (void *)lavd_set_cpumask,
			   .cpu_acquire		= (void *)lavd_cpu_acquire,
			   .cpu_release		= (void *)lavd_cpu_release,
			   .enable			= (void *)lavd_enable,
			   .init_task		= (void *)lavd_init_task,
			   .exit_task		= (void *)lavd_exit_task,
			   .cgroup_init		= (void *)lavd_cgroup_init,
			   .cgroup_exit		= (void *)lavd_cgroup_exit,
			   .cgroup_move		= (void *)lavd_cgroup_move,
			   .cgroup_set_bandwidth	= (void *)lavd_cgroup_set_bandwidth,
			   .init			= (void *)lavd_init,
			   .exit			= (void *)lavd_exit,
			   .timeout_ms		= 30000U,
			   .name			= "lavd");
