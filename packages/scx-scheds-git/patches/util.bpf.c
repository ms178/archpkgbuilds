/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 *
 * Utility functions for scx_lavd scheduler
 * Optimized for Intel Raptor Lake hybrid architecture
 */

#include <scx/common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

/*
 * Sched related globals
 */
private(LAVD) struct bpf_cpumask __kptr *turbo_cpumask;
private(LAVD) struct bpf_cpumask __kptr *big_cpumask;
private(LAVD) struct bpf_cpumask __kptr *active_cpumask;
private(LAVD) struct bpf_cpumask __kptr *ovrflw_cpumask;

const volatile u64	nr_llcs;
const volatile u64	__nr_cpu_ids;
volatile u64		nr_cpus_onln;

const volatile u32	cpu_sibling[LAVD_CPU_ID_MAX];

/*
 * Options
 */
volatile bool		reinit_cpumask_for_performance;
volatile bool		no_preemption;
volatile bool		no_core_compaction;
volatile bool		no_freq_scaling;

const volatile bool	no_wake_sync;
const volatile bool	no_slice_boost;
const volatile bool	per_cpu_dsq;
const volatile bool	enable_cpu_bw;
const volatile bool	is_autopilot_on;
const volatile u8	verbose;

/*
 * Exit information
 */
UEI_DEFINE(uei);

/*
 * per-CPU globals
 */
struct {
	__uint(type, BPF_MAP_TYPE_PERCPU_ARRAY);
	__type(key, u32);
	__type(value, struct cpu_ctx);
	__uint(max_entries, 1);
} cpu_ctx_stor SEC(".maps");

__hidden
u64 get_task_ctx_internal(struct task_struct __arg_trusted *p)
{
	return (u64)scx_task_data(p);
}

/*
 * Get CPU context for current CPU.
 * Prefetch optimization: cpu_ctx is frequently accessed in hot scheduling
 * paths. Prefetching to L1 with high locality hint reduces access latency
 * by avoiding L2 round-trips on Raptor Lake's split L1/L2 hierarchy.
 */
__hidden
struct cpu_ctx *get_cpu_ctx(void)
{
	const u32 idx = 0;
	struct cpu_ctx *cpuc = bpf_map_lookup_elem(&cpu_ctx_stor, &idx);

	if (cpuc)
		__builtin_prefetch(cpuc, 0, 3);

	return cpuc;
}

/*
 * Get CPU context for specified CPU ID.
 * Same prefetch optimization as get_cpu_ctx() for cross-CPU lookups
 * during load balancing and task migration decisions.
 */
__hidden
struct cpu_ctx *get_cpu_ctx_id(s32 cpu_id)
{
	const u32 idx = 0;
	u32 nr;

	if (cpu_id < 0)
		return NULL;

	if ((u32)cpu_id >= LAVD_CPU_ID_MAX)
		return NULL;

	/*
	 * nr_cpu_ids is provided by the kernel BPF global and should be
	 * valid at init. Guard only if it is non-zero to avoid rejecting
	 * valid CPUs during early init.
	 */
	nr = READ_ONCE(nr_cpu_ids);
	if (nr && (u32)cpu_id >= nr)
		return NULL;

	{
		struct cpu_ctx *cpuc =
			bpf_map_lookup_percpu_elem(&cpu_ctx_stor, &idx, cpu_id);

		if (cpuc)
			__builtin_prefetch(cpuc, 0, 3);

		return cpuc;
	}
}

__hidden
struct cpu_ctx *get_cpu_ctx_task(const struct task_struct *p)
{
	return get_cpu_ctx_id(scx_bpf_task_cpu(p));
}

__hidden
u32 __attribute__((noinline)) calc_avg32(u32 old_val, u32 new_val)
{
	/*
	 * Calculate the exponential weighted moving average (EWMA).
	 *  - EWMA = (0.875 * old) + (0.125 * new)
	 */
	return __calc_avg(old_val, new_val, 3);
}

__hidden
u64 __attribute__((noinline)) calc_avg(u64 old_val, u64 new_val)
{
	/*
	 * Calculate the exponential weighted moving average (EWMA).
	 *  - EWMA = (0.875 * old) + (0.125 * new)
	 */
	return __calc_avg(old_val, new_val, 3);
}

__hidden
u64 __attribute__((noinline)) calc_asym_avg(u64 old_val, u64 new_val)
{
	/*
	 * Increase fast but decrease slowly.
	 * Useful for tracking peak values while smoothing noise.
	 */
	if (old_val < new_val)
		return __calc_avg(new_val, old_val, 2);
	else
		return __calc_avg(old_val, new_val, 3);
}

__hidden
u64 __attribute__((noinline)) calc_avg_freq(u64 old_freq, u64 interval)
{
	u64 new_freq, ewma_freq;

	/*
	 * Calculate the exponential weighted moving average (EWMA) of a
	 * frequency with a new interval measured.
	 */
	if (unlikely(interval == 0))
		interval = 1;

	new_freq = LAVD_TIME_ONE_SEC / interval;
	ewma_freq = __calc_avg(old_freq, new_freq, 3);
	return ewma_freq;
}

__hidden
bool is_kernel_task(struct task_struct *p)
{
	return !!(p->flags & PF_KTHREAD);
}

__hidden
bool is_kernel_worker(struct task_struct *p)
{
	return !!(p->flags & (PF_WQ_WORKER | PF_IO_WORKER));
}

__hidden
bool is_ksoftirqd(struct task_struct *p)
{
	return is_kernel_task(p) && !__builtin_memcmp(p->comm, "ksoftirqd/", 10);
}

__hidden
bool is_pinned(const struct task_struct *p)
{
	return p->nr_cpus_allowed == 1;
}

__hidden
bool test_task_flag(task_ctx __arg_arena *taskc, u64 flag)
{
	return (taskc->flags & flag) == flag;
}

__hidden
void set_task_flag(task_ctx __arg_arena *taskc, u64 flag)
{
	taskc->flags |= flag;
}

__hidden
void reset_task_flag(task_ctx __arg_arena *taskc, u64 flag)
{
	taskc->flags &= ~flag;
}

__hidden
inline bool test_cpu_flag(struct cpu_ctx *cpuc, u64 flag)
{
	return (cpuc->flags & flag) == flag;
}

__hidden
inline void set_cpu_flag(struct cpu_ctx *cpuc, u64 flag)
{
	cpuc->flags |= flag;
}

__hidden
inline void reset_cpu_flag(struct cpu_ctx *cpuc, u64 flag)
{
	cpuc->flags &= ~flag;
}

__hidden
bool is_lat_cri(task_ctx __arg_arena *taskc)
{
	return taskc->lat_cri >= READ_ONCE(sys_stat.avg_lat_cri);
}

__hidden
bool is_lock_holder(task_ctx __arg_arena *taskc)
{
	return test_task_flag(taskc, LAVD_FLAG_FUTEX_BOOST);
}

__hidden
bool is_lock_holder_running(struct cpu_ctx *cpuc)
{
	return test_cpu_flag(cpuc, LAVD_FLAG_FUTEX_BOOST);
}

__hidden
bool have_scheduled(task_ctx __arg_arena *taskc)
{
	/*
	 * If task's time slice hasn't been updated, that means the task has
	 * been scheduled by this scheduler.
	 */
	return taskc->slice != 0;
}

__hidden
bool can_boost_slice(void)
{
	return READ_ONCE(sys_stat.nr_queued_task) <= READ_ONCE(sys_stat.nr_active);
}

__hidden
u16 get_nice_prio(struct task_struct __arg_trusted *p)
{
	s32 prio = (s32)p->static_prio - MAX_RT_PRIO; /* [0, 40) for normal tasks */
	if (prio < 0)
		prio = 0;
	return (u16)prio;
}

__hidden
bool use_full_cpus(void)
{
	return READ_ONCE(sys_stat.nr_active) >= READ_ONCE(nr_cpus_onln);
}

__hidden
void set_on_core_type(task_ctx __arg_arena *taskc,
		      const struct cpumask *cpumask)
{
	bool on_big = false, on_little = false;
	struct cpu_ctx *cpuc;
	u32 nr = READ_ONCE(nr_cpu_ids);
	int cpu;

	if (nr == 0)
		return;

	bpf_for(cpu, 0, nr) {
		if (!bpf_cpumask_test_cpu(cpu, cpumask))
			continue;

		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to look up cpu_ctx: %d", cpu);
			return;
		}

		if (cpuc->big_core)
			on_big = true;
		else
			on_little = true;

		/* Early exit once both types found */
		if (on_big && on_little)
			break;
	}

	if (on_big)
		set_task_flag(taskc, LAVD_FLAG_ON_BIG);
	else
		reset_task_flag(taskc, LAVD_FLAG_ON_BIG);

	if (on_little)
		set_task_flag(taskc, LAVD_FLAG_ON_LITTLE);
	else
		reset_task_flag(taskc, LAVD_FLAG_ON_LITTLE);
}

__hidden
bool __attribute__((noinline)) prob_x_out_of_y(u32 x, u32 y)
{
	u32 r;

	if (y == 0)
		return false;

	if (x >= y)
		return true;

	/*
	 * Generate random number in [0, y) and check if < x.
	 * This gives probability x/y of returning true.
	 */
	r = bpf_get_prandom_u32() % y;
	return r < x;
}

/*
 * We define the primary cpu in the physical core as the lowest logical cpu id.
 * For Raptor Lake: P-cores have 2 threads (SMT), E-cores have 1 thread.
 * This function returns the primary (lower) thread ID for SMT siblings.
 */
__hidden
u32 __attribute__((noinline)) get_primary_cpu(u32 cpu)
{
	const volatile u32 *sibling;

	if (!is_smt_active)
		return cpu;

	if (cpu >= LAVD_CPU_ID_MAX || cpu >= (u32)__nr_cpu_ids)
		return cpu;

	sibling = MEMBER_VPTR(cpu_sibling, [cpu]);
	if (!sibling) {
		debugln("Infeasible CPU id: %d", cpu);
		return cpu;
	}

	return ((cpu < *sibling) ? cpu : *sibling);
}

__hidden
u32 cpu_to_dsq(u32 cpu)
{
	return (get_primary_cpu(cpu)) | LAVD_DSQ_TYPE_CPU << LAVD_DSQ_TYPE_SHFT;
}

/*
 * Check if there are any tasks queued for this CPU.
 * Uses short-circuit evaluation for efficiency - returns true immediately
 * upon finding any queued task, avoiding unnecessary DSQ queries.
 * This is optimal for the common case where we only need presence detection.
 */
__hidden
bool queued_on_cpu(struct cpu_ctx *cpuc)
{
	if (unlikely(!cpuc))
		return false;

	/*
	 * Check LOCAL_ON DSQ first - tasks explicitly pinned to this CPU
	 * via scx_bpf_dsq_insert with SCX_DSQ_LOCAL_ON flag.
	 */
	if (scx_bpf_dsq_nr_queued(SCX_DSQ_LOCAL_ON | cpuc->cpu_id))
		return true;

	if (use_per_cpu_dsq() && scx_bpf_dsq_nr_queued(cpu_to_dsq(cpuc->cpu_id)))
		return true;

	if (use_cpdom_dsq() && scx_bpf_dsq_nr_queued(cpdom_to_dsq(cpuc->cpdom_id)))
		return true;

	return false;
}

__hidden
u64 get_target_dsq_id(struct task_struct *p, struct cpu_ctx *cpuc)
{
	if (per_cpu_dsq || (READ_ONCE(pinned_slice_ns) && is_pinned(p)))
		return cpu_to_dsq(cpuc->cpu_id);
	return cpdom_to_dsq(cpuc->cpdom_id);
}

__hidden
u64 task_exec_time(struct task_struct __arg_trusted *p)
{
	return READ_ONCE(p->se.sum_exec_runtime);
}
