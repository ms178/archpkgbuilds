/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 *
 * Raptor-Lake-tuned cacheline-hygiene pass — see CHANGES.md for the
 * rationale behind every modification, anchored to the upstream
 * cache-aware scheduling and BORE work this revision was
 * cross-pollinated from.
 *
 * Layout-stability contract: every reorder/pad in this file is safe
 * for any consumer that accesses fields by NAME (the standard
 * BPF skeleton path: `skel->bss->sys_stat.<field>`). Out-of-tree
 * consumers that read the BSS by hardcoded offset will need to
 * regenerate against the updated BTF.
 */
#ifndef __INTF_H
#define __INTF_H

#include <limits.h>

#ifndef __VMLINUX_H__
typedef unsigned char		u8;
typedef unsigned short		u16;
typedef unsigned int		u32;
typedef unsigned long		u64;

typedef signed char		s8;
typedef signed short		s16;
typedef signed int		s32;
typedef signed long		s64;

typedef int pid_t;

enum {
	TASK_COMM_LEN = 16,
};
#endif

/*
 * Cacheline geometry.
 *
 * Raptor Lake P-cores have 64-byte L1D/L2/L3 line size, so do its
 * E-cores. Both AMD Zen 4/5 and Apple-Silicon-class arm64 also use
 * 64-byte lines. We hardcode 64 here for layout determinism; if a
 * future target uses a larger line, bump this and rebuild.
 *
 * Why a macro and not __aligned() directly? Because the same header
 * is included from BPF *and* userspace .c files — keeping the value
 * symbolic ensures both sides see exactly the same alignment.
 */
#ifndef LAVD_CACHELINE_SIZE
#define LAVD_CACHELINE_SIZE	64
#endif

#define __lavd_cacheline_aligned	__attribute__((aligned(LAVD_CACHELINE_SIZE)))

/*
 * common constants
 */
enum {
	LAVD_CPU_ID_MAX			= 512,
	LAVD_CPDOM_MAX_NR		= 128,	/* maximum number of compute domain */
	LAVD_CPDOM_MAX_DIST		= 3,	/* maximum distance from one compute domain to another */
	LAVD_PCO_STATE_MAX		= 11,	/* maximum number of performance vs. CPU order states */
	LAVD_STATUS_STR_LEN		= 4,	/* {LR: Latency-critical, Regular} {HI: performance-Hungry, performance-Insensitive} {BT: Big, liTtle} {EG: Eligible, Greedy} */
};

/*
 * System-wide stats.
 *
 * Layout is split into three contiguous groups, each padded out to a
 * full cacheline boundary so that:
 *
 *   GROUP A (hot, read-mostly):
 *     The handful of fields that every per-CPU consumer reads on every
 *     tick — `slice_wall`, `avg_lat_cri`, `avg_perf_cri`,
 *     `avg_svc_time_iwgt`, `nr_queued_task`, plus the `*_perf_cri`
 *     and `*_lat_cri` triples used by the slice/deadline calculators.
 *
 *   GROUP B (collector-internal):
 *     Fields touched only by the periodic sys_stat collector and by
 *     report/dump paths — `last_update_clk`, `avg_util_*`, the
 *     `nr_active*` and `nr_stealee` summaries.
 *
 *   GROUP C (cold, write-mostly counters):
 *     Pure aggregation counters (`nr_sched`, `nr_preempt`,
 *     `nr_perf_cri`, `nr_lat_cri`, `nr_x_migration`, `nr_*_on_big`)
 *     incremented in batch by the collector and read only at
 *     dump/report time.
 *
 * Mixing all three groups on one or two cachelines (the original
 * layout) means every collector tick invalidates the line that all
 * 24 Raptor Lake hardware threads are reading on every scheduling
 * decision — a textbook false-sharing pattern. After this split, the
 * hot group lives on its own line(s) that the collector touches at
 * most once per tunable interval, so the per-CPU readers see a
 * stable line that stays L1/L2-resident.
 *
 * The whole struct is cacheline-aligned so it does not share its
 * first line with neighbouring BSS variables.
 *
 * Layout-stability note: every original field is preserved by name;
 * the only changes are (a) field reordering within the struct and
 * (b) explicit `_pad*` members. Any consumer that accesses fields by
 * name (the standard BPF skeleton pattern) is unaffected.
 */
struct sys_stat {
	/* ----- GROUP A: hot, read-mostly (per-CPU on every tick) ----- */
	u64	slice_wall;		/* base time slice (wall clock time) */
	u64	avg_svc_time_iwgt;	/* average service time per task (weighted invariant time) */
	u64	nr_queued_task;
	u32	avg_lat_cri;		/* average latency criticality (LC) */
	u32	avg_perf_cri;		/* average performance criticality */
	u32	max_lat_cri;		/* maximum latency criticality (LC) */
	u32	thr_lat_cri;		/* latency criticality threshold for kicking */
	u32	min_perf_cri;		/* minimum performance criticality */
	u32	max_perf_cri;		/* maximum performance criticality */
	u32	thr_perf_cri;		/* performance criticality threshold */
	u8	_pad_a[LAVD_CACHELINE_SIZE -
		       ((3 * sizeof(u64) + 7 * sizeof(u32)) %
			LAVD_CACHELINE_SIZE)];

	/* ----- GROUP B: collector-internal & report-only ----- */
	u64	last_update_clk;
	u64	avg_util_wall;		/* average CPU utilization (wall clock) */
	u64	avg_util_invr;		/* scaled CPU utilization (capacity- and frequency-invariant) */
	u32	nr_stealee;		/* number of compute domains to be migrated */
	u32	nr_active;		/* number of active CPUs */
	u32	nr_active_cpdoms;	/* number of active compute domains */
	u8	_pad_b[LAVD_CACHELINE_SIZE -
		       ((3 * sizeof(u64) + 3 * sizeof(u32)) %
			LAVD_CACHELINE_SIZE)];

	/* ----- GROUP C: cold, write-mostly aggregation counters ----- */
	u64	nr_sched;		/* total scheduling so far */
	u64	nr_preempt;		/* total number of preemption operations triggered */
	u64	nr_perf_cri;		/* number of performance-critical tasks scheduled */
	u64	nr_lat_cri;		/* number of latency-critical tasks scheduled */
	u64	nr_x_migration;		/* number of cross domain migration */
	u64	nr_big;			/* scheduled on big core */
	u64	nr_pc_on_big;		/* performance-critical tasks scheduled on big core */
	u64	nr_lc_on_big;		/* latency-critical tasks scheduled on big core */
} __lavd_cacheline_aligned;

/*
 * Task information for report.
 *
 * This is a wire format consumed by the userspace report path
 * (msg_task_ctx); layout is intentionally untouched. Do not reorder.
 */
struct task_ctx_x {
	pid_t	pid;				/* pid for this task */
	char	comm[TASK_COMM_LEN + 1];
	char	stat[LAVD_STATUS_STR_LEN + 1];
	u32	cpu_id;				/* where a task is running now */
	u32	prev_cpu_id;			/* where a task ran last time */
	u32	suggested_cpu_id;		/* suggested CPU ID at ops.enqueue() and ops.select_cpu() */
	pid_t	waker_pid;			/* last waker's PID */
	char	waker_comm[TASK_COMM_LEN + 1];	/* last waker's comm */
	u64	slice_wall;			/* base time slice (wall clock time) */
	u32	lat_cri;			/* final context-aware latency criticality */
	u32	avg_lat_cri;			/* average latency criticality */
	u16	static_prio;			/* nice priority */
	u64	rerunnable_interval_wall;	/* rerunnable interval in ns: [last quiescent, last runnable] */
	u64	resched_interval_wall;		/* reschedule interval in ns: [last running, this running] */
	u64	run_freq;			/* scheduling frequency in a second */
	u64	avg_runtime_wall;		/* average runtime per schedule (wall clock time) */
	u64	wait_freq;			/* waiting frequency in a second */
	u64	wake_freq;			/* waking-up frequency in a second */
	u32	perf_cri;			/* performance criticality of a task */
	u32	thr_perf_cri;			/* performance criticality threshold */
	u32	cpuperf_cur;			/* CPU's current performance target */
	u64	cpu_util_wall;			/* cpu utilization in [0..100] */
	u64	cpu_util_invr;			/* scaled cpu utilization in [0..100] */
	u64	steal_util_wall;		/* steal utilization in [0..100] */
	u64	steal_util_invr;		/* scaled steal utilization in [0..100] */
	u64	dom_pinned_util_wall;		/* domain-pinned task utilization in [0..100] */
	u64	dom_pinned_util_invr;		/* scaled domain-pinned task utilization in [0..100] */
	u32	nr_active;			/* number of active cores */
	u64	dsq_id;				/* CPU's associated DSQ */
	u64	dsq_consume_lat;		/* DSQ's consume latency */
	u64	last_slice_used_wall;		/* time(ns) used in last scheduled interval */
	u32	lat_headroom;			/* CPU's latency headroom */
	u32	vuln_thresh;			/* preemption vulnerability threshold step */
	u32	task_util_est;			/* task's estimated utilization from ravg */
	u16	norm_lat_cri;			/* task's normalized latency criticality [0, 1024] */
};

/*
 * Introspection — wire format. Do not reorder.
 */
enum {
	LAVD_CMD_NOP		= 0x0,
	LAVD_CMD_SCHED_N	= 0x1,
};

enum {
	LAVD_MSG_TASKC = 0x1
};

struct introspec {
	volatile u64	arg;
	volatile u32	cmd;
};

struct msg_hdr {
	u32		kind;
};

struct msg_task_ctx {
	struct msg_hdr		hdr;
	struct task_ctx_x	taskc_x;
};

/*
 * BPF syscall — wire format. Do not reorder.
 */
enum {
	LAVD_PM_PERFORMANCE	= 0,
	LAVD_PM_BALANCED	= 1,
	LAVD_PM_POWERSAVE	= 2,
	LAVD_PM_MAX		= 3
};

struct power_arg {
	s32	power_mode;
};

#endif /* __INTF_H */
