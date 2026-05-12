/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */
#ifndef __LAVD_H
#define __LAVD_H

#include <scx/common.bpf.h>
#include <bpf_arena_common.bpf.h>
#include <lib/ravg.h>
#include <lib/sdt_task.h>
#include <lib/atq.h>
#include <lib/cgroup.h>

/*
 * common macros
 */
#define U64_MAX		((u64)~0ULL)
#define S64_MAX		((s64)(U64_MAX >> 1))
#define U32_MAX		((u32)~0U)
#define S32_MAX		((s32)(U32_MAX >> 1))

#define MAX_RT_PRIO	100

#define LAVD_SHIFT			10
#define LAVD_SCALE			(1L << LAVD_SHIFT)
#define p2s(percent)			(((percent) << LAVD_SHIFT) / 100)
#define s2p(scale)			(((scale) * 100) >> LAVD_SHIFT)

#define LAVD_CACHELINE_ALIGNED		__attribute__((aligned(CACHELINE_SIZE)))

#define cpdom_to_dsq(cpdom_id) \
	(((u64)(cpdom_id)) | ((u64)LAVD_DSQ_TYPE_CPDOM << LAVD_DSQ_TYPE_SHFT))
#define cpdom_to_turb_dsq(cpdom_id) \
	(((u64)(cpdom_id)) | ((u64)LAVD_DSQ_TYPE_CPDOM_TURB << LAVD_DSQ_TYPE_SHFT))
#define dsq_to_cpdom(dsq_id)		((u64)(dsq_id) & (u64)LAVD_DSQ_ID_MASK)
#define dsq_to_cpu(dsq_id)		((u64)(dsq_id) & (u64)LAVD_DSQ_ID_MASK)
#define dsq_type(dsq_id)		(((u64)(dsq_id) & (u64)LAVD_DSQ_TYPE_MASK) >> LAVD_DSQ_TYPE_SHFT)

static __always_inline u64 lavd_time_delta(u64 after, u64 before)
{
	return (after >= before) ? (after - before) : 0;
}
#define time_delta(after, before) lavd_time_delta((after), (before))

/*
 *  DSQ (dispatch queue) IDs are 64bit of the format:
 *  Lower 63 bits are reserved by users
 *
 *   Bits: [63] [62 .. 14] [13 .. 12] [11 .. 0]
 *         [ B] [    R   ] [    T   ] [   ID  ]
 *
 *    B: Sched_ext built-in ID bit, see include/linux/sched/ext.h
 *    R: Reserved
 *    T: Type of LAVD DSQ
 *   ID: DSQ ID
 */
enum {
	LAVD_DSQ_TYPE_SHFT		= 12,
	LAVD_DSQ_TYPE_MASK		= (u64)(0x3ULL << LAVD_DSQ_TYPE_SHFT),
	LAVD_DSQ_ID_SHFT		= 0,
	LAVD_DSQ_ID_MASK		= (u64)(0xfffULL << LAVD_DSQ_ID_SHFT),
	LAVD_DSQ_NR_TYPES		= 3,
	LAVD_DSQ_TYPE_CPDOM		= 1,
	LAVD_DSQ_TYPE_CPDOM_TURB	= 2,
	LAVD_DSQ_TYPE_CPU		= 0,
};

/*
 * common constants
 */
enum consts_internal {
	CLOCK_BOOTTIME			= 7,
	CACHELINE_SIZE			= 64,

	NSEC_PER_USEC			= 1000ULL,
	NSEC_PER_MSEC			= (1000ULL * NSEC_PER_USEC),

	LAVD_TIME_ONE_SEC		= (1000ULL * NSEC_PER_MSEC),
	LAVD_MAX_RETRY			= 3,

	LAVD_TARGETED_LATENCY_NS	= (10ULL * NSEC_PER_MSEC),
	LAVD_SLICE_MIN_NS_DFL		= (500ULL * NSEC_PER_USEC), /* min time slice */
	LAVD_SLICE_MAX_NS_DFL		= (5ULL * NSEC_PER_MSEC),   /* max time slice */
	LAVD_SLICE_BOOST_BONUS		= LAVD_SLICE_MIN_NS_DFL,
	LAVD_SLICE_BOOST_MAX		= (500ULL * NSEC_PER_MSEC),
	LAVD_SLICE_BOOST_UTIL_WALL	= p2s(95), /* < 95%: cpu utilization threshold for slice boost */
	LAVD_ACC_RUNTIME_MAX		= LAVD_SLICE_MAX_NS_DFL,
	LAVD_TASK_LAG_MAX		= (500ULL * NSEC_PER_MSEC),
	LAVD_DL_COMPETE_WINDOW		= ((300ULL * NSEC_PER_MSEC) >> 16),

	LAVD_LC_FREQ_MAX                = 100000, /* shortest interval: 10usec */
	LAVD_LC_RUNTIME_MAX		= LAVD_TIME_ONE_SEC,
	LAVD_LC_WEIGHT_BOOST_REGULAR	= 128, /* 2^7 */
	LAVD_LC_WEIGHT_BOOST_MEDIUM	= (2 * LAVD_LC_WEIGHT_BOOST_REGULAR),
	LAVD_LC_WEIGHT_BOOST_HIGH	= (2 * LAVD_LC_WEIGHT_BOOST_MEDIUM),
	LAVD_LC_WEIGHT_BOOST_HIGHEST	= (2 * LAVD_LC_WEIGHT_BOOST_HIGH),
	LAVD_LC_GREEDY_SHIFT		= 1, /* 50% */
	LAVD_LC_WAKE_INTERVAL_MIN	= LAVD_SLICE_MIN_NS_DFL,
	LAVD_LC_INH_RECEIVER_SHIFT	= 2, /* 25.0% of receiver's latency criticality */
	LAVD_LC_INH_GIVER_SHIFT		= 3, /* 12.5 of giver's latency criticality */
	LAVD_LC_LATENCY_SENSITIVE_THRESH = LAVD_SCALE - (LAVD_SCALE >> 3), /* top 12.5% most latency-critical tasks */
	LAVD_VULN_THRESH_STEP_SIZE	= 64, /* granularity for lat and util in threshold space */
	LAVD_VULN_THRESH_UTIL_STEPS	= LAVD_SCALE / LAVD_VULN_THRESH_STEP_SIZE, /* util sub-steps per lat level (16) */
	LAVD_VULN_THRESH_MAX		= (LAVD_SCALE / LAVD_VULN_THRESH_STEP_SIZE) * (LAVD_SCALE / LAVD_VULN_THRESH_STEP_SIZE), /* 16 lat x 16 util = 256 */
	LAVD_VULN_THRESH_INIT		= 32, /* initial threshold */

	LAVD_RAVG_HALFLIFE_NS		= (128ULL * NSEC_PER_MSEC),

	LAVD_SYS_STAT_INTERVAL_NS	= (10ULL * NSEC_PER_MSEC),
	LAVD_SYS_STAT_DECAY_TIMES	= ((2ULL * LAVD_TIME_ONE_SEC) / LAVD_SYS_STAT_INTERVAL_NS),

	LAVD_CPU_UTIL_MAX_FOR_CPUPERF	= p2s(85), /* 85.0% */
	LAVD_CPU_UTIL_THR_FOR_MAX_FREQ	= p2s(80), /* cpu utilization threshold to update max freq */

	LAVD_CC_REQ_CAPACITY_HEADROOM	= p2s(25), /* 25%: inflate required capacity by 25% to handle sudden spikes */
	LAVD_CC_PER_CPU_UTIL		= p2s(50), /* 50%: maximum per-CPU utilization */
	LAVD_CC_UTIL_SPIKE		= p2s(90), /* near-saturation guard */
	LAVD_CC_CPU_PIN_INTERVAL	= (250ULL * NSEC_PER_MSEC),
	LAVD_CC_CPU_PIN_INTERVAL_DIV	= (LAVD_CC_CPU_PIN_INTERVAL / LAVD_SYS_STAT_INTERVAL_NS),

	LAVD_AP_HIGH_UTIL_DFL_SMT_RT	= p2s(25),
	LAVD_AP_HIGH_UTIL_DFL_NO_SMT_RT	= p2s(50),

	LAVD_CPDOM_MIG_SHIFT_UL		= 2, /* under-loaded:  +/-25.0%  */
	LAVD_CPDOM_MIG_SHIFT		= 3, /* mildly loaded: +/-12.5%  */
	LAVD_CPDOM_MIG_SHIFT_OL		= 4, /* over-loaded:   +/-6.25%  */
	LAVD_CPDOM_MIG_PROB_FT		= (LAVD_SYS_STAT_INTERVAL_NS / LAVD_SLICE_MAX_NS_DFL),

	LAVD_FUTEX_OP_INVALID		= -1,
};

enum consts_flags {
	LAVD_FLAG_FUTEX_BOOST		= (0x1 << 0),  /* futex acquired or not */
	LAVD_FLAG_NEED_LOCK_BOOST	= (0x1 << 1),  /* need to boost lock for deadline calculation */
	LAVD_FLAG_IS_GREEDY		= (0x1 << 2),  /* overscheduling ratio vs nice */
	LAVD_FLAG_IS_AFFINITIZED	= (0x1 << 3),  /* pinned to subset of all CPUs */
	LAVD_FLAG_IS_WAKEUP		= (0x1 << 4),  /* wake up path */
	LAVD_FLAG_IS_SYNC_WAKEUP	= (0x1 << 5),  /* sync wake up */
	LAVD_FLAG_ON_BIG		= (0x1 << 6),  /* can run on big core */
	LAVD_FLAG_ON_LITTLE		= (0x1 << 7),  /* can run on little core */
	LAVD_FLAG_SLICE_BOOST		= (0x1 << 8),  /* slice boosted */
	LAVD_FLAG_IDLE_CPU_PICKED	= (0x1 << 9),  /* idle CPU picked in select_cpu() */
	LAVD_FLAG_KSOFTIRQD		= (0x1 << 10), /* ksoftirqd/%u */
	LAVD_FLAG_WOKEN_BY_RT_DL	= (0x1 << 11), /* woken by RT/DL task */
	LAVD_FLAG_WOKEN_BY_HARDIRQ	= (0x1 << 12), /* woken by hard IRQ */
	LAVD_FLAG_WOKEN_BY_SOFTIRQ	= (0x1 << 13), /* woken by soft IRQ */
	LAVD_FLAG_MIGRATION_AGGRESSIVE	= (0x1 << 14), /* immediate migration required */
	LAVD_FLAG_DOMAIN_PINNED		= (0x1 << 15), /* cpumask confined to one compute domain */
};

#define LAVD_MASK_MIGRATION		(LAVD_FLAG_MIGRATION_AGGRESSIVE)

/*
 * Suffix convention for time-related variables
 * --------------------------------------------
 *  - _wall: wall clock time
 *  - _invr: CPU capacity and frequency-invariant time
 *  - _wwgt: weighted wall clock time scaled by task's weight
 *  - _iwgt: weighted invariant time scaled by task's weight
 */

/*
 * Task context
 */
struct task_ctx {
	/* --- cacheline 0 boundary (0 bytes) --- */
	/*
	 * Do NOT change the position of atq. It should be at the beginning
	 * of the task_ctx.
	 */
	struct scx_task_cgroup_bw atq LAVD_CACHELINE_ALIGNED;

	/* --- cacheline 1 boundary (64 bytes): running/stopping hot --- */
	volatile u64	flags;		/* LAVD_FLAG_* */
	u64	slice_wall;		/* time slice (wall clock time) */
	u64	last_measured_wall_clk;	/* last runtime measurement (wall clock) */
	u64	last_measured_pelt_clk;	/* last runtime measurement (pelt clock) */
	u64	last_measured_task_clk;	/* last runtime measurement (task clock) */
	u64	acc_runtime_wall;	/* runtime from runnable to quiescent */
	u64	acc_runtime_invr;
	u64	svc_time_iwgt;		/* weighted invariant service time */

	/* --- cacheline 2 boundary (128 bytes): enqueue/wakeup hot --- */
	u16	lat_cri;		/* final context-aware latency criticality */
	u16	normalized_lat_cri;	/* lat_cri normalized to [0, 1024] scale */
	u16	lat_cri_waker;		/* waker's latency criticality */
	u16	lat_cri_wakee;		/* wakee's latency criticality */
	u16	perf_cri;		/* performance criticality */
	u32	cpdom_id;		/* chosen compute domain at enqueue */
	u32	suggested_cpu_id;	/* suggested CPU at enqueue/select_cpu */
	s32	pinned_cpu_id;		/* -ENOENT if not pinned or not runnable */
	u32	__pad0;
	u64	last_running_clk;	/* last scheduled-in time */
	u64	run_freq;		/* scheduling frequency in one second */
	u64	wait_freq;		/* waiting frequency in one second */
	u64	wake_freq;		/* wake frequency in one second */
	u64	avg_runtime_invr;	/* avg invariant runtime per schedule */

	/* --- cacheline 3 boundary (192 bytes): lower-frequency / monitoring --- */
	u64	avg_runtime_wall;	/* avg wall runtime per schedule */
	u64	last_runnable_clk;	/* last runnable time */
	u64	last_quiescent_clk;	/* last sleep time */
	u64	cgrp_id;		/* cgroup id */
	u64	resched_interval_wall;	/* [last running, this running] */
	u64	last_slice_used_wall;	/* [last running, last stopping] */
	u32	cpu_id;			/* current CPU */
	u32	prev_cpu_id;		/* previous CPU */
	u8	queued_in_cpdom_id;	/* cpdom where load was counted */
	u32	queued_load_snapshot;	/* task_load_metric() snapshot at enqueue */
	pid_t	pid;			/* pid */
	pid_t	waker_pid;		/* last waker pid */

	/* --- cacheline 4 boundary (256 bytes) --- */
	u32	util_est;		/* estimated task util from ravg duty cycle */
	struct ravg_data avg_util_ravg;	/* running avg of task utilization */
	char	waker_comm[TASK_COMM_LEN + 1];
} LAVD_CACHELINE_ALIGNED;

/*
 * Compute domain context
 * - system > numa node > llc domain > compute domain per core type (P or E)
 */
struct cpdom_ctx {
	/* --- cacheline 0 boundary (0 bytes): read-only --- */
	u64	id;				    /* compute domain id */
	u64	alt_id;				    /* closest alternative type domain id */
	u8	numa_id;			    /* numa domain id */
	u8	llc_id;				    /* llc domain id */
	u8	is_big;				    /* big vs little domain */
	u8	is_valid;			    /* valid domain */
	u8	nr_neighbors[LAVD_CPDOM_MAX_DIST];  /* neighbors per distance */
	u64	__cpumask[LAVD_CPU_ID_MAX/64];	    /* cpumask of this domain */
	u8	neighbor_ids[LAVD_CPDOM_MAX_DIST * LAVD_CPDOM_MAX_NR]; /* neighbor IDs per distance */

	/* --- cacheline 8 boundary (512 bytes): read-write, read-mostly --- */
	u8	is_stealer LAVD_CACHELINE_ALIGNED; /* should steal tasks */
	u8	is_stealee;			    /* can be stolen from */
	u16	nr_active_cpus;			    /* active CPUs in this domain */
	u16	nr_acpus_temp;			    /* temp for nr_active_cpus */
	u64	qload_invr;			    /* queued load (atomic updates) */
	u64	load_invr;			    /* avg_util_invr_sum + qload_invr */
	u32	nr_queued_task;			    /* queued task count */
	u32	cur_util_wall_sum;		    /* current wall util sum */
	u32	avg_util_wall_sum;		    /* avg wall util sum */
	u32	cur_util_invr_sum;		    /* current invariant util sum */
	u32	avg_util_invr_sum;		    /* avg invariant util sum */
	u32	cur_steal_util_wall_sum;	    /* current steal wall util sum */
	u32	avg_steal_util_wall_sum;	    /* avg steal wall util sum */
	u32	cur_steal_util_invr_sum;	    /* current steal invariant util sum */
	u32	avg_steal_util_invr_sum;	    /* avg steal invariant util sum */
	u32	cur_dom_pinned_util_wall_sum;	    /* current domain-pinned wall util sum */
	u32	avg_dom_pinned_util_wall_sum;	    /* avg domain-pinned wall util sum */
	u32	cur_dom_pinned_util_invr_sum;	    /* current domain-pinned invariant util sum */
	u32	avg_dom_pinned_util_invr_sum;	    /* avg domain-pinned invariant util sum */
	u32	cap_sum_active_cpus;		    /* sum capacities of active CPUs */
	u32	cap_sum_temp;			    /* temp for cap_sum_active_cpus */
	u32	dsq_consume_lat;		    /* DSQ consume latency */

	/* per-cpdom preemption vulnerability threshold tracking */
	u32	vuln_thresh;			    /* [0, LAVD_VULN_THRESH_MAX] */
	u32	util_sum_steady;			    /* util_est sum on steady CPUs */
	u32	util_sum_turb;			    /* util_est sum on turbulent CPUs */
	u32	cap_sum_steady;			    /* capacity sum steady CPUs */
	u32	cap_sum_turb;			    /* capacity sum turbulent CPUs */
	u16	nr_steady_cpus;			    /* steady CPU count */
	u16	nr_turb_cpus;			    /* turbulent CPU count */

	s64	stealee_budget_invr;		    /* egress budget per balancing round */
	s64	stealer_budget_invr;		    /* ingress budget per balancing round */
} LAVD_CACHELINE_ALIGNED;

#define get_neighbor_id(cpdomc, d, i) ((cpdomc)->neighbor_ids[((d) * LAVD_CPDOM_MAX_NR) + (i)])

/*
 * Atomically subtract @amount from the stealee's egress budget.
 */
static __always_inline void decrement_stealee_budget(struct cpdom_ctx *cpdomc, u64 amount)
{
	__sync_fetch_and_sub(&cpdomc->stealee_budget_invr, amount);

	if (unlikely(READ_ONCE(cpdomc->stealee_budget_invr) <= 0))
		WRITE_ONCE(cpdomc->is_stealee, false);
}

/*
 * Atomically subtract @amount from the stealer's ingress budget.
 */
static __always_inline void decrement_stealer_budget(struct cpdom_ctx *cpdomc, u64 amount)
{
	__sync_fetch_and_sub(&cpdomc->stealer_budget_invr, amount);

	if (unlikely(READ_ONCE(cpdomc->stealer_budget_invr) <= 0))
		WRITE_ONCE(cpdomc->is_stealer, false);
}

extern struct cpdom_ctx		cpdom_ctxs[LAVD_CPDOM_MAX_NR];
extern struct bpf_cpumask	cpdom_cpumask[LAVD_CPDOM_MAX_NR];
extern int			nr_cpdoms;

typedef struct task_ctx __arena task_ctx;

struct cpu_ctx *get_cpu_ctx(void);
struct cpu_ctx *get_cpu_ctx_id(s32 cpu_id);
struct cpu_ctx *get_cpu_ctx_task(const struct task_struct *p);

/*
 * CPU context
 */
struct cpu_ctx {
	/* --- cacheline 0 boundary (0 bytes): IPI-read + topology --- */
	volatile u64	flags;		/* cached copy of task flags */
	volatile u64	est_stopping_clk; /* estimated stopping time */
	volatile u64	running_clk;	/* when current task started running */
	volatile u16	lat_cri;	/* latency criticality */
	volatile u16	effective_capacity; /* effective CPU capacity now */
	u16		cpu_id;		/* cpu id */
	u16		max_capacity;	/* max CPU capacity */
	volatile u64	sum_lat_cri;	/* sum of latency criticality */
	volatile u32	max_lat_cri;	/* max latency criticality */
	volatile u32	nr_pinned_tasks; /* # pinned tasks waiting on this CPU */
	u8		cpdom_id;	/* compute domain id */
	u8		big_core;	/* big-core marker */
	u8		turbo_core;	/* turbo-core marker */
	u8		llc_id;		/* llc id */
	u8		cpdom_alt_id;	/* alternative compute domain id */
	u8		is_online;	/* online marker */
	u8		__pad0[2];
	u32		cpuperf_cur;	/* current performance target */
	volatile s32	futex_op;	/* futex op in futex V1 */

	/* --- cacheline 1 boundary (64 bytes): write accumulators --- */
	volatile u64	tot_task_time_wall;
	volatile u64	tot_task_time_iwgt;
	volatile u64	tot_task_time_invr;
	volatile u64	tot_dom_pinned_task_time_wall;
	volatile u64	tot_dom_pinned_task_time_invr;
	volatile u64	sum_perf_cri;
	volatile u32	min_perf_cri;
	volatile u32	max_perf_cri;
	volatile u32	nr_sched;
	volatile u32	nr_preempt;
	u64		cached_task;		/* (struct task_struct *) as u64 */
	u64		cached_taskc_raw;	/* (task_ctx __arena *) as u64 */
	u32		cached_pid;

	/* --- cacheline 2 boundary (128 bytes): per-interval results --- */
	volatile u32	nr_x_migration;
	volatile u32	nr_perf_cri;
	volatile u32	nr_lat_cri;
	volatile u32	avg_util_wall;
	volatile u32	cur_util_wall;
	volatile u32	avg_util_invr;
	volatile u32	cur_util_invr;
	volatile u32	lat_headroom;
	u32		cur_steal_util_wall;
	u32		avg_steal_util_wall;
	u32		cur_steal_util_invr;
	u32		avg_steal_util_invr;
	u32		cur_dom_pinned_util_wall;
	u32		avg_dom_pinned_util_wall;
	u32		cur_dom_pinned_util_invr;
	u32		avg_dom_pinned_util_invr;

	/* --- cacheline 3 boundary (192 bytes): sys_stat raw inputs --- */
	u64		steal_time_wall;
	u64		steal_time_invr;
	volatile u64	idle_total_wall;
	volatile u64	idle_start_clk;
	u32		avg_perf_factor;
	volatile u32	max_freq_observed;
	volatile u32	max_freq;
	u32		__pad2;
	u64		prev_task_clk;
	u64		prev_pelt_clk;

	/* --- cacheline 4 boundary (256 bytes): cpumask temps --- */
	struct bpf_cpumask __kptr *tmp_a_mask;
	struct bpf_cpumask __kptr *tmp_o_mask;
	struct bpf_cpumask __kptr *tmp_l_mask;
	struct bpf_cpumask __kptr *tmp_i_mask;
	struct bpf_cpumask __kptr *tmp_t_mask;
	struct bpf_cpumask __kptr *tmp_t2_mask;
	struct bpf_cpumask __kptr *tmp_t3_mask;

	struct ravg_data avg_irq_steal_ravg;
	struct ravg_data avg_util_ravg;
	volatile u32	util_est;
} LAVD_CACHELINE_ALIGNED;

extern const volatile u64	nr_llcs;
extern const volatile u32	nr_cpu_ids;
extern volatile u64		nr_cpus_onln;

extern const volatile u16	cpu_capacity[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_big[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_turbo[LAVD_CPU_ID_MAX];

/* Logging helpers. */

extern const volatile bool	no_wake_sync;
extern const volatile bool	no_slice_boost;
extern const volatile u8	verbose;

#define debugln(fmt, ...)						\
({									\
	if (verbose > 0)						\
		bpf_printk("[%s:%d] " fmt, __func__, __LINE__, ##__VA_ARGS__); \
})

#define traceln(fmt, ...)						\
({									\
	if (verbose > 1)						\
		bpf_printk("[%s:%d] " fmt, __func__, __LINE__, ##__VA_ARGS__); \
})

/* Arithmetic helpers. */

#ifndef min
#define min(X, Y) (((X) < (Y)) ? (X) : (Y))
#endif

#ifndef max
#define max(X, Y) (((X) < (Y)) ? (Y) : (X))
#endif

#ifndef clamp
#define clamp(val, lo, hi) min(max(val, lo), hi)
#endif

#ifndef likely
#define likely(x)	__builtin_expect(!!(x), 1)
#endif

#ifndef unlikely
#define unlikely(x)	__builtin_expect(!!(x), 0)
#endif

u64 calc_avg(u64 old_val, u64 new_val);
u64 calc_asym_avg(u64 old_val, u64 new_val);

/* Bitmask helpers. */
static __always_inline int cpumask_next_set_bit(u64 *cpumask)
{
	/*
	 * ctzll() is only well-defined for non-zero x.
	 */
	if (!*cpumask)
		return -ENOENT;

	/* Find and clear least-significant set bit. */
	int bit = ctzll(*cpumask);
	*cpumask &= *cpumask - 1;
	return bit;
}

/* System statistics module. */
extern struct sys_stat		sys_stat;

s32 init_sys_stat(u64 now);
int update_sys_stat(void);

extern volatile u64		performance_mode_ns;
extern volatile u64		balanced_mode_ns;
extern volatile u64		powersave_mode_ns;

/* Helpers from util.bpf.c for querying CPU/task state. */
extern const volatile bool	per_cpu_dsq;
extern const volatile u64	pinned_slice_ns;

extern volatile bool		reinit_cpumask_for_performance;
extern volatile bool		no_preemption;
extern volatile bool		no_core_compaction;
extern volatile bool		no_freq_scaling;

bool test_cpu_flag(struct cpu_ctx *cpuc, u64 flag);
void set_cpu_flag(struct cpu_ctx *cpuc, u64 flag);
void reset_cpu_flag(struct cpu_ctx *cpuc, u64 flag);

bool is_lock_holder(task_ctx *taskc);
bool is_lock_holder_running(struct cpu_ctx *cpuc);
bool have_scheduled(task_ctx *taskc);
bool have_pending_tasks(struct cpu_ctx *cpuc);
bool can_boost_slice(void);
bool is_lat_cri(task_ctx *taskc);
u16 get_nice_prio(struct task_struct *p);
u32 cpu_to_dsq(u32 cpu);

void set_task_flag(task_ctx *taskc, u64 flag);
void reset_task_flag(task_ctx *taskc, u64 flag);
bool test_task_flag(task_ctx *taskc, u64 flag);
bool test_task_flag_mask(task_ctx __arg_arena *taskc, u64 flag);

static __always_inline bool use_per_cpu_dsq(void)
{
	return unlikely(per_cpu_dsq || pinned_slice_ns != 0);
}

static __always_inline bool is_per_cpu_dsq_migratable(void)
{
	/*
	 * When per_cpu_dsq is on, all tasks go to per-CPU DSQ and are migratable.
	 * When only pinned_slice_ns is on, only pinned tasks go to per-CPU DSQ.
	 */
	return per_cpu_dsq;
}

static __always_inline bool use_cpdom_dsq(void)
{
	return likely(!per_cpu_dsq);
}

bool queued_on_cpu(struct cpu_ctx *cpuc);
u64 get_target_dsq_id(struct task_struct *p, struct cpu_ctx *cpuc, task_ctx *taskc);
u16 normalize_lat_cri(u16 lat_cri);

/*
 * Compute task preemption vulnerability.
 */
static __always_inline u32 preemption_vulnerability(u16 normalized_lat_cri, u32 util_est)
{
	u32 lat_step = normalized_lat_cri / LAVD_VULN_THRESH_STEP_SIZE;
	u32 util_step = util_est / LAVD_VULN_THRESH_STEP_SIZE;
	return lat_step * LAVD_VULN_THRESH_UTIL_STEPS + util_step;
}

/*
 * Return task load contribution for queued-load tracking.
 */
static __always_inline u32 task_load_metric(task_ctx *taskc)
{
	return READ_ONCE(taskc->util_est);
}

extern struct bpf_cpumask __kptr *turbo_cpumask;
extern struct bpf_cpumask __kptr *big_cpumask;
extern struct bpf_cpumask __kptr *active_cpumask;
extern struct bpf_cpumask __kptr *ovrflw_cpumask;
extern struct bpf_cpumask __kptr *steady_cpumask;

/* DSQ helpers. */

struct dsq_entry {
	u64 dsq_id;
	u64 vtime;
	bool eligible;
};

u64 peek_dsq_vtime(u64 dsq_id);
void sort_dsqs(struct dsq_entry *a, struct dsq_entry *b, struct dsq_entry *c);

/* Load balancer helpers. */

int plan_x_cpdom_migration(void);

/* Preemption management helpers. */
void shrink_slice_at_tick(struct task_struct *p, struct cpu_ctx *cpuc, u64 now);

/* Futex lock-related helpers. */

void reset_lock_futex_boost(task_ctx *taskc, struct cpu_ctx *cpuc);

/* Scheduler introspection-related helpers. */

u64 get_est_stopping_clk(task_ctx *taskc, u64 now);
void try_proc_introspec_cmd(struct task_struct *p, task_ctx *taskc);
void reset_cpu_preemption_info(struct cpu_ctx *cpuc, bool released);
int shrink_boosted_slice_remote(struct cpu_ctx *cpuc, u64 now);
void shrink_boosted_slice_at_tick(struct task_struct *p, struct cpu_ctx *cpuc, u64 now);
void preempt_at_tick(struct task_struct *p, struct cpu_ctx *cpuc);
void try_find_and_kick_victim_cpu(struct task_struct *p,
				  task_ctx *taskc,
				  s32 preferred_cpu,
				  u64 dsq_id);

extern volatile bool is_monitored;

/* Idle CPU pick helpers */

struct pick_ctx {
	/*
	 * Input arguments for pick_idle_cpu().
	 */
	const struct task_struct *p;
	task_ctx *taskc;
	u64 wake_flags;
	s32 prev_cpu;
	/*
	 * Additional input arguments for find_sticky_cpu_and_cpdom().
	 */
	s32 sync_waker_cpu;
	/*
	 * Additional output arguments for init_active_ovrflw_masks().
	 */
	struct bpf_cpumask *active;
	struct bpf_cpumask *ovrflw;
	/*
	 * Additional output arguments for init_ao_masks().
	 * Additional input arguments for find_sticky_cpu_and_cpdom().
	 */
	struct cpu_ctx *cpuc_cur;
	struct bpf_cpumask *a_mask;
	struct bpf_cpumask *o_mask;
	/*
	 * Additional input arguments for init_idle_i_mask().
	 */
	const struct cpumask *i_mask;
	/*
	 * Additional input arguments for init_idle_ato_masks().
	 * Additional input arguments for pick_idle_cpu_at_cpdom().
	 */
	struct bpf_cpumask *ia_mask;
	struct bpf_cpumask *iat_mask;
	struct bpf_cpumask *io_mask;
	struct bpf_cpumask *temp_mask;
	/*
	 * Flags.
	 */
	bool a_empty:1;
	bool o_empty:1;
	bool is_task_big:1;
	bool i_empty:1;
	bool ia_empty:1;
	bool iat_empty:1;
	bool io_empty:1;
};

s32 find_cpu_in(const struct cpumask *src_mask, struct cpu_ctx *cpuc_cur);
s32 pick_idle_cpu(struct pick_ctx *ctx, bool *is_idle);

bool consume_task(u64 cpu_dsq_id, u64 cpdom_dsq_id);

extern u64 cur_logical_clk;
u64 calc_when_to_run(struct task_struct *p, task_ctx *taskc);

#endif /* __LAVD_H */
