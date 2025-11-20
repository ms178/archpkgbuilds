/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: Latency-criticality Aware Virtual Deadline (LAVD) scheduler
 *
 * Optimized Header for Intel Raptor Lake (i7-14700KF)
 * - 64-byte Cache Line Alignment
 * - False Sharing Mitigation via struct padding
 * - C11 Atomics for Memory Model correctness
 * - Upstream patch integration (task_ctx migration)
 */
#ifndef __LAVD_H
#define __LAVD_H

#include <scx/common.bpf.h>
#include <scx/bpf_arena_common.bpf.h>
#include <lib/sdt_task.h>
#include <lib/atq.h>

/*
 * Common Macros & Constants
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

#define cpdom_to_dsq(cpdom_id)		((cpdom_id) | LAVD_DSQ_TYPE_CPDOM << LAVD_DSQ_TYPE_SHFT)
#define dsq_to_cpdom(dsq_id)		((dsq_id) & LAVD_DSQ_ID_MASK)
#define dsq_to_cpu(dsq_id)		((dsq_id) & LAVD_DSQ_ID_MASK)
#define dsq_type(dsq_id)		(((dsq_id) & LAVD_DSQ_TYPE_MASK) >> LAVD_DSQ_TYPE_SHFT)

enum {
	LAVD_DSQ_TYPE_SHFT		= 12,
	LAVD_DSQ_TYPE_MASK		= 0x3 << LAVD_DSQ_TYPE_SHFT,
	LAVD_DSQ_ID_SHFT		= 0,
	LAVD_DSQ_ID_MASK		= 0xfff << LAVD_DSQ_ID_SHFT,
	LAVD_DSQ_NR_TYPES		= 2,
	LAVD_DSQ_TYPE_CPDOM		= 1,
	LAVD_DSQ_TYPE_CPU		= 0,
};

enum consts_internal {
	CLOCK_BOOTTIME			= 7,
	CACHELINE_SIZE			= 64,

	NSEC_PER_USEC			= 1000ULL,
	NSEC_PER_MSEC			= (1000ULL * NSEC_PER_USEC),

	LAVD_TIME_ONE_SEC		= (1000ULL * NSEC_PER_MSEC),
	LAVD_MAX_RETRY			= 3,

	LAVD_TARGETED_LATENCY_NS	= (10ULL * NSEC_PER_MSEC),
	LAVD_SLICE_MIN_NS_DFL		= (500ULL * NSEC_PER_USEC),
	LAVD_SLICE_MAX_NS_DFL		= (5ULL * NSEC_PER_MSEC),
	LAVD_SLICE_BOOST_BONUS		= LAVD_SLICE_MIN_NS_DFL,
	LAVD_SLICE_BOOST_MAX		= (500ULL * NSEC_PER_MSEC),
	LAVD_ACC_RUNTIME_MAX		= LAVD_SLICE_MAX_NS_DFL,
	LAVD_DL_COMPETE_WINDOW		= (LAVD_SLICE_MAX_NS_DFL >> 16),

	LAVD_LC_FREQ_MAX                = 400000,
	LAVD_LC_RUNTIME_MAX		= LAVD_TIME_ONE_SEC,
	LAVD_LC_WEIGHT_BOOST		= 128,
	LAVD_LC_GREEDY_SHIFT		= 3,
	LAVD_LC_WAKE_INTERVAL_MIN	= LAVD_SLICE_MIN_NS_DFL,
	LAVD_LC_INH_WAKEE_SHIFT		= 2,
	LAVD_LC_INH_WAKER_SHIFT		= 3,

	LAVD_CPU_UTIL_MAX_FOR_CPUPERF	= p2s(85),

	LAVD_SYS_STAT_INTERVAL_NS	= (2 * LAVD_SLICE_MAX_NS_DFL),
	LAVD_SYS_STAT_DECAY_TIMES	= ((2ULL * LAVD_TIME_ONE_SEC) / LAVD_SYS_STAT_INTERVAL_NS),

	LAVD_CC_PER_CORE_SHIFT		= 1,
	LAVD_CC_UTIL_SPIKE		= p2s(90),
	LAVD_CC_CPU_PIN_INTERVAL	= (250ULL * NSEC_PER_MSEC),
	LAVD_CC_CPU_PIN_INTERVAL_DIV	= (LAVD_CC_CPU_PIN_INTERVAL / LAVD_SYS_STAT_INTERVAL_NS),

	LAVD_AP_HIGH_UTIL_DFL_SMT_RT	= p2s(25),
	LAVD_AP_HIGH_UTIL_DFL_NO_SMT_RT	= p2s(50),

	LAVD_CPDOM_MIG_SHIFT_UL		= 2,
	LAVD_CPDOM_MIG_SHIFT		= 3,
	LAVD_CPDOM_MIG_SHIFT_OL		= 4,
	LAVD_CPDOM_MIG_PROB_FT		= (LAVD_SYS_STAT_INTERVAL_NS / LAVD_SLICE_MAX_NS_DFL),

	LAVD_FUTEX_OP_INVALID		= -1,
};

enum consts_flags {
	LAVD_FLAG_FUTEX_BOOST		= (0x1 << 0),
	LAVD_FLAG_NEED_LOCK_BOOST	= (0x1 << 1),
	LAVD_FLAG_IS_GREEDY		= (0x1 << 2),
	LAVD_FLAG_IS_AFFINITIZED	= (0x1 << 3),
	LAVD_FLAG_IS_WAKEUP		= (0x1 << 4),
	LAVD_FLAG_IS_SYNC_WAKEUP	= (0x1 << 5),
	LAVD_FLAG_ON_BIG		= (0x1 << 6),
	LAVD_FLAG_ON_LITTLE		= (0x1 << 7),
	LAVD_FLAG_SLICE_BOOST		= (0x1 << 8),
	LAVD_FLAG_IDLE_CPU_PICKED	= (0x1 << 9),
	LAVD_FLAG_PINNED_COUNTED	= (0x1 << 10),
};

/*
 * Compute Domain Context
 * Aligned to 64B to prevent false sharing during load balancing scans.
 */
struct cpdom_ctx {
	/* Hot path: Accessed during enqueue/select_cpu */
	u64	id;
	u64	alt_id;
	u32	sc_load;
	u32	nr_queued_task;
	u32	cur_util_sum;
	u32	avg_util_sum;
	u32	cap_sum_active_cpus;
	u32	dsq_consume_lat;
	u16	nr_cpus;
	u16	nr_active_cpus;
	u8	is_big;
	u8	is_valid;
	u8	is_stealer;
	u8	is_stealee;
	u8	numa_id;
	u8	llc_id;

	/* Warm/Cold path */
	u32	cap_sum_temp;
	u16	nr_acpus_temp;
	u8	nr_neighbors[LAVD_CPDOM_MAX_DIST];

	/* Pad to next cacheline for neighbor_ids array start */
	u8	__pad0[2];

	/* Read-mostly topology data */
	u8	neighbor_ids[LAVD_CPDOM_MAX_DIST * LAVD_CPDOM_MAX_NR];
	u64	__cpumask[LAVD_CPU_ID_MAX/64] __attribute__((aligned(CACHELINE_SIZE)));
} __attribute__((aligned(CACHELINE_SIZE)));

#define get_neighbor_id(cpdomc, d, i) ((cpdomc)->neighbor_ids[((d) * LAVD_CPDOM_MAX_NR) + (i)])

extern struct cpdom_ctx		cpdom_ctxs[LAVD_CPDOM_MAX_NR];
extern struct bpf_cpumask	cpdom_cpumask[LAVD_CPDOM_MAX_NR];
extern int			nr_cpdoms;

/*
 * Task Context
 * Upstreamed from intf.h to lavd.bpf.h
 * Reordered for packing efficiency.
 */
struct task_ctx {
	/* MUST be first: Upstream requirement for SCX */
	struct scx_task_common atq;

	/* Hot Path: State & Flags (accessed in enqueue/dispatch) */
	volatile u64	flags;
	u64		slice;
	u32		lat_cri;
	u32		perf_cri;
	u32		cpu_id;
	u32		prev_cpu_id;
	u32		cpdom_id;
	u32		suggested_cpu_id;
	s32		pinned_cpu_id;

	/* Warm Path: Statistics & Runtime accounting */
	u64		last_runnable_clk;
	u64		last_running_clk;
	u64		last_stopping_clk;
	u64		last_measured_clk;
	u64		last_quiescent_clk;
	u64		acc_runtime;
	u64		avg_runtime;
	u64		svc_time;
	u64		run_freq;
	u64		wait_freq;
	u64		wake_freq;

	/* Cold Path: Introspection & ID */
	u64		cgrp_id;
	u64		resched_interval;
	u64		last_slice_used;
	pid_t		pid;
	pid_t		waker_pid;
	u32		lat_cri_waker;
	char		waker_comm[TASK_COMM_LEN + 1];
} __attribute__((aligned(CACHELINE_SIZE)));

typedef struct task_ctx __arena task_ctx;

u64 get_task_ctx_internal(struct task_struct *p);
#define get_task_ctx(p) ((task_ctx *)get_task_ctx_internal((p)))

struct cpu_ctx *get_cpu_ctx(void);
struct cpu_ctx *get_cpu_ctx_id(s32 cpu_id);
struct cpu_ctx *get_cpu_ctx_task(const struct task_struct *p);

/*
 * CPU Context
 * Optimized for Raptor Lake (64B cache lines).
 * Separates per-cpu Write-Heavy fields from Read-Mostly/Global fields.
 */
struct cpu_ctx {
	/* CACHE LINE 0 (0-63): Nuclear Hot (Per-CPU execution state) */
	volatile u64	running_clk;
	volatile u64	est_stopping_clk;
	volatile u64	flags;
	volatile u32	nr_pinned_tasks;
	volatile u16	lat_cri;
	u16		cpu_id;
	u16		capacity;
	u8		is_online;
	u8		big_core;
	u8		turbo_core;
	u8		llc_id;
	u8		cpdom_id;
	u8		cpdom_alt_id;
	u8		cpdom_poll_pos;
	u8		__pad0[3];
	volatile s32	futex_op;
	u8		__pad1[16];

	/* CACHE LINE 1 (64-127): Hot (Utilization & Load Tracking) */
	volatile u64	idle_start_clk;
	volatile u64	tot_svc_time;
	volatile u64	tot_sc_time;
	volatile u64	cpu_release_clk;
	volatile u64	idle_total;
	volatile u32	cur_util;
	volatile u32	avg_util;
	volatile u32	cur_sc_util;
	volatile u32	avg_sc_util;
	u32		cpuperf_cur;
	u32		__pad2;

	/* CACHE LINE 2 (128-191): Warm/Global (Criticality Sums & Hotplug) */
	volatile u64	sum_lat_cri;
	volatile u64	sum_perf_cri;
	volatile u32	max_lat_cri;
	volatile u32	nr_sched;
	volatile u32	min_perf_cri;
	volatile u32	max_perf_cri;
	u64		online_clk;
	u64		offline_clk;
	u8		__pad3[16];

	/* CACHE LINE 3 (192-255): Cold (Stats & Pointers) */
	volatile u32	nr_preempt	__attribute__((aligned(64)));
	volatile u32	nr_x_migration;
	volatile u32	nr_perf_cri;
	volatile u32	nr_lat_cri;
	u8		__pad4[48];

	/* CACHE LINE 4+ (256+): Cpumask Pointers (Read-only mostly) */
	struct bpf_cpumask __kptr *tmp_a_mask	__attribute__((aligned(64)));
	struct bpf_cpumask __kptr *tmp_o_mask;
	struct bpf_cpumask __kptr *tmp_l_mask;
	struct bpf_cpumask __kptr *tmp_i_mask;
	struct bpf_cpumask __kptr *tmp_t_mask;
	struct bpf_cpumask __kptr *tmp_t2_mask;
	struct bpf_cpumask __kptr *tmp_t3_mask;
} __attribute__((aligned(CACHELINE_SIZE)));

extern const volatile u64	nr_llcs;
const extern volatile u32	nr_cpu_ids;
extern volatile u64		nr_cpus_onln;

extern const volatile u16	cpu_capacity[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_big[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_turbo[LAVD_CPU_ID_MAX];

/* Logging & Debugging */
extern const volatile bool	no_wake_sync;
extern const volatile bool	no_slice_boost;
extern const volatile u8	verbose;

#define debugln(fmt, ...)						\
({									\
	if (verbose > 0)						\
		bpf_printk("[%s:%d] " fmt, __func__, __LINE__,		\
					##__VA_ARGS__);			\
})

#define traceln(fmt, ...)						\
({									\
	if (verbose > 1)						\
		bpf_printk("[%s:%d] " fmt, __func__, __LINE__,		\
					##__VA_ARGS__);			\
})

/* Compiler Hints & Math */
#ifndef min
#define min(X, Y) ({				\
	__auto_type __x = (X);			\
	__auto_type __y = (Y);			\
	(__x < __y) ? __x : __y;		\
})
#endif

#ifndef max
#define max(X, Y) ({				\
	__auto_type __x = (X);			\
	__auto_type __y = (Y);			\
	(__x > __y) ? __x : __y;		\
})
#endif

#ifndef clamp
#define clamp(val, lo, hi) ({			\
	__auto_type __v = (val);		\
	__auto_type __l = (lo);			\
	__auto_type __h = (hi);			\
	(__v < __l) ? __l : ((__v > __h) ? __h : __v); \
})
#endif

#ifndef likely
#define likely(x)	__builtin_expect(!!(x), 1)
#endif

#ifndef unlikely
#define unlikely(x)	__builtin_expect(!!(x), 0)
#endif

u64 calc_avg(u64 old_val, u64 new_val);
u64 calc_asym_avg(u64 old_val, u64 new_val);

/*
 * C11 Atomic Builtins (preferred over volatile casts)
 * Ensures correct Acquire/Release semantics on x86_64.
 */
static __always_inline u32 atomic_read_u32(const volatile u32 *ptr)
{
	return __atomic_load_n(ptr, __ATOMIC_ACQUIRE);
}

static __always_inline void atomic_write_u32(volatile u32 *ptr, u32 val)
{
	__atomic_store_n(ptr, val, __ATOMIC_RELEASE);
}

static __always_inline void atomic_inc_u32(volatile u32 *ptr)
{
	__atomic_add_fetch(ptr, 1U, __ATOMIC_RELAXED);
}

static __always_inline void atomic_dec_u32(volatile u32 *ptr)
{
	__atomic_sub_fetch(ptr, 1U, __ATOMIC_RELAXED);
}

static __always_inline void atomic_add_u32(volatile u32 *ptr, u32 val)
{
	__atomic_add_fetch(ptr, val, __ATOMIC_RELAXED);
}

static __always_inline void atomic_sub_u32(volatile u32 *ptr, u32 val)
{
	__atomic_sub_fetch(ptr, val, __ATOMIC_RELAXED);
}

static __always_inline int cpumask_next_set_bit(u64 *cpumask)
{
	if (!*cpumask)
		return -ENOENT;
	int bit = __builtin_ctzll(*cpumask);
	*cpumask &= *cpumask - 1;
	return bit;
}

/* System Stats & Globals */
extern struct sys_stat		sys_stat;

s32 init_sys_stat(u64 now);
int update_sys_stat(void);

extern volatile u64		performance_mode_ns;
extern volatile u64		balanced_mode_ns;
extern volatile u64		powersave_mode_ns;

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

static __always_inline bool use_per_cpu_dsq(void)
{
	return unlikely(per_cpu_dsq || pinned_slice_ns);
}

static __always_inline bool use_cpdom_dsq(void)
{
	return likely(!per_cpu_dsq);
}

s32 nr_queued_on_cpu(struct cpu_ctx *cpuc);
u64 get_target_dsq_id(struct task_struct *p, struct cpu_ctx *cpuc);

extern struct bpf_cpumask __kptr *turbo_cpumask;
extern struct bpf_cpumask __kptr *big_cpumask;
extern struct bpf_cpumask __kptr *little_cpumask;
extern struct bpf_cpumask __kptr *active_cpumask;
extern struct bpf_cpumask __kptr *ovrflw_cpumask;

int do_core_compaction(void);
int update_thr_perf_cri(void);
int reinit_active_cpumask_for_performance(void);
bool is_perf_cri(task_ctx *taskc);

extern bool			have_little_core;
extern bool			have_turbo_core;
extern const volatile bool	is_smt_active;

extern u64			total_capacity;
extern u64			one_little_capacity;
extern u32			cur_big_core_scale;
extern u32			default_big_core_scale;

int init_autopilot_caps(void);
int update_autopilot_high_cap(void);
u64 scale_cap_freq(u64 dur, s32 cpu);

int reset_cpuperf_target(struct cpu_ctx *cpuc);
int update_cpuperf_target(struct cpu_ctx *cpuc);
u16 get_cpuperf_cap(s32 cpu);

int reset_suspended_duration(struct cpu_ctx *cpuc);
u64 get_suspended_duration_and_reset(struct cpu_ctx *cpuc);

const volatile u16 *get_cpu_order(void);
int plan_x_cpdom_migration(void);
void shrink_slice_at_tick(struct task_struct *p, struct cpu_ctx *cpuc, u64 now);
void reset_lock_futex_boost(task_ctx *taskc, struct cpu_ctx *cpuc);

u64 get_est_stopping_clk(task_ctx *taskc, u64 now);
void try_proc_introspec_cmd(struct task_struct *p, task_ctx *taskc);
void reset_cpu_preemption_info(struct cpu_ctx *cpuc, bool released);
int shrink_boosted_slice_remote(struct cpu_ctx *cpuc, u64 now);
void shrink_boosted_slice_at_tick(struct task_struct *p, struct cpu_ctx *cpuc, u64 now);
void preempt_at_tick(struct task_struct *p, struct cpu_ctx *cpuc);
void try_find_and_kick_victim_cpu(struct task_struct *p, task_ctx *taskc, s32 preferred_cpu, u64 dsq_id);

extern volatile bool is_monitored;

#endif /* __LAVD_H */
