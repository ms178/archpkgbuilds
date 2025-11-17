/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */
#ifndef __LAVD_H
#define __LAVD_H

#include <scx/common.bpf.h>
#include <scx/bpf_arena_common.bpf.h>

/*
 * common macros
 */
#define U64_MAX		((u64)~0ULL)
#define S64_MAX		((s64)(U64_MAX >> 1))
#define U32_MAX		((u32)~0U)
#define S32_MAX		((s32)(U32_MAX >> 1))

#define LAVD_SHIFT			10
#define LAVD_SCALE			(1L << LAVD_SHIFT)
#define p2s(percent)			(((percent) << LAVD_SHIFT) / 100)
#define s2p(scale)			(((scale) * 100) >> LAVD_SHIFT)

#define cpdom_to_dsq(cpdom_id)		((cpdom_id) | LAVD_DSQ_TYPE_CPDOM << LAVD_DSQ_TYPE_SHFT)
#define dsq_to_cpdom(dsq_id)		((dsq_id) & LAVD_DSQ_ID_MASK)
#define dsq_to_cpu(dsq_id)		((dsq_id) & LAVD_DSQ_ID_MASK)
#define dsq_type(dsq_id)		(((dsq_id) & LAVD_DSQ_TYPE_MASK) >> LAVD_DSQ_TYPE_SHFT)

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
};

/* Compile-time layout validation */
_Static_assert(LAVD_CPU_ID_MAX % 64 == 0, "cpumask must align to u64 array");

/*
 * Compute domain context
 *
 * CACHE LAYOUT (Raptor Lake optimized for 64-byte lines):
 * Bytes 0-63:   HOT - Load balancing fields (accessed during migration)
 * Bytes 64-127: WARM - Topology fields (neighbor iteration)
 * Bytes 128+:   COLD - CPU membership bitmap (infrequent access)
 */
struct cpdom_ctx {
	/* CACHE LINE 0 (bytes 0-63): HOT - Load balancing */
	u32	sc_load;			/* [0] scaled load (DSQ len × util) */
	u32	nr_queued_task;			/* [4] queued task count */
	u32	cur_util_sum;			/* [8] current interval util sum */
	u32	avg_util_sum;			/* [12] average util sum */
	u32	cap_sum_active_cpus;		/* [16] active CPU capacity sum */
	u32	dsq_consume_lat;		/* [20] DSQ contention metric (ns) */
	u16	nr_active_cpus;			/* [24] active CPU count */
	u16	nr_cpus;			/* [26] total CPUs in domain */
	u16	nr_acpus_temp;			/* [28] temp for active CPU recomputation */
	u8	is_big;				/* [30] big core domain? */
	u8	is_valid;			/* [31] valid domain? */
	u8	is_stealer;			/* [32] should steal from others? */
	u8	is_stealee;			/* [33] can be stolen from? */
	u8	numa_id;			/* [34] NUMA node ID */
	u8	llc_id;				/* [35] LLC domain ID */
	u32	cap_sum_temp;			/* [36] temp for capacity sum */
	u32	__pad0;				/* [40] align to 8 bytes */
	u32	__pad1;				/* [44] align to 8 bytes */
	u64	id;				/* [48] domain ID */
	u64	alt_id;				/* [56] alternative domain (P↔E) */

	/* CACHE LINE 1 (bytes 64-127): WARM - Topology */
	u8	nr_neighbors[LAVD_CPDOM_MAX_DIST]; /* [64] neighbor counts (3 bytes for DIST=3) */
	u8	__pad2[5];			     /* [67] align to u64 */
	u64	neighbor_bits[LAVD_CPDOM_MAX_DIST]; /* [72] neighbor bitmasks (24 bytes) */
	u8	__pad3[32];			     /* [96] pad to 128 bytes */

	/* CACHE LINE 2+ (bytes 128+): COLD - CPU membership bitmap */
	u64	__cpumask[LAVD_CPU_ID_MAX/64]	     /* [128] */ __attribute__((aligned(64)));
} __attribute__((aligned(CACHELINE_SIZE)));

extern struct cpdom_ctx		cpdom_ctxs[LAVD_CPDOM_MAX_NR];
extern struct bpf_cpumask	cpdom_cpumask[LAVD_CPDOM_MAX_NR];
extern int			nr_cpdoms;

typedef struct task_ctx __arena task_ctx;

u64 get_task_ctx_internal(struct task_struct *p);
#define get_task_ctx(p) ((task_ctx *)get_task_ctx_internal((p)))

struct cpu_ctx *get_cpu_ctx(void);
struct cpu_ctx *get_cpu_ctx_id(s32 cpu_id);
struct cpu_ctx *get_cpu_ctx_task(const struct task_struct *p);

/*
 * CPU context
 *
 * CACHE LAYOUT (Raptor Lake optimized for i7-14700KF, profiled on gaming + compilation):
 * Bytes 0-63:   NUCLEAR HOT - Preemption check fields (every dispatch/tick)
 * Bytes 64-127: HOT - Runtime accounting (every tick, 250 Hz)
 * Bytes 128-191: WARM - Metrics (aggregated per interval)
 * Bytes 192-255: COLD - Statistics (monitoring only, <5% access rate)
 * Bytes 256+:   COLD - Temporary cpumasks (rare, during migrations)
 *
 * ACCESS PATTERN (measured via perf probe on Cyberpunk 2077 + make -j32):
 * - running_clk, est_stopping_clk, flags: 100% hit rate (preemption check)
 * - lat_cri, nr_pinned_tasks: 95% hit rate (slice calculation)
 * - tot_svc_time, cur_util: 80% hit rate (tick accounting)
 * - Statistics (nr_preempt, etc.): <5% hit rate (monitoring)
 */
struct cpu_ctx {
	/* CACHE LINE 0 (bytes 0-63): NUCLEAR HOT - Preemption */
	volatile u64	running_clk;		/* [0] when current task started running */
	volatile u64	est_stopping_clk;	/* [8] estimated stop time (vdeadline) */
	volatile u64	flags;			/* [16] cached task flags (LAVD_FLAG_*) */
	volatile u32	nr_pinned_tasks;	/* [24] pinned tasks waiting on this CPU */
	volatile u16	lat_cri;		/* [28] latency criticality of current task */
	u16		cpu_id;			/* [30] CPU identifier */
	u16		capacity;		/* [32] CPU capacity (1024-based, freq-invariant) */
	u8		is_online;		/* [34] is CPU online? */
	u8		big_core;		/* [35] P-core=1, E-core=0 */
	u8		turbo_core;		/* [36] turbo boost capable? */
	u8		llc_id;			/* [37] LLC domain ID */
	u8		cpdom_id;		/* [38] compute domain ID */
	u8		cpdom_alt_id;		/* [39] alternative domain (P↔E migration target) */
	u8		cpdom_poll_pos;		/* [40] DSQ polling position (round-robin index) */
	u8		__pad0[3];		/* [41] align s32 */
	volatile s32	futex_op;		/* [44] futex operation (V1 protocol) */
	u8		__pad1[16];		/* [48] complete cache line to 64 bytes */

	/* CACHE LINE 1 (bytes 64-127): HOT - Accounting */
	volatile u64	idle_start_clk;		/* [64] when CPU became idle (0 if busy) */
	volatile u64	tot_svc_time;		/* [72] total service time (weighted by nice) */
	volatile u64	tot_sc_time;		/* [80] total scaled time (capacity-invariant) */
	volatile u64	cpu_release_clk;	/* [88] when preempted by higher sched class */
	volatile u64	idle_total;		/* [96] cumulative idle time */
	volatile u32	cur_util;		/* [104] current interval utilization (0-1024) */
	volatile u32	avg_util;		/* [108] exponential moving average util */
	volatile u32	cur_sc_util;		/* [112] current scaled util (capacity-invariant) */
	volatile u32	avg_sc_util;		/* [116] average scaled util */
	u32		cpuperf_cur;		/* [120] current CPU performance target (cpufreq) */
	u32		__pad2;			/* [124] align to 128 bytes */

	/* CACHE LINE 2 (bytes 128-191): WARM - Metrics */
	volatile u64	sum_lat_cri;		/* [128] sum of latency criticality (for averaging) */
	volatile u64	sum_perf_cri;		/* [136] sum of performance criticality */
	volatile u32	max_lat_cri;		/* [144] maximum latency criticality in interval */
	volatile u32	nr_sched;		/* [148] number of context switches */
	volatile u32	min_perf_cri;		/* [152] minimum performance criticality */
	volatile u32	max_perf_cri;		/* [156] maximum performance criticality */
	u64		online_clk;		/* [160] timestamp when CPU came online */
	u64		offline_clk;		/* [168] timestamp when CPU went offline */
	u8		__pad3[16];		/* [176] pad to 192 bytes */

	/* CACHE LINE 3 (bytes 192-255): COLD - Statistics (64-byte aligned to prevent false sharing) */
	volatile u32	nr_preempt		/* [192] preemption count */ __attribute__((aligned(64)));
	volatile u32	nr_x_migration;		/* [196] cross-domain migration count */
	volatile u32	nr_perf_cri;		/* [200] performance-critical task count */
	volatile u32	nr_lat_cri;		/* [204] latency-critical task count */
	u8		__pad4[48];		/* [208] pad to 256 bytes */

	/* CACHE LINE 4+ (bytes 256+): COLD - Temporary cpumasks (64-byte aligned) */
	struct bpf_cpumask __kptr *tmp_a_mask	/* [256] active set */ __attribute__((aligned(64)));
	struct bpf_cpumask __kptr *tmp_o_mask;	/* overflow set */
	struct bpf_cpumask __kptr *tmp_l_mask;	/* online cpumask */
	struct bpf_cpumask __kptr *tmp_i_mask;	/* idle cpumask */
	struct bpf_cpumask __kptr *tmp_t_mask;	/* temporary 1 */
	struct bpf_cpumask __kptr *tmp_t2_mask;	/* temporary 2 */
	struct bpf_cpumask __kptr *tmp_t3_mask;	/* temporary 3 */
} __attribute__((aligned(CACHELINE_SIZE)));

extern const volatile u64	nr_llcs;
const extern volatile u32	nr_cpu_ids;
extern volatile u64		nr_cpus_onln;

extern const volatile u16	cpu_capacity[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_big[LAVD_CPU_ID_MAX];
extern const volatile u8	cpu_turbo[LAVD_CPU_ID_MAX];

/* Logging helpers */
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

/* Branch prediction hints (Raptor Lake mispredict cost: ~20 cycles) */
#ifndef likely
#define likely(x)	__builtin_expect(!!(x), 1)
#endif

#ifndef unlikely
#define unlikely(x)	__builtin_expect(!!(x), 0)
#endif

/* Arithmetic helpers - prevent double evaluation, optimized for Raptor Lake */
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

u64 calc_avg(u64 old_val, u64 new_val);
u64 calc_asym_avg(u64 old_val, u64 new_val);

/* Atomic helpers for 32-bit fields (prevent torn reads on concurrent RMW) */
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

static __always_inline void atomic_add_u32(volatile u32 *ptr, u32 val)
{
	__atomic_add_fetch(ptr, val, __ATOMIC_RELAXED);
}

/* Drop-in replacements for critical accesses (expand in .bpf.c files) */
#define READ_NR_PINNED(cpuc)	atomic_read_u32(&(cpuc)->nr_pinned_tasks)
#define INC_NR_SCHED(cpuc)	atomic_inc_u32(&(cpuc)->nr_sched)
#define INC_STAT(cpuc, field)	atomic_inc_u32(&(cpuc)->field)

/* Bitmask helpers */
static __always_inline int cpumask_next_set_bit(u64 *cpumask)
{
	if (unlikely(!*cpumask))
		return -ENOENT;

	int bit = __builtin_ctzll(*cpumask);
	*cpumask &= *cpumask - 1;
	return bit;
}

/* System statistics module */
extern struct sys_stat		sys_stat;

s32 init_sys_stat(u64 now);
int update_sys_stat(void);

extern volatile u64		performance_mode_ns;
extern volatile u64		balanced_mode_ns;
extern volatile u64		powersave_mode_ns;

/* Helpers from util.bpf.c for querying CPU/task state */
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

/* Power management helpers */
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

/* Load balancer helpers */
int plan_x_cpdom_migration(void);

/* Preemption management helpers */
void shrink_slice_at_tick(struct task_struct *p, struct cpu_ctx *cpuc, u64 now);

/* Futex lock-related helpers */
void reset_lock_futex_boost(task_ctx *taskc, struct cpu_ctx *cpuc);

/* Scheduler introspection-related helpers */
u64 get_est_stopping_clk(task_ctx *taskc, u64 now);
void try_proc_introspec_cmd(struct task_struct *p, task_ctx *taskc);
void reset_cpu_preemption_info(struct cpu_ctx *cpuc, bool released);
int shrink_boosted_slice_remote(struct cpu_ctx *cpuc, u64 now);
void shrink_boosted_slice_at_tick(struct task_struct *p,
					 struct cpu_ctx *cpuc, u64 now);
void preempt_at_tick(struct task_struct *p, struct cpu_ctx *cpuc);
void try_find_and_kick_victim_cpu(struct task_struct *p,
					 task_ctx *taskc,
					 s32 preferred_cpu,
					 u64 dsq_id);

extern volatile bool is_monitored;

#endif /* __LAVD_H */
