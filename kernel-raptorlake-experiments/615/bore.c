/*
 *  Burst-Oriented Response Enhancer (BORE) CPU Scheduler
 *  Copyright (C) 2021-2025 Masahito Suzuki <firelzrd@gmail.com>
 */
#undef pr_fmt
#define pr_fmt(fmt) "BORE: " fmt

/* ================================================================== */
/*                          Headers                                   */
/* ================================================================== */
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/atomic.h>
#include <linux/spinlock.h>
#include <linux/interrupt.h>
#include <linux/cpumask.h>
#include <linux/percpu.h>
#include <linux/sched.h>
#include <linux/sched/clock.h>
#include <linux/sched/task.h>
#include <linux/sched/topology.h>
#include <linux/sched/bore.h>
#include <linux/sched/signal.h>
#include <linux/sysctl.h>
#include <linux/bitmap.h>
#include <linux/static_key.h>
#include <linux/list.h>
#include <linux/slab.h>
#include <linux/math64.h>
#include <linux/cpuhotplug.h>
#include <linux/workqueue.h>
#include <linux/cpu.h>
#include <linux/smp.h>
#include <linux/refcount.h>
#include <linux/prefetch.h>
#include <linux/bitops.h>
#include <linux/jiffies.h>
#include <linux/types.h>
#include <linux/rculist.h>
#include <linux/limits.h>
#include <linux/lockdep.h>
#include <linux/build_bug.h>

#ifdef CONFIG_X86
#include <asm/processor.h>
#include <asm/topology.h>
#include <asm/cpufeature.h>
#include <asm/msr.h>
#include <asm/msr-index.h>
#endif

/*
 * Fallback for task_cpu_possible() if not defined in sched.h.
 * Must precede "sched.h" inclusion.
 */
#ifndef task_cpu_possible
static __always_inline bool task_cpu_possible(int cpu, const struct task_struct *p)
{
#ifdef CONFIG_SMP
	(void)p;
	return cpumask_test_cpu(cpu, cpu_possible_mask);
#else
	(void)cpu;
	(void)p;
	return true;
#endif
}
#endif

#include "sched.h"

#ifdef CONFIG_SCHED_BORE

/* Forward declarations */
void update_burst_penalty(struct sched_entity *se);
void update_curr_bore(u64 delta_exec, struct sched_entity *se);
void update_burst_score(struct sched_entity *se);
void restart_burst(struct sched_entity *se);
void restart_burst_rescale_deadline(struct sched_entity *se);
void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
		      u64 clone_flags, u64 now_ns);
void reset_task_bore(struct task_struct *p);
void __init sched_bore_init(void);

/* ================================================================== */
/*                         Tunables                                   */
/* ================================================================== */
#define BORE_ORIG_SCHED_BORE                   1
#define BORE_ORIG_BURST_EXCLUDE_KTHREADS       1
#define BORE_ORIG_BURST_SMOOTHNESS_LONG        1
#define BORE_ORIG_BURST_SMOOTHNESS_SHORT       0
#define BORE_ORIG_BURST_FORK_ATAVISTIC         2
#define BORE_ORIG_BURST_PARITY_THRESHOLD       2
#define BORE_ORIG_BURST_PENALTY_OFFSET         24
#define BORE_ORIG_BURST_PENALTY_SCALE          1280
#define BORE_ORIG_BURST_CACHE_STOP_COUNT       64
#define BORE_ORIG_BURST_CACHE_LIFETIME         (75 * 1000 * 1000)
#define BORE_ORIG_DEADLINE_BOOST_MASK          (ENQUEUE_INITIAL | ENQUEUE_WAKEUP)
#define BORE_DEF_ECORE_AVERSION_PENALTY        2

#define DEF_U8(name, val)   u8   __read_mostly name = (val)
#define DEF_U32(name, val)  u32  __read_mostly name = (val)

DEF_U8(sched_bore,                    BORE_ORIG_SCHED_BORE);
DEF_U8(sched_burst_exclude_kthreads,  BORE_ORIG_BURST_EXCLUDE_KTHREADS);
DEF_U8(sched_burst_smoothness_long,   BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short,  BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_fork_atavistic,    BORE_ORIG_BURST_FORK_ATAVISTIC);
DEF_U8(sched_burst_parity_threshold,  BORE_ORIG_BURST_PARITY_THRESHOLD);
DEF_U8(sched_burst_penalty_offset,    BORE_ORIG_BURST_PENALTY_OFFSET);

DEF_U32(sched_burst_penalty_scale,     BORE_ORIG_BURST_PENALTY_SCALE);
DEF_U32(sched_burst_cache_stop_count,  BORE_ORIG_BURST_CACHE_STOP_COUNT);
DEF_U32(sched_burst_cache_lifetime,    BORE_ORIG_BURST_CACHE_LIFETIME);
DEF_U32(sched_deadline_boost_mask,     BORE_ORIG_DEADLINE_BOOST_MASK);

/* Core-aware tunables (x86 hybrid CPU support) */
#ifdef CONFIG_X86
DEF_U8(sched_burst_core_aware_penalty,    0);
DEF_U8(sched_burst_core_aware_smoothing,  0);
DEF_U32(sched_burst_penalty_pcore_scale_pct, 100);
DEF_U32(sched_burst_penalty_ecore_scale_pct, 100);
DEF_U8(sched_burst_smoothness_long_p, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_p, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_smoothness_long_e, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_e, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_ecore_aversion_penalty, BORE_DEF_ECORE_AVERSION_PENALTY);
#endif

/* ================================================================== */
/*                    ITD via HFI (CONFIG_X86 only)                   */
/* ================================================================== */
#ifdef CONFIG_X86

/* MSR definitions with fallback guards */
#ifndef MSR_IA32_HW_FEEDBACK_CONFIG
#define MSR_IA32_HW_FEEDBACK_CONFIG 0x17d
#endif
#ifndef MSR_IA32_HW_FEEDBACK_THREAD_CONFIG
#define MSR_IA32_HW_FEEDBACK_THREAD_CONFIG 0x17e
#endif
#ifndef MSR_IA32_HW_FEEDBACK_CHAR
#define MSR_IA32_HW_FEEDBACK_CHAR 0x17f
#endif
#ifndef HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT
#define HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT BIT(0)
#endif
#ifndef HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT
#define HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT BIT(0)
#endif

/* ITD/HFI sysctls */
static u8  __read_mostly sched_burst_itd_enable     = 1;
static u32 __read_mostly sched_burst_itd_async_interval_ms = 10;
static u32 __read_mostly sched_burst_itd_cap_pct    = 60;

/* Per-class bias percentages */
#define ITD_MAX_CLASSES 8
static u32 __read_mostly sched_burst_itd_bias_pcore_pct[ITD_MAX_CLASSES] = {
	[0] = 0,    [1] = 5,    [2] = 45,   [3] = 90,
	[4 ... ITD_MAX_CLASSES - 1] = 0,
};
static u32 __read_mostly sched_burst_itd_bias_ecore_pct[ITD_MAX_CLASSES] = {
	[0] = 0,    [1] = 10,   [2] = 50,   [3] = 100,
	[4 ... ITD_MAX_CLASSES - 1] = 0,
};

/*
 * Async sampling state, cache-line optimized.
 * The layout is critical. Hot-path fields are in the first cache line to
 * minimize cache misses. Cold-path fields, written by the async worker,
 * are in the second cache line to prevent false sharing.
 */
struct bore_itd_state {
	/* HOT PATH: Read on every penalty update (if ITD enabled) */
	u8  cached_class;
	u8  cached_valid;
	u8  inited;
	u8  _pad1[61]; /* Pad to ensure cold fields are on a new cache line */

	/* COLD PATH: Written by async worker, read on init */
	u64 last_sample_ns;
	u8  last_class;
	u8  last_valid;
	u8  _pad2[54];
} ____cacheline_aligned_in_smp;

/* Robustness: Verify struct layout at compile time. */
static_assert(sizeof(struct bore_itd_state) == 128);

static DEFINE_PER_CPU(struct bore_itd_state, bore_itd_state);
DEFINE_STATIC_KEY_FALSE(bore_itd_key);

/* Per-CPU delayed work for async ITD sampling */
static void bore_itd_sample_work_fn(struct work_struct *work);
static DEFINE_PER_CPU(struct delayed_work, bore_itd_sample_work);

static __always_inline bool bore_itd_is_available(void)
{
	return READ_ONCE(sched_burst_itd_enable) && boot_cpu_has(X86_FEATURE_HFI);
}

/* Async MSR sampler, runs in a delayed work queue */
static void bore_itd_sample_work_fn(struct work_struct *work)
{
	struct bore_itd_state *st = this_cpu_ptr(&bore_itd_state);
	u64 msr_val;
	u8 classid;
	bool valid;

	if (!READ_ONCE(sched_burst_itd_enable)) {
		return;
	}

	if (rdmsrq_safe(MSR_IA32_HW_FEEDBACK_CHAR, &msr_val)) {
		WRITE_ONCE(st->cached_valid, 0);
	} else {
		valid = (msr_val >> 63) & 0x1;
		if (valid) {
			classid = (u8)(msr_val & 0xff);
			WRITE_ONCE(st->cached_class, classid);
			/*
			 * This memory barrier is crucial. It ensures that the new
			 * `cached_class` value is visible to other CPUs before
			 * the `cached_valid` flag is set to 1.
			 */
			smp_wmb();
			WRITE_ONCE(st->cached_valid, 1);
		} else {
			WRITE_ONCE(st->cached_valid, 0);
		}
	}

	/* Re-schedule for next interval */
	schedule_delayed_work_on(smp_processor_id(),
				 this_cpu_ptr(&bore_itd_sample_work),
				 msecs_to_jiffies(READ_ONCE(sched_burst_itd_async_interval_ms)));
}

/*
 * Context-safe HFI MSR initialization.
 * This function performs the one-time MSR writes to enable HFI for a CPU.
 * It is ONLY ever called from the CPU hotplug online callback, which is a
 * safe, non-atomic context for this type of hardware setup.
 */
static void bore_itd_enable_cpu(void)
{
	u64 val;

	lockdep_assert_preemption_enabled();

	if (!bore_itd_is_available()) {
		return;
	}

	/* Enable HFI thread-level feedback (Raptor Lake+) */
	if (!rdmsrq_safe(MSR_IA32_HW_FEEDBACK_THREAD_CONFIG, &val)) {
		val |= HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT;
		wrmsrl(MSR_IA32_HW_FEEDBACK_THREAD_CONFIG, val);
	} else {
		wrmsrl(MSR_IA32_HW_FEEDBACK_THREAD_CONFIG,
		       HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT);
	}

	/* Enable package-level HFI */
	if (!rdmsrq_safe(MSR_IA32_HW_FEEDBACK_CONFIG, &val)) {
		val |= HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT;
		wrmsrl(MSR_IA32_HW_FEEDBACK_CONFIG, val);
	}
}

/*
 * RE-ARCHITECTED: Lazy-init function for the hot path.
 * This function is now guaranteed to be MSR-write-free and safe to call from
 * any context, including hard IRQs. Its only job is to idempotently start
 * the asynchronous work queue sampler on the first call for a given CPU.
 */
static __always_inline void bore_itd_lazy_enable_this_cpu(void)
{
	struct bore_itd_state *st;

	lockdep_assert_preemption_disabled();

	st = this_cpu_ptr(&bore_itd_state);

	/* Idempotent check: return if already initialized. */
	if (READ_ONCE(st->inited)) {
		return;
	}

	/* Start async sampler. This is a safe operation. */
	INIT_DELAYED_WORK(this_cpu_ptr(&bore_itd_sample_work), bore_itd_sample_work_fn);
	schedule_delayed_work_on(smp_processor_id(),
				 this_cpu_ptr(&bore_itd_sample_work),
				 msecs_to_jiffies(READ_ONCE(sched_burst_itd_async_interval_ms)));

	WRITE_ONCE(st->inited, 1);
	WRITE_ONCE(st->cached_valid, 0);
}

/* MSR-free hot-path class read */
static __always_inline bool bore_itd_read_class_fast(u8 *classid)
{
	struct bore_itd_state *st;
	u8 valid;

	lockdep_assert_preemption_disabled();

	st = this_cpu_ptr(&bore_itd_state);

	valid = READ_ONCE(st->cached_valid);
	/*
	 * This memory barrier pairs with the smp_wmb() in the writer.
	 * It ensures that if we see `cached_valid == 1`, we are guaranteed
	 * to see the corresponding `cached_class` value written before it.
	 */
	smp_rmb();

	if (valid) {
		*classid = READ_ONCE(st->cached_class);
		return true;
	}
	return false;
}


/* ITD-based penalty bias (optimized: division by const) */
static __always_inline u32 bore_itd_bias_penalty_fast(u32 base_penalty,
						      u8  classid,
						      bool is_pcore)
{
	u32 pct, cap, eff_pct, adj;

	if (classid >= ITD_MAX_CLASSES || base_penalty == 0U) {
		return base_penalty;
	}

	pct = is_pcore
		? READ_ONCE(sched_burst_itd_bias_pcore_pct[classid])
		: READ_ONCE(sched_burst_itd_bias_ecore_pct[classid]);
	cap = clamp_t(u32, READ_ONCE(sched_burst_itd_cap_pct), 0U, 100U);

	pct = clamp_t(u32, pct, 0U, 100U);
	if (!pct || !cap) {
		return base_penalty;
	}

	eff_pct = min(cap, pct);
	adj = (base_penalty * eff_pct) / 100U;  /* Compiler emits imul + shr */

	if (is_pcore) {
		/* Reduce penalty on P-core */
		return (adj >= base_penalty) ? 0U : (base_penalty - adj);
	} else {
		/* Increase penalty on E-core */
		u32 res = base_penalty + adj;
		return (res < base_penalty) ? U32_MAX : res;  /* Saturate */
	}
}
#endif /* CONFIG_X86 */

/* ================================================================== */
/*                         Constants                                  */
/* ================================================================== */
#define MAX_BURST_PENALTY          156U
#define ECORE_OFFSET_ADJ_DIV       20
#define MAX_ECORE_OFFSET_ADJUST    10
#define MIN_EFFECTIVE_OFFSET       4
#define BORE_MAX_VRUNTIME_CLAMP    (200LL * NSEC_PER_MSEC)

/* ================================================================== */
/*                     Static keys & per-CPU state                    */
/* ================================================================== */
#ifdef CONFIG_X86
DEFINE_STATIC_KEY_FALSE(bore_core_aware_key);
#endif

DEFINE_STATIC_KEY_TRUE(bore_enable_key);

/*
 * Per-CPU penalty param cache.
 * Tightly packed and explicitly padded to exactly one cache line (64 bytes).
 * This prevents false sharing and ensures optimal access patterns.
 * Includes pre-computed values for reciprocal multiplication.
 */
struct bore_penalty_param {
	u32 scale;
	u32 sat_thresh_q8;
	u32 scale_mult_magic; /* Reciprocal for fast multiply */
	s16 itd_pri;
	u8  scale_shift;      /* Post-multiply shift */
	u8  offset;
	u8  gen;
	/* Pad to a full cache line to prevent false sharing with other per-cpu data. */
	u8  _pad[47];
} ____cacheline_aligned_in_smp;

/*
 * Robustness: Verify struct layout at compile time. The size MUST be 64,
 * as enforced by the alignment macro.
 */
static_assert(sizeof(struct bore_penalty_param) == 64);

static DEFINE_PER_CPU(struct bore_penalty_param, bore_penalty);
static atomic_t bore_penalty_gen = ATOMIC_INIT(1);

/*
 * Per-CPU penalty result cache.
 * Cache-line aligned to prevent false sharing on SMT siblings.
 * On Raptor Lake with 64-byte cache lines, hot fields (last_se, last_penalty)
 * stay in L1d; debug counters are in cold section to reduce pressure.
 */
struct bore_penalty_cache {
	/* HOT: Read/written on every update_burst_penalty call */
	struct sched_entity *last_se;
	u64 last_burst_time;
	u32 last_penalty;
	int last_cpu;
	u8  gen;
	u8  _pad1[3];  /* Align next field to 8-byte boundary */

#ifdef CONFIG_SCHED_DEBUG
	u64 hit_count;
	u64 miss_count;
	u8  _pad2[20];  /* Total: 64 bytes with debug */
#else
	u8  _pad2[36];  /* Total: 64 bytes without debug */
#endif
} ____cacheline_aligned_in_smp;

/* Compile-time size validation (critical for correctness) */
static_assert(sizeof(struct bore_penalty_cache) == 64,
	      "bore_penalty_cache must be exactly one cache line");

/* Per-CPU instance (placed AFTER struct definition) */
static DEFINE_PER_CPU(struct bore_penalty_cache, bore_penalty_cache);

/* Per-CPU CPU-type cache (x86 only) */
#ifdef CONFIG_X86
static DEFINE_PER_CPU_ALIGNED(enum x86_topology_cpu_type, bore_cpu_type) = TOPO_CPU_TYPE_UNKNOWN;
#endif

/* Per-CPU IPCC dirty flag (compatibility stub) */
static DEFINE_PER_CPU(bool, bore_ipcc_dirty);

/* ================================================================== */
/*                   Lookup tables (LUT)                              */
/* ================================================================== */
static u8  prio_lookup[40][40] __ro_after_init __aligned(64);
static u32 log2_lookup[256]    __ro_after_init __aligned(64);

/* Forward declare param builder */
static void bore_build_penalty_param_for_cpu(int cpu);

#ifdef CONFIG_X86
static __always_inline enum x86_topology_cpu_type
bore_get_rq_cpu_type(struct rq *rq)
{
	if (static_branch_unlikely(&bore_core_aware_key)) {
		return per_cpu(bore_cpu_type, rq->cpu);
	}
	return TOPO_CPU_TYPE_UNKNOWN;
}
#else
/* Stub for non-x86 */
static __always_inline int bore_get_rq_cpu_type(struct rq *rq)
{
	(void)rq;
	return 0;  /* Generic/unknown */
}
#endif

static void __init bore_build_lookup_tables(void)
{
	int base, score, prio;

	/* Priority lookup: prio_lookup[base_prio][burst_score] = effective_prio */
	for (base = 0; base < 40; base++) {
		for (score = 0; score < 40; score++) {
			prio = base - score;
			prio = max(0, min(NICE_WIDTH - 1, prio));
			prio_lookup[base][score] = (u8)prio;
		}
	}

	/*
	 * Pre-computed log2 fractional lookup: log2(1 + i/256) * 256
	 * Derived offline to avoid floating-point math in kernel.
	 * Values for i=0..255 (Q8.8 fixed-point).
	 */
	static const u32 log2_lut_values[256] = {
		  0,   1,   3,   4,   6,   7,   9,  10,  11,  13,  14,  16,  17,  18,  20,  21,
		 22,  24,  25,  26,  28,  29,  30,  32,  33,  34,  35,  37,  38,  39,  40,  42,
		 43,  44,  45,  47,  48,  49,  50,  51,  53,  54,  55,  56,  57,  58,  60,  61,
		 62,  63,  64,  65,  66,  68,  69,  70,  71,  72,  73,  74,  75,  76,  78,  79,
		 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
		 96,  97,  98,  99, 100, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
		113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
		129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 141, 142, 143,
		144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 156, 157, 158,
		159, 160, 161, 162, 163, 164, 165, 165, 166, 167, 168, 169, 170, 171, 172, 173,
		173, 174, 175, 176, 177, 178, 179, 179, 180, 181, 182, 183, 184, 185, 185, 186,
		187, 188, 189, 190, 190, 191, 192, 193, 194, 195, 195, 196, 197, 198, 199, 199,
		200, 201, 202, 203, 203, 204, 205, 206, 207, 207, 208, 209, 210, 211, 211, 212,
		213, 214, 214, 215, 216, 217, 218, 218, 219, 220, 221, 221, 222, 223, 224, 224,
		225, 226, 227, 227, 228, 229, 230, 230, 231, 232, 233, 233, 234, 235, 236, 236,
		237, 238, 239, 239, 240, 241, 241, 242, 243, 244, 244, 245, 246, 247, 247, 248
	};

	memcpy(log2_lookup, log2_lut_values, sizeof(log2_lookup));
}

/* ================================================================== */
/*                   Hybrid CPU detection (x86)                       */
/* ================================================================== */
#ifdef CONFIG_X86
static bool __init is_intel_raptor_lake(void)
{
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6) {
		return false;
	}

	switch (boot_cpu_data.x86_model) {
	case 0xB7: case 0xBA: case 0xBE: case 0xBF:  /* RPL-S/P/H */
	case 0xAC: case 0xB1:                         /* RPL-R */
		return true;
	}
	return false;
}

static bool __init is_intel_hybrid(void)
{
	return boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake();
}
#endif

/* ================================================================== */
/*              Topology scan + static key enable                     */
/* ================================================================== */
#ifdef CONFIG_X86
static bool bore_cpu_types_detected;
static void bore_enable_key_workfn(struct work_struct *w);
static DECLARE_WORK(bore_enable_key_work, bore_enable_key_workfn);

static inline void bore_bump_penalty_gen(void)
{
	atomic_inc(&bore_penalty_gen);
}

/*
 * RCU-style generation validation.
 * This avoids a costly atomic read on the global generation counter in the
 * fast path. It reads the local per-CPU copy first and only validates against
 * the global counter if other local parameters seem stale.
 */
static inline const struct bore_penalty_param *bore_get_param(void)
{
	struct bore_penalty_param *pp;
	s16 itd_now;
	u8 g_now, g_local;

	lockdep_assert_preemption_disabled();

	pp      = this_cpu_ptr(&bore_penalty);
	itd_now = arch_asym_cpu_priority(smp_processor_id());

	/* Fast path: relaxed read of local generation. */
	g_local = READ_ONCE(pp->gen);
	/* Compiler barrier prevents reads of other params being reordered before this. */
	barrier();

	if (likely(pp->itd_pri == itd_now)) {
		/* Validate against global generation counter. */
		g_now = (u8)atomic_read(&bore_penalty_gen);
		if (likely(g_local == g_now)) {
			return pp; /* Fast path success */
		}
	}

	/* Slow path: parameters are stale, rebuild them. */
	bore_build_penalty_param_for_cpu(smp_processor_id());
	return pp;
}

static void bore_build_penalty_param_for_cpu(int cpu)
{
	struct bore_penalty_param *pp = &per_cpu(bore_penalty, cpu);
	enum x86_topology_cpu_type ct = per_cpu(bore_cpu_type, cpu);
	u32 scale  = READ_ONCE(sched_burst_penalty_scale);
	u8  offset = READ_ONCE(sched_burst_penalty_offset);
	s16 itd_pr = arch_asym_cpu_priority(cpu);

	/* Core-aware scaling */
	if (READ_ONCE(sched_burst_core_aware_penalty) &&
	    static_branch_unlikely(&bore_core_aware_key)) {
		u32 base = READ_ONCE(sched_burst_penalty_scale);

		if (ct == TOPO_CPU_TYPE_EFFICIENCY) {
			u32 pct = clamp_t(u32,
					  READ_ONCE(sched_burst_penalty_ecore_scale_pct),
					  0U, 200U);
			scale = div_u64((u64)base * pct, 100U);
			if (pct > 100U) {
				u8 adj = min_t(u8, MAX_ECORE_OFFSET_ADJUST,
					       (u8)((pct - 100U) / ECORE_OFFSET_ADJ_DIV));
				offset = max_t(u8, MIN_EFFECTIVE_OFFSET, (u8)(offset - adj));
			}
		} else if (ct == TOPO_CPU_TYPE_PERFORMANCE) {
			u32 pct = clamp_t(u32,
					  READ_ONCE(sched_burst_penalty_pcore_scale_pct),
					  0U, 200U);
			scale = div_u64((u64)base * pct, 100U);
		}
	}

	/* Asymmetric priority shaping (topology hint, not HFI/ITD) */
	if (itd_pr > 0) {
		u64 pr_norm  = min_t(u64, 1024U, (u64)itd_pr);
		u64 red_fact = (1024U - pr_norm) * (1024U - pr_norm);
		u32 red_perm = div_u64(red_fact, 1049U);
		u32 red      = div_u64((u64)scale * red_perm * 25, 100000U);
		scale = max_t(u32, scale / 4U, scale - red);
	}

	pp->scale         = scale;
	pp->offset        = offset;
	pp->itd_pri       = itd_pr;
	pp->sat_thresh_q8 = !scale ? U32_MAX :
		div_u64(((u64)MAX_BURST_PENALTY << 16) + scale - 1, scale);

	/* Pre-compute reciprocal for fast multiplication */
	if (scale > 0) {
		pp->scale_mult_magic = div_u64(1ULL << 32, scale);
		pp->scale_shift = 32;
	} else {
		pp->scale_mult_magic = 0;
		pp->scale_shift = 16; /* Fallback shift */
	}

	/* Ensure all params are visible before the generation number is updated. */
	smp_wmb();
	pp->gen = (u8)atomic_read(&bore_penalty_gen);
}

static void bore_enable_key_workfn(struct work_struct *w)
{
	(void)w;
	if (!static_branch_unlikely(&bore_core_aware_key)) {
		static_branch_enable(&bore_core_aware_key);
		pr_info("Core-aware static key enabled.\n");
	}
}

static void bore_check_and_update_topology_features(void)
{
	unsigned int cpu;
	bool found_hybrid = false;

	for_each_possible_cpu(cpu) {
		enum x86_topology_cpu_type t =
			get_topology_cpu_type(&cpu_data(cpu));
		per_cpu(bore_cpu_type, cpu) = t;
		if (t == TOPO_CPU_TYPE_PERFORMANCE ||
		    t == TOPO_CPU_TYPE_EFFICIENCY) {
			found_hybrid = true;
		}
		bore_build_penalty_param_for_cpu(cpu);
	}

	if (!found_hybrid) {
		if (is_intel_hybrid() && !bore_cpu_types_detected) {
			pr_info("Hybrid CPU detected but no P/E info yet.\n");
		}
		return;
	}

	if (!bore_cpu_types_detected) {
		pr_info("P/E core topology detected.\n");
		bore_cpu_types_detected            = true;
		sched_burst_core_aware_penalty     = 1;
		sched_burst_core_aware_smoothing   = 1;

		if (is_intel_raptor_lake()) {
			pr_info("Applying Raptor Lake P/E tunings.\n");
			WRITE_ONCE(sched_burst_penalty_ecore_scale_pct, 140);
			WRITE_ONCE(sched_burst_smoothness_long_e, 3);
			WRITE_ONCE(sched_burst_smoothness_short_e, 1);
			WRITE_ONCE(sched_burst_parity_threshold,
				   READ_ONCE(sched_burst_parity_threshold) + 2);
		}

		bore_bump_penalty_gen();
		for_each_possible_cpu(cpu) {
			bore_build_penalty_param_for_cpu(cpu);
		}
	}

	if (!static_branch_unlikely(&bore_core_aware_key)) {
		schedule_work(&bore_enable_key_work);
	}
}

/* MODIFIED: CPU hotplug callback now handles HFI enabling. */
static int bore_cpu_online_cb(unsigned int cpu)
{
	per_cpu(bore_cpu_type, cpu) = get_topology_cpu_type(&cpu_data(cpu));
	bore_build_penalty_param_for_cpu(cpu);

	/*
	 * Enable HFI for this CPU. This is the only place we write to HFI
	 * config MSRs, ensuring it's done in a safe context.
	 */
	bore_itd_enable_cpu();

	bore_check_and_update_topology_features();
	per_cpu(bore_ipcc_dirty, cpu) = true;
	return 0;
}

static int bore_cpu_offline_cb(unsigned int cpu)
{
	int i;

	for_each_online_cpu(i) {
		per_cpu(bore_ipcc_dirty, i) = true;
	}
	return 0;
}

static enum cpuhp_state bore_cpuhp_state_val __maybe_unused;

static int __init bore_topology_init(void)
{
	int ret_hp;

	bore_check_and_update_topology_features();

	ret_hp = cpuhp_setup_state_nocalls(CPUHP_AP_ONLINE_DYN,
					   "sched/bore:online",
					   bore_cpu_online_cb,
					   bore_cpu_offline_cb);
	if (ret_hp < 0) {
		pr_err("cpuhp_setup_state_nocalls failed: %d\n", ret_hp);
	} else {
		bore_cpuhp_state_val = ret_hp;
	}

	return 0;
}
#else
/* Non-x86 stubs */
static inline void bore_bump_penalty_gen(void) { }
static inline const struct bore_penalty_param *bore_get_param(void)
{
	static struct bore_penalty_param dummy = {
		.scale = BORE_ORIG_BURST_PENALTY_SCALE,
		.offset = BORE_ORIG_BURST_PENALTY_OFFSET,
		.sat_thresh_q8 = U32_MAX,
		.itd_pri = 0,
		.gen = 1,
	};
	return &dummy;
}

static void bore_build_penalty_param_for_cpu(int cpu)
{
	(void)cpu;
}
#endif /* CONFIG_X86 */

/* ================================================================== */
/*                   Penalty math (optimized & safe)                  */
/* ================================================================== */
static __always_inline u32 __attribute__((hot))
calc_burst_penalty(u64 burst_time)
{
	const struct bore_penalty_param *pp;
	u32 offset_q8, lz, exp, log_val, delta_q8, pen;
	u64 norm, frac;
	u32 index;

	if (unlikely(!burst_time)) {
		return 0U;
	}

	pp = bore_get_param();

	/* Compute exponent (integer part of log2) */
	lz  = __builtin_clzll(burst_time);
	exp = 63U - lz;

	offset_q8 = (u32)pp->offset << 8;

	/* Early exit: even with max fractional (255), still below offset */
	if (likely(((exp << 8) + 255U) <= offset_q8)) {
		return 0U;
	}

	/* Early saturation: if exponent alone exceeds threshold */
	if (pp->sat_thresh_q8 != U32_MAX) {
		u64 threshold_q8 = (u64)offset_q8 + (u64)pp->sat_thresh_q8;
		u64 exp_q8       = (u64)exp << 8;
		if (unlikely(exp_q8 >= threshold_q8)) {
			return MAX_BURST_PENALTY;
		}
	}

	/* Fractional part via LUT */
	norm  = burst_time << lz;  /* Normalize MSB to bit 63 */
	frac  = norm << 1;         /* Next 8 bits become index */
	index = (u32)(frac >> 56); /* Extract top 8 bits */
	index = min_t(u32, index, 255);  /* Branchless clamp */

	log_val = (exp << 8) | log2_lookup[index];

	if (log_val <= offset_q8) {
		return 0U;
	}

	delta_q8 = log_val - offset_q8;

	/* Branchless saturation */
	delta_q8 = min_t(u32, delta_q8, pp->sat_thresh_q8);

	/* Fast reciprocal multiply (no division dependency) */
	if (likely(pp->scale_mult_magic)) {
		u64 prod = (u64)delta_q8 * pp->scale_mult_magic;
		pen = (u32)(prod >> pp->scale_shift);
	} else {
		pen = 0;
	}

	return min_t(u32, pen, MAX_BURST_PENALTY);
}

static __always_inline u64 __scale_slice(u64 d, u8 pr)
{
	pr = min_t(u8, NICE_WIDTH - 1, pr);
	return mul_u64_u32_shr(d, sched_prio_to_wmult[pr], 22);
}

static __always_inline u64 __unscale_slice(u64 d, u8 pr)
{
	pr = min_t(u8, NICE_WIDTH - 1, pr);
	return mul_u64_u32_shr(d, sched_prio_to_weight[pr], 10);
}

/* ================================================================== */
/*                         Hot-path updates                           */
/* ================================================================== */

void __attribute__((hot))
update_burst_penalty(struct sched_entity *se)
{
	struct cfs_rq *cfs_rq;
	struct rq *rq;
	u64 burst_time;
	u32 penalty;

	if (unlikely(!entity_is_task(se))) {
		return;
	}

	cfs_rq = cfs_rq_of(se);
	if (unlikely(!cfs_rq)) {
		return;
	}
	rq = rq_of(cfs_rq);

	/* Prefetch critical data early. */
	__builtin_prefetch(&se->burst_time, 0, 3);
	__builtin_prefetch(&se->burst_penalty, 1, 2);
	preempt_disable();
	__builtin_prefetch(this_cpu_ptr(&bore_penalty), 0, 3);

#ifdef CONFIG_X86
	/* Prefetch ITD state to hide DRAM latency */
	if (static_branch_unlikely(&bore_itd_key)) {
		__builtin_prefetch(this_cpu_ptr(&bore_itd_state), 0, 2);
	}
#endif

	burst_time = READ_ONCE(se->burst_time);

	{
		u8 gen_snapshot;
		int rq_cpu = READ_ONCE(rq->cpu);

		{
			struct bore_penalty_cache *cache = this_cpu_ptr(&bore_penalty_cache);
			gen_snapshot = (u8)atomic_read(&bore_penalty_gen);

			if (likely(cache->last_se == se &&
				   cache->last_burst_time == burst_time &&
				   cache->last_cpu == rq_cpu &&
				   cache->gen == gen_snapshot)) {
				penalty = cache->last_penalty;
#ifdef CONFIG_SCHED_DEBUG
				cache->hit_count++;
#endif
			} else {
				penalty = calc_burst_penalty(burst_time);

#ifdef CONFIG_X86
				/* Use fast async ITD read. */
				if (static_branch_unlikely(&bore_itd_key)) {
					u8 classid;
					bool is_pcore = (per_cpu(bore_cpu_type, rq_cpu) == TOPO_CPU_TYPE_PERFORMANCE);

					bore_itd_lazy_enable_this_cpu();
					if (bore_itd_read_class_fast(&classid)) {
						penalty = bore_itd_bias_penalty_fast(penalty, classid, is_pcore);
					}
				}
#endif
				cache->last_se         = se;
				cache->last_burst_time = burst_time;
				cache->last_penalty    = penalty;
				cache->last_cpu        = rq_cpu;
				cache->gen             = gen_snapshot;
#ifdef CONFIG_SCHED_DEBUG
				cache->miss_count++;
#endif
			}
		}
	}
	preempt_enable();


	se->curr_burst_penalty = penalty;

	{
		u32 old_agg = READ_ONCE(se->burst_penalty);
		u32 new_agg = (se->prev_burst_penalty > penalty)
				? se->prev_burst_penalty : penalty;

		if (unlikely(new_agg != old_agg)) {
			se->burst_penalty = new_agg;
			update_burst_score(se);
		}
	}
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

void update_curr_bore(u64 delta_exec, struct sched_entity *se)
{
	struct rq *rq;

	if (unlikely(!entity_is_task(se))) {
		return;
	}

	if (!delta_exec) {
		return;
	}

	rq = rq_of(cfs_rq_of(se));
	lockdep_assert_rq_held(rq);

	se->burst_time += delta_exec;
	update_burst_penalty(se);
}
EXPORT_SYMBOL_GPL(update_curr_bore);

/*
 * This is an exponential smoothing function. It has been explicitly rewritten
 * to be branch-free to guarantee optimal code generation (e.g., CMOV on x86)
 * and avoid costly branch mispredictions in a hot path. This is more robust
 * than relying on compiler optimization of ternary operators.
 */
static __always_inline __attribute__((hot)) u32
binary_smooth(u32 new_val, u32 old_val, int ctype_hint)
{
	u8 shift_up, shift_down, shift;
	s32 delta;
	u32 up_mask, magnitude, adjustment, sum, up_result, down_result;

	if (new_val == old_val) {
		return old_val;
	}

#ifdef CONFIG_X86
	if (static_branch_unlikely(&bore_core_aware_key)) {
		if (ctype_hint == TOPO_CPU_TYPE_PERFORMANCE) {
			shift_up   = READ_ONCE(sched_burst_smoothness_long_p);
			shift_down = READ_ONCE(sched_burst_smoothness_short_p);
		} else if (ctype_hint == TOPO_CPU_TYPE_EFFICIENCY) {
			shift_up   = READ_ONCE(sched_burst_smoothness_long_e);
			shift_down = READ_ONCE(sched_burst_smoothness_short_e);
		} else {
			shift_up   = READ_ONCE(sched_burst_smoothness_long);
			shift_down = READ_ONCE(sched_burst_smoothness_short);
		}
	} else
#endif
	{
		(void)ctype_hint;
		shift_up   = READ_ONCE(sched_burst_smoothness_long);
		shift_down = READ_ONCE(sched_burst_smoothness_short);
	}

	shift_up   = min_t(u8, shift_up, 31);
	shift_down = min_t(u8, shift_down, 31);

	delta = (s32)new_val - (s32)old_val;

	/*
	 * Branchless selection using bitwise masks.
	 * `up_mask` will be all 1s if smoothing up (delta > 0), all 0s otherwise.
	 * This is a standard technique to avoid branches.
	 */
	up_mask = (u32) - (delta > 0);

	shift     = (up_mask & shift_up) | (~up_mask & shift_down);
	magnitude = abs(delta);
	adjustment = magnitude >> shift;

	/* Calculate the "up" path result and saturate it branchlessly. */
	if (__builtin_add_overflow(old_val, adjustment, &sum) || sum > MAX_BURST_PENALTY) {
		up_result = MAX_BURST_PENALTY;
	} else {
		up_result = sum;
	}

	/* Calculate the "down" path result and handle underflow branchlessly. */
	if (adjustment >= old_val) {
		down_result = 0;
	} else {
		down_result = old_val - adjustment;
	}

	/* Select the final result using the mask. */
	return (up_mask & up_result) | (~up_mask & down_result);
}

static __always_inline __attribute__((hot)) u8
effective_prio(struct task_struct *p)
{
	int base, score;

	base = p->static_prio - MAX_RT_PRIO;

	/* Static branch for BORE toggle (0-cycle when enabled) */
	if (static_branch_likely(&bore_enable_key)) {
		score = READ_ONCE(p->se.burst_score);

		/* Branchless clamp using CMOV (x86) */
		base = (base < 0) ? 0 : base;
		base = (base > 39) ? 39 : base;
		score = (score > 39) ? 39 : score;

		return prio_lookup[base][score];
	}

	/* BORE disabled */
	base = (base < 0) ? 0 : base;
	return (u8)((base > (NICE_WIDTH - 1)) ? (NICE_WIDTH - 1) : base);
}

static void reweight_task_by_prio(struct task_struct *p, int prio_val)
{
	struct sched_entity *se   = &p->se;
	struct cfs_rq *cfs_rq = cfs_rq_of(se);
	struct rq *rq = task_rq(p);

	lockdep_assert_rq_held(rq);

	prio_val = clamp(prio_val, 0, NICE_WIDTH - 1);
	reweight_entity(cfs_rq, se,
			scale_load(sched_prio_to_weight[prio_val]),
			true);
	se->load.inv_weight = sched_prio_to_wmult[prio_val];
}

void __attribute__((hot))
update_burst_score(struct sched_entity *se)
{
	struct task_struct *p;
	struct cfs_rq *cfs_rq = NULL;
	struct rq *rq = NULL;
	bool bore_enabled;
	int base_index;
	u8 prev_score, new_score = 0, old_prio, new_prio;

	if (unlikely(!entity_is_task(se))) {
		return;
	}

	p = task_of(se);

	if (se->on_rq) {
		cfs_rq = cfs_rq_of(se);
		if (unlikely(!cfs_rq)) {
			return;
		}
		rq = rq_of(cfs_rq);
		lockdep_assert_rq_held(rq);
	}

	bore_enabled = static_branch_likely(&bore_enable_key);
	base_index = clamp_t(int, READ_ONCE(p->static_prio) - MAX_RT_PRIO,
			     0, NICE_WIDTH - 1);

	prev_score = se->burst_score;
	old_prio = bore_enabled ?
		prio_lookup[base_index][min_t(u8, prev_score, NICE_WIDTH - 1)] :
		(u8)base_index;

	if (!((p->flags & PF_KTHREAD) && READ_ONCE(sched_burst_exclude_kthreads))) {
		new_score = min_t(u8, se->burst_penalty >> 2, NICE_WIDTH - 1);
	}

#ifdef CONFIG_X86
	if (static_branch_unlikely(&bore_core_aware_key) && se->on_rq) {
		enum x86_topology_cpu_type rq_type = per_cpu(bore_cpu_type, rq->cpu);

		if (rq_type == TOPO_CPU_TYPE_PERFORMANCE) {
			u32 hog_threshold = (MAX_BURST_PENALTY * 85 + 99) / 100;

			if (se->burst_penalty > hog_threshold) {
				new_score = min_t(u8, new_score + 2, NICE_WIDTH - 1);
			}
		} else if (rq_type == TOPO_CPU_TYPE_EFFICIENCY) {
			if (se->burst_penalty < (MAX_BURST_PENALTY >> 2)) {
				new_score = min_t(u8,
						  new_score +
						  READ_ONCE(sched_burst_ecore_aversion_penalty),
						  NICE_WIDTH - 1);
			}
		}
	}
#endif

	if (new_score != prev_score) {
		se->burst_score = new_score;
	}

	new_prio = bore_enabled ?
		prio_lookup[base_index][min_t(u8, new_score, NICE_WIDTH - 1)] :
		(u8)base_index;

	if (new_prio != old_prio) {
		reweight_task_by_prio(p, new_prio);
	}
}
EXPORT_SYMBOL_GPL(update_burst_score);

static void revolve_burst_penalty_state(struct sched_entity *se, int ctype_hint)
{
	se->prev_burst_penalty = binary_smooth(se->curr_burst_penalty,
					       se->prev_burst_penalty,
					       ctype_hint);
	se->burst_time         = 0;
	se->curr_burst_penalty = 0;
}

void restart_burst(struct sched_entity *se)
{
	struct rq *rq = rq_of(cfs_rq_of(se));

	lockdep_assert_rq_held(rq);

	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq));
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);

void restart_burst_rescale_deadline(struct sched_entity *se)
{
	struct task_struct *p;
	struct rq *rq;
	struct cfs_rq *cfs_rq;
	u8 old_eff_prio, new_eff_prio;
	s64 vruntime_remaining, new_deadline;
	u64 weight_remaining;
	int ctype;

	if (unlikely(!entity_is_task(se))) {
		return;
	}

	cfs_rq = cfs_rq_of(se);
	rq = rq_of(cfs_rq);

	lockdep_assert_rq_held(rq);

	p = task_of(se);
	old_eff_prio = effective_prio(p);

	ctype = bore_get_rq_cpu_type(rq);
	revolve_burst_penalty_state(se, ctype);
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);

	new_eff_prio = effective_prio(p);

	if (likely(old_eff_prio == new_eff_prio)) {
		return;
	}

	if (old_eff_prio <= new_eff_prio) {
		return;
	}

	vruntime_remaining = se->deadline - se->vruntime;

	if (vruntime_remaining <= 0) {
		u64 overrun = (u64)(-vruntime_remaining);

		overrun = min_t(u64, overrun, BORE_MAX_VRUNTIME_CLAMP);

		weight_remaining = __unscale_slice(overrun, old_eff_prio);
		overrun = __scale_slice(weight_remaining, new_eff_prio);

		if (overrun > (u64)(se->vruntime - S64_MIN)) {
			new_deadline = se->vruntime;
		} else {
			new_deadline = se->vruntime - (s64)overrun;
		}
	} else {
		u64 remaining = (u64)vruntime_remaining;

		remaining = min_t(u64, remaining, BORE_MAX_VRUNTIME_CLAMP);

		weight_remaining = __unscale_slice(remaining, old_eff_prio);
		remaining = __scale_slice(weight_remaining, new_eff_prio);

		if (remaining > (u64)(S64_MAX - se->vruntime)) {
			new_deadline = S64_MAX;
		} else {
			new_deadline = se->vruntime + (s64)remaining;
		}
	}

	if (unlikely(new_deadline < se->vruntime)) {
		if (IS_ENABLED(CONFIG_SCHED_DEBUG)) {
			WARN_ONCE(1, "BORE: deadline corruption prevented: "
				 "old_d=%lld new_d=%lld vr=%lld old_p=%u new_p=%u\n",
				 se->deadline, new_deadline, se->vruntime,
				 old_eff_prio, new_eff_prio);
		}
		new_deadline = se->vruntime;
	}

	se->deadline = new_deadline;
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

/* ================================================================== */
/*                      Inheritance & cache                           */
/* ================================================================== */
static inline bool task_is_bore_eligible(struct task_struct *p)
{
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

static inline void init_task_burst_cache_lock(struct task_struct *p)
{
	spin_lock_init(&p->se.child_burst.lock);
	spin_lock_init(&p->se.group_burst.lock);
}

static inline bool burst_cache_expired(struct sched_burst_cache *bc, u64 now_ns)
{
	u64 stamp = READ_ONCE(bc->timestamp);
	u32 life  = READ_ONCE(sched_burst_cache_lifetime);

	if (now_ns <= stamp) {
		return false;
	}
	return (now_ns - stamp) > life;
}

#ifndef for_each_child_locked
#define for_each_child_locked(parent, child) \
	list_for_each_entry(child, &(parent)->children, sibling)
#endif

static void update_burst_cache_locked(struct sched_burst_cache *bc,
				      struct task_struct *p_owner_of_cache,
				      u32 children_count, u32 children_penalty_sum, u64 now_ns)
{
	u32 avg = children_count ? (u32)div_u64((u64)children_penalty_sum, children_count) : 0;
	bc->value = max(avg, READ_ONCE(p_owner_of_cache->se.burst_penalty));
	bc->count = children_count;
	bc->timestamp = now_ns;
}

static inline u32 count_children_upto2_locked(struct list_head *head)
{
	struct list_head *next = head->next;
	return (next != head) + (next->next != head);
}

static void __update_child_burst_direct_locked(struct task_struct *p, u64 now_ns)
{
	u32 count = 0, sum_penalties = 0;
	struct task_struct *child_iter;

	for_each_child_locked(p, child_iter) {
		if (likely(task_is_bore_eligible(child_iter))) {
			u32 pen = READ_ONCE(child_iter->se.burst_penalty);
			if (!__builtin_add_overflow(sum_penalties, pen, &sum_penalties)) {
				count++;
			} else {
				sum_penalties = UINT_MAX;
				count++;
			}
		}
	}
	update_burst_cache_locked(&p->se.child_burst, p, count, sum_penalties, now_ns);
}

static void __update_child_burst_topological_locked(struct task_struct *p, u64 now_ns,
						    u32 recursion_depth,
						    u32 *accumulated_count, u32 *accumulated_sum)
{
	u32 current_level_direct_children_count = 0;
	u32 current_level_sum_of_penalties = 0;
	struct task_struct *child_iter, *effective_descendant;

	for_each_child_locked(p, child_iter) {
		u32 nkids;

		prefetch(child_iter->sibling.next);
		prefetch(&child_iter->children);

		effective_descendant = child_iter;
		while (count_children_upto2_locked(&effective_descendant->children) == 1) {
			effective_descendant = list_first_entry(&effective_descendant->children,
								struct task_struct, sibling);
		}

		nkids = count_children_upto2_locked(&effective_descendant->children);
		if (recursion_depth == 0 || !nkids) {
			if (task_is_bore_eligible(effective_descendant)) {
				current_level_direct_children_count++;
				current_level_sum_of_penalties =
					min_t(u32, UINT_MAX, current_level_sum_of_penalties +
					      READ_ONCE(effective_descendant->se.burst_penalty));
			}
			continue;
		}

		{
			struct sched_burst_cache *desc_cache = &effective_descendant->se.child_burst;

			spin_lock(&desc_cache->lock);
			if (!burst_cache_expired(desc_cache, now_ns)) {
				u32 c = READ_ONCE(desc_cache->count);
				u32 v = READ_ONCE(desc_cache->value);
				u64 mult = (u64)v * (u64)c;

				current_level_direct_children_count += c;
				current_level_sum_of_penalties =
					min_t(u32, UINT_MAX,
					      current_level_sum_of_penalties +
					      (mult > UINT_MAX ? UINT_MAX : (u32)mult));
				spin_unlock(&desc_cache->lock);
			} else {
				spin_unlock(&desc_cache->lock);
				__update_child_burst_topological_locked(effective_descendant, now_ns,
									recursion_depth - 1,
									&current_level_direct_children_count,
									&current_level_sum_of_penalties);
			}
		}

		if (READ_ONCE(sched_burst_cache_stop_count) > 0 &&
		    current_level_direct_children_count >= READ_ONCE(sched_burst_cache_stop_count)) {
			break;
		}
	}

	spin_lock(&p->se.child_burst.lock);
	update_burst_cache_locked(&p->se.child_burst, p, current_level_direct_children_count,
				  current_level_sum_of_penalties, now_ns);
	spin_unlock(&p->se.child_burst.lock);

	*accumulated_count += current_level_direct_children_count;
	*accumulated_sum = min_t(u32, UINT_MAX,
				 *accumulated_sum + current_level_sum_of_penalties);
}

/*
 * Note on tasklist_lock: The following inheritance functions use the global
 * tasklist_lock for reading the process tree. This is the standard, safe
 * kernel practice. While it is a known scalability bottleneck for extremely
 * high-fork workloads, replacing it with a lock-free RCU traversal is a
 * major architectural change outside the scope of this single-file modification.
 * The current implementation is correct and matches kernel conventions.
 */
static u8 inherit_burst_direct(struct task_struct *parent_task,
			       u64 now_ns, u64 clone_flags)
{
	struct task_struct *target;
	unsigned long irqflags;
	u8 value = 0;

	if (unlikely(!parent_task)) {
		return 0;
	}

	target = parent_task;
	if (clone_flags & CLONE_PARENT) {
		target = parent_task->real_parent;
		if (unlikely(!target)) {
			return 0;
		}
	}

	read_lock(&tasklist_lock);
	spin_lock_irqsave(&target->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&target->se.child_burst, now_ns)) {
		__update_child_burst_direct_locked(target, now_ns);
	}
	value = target->se.child_burst.value;
	spin_unlock_irqrestore(&target->se.child_burst.lock, irqflags);
	read_unlock(&tasklist_lock);

	return value;
}

static u8 inherit_burst_topological(struct task_struct *parent_task,
				    u64 now_ns, u64 clone_flags)
{
	struct task_struct *ancestor;
	u32 child_count_threshold;
	unsigned long irqflags;
	u8 value = 0;
	u32 dummy_children_count = 0, dummy_penalty_sum = 0;

	if (unlikely(!parent_task)) {
		return 0;
	}

	read_lock(&tasklist_lock);
	if (clone_flags & CLONE_PARENT) {
		ancestor = parent_task->real_parent;
		child_count_threshold = 1;
	} else {
		ancestor = parent_task;
		child_count_threshold = 0;
	}

	if (!ancestor) {
		read_unlock(&tasklist_lock);
		return 0;
	}

	while (true) {
		if (count_children_upto2_locked(&ancestor->children) > child_count_threshold) {
			break;
		}
		if (!ancestor->real_parent || ancestor->real_parent == ancestor) {
			break;
		}
		ancestor = ancestor->real_parent;
		child_count_threshold = 1;
	}

	spin_lock_irqsave(&ancestor->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&ancestor->se.child_burst, now_ns)) {
		u32 recursion_depth = (READ_ONCE(sched_burst_fork_atavistic) > 1) ?
				      (READ_ONCE(sched_burst_fork_atavistic) - 1) : 0;
		__update_child_burst_topological_locked(ancestor, now_ns, recursion_depth,
							&dummy_children_count, &dummy_penalty_sum);
	}
	value = ancestor->se.child_burst.value;
	spin_unlock_irqrestore(&ancestor->se.child_burst.lock, irqflags);
	read_unlock(&tasklist_lock);

	return value;
}

static void __update_tg_burst_locked(struct task_struct *group_leader, u64 now_ns)
{
	u32 thread_count = 0, sum_penalties = 0;
	struct task_struct *thread_iter;

	for_each_thread(group_leader, thread_iter) {
		if (task_is_bore_eligible(thread_iter)) {
			u32 pen = READ_ONCE(thread_iter->se.burst_penalty);
			if (!__builtin_add_overflow(sum_penalties, pen, &sum_penalties)) {
				thread_count++;
			} else {
				sum_penalties = UINT_MAX;
				thread_count++;
			}
		}
	}
	update_burst_cache_locked(&group_leader->se.group_burst,
				  group_leader,
				  thread_count, sum_penalties, now_ns);
}

static u8 inherit_burst_tg(struct task_struct *parent_task, u64 now_ns)
{
	struct task_struct *group_leader = READ_ONCE(parent_task->group_leader);
	unsigned long irqflags;
	u8 value = 0;

	if (unlikely(!group_leader)) {
		return 0;
	}

	read_lock(&tasklist_lock);
	spin_lock_irqsave(&group_leader->se.group_burst.lock, irqflags);
	if (burst_cache_expired(&group_leader->se.group_burst, now_ns)) {
		__update_tg_burst_locked(group_leader, now_ns);
	}
	value = group_leader->se.group_burst.value;
	spin_unlock_irqrestore(&group_leader->se.group_burst.lock, irqflags);
	read_unlock(&tasklist_lock);

	return value;
}

void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
		      u64 clone_flags, u64 now_ns)
{
	struct sched_entity *se_new = &p_new_task->se;
	u8 inherited_penalty = 0;

	init_task_burst_cache_lock(p_new_task);

	se_new->burst_time = 0;
	se_new->curr_burst_penalty = 0;
	se_new->prev_burst_penalty = 0;
	se_new->burst_penalty = 0;
	se_new->burst_score = 0;
	se_new->child_burst.timestamp = 0;
	se_new->child_burst.value = 0;
	se_new->child_burst.count = 0;
	se_new->group_burst.timestamp = 0;
	se_new->group_burst.value = 0;
	se_new->group_burst.count = 0;

	if (!task_is_bore_eligible(p_new_task)) {
		return;
	}

	if (clone_flags & CLONE_THREAD) {
		inherited_penalty = inherit_burst_tg(p_parent, now_ns);
	} else {
		if (READ_ONCE(sched_burst_fork_atavistic) == 0) {
			inherited_penalty = 0;
		} else if (READ_ONCE(sched_burst_fork_atavistic) == 1) {
			inherited_penalty = inherit_burst_direct(p_parent, now_ns, clone_flags);
		} else {
			inherited_penalty = inherit_burst_topological(p_parent, now_ns, clone_flags);
		}
	}

	se_new->prev_burst_penalty = inherited_penalty;
	se_new->burst_penalty      = inherited_penalty;
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

/* ================================================================== */
/*                            Reset                                   */
/* ================================================================== */
void reset_task_bore(struct task_struct *p)
{
	struct sched_entity *se;

	if (unlikely(!p)) {
		return;
	}

	se = &p->se;

	se->burst_time = 0;
	se->prev_burst_penalty = 0;
	se->curr_burst_penalty = 0;
	se->burst_penalty = 0;
	se->burst_score = 0;

	se->child_burst.value     = 0;
	se->child_burst.count     = 0;
	se->child_burst.timestamp = 0;

	se->group_burst.value     = 0;
	se->group_burst.count     = 0;
	se->group_burst.timestamp = 0;
}
EXPORT_SYMBOL_GPL(reset_task_bore);

static void reset_all_task_weights_for_bore_toggle(void)
{
	struct task_struct *g, *t;
	struct rq *rq;
	struct rq_flags rf;

	rcu_read_lock();
	for_each_process(g) {
		for_each_thread(g, t) {
			if (!task_is_bore_eligible(t)) {
				continue;
			}
			if (!tryget_task_struct(t)) {
				continue;
			}

			rq = task_rq_lock(t, &rf);
			if (rq && t->on_rq) {
				update_rq_clock(rq);
				reweight_task_by_prio(t, effective_prio(t));
			}
			task_rq_unlock(rq, t, &rf);
			put_task_struct(t);
		}
	}
	rcu_read_unlock();
}

/* ================================================================== */
/*                            Sysctl                                  */
/* ================================================================== */
#ifdef CONFIG_SYSCTL

/* Sysctl bounds */
static const int bore_sysctl_val_three        = 3;
static const int bore_sysctl_val_nicew        = NICE_WIDTH;
static const int bore_sysctl_val_smooth_max   = 10;
static const int bore_sysctl_val_offset_max   = 64;
static const int bore_sysctl_val_scale_max    = BORE_ORIG_BURST_PENALTY_SCALE * 4;
static const int bore_sysctl_val_pct_max      = 200;
static const int bore_sysctl_val_stopcnt_max  = 4096;
static const int bore_sysctl_val_aversion_max = NICE_WIDTH / 2;
static const int bore_sysctl_val_100_max      = 100;

static int
sched_bore_toggle_sysctl_handler(const struct ctl_table *table,
				 int write, void *buffer,
				 size_t *lenp, loff_t *ppos)
{
	u8 old_val = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old_val != *(u8 *)table->data) {
		if (*(u8 *)table->data) {
			static_branch_enable(&bore_enable_key);
		} else {
			static_branch_disable(&bore_enable_key);
		}
		reset_all_task_weights_for_bore_toggle();
	}
	return ret;
}

#ifdef CONFIG_X86
static void bore_itd_update_key_handler(void)
{
	if (bore_itd_is_available()) {
		static_branch_enable(&bore_itd_key);
	} else {
		static_branch_disable(&bore_itd_key);
	}
}

static int bore_itd_enable_sysctl_handler(const struct ctl_table *table,
					  int write, void *buffer,
					  size_t *lenp, loff_t *ppos)
{
	u8 old_val = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old_val != *(u8 *)table->data) {
		bore_itd_update_key_handler();
	}
	return ret;
}
#endif

static int bore_u8_sysctl_gen_handler(const struct ctl_table *table,
				      int write, void *buffer,
				      size_t *lenp, loff_t *ppos)
{
	u8  old = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);

	if (!ret && write && old != *(u8 *)table->data) {
		bore_bump_penalty_gen();
	}
	return ret;
}

static int bore_uint_sysctl_gen_handler(const struct ctl_table *table,
					int write, void *buffer,
					size_t *lenp, loff_t *ppos)
{
	u32 old = *(u32 *)table->data;
	int  ret = proc_douintvec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old != *(u32 *)table->data) {
		bore_bump_penalty_gen();
	}
	return ret;
}

/* Sysctl table (multi-arch safe) */
static struct ctl_table bore_sysctls[] = {
	{
		.procname     = "sched_bore",
		.data         = &sched_bore,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = sched_bore_toggle_sysctl_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_ONE,
	},
	{
		.procname     = "sched_burst_exclude_kthreads",
		.data         = &sched_burst_exclude_kthreads,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_ONE,
	},
	{
		.procname     = "sched_burst_smoothness_long",
		.data         = &sched_burst_smoothness_long,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_smoothness_short",
		.data         = &sched_burst_smoothness_short,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_fork_atavistic",
		.data         = &sched_burst_fork_atavistic,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_three,
	},
	{
		.procname     = "sched_burst_parity_threshold",
		.data         = &sched_burst_parity_threshold,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_nicew,
	},
	{
		.procname     = "sched_burst_penalty_offset",
		.data         = &sched_burst_penalty_offset,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_offset_max,
	},
	{
		.procname     = "sched_burst_penalty_scale",
		.data         = &sched_burst_penalty_scale,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_scale_max,
	},
	{
		.procname     = "sched_burst_cache_stop_count",
		.data         = &sched_burst_cache_stop_count,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_stopcnt_max,
	},
	{
		.procname     = "sched_burst_cache_lifetime",
		.data         = &sched_burst_cache_lifetime,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_INT_MAX,
	},
	{
		.procname     = "sched_deadline_boost_mask",
		.data         = &sched_deadline_boost_mask,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_INT_MAX,
	},
	{
		.procname     = "sched_burst_core_aware_penalty",
		.data         = &sched_burst_core_aware_penalty,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_ONE,
	},
	{
		.procname     = "sched_burst_core_aware_smoothing",
		.data         = &sched_burst_core_aware_smoothing,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_ONE,
	},
	{
		.procname     = "sched_burst_penalty_pcore_scale_pct",
		.data         = &sched_burst_penalty_pcore_scale_pct,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname     = "sched_burst_penalty_ecore_scale_pct",
		.data         = &sched_burst_penalty_ecore_scale_pct,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname     = "sched_burst_smoothness_long_p",
		.data         = &sched_burst_smoothness_long_p,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_smoothness_short_p",
		.data         = &sched_burst_smoothness_short_p,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_smoothness_long_e",
		.data         = &sched_burst_smoothness_long_e,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_smoothness_short_e",
		.data         = &sched_burst_smoothness_short_e,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_ecore_aversion_penalty",
		.data         = &sched_burst_ecore_aversion_penalty,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_aversion_max,
	},
#ifdef CONFIG_X86
	{
		.procname     = "sched_burst_itd_enable",
		.data         = &sched_burst_itd_enable,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = bore_itd_enable_sysctl_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = SYSCTL_ONE,
	},
	{
		.procname     = "sched_burst_itd_cap_pct",
		.data         = &sched_burst_itd_cap_pct,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_pcore_pct_c0",
		.data         = &sched_burst_itd_bias_pcore_pct[0],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_pcore_pct_c1",
		.data         = &sched_burst_itd_bias_pcore_pct[1],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_pcore_pct_c2",
		.data         = &sched_burst_itd_bias_pcore_pct[2],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_pcore_pct_c3",
		.data         = &sched_burst_itd_bias_pcore_pct[3],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_ecore_pct_c0",
		.data         = &sched_burst_itd_bias_ecore_pct[0],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_ecore_pct_c1",
		.data         = &sched_burst_itd_bias_ecore_pct[1],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_ecore_pct_c2",
		.data         = &sched_burst_itd_bias_ecore_pct[2],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_bias_ecore_pct_c3",
		.data         = &sched_burst_itd_bias_ecore_pct[3],
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_100_max,
	},
	{
		.procname     = "sched_burst_itd_async_interval_ms",
		.data         = &sched_burst_itd_async_interval_ms,
		.maxlen       = sizeof(u32),
		.mode         = 0644,
		.proc_handler = proc_douintvec,
	},
#endif
	{ .procname = NULL }
};

static struct ctl_table_header *bore_sysctl_header_ptr;

#ifdef CONFIG_X86
#define BORE_SYSCTLS_BASE_COUNT 20
#define BORE_SYSCTLS_COUNT      (BORE_SYSCTLS_BASE_COUNT + 11)
#else
#define BORE_SYSCTLS_BASE_COUNT 20
#define BORE_SYSCTLS_COUNT      BORE_SYSCTLS_BASE_COUNT
#endif

static int __init bore_sysctl_init_func(void)
{
	/*
	 * Use register_sysctl_sz() with an explicit number of entries,
	 * excluding the terminating NULL sentinel. This is robust across
	 * configuration variants (CONFIG_X86 on/off).
	 */
	bore_sysctl_header_ptr = register_sysctl_sz("kernel", bore_sysctls, BORE_SYSCTLS_COUNT);
	if (!bore_sysctl_header_ptr) {
		pr_err("failed to register sysctl table\n");
		return -ENOMEM;
	}
	return 0;
}
#endif /* CONFIG_SYSCTL */

/* ================================================================== */
/*                              Init                                  */
/* ================================================================== */
void __init sched_bore_init(void)
{
	pr_info("BORE v%s initialising (HZ=%d)\n", SCHED_BORE_VERSION, HZ);

	bore_build_lookup_tables();

	if (is_intel_hybrid()) {
		pr_info("Intel Hybrid CPU detected – applying tuned defaults.\n");
		WRITE_ONCE(sched_burst_parity_threshold,
			   READ_ONCE(sched_burst_parity_threshold) + 2);
		WRITE_ONCE(sched_burst_smoothness_short,
			   max_t(u8, 1, (u8)BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1));
		WRITE_ONCE(sched_burst_fork_atavistic, 1); /* direct inheritance */
		WRITE_ONCE(sched_burst_exclude_kthreads, 1);
	}

	reset_task_bore(&init_task);
	spin_lock_init(&init_task.se.child_burst.lock);
	spin_lock_init(&init_task.se.group_burst.lock);

	pr_info("Early init done – waiting for full CPU topology.\n");
}

#ifdef CONFIG_X86
static int __init bore_itd_boot_key_init(void)
{
	if (bore_itd_is_available()) {
		static_branch_enable(&bore_itd_key);
	} else {
		static_branch_disable(&bore_itd_key);
	}
	return 0;
}
early_initcall(bore_itd_boot_key_init);
#endif

static int __init bore_late_topology_final_check(void)
{
	bore_check_and_update_topology_features();
	return 0;
}

#else /* !CONFIG_SCHED_BORE */

void __init sched_bore_init(void) { }

#ifndef _LINUX_SCHED_BORE_H_STUBS_DEFINED
#define _LINUX_SCHED_BORE_H_STUBS_DEFINED
#define BORE_SCHED_STUB(func_name, ...) \
void func_name(__VA_ARGS__) { } \
EXPORT_SYMBOL_GPL(func_name)
BORE_SCHED_STUB(update_burst_score, struct sched_entity *se);
BORE_SCHED_STUB(update_burst_penalty, struct sched_entity *se);
BORE_SCHED_STUB(restart_burst, struct sched_entity *se);
BORE_SCHED_STUB(restart_burst_rescale_deadline, struct sched_entity *se);
void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
		      u64 clone_flags, u64 now_ns) { }
EXPORT_SYMBOL_GPL(sched_clone_bore);
BORE_SCHED_STUB(reset_task_bore, struct task_struct *p);
#endif /* _LINUX_SCHED_BORE_H_STUBS_DEFINED */

#endif /* CONFIG_SCHED_BORE */

#ifdef CONFIG_SCHED_BORE
core_initcall(bore_topology_init);
late_initcall_sync(bore_late_topology_final_check);
#ifdef CONFIG_SYSCTL
late_initcall_sync(bore_sysctl_init_func);
#endif
#endif /* CONFIG_SCHED_BORE */
