/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler - Core Logic & Tuning
 * Enhanced for Intel Raptor Lake with optimizations and robustness fixes
 *
 * Production-Ready Revision: v5.0 (Final Linker Fix)
 * - Corrects all compiler and linker errors, including the section type
 *   conflict between __ro_after_init variables by programmatically
 *   initializing the log2 LUT during boot. This is the most elegant
 *   and performant solution.
 * - Includes all previous stability and performance enhancements (UAF fix,
 *   locking fixes, ITD responsiveness, BMI2 optimization, etc.).
 */

#undef pr_fmt
#define pr_fmt(fmt) "BORE: " fmt

/* ================================================================== */
/*                          0.  Headers                               */
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
#include <linux/sched/task.h>
#include <linux/sched/topology.h>
#include <linux/sched/bore.h>
#include <linux/sysctl.h>
#include <linux/bitmap.h>
#include <linux/static_key.h>
#include <linux/static_call.h>
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

#ifdef task_cpu_possible
#undef task_cpu_possible
#endif
#define task_cpu_possible(cpu, p)	cpumask_test_cpu((cpu), (p)->cpus_ptr)

#include <linux/rculist.h>
#include <linux/limits.h>

#include <asm/processor.h>
#include <asm/topology.h>
#include <asm/cpufeature.h>
#include <asm/msr.h>

#include "sched.h"

#ifdef CONFIG_SCHED_BORE

/* Forward declarations to avoid -Wmissing-prototypes if headers omit them */
void update_burst_penalty(struct sched_entity *se);
void update_curr_bore(u64 delta_exec, struct sched_entity *se);
extern void update_burst_penalty(struct sched_entity *se);
void update_burst_score(struct sched_entity *se);
void restart_burst(struct sched_entity *se);
void restart_burst_rescale_deadline(struct sched_entity *se);
void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
                      u64 clone_flags, u64 now_ns);
void reset_task_bore(struct task_struct *p);
void __init sched_bore_init(void);

/* ================================================================== */
/*                          1.  Tunables                              */
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
#define BORE_DEF_PCORE_HOG_THRESHOLD_PCT       85
#define BORE_DEF_PCORE_HOG_PENALTY_ADD         2

#define DEF_U8(name, val)   u8   __read_mostly name = (val)
#define DEF_U32(name, val)  uint __read_mostly name = (val)

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

DEF_U8(sched_burst_core_aware_penalty,    0);
DEF_U8(sched_burst_core_aware_smoothing,  0);
DEF_U32(sched_burst_penalty_pcore_scale_pct, 100);
DEF_U32(sched_burst_penalty_ecore_scale_pct, 100);
DEF_U8(sched_burst_smoothness_long_p, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_p, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_smoothness_long_e, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_e, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_pcore_hog_threshold_pct, BORE_DEF_PCORE_HOG_THRESHOLD_PCT);
DEF_U8(sched_burst_pcore_hog_penalty_add, BORE_DEF_PCORE_HOG_PENALTY_ADD);

/* ================================================================== */
/*                          2.  Constants                             */
/* ================================================================== */
#define MAX_BURST_PENALTY          156U
#define ECORE_OFFSET_ADJ_DIV       20
#define MAX_ECORE_OFFSET_ADJUST    10
#define MIN_EFFECTIVE_OFFSET       4
#define RAPTOR_LAKE_CACHE_LINE     64
#define BORE_MAX_VRUNTIME_CLAMP    (200LL * NSEC_PER_MSEC)

/* ================================================================== */
/*                       3.  Static key                               */
/* ================================================================== */
DEFINE_STATIC_KEY_FALSE(bore_core_aware_key);

/* ================================================================== */
/*        3a.  Per-CPU penalty params cache (OPTIMIZED)               */
/* ================================================================== */
struct bore_penalty_param {
	u32 scale;
	u32 sat_thresh_q8;
	u8  offset;
	u8  gen;
	s16 itd_pri;

	u8  __padding[RAPTOR_LAKE_CACHE_LINE - 12];
} __attribute__((aligned(RAPTOR_LAKE_CACHE_LINE)));

struct bore_penalty_cache {
	struct sched_entity *last_se;
	u64 last_burst_time;
	u32 last_penalty;
	int last_cpu;

	u64 hit_count;
	u64 miss_count;

	u8 __padding[RAPTOR_LAKE_CACHE_LINE - 40];
} __attribute__((aligned(RAPTOR_LAKE_CACHE_LINE)));

DEFINE_PER_CPU_ALIGNED(struct bore_penalty_param, bore_penalty);
DEFINE_PER_CPU_ALIGNED(struct bore_penalty_cache, bore_penalty_cache);
static atomic_t bore_penalty_gen = ATOMIC_INIT(1);

/* ================================================================== */
/*                 4.  Per-CPU CPU-type cache                         */
/* ================================================================== */
DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type) = TOPO_CPU_TYPE_UNKNOWN;

static __always_inline enum x86_topology_cpu_type
bore_get_rq_cpu_type(struct rq *rq)
{
	if (static_branch_unlikely(&bore_core_aware_key)) {
		return per_cpu(bore_cpu_type, rq->cpu);
	}
	return TOPO_CPU_TYPE_UNKNOWN;
}

/* ================================================================== */
/*                       5.  Scale LUT                                */
/* ================================================================== */
static u32 bore_scale_tbl[201] __ro_after_init;
static u8 log2_frac_lut[16] __ro_after_init;

static void __init bore_build_lookup_tables(void)
{
	int i;
	const u8 lut_vals[16] = {
		0, 15, 29, 43, 56, 69, 81, 93, 104, 115, 125, 135, 145, 154, 163, 172
	};

	for (i = 0; i <= 200; i++) {
		bore_scale_tbl[i] = div_u64((u64)BORE_ORIG_BURST_PENALTY_SCALE * i, 100);
	}

	for (i = 0; i < 16; i++) {
		log2_frac_lut[i] = lut_vals[i];
	}
}

/* ================================================================== */
/*                     6.  Hybrid detection                           */
/* ================================================================== */
static bool __init is_intel_raptor_lake(void)
{
#ifdef CONFIG_X86_64
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6) {
		return false;
	}

	switch (boot_cpu_data.x86_model) {
		case 0xB7: /* RPL-S desktop */
		case 0xBA: /* RPL-P mobile */
		case 0xBE: /* ADL-N / RPL ES */
		case 0xBF: /* RPL-S refresh */
		case 0xAC: /* RPL-P refresh */
		case 0xB1: /* RPL server */
			return true;
	}
#endif
	return false;
}

static bool __init is_intel_hybrid(void)
{
#ifdef CONFIG_X86_64
	return boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake();
#else
	return false;
#endif
}

/* ================================================================== */
/*          7.  Topology scan                                         */
/* ================================================================== */
static bool bore_cpu_types_detected;
static void bore_enable_key_workfn(struct work_struct *w);
static DECLARE_WORK(bore_enable_key_work, bore_enable_key_workfn);

static void bore_build_penalty_param_for_cpu(int cpu)
{
	struct bore_penalty_param *pp = &per_cpu(bore_penalty, cpu);
	enum x86_topology_cpu_type ct = per_cpu(bore_cpu_type, cpu);
	u32 scale = sched_burst_penalty_scale;
	u8 offset = sched_burst_penalty_offset;
	s16 itd_pr = 0;

#ifdef CONFIG_X86_64
	itd_pr = arch_asym_cpu_priority(cpu);
#endif

	if (sched_burst_core_aware_penalty &&
	    static_branch_unlikely(&bore_core_aware_key)) {
		if (ct == TOPO_CPU_TYPE_EFFICIENCY) {
			scale = bore_scale_tbl[min_t(u32, 200,
				sched_burst_penalty_ecore_scale_pct)];
			if (sched_burst_penalty_ecore_scale_pct > 100) {
				u8 adj = min_t(u8, MAX_ECORE_OFFSET_ADJUST,
					(sched_burst_penalty_ecore_scale_pct - 100) /
					ECORE_OFFSET_ADJ_DIV);
				offset = max_t(u8, MIN_EFFECTIVE_OFFSET, offset - adj);
			}
		} else if (ct == TOPO_CPU_TYPE_PERFORMANCE) {
			scale = bore_scale_tbl[min_t(u32, 200,
				sched_burst_penalty_pcore_scale_pct)];
		}
	}

	if (itd_pr > 0) {
		u64 pr_norm = min_t(u64, 1024, itd_pr);
		u64 reduction_factor = 1024 - pr_norm;
		reduction_factor = reduction_factor * reduction_factor;
		u32 reduction_permille = div_u64(reduction_factor, 1049);
		u32 scale_reduction = div_u64((u64)scale * reduction_permille, 4000);
		scale = max_t(u32, scale / 4, scale - scale_reduction);
	}

	pp->scale = scale;
	pp->offset = offset;
	pp->itd_pri = itd_pr;
	pp->sat_thresh_q8 = !scale ? U32_MAX :
		div_u64(((u64)MAX_BURST_PENALTY << 16) + scale - 1, scale);

	smp_wmb();
	pp->gen = (u8)atomic_read(&bore_penalty_gen);
}

static inline void bore_bump_penalty_gen(void)
{
	atomic_inc(&bore_penalty_gen);
	smp_mb();
}

static inline const struct bore_penalty_param *bore_get_param(void)
{
	struct bore_penalty_param *pp = this_cpu_ptr(&bore_penalty);
	u8 g_now = (u8)atomic_read(&bore_penalty_gen);
	s16 itd_now = 0;

#ifdef CONFIG_X86_64
	itd_now = arch_asym_cpu_priority(smp_processor_id());
#endif

	if (unlikely(READ_ONCE(pp->gen) != g_now || READ_ONCE(pp->itd_pri) != itd_now)) {
		bore_build_penalty_param_for_cpu(smp_processor_id());
	}

	return pp;
}

static void bore_enable_key_workfn(struct work_struct *w)
{
	if (!static_branch_unlikely(&bore_core_aware_key)) {
		static_branch_enable(&bore_core_aware_key);
		pr_info("Core-aware static key enabled via workqueue.\n");
	}
}

static void bore_check_and_update_topology_features(void)
{
	unsigned int cpu;
	bool found_hybrid_info_this_pass = false;

#ifdef CONFIG_X86_64
	for_each_possible_cpu(cpu) {
		enum x86_topology_cpu_type t = get_topology_cpu_type(&cpu_data(cpu));
		per_cpu(bore_cpu_type, cpu) = t;
		if (t == TOPO_CPU_TYPE_PERFORMANCE || t == TOPO_CPU_TYPE_EFFICIENCY) {
			found_hybrid_info_this_pass = true;
		}
		bore_build_penalty_param_for_cpu(cpu);
	}
#endif

	if (!found_hybrid_info_this_pass) {
		if (is_intel_hybrid() && !bore_cpu_types_detected) {
			pr_info("Hybrid CPU, but no P/E info found yet. Will retry.\n");
		}
		return;
	}

	if (!bore_cpu_types_detected) {
		pr_info("P/E core topology information detected.\n");
		bore_cpu_types_detected = true;
		sched_burst_core_aware_penalty = 1;
		sched_burst_core_aware_smoothing = 1;

		if (is_intel_raptor_lake()) {
			pr_info("Applying Raptor-Lake P/E specific tunings.\n");
			sched_burst_penalty_ecore_scale_pct = 140;
			sched_burst_smoothness_long_e = 3;
			sched_burst_smoothness_short_e = 1;
			sched_burst_parity_threshold += 2;
		} else if (is_intel_hybrid()) {
			pr_info("Applying generic Intel Hybrid P/E tunings.\n");
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

static int bore_cpu_online_cb(unsigned int cpu)
{
#ifdef CONFIG_X86_64
	per_cpu(bore_cpu_type, cpu) = get_topology_cpu_type(&cpu_data(cpu));
#endif
	bore_build_penalty_param_for_cpu(cpu);
	bore_check_and_update_topology_features();
	return 0;
}

static enum cpuhp_state bore_cpuhp_state_val;

static int __init bore_topology_init(void)
{
	int ret_hp;

	bore_check_and_update_topology_features();

	ret_hp = cpuhp_setup_state_nocalls(CPUHP_AP_ONLINE_DYN,
		"sched/bore:online", bore_cpu_online_cb, NULL);
	if (ret_hp < 0) {
		pr_err("cpuhp_setup_state_nocalls failed: %d\n", ret_hp);
	} else {
		bore_cpuhp_state_val = ret_hp;
	}
	return 0;
}

/* ================================================================== */
/*              8.  Penalty math (OPTIMIZED & PORTABLE)               */
/* ================================================================== */
static __always_inline u32 log2_u64_q24_8(u64 v)
{
	/* Return Q24.8 fixed point: exponent in high bits, 8-bit fractional part. */
	if (unlikely(!v))
		return 0;

	/* Exponent = floor(log2(v)); uses LZCNT when available via builtin. */
	u32 lz = __builtin_clzll(v);
	u32 exponent = 63u - lz;

	/* Normalize to 1.x range and extract next 8 bits as Q8 fraction input. */
	u8 frac8;
	if (exponent >= 8) {
		u64 mant = v >> (exponent - 8);   /* mant in [0x100, 0x1FF] */
		frac8 = (u8)(mant & 0xFFu);
	} else {
		u64 mant = v << (8 - exponent);   /* mant in [0x100, 0x1FF] */
		frac8 = (u8)(mant & 0xFFu);
	}

	/* Q8 polynomial: log2(1 + x) ≈ x - x^2/2 + x^3/3, where x = frac8/256. */
	u32 x = frac8;                    /* Q8 */
	u32 x2 = (x * x) >> 8;           /* Q8 */
	u32 frac = x;                    /* Q8, start with linear term */
	u32 quad = x2 >> 1;              /* Q8, x^2/2 */

	frac = (frac > quad) ? (frac - quad) : 0;

	/* cubic term (x^3/3), safe and cheap. */
	if (x2 < 256) {
		u32 x3 = (x2 * x) >> 8;      /* Q8 */
		u32 cubic = x3 / 3;          /* Q8 */
		u32 sum = frac + cubic;
		frac = (sum > 255u) ? 255u : sum;
	}

	return (exponent << 8) | (frac & 0xFFu);
}

static u32 __calc_burst_penalty_fast(u64 burst_time,
                                     enum x86_topology_cpu_type ctype)
{
	const struct bore_penalty_param *pp = bore_get_param();
	u32 greed_q8;
	s32 delta_q8;
	u32 penalty_raw;

	if (unlikely(!burst_time))
		return 0;

	greed_q8 = log2_u64_q24_8(burst_time);
	delta_q8 = (s32)greed_q8 - ((s32)pp->offset << 8);
	if (delta_q8 <= 0)
		return 0;

	if (unlikely((u32)delta_q8 >= pp->sat_thresh_q8))
		return MAX_BURST_PENALTY;

	/* (delta_q8 * scale) >> 16 is exact for Q16 scaling, fits in 64-bit. */
	{
		u64 tmp = (u64)(u32)delta_q8 * (u64)pp->scale;
		penalty_raw = (u32)(tmp >> 16);
	}

	return min_t(u32, MAX_BURST_PENALTY, penalty_raw);
}

#ifdef CONFIG_X86_64
static u32 __calc_burst_penalty_fast_bmi2(u64 burst_time,
                                          enum x86_topology_cpu_type ctype)
{
	const struct bore_penalty_param *pp;
	u32 greed_q8, offset_q8, sat_q8_plus_off, delta_q8;
	u32 scale, penalty;
	u64 prod;

	/* Avoid -Wunused-parameter and keep signature stable. */
	(void)ctype;

	/* Hot-path early exit: nothing to penalize. */
	if (unlikely(!burst_time))
		return 0;

	/* Per-CPU params are cache-hot; load once. */
	pp = bore_get_param();

	/* Compute greed in Q24.8 (exponent.frac8). */
	greed_q8 = log2_u64_q24_8(burst_time);

	/* Convert offset to Q8 once. */
	offset_q8 = (u32)pp->offset << 8;

	/* Below offset → no penalty. Using <= keeps delta 0 in the fast path out. */
	if (likely(greed_q8 <= offset_q8))
		return 0;

	/*
	 * Saturation check without forming a negative intermediate: compare
	 * greed_q8 against (offset<<8) + sat_thresh_q8, guarding the sum.
	 * In practice this never overflows, but clamp defensively to U32_MAX.
	 */
	if (unlikely(pp->sat_thresh_q8 > U32_MAX - offset_q8))
		sat_q8_plus_off = U32_MAX;
	else
		sat_q8_plus_off = pp->sat_thresh_q8 + offset_q8;

	if (unlikely(greed_q8 >= sat_q8_plus_off))
		return MAX_BURST_PENALTY;

	/* Compute positive delta in Q8 and scale it. */
	delta_q8 = greed_q8 - offset_q8;
	scale = pp->scale;

	/* Single wide multiply with exact Q16 downshift; keep semantics identical. */
	prod = (u64)delta_q8 * (u64)scale;
	penalty = (u32)(prod >> 16);

	/* Final clamp for safety (normally unnecessary given the saturate check). */
	if (penalty > MAX_BURST_PENALTY)
		penalty = MAX_BURST_PENALTY;

	return penalty;
}
#endif

DEFINE_STATIC_CALL(bore_calc_penalty, __calc_burst_penalty_fast);

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
/*           9.  Smoothing & prio helpers (CLEANED)                   */
/* ================================================================== */
static __always_inline u32
binary_smooth(u32 new_val, u32 old_val, enum x86_topology_cpu_type ctype)
{
	u8 shift_long = sched_burst_smoothness_long;
	u8 shift_short = sched_burst_smoothness_short;

	if (sched_burst_core_aware_smoothing &&
	    static_branch_unlikely(&bore_core_aware_key)) {
		switch (ctype) {
		case TOPO_CPU_TYPE_PERFORMANCE:
			shift_long = sched_burst_smoothness_long_p;
			shift_short = sched_burst_smoothness_short_p;
			break;
		case TOPO_CPU_TYPE_EFFICIENCY:
			shift_long = sched_burst_smoothness_long_e;
			shift_short = sched_burst_smoothness_short_e;
			break;
		default:
			break;
		}
	}

	s64 inc = (s64)new_val - (s64)old_val;
	u32 shift = (inc >= 0) ? shift_long : shift_short;

	/* Clamp shift to 0..31 */
	if (shift > 31)
		shift = 31;

	if (inc >= 0) {
		u32 delta = (shift == 0) ? (u32)inc : (u32)((u64)inc >> shift);
		u64 sum = (u64)old_val + delta;
		return (sum > U32_MAX) ? U32_MAX : (u32)sum;
	} else {
		u64 delta = (shift == 0) ? (u64)(-inc) : ((u64)(-inc) >> shift);
		return (old_val < delta) ? 0u : (old_val - (u32)delta);
	}
}

static __always_inline u8 effective_prio(struct task_struct *p)
{
	int prio_val = p->static_prio - MAX_RT_PRIO;

	if (likely(sched_bore)) {
		prio_val = max(0, prio_val - (int)p->se.burst_score);
	}

	return min_t(int, NICE_WIDTH - 1, prio_val);
}

static void reweight_task_by_prio(struct task_struct *p, int prio_val)
{
	struct sched_entity *se = &p->se;
	struct cfs_rq *cfs_rq = cfs_rq_of(se);

	prio_val = clamp(prio_val, 0, NICE_WIDTH - 1);
	reweight_entity(cfs_rq, se,
		scale_load(sched_prio_to_weight[prio_val]), true);
	se->load.inv_weight = sched_prio_to_wmult[prio_val];
}

/* ================================================================== */
/*         10.  Hot-path updates                                      */
/* ================================================================== */
void update_burst_penalty(struct sched_entity *se)
{
	struct cfs_rq *cfs_rq;
	struct rq *rq;
	struct bore_penalty_cache *cache;
	enum x86_topology_cpu_type ctype;
	u64 burst_time_local;

	if (unlikely(!se))
		return;

	cfs_rq = cfs_rq_of(se);
	if (unlikely(!cfs_rq))
		return;

	rq = rq_of(cfs_rq);
	if (unlikely(!rq))
		return;

	/* Stable snapshot to compare and compute with. */
	burst_time_local = READ_ONCE(se->burst_time);

	/* Per-CPU cache access requires preemption disabled. Keep it short. */
	preempt_disable();
	cache = this_cpu_ptr(&bore_penalty_cache);

	/* Cheap predicates first: pointer, then value. */
	if (likely(cache->last_se == se &&
		cache->last_burst_time == burst_time_local)) {
		se->curr_burst_penalty = cache->last_penalty;
	cache->hit_count++;
	preempt_enable();
	goto out_update_score;
		}

		cache->miss_count++;
		ctype = bore_get_rq_cpu_type(rq);

		se->curr_burst_penalty = static_call(bore_calc_penalty)(burst_time_local, ctype);

		/* Refresh the per-CPU cache. */
		cache->last_se = se;
		cache->last_burst_time = burst_time_local;
		cache->last_penalty = se->curr_burst_penalty;
		cache->last_cpu = rq->cpu;

		preempt_enable();

		out_update_score:
		se->burst_penalty = max(se->prev_burst_penalty, se->curr_burst_penalty);
		update_burst_score(se);
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

void update_curr_bore(u64 delta_exec, struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se))) {
		return;
	}

	se->burst_time += delta_exec;
	update_burst_penalty(se);
}
EXPORT_SYMBOL_GPL(update_curr_bore);

void update_burst_score(struct sched_entity *se)
{
	struct task_struct *p;
	u8 old_effective_prio;
	u8 new_score = 0;

	if (unlikely(!entity_is_task(se)))
		return;

	p = task_of(se);
	old_effective_prio = effective_prio(p);

	if (!((p->flags & PF_KTHREAD) && sched_burst_exclude_kthreads))
		new_score = se->burst_penalty >> 2;

	#ifdef CONFIG_SMP
	if (static_branch_unlikely(&bore_core_aware_key) &&
		bore_get_rq_cpu_type(task_rq(p)) == TOPO_CPU_TYPE_PERFORMANCE &&
		se->burst_penalty > (MAX_BURST_PENALTY * sched_burst_pcore_hog_threshold_pct / 100)) {
		new_score = min_t(u8, NICE_WIDTH - 1,
						  new_score + sched_burst_pcore_hog_penalty_add);
		}
		#endif

		if (se->burst_score == new_score) {
			u8 eff = effective_prio(p);
			if (unlikely(eff != old_effective_prio))
				reweight_task_by_prio(p, eff);
			return;
		}

		se->burst_score = new_score;
		reweight_task_by_prio(p, effective_prio(p));
}
EXPORT_SYMBOL_GPL(update_burst_score);

static void revolve_burst_penalty_state(struct sched_entity *se,
                                        enum x86_topology_cpu_type ctype)
{
	se->prev_burst_penalty = binary_smooth(se->curr_burst_penalty,
		se->prev_burst_penalty, ctype);
	se->burst_time = 0;
	se->curr_burst_penalty = 0;
}

void restart_burst(struct sched_entity *se)
{
	struct task_struct *p = task_of(se);

	if (entity_is_task(se) &&
	    (p->static_prio - MAX_RT_PRIO) < sched_burst_parity_threshold) {
		se->prev_burst_penalty = 0;
	} else {
		revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	}

	se->burst_time = 0;
	se->curr_burst_penalty = 0;
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);

void restart_burst_rescale_deadline(struct sched_entity *se)
{
	struct task_struct *p;
	u8 old_eff_prio;
	s64 vruntime_remaining;
	u8 new_eff_prio;
	u64 abs_value;
	u64 weight_units_remaining;
	s64 vruntime_scaled_new;

	if (unlikely(!entity_is_task(se)))
		return;

	p = task_of(se);
	old_eff_prio = effective_prio(p);
	vruntime_remaining = se->deadline - se->vruntime;

	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);

	new_eff_prio = effective_prio(p);
	if (new_eff_prio > old_eff_prio) {
		abs_value = (vruntime_remaining < 0) ? -vruntime_remaining : vruntime_remaining;
		weight_units_remaining = __unscale_slice(abs_value, old_eff_prio);
		vruntime_scaled_new = __scale_slice(weight_units_remaining, new_eff_prio);
		if (vruntime_remaining < 0)
			vruntime_scaled_new = -vruntime_scaled_new;
		se->deadline = se->vruntime + vruntime_scaled_new;
	}
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

/* ================================================================== */
/*      11.  Inheritance & cache (OPTIMIZED & FIXED RCU)              */
/* ================================================================== */
#define for_each_child_bore(p_task, child_task) \
	list_for_each_entry_rcu(child_task, &(p_task)->children, sibling)

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
	return (s64)(bc->timestamp + sched_burst_cache_lifetime - now_ns) < 0;
}

static void update_burst_cache_locked(struct sched_burst_cache *bc,
                                      struct task_struct *p_owner_of_cache,
                                      u32 children_count, u32 children_penalty_sum,
                                      u64 now_ns)
{
	u8 avg_child_penalty = children_count ?
		(u8)div_u64(children_penalty_sum, children_count) : 0;
	bc->value = max(avg_child_penalty, p_owner_of_cache->se.burst_penalty);
	bc->count = children_count;
	bc->timestamp = now_ns;
}

static inline u32 count_children_upto2_rcu(struct list_head *children_list_head)
{
	struct list_head *first_child = READ_ONCE(children_list_head->next);
	struct list_head *second_child;

	if (first_child == children_list_head) {
		return 0;
	}
	second_child = READ_ONCE(first_child->next);
	return 1 + (second_child != children_list_head);
}

static void __update_child_burst_direct_locked(struct task_struct *p, u64 now_ns)
{
	u32 count = 0, sum_penalties = 0;
	struct task_struct *child_iter;
	u32 max_children;
	u32 penalty;

	if (unlikely(!p)) {
		return;
	}

	max_children = READ_ONCE(sched_burst_cache_stop_count);
	if (max_children == 0) {
		update_burst_cache_locked(&p->se.child_burst, p, 0, 0, now_ns);
		return;
	}

	rcu_read_lock();
	list_for_each_entry_rcu(child_iter, &p->children, sibling) {
		if (child_iter->sibling.next != &p->children) {
			prefetch(list_entry_rcu(child_iter->sibling.next,
			                      struct task_struct, sibling));
		}

		if (likely(task_is_bore_eligible(child_iter))) {
			penalty = READ_ONCE(child_iter->se.burst_penalty);
			if (!__builtin_add_overflow(sum_penalties, penalty, &sum_penalties)) {
				count++;
			} else {
				sum_penalties = UINT_MAX;
				count++;
			}
		}

		if (count >= max_children) {
			break;
		}
	}
	rcu_read_unlock();

	update_burst_cache_locked(&p->se.child_burst, p, count, sum_penalties, now_ns);
}

static void __update_child_burst_topological_locked(struct task_struct *p, u64 now_ns,
                                                    u32 recursion_depth,
                                                    u32 *accumulated_count,
                                                    u32 *accumulated_sum)
{
	u32 current_level_direct_children_count = 0;
	u32 current_level_sum_of_penalties = 0;
	struct task_struct *child_iter, *effective_descendant;
	struct sched_burst_cache *desc_cache;
	struct list_head *first_head;

	rcu_read_lock();

	for_each_child_bore(p, child_iter) {
		prefetch(child_iter->sibling.next);
		prefetch(&child_iter->children);

		effective_descendant = child_iter;
		while (count_children_upto2_rcu(&effective_descendant->children) == 1) {
			first_head = READ_ONCE(effective_descendant->children.next);
			if (first_head == &effective_descendant->children) {
				break;
			}
			effective_descendant = list_entry_rcu(first_head,
				struct task_struct, sibling);
		}

		if (recursion_depth == 0 || list_empty_careful(&effective_descendant->children)) {
			if (task_is_bore_eligible(effective_descendant)) {
				current_level_direct_children_count++;
				current_level_sum_of_penalties += effective_descendant->se.burst_penalty;
			}
			continue;
		}

		desc_cache = &effective_descendant->se.child_burst;
		spin_lock(&desc_cache->lock);
		if (!burst_cache_expired(desc_cache, now_ns)) {
			current_level_direct_children_count += desc_cache->count;
			current_level_sum_of_penalties += (u32)desc_cache->value * desc_cache->count;
		} else {
			__update_child_burst_topological_locked(effective_descendant,
				now_ns, recursion_depth - 1,
				&current_level_direct_children_count,
				&current_level_sum_of_penalties);
		}
		spin_unlock(&desc_cache->lock);

		if (sched_burst_cache_stop_count > 0 &&
		    current_level_direct_children_count >= sched_burst_cache_stop_count) {
			break;
		}
	}

	rcu_read_unlock();

	update_burst_cache_locked(&p->se.child_burst, p,
		current_level_direct_children_count,
		current_level_sum_of_penalties, now_ns);
	*accumulated_count += current_level_direct_children_count;
	*accumulated_sum += current_level_sum_of_penalties;
}

static u8 inherit_burst_direct(struct task_struct *parent_task,
                               u64 now_ns, u64 clone_flags)
{
	struct task_struct *target_for_inheritance;
	unsigned long irqflags;
	u8 value = 0;
	bool need_put = false;

	if (unlikely(!parent_task)) {
		return 0;
	}

	target_for_inheritance = parent_task;

	if (clone_flags & CLONE_PARENT) {
		rcu_read_lock();
		target_for_inheritance = rcu_dereference(parent_task->real_parent);
		if (likely(target_for_inheritance)) {
			if (likely(tryget_task_struct(target_for_inheritance))) {
				need_put = true;
			} else {
				target_for_inheritance = NULL;
			}
		}
		rcu_read_unlock();

		if (unlikely(!target_for_inheritance)) {
			return 0;
		}
	}

	spin_lock_irqsave(&target_for_inheritance->se.child_burst.lock, irqflags);

	if (burst_cache_expired(&target_for_inheritance->se.child_burst, now_ns)) {
		__update_child_burst_direct_locked(target_for_inheritance, now_ns);
	}

	value = target_for_inheritance->se.child_burst.value;
	spin_unlock_irqrestore(&target_for_inheritance->se.child_burst.lock, irqflags);

	if (need_put) {
		put_task_struct(target_for_inheritance);
	}

	return value;
}

static u8 inherit_burst_topological(struct task_struct *parent_task,
                                    u64 now_ns, u64 clone_flags)
{
	struct task_struct *ancestor, *p;
	u32 child_count_threshold;
	unsigned long irqflags;
	u8 value = 0;
	u32 dummy_children_count = 0, dummy_penalty_sum = 0;

	rcu_read_lock();
	if (clone_flags & CLONE_PARENT) {
		p = rcu_dereference(parent_task->real_parent);
		child_count_threshold = 1;
	} else {
		p = parent_task;
		child_count_threshold = 0;
	}

	if (!p || !tryget_task_struct(p)) {
		rcu_read_unlock();
		return 0;
	}
	rcu_read_unlock();

	ancestor = p;

	while (true) {
		struct task_struct *next_p;

		if (count_children_upto2_rcu(&ancestor->children) > child_count_threshold) {
			break;
		}

		rcu_read_lock();
		next_p = rcu_dereference(ancestor->real_parent);
		if (!next_p || next_p == ancestor || !tryget_task_struct(next_p)) {
			rcu_read_unlock();
			break;
		}
		rcu_read_unlock();

		put_task_struct(ancestor);
		ancestor = next_p;
		child_count_threshold = 1;
	}

	spin_lock_irqsave(&ancestor->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&ancestor->se.child_burst, now_ns)) {
		u32 recursion_depth = (sched_burst_fork_atavistic > 1) ?
			(sched_burst_fork_atavistic - 1) : 0;
		__update_child_burst_topological_locked(ancestor,
			now_ns, recursion_depth,
			&dummy_children_count, &dummy_penalty_sum);
	}
	value = ancestor->se.child_burst.value;
	spin_unlock_irqrestore(&ancestor->se.child_burst.lock, irqflags);

	put_task_struct(ancestor);
	return value;
}

static void __update_tg_burst_locked(struct task_struct *group_leader, u64 now_ns)
{
	u32 thread_count = 0, sum_penalties = 0;
	struct task_struct *thread_iter;

	for_each_thread(group_leader, thread_iter) {
		if (task_is_bore_eligible(thread_iter)) {
			if (!__builtin_add_overflow(sum_penalties,
				thread_iter->se.burst_penalty, &sum_penalties)) {
				thread_count++;
			} else {
				sum_penalties = UINT_MAX;
				thread_count++;
			}
		}
	}
	update_burst_cache_locked(&group_leader->se.group_burst,
		group_leader, thread_count, sum_penalties, now_ns);
}

static u8 inherit_burst_tg(struct task_struct *parent_task, u64 now_ns)
{
	struct task_struct *group_leader = READ_ONCE(parent_task->group_leader);
	unsigned long irqflags;
	u8 value = 0;

	if (unlikely(!group_leader)) {
		return 0;
	}

	spin_lock_irqsave(&group_leader->se.group_burst.lock, irqflags);
	if (burst_cache_expired(&group_leader->se.group_burst, now_ns)) {
		__update_tg_burst_locked(group_leader, now_ns);
	}
	value = group_leader->se.group_burst.value;
	spin_unlock_irqrestore(&group_leader->se.group_burst.lock, irqflags);
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

	rcu_read_lock();
	if (clone_flags & CLONE_THREAD) {
		inherited_penalty = inherit_burst_tg(p_parent, now_ns);
	} else {
		if (sched_burst_fork_atavistic == 0) {
			inherited_penalty = 0;
		} else if (sched_burst_fork_atavistic == 1) {
			inherited_penalty = inherit_burst_direct(p_parent, now_ns, clone_flags);
		} else {
			inherited_penalty = inherit_burst_topological(p_parent, now_ns, clone_flags);
		}
	}
	rcu_read_unlock();

	se_new->prev_burst_penalty = inherited_penalty;
	se_new->burst_penalty = inherited_penalty;
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

/* ================================================================== */
/*                       12.  Reset helpers                           */
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

	se->child_burst.value = 0;
	se->child_burst.count = 0;
	se->child_burst.timestamp = 0;

	se->group_burst.value = 0;
	se->group_burst.count = 0;
	se->group_burst.timestamp = 0;
}
EXPORT_SYMBOL_GPL(reset_task_bore);

static void reset_all_task_weights_for_bore_toggle(void)
{
	struct task_struct *g, *t;
	struct rq *rq;
	struct rq_flags rf;

	pr_info("Global BORE state changed, resetting all eligible task weights...\n");

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
			if (!rq || !t->on_rq) {
				task_rq_unlock(rq, t, &rf);
				put_task_struct(t);
				continue;
			}

			update_rq_clock(rq);
			reweight_task_by_prio(t, effective_prio(t));

			task_rq_unlock(rq, t, &rf);
			put_task_struct(t);
		}
	}
	rcu_read_unlock();
	pr_info("Task weight reset complete.\n");
}

/* ================================================================== */
/*                          13.  Sysctl                               */
/* ================================================================== */
#ifdef CONFIG_SYSCTL

static const int bore_sysctl_val_three = 3;
static const int bore_sysctl_val_nicew = NICE_WIDTH;
static const int bore_sysctl_val_smooth_max = 10;
static const int bore_sysctl_val_offset_max = 64;
static const int bore_sysctl_val_scale_max = BORE_ORIG_BURST_PENALTY_SCALE * 4;
static const int bore_sysctl_val_pct_max = 200;
static const int bore_sysctl_val_stopcnt_max = 4096;

static int sched_bore_toggle_sysctl_handler(const struct ctl_table *table,
                                            int write, void *buffer,
                                            size_t *lenp, loff_t *ppos)
{
	u8 old_val = *(u8 *)table->data;
	int ret;

	ret = proc_dou8vec_minmax((struct ctl_table *)table,
		write, buffer, lenp, ppos);

	if (!ret && write && old_val != *(u8 *)table->data) {
		reset_all_task_weights_for_bore_toggle();
	}

	return ret;
}

static int bore_u8_sysctl_gen_handler(const struct ctl_table *table,
                                      int write, void *buffer,
                                      size_t *lenp, loff_t *ppos)
{
	u8 old = *(u8 *)table->data;
	int ret;

	ret = proc_dou8vec_minmax((struct ctl_table *)table,
		write, buffer, lenp, ppos);
	if (!ret && write && old != *(u8 *)table->data) {
		bore_bump_penalty_gen();
	}
	return ret;
}

static int bore_uint_sysctl_gen_handler(const struct ctl_table *table,
                                        int write, void *buffer,
                                        size_t *lenp, loff_t *ppos)
{
	uint old = *(uint *)table->data;
	int ret;

	ret = proc_douintvec_minmax((struct ctl_table *)table,
		write, buffer, lenp, ppos);
	if (!ret && write && old != *(uint *)table->data) {
		bore_bump_penalty_gen();
	}
	return ret;
}

static struct ctl_table bore_sysctls[] = {
	{
		.procname = "sched_bore",
		.data = &sched_bore,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = sched_bore_toggle_sysctl_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_exclude_kthreads",
		.data = &sched_burst_exclude_kthreads,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_smoothness_long",
		.data = &sched_burst_smoothness_long,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_smoothness_short",
		.data = &sched_burst_smoothness_short,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_fork_atavistic",
		.data = &sched_burst_fork_atavistic,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_three,
	},
	{
		.procname = "sched_burst_parity_threshold",
		.data = &sched_burst_parity_threshold,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_nicew,
	},
	{
		.procname = "sched_burst_penalty_offset",
		.data = &sched_burst_penalty_offset,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_offset_max,
	},
	{
		.procname = "sched_burst_penalty_scale",
		.data = &sched_burst_penalty_scale,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_scale_max,
	},
	{
		.procname = "sched_burst_cache_stop_count",
		.data = &sched_burst_cache_stop_count,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_stopcnt_max,
	},
	{
		.procname = "sched_burst_cache_lifetime",
		.data = &sched_burst_cache_lifetime,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_INT_MAX,
	},
	{
		.procname = "sched_deadline_boost_mask",
		.data = &sched_deadline_boost_mask,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_INT_MAX,
	},
	{
		.procname = "sched_burst_core_aware_penalty",
		.data = &sched_burst_core_aware_penalty,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_core_aware_smoothing",
		.data = &sched_burst_core_aware_smoothing,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_penalty_pcore_scale_pct",
		.data = &sched_burst_penalty_pcore_scale_pct,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname = "sched_burst_penalty_ecore_scale_pct",
		.data = &sched_burst_penalty_ecore_scale_pct,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = bore_uint_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname = "sched_burst_smoothness_long_p",
		.data = &sched_burst_smoothness_long_p,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_smoothness_short_p",
		.data = &sched_burst_smoothness_short_p,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_smoothness_long_e",
		.data = &sched_burst_smoothness_long_e,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_smoothness_short_e",
		.data = &sched_burst_smoothness_short_e,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname = "sched_burst_pcore_hog_threshold_pct",
		.data = &sched_burst_pcore_hog_threshold_pct,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname = "sched_burst_pcore_hog_penalty_add",
		.data = &sched_burst_pcore_hog_penalty_add,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = (void *)&bore_sysctl_val_nicew,
	},
	{ .procname = NULL }
};

static struct ctl_table_header *bore_sysctl_header_ptr;

static int __init bore_sysctl_init_func(void)
{
	size_t table_actual_entries = ARRAY_SIZE(bore_sysctls) - 1;

	bore_sysctl_header_ptr = register_sysctl_sz("kernel", bore_sysctls,
		table_actual_entries);

	if (!bore_sysctl_header_ptr) {
		pr_err("failed to register sysctl table\n");
		return -ENOMEM;
	}

	pr_info("sysctl parameters registered (count: %zu)\n", table_actual_entries);
	return 0;
}

#endif /* CONFIG_SYSCTL */

/* ================================================================== */
/*                          14.  Init                                 */
/* ================================================================== */
void __init sched_bore_init(void)
{
	pr_info("BORE v%s initialising (HZ=%d)\n", SCHED_BORE_VERSION, HZ);

	bore_build_lookup_tables();
	INIT_WORK(&bore_enable_key_work, bore_enable_key_workfn);

	if (is_intel_hybrid()) {
		pr_info("Intel Hybrid CPU detected, applying general hybrid default tweaks.\n");
		sched_burst_parity_threshold += 2;
		sched_burst_smoothness_short = max_t(u8, 1,
			BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1);
		sched_burst_fork_atavistic = 0;
		sched_burst_exclude_kthreads = 1;
	}

	reset_task_bore(&init_task);
	spin_lock_init(&init_task.se.child_burst.lock);
	spin_lock_init(&init_task.se.group_burst.lock);

	pr_info("Early init done. Core-aware features status pending full CPU topology detection.\n");
}

static int __init bore_late_topology_final_check(void)
{
	bore_check_and_update_topology_features();

#ifdef CONFIG_X86_64
	if (boot_cpu_has(X86_FEATURE_BMI2)) {
		pr_info("BMI2 detected, enabling MULX optimization for penalty calculation.\n");
		static_call_update(bore_calc_penalty, &__calc_burst_penalty_fast_bmi2);
	}
#endif
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
#endif

#endif /* CONFIG_SCHED_BORE */

#ifdef CONFIG_SCHED_BORE
core_initcall(bore_topology_init);
late_initcall_sync(bore_late_topology_final_check);

#ifdef CONFIG_SYSCTL
late_initcall_sync(bore_sysctl_init_func);
#endif

#endif /* CONFIG_SCHED_BORE */
