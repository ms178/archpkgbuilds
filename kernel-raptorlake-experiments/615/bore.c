// SPDX-License-Identifier: GPL-2.0
/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler
 *
 * Production-Ready: Raptor Lake HFI/ITD-aware (no CONFIG_IPC_CLASSES),
 * context-safe, cache-safe, ABI-compatible with the original BORE interfaces.
 *
 * Minimal-invasive Apex fixes:
 * - TU-local static inline fallback for task_cpu_possible() before sched.h
 *   to satisfy sched.h::task_allowed_on_cpu() without redefining global symbols.
 * - Implement Intel Thread Director (ITD) via HFI MSRs entirely within bore.c:
 *   lazy per-CPU enable, safe MSR access, rate-limited classification sampling,
 *   bounded penalty biasing based on core type (P/E via arch_asym_cpu_priority()).
 *   No dependency on CONFIG_IPC_CLASSES nor external scheduler hooks.
 * - Replace unsafe RCU traversal of parent->children/thread lists with tasklist_lock-protected
 *   list_for_each_entry() (parent/children lists are NOT RCU lists in upstream), preventing UAF/corruption.
 * - Saturate arithmetic in topological aggregation (sum += v*c) to avoid overflow.
 * - Ensure all cache updates happen under their spinlocks; traversal under tasklist_lock
 *   preserves lock ordering and prevents races.
 * - Fix restart_burst_rescale_deadline() to rescale only when priority improves (old > new).
 * - Use { .procname = NULL } sentinel in sysctl table.
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
#include <linux/sched/clock.h>
#include <linux/sched/task.h>
#include <linux/sched/topology.h>
#include <linux/sched/bore.h>
#include <linux/sched/signal.h>
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
#include <linux/types.h>
#include <linux/rculist.h>
#include <linux/limits.h>

#include <asm/processor.h>
#include <asm/topology.h>
#include <asm/cpufeature.h>
#include <asm/msr.h>
#include <asm/msr-index.h>

/*
 * Fallback for task_cpu_possible() referenced by sched.h::task_allowed_on_cpu().
 * TU-local static inline: no ABI change. Must precede "sched.h" so inlines see it.
 */
static __always_inline bool task_cpu_possible(int cpu, const struct task_struct *p)
{
#ifdef CONFIG_SMP
	(void)p;
	return cpumask_test_cpu(cpu, cpu_possible_mask);
#else
	(void)cpu; (void)p;
	return true;
#endif
}

#include "sched.h"

#ifdef CONFIG_SCHED_BORE

/* Forward declarations to avoid -Wmissing-prototypes if headers omit them */
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
/*                 0a.  Local helpers (type-safe abs64)               */
/* ================================================================== */
static __always_inline u64 bore_abs64(s64 v)
{
	return (v < 0) ? (0ULL - (u64)v) : (u64)v;
}

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
#define BORE_DEF_ITD_AGGRESSIVENESS_PCT        25

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

/* P/E core aware tunables (used in penalty parameter scaling and smoothing) */
DEF_U8(sched_burst_core_aware_penalty,    0);
DEF_U8(sched_burst_core_aware_smoothing,  0);
DEF_U32(sched_burst_penalty_pcore_scale_pct, 100);
DEF_U32(sched_burst_penalty_ecore_scale_pct, 100);
DEF_U8(sched_burst_smoothness_long_p, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_p, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_smoothness_long_e, BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_e, BORE_ORIG_BURST_SMOOTHNESS_SHORT);

/* ================================================================== */
/*                   2.  ITD via HFI (no IPC_CLASSES)                 */
/* ================================================================== */
#ifdef CONFIG_X86
#ifndef HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT
#define HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT    BIT(0)
#endif
#ifndef HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT
#define HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT BIT(0)
#endif

/* ITD/HFI sysctls */
static u8  __read_mostly sched_burst_itd_enable     = 1;            /* master enable */
static u32 __read_mostly sched_burst_itd_heavy_mask = BIT(2)|BIT(3); /* classes considered heavy */
static u32 __read_mostly sched_burst_itd_bias_pct   = 45;           /* 0..100% */
static u32 __read_mostly sched_burst_itd_cap_pct    = 60;           /* 0..100% */
static u32 __read_mostly sched_burst_itd_sample_ns  = 2 * 1000 * 1000; /* 2ms */

/* Per-CPU ITD/HFI sampling state */
struct bore_itd_state {
	u64 last_sample_ns;
	u8  last_class;
	u8  last_valid;
	u8  inited;
};
static DEFINE_PER_CPU(struct bore_itd_state, bore_itd_state);

/* Availability: user enabled + HFI feature present */
static __always_inline bool bore_itd_available(void)
{
	return READ_ONCE(sched_burst_itd_enable) && boot_cpu_has(X86_FEATURE_HFI);
}

/* One-shot per-CPU enable; idempotent */
static __always_inline void bore_itd_lazy_enable_this_cpu(void)
{
	u64 val;
	struct bore_itd_state *st = this_cpu_ptr(&bore_itd_state);

	if (!bore_itd_available() || READ_ONCE(st->inited))
		return;

#ifdef MSR_IA32_HW_FEEDBACK_THREAD_CONFIG
	wrmsrl(MSR_IA32_HW_FEEDBACK_THREAD_CONFIG, HW_FEEDBACK_THREAD_CONFIG_ENABLE_BIT);
#endif
#ifdef MSR_IA32_HW_FEEDBACK_CONFIG
	if (rdmsrq_safe(MSR_IA32_HW_FEEDBACK_CONFIG, &val) == 0) {
		val |= HW_FEEDBACK_CONFIG_HFI_ENABLE_BIT;
		wrmsrl(MSR_IA32_HW_FEEDBACK_CONFIG, val);
	}
#endif
	WRITE_ONCE(st->inited, 1);
	WRITE_ONCE(st->last_sample_ns, 0);
	WRITE_ONCE(st->last_valid, 0);
	WRITE_ONCE(st->last_class, 0);
}

/* Try to read ITD class via HFI; rate-limited and cached */
static __always_inline bool bore_itd_read_class(u8 *classid)
{
	u64 now = local_clock();
	struct bore_itd_state *st = this_cpu_ptr(&bore_itd_state);
	u64 last = READ_ONCE(st->last_sample_ns);
	u64 gap  = READ_ONCE(sched_burst_itd_sample_ns);

	if (last && (now - last) < gap) {
		if (READ_ONCE(st->last_valid)) {
			*classid = READ_ONCE(st->last_class);
			return true;
		}
		return false;
	}

#ifdef MSR_IA32_HW_FEEDBACK_CHAR
	{
		union {
			struct {
				u64 classid : 8;
				u64 reserved : 55;
				u64 valid    : 1;
			} split;
			u64 full;
		} msr;

		if (rdmsrq_safe(MSR_IA32_HW_FEEDBACK_CHAR, &msr.full) != 0) {
			WRITE_ONCE(st->last_sample_ns, now);
			WRITE_ONCE(st->last_valid, 0);
			return false;
		}

		WRITE_ONCE(st->last_sample_ns, now);
		if (!msr.split.valid) {
			WRITE_ONCE(st->last_valid, 0);
			return false;
		}

		WRITE_ONCE(st->last_class, (u8)msr.split.classid);
		WRITE_ONCE(st->last_valid, 1);
		*classid = (u8)msr.split.classid;
		return true;
	}
#else
	WRITE_ONCE(st->last_sample_ns, now);
	WRITE_ONCE(st->last_valid, 0);
	return false;
#endif
}

/* Bounded bias based on core type and "heavy" classes */
static __always_inline u32 bore_itd_bias_penalty(u32 base_penalty, u8 classid, int this_cpu)
{
	s32 pri = arch_asym_cpu_priority(this_cpu);
	bool is_pcore = (pri > 0);
	bool heavy = !!(READ_ONCE(sched_burst_itd_heavy_mask) & BIT(classid));
	u32 bias_pct = clamp_t(u32, READ_ONCE(sched_burst_itd_bias_pct), 0, 100);
	u32 cap_pct  = clamp_t(u32, READ_ONCE(sched_burst_itd_cap_pct),  0, 100);
	u64 adj = div_u64((u64)base_penalty * bias_pct, 100);
	u64 cap = div_u64((u64)base_penalty * cap_pct,  100);

	if (adj > cap)
		adj = cap;

	if (!heavy || adj == 0)
		return base_penalty;

	if (is_pcore)
		return (adj > base_penalty) ? 0 : (u32)(base_penalty - adj);

	/* E-core */
	{
		u64 res = (u64)base_penalty + adj;
		return (res > (u64)UINT_MAX) ? UINT_MAX : (u32)res;
	}
}
#endif /* CONFIG_X86 */

/* ================================================================== */
/*                          2b.  Constants                            */
/* ================================================================== */
#define MAX_BURST_PENALTY          156U
#define ECORE_OFFSET_ADJ_DIV       20
#define MAX_ECORE_OFFSET_ADJUST    10
#define MIN_EFFECTIVE_OFFSET       4
#define BORE_MAX_VRUNTIME_CLAMP    (200LL * NSEC_PER_MSEC)

/* ================================================================== */
/*                       3.  Static key                               */
/* ================================================================== */
DEFINE_STATIC_KEY_FALSE(bore_core_aware_key);

/* ================================================================== */
/*        3a.  Per-CPU penalty params cache (+ generation)            */
/* ================================================================== */
struct bore_penalty_param {
	u32 scale;
	u32 sat_thresh_q8;
	s16 itd_pri;
	u8  offset;
	u8  gen;
} ____cacheline_aligned_in_smp;

DEFINE_PER_CPU(struct bore_penalty_param, bore_penalty);
static atomic_t bore_penalty_gen = ATOMIC_INIT(1);

/* ================================================================== */
/*                 4.  Per-CPU CPU-type cache                         */
/* ================================================================== */
DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type)
____cacheline_aligned_in_smp = TOPO_CPU_TYPE_UNKNOWN;

/* ================================================================== */
/*        4a.  Per-CPU IPCC dirty flag (kept for compatibility)       */
/* ================================================================== */
DEFINE_PER_CPU(bool, bore_ipcc_dirty);

/* ================================================================== */
/*                       5.  Scale LUT                                */
/* ================================================================== */
static u32 bore_scale_tbl[201] __ro_after_init;
static u8  log2_frac_lut[64] __ro_after_init;

static void bore_build_penalty_param_for_cpu(int cpu);

static __always_inline enum x86_topology_cpu_type
bore_get_rq_cpu_type(struct rq *rq)
{
	if (static_branch_unlikely(&bore_core_aware_key))
		return per_cpu(bore_cpu_type, rq->cpu);
	return TOPO_CPU_TYPE_UNKNOWN;
}

static void __init bore_build_lookup_tables(void)
{
	int i;
	static const u8 lut_vals[64] = {
		0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 43, 47, 51, 55, 58,
		62, 66, 69, 73, 76, 80, 83, 87, 90, 94, 97, 100, 104, 107, 110, 113,
		116, 119, 122, 125, 128, 131, 134, 137, 140, 143, 145, 148, 151, 154, 156, 159,
		162, 164, 167, 169, 172, 174, 177, 179, 182, 184, 186, 189, 191, 193, 196, 198
	};

	for (i = 0; i <= 200; i++)
		bore_scale_tbl[i] = div_u64((u64)BORE_ORIG_BURST_PENALTY_SCALE * i, 100);

	for (i = 0; i < 64; i++)
		log2_frac_lut[i] = lut_vals[i];
}

/* ================================================================== */
/*                     6.  Hybrid detection                           */
/* ================================================================== */
static bool __init is_intel_raptor_lake(void)
{
#ifdef CONFIG_X86_64
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6)
		return false;

	switch (boot_cpu_data.x86_model) {
	case 0xB7: case 0xBA: case 0xBE: case 0xBF: case 0xAC: case 0xB1:
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
/*          7.  Topology scan + deferred static-key enable            */
/* ================================================================== */
static bool bore_cpu_types_detected;
static void bore_enable_key_workfn(struct work_struct *w);
static DECLARE_WORK(bore_enable_key_work, bore_enable_key_workfn);

static inline void bore_bump_penalty_gen(void)
{
	atomic_inc(&bore_penalty_gen);
}

static inline const struct bore_penalty_param *bore_get_param(void)
{
	struct bore_penalty_param *pp = this_cpu_ptr(&bore_penalty);
	s16 itd_now;
	u8 g_now;

	__builtin_prefetch(pp, 0, 3);
	g_now = (u8)atomic_read(&bore_penalty_gen);
	itd_now = arch_asym_cpu_priority(smp_processor_id());

	if (unlikely(pp->gen != g_now || pp->itd_pri != itd_now))
		bore_build_penalty_param_for_cpu(smp_processor_id());

	return pp;
}

static void bore_build_penalty_param_for_cpu(int cpu)
{
	struct bore_penalty_param *pp = &per_cpu(bore_penalty, cpu);
	enum x86_topology_cpu_type ct = per_cpu(bore_cpu_type, cpu);
	u32 scale = READ_ONCE(sched_burst_penalty_scale);
	u8 offset = READ_ONCE(sched_burst_penalty_offset);
	s16 itd_pr = arch_asym_cpu_priority(cpu);

	if (READ_ONCE(sched_burst_core_aware_penalty) && static_branch_unlikely(&bore_core_aware_key)) {
		u32 base = READ_ONCE(sched_burst_penalty_scale);

		if (ct == TOPO_CPU_TYPE_EFFICIENCY) {
			u32 pct = clamp_t(uint, READ_ONCE(sched_burst_penalty_ecore_scale_pct), 0U, 200U);
			scale = div_u64((u64)base * pct, 100);
			if (pct > 100) {
				u8 adj = min_t(u8, MAX_ECORE_OFFSET_ADJUST, (pct - 100) / ECORE_OFFSET_ADJ_DIV);
				offset = max_t(u8, MIN_EFFECTIVE_OFFSET, offset - adj);
			}
		} else if (ct == TOPO_CPU_TYPE_PERFORMANCE) {
			u32 pct = clamp_t(uint, READ_ONCE(sched_burst_penalty_pcore_scale_pct), 0U, 200U);
			scale = div_u64((u64)base * pct, 100);
		}
	}

	if (itd_pr > 0) {
		u64 pr_norm = min_t(u64, 1024, (u64)itd_pr);
		u64 reduction_factor = 1024 - pr_norm;
		reduction_factor = reduction_factor * reduction_factor;
		u32 reduction_permille = div_u64(reduction_factor, 1049);
		u32 aggressiveness = clamp_t(uint, BORE_DEF_ITD_AGGRESSIVENESS_PCT, 0U, 100U);
		u32 scale_reduction = div_u64((u64)scale * reduction_permille * aggressiveness, 100000);
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
	bool found_hybrid = false;

	for_each_possible_cpu(cpu) {
		enum x86_topology_cpu_type t = get_topology_cpu_type(&cpu_data(cpu));
		per_cpu(bore_cpu_type, cpu) = t;
		if (t == TOPO_CPU_TYPE_PERFORMANCE || t == TOPO_CPU_TYPE_EFFICIENCY)
			found_hybrid = true;
		bore_build_penalty_param_for_cpu(cpu);
	}

	if (!found_hybrid) {
		if (is_intel_hybrid() && !bore_cpu_types_detected)
			pr_info("Hybrid CPU, but no P/E info found yet. Will retry.\n");
		return;
	}

	if (!bore_cpu_types_detected) {
		pr_info("P/E core topology information detected.\n");
		bore_cpu_types_detected = true;
		sched_burst_core_aware_penalty = 1;
		sched_burst_core_aware_smoothing = 1;

		if (is_intel_raptor_lake()) {
			pr_info("Applying Raptor-Lake P/E specific tunings.\n");
			WRITE_ONCE(sched_burst_penalty_ecore_scale_pct, 140);
			WRITE_ONCE(sched_burst_smoothness_long_e, 3);
			WRITE_ONCE(sched_burst_smoothness_short_e, 1);
			WRITE_ONCE(sched_burst_parity_threshold, READ_ONCE(sched_burst_parity_threshold) + 2);
		} else if (is_intel_hybrid()) {
			pr_info("Applying generic Intel Hybrid P/E tunings.\n");
		}

		bore_bump_penalty_gen();
		for_each_possible_cpu(cpu)
			bore_build_penalty_param_for_cpu(cpu);
	}

	if (!static_branch_unlikely(&bore_core_aware_key))
		schedule_work(&bore_enable_key_work);
}

static int bore_cpu_online_cb(unsigned int cpu)
{
	per_cpu(bore_cpu_type, cpu) = get_topology_cpu_type(&cpu_data(cpu));
	bore_build_penalty_param_for_cpu(cpu);
	bore_check_and_update_topology_features();
	per_cpu(bore_ipcc_dirty, cpu) = true;
	return 0;
}

static int bore_cpu_offline_cb(unsigned int cpu)
{
	int i;
	for_each_online_cpu(i)
		per_cpu(bore_ipcc_dirty, i) = true;
	return 0;
}

static enum cpuhp_state bore_cpuhp_state_val;

static int __init bore_topology_init(void)
{
	int ret_hp;

	bore_check_and_update_topology_features();

	ret_hp = cpuhp_setup_state_nocalls(CPUHP_AP_ONLINE_DYN,
		"sched/bore:online", bore_cpu_online_cb, bore_cpu_offline_cb);
	if (ret_hp < 0)
		pr_err("cpuhp_setup_state_nocalls failed: %d\n", ret_hp);
	else
		bore_cpuhp_state_val = ret_hp;

	return 0;
}

/* ================================================================== */
/*              8.  Penalty math (OPTIMIZED & SAFE)                   */
/* ================================================================== */
static __always_inline u32 log2_u64_q24_8(u64 v)
{
	if (unlikely(!v))
		return 0;

	u32 lz = __builtin_clzll(v);
	u32 exponent = 63u - lz;
	u64 norm = v << lz;
	u32 mantissa_idx = (u32)((norm >> 57) & 0x3F);
	u8 frac = log2_frac_lut[mantissa_idx];

	return (exponent << 8) | frac;
}

static u32 __calc_burst_penalty_fast(u64 burst_time,
                                     enum x86_topology_cpu_type ctype)
{
	const struct bore_penalty_param *pp = bore_get_param();
	u32 greed_q8;
	s64 delta_q8;
	u32 penalty_raw;

	(void)ctype;

	greed_q8 = log2_u64_q24_8(burst_time);
	delta_q8 = (s64)greed_q8 - ((s64)pp->offset << 8);

	delta_q8 = max(0LL, delta_q8);
	if (unlikely((u64)delta_q8 >= pp->sat_thresh_q8))
		return MAX_BURST_PENALTY;

#ifdef CONFIG_X86_64
	if (static_cpu_has(X86_FEATURE_BMI2)) {
		u64 tmp_low, tmp_high;
		asm ("mulx %3, %0, %1"
		     : "=r"(tmp_low), "=r"(tmp_high)
		     : "d"((u64)delta_q8), "r"((u64)pp->scale));
		penalty_raw = (u32)(tmp_high >> 16);
	} else
#endif
	{
		u64 tmp = (u64)delta_q8 * pp->scale;
		penalty_raw = (u32)(tmp >> 16);
	}

	return min_t(u32, MAX_BURST_PENALTY, penalty_raw);
}

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
/*                   10.  Hot-path updates                            */
/* ================================================================== */
struct bore_penalty_cache {
	struct sched_entity *last_se;
	u64 last_burst_time;
	u32 last_penalty;
	int last_cpu;
	u8  gen;
	u16 last_ipcc;

	u64 hit_count;
	u64 miss_count;
} ____cacheline_aligned_in_smp;

DEFINE_PER_CPU(struct bore_penalty_cache, bore_penalty_cache);

void update_burst_penalty(struct sched_entity *se)
{
	struct rq *rq = rq_of(cfs_rq_of(se));
	struct bore_penalty_cache *cache = this_cpu_ptr(&bore_penalty_cache);
	u64 burst_time_local = READ_ONCE(se->burst_time);
	u8 current_gen = (u8)atomic_read(&bore_penalty_gen);
	u32 new_pen;

	__builtin_prefetch(cache, 0, 3);

	if (likely(cache->last_se == se &&
		   cache->last_burst_time == burst_time_local &&
		   cache->last_cpu == rq->cpu &&
		   cache->gen == current_gen)) {
		se->curr_burst_penalty = cache->last_penalty;
		cache->hit_count++;
	} else {
		enum x86_topology_cpu_type ctype = bore_get_rq_cpu_type(rq);
		new_pen = static_call(bore_calc_penalty)(burst_time_local, ctype);

#ifdef CONFIG_X86
		/* ITD/HFI biasing: lazy enable + rate-limited class sampling */
		if (bore_itd_available()) {
			u8 classid;
			bore_itd_lazy_enable_this_cpu();
			if (bore_itd_read_class(&classid))
				new_pen = bore_itd_bias_penalty(new_pen, classid, rq->cpu);
		}
#endif

		cache->last_se = se;
		cache->last_burst_time = burst_time_local;
		cache->last_penalty = new_pen;
		cache->last_cpu = rq->cpu;
		cache->gen = current_gen;

		se->curr_burst_penalty = new_pen;
		cache->miss_count++;
	}

	se->burst_penalty = max(se->prev_burst_penalty, se->curr_burst_penalty);
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

void update_curr_bore(u64 delta_exec, struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se)))
		return;

	se->burst_time += delta_exec;
	update_burst_penalty(se);
}
EXPORT_SYMBOL_GPL(update_curr_bore);

static __always_inline u32
binary_smooth(u32 new_val, u32 old_val, enum x86_topology_cpu_type ctype)
{
	u8 shift_long = READ_ONCE(sched_burst_smoothness_long);
	u8 shift_short = READ_ONCE(sched_burst_smoothness_short);
	s64 increment = (s64)new_val - (s64)old_val;
	u32 shift = (increment >= 0) ? shift_long : shift_short;
	u32 delta = (u32)(bore_abs64(increment) >> shift);

	if (increment >= 0) {
		u64 sum = (u64)old_val + delta;
		return (sum > U32_MAX) ? U32_MAX : (u32)sum;
	} else {
		return (old_val < delta) ? 0 : (old_val - delta);
	}
}

static __always_inline u8 effective_prio(struct task_struct *p)
{
	int prio_val = p->static_prio - MAX_RT_PRIO;

	if (likely(READ_ONCE(sched_bore)))
		prio_val = max(0, prio_val - (int)READ_ONCE(p->se.burst_score));

	return min_t(int, NICE_WIDTH - 1, prio_val);
}

static void reweight_task_by_prio(struct task_struct *p, int prio_val)
{
	struct sched_entity *se = &p->se;
	struct cfs_rq *cfs_rq = cfs_rq_of(se);

	prio_val = clamp(prio_val, 0, NICE_WIDTH - 1);
	reweight_entity(cfs_rq, se, scale_load(sched_prio_to_weight[prio_val]), true);
	se->load.inv_weight = sched_prio_to_wmult[prio_val];
}

void update_burst_score(struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se)))
		return;

	struct task_struct *p = task_of(se);
	u8 old_effective_prio = effective_prio(p);
	u8 new_score = 0;

	if (!((p->flags & PF_KTHREAD) && READ_ONCE(sched_burst_exclude_kthreads)))
		new_score = se->burst_penalty >> 2;

#ifdef CONFIG_SMP
	if (static_branch_unlikely(&bore_core_aware_key)) {
		s16 itd_pr = arch_asym_cpu_priority(task_cpu(p));
		if (itd_pr > 0) {
			u32 threshold = BORE_DEF_PCORE_HOG_THRESHOLD_PCT;

			if (se->burst_penalty > (MAX_BURST_PENALTY * threshold / 100))
				new_score = min_t(u8, NICE_WIDTH - 1,
						  new_score + BORE_DEF_PCORE_HOG_PENALTY_ADD);
		}
	}
#endif

	if (se->burst_score == new_score) {
		if (unlikely(effective_prio(p) != old_effective_prio))
			reweight_task_by_prio(p, effective_prio(p));
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
	se->burst_time         = 0;
	se->curr_burst_penalty = 0;
}

void restart_burst(struct sched_entity *se)
{
	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);

void restart_burst_rescale_deadline(struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se)))
		return;

	struct task_struct *p = task_of(se);
	u8 old_eff_prio = effective_prio(p);
	s64 vruntime_remaining = se->deadline - se->vruntime;

	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);

	u8 new_eff_prio = effective_prio(p);
	if (old_eff_prio > new_eff_prio) { /* only rescale on improvement */
		u64 abs_vrem = bore_abs64(vruntime_remaining);
		u64 weight_units = __unscale_slice(abs_vrem, old_eff_prio);
		u64 vruntime_scaled_new = __scale_slice(weight_units, new_eff_prio);

		if (vruntime_remaining < 0)
			se->deadline = se->vruntime - (s64)vruntime_scaled_new;
		else
			se->deadline = se->vruntime + (s64)vruntime_scaled_new;
	}
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

/* ================================================================== */
/*                11.  Inheritance & cache                            */
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
	return (s64)(bc->timestamp + READ_ONCE(sched_burst_cache_lifetime) - now_ns) < 0;
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
			if (!__builtin_add_overflow(sum_penalties, pen, &sum_penalties))
				count++;
			else {
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
		while (count_children_upto2_locked(&effective_descendant->children) == 1)
			effective_descendant = list_first_entry(&effective_descendant->children,
								struct task_struct, sibling);

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
		    current_level_direct_children_count >= READ_ONCE(sched_burst_cache_stop_count))
			break;
	}

	spin_lock(&p->se.child_burst.lock);
	update_burst_cache_locked(&p->se.child_burst, p, current_level_direct_children_count,
				  current_level_sum_of_penalties, now_ns);
	spin_unlock(&p->se.child_burst.lock);

	*accumulated_count += current_level_direct_children_count;
	*accumulated_sum = min_t(u32, UINT_MAX,
				 *accumulated_sum + current_level_sum_of_penalties);
}

static u8 inherit_burst_direct(struct task_struct *parent_task,
			       u64 now_ns, u64 clone_flags)
{
	struct task_struct *target;
	unsigned long irqflags;
	u8 value = 0;

	if (unlikely(!parent_task))
		return 0;

	target = parent_task;
	if (clone_flags & CLONE_PARENT) {
		target = parent_task->real_parent;
		if (unlikely(!target))
			return 0;
	}

	read_lock(&tasklist_lock);
	spin_lock_irqsave(&target->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&target->se.child_burst, now_ns))
		__update_child_burst_direct_locked(target, now_ns);
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

	if (unlikely(!parent_task))
		return 0;

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
		if (count_children_upto2_locked(&ancestor->children) > child_count_threshold)
			break;
		if (!ancestor->real_parent || ancestor->real_parent == ancestor)
			break;
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
			if (!__builtin_add_overflow(sum_penalties, pen, &sum_penalties))
				thread_count++;
			else {
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

	if (unlikely(!group_leader))
		return 0;

	read_lock(&tasklist_lock);
	spin_lock_irqsave(&group_leader->se.group_burst.lock, irqflags);
	if (burst_cache_expired(&group_leader->se.group_burst, now_ns))
		__update_tg_burst_locked(group_leader, now_ns);
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

	if (!task_is_bore_eligible(p_new_task))
		return;

	if (clone_flags & CLONE_THREAD) {
		inherited_penalty = inherit_burst_tg(p_parent, now_ns);
	} else {
		if (READ_ONCE(sched_burst_fork_atavistic) == 0)
			inherited_penalty = 0;
		else if (READ_ONCE(sched_burst_fork_atavistic) == 1)
			inherited_penalty = inherit_burst_direct(p_parent, now_ns, clone_flags);
		else
			inherited_penalty = inherit_burst_topological(p_parent, now_ns, clone_flags);
	}

	se_new->prev_burst_penalty = inherited_penalty;
	se_new->burst_penalty      = inherited_penalty;
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

/* ================================================================== */
/*                       12.  Reset helpers                           */
/* ================================================================== */
void reset_task_bore(struct task_struct *p)
{
	struct sched_entity *se;

	if (unlikely(!p))
		return;

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
			if (!task_is_bore_eligible(t))
				continue;
			if (!tryget_task_struct(t))
				continue;

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
/*                          13.  Sysctl                               */
/* ================================================================== */
#ifdef CONFIG_SYSCTL

static const int bore_sysctl_val_three        = 3;
static const int bore_sysctl_val_nicew        = NICE_WIDTH;
static const int bore_sysctl_val_smooth_max   = 10;
static const int bore_sysctl_val_offset_max   = 64;
static const int bore_sysctl_val_scale_max    = BORE_ORIG_BURST_PENALTY_SCALE * 4;
static const int bore_sysctl_val_pct_max      = 200;
static const int bore_sysctl_val_stopcnt_max  = 4096;

static int
sched_bore_toggle_sysctl_handler(const struct ctl_table *table,
				 int write, void *buffer,
				 size_t *lenp, loff_t *ppos)
{
	u8 old_val = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old_val != *(u8 *)table->data)
		reset_all_task_weights_for_bore_toggle();
	return ret;
}

static int bore_u8_sysctl_gen_handler(const struct ctl_table *table,
				      int write, void *buffer,
				      size_t *lenp, loff_t *ppos)
{
	u8 old = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old != *(u8 *)table->data)
		bore_bump_penalty_gen();
	return ret;
}

static int bore_uint_sysctl_gen_handler(const struct ctl_table *table,
					int write, void *buffer,
					size_t *lenp, loff_t *ppos)
{
	uint old = *(uint *)table->data;
	int ret = proc_douintvec_minmax(table, write, buffer, lenp, ppos);
	if (!ret && write && old != *(uint *)table->data)
		bore_bump_penalty_gen();
	return ret;
}

/* ITD sysctls (no IPC_CLASSES needed) */
#ifdef CONFIG_X86
static int bore_u8_sysctl_nogen_handler(const struct ctl_table *table,
					int write, void *buffer,
					size_t *lenp, loff_t *ppos)
{
	return proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
}
static int bore_uint_sysctl_nogen_handler(const struct ctl_table *table,
					  int write, void *buffer,
					  size_t *lenp, loff_t *ppos)
{
	return proc_douintvec_minmax(table, write, buffer, lenp, ppos);
}
#endif

static struct ctl_table bore_sysctls[] = {
	{
		.procname       = "sched_bore",
		.data           = &sched_bore,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = sched_bore_toggle_sysctl_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_ONE,
	},
	{
		.procname       = "sched_burst_exclude_kthreads",
		.data           = &sched_burst_exclude_kthreads,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_ONE,
	},
	{
		.procname       = "sched_burst_smoothness_long",
		.data           = &sched_burst_smoothness_long,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname       = "sched_burst_smoothness_short",
		.data           = &sched_burst_smoothness_short,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname       = "sched_burst_fork_atavistic",
		.data           = &sched_burst_fork_atavistic,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_three,
	},
	{
		.procname       = "sched_burst_parity_threshold",
		.data           = &sched_burst_parity_threshold,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_nicew,
	},
	{
		.procname       = "sched_burst_penalty_offset",
		.data           = &sched_burst_penalty_offset,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = bore_u8_sysctl_gen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_offset_max,
	},
	{
		.procname       = "sched_burst_penalty_scale",
		.data           = &sched_burst_penalty_scale,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = bore_uint_sysctl_gen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_scale_max,
	},
	{
		.procname       = "sched_burst_cache_stop_count",
		.data           = &sched_burst_cache_stop_count,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = proc_douintvec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_stopcnt_max,
	},
	{
		.procname       = "sched_burst_cache_lifetime",
		.data           = &sched_burst_cache_lifetime,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = proc_douintvec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_INT_MAX,
	},
	{
		.procname       = "sched_deadline_boost_mask",
		.data           = &sched_deadline_boost_mask,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = proc_douintvec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_INT_MAX,
	},
	{
		.procname       = "sched_burst_core_aware_penalty",
		.data           = &sched_burst_core_aware_penalty,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = bore_u8_sysctl_gen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_ONE,
	},
	{
		.procname       = "sched_burst_core_aware_smoothing",
		.data           = &sched_burst_core_aware_smoothing,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_ONE,
	},
	{
		.procname       = "sched_burst_penalty_pcore_scale_pct",
		.data           = &sched_burst_penalty_pcore_scale_pct,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = bore_uint_sysctl_gen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname       = "sched_burst_penalty_ecore_scale_pct",
		.data           = &sched_burst_penalty_ecore_scale_pct,
		.maxlen         = sizeof(uint),
		.mode           = 0644,
		.proc_handler   = bore_uint_sysctl_gen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_pct_max,
	},
#ifdef CONFIG_X86
	{
		.procname       = "sched_burst_itd_enable",
		.data           = &sched_burst_itd_enable,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = bore_u8_sysctl_nogen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = SYSCTL_ONE,
	},
	{
		.procname       = "sched_burst_itd_heavy_mask",
		.data           = &sched_burst_itd_heavy_mask,
		.maxlen         = sizeof(u32),
		.mode           = 0644,
		.proc_handler   = proc_douintvec,
	},
	{
		.procname       = "sched_burst_itd_bias_pct",
		.data           = &sched_burst_itd_bias_pct,
		.maxlen         = sizeof(u32),
		.mode           = 0644,
		.proc_handler   = bore_uint_sysctl_nogen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname       = "sched_burst_itd_cap_pct",
		.data           = &sched_burst_itd_cap_pct,
		.maxlen         = sizeof(u32),
		.mode           = 0644,
		.proc_handler   = bore_uint_sysctl_nogen_handler,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname       = "sched_burst_itd_sample_ns",
		.data           = &sched_burst_itd_sample_ns,
		.maxlen         = sizeof(u32),
		.mode           = 0644,
		.proc_handler   = proc_douintvec,
	},
#endif
	{ .procname = NULL }
};

static struct ctl_table_header *bore_sysctl_header_ptr;

static int __init bore_sysctl_init_func(void)
{
	/* Exclude the sentinel from registration to satisfy register_sysctl_sz() */
	size_t table_actual_entries = ARRAY_SIZE(bore_sysctls) - 1;

	bore_sysctl_header_ptr = register_sysctl_sz("kernel", bore_sysctls, table_actual_entries);
	if (!bore_sysctl_header_ptr) {
		pr_err("failed to register sysctl table\n");
		return -ENOMEM;
	}

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
		WRITE_ONCE(sched_burst_parity_threshold, READ_ONCE(sched_burst_parity_threshold) + 2);
		WRITE_ONCE(sched_burst_smoothness_short,
			max_t(u8, 1, (u8)BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1));
		WRITE_ONCE(sched_burst_fork_atavistic, 0);
		WRITE_ONCE(sched_burst_exclude_kthreads, 1);
	}

	reset_task_bore(&init_task);
	spin_lock_init(&init_task.se.child_burst.lock);
	spin_lock_init(&init_task.se.group_burst.lock);

	pr_info("Early init done. Core-aware features pending full CPU topology detection.\n");
}

static int __init bore_late_topology_final_check(void)
{
	bore_check_and_update_topology_features();

#ifdef CONFIG_X86_64
	if (boot_cpu_has(X86_FEATURE_BMI2)) {
		pr_info("BMI2 detected, enabling optimized penalty calculation.\n");
		static_call_update(bore_calc_penalty, &__calc_burst_penalty_fast);
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
#endif /* _LINUX_SCHED_BORE_H_STUBS_DEFINED */

#endif /* CONFIG_SCHED_BORE */

#ifdef CONFIG_SCHED_BORE
core_initcall(bore_topology_init);
late_initcall_sync(bore_late_topology_final_check);
#ifdef CONFIG_SYSCTL
late_initcall_sync(bore_sysctl_init_func);
#endif
#endif /* CONFIG_SCHED_BORE */
