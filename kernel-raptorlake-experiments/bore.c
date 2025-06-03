/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler - Core Logic & Tuning
 * Copyright (C) 2021-2024 Masahito Suzuki <firelzrd@gmail.com>
 *
 * This version incorporates several enhancements over the upstream BORE 5.9.6
 * (commit a5aad25a91f5), primarily focused on:
 *
 * 1.  Advanced Intel Hybrid CPU (P/E core) detection and adaptation:
 *     - Automatic detection of P/E core topology via kernel interfaces.
 *     - Dynamic application of P/E core-specific tunables for penalty
 *       offset, penalty scaling, and smoothing factors.
 *     - Raptor Lake specific tunings are automatically applied if detected.
 *     - Uses a static key (`bore_core_aware_key`) for efficient conditional
 *       execution of core-aware logic in hot paths.
 *     - CPU hotplug support for updating topology information.
 *     - Deferred initialization of core-aware features if topology information
 *       is not immediately available at early boot, with a late_initcall retry.
 *     - Per-CPU penalty parameters cache that dynamically tracks ITD hints.
 *     - ITD-aware amplification of burst score for tasks hogging P-cores.
 *
 * 2.  Refined Initialization and Sysctl Handling:
 *     - More robust sysctl registration using `register_sysctl_sz` with
 *       explicit table sizing.
 *     - Sysctl table for core-aware tunables.
 *     - Tunable definitions (`DEF_U8`, `DEF_U32`) for clarity.
 *     - Pre-computation of a penalty scale lookup table (`bore_scale_tbl`).
 *     - Sysctl handlers that correctly invalidate per-CPU caches on change.
 *
 * 3.  Code Structure and Clarity:
 *     - Organized into numbered sections for better readability.
 *     - Clear separation of tunables, constants, and functional blocks.
 *     - Explicit `pr_fmt` for BORE-specific kernel messages.
 *     - Micro-optimizations like static_call for penalty calculation,
 *       builtin for log2, and cache-line alignment for per-CPU data.
 *
 * For the original BORE implementation and concepts, please refer to
 * Masahito Suzuki's work. This file focuses on the core BORE logic
 * and its integration with hybrid CPU awareness.
 */

#undef pr_fmt
#define pr_fmt(fmt) "BORE: " fmt

/* ================================================================== */
/*                          0.  Headers                               */
/* ================================================================== */
#include <linux/kernel.h>          /* printk()                         */
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
#include <linux/sched/bore.h>      /* SCHED_BORE_VERSION              */
#include <linux/sysctl.h>
#include <linux/bitmap.h>
#include <linux/static_key.h>
#include <linux/static_call.h>
#include <linux/list.h>
#include <linux/slab.h>
#include <linux/math64.h>          /* abs64()                         */
#include <linux/cpuhotplug.h>
#include <linux/workqueue.h>
#include <linux/cpu.h>             /* cpu_online(), topology helpers  */
#include <linux/smp.h>

/*
 * <linux/cpu.h> → <asm-generic/topology.h> briefly introduces a stub
 * task_cpu_possible() for early-boot code and then removes it again.
 * The first inline in kernel/sched/sched.h expects the real helper to
 * exist, so (re-)install the canonical definition here before we pull
 * in sched.h.
 */
#ifdef task_cpu_possible
#undef task_cpu_possible
#endif
#define task_cpu_possible(cpu, p)	cpumask_test_cpu((cpu), (p)->cpus_ptr)

#include <linux/rculist.h>         /* list_first_entry_rcu, …         */
#include <linux/limits.h>          /* INT_MAX, UINT_MAX               */

#include <asm/processor.h>
#include <asm/topology.h>
#include <asm/cpufeature.h>
#include "sched.h"                 /* rq_of(), cfs_rq_of(), …         */

#ifdef CONFIG_SCHED_BORE

/* ================================================================== */
/*                          1.  Tunables                              */
/* ================================================================== */
#define BORE_ORIG_SCHED_BORE                   1
#define BORE_ORIG_BURST_EXCLUDE_KTHREADS       1
#define BORE_ORIG_BURST_SMOOTHNESS_LONG        1 /* Shift value for slow increase */
#define BORE_ORIG_BURST_SMOOTHNESS_SHORT       0 /* Shift value for fast decrease */
#define BORE_ORIG_BURST_FORK_ATAVISTIC         2 /* 0=off, 1=direct parent, >=2 topological depth */
#define BORE_ORIG_BURST_PARITY_THRESHOLD       2 /* Min cfs_rq->nr_queued to apply RUN_TO_PARITY */
#define BORE_ORIG_BURST_PENALTY_OFFSET         24 /* Base offset for penalty calc (non-Q8) */
#define BORE_ORIG_BURST_PENALTY_SCALE          1280 /* Base scale factor */
#define BORE_ORIG_BURST_CACHE_STOP_COUNT       64 /* Max children to scan for topological inheritance */
#define BORE_ORIG_BURST_CACHE_LIFETIME         (75 * 1000 * 1000) /* 75 ms in ns */
#define BORE_ORIG_DEADLINE_BOOST_MASK          (ENQUEUE_INITIAL | ENQUEUE_WAKEUP) /* Event mask */
#define BORE_DEF_PCORE_HOG_THRESHOLD_PCT   85   /* 0-100, % of MAX_BURST */
#define BORE_DEF_PCORE_HOG_PENALTY_ADD      2   /* extra burst_score */

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
DEF_U8 (sched_burst_pcore_hog_threshold_pct, BORE_DEF_PCORE_HOG_THRESHOLD_PCT);
DEF_U8 (sched_burst_pcore_hog_penalty_add, BORE_DEF_PCORE_HOG_PENALTY_ADD);

/* ================================================================== */
/*                          2.  Constants                             */
/* ================================================================== */
#define MAX_BURST_PENALTY          156U        /* 39 << 2 */
#define ECORE_OFFSET_ADJ_DIV       20
#define MAX_ECORE_OFFSET_ADJUST    10
#define MIN_EFFECTIVE_OFFSET        4

/* ================================================================== */
/*                       3.  Static key                               */
/* ================================================================== */
DEFINE_STATIC_KEY_FALSE(bore_core_aware_key);

/* ================================================================== */
/*        3a.  Per-CPU “penalty params” cache (+ generation)          */
/* ================================================================== */
struct bore_penalty_param {
	u32 scale;                 /* same FP as sched_burst_penalty_scale */
	u32 sat_thresh_q8;         /* delta_q8 at which result saturates   */
	s16 itd_pri;               /* arch_asym_cpu_priority() snapshot    */
	u8  offset;                /* non-Q8 offset                        */
	u8  gen;                   /* low 8 bits of global generation      */
} ____cacheline_aligned_in_smp;

DEFINE_PER_CPU(struct bore_penalty_param, bore_penalty);
static atomic_t              bore_penalty_gen = ATOMIC_INIT(1);

static void bore_build_penalty_param_for_cpu(int cpu);

static inline void bore_bump_penalty_gen(void)
{
	atomic_inc(&bore_penalty_gen);
}

static inline const struct bore_penalty_param *bore_get_param(void)
{
	struct bore_penalty_param *pp = this_cpu_ptr(&bore_penalty);
	u8  g_now  = (u8)atomic_read(&bore_penalty_gen);
	s16 itd_now = arch_asym_cpu_priority(smp_processor_id());

	if (unlikely(pp->gen != g_now || pp->itd_pri != itd_now))
		bore_build_penalty_param_for_cpu(smp_processor_id());

	return pp;
}

/* ================================================================== */
/*                 4.  Per-CPU CPU-type cache                         */
/* ================================================================== */
DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type)
____cacheline_aligned_in_smp = TOPO_CPU_TYPE_UNKNOWN;

static __always_inline enum x86_topology_cpu_type
bore_get_rq_cpu_type(struct rq *rq)
{
	if (static_branch_unlikely(&bore_core_aware_key))
		return per_cpu(bore_cpu_type, rq->cpu);
	return TOPO_CPU_TYPE_UNKNOWN;
}

/* ================================================================== */
/*                       5.  Scale LUT                                */
/* ================================================================== */
static u32 bore_scale_tbl[201] __ro_after_init; /* 0-200 % */

static void __init bore_build_scale_tbl(void)
{
	int i;

	for (i = 0; i <= 200; i++)
		bore_scale_tbl[i] =
		div_u64((u64)BORE_ORIG_BURST_PENALTY_SCALE * i, 100);
}

/* ================================================================== */
/*                     6.  Hybrid detection                           */
/* ================================================================== */
static bool __init is_intel_raptor_lake(void)
{
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6)
		return false;

	switch (boot_cpu_data.x86_model) {
		case 0xB7: /* RPL-S desktop */
		case 0xBA: /* RPL-P mobile  */
		case 0xBE: /* ADL-N / RPL ES */
		case 0xBF: /* RPL-S refresh */
		case 0xAC: /* RPL-P refresh */
		case 0xB1: /* RPL server   */
			return true;
	}
	return false;
}

static bool __init is_intel_hybrid(void)
{
	return boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake();
}

/* ================================================================== */
/*          7.  Topology scan + deferred static-key enable            */
/* ================================================================== */
static bool bore_cpu_types_detected;
static void bore_enable_key_workfn(struct work_struct *w);
static DECLARE_WORK(bore_enable_key_work, bore_enable_key_workfn);


static void bore_build_penalty_param_for_cpu(int cpu)
{
	enum x86_topology_cpu_type ct = per_cpu(bore_cpu_type, cpu);
	struct bore_penalty_param *pp = &per_cpu(bore_penalty, cpu);
	u32 scale  = sched_burst_penalty_scale;
	u8  offset = sched_burst_penalty_offset;
	s16 itd_pr = arch_asym_cpu_priority(cpu);

	if (sched_burst_core_aware_penalty &&
		static_branch_unlikely(&bore_core_aware_key)) {
		if (ct == TOPO_CPU_TYPE_EFFICIENCY) {
			scale = bore_scale_tbl[clamp(sched_burst_penalty_ecore_scale_pct,
										 0U, 200U)];
										 if (sched_burst_penalty_ecore_scale_pct > 100) {
											 u8 adj = min_t(u8, MAX_ECORE_OFFSET_ADJUST,
															(sched_burst_penalty_ecore_scale_pct - 100) /
															ECORE_OFFSET_ADJ_DIV);
											 offset = max_t(u8, MIN_EFFECTIVE_OFFSET, offset - adj);
										 }
		} else if (ct == TOPO_CPU_TYPE_PERFORMANCE) {
			scale = bore_scale_tbl[clamp(sched_burst_penalty_pcore_scale_pct,
										 0U, 200U)];
		}
		}

		/* small ITD bias: highly-preferred CPUs get slightly smaller scale */
		if (itd_pr >= 512) {
			scale = scale * 88 / 100;
		}

			pp->scale  = scale;
		pp->offset = offset;
		pp->itd_pri = itd_pr;
		pp->sat_thresh_q8 = !scale ? U32_MAX :
		div_u64(((u64)MAX_BURST_PENALTY << 16) + scale - 1,
				scale);
		pp->gen    = (u8)atomic_read(&bore_penalty_gen);
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

	for_each_possible_cpu(cpu) {
		enum x86_topology_cpu_type t =
		get_topology_cpu_type(&cpu_data(cpu));
		per_cpu(bore_cpu_type, cpu) = t;
		if (t == TOPO_CPU_TYPE_PERFORMANCE ||
			t == TOPO_CPU_TYPE_EFFICIENCY)
			found_hybrid_info_this_pass = true;
		/* (Re)build penalty params for all CPUs after potential global changes */
		bore_build_penalty_param_for_cpu(cpu);
	}


	if (!found_hybrid_info_this_pass) {
		if (is_intel_hybrid() && !bore_cpu_types_detected)
			pr_info("Hybrid CPU, but no P/E info found yet. Will retry.\n");
		return;
	}

	if (!bore_cpu_types_detected) {
		pr_info("P/E core topology information detected.\n");
		bore_cpu_types_detected = true;
		sched_burst_core_aware_penalty   = 1;
		sched_burst_core_aware_smoothing = 1;

		if (is_intel_raptor_lake()) {
			pr_info("Applying Raptor-Lake P/E specific tunings.\n");
			sched_burst_penalty_ecore_scale_pct = 140;
			sched_burst_smoothness_long_e       = 3;
			sched_burst_smoothness_short_e      = 1;
			sched_burst_parity_threshold       += 2;
		} else if (is_intel_hybrid()) {
			pr_info("Applying generic Intel Hybrid P/E tunings (defaults).\n");
		}
		/*
		 * Global tunables might have changed, ensure all per-CPU params
		 * are rebuilt again to reflect these. This is slightly redundant
		 * for the first pass but ensures correctness if this function
		 * is called multiple times with evolving global state.
		 */
		for_each_possible_cpu(cpu)
			bore_build_penalty_param_for_cpu(cpu);
	}


	if (!static_branch_unlikely(&bore_core_aware_key)) {
		schedule_work(&bore_enable_key_work);
	}
}

static int bore_cpu_online_cb(unsigned int cpu)
{
	per_cpu(bore_cpu_type, cpu) =
	get_topology_cpu_type(&cpu_data(cpu));
	/* Build params for the newly online CPU first */
	bore_build_penalty_param_for_cpu(cpu);
	/* Then, check global topology features which might update global tunables
	 * and re-calculate for all if needed.
	 */
	bore_check_and_update_topology_features();
	return 0;
}

static enum cpuhp_state bore_cpuhp_state_val;

static int __init bore_topology_init(void)
{
	int ret_hp;

	bore_check_and_update_topology_features();

	ret_hp = cpuhp_setup_state_nocalls(CPUHP_AP_ONLINE_DYN,
									   "sched/bore:online",
									bore_cpu_online_cb,
									NULL);
	if (ret_hp < 0) {
		pr_err("cpuhp_setup_state_nocalls failed: %d\n", ret_hp);
	} else {
		bore_cpuhp_state_val = ret_hp;
	}
	return 0;
}

static int __init bore_late_topology_final_check(void)
{
	bore_check_and_update_topology_features();
	return 0;
}

/* ================================================================== */
/*                       8.  Penalty math                             */
/* ================================================================== */
static __always_inline u32 log2_u64_q24_8(u64 v)
{
	if (unlikely(!v))
		return 0;

	#if defined(__GNUC__) || defined(__clang__)
	u32 i = 63 - __builtin_clzll(v);
	u8  f = (u8)((v << (63 - i)) >> 55);
	#else
	u32 i = fls64(v) - 1;
	u8  f = (u8)(v << (63 - i) >> 55);
	#endif
	return (i << 8) | f;
}

/* ---------- fast helper fed from per-CPU cache  ------------------- */
static u32 __calc_burst_penalty_fast(u64 burst_time,
									 enum x86_topology_cpu_type ctype)
{
	const struct bore_penalty_param *pp = bore_get_param();

	s32 greed_q8 = (s32)log2_u64_q24_8(burst_time);
	s32 delta_q8 = greed_q8 - ((s32)pp->offset << 8);

	if (delta_q8 <= 0)
		return 0;

	/* early saturate */
	if ((u32)delta_q8 >= pp->sat_thresh_q8)
		return MAX_BURST_PENALTY;

	u64 tmp = (u64)(u32)delta_q8 * pp->scale;
	return (u32)min_t(u32, MAX_BURST_PENALTY, (u32)(tmp >> 16));
}

DEFINE_STATIC_CALL(bore_calc_penalty, __calc_burst_penalty_fast);

/* Original __calc_burst_penalty is no longer needed */

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
/*                   9.  Smoothing & prio helpers                     */
/* ================================================================== */
static __always_inline u32
binary_smooth(u32 new_val, u32 old_val, enum x86_topology_cpu_type ctype)
{
	int increment = (int)new_val - (int)old_val;
	u8 shift_long  = sched_burst_smoothness_long;
	u8 shift_short = sched_burst_smoothness_short;

	if (sched_burst_core_aware_smoothing &&
		static_branch_unlikely(&bore_core_aware_key)) {
		if (ctype == TOPO_CPU_TYPE_PERFORMANCE) {
			shift_long = sched_burst_smoothness_long_p;
			shift_short = sched_burst_smoothness_short_p;
		} else if (ctype == TOPO_CPU_TYPE_EFFICIENCY) {
			shift_long = sched_burst_smoothness_long_e;
			shift_short = sched_burst_smoothness_short_e;
		}
		}
		shift_long = min_t(u8, 31, shift_long);
	shift_short = min_t(u8, 31, shift_short);

	return increment >= 0 ? old_val + (increment >> shift_long)
	: old_val - ((-increment) >> shift_short);
}

static __always_inline u8 effective_prio(struct task_struct *p)
{
	int prio_val = NICE_TO_PRIO(p->static_prio);

	if (likely(sched_bore)) {
		prio_val = max(0, prio_val - (int)p->se.burst_score);
	}
	return min_t(int, NICE_WIDTH - 1, prio_val);
}

static void reweight_task_by_prio(struct task_struct *p, int prio_val)
{
	struct sched_entity *se = &p->se;
	struct cfs_rq *cfs_rq   = cfs_rq_of(se);

	prio_val = clamp(prio_val, 0, NICE_WIDTH - 1);
	reweight_entity(cfs_rq, se,
					scale_load(sched_prio_to_weight[prio_val]));
	se->load.inv_weight = sched_prio_to_wmult[prio_val];
}

/* ================================================================== */
/*                   10.  Hot-path updates                            */
/* ================================================================== */
void update_burst_penalty(struct sched_entity *se)
{
	struct rq *rq = rq_of(cfs_rq_of(se));

	se->curr_burst_penalty =
	static_call(bore_calc_penalty)(se->burst_time,
								   bore_get_rq_cpu_type(rq));

	se->burst_penalty = max(se->prev_burst_penalty,
							se->curr_burst_penalty);
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

void update_burst_score(struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se)))
		return;

	struct task_struct *p = task_of(se);
	u8 old_effective_prio = effective_prio(p);
	u8 new_score = 0;

	if (!((p->flags & PF_KTHREAD) && sched_burst_exclude_kthreads)) {
		new_score = se->burst_penalty >> 2;
	}

	/* ITD-aware booster for long-running tasks hogging P-cores */
	#ifdef CONFIG_SMP
	if (static_branch_unlikely(&bore_core_aware_key) &&
		arch_asym_cpu_priority(task_cpu(p)) > 0 &&
		se->burst_penalty >
		(MAX_BURST_PENALTY * sched_burst_pcore_hog_threshold_pct / 100)) {
		new_score = min_t(u8, NICE_WIDTH - 1,
						  new_score + sched_burst_pcore_hog_penalty_add);
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
	se->prev_burst_penalty =
	binary_smooth(se->curr_burst_penalty,
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
	s64 vruntime_remaining = se->deadline - se->vruntime; // Capture before penalty update

	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);

	u8 new_eff_prio = effective_prio(p);
	if (new_eff_prio > old_eff_prio) { // Priority got worse
		u64 abs_value = vruntime_remaining < 0 ? -vruntime_remaining : vruntime_remaining;
		u64 weight_units_remaining = __unscale_slice(abs_value, old_eff_prio);
		s64 vruntime_scaled_new = __scale_slice(weight_units_remaining, new_eff_prio);

		if (vruntime_remaining < 0)
			vruntime_scaled_new = -vruntime_scaled_new;
		se->deadline = se->vruntime + vruntime_scaled_new;
	}
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

/* ================================================================== */
/*                11.  Inheritance & cache                            */
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
									  u32 children_count, u32 children_penalty_sum, u64 now_ns)
{
	u8 avg_child_penalty = children_count ? (u8)(children_penalty_sum / children_count) : 0;
	bc->score = max(avg_child_penalty, p_owner_of_cache->se.burst_penalty);
	bc->count = children_count;
	bc->timestamp = now_ns;
}

static inline u32 count_children_upto2_rcu(struct list_head *children_list_head)
{
	struct list_head *first_child  = READ_ONCE(children_list_head->next);
	if (first_child == children_list_head)
		return 0;
	struct list_head *second_child = READ_ONCE(first_child->next);
	return 1 + (second_child != children_list_head);
}

static void __update_child_burst_direct_locked(struct task_struct *p, u64 now_ns)
{
	u32 count = 0, sum_penalties = 0;
	struct task_struct *child_iter;

	for_each_child_bore(p, child_iter) {
		prefetch(child_iter->sibling.next);
		prefetch(&child_iter->children);
		if (task_is_bore_eligible(child_iter)) {
			count++;
			sum_penalties += child_iter->se.burst_penalty;
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

	for_each_child_bore(p, child_iter) {
		prefetch(child_iter->sibling.next);
		prefetch(&child_iter->children);

		effective_descendant = child_iter;
		while (count_children_upto2_rcu(&effective_descendant->children) == 1) {
			struct list_head *first_head = rcu_dereference(effective_descendant->children.next);
			if (first_head == &effective_descendant->children) break;
			effective_descendant = list_entry_rcu(first_head, struct task_struct, sibling);
		}

		if (recursion_depth == 0 || list_empty_careful(&effective_descendant->children)) {
			if (task_is_bore_eligible(effective_descendant)) {
				current_level_direct_children_count++;
				current_level_sum_of_penalties += effective_descendant->se.burst_penalty;
			}
			continue;
		}

		struct sched_burst_cache *desc_cache = &effective_descendant->se.child_burst;
		spin_lock(&desc_cache->lock);
		if (!burst_cache_expired(desc_cache, now_ns)) {
			current_level_direct_children_count += desc_cache->count;
			current_level_sum_of_penalties      +=
			(u32)desc_cache->score * desc_cache->count;
		} else {
			__update_child_burst_topological_locked(effective_descendant,
													now_ns,
										   recursion_depth - 1,
										   &current_level_direct_children_count,
										   &current_level_sum_of_penalties);
		}
		spin_unlock(&desc_cache->lock);

		if (sched_burst_cache_stop_count > 0 &&
			current_level_direct_children_count >= sched_burst_cache_stop_count)
			break;
	}

	update_burst_cache_locked(&p->se.child_burst, p, current_level_direct_children_count,
							  current_level_sum_of_penalties, now_ns);
	*accumulated_count += current_level_direct_children_count;
	*accumulated_sum += current_level_sum_of_penalties;
}

static u8 inherit_burst_direct(struct task_struct *parent_task,
							   u64 now_ns, u64 clone_flags)
{
	struct task_struct *target_for_inheritance = parent_task;
	unsigned long irqflags;
	u8 score = 0;

	if (clone_flags & CLONE_PARENT) {
		target_for_inheritance = rcu_dereference(parent_task->real_parent);
	}
	if (unlikely(!target_for_inheritance)) return 0;

	spin_lock_irqsave(&target_for_inheritance->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&target_for_inheritance->se.child_burst, now_ns)) {
		__update_child_burst_direct_locked(target_for_inheritance, now_ns);
	}
	score = target_for_inheritance->se.child_burst.score;
	spin_unlock_irqrestore(&target_for_inheritance->se.child_burst.lock, irqflags);
	return score;
}

static u8 inherit_burst_topological(struct task_struct *parent_task,
									u64 now_ns, u64 clone_flags)
{
	struct task_struct *ancestor_for_inheritance = parent_task;
	u32 child_count_threshold = (clone_flags & CLONE_PARENT) ? 1 : 0;
	unsigned long irqflags;
	u8 score = 0;
	u32 dummy_children_count = 0, dummy_penalty_sum = 0;

	if (clone_flags & CLONE_PARENT) {
		ancestor_for_inheritance = rcu_dereference(ancestor_for_inheritance->real_parent);
	}
	if (unlikely(!ancestor_for_inheritance)) return 0;

	while (ancestor_for_inheritance->real_parent != ancestor_for_inheritance &&
		ancestor_for_inheritance->real_parent != NULL) {
		if (count_children_upto2_rcu(&ancestor_for_inheritance->children) > child_count_threshold)
			break;
		ancestor_for_inheritance = rcu_dereference(ancestor_for_inheritance->real_parent);
	if (unlikely(!ancestor_for_inheritance)) return 0;
	child_count_threshold = 1;
		}
		if (unlikely(!ancestor_for_inheritance)) return 0;

		spin_lock_irqsave(&ancestor_for_inheritance->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&ancestor_for_inheritance->se.child_burst, now_ns)) {
		u32 recursion_depth = (sched_burst_fork_atavistic > 1) ? (sched_burst_fork_atavistic - 1) : 0;
		__update_child_burst_topological_locked(ancestor_for_inheritance, now_ns, recursion_depth,
												&dummy_children_count, &dummy_penalty_sum);
	}
	score = ancestor_for_inheritance->se.child_burst.score;
	spin_unlock_irqrestore(&ancestor_for_inheritance->se.child_burst.lock, irqflags);
	return score;
}

static void __update_tg_burst_locked(struct task_struct *group_leader, u64 now_ns)
{
	u32 thread_count = 0, sum_penalties = 0;
	struct task_struct *thread_iter;

	for_each_thread(group_leader, thread_iter) {
		if (task_is_bore_eligible(thread_iter)) {
			thread_count++;
			sum_penalties += thread_iter->se.burst_penalty;
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
	u8 score = 0;

	if (unlikely(!group_leader)) return 0;

	spin_lock_irqsave(&group_leader->se.group_burst.lock, irqflags);
	if (burst_cache_expired(&group_leader->se.group_burst, now_ns)) {
		__update_tg_burst_locked(group_leader, now_ns);
	}
	score = group_leader->se.group_burst.score;
	spin_unlock_irqrestore(&group_leader->se.group_burst.lock, irqflags);
	return score;
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
	se_new->child_burst.score = 0; se_new->child_burst.count = 0;
	se_new->group_burst.timestamp = 0;
	se_new->group_burst.score = 0; se_new->group_burst.count = 0;

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
	se_new->burst_penalty      = inherited_penalty;
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

/* ================================================================== */
/*                       12.  Reset helpers                           */
/* ================================================================== */
void reset_task_bore(struct task_struct *p)
{
	if (unlikely(!p)) return;
	struct sched_entity *se = &p->se;

	se->burst_time = 0;
	se->prev_burst_penalty = 0;
	se->curr_burst_penalty = 0;
	se->burst_penalty = 0;
	se->burst_score = 0;

	se->child_burst.score     = 0;
	se->child_burst.count     = 0;
	se->child_burst.timestamp = 0;

	se->group_burst.score     = 0;
	se->group_burst.count     = 0;
	se->group_burst.timestamp = 0;
}
EXPORT_SYMBOL_GPL(reset_task_bore);

static void reset_all_task_weights_for_bore_toggle(void)
{
	struct task_struct *g, *t;

	pr_info("Global BORE state changed, resetting all eligible task weights...\n");

	rcu_read_lock();
	for_each_process(g) {
		for_each_thread(g, t) {
			if (!task_is_bore_eligible(t))
				continue;
			if (!tryget_task_struct(t))
				continue;

			struct rq *rq = task_rq(t);
			if (!rq || !t->on_rq) {
				put_task_struct(t);
				continue;
			}

			struct rq_flags rf;
			rq_pin_lock(rq, &rf);
			if (task_rq(t) == rq && t->on_rq) {
				update_rq_clock(rq);
				reweight_task_by_prio(t, effective_prio(t));
			}
			rq_unpin_lock(rq, &rf);
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

/*
 * numeric limits used by several entries
 */
static const int bore_sysctl_val_three        = 3;
static const int bore_sysctl_val_nicew        = NICE_WIDTH;
static const int bore_sysctl_val_smooth_max   = 10;
static const int bore_sysctl_val_offset_max   = 64;
static const int bore_sysctl_val_scale_max    = BORE_ORIG_BURST_PENALTY_SCALE * 4;
static const int bore_sysctl_val_pct_max      = 200;
static const int bore_sysctl_val_stopcnt_max  = 4096;

/*
 * Wrapper for the global sched_bore on/off switch.
 * Prototype MUST exactly match <linux/sysctl.h>
 */
static int
sched_bore_toggle_sysctl_handler(const struct ctl_table *table,
								 int write, void *buffer,
								 size_t *lenp, loff_t *ppos)
{
	u8 old_val = *(u8 *)table->data;

	int ret = proc_dou8vec_minmax((struct ctl_table *)table,
								  write, buffer, lenp, ppos);

	if (!ret && write && old_val != *(u8 *)table->data)
		reset_all_task_weights_for_bore_toggle();

	return ret;
}

/* generic helpers that also refresh the per-CPU penalty cache */
static int bore_u8_sysctl_gen_handler(const struct ctl_table *table,
									  int write, void *buffer,
									  size_t *lenp, loff_t *ppos)
{
	u8 old = *(u8 *)table->data;
	int ret = proc_dou8vec_minmax((struct ctl_table *)table,
								  write, buffer, lenp, ppos);
	if (!ret && write && old != *(u8 *)table->data)
		bore_bump_penalty_gen();
	return ret;
}

static int bore_uint_sysctl_gen_handler(const struct ctl_table *table,
										int write, void *buffer,
										size_t *lenp, loff_t *ppos)
{
	uint old = *(uint *)table->data;
	int ret  = proc_douintvec_minmax((struct ctl_table *)table,
									 write, buffer, lenp, ppos);
	if (!ret && write && old != *(uint *)table->data)
		bore_bump_penalty_gen();
	return ret;
}

/*
 * Sysctl array
 */
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
	/* --- Core-aware knobs ------------------------------------------------ */
	{
		.procname       = "sched_burst_core_aware_penalty",
		.data           = &sched_burst_core_aware_penalty,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
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
	{
		.procname       = "sched_burst_smoothness_long_p",
		.data           = &sched_burst_smoothness_long_p,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname       = "sched_burst_smoothness_short_p",
		.data           = &sched_burst_smoothness_short_p,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname       = "sched_burst_smoothness_long_e",
		.data           = &sched_burst_smoothness_long_e,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname       = "sched_burst_smoothness_short_e",
		.data           = &sched_burst_smoothness_short_e,
		.maxlen         = sizeof(u8),
		.mode           = 0644,
		.proc_handler   = proc_dou8vec_minmax,
		.extra1         = SYSCTL_ZERO,
		.extra2         = (void *)&bore_sysctl_val_smooth_max,
	},
	{
		.procname     = "sched_burst_pcore_hog_threshold_pct",
		.data         = &sched_burst_pcore_hog_threshold_pct,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_pct_max,
	},
	{
		.procname     = "sched_burst_pcore_hog_penalty_add",
		.data         = &sched_burst_pcore_hog_penalty_add,
		.maxlen       = sizeof(u8),
		.mode         = 0644,
		.proc_handler = bore_u8_sysctl_gen_handler,
		.extra1       = SYSCTL_ZERO,
		.extra2       = (void *)&bore_sysctl_val_nicew,
	},
	{ .procname = NULL }   /* Sentinel - marks end of table */
};

static struct ctl_table_header *bore_sysctl_header_ptr;

static int __init bore_sysctl_init_func(void)
{
	size_t table_actual_entries = ARRAY_SIZE(bore_sysctls) - 1;

	bore_sysctl_header_ptr = register_sysctl_sz("kernel", bore_sysctls, table_actual_entries);

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
	pr_info("BORE v%s initialising (HZ=%d)\n",
			SCHED_BORE_VERSION, HZ);

	bore_build_scale_tbl();
	INIT_WORK(&bore_enable_key_work, bore_enable_key_workfn);

	if (is_intel_hybrid()) {
		pr_info("Intel Hybrid CPU detected, applying general hybrid default tweaks.\n");
		sched_burst_parity_threshold += 2;
		sched_burst_smoothness_short =
		max_t(u8, 1, BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1);
		sched_burst_fork_atavistic   = 0;
		sched_burst_exclude_kthreads = 1;
	}
	/* Raptor Lake specific global tunables are set in bore_check_and_update_topology_features */

	reset_task_bore(&init_task);
	spin_lock_init(&init_task.se.child_burst.lock);
	spin_lock_init(&init_task.se.group_burst.lock);

	pr_info("Early init done. Core-aware features status pending full CPU topology detection.\n");
}

#else   /* !CONFIG_SCHED_BORE – stubs */

void __init sched_bore_init(void) { /* Empty init */ }
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
					  // These initcalls MUST be at file scope.
					  core_initcall(bore_topology_init);
					  late_initcall_sync(bore_late_topology_final_check);

					  #ifdef CONFIG_SYSCTL
					  late_initcall_sync(bore_sysctl_init_func);
					  #endif // CONFIG_SYSCTL

					  #endif // CONFIG_SCHED_BORE
