// SPDX-License-Identifier: GPL-2.0
/*
 *  Burst-Oriented Response Enhancer (BORE) Scheduler
 *  Upstream base: v5.9.6         Target kernel: >=6.8
 *  Hybrid-aware, Raptor-Lake-tuned – audit-fixes 2025-04-14
 *
 *  Maintainer:  Masahito Suzuki <firelzrd@gmail.com>
 *
 *  This file is intentionally verbose.  Do NOT strip comments.
 */

// Undefine pr_fmt before redefining it locally to avoid -Wmacro-redefined
#undef pr_fmt
#define pr_fmt(fmt) "BORE: " fmt

/* ================================================================== */
/*                          0.  Headers                               */
/* ================================================================== */
#include <linux/kernel.h>          /* printk()                         */
#include <linux/module.h>
#include <linux/init.h>
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
#include <linux/list.h>
#include <linux/slab.h>
#include <linux/math64.h>          /* abs64()                         */
#include <linux/cpuhotplug.h>
#include <linux/workqueue.h>
#include <linux/cpu.h>             /* cpu_online(), topology helpers  */

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

/* Original BORE defaults (used for initialization and as base for some tables) */
#define BORE_ORIG_SCHED_BORE                   1
#define BORE_ORIG_BURST_EXCLUDE_KTHREADS       1
#define BORE_ORIG_BURST_SMOOTHNESS_LONG        1 /* Shift value for slow increase */
#define BORE_ORIG_BURST_SMOOTHNESS_SHORT       0 /* Shift value for fast decrease */
#define BORE_ORIG_BURST_FORK_ATAVISTIC         2 /* 0=off, 1=direct parent, >=2 topological depth */
#define BORE_ORIG_BURST_PARITY_THRESHOLD       2 /* Min cfs_rq->nr_queued to apply RUN_TO_PARITY */
#define BORE_ORIG_BURST_PENALTY_OFFSET         24 /* Base offset for penalty calc (non-Q8) */
#define BORE_ORIG_BURST_PENALTY_SCALE          1280 /* Base scale factor (scaled by 2^16) */
#define BORE_ORIG_BURST_CACHE_STOP_COUNT       64 /* Max children to scan for topological inheritance */
#define BORE_ORIG_BURST_CACHE_LIFETIME         (75 * 1000 * 1000) /* 75 ms in ns */
#define BORE_ORIG_DEADLINE_BOOST_MASK          (ENQUEUE_INITIAL | ENQUEUE_WAKEUP) /* Event mask */

/* helpers to declare __read_mostly globals */
#define DEF_U8(name, val)   u8   __read_mostly name = (val)
#define DEF_U32(name, val)  uint __read_mostly name = (val)

/* generic knobs */
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

/* hybrid extras (default off) */
DEF_U8(sched_burst_core_aware_penalty,    0);
DEF_U8(sched_burst_core_aware_smoothing,  0);
DEF_U32(sched_burst_penalty_pcore_scale_pct, 100);
DEF_U32(sched_burst_penalty_ecore_scale_pct, 100);
DEF_U8(sched_burst_smoothness_long_p,  BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_p, BORE_ORIG_BURST_SMOOTHNESS_SHORT);
DEF_U8(sched_burst_smoothness_long_e,  BORE_ORIG_BURST_SMOOTHNESS_LONG);
DEF_U8(sched_burst_smoothness_short_e, BORE_ORIG_BURST_SMOOTHNESS_SHORT);

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
DEFINE_STATIC_KEY_FALSE(bore_core_aware_key); /* Controls P/E core specific logic paths */
/* Could be __ro_after_init for minor optimization */

/* ================================================================== */
/*                 4.  Per-CPU CPU-type cache                         */
/* ================================================================== */
DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type) =
TOPO_CPU_TYPE_UNKNOWN;

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
/*
 * This table stores scaled penalty values. Index is percentage (0-200).
 * The scale is applied to BORE_ORIG_BURST_PENALTY_SCALE.
 * If sched_burst_core_aware_penalty is active, this table is used.
 * Otherwise, the global sched_burst_penalty_scale sysctl is used directly.
 */
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
	/* Check CPUID HYBRID bit or specific known hybrid model families */
	return boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake();
}

/* ================================================================== */
/*          7.  Topology scan + deferred static-key enable            */
/* ================================================================== */
static bool bore_cpu_types_detected; /* global latch, true if P/E info found on any CPU */

static void bore_enable_key_workfn(struct work_struct *w);
/* Work item to defer static_branch_enable from atomic contexts (like hotplug callbacks) */
static DECLARE_WORK(bore_enable_key_work, bore_enable_key_workfn); /* Uses system_wq by default */

static void bore_enable_key_workfn(struct work_struct *w)
{
	/* Idempotent check: only enable if not already enabled */
	if (!static_branch_unlikely(&bore_core_aware_key)) {
		static_branch_enable(&bore_core_aware_key);
		pr_info("Core-aware static key enabled via workqueue.\n");
	}
}

/*
 * Common function to detect CPU types and configure core-aware features.
 * Can be called early during boot, late, or from CPU hotplug context (indirectly).
 */
static void bore_check_and_update_topology_features(void)
{
	unsigned int cpu;
	bool found_hybrid_info_this_pass = false;

	/*
	 * If P/E types already detected and static key enabled, most of this work is done.
	 * However, a subsequent call (e.g., from late_init or another CPU hotplug)
	 * might be for a CPU whose type wasn't available initially.
	 * The per_cpu(bore_cpu_type, cpu) update is always needed for each CPU.
	 */
	for_each_possible_cpu(cpu) {
		enum x86_topology_cpu_type t =
		get_topology_cpu_type(&cpu_data(cpu)); /* Use common cpu_data() */
		per_cpu(bore_cpu_type, cpu) = t;
		if (t == TOPO_CPU_TYPE_PERFORMANCE ||
			t == TOPO_CPU_TYPE_EFFICIENCY)
			found_hybrid_info_this_pass = true;
	}

	if (!found_hybrid_info_this_pass) {
		if (is_intel_hybrid() && !bore_cpu_types_detected) /* Only print if first-time failure on hybrid */
			pr_info("Hybrid CPU, but no P/E info found yet. Will retry.\n");
		return; /* No P/E info found in this pass */
	}

	/* P/E info found on at least one CPU */
	if (!bore_cpu_types_detected) { /* First time P/E info is detected globally */
		pr_info("P/E core topology information detected.\n");
		bore_cpu_types_detected = true;

		/* Apply default hybrid knobs only once when P/E info is first confirmed */
		sched_burst_core_aware_penalty   = 1;
		sched_burst_core_aware_smoothing = 1;

		if (is_intel_raptor_lake()) {
			pr_info("Applying Raptor-Lake P/E specific tunings.\n");
			sched_burst_penalty_ecore_scale_pct = 120;
			sched_burst_smoothness_long_e = 2;
			sched_burst_smoothness_short_e = 1;
			/* P-core scale_pct and P/E long_p/short_p remain at their defaults (100% / BORE_ORIG_*) */
		} else if (is_intel_hybrid()) {
			pr_info("Applying generic Intel Hybrid P/E tunings (defaults).\n");
			/*
			 * For non-RPL hybrid, typically stick to 100% scale and BORE_ORIG_ smoothness values,
			 * which are already the defaults for the _pcore_scale_pct, _ecore_scale_pct,
			 * and _p/_e smoothness variables. Explicitly setting them here is for clarity if desired.
			 * sched_burst_penalty_pcore_scale_pct = 100;
			 * sched_burst_penalty_ecore_scale_pct = 100;
			 * sched_burst_smoothness_long_p  = BORE_ORIG_BURST_SMOOTHNESS_LONG;
			 * etc.
			 */
		}
	}

	/* Schedule static key enable if not already active. Safe to call multiple times. */
	if (!static_branch_unlikely(&bore_core_aware_key)) {
		if (!schedule_work(&bore_enable_key_work)) {
			/* This case (schedule_work returning false if work already pending) is fine. */
		}
	}
}

/* Hotplug notifier callback (runs in atomic context, hence deferred static_key enable) */
static int bore_cpu_online_cb(unsigned int cpu)
{
	/* Update this CPU's type information */
	per_cpu(bore_cpu_type, cpu) =
	get_topology_cpu_type(&cpu_data(cpu));
	/* Re-check global topology and potentially enable core-aware features */
	bore_check_and_update_topology_features();
	return 0;
}

static enum cpuhp_state bore_cpuhp_state_val; /* Store registered cpuhp state */

/* Called via core_initcall */
static int __init bore_topology_init(void)
{
	int ret_hp;

	bore_check_and_update_topology_features(); /* Initial check at boot */

	/* Register CPU hotplug notifier */
	ret_hp = cpuhp_setup_state_nocalls(CPUHP_AP_ONLINE_DYN,
									   "sched/bore:online", /* Name for cpuhp state */
									bore_cpu_online_cb,
									NULL /* No teardown callback needed for this simple case */);
	if (ret_hp < 0) {
		/* Use pr_fmt-aware printing */
		pr_err("cpuhp_setup_state_nocalls failed: %d\n", ret_hp);
		/* Not returning error from core_initcall, BORE can still partially function */
	} else {
		bore_cpuhp_state_val = ret_hp; /* Store the successfully registered state */
	}
	return 0;
}
core_initcall(bore_topology_init); /* Run early in boot sequence */

/* Called via late_initcall_sync */
static int __init bore_late_topology_final_check(void) /* Renamed for clarity */
{
	bore_check_and_update_topology_features(); /* Re-check at late_init */
	return 0;
}
late_initcall_sync(bore_late_topology_final_check); /* Ensures it runs after most topology setup */

/* ================================================================== */
/*                       8.  Penalty math                             */
/* ================================================================== */

/* Fast log2 helper for u64, returns Q24.8 fixed-point (integer part in high 24 bits, fractional in low 8) */
static __always_inline u32 log2_u64_q24_8(u64 v)
{
	if (unlikely(!v)) /* log2(0) is undefined, return 0 */
		return 0;

	/* fls64(v) returns 1 + floor(log2(v)), so (fls64(v) - 1) is floor(log2(v)) */
	u32 i = fls64(v) - 1;
	/*
	 * To get fractional part:
	 * Normalize v so its MSB is at bit 63: v_norm = v << (63 - i).
	 * Then extract the top 8 bits after the MSB (which are bits 62 down to 55 of v_norm).
	 * This is achieved by (v_norm >> 55).
	 */
	u8  f = (u8)(v << (63 - i) >> 55);
	return (i << 8) | f;
}

static __always_inline u32
__calc_burst_penalty(u64 burst_time, enum x86_topology_cpu_type ctype)
{
	s32 greed_q8;
	s32 offset_non_q8; /* Original offset value, not Q8 */
	s32 offset_q8_final;
	s32 delta_q8;
	u32 penalty_val_q8;
	u32 scale_factor;
	u64 temp_scaled_penalty;

	greed_q8 = (s32)log2_u64_q24_8(burst_time);
	offset_non_q8 = sched_burst_penalty_offset;

	if (sched_burst_core_aware_penalty &&
		static_branch_unlikely(&bore_core_aware_key) &&
		ctype == TOPO_CPU_TYPE_EFFICIENCY &&
		sched_burst_penalty_ecore_scale_pct > 100) { /* E-core offset reduction condition */
			u8 adj = min_t(u8, MAX_ECORE_OFFSET_ADJUST,
						   (u8)((sched_burst_penalty_ecore_scale_pct - 100) /
						   ECORE_OFFSET_ADJ_DIV));
			offset_non_q8 = max_t(s32, MIN_EFFECTIVE_OFFSET, offset_non_q8 - adj);
		}
		offset_q8_final = offset_non_q8 << 8; /* Convert final offset to Q8 */

		delta_q8 = greed_q8 - offset_q8_final;
		if (delta_q8 < 0)
			delta_q8 = 0;
	penalty_val_q8 = (u32)delta_q8;

	scale_factor = sched_burst_penalty_scale; /* Default scale factor */
	if (sched_burst_core_aware_penalty &&
		static_branch_unlikely(&bore_core_aware_key)) {
		u32 idx = (ctype == TOPO_CPU_TYPE_EFFICIENCY)
		? sched_burst_penalty_ecore_scale_pct
		: sched_burst_penalty_pcore_scale_pct;
	scale_factor = bore_scale_tbl[clamp(idx, 0U, 200U)]; /* Use LUT for core-aware scaling */
		}

		/*
		 * Calculation: (penalty_val_q8 * scale_factor) >> 16
		 * penalty_val_q8 is UQ8.8 (effective value = penalty_val_q8 / 256)
		 * scale_factor is intended to be scaled by 2^16 (e.g., 1280 means 1280/65536)
		 * So, ( (UQ8.8 / 256.0) * (scale_factor / 65536.0) )
		 * To get back to Q8.8 for MAX_BURST_PENALTY comparison, we need to multiply by 256.
		 * (UQ8.8 * scale_factor / 65536) -> result is scaled by 256 too much.
		 * The original BORE calculation was `penalty * scale >> 16`, where penalty was Q8.8.
		 * This implies scale is effectively an integer multiplier, and then the whole product is scaled down.
		 */
		temp_scaled_penalty = mul_u32_u32(penalty_val_q8, scale_factor);
		return min_t(u32, MAX_BURST_PENALTY, (u32)(temp_scaled_penalty >> 16));
}

/* helpers for EEVDF weight/vruntime conversion (params: d=delta, pr=CFS priority) */
static __always_inline u64 __scale_slice(u64 d, u8 pr)
{
	pr = min_t(u8, NICE_WIDTH - 1, pr); /* Clamp priority to valid CFS range */
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
	u8 shift_long  = sched_burst_smoothness_long;  /* Default global smoothness for slow increase */
	u8 shift_short = sched_burst_smoothness_short; /* Default global smoothness for fast decrease */

	if (sched_burst_core_aware_smoothing &&
		static_branch_unlikely(&bore_core_aware_key)) {
		if (ctype == TOPO_CPU_TYPE_PERFORMANCE) {
			shift_long = sched_burst_smoothness_long_p;
			shift_short = sched_burst_smoothness_short_p;
		} else if (ctype == TOPO_CPU_TYPE_EFFICIENCY) {
			shift_long = sched_burst_smoothness_long_e;
			shift_short = sched_burst_smoothness_short_e;
		}
		/* If ctype is UNKNOWN or other, global smoothing values remain active. */
		}
		shift_long = min_t(u8, 31, shift_long);   /* Max shift for u32/s32 */
		shift_short = min_t(u8, 31, shift_short);

		return increment >= 0 ? old_val + (increment >> shift_long)
		: old_val - ((-increment) >> shift_short);
}

static __always_inline u8 effective_prio(struct task_struct *p)
{
	int prio_val = NICE_TO_PRIO(p->static_prio); /* Convert nice value (-20 to +19) to CFS priority (0 to 39) */

	if (likely(sched_bore)) { /* Only adjust if BORE is globally enabled */
		prio_val = max(0, prio_val - (int)p->se.burst_score); /* Lower numerical prio is higher sched prio */
	}
	/* Corrected indentation: return is part of the function, not the if block */
	return min_t(int, NICE_WIDTH - 1, prio_val); /* Clamp to valid CFS priority range */
}

static void reweight_task_by_prio(struct task_struct *p, int prio_val)
{
	struct sched_entity *se = &p->se;
	struct cfs_rq *cfs_rq   = cfs_rq_of(se);

	prio_val = clamp(prio_val, 0, NICE_WIDTH - 1);
	/* reweight_entity is defined in fair.c */
	reweight_entity(cfs_rq, se,
					scale_load(sched_prio_to_weight[prio_val]));
	se->load.inv_weight = sched_prio_to_wmult[prio_val];
}

/* ================================================================== */
/*                   10.  Hot-path updates                            */
/* ================================================================== */

/* Called regularly for running task (e.g. from update_curr in fair.c) */
void update_burst_penalty(struct sched_entity *se)
{
	struct rq *rq = rq_of(cfs_rq_of(se));

	se->curr_burst_penalty =
	__calc_burst_penalty(se->burst_time,
						 bore_get_rq_cpu_type(rq));

	se->burst_penalty = max(se->prev_burst_penalty,
							se->curr_burst_penalty);
	update_burst_score(se); /* This will call reweight_task_by_prio if prio changes */
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

void update_burst_score(struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se))) /* Only for tasks, not group entities */
		return;

	struct task_struct *p = task_of(se);
	u8 old_effective_prio = effective_prio(p); /* Prio based on current se->burst_score */
	u8 new_score = 0;

	if (!((p->flags & PF_KTHREAD) && sched_burst_exclude_kthreads)) {
		new_score = se->burst_penalty >> 2; /* Score is a fraction of total penalty */
	}

	if (se->burst_score == new_score) {
		/*
		 * Score itself hasn't changed.
		 * Effective_prio might still change if sched_bore global toggle changed.
		 * This check ensures reweighting if that happens.
		 */
		if (unlikely(effective_prio(p) != old_effective_prio))
			reweight_task_by_prio(p, effective_prio(p));
		return;
	}

	se->burst_score = new_score;
	/* Effective prio has changed due to score change, reweight is needed. */
	reweight_task_by_prio(p, effective_prio(p));
}
EXPORT_SYMBOL_GPL(update_burst_score);

/* Internal helper to update smoothed penalty and reset current burst state */
static void revolve_burst_penalty_state(struct sched_entity *se,
										enum x86_topology_cpu_type ctype)
{
	se->prev_burst_penalty =
	binary_smooth(se->curr_burst_penalty,
				  se->prev_burst_penalty, ctype);
	se->burst_time         = 0;    /* Reset burst time accumulator */
	se->curr_burst_penalty = 0;    /* Current burst's penalty is now zero */
}

/* Called when task sleeps or blocks (e.g. from dequeue_task_fair) */
void restart_burst(struct sched_entity *se)
{
	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty; /* New active penalty is the smoothed historical one */
	update_burst_score(se); /* Update score and potentially reweight */
}
EXPORT_SYMBOL_GPL(restart_burst);

/* Called when task yields (e.g. from yield_task_fair) */
void restart_burst_rescale_deadline(struct sched_entity *se)
{
	if (unlikely(!entity_is_task(se)))
		return;

	struct task_struct *p = task_of(se);
	u8 old_eff_prio = effective_prio(p); /* Prio before yield's penalty processing */

	revolve_burst_penalty_state(se, bore_get_rq_cpu_type(rq_of(cfs_rq_of(se))));
	se->burst_penalty = se->prev_burst_penalty; /* Update active penalty from smoothed history */
	update_burst_score(se);                     /* Update score and reweight based on new penalty */

	u8 new_eff_prio = effective_prio(p);        /* Get new prio after score update */
	if (new_eff_prio > old_eff_prio) { /* If priority got worse (numerically higher) due to penalty */
		s64 vruntime_remaining = se->deadline - se->vruntime;
		u64 abs_value = vruntime_remaining < 0 ? -vruntime_remaining : vruntime_remaining;
		u64 weight_units_remaining = __unscale_slice(abs_value, old_eff_prio);
		s64 vruntime_scaled_new = __scale_slice(weight_units_remaining, new_eff_prio);

		if (vruntime_remaining < 0) /* Was already late */
			vruntime_scaled_new = -vruntime_scaled_new;
		se->deadline = se->vruntime + vruntime_scaled_new;
	}
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

/* ================================================================== */
/*                11.  Inheritance & cache                            */
/* ================================================================== */

/* RCU-safe iteration over task's children */
#define for_each_child_bore(p_task, child_task) \
list_for_each_entry_rcu(child_task, &(p_task)->children, sibling)

static inline bool task_is_bore_eligible(struct task_struct *p)
{
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

/* Initialize spinlocks within sched_burst_cache structs embedded in sched_entity */
static inline void init_task_burst_cache_lock(struct task_struct *p)
{
	spin_lock_init(&p->se.child_burst.lock);
	spin_lock_init(&p->se.group_burst.lock);
}

static inline bool burst_cache_expired(struct sched_burst_cache *bc, u64 now_ns)
{
	/* Check if cache entry is older than its lifetime */
	return (s64)(bc->timestamp + sched_burst_cache_lifetime - now_ns) < 0;
}

/* Assumes bc->lock held by caller. Updates a burst cache entry. */
static void update_burst_cache_locked(struct sched_burst_cache *bc,
									  struct task_struct *p_owner_of_cache,
									  u32 children_count, u32 children_penalty_sum, u64 now_ns)
{
	u8 avg_child_penalty = children_count ? (u8)(children_penalty_sum / children_count) : 0;
	/* Cache score is max of children's average penalty or parent's own current penalty */
	bc->score = max(avg_child_penalty, p_owner_of_cache->se.burst_penalty);
	bc->count = children_count;
	bc->timestamp = now_ns;
}

/*
 * Fast “up-to-2” counter for list entries.
 * Caller must hold RCU read lock if 'children_list_head' can be modified concurrently.
 * For task_struct->children, this is usually the case.
 */
static inline u32 count_children_upto2_rcu(struct list_head *children_list_head)
{
	struct list_head *first_child  = READ_ONCE(children_list_head->next); /* RCU-safe read */
	if (first_child == children_list_head) /* Empty list */
		return 0;
	struct list_head *second_child = READ_ONCE(first_child->next); /* RCU-safe read */
	return 1 + (second_child != children_list_head); /* 1 if one child, 2 if two or more */
}


/* Assumes p->se.child_burst.lock held. Caller must hold RCU read lock. */
static void __update_child_burst_direct_locked(struct task_struct *p, u64 now_ns)
{
	u32 count = 0, sum_penalties = 0;
	struct task_struct *child_iter;

	/* RCU read lock held by caller allows safe traversal of p->children */
	for_each_child_bore(p, child_iter) {
		if (task_is_bore_eligible(child_iter)) {
			count++;
			sum_penalties += child_iter->se.burst_penalty;
		}
	}
	update_burst_cache_locked(&p->se.child_burst, p, count, sum_penalties, now_ns);
}

/* Assumes p->se.child_burst.lock held. Caller must hold RCU read lock.
 * Recursively updates child burst cache based on descendants.
 * accumulated_count and accumulated_sum are in/out parameters.
 */
static void __update_child_burst_topological_locked(struct task_struct *p, u64 now_ns,
													u32 recursion_depth,
													u32 *accumulated_count, u32 *accumulated_sum)
{
	u32 current_level_direct_children_count = 0;
	u32 current_level_sum_of_penalties = 0;
	struct task_struct *child_iter, *effective_descendant;

	/* RCU read lock held by caller allows safe traversal of p->children */
	for_each_child_bore(p, child_iter) {
		effective_descendant = child_iter;

		/* Traverse down single-child branches to find the "significant" descendant */
		while (count_children_upto2_rcu(&effective_descendant->children) == 1) {
			struct list_head *first_head = rcu_dereference(effective_descendant->children.next);
			if (first_head == &effective_descendant->children) {
				break;
			}
			effective_descendant = list_entry_rcu(first_head, struct task_struct, sibling);
		}

		/* Base case for recursion: leaf node (no children) or depth limit reached */
		if (recursion_depth == 0 || list_empty_careful(&effective_descendant->children)) {
			if (task_is_bore_eligible(effective_descendant)) {
				current_level_direct_children_count++;
				current_level_sum_of_penalties += effective_descendant->se.burst_penalty;
			}
			continue; /* To next child of 'p' */
		}

		/* Recursive case: process descendant's cache or recurse further */
		struct sched_burst_cache *desc_cache = &effective_descendant->se.child_burst;
		spin_lock(&desc_cache->lock); /* Lock descendant's child_burst cache */
		if (!burst_cache_expired(desc_cache, now_ns)) {
			/* Use cached values from descendant */
			current_level_direct_children_count += desc_cache->count;
			current_level_sum_of_penalties += (u32)desc_cache->score * desc_cache->count;
		} else {
			/* Descendant cache expired, recurse. */
			__update_child_burst_topological_locked(effective_descendant, now_ns, recursion_depth - 1,
													&current_level_direct_children_count,
										   &current_level_sum_of_penalties);
		}
		spin_unlock(&desc_cache->lock);

		if (sched_burst_cache_stop_count > 0 &&
			current_level_direct_children_count >= sched_burst_cache_stop_count)
			break; /* Stop scanning if limit reached */
	}

	/* Update p's own child_burst cache with aggregated values from this level */
	update_burst_cache_locked(&p->se.child_burst, p, current_level_direct_children_count,
							  current_level_sum_of_penalties, now_ns);

	/* Add this level's findings to the total accumulators passed by the caller */
	*accumulated_count += current_level_direct_children_count;
	*accumulated_sum += current_level_sum_of_penalties;
}

/* Assumes caller holds RCU read lock. */
static u8 inherit_burst_direct(struct task_struct *parent_task,
							   u64 now_ns, u64 clone_flags)
{
	struct task_struct *target_for_inheritance = parent_task;
	unsigned long irqflags;
	u8 score = 0;

	if (clone_flags & CLONE_PARENT) { /* Inherit from grandparent */
		target_for_inheritance = rcu_dereference(parent_task->real_parent); /* RCU safe access */
	}
	if (unlikely(!target_for_inheritance))
		return 0;

	spin_lock_irqsave(&target_for_inheritance->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&target_for_inheritance->se.child_burst, now_ns)) {
		/* RCU read lock is held by sched_clone_bore, safe for __update_child_burst_direct_locked */
		__update_child_burst_direct_locked(target_for_inheritance, now_ns);
	}
	score = target_for_inheritance->se.child_burst.score;
	spin_unlock_irqrestore(&target_for_inheritance->se.child_burst.lock, irqflags);
	return score;
}

/* Assumes caller holds RCU read lock. */
static u8 inherit_burst_topological(struct task_struct *parent_task,
									u64 now_ns, u64 clone_flags)
{
	struct task_struct *ancestor_for_inheritance = parent_task;
	/* Min children for ancestor to be considered a topological root for inheritance */
	u32 child_count_threshold = (clone_flags & CLONE_PARENT) ? 1 : 0;
	unsigned long irqflags;
	u8 score = 0;
	u32 dummy_children_count = 0, dummy_penalty_sum = 0; /* For top-level call to topological update */

	if (clone_flags & CLONE_PARENT) {
		ancestor_for_inheritance = rcu_dereference(ancestor_for_inheritance->real_parent);
	}
	if (unlikely(!ancestor_for_inheritance))
		return 0;

	/* Find the "significant" ancestor for topological inheritance */
	/* Loop while ancestor is not init_task (real_parent == self) and not detached (real_parent != NULL) */
	while (ancestor_for_inheritance->real_parent != ancestor_for_inheritance &&
		ancestor_for_inheritance->real_parent != NULL) {
		if (count_children_upto2_rcu(&ancestor_for_inheritance->children) > child_count_threshold)
			break;

		ancestor_for_inheritance = rcu_dereference(ancestor_for_inheritance->real_parent);
	if (unlikely(!ancestor_for_inheritance)) {
		return 0;
	}
	child_count_threshold = 1; /* Subsequent ancestors need >1 child */
		}
		if (unlikely(!ancestor_for_inheritance))
			return 0;


	spin_lock_irqsave(&ancestor_for_inheritance->se.child_burst.lock, irqflags);
	if (burst_cache_expired(&ancestor_for_inheritance->se.child_burst, now_ns)) {
		/* Depth for recursion is fork_atavistic level - 1. Max depth is (atavistic-1).
		 * If atavistic is 2 (default), depth is 1. If 3, depth is 2. If 1, depth is 0.
		 */
		u32 recursion_depth = (sched_burst_fork_atavistic > 1) ? (sched_burst_fork_atavistic - 1) : 0;
		__update_child_burst_topological_locked(ancestor_for_inheritance, now_ns, recursion_depth,
												&dummy_children_count, &dummy_penalty_sum);
	}
	score = ancestor_for_inheritance->se.child_burst.score;
	spin_unlock_irqrestore(&ancestor_for_inheritance->se.child_burst.lock, irqflags);
	return score;
}

/* Assumes group_leader->se.group_burst.lock held. Caller must hold RCU read lock. */
static void __update_tg_burst_locked(struct task_struct *group_leader, u64 now_ns)
{
	u32 thread_count = 0, sum_penalties = 0;
	struct task_struct *thread_iter;

	/* RCU read lock held by caller allows safe traversal of thread group */
	for_each_thread(group_leader, thread_iter) {
		if (task_is_bore_eligible(thread_iter)) {
			thread_count++;
			sum_penalties += thread_iter->se.burst_penalty;
		}
	}
	update_burst_cache_locked(&group_leader->se.group_burst,
							  group_leader, /* Cache owner is the group leader */
						   thread_count, sum_penalties, now_ns);
}

/* Assumes caller holds RCU read lock. */
static u8 inherit_burst_tg(struct task_struct *parent_task, u64 now_ns)
{
	struct task_struct *group_leader = READ_ONCE(parent_task->group_leader); /* RCU safe read */
	unsigned long irqflags;
	u8 score = 0;

	if (unlikely(!group_leader))
		return 0; /* Parent task might be exiting or in an inconsistent state */

		spin_lock_irqsave(&group_leader->se.group_burst.lock, irqflags);
	if (burst_cache_expired(&group_leader->se.group_burst, now_ns)) {
		/* RCU read lock is held by sched_clone_bore for __update_tg_burst_locked */
		__update_tg_burst_locked(group_leader, now_ns);
	}
	score = group_leader->se.group_burst.score;
	spin_unlock_irqrestore(&group_leader->se.group_burst.lock, irqflags);
	return score;
}

/* Called from kernel/fork.c `copy_process` */
void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
					  u64 clone_flags, u64 now_ns)
{
	struct sched_entity *se_new = &p_new_task->se;
	u8 inherited_penalty = 0;

	init_task_burst_cache_lock(p_new_task); /* CRITICAL: Initialize spinlocks in new task's sched_entity */

	/* Clear BORE state for the new task */
	se_new->burst_time = 0;
	se_new->curr_burst_penalty = 0;
	se_new->prev_burst_penalty = 0; /* Will be set by inheritance */
	se_new->burst_penalty = 0;      /* Will be set by inheritance */
	se_new->burst_score = 0;
	/* Cache data: timestamps 0 make them immediately expired, counts/scores 0 */
	se_new->child_burst.timestamp = 0;
	se_new->child_burst.score = 0; se_new->child_burst.count = 0;
	se_new->group_burst.timestamp = 0;
	se_new->group_burst.score = 0; se_new->group_burst.count = 0;


	if (!task_is_bore_eligible(p_new_task)) { /* Not a CFS task or is exiting */
		return; /* No BORE inheritance processing for non-eligible tasks */
	}

	rcu_read_lock();
	if (clone_flags & CLONE_THREAD) { /* New thread in existing thread group */
		inherited_penalty = inherit_burst_tg(p_parent, now_ns);
	} else { /* New process (not just a thread) */
		if (sched_burst_fork_atavistic == 0) {
			inherited_penalty = 0; /* No inheritance for new processes */
		} else if (sched_burst_fork_atavistic == 1) {
			inherited_penalty = inherit_burst_direct(p_parent, now_ns, clone_flags);
		} else { /* sched_burst_fork_atavistic >= 2, use topological */
			inherited_penalty = inherit_burst_topological(p_parent, now_ns, clone_flags);
		}
	}
	rcu_read_unlock();

	/* Set initial penalty based on inheritance. prev_burst_penalty acts as the history. */
	se_new->prev_burst_penalty = inherited_penalty;
	se_new->burst_penalty      = inherited_penalty; /* Start with inherited penalty */
	/* burst_score will be updated when task is enqueued or first ticked via update_burst_score */
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

/* ================================================================== */
/*                       12.  Reset helpers                           */
/* ================================================================== */

/* Reset BORE-specific fields of a task, e.g., when changing sched class */
void reset_task_bore(struct task_struct *p)
{
	if (unlikely(!p)) return; /* Should not happen if called correctly */
		struct sched_entity *se = &p->se;

	/* Reset runtime state */
	se->burst_time = 0;
	se->prev_burst_penalty = 0;
	se->curr_burst_penalty = 0;
	se->burst_penalty = 0;
	se->burst_score = 0;

	/*
	 * Reset cache data fields only – leave spinlocks intact.
	 * Spinlocks are part of sched_entity and initialized once with the task_struct
	 * (by init_task_burst_cache_lock for init_task or fork path for others).
	 */
	se->child_burst.score     = 0;
	se->child_burst.count     = 0;
	se->child_burst.timestamp = 0; /* Mark cache as "expired" */

	se->group_burst.score     = 0;
	se->group_burst.count     = 0;
	se->group_burst.timestamp = 0; /* Mark cache as "expired" */
}
EXPORT_SYMBOL_GPL(reset_task_bore);

/* Called when sched_bore sysctl is toggled, to re-evaluate task weights globally */
static void reset_all_task_weights_for_bore_toggle(void)
{
	struct task_struct *g, *t; /* g for group_leader, t for thread */

	pr_info("Global BORE state changed, resetting all eligible task weights...\n");

	/*
	 * Iterating all tasks can be lengthy.
	 * RCU read lock protects task_rq() and task_struct fields during iteration.
	 * try_get_task_struct ensures 't' remains valid.
	 * rq_pin_lock can spin; holding it briefly is acceptable for this rare operation.
	 */
	rcu_read_lock();
	for_each_process(g) {
		for_each_thread(g, t) {
			if (!task_is_bore_eligible(t))
				continue;
			if (!tryget_task_struct(t))
				continue;

			struct rq *rq = task_rq(t); /* Get task's runqueue */
			if (!rq || !t->on_rq) {     /* Task not on a runqueue or invalid */
				put_task_struct(t);
				continue;
			}

			struct rq_flags rf;
			rq_pin_lock(rq, &rf);       /* Pin RQ, task cannot migrate */
			/* Re-check task's RQ and on_rq status after pinning */
			if (task_rq(t) == rq && t->on_rq) {
				update_rq_clock(rq); /* Ensure rq clock is current for vruntime calculations */
				/* Recalculate effective priority (may change due to sched_bore toggle) and reweight */
				reweight_task_by_prio(t, effective_prio(t));
			}
			rq_unpin_lock(rq, &rf);
			put_task_struct(t);         /* Release task reference */
		}
	}
	rcu_read_unlock();
	pr_info("Task weight reset complete.\n");
}

/* ================================================================== */
/*                          13.  Sysctl                               */
/* ================================================================== */
#ifdef CONFIG_SYSCTL
/* Static const int variables for sysctl extra1/extra2 (cannot take & of a #define literal) */
static const int bore_sysctl_zero = 0, bore_sysctl_one = 1, bore_sysctl_three = 3;
static const int bore_sysctl_nicew = NICE_WIDTH, bore_sysctl_smooth_max = 10; /* Max shift for smoothness */
static const int bore_sysctl_offset_max = 64;
static const int bore_sysctl_scale_max = BORE_ORIG_BURST_PENALTY_SCALE * 4; /* Allow up to 4x original scale */
static const int bore_sysctl_pct_max = 200; /* Percentages for P/E core scaling */
static const unsigned int bore_sysctl_stopcnt_max = 4096; /* Max for cache_stop_count */

/*
 * Using INT_MAX from <linux/limits.h> for proc_douintvec_minmax extra2,
 * as it expects int*. This should be safe as cache_lifetime is unlikely
 * to exceed INT_MAX in practice for its nanosecond scale.
 */
static const int bore_sysctl_uint_max_as_int = INT_MAX;


/* Custom handler for the main sched_bore toggle sysctl */
static int sched_bore_global_sysctl_handler(const struct ctl_table *tbl, int write,
											void __user *buf, size_t *len, loff_t *ppos)
{
	u8 old_sched_bore_state = sched_bore; /* Read current state before proc_dou8vec_minmax changes it */
	int ret_val = proc_dou8vec_minmax(tbl, write, buf, len, ppos);

	if (ret_val == 0 && write && old_sched_bore_state != sched_bore) {
		/* Value was changed by user and the change was successful */
		reset_all_task_weights_for_bore_toggle();
	}
	return ret_val;
}

static struct ctl_table bore_sysctls[] = {
	{ .procname = "sched_bore",
		.data = &sched_bore, .maxlen = sizeof(u8), .mode = 0644,
		.proc_handler = sched_bore_global_sysctl_handler, /* Uses custom handler */
		.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_one },
		{ .procname = "sched_burst_exclude_kthreads",
			.data = &sched_burst_exclude_kthreads, .maxlen = sizeof(u8), .mode = 0644,
			.proc_handler = proc_dou8vec_minmax,
			.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_one },
			{ .procname = "sched_burst_smoothness_long",
				.data = &sched_burst_smoothness_long, .maxlen = sizeof(u8), .mode = 0644,
				.proc_handler = proc_dou8vec_minmax,
				.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
				{ .procname = "sched_burst_smoothness_short",
					.data = &sched_burst_smoothness_short, .maxlen = sizeof(u8), .mode = 0644,
					.proc_handler = proc_dou8vec_minmax,
					.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
					{ .procname = "sched_burst_fork_atavistic",
						.data = &sched_burst_fork_atavistic, .maxlen = sizeof(u8), .mode = 0644,
						.proc_handler = proc_dou8vec_minmax,
						.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_three },
						{ .procname = "sched_burst_parity_threshold",
							.data = &sched_burst_parity_threshold, .maxlen = sizeof(u8), .mode = 0644,
							.proc_handler = proc_dou8vec_minmax,
							.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_nicew },
							{ .procname = "sched_burst_penalty_offset",
								.data = &sched_burst_penalty_offset, .maxlen = sizeof(u8), .mode = 0644,
								.proc_handler = proc_dou8vec_minmax,
								.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_offset_max },
								{ .procname = "sched_burst_penalty_scale",
									.data = &sched_burst_penalty_scale, .maxlen = sizeof(uint), .mode = 0644,
									.proc_handler = proc_douintvec_minmax,
									.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_scale_max },
									{ .procname = "sched_burst_cache_stop_count",
										.data = &sched_burst_cache_stop_count, .maxlen = sizeof(uint), .mode = 0644,
										.proc_handler = proc_douintvec_minmax,
										.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_stopcnt_max },
										{ .procname = "sched_burst_cache_lifetime",
											.data = &sched_burst_cache_lifetime, .maxlen = sizeof(uint), .mode = 0644,
											.proc_handler = proc_douintvec_minmax,
											.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_uint_max_as_int },
											{ .procname = "sched_deadline_boost_mask",
												.data = &sched_deadline_boost_mask, .maxlen = sizeof(uint), .mode = 0644,
												.proc_handler = proc_douintvec_minmax,
												.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_uint_max_as_int },
												/* Core-aware sysctls */
												{ .procname = "sched_burst_core_aware_penalty",
													.data = &sched_burst_core_aware_penalty, .maxlen = sizeof(u8), .mode = 0644,
													.proc_handler = proc_dou8vec_minmax, /* May need custom handler if dynamic reweight desired */
													.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_one },
													{ .procname = "sched_burst_core_aware_smoothing",
														.data = &sched_burst_core_aware_smoothing, .maxlen = sizeof(u8), .mode = 0644,
														.proc_handler = proc_dou8vec_minmax,
														.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_one },
														{ .procname = "sched_burst_penalty_pcore_scale_pct",
															.data = &sched_burst_penalty_pcore_scale_pct, .maxlen = sizeof(uint), .mode = 0644,
															.proc_handler = proc_douintvec_minmax,
															.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_pct_max },
															{ .procname = "sched_burst_penalty_ecore_scale_pct",
																.data = &sched_burst_penalty_ecore_scale_pct, .maxlen = sizeof(uint), .mode = 0644,
																.proc_handler = proc_douintvec_minmax,
																.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_pct_max },
																{ .procname = "sched_burst_smoothness_long_p",
																	.data = &sched_burst_smoothness_long_p, .maxlen = sizeof(u8), .mode = 0644,
																	.proc_handler = proc_dou8vec_minmax,
																	.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
																	{ .procname = "sched_burst_smoothness_short_p",
																		.data = &sched_burst_smoothness_short_p, .maxlen = sizeof(u8), .mode = 0644,
																		.proc_handler = proc_dou8vec_minmax,
																		.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
																		{ .procname = "sched_burst_smoothness_long_e",
																			.data = &sched_burst_smoothness_long_e, .maxlen = sizeof(u8), .mode = 0644,
																			.proc_handler = proc_dou8vec_minmax,
																			.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
																			{ .procname = "sched_burst_smoothness_short_e",
																				.data = &sched_burst_smoothness_short_e, .maxlen = sizeof(u8), .mode = 0644,
																				.proc_handler = proc_dou8vec_minmax,
																				.extra1 = (void *)&bore_sysctl_zero, .extra2 = (void *)&bore_sysctl_smooth_max },
																				{}  /* terminator */
};

static struct ctl_table_header *bore_sysctl_header_ptr; /* Store registered header */

static int __init bore_sysctl_init(void)
{
	/* register_sysctl will use the embedded NULL in the table to find the end */
	bore_sysctl_header_ptr = register_sysctl("kernel", bore_sysctls);
	if (unlikely(!bore_sysctl_header_ptr)) {
		pr_err("Failed to register sysctl parameters!\n");
		return -ENOMEM;
	}
	pr_info("Registered sysctl parameters.\n");
	return 0;
}
late_initcall(bore_sysctl_init); /* Register sysctls late in init sequence */
#endif  /* CONFIG_SYSCTL */

/* ================================================================== */
/*                          14.  Init                                 */
/* ================================================================== */
void __init sched_bore_init(void)
{
	/* SCHED_BORE_VERSION should be defined in include/linux/sched/bore.h */
	pr_info("BORE v%s initialising (HZ=%d)\n",
			SCHED_BORE_VERSION, HZ);

	bore_build_scale_tbl(); /* Build LUT before any tasks might use it */

	/*
	 * Apply general hybrid/RPL tweaks to global defaults.
	 * These can be overridden by user sysctl changes later.
	 * Core-aware specific P/E tunings are applied in bore_check_and_update_topology_features()
	 * if P/E core info is successfully found and core_aware features are enabled.
	 */
	if (is_intel_hybrid()) {
		pr_info("Intel Hybrid CPU detected, applying general hybrid default tweaks.\n");
		sched_burst_parity_threshold += 2; /* BORE_ORIG + 2 */
		sched_burst_smoothness_short =
		max_t(u8, 1, BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1); /* At least 1 */
		sched_burst_fork_atavistic   = 0; /* Simpler inheritance for hybrid by default */
		sched_burst_exclude_kthreads = 1;
	}
	/* Additional tweaks for Raptor Lake if it's also a generic hybrid */
	if (is_intel_raptor_lake()) {
		pr_info("Intel Raptor Lake CPU detected, applying additional specific tweaks.\n");
		sched_burst_parity_threshold++; /* Cumulative adjustment on top of hybrid's */
	}

	reset_task_bore(&init_task);            /* Initialize BORE state for the init_task */
	init_task_burst_cache_lock(&init_task); /* CRITICAL: Initialize spinlocks in init_task's sched_entity */

	/*
	 * bore_topology_init() is called via core_initcall to detect CPU types
	 * and register hotplug notifier. It will call bore_check_and_update_topology_features().
	 */
	pr_info("Early init done. Core-aware features status pending CPU topology detection.\n");
}

#else   /* !CONFIG_SCHED_BORE – provide stubs for callers to allow unconditional calls */

/*
 * Stubs for BORE functions when CONFIG_SCHED_BORE is not defined.
 * This allows other kernel files (e.g., patched fair.c, core.c) to call these
 * functions unconditionally without breaking the build if BORE is disabled.
 * EXPORT_SYMBOL_GPL is included for stubs to match the real functions' linkage.
 */
void __init sched_bore_init(void) { /* Empty init */ }

#ifndef _LINUX_SCHED_BORE_H_STUBS_DEFINED /* Guard for header include scenarios */
#define _LINUX_SCHED_BORE_H_STUBS_DEFINED

#define BORE_SCHED_STUB(func_name, ...) \
void func_name(__VA_ARGS__) { } \
EXPORT_SYMBOL_GPL(func_name)

BORE_SCHED_STUB(update_burst_score, struct sched_entity *se);
BORE_SCHED_STUB(update_burst_penalty, struct sched_entity *se);
BORE_SCHED_STUB(restart_burst, struct sched_entity *se);
BORE_SCHED_STUB(restart_burst_rescale_deadline, struct sched_entity *se);

/* sched_clone_bore has a different signature, handle separately */
void sched_clone_bore(struct task_struct *p_new_task, struct task_struct *p_parent,
					  u64 clone_flags, u64 now_ns) { }
					  EXPORT_SYMBOL_GPL(sched_clone_bore);

					  BORE_SCHED_STUB(reset_task_bore, struct task_struct *p);

					  #endif /* _LINUX_SCHED_BORE_H_STUBS_DEFINED */

					  #endif /* CONFIG_SCHED_BORE */
