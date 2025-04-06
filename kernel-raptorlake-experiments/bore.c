// SPDX-License-Identifier: GPL-2.0
/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler - Core Logic & Tuning
 * Copyright (C) 2021-2024 Masahito Suzuki <firelzrd@gmail.com>
 * Integration of Intel Hybrid CPU default tuning with Raptor Lake optimizations
 * and experimental core-aware runtime adaptations.
 * Relies on kernel topology information (get_topology_cpu_type) and defers
 * core-aware tuning application to late_init if needed.
 * Core-aware features enabled by default if P/E info found.
 * Uses ecore_scale_pct to implicitly adjust E-core penalty offset.
 *
 * CRITICAL: Requires corresponding BORE hooks/modifications in core kernel
 * scheduler files (fair.c, core.c, etc.) from the BORE patchset for ~6.13.
 */

#include <linux/sched.h>
#include <linux/sched/task.h>
#include <linux/sched/topology.h>
#include <linux/cpumask.h>
#include <linux/static_key.h>
#include <linux/printk.h>
#include <linux/cpu.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/gfp.h>
#include <linux/sysctl.h>
#include <linux/spinlock.h>
#include <linux/cpuset.h>
#include <linux/rcupdate.h>
#include <linux/list.h>
#include <linux/math64.h>
#include <linux/module.h>
#include <linux/slab.h>
#include <linux/percpu.h>
// #include <linux/once.h> // Removed for workaround

#include <asm/cpufeatures.h>
#include <asm/processor.h>  // Provides struct cpuinfo_x86
#include <asm/topology.h>   // Provides get_topology_cpu_type()
#include <asm/cpufeature.h> // For boot_cpu_has

#include <linux/sched/bore.h>
#include "sched.h"

#include <linux/delay.h>
#include <asm/msr.h>

#ifdef CONFIG_SCHED_BORE

#define MAX_BURST_PENALTY (156U)
#define ECORE_OFFSET_ADJUST_DIVISOR 20 // Divisor for ecore_scale_pct -> offset adjustment
#define MIN_ECORE_OFFSET_ADJUST 0     // Minimum resulting adjustment value
#define MAX_ECORE_OFFSET_ADJUST 10    // Max offset reduction derived from scale_pct
#define MIN_EFFECTIVE_OFFSET    4     // Minimum allowed effective offset

// --- Global Variables (Sysctl Tunables) ---
u8   sched_bore;
u8   sched_burst_exclude_kthreads;
u8   sched_burst_fork_atavistic;
u8   sched_burst_parity_threshold;
uint sched_burst_cache_stop_count;
uint sched_burst_cache_lifetime;
uint sched_deadline_boost_mask;
u8   sched_burst_penalty_offset; // Base offset (used for P-cores)
uint sched_burst_penalty_scale; // Base scale, potentially reduced if core-aware
u8   sched_burst_smoothness_long;
u8   sched_burst_smoothness_short;
// Core-aware tunables (enabled by default if P/E info found)
u8   sched_burst_core_aware_penalty;
u8   sched_burst_core_aware_smoothing;
uint sched_burst_penalty_pcore_scale_pct; // P-core scale multiplier %
uint sched_burst_penalty_ecore_scale_pct; // E-core scale multiplier % AND offset basis
u8   sched_burst_smoothness_long_p;
u8   sched_burst_smoothness_short_p;
u8   sched_burst_smoothness_long_e;
u8   sched_burst_smoothness_short_e;

// --- Original BORE Defaults ---
#define BORE_ORIG_SCHED_BORE                   1
#define BORE_ORIG_BURST_EXCLUDE_KTHREADS 1
#define BORE_ORIG_BURST_SMOOTHNESS_LONG  1
#define BORE_ORIG_BURST_SMOOTHNESS_SHORT 0
#define BORE_ORIG_BURST_FORK_ATAVISTIC   2 // Default conservative for hybrid
#define BORE_ORIG_BURST_PARITY_THRESHOLD 2
#define BORE_ORIG_BURST_PENALTY_OFFSET   24 // Original base offset
#define BORE_ORIG_BURST_PENALTY_SCALE    1280 // The absolute base value
#define BORE_ORIG_BURST_CACHE_STOP_COUNT 64
#define BORE_ORIG_BURST_CACHE_LIFETIME   75000000 // 75ms
#define BORE_ORIG_DEADLINE_BOOST_MASK    (ENQUEUE_INITIAL | ENQUEUE_WAKEUP)
// Initial defaults are OFF, but they get turned ON if P/E detected
#define BORE_ORIG_CORE_AWARE_PENALTY    0
#define BORE_ORIG_CORE_AWARE_SMOOTHING  0
// Percentages applied relative to sched_burst_penalty_scale
#define BORE_ORIG_PENALTY_PCORE_SCALE_PCT 100
#define BORE_ORIG_PENALTY_ECORE_SCALE_PCT 100 // Also used for offset adjustment base
#define BORE_ORIG_SMOOTHNESS_LONG_P      BORE_ORIG_BURST_SMOOTHNESS_LONG
#define BORE_ORIG_SMOOTHNESS_SHORT_P     BORE_ORIG_BURST_SMOOTHNESS_SHORT
#define BORE_ORIG_SMOOTHNESS_LONG_E      BORE_ORIG_BURST_SMOOTHNESS_LONG
#define BORE_ORIG_SMOOTHNESS_SHORT_E     BORE_ORIG_BURST_SMOOTHNESS_SHORT

// --- CPU Type Detection ---
static DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type) = TOPO_CPU_TYPE_UNKNOWN;
// Workaround: Use static flag instead of DEFINE_ONCE/do_once
static bool bore_early_detection_has_run __initdata = false;
// Flag indicating if the kernel provided valid P/E core type info (set early or late)
static bool bore_has_core_type_info = false;
// Flag indicating core-aware tuning needs to be applied later
static bool bore_apply_core_aware_tuning_late __initdata = false;

// Static key to enable core-aware features (enabled early or late if P/E info found)
static struct static_key_false bore_core_aware_key = STATIC_KEY_FALSE_INIT;

// Define is_intel_raptor_lake *before* is_intel_hybrid
static bool __init is_intel_raptor_lake(void) {
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6) return false;
	switch (boot_cpu_data.x86_model) {
		case 0xB7: // Raptor Lake-S (Desktop) / Alder Lake-N
		case 0xBA: // Raptor Lake-P (Mobile)
		case 0xBE: // Raptor Lake-H/S (?)
		case 0xBF: // Raptor Lake-S (Refresh / 14th Gen Desktop) - Covers 14700KF
		case 0xAD: // Alder Lake-M/N (?) E-cores only
		case 0xAC: // Alder Lake-S (?) E-cores only
			// Alder Lake Models
		case 0x97: // Alder Lake-S
		case 0x9A: // Alder Lake-P
			return true; // Treat ADL/RPL as needing potential late check
		default:
			return false;
	}
}

static bool __init is_intel_hybrid(void) {
	// Check CPUID feature bit OR specific known hybrid model families
	return boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake();
}


/*
 * Initial attempt to detect core types using get_topology_cpu_type().
 * Stores results and sets flags for potential later re-check.
 */
static void __init detect_all_cpu_types_early(void)
{
	unsigned int cpu;
	enum x86_topology_cpu_type type;
	int p_cores = 0, e_cores = 0, unknown_cores = 0;
	bool p_or_e_found_early = false;

	/* Ensure this runs only once */
	if (bore_early_detection_has_run)
		return;
	bore_early_detection_has_run = true;

	pr_info("BORE: Performing initial CPU core type detection using get_topology_cpu_type()...\n");

	for_each_possible_cpu(cpu) {
		struct cpuinfo_x86 *c = &per_cpu(cpu_info, cpu);
		type = get_topology_cpu_type(c);

		per_cpu(bore_cpu_type, cpu) = type;
		switch (type) {
			case TOPO_CPU_TYPE_PERFORMANCE:
				p_cores++;
				p_or_e_found_early = true;
				break;
			case TOPO_CPU_TYPE_EFFICIENCY:
				e_cores++;
				p_or_e_found_early = true;
				break;
			default:
				unknown_cores++;
				break;
		}
	}

	pr_info("BORE: Early core detection results: %d P-cores (logical), %d E-cores (logical), %d unknown\n",
			p_cores, e_cores, unknown_cores);

	if (p_or_e_found_early) {
		bore_has_core_type_info = true;
		pr_info("BORE: Early check: Kernel provided valid P/E core topology information.\n");
	} else {
		bore_has_core_type_info = false;
		pr_info("BORE: Early check: Kernel did not provide P/E core topology information.\n");
		// If it's a known hybrid platform, flag for later check
		if (is_intel_hybrid()) {
			bore_apply_core_aware_tuning_late = true;
			pr_info("BORE: Flagging hybrid system for late topology re-check.\n");
		}
	}
}

/*
 * Apply core-aware tuning settings. Should only be called if bore_has_core_type_info is true.
 * Enables core-aware features, reduces base penalty scale, and sets P/E params by default.
 */
static void __init bore_apply_core_aware_settings(void)
{
	// This function is only called if bore_has_core_type_info is true.

	pr_info("BORE: Enabling core-aware penalty/smoothing defaults and adjusting base parameters (P/E info available).\n");

	// Reduce base penalty scale for hybrid systems
	sched_burst_penalty_scale = max(1U, BORE_ORIG_BURST_PENALTY_SCALE / 2U);
	// Reduce base offset slightly for hybrid P-cores vs original monolithic default
	sched_burst_penalty_offset = max((u8)4, (u8)(BORE_ORIG_BURST_PENALTY_OFFSET * 7 / 8)); // ~87.5% of original, min 4
	pr_info("BORE: Adjusted base penalty scale=%u, base offset=%u for hybrid.\n",
			sched_burst_penalty_scale, sched_burst_penalty_offset);

	// Enable core-aware features now
	sched_burst_core_aware_penalty = 1;
	sched_burst_core_aware_smoothing = 1;

	// Set the specific P/E tuning parameters
	if (is_intel_raptor_lake()) {
		pr_info("BORE: Applying Raptor Lake P/E scaling percentages and smoothing.\n");
		// Use the specific RL values
		sched_burst_penalty_pcore_scale_pct = 100; // 100% of reduced base scale
		sched_burst_penalty_ecore_scale_pct = 120; // 120% of reduced base scale (also used for offset)
		sched_burst_smoothness_long_p = 1;
		sched_burst_smoothness_short_p = 0;
		sched_burst_smoothness_long_e = 2;
		sched_burst_smoothness_short_e = 1;
	} else {
		// Use original defaults for other hybrid (e.g., Alder Lake) percentages
		pr_info("BORE: Applying default P/E scaling percentages and smoothing for non-Raptor Lake hybrid.\n");
		sched_burst_penalty_pcore_scale_pct = BORE_ORIG_PENALTY_PCORE_SCALE_PCT; // 100% of reduced base scale
		sched_burst_penalty_ecore_scale_pct = BORE_ORIG_PENALTY_ECORE_SCALE_PCT; // 100% of reduced base scale
		sched_burst_smoothness_long_p = BORE_ORIG_SMOOTHNESS_LONG_P; // 1
		sched_burst_smoothness_short_p = BORE_ORIG_SMOOTHNESS_SHORT_P; // 0
		sched_burst_smoothness_long_e = BORE_ORIG_SMOOTHNESS_LONG_E; // 1
		sched_burst_smoothness_short_e = BORE_ORIG_SMOOTHNESS_SHORT_E; // 0
	}
	// Enable the static key now that settings are applied
	static_key_enable(&bore_core_aware_key.key);
	pr_info("BORE: Core-aware tuning key enabled.\n");
}

/*
 * Recalculate and apply P-core mask. Should only be called if bore_has_core_type_info is true.
 */
static void __init bore_recalculate_pcore_mask(void)
{
	cpumask_var_t pcore_mask;
	unsigned int cpu, calculated_mask = 0;
	bool pcore_found = false;

	if (!bore_has_core_type_info) return; // Should not happen if called correctly, but safety check

	if (!zalloc_cpumask_var(&pcore_mask, GFP_KERNEL)) {
		pr_err("BORE: Failed P-core mask alloc during late init! Using previous boost mask (0x%x).\n", sched_deadline_boost_mask);
		return;
	}

	for_each_possible_cpu(cpu) {
		if (!cpu_online(cpu)) continue;
		if (per_cpu(bore_cpu_type, cpu) == TOPO_CPU_TYPE_PERFORMANCE) {
			cpumask_set_cpu(cpu, pcore_mask);
			pcore_found = true;
		}
	}

	if (pcore_found) {
		for_each_cpu(cpu, pcore_mask) {
			if (cpu < (sizeof(sched_deadline_boost_mask) * BITS_PER_BYTE)) {
				calculated_mask |= (1U << cpu);
			} else {
				pr_warn_once("BORE: (Late) Online P-core CPU %u exceeds uint boost_mask range.\n", cpu);
			}
		}
		if (calculated_mask != 0) {
			sched_deadline_boost_mask = calculated_mask;
			pr_info("BORE: (Late) Applied P-core specific deadline boost mask (0x%x).\n", sched_deadline_boost_mask);
		} else {
			pr_warn("BORE: (Late) Calculated P-core mask empty? Using default boost mask (0x%x).\n", BORE_ORIG_DEADLINE_BOOST_MASK);
			sched_deadline_boost_mask = BORE_ORIG_DEADLINE_BOOST_MASK;
		}
	} else {
		pr_warn("BORE: (Late) No online P-cores detected for boost mask? Using default boost mask (0x%x).\n", BORE_ORIG_DEADLINE_BOOST_MASK);
		sched_deadline_boost_mask = BORE_ORIG_DEADLINE_BOOST_MASK;
	}
	free_cpumask_var(pcore_mask);
}

/*
 * Late initcall to recheck topology and apply core-aware settings if needed.
 */
static int __init bore_late_topology_check_and_apply(void)
{
	// Only proceed if the early check failed on a hybrid system
	if (!bore_apply_core_aware_tuning_late)
		return 0;

	pr_info("BORE: Performing late topology re-check...\n");

	unsigned int cpu;
	enum x86_topology_cpu_type type;
	bool p_or_e_found_late = false;

	// Re-populate per-cpu data using the (now hopefully initialized) kernel function
	for_each_possible_cpu(cpu) {
		struct cpuinfo_x86 *c = &per_cpu(cpu_info, cpu);
		type = get_topology_cpu_type(c);
		per_cpu(bore_cpu_type, cpu) = type; // Update stored type
		if (type == TOPO_CPU_TYPE_PERFORMANCE || type == TOPO_CPU_TYPE_EFFICIENCY) {
			p_or_e_found_late = true;
		}
	}

	if (p_or_e_found_late) {
		bore_has_core_type_info = true; // Update global flag
		pr_info("BORE: Late check: Kernel provided valid P/E core topology information.\n");

		// Apply the core-aware settings (incl. base scale/offset reduction) and P-core mask now
		bore_apply_core_aware_settings();
		bore_recalculate_pcore_mask();

	} else {
		// Still no info even at late_initcall? Unlikely but possible.
		bore_has_core_type_info = false;
		pr_warn("BORE: Late check: Kernel still did not provide P/E core topology info. Core-aware tuning remains disabled.\n");
	}

	return 0;
}
// Run after standard topology/SMP setup, but before scheduler really gets going
late_initcall_sync(bore_late_topology_check_and_apply);


/*
 * Initial check for Hybrid CPU and application of NON-core-aware defaults.
 */
static void __init bore_detect_intel_hybrid_early(void)
{
	if (boot_cpu_data.x86_vendor == X86_VENDOR_INTEL && is_intel_hybrid()) {
		detect_all_cpu_types_early(); // Sets flags

		// Apply non-core-aware hybrid defaults immediately
		pr_info("BORE: Intel Hybrid CPU detected. Applying initial non-core-aware defaults.\n");
		sched_burst_parity_threshold = BORE_ORIG_BURST_PARITY_THRESHOLD + 2;
		// Base scale/offset are NOT adjusted here anymore
		sched_burst_smoothness_short = max((u8)1, (u8)(BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1));
		sched_burst_fork_atavistic   = 0;
		sched_burst_exclude_kthreads = 1;

		// If core types *were* successfully found early, apply core-aware settings now
		if (bore_has_core_type_info) {
			bore_apply_core_aware_settings(); // This now reduces base scale/offset and sets P/E specifics
			bore_recalculate_pcore_mask();
		}
	}
}

/*
 * Apply Raptor Lake specific NON-core-aware tunings. Core-aware parts handled separately.
 */
static void __init bore_apply_raptor_lake_tuning_early(void)
{
	if (!is_intel_raptor_lake()) return;
	pr_info("BORE: Raptor Lake CPU detected, applying initial non-core-aware optimizations\n");

	// Apply minor general RL adjustments (always safe)
	sched_burst_parity_threshold++; // Apply cumulative adjustment if hybrid already did

	// Base penalty scale/offset are handled later in bore_apply_core_aware_settings if P/E detected
	// No adjustments here.

	// Core-aware values are set by bore_apply_core_aware_settings() called early or late
}

/* --- BORE Helper Functions --- */

// Returns the CPU type stored (potentially updated by late init)
static inline enum x86_topology_cpu_type bore_get_task_cpu_type(struct task_struct *p) {
	int cpu = task_cpu(p);
	if (cpu >= nr_cpu_ids || cpu < 0) return TOPO_CPU_TYPE_UNKNOWN;
	return per_cpu(bore_cpu_type, cpu);
}

static inline u32 log2plus1_u64_u32f8(u64 v) {
	unsigned int integral; u8 fractional;
	if (unlikely(v == 0)) return 0;
	integral = fls64(v);
	fractional = (integral > 0) ? (v << (64 - integral) >> 55) : 0;
	return (integral << 8) | fractional;
}

// Godlike calculation: incorporates implicit E-core offset adjustment
static inline u32 calc_burst_penalty(u64 burst_time) {
	u32 greed, tolerance, penalty, scaled_penalty;
	u8 core_offset = sched_burst_penalty_offset; // Start with base (P-core) offset

	// If core-aware features active, potentially adjust offset for E-cores
	if (static_key_enabled(&bore_core_aware_key.key)) {
		// Lightweight check within the function (task_cpu is usually fast)
		// Avoid calling bore_get_task_cpu_type repeatedly if possible
		int cpu = raw_smp_processor_id(); // Get current CPU - assumes task runs here
		// Alternative: int cpu = task_cpu(current); if needed, but adds overhead
		if (likely(cpu >= 0 && cpu < nr_cpu_ids)) {
			if (per_cpu(bore_cpu_type, cpu) == TOPO_CPU_TYPE_EFFICIENCY) {
				// Calculate E-core offset adjustment based on ecore scale percentage
				// Only adjust if ecore scale > 100
				if (sched_burst_penalty_ecore_scale_pct > 100) {
					u8 adj = (u8)min_t(uint, MAX_ECORE_OFFSET_ADJUST,
									   (sched_burst_penalty_ecore_scale_pct - 100) / ECORE_OFFSET_ADJUST_DIVISOR);
					adj = max((u8)MIN_ECORE_OFFSET_ADJUST, adj);
					// Apply adjustment, ensuring it doesn't go below minimum
					core_offset = max((u8)MIN_EFFECTIVE_OFFSET, (u8)max(0, (int)core_offset - adj));
				}
				// If ecore scale <= 100, E-core uses same offset as P-core (no adjustment)
			}
			// If P-core, core_offset remains sched_burst_penalty_offset
		}
		// If cpu invalid or type unknown (shouldn't happen if key enabled), use base offset
	}

	greed = log2plus1_u64_u32f8(burst_time);
	tolerance = (u32)core_offset << 8; // Use potentially adjusted offset
	penalty = max(0, (s32)(greed - tolerance));

	// Apply scaling (base scale * percentage)
	uint scale = sched_burst_penalty_scale; // Base scale (original or reduced)
	if (static_key_enabled(&bore_core_aware_key.key)) {
		int cpu = raw_smp_processor_id();
		if (likely(cpu >= 0 && cpu < nr_cpu_ids)) {
			uint scale_pct = 100; // Default multiplier
			if (per_cpu(bore_cpu_type, cpu) == TOPO_CPU_TYPE_EFFICIENCY) {
				scale_pct = sched_burst_penalty_ecore_scale_pct;
			} else if (per_cpu(bore_cpu_type, cpu) == TOPO_CPU_TYPE_PERFORMANCE) {
				scale_pct = sched_burst_penalty_pcore_scale_pct;
			}
			if (scale_pct != 100) {
				// Apply percentage multiplier to the base scale
				scale = (u32)div_u64((u64)scale * scale_pct, 100);
			}
		}
	}

	scaled_penalty = mul_u32_u32(penalty, scale) >> 16;
	return min(MAX_BURST_PENALTY, scaled_penalty);
}


static inline u64 __scale_slice(u64 delta, u8 score) {
	score = min((u8)(NICE_WIDTH - 1), score);
	return mul_u64_u32_shr(delta, sched_prio_to_wmult[score], 22);
}

static inline u64 __unscale_slice(u64 delta, u8 score) {
	score = min((u8)(NICE_WIDTH - 1), score);
	return mul_u64_u32_shr(delta, sched_prio_to_weight[score], 10);
}

static void reweight_task_by_prio(struct task_struct *p, int prio) {
	struct sched_entity *se = &p->se; unsigned long weight;
	prio = clamp(prio, 0, NICE_WIDTH - 1);
	weight = scale_load(sched_prio_to_weight[prio]);
	reweight_entity(cfs_rq_of(se), se, weight);
	se->load.inv_weight = sched_prio_to_wmult[prio];
}

static inline u8 effective_prio(struct task_struct *p) {
	int prio = NICE_TO_PRIO(p->static_prio);
	if (likely(sched_bore))
		prio = max(0, prio - (int)p->se.burst_score);
	return (u8)min_t(int, NICE_WIDTH - 1, prio);
}

// Apply core-aware smoothing only if the core-aware key is enabled
static inline u32 binary_smooth(u32 new, u32 old, enum x86_topology_cpu_type cpu_type) {
	int increment = (int)new - (int)old;
	u8 shift_long = sched_burst_smoothness_long;
	u8 shift_short = sched_burst_smoothness_short;

	// Apply core-aware smoothing only if the core-aware key is enabled
	if (sched_burst_core_aware_smoothing && static_key_enabled(&bore_core_aware_key.key)) {
		// We can trust cpu_type IF the key is enabled (means bore_has_core_type_info was true)
		if (cpu_type == TOPO_CPU_TYPE_PERFORMANCE) {
			shift_long = sched_burst_smoothness_long_p;
			shift_short = sched_burst_smoothness_short_p;
		} else if (cpu_type == TOPO_CPU_TYPE_EFFICIENCY) {
			shift_long = sched_burst_smoothness_long_e;
			shift_short = sched_burst_smoothness_short_e;
		}
	} // else use the global defaults

	if (increment >= 0) {
		shift_long = min((u8)31, shift_long);
		return old + (increment >> shift_long);
	} else {
		shift_short = min((u8)31, shift_short);
		return old - ((-increment) >> shift_short);
	}
}

static void revolve_burst_penalty(struct sched_entity *se) {
	enum x86_topology_cpu_type cpu_type = TOPO_CPU_TYPE_UNKNOWN;
	// Get current CPU type *without* locking task rq, faster but assumes task is running here
	// This is generally safe within scheduler context where revolve is called
	int cpu = raw_smp_processor_id();
	if (likely(cpu >= 0 && cpu < nr_cpu_ids))
		cpu_type = per_cpu(bore_cpu_type, cpu);

	se->prev_burst_penalty = binary_smooth(se->curr_burst_penalty, se->prev_burst_penalty, cpu_type);
	se->burst_time = 0;
	se->curr_burst_penalty = 0;
}

static inline bool task_is_bore_eligible(struct task_struct *p) {
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

/* --- Burst Cache & Inheritance Helpers --- */
#define for_each_child(p, t) list_for_each_entry_rcu(t, &(p)->children, sibling)
static u32 count_entries_upto2(struct list_head *head) {
	struct list_head *next = head->next; return (next != head) + (next->next != head);
}
static inline void init_task_burst_cache_lock(struct task_struct *p) {
	spin_lock_init(&p->se.child_burst.lock); spin_lock_init(&p->se.group_burst.lock);
}
static inline bool burst_cache_expired(struct sched_burst_cache *bc, u64 now) {
	return (s64)(bc->timestamp + sched_burst_cache_lifetime - now) < 0;
}

// Assumes bc->lock held
static void update_burst_cache(struct sched_burst_cache *bc, struct task_struct *p, u32 cnt, u32 sum, u64 now) {
	u8 avg = cnt ? (u8)(sum / cnt) : 0;
	bc->score = max(avg, p->se.burst_penalty); bc->count = cnt; bc->timestamp = now;
}

// Assumes p->se.child_burst.lock held
static void update_child_burst_direct(struct task_struct *p, u64 now) {
	u32 cnt = 0, sum = 0; struct task_struct *child;
	rcu_read_lock();
	for_each_child(p, child) {
		if (task_is_bore_eligible(child)) { cnt++; sum += child->se.burst_penalty; }
	}
	rcu_read_unlock();
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now);
}

// Simplified topological update
// Assumes p->se.child_burst.lock held
static void update_child_burst_topological(struct task_struct *p, u64 now, u32 depth, u32 *acnt, u32 *asum) {
	u32 cnt = 0, sum = 0; struct task_struct *child;
	rcu_read_lock();
	for_each_child(p, child) {
		if (task_is_bore_eligible(child)) { cnt++; sum += child->se.burst_penalty; }
	}
	rcu_read_unlock();
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now);
	*acnt += cnt; *asum += sum;
}

// Assumes caller holds RCU read lock
static inline u8 inherit_burst_direct(struct task_struct *parent_task, u64 now, u64 clone_flags) {
	struct task_struct *target_parent = parent_task; unsigned long flags; u8 score = 0;
	if (clone_flags & CLONE_PARENT) target_parent = target_parent->real_parent;
	if (unlikely(!target_parent)) return 0;

	spin_lock_irqsave(&target_parent->se.child_burst.lock, flags);
	if (burst_cache_expired(&target_parent->se.child_burst, now)) {
		update_child_burst_direct(target_parent, now); // Lock held
	}
	score = target_parent->se.child_burst.score;
	spin_unlock_irqrestore(&target_parent->se.child_burst.lock, flags);
	return score;
}

// Assumes caller holds RCU read lock
static inline u8 inherit_burst_topological(struct task_struct *parent_task, u64 now, u64 clone_flags) {
	struct task_struct *anc = parent_task; u32 cnt = 0, sum = 0; u32 base_thresh = 0; unsigned long flags; u8 score = 0;
	if (clone_flags & CLONE_PARENT) { anc = anc->real_parent; base_thresh = 1; }

	while (anc && anc != anc->real_parent && count_entries_upto2(&anc->children) <= base_thresh) {
		anc = anc->real_parent; base_thresh = 1;
	}
	if (unlikely(!anc)) return 0;

	spin_lock_irqsave(&anc->se.child_burst.lock, flags);
	if (burst_cache_expired(&anc->se.child_burst, now)) {
		update_child_burst_topological(anc, now, 0, &cnt, &sum); // Lock held, pass 0 for depth
	}
	score = anc->se.child_burst.score;
	spin_unlock_irqrestore(&anc->se.child_burst.lock, flags);
	return score;
}

// Assumes caller holds group_leader->se.group_burst.lock AND RCU read lock
static void update_tg_burst(struct task_struct *group_leader, u64 now) {
	struct task_struct *task; u32 cnt = 0, sum = 0;
	for_each_thread(group_leader, task) {
		if (task_is_bore_eligible(task)) { cnt++; sum += task->se.burst_penalty; }
	}
	update_burst_cache(&group_leader->se.group_burst, group_leader, cnt, sum, now);
}

// Assumes caller holds NO locks initially
static inline u8 inherit_burst_tg(struct task_struct *parent_task, u64 now) {
	struct task_struct *leader; unsigned long flags; u8 score = 0;
	rcu_read_lock();
	leader = READ_ONCE(parent_task->group_leader);
	if (unlikely(!leader)) {
		rcu_read_unlock();
		return 0;
	}
	spin_lock_irqsave(&leader->se.group_burst.lock, flags);
	if (burst_cache_expired(&leader->se.group_burst, now)) {
		update_tg_burst(leader, now); // RCU lock is held
	}
	score = leader->se.group_burst.score;
	spin_unlock_irqrestore(&leader->se.group_burst.lock, flags);
	rcu_read_unlock();
	return score;
}


/* --- BORE Public Functions --- */

// Assumes rq lock held
void update_burst_penalty(struct sched_entity *se) {
	u32 raw_penalty;

	// Calculation now happens entirely within calc_burst_penalty
	raw_penalty = calc_burst_penalty(se->burst_time);

	se->curr_burst_penalty = raw_penalty;
	se->burst_penalty = max(se->prev_burst_penalty, se->curr_burst_penalty);
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(update_burst_penalty);

// Assumes rq lock held
void update_burst_score(struct sched_entity *se) {
	struct task_struct *p; u8 prev_prio, new_prio, burst_score = 0;
	if (!entity_is_task(se)) return;
	p = task_of(se);
	prev_prio = effective_prio(p);
	if (!((p->flags & PF_KTHREAD) && likely(sched_burst_exclude_kthreads)))
		burst_score = se->burst_penalty >> 2;
	se->burst_score = burst_score;
	new_prio = effective_prio(p);
	if (new_prio != prev_prio)
		reweight_task_by_prio(p, new_prio);
}
EXPORT_SYMBOL_GPL(update_burst_score);

// Assumes rq lock held
inline void restart_burst(struct sched_entity *se) {
	revolve_burst_penalty(se);
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);

// Assumes rq lock held
void restart_burst_rescale_deadline(struct sched_entity *se) {
	s64 vscaled, wremain, vremain; struct task_struct *p; u8 prev_prio, new_prio;
	if (unlikely(!entity_is_task(se))) return;
	p = task_of(se);
	prev_prio = effective_prio(p);
	vremain = se->deadline - se->vruntime;
	restart_burst(se);
	new_prio = effective_prio(p);
	if (new_prio > prev_prio) {
		wremain = __unscale_slice(abs(vremain), prev_prio);
		vscaled = __scale_slice(wremain, new_prio);
		if (unlikely(vremain < 0)) vscaled = -vscaled;
		se->deadline = se->vruntime + vscaled;
	}
}
EXPORT_SYMBOL_GPL(restart_burst_rescale_deadline);

// Called during fork.
void sched_clone_bore(struct task_struct *p, struct task_struct *parent_task, u64 clone_flags, u64 now) {
	struct sched_entity *se = &p->se; u8 inherited_penalty = 0;
	if (unlikely(!p || !parent_task)) return;

	init_task_burst_cache_lock(p);
	se->child_burst.timestamp = 0; se->group_burst.timestamp = 0;
	se->child_burst.score = 0; se->group_burst.score = 0;
	se->child_burst.count = 0; se->group_burst.count = 0;
	se->burst_time = 0; se->curr_burst_penalty = 0; se->burst_score = 0;

	if (task_is_bore_eligible(p)) {
		if (clone_flags & CLONE_THREAD) {
			inherited_penalty = inherit_burst_tg(parent_task, now);
		} else {
			rcu_read_lock(); // Acquire RCU lock for direct/topological parent access
			if (sched_burst_fork_atavistic == 0) inherited_penalty = 0;
			else if (sched_burst_fork_atavistic == 1) inherited_penalty = inherit_burst_direct(parent_task, now, clone_flags);
			else inherited_penalty = inherit_burst_topological(parent_task, now, clone_flags);
			rcu_read_unlock();
		}
	}
	se->prev_burst_penalty = inherited_penalty;
	se->burst_penalty = inherited_penalty;
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

void reset_task_bore(struct task_struct *p) {
	struct sched_entity *se;
	if (unlikely(!p)) return;
	se = &p->se;
	se->burst_time = 0; se->prev_burst_penalty = 0; se->curr_burst_penalty = 0;
	se->burst_penalty = 0; se->burst_score = 0;
	se->child_burst.score = 0; se->child_burst.count = 0; se->child_burst.timestamp = 0;
	se->group_burst.score = 0; se->group_burst.count = 0; se->group_burst.timestamp = 0;
}
EXPORT_SYMBOL_GPL(reset_task_bore);

/* --- Sysctl Handler for Global Toggle --- */
static void reset_task_weights_bore(void) {
	struct task_struct *g, *p; struct rq *rq; struct rq_flags rf;
	read_lock(&tasklist_lock);
	for_each_process(g) {
		for_each_thread(g, p) {
			if (!task_is_bore_eligible(p)) continue;
			rq = task_rq(p);
			if (unlikely(!rq || !p->on_rq)) continue; // Check rq and on_rq status
			rq_pin_lock(rq, &rf);
			if (task_rq(p) != rq || !p->on_rq) { // Recheck after pinning
				rq_unpin_lock(rq, &rf);
				continue;
			}
			update_rq_clock(rq);
			reweight_task_by_prio(p, effective_prio(p));
			rq_unpin_lock(rq, &rf);
		}
	}
	read_unlock(&tasklist_lock);
}

int sched_bore_update_handler(const struct ctl_table *table, int write,
							  void *buffer, size_t *lenp, loff_t *ppos) {
	u8 old_state = sched_bore;
	int ret = proc_dointvec_minmax(table, write, buffer, lenp, ppos);
	if (ret || !write) return ret;
	if (old_state != sched_bore) {
		pr_info("BORE: Global state changed to %u. Resetting task weights...\n", sched_bore);
		reset_task_weights_bore();
		pr_info("BORE: Task weight reset complete.\n");
	}
	return 0;
							  }


							  /* --- Initialization --- */
							  void __init sched_bore_init(void) {
								  pr_info("BORE (Burst-Oriented Response Enhancer) CPU Scheduler modification %s by Masahito Suzuki\n", SCHED_BORE_VERSION);

								  // Set initial default values FIRST
								  sched_bore                   = BORE_ORIG_SCHED_BORE;
								  sched_burst_exclude_kthreads = BORE_ORIG_BURST_EXCLUDE_KTHREADS;
								  sched_burst_smoothness_long  = BORE_ORIG_BURST_SMOOTHNESS_LONG;
								  sched_burst_smoothness_short = BORE_ORIG_BURST_SMOOTHNESS_SHORT;
								  sched_burst_fork_atavistic   = BORE_ORIG_BURST_FORK_ATAVISTIC;
								  sched_burst_parity_threshold = BORE_ORIG_BURST_PARITY_THRESHOLD;
								  sched_burst_penalty_offset   = BORE_ORIG_BURST_PENALTY_OFFSET; // Original base offset
								  sched_burst_penalty_scale    = BORE_ORIG_BURST_PENALTY_SCALE;   // Original base scale
								  sched_burst_cache_stop_count = BORE_ORIG_BURST_CACHE_STOP_COUNT;
								  sched_burst_cache_lifetime   = BORE_ORIG_BURST_CACHE_LIFETIME;
								  sched_deadline_boost_mask    = BORE_ORIG_DEADLINE_BOOST_MASK;
								  // Initialize core-aware toggles from defaults (0 = disabled)
								  sched_burst_core_aware_penalty = BORE_ORIG_CORE_AWARE_PENALTY;
								  sched_burst_core_aware_smoothing = BORE_ORIG_CORE_AWARE_SMOOTHING;
								  // Initialize core-specific values from defaults (will be overridden if core-aware activated)
								  sched_burst_penalty_pcore_scale_pct = BORE_ORIG_PENALTY_PCORE_SCALE_PCT;
								  sched_burst_penalty_ecore_scale_pct = BORE_ORIG_PENALTY_ECORE_SCALE_PCT;
								  sched_burst_smoothness_long_p = BORE_ORIG_SMOOTHNESS_LONG_P;
								  sched_burst_smoothness_short_p = BORE_ORIG_SMOOTHNESS_SHORT_P;
								  sched_burst_smoothness_long_e = BORE_ORIG_SMOOTHNESS_LONG_E;
								  sched_burst_smoothness_short_e = BORE_ORIG_SMOOTHNESS_SHORT_E;

								  // Run early detection and apply *only* non-scale/offset hybrid defaults
								  bore_detect_intel_hybrid_early();

								  // Apply non-scale/offset Raptor Lake tunings if applicable
								  bore_apply_raptor_lake_tuning_early();

								  // Core-aware settings (incl. base scale/offset reduction) and P-core mask are applied
								  // either now (if detected early) or later via bore_late_topology_check_and_apply().

								  // Initialize BORE state for the init task
								  reset_task_bore(&init_task);
								  init_task_burst_cache_lock(&init_task);

								  pr_info("BORE: Initial setup complete. Active status: %u. Core-aware status pending late init.\n", sched_bore);
							  }

							  /* --- Sysctl Setup --- */
							  #ifdef CONFIG_SYSCTL
							  static const int const_int_zero = 0;
							  static const int const_int_one = 1;
							  static const int const_int_three = 3;
							  static const int const_prio_thresh_max = NICE_WIDTH;
							  static const int const_smoothness_min = 0;
							  static const int const_smoothness_max = 10;
							  static const int const_penalty_offset_max = 64;
							  static const int const_int_max = INT_MAX;
							  static const int const_penalty_scale_max = 4095;
							  static const int const_penalty_scale_pct_min = 0;
							  static const int const_penalty_scale_pct_max = 200;

							  static struct ctl_table sched_bore_sysctls[] = {
								  { .procname = "sched_bore", .data = &sched_bore, .maxlen = sizeof(u8), .mode = 0644,
									  .proc_handler = sched_bore_update_handler, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_one },
									  { .procname = "sched_burst_exclude_kthreads", .data = &sched_burst_exclude_kthreads, .maxlen = sizeof(u8), .mode = 0644,
										  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_one },
										  { .procname = "sched_burst_fork_atavistic", .data = &sched_burst_fork_atavistic, .maxlen = sizeof(u8), .mode = 0644,
											  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_three },
											  { .procname = "sched_burst_parity_threshold", .data = &sched_burst_parity_threshold, .maxlen = sizeof(u8), .mode = 0644,
												  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_prio_thresh_max },
												  { .procname = "sched_burst_penalty_offset", .data = &sched_burst_penalty_offset, .maxlen = sizeof(u8), .mode = 0644,
													  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_penalty_offset_max },
													  { .procname = "sched_burst_penalty_scale", .data = &sched_burst_penalty_scale, .maxlen = sizeof(uint), .mode = 0644,
														  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_penalty_scale_max, },
														  { .procname = "sched_burst_cache_stop_count", .data = &sched_burst_cache_stop_count, .maxlen = sizeof(uint), .mode = 0644,
															  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_max, },
															  { .procname = "sched_burst_cache_lifetime", .data = &sched_burst_cache_lifetime, .maxlen = sizeof(uint), .mode = 0644,
																  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_max, },
																  { .procname = "sched_deadline_boost_mask", .data = &sched_deadline_boost_mask, .maxlen = sizeof(uint), .mode = 0644,
																	  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_max, },
																	  { .procname = "sched_burst_smoothness_long", .data = &sched_burst_smoothness_long, .maxlen = sizeof(u8), .mode = 0644,
																		  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max },
																		  { .procname = "sched_burst_smoothness_short", .data = &sched_burst_smoothness_short, .maxlen = sizeof(u8), .mode = 0644,
																			  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max },
																			  { .procname = "sched_burst_core_aware_penalty", .data = &sched_burst_core_aware_penalty, .maxlen = sizeof(u8), .mode = 0644,
																				  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_one },
																				  { .procname = "sched_burst_core_aware_smoothing", .data = &sched_burst_core_aware_smoothing, .maxlen = sizeof(u8), .mode = 0644,
																					  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_int_zero, .extra2 = (void *)&const_int_one },
																					  { .procname = "sched_burst_penalty_pcore_scale_pct", .data = &sched_burst_penalty_pcore_scale_pct, .maxlen = sizeof(uint), .mode = 0644,
																						  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_penalty_scale_pct_min, .extra2 = (void *)&const_penalty_scale_pct_max, },
																						  { .procname = "sched_burst_penalty_ecore_scale_pct", .data = &sched_burst_penalty_ecore_scale_pct, .maxlen = sizeof(uint), .mode = 0644,
																							  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_penalty_scale_pct_min, .extra2 = (void *)&const_penalty_scale_pct_max, },
																							  { .procname = "sched_burst_smoothness_long_p", .data = &sched_burst_smoothness_long_p, .maxlen = sizeof(u8), .mode = 0644,
																								  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max, },
																								  { .procname = "sched_burst_smoothness_short_p", .data = &sched_burst_smoothness_short_p, .maxlen = sizeof(u8), .mode = 0644,
																									  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max, },
																									  { .procname = "sched_burst_smoothness_long_e", .data = &sched_burst_smoothness_long_e, .maxlen = sizeof(u8), .mode = 0644,
																										  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max, },
																										  { .procname = "sched_burst_smoothness_short_e", .data = &sched_burst_smoothness_short_e, .maxlen = sizeof(u8), .mode = 0644,
																											  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max, },
																											  { .procname = NULL, } // Explicit Terminator
							  };

							  static struct ctl_table_header *bore_sysctl_header;

							  static int __init sched_bore_sysctl_init(void) {
								  size_t table_size = ARRAY_SIZE(sched_bore_sysctls) - 1;
								  bore_sysctl_header = register_sysctl_sz("kernel", sched_bore_sysctls, table_size);
								  if (unlikely(!bore_sysctl_header)) {
									  pr_err("BORE: Failed to register sysctl parameters! (table_size=%zu)\n", table_size);
									  return -ENOMEM;
								  }
								  pr_info("BORE: Registered %zu sysctl parameters.\n", table_size);
								  return 0;
							  }
							  late_initcall(sched_bore_sysctl_init);
							  #endif // CONFIG_SYSCTL

							  #else /* CONFIG_SCHED_BORE */

							  /* --- Stubs when BORE is disabled --- */
							  void __init sched_bore_init(void) { }
							  #ifndef _LINUX_SCHED_BORE_H_STUBS_DEFINED
							  #define _LINUX_SCHED_BORE_H_STUBS_DEFINED
							  void update_burst_score(struct sched_entity *se) { }
							  void update_burst_penalty(struct sched_entity *se) { }
							  void restart_burst(struct sched_entity *se) { }
							  void restart_burst_rescale_deadline(struct sched_entity *se) { }
							  void sched_clone_bore(struct task_struct *p, struct task_struct *parent_task, u64 clone_flags, u64 now) { }
							  void reset_task_bore(struct task_struct *p) { }
							  #endif // _LINUX_SCHED_BORE_H_STUBS_DEFINED

							  #endif /* CONFIG_SCHED_BORE */
