// SPDX-License-Identifier: GPL-2.0
/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler - Core Logic & Tuning
 * Copyright (C) 2021-2024 Masahito Suzuki <firelzrd@gmail.com>
 * Integration of Intel Hybrid CPU default tuning with Raptor Lake optimizations
 * and experimental core-aware runtime adaptations.
 *
 * CRITICAL: Requires corresponding BORE hooks/modifications in core kernel
 * scheduler files (fair.c, core.c, etc.) from the BORE patchset for 6.13.8.
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
#include <asm/processor.h>
#include <asm/topology.h>

#include <linux/sched/bore.h>
#include "sched.h"

#include <linux/delay.h>
#include <asm/msr.h>

#ifdef CONFIG_SCHED_BORE

#define MAX_BURST_PENALTY (156U)

u8   sched_bore;
u8   sched_burst_exclude_kthreads;
u8   sched_burst_fork_atavistic;
u8   sched_burst_parity_threshold;
uint sched_burst_cache_stop_count;
uint sched_burst_cache_lifetime;
uint sched_deadline_boost_mask;
u8   sched_burst_penalty_offset;
uint sched_burst_penalty_scale;
u8   sched_burst_smoothness_long;
u8   sched_burst_smoothness_short;
u8   sched_burst_core_aware_penalty;
u8   sched_burst_core_aware_smoothing;
uint sched_burst_penalty_pcore_scale_pct;
uint sched_burst_penalty_ecore_scale_pct;
u8   sched_burst_smoothness_long_p;
u8   sched_burst_smoothness_short_p;
u8   sched_burst_smoothness_long_e;
u8   sched_burst_smoothness_short_e;

#define BORE_ORIG_SCHED_BORE                   1
#define BORE_ORIG_BURST_EXCLUDE_KTHREADS 1
#define BORE_ORIG_BURST_SMOOTHNESS_LONG  1
#define BORE_ORIG_BURST_SMOOTHNESS_SHORT 0
#define BORE_ORIG_BURST_FORK_ATAVISTIC   2
#define BORE_ORIG_BURST_PARITY_THRESHOLD 2
#define BORE_ORIG_BURST_PENALTY_OFFSET   24
#define BORE_ORIG_BURST_PENALTY_SCALE    1280
#define BORE_ORIG_BURST_CACHE_STOP_COUNT 64
#define BORE_ORIG_BURST_CACHE_LIFETIME   75000000 // 75ms
#define BORE_ORIG_DEADLINE_BOOST_MASK    (ENQUEUE_INITIAL | ENQUEUE_WAKEUP)
#define BORE_ORIG_CORE_AWARE_PENALTY    0
#define BORE_ORIG_CORE_AWARE_SMOOTHING  0
#define BORE_ORIG_PENALTY_PCORE_SCALE_PCT 100
#define BORE_ORIG_PENALTY_ECORE_SCALE_PCT 100
#define BORE_ORIG_SMOOTHNESS_LONG_P      BORE_ORIG_BURST_SMOOTHNESS_LONG
#define BORE_ORIG_SMOOTHNESS_SHORT_P     BORE_ORIG_BURST_SMOOTHNESS_SHORT
#define BORE_ORIG_SMOOTHNESS_LONG_E      BORE_ORIG_BURST_SMOOTHNESS_LONG
#define BORE_ORIG_SMOOTHNESS_SHORT_E     BORE_ORIG_BURST_SMOOTHNESS_SHORT

static DEFINE_PER_CPU(enum x86_topology_cpu_type, bore_cpu_type) = TOPO_CPU_TYPE_UNKNOWN;
// Workaround: Use static flag instead of DEFINE_ONCE/do_once
static bool bore_core_detection_has_run __initdata = false;
static bool bore_using_custom_topology = false;

#define BORE_DETECT_CACHE_SIZE      0x01
#define BORE_DETECT_CORE_ID         0x02
#define BORE_DETECT_FREQ            0x04
#define BORE_DETECT_ALL             0x07

static struct static_key_false bore_intel_hybrid_defaults_key = STATIC_KEY_FALSE_INIT;

static bool __init is_intel_raptor_lake(void) {
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6) return false;
	switch (boot_cpu_data.x86_model) {
		case 0xB7: case 0xBA: case 0xBF: return true; default: return false;
	}
}

static enum x86_topology_cpu_type detect_raptor_lake_core_type(int cpu, unsigned int methods)
{
	enum x86_topology_cpu_type kernel_type = get_topology_cpu_type(&per_cpu(cpu_info, cpu));
	if (kernel_type != TOPO_CPU_TYPE_UNKNOWN)
		return kernel_type;

	int is_pcore = 0;
	int evidence_count = 0;

	if (methods & BORE_DETECT_CACHE_SIZE) {
		unsigned int cache_size = per_cpu(cpu_info, cpu).x86_cache_size;
		if (cache_size >= 2048) {
			is_pcore++;
		} else if (cache_size <= 1280) {
			is_pcore--;
		}
		evidence_count++;
	}

	if (methods & BORE_DETECT_CORE_ID) {
		int core_id = topology_core_id(cpu);
		if (core_id < 8) {
			is_pcore++;
		} else if (core_id >= 16) {
			is_pcore--;
		}
		evidence_count++;
	}

	if (methods & BORE_DETECT_FREQ) {
		u64 freq;
		u32 eax, ebx, ecx, edx;
		if (per_cpu(cpu_info, cpu).cpuid_level >= 0x16) {
			cpuid(0x16, &eax, &ebx, &ecx, &edx);
			freq = (u64)eax * 1000000;
			if (freq >= 3000000000ULL) {
				is_pcore++;
			} else if (freq <= 2400000000ULL) {
				is_pcore--;
			}
			evidence_count++;
		}
	}

	enum x86_topology_cpu_type result;
	if (evidence_count == 0) {
		result = TOPO_CPU_TYPE_UNKNOWN;
	} else if (is_pcore > 0) {
		result = TOPO_CPU_TYPE_PERFORMANCE;
	} else if (is_pcore < 0) {
		result = TOPO_CPU_TYPE_EFFICIENCY;
	} else {
		result = (topology_core_id(cpu) % 16) < 8 ?
		TOPO_CPU_TYPE_PERFORMANCE : TOPO_CPU_TYPE_EFFICIENCY;
	}
	return result;
}

/*
 * Detect core types for all CPUs using a static flag for run-once logic.
 */
static void __init detect_all_cpu_types(void)
{
	unsigned int cpu;
	enum x86_topology_cpu_type type;
	int p_cores = 0, e_cores = 0, unknown_cores = 0;

	/* Ensure this runs only once using a simple static flag */
	if (bore_core_detection_has_run)
		return;
	bore_core_detection_has_run = true;

	pr_info("BORE: Performing enhanced CPU core type detection...\n");
	bool is_raptor = is_intel_raptor_lake();

	for_each_possible_cpu(cpu) {
		if (is_raptor) {
			type = detect_raptor_lake_core_type(cpu, BORE_DETECT_ALL);
		} else {
			// Fallback for non-Raptor Lake or if custom detection isn't needed
			type = get_topology_cpu_type(&per_cpu(cpu_info, cpu));
		}
		per_cpu(bore_cpu_type, cpu) = type;
		switch (type) {
			case TOPO_CPU_TYPE_PERFORMANCE: p_cores++; break;
			case TOPO_CPU_TYPE_EFFICIENCY: e_cores++; break;
			default: unknown_cores++; break;
		}
	}

	pr_info("BORE: Core detection results: %d P-cores, %d E-cores, %d unknown\n",
			p_cores, e_cores, unknown_cores);

	// Determine if our custom detection was successful and useful
	if (p_cores > 0 || e_cores > 0) {
		bore_using_custom_topology = true;
		pr_info("BORE: Using enhanced core type detection results.\n");
	} else {
		bore_using_custom_topology = false;
		pr_info("BORE: Enhanced detection found no P/E cores, relying on standard kernel info if available.\n");
	}
}

/*
 * Detect Intel Hybrid CPUs and enable specific tunings.
 * Prioritizes custom detection results if successful.
 */
static void __init bore_detect_intel_hybrid(void)
{
	if (boot_cpu_data.x86_vendor == X86_VENDOR_INTEL &&
		(boot_cpu_has(X86_FEATURE_HYBRID_CPU) || is_intel_raptor_lake())) {

		// Run our custom detection logic first
		detect_all_cpu_types();

	// Check if our custom detection succeeded
	if (bore_using_custom_topology) {
		pr_info("BORE: Intel Hybrid CPU detected. Applying tuned defaults based on enhanced topology detection.\n");
		static_key_enable(&bore_intel_hybrid_defaults_key.key);
	}
	// Fallback: Check if the kernel has topology info for the boot CPU (even if our detection failed)
	else if (get_topology_cpu_type(&per_cpu(cpu_info, raw_smp_processor_id())) != TOPO_CPU_TYPE_UNKNOWN) {
		pr_info("BORE: Intel Hybrid CPU detected. Applying tuned defaults based on kernel topology info.\n");
		static_key_enable(&bore_intel_hybrid_defaults_key.key);
	}
	// If neither our detection nor kernel info is available/useful
	else {
		pr_warn("BORE: Intel Hybrid CPU detected, but kernel lacks CPU type info and enhanced detection failed. Using standard defaults.\n");
	}
		}
}


static void __init bore_apply_hybrid_defaults(void) {
	cpumask_var_t pcore_mask;
	unsigned int cpu, calculated_mask = 0;
	bool pcore_found = false;

	if (!static_key_enabled(&bore_intel_hybrid_defaults_key.key)) return;
	pr_info("BORE: Applying Intel Hybrid specific default tunings...\n");

	sched_burst_parity_threshold = BORE_ORIG_BURST_PARITY_THRESHOLD + 2;
	sched_burst_penalty_scale    = max(1U, BORE_ORIG_BURST_PENALTY_SCALE / 2U);
	sched_burst_penalty_offset   = max((u8)1, (u8)(BORE_ORIG_BURST_PENALTY_OFFSET / 2U));
	sched_burst_smoothness_short = max((u8)1, (u8)(BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1));
	sched_burst_fork_atavistic   = 0;
	sched_burst_exclude_kthreads = 1;

	if (!zalloc_cpumask_var(&pcore_mask, GFP_KERNEL)) {
		pr_err("BORE: Failed P-core mask alloc! Using default boost mask (0x%x).\n", sched_deadline_boost_mask);
		goto mask_done;
	}

	// Use our enhanced detection results if they were successful
	if (bore_using_custom_topology) {
		for_each_possible_cpu(cpu) {
			if (!cpu_online(cpu)) continue;
			if (per_cpu(bore_cpu_type, cpu) == TOPO_CPU_TYPE_PERFORMANCE) {
				cpumask_set_cpu(cpu, pcore_mask);
				pcore_found = true;
			}
		}
	}
	// Fallback to kernel topology if our detection wasn't used/successful
	else {
		for_each_possible_cpu(cpu) {
			if (!cpu_online(cpu)) continue;
			if (get_topology_cpu_type(&per_cpu(cpu_info, cpu)) == TOPO_CPU_TYPE_PERFORMANCE) {
				cpumask_set_cpu(cpu, pcore_mask);
				pcore_found = true;
			}
		}
	}

	if (pcore_found) {
		for_each_cpu(cpu, pcore_mask) {
			if (cpu < (sizeof(sched_deadline_boost_mask) * BITS_PER_BYTE)) {
				calculated_mask |= (1U << cpu);
			} else {
				pr_warn_once("BORE: Online P-core CPU %u exceeds uint boost_mask range.\n", cpu);
			}
		}
		if (calculated_mask != 0) {
			sched_deadline_boost_mask = calculated_mask;
			pr_info("BORE: Applied P-core specific deadline boost mask (0x%x).\n", sched_deadline_boost_mask);
		} else {
			pr_warn("BORE: Calculated P-core mask empty (range limit?). Using default boost mask (0x%x).\n", sched_deadline_boost_mask);
		}
	} else {
		pr_warn("BORE: No online P-cores detected for boost mask. Using default boost mask (0x%x).\n", sched_deadline_boost_mask);
	}
	free_cpumask_var(pcore_mask);
	mask_done:;
}

static void __init bore_apply_raptor_lake_tuning(void) {
	if (!is_intel_raptor_lake()) return;
	pr_info("BORE: Raptor Lake CPU detected, applying specific conservative optimizations\n");
	sched_burst_parity_threshold++;
	sched_burst_penalty_scale = max(1U, (sched_burst_penalty_scale * 9) / 10);

	// Only apply core-aware tuning if our custom detection actually worked
	if (bore_using_custom_topology) {
		sched_burst_core_aware_penalty = 1;
		sched_burst_core_aware_smoothing = 1;
		sched_burst_penalty_pcore_scale_pct = 95;
		sched_burst_penalty_ecore_scale_pct = 120;
		sched_burst_smoothness_long_p = 1;
		sched_burst_smoothness_short_p = 0;
		sched_burst_smoothness_long_e = 2;
		sched_burst_smoothness_short_e = 1;
		pr_info("BORE: Applied Raptor Lake core-aware optimizations\n");
	} else {
		pr_info("BORE: Skipping Raptor Lake core-aware optimizations as enhanced detection was not used/successful.\n");
	}
}

static inline enum x86_topology_cpu_type bore_get_task_cpu_type(struct task_struct *p) {
	int cpu = task_cpu(p);
	if (cpu >= nr_cpu_ids || cpu < 0) return TOPO_CPU_TYPE_UNKNOWN;
	// Prioritize our detected type if available
	if (bore_using_custom_topology)
		return per_cpu(bore_cpu_type, cpu);
	// Fallback to kernel's topology info
	return get_topology_cpu_type(&per_cpu(cpu_info, cpu));
}

static inline u32 log2plus1_u64_u32f8(u64 v) {
	unsigned int integral; u8 fractional;
	if (unlikely(v == 0)) return 0;
	integral = fls64(v);
	fractional = (integral > 0) ? (v << (64 - integral) >> 55) : 0;
	return (integral << 8) | fractional;
}

static inline u32 calc_burst_penalty(u64 burst_time) {
	u32 greed, tolerance, penalty, scaled_penalty;
	greed = log2plus1_u64_u32f8(burst_time);
	tolerance = (u32)sched_burst_penalty_offset << 8;
	penalty = max(0, (s32)(greed - tolerance));
	scaled_penalty = mul_u32_u32(penalty, sched_burst_penalty_scale) >> 16;
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

static inline u32 binary_smooth(u32 new, u32 old, enum x86_topology_cpu_type cpu_type) {
	int increment = (int)new - (int)old;
	u8 shift_long = sched_burst_smoothness_long;
	u8 shift_short = sched_burst_smoothness_short;

	if (sched_burst_core_aware_smoothing && static_key_enabled(&bore_intel_hybrid_defaults_key.key) && bore_using_custom_topology) {
		if (cpu_type == TOPO_CPU_TYPE_PERFORMANCE) {
			shift_long = sched_burst_smoothness_long_p;
			shift_short = sched_burst_smoothness_short_p;
		} else if (cpu_type == TOPO_CPU_TYPE_EFFICIENCY) {
			shift_long = sched_burst_smoothness_long_e;
			shift_short = sched_burst_smoothness_short_e;
		}
	}

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
	if (entity_is_task(se)) {
		cpu_type = bore_get_task_cpu_type(task_of(se)); // Uses helper which respects custom topology
	}
	se->prev_burst_penalty = binary_smooth(se->curr_burst_penalty, se->prev_burst_penalty, cpu_type);
	se->burst_time = 0;
	se->curr_burst_penalty = 0;
}

static inline bool task_is_bore_eligible(struct task_struct *p) {
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

#define for_each_child(p, t) list_for_each_entry(t, &(p)->children, sibling)
static u32 count_entries_upto2(struct list_head *head) {
	struct list_head *next = head->next; return (next != head) + (next->next != head);
}
static inline void init_task_burst_cache_lock(struct task_struct *p) {
	spin_lock_init(&p->se.child_burst.lock); spin_lock_init(&p->se.group_burst.lock);
}
static inline bool burst_cache_expired(struct sched_burst_cache *bc, u64 now) {
	return (s64)(bc->timestamp + sched_burst_cache_lifetime - now) < 0;
}

static void update_burst_cache(struct sched_burst_cache *bc, struct task_struct *p, u32 cnt, u32 sum, u64 now) {
	u8 avg = cnt ? (u8)(sum / cnt) : 0;
	bc->score = max(avg, p->se.burst_penalty); bc->count = cnt; bc->timestamp = now;
}

static void update_child_burst_direct(struct task_struct *p, u64 now) {
	u32 cnt = 0, sum = 0; struct task_struct *child;
	rcu_read_lock(); // Needed for task iteration
	for_each_child(p, child) {
		if (task_is_bore_eligible(child)) { cnt++; sum += child->se.burst_penalty; }
	}
	rcu_read_unlock();
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now);
}

static void update_child_burst_topological(struct task_struct *p, u64 now, u32 depth, u32 *acnt, u32 *asum) {
	u32 cnt = 0, sum = 0; struct task_struct *child, *desc; u32 dcnt; unsigned long flags;
	rcu_read_lock(); // Needed for task iteration
	for_each_child(p, child) {
		desc = child;
		while ((dcnt = count_entries_upto2(&desc->children)) == 1) {
			struct task_struct *next_desc = list_first_entry_or_null(&desc->children, struct task_struct, sibling);
			if (!next_desc || next_desc == desc) break; desc = next_desc;
		}
		if (!dcnt || !depth) {
			if (task_is_bore_eligible(desc)) { cnt++; sum += desc->se.burst_penalty; }
			if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto exit_loop; continue;
		}
		spin_lock_irqsave(&desc->se.child_burst.lock, flags);
		if (!burst_cache_expired(&desc->se.child_burst, now)) {
			cnt += desc->se.child_burst.count; sum += (u32)desc->se.child_burst.score * desc->se.child_burst.count;
			spin_unlock_irqrestore(&desc->se.child_burst.lock, flags);
			if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto exit_loop; continue;
		}
		// Unlock before recursion, lock will be taken inside
		spin_unlock_irqrestore(&desc->se.child_burst.lock, flags);
		update_child_burst_topological(desc, now, depth - 1, &cnt, &sum);
		// Lock again to update this level's cache (though update_burst_cache does it)
		spin_lock_irqsave(&desc->se.child_burst.lock, flags);
		update_burst_cache(&desc->se.child_burst, desc, cnt, sum, now); // Update cache of intermediate node
		spin_unlock_irqrestore(&desc->se.child_burst.lock, flags);
		if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto exit_loop;
	}
	exit_loop:
	rcu_read_unlock();
	// Update current node's cache outside the loop
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now);
	*acnt += cnt; *asum += sum; // Update accumulated counts passed up
}

static inline u8 inherit_burst_direct(struct task_struct *p, u64 now, u64 clone_flags) {
	struct task_struct *parent = p; unsigned long flags; u8 score = 0;
	rcu_read_lock(); // Needed for parent access
	if (clone_flags & CLONE_PARENT) parent = parent->real_parent;
	if (unlikely(!parent)) { rcu_read_unlock(); return 0; }
	spin_lock_irqsave(&parent->se.child_burst.lock, flags);
	rcu_read_unlock(); // Safe to unlock after lock acquired
	if (burst_cache_expired(&parent->se.child_burst, now)) {
		update_child_burst_direct(parent, now); // Lock already held
		score = parent->se.child_burst.score;
	}
	spin_unlock_irqrestore(&parent->se.child_burst.lock, flags);
	return score;
}

static inline u8 inherit_burst_topological(struct task_struct *p, u64 now, u64 clone_flags) {
	struct task_struct *anc = p; u32 cnt = 0, sum = 0, depth = 0; u32 base_thresh = 0; unsigned long flags; u8 score = 0;
	rcu_read_lock(); // Needed for ancestry traversal
	if (clone_flags & CLONE_PARENT) { anc = anc->real_parent; base_thresh = 1; }
	while (anc && anc != anc->real_parent && count_entries_upto2(&anc->children) <= base_thresh) {
		anc = anc->real_parent; base_thresh = 1;
	}
	if (unlikely(!anc)) { rcu_read_unlock(); return 0; }
	spin_lock_irqsave(&anc->se.child_burst.lock, flags);
	rcu_read_unlock(); // Safe to unlock after lock acquired
	if (burst_cache_expired(&anc->se.child_burst, now)) {
		depth = (sched_burst_fork_atavistic > 1) ? (sched_burst_fork_atavistic - 1) : 0;
		update_child_burst_topological(anc, now, depth, &cnt, &sum); // Lock already held
	}
	score = anc->se.child_burst.score;
	spin_unlock_irqrestore(&anc->se.child_burst.lock, flags);
	return score;
}

static void update_tg_burst(struct task_struct *group_leader, u64 now) {
	struct task_struct *task; u32 cnt = 0, sum = 0;
	// RCU lock should be held by caller (inherit_burst_tg)
	for_each_thread(group_leader, task) {
		if (task_is_bore_eligible(task)) { cnt++; sum += task->se.burst_penalty; }
	}
	update_burst_cache(&group_leader->se.group_burst, group_leader, cnt, sum, now);
}

static inline u8 inherit_burst_tg(struct task_struct *p, u64 now) {
	struct task_struct *leader; unsigned long flags; u8 score = 0;
	rcu_read_lock(); // Needed for group_leader access and update_tg_burst
	leader = READ_ONCE(p->group_leader);
	if (unlikely(!leader)) { rcu_read_unlock(); return 0; }
	spin_lock_irqsave(&leader->se.group_burst.lock, flags);
	if (burst_cache_expired(&leader->se.group_burst, now)) {
		update_tg_burst(leader, now); // RCU lock is held
	}
	score = leader->se.group_burst.score;
	spin_unlock_irqrestore(&leader->se.group_burst.lock, flags);
	rcu_read_unlock();
	return score;
}


void update_burst_penalty(struct sched_entity *se) {
	u32 raw_penalty;
	enum x86_topology_cpu_type cpu_type;
	uint scale_pct = 100;

	raw_penalty = calc_burst_penalty(se->burst_time);

	// Only apply core-aware scaling if hybrid defaults are active AND custom topo was successful
	if (sched_burst_core_aware_penalty && static_key_enabled(&bore_intel_hybrid_defaults_key.key) && bore_using_custom_topology) {
		if (entity_is_task(se)) {
			cpu_type = bore_get_task_cpu_type(task_of(se)); // Uses helper which respects custom topology
			if (cpu_type == TOPO_CPU_TYPE_EFFICIENCY) {
				scale_pct = sched_burst_penalty_ecore_scale_pct;
			} else if (cpu_type == TOPO_CPU_TYPE_PERFORMANCE) {
				scale_pct = sched_burst_penalty_pcore_scale_pct;
			}
			if (scale_pct != 100) {
				raw_penalty = (u32)div_u64((u64)raw_penalty * scale_pct, 100);
				raw_penalty = min(MAX_BURST_PENALTY, raw_penalty);
			}
		}
	}

	se->curr_burst_penalty = raw_penalty;
	se->burst_penalty = max(se->prev_burst_penalty, se->curr_burst_penalty);
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(update_burst_penalty);


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


inline void restart_burst(struct sched_entity *se) {
	revolve_burst_penalty(se);
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);


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


void sched_clone_bore(struct task_struct *p, struct task_struct *parent, u64 clone_flags, u64 now) {
	struct sched_entity *se = &p->se; u8 inherited_penalty = 0;
	if (unlikely(!p || !parent)) return;
	init_task_burst_cache_lock(p);
	se->child_burst.timestamp = 0; se->group_burst.timestamp = 0;
	se->child_burst.score = 0; se->group_burst.score = 0;
	se->child_burst.count = 0; se->group_burst.count = 0;
	se->burst_time = 0; se->curr_burst_penalty = 0; se->burst_score = 0;

	if (task_is_bore_eligible(p)) {
		if (clone_flags & CLONE_THREAD) { inherited_penalty = inherit_burst_tg(p, now); } // Pass child 'p'
		else {
			if (sched_burst_fork_atavistic == 0) inherited_penalty = 0;
			else if (sched_burst_fork_atavistic == 1) inherited_penalty = inherit_burst_direct(p, now, clone_flags); // Pass child 'p'
			else inherited_penalty = inherit_burst_topological(p, now, clone_flags); // Pass child 'p'
		}
	}
	se->prev_burst_penalty = inherited_penalty;
	se->burst_penalty = inherited_penalty;
	update_burst_score(se);
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

static void reset_task_weights_bore(void) {
	struct task_struct *g, *p; struct rq *rq; struct rq_flags rf;
	read_lock(&tasklist_lock);
	for_each_process(g) {
		for_each_thread(g, p) {
			if (!task_is_bore_eligible(p)) continue;
			rq = task_rq(p);
			if (unlikely(!rq)) continue;
			rq_pin_lock(rq, &rf);
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

							  void __init sched_bore_init(void) {
								  pr_info("BORE (Burst-Oriented Response Enhancer) CPU Scheduler modification %s by Masahito Suzuki\n", SCHED_BORE_VERSION);

								  sched_bore                   = BORE_ORIG_SCHED_BORE;
								  sched_burst_exclude_kthreads = BORE_ORIG_BURST_EXCLUDE_KTHREADS;
								  sched_burst_smoothness_long  = BORE_ORIG_BURST_SMOOTHNESS_LONG;
								  sched_burst_smoothness_short = BORE_ORIG_BURST_SMOOTHNESS_SHORT;
								  sched_burst_fork_atavistic   = BORE_ORIG_BURST_FORK_ATAVISTIC;
								  sched_burst_parity_threshold = BORE_ORIG_BURST_PARITY_THRESHOLD;
								  sched_burst_penalty_offset   = BORE_ORIG_BURST_PENALTY_OFFSET;
								  sched_burst_penalty_scale    = BORE_ORIG_BURST_PENALTY_SCALE;
								  sched_burst_cache_stop_count = BORE_ORIG_BURST_CACHE_STOP_COUNT;
								  sched_burst_cache_lifetime   = BORE_ORIG_BURST_CACHE_LIFETIME;
								  sched_deadline_boost_mask    = BORE_ORIG_DEADLINE_BOOST_MASK;
								  sched_burst_core_aware_penalty = BORE_ORIG_CORE_AWARE_PENALTY;
								  sched_burst_core_aware_smoothing = BORE_ORIG_CORE_AWARE_SMOOTHING;
								  sched_burst_penalty_pcore_scale_pct = BORE_ORIG_PENALTY_PCORE_SCALE_PCT;
								  sched_burst_penalty_ecore_scale_pct = BORE_ORIG_PENALTY_ECORE_SCALE_PCT;
								  sched_burst_smoothness_long_p = BORE_ORIG_SMOOTHNESS_LONG_P;
								  sched_burst_smoothness_short_p = BORE_ORIG_SMOOTHNESS_SHORT_P;
								  sched_burst_smoothness_long_e = BORE_ORIG_SMOOTHNESS_LONG_E;
								  sched_burst_smoothness_short_e = BORE_ORIG_SMOOTHNESS_SHORT_E;

								  // Run detection logic which sets bore_using_custom_topology and enables the key if successful
								  bore_detect_intel_hybrid();
								  // Apply hybrid defaults if the key was enabled (either by custom detection or kernel fallback)
								  bore_apply_hybrid_defaults();
								  // Apply Raptor Lake tuning if applicable (will check bore_using_custom_topology internally)
								  bore_apply_raptor_lake_tuning();

								  reset_task_bore(&init_task);
								  init_task_burst_cache_lock(&init_task);

								  pr_info("BORE: Initialization complete. Active status: %u\n", sched_bore);
							  }

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

							  /* Stubs when BORE is disabled */
							  void __init sched_bore_init(void) { }
							  #ifndef _LINUX_SCHED_BORE_H_STUBS_DEFINED
							  #define _LINUX_SCHED_BORE_H_STUBS_DEFINED
							  void update_burst_score(struct sched_entity *se) { }
							  void update_burst_penalty(struct sched_entity *se) { }
							  void restart_burst(struct sched_entity *se) { }
							  void restart_burst_rescale_deadline(struct sched_entity *se) { }
							  void sched_clone_bore(struct task_struct *p, struct task_struct *parent, u64 clone_flags, u64 now) { }
							  void reset_task_bore(struct task_struct *p) { }
							  #endif // _LINUX_SCHED_BORE_H_STUBS_DEFINED

							  #endif /* CONFIG_SCHED_BORE */
