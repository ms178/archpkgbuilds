// SPDX-License-Identifier: GPL-2.0
/*
 * Burst-Oriented Response Enhancer (BORE) CPU Scheduler - Core Logic & Tuning
 * Copyright (C) 2021-2024 Masahito Suzuki <firelzrd@gmail.com>
 * Integration of Intel Hybrid CPU default tuning with Raptor Lake optimizations
 * and experimental core-aware runtime adaptations.
 *
 * Implements core BORE algorithm functions and initialization, including
 * tuned defaults for Intel Hybrid CPUs and optional runtime adaptations
 * based on P-core/E-core type (based on Kernel ~6.13.8 APIs).
 *
 * CRITICAL: Requires corresponding BORE hooks/modifications in core kernel
 * scheduler files (fair.c, core.c, etc.) from the BORE patchset for 6.13.8.
 */

#include <linux/sched.h>
#include <linux/sched/task.h>
#include <linux/sched/topology.h> // Needs definition from asm/topology.h
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
#include <linux/math64.h> // For div_u64
#include <linux/module.h>
#include <linux/slab.h>
#include <linux/percpu.h>       // Needed for per_cpu access
// #include <linux/array_size.h> // Not needed when using register_sysctl_init

#include <asm/cpufeatures.h>
#include <asm/processor.h>
#include <asm/topology.h>     // Provides get_topology_cpu_type for 6.13.8

#include <linux/sched/bore.h> // BORE public definitions
#include "sched.h" // Internal scheduler definitions

#ifdef CONFIG_SCHED_BORE

/* --- Constants --- */
#define MAX_BURST_PENALTY (156U)

/* --- Global Variables (Sysctl Tunables) --- */
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

/* --- CRITICAL: Verify and Update Original BORE Defaults --- */
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


/* --- CPU Architecture Detection & Tuning --- */
static struct static_key_false bore_intel_hybrid_defaults_key = STATIC_KEY_FALSE_INIT; // Removed __initdata

static bool __init is_intel_raptor_lake(void) { /* ... (no changes) ... */
	if (boot_cpu_data.x86_vendor != X86_VENDOR_INTEL || boot_cpu_data.x86 != 6) return false;
	switch (boot_cpu_data.x86_model) {
		case 0xB7: case 0xBA: case 0xBF: return true; default: return false;
	}
}

static void __init bore_detect_intel_hybrid(void) { /* ... (no changes) ... */
	if (boot_cpu_data.x86_vendor == X86_VENDOR_INTEL && boot_cpu_has(X86_FEATURE_HYBRID_CPU)) {
		if (get_topology_cpu_type(&per_cpu(cpu_info, raw_smp_processor_id())) != TOPO_CPU_TYPE_UNKNOWN) {
			pr_info("BORE: Intel Hybrid CPU detected. Applying tuned defaults.\n");
			static_key_enable(&bore_intel_hybrid_defaults_key.key);
		} else { pr_warn("BORE: Intel Hybrid CPU detected, but kernel lacks CPU type info. Using standard defaults.\n"); }
	}
}

static void __init bore_apply_hybrid_defaults(void) { /* ... (no changes) ... */
	cpumask_var_t pcore_mask; unsigned int cpu, calculated_mask = 0; bool pcore_found = false;
	if (!static_key_enabled(&bore_intel_hybrid_defaults_key.key)) return;
	pr_info("BORE: Applying Intel Hybrid specific default tunings...\n");

	sched_burst_parity_threshold = BORE_ORIG_BURST_PARITY_THRESHOLD + 2;
	sched_burst_penalty_scale    = max(1U, BORE_ORIG_BURST_PENALTY_SCALE / 2U);
	sched_burst_penalty_offset   = max((u8)1, (u8)(BORE_ORIG_BURST_PENALTY_OFFSET / 2U));
	sched_burst_smoothness_short = max((u8)1, (u8)(BORE_ORIG_BURST_SMOOTHNESS_SHORT + 1));
	sched_burst_fork_atavistic   = 0;
	sched_burst_exclude_kthreads = 1;

	if (!zalloc_cpumask_var(&pcore_mask, GFP_KERNEL)) {
		pr_err("BORE: Failed P-core mask alloc! Using default boost mask (0x%x).\n", sched_deadline_boost_mask); goto mask_done;
	}
	for_each_possible_cpu(cpu) {
		if (!cpu_online(cpu)) continue;
		if (get_topology_cpu_type(&per_cpu(cpu_info, cpu)) == TOPO_CPU_TYPE_PERFORMANCE) {
			cpumask_set_cpu(cpu, pcore_mask); pcore_found = true;
		}
	}
	if (pcore_found) {
		for_each_cpu(cpu, pcore_mask) {
			if (cpu < (sizeof(sched_deadline_boost_mask) * BITS_PER_BYTE)) { calculated_mask |= (1U << cpu); }
			else { pr_warn_once("BORE: Online P-core CPU %u exceeds uint boost_mask range.\n", cpu); }
		}
		if (calculated_mask != 0) {
			sched_deadline_boost_mask = calculated_mask; pr_info("BORE: Applied P-core specific deadline boost mask (0x%x).\n", sched_deadline_boost_mask);
		} else { pr_warn("BORE: Calculated P-core mask empty (range limit?). Using default boost mask (0x%x).\n", sched_deadline_boost_mask); }
	} else { pr_warn("BORE: No online P-cores detected. Using default boost mask (0x%x).\n", sched_deadline_boost_mask); }
	free_cpumask_var(pcore_mask);
	mask_done:;
}

static void __init bore_apply_raptor_lake_tuning(void) { /* ... (no changes) ... */
	if (!is_intel_raptor_lake()) return;
	pr_info("BORE: Raptor Lake CPU detected, applying specific conservative optimizations\n");
	sched_burst_parity_threshold++;
	sched_burst_penalty_scale = max(1U, (sched_burst_penalty_scale * 9) / 10);
}

/* --- BORE Helper Functions --- */

static inline enum x86_topology_cpu_type bore_get_task_cpu_type(struct task_struct *p) { /* ... (no changes) ... */
	int cpu = task_cpu(p);
	if (cpu >= nr_cpu_ids || cpu < 0) return TOPO_CPU_TYPE_UNKNOWN;
	return get_topology_cpu_type(&per_cpu(cpu_info, cpu));
}

static inline u32 log2plus1_u64_u32f8(u64 v) { /* ... (no changes) ... */
	unsigned int integral; u8 fractional;
	if (unlikely(v == 0)) return 0;
	integral = fls64(v);
	fractional = (integral > 0) ? (v << (64 - integral) >> 55) : 0;
	return (integral << 8) | fractional;
}

static inline u32 calc_burst_penalty(u64 burst_time) { /* ... (no changes) ... */
	u32 greed, tolerance, penalty, scaled_penalty;
	greed = log2plus1_u64_u32f8(burst_time);
	tolerance = (u32)sched_burst_penalty_offset << 8;
	penalty = max(0, (s32)(greed - tolerance));
	scaled_penalty = mul_u32_u32(penalty, sched_burst_penalty_scale) >> 16;
	return min(MAX_BURST_PENALTY, scaled_penalty);
}

static inline u64 __scale_slice(u64 delta, u8 score) { /* ... (no changes) ... */
	score = min((u8)(NICE_WIDTH - 1), score);
	return mul_u64_u32_shr(delta, sched_prio_to_wmult[score], 22);
}

static inline u64 __unscale_slice(u64 delta, u8 score) { /* ... (no changes) ... */
	score = min((u8)(NICE_WIDTH - 1), score);
	return mul_u64_u32_shr(delta, sched_prio_to_weight[score], 10);
}

static void reweight_task_by_prio(struct task_struct *p, int prio) { /* ... (no changes) ... */
	struct sched_entity *se = &p->se; unsigned long weight;
	prio = clamp(prio, 0, NICE_WIDTH - 1);
	weight = scale_load(sched_prio_to_weight[prio]);
	reweight_entity(cfs_rq_of(se), se, weight);
	se->load.inv_weight = sched_prio_to_wmult[prio];
}

static inline u8 effective_prio(struct task_struct *p) { /* ... (no changes) ... */
	int prio = NICE_TO_PRIO(p->static_prio);
	if (likely(sched_bore))
		prio = max(0, prio - (int)p->se.burst_score);
	return (u8)min_t(int, NICE_WIDTH - 1, prio);
}

// Modified: Accepts cpu_type for core-aware smoothing
static inline u32 binary_smooth(u32 new, u32 old, enum x86_topology_cpu_type cpu_type) {
	int increment = (int)new - (int)old;
	u8 shift_long = sched_burst_smoothness_long;
	u8 shift_short = sched_burst_smoothness_short;

	// Idea #2: Core-Type Aware Adaptive Smoothing
	if (sched_burst_core_aware_smoothing && static_key_enabled(&bore_intel_hybrid_defaults_key.key)) {
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

// Modified: Gets core type and passes to binary_smooth
static void revolve_burst_penalty(struct sched_entity *se) {
	enum x86_topology_cpu_type cpu_type = TOPO_CPU_TYPE_UNKNOWN;
	if (entity_is_task(se)) {
		cpu_type = bore_get_task_cpu_type(task_of(se));
	}
	se->prev_burst_penalty = binary_smooth(se->curr_burst_penalty, se->prev_burst_penalty, cpu_type);
	se->burst_time = 0;
	se->curr_burst_penalty = 0;
}

static inline bool task_is_bore_eligible(struct task_struct *p) { /* ... (no changes) ... */
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

/* --- Burst Cache & Inheritance Helpers --- */
#define for_each_child(p, t) list_for_each_entry(t, &(p)->children, sibling)
static u32 count_entries_upto2(struct list_head *head) { /* ... (no changes) ... */
	struct list_head *next = head->next; return (next != head) + (next->next != head);
}
static inline void init_task_burst_cache_lock(struct task_struct *p) { /* ... (no changes) ... */
	spin_lock_init(&p->se.child_burst.lock); spin_lock_init(&p->se.group_burst.lock);
}
static inline bool burst_cache_expired(struct sched_burst_cache *bc, u64 now) { /* ... (no changes) ... */
	return (s64)(bc->timestamp + sched_burst_cache_lifetime - now) < 0;
}
// Assumes bc->lock held
static void update_burst_cache(struct sched_burst_cache *bc, struct task_struct *p, u32 cnt, u32 sum, u64 now) { /* ... (no changes) ... */
	u8 avg = cnt ? (u8)(sum / cnt) : 0;
	bc->score = max(avg, p->se.burst_penalty); bc->count = cnt; bc->timestamp = now;
}
// Assumes p->se.child_burst.lock held, plus tasklist lock for iteration
static void update_child_burst_direct(struct task_struct *p, u64 now) { /* ... (no changes) ... */
	u32 cnt = 0, sum = 0; struct task_struct *child;
	for_each_child(p, child) {
		if (task_is_bore_eligible(child)) { cnt++; sum += child->se.burst_penalty; }
	}
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now);
}
// Assumes p->se.child_burst.lock held, plus tasklist lock for iteration
static void update_child_burst_topological(struct task_struct *p, u64 now, u32 depth, u32 *acnt, u32 *asum) { /* ... (lock fixes included) ... */
	u32 cnt = 0, sum = 0; struct task_struct *child, *desc; u32 dcnt; unsigned long flags;
	for_each_child(p, child) {
		desc = child;
		while ((dcnt = count_entries_upto2(&desc->children)) == 1) {
			struct task_struct *next_desc = list_first_entry_or_null(&desc->children, struct task_struct, sibling);
			if (!next_desc || next_desc == desc) break; desc = next_desc;
		}
		if (!dcnt || !depth) {
			if (task_is_bore_eligible(desc)) { cnt++; sum += desc->se.burst_penalty; }
			if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto update_and_exit; continue;
		}
		spin_lock_irqsave(&desc->se.child_burst.lock, flags);
		if (!burst_cache_expired(&desc->se.child_burst, now)) {
			cnt += desc->se.child_burst.count; sum += (u32)desc->se.child_burst.score * desc->se.child_burst.count;
			spin_unlock_irqrestore(&desc->se.child_burst.lock, flags);
			if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto update_and_exit; continue;
		}
		update_child_burst_topological(desc, now, depth - 1, &cnt, &sum);
		spin_unlock_irqrestore(&desc->se.child_burst.lock, flags);
		if (sched_burst_cache_stop_count <= (*acnt + cnt)) goto update_and_exit;
	}
	update_and_exit:
	update_burst_cache(&p->se.child_burst, p, cnt, sum, now); *acnt += cnt; *asum += sum;
}
// Assumes caller holds tasklist_lock (read)
static inline u8 inherit_burst_direct(struct task_struct *p, u64 now, u64 clone_flags) { /* ... (lock fixes included) ... */
	struct task_struct *parent = p; unsigned long flags; u8 score = 0;
	if (clone_flags & CLONE_PARENT) parent = parent->real_parent;
	if (unlikely(!parent)) return 0;
	spin_lock_irqsave(&parent->se.child_burst.lock, flags);
	if (burst_cache_expired(&parent->se.child_burst, now))
		update_child_burst_direct(parent, now);
	score = parent->se.child_burst.score;
	spin_unlock_irqrestore(&parent->se.child_burst.lock, flags);
	return score;
}
// Assumes caller holds tasklist_lock (read)
static inline u8 inherit_burst_topological(struct task_struct *p, u64 now, u64 clone_flags) { /* ... (lock fixes included) ... */
	struct task_struct *anc = p; u32 cnt = 0, sum = 0, depth = 0; u32 base_thresh = 0; unsigned long flags; u8 score = 0;
	if (clone_flags & CLONE_PARENT) { anc = anc->real_parent; base_thresh = 1; }
	while (anc != anc->real_parent && count_entries_upto2(&anc->children) <= base_thresh) {
		anc = anc->real_parent; base_thresh = 1;
	}
	if (unlikely(!anc)) return 0;
	spin_lock_irqsave(&anc->se.child_burst.lock, flags);
	if (burst_cache_expired(&anc->se.child_burst, now)) {
		depth = (sched_burst_fork_atavistic > 1) ? (sched_burst_fork_atavistic - 1) : 0;
		update_child_burst_topological(anc, now, depth, &cnt, &sum);
	}
	score = anc->se.child_burst.score;
	spin_unlock_irqrestore(&anc->se.child_burst.lock, flags);
	return score;
}
// Assumes caller holds RCU read lock and group_leader->se.group_burst.lock
static void update_tg_burst(struct task_struct *group_leader, u64 now) { /* ... (no changes) ... */
	struct task_struct *task; u32 cnt = 0, sum = 0;
	for_each_thread(group_leader, task) {
		if (task_is_bore_eligible(task)) { cnt++; sum += task->se.burst_penalty; }
	}
	update_burst_cache(&group_leader->se.group_burst, group_leader, cnt, sum, now);
}
// Assumes caller holds RCU read lock
static inline u8 inherit_burst_tg(struct task_struct *p, u64 now) { /* ... (lock fixes and NULL check included) ... */
	struct task_struct *leader; unsigned long flags; u8 score = 0;
	if (unlikely(!p)) return 0;
	leader = READ_ONCE(p->group_leader);
	if (unlikely(!leader)) return 0;
	spin_lock_irqsave(&leader->se.group_burst.lock, flags);
	if (burst_cache_expired(&leader->se.group_burst, now)) {
		update_tg_burst(leader, now);
	}
	score = leader->se.group_burst.score;
	spin_unlock_irqrestore(&leader->se.group_burst.lock, flags);
	return score;
}

/* --- BORE Public Functions --- */

// Modified: Applies core-aware penalty scaling if enabled
// Assumes rq lock held
void update_burst_penalty(struct sched_entity *se) {
	u32 raw_penalty;
	enum x86_topology_cpu_type cpu_type;
	uint scale_pct = 100;

	raw_penalty = calc_burst_penalty(se->burst_time);

	// Idea #1: Core-Type Aware Penalty Scaling
	if (sched_burst_core_aware_penalty && static_key_enabled(&bore_intel_hybrid_defaults_key.key)) {
		if (entity_is_task(se)) {
			cpu_type = bore_get_task_cpu_type(task_of(se));
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

// Assumes rq lock held
void update_burst_score(struct sched_entity *se) { /* ... (no changes) ... */
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
// Modified: Calls updated revolve_burst_penalty which uses core-aware smoothing
inline void restart_burst(struct sched_entity *se) {
	revolve_burst_penalty(se);
	se->burst_penalty = se->prev_burst_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(restart_burst);

// Assumes rq lock held
void restart_burst_rescale_deadline(struct sched_entity *se) { /* ... (no changes) ... */
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

// Assumes tasklist_lock or RCU lock held by caller
void sched_clone_bore(struct task_struct *p, struct task_struct *parent, u64 clone_flags, u64 now) { /* ... (no changes) ... */
	struct sched_entity *se = &p->se; u8 inherited_penalty = 0;
	if (unlikely(!p || !parent)) return;
	init_task_burst_cache_lock(p);
	se->child_burst.timestamp = 0; se->group_burst.timestamp = 0;
	se->child_burst.score = 0; se->group_burst.score = 0;
	se->child_burst.count = 0; se->group_burst.count = 0;
	se->burst_time = 0; se->curr_burst_penalty = 0; se->burst_score = 0;

	if (task_is_bore_eligible(p)) {
		if (clone_flags & CLONE_THREAD) { inherited_penalty = inherit_burst_tg(parent, now); }
		else {
			if (sched_burst_fork_atavistic == 0) inherited_penalty = 0;
			else if (sched_burst_fork_atavistic == 1) inherited_penalty = inherit_burst_direct(parent, now, clone_flags);
			else inherited_penalty = inherit_burst_topological(parent, now, clone_flags);
		}
	}
	se->prev_burst_penalty = inherited_penalty;
	se->burst_penalty = inherited_penalty;
	update_burst_score(se);
}
EXPORT_SYMBOL_GPL(sched_clone_bore);

void reset_task_bore(struct task_struct *p) { /* ... (no changes) ... */
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
static void reset_task_weights_bore(void) { /* ... (no changes) ... */
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
	// Use proc_dointvec_minmax consistent with table definition
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

								  /* Assign original defaults */
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
								  // Initialize new core-aware defaults
								  sched_burst_core_aware_penalty = BORE_ORIG_CORE_AWARE_PENALTY;
								  sched_burst_core_aware_smoothing = BORE_ORIG_CORE_AWARE_SMOOTHING;
								  sched_burst_penalty_pcore_scale_pct = BORE_ORIG_PENALTY_PCORE_SCALE_PCT;
								  sched_burst_penalty_ecore_scale_pct = BORE_ORIG_PENALTY_ECORE_SCALE_PCT;
								  sched_burst_smoothness_long_p = BORE_ORIG_SMOOTHNESS_LONG_P;
								  sched_burst_smoothness_short_p = BORE_ORIG_SMOOTHNESS_SHORT_P;
								  sched_burst_smoothness_long_e = BORE_ORIG_SMOOTHNESS_LONG_E;
								  sched_burst_smoothness_short_e = BORE_ORIG_SMOOTHNESS_SHORT_E;

								  /* Apply tuning layers */
								  bore_detect_intel_hybrid();
								  bore_apply_hybrid_defaults();
								  bore_apply_raptor_lake_tuning();

								  reset_task_bore(&init_task);
								  init_task_burst_cache_lock(&init_task);

								  pr_info("BORE: Initialization complete. Active status: %u\n", sched_bore);
							  }

							  /* --- Sysctl Setup --- */
							  #ifdef CONFIG_SYSCTL
							  // Define ALL limits as const int, used by proc_d*int*vec_minmax handlers
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
								  // Base BORE Controls (Use proc_dointvec_minmax for u8 data)
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

																	  // Base Smoothing Controls (use int handler)
																	  { .procname = "sched_burst_smoothness_long", .data = &sched_burst_smoothness_long, .maxlen = sizeof(u8), .mode = 0644,
																		  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max },
																		  { .procname = "sched_burst_smoothness_short", .data = &sched_burst_smoothness_short, .maxlen = sizeof(u8), .mode = 0644,
																			  .proc_handler = proc_dointvec_minmax, .extra1 = (void *)&const_smoothness_min, .extra2 = (void *)&const_smoothness_max },

																			  // Core-Aware Controls (use int handler for u8 data)
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
								  // Fixed: Use register_sysctl_sz with explicit table size
								  size_t table_size = ARRAY_SIZE(sched_bore_sysctls) - 1; // Exclude terminator

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
							  void sched_clone_bore(struct task_struct *p, struct task_struct *parent, u64 clone_flags, u64 now) { }
							  void reset_task_bore(struct task_struct *p) { }
							  #endif // _LINUX_SCHED_BORE_H_STUBS_DEFINED

							  #endif /* CONFIG_SCHED_BORE */
