// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2016 Thomas Gleixner.
 * Copyright (C) 2016-2017 Christoph Hellwig.
 * Raptor Lake optimizations (C) 2025 ms178.
 */
#include <linux/interrupt.h>
#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/cpu.h>
#include <linux/group_cpus.h>
#include <linux/cpufreq.h>
#include <linux/sched/topology.h>
#include <linux/topology.h>
#include <linux/numa.h>
#include <linux/overflow.h>
#include <linux/bitmap.h>
#ifdef CONFIG_X86
#include <linux/module.h>
#include <asm/cpu_device_id.h>
#include <asm/intel-family.h>
#include <asm/topology.h>
#include <asm/cpu.h>
#include <asm/smp.h>
#endif

#ifdef CONFIG_X86
/* Maximum number of cores to handle */
#define MAX_CORES_PER_NODE 64  /* Increased to handle future processors */

/* Module parameters */
static bool irq_pcore_affinity = true;
module_param_named(pcore_affinity, irq_pcore_affinity, bool, 0644);
MODULE_PARM_DESC(pcore_affinity, "Enable P-core IRQ affinity (default: 1)");

/* Define CPU IDs if not already defined */
#ifndef INTEL_FAM6_RAPTORLAKE
#define INTEL_FAM6_RAPTORLAKE 0xB7
#endif

#ifndef INTEL_FAM6_ALDERLAKE
#define INTEL_FAM6_ALDERLAKE 0x97
#endif

#ifndef INTEL_FAM6_ALDERLAKE_L
#define INTEL_FAM6_ALDERLAKE_L 0x9A
#endif

/* Core type definition if not available */
#ifndef X86_CORE_TYPE_INTEL_CORE
#define X86_CORE_TYPE_INTEL_CORE 1
#endif

#ifndef X86_CORE_TYPE_INTEL_ATOM
#define X86_CORE_TYPE_INTEL_ATOM 0
#endif

/* Gaming-focused controls */
static bool gaming_mode = true;
module_param(gaming_mode, bool, 0644);
MODULE_PARM_DESC(gaming_mode, "Favor low variance: pack small vector sets onto few P-core SMT domains (default: true)");

static unsigned int smallvec_threshold = 16;
module_param(smallvec_threshold, uint, 0644);
MODULE_PARM_DESC(smallvec_threshold, "Vectors <= this are 'small' and tightly packed in gaming_mode (default: 16)");

static unsigned int smallvec_pack_domains = 2;
module_param(smallvec_pack_domains, uint, 0644);
MODULE_PARM_DESC(smallvec_pack_domains, "Max P-core SMT domains for small vector sets in gaming_mode (default: 2)");

static unsigned int pcore_spread_width = 3;
module_param(pcore_spread_width, uint, 0644);
MODULE_PARM_DESC(pcore_spread_width, "Max P-core SMT domains to spread across for larger vector sets (default: 3)");

/* Anchor policy for balanced-mode mitigation:
 * true  -> bias domains that include CPU0 to keep a domain hot
 * false -> penalize CPU0 domains (useful when you pin away from CPU0)
 */
static bool anchor_prefer_cpu0 = true;
module_param(anchor_prefer_cpu0, bool, 0644);
MODULE_PARM_DESC(anchor_prefer_cpu0,
				 "Bias selection toward domains containing CPU0 to keep a domain 'hot' in balanced mode (default: true)");

/* Optional default-affinity override: 0=no change (default), 1=P-cores, 2=E-cores */
static unsigned int default_affinity_mode;
module_param(default_affinity_mode, uint, 0644);
MODULE_PARM_DESC(default_affinity_mode,
				 "Default IRQ affinity override: 0=no change (default), 1=P-cores, 2=E-cores");

/* Hybrid integration policy */
static int irq_hybrid_policy = 1;
/* 0 = spillover after P budget, 1 = capacity-aware (recommended), 2 = budgeted E share */
module_param_named(hybrid_policy, irq_hybrid_policy, int, 0644);
MODULE_PARM_DESC(hybrid_policy,
				 "Hybrid IRQ E-core policy: 0=spillover-after-P-budget, 1=capacity-aware (default), 2=budgeted-E-share");

/* Vectors per P-core (SMT) domain before we consider E-cores */
static unsigned int pcore_budget_per_domain = 2;
module_param(pcore_budget_per_domain, uint, 0644);
MODULE_PARM_DESC(pcore_budget_per_domain,
				 "Vectors per P-core (SMT) domain before spilling to E-cores (default: 2)");

/* When using policy 2 (budgeted share), percentage of overflow vectors going to E-cores */
static unsigned int ecore_share_pct = 25;
module_param(ecore_share_pct, uint, 0644);
MODULE_PARM_DESC(ecore_share_pct,
				 "E-core share percentage of overflow vectors (policy=2); default: 25");

/* P-core mask management with proper locking */
static DEFINE_MUTEX(pcore_mask_lock);
static struct cpumask pcore_mask;
static atomic_t pcore_mask_initialized = ATOMIC_INIT(0);
static int numa_node_for_cpu[NR_CPUS];

/* Store L2 cache domain information */
static struct cpumask *l2_domain_masks;
static int l2_domain_count;

/* Cache to store CPU core types: -2 = uninitialized, -1 = not hybrid/unknown, 0 = E-core, 1 = P-core */
static DEFINE_SPINLOCK(core_type_lock);
static int cpu_core_type[NR_CPUS] = { [0 ... NR_CPUS-1] = -2 };

/* Frequency heuristic information */
static unsigned int max_cpu_freq;
static atomic_t freq_initialized = ATOMIC_INIT(0);

/* L2 core ID cache to avoid recalculation */
static int l2_core_ids[NR_CPUS];
static atomic_t l2_ids_initialized = ATOMIC_INIT(0);

/* CPU hotplug dynamic state id (for correct unregister) */
static int pcore_cpuhp_state = -1;

/**
 * hybrid_cpu_detected - Check if system has hybrid CPU architecture
 *
 * Detects Intel hybrid architectures like Raptor Lake and Alder Lake.
 * Result is safely cached for performance.
 *
 * Return: true if hybrid CPU detected, false otherwise
 */
static bool hybrid_cpu_detected(void)
{
	static int is_hybrid = -1; /* -1: unknown, 0: no, 1: yes */
	static DEFINE_MUTEX(hybrid_detect_lock);
	static const struct x86_cpu_id hybrid_ids[] = {
		{ .family = 6, .model = INTEL_FAM6_RAPTORLAKE,   .driver_data = 0 },
		{ .family = 6, .model = INTEL_FAM6_ALDERLAKE,    .driver_data = 0 },
		{ .family = 6, .model = INTEL_FAM6_ALDERLAKE_L,  .driver_data = 0 },
		{}
	};
	int v = is_hybrid;

	if (v != -1) {
		return v == 1;
	}

	mutex_lock(&hybrid_detect_lock);
	v = is_hybrid;
	if (v == -1) {
		v = x86_match_cpu(hybrid_ids) ? 1 : 0;
		is_hybrid = v;
	}
	mutex_unlock(&hybrid_detect_lock);

	return v == 1;
}

/**
 * init_freq_info - Initialize frequency information for heuristic detection
 *
 * Efficiently calculates and caches maximum CPU frequency for use in core type detection.
 * Only performs the calculation once for all CPUs.
 */
static void init_freq_info(void)
{
	static DEFINE_MUTEX(freq_lock);
	unsigned int freq, temp_max = 0;
	int c;

	if (atomic_read_acquire(&freq_initialized)) {
		return;
	}

	mutex_lock(&freq_lock);
	if (!atomic_read(&freq_initialized)) {
		for_each_online_cpu(c) {
			freq = cpufreq_quick_get_max(c);
			if (freq > temp_max) {
				temp_max = freq;
			}
		}

		/* Publish data before flipping the initialized flag */
		max_cpu_freq = temp_max;
		smp_wmb();
		atomic_set(&freq_initialized, 1);
	}
	mutex_unlock(&freq_lock);
}

/**
 * init_l2_core_ids - Pre-calculate L2 domain IDs once
 *
 * Pre-computes the L2 domain IDs for all CPUs to avoid expensive
 * recalculations during L2 domain detection fallback.
 */
static void init_l2_core_ids(void)
{
	int cpu;

	if (atomic_read_acquire(&l2_ids_initialized)) {
		return;
	}

	for_each_possible_cpu(cpu) {
		int pkg = topology_physical_package_id(cpu);
		int cid = topology_core_id(cpu);

		if (pkg < 0) {
			pkg = 0;
		}
		if (cid < 0) {
			cid = cpu; /* mild fallback avoids collapse */
		}

		l2_core_ids[cpu] = ((pkg & 0xFFFF) << 16) | (cid & 0xFFFF);
	}

	/* Publish array before setting initialized flag */
	smp_wmb();
	atomic_set(&l2_ids_initialized, 1);
}

/**
 * get_core_type - Optimized CPU core type detection with caching
 * @cpu: CPU number to check
 *
 * Efficiently determines whether a CPU is a P-core or E-core using three methods
 * in order of reliability, with results cached for maximum performance.
 *
 * Return: 1 for P-core, 0 for E-core, -1 if unknown/not hybrid
 */
static int get_core_type(int cpu)
{
	int core_type;
	unsigned long flags;

	if (!cpu_possible(cpu)) {
		return -1;
	}

	core_type = cpu_core_type[cpu];
	if (core_type != -2) {
		return core_type;
	}

	if (!hybrid_cpu_detected()) {
		spin_lock_irqsave(&core_type_lock, flags);
		if (cpu_core_type[cpu] == -2) {
			cpu_core_type[cpu] = -1;
		}
		core_type = cpu_core_type[cpu];
		spin_unlock_irqrestore(&core_type_lock, flags);
		return core_type;
	}

#ifdef CONFIG_INTEL_HYBRID_CPU
	{
		u8 type = cpu_data(cpu).x86_core_type;

		/* P-core: support both macro spellings across kernels */
		if (
#ifdef X86_CORE_TYPE_INTEL_CORE
		    type == X86_CORE_TYPE_INTEL_CORE ||
#endif
#ifdef X86_CORE_TYPE_CORE
		    type == X86_CORE_TYPE_CORE ||
#endif
		    false) {
			spin_lock_irqsave(&core_type_lock, flags);
			if (cpu_core_type[cpu] == -2) {
				cpu_core_type[cpu] = 1;
			}
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 1;
		}

		/* E-core: support both macro spellings across kernels */
		if (
#ifdef X86_CORE_TYPE_INTEL_ATOM
		    type == X86_CORE_TYPE_INTEL_ATOM ||
#endif
#ifdef X86_CORE_TYPE_ATOM
		    type == X86_CORE_TYPE_ATOM ||
#endif
		    false) {
			spin_lock_irqsave(&core_type_lock, flags);
			if (cpu_core_type[cpu] == -2) {
				cpu_core_type[cpu] = 0;
			}
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 0;
		}
	}
#endif /* CONFIG_INTEL_HYBRID_CPU */

	/* Heuristic 1: SMT sibling count */
	{
		const struct cpumask *thread_siblings = topology_sibling_cpumask(cpu);

		if (thread_siblings && cpumask_weight(thread_siblings) > 1) {
			spin_lock_irqsave(&core_type_lock, flags);
			if (cpu_core_type[cpu] == -2) {
				cpu_core_type[cpu] = 1;
			}
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 1;
		}
	}

	/* Heuristic 2: frequency-based last resort */
	if (!atomic_read_acquire(&freq_initialized)) {
		init_freq_info();
	}

	if (max_cpu_freq > 0) {
		unsigned int cpu_freq = cpufreq_quick_get_max(cpu);
		unsigned int maxf = max_cpu_freq;
		unsigned int thr_p = (maxf / 100U) * 95U;
		unsigned int thr_e = (maxf / 100U) * 70U;

		if (cpu_freq >= thr_p) {
			spin_lock_irqsave(&core_type_lock, flags);
			if (cpu_core_type[cpu] == -2) {
				cpu_core_type[cpu] = 1;
			}
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 1;
		} else if (cpu_freq > 0 && cpu_freq <= thr_e) {
			spin_lock_irqsave(&core_type_lock, flags);
			if (cpu_core_type[cpu] == -2) {
				cpu_core_type[cpu] = 0;
			}
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 0;
		}
	}

	/* Unknown */
	spin_lock_irqsave(&core_type_lock, flags);
	if (cpu_core_type[cpu] == -2) {
		cpu_core_type[cpu] = -1;
	}
	core_type = cpu_core_type[cpu];
	spin_unlock_irqrestore(&core_type_lock, flags);

	return core_type;
}

/**
 * free_l2_domain_masks - Free L2 domain mask resources
 *
 * Helper function to safely clean up L2 domain resources.
 * Can be called from any context including error paths.
 */
static void free_l2_domain_masks(void)
{
	mutex_lock(&pcore_mask_lock);
	if (l2_domain_masks) {
		kfree(l2_domain_masks);
		l2_domain_masks = NULL;
		l2_domain_count = 0;
	}
	mutex_unlock(&pcore_mask_lock);
}

/**
 * get_pcore_mask - Fill provided mask with performance cores
 * @dst: Destination cpumask to fill with P-cores
 *
 * Thread-safe function to identify performance cores on hybrid CPUs.
 * Caller must provide the destination buffer.
 *
 * Return: 0 on success, negative error code on failure
 */
static int get_pcore_mask(struct cpumask *dst)
{
	if (!dst) {
		return -EINVAL;
	}

	if (!atomic_read_acquire(&pcore_mask_initialized)) {
		mutex_lock(&pcore_mask_lock);
		if (!atomic_read(&pcore_mask_initialized)) {
			int cpu;
			bool direct_detection = false;

			cpumask_clear(&pcore_mask);

			/* Direct core type detection (most reliable) */
			for_each_possible_cpu(cpu) {
				int core_type = get_core_type(cpu);

				if (core_type == 1) {
					cpumask_set_cpu(cpu, &pcore_mask);
				}
				if (cpu < NR_CPUS) {
					numa_node_for_cpu[cpu] = cpu_to_node(cpu);
				}
			}
			direct_detection = !cpumask_empty(&pcore_mask);

			/* SMT sibling union */
			if (!direct_detection) {
				for_each_online_cpu(cpu) {
					const struct cpumask *sib = topology_sibling_cpumask(cpu);

					if (sib && cpumask_weight(sib) > 1) {
						cpumask_or(&pcore_mask, &pcore_mask, sib);
					}
				}
			}

			/* Frequency-based fallback */
			if (cpumask_empty(&pcore_mask)) {
				unsigned int max_freq = 0;
				int max_freq_cpu = -1;

				for_each_online_cpu(cpu) {
					unsigned int f = cpufreq_quick_get_max(cpu);

					if (f > max_freq && f > 0) {
						max_freq = f;
						max_freq_cpu = cpu;
					}
				}

				if (max_freq_cpu >= 0 && max_freq > 0) {
					unsigned int threshold = (max_freq / 100U) * 95U;

					for_each_online_cpu(cpu) {
						unsigned int f = cpufreq_quick_get_max(cpu);

						if (f >= threshold && f > 0) {
							cpumask_set_cpu(cpu, &pcore_mask);
						}
					}
				}
			}

			/* Fallback: all online CPUs */
			if (cpumask_empty(&pcore_mask)) {
				cpumask_copy(&pcore_mask, cpu_online_mask);
			}

			/* Publish mask before setting initialized flag */
			smp_wmb();
			atomic_set(&pcore_mask_initialized, 1);
		}
		mutex_unlock(&pcore_mask_lock);
	}

	mutex_lock(&pcore_mask_lock);
	cpumask_copy(dst, &pcore_mask);
	mutex_unlock(&pcore_mask_lock);

	return 0;
}

/**
 * identify_l2_domains - Build per-core (SMT) domains for P-cores
 * @p_core_mask: Mask of P-cores to analyze (required, non-empty)
 *
 * Builds unique per-core domains using the SMT sibling mask intersected with
 * the P-core mask. This prevents collapsing all P-cores into a single LLC
 * domain and provides stable, cache-friendly grouping.
 *
 * Return: 0 on success, negative error code on failure
 *
 * Caller should hold cpus_read_lock for a stable topology view.
 */
static int identify_l2_domains(struct cpumask *p_core_mask)
{
    int cpu, j;
    int max_domains;
    cpumask_var_t dom;

    if (!p_core_mask || cpumask_empty(p_core_mask)) {
        pr_warn("identify_l2_domains: Empty P-core mask provided\n");
        return -EINVAL;
    }

    max_domains = cpumask_weight(p_core_mask);
    if (max_domains <= 0)
        return -ENODATA;

    if (!zalloc_cpumask_var(&dom, GFP_KERNEL))
        return -ENOMEM;

    mutex_lock(&pcore_mask_lock);

    if (l2_domain_masks) {
        kfree(l2_domain_masks);
        l2_domain_masks = NULL;
        l2_domain_count = 0;
    }

    l2_domain_masks = kcalloc(max_domains, sizeof(struct cpumask), GFP_KERNEL);
    if (!l2_domain_masks) {
        mutex_unlock(&pcore_mask_lock);
        free_cpumask_var(dom);
        return -ENOMEM;
    }

    l2_domain_count = 0;

    for_each_cpu(cpu, p_core_mask) {
        const struct cpumask *sib = topology_sibling_cpumask(cpu);
        bool exists = false;

        if (sib && !cpumask_empty(sib)) {
            cpumask_and(dom, sib, p_core_mask);
        } else {
            cpumask_clear(dom);
            cpumask_set_cpu(cpu, dom);
        }

        if (cpumask_empty(dom))
            continue;

        for (j = 0; j < l2_domain_count; j++) {
            if (cpumask_equal(&l2_domain_masks[j], dom)) {
                exists = true;
                break;
            }
        }
        if (exists)
            continue;

        if (l2_domain_count < max_domains)
            cpumask_copy(&l2_domain_masks[l2_domain_count++], dom);
        else
            break;
    }

    mutex_unlock(&pcore_mask_lock);
    free_cpumask_var(dom);
    return l2_domain_count > 0 ? 0 : -ENODATA;
}

/**
 * group_cpus_hybrid_first - Hybrid IRQ distribution optimized for balanced and gaming
 * @num_grps: Number of groups to create (>0)
 *
 * Strategy:
 * - For small vector counts: pack onto a minimal number of high-capacity P-core
 *   SMT domains (keep a domain hot to reduce wake/ramp latency in balanced mode).
 * - For larger vector counts: limit spread across P-core domains to a configurable
 *   width, reusing domains before expanding (leave other P-cores clean).
 * - E-cores: used only after a configurable P budget is exhausted (spillover).
 *
 * Safety:
 * - Filters P-core mask to online CPUs.
 * - Snapshots P SMT domains under lock; operates on a private copy (no UAF).
 * - Robust fallbacks to group_cpus_evenly() if anything becomes inconsistent.
 * - No internal default-affinity fill; we either generate a valid plan or fall back.
 *
 * Caller should hold cpus_read_lock for a stable topology view.
 */
static struct cpumask *group_cpus_hybrid_first(unsigned int num_grps)
{
	cpumask_var_t p_core_copy;
	cpumask_var_t e_cores_mask;
	unsigned long *assigned = NULL;
	struct cpumask *result = NULL;
	struct cpumask *l2_local_masks = NULL;
	int l2_local_count = 0;
	int i, j, cpu, grp_idx = 0;
	int ret;

	if (!num_grps)
		return NULL;

	/* If hybrid-aware path disabled or not hybrid, use vanilla */
	if (!irq_pcore_affinity || !hybrid_cpu_detected())
		return group_cpus_evenly(num_grps);

	if (!zalloc_cpumask_var(&p_core_copy, GFP_KERNEL))
		return group_cpus_evenly(num_grps);
	if (!zalloc_cpumask_var(&e_cores_mask, GFP_KERNEL)) {
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}
	assigned = bitmap_zalloc(nr_cpu_ids, GFP_KERNEL);
	if (!assigned) {
		free_cpumask_var(e_cores_mask);
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}

	/* Compute P-core set; bail to even if empty/failed */
	ret = get_pcore_mask(p_core_copy);
	if (ret || cpumask_empty(p_core_copy)) {
		bitmap_free(assigned);
		free_cpumask_var(e_cores_mask);
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}

	/* Only operate on online CPUs */
	cpumask_and(p_core_copy, p_core_copy, cpu_online_mask);

	result = kcalloc(num_grps, sizeof(struct cpumask), GFP_KERNEL);
	if (!result) {
		bitmap_free(assigned);
		free_cpumask_var(e_cores_mask);
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}
	for (i = 0; i < (int)num_grps; i++)
		cpumask_clear(&result[i]);

	/* E-cores = online - P-cores */
	cpumask_andnot(e_cores_mask, cpu_online_mask, p_core_copy);

	/* Build per-core SMT domains for P-cores and snapshot them */
	ret = identify_l2_domains(p_core_copy);
	if (ret == 0) {
		mutex_lock(&pcore_mask_lock);
		if (l2_domain_count > 0 && l2_domain_masks) {
			l2_local_count = l2_domain_count;
			l2_local_masks = kcalloc(l2_local_count, sizeof(struct cpumask), GFP_KERNEL);
			if (l2_local_masks) {
				for (i = 0; i < l2_local_count; i++)
					cpumask_copy(&l2_local_masks[i], &l2_domain_masks[i]);
			}
		}
		mutex_unlock(&pcore_mask_lock);

		if (!l2_local_masks || l2_local_count == 0)
			ret = -ENOMEM;
	}

	/* Determine domain limit (packing vs limited spread) and place groups */
	{
		unsigned int p_grps = 0, e_grps = 0;
		unsigned int limit;

		if (ret) {
			/* No domain data; fallback to vanilla */
			goto fallback_evenly;
		}

		if (num_grps <= smallvec_threshold)
			limit = smallvec_pack_domains;
		else
			limit = pcore_spread_width;

		if (limit == 0 || limit > (unsigned int)l2_local_count)
			limit = (unsigned int)l2_local_count;

		/* P budget across selected domains */
		{
			unsigned int budget = pcore_budget_per_domain ? (pcore_budget_per_domain * limit) : limit;
			if (budget < 1)
				budget = limit;
			p_grps = min_t(unsigned int, num_grps, budget);
			e_grps = num_grps - p_grps;
		}

		/* Score domains: prefer higher capacity; optional CPU0 bias */
		struct {
			int idx;
			s64 score;
		} *ds = NULL;

		ds = kcalloc(l2_local_count, sizeof(*ds), GFP_KERNEL);
		if (!ds)
			goto fallback_evenly;

		for (i = 0; i < l2_local_count; i++) {
			u64 cap = 0;
			for_each_cpu(cpu, &l2_local_masks[i])
				cap += (u64)arch_scale_cpu_capacity(cpu);

			if (cpumask_test_cpu(0, &l2_local_masks[i]))
				ds[i].score = anchor_prefer_cpu0 ? (s64)cap + 1000000000LL : (s64)cap - 1000000000LL;
			else
				ds[i].score = (s64)cap;
			ds[i].idx = i;
		}

		/* Partial selection: top 'limit' domains */
		for (i = 0; i < (int)limit; i++) {
			int maxk = i;
			for (j = i + 1; j < l2_local_count; j++)
				if (ds[j].score > ds[maxk].score)
					maxk = j;
			if (maxk != i) {
				typeof(*ds) tmp = ds[i];
				ds[i] = ds[maxk];
				ds[maxk] = tmp;
			}
		}

		/* Place P-groups round-robin across selected top domains */
		{
			unsigned int placed = 0;
			for (i = 0; i < (int)p_grps && grp_idx < (int)num_grps; i++) {
				int dom_idx = ds[i % (int)limit].idx;
				bool placed_one = false;

				for_each_cpu(cpu, &l2_local_masks[dom_idx]) {
					if (!test_and_set_bit(cpu, assigned)) {
						cpumask_set_cpu(cpu, &result[grp_idx++]);
						placed++;
						placed_one = true;
						break;
					}
				}
				/* If domain is full, fall back to any free P-core */
				if (!placed_one) {
					for_each_cpu(cpu, p_core_copy) {
						if (!test_and_set_bit(cpu, assigned)) {
							cpumask_set_cpu(cpu, &result[grp_idx++]);
							placed++;
							break;
						}
					}
				}
			}
			if (placed < p_grps) {
				kfree(ds);
				goto fallback_evenly;
			}
		}

		kfree(ds);

		/* Evenly place spillover groups to E-cores, clamped by E-core count */
		if (e_grps > 0 && !cpumask_empty(e_cores_mask)) {
			int e_count = cpumask_weight(e_cores_mask);
			int groups_to_place = (int)e_grps;

			if (groups_to_place > e_count)
				groups_to_place = e_count;

			if (groups_to_place > 0) {
				int per_group = e_count / groups_to_place;
				int extra = e_count % groups_to_place;
				int g = 0;

				for_each_cpu(cpu, e_cores_mask) {
					if (grp_idx + g >= (int)num_grps)
						break;
					cpumask_set_cpu(cpu, &result[grp_idx + g]);
					if (cpumask_weight(&result[grp_idx + g]) >= (per_group + (g < extra ? 1 : 0)))
						g++;
					if (g >= groups_to_place)
						break;
				}
				grp_idx += groups_to_place;
			}
		}
	}

	/* Success path */
	kfree(l2_local_masks);
	bitmap_free(assigned);
	free_cpumask_var(e_cores_mask);
	free_cpumask_var(p_core_copy);
	return result;

fallback_evenly:
	kfree(l2_local_masks);
	kfree(result);
	bitmap_free(assigned);
	free_cpumask_var(e_cores_mask);
	free_cpumask_var(p_core_copy);
	return group_cpus_evenly(num_grps);
}

/**
 * pcore_cpu_notify - Optimized CPU hotplug notification handler
 * @cpu: CPU number that changed state
 *
 * Efficiently handles CPU hotplug events with minimal blocking.
 * Uses trylock where appropriate to avoid stalling critical paths.
 *
 * Return: 0 on success, negative error code on failure
 */
static int pcore_cpu_notify(unsigned int cpu)
{
	if (unlikely(system_state != SYSTEM_RUNNING)) {
		return 0;
	}

	if (cpu >= NR_CPUS) {
		pr_warn("pcore_cpu_notify: cpu %u out of range\n", cpu);
		return -EINVAL;
	}

	numa_node_for_cpu[cpu] = cpu_to_node(cpu);

	atomic_set(&pcore_mask_initialized, 0);
	atomic_set(&freq_initialized, 0);
	atomic_set(&l2_ids_initialized, 0);

	spin_lock(&core_type_lock);
	cpu_core_type[cpu] = -2;
	spin_unlock(&core_type_lock);

	mutex_lock(&pcore_mask_lock);
	l2_domain_count = 0;
	mutex_unlock(&pcore_mask_lock);

	return 0;
}

/**
 * hybrid_irq_tuning_exit - Module exit function
 *
 * Cleans up all resources and restores system state when module is unloaded.
 */
static void __exit hybrid_irq_tuning_exit(void)
{
	/* If we didn't register or disabled, nothing to do */
	if (pcore_cpuhp_state >= 0) {
		cpuhp_remove_state_nocalls(pcore_cpuhp_state);
		pcore_cpuhp_state = -1;
	}

	/* Free all resources */
	free_l2_domain_masks();

	/* Reset state */
	atomic_set(&pcore_mask_initialized, 0);
}

/**
 * hybrid_irq_tuning - Module initialization function
 *
 * Sets up hybrid CPU optimization for IRQ affinity on Raptor Lake
 * and similar hybrid architectures.
 *
 * Return: 0 on success, negative error code on failure
 */
static int __init hybrid_irq_tuning(void)
{
	int ret = 0, cpu;
	cpumask_var_t pcore_copy, ecore_mask;

	if (!hybrid_cpu_detected() || !irq_pcore_affinity)
		return 0;

	for_each_possible_cpu(cpu) {
		if (cpu < NR_CPUS)
			numa_node_for_cpu[cpu] = cpu_to_node(cpu);
	}

	init_l2_core_ids();
	init_freq_info();

	ret = cpuhp_setup_state(CPUHP_AP_ONLINE_DYN, "irq/pcore_affinity:online",
				pcore_cpu_notify, pcore_cpu_notify);
	if (ret < 0) {
		pr_err("Failed to register CPU hotplug callback: %d\n", ret);
		return ret;
	}
	pcore_cpuhp_state = ret;

	/* Optional default affinity override */
	if (default_affinity_mode &&
	    zalloc_cpumask_var(&pcore_copy, GFP_KERNEL) &&
	    zalloc_cpumask_var(&ecore_mask, GFP_KERNEL)) {

		if (get_pcore_mask(pcore_copy) == 0 && !cpumask_empty(pcore_copy)) {
			cpumask_and(pcore_copy, pcore_copy, cpu_online_mask);
			cpumask_andnot(ecore_mask, cpu_online_mask, pcore_copy);

			if (default_affinity_mode == 1) {
				cpumask_copy(irq_default_affinity, pcore_copy);
				pr_info("IRQ default affinity set to P-cores\n");
			} else if (default_affinity_mode == 2 && !cpumask_empty(ecore_mask)) {
				cpumask_copy(irq_default_affinity, ecore_mask);
				pr_info("IRQ default affinity set to E-cores\n");
			}
		}

		free_cpumask_var(ecore_mask);
		free_cpumask_var(pcore_copy);
	}

	return 0;
}
core_initcall(hybrid_irq_tuning);
module_exit(hybrid_irq_tuning_exit);
#endif /* CONFIG_X86 */

/* Preserve original algorithm with safety checks */
static void default_calc_sets(struct irq_affinity *affd, unsigned int affvecs)
{
	if (!affd)
		return;

	affd->nr_sets = 1;
	affd->set_size[0] = affvecs;
}

/**
 * irq_create_affinity_masks - Create CPU affinity masks for IRQ distribution
 * @nvecs: Number of vectors to create masks for
 * @affd: IRQ affinity descriptor
 *
 * Creates affinity masks for IRQ vectors, optimized for hybrid CPU architectures
 * when available. Includes proper bounds checking and error handling.
 *
 * Return: Array of affinity descriptors or NULL on failure
 */
struct irq_affinity_desc *
irq_create_affinity_masks(unsigned int nvecs, struct irq_affinity *affd)
{
	unsigned int affvecs, curvec, usedvecs, i;
	struct irq_affinity_desc *masks = NULL;
	bool hotplug_locked = false;

	if (!affd) {
		return NULL;
	}

	if (nvecs > affd->pre_vectors + affd->post_vectors) {
		affvecs = nvecs - affd->pre_vectors - affd->post_vectors;
	} else {
		affvecs = 0;
	}

	if (!affd->calc_sets) {
		affd->calc_sets = default_calc_sets;
	}

	affd->calc_sets(affd, affvecs);

	if (WARN_ON_ONCE(affd->nr_sets > IRQ_AFFINITY_MAX_SETS)) {
		return NULL;
	}

	if (!affvecs) {
		return NULL;
	}

	masks = kcalloc(nvecs, sizeof(*masks), GFP_KERNEL);
	if (!masks) {
		return NULL;
	}

	for (curvec = 0; curvec < affd->pre_vectors && curvec < nvecs; curvec++) {
		cpumask_copy(&masks[curvec].mask, irq_default_affinity);
	}

	cpus_read_lock();
	hotplug_locked = true;

	for (i = 0, usedvecs = 0, curvec = affd->pre_vectors;
	     i < affd->nr_sets && curvec < nvecs; i++) {
		unsigned int this_vecs = affd->set_size[i];
		struct cpumask *result = NULL;
		int j;

		if (this_vecs == 0) {
			continue;
		}

#ifdef CONFIG_X86
		if (hybrid_cpu_detected() && irq_pcore_affinity) {
			result = group_cpus_hybrid_first(this_vecs);
		} else
#endif
		{
			result = group_cpus_evenly(this_vecs);
		}

		if (!result) {
			if (hotplug_locked) {
				cpus_read_unlock();
			}
			kfree(masks);
			return NULL;
		}

		for (j = 0; j < (int)this_vecs && (curvec + (unsigned int)j) < nvecs; j++) {
			if (cpumask_empty(&result[j])) {
				cpumask_copy(&masks[curvec + (unsigned int)j].mask, irq_default_affinity);
			} else {
				cpumask_copy(&masks[curvec + (unsigned int)j].mask, &result[j]);
			}
		}

		kfree(result);

		{
			unsigned int used = min(this_vecs, nvecs - curvec);
			curvec += used;
			usedvecs += used;
		}
	}

	if (hotplug_locked) {
		cpus_read_unlock();
		hotplug_locked = false;
	}

	for (; curvec < nvecs; curvec++) {
		cpumask_copy(&masks[curvec].mask, irq_default_affinity);
	}

	for (i = affd->pre_vectors; i < nvecs - affd->post_vectors; i++) {
		masks[i].is_managed = 1;
	}

	return masks;
}

/**
 * irq_calc_affinity_vectors - Calculate optimal number of vectors for IRQ affinity
 * @minvec: Minimum number of vectors
 * @maxvec: Maximum number of vectors
 * @affd: IRQ affinity descriptor
 *
 * Do not restrict vectors to P-cores; allow drivers (e.g., NICs) to use full parallelism
 * across online CPUs where appropriate. This restores expected vector counts and avoids
 * raising CPU utilization due to queue under-allocation.
 */
unsigned int irq_calc_affinity_vectors(unsigned int minvec, unsigned int maxvec,
                                       const struct irq_affinity *affd)
{
    unsigned int resv, set_vecs = 0;
    unsigned int diff;

    if (!affd)
        return 0;

    resv = affd->pre_vectors + affd->post_vectors;
    if (resv > minvec)
        return 0;

    if (check_sub_overflow(maxvec, resv, &diff))
        return 0;

    if (affd->calc_sets) {
        set_vecs = diff;
    } else {
        cpus_read_lock();
        set_vecs = cpumask_weight(cpu_online_mask);
        cpus_read_unlock();
    }

    if (set_vecs == 0)
        set_vecs = 1;

    return resv + min(set_vecs, diff);
}

/* Module metadata */
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Intel Corporation");
MODULE_DESCRIPTION("Raptor Lake IRQ Affinity Optimizations");
