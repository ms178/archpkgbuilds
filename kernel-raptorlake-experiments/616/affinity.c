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
 * get_cache_shared_mask - Get cache sharing mask for CPU
 * @cpu: CPU number
 *
 * Returns the appropriate cache sharing mask based on core type
 *
 * Return: Pointer to cpumask
 */
static const struct cpumask *get_cache_shared_mask(int cpu)
{
	const struct cpumask *m = NULL;
	int core_type = get_core_type(cpu);

	#ifdef CONFIG_SCHED_CLUSTER
	if (core_type == 0) {
		m = cpu_clustergroup_mask(cpu);
	}
	#endif
	if (!m || cpumask_empty(m)) {
		m = cpu_llc_shared_mask(cpu);
	}

	return m;
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
 * identify_l2_domains - Optimized L2 cache domain detection
 * @p_core_mask: Mask of P-cores to analyze
 *
 * Maps L2 cache sharing domains on Raptor Lake with optimized fallback mechanism.
 * Pre-calculates L2 core IDs to avoid expensive operations in inner loops.
 *
 * Return: 0 on success, negative error code on failure
 */
static int identify_l2_domains(struct cpumask *p_core_mask)
{
	int i, cpu;
	int total_cpus;

	if (!p_core_mask || cpumask_empty(p_core_mask)) {
		pr_warn("Empty P-core mask provided\n");
		return -EINVAL;
	}

	if (!atomic_read_acquire(&l2_ids_initialized)) {
		init_l2_core_ids();
	}

	mutex_lock(&pcore_mask_lock);

	if (l2_domain_masks) {
		kfree(l2_domain_masks);
		l2_domain_masks = NULL;
		l2_domain_count = 0;
	}

	{
		int alloc_domains = cpumask_weight(p_core_mask);

		if (alloc_domains <= 0) {
			mutex_unlock(&pcore_mask_lock);
			return -ENODATA;
		}

		l2_domain_masks = kcalloc(alloc_domains, sizeof(struct cpumask), GFP_KERNEL);
		if (!l2_domain_masks) {
			mutex_unlock(&pcore_mask_lock);
			pr_warn("Failed to allocate L2 domain masks\n");
			return -ENOMEM;
		}
	}

	l2_domain_count = 0;

	/* Use a heap cpumask to avoid large stack frames */
	{
		cpumask_var_t tmp;
		if (!zalloc_cpumask_var(&tmp, GFP_KERNEL)) {
			kfree(l2_domain_masks);
			l2_domain_masks = NULL;
			mutex_unlock(&pcore_mask_lock);
			return -ENOMEM;
		}

		for_each_cpu(cpu, p_core_mask) {
			const struct cpumask *shared_mask = get_cache_shared_mask(cpu);
			bool found = false;
			int j;

			if (!shared_mask || cpumask_empty(shared_mask)) {
				continue;
			}

			cpumask_and(tmp, shared_mask, p_core_mask);
			if (cpumask_empty(tmp)) {
				continue;
			}

			for (j = 0; j < l2_domain_count; j++) {
				if (cpumask_test_cpu(cpu, &l2_domain_masks[j])) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}

			for (j = 0; j < l2_domain_count; j++) {
				if (cpumask_equal(&l2_domain_masks[j], tmp)) {
					found = true;
					break;
				}
			}

			if (!found) {
				cpumask_copy(&l2_domain_masks[l2_domain_count], tmp);
				l2_domain_count++;
			}
		}

		free_cpumask_var(tmp);
	}

	if (l2_domain_count == 0) {
		int max_domains = cpumask_weight(p_core_mask);
		struct id_map {
			int id;
			int dom_idx;
		} *map = NULL;
		int map_count = 0;

		if (max_domains <= 0) {
			kfree(l2_domain_masks);
			l2_domain_masks = NULL;
			mutex_unlock(&pcore_mask_lock);
			return -ENODATA;
		}

		map = kcalloc(max_domains, sizeof(*map), GFP_KERNEL);
		if (!map) {
			kfree(l2_domain_masks);
			l2_domain_masks = NULL;
			mutex_unlock(&pcore_mask_lock);
			return -ENOMEM;
		}

		for (i = 0; i < max_domains; i++) {
			map[i].id = -1;
			map[i].dom_idx = -1;
		}

		for_each_cpu(cpu, p_core_mask) {
			int l2_id, dom_idx = -1;
			int m;

			if (cpu >= NR_CPUS) {
				continue;
			}

			l2_id = l2_core_ids[cpu];

			for (m = 0; m < map_count; m++) {
				if (map[m].id == l2_id) {
					dom_idx = map[m].dom_idx;
					break;
				}
			}

			if (dom_idx == -1) {
				if (map_count < max_domains) {
					dom_idx = map_count;
					map[map_count].id = l2_id;
					map[map_count].dom_idx = dom_idx;
					cpumask_clear(&l2_domain_masks[dom_idx]);
					map_count++;
					l2_domain_count++;
				} else {
					continue;
				}
			}

			cpumask_set_cpu(cpu, &l2_domain_masks[dom_idx]);
		}

		kfree(map);
	}

	total_cpus = 0;
	for (i = 0; i < l2_domain_count; i++) {
		total_cpus += cpumask_weight(&l2_domain_masks[i]);
	}

	if (total_cpus < cpumask_weight(p_core_mask)) {
		pr_warn("L2 domain detection incomplete: %d/%d CPUs\n",
			total_cpus, cpumask_weight(p_core_mask));
	}

	mutex_unlock(&pcore_mask_lock);
	return l2_domain_count > 0 ? 0 : -ENODATA;
}

/**
 * group_cpus_hybrid_first - Distribute IRQs with hybrid CPU awareness
 * @num_grps: Number of groups to create
 *
 * Creates CPU groups optimized for IRQ distribution on hybrid CPUs.
 * Prioritizes P-cores and considers cache topology for performance.
 *
 * Return: Array of CPU masks or NULL on failure
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

	if (!num_grps) {
		return NULL;
	}

	if (!irq_pcore_affinity || !hybrid_cpu_detected()) {
		return group_cpus_evenly(num_grps);
	}

	if (!zalloc_cpumask_var(&p_core_copy, GFP_KERNEL)) {
		return group_cpus_evenly(num_grps);
	}
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

	ret = get_pcore_mask(p_core_copy);
	if (ret || cpumask_empty(p_core_copy)) {
		bitmap_free(assigned);
		free_cpumask_var(e_cores_mask);
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}

	result = kcalloc(num_grps, sizeof(struct cpumask), GFP_KERNEL);
	if (!result) {
		bitmap_free(assigned);
		free_cpumask_var(e_cores_mask);
		free_cpumask_var(p_core_copy);
		return group_cpus_evenly(num_grps);
	}
	for (i = 0; i < num_grps; i++) {
		cpumask_clear(&result[i]);
	}

	cpumask_andnot(e_cores_mask, cpu_online_mask, p_core_copy);

	/* Identify L2 domains and snapshot them under the lock */
	ret = identify_l2_domains(p_core_copy);
	if (ret == 0) {
		mutex_lock(&pcore_mask_lock);
		if (l2_domain_count > 0 && l2_domain_masks) {
			l2_local_count = l2_domain_count;
			l2_local_masks = kcalloc(l2_local_count, sizeof(struct cpumask), GFP_KERNEL);
			if (l2_local_masks) {
				for (i = 0; i < l2_local_count; i++) {
					cpumask_copy(&l2_local_masks[i], &l2_domain_masks[i]);
				}
			}
		}
		mutex_unlock(&pcore_mask_lock);

		if (!l2_local_masks || l2_local_count == 0) {
			ret = -ENOMEM;
		}
	}

	if (ret) {
		int cores = cpumask_weight(p_core_copy);
		int cores_per_group = num_grps ? (cores / num_grps) : 0;
		int extra = num_grps ? (cores % num_grps) : 0;

		for (i = 0; i < (int)num_grps; i++) {
			int count = 0;
			int cores_this_group = cores_per_group + (i < extra ? 1 : 0);

			for_each_cpu(cpu, p_core_copy) {
				if (!test_bit(cpu, assigned) && count < cores_this_group) {
					cpumask_set_cpu(cpu, &result[i]);
					set_bit(cpu, assigned);
					count++;
				}
			}
		}
	} else {
		int total_cores = 0;

		for (i = 0; i < l2_local_count; i++) {
			total_cores += cpumask_weight(&l2_local_masks[i]);
		}

		for (i = 0; i < l2_local_count && grp_idx < (int)num_grps; i++) {
			int domain_cores = cpumask_weight(&l2_local_masks[i]);
			int grps_for_domain = 1;

			if (domain_cores == 0) {
				continue;
			}

			if (total_cores > 0) {
				grps_for_domain = (num_grps * domain_cores + total_cores - 1) / total_cores;
				if (grps_for_domain > (int)num_grps - grp_idx) {
					grps_for_domain = (int)num_grps - grp_idx;
				}
			}
			if (grps_for_domain < 1) {
				grps_for_domain = 1;
			}

			{
				int cores_per_domain_group = domain_cores / grps_for_domain;
				int domain_extra = domain_cores % grps_for_domain;

				for (j = 0; j < grps_for_domain && grp_idx < (int)num_grps; j++, grp_idx++) {
					int cores_this_group = cores_per_domain_group + (j < domain_extra ? 1 : 0);
					int count = 0;

					for_each_cpu(cpu, &l2_local_masks[i]) {
						if (count >= cores_this_group) {
							break;
						}
						if (!test_bit(cpu, assigned)) {
							cpumask_set_cpu(cpu, &result[grp_idx]);
							set_bit(cpu, assigned);
							count++;
						}
					}
				}
			}
		}
	}

	if (grp_idx < (int)num_grps && !cpumask_empty(e_cores_mask)) {
		int e_cores = cpumask_weight(e_cores_mask);
		int remaining = (int)num_grps - grp_idx;
		int cores_per_group = remaining > 0 ? e_cores / remaining : 0;
		int extra = remaining > 0 ? e_cores % remaining : 0;

		for (i = grp_idx; i < (int)num_grps; i++) {
			int count = 0;
			int target = cores_per_group + (i - grp_idx < extra ? 1 : 0);

			for_each_cpu(cpu, e_cores_mask) {
				if (count >= target) {
					break;
				}
				if (!test_bit(cpu, assigned)) {
					cpumask_set_cpu(cpu, &result[i]);
					set_bit(cpu, assigned);
					count++;
				}
			}
		}
	}

	for (i = 0; i < (int)num_grps; i++) {
		if (cpumask_empty(&result[i])) {
			int donor_cpu = -1;
			int donor_group = -1;
			int best_score = -1;
			int target_node = -1;
			unsigned int j_start, j_end;

			j_start = (i > 0) ? (i - 1) : 0;
			j_end = (i + 1 < (int)num_grps) ? (i + 1) : (unsigned int)i;

			for (j = j_start; j <= j_end; j++) {
				if ((int)j != i && cpumask_weight(&result[j]) > 0) {
					int temp_cpu = cpumask_first(&result[j]);

					if (temp_cpu < NR_CPUS) {
						target_node = numa_node_for_cpu[temp_cpu];
						break;
					}
				}
			}

			for (j = 0; j < (int)num_grps; j++) {
				if (cpumask_weight(&result[j]) > 1) {
					for_each_cpu(cpu, &result[j]) {
						int score = 0;
						int cpu_node = (cpu < NR_CPUS) ? numa_node_for_cpu[cpu] : -1;
						int ctype = get_core_type(cpu);
						const struct cpumask *cache_mask;
						int cache_siblings = 0;
						int numa_siblings = 0;
						int sibling;

						if (target_node >= 0 && cpu_node == target_node) {
							score += 1000;
						}
						if (ctype == 0) {
							score += 500;
						}

						cache_mask = get_cache_shared_mask(cpu);
						for_each_cpu(sibling, &result[j]) {
							if (sibling != cpu) {
								if (cache_mask && cpumask_test_cpu(sibling, cache_mask)) {
									cache_siblings++;
								}
								if (cpu_node >= 0 && sibling < NR_CPUS &&
								    numa_node_for_cpu[sibling] == cpu_node) {
									numa_siblings++;
								}
							}
						}

						score += cache_siblings * 10;
						score += numa_siblings * 50;

						if (score > best_score) {
							best_score = score;
							donor_cpu = cpu;
							donor_group = j;
						}
					}
				}
			}

			if (donor_group >= 0 && donor_cpu >= 0) {
				cpumask_clear_cpu(donor_cpu, &result[donor_group]);
				cpumask_set_cpu(donor_cpu, &result[i]);
			} else {
				kfree(l2_local_masks);
				kfree(result);
				bitmap_free(assigned);
				free_cpumask_var(e_cores_mask);
				free_cpumask_var(p_core_copy);
				return group_cpus_evenly(num_grps);
			}
		}
	}

	kfree(l2_local_masks);
	bitmap_free(assigned);
	free_cpumask_var(e_cores_mask);
	free_cpumask_var(p_core_copy);
	return result;
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
	cpumask_var_t pcore_copy;

	if (!hybrid_cpu_detected() || !irq_pcore_affinity)
		return 0;

	/* Initialize NUMA node mapping with bounds checking */
	for_each_possible_cpu(cpu) {
		if (cpu < NR_CPUS)
			numa_node_for_cpu[cpu] = cpu_to_node(cpu);
	}

	/* Pre-initialize L2 core IDs */
	init_l2_core_ids();

	/* Pre-initialize frequency information */
	init_freq_info();

	/* Register CPU hotplug callback; store the dynamic state id */
	ret = cpuhp_setup_state(CPUHP_AP_ONLINE_DYN, "irq/pcore_affinity:online",
				pcore_cpu_notify, pcore_cpu_notify);
	if (ret < 0) {
		pr_err("Failed to register CPU hotplug callback: %d\n", ret);
		return ret;
	}
	pcore_cpuhp_state = ret;

	/* Get P-core mask and apply to default affinity */
	if (zalloc_cpumask_var(&pcore_copy, GFP_KERNEL)) {
		ret = get_pcore_mask(pcore_copy);
		if (ret < 0) {
			pr_warn("Failed to get P-core mask: %d\n", ret);
			/* Continue anyway - will use default affinity */
		} else if (!cpumask_empty(pcore_copy)) {
			cpumask_copy(irq_default_affinity, pcore_copy);
		}
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
 * Determines the optimal number of interrupt vectors for the system
 * based on CPU topology.
 *
 * Return: Optimal number of vectors or 0 on failure
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

	/* Guard overflow */
	if (check_sub_overflow(maxvec, resv, &diff))
		return 0;

	if (affd->calc_sets) {
		set_vecs = diff;
	} else {
		cpus_read_lock();
#ifdef CONFIG_X86
		if (hybrid_cpu_detected() && irq_pcore_affinity) {
			cpumask_var_t pcpu_mask;
			if (zalloc_cpumask_var(&pcpu_mask, GFP_KERNEL)) {
				if (get_pcore_mask(pcpu_mask) == 0 && !cpumask_empty(pcpu_mask)) {
					set_vecs = cpumask_weight(pcpu_mask);
				} else {
					set_vecs = cpumask_weight(cpu_online_mask);
				}
				free_cpumask_var(pcpu_mask);
			} else {
				set_vecs = cpumask_weight(cpu_online_mask);
			}
		} else
#endif
		{
			set_vecs = cpumask_weight(cpu_possible_mask);
		}
		cpus_read_unlock();
	}

	/* Ensure at least one vector */
	if (set_vecs == 0)
		set_vecs = 1;

	return resv + min(set_vecs, diff);
}

/* Module metadata */
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Intel Corporation");
MODULE_DESCRIPTION("Raptor Lake IRQ Affinity Optimizations");
