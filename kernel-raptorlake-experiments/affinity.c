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
#ifdef CONFIG_X86
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
	static int is_hybrid = -1;
	static const struct x86_cpu_id hybrid_ids[] = {
		{ .family = 6, .model = INTEL_FAM6_RAPTORLAKE,   .driver_data = 0 },
		{ .family = 6, .model = INTEL_FAM6_ALDERLAKE,    .driver_data = 0 },
		{ .family = 6, .model = INTEL_FAM6_ALDERLAKE_L,  .driver_data = 0 },
		{}
	};

	if (is_hybrid == -1)
		is_hybrid = x86_match_cpu(hybrid_ids) ? 1 : 0;

	return is_hybrid == 1;
}

/**
 * init_freq_info - Initialize frequency information for heuristic detection
 *
 * Efficiently calculates and caches maximum CPU frequency for use in core type detection.
 * Only performs the calculation once for all CPUs.
 */
static void init_freq_info(void)
{
	unsigned int freq, temp_max = 0;
	int c;

	/* Only initialize once - avoid unnecessary work */
	if (atomic_read(&freq_initialized) != 0)
		return;

	/* Calculate max frequency in a single pass */
	for_each_online_cpu(c) {
		freq = cpufreq_quick_get_max(c);
		if (freq > temp_max)
			temp_max = freq;
	}

	/* Atomic update to ensure we only set the value once */
	if (atomic_cmpxchg(&freq_initialized, 0, 1) == 0)
		max_cpu_freq = temp_max;
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

	if (atomic_read(&l2_ids_initialized) != 0)
		return;

	for_each_possible_cpu(cpu) {
		if (cpu < NR_CPUS)
			l2_core_ids[cpu] = topology_physical_package_id(cpu) * 100 + topology_core_id(cpu);
	}

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

	/* Validate CPU ID */
	if (!cpu_possible(cpu))
		return -1;

	/* Fast path: return cached result if available */
	if (cpu_core_type[cpu] != -2)
		return cpu_core_type[cpu];

	/* Early return for non-hybrid CPUs */
	if (!hybrid_cpu_detected()) {
		spin_lock_irqsave(&core_type_lock, flags);
		if (cpu_core_type[cpu] == -2) /* Check again under lock */
			cpu_core_type[cpu] = -1;
		core_type = cpu_core_type[cpu];
		spin_unlock_irqrestore(&core_type_lock, flags);
		return core_type;
	}

	/* Method 1: Use official core type if available (most reliable) */
	#ifdef CONFIG_INTEL_HYBRID_CPU
	if (cpu_data(cpu).x86_core_type == X86_CORE_TYPE_INTEL_CORE) {
		spin_lock_irqsave(&core_type_lock, flags);
		if (cpu_core_type[cpu] == -2)
			cpu_core_type[cpu] = 1;
		spin_unlock_irqrestore(&core_type_lock, flags);
		return 1;  /* P-core */
	} else if (cpu_data(cpu).x86_core_type == X86_CORE_TYPE_INTEL_ATOM) {
		spin_lock_irqsave(&core_type_lock, flags);
		if (cpu_core_type[cpu] == -2)
			cpu_core_type[cpu] = 0;
		spin_unlock_irqrestore(&core_type_lock, flags);
		return 0;  /* E-core */
	}
	#endif

	/* Get lock for remaining detection methods */
	spin_lock_irqsave(&core_type_lock, flags);

	/* Check cache again under lock */
	if (cpu_core_type[cpu] != -2) {
		core_type = cpu_core_type[cpu];
		spin_unlock_irqrestore(&core_type_lock, flags);
		return core_type;
	}

	/* Method 2: Thread siblings count (also reliable for Raptor Lake) */
	const struct cpumask *thread_siblings = topology_sibling_cpumask(cpu);
	if (thread_siblings && cpumask_weight(thread_siblings) > 1) {
		cpu_core_type[cpu] = 1;  /* Multiple threads per core = P-core */
		spin_unlock_irqrestore(&core_type_lock, flags);
		return 1;
	}

	/* Release lock for potentially expensive frequency operations */
	spin_unlock_irqrestore(&core_type_lock, flags);

	/* Initialize frequency info if needed */
	if (atomic_read(&freq_initialized) == 0)
		init_freq_info();

	/* Reacquire lock */
	spin_lock_irqsave(&core_type_lock, flags);

	/* Check cache again after reacquiring lock */
	if (cpu_core_type[cpu] != -2) {
		core_type = cpu_core_type[cpu];
		spin_unlock_irqrestore(&core_type_lock, flags);
		return core_type;
	}

	/* Method 3: Frequency-based heuristic (last resort) */
	if (max_cpu_freq > 0) {
		unsigned int cpu_freq = cpufreq_quick_get_max(cpu);
		if (cpu_freq >= max_cpu_freq * 95 / 100) {
			cpu_core_type[cpu] = 1;  /* Within 5% of max frequency = likely P-core */
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 1;
		} else if (cpu_freq <= max_cpu_freq * 70 / 100) {
			cpu_core_type[cpu] = 0;  /* Below 70% of max frequency = likely E-core */
			spin_unlock_irqrestore(&core_type_lock, flags);
			return 0;
		}
	}

	/* Cannot determine reliably */
	cpu_core_type[cpu] = -1;
	spin_unlock_irqrestore(&core_type_lock, flags);
	return -1;
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
	int core_type = get_core_type(cpu);

	if (core_type == 0) /* E-core */
		return cpu_l2c_shared_mask(cpu);
	else if (core_type == 1) /* P-core */
		return cpu_llc_shared_mask(cpu);
	else
		return cpu_llc_shared_mask(cpu); /* Default to LLC */
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
	if (!dst)
		return -EINVAL;

	if (atomic_read_acquire(&pcore_mask_initialized) == 0) {
		mutex_lock(&pcore_mask_lock);
		if (atomic_read(&pcore_mask_initialized) == 0) {
			int cpu;
			int core_id, prev_core = -1;
			int siblings = 0;
			struct cpumask temp_mask;

			cpumask_clear(&pcore_mask);
			cpumask_clear(&temp_mask);

			/* First try: direct core type detection if available */
			bool direct_detection = false;

			for_each_possible_cpu(cpu) {
				int core_type = get_core_type(cpu);
				if (core_type == 1) {  /* P-core */
					cpumask_set_cpu(cpu, &pcore_mask);
					direct_detection = true;
				}
				/* Store NUMA node information for each CPU */
				if (cpu < NR_CPUS)
					numa_node_for_cpu[cpu] = cpu_to_node(cpu);
			}

			/* If direct detection didn't work, use heuristics */
			if (!direct_detection) {
				/* Second try: count siblings per core to identify P-cores */
				for_each_online_cpu(cpu) {
					core_id = topology_core_id(cpu);

					/* Check if this is a new core */
					if (core_id != prev_core) {
						/* New core encountered */
						if (prev_core != -1) {
							/* Process previous core */
							if (siblings >= 2) {
								/* Previous core had hyperthreading - likely a P-core */
								cpumask_or(&pcore_mask, &pcore_mask, &temp_mask);
							}
							cpumask_clear(&temp_mask);
						}

						prev_core = core_id;
						siblings = 1;
						cpumask_set_cpu(cpu, &temp_mask);
					} else {
						/* Another sibling of the current core */
						siblings++;
						cpumask_set_cpu(cpu, &temp_mask);
					}
				}

				/* Handle the last core */
				if (prev_core != -1 && siblings >= 2) {
					cpumask_or(&pcore_mask, &pcore_mask, &temp_mask);
				}

				/* Third try: find fastest cores by frequency */
				if (cpumask_empty(&pcore_mask)) {
					unsigned int max_freq = 0;
					int max_freq_cpu = -1;

					for_each_online_cpu(cpu) {
						unsigned int freq = cpufreq_quick_get_max(cpu);
						if (freq > max_freq && freq > 0) {
							max_freq = freq;
							max_freq_cpu = cpu;
						}
					}

					if (max_freq_cpu >= 0 && max_freq > 0) {
						/* Use cores with the same max frequency (within 5%) */
						unsigned int threshold = max_freq * 95 / 100;

						for_each_online_cpu(cpu) {
							unsigned int freq = cpufreq_quick_get_max(cpu);
							if (freq >= threshold && freq > 0)
								cpumask_set_cpu(cpu, &pcore_mask);
						}
					}
				}
			}

			/* Fallback to all CPUs if still no cores identified */
			if (cpumask_empty(&pcore_mask))
				cpumask_copy(&pcore_mask, cpu_online_mask);

			/* Memory barrier before setting initialized flag */
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
	bool using_fallback = false;
	int total_cpus;

	/* Validate input */
	if (!p_core_mask || cpumask_empty(p_core_mask)) {
		pr_warn("Empty P-core mask provided\n");
		return -EINVAL;
	}

	/* Pre-calculate L2 core IDs if not done already */
	if (atomic_read(&l2_ids_initialized) == 0)
		init_l2_core_ids();

	mutex_lock(&pcore_mask_lock);

	/* Clean up existing resources */
	if (l2_domain_masks) {
		kfree(l2_domain_masks);
		l2_domain_masks = NULL;
		l2_domain_count = 0;
	}

	/* Allocate memory with bounds check */
	if (MAX_CORES_PER_NODE == 0) {
		mutex_unlock(&pcore_mask_lock);
		pr_err("Invalid MAX_CORES_PER_NODE value\n");
		return -EINVAL;
	}

	l2_domain_masks = kcalloc(MAX_CORES_PER_NODE, sizeof(struct cpumask), GFP_KERNEL);
	if (!l2_domain_masks) {
		mutex_unlock(&pcore_mask_lock);
		pr_warn("Failed to allocate L2 domain masks\n");
		return -ENOMEM;
	}

	l2_domain_count = 0;

	/* Primary detection: use cache topology more efficiently */
	for_each_cpu(cpu, p_core_mask) {
		const struct cpumask *shared_mask = get_cache_shared_mask(cpu);
		bool found = false;

		/* Validate mask */
		if (!shared_mask || cpumask_empty(shared_mask) ||
			cpumask_weight(shared_mask) > MAX_CORES_PER_NODE/2) {
			using_fallback = true;
		continue;
			}

			/* Skip CPUs already in a domain to avoid redundant checks */
			for (i = 0; i < l2_domain_count; i++) {
				if (cpumask_test_cpu(cpu, &l2_domain_masks[i])) {
					found = true;
					break;
				}
			}
			if (found)
				continue;

		/* Check if domain already exists */
		for (i = 0; i < l2_domain_count; i++) {
			if (cpumask_equal(&l2_domain_masks[i], shared_mask)) {
				found = true;
				break;
			}
		}

		/* Add new domain if needed */
		if (!found && l2_domain_count < MAX_CORES_PER_NODE) {
			cpumask_copy(&l2_domain_masks[l2_domain_count], shared_mask);
			l2_domain_count++;
		}
	}

	/* Optimized fallback: use pre-calculated L2 IDs */
	if (l2_domain_count == 0 || using_fallback) {
		/* Use a more efficient approach with a hash table-like structure */
		int l2_id_max = 0;
		int l2_id, dom_idx;
		int *id_to_domain = NULL;

		/* Reset domain count */
		l2_domain_count = 0;

		/* Find maximum L2 ID */
		for_each_cpu(cpu, p_core_mask) {
			if (cpu < NR_CPUS && l2_core_ids[cpu] > l2_id_max)
				l2_id_max = l2_core_ids[cpu];
		}

		/* Create mapping array (+1 for zero-based indexing) */
		id_to_domain = kcalloc(l2_id_max + 1, sizeof(int), GFP_KERNEL);
		if (!id_to_domain) {
			kfree(l2_domain_masks);  /* Free previously allocated memory */
			l2_domain_masks = NULL;
			mutex_unlock(&pcore_mask_lock);
			return -ENOMEM;
		}

		/* Initialize all entries to -1 (no domain) */
		for (i = 0; i <= l2_id_max; i++)
			id_to_domain[i] = -1;

		/* One-pass domain assignment using direct mapping */
		for_each_cpu(cpu, p_core_mask) {
			if (cpu < NR_CPUS) {
				l2_id = l2_core_ids[cpu];

				/* Check bounds */
				if (l2_id < 0 || l2_id > l2_id_max)
					continue;

				dom_idx = id_to_domain[l2_id];

				if (dom_idx == -1) {
					/* Create new domain */
					if (l2_domain_count < MAX_CORES_PER_NODE) {
						dom_idx = l2_domain_count++;
						id_to_domain[l2_id] = dom_idx;
						cpumask_clear(&l2_domain_masks[dom_idx]);
					} else {
						continue;  /* Too many domains */
					}
				}

				/* Add CPU to domain */
				cpumask_set_cpu(cpu, &l2_domain_masks[dom_idx]);
			}
		}

		kfree(id_to_domain);
	}

	/* Verify all CPUs were assigned */
	total_cpus = 0;
	for (i = 0; i < l2_domain_count; i++)
		total_cpus += cpumask_weight(&l2_domain_masks[i]);

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
	struct cpumask p_core_copy;
	struct cpumask *result = NULL;
	struct cpumask e_cores_mask;
	DECLARE_BITMAP(assigned, NR_CPUS);
	int i, j, cpu, grp_idx = 0;
	int ret;

	if (!num_grps)
		return NULL;

	if (!irq_pcore_affinity || !hybrid_cpu_detected())
		return group_cpus_evenly(num_grps);

	/* Get P-cores using our improved function */
	cpumask_clear(&p_core_copy);
	ret = get_pcore_mask(&p_core_copy);
	if (ret || cpumask_empty(&p_core_copy))
		return group_cpus_evenly(num_grps);

	/* Create result masks */
	result = kcalloc(num_grps, sizeof(struct cpumask), GFP_KERNEL);
	if (!result)
		return group_cpus_evenly(num_grps);

	/* Clear all result masks */
	for (i = 0; i < num_grps; i++)
		cpumask_clear(&result[i]);

	/* Identify E-cores */
	bitmap_zero(assigned, NR_CPUS);
	cpumask_andnot(&e_cores_mask, cpu_online_mask, &p_core_copy);

	/* Identify L2 domains */
	ret = identify_l2_domains(&p_core_copy);
	if (ret) {
		/* Fall back to simple distribution on error */
		int cores = cpumask_weight(&p_core_copy);
		int cores_per_group = cores / num_grps;
		int extra = cores % num_grps;

		for (i = 0; i < num_grps; i++) {
			int count = 0;
			int cores_this_group = cores_per_group + (i < extra ? 1 : 0);

			for_each_cpu(cpu, &p_core_copy) {
				if (!test_bit(cpu, assigned) && count < cores_this_group) {
					cpumask_set_cpu(cpu, &result[i]);
					set_bit(cpu, assigned);
					count++;
				}
			}
		}
	} else {
		/* Cache-aware distribution */
		int total_cores = 0;
		for (i = 0; i < l2_domain_count; i++)
			total_cores += cpumask_weight(&l2_domain_masks[i]);

		/* Distribute domains proportionally */
		for (i = 0; i < l2_domain_count && grp_idx < num_grps; i++) {
			int domain_cores = cpumask_weight(&l2_domain_masks[i]);
			if (domain_cores == 0)
				continue;

			/* Calculate groups for this domain */
			int grps_for_domain = 1;
			if (total_cores > 0) {
				grps_for_domain = (num_grps * domain_cores + total_cores - 1) / total_cores;
				grps_for_domain = min_t(int, grps_for_domain, num_grps - grp_idx);
			}
			grps_for_domain = max(1, grps_for_domain);

			/* Calculate cores per group */
			int cores_per_domain_group = domain_cores / grps_for_domain;
			int domain_extra = domain_cores % grps_for_domain;

			/* Distribute cores */
			for (j = 0; j < grps_for_domain && grp_idx < num_grps; j++, grp_idx++) {
				int cores_this_group = cores_per_domain_group + (j < domain_extra ? 1 : 0);
				int count = 0;

				for_each_cpu(cpu, &l2_domain_masks[i]) {
					if (count >= cores_this_group)
						break;
					if (!test_bit(cpu, assigned)) {
						cpumask_set_cpu(cpu, &result[grp_idx]);
						set_bit(cpu, assigned);
						count++;
					}
				}
			}
		}
	}

	/* Handle E-cores for remaining groups */
	if (grp_idx < num_grps && !cpumask_empty(&e_cores_mask)) {
		int e_cores = cpumask_weight(&e_cores_mask);
		int cores_per_group = e_cores / (num_grps - grp_idx);
		int extra = e_cores % (num_grps - grp_idx);

		for (i = grp_idx; i < num_grps; i++) {
			int count = 0;
			int target = cores_per_group + (i - grp_idx < extra ? 1 : 0);

			for_each_cpu(cpu, &e_cores_mask) {
				if (count >= target)
					break;
				if (!test_bit(cpu, assigned)) {
					cpumask_set_cpu(cpu, &result[i]);
					set_bit(cpu, assigned);
					count++;
				}
			}
		}
	}

	/* NUMA-aware rebalancing for empty groups */
	for (i = 0; i < num_grps; i++) {
		if (cpumask_empty(&result[i])) {
			/* Find best donor CPU from a group with multiple CPUs */
			int donor_cpu = -1;
			int donor_group = -1;
			int best_score = -1;
			int target_node = -1;
			unsigned int j_start, j_end;

			/* Calculate bounds safely without signedness issues */
			j_start = (i > 0) ? (i - 1) : 0;
			j_end = (i + 1 < num_grps) ? (i + 1) : i;

			/* Identify target NUMA node if possible */
			for (j = j_start; j <= j_end; j++) {
				if (j != i && cpumask_weight(&result[j]) > 0) {
					int temp_cpu = cpumask_first(&result[j]);
					if (temp_cpu < NR_CPUS) {
						target_node = numa_node_for_cpu[temp_cpu];
						break;
					}
				}
			}

			/* Find groups with multiple CPUs */
			for (j = 0; j < num_grps; j++) {
				if (cpumask_weight(&result[j]) > 1) {
					/* Evaluate each CPU as potential donor */
					for_each_cpu(cpu, &result[j]) {
						int score = 0;
						int cpu_node = (cpu < NR_CPUS) ? numa_node_for_cpu[cpu] : -1;
						int core_type = get_core_type(cpu);
						const struct cpumask *cache_mask;
						int cache_siblings = 0;
						int numa_siblings = 0;
						int sibling;

						/* NUMA locality is highest priority */
						if (target_node >= 0 && cpu_node == target_node)
							score += 1000;

						/* Core type considerations - prefer donating E-cores */
						if (core_type == 0)
							score += 500;

						/* Cache topology considerations */
						cache_mask = get_cache_shared_mask(cpu);
						for_each_cpu(sibling, &result[j]) {
							if (sibling != cpu) {
								if (cache_mask && cpumask_test_cpu(sibling, cache_mask))
									cache_siblings++;
								if (cpu_node >= 0 && sibling < NR_CPUS &&
									numa_node_for_cpu[sibling] == cpu_node)
									numa_siblings++;
							}
						}

						/* Prefer CPUs with more siblings left behind in same group */
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
				/* Last resort: fall back to standard distribution */
				kfree(result);
				return group_cpus_evenly(num_grps);
			}
		}
	}

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
	if (cpu >= NR_CPUS) {
		pr_warn("pcore_cpu_notify: cpu %u out of range\n", cpu);
		return -EINVAL;
	}

	/* Update NUMA node info (doesn't require lock) */
	numa_node_for_cpu[cpu] = cpu_to_node(cpu);

	/* Reset initialized flags to force recalculation */
	atomic_set(&pcore_mask_initialized, 0);
	atomic_set(&freq_initialized, 0);
	atomic_set(&l2_ids_initialized, 0);

	/* Reset core type cache for changed CPU */
	spin_lock(&core_type_lock);
	cpu_core_type[cpu] = -2;
	spin_unlock(&core_type_lock);

	/* Try to clean up L2 domain information without blocking critical paths */
	if (mutex_trylock(&pcore_mask_lock)) {
		if (l2_domain_masks) {
			kfree(l2_domain_masks);
			l2_domain_masks = NULL;
			l2_domain_count = 0;
		}
		mutex_unlock(&pcore_mask_lock);
	}

	return 0;
}

/**
 * hybrid_irq_tuning_exit - Module exit function
 *
 * Cleans up all resources and restores system state when module is unloaded.
 */
static void __exit hybrid_irq_tuning_exit(void)
{
	if (!hybrid_cpu_detected() || !irq_pcore_affinity)
		return;

	/* Remove hotplug callback */
	cpuhp_remove_state_nocalls(CPUHP_AP_ONLINE_DYN);

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
	struct cpumask pcore_copy;

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

	/* Register CPU hotplug callback */
	ret = cpuhp_setup_state(CPUHP_AP_ONLINE_DYN, "irq/pcore_affinity:online",
							pcore_cpu_notify, pcore_cpu_notify);
	if (ret < 0) {
		pr_err("Failed to register CPU hotplug callback: %d\n", ret);
		return ret;
	}

	/* Get P-core mask and apply to default affinity */
	cpumask_clear(&pcore_copy);
	ret = get_pcore_mask(&pcore_copy);
	if (ret < 0) {
		pr_warn("Failed to get P-core mask: %d\n", ret);
		/* Continue anyway - will use default affinity */
	} else if (!cpumask_empty(&pcore_copy)) {
		cpumask_copy(irq_default_affinity, &pcore_copy);
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

	if (!affd)
		return NULL;

	if (nvecs > affd->pre_vectors + affd->post_vectors)
		affvecs = nvecs - affd->pre_vectors - affd->post_vectors;
	else
		affvecs = 0;

	if (!affd->calc_sets)
		affd->calc_sets = default_calc_sets;

	affd->calc_sets(affd, affvecs);

	if (WARN_ON_ONCE(affd->nr_sets > IRQ_AFFINITY_MAX_SETS))
		return NULL;

	if (!affvecs)
		return NULL;

	masks = kcalloc(nvecs, sizeof(*masks), GFP_KERNEL);
	if (!masks)
		return NULL;

	/* Set pre-vectors to default affinity */
	for (curvec = 0; curvec < affd->pre_vectors && curvec < nvecs; curvec++)
		cpumask_copy(&masks[curvec].mask, irq_default_affinity);

	/* Distribute vectors according to set sizes */
	for (i = 0, usedvecs = 0, curvec = affd->pre_vectors;
		 i < affd->nr_sets && curvec < nvecs; i++) {
		unsigned int this_vecs = affd->set_size[i];
	struct cpumask *result = NULL;
	int j;

	if (this_vecs == 0)
		continue;

		#ifdef CONFIG_X86
		if (hybrid_cpu_detected() && irq_pcore_affinity)
			result = group_cpus_hybrid_first(this_vecs);
		else
			#endif
			result = group_cpus_evenly(this_vecs);

		if (!result) {
			kfree(masks);
			return NULL;
		}

		/* Copy result masks to output */
		for (j = 0; j < this_vecs && (curvec + j) < nvecs; j++) {
			if (cpumask_empty(&result[j]))
				cpumask_copy(&masks[curvec + j].mask, irq_default_affinity);
			else
				cpumask_copy(&masks[curvec + j].mask, &result[j]);
		}

		kfree(result);

		/* Safely advance counters */
		unsigned int used = min(this_vecs, nvecs - curvec);
		curvec += used;
		usedvecs += used;
		 }

		 /* Set remaining vectors to default affinity */
		 for (; curvec < nvecs; curvec++)
			 cpumask_copy(&masks[curvec].mask, irq_default_affinity);

	/* Mark managed vectors */
	for (i = affd->pre_vectors; i < nvecs - affd->post_vectors; i++)
		masks[i].is_managed = 1;

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

	/* Check for overflow */
	if (check_sub_overflow(maxvec, resv, &diff))
		return 0;

	if (affd->calc_sets) {
		set_vecs = diff;
	} else {
		cpus_read_lock();
		#ifdef CONFIG_X86
		if (hybrid_cpu_detected() && irq_pcore_affinity) {
			struct cpumask pcpu_mask;
			cpumask_clear(&pcpu_mask);
			if (get_pcore_mask(&pcpu_mask) == 0 && !cpumask_empty(&pcpu_mask)) {
				set_vecs = cpumask_weight(&pcpu_mask);
			} else {
				set_vecs = cpumask_weight(cpu_online_mask);
			}
		} else
			#endif
			set_vecs = cpumask_weight(cpu_possible_mask);
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
