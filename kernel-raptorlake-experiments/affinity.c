// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2016 Thomas Gleixner.
 * Copyright (C) 2016-2017 Christoph Hellwig.
 * Raptor Lake optimizations (C) 2023 Intel Corporation.
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
#include <linux/cpuhotplug.h>
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
#define INTEL_FAM6_RAPTORLAKE 0x7E
#endif

#ifndef INTEL_FAM6_ALDERLAKE
#define INTEL_FAM6_ALDERLAKE 0x7A
#endif

#ifndef INTEL_FAM6_ALDERLAKE_L
#define INTEL_FAM6_ALDERLAKE_L 0x7A
#endif

/* Core type definition if not available */
#ifndef X86_CORE_TYPE_INTEL_CORE
#define X86_CORE_TYPE_INTEL_CORE 1
#endif

/* P-core mask management with proper locking */
static DEFINE_MUTEX(pcore_mask_lock);
static struct cpumask pcore_mask;
static atomic_t pcore_mask_initialized = ATOMIC_INIT(0);
static int numa_node_for_cpu[NR_CPUS];

/* Store L2 cache domain information */
static struct cpumask *l2_domain_masks;
static int l2_domain_count;

/* Optimized detection with proper caching */
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

/* Direct core type detection when available */
static int get_core_type(int cpu)
{
	if (!hybrid_cpu_detected())
		return -1;

	if (!cpu_possible(cpu))
		return -1;

	#ifdef CONFIG_INTEL_HYBRID_CPU
	/* If hybrid detection is supported, check the official field */
	return cpu_data(cpu).x86_core_type == X86_CORE_TYPE_INTEL_CORE ? 1 : 0;
	#else
	/* If not available, use other methods to identify core type */
	return -1;
	#endif
}

/* Cache-aware mask retrieval */
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

/* Enhanced P-core detection using multiple methods */
static const struct cpumask *cpu_pcore_mask(void)
{
	struct cpumask *mask_copy = NULL;

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

			atomic_set_release(&pcore_mask_initialized, 1);
		}
		mutex_unlock(&pcore_mask_lock);
	}

	/* Return a copy to avoid race conditions */
	mask_copy = kmalloc(sizeof(struct cpumask), GFP_KERNEL);
	if (mask_copy) {
		mutex_lock(&pcore_mask_lock);
		cpumask_copy(mask_copy, &pcore_mask);
		mutex_unlock(&pcore_mask_lock);
	} else {
		/* If allocation fails, return the static mask directly - not ideal but better than NULL */
		return &pcore_mask;
	}

	return mask_copy;
}

/* Improved L2 domain identification with cache awareness */
static void identify_l2_domains(const struct cpumask *p_core_mask)
{
	int i, cpu;

	/* Free previous domain masks if they exist */
	if (l2_domain_masks) {
		kfree(l2_domain_masks);
		l2_domain_masks = NULL;
	}

	/* Allocate memory for domain masks */
	l2_domain_masks = kcalloc(MAX_CORES_PER_NODE, sizeof(struct cpumask), GFP_KERNEL);
	if (!l2_domain_masks) {
		l2_domain_count = 0;
		return;
	}

	l2_domain_count = 0;

	/* Group P-cores by their cache domains using direct cache info */
	for_each_cpu(cpu, p_core_mask) {
		const struct cpumask *shared_mask = get_cache_shared_mask(cpu);
		bool found = false;

		if (cpumask_empty(shared_mask))
			continue;

		/* Check if we already have this cache domain */
		for (i = 0; i < l2_domain_count; i++) {
			if (cpumask_equal(&l2_domain_masks[i], shared_mask)) {
				found = true;
				break;
			}
		}

		/* If not found, create a new domain */
		if (!found && l2_domain_count < MAX_CORES_PER_NODE) {
			cpumask_copy(&l2_domain_masks[l2_domain_count], shared_mask);
			l2_domain_count++;
		}
	}

	/* If no domains found using cache info, fall back to core ID */
	if (l2_domain_count == 0) {
		for_each_cpu(cpu, p_core_mask) {
			int l2_id = topology_core_id(cpu);
			bool found = false;

			/* Check if we already have this L2 domain */
			for (i = 0; i < l2_domain_count; i++) {
				int check_cpu;
				for_each_cpu(check_cpu, &l2_domain_masks[i]) {
					if (topology_core_id(check_cpu) == l2_id) {
						found = true;
						cpumask_set_cpu(cpu, &l2_domain_masks[i]);
						break;
					}
				}
				if (found)
					break;
			}

			/* If not found, create a new L2 domain */
			if (!found && l2_domain_count < MAX_CORES_PER_NODE) {
				cpumask_clear(&l2_domain_masks[l2_domain_count]);
				cpumask_set_cpu(cpu, &l2_domain_masks[l2_domain_count]);
				l2_domain_count++;
			}
		}
	}
}

/* Improved cache-aware IRQ distribution algorithm */
static struct cpumask *group_cpus_hybrid_first(unsigned int num_grps)
{
	const struct cpumask *p_core_mask;
	struct cpumask *result = NULL;
	struct cpumask e_cores_mask;
	DECLARE_BITMAP(assigned, NR_CPUS);
	int i, j, cpu, grp_idx = 0;

	if (!num_grps)
		return NULL;

	if (!irq_pcore_affinity || !hybrid_cpu_detected())
		return group_cpus_evenly(num_grps);

	/* Get P-cores - our algorithm focuses on these for IRQs */
	p_core_mask = cpu_pcore_mask();
	if (!p_core_mask || cpumask_empty(p_core_mask))
		return group_cpus_evenly(num_grps);

	/* Create result masks */
	result = kcalloc(num_grps, sizeof(struct cpumask), GFP_KERNEL);
	if (!result) {
		if (p_core_mask != &pcore_mask)
			kfree((void *)p_core_mask);
		return group_cpus_evenly(num_grps);
	}

	/* Clear all result masks */
	for (i = 0; i < num_grps; i++)
		cpumask_clear(&result[i]);

	/* Identify E-cores for later use */
	bitmap_zero(assigned, NR_CPUS);
	cpumask_andnot(&e_cores_mask, cpu_online_mask, p_core_mask);

	/* Identify L2 domains if not already done */
	identify_l2_domains(p_core_mask);

	/* If L2 domain identification failed, fall back to simple distribution */
	if (!l2_domain_masks || l2_domain_count == 0) {
		int cores = cpumask_weight(p_core_mask);
		int cores_per_group = cores / num_grps;
		int extra = cores % num_grps;

		for (i = 0; i < num_grps; i++) {
			int count = 0;
			int cores_this_group = cores_per_group + (i < extra ? 1 : 0);

			for_each_cpu(cpu, p_core_mask) {
				if (!test_bit(cpu, assigned) && count < cores_this_group) {
					cpumask_set_cpu(cpu, &result[i]);
					set_bit(cpu, assigned);
					count++;
				}
			}
		}
	} else {
		/* Distribute cores with cache awareness using proportional allocation */
		int total_cores = 0;
		for (i = 0; i < l2_domain_count; i++)
			total_cores += cpumask_weight(&l2_domain_masks[i]);

		/* Distribute each L2 domain proportionally */
		for (i = 0; i < l2_domain_count && grp_idx < num_grps; i++) {
			int domain_cores = cpumask_weight(&l2_domain_masks[i]);
			if (domain_cores == 0)
				continue;

			/* Calculate groups for this domain proportional to its size */
			int grps_for_domain;

			if (total_cores > 0) {
				grps_for_domain = (num_grps * domain_cores + total_cores - 1) / total_cores;
				grps_for_domain = min_t(int, grps_for_domain, num_grps - grp_idx);
			} else {
				grps_for_domain = 1;
			}

			grps_for_domain = max(1, grps_for_domain);

			/* Calculate cores per group */
			int cores_per_domain_group = domain_cores / grps_for_domain;
			int domain_extra = domain_cores % grps_for_domain;

			/* Distribute cores from this domain to groups */
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

	/* Handle remaining groups with E-cores */
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

	/* Final validation and rebalancing */
	for (i = 0; i < num_grps; i++) {
		if (cpumask_empty(&result[i])) {
			/* Find a CPU from a group with more than one CPU */
			int donor_cpu = -1;
			int donor_group = -1;

			for (j = 0; j < num_grps; j++) {
				if (cpumask_weight(&result[j]) > 1) {
					donor_group = j;
					break;
				}
			}

			if (donor_group >= 0) {
				/* Take the first CPU from the donor group */
				for_each_cpu(cpu, &result[donor_group]) {
					donor_cpu = cpu;
					break;
				}

				if (donor_cpu >= 0) {
					cpumask_clear_cpu(donor_cpu, &result[donor_group]);
					cpumask_set_cpu(donor_cpu, &result[i]);
				}
			} else {
				/* If no group has multiple CPUs, fall back to standard distribution */
				if (p_core_mask != &pcore_mask)
					kfree((void *)p_core_mask);
				kfree(result);
				return group_cpus_evenly(num_grps);
			}
		}
	}

	/* Clean up */
	if (p_core_mask != &pcore_mask)
		kfree((void *)p_core_mask);

	return result;
}

/* CPU hotplug notification handler */
static int pcore_cpu_notify(unsigned int cpu)
{
	atomic_set_release(&pcore_mask_initialized, 0);
	numa_node_for_cpu[cpu] = cpu_to_node(cpu);

	/* Force L2 domain recalculation on next use */
	if (l2_domain_masks) {
		kfree(l2_domain_masks);
		l2_domain_masks = NULL;
		l2_domain_count = 0;
	}

	return 0;
}

/* Improved initialization with proper hotplug registration */
static int __init hybrid_irq_tuning(void)
{
	int ret = 0;

	if (!hybrid_cpu_detected() || !irq_pcore_affinity)
		return 0;

	/* Initialize NUMA node mapping */
	int cpu;
	for_each_possible_cpu(cpu) {
		numa_node_for_cpu[cpu] = cpu_to_node(cpu);
	}

	/* Register for CPU hotplug notifications */
	ret = cpuhp_setup_state(CPUHP_AP_ONLINE_DYN, "irq/pcore_affinity:online",
							pcore_cpu_notify, pcore_cpu_notify);
	if (ret < 0)
		return ret;

	/* Apply P-core affinity if enabled */
	const struct cpumask *pcpu_mask = cpu_pcore_mask();
	if (pcpu_mask && !cpumask_empty(pcpu_mask))
		cpumask_copy(irq_default_affinity, pcpu_mask);

	if (pcpu_mask != &pcore_mask)
		kfree((void *)pcpu_mask);

	return 0;
}
core_initcall(hybrid_irq_tuning);
#endif /* CONFIG_X86 */

/* Preserve original algorithm with safety checks */
static void default_calc_sets(struct irq_affinity *affd, unsigned int affvecs)
{
	if (!affd)
		return;

	affd->nr_sets = 1;
	affd->set_size[0] = affvecs;
}

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
			const struct cpumask *pcpu_mask = cpu_pcore_mask();
			if (pcpu_mask && !cpumask_empty(pcpu_mask)) {
				set_vecs = cpumask_weight(pcpu_mask);
				if (pcpu_mask != &pcore_mask)
					kfree((void *)pcpu_mask);
			} else {
				set_vecs = cpumask_weight(cpu_online_mask);
				if (pcpu_mask != &pcore_mask)
					kfree((void *)pcpu_mask);
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
