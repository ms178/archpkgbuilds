/*
 * Copyright 2008 Advanced Micro Devices, Inc.
 * Copyright 2008 Red Hat Inc.
 * Copyright 2009 Jerome Glisse.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER(S) OR AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors: Dave Airlie
 *          Alex Deucher
 *          Jerome Glisse
 */
#include <linux/ktime.h>
#include <linux/module.h>
#include <linux/pagemap.h>
#include <linux/pci.h>
#include <linux/dma-buf.h>

#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_exec.h>
#include <drm/drm_gem_ttm_helper.h>
#include <drm/ttm/ttm_tt.h>

#include "amdgpu.h"
#include "amdgpu_display.h"
#include "amdgpu_dma_buf.h"
#include "amdgpu_hmm.h"
#include "amdgpu_xgmi.h"
#include "amdgpu_vm.h" // Included in orig, needed for some Vega functions

/* Define constants for Vega memory management */
#define AMDGPU_VEGA_HBM2_BANK_SIZE (1ULL * 1024 * 1024) /* 1MB HBM2 bank size */
#define AMDGPU_VEGA_SMALL_BUFFER_SIZE (1ULL * 1024 * 1024) /* 1MB */
#define AMDGPU_VEGA_MEDIUM_BUFFER_SIZE (4ULL * 1024 * 1024) /* 4MB */
#define AMDGPU_VEGA_LARGE_BUFFER_SIZE (16ULL * 1024 * 1024) /* 16MB */

/* Module parameters for tunable thresholds */
static int amdgpu_vega_vram_pressure_low = 65;
static int amdgpu_vega_vram_pressure_mid = 75;
static int amdgpu_vega_vram_pressure_high = 85;

module_param_named(vram_pressure_low, amdgpu_vega_vram_pressure_low, int, 0644);
MODULE_PARM_DESC(vram_pressure_low, "Low VRAM pressure threshold for Vega (65)");
module_param_named(vram_pressure_mid, amdgpu_vega_vram_pressure_mid, int, 0644);
MODULE_PARM_DESC(vram_pressure_mid, "Medium VRAM pressure threshold for Vega (75)");
module_param_named(vram_pressure_high, amdgpu_vega_vram_pressure_high, int, 0644);
MODULE_PARM_DESC(vram_pressure_high, "High VRAM pressure threshold for Vega (85)");

/**
 * amdgpu_vega_get_vram_usage - Get current VRAM usage percentage
 * @adev: AMDGPU device
 *
 * Returns the current VRAM usage as a percentage (0-100)
 */
static uint32_t amdgpu_vega_get_vram_usage(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *vram_man;
	uint64_t vram_usage = 0;
	uint64_t vram_size = 0;
	uint32_t usage_percent = 0;

	if (!adev || !adev->gmc.mc_vram_size)
		return 0;

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (!vram_man)
		return 0;

	vram_usage = ttm_resource_manager_usage(vram_man);
	vram_size = adev->gmc.mc_vram_size;

	if (vram_size)
		usage_percent = div64_u64(vram_usage * 100, vram_size);

	return usage_percent;
}

/**
 * amdgpu_vega_get_effective_vram_usage - Get enhanced VRAM usage metrics
 * @adev: AMDGPU device
 *
 * Returns an enhanced VRAM usage value (0-100) that incorporates TTM memory
 * management state to detect actual memory pressure for Vega GPUs.
 */
static uint32_t amdgpu_vega_get_effective_vram_usage(struct amdgpu_device *adev)
{
	uint32_t usage_percent;
	uint32_t effective_percent;
	struct ttm_resource_manager *vram_man;

	if (!adev)
		return 0;

	/* Get basic VRAM usage percentage using our helper function */
	usage_percent = amdgpu_vega_get_vram_usage(adev);
	effective_percent = usage_percent;

	/* Only apply enhancements for Vega GPUs */
	if (adev->asic_type != CHIP_VEGA10)
		return usage_percent;

	/* Get TTM resource manager for additional pressure checks */
	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (!vram_man)
		return usage_percent;

	/*
	 * If TTM is using system memory as fallback (use_tt flag),
	 * it indicates memory pressure - add a larger margin
	 */
	if (vram_man->use_tt) {
		effective_percent = min_t(uint32_t, usage_percent + 10, 100);
	}
	/* Otherwise add a small margin for translation layer overhead */
	else if (usage_percent > amdgpu_vega_vram_pressure_mid) {
		effective_percent = min_t(uint32_t, usage_percent + 5, 100);
	}

	return effective_percent;
}

/**
 * amdgpu_vega_optimize_buffer_placement - Optimize buffer placement for Vega GPUs
 * @adev: AMDGPU device
 * @bo: Buffer object (can be NULL for new buffer creation)
 * @size: Buffer size in bytes
 * @flags: Buffer creation flags
 * @domain: Pointer to domain flags to be modified
 *
 * Optimizes buffer placement for Vega GPUs based on workload classification.
 * Specifically tuned for DX12/Vulkan native and DXVK translation performance.
 *
 * Returns: true if placement was optimized, false otherwise
 */
static bool amdgpu_vega_optimize_buffer_placement(struct amdgpu_device *adev,
												  struct amdgpu_bo *bo,
												  uint64_t size,
												  uint64_t flags,
												  uint32_t *domain)
{
	uint32_t vram_usage;

	/* Validate inputs */
	if (!adev || adev->asic_type != CHIP_VEGA10 || !domain)
		return false;

	/* Get current VRAM usage percentage */
	vram_usage = amdgpu_vega_get_effective_vram_usage(adev);

	/* Validate module parameters to ensure they're in valid range */
	if (amdgpu_vega_vram_pressure_low > 100 || amdgpu_vega_vram_pressure_mid > 100 ||
		amdgpu_vega_vram_pressure_high > 100) {
		/* Use safe defaults if invalid */
		vram_usage = min_t(uint32_t, vram_usage, 85);
		}

		/*
		 * Category 1: Textures and framebuffer resources (critical for gaming)
		 * For DX12/Vulkan games, textures are performance-critical and benefit
		 * greatly from HBM2 bandwidth
		 */
		if (flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) {
			if (vram_usage >= amdgpu_vega_vram_pressure_high &&
				size < AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
				/* Small textures can use GTT under extreme pressure */
				*domain = (*domain & ~AMDGPU_GEM_DOMAIN_VRAM) | AMDGPU_GEM_DOMAIN_GTT;
				} else {
					/* Prioritize VRAM for textures - critical for game performance */
					*domain |= AMDGPU_GEM_DOMAIN_VRAM;
				}
				return true;
		}

		/*
		 * Category 2: Compute resources (shader storage, image buffers)
		 * DX12/Vulkan games heavily use compute shaders that need maximum bandwidth
		 */
		if (flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) {
			if (vram_usage >= amdgpu_vega_vram_pressure_high &&
				size > AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
				/* Very large compute buffers can use GTT under extreme pressure */
				*domain |= AMDGPU_GEM_DOMAIN_GTT;
				} else {
					/* Keep compute resources in VRAM for best performance */
					*domain |= AMDGPU_GEM_DOMAIN_VRAM;
				}
				return true;
		}

		/*
		 * Category 3: Small buffers with CPU access (DXVK translation buffers)
		 * DXVK generates many small, CPU-accessible buffers for API translation
		 */
		if (size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE &&
			(flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)) {
			if (vram_usage >= amdgpu_vega_vram_pressure_mid) {
				/* Keep translation buffers in GTT under medium+ pressure */
				*domain = (*domain & ~AMDGPU_GEM_DOMAIN_VRAM) | AMDGPU_GEM_DOMAIN_GTT;
			} else if (*domain == 0) {
				/* Under low pressure, still prefer GTT for better CPU access */
				*domain |= AMDGPU_GEM_DOMAIN_GTT;
			}
			return true;
			}

			/*
			 * Category 4: Medium buffers (mixed usage)
			 * Often used for dynamic resources or intermediate results
			 */
			if (size > AMDGPU_VEGA_SMALL_BUFFER_SIZE &&
				size <= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
				if (vram_usage >= amdgpu_vega_vram_pressure_mid) {
					/* Use GTT under pressure */
					*domain = (*domain & ~AMDGPU_GEM_DOMAIN_VRAM) | AMDGPU_GEM_DOMAIN_GTT;
				} else if (*domain == 0) {
					/* Prefer VRAM for better GPU access */
					*domain |= AMDGPU_GEM_DOMAIN_VRAM;
				}
				return true;
				}

				/*
				 * Category 5: Large buffers
				 * Typically used for large geometry, large textures, or large compute data
				 */
				if (size > AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
					/* For DX12/Vulkan, some large buffers are critical for performance */
					if ((flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) &&
						vram_usage < amdgpu_vega_vram_pressure_mid) {
						/* Large shader buffers benefit from VRAM when pressure isn't high */
						*domain |= AMDGPU_GEM_DOMAIN_VRAM;
						} else if (vram_usage >= amdgpu_vega_vram_pressure_low &&
							!(*domain & AMDGPU_GEM_DOMAIN_VRAM)) {
							/* Otherwise use GTT for large buffers unless explicitly VRAM */
							*domain |= AMDGPU_GEM_DOMAIN_GTT;
							} else if (*domain == 0) {
								/* Default large allocations to GTT unless otherwise specified */
								*domain |= AMDGPU_GEM_DOMAIN_GTT;
							}
							return true;
				}

				/* No specific optimization applied */
				return false;
}

/**
 * amdgpu_vega_optimize_hbm2_bank_access - Optimize HBM2 bank distribution
 * @adev: AMDGPU device
 * @bo: Buffer object (can be NULL during buffer creation)
 * @aligned_size: Pointer to the size to be potentially adjusted
 * @alignment: Pointer to alignment to be potentially adjusted
 *
 * Optimize HBM2 bank access patterns for Vega by adjusting buffer alignment
 * and size to minimize bank conflicts. Critical for maximizing memory bandwidth.
 *
 * Returns: true if optimization was applied, false otherwise
 */
static bool amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
												  struct amdgpu_bo *bo,
												  uint64_t *aligned_size,
												  uint32_t *alignment)
{
	/* Validate input parameters */
	if (!adev || adev->asic_type != CHIP_VEGA10 || !aligned_size || !alignment)
		return false;

	/* Check if size is reasonable to avoid integer overflow */
	if (*aligned_size == 0 || *aligned_size > (16ULL * 1024 * 1024 * 1024))
		return false;

	/* For texture buffers, align to minimize HBM2 bank conflicts */
	if (bo && bo->tbo.base.size > 0 && (bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS)) {
		/* For larger textures, 4KB alignment helps with HBM2 efficiency */
		if (*aligned_size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
			*alignment = max_t(uint32_t, *alignment, 4096);
			*aligned_size = ALIGN(*aligned_size, 4096);
			return true;
		}
	}

	/* For compute buffers, align to optimize HBM2 throughput */
	if (bo && bo->tbo.base.size > 0 && (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
		/* 8KB alignment for compute helps with efficient access patterns */
		if (*aligned_size >= AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
			*alignment = max_t(uint32_t, *alignment, 8192);
			*aligned_size = ALIGN(*aligned_size, 8192);
			return true;
		}
	}

	/* When bo is NULL (during creation), use flags directly */
	if (!bo && *aligned_size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
		/* Use 4KB alignment as a reasonable default for medium/large buffers */
		*alignment = max_t(uint32_t, *alignment, 4096);
		*aligned_size = ALIGN(*aligned_size, 4096);
		return true;
	}

	return false;
}

/**
 * amdgpu_vega_determine_optimal_prefetch - Calculate optimal prefetch size
 * @adev: AMDGPU device
 * @bo: Buffer object
 * @base_prefetch_pages: Base number of pages to prefetch
 * @vram_usage: Current VRAM usage percentage
 *
 * Dynamically adjust prefetch size based on buffer characteristics and current
 * VRAM usage for optimized HBM2 access patterns.
 *
 * Returns: Optimized number of pages to prefetch
 */
static unsigned int amdgpu_vega_determine_optimal_prefetch(
	struct amdgpu_device *adev,
	struct amdgpu_bo *bo,
	unsigned int base_prefetch_pages,
	uint32_t vram_usage)
{
	unsigned int prefetch_pages = base_prefetch_pages;
	uint64_t size;
	bool is_vram;

	/* Validate input parameters */
	if (!adev || !bo || adev->asic_type != CHIP_VEGA10)
		return base_prefetch_pages;

	/* Get buffer size and domain safely */
	size = amdgpu_bo_size(bo);
	if (size == 0)
		return base_prefetch_pages;

	is_vram = (bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) != 0;

	/* Under high memory pressure, reduce prefetch to avoid thrashing */
	if (vram_usage > amdgpu_vega_vram_pressure_high) {
		return max_t(unsigned int, base_prefetch_pages / 2, 8);
	}

	/* Optimize prefetch for shader storage (SSBO) and image loads */
	if (is_vram && (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
		/* Large compute buffers benefit from aggressive prefetching */
		if (size > AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
			return min_t(unsigned int, base_prefetch_pages * 2, 128);
		}
	}

	/* Optimize texture fetch patterns */
	if (is_vram && (bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS)) {
		/* Textures benefit from larger prefetch except under pressure */
		if (vram_usage < amdgpu_vega_vram_pressure_mid) {
			return min_t(unsigned int, base_prefetch_pages * 6 / 5, 64);
		}
	}

	return prefetch_pages;
}

/**
 * amdgpu_vega_should_use_async_fence - Determine if async fencing is appropriate
 * @adev: AMDGPU device
 * @bo: Buffer object
 * @flags: Buffer flags
 *
 * Determines if a buffer object should use asynchronous fencing based on
 * its characteristics and usage patterns. Enhances performance by reducing
 * unnecessary synchronization.
 *
 * Returns: true if async fencing is safe to use
 */
static bool amdgpu_vega_should_use_async_fence(struct amdgpu_device *adev,
											   struct amdgpu_bo *bo,
											   uint64_t flags)
{
	uint64_t size;

	/* Validate input parameters */
	if (!adev || !bo || adev->asic_type != CHIP_VEGA10)
		return false;

	/* Get buffer size safely */
	size = amdgpu_bo_size(bo);
	if (size == 0)
		return false;

	/* Never use async fencing for buffers that explicitly need sync */
	if (flags & AMDGPU_GEM_CREATE_EXPLICIT_SYNC)
		return false;

	/* For small GTT buffers with CPU access, async fencing is usually safe */
	if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_GTT) &&
		(flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) &&
		size < AMDGPU_VEGA_SMALL_BUFFER_SIZE) {
		return true;
		}

		/* For shader buffers that don't need CPU access, async is safe */
		if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) &&
			(flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) &&
			!(flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)) {
			return true;
			}

			/* Conservative default - use synchronous fencing */
			return false;
}

/**
 * amdgpu_vega_set_compute_placement - Set optimal placement for compute buffers
 * @adev: AMDGPU device
 * @bo: Buffer object
 * @size: Buffer size
 * @domain: Pointer to preferred domain
 *
 * Optimizes the placement of compute buffers for Vega GPUs.
 *
 * Note: Caller must hold the BO reservation lock.
 *
 * Returns true if optimization was applied.
 */
static bool amdgpu_vega_set_compute_placement(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t size,
											  uint32_t *domain)
{
	uint32_t vram_usage;

	/* Validate inputs */
	if (!adev || !bo || !domain || adev->asic_type != CHIP_VEGA10)
		return false;

	/* Ensure the caller holds the BO reservation lock */
	lockdep_assert_held(bo->tbo.base.resv);

	/* Only apply to compute buffers */
	if (!(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS))
		return false;

	vram_usage = amdgpu_vega_get_effective_vram_usage(adev);

	/* For large compute buffers, placement depends on memory pressure */
	if (size > AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
		/* Under low pressure, use VRAM for large compute buffers */
		if (vram_usage < amdgpu_vega_vram_pressure_low) {
			*domain = AMDGPU_GEM_DOMAIN_VRAM;
			/* Add GTT as a fallback domain */
			bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
			return true;
		}
	}

	/* For medium compute buffers, use VRAM until medium pressure */
	if (size > AMDGPU_VEGA_MEDIUM_BUFFER_SIZE &&
		size <= AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
		if (vram_usage < amdgpu_vega_vram_pressure_mid) {
			*domain = AMDGPU_GEM_DOMAIN_VRAM;
			/* Add GTT as a fallback domain */
			bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
			return true;
		}
		}

		/* For small compute buffers, always try VRAM first - critical for performance */
		if (size <= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
			*domain = AMDGPU_GEM_DOMAIN_VRAM;
			/* Add GTT as a fallback domain */
			bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
			return true;
		}

		return false;
}

/**
 * amdgpu_vega_set_buffer_domains - Update buffer domains for optimal performance
 * @adev: AMDGPU device
 * @bo: Buffer object to update
 *
 * Ensures buffers have appropriate fallback domains and optimal settings
 * for Vega's HBM2 memory. Specifically tuned for gaming workloads.
 *
 * Note: Caller must hold the BO reservation lock.
 */
static void amdgpu_vega_set_buffer_domains(struct amdgpu_device *adev,
										   struct amdgpu_bo *bo)
{
	/* Validate inputs */
	if (!adev || adev->asic_type != CHIP_VEGA10 || !bo)
		return;

	/* Ensure the caller holds the BO reservation lock */
	lockdep_assert_held(bo->tbo.base.resv);

	/*
	 * Always ensure VRAM-only buffers have GTT as fallback
	 * This prevents allocation failures under memory pressure
	 */
	if (bo->preferred_domains == AMDGPU_GEM_DOMAIN_VRAM) {
		bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;
	}

	/*
	 * For compute buffers (shader storage, compute data):
	 * These are critical for DX12/Vulkan game performance
	 */
	if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) {
		/* Only add VRAM preference if allowed in the domain mask */
		if (bo->allowed_domains & AMDGPU_GEM_DOMAIN_VRAM) {
			bo->preferred_domains |= AMDGPU_GEM_DOMAIN_VRAM;
		}

		/* Always ensure GTT is available as fallback */
		bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;
	}

	/*
	 * For CPU-accessible buffers (common in DXVK translation):
	 * These need efficient CPU access via GTT
	 */
	if (bo->flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) {
		bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;
	}
}


/**
 * amdgpu_vega_optimize_for_workload - Apply workload-specific optimizations
 * @adev: AMDGPU device
 * @bo: Buffer object
 * @flags: Buffer creation flags
 *
 * Apply specific optimizations based on likely workload patterns detected
 * from buffer flags and properties. Especially effective for gaming,
 * compute, and content creation workloads.
 *
 * Note: Caller must hold the BO reservation lock before calling this function.
 *
 * Returns: true if optimizations were applied
 */
static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags)
{
	uint64_t size;

	/* Validate inputs */
	if (!adev || !bo || adev->asic_type != CHIP_VEGA10)
		return false;

	/* Ensure the BO is valid */
	if (!bo->tbo.base.dev)
		return false;

	/* Get buffer size safely */
	size = amdgpu_bo_size(bo);
	if (size == 0)
		return false;

	/* Caller must hold the reservation lock */
	if (!dma_resv_is_locked(bo->tbo.base.resv))
		return false;

	/* Gaming workload optimization - prioritize texture bandwidth */
	if ((flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) &&
		size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
		/* Set VRAM domain but with GTT as fallback */
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
	bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
	return true;
		}

		/* Compute workload optimization */
		if ((flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) &&
			!(flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)) {
			/* Prioritize VRAM for compute buffers */
			bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
		return true;
			}

			/* API translation layer optimization (e.g., DXVK, VKD3D) */
			if ((flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) &&
				size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE) {
				/* Small CPU-accessible buffers work better in GTT for translation layers */
				bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
			bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT | AMDGPU_GEM_DOMAIN_VRAM;
			return true;
				}

				return false;
}

static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo;
	struct drm_device *ddev;
	vm_fault_t ret;
	int idx;

	// Start of original code section
	bo = vmf->vma->vm_private_data;
	if (!bo) // Added safety check from source
		return VM_FAULT_SIGBUS;

	ddev = bo->base.dev;
	if (!ddev) // Added safety check from source
		return VM_FAULT_SIGBUS;
	// End of original code section (with safety checks added)

	ret = ttm_bo_vm_reserve(bo, vmf);
	if (ret)
		return ret;

	if (drm_dev_enter(ddev, &idx)) {
		// Start of original code section
		struct amdgpu_device *adev = drm_to_adev(ddev); // Moved up for Vega check
		// End of original code section

		ret = amdgpu_bo_fault_reserve_notify(bo);
		if (ret) {
			drm_dev_exit(idx);
			goto unlock;
		}

		/* Vega-specific prefetch optimizations for HBM2 memory */
		if (adev && adev->asic_type == CHIP_VEGA10) {
			struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
			unsigned int prefetch_pages = TTM_BO_VM_NUM_PREFAULT;

			if (abo) {
				/* Get current VRAM usage for context-aware prefetch */
				uint32_t vram_usage = amdgpu_vega_get_effective_vram_usage(adev);

				/* Calculate optimal prefetch size based on buffer characteristics */
				prefetch_pages = amdgpu_vega_determine_optimal_prefetch(
					adev, abo, TTM_BO_VM_NUM_PREFAULT, vram_usage);
			}

			ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot, prefetch_pages);
		} else {
			/* Standard fault handling for other GPUs */
			ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
										   TTM_BO_VM_NUM_PREFAULT);
		}

		drm_dev_exit(idx);
	} else {
		ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
	}
	if (ret == VM_FAULT_RETRY && !(vmf->flags & FAULT_FLAG_RETRY_NOWAIT))
		return ret;

	unlock:
	dma_resv_unlock(bo->base.resv);
	return ret;
}

static const struct vm_operations_struct amdgpu_gem_vm_ops = {
	.fault = amdgpu_gem_fault,
	.open = ttm_bo_vm_open,
	.close = ttm_bo_vm_close,
	.access = ttm_bo_vm_access
};

static void amdgpu_gem_object_free(struct drm_gem_object *gobj)
{
	struct amdgpu_bo *aobj = gem_to_amdgpu_bo(gobj);

	if (aobj) { // Added check from source
		amdgpu_hmm_unregister(aobj);
		ttm_bo_put(&aobj->tbo);
	}
}

int amdgpu_gem_object_create(struct amdgpu_device *adev, unsigned long size,
							 int alignment, u32 initial_domain,
							 u64 flags, enum ttm_bo_type type,
							 struct dma_resv *resv,
							 struct drm_gem_object **obj, int8_t xcp_id_plus1)
{
	struct amdgpu_bo *bo;
	struct amdgpu_bo_user *ubo;
	struct amdgpu_bo_param bp;
	int r;

	if (!adev || !obj) // Added check from source
		return -EINVAL;

	memset(&bp, 0, sizeof(bp));
	*obj = NULL;
	flags |= AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;

	/* Apply Vega-specific optimizations for buffer placement */
	if (adev->asic_type == CHIP_VEGA10) {
		/* Optimize domain for new buffer based on size, flags, and VRAM usage */
		/* Note: size is unsigned long here, optimize_buffer_placement expects uint64_t */
		/* Implicit conversion from unsigned long to uint64_t is usually safe */
		amdgpu_vega_optimize_buffer_placement(adev, NULL, (uint64_t)size, flags, &initial_domain);
	}

	bp.size = size; // bp.size is unsigned long
	bp.byte_align = alignment;

	/* Apply Vega-specific optimizations for HBM2 bank access */
	if (adev->asic_type == CHIP_VEGA10) {
		/* Introduce temporary uint64_t to match helper function signature */
		uint64_t temp_size = (uint64_t)bp.size;
		uint32_t temp_align = bp.byte_align; /* Use temp for alignment too for symmetry */

		/* Optimize buffer alignment and size for HBM2 memory */
		if (amdgpu_vega_optimize_hbm2_bank_access(adev, NULL, &temp_size, &temp_align)) {
			/* Copy potentially modified values back */
			/* Careful about potential truncation if unsigned long is 32b and temp_size > 32b max */
			/* However, bp.size originates from 'size' (unsigned long), so if temp_size */
			/* grew beyond ULONG_MAX, it implies an issue earlier or requires bp.size */
			/* to also be uint64_t. Assuming 64-bit kernel or size doesn't overflow ULONG_MAX. */
			bp.size = (unsigned long)temp_size;
			bp.byte_align = temp_align;
		}
	}


	bp.type = type;
	bp.resv = resv;
	bp.preferred_domain = initial_domain;
	bp.flags = flags;
	bp.domain = initial_domain;
	bp.bo_ptr_size = sizeof(struct amdgpu_bo);
	bp.xcp_id_plus1 = xcp_id_plus1;

	r = amdgpu_bo_create_user(adev, &bp, &ubo);
	if (r)
		return r;

	bo = &ubo->bo;

	/* Apply additional Vega-specific domain optimizations post-creation */
	if (adev->asic_type == CHIP_VEGA10) {
		/* For compute buffers, apply specialized optimization */
		if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) {
			/* amdgpu_vega_set_compute_placement expects uint64_t size */
			/* BO needs reservation lock here, which is held during creation */
			amdgpu_vega_set_compute_placement(adev, bo, (uint64_t)size, &bo->preferred_domains);
		}

		/* Apply workload-specific optimizations - bo is already reserved during creation */
		amdgpu_vega_optimize_for_workload(adev, bo, flags);

		/* Apply general domain optimizations - needs lock */
		amdgpu_vega_set_buffer_domains(adev, bo);
	}

	*obj = &bo->tbo.base;

	return 0;
}

void amdgpu_gem_force_release(struct amdgpu_device *adev)
{
	struct drm_device *ddev = adev_to_drm(adev);
	struct drm_file *file;

	mutex_lock(&ddev->filelist_mutex);

	list_for_each_entry(file, &ddev->filelist, lhead) {
		struct drm_gem_object *gobj;
		int handle;

		WARN_ONCE(1, "Still active user space clients!\n");
		spin_lock(&file->table_lock);
		idr_for_each_entry(&file->object_idr, gobj, handle) {
			WARN_ONCE(1, "And also active allocations!\n");
			drm_gem_object_put(gobj);
		}
		idr_destroy(&file->object_idr);
		spin_unlock(&file->table_lock);
	}

	mutex_unlock(&ddev->filelist_mutex);
}

/*
 * Call from drm_gem_handle_create which appear in both new and open ioctl
 * case.
 */
static int amdgpu_gem_object_open(struct drm_gem_object *obj,
								  struct drm_file *file_priv)
{
	struct amdgpu_bo *abo;
	struct amdgpu_device *adev;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_vm *vm;
	struct amdgpu_bo_va *bo_va;
	struct mm_struct *mm;
	int r;

	// Start of original code section (with source checks added)
	if (!obj || !file_priv) // Added check from source
		return -EINVAL;

	abo = gem_to_amdgpu_bo(obj);
	if (!abo) // Added check from source
		return -EINVAL;

	adev = amdgpu_ttm_adev(abo->tbo.bdev);
	if (!adev) // Added check from source
		return -EINVAL;

	fpriv = file_priv->driver_priv;
	if (!fpriv) // Added check from source
		return -EINVAL;

	vm = &fpriv->vm;
	// End of original code section (with source checks added)

	mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
	if (mm && mm != current->mm)
		return -EPERM;

	/* For BOs with ALWAYS_VALID flag, ensure that the VM mapping has been pre-validated */
	// Merged check from source
	if ((abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
		!amdgpu_vm_is_bo_always_valid(vm, abo))
		return -EPERM;

	r = amdgpu_bo_reserve(abo, false);
	if (r)
		return r;

	// Merged amdgpu_vm_bo_update_shared call from orig into find/add logic
	bo_va = amdgpu_vm_bo_find(vm, abo);
	if (!bo_va) {
		amdgpu_vm_bo_update_shared(abo); // From orig, call before add
		bo_va = amdgpu_vm_bo_add(adev, vm, abo);
	} else {
		amdgpu_vm_bo_update_shared(abo); // From orig, call before increment
		++bo_va->ref_count;
	}
	amdgpu_bo_unreserve(abo);

	/* Validate and add eviction fence to DMABuf imports with dynamic
	 * attachment in compute VMs. Re-validation will be done by
	 * amdgpu_vm_validate. Fences are on the reservation shared with the
	 * export, which is currently required to be validated and fenced
	 * already by amdgpu_amdkfd_gpuvm_restore_process_bos.
	 *
	 * Nested locking below for the case that a GEM object is opened in
	 * kfd_mem_export_dmabuf. Since the lock below is only taken for imports,
	 * but not for export, this is a different lock class that cannot lead to
	 * circular lock dependencies.
	 */
	if (!vm->is_compute_context || !vm->process_info)
		return 0;
	if (!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf))
		return 0;

	/* Lock process_info to run eviction fence validation */
	mutex_lock_nested(&vm->process_info->lock, 1);
	lockdep_assert_held(&vm->process_info->lock); // Added from source

	if (!WARN_ON(!vm->process_info->eviction_fence)) {
		/* For Vega GPUs, use domain-optimized validation for imported buffers */
		if (adev->asic_type == CHIP_VEGA10) {
			uint32_t domain = AMDGPU_GEM_DOMAIN_GTT;

			/* Apply HBM2-specific optimizations based on buffer properties */
			if (abo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS ||
				abo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) {
				/* Textures and compute buffers benefit from VRAM */
				domain = AMDGPU_GEM_DOMAIN_VRAM;
			/* Check VRAM pressure and fall back if needed */
			if (amdgpu_vega_get_effective_vram_usage(adev) > amdgpu_vega_vram_pressure_high) {
				domain = AMDGPU_GEM_DOMAIN_GTT;
			}
				}
				r = amdgpu_amdkfd_bo_validate_and_fence(abo, domain,
														&vm->process_info->eviction_fence->base);
		} else {
			/* Original non-Vega path */
			r = amdgpu_amdkfd_bo_validate_and_fence(abo,
													AMDGPU_GEM_DOMAIN_GTT,
										   &vm->process_info->eviction_fence->base);
		}

		if (r) {
			struct amdgpu_task_info *ti = amdgpu_vm_get_task_info_vm(vm);

			dev_warn(adev->dev, "validate_and_fence failed: %d\n", r);
			if (ti) {
				dev_warn(adev->dev, "pid %d\n", ti->pid);
				amdgpu_vm_put_task_info(ti);
			}
		}
	}
	mutex_unlock(&vm->process_info->lock);

	return r;
}

static void amdgpu_gem_object_close(struct drm_gem_object *obj,
									struct drm_file *file_priv)
{
	struct amdgpu_bo *bo;
	struct amdgpu_device *adev;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_vm *vm;
	struct dma_fence *fence = NULL;
	struct amdgpu_bo_va *bo_va;
	struct drm_exec exec;
	long r = 0; // Initialize r from source
	bool use_async = false; // Default to sync fence

	// Start of original code section (with source checks)
	if (!obj || !file_priv) // Added check from source
		return;

	bo = gem_to_amdgpu_bo(obj);
	if (!bo) // Added check from source
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);
	if (!adev) // Added check from source
		return;

	fpriv = file_priv->driver_priv;
	if (!fpriv) // Added check from source
		return;

	vm = &fpriv->vm;
	// End of original code section (with source checks)

	/* For Vega, determine if we can use async fencing */
	if (adev->asic_type == CHIP_VEGA10) {
		use_async = amdgpu_vega_should_use_async_fence(adev, bo, bo->flags);
	}

	/* Use the DRM exec framework to acquire multi-object locks */
	drm_exec_init(&exec, DRM_EXEC_IGNORE_DUPLICATES, 0);
	drm_exec_until_all_locked(&exec) {
		r = drm_exec_prepare_obj(&exec, &bo->tbo.base, 1);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;

		r = amdgpu_vm_lock_pd(vm, &exec, 0);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;
	}

	bo_va = amdgpu_vm_bo_find(vm, bo);
	// Start of combined logic from source and orig
	if (!bo_va) // Simplified check from source
		goto out_unlock;

	/* Use proper reference counting */
	if (--bo_va->ref_count > 0) // Logic from source
		goto out_unlock;

	/* Remove the BO-VA mapping, as reference count is now zero */
	amdgpu_vm_bo_del(adev, bo_va); // Logic from source
	amdgpu_vm_bo_update_shared(bo); // Added from orig
	// End of combined logic

	if (!amdgpu_vm_ready(vm))
		goto out_unlock;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (unlikely(r < 0)) {
		// Use error message from source
		dev_err(adev->dev, "failed to clear page tables on GEM object close (%ld)\n", r);
		goto out_unlock;
	}
	if (r || !fence)
		goto out_unlock;

	/* Fence the BO - use async for eligible buffers to improve performance */
	amdgpu_bo_fence(bo, fence, use_async); // Use calculated use_async flag
	dma_fence_put(fence);

	out_unlock:
	if (r) {
		// Use error message from source
		dev_err(adev->dev, "Error in GEM object close for pid %d, potential leak of bo_va (%ld)\n",
				task_pid_nr(current), r);
	}
	drm_exec_fini(&exec);
}

static int amdgpu_gem_object_mmap(struct drm_gem_object *obj, struct vm_area_struct *vma)
{
	struct amdgpu_bo *bo = gem_to_amdgpu_bo(obj);

	if (amdgpu_ttm_tt_get_usermm(bo->tbo.ttm))
		return -EPERM;
	if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)
		return -EPERM;

	/* Workaround for Thunk bug creating PROT_NONE,MAP_PRIVATE mappings
	 * for debugger access to invisible VRAM. Should have used MAP_SHARED
	 * instead. Clearing VM_MAYWRITE prevents the mapping from ever
	 * becoming writable and makes is_cow_mapping(vm_flags) false.
	 */
	if (is_cow_mapping(vma->vm_flags) &&
		!(vma->vm_flags & VM_ACCESS_FLAGS))
		vm_flags_clear(vma, VM_MAYWRITE);

	return drm_gem_ttm_mmap(obj, vma);
}

const struct drm_gem_object_funcs amdgpu_gem_object_funcs = {
	.free = amdgpu_gem_object_free,
	.open = amdgpu_gem_object_open,
	.close = amdgpu_gem_object_close,
	.export = amdgpu_gem_prime_export,
	.vmap = drm_gem_ttm_vmap,
	.vunmap = drm_gem_ttm_vunmap,
	.mmap = amdgpu_gem_object_mmap,
	.vm_ops = &amdgpu_gem_vm_ops,
};

/*
 * GEM ioctls.
 */
int amdgpu_gem_create_ioctl(struct drm_device *dev, void *data,
							struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	union drm_amdgpu_gem_create *args = data;
	uint64_t flags = args->in.domain_flags;
	uint64_t size = args->in.bo_size;
	struct dma_resv *resv = NULL;
	struct drm_gem_object *gobj;
	uint32_t handle, initial_domain;
	int r;

	/* reject DOORBELLs until userspace code to use it is available */
	if (args->in.domains & AMDGPU_GEM_DOMAIN_DOORBELL)
		return -EINVAL;

	/* reject invalid gem flags */
	// Flag list updated from orig
	if (flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
		AMDGPU_GEM_CREATE_NO_CPU_ACCESS |
		AMDGPU_GEM_CREATE_CPU_GTT_USWC |
		AMDGPU_GEM_CREATE_VRAM_CLEARED |
		AMDGPU_GEM_CREATE_VM_ALWAYS_VALID |
		AMDGPU_GEM_CREATE_EXPLICIT_SYNC |
		AMDGPU_GEM_CREATE_ENCRYPTED |
		AMDGPU_GEM_CREATE_GFX12_DCC |
		AMDGPU_GEM_CREATE_DISCARDABLE))
		return -EINVAL;

	/* reject invalid gem domains */
	if (args->in.domains & ~AMDGPU_GEM_DOMAIN_MASK)
		return -EINVAL;

	if (!amdgpu_is_tmz(adev) && (flags & AMDGPU_GEM_CREATE_ENCRYPTED)) {
		DRM_NOTE_ONCE("Cannot allocate secure buffer since TMZ is disabled\n");
		return -EINVAL;
	}

	/* always clear VRAM */
	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	/* create a gem object to contain this object in */
	if (args->in.domains & (AMDGPU_GEM_DOMAIN_GDS |
		AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			/* if gds bo is created from user space, it must be
			 * passed to bo list
			 */
			DRM_ERROR("GDS bo cannot be per-vm-bo\n");
			return -EINVAL;
		}
		flags |= AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
		}

		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			r = amdgpu_bo_reserve(vm->root.bo, false);
			if (r)
				return r;

			resv = vm->root.bo->tbo.base.resv;
		}

		initial_domain = (u32)(0xffffffff & args->in.domains);
		retry:
		// Call to amdgpu_gem_object_create now includes Vega optimizations
		r = amdgpu_gem_object_create(adev, size, args->in.alignment,
									 initial_domain,
							   flags, ttm_bo_type_device, resv, &gobj, fpriv->xcp_id + 1);
		if (r && r != -ERESTARTSYS) {
			if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) {
				flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
				goto retry;
			}

			if (initial_domain == AMDGPU_GEM_DOMAIN_VRAM) {
				initial_domain |= AMDGPU_GEM_DOMAIN_GTT;
				goto retry;
			}
			DRM_DEBUG("Failed to allocate GEM object (%llu, %d, %llu, %d)\n",
					  size, initial_domain, args->in.alignment, r);
		}

		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			if (!r) {
				struct amdgpu_bo *abo = gem_to_amdgpu_bo(gobj);

				abo->parent = amdgpu_bo_ref(vm->root.bo);
			}
			amdgpu_bo_unreserve(vm->root.bo);
		}
		if (r)
			return r;

	r = drm_gem_handle_create(filp, gobj, &handle);
	/* drop reference from allocate - handle holds it now */
	drm_gem_object_put(gobj);
	if (r)
		return r;

	memset(args, 0, sizeof(*args));
	args->out.handle = handle;
	return 0;
}

int amdgpu_gem_userptr_ioctl(struct drm_device *dev, void *data,
							 struct drm_file *filp)
{
	struct ttm_operation_ctx ctx = { true, false };
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct drm_amdgpu_gem_userptr *args = data;
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct drm_gem_object *gobj;
	struct hmm_range *range;
	struct amdgpu_bo *bo;
	uint32_t handle;
	int r;

	args->addr = untagged_addr(args->addr);

	if (offset_in_page(args->addr | args->size))
		return -EINVAL;

	/* reject unknown flag values */
	// Flag list from orig
	if (args->flags & ~(AMDGPU_GEM_USERPTR_READONLY |
		AMDGPU_GEM_USERPTR_ANONONLY | AMDGPU_GEM_USERPTR_VALIDATE |
		AMDGPU_GEM_USERPTR_REGISTER))
		return -EINVAL;

	if (!(args->flags & AMDGPU_GEM_USERPTR_READONLY) &&
		!(args->flags & AMDGPU_GEM_USERPTR_REGISTER)) {

		/* if we want to write to it we must install a MMU notifier */
		return -EACCES;
		}

		/* create a gem object to contain this object in */
		// Call to amdgpu_gem_object_create now includes Vega optimizations
		r = amdgpu_gem_object_create(adev, args->size, 0, AMDGPU_GEM_DOMAIN_CPU,
									 0, ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
		if (r)
			return r;

	bo = gem_to_amdgpu_bo(gobj);
	bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
	bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT;
	r = amdgpu_ttm_tt_set_userptr(&bo->tbo, args->addr, args->flags);
	if (r)
		goto release_object;

	r = amdgpu_hmm_register(bo, args->addr);
	if (r)
		goto release_object;

	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE) {
		r = amdgpu_ttm_tt_get_user_pages(bo, bo->tbo.ttm->pages,
										 &range);
		if (r)
			goto release_object;

		r = amdgpu_bo_reserve(bo, true);
		if (r)
			goto user_pages_done;

		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_GTT);
		r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
		amdgpu_bo_unreserve(bo);
		if (r)
			goto user_pages_done;
	}

	r = drm_gem_handle_create(filp, gobj, &handle);
	if (r)
		goto user_pages_done;

	args->handle = handle;

	user_pages_done:
	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE)
		amdgpu_ttm_tt_get_user_pages_done(bo->tbo.ttm, range);

	release_object:
	drm_gem_object_put(gobj);

	return r;
}

int amdgpu_mode_dumb_mmap(struct drm_file *filp,
						  struct drm_device *dev,
						  uint32_t handle, uint64_t *offset_p)
{
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;

	gobj = drm_gem_object_lookup(filp, handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm) ||
		(robj->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
		drm_gem_object_put(gobj);
	return -EPERM;
		}
		*offset_p = amdgpu_bo_mmap_offset(robj);
		drm_gem_object_put(gobj);
		return 0;
}

int amdgpu_gem_mmap_ioctl(struct drm_device *dev, void *data,
						  struct drm_file *filp)
{
	union drm_amdgpu_gem_mmap *args = data;
	uint32_t handle = args->in.handle;

	memset(args, 0, sizeof(*args));
	return amdgpu_mode_dumb_mmap(filp, dev, handle, &args->out.addr_ptr);
}

/**
 * amdgpu_gem_timeout - calculate jiffies timeout from absolute value
 *
 * @timeout_ns: timeout in ns
 *
 * Calculate the timeout in jiffies from an absolute timeout in ns.
 */
unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	unsigned long timeout_jiffies;
	ktime_t timeout;

	/* clamp timeout if it's to large */
	if (((int64_t)timeout_ns) < 0)
		return MAX_SCHEDULE_TIMEOUT;

	timeout = ktime_sub(ns_to_ktime(timeout_ns), ktime_get());
	if (ktime_to_ns(timeout) < 0)
		return 0;

	timeout_jiffies = nsecs_to_jiffies(ktime_to_ns(timeout));
	/*  clamp timeout to avoid unsigned-> signed overflow */
	if (timeout_jiffies > MAX_SCHEDULE_TIMEOUT)
		return MAX_SCHEDULE_TIMEOUT - 1;

	return timeout_jiffies;
}

int amdgpu_gem_wait_idle_ioctl(struct drm_device *dev, void *data,
							   struct drm_file *filp)
{
	union drm_amdgpu_gem_wait_idle *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	uint32_t handle = args->in.handle;
	unsigned long timeout = amdgpu_gem_timeout(args->in.timeout);
	int r = 0;
	long ret;

	gobj = drm_gem_object_lookup(filp, handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	ret = dma_resv_wait_timeout(robj->tbo.base.resv, DMA_RESV_USAGE_READ,
								true, timeout);

	/* ret == 0 means not signaled,
	 * ret > 0 means signaled
	 * ret < 0 means interrupted before timeout
	 */
	if (ret >= 0) {
		memset(args, 0, sizeof(*args));
		args->out.status = (ret == 0);
	} else
		r = ret;

	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_metadata_ioctl(struct drm_device *dev, void *data,
							  struct drm_file *filp)
{
	struct drm_amdgpu_gem_metadata *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	int r = -1;

	DRM_DEBUG("%d\n", args->handle);
	gobj = drm_gem_object_lookup(filp, args->handle);
	if (gobj == NULL)
		return -ENOENT;
	robj = gem_to_amdgpu_bo(gobj);

	r = amdgpu_bo_reserve(robj, false);
	if (unlikely(r != 0))
		goto out;

	if (args->op == AMDGPU_GEM_METADATA_OP_GET_METADATA) {
		amdgpu_bo_get_tiling_flags(robj, &args->data.tiling_info);
		r = amdgpu_bo_get_metadata(robj, args->data.data,
								   sizeof(args->data.data),
								   &args->data.data_size_bytes,
							 &args->data.flags);
	} else if (args->op == AMDGPU_GEM_METADATA_OP_SET_METADATA) {
		if (args->data.data_size_bytes > sizeof(args->data.data)) {
			r = -EINVAL;
			goto unreserve;
		}
		r = amdgpu_bo_set_tiling_flags(robj, args->data.tiling_info);
		if (!r)
			r = amdgpu_bo_set_metadata(robj, args->data.data,
									   args->data.data_size_bytes,
							  args->data.flags);
	}

	unreserve:
	amdgpu_bo_unreserve(robj);
	out:
	drm_gem_object_put(gobj);
	return r;
}

/**
 * amdgpu_gem_va_update_vm -update the bo_va in its VM
 *
 * @adev: amdgpu_device pointer
 * @vm: vm to update
 * @bo_va: bo_va to update
 * @operation: map, unmap or clear
 *
 * Update the bo_va directly after setting its address. Errors are not
 * vital here, so they are not reported back to userspace.
 */
static void amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
									struct amdgpu_vm *vm,
									struct amdgpu_bo_va *bo_va,
									uint32_t operation)
{
	int r;

	if (!amdgpu_vm_ready(vm))
		return;

	r = amdgpu_vm_clear_freed(adev, vm, NULL);
	if (r)
		goto error;

	if (operation == AMDGPU_VA_OP_MAP ||
		operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
	if (r)
		goto error;
		}

		r = amdgpu_vm_update_pdes(adev, vm, false);

	error:
	if (r && r != -ERESTARTSYS)
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);
}

/**
 * amdgpu_gem_va_map_flags - map GEM UAPI flags into hardware flags
 *
 * @adev: amdgpu_device pointer
 * @flags: GEM UAPI flags
 *
 * Returns the GEM UAPI flags mapped into hardware for the ASIC.
 */
uint64_t amdgpu_gem_va_map_flags(struct amdgpu_device *adev, uint32_t flags)
{
	uint64_t pte_flag = 0;

	if (flags & AMDGPU_VM_PAGE_EXECUTABLE)
		pte_flag |= AMDGPU_PTE_EXECUTABLE;
	if (flags & AMDGPU_VM_PAGE_READABLE)
		pte_flag |= AMDGPU_PTE_READABLE;
	if (flags & AMDGPU_VM_PAGE_WRITEABLE)
		pte_flag |= AMDGPU_PTE_WRITEABLE;
	if (flags & AMDGPU_VM_PAGE_PRT)
		pte_flag |= AMDGPU_PTE_PRT_FLAG(adev);
	if (flags & AMDGPU_VM_PAGE_NOALLOC)
		pte_flag |= AMDGPU_PTE_NOALLOC;

	/* Simplified null check - only check the function pointer */
	if (adev->gmc.gmc_funcs && adev->gmc.gmc_funcs->map_mtype) { /* Fixed Line */
		pte_flag |= amdgpu_gmc_map_mtype(adev,
										 flags & AMDGPU_VM_MTYPE_MASK);
	} /* Fixed Line */

	return pte_flag;
}

int amdgpu_gem_va_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
	const uint32_t valid_flags = AMDGPU_VM_DELAY_UPDATE |
	AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE |
	AMDGPU_VM_PAGE_EXECUTABLE | AMDGPU_VM_MTYPE_MASK |
	AMDGPU_VM_PAGE_NOALLOC;
	const uint32_t prt_flags = AMDGPU_VM_DELAY_UPDATE |
	AMDGPU_VM_PAGE_PRT;

	struct drm_amdgpu_gem_va *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_bo *abo;
	struct amdgpu_bo_va *bo_va;
	struct drm_exec exec;
	uint64_t va_flags;
	uint64_t vm_size;
	int r = 0;

	if (args->va_address < AMDGPU_VA_RESERVED_BOTTOM) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in reserved area 0x%llx\n",
		  args->va_address, AMDGPU_VA_RESERVED_BOTTOM);
		return -EINVAL;
	}

	if (args->va_address >= AMDGPU_GMC_HOLE_START &&
		args->va_address < AMDGPU_GMC_HOLE_END) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in VA hole 0x%llx-0x%llx\n",
		  args->va_address, AMDGPU_GMC_HOLE_START,
		  AMDGPU_GMC_HOLE_END);
		return -EINVAL;
		}

		args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size = adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
	vm_size -= AMDGPU_VA_RESERVED_TOP;
	if (args->va_address + args->map_size > vm_size) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in top reserved area 0x%llx\n",
		  args->va_address + args->map_size, vm_size);
		return -EINVAL;
	}

	if ((args->flags & ~valid_flags) && (args->flags & ~prt_flags)) {
		dev_dbg(dev->dev, "invalid flags combination 0x%08X\n",
				args->flags);
		return -EINVAL;
	}

	switch (args->operation) {
		case AMDGPU_VA_OP_MAP:
		case AMDGPU_VA_OP_UNMAP:
		case AMDGPU_VA_OP_CLEAR:
		case AMDGPU_VA_OP_REPLACE:
			break;
		default:
			dev_dbg(dev->dev, "unsupported operation %d\n",
					args->operation);
			return -EINVAL;
	}

	if ((args->operation != AMDGPU_VA_OP_CLEAR) &&
		!(args->flags & AMDGPU_VM_PAGE_PRT)) {
		gobj = drm_gem_object_lookup(filp, args->handle);
	if (gobj == NULL) { /* Fixed Line */
		return -ENOENT;
	} /* Fixed Line */
	abo = gem_to_amdgpu_bo(gobj);
		} else {
			gobj = NULL;
			abo = NULL;
		}

		drm_exec_init(&exec, DRM_EXEC_INTERRUPTIBLE_WAIT |
		DRM_EXEC_IGNORE_DUPLICATES, 0);
		drm_exec_until_all_locked(&exec) {
			if (gobj) {
				r = drm_exec_lock_obj(&exec, gobj);
				drm_exec_retry_on_contention(&exec);
				if (unlikely(r))
					goto error;
			}

			r = amdgpu_vm_lock_pd(&fpriv->vm, &exec, 2);
			drm_exec_retry_on_contention(&exec);
			if (unlikely(r))
				goto error;
		}

		if (abo) {
			bo_va = amdgpu_vm_bo_find(&fpriv->vm, abo);
			if (!bo_va) {
				r = -ENOENT;
				goto error;
			}
		} else if (args->operation != AMDGPU_VA_OP_CLEAR) {
			bo_va = fpriv->prt_va;
		} else {
			bo_va = NULL;
		}

		/* Apply Vega-specific workload optimization if mapping/replacing */
		if (abo && adev->asic_type == CHIP_VEGA10) {
			/* Buffer is already locked by drm_exec at this point, verify lock */
			if (dma_resv_is_locked(abo->tbo.base.resv)) {
				/* Apply workload-specific optimizations */
				amdgpu_vega_optimize_for_workload(adev, abo, abo->flags);
			}
		}

		switch (args->operation) {
			case AMDGPU_VA_OP_MAP:
				va_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
									 args->offset_in_bo, args->map_size,
						 va_flags);
				break;
			case AMDGPU_VA_OP_UNMAP:
				r = amdgpu_vm_bo_unmap(adev, bo_va, args->va_address);
				break;

			case AMDGPU_VA_OP_CLEAR:
				r = amdgpu_vm_bo_clear_mappings(adev, &fpriv->vm,
												args->va_address,
									args->map_size);
				break;
			case AMDGPU_VA_OP_REPLACE:
				va_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
											 args->offset_in_bo, args->map_size,
								 va_flags);
				break;
			default:
				break;
		}
		if (!r && !(args->flags & AMDGPU_VM_DELAY_UPDATE) && !adev->debug_vm)
			amdgpu_gem_va_update_vm(adev, &fpriv->vm, bo_va,
									args->operation);

			error:
			drm_exec_fini(&exec);
		if (gobj) // Check added from source
			drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_op_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev); // Added for Vega check
	struct drm_amdgpu_gem_op *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_vm_bo_base *base;
	struct amdgpu_bo *robj;
	int r;

	gobj = drm_gem_object_lookup(filp, args->handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);

	r = amdgpu_bo_reserve(robj, false);
	if (unlikely(r))
		goto out;

	switch (args->op) {
		case AMDGPU_GEM_OP_GET_GEM_CREATE_INFO: {
			struct drm_amdgpu_gem_create_in info;
			void __user *out = u64_to_user_ptr(args->value);

			info.bo_size = robj->tbo.base.size;
			info.alignment = robj->tbo.page_alignment << PAGE_SHIFT;
			info.domains = robj->preferred_domains;
			info.domain_flags = robj->flags;
			amdgpu_bo_unreserve(robj);
			if (copy_to_user(out, &info, sizeof(info)))
				r = -EFAULT;
			break;
		}
		case AMDGPU_GEM_OP_SET_PLACEMENT:
			if (robj->tbo.base.import_attach &&
				args->value & AMDGPU_GEM_DOMAIN_VRAM) {
				r = -EINVAL;
			amdgpu_bo_unreserve(robj);
			break;
				}
				if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm)) {
					r = -EPERM;
					amdgpu_bo_unreserve(robj);
					break;
				}
				for (base = robj->vm_bo; base; base = base->next)
					if (amdgpu_xgmi_same_hive(amdgpu_ttm_adev(robj->tbo.bdev),
						amdgpu_ttm_adev(base->vm->root.bo->tbo.bdev))) {
						r = -EINVAL;
					amdgpu_bo_unreserve(robj);
				goto out;
						}


						robj->preferred_domains = args->value & (AMDGPU_GEM_DOMAIN_VRAM |
						AMDGPU_GEM_DOMAIN_GTT |
						AMDGPU_GEM_DOMAIN_CPU);
						robj->allowed_domains = robj->preferred_domains;
						if (robj->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
							robj->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

		/* Apply Vega-specific workload optimizations - buffer is already reserved */
		if (adev->asic_type == CHIP_VEGA10) {
			amdgpu_vega_optimize_for_workload(adev, robj, robj->flags);
		}

		if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) { /* Fixed Line */
			amdgpu_vm_bo_invalidate(robj, true); // Changed from amdgpu_vm_bo_invalidate(adev, robj, true); in source
		} /* Fixed Line */

		amdgpu_bo_unreserve(robj);
		break;
		default:
			amdgpu_bo_unreserve(robj);
			r = -EINVAL;
	}

	out:
	drm_gem_object_put(gobj);
	return r;
}

static int amdgpu_gem_align_pitch(struct amdgpu_device *adev,
								  int width,
								  int cpp,
								  bool tiled)
{
	int aligned = width;
	int pitch_mask = 0;

	switch (cpp) {
		case 1:
			pitch_mask = 255;
			break;
		case 2:
			pitch_mask = 127;
			break;
		case 3:
		case 4:
			pitch_mask = 63;
			break;
	}

	aligned += pitch_mask;
	aligned &= ~pitch_mask;
	return aligned * cpp;
}

int amdgpu_mode_dumb_create(struct drm_file *file_priv,
							struct drm_device *dev,
							struct drm_mode_create_dumb *args)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = file_priv->driver_priv;
	struct drm_gem_object *gobj;
	uint32_t handle;
	u64 flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
	AMDGPU_GEM_CREATE_CPU_GTT_USWC |
	AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS;
	u32 domain;
	int r;

	/*
	 * The buffer returned from this function should be cleared, but
	 * it can only be done if the ring is enabled or we'll fail to
	 * create the buffer.
	 */
	if (adev->mman.buffer_funcs_enabled)
		flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	args->pitch = amdgpu_gem_align_pitch(adev, args->width,
										 DIV_ROUND_UP(args->bpp, 8), 0);
	args->size = (u64)args->pitch * args->height;
	args->size = ALIGN(args->size, PAGE_SIZE);
	domain = amdgpu_bo_get_preferred_domain(adev,
											amdgpu_display_supported_domains(adev, flags));

	/* Apply Vega-specific HBM2 memory optimizations for dumb buffers */
	if (adev->asic_type == CHIP_VEGA10) {
		uint32_t alignment = 0; /* Default alignment */
		uint64_t optimized_size = args->size;

		/* Optimize dumb buffer alignment and size for HBM2 memory */
		/* alignment value from optimize_hbm2_bank_access is used here */
		amdgpu_vega_optimize_hbm2_bank_access(adev, NULL, &optimized_size, &alignment);

		/* Use optimized alignment in buffer creation */
		r = amdgpu_gem_object_create(adev, optimized_size, alignment, domain, flags,
									 ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
		if (r == 0) { /* Success case */
			r = drm_gem_handle_create(file_priv, gobj, &handle);
			/* drop reference from allocate - handle holds it now */
			drm_gem_object_put(gobj);
			if (r)
				return r;

			args->handle = handle;
			return 0;
		}
		/* Fall through to normal path if Vega optimization fails or r != 0 */
	}

	/* Original path for non-Vega or if Vega optimization failed */
	// Call to amdgpu_gem_object_create now includes Vega optimizations,
	// but here we use default alignment 0 as in the original code.
	r = amdgpu_gem_object_create(adev, args->size, 0, domain, flags,
								 ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
	if (r)
		return -ENOMEM;

	r = drm_gem_handle_create(file_priv, gobj, &handle);
	/* drop reference from allocate - handle holds it now */
	drm_gem_object_put(gobj);
	if (r)
		return r;

	args->handle = handle;
	return 0;
}

#if defined(CONFIG_DEBUG_FS)
static int amdgpu_debugfs_gem_info_show(struct seq_file *m, void *unused)
{
	struct amdgpu_device *adev = m->private;
	struct drm_device *dev = adev_to_drm(adev);
	struct drm_file *file;
	int r;

	r = mutex_lock_interruptible(&dev->filelist_mutex);
	if (r)
		return r;

	list_for_each_entry(file, &dev->filelist, lhead) {
		struct task_struct *task;
		struct drm_gem_object *gobj;
		struct pid *pid;
		int id;

		/*
		 * Although we have a valid reference on file->pid, that does
		 * not guarantee that the task_struct who called get_pid() is
		 * still alive (e.g. get_pid(current) => fork() => exit()).
		 * Therefore, we need to protect this ->comm access using RCU.
		 */
		rcu_read_lock();
		pid = rcu_dereference(file->pid);
		task = pid_task(pid, PIDTYPE_TGID);
		seq_printf(m, "pid %8d command %s:\n", pid_nr(pid),
				   task ? task->comm : "<unknown>");
		rcu_read_unlock();

		spin_lock(&file->table_lock);
		idr_for_each_entry(&file->object_idr, gobj, id) {
			struct amdgpu_bo *bo = gem_to_amdgpu_bo(gobj);

			amdgpu_bo_print_info(id, bo, m);
		}
		spin_unlock(&file->table_lock);
	}

	mutex_unlock(&dev->filelist_mutex);
	return 0;
}

DEFINE_SHOW_ATTRIBUTE(amdgpu_debugfs_gem_info);

#endif

void amdgpu_debugfs_gem_init(struct amdgpu_device *adev)
{
	#if defined(CONFIG_DEBUG_FS)
	struct drm_minor *minor = adev_to_drm(adev)->primary;
	struct dentry *root = minor->debugfs_root;

	debugfs_create_file("amdgpu_gem_info", 0444, root, adev,
						&amdgpu_debugfs_gem_info_fops);
	#endif
}
