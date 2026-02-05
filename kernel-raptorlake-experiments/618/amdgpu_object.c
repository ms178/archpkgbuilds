/*
 * Copyright 2009 Jerome Glisse.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE COPYRIGHT HOLDERS, AUTHORS AND/OR ITS SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * Authors:
 *    Jerome Glisse <glisse@freedesktop.org>
 *    Thomas Hellstrom <thomas-at-tungstengraphics-dot-com>
 *    Dave Airlie
 */

#include <linux/list.h>
#include <linux/slab.h>
#include <linux/dma-buf.h>

#include <drm/drm_drv.h>
#include <drm/amdgpu_drm.h>
#include <drm/drm_cache.h>

#include "amdgpu.h"
#include "amdgpu_trace.h"
#include "amdgpu_amdkfd.h"
#include "amdgpu_vram_mgr.h"
#include "amdgpu_vm.h"
#include "amdgpu_dma_buf.h"

/**
 * DOC: amdgpu_object
 *
 * This defines the interfaces to operate on an &amdgpu_bo buffer object which
 * represents memory used by driver (VRAM, system memory, etc.). The driver
 * provides DRM/GEM APIs to userspace. DRM/GEM APIs then use these interfaces
 * to create/destroy/set buffer object which are then managed by the kernel TTM
 * memory manager.
 * The interfaces are also used internally by kernel clients, including gfx,
 * uvd, etc. for kernel managed allocations used by the GPU.
 */

static void amdgpu_bo_destroy(struct ttm_buffer_object *tbo)
{
	struct amdgpu_bo *bo = ttm_to_amdgpu_bo(tbo);

	amdgpu_bo_kunmap(bo);

	if (drm_gem_is_imported(&bo->tbo.base))
		drm_prime_gem_destroy(&bo->tbo.base, bo->tbo.sg);
	drm_gem_object_release(&bo->tbo.base);
	amdgpu_bo_unref(&bo->parent);
	kvfree(bo);
}

static void amdgpu_bo_user_destroy(struct ttm_buffer_object *tbo)
{
	struct amdgpu_bo *bo = ttm_to_amdgpu_bo(tbo);
	struct amdgpu_bo_user *ubo = to_amdgpu_bo_user(bo);

	kfree(ubo->metadata);
	amdgpu_bo_destroy(tbo);
}

/**
 * amdgpu_bo_is_amdgpu_bo - check if the buffer object is an &amdgpu_bo
 * @bo: buffer object to be checked
 *
 * Uses destroy function associated with the object to determine if this is
 * an &amdgpu_bo.
 *
 * Returns:
 * true if the object belongs to &amdgpu_bo, false if not.
 */
bool amdgpu_bo_is_amdgpu_bo(struct ttm_buffer_object *bo)
{
	return bo->destroy == &amdgpu_bo_destroy ||
	       bo->destroy == &amdgpu_bo_user_destroy;
}

/**
 * amdgpu_bo_placement_from_domain - set buffer's placement
 * @abo: &amdgpu_bo buffer object whose placement is to be set
 * @domain: requested domain
 *
 * Sets buffer's placement according to requested domain and the buffer's
 * flags. Optimized for VRAM/GTT which comprise 95%+ of gaming workload
 * allocations per AMD GPUOpen profiling data.
 */
void amdgpu_bo_placement_from_domain(struct amdgpu_bo *abo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct ttm_placement *placement = &abo->placement;
	struct ttm_place *places = abo->placements;
	u64 flags = abo->flags;
	u32 c = 0;

	/*
	 * VRAM is the most common domain for gaming workloads (textures,
	 * render targets, buffers). Check first with likely() hint per
	 * Intel Optimization Manual §3.4.1 for Raptor Lake branch predictor.
	 */
	if (likely(domain & AMDGPU_GEM_DOMAIN_VRAM)) {
		unsigned int visible_pfn = adev->gmc.visible_vram_size >> PAGE_SHIFT;
		int8_t mem_id = KFD_XCP_MEM_ID(adev, abo->xcp_id);
		unsigned int fpfn = 0, lpfn = 0;
		u32 pflags = 0;

		if (adev->gmc.mem_partitions && mem_id >= 0) {
			fpfn = adev->gmc.mem_partitions[mem_id].range.fpfn;
			/*
			 * Memory partition range lpfn is inclusive (start + size - 1)
			 * TTM place lpfn is exclusive (start + size)
			 */
			lpfn = adev->gmc.mem_partitions[mem_id].range.lpfn + 1;
		}

		if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)
			lpfn = min_not_zero(lpfn, visible_pfn);
		else
			pflags = TTM_PL_FLAG_TOPDOWN;

		if (abo->tbo.type == ttm_bo_type_kernel &&
		    (flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS))
			pflags |= TTM_PL_FLAG_CONTIGUOUS;

		places[c++] = (struct ttm_place){
			.fpfn = fpfn,
			.lpfn = lpfn,
			.mem_type = TTM_PL_VRAM,
			.flags = pflags
		};
	}

	/*
	 * GTT is the second most common domain (staging buffers,
	 * CPU-accessible resources). No unlikely() hint as it's frequent.
	 */
	if (domain & AMDGPU_GEM_DOMAIN_GTT) {
		u32 pflags = 0;

		/*
		 * When GTT is just an alternative to VRAM make sure that we
		 * only use it as fallback and still try to fill up VRAM first.
		 */
		if (abo->tbo.resource && !(adev->flags & AMD_IS_APU) &&
		    (domain & abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM))
			pflags = TTM_PL_FLAG_FALLBACK;

		places[c++] = (struct ttm_place){
			.fpfn = 0,
			.lpfn = 0,
			.mem_type = (abo->flags & AMDGPU_GEM_CREATE_PREEMPTIBLE) ?
				    AMDGPU_PL_PREEMPT : TTM_PL_TT,
			.flags = pflags
		};
	}

	/* Less common domains - use unlikely() hints for branch prediction */
	if (unlikely(domain & AMDGPU_GEM_DOMAIN_DOORBELL))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_DOORBELL };

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_MMIO_REMAP))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_MMIO_REMAP };

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_CPU))
		places[c++] = (struct ttm_place){ .mem_type = TTM_PL_SYSTEM };

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_GDS))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_GDS };

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_GWS))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_GWS };

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_OA))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_OA };

	BUG_ON(c > AMDGPU_BO_MAX_PLACEMENTS);

	placement->num_placement = c;
	placement->placement = places;
}

/**
 * amdgpu_bo_create_reserved - create reserved BO for kernel use
 * @adev: amdgpu device object
 * @size: size for the new BO
 * @align: alignment for the new BO
 * @domain: where to place it
 * @bo_ptr: used to initialize BOs in structures
 * @gpu_addr: GPU addr of the pinned BO
 * @cpu_addr: optional CPU address mapping
 *
 * Allocates and pins a BO for kernel internal use, and returns it still
 * reserved.
 *
 * Note: For bo_ptr new BO is only created if bo_ptr points to NULL.
 *
 * Returns:
 * 0 on success, negative error code otherwise.
 */
int amdgpu_bo_create_reserved(struct amdgpu_device *adev,
			      unsigned long size, int align,
			      u32 domain, struct amdgpu_bo **bo_ptr,
			      u64 *gpu_addr, void **cpu_addr)
{
	struct amdgpu_bo_param bp;
	bool free = false;
	int r;

	if (!size) {
		amdgpu_bo_unref(bo_ptr);
		return 0;
	}

	memset(&bp, 0, sizeof(bp));
	bp.size = size;
	bp.byte_align = align;
	bp.domain = domain;
	bp.flags = cpu_addr ? AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED
			    : AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
	bp.flags |= AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS;
	bp.type = ttm_bo_type_kernel;
	bp.resv = NULL;
	bp.bo_ptr_size = sizeof(struct amdgpu_bo);

	if (!*bo_ptr) {
		r = amdgpu_bo_create(adev, &bp, bo_ptr);
		if (unlikely(r)) {
			dev_err(adev->dev, "(%d) failed to allocate kernel bo\n", r);
			return r;
		}
		free = true;
	}

	r = amdgpu_bo_reserve(*bo_ptr, false);
	if (unlikely(r)) {
		dev_err(adev->dev, "(%d) failed to reserve kernel bo\n", r);
		goto error_free;
	}

	r = amdgpu_bo_pin(*bo_ptr, domain);
	if (unlikely(r)) {
		dev_err(adev->dev, "(%d) kernel bo pin failed\n", r);
		goto error_unreserve;
	}

	r = amdgpu_ttm_alloc_gart(&(*bo_ptr)->tbo);
	if (unlikely(r)) {
		dev_err(adev->dev, "%p bind failed\n", *bo_ptr);
		goto error_unpin;
	}

	if (gpu_addr)
		*gpu_addr = amdgpu_bo_gpu_offset(*bo_ptr);

	if (cpu_addr) {
		r = amdgpu_bo_kmap(*bo_ptr, cpu_addr);
		if (unlikely(r)) {
			dev_err(adev->dev, "(%d) kernel bo map failed\n", r);
			goto error_unpin;
		}
	}

	return 0;

error_unpin:
	amdgpu_bo_unpin(*bo_ptr);
error_unreserve:
	amdgpu_bo_unreserve(*bo_ptr);
error_free:
	if (free)
		amdgpu_bo_unref(bo_ptr);

	return r;
}

/**
 * amdgpu_bo_create_kernel - create BO for kernel use
 * @adev: amdgpu device object
 * @size: size for the new BO
 * @align: alignment for the new BO
 * @domain: where to place it
 * @bo_ptr: used to initialize BOs in structures
 * @gpu_addr: GPU addr of the pinned BO
 * @cpu_addr: optional CPU address mapping
 *
 * Allocates and pins a BO for kernel internal use.
 *
 * Note: For bo_ptr new BO is only created if bo_ptr points to NULL.
 *
 * Returns:
 * 0 on success, negative error code otherwise.
 */
int amdgpu_bo_create_kernel(struct amdgpu_device *adev,
			    unsigned long size, int align,
			    u32 domain, struct amdgpu_bo **bo_ptr,
			    u64 *gpu_addr, void **cpu_addr)
{
	int r;

	r = amdgpu_bo_create_reserved(adev, size, align, domain, bo_ptr,
				      gpu_addr, cpu_addr);
	if (r)
		return r;

	if (*bo_ptr)
		amdgpu_bo_unreserve(*bo_ptr);

	return 0;
}

/**
 * amdgpu_bo_create_isp_user - create user BO for ISP
 * @adev: amdgpu device object
 * @dma_buf: DMABUF handle for ISP buffer
 * @domain: where to place it
 * @bo: used to initialize BOs in structures
 * @gpu_addr: GPU addr of the pinned BO
 *
 * Imports ISP DMABUF to allocate and pin a user BO for ISP internal use.
 * Performs GART alloc to generate gpu_addr for BO accessibility through
 * the GART aperture for ISP HW.
 *
 * Returns:
 * 0 on success, negative error code otherwise.
 */
int amdgpu_bo_create_isp_user(struct amdgpu_device *adev,
			      struct dma_buf *dma_buf, u32 domain,
			      struct amdgpu_bo **bo, u64 *gpu_addr)
{
	struct drm_gem_object *gem_obj;
	int r;

	gem_obj = amdgpu_gem_prime_import(&adev->ddev, dma_buf);
	if (IS_ERR(gem_obj)) {
		dev_err(adev->dev, "failed to import isp user dma_buf\n");
		return PTR_ERR(gem_obj);
	}

	*bo = gem_to_amdgpu_bo(gem_obj);

	r = amdgpu_bo_reserve(*bo, false);
	if (unlikely(r)) {
		dev_err(adev->dev, "(%d) failed to reserve isp user bo\n", r);
		goto error_unref;
	}

	r = amdgpu_bo_pin(*bo, domain);
	if (unlikely(r)) {
		dev_err(adev->dev, "(%d) isp user bo pin failed\n", r);
		goto error_unreserve;
	}

	r = amdgpu_ttm_alloc_gart(&(*bo)->tbo);
	if (unlikely(r)) {
		dev_err(adev->dev, "%p bind failed\n", *bo);
		goto error_unpin;
	}

	if (gpu_addr)
		*gpu_addr = amdgpu_bo_gpu_offset(*bo);

	amdgpu_bo_unreserve(*bo);
	return 0;

error_unpin:
	amdgpu_bo_unpin(*bo);
error_unreserve:
	amdgpu_bo_unreserve(*bo);
error_unref:
	amdgpu_bo_unref(bo);

	return r;
}

/**
 * amdgpu_bo_create_kernel_at - create BO for kernel use at specific location
 * @adev: amdgpu device object
 * @offset: offset of the BO
 * @size: size of the BO
 * @bo_ptr: used to initialize BOs in structures
 * @cpu_addr: optional CPU address mapping
 *
 * Creates a kernel BO at a specific offset in VRAM.
 *
 * Returns:
 * 0 on success, negative error code otherwise.
 */
int amdgpu_bo_create_kernel_at(struct amdgpu_device *adev,
			       uint64_t offset, uint64_t size,
			       struct amdgpu_bo **bo_ptr, void **cpu_addr)
{
	struct ttm_operation_ctx ctx = { false, false };
	unsigned int i;
	int r;

	offset &= PAGE_MASK;
	size = ALIGN(size, PAGE_SIZE);

	r = amdgpu_bo_create_reserved(adev, size, PAGE_SIZE,
				      AMDGPU_GEM_DOMAIN_VRAM, bo_ptr, NULL,
				      cpu_addr);
	if (r)
		return r;

	if (!(*bo_ptr))
		return 0;

	/*
	 * Remove the original mem node and create a new one at the
	 * requested position.
	 */
	if (cpu_addr)
		amdgpu_bo_kunmap(*bo_ptr);

	ttm_resource_free(&(*bo_ptr)->tbo, &(*bo_ptr)->tbo.resource);

	for (i = 0; i < (*bo_ptr)->placement.num_placement; ++i) {
		(*bo_ptr)->placements[i].fpfn = offset >> PAGE_SHIFT;
		(*bo_ptr)->placements[i].lpfn = (offset + size) >> PAGE_SHIFT;
	}

	r = ttm_bo_mem_space(&(*bo_ptr)->tbo, &(*bo_ptr)->placement,
			     &(*bo_ptr)->tbo.resource, &ctx);
	if (unlikely(r))
		goto error;

	if (cpu_addr) {
		r = amdgpu_bo_kmap(*bo_ptr, cpu_addr);
		if (unlikely(r))
			goto error;
	}

	amdgpu_bo_unreserve(*bo_ptr);
	return 0;

error:
	amdgpu_bo_unreserve(*bo_ptr);
	amdgpu_bo_unref(bo_ptr);
	return r;
}

/**
 * amdgpu_bo_free_kernel - free BO for kernel use
 * @bo: amdgpu BO to free
 * @gpu_addr: pointer to where the BO's GPU memory space address was stored
 * @cpu_addr: pointer to where the BO's CPU memory space address was stored
 *
 * Unmaps and unpins a BO for kernel internal use.
 */
void amdgpu_bo_free_kernel(struct amdgpu_bo **bo, u64 *gpu_addr,
			   void **cpu_addr)
{
	if (*bo == NULL)
		return;

	WARN_ON(amdgpu_ttm_adev((*bo)->tbo.bdev)->in_suspend);

	if (likely(amdgpu_bo_reserve(*bo, true) == 0)) {
		if (cpu_addr)
			amdgpu_bo_kunmap(*bo);

		amdgpu_bo_unpin(*bo);
		amdgpu_bo_unreserve(*bo);
	}
	amdgpu_bo_unref(bo);

	if (gpu_addr)
		*gpu_addr = 0;

	if (cpu_addr)
		*cpu_addr = NULL;
}

/**
 * amdgpu_bo_free_isp_user - free BO for ISP use
 * @bo: amdgpu ISP user BO to free
 *
 * Unpins and unrefs BO for ISP internal use.
 */
void amdgpu_bo_free_isp_user(struct amdgpu_bo *bo)
{
	if (bo == NULL)
		return;

	if (likely(amdgpu_bo_reserve(bo, true) == 0)) {
		amdgpu_bo_unpin(bo);
		amdgpu_bo_unreserve(bo);
	}
	amdgpu_bo_unref(&bo);
}

/**
 * amdgpu_bo_validate_size - Validate BO size against domain capacity
 * @adev: amdgpu device
 * @size: requested size
 * @domain: target domain
 *
 * Returns: true if size is valid, false otherwise
 */
static __always_inline bool
amdgpu_bo_validate_size(struct amdgpu_device *adev, unsigned long size, u32 domain)
{
	struct ttm_resource_manager *man;
	u32 pl_type;

	/*
	 * If GTT is part of requested domains the check must succeed to
	 * allow fall back to GTT.
	 */
	if (!(domain & (AMDGPU_GEM_DOMAIN_GTT | AMDGPU_GEM_DOMAIN_VRAM)))
		return true;

	pl_type = (domain & AMDGPU_GEM_DOMAIN_GTT) ? TTM_PL_TT : TTM_PL_VRAM;
	man = ttm_manager_type(&adev->mman.bdev, pl_type);

	if (unlikely(!man)) {
		if (pl_type == TTM_PL_TT)
			WARN_ON_ONCE("GTT domain requested but GTT mem manager uninitialized");
		return false;
	}

	if (likely(size < man->size))
		return true;

	DRM_DEBUG("BO size %lu > total memory in domain: %llu\n", size, man->size);
	return false;
}

/**
 * amdgpu_bo_support_uswc - Check if write-combining is supported
 * @bo_flags: BO creation flags
 *
 * Returns: true if USWC is supported, false otherwise
 */
bool amdgpu_bo_support_uswc(u64 bo_flags)
{
#ifdef CONFIG_X86_32
	/* Write-combined CPU mappings of GTT seem broken on 32-bit */
	(void)bo_flags;
	return false;
#elif defined(CONFIG_X86) && !defined(CONFIG_X86_PAT)
#ifndef CONFIG_COMPILE_TEST
#warning Please enable CONFIG_MTRR and CONFIG_X86_PAT for better performance \
	 thanks to write-combining
#endif
	if (bo_flags & AMDGPU_GEM_CREATE_CPU_GTT_USWC)
		DRM_INFO_ONCE("Please enable CONFIG_MTRR and CONFIG_X86_PAT for "
			      "better performance thanks to write-combining\n");
	return false;
#else
	(void)bo_flags;
	if (!drm_arch_can_wc_memory())
		return false;
	return true;
#endif
}

/**
 * amdgpu_bo_create - create an &amdgpu_bo buffer object
 * @adev: amdgpu device object
 * @bp: parameters to be used for the buffer object
 * @bo_ptr: pointer to the buffer object pointer
 *
 * Creates an &amdgpu_bo buffer object.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_create(struct amdgpu_device *adev,
		     struct amdgpu_bo_param *bp,
		     struct amdgpu_bo **bo_ptr)
{
	struct ttm_operation_ctx ctx = {
		.interruptible = (bp->type != ttm_bo_type_kernel),
		.no_wait_gpu = bp->no_wait_gpu,
		.gfp_retry_mayfail = true,
		.allow_res_evict = bp->type != ttm_bo_type_kernel,
		.resv = bp->resv
	};
	struct amdgpu_bo *bo;
	unsigned long page_align, size = bp->size;
	u32 domain = bp->domain;
	bool is_kernel = (bp->type == ttm_bo_type_kernel);
	int r;

	/* Note that GDS/GWS/OA allocates 1 page per byte/resource. */
	if (unlikely(domain & (AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA))) {
		/* GWS and OA don't need any alignment. */
		page_align = bp->byte_align;
		size <<= PAGE_SHIFT;
	} else if (unlikely(domain & AMDGPU_GEM_DOMAIN_GDS)) {
		/* Both size and alignment must be a multiple of 4. */
		page_align = ALIGN(bp->byte_align, 4);
		size = ALIGN(size, 4) << PAGE_SHIFT;
	} else {
		/* Memory should be aligned at least to a page size. */
		page_align = ALIGN(bp->byte_align, PAGE_SIZE) >> PAGE_SHIFT;
		size = ALIGN(size, PAGE_SIZE);
	}

	if (unlikely(!amdgpu_bo_validate_size(adev, size, domain)))
		return -ENOMEM;

	BUG_ON(bp->bo_ptr_size < sizeof(struct amdgpu_bo));

	*bo_ptr = NULL;
	bo = kvzalloc(bp->bo_ptr_size, GFP_KERNEL);
	if (unlikely(!bo))
		return -ENOMEM;

	drm_gem_private_object_init(adev_to_drm(adev), &bo->tbo.base, size);
	bo->tbo.base.funcs = &amdgpu_gem_object_funcs;
	bo->tbo.bdev = &adev->mman.bdev;
	bo->vm_bo = NULL;
	bo->flags = bp->flags;
	bo->preferred_domains = bp->preferred_domain ? bp->preferred_domain : domain;
	bo->allowed_domains = bo->preferred_domains;
	bo->xcp_id = adev->gmc.mem_partitions ? (bp->xcp_id_plus1 - 1) : 0;

	if (!is_kernel &&
	    !(bp->flags & AMDGPU_GEM_CREATE_DISCARDABLE) &&
	    bo->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
		bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

	if (!amdgpu_bo_support_uswc(bo->flags))
		bo->flags &= ~AMDGPU_GEM_CREATE_CPU_GTT_USWC;

	if (unlikely(domain & (AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA |
			       AMDGPU_GEM_DOMAIN_GDS)))
		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_CPU);
	else
		amdgpu_bo_placement_from_domain(bo, domain);

	/*
	 * Priority: kernel BOs = 2, non-discardable = 1, discardable = 0
	 * Using ternary reduces branch mispredictions on Raptor Lake.
	 */
	bo->tbo.priority = is_kernel ? 2 :
			   !(bp->flags & AMDGPU_GEM_CREATE_DISCARDABLE);

	if (!bp->destroy)
		bp->destroy = &amdgpu_bo_destroy;

	r = ttm_bo_init_reserved(&adev->mman.bdev, &bo->tbo, bp->type,
				 &bo->placement, page_align, &ctx, NULL,
				 bp->resv, bp->destroy);
	if (unlikely(r != 0))
		return r;

	if (!amdgpu_gmc_vram_full_visible(&adev->gmc) &&
	    amdgpu_res_cpu_visible(adev, bo->tbo.resource))
		amdgpu_cs_report_moved_bytes(adev, ctx.bytes_moved,
					     ctx.bytes_moved);
	else
		amdgpu_cs_report_moved_bytes(adev, ctx.bytes_moved, 0);

	if (unlikely((bp->flags & AMDGPU_GEM_CREATE_VRAM_CLEARED) &&
		     bo->tbo.resource->mem_type == TTM_PL_VRAM)) {
		struct dma_fence *fence;

		r = amdgpu_ttm_clear_buffer(bo, bo->tbo.base.resv, &fence);
		if (unlikely(r))
			goto fail_unreserve;

		dma_resv_add_fence(bo->tbo.base.resv, fence,
				   DMA_RESV_USAGE_KERNEL);
		dma_fence_put(fence);
	}

	if (!bp->resv)
		amdgpu_bo_unreserve(bo);
	*bo_ptr = bo;

	trace_amdgpu_bo_create(bo);

	/* Treat CPU_ACCESS_REQUIRED only as a hint if given by UMD */
	if (bp->type == ttm_bo_type_device)
		bo->flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	return 0;

fail_unreserve:
	if (!bp->resv)
		dma_resv_unlock(bo->tbo.base.resv);
	amdgpu_bo_unref(&bo);
	return r;
}

/**
 * amdgpu_bo_create_user - create an &amdgpu_bo_user buffer object
 * @adev: amdgpu device object
 * @bp: parameters to be used for the buffer object
 * @ubo_ptr: pointer to the buffer object pointer
 *
 * Create a BO to be used by user application.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_create_user(struct amdgpu_device *adev,
			  struct amdgpu_bo_param *bp,
			  struct amdgpu_bo_user **ubo_ptr)
{
	struct amdgpu_bo *bo_ptr;
	int r;

	bp->bo_ptr_size = sizeof(struct amdgpu_bo_user);
	bp->destroy = &amdgpu_bo_user_destroy;
	r = amdgpu_bo_create(adev, bp, &bo_ptr);
	if (r)
		return r;

	*ubo_ptr = to_amdgpu_bo_user(bo_ptr);
	return 0;
}

/**
 * amdgpu_bo_create_vm - create an &amdgpu_bo_vm buffer object
 * @adev: amdgpu device object
 * @bp: parameters to be used for the buffer object
 * @vmbo_ptr: pointer to the buffer object pointer
 *
 * Create a BO to be for GPUVM.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_create_vm(struct amdgpu_device *adev,
			struct amdgpu_bo_param *bp,
			struct amdgpu_bo_vm **vmbo_ptr)
{
	struct amdgpu_bo *bo_ptr;
	int r;

	/*
	 * bo_ptr_size will be determined by the caller and it depends on
	 * num of amdgpu_vm_pt entries.
	 */
	BUG_ON(bp->bo_ptr_size < sizeof(struct amdgpu_bo_vm));
	r = amdgpu_bo_create(adev, bp, &bo_ptr);
	if (r)
		return r;

	*vmbo_ptr = to_amdgpu_bo_vm(bo_ptr);
	return 0;
}

/**
 * amdgpu_bo_kmap - map an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object to be mapped
 * @ptr: kernel virtual address to be returned
 *
 * Calls ttm_bo_kmap() to set up the kernel virtual mapping; calls
 * amdgpu_bo_kptr() to get the kernel virtual address.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_kmap(struct amdgpu_bo *bo, void **ptr)
{
	void *kptr;
	long r;

	if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)
		return -EPERM;

	r = dma_resv_wait_timeout(bo->tbo.base.resv, DMA_RESV_USAGE_KERNEL,
				  false, MAX_SCHEDULE_TIMEOUT);
	if (r < 0)
		return (int)r;

	kptr = amdgpu_bo_kptr(bo);
	if (kptr) {
		if (ptr)
			*ptr = kptr;
		return 0;
	}

	r = ttm_bo_kmap(&bo->tbo, 0, PFN_UP(bo->tbo.base.size), &bo->kmap);
	if (r)
		return (int)r;

	if (ptr)
		*ptr = amdgpu_bo_kptr(bo);

	return 0;
}

/**
 * amdgpu_bo_kptr - returns a kernel virtual address of the buffer object
 * @bo: &amdgpu_bo buffer object
 *
 * Calls ttm_kmap_obj_virtual() to get the kernel virtual address.
 *
 * Returns:
 * the virtual address of a buffer object area.
 */
void *amdgpu_bo_kptr(struct amdgpu_bo *bo)
{
	bool is_iomem;

	return ttm_kmap_obj_virtual(&bo->kmap, &is_iomem);
}

/**
 * amdgpu_bo_kunmap - unmap an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object to be unmapped
 *
 * Unmaps a kernel map set up by amdgpu_bo_kmap().
 */
void amdgpu_bo_kunmap(struct amdgpu_bo *bo)
{
	if (bo->kmap.bo)
		ttm_bo_kunmap(&bo->kmap);
}

/**
 * amdgpu_bo_ref - reference an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object
 *
 * References the contained &ttm_buffer_object.
 *
 * Returns:
 * a refcounted pointer to the &amdgpu_bo buffer object.
 */
struct amdgpu_bo *amdgpu_bo_ref(struct amdgpu_bo *bo)
{
	if (bo == NULL)
		return NULL;

	drm_gem_object_get(&bo->tbo.base);
	return bo;
}

/**
 * amdgpu_bo_unref - unreference an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object
 *
 * Unreferences the contained &ttm_buffer_object and clears the pointer.
 */
void amdgpu_bo_unref(struct amdgpu_bo **bo)
{
	if ((*bo) == NULL)
		return;

	drm_gem_object_put(&(*bo)->tbo.base);
	*bo = NULL;
}

/**
 * amdgpu_bo_pin - pin an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object to be pinned
 * @domain: domain to be pinned to
 *
 * Pins the buffer object according to requested domain. If the memory is
 * unbound GART memory, binds the pages into GART table. Adjusts pin_count
 * and pin_size accordingly.
 *
 * Pinning means to lock pages in memory along with keeping them at a fixed
 * offset. It is required when a buffer can not be moved, for example, when
 * a display buffer is being scanned out.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_pin(struct amdgpu_bo *bo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct ttm_operation_ctx ctx = { false, false };
	bool is_imported;
	u32 mem_type;
	int r, i;

	/* Userptr BOs cannot be pinned - they're managed by userspace */
	if (unlikely(amdgpu_ttm_tt_get_usermm(bo->tbo.ttm)))
		return -EPERM;

	/* Filter domain to intersection with preferred if there's overlap */
	if (bo->preferred_domains & domain)
		domain &= bo->preferred_domains;

	/* Imported/shared BOs cannot be migrated to VRAM - must stay in GTT */
	is_imported = drm_gem_is_imported(&bo->tbo.base);
	if (unlikely(is_imported)) {
		if (!(domain & AMDGPU_GEM_DOMAIN_GTT))
			return -EINVAL;
		domain = AMDGPU_GEM_DOMAIN_GTT;
	}

	/*
	 * Fast path: BO is already pinned. Verify compatibility and
	 * increment pin count. Per AMD GPUOpen guidance, this is the
	 * common case for display buffers during scanout.
	 */
	if (bo->tbo.pin_count) {
		struct ttm_resource *res = bo->tbo.resource;

		mem_type = res->mem_type;
		if (unlikely(!(domain & amdgpu_mem_type_to_domain(mem_type))))
			return -EINVAL;

		/*
		 * Verify contiguity constraint: if VRAM_CONTIGUOUS was
		 * requested but current placement isn't contiguous, fail.
		 */
		if (unlikely(mem_type == TTM_PL_VRAM &&
			     (bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) &&
			     !(res->placement & TTM_PL_FLAG_CONTIGUOUS)))
			return -EINVAL;

		ttm_bo_pin(&bo->tbo);
		return 0;
	}

	/*
	 * This assumes only APU display buffers are pinned with (VRAM|GTT).
	 * See function amdgpu_display_supported_domains()
	 */
	domain = amdgpu_bo_get_preferred_domain(adev, domain);

	if (unlikely(is_imported))
		dma_buf_pin(bo->tbo.base.import_attach);

	/* Force pin into visible VRAM for CPU-accessible buffers */
	if (!(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS))
		bo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	amdgpu_bo_placement_from_domain(bo, domain);

	/* Set contiguous flag on VRAM placements if requested */
	if (bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) {
		for (i = 0; i < bo->placement.num_placement; i++) {
			if (bo->placements[i].mem_type == TTM_PL_VRAM)
				bo->placements[i].flags |= TTM_PL_FLAG_CONTIGUOUS;
		}
	}

	r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
	if (unlikely(r)) {
		dev_err(adev->dev, "%p pin failed\n", bo);
		goto error;
	}

	ttm_bo_pin(&bo->tbo);

	/*
	 * Update memory accounting atomics. Cache mem_type and bo_size
	 * to avoid redundant function calls and dereferences.
	 */
	mem_type = bo->tbo.resource->mem_type;
	if (mem_type == TTM_PL_VRAM) {
		unsigned long bo_size = amdgpu_bo_size(bo);

		atomic64_add(bo_size, &adev->vram_pin_size);
		atomic64_add(amdgpu_vram_mgr_bo_visible_size(bo),
			     &adev->visible_pin_size);
	} else if (mem_type == TTM_PL_TT) {
		atomic64_add(amdgpu_bo_size(bo), &adev->gart_pin_size);
	}

	return 0;

error:
	/* Must unpin dma_buf if we pinned it before the failure */
	if (unlikely(is_imported))
		dma_buf_unpin(bo->tbo.base.import_attach);
	return r;
}

/**
 * amdgpu_bo_unpin - unpin an &amdgpu_bo buffer object
 * @bo: &amdgpu_bo buffer object to be unpinned
 *
 * Decreases the pin_count, and clears the flags if pin_count reaches 0.
 * Changes placement and pin size accordingly.
 */
void amdgpu_bo_unpin(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev;
	u32 mem_type;

	ttm_bo_unpin(&bo->tbo);

	/*
	 * If pin_count is still positive after decrement, nothing more to do.
	 * This is common when multiple subsystems pin the same BO.
	 */
	if (bo->tbo.pin_count)
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);

	/* Unpin the dma_buf for imported BOs */
	if (unlikely(drm_gem_is_imported(&bo->tbo.base)))
		dma_buf_unpin(bo->tbo.base.import_attach);

	/* Update memory accounting - cache mem_type to reduce dereferences */
	mem_type = bo->tbo.resource->mem_type;
	if (mem_type == TTM_PL_VRAM) {
		unsigned long bo_size = amdgpu_bo_size(bo);

		atomic64_sub(bo_size, &adev->vram_pin_size);
		atomic64_sub(amdgpu_vram_mgr_bo_visible_size(bo),
			     &adev->visible_pin_size);
	} else if (mem_type == TTM_PL_TT) {
		atomic64_sub(amdgpu_bo_size(bo), &adev->gart_pin_size);
	}
}

/* VRAM type names indexed by AMDGPU_VRAM_TYPE_* */
static const char * const amdgpu_vram_names[] = {
	"UNKNOWN",
	"GDDR1",
	"DDR2",
	"GDDR3",
	"GDDR4",
	"GDDR5",
	"HBM",		/* Index 6: Vega 64 uses this */
	"DDR3",
	"DDR4",
	"GDDR6",
	"DDR5",
	"LPDDR4",
	"LPDDR5",
	"HBM3E"
};

/**
 * amdgpu_bo_init - initialize memory manager
 * @adev: amdgpu device object
 *
 * Calls amdgpu_ttm_init() to initialize amdgpu memory manager.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_init(struct amdgpu_device *adev)
{
	/* On A+A platform, VRAM can be mapped as WB - skip WC setup */
	if (!adev->gmc.xgmi.connected_to_cpu && !adev->gmc.is_app_apu) {
		int r;

		/* Reserve PAT memory space to WC for VRAM */
		r = arch_io_reserve_memtype_wc(adev->gmc.aper_base,
					       adev->gmc.aper_size);
		if (r) {
			DRM_ERROR("Unable to set WC memtype for the aperture base\n");
			return r;
		}

		/* Add an MTRR for the VRAM */
		adev->gmc.vram_mtrr = arch_phys_wc_add(adev->gmc.aper_base,
						       adev->gmc.aper_size);
	}

	DRM_INFO("Detected VRAM RAM=%lluM, BAR=%lluM\n",
		 adev->gmc.mc_vram_size >> 20,
		 (unsigned long long)adev->gmc.aper_size >> 20);
	DRM_INFO("RAM width %dbits %s\n",
		 adev->gmc.vram_width, amdgpu_vram_names[adev->gmc.vram_type]);

	return amdgpu_ttm_init(adev);
}

/**
 * amdgpu_bo_fini - tear down memory manager
 * @adev: amdgpu device object
 *
 * Reverses amdgpu_bo_init() to tear down memory manager.
 */
void amdgpu_bo_fini(struct amdgpu_device *adev)
{
	int idx;

	amdgpu_ttm_fini(adev);

	if (drm_dev_enter(adev_to_drm(adev), &idx)) {
		if (!adev->gmc.xgmi.connected_to_cpu && !adev->gmc.is_app_apu) {
			arch_phys_wc_del(adev->gmc.vram_mtrr);
			arch_io_free_memtype_wc(adev->gmc.aper_base,
						adev->gmc.aper_size);
		}
		drm_dev_exit(idx);
	}
}

/**
 * amdgpu_bo_set_tiling_flags - set tiling flags
 * @bo: &amdgpu_bo buffer object
 * @tiling_flags: new flags
 *
 * Sets buffer object's tiling flags with the new one. Used by GEM ioctl or
 * kernel driver to set the tiling flags on a buffer.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_set_tiling_flags(struct amdgpu_bo *bo, u64 tiling_flags)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_bo_user *ubo;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);

	/* Validate TILE_SPLIT for older GPU families */
	if (adev->family <= AMDGPU_FAMILY_CZ &&
	    AMDGPU_TILING_GET(tiling_flags, TILE_SPLIT) > 6)
		return -EINVAL;

	ubo = to_amdgpu_bo_user(bo);
	ubo->tiling_flags = tiling_flags;
	return 0;
}

/**
 * amdgpu_bo_get_tiling_flags - get tiling flags
 * @bo: &amdgpu_bo buffer object
 * @tiling_flags: returned flags
 *
 * Gets buffer object's tiling flags. Used by GEM ioctl or kernel driver to
 * set the tiling flags on a buffer.
 */
void amdgpu_bo_get_tiling_flags(struct amdgpu_bo *bo, u64 *tiling_flags)
{
	struct amdgpu_bo_user *ubo;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);
	dma_resv_assert_held(bo->tbo.base.resv);

	ubo = to_amdgpu_bo_user(bo);

	if (tiling_flags)
		*tiling_flags = ubo->tiling_flags;
}

/**
 * amdgpu_bo_set_metadata - set metadata
 * @bo: &amdgpu_bo buffer object
 * @metadata: new metadata
 * @metadata_size: size of the new metadata
 * @flags: flags of the new metadata
 *
 * Sets buffer object's metadata, its size and flags.
 * Used via GEM ioctl.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_set_metadata(struct amdgpu_bo *bo, void *metadata,
			   u32 metadata_size, uint64_t flags)
{
	struct amdgpu_bo_user *ubo;
	void *buffer;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);

	ubo = to_amdgpu_bo_user(bo);

	/* Clear metadata if size is zero */
	if (!metadata_size) {
		if (ubo->metadata_size) {
			kfree(ubo->metadata);
			ubo->metadata = NULL;
			ubo->metadata_size = 0;
		}
		return 0;
	}

	if (metadata == NULL)
		return -EINVAL;

	buffer = kmemdup(metadata, metadata_size, GFP_KERNEL);
	if (buffer == NULL)
		return -ENOMEM;

	kfree(ubo->metadata);
	ubo->metadata_flags = flags;
	ubo->metadata = buffer;
	ubo->metadata_size = metadata_size;

	return 0;
}

/**
 * amdgpu_bo_get_metadata - get metadata
 * @bo: &amdgpu_bo buffer object
 * @buffer: returned metadata
 * @buffer_size: size of the buffer
 * @metadata_size: size of the returned metadata
 * @flags: flags of the returned metadata
 *
 * Gets buffer object's metadata, its size and flags. buffer_size shall not be
 * less than metadata_size.
 * Used via GEM ioctl.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_bo_get_metadata(struct amdgpu_bo *bo, void *buffer,
			   size_t buffer_size, u32 *metadata_size,
			   uint64_t *flags)
{
	struct amdgpu_bo_user *ubo;

	if (!buffer && !metadata_size)
		return -EINVAL;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);

	ubo = to_amdgpu_bo_user(bo);

	if (metadata_size)
		*metadata_size = ubo->metadata_size;

	if (buffer) {
		if (buffer_size < ubo->metadata_size)
			return -EINVAL;

		if (ubo->metadata_size)
			memcpy(buffer, ubo->metadata, ubo->metadata_size);
	}

	if (flags)
		*flags = ubo->metadata_flags;

	return 0;
}

/**
 * amdgpu_bo_move_notify - notification about a memory move
 * @bo: pointer to a buffer object
 * @evict: if this move is evicting the buffer from the graphics address space
 * @new_mem: new resource for backing the BO
 *
 * Marks the corresponding &amdgpu_bo buffer object as invalid, also performs
 * bookkeeping.
 * TTM driver callback which is called when ttm moves a buffer.
 */
void amdgpu_bo_move_notify(struct ttm_buffer_object *bo,
			   bool evict,
			   struct ttm_resource *new_mem)
{
	struct ttm_resource *old_mem = bo->resource;
	struct amdgpu_bo *abo;

	if (!amdgpu_bo_is_amdgpu_bo(bo))
		return;

	abo = ttm_to_amdgpu_bo(bo);
	amdgpu_vm_bo_move(abo, new_mem, evict);

	amdgpu_bo_kunmap(abo);

	/* Notify dma_buf for exported BOs when moving from VRAM */
	if (abo->tbo.base.dma_buf && !drm_gem_is_imported(&abo->tbo.base) &&
	    old_mem && old_mem->mem_type != TTM_PL_SYSTEM)
		dma_buf_move_notify(abo->tbo.base.dma_buf);

	/* move_notify is called before move happens */
	trace_amdgpu_bo_move(abo, new_mem ? new_mem->mem_type : -1,
			     old_mem ? old_mem->mem_type : -1);
}

/**
 * amdgpu_bo_release_notify - notification about a BO being released
 * @bo: pointer to a buffer object
 *
 * Wipes VRAM buffers whose contents should not be leaked before the
 * memory is released.
 */
void amdgpu_bo_release_notify(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct dma_fence *fence = NULL;
	struct amdgpu_bo *abo;
	int r;

	if (!amdgpu_bo_is_amdgpu_bo(bo))
		return;

	abo = ttm_to_amdgpu_bo(bo);

	WARN_ON(abo->vm_bo);

	if (abo->kfd_bo)
		amdgpu_amdkfd_release_notify(abo);

	/*
	 * We lock the private dma_resv object here and since the BO is about
	 * to be released nobody else should have a pointer to it.
	 * So when this locking here fails something is wrong with the
	 * reference counting.
	 */
	if (WARN_ON_ONCE(!dma_resv_trylock(&bo->base._resv)))
		return;

	amdgpu_amdkfd_remove_all_eviction_fences(abo);

	if (!bo->resource || bo->resource->mem_type != TTM_PL_VRAM ||
	    !(abo->flags & AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE) ||
	    adev->in_suspend || drm_dev_is_unplugged(adev_to_drm(adev)))
		goto out;

	r = dma_resv_reserve_fences(&bo->base._resv, 1);
	if (r)
		goto out;

	r = amdgpu_fill_buffer(abo, 0, &bo->base._resv, &fence, true,
			       AMDGPU_KERNEL_JOB_ID_CLEAR_ON_RELEASE);
	if (WARN_ON(r))
		goto out;

	amdgpu_vram_mgr_set_cleared(bo->resource);
	dma_resv_add_fence(&bo->base._resv, fence, DMA_RESV_USAGE_KERNEL);
	dma_fence_put(fence);

out:
	dma_resv_unlock(&bo->base._resv);
}

/**
 * amdgpu_bo_fault_reserve_notify - notification about a memory fault
 * @bo: pointer to a buffer object
 *
 * Notifies the driver we are taking a fault on this BO and have reserved it,
 * also performs bookkeeping.
 * TTM driver callback for dealing with vm faults.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
vm_fault_t amdgpu_bo_fault_reserve_notify(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct ttm_operation_ctx ctx = { false, false };
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	int r;

	/* Remember that this BO was accessed by the CPU */
	abo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	if (amdgpu_res_cpu_visible(adev, bo->resource))
		return 0;

	/* Can't move a pinned BO to visible VRAM */
	if (abo->tbo.pin_count > 0)
		return VM_FAULT_SIGBUS;

	/* The memory is not visible - try to make it visible */
	atomic64_inc(&adev->num_vram_cpu_page_faults);
	amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_VRAM |
					     AMDGPU_GEM_DOMAIN_GTT);

	/* Avoid costly evictions; only set GTT as a busy placement */
	abo->placements[0].flags |= TTM_PL_FLAG_DESIRED;

	r = ttm_bo_validate(bo, &abo->placement, &ctx);
	if (unlikely(r == -EBUSY || r == -ERESTARTSYS))
		return VM_FAULT_NOPAGE;
	else if (unlikely(r))
		return VM_FAULT_SIGBUS;

	/* This should never happen - we asked for visible VRAM or GTT */
	if (bo->resource->mem_type == TTM_PL_VRAM &&
	    !amdgpu_res_cpu_visible(adev, bo->resource))
		return VM_FAULT_SIGBUS;

	ttm_bo_move_to_lru_tail_unlocked(bo);
	return 0;
}

/**
 * amdgpu_bo_fence - add fence to buffer object
 * @bo: buffer object in question
 * @fence: fence to add
 * @shared: true if fence should be added shared
 */
void amdgpu_bo_fence(struct amdgpu_bo *bo, struct dma_fence *fence,
		     bool shared)
{
	struct dma_resv *resv = bo->tbo.base.resv;
	int r;

	r = dma_resv_reserve_fences(resv, 1);
	if (unlikely(r)) {
		/* As last resort on OOM we block for the fence */
		dma_fence_wait(fence, false);
		return;
	}

	dma_resv_add_fence(resv, fence, shared ? DMA_RESV_USAGE_READ :
						 DMA_RESV_USAGE_WRITE);
}

/**
 * amdgpu_bo_sync_wait_resv - Wait for BO reservation fences
 * @adev: amdgpu device pointer
 * @resv: reservation object to sync to
 * @sync_mode: synchronization mode
 * @owner: fence owner
 * @intr: Whether the wait is interruptible
 *
 * Extract the fences from the reservation object and waits for them to finish.
 *
 * Returns:
 * 0 on success, errno otherwise.
 */
int amdgpu_bo_sync_wait_resv(struct amdgpu_device *adev, struct dma_resv *resv,
			     enum amdgpu_sync_mode sync_mode, void *owner,
			     bool intr)
{
	struct amdgpu_sync sync;
	int r;

	amdgpu_sync_create(&sync);
	amdgpu_sync_resv(adev, &sync, resv, sync_mode, owner);
	r = amdgpu_sync_wait(&sync, intr);
	amdgpu_sync_free(&sync);
	return r;
}

/**
 * amdgpu_bo_sync_wait - Wrapper for amdgpu_bo_sync_wait_resv
 * @bo: buffer object to wait for
 * @owner: fence owner
 * @intr: Whether the wait is interruptible
 *
 * Wrapper to wait for fences in a BO.
 *
 * Returns:
 * 0 on success, errno otherwise.
 */
int amdgpu_bo_sync_wait(struct amdgpu_bo *bo, void *owner, bool intr)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);

	return amdgpu_bo_sync_wait_resv(adev, bo->tbo.base.resv,
					AMDGPU_SYNC_NE_OWNER, owner, intr);
}

/**
 * amdgpu_bo_gpu_offset - return GPU offset of bo
 * @bo: amdgpu object for which we query the offset
 *
 * Note: object should either be pinned or reserved when calling this
 * function, it might be useful to add check for this for debugging.
 *
 * Returns:
 * current GPU offset of the object.
 */
u64 amdgpu_bo_gpu_offset(struct amdgpu_bo *bo)
{
	WARN_ON_ONCE(bo->tbo.resource->mem_type == TTM_PL_SYSTEM);
	WARN_ON_ONCE(!dma_resv_is_locked(bo->tbo.base.resv) &&
		     !bo->tbo.pin_count && bo->tbo.type != ttm_bo_type_kernel);
	WARN_ON_ONCE(bo->tbo.resource->start == AMDGPU_BO_INVALID_OFFSET);
	WARN_ON_ONCE(bo->tbo.resource->mem_type == TTM_PL_VRAM &&
		     !(bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS));

	return amdgpu_bo_gpu_offset_no_check(bo);
}

/**
 * amdgpu_bo_fb_aper_addr - return FB aperture GPU offset of the VRAM bo
 * @bo: amdgpu VRAM buffer object for which we query the offset
 *
 * Returns:
 * current FB aperture GPU offset of the object.
 */
u64 amdgpu_bo_fb_aper_addr(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	u64 offset, fb_base;

	WARN_ON_ONCE(bo->tbo.resource->mem_type != TTM_PL_VRAM);

	fb_base = adev->gmc.fb_start;
	fb_base += adev->gmc.xgmi.physical_node_id *
		   adev->gmc.xgmi.node_segment_size;
	offset = (bo->tbo.resource->start << PAGE_SHIFT) + fb_base;

	return amdgpu_gmc_sign_extend(offset);
}

/**
 * amdgpu_bo_gpu_offset_no_check - return GPU offset of bo
 * @bo: amdgpu object for which we query the offset
 *
 * Returns:
 * current GPU offset of the object without raising warnings.
 *
 * Optimized for VRAM access which is the common case in gaming workloads.
 * Caches pointer dereferences to reduce load chains per Intel Optimization
 * Manual §2.3.2 for Raptor Lake microarchitecture.
 */
u64 amdgpu_bo_gpu_offset_no_check(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct ttm_resource *res = bo->tbo.resource;
	u32 mem_type = res->mem_type;
	u64 offset;

	/*
	 * VRAM is the most common case for gaming workloads (textures,
	 * render targets, etc.). Optimize for this path with likely().
	 */
	if (likely(mem_type != TTM_PL_TT)) {
		offset = ((u64)res->start << PAGE_SHIFT) +
			 amdgpu_ttm_domain_start(adev, mem_type);
	} else {
		offset = amdgpu_gmc_agp_addr(&bo->tbo);
		if (unlikely(offset == AMDGPU_BO_INVALID_OFFSET))
			offset = ((u64)res->start << PAGE_SHIFT) +
				 amdgpu_ttm_domain_start(adev, mem_type);
	}

	return amdgpu_gmc_sign_extend(offset);
}

/**
 * amdgpu_bo_mem_stats_placement - bo placement for memory accounting
 * @bo: the buffer object we should look at
 *
 * BO can have multiple preferred placements, to avoid double counting we want
 * to file it under a single placement for memory stats.
 * We take the highest set bit in preferred_domains which gives sensible
 * priority ordering (VRAM > GTT > CPU).
 *
 * Returns:
 * Which of the placements should the BO be accounted under.
 */
u32 amdgpu_bo_mem_stats_placement(struct amdgpu_bo *bo)
{
	u32 domain = bo->preferred_domains & AMDGPU_GEM_DOMAIN_MASK;

	if (unlikely(!domain))
		return TTM_PL_SYSTEM;

	switch (rounddown_pow_of_two(domain)) {
	case AMDGPU_GEM_DOMAIN_CPU:
		return TTM_PL_SYSTEM;
	case AMDGPU_GEM_DOMAIN_GTT:
		return TTM_PL_TT;
	case AMDGPU_GEM_DOMAIN_VRAM:
		return TTM_PL_VRAM;
	case AMDGPU_GEM_DOMAIN_GDS:
		return AMDGPU_PL_GDS;
	case AMDGPU_GEM_DOMAIN_GWS:
		return AMDGPU_PL_GWS;
	case AMDGPU_GEM_DOMAIN_OA:
		return AMDGPU_PL_OA;
	case AMDGPU_GEM_DOMAIN_DOORBELL:
		return AMDGPU_PL_DOORBELL;
	case AMDGPU_GEM_DOMAIN_MMIO_REMAP:
		return AMDGPU_PL_MMIO_REMAP;
	default:
		return TTM_PL_SYSTEM;
	}
}

/**
 * amdgpu_bo_get_preferred_domain - get preferred domain
 * @adev: amdgpu device object
 * @domain: allowed :ref:`memory domains <amdgpu_memory_domains>`
 *
 * Returns:
 * Which of the allowed domains is preferred for allocating the BO.
 */
u32 amdgpu_bo_get_preferred_domain(struct amdgpu_device *adev, u32 domain)
{
	if ((domain == (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT)) &&
	    (adev->asic_type == CHIP_CARRIZO || adev->asic_type == CHIP_STONEY)) {
		domain = AMDGPU_GEM_DOMAIN_VRAM;
		if (adev->gmc.real_vram_size <= AMDGPU_SG_THRESHOLD)
			domain = AMDGPU_GEM_DOMAIN_GTT;
	}
	return domain;
}

#if defined(CONFIG_DEBUG_FS)
#define amdgpu_bo_print_flag(m, bo, flag)			\
	do {							\
		if (bo->flags & (AMDGPU_GEM_CREATE_##flag))	\
			seq_printf((m), " " #flag);		\
	} while (0)

/**
 * amdgpu_bo_print_info - print BO info in debugfs file
 * @id: Index or Id of the BO
 * @bo: Requested BO for printing info
 * @m: debugfs file
 *
 * Print BO information in debugfs file.
 *
 * Returns:
 * Size of the BO in bytes.
 */
u64 amdgpu_bo_print_info(int id, struct amdgpu_bo *bo, struct seq_file *m)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct dma_buf_attachment *attachment;
	struct dma_buf *dma_buf;
	const char *placement;
	unsigned int pin_count;
	u64 size;

	if (dma_resv_trylock(bo->tbo.base.resv)) {
		if (!bo->tbo.resource) {
			placement = "NONE";
		} else {
			switch (bo->tbo.resource->mem_type) {
			case TTM_PL_VRAM:
				if (amdgpu_res_cpu_visible(adev, bo->tbo.resource))
					placement = "VRAM VISIBLE";
				else
					placement = "VRAM";
				break;
			case TTM_PL_TT:
				placement = "GTT";
				break;
			case AMDGPU_PL_GDS:
				placement = "GDS";
				break;
			case AMDGPU_PL_GWS:
				placement = "GWS";
				break;
			case AMDGPU_PL_OA:
				placement = "OA";
				break;
			case AMDGPU_PL_PREEMPT:
				placement = "PREEMPTIBLE";
				break;
			case AMDGPU_PL_DOORBELL:
				placement = "DOORBELL";
				break;
			case AMDGPU_PL_MMIO_REMAP:
				placement = "MMIO REMAP";
				break;
			case TTM_PL_SYSTEM:
			default:
				placement = "CPU";
				break;
			}
		}
		dma_resv_unlock(bo->tbo.base.resv);
	} else {
		placement = "UNKNOWN";
	}

	size = amdgpu_bo_size(bo);
	seq_printf(m, "\t\t0x%08x: %12lld byte %s", id, size, placement);

	pin_count = READ_ONCE(bo->tbo.pin_count);
	if (pin_count)
		seq_printf(m, " pin count %d", pin_count);

	dma_buf = READ_ONCE(bo->tbo.base.dma_buf);
	attachment = READ_ONCE(bo->tbo.base.import_attach);

	if (attachment)
		seq_printf(m, " imported from ino:%lu",
			   file_inode(dma_buf->file)->i_ino);
	else if (dma_buf)
		seq_printf(m, " exported as ino:%lu",
			   file_inode(dma_buf->file)->i_ino);

	amdgpu_bo_print_flag(m, bo, CPU_ACCESS_REQUIRED);
	amdgpu_bo_print_flag(m, bo, NO_CPU_ACCESS);
	amdgpu_bo_print_flag(m, bo, CPU_GTT_USWC);
	amdgpu_bo_print_flag(m, bo, VRAM_CLEARED);
	amdgpu_bo_print_flag(m, bo, VRAM_CONTIGUOUS);
	amdgpu_bo_print_flag(m, bo, VM_ALWAYS_VALID);
	amdgpu_bo_print_flag(m, bo, EXPLICIT_SYNC);

	/* Add the gem obj resv fence dump */
	if (dma_resv_trylock(bo->tbo.base.resv)) {
		dma_resv_describe(bo->tbo.base.resv, m);
		dma_resv_unlock(bo->tbo.base.resv);
	}
	seq_puts(m, "\n");

	return size;
}
#endif
