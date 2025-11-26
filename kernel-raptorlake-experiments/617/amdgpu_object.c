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
 */
/*
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

bool amdgpu_bo_is_amdgpu_bo(struct ttm_buffer_object *bo)
{
	return bo->destroy == &amdgpu_bo_destroy ||
	       bo->destroy == &amdgpu_bo_user_destroy;
}

void amdgpu_bo_placement_from_domain(struct amdgpu_bo *abo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct ttm_placement *placement = &abo->placement;
	struct ttm_place *places = abo->placements;
	u64 flags = abo->flags;
	u32 c = 0;

	if (likely(domain & AMDGPU_GEM_DOMAIN_VRAM)) {
		unsigned int visible_pfn = adev->gmc.visible_vram_size >> PAGE_SHIFT;
		int8_t mem_id = KFD_XCP_MEM_ID(adev, abo->xcp_id);
		unsigned int fpfn = 0, lpfn = 0;
		u32 pflags = 0;

		if (adev->gmc.mem_partitions && mem_id >= 0) {
			fpfn = adev->gmc.mem_partitions[mem_id].range.fpfn;
			lpfn = adev->gmc.mem_partitions[mem_id].range.lpfn + 1;
		}

		if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)
			lpfn = min_not_zero(lpfn, visible_pfn);
		else
			pflags = TTM_PL_FLAG_TOPDOWN;

		if (abo->tbo.type == ttm_bo_type_kernel &&
		    flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS)
			pflags |= TTM_PL_FLAG_CONTIGUOUS;

		places[c++] = (struct ttm_place){
			.fpfn = fpfn,
			.lpfn = lpfn,
			.mem_type = TTM_PL_VRAM,
			.flags = pflags
		};
	}

	if (domain & AMDGPU_GEM_DOMAIN_GTT) {
		u32 pflags = 0;

		if (abo->tbo.resource && !(adev->flags & AMD_IS_APU) &&
		    domain & abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM)
			pflags = TTM_PL_FLAG_FALLBACK;

		places[c++] = (struct ttm_place){
			.fpfn = 0,
			.lpfn = 0,
			.mem_type = (abo->flags & AMDGPU_GEM_CREATE_PREEMPTIBLE) ?
				AMDGPU_PL_PREEMPT : TTM_PL_TT,
			.flags = pflags
		};
	}

	if (unlikely(domain & AMDGPU_GEM_DOMAIN_DOORBELL))
		places[c++] = (struct ttm_place){ .mem_type = AMDGPU_PL_DOORBELL };

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

static __always_inline bool
amdgpu_bo_validate_size(struct amdgpu_device *adev, unsigned long size, u32 domain)
{
	struct ttm_resource_manager *man;
	u32 pl_type;

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

bool amdgpu_bo_support_uswc(u64 bo_flags)
{
#ifdef CONFIG_X86_32
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
	bool is_kernel = bp->type == ttm_bo_type_kernel;
	int r;

	if (domain & (AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		page_align = bp->byte_align;
		size <<= PAGE_SHIFT;
	} else if (unlikely(domain & AMDGPU_GEM_DOMAIN_GDS)) {
		page_align = ALIGN(bp->byte_align, 4);
		size = ALIGN(size, 4) << PAGE_SHIFT;
	} else {
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

	if (bp->flags & AMDGPU_GEM_CREATE_VRAM_CLEARED &&
	    bo->tbo.resource->mem_type == TTM_PL_VRAM) {
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

	if (bp->type == ttm_bo_type_device)
		bo->flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	return 0;

fail_unreserve:
	if (!bp->resv)
		dma_resv_unlock(bo->tbo.base.resv);
	amdgpu_bo_unref(&bo);
	return r;
}

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

int amdgpu_bo_create_vm(struct amdgpu_device *adev,
			struct amdgpu_bo_param *bp,
			struct amdgpu_bo_vm **vmbo_ptr)
{
	struct amdgpu_bo *bo_ptr;
	int r;

	BUG_ON(bp->bo_ptr_size < sizeof(struct amdgpu_bo_vm));
	r = amdgpu_bo_create(adev, bp, &bo_ptr);
	if (r)
		return r;

	*vmbo_ptr = to_amdgpu_bo_vm(bo_ptr);
	return 0;
}

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

void *amdgpu_bo_kptr(struct amdgpu_bo *bo)
{
	bool is_iomem;

	return ttm_kmap_obj_virtual(&bo->kmap, &is_iomem);
}

void amdgpu_bo_kunmap(struct amdgpu_bo *bo)
{
	if (bo->kmap.bo)
		ttm_bo_kunmap(&bo->kmap);
}

struct amdgpu_bo *amdgpu_bo_ref(struct amdgpu_bo *bo)
{
	if (bo == NULL)
		return NULL;

	drm_gem_object_get(&bo->tbo.base);
	return bo;
}

void amdgpu_bo_unref(struct amdgpu_bo **bo)
{
	if ((*bo) == NULL)
		return;

	drm_gem_object_put(&(*bo)->tbo.base);
	*bo = NULL;
}

int amdgpu_bo_pin(struct amdgpu_bo *bo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct ttm_operation_ctx ctx = { false, false };
	bool is_imported;
	u32 mem_type;
	int r, i;

	if (unlikely(amdgpu_ttm_tt_get_usermm(bo->tbo.ttm)))
		return -EPERM;

	if (bo->preferred_domains & domain)
		domain &= bo->preferred_domains;

	is_imported = drm_gem_is_imported(&bo->tbo.base);
	if (unlikely(is_imported)) {
		if (!(domain & AMDGPU_GEM_DOMAIN_GTT))
			return -EINVAL;
		domain = AMDGPU_GEM_DOMAIN_GTT;
	}

	if (bo->tbo.pin_count) {
		struct ttm_resource *res = bo->tbo.resource;

		mem_type = res->mem_type;
		if (unlikely(!(domain & amdgpu_mem_type_to_domain(mem_type))))
			return -EINVAL;

		if (unlikely(mem_type == TTM_PL_VRAM &&
			     (bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) &&
			     !(res->placement & TTM_PL_FLAG_CONTIGUOUS)))
			return -EINVAL;

		ttm_bo_pin(&bo->tbo);
		return 0;
	}

	domain = amdgpu_bo_get_preferred_domain(adev, domain);

	if (unlikely(is_imported))
		dma_buf_pin(bo->tbo.base.import_attach);

	if (!(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS))
		bo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	amdgpu_bo_placement_from_domain(bo, domain);

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
	if (unlikely(is_imported))
		dma_buf_unpin(bo->tbo.base.import_attach);
	return r;
}

void amdgpu_bo_unpin(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev;
	u32 mem_type;

	ttm_bo_unpin(&bo->tbo);

	if (likely(bo->tbo.pin_count))
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);

	if (unlikely(drm_gem_is_imported(&bo->tbo.base)))
		dma_buf_unpin(bo->tbo.base.import_attach);

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

static const char * const amdgpu_vram_names[] = {
	"UNKNOWN",
	"GDDR1",
	"DDR2",
	"GDDR3",
	"GDDR4",
	"GDDR5",
	"HBM",
	"DDR3",
	"DDR4",
	"GDDR6",
	"DDR5",
	"LPDDR4",
	"LPDDR5",
	"HBM3E"
};

int amdgpu_bo_init(struct amdgpu_device *adev)
{
	if (!adev->gmc.xgmi.connected_to_cpu && !adev->gmc.is_app_apu) {
		int r = arch_io_reserve_memtype_wc(adev->gmc.aper_base,
						   adev->gmc.aper_size);
		if (r) {
			DRM_ERROR("Unable to set WC memtype for the aperture base\n");
			return r;
		}

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

int amdgpu_bo_set_tiling_flags(struct amdgpu_bo *bo, u64 tiling_flags)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_bo_user *ubo;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);

	if (adev->family <= AMDGPU_FAMILY_CZ &&
	    AMDGPU_TILING_GET(tiling_flags, TILE_SPLIT) > 6)
		return -EINVAL;

	ubo = to_amdgpu_bo_user(bo);
	ubo->tiling_flags = tiling_flags;
	return 0;
}

void amdgpu_bo_get_tiling_flags(struct amdgpu_bo *bo, u64 *tiling_flags)
{
	struct amdgpu_bo_user *ubo;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);
	dma_resv_assert_held(bo->tbo.base.resv);
	ubo = to_amdgpu_bo_user(bo);

	if (tiling_flags)
		*tiling_flags = ubo->tiling_flags;
}

int amdgpu_bo_set_metadata(struct amdgpu_bo *bo, void *metadata,
			   u32 metadata_size, uint64_t flags)
{
	struct amdgpu_bo_user *ubo;
	void *buffer;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);
	ubo = to_amdgpu_bo_user(bo);

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

	if (abo->tbo.base.dma_buf && !drm_gem_is_imported(&abo->tbo.base) &&
	    old_mem && old_mem->mem_type != TTM_PL_SYSTEM)
		dma_buf_move_notify(abo->tbo.base.dma_buf);

	trace_amdgpu_bo_move(abo, new_mem ? new_mem->mem_type : -1,
			     old_mem ? old_mem->mem_type : -1);
}

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

	r = amdgpu_fill_buffer(abo, 0, &bo->base._resv, &fence, true);
	if (WARN_ON(r))
		goto out;

	amdgpu_vram_mgr_set_cleared(bo->resource);
	dma_resv_add_fence(&bo->base._resv, fence, DMA_RESV_USAGE_KERNEL);
	dma_fence_put(fence);

out:
	dma_resv_unlock(&bo->base._resv);
}

vm_fault_t amdgpu_bo_fault_reserve_notify(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct ttm_operation_ctx ctx = { false, false };
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	int r;

	abo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	if (amdgpu_res_cpu_visible(adev, bo->resource))
		return 0;

	if (abo->tbo.pin_count > 0)
		return VM_FAULT_SIGBUS;

	atomic64_inc(&adev->num_vram_cpu_page_faults);
	amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_VRAM |
					     AMDGPU_GEM_DOMAIN_GTT);

	abo->placements[0].flags |= TTM_PL_FLAG_DESIRED;

	r = ttm_bo_validate(bo, &abo->placement, &ctx);
	if (unlikely(r == -EBUSY || r == -ERESTARTSYS))
		return VM_FAULT_NOPAGE;
	else if (unlikely(r))
		return VM_FAULT_SIGBUS;

	if (bo->resource->mem_type == TTM_PL_VRAM &&
	    !amdgpu_res_cpu_visible(adev, bo->resource))
		return VM_FAULT_SIGBUS;

	ttm_bo_move_to_lru_tail_unlocked(bo);
	return 0;
}

void amdgpu_bo_fence(struct amdgpu_bo *bo, struct dma_fence *fence,
		     bool shared)
{
	struct dma_resv *resv = bo->tbo.base.resv;
	int r;

	r = dma_resv_reserve_fences(resv, 1);
	if (unlikely(r)) {
		dma_fence_wait(fence, false);
		return;
	}

	dma_resv_add_fence(resv, fence, shared ? DMA_RESV_USAGE_READ :
			   DMA_RESV_USAGE_WRITE);
}

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

int amdgpu_bo_sync_wait(struct amdgpu_bo *bo, void *owner, bool intr)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);

	return amdgpu_bo_sync_wait_resv(adev, bo->tbo.base.resv,
					AMDGPU_SYNC_NE_OWNER, owner, intr);
}

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

u64 amdgpu_bo_gpu_offset_no_check(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	u64 offset = AMDGPU_BO_INVALID_OFFSET;

	if (bo->tbo.resource->mem_type == TTM_PL_TT)
		offset = amdgpu_gmc_agp_addr(&bo->tbo);

	if (offset == AMDGPU_BO_INVALID_OFFSET)
		offset = (bo->tbo.resource->start << PAGE_SHIFT) +
			 amdgpu_ttm_domain_start(adev, bo->tbo.resource->mem_type);

	return amdgpu_gmc_sign_extend(offset);
}

uint32_t amdgpu_bo_mem_stats_placement(struct amdgpu_bo *bo)
{
	uint32_t domain = bo->preferred_domains & AMDGPU_GEM_DOMAIN_MASK;

	if (!domain)
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
	default:
		return TTM_PL_SYSTEM;
	}
}

uint32_t amdgpu_bo_get_preferred_domain(struct amdgpu_device *adev,
					uint32_t domain)
{
	if ((domain == (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT)) &&
	    ((adev->asic_type == CHIP_CARRIZO) ||
	     (adev->asic_type == CHIP_STONEY))) {
		domain = AMDGPU_GEM_DOMAIN_VRAM;
		if (adev->gmc.real_vram_size <= AMDGPU_SG_THRESHOLD)
			domain = AMDGPU_GEM_DOMAIN_GTT;
	}
	return domain;
}

#if defined(CONFIG_DEBUG_FS)
#define amdgpu_bo_print_flag(m, bo, flag)			\
	do {							\
		if (bo->flags & (AMDGPU_GEM_CREATE_ ## flag))	\
			seq_printf((m), " " #flag);		\
	} while (0)

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

	if (dma_resv_trylock(bo->tbo.base.resv)) {
		dma_resv_describe(bo->tbo.base.resv, m);
		dma_resv_unlock(bo->tbo.base.resv);
	}
	seq_puts(m, "\n");

	return size;
}
#endif
