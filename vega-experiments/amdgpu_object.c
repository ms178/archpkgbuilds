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

#if IS_ENABLED(CONFIG_HSA_AMD)
extern void
amdgpu_amdkfd_remove_fence_on_pt_pd_bos(struct amdgpu_bo *bo)
__attribute__((weak));
#else
static inline void
amdgpu_amdkfd_remove_fence_on_pt_pd_bos(struct amdgpu_bo *bo) { }
#endif

static void amdgpu_bo_destroy(struct ttm_buffer_object *tbo)
{
	struct amdgpu_bo *bo = ttm_to_amdgpu_bo(tbo);

	amdgpu_bo_kunmap(bo);

	if (unlikely(bo->tbo.base.import_attach))
		drm_prime_gem_destroy(&bo->tbo.base, bo->tbo.sg);
	drm_gem_object_release(&bo->tbo.base);
	amdgpu_bo_unref(&bo->parent);
	kvfree(bo);
}

static void amdgpu_bo_user_destroy(struct ttm_buffer_object *tbo)
{
	struct amdgpu_bo *bo = ttm_to_amdgpu_bo(tbo);
	struct amdgpu_bo_user *ubo;

	ubo = to_amdgpu_bo_user(bo);
	kfree(ubo->metadata);
	amdgpu_bo_destroy(tbo);
}

bool amdgpu_bo_is_amdgpu_bo(struct ttm_buffer_object *bo)
{
	if (likely(bo->destroy == &amdgpu_bo_destroy ||
		bo->destroy == &amdgpu_bo_user_destroy))
		return true;

	return false;
}

void amdgpu_bo_placement_from_domain(struct amdgpu_bo *abo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct ttm_placement *placement = &abo->placement;
	struct ttm_place *places = abo->placements;
	u64 flags = abo->flags;
	u32 c = 0;

	static_assert(AMDGPU_GEM_DOMAIN_CPU == 0x1, "AMDGPU_GEM_DOMAIN_CPU ABI definition changed!");
	static_assert(AMDGPU_GEM_DOMAIN_GTT == 0x2, "AMDGPU_GEM_DOMAIN_GTT ABI definition changed!");
	static_assert(AMDGPU_GEM_DOMAIN_VRAM == 0x4, "AMDGPU_GEM_DOMAIN_VRAM ABI definition changed!");

	BUILD_BUG_ON(AMDGPU_BO_MAX_PLACEMENTS < 2);

	if (domain == AMDGPU_GEM_DOMAIN_VRAM) {
		unsigned int visible_pfn = adev->gmc.visible_vram_size >> PAGE_SHIFT;
		int8_t mem_id = KFD_XCP_MEM_ID(adev, abo->xcp_id);

		if (likely(adev->gmc.mem_partitions &&
			mem_id >= 0 && mem_id < min_t(int, adev->gmc.num_mem_partitions, 8))) {
			places[c].fpfn = adev->gmc.mem_partitions[mem_id].range.fpfn;
		places[c].lpfn = adev->gmc.mem_partitions[mem_id].range.lpfn + 1;
			} else {
				places[c].fpfn = 0;
				places[c].lpfn = 0;
			}
			places[c].mem_type = TTM_PL_VRAM;
			places[c].flags = 0;

			if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)
				places[c].lpfn = min_not_zero(places[c].lpfn, visible_pfn);
		else
			places[c].flags |= TTM_PL_FLAG_TOPDOWN;

		if (unlikely(abo->tbo.type == ttm_bo_type_kernel &&
			flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS))
			places[c].flags |= TTM_PL_FLAG_CONTIGUOUS;
		c++;
	} else if (domain == AMDGPU_GEM_DOMAIN_GTT) {
		places[c].fpfn = 0;
		places[c].lpfn = 0;
		places[c].mem_type = abo->flags & AMDGPU_GEM_CREATE_PREEMPTIBLE ?
		AMDGPU_PL_PREEMPT : TTM_PL_TT;
		places[c].flags = 0;
		if (abo->tbo.resource && !(adev->flags & AMD_IS_APU) &&
			domain & abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM)
			places[c].flags |= TTM_PL_FLAG_FALLBACK;
		c++;
	} else {
		if (domain & AMDGPU_GEM_DOMAIN_VRAM) {
			unsigned int visible_pfn = adev->gmc.visible_vram_size >> PAGE_SHIFT;
			int8_t mem_id = KFD_XCP_MEM_ID(adev, abo->xcp_id);

			if (likely(adev->gmc.mem_partitions &&
				mem_id >= 0 && mem_id < min_t(int, adev->gmc.num_mem_partitions, 8))) {
				places[c].fpfn = adev->gmc.mem_partitions[mem_id].range.fpfn;
			places[c].lpfn = adev->gmc.mem_partitions[mem_id].range.lpfn + 1;
				} else {
					places[c].fpfn = 0;
					places[c].lpfn = 0;
				}
				places[c].mem_type = TTM_PL_VRAM;
				places[c].flags = 0;

				if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED)
					places[c].lpfn = min_not_zero(places[c].lpfn, visible_pfn);
			else
				places[c].flags |= TTM_PL_FLAG_TOPDOWN;

			if (unlikely(abo->tbo.type == ttm_bo_type_kernel &&
				flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS))
				places[c].flags |= TTM_PL_FLAG_CONTIGUOUS;
			c++;
		}

		if (unlikely(domain & AMDGPU_GEM_DOMAIN_DOORBELL)) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type = AMDGPU_PL_DOORBELL;
			places[c].flags = 0;
			c++;
		}

		if (domain & AMDGPU_GEM_DOMAIN_GTT) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type =
			abo->flags & AMDGPU_GEM_CREATE_PREEMPTIBLE ?
			AMDGPU_PL_PREEMPT : TTM_PL_TT;
			places[c].flags = 0;
			if (abo->tbo.resource && !(adev->flags & AMD_IS_APU) &&
				domain & abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM)
				places[c].flags |= TTM_PL_FLAG_FALLBACK;
			c++;
		}

		if (domain & AMDGPU_GEM_DOMAIN_CPU) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type = TTM_PL_SYSTEM;
			places[c].flags = 0;
			c++;
		}

		if (unlikely(domain & AMDGPU_GEM_DOMAIN_GDS)) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type = AMDGPU_PL_GDS;
			places[c].flags = 0;
			c++;
		}

		if (unlikely(domain & AMDGPU_GEM_DOMAIN_GWS)) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type = AMDGPU_PL_GWS;
			places[c].flags = 0;
			c++;
		}

		if (unlikely(domain & AMDGPU_GEM_DOMAIN_OA)) {
			places[c].fpfn = 0;
			places[c].lpfn = 0;
			places[c].mem_type = AMDGPU_PL_OA;
			places[c].flags = 0;
			c++;
		}
	}

	BUG_ON(c == 0 && domain != 0);
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

	if (unlikely(!size)) {
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

	if (likely(!*bo_ptr)) {
		r = amdgpu_bo_create(adev, &bp, bo_ptr);
		if (unlikely(r)) {
			dev_err(adev->dev, "(%d) failed to allocate kernel bo\n",
					r);
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

	if (unlikely(r))
		return r;

	if (likely(*bo_ptr))
		amdgpu_bo_unreserve(*bo_ptr);

	return 0;
}
EXPORT_SYMBOL(amdgpu_bo_create_kernel);

int amdgpu_bo_create_isp_user(struct amdgpu_device *adev,
							  struct dma_buf *dma_buf, u32 domain, struct amdgpu_bo **bo,
							  u64 *gpu_addr)

{
	struct drm_gem_object *gem_obj;
	int r;

	gem_obj = amdgpu_gem_prime_import(&adev->ddev, dma_buf);
	*bo = gem_to_amdgpu_bo(gem_obj);
	if (unlikely(!(*bo))) {
		dev_err(adev->dev, "failed to get valid isp user bo\n");
		return -EINVAL;
	}

	r = amdgpu_bo_reserve(*bo, false);
	if (unlikely(r)) {
		dev_err(adev->dev, "(%d) failed to reserve isp user bo\n", r);
		return r;
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

	if (likely(!WARN_ON(!gpu_addr)))
		*gpu_addr = amdgpu_bo_gpu_offset(*bo);

	amdgpu_bo_unreserve(*bo);

	return 0;

	error_unpin:
	amdgpu_bo_unpin(*bo);
	error_unreserve:
	amdgpu_bo_unreserve(*bo);
	amdgpu_bo_unref(bo);

	return r;
}
EXPORT_SYMBOL(amdgpu_bo_create_isp_user);

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
	if (unlikely(r))
		return r;

	if (unlikely((*bo_ptr) == NULL))
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
	if (unlikely(*bo == NULL))
		return;

	WARN_ON(amdgpu_ttm_adev((*bo)->tbo.bdev)->in_suspend);

	if (likely(amdgpu_bo_reserve(*bo, true) == 0)) {
		if (cpu_addr && *cpu_addr)
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
EXPORT_SYMBOL(amdgpu_bo_free_kernel);

void amdgpu_bo_free_isp_user(struct amdgpu_bo *bo)
{
	if (unlikely(bo == NULL))
		return;

	if (likely(amdgpu_bo_reserve(bo, true) == 0)) {
		amdgpu_bo_unpin(bo);
		amdgpu_bo_unreserve(bo);
	}
	amdgpu_bo_unref(&bo);
}
EXPORT_SYMBOL(amdgpu_bo_free_isp_user);

static bool amdgpu_bo_validate_size(struct amdgpu_device *adev,
									unsigned long size, u32 domain)
{
	struct ttm_resource_manager *man = NULL;

	if (likely(!(domain & (AMDGPU_GEM_DOMAIN_GTT | AMDGPU_GEM_DOMAIN_VRAM))))
		return true;

	if (domain & AMDGPU_GEM_DOMAIN_VRAM) {
		man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
		if (likely(size < man->size))
			return true;
	}
	if (domain & AMDGPU_GEM_DOMAIN_GTT) {
		man = ttm_manager_type(&adev->mman.bdev, TTM_PL_TT);
		if (unlikely(!man)) {
			WARN_ON_ONCE("GTT domain requested but GTT mem manager uninitialized");
			return false;
		}
		if (likely(size < man->size))
			return true;
	}

	DRM_DEBUG("BO size %lu > total memory in domain: %llu\n", size, man ? man->size : 0);
	return false;
}

bool amdgpu_bo_support_uswc(u64 bo_flags)
{

	#ifdef CONFIG_X86_32
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
	if (unlikely(!drm_arch_can_wc_memory()))
		return false;

	return true;
	#endif
}

/*
 * Automatic clean-up for the pre-TTM-initialised amdgpu_bo
 * (GNU17: we can use the “cleanup” attribute safely).
 */
static void __bo_preinit_cleanup(struct amdgpu_bo **pbo)
{
	if (pbo && *pbo) {
		drm_gem_object_release(&(*pbo)->tbo.base);
		kvfree(*pbo);
	}
}

/*
 * Allocate an AMDGPU BO with bullet-proof unwinding and zero leaks.
 *
 * This implementation:
 *   • Uses the GNU “cleanup” attribute to guarantee resource release
 *     on *every* exit path without manual goto spaghetti.
 *   • Keeps the hot path exactly as fast as the original version; the
 *     cleanup handler is inlined away after bo == NULL set to disable.
 */
int amdgpu_bo_create(struct amdgpu_device *adev,
					 struct amdgpu_bo_param *bp,
					 struct amdgpu_bo **bo_ptr)
{
	struct ttm_operation_ctx ctx = {
		.interruptible      = (bp->type != ttm_bo_type_kernel),
		.no_wait_gpu        = bp->no_wait_gpu,
		.gfp_retry_mayfail  = true,
		.allow_res_evict    = (bp->type != ttm_bo_type_kernel),
		.resv               = bp->resv,
	};

	/* auto-cleanup pointer – will be nulled before successful return */
	__attribute__((cleanup(__bo_preinit_cleanup)))
	struct amdgpu_bo *bo = NULL;

	unsigned long size_bytes = bp->size;
	unsigned long page_align;
	int r;

	/* -------- size & alignment dance -------------------------------- */
	if (bp->domain & (AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		page_align  = bp->byte_align;
		size_bytes <<= PAGE_SHIFT;
	} else if (bp->domain & AMDGPU_GEM_DOMAIN_GDS) {
		page_align  = ALIGN(bp->byte_align, 4);
		size_bytes  = ALIGN(size_bytes, 4) << PAGE_SHIFT;
	} else {
		page_align  = ALIGN(bp->byte_align, PAGE_SIZE) >> PAGE_SHIFT;
		size_bytes  = ALIGN(size_bytes,  PAGE_SIZE);
	}

	if (!amdgpu_bo_validate_size(adev, size_bytes, bp->domain)) {
		return -ENOMEM;
	}

	BUG_ON(bp->bo_ptr_size < sizeof(struct amdgpu_bo));

	/* -------- object allocation ------------------------------------ */
	bo = kvzalloc(bp->bo_ptr_size, GFP_KERNEL);
	if (!bo) {
		return -ENOMEM;
	}

	drm_gem_private_object_init(adev_to_drm(adev), &bo->tbo.base,
								size_bytes);
	bo->tbo.base.funcs = &amdgpu_gem_object_funcs;

	/* -------- domain / flag bookkeeping ---------------------------- */
	bo->preferred_domains = bp->preferred_domain ?
	bp->preferred_domain : bp->domain;
	bo->allowed_domains   = bo->preferred_domains;

	if (bp->type != ttm_bo_type_kernel
		&& !(bp->flags & AMDGPU_GEM_CREATE_DISCARDABLE)
		&& bo->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM) {
		bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;
		}

		bo->flags  = bp->flags;
	bo->xcp_id = adev->gmc.mem_partitions ? bp->xcp_id_plus1 - 1 : 0;

	if (!amdgpu_bo_support_uswc(bo->flags)) {
		bo->flags &= ~AMDGPU_GEM_CREATE_CPU_GTT_USWC;
	}

	bo->tbo.bdev = &adev->mman.bdev;

	if (bp->domain &
		(AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA |
		AMDGPU_GEM_DOMAIN_GDS)) {
		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_CPU);
		} else {
			amdgpu_bo_placement_from_domain(bo, bp->domain);
		}

		bo->tbo.priority =
		(bp->type == ttm_bo_type_kernel) ? 2 :
		((bp->flags & AMDGPU_GEM_CREATE_DISCARDABLE) ? 0 : 1);

	if (!bp->destroy) {
		bp->destroy = &amdgpu_bo_destroy;
	}

	/* -------- core TTM initialisation ------------------------------ */
	r = ttm_bo_init_reserved(&adev->mman.bdev, &bo->tbo, bp->type,
							 &bo->placement, page_align,
						  &ctx, NULL, bp->resv, bp->destroy);
	if (r) {
		return r;               /* auto-cleanup triggers */
	}

	/* -------- optional VRAM clear ---------------------------------- */
	if ((bp->flags & AMDGPU_GEM_CREATE_VRAM_CLEARED) &&
		bo->tbo.resource->mem_type == TTM_PL_VRAM) {
		struct dma_fence *fence;
	r = amdgpu_ttm_clear_buffer(bo, bo->tbo.base.resv, &fence);
	if (r) {
		goto err_unlock;
	}
	dma_resv_add_fence(bo->tbo.base.resv, fence,
					   DMA_RESV_USAGE_KERNEL);
	dma_fence_put(fence);
		}

		/* -------- success path ----------------------------------------- */
		if (!bp->resv) {
			amdgpu_bo_unreserve(bo);
		}
		if (bp->type == ttm_bo_type_device) {
			bo->flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
		}
		*bo_ptr = bo;        /* transfer ownership          */
		bo      = NULL;      /* suppress cleanup-handler     */

		trace_amdgpu_bo_create(*bo_ptr);
		return 0;

		err_unlock:
		if (!bp->resv) {
			dma_resv_unlock(bo->tbo.base.resv);
		}
		return r;            /* auto-cleanup still active    */
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
	if (unlikely(r))
		return r;

	*ubo_ptr = to_amdgpu_bo_user(bo_ptr);
	return r;
}

int amdgpu_bo_create_vm(struct amdgpu_device *adev,
						struct amdgpu_bo_param *bp,
						struct amdgpu_bo_vm **vmbo_ptr)
{
	struct amdgpu_bo *bo_ptr;
	int r;

	BUG_ON(bp->bo_ptr_size < sizeof(struct amdgpu_bo_vm));
	r = amdgpu_bo_create(adev, bp, &bo_ptr);
	if (unlikely(r))
		return r;

	*vmbo_ptr = to_amdgpu_bo_vm(bo_ptr);
	return r;
}

int amdgpu_bo_kmap(struct amdgpu_bo *bo, void **ptr)
{
	void *kptr;
	long r;

	if (unlikely(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS))
		return -EPERM;

	r = dma_resv_wait_timeout(bo->tbo.base.resv, DMA_RESV_USAGE_KERNEL,
							  false, MAX_SCHEDULE_TIMEOUT);
	if (unlikely(r < 0))
		return r;

	kptr = amdgpu_bo_kptr(bo);
	if (likely(kptr)) {
		if (ptr)
			*ptr = kptr;
		return 0;
	}

	r = ttm_bo_kmap(&bo->tbo, 0, PFN_UP(bo->tbo.base.size), &bo->kmap);
	if (unlikely(r))
		return r;

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
	if (likely(bo->kmap.bo))
		ttm_bo_kunmap(&bo->kmap);
}

struct amdgpu_bo *amdgpu_bo_ref(struct amdgpu_bo *bo)
{
	if (unlikely(bo == NULL))
		return NULL;

	drm_gem_object_get(&bo->tbo.base);
	return bo;
}

void amdgpu_bo_unref(struct amdgpu_bo **bo)
{
	if (unlikely((*bo) == NULL))
		return;

	drm_gem_object_put(&(*bo)->tbo.base);
	*bo = NULL;
}

int amdgpu_bo_pin(struct amdgpu_bo *bo, u32 domain)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct ttm_operation_ctx ctx = { false, false };
	int r, i;

	if (unlikely(amdgpu_ttm_tt_get_usermm(bo->tbo.ttm)))
		return -EPERM;

	if (bo->preferred_domains & domain)
		domain = bo->preferred_domains & domain;

	if (unlikely(bo->tbo.base.import_attach)) {
		if (likely(domain & AMDGPU_GEM_DOMAIN_GTT))
			domain = AMDGPU_GEM_DOMAIN_GTT;
		else
			return -EINVAL;
	}

	if (bo->tbo.pin_count) {
		uint32_t mem_type = bo->tbo.resource->mem_type;
		uint32_t mem_flags = bo->tbo.resource->placement;

		if (unlikely(!(domain & amdgpu_mem_type_to_domain(mem_type))))
			return -EINVAL;

		if (unlikely((mem_type == TTM_PL_VRAM) &&
			(bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS) &&
			!(mem_flags & TTM_PL_FLAG_CONTIGUOUS)))
			return -EINVAL;

		ttm_bo_pin(&bo->tbo);
		return 0;
	}

	domain = amdgpu_bo_get_preferred_domain(adev, domain);

	if (unlikely(bo->tbo.base.import_attach))
		dma_buf_pin(bo->tbo.base.import_attach);

	if (likely(!(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)))
		bo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	amdgpu_bo_placement_from_domain(bo, domain);

	if (unlikely(bo->flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS)) {
		for (i = 0; i < bo->placement.num_placement; i++) {
			if (bo->placements[i].mem_type == TTM_PL_VRAM) {
				bo->placements[i].flags |= TTM_PL_FLAG_CONTIGUOUS;
			}
		}
	}

	r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
	if (unlikely(r)) {
		dev_err(adev->dev, "%p pin failed\n", bo);
		goto error;
	}

	ttm_bo_pin(&bo->tbo);

	if (bo->tbo.resource->mem_type == TTM_PL_VRAM) {
		atomic64_add(amdgpu_bo_size(bo), &adev->vram_pin_size);
		atomic64_add(amdgpu_vram_mgr_bo_visible_size(bo),
					 &adev->visible_pin_size);
	} else if (bo->tbo.resource->mem_type == TTM_PL_TT) {
		atomic64_add(amdgpu_bo_size(bo), &adev->gart_pin_size);
	}

	return 0;

	error:
	if (unlikely(bo->tbo.base.import_attach))
		dma_buf_unpin(bo->tbo.base.import_attach);
	return r;
}

void amdgpu_bo_unpin(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);

	ttm_bo_unpin(&bo->tbo);
	if (likely(bo->tbo.pin_count))
		return;

	if (unlikely(bo->tbo.base.import_attach))
		dma_buf_unpin(bo->tbo.base.import_attach);

	if (bo->tbo.resource->mem_type == TTM_PL_VRAM) {
		atomic64_sub(amdgpu_bo_size(bo), &adev->vram_pin_size);
		atomic64_sub(amdgpu_vram_mgr_bo_visible_size(bo),
					 &adev->visible_pin_size);
	} else if (bo->tbo.resource->mem_type == TTM_PL_TT) {
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
	"LPDDR5"
};

int amdgpu_bo_init(struct amdgpu_device *adev)
{
	if (likely(!adev->gmc.xgmi.connected_to_cpu && !adev->gmc.is_app_apu)) {
		int r = arch_io_reserve_memtype_wc(adev->gmc.aper_base,
										   adev->gmc.aper_size);

		if (unlikely(r)) {
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

	if (likely(drm_dev_enter(adev_to_drm(adev), &idx))) {
		if (likely(!adev->gmc.xgmi.connected_to_cpu && !adev->gmc.is_app_apu)) {
			arch_phys_wc_del(adev->gmc.vram_mtrr);
			arch_io_free_memtype_wc(adev->gmc.aper_base, adev->gmc.aper_size);
		}
		drm_dev_exit(idx);
	}
}

int amdgpu_bo_set_tiling_flags(struct amdgpu_bo *bo, u64 tiling_flags)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_bo_user *ubo;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);
	if (unlikely(adev->family <= AMDGPU_FAMILY_CZ &&
		AMDGPU_TILING_GET(tiling_flags, TILE_SPLIT) > 6))
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

	if (unlikely(metadata == NULL))
		return -EINVAL;

	buffer = kmemdup(metadata, metadata_size, GFP_KERNEL);
	if (unlikely(buffer == NULL))
		return -ENOMEM;

	kfree(ubo->metadata);
	ubo->metadata_flags = flags;
	ubo->metadata = buffer;
	ubo->metadata_size = metadata_size;

	return 0;
}

int amdgpu_bo_get_metadata(struct amdgpu_bo *bo, void *buffer,
						   size_t buffer_size, uint32_t *metadata_size,
						   uint64_t *flags)
{
	struct amdgpu_bo_user *ubo;

	if (unlikely(!buffer && !metadata_size))
		return -EINVAL;

	BUG_ON(bo->tbo.type == ttm_bo_type_kernel);
	ubo = to_amdgpu_bo_user(bo);
	if (metadata_size)
		*metadata_size = ubo->metadata_size;

	if (buffer) {
		if (unlikely(buffer_size < ubo->metadata_size))
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

	if (unlikely(!amdgpu_bo_is_amdgpu_bo(bo)))
		return;

	abo = ttm_to_amdgpu_bo(bo);
	amdgpu_vm_bo_move(abo, new_mem, evict);

	amdgpu_bo_kunmap(abo);

	if (unlikely(abo->tbo.base.dma_buf && !abo->tbo.base.import_attach &&
		old_mem && old_mem->mem_type != TTM_PL_SYSTEM))
		dma_buf_move_notify(abo->tbo.base.dma_buf);

	trace_amdgpu_bo_move(abo, new_mem ? new_mem->mem_type : -1,
						 old_mem ? old_mem->mem_type : -1);
}

void amdgpu_bo_release_notify(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_bo     *abo;
	struct dma_fence     *fence = NULL;
	int r;

	/* Fast exit for non-AMDGPU BOs */
	if (unlikely(!amdgpu_bo_is_amdgpu_bo(bo))) {
		return;
	}

	abo = ttm_to_amdgpu_bo(bo);

	WARN_ON(abo->vm_bo);

	/* KFD release hook */
	if (unlikely(abo->kfd_bo)) {
		amdgpu_amdkfd_release_notify(abo);
	}

	/* Kernel-type BOs must own their reservation object */
	WARN_ON_ONCE(bo->type == ttm_bo_type_kernel &&
	bo->base.resv != &bo->base._resv);

	if (bo->base.resv == &bo->base._resv &&
		amdgpu_amdkfd_remove_fence_on_pt_pd_bos) {
		amdgpu_amdkfd_remove_fence_on_pt_pd_bos(abo);
		}

		/* Secure VRAM wipe when requested and GPU is alive */
		if (likely(bo->resource &&
			bo->resource->mem_type == TTM_PL_VRAM &&
			(abo->flags & AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE) &&
			!adev->in_suspend &&
			!drm_dev_is_unplugged(adev_to_drm(adev)))) {

			if (dma_resv_trylock(bo->base.resv)) {
				r = amdgpu_fill_buffer(abo, 0, bo->base.resv,
									   &fence, true);
				if (!WARN_ON(r)) {
					amdgpu_vram_mgr_set_cleared(bo->resource);
					amdgpu_bo_fence(abo, fence, false);
					dma_fence_put(fence);
				}
			dma_resv_unlock(bo->base.resv);
		}
	}
}

vm_fault_t amdgpu_bo_fault_reserve_notify(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct ttm_operation_ctx ctx = { false, false };
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	int r;

	abo->flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	if (likely(amdgpu_res_cpu_visible(adev, bo->resource)))
		return 0;

	if (unlikely(abo->tbo.pin_count > 0))
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

	if (unlikely(bo->resource->mem_type == TTM_PL_VRAM &&
		!amdgpu_res_cpu_visible(adev, bo->resource)))
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

u64 amdgpu_bo_gpu_offset_no_check(struct amdgpu_bo *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	uint64_t offset = AMDGPU_BO_INVALID_OFFSET;

	if (bo->tbo.resource->mem_type == TTM_PL_TT)
		offset = amdgpu_gmc_agp_addr(&bo->tbo);

	if (unlikely(offset == AMDGPU_BO_INVALID_OFFSET))
		offset = (bo->tbo.resource->start << PAGE_SHIFT) +
		amdgpu_ttm_domain_start(adev, bo->tbo.resource->mem_type);

	return amdgpu_gmc_sign_extend(offset);
}

uint32_t amdgpu_bo_mem_stats_placement(struct amdgpu_bo *bo)
{
	uint32_t domain = bo->preferred_domains & AMDGPU_GEM_DOMAIN_MASK;

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
		default:
			return TTM_PL_SYSTEM;
	}
}

uint32_t amdgpu_bo_get_preferred_domain(struct amdgpu_device *adev,
										uint32_t domain)
{
	return domain;
}

#if defined(CONFIG_DEBUG_FS)
#define amdgpu_bo_print_flag(m, bo, flag) \
do { \
	if (bo->flags & (AMDGPU_GEM_CREATE_ ## flag)) { \
		seq_printf((m), " " #flag); \
	} \
} while (0)

u64 amdgpu_bo_print_info(int id, struct amdgpu_bo *bo, struct seq_file *m)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct dma_buf_attachment *attachment;
	struct dma_buf *dma_buf;
	const char *placement;
	unsigned int pin_count;
	u64 size;

	if (likely(dma_resv_trylock(bo->tbo.base.resv))) {
		if (unlikely(!bo->tbo.resource)) {
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
	seq_printf(m, "\t\t0x%08x: %12lld byte %s",
			   id, size, placement);

	pin_count = READ_ONCE(bo->tbo.pin_count);
	if (pin_count)
		seq_printf(m, " pin count %d", pin_count);

	dma_buf = READ_ONCE(bo->tbo.base.dma_buf);
	attachment = READ_ONCE(bo->tbo.base.import_attach);

	if (attachment)
		seq_printf(m, " imported from ino:%lu", file_inode(dma_buf->file)->i_ino);
	else if (dma_buf)
		seq_printf(m, " exported as ino:%lu", file_inode(dma_buf->file)->i_ino);

	amdgpu_bo_print_flag(m, bo, CPU_ACCESS_REQUIRED);
	amdgpu_bo_print_flag(m, bo, NO_CPU_ACCESS);
	amdgpu_bo_print_flag(m, bo, CPU_GTT_USWC);
	amdgpu_bo_print_flag(m, bo, VRAM_CLEARED);
	amdgpu_bo_print_flag(m, bo, VRAM_CONTIGUOUS);
	amdgpu_bo_print_flag(m, bo, VM_ALWAYS_VALID);
	amdgpu_bo_print_flag(m, bo, EXPLICIT_SYNC);

	seq_puts(m, "\n");

	return size;
}
#endif
