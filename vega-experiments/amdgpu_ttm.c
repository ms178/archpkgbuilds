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

#include <linux/dma-mapping.h>
#include <linux/iommu.h>
#include <linux/pagemap.h>
#include <linux/sched/task.h>
#include <linux/sched/mm.h>
#include <linux/seq_file.h>
#include <linux/slab.h>
#include <linux/swap.h>
#include <linux/dma-buf.h>
#include <linux/sizes.h>
#include <linux/module.h>
#include <linux/dma-fence-array.h>

#include <drm/drm_drv.h>
#include <drm/gpu_scheduler.h>
#include <drm/ttm/ttm_bo.h>
#include <drm/ttm/ttm_placement.h>
#include <drm/ttm/ttm_range_manager.h>
#include <drm/ttm/ttm_tt.h>

#include <drm/amdgpu_drm.h>

#include "amdgpu.h"
#include "amdgpu_object.h"
#include "amdgpu_trace.h"
#include "amdgpu_amdkfd.h"
#include "amdgpu_sdma.h"
#include "amdgpu_ras.h"
#include "amdgpu_hmm.h"
#include "amdgpu_atomfirmware.h"
#include "amdgpu_res_cursor.h"
#include "bif/bif_4_1_d.h"

MODULE_IMPORT_NS("DMA_BUF");

/* ---------------------------------------------------------------------- */
/*  Forward declarations                                                  */
/* ---------------------------------------------------------------------- */
static int amdgpu_copy_buffer_multi(struct amdgpu_device *adev,
									struct drm_gpu_scheduler **scheds,
									unsigned int numscheds,
									u64 src, u64 dst, u64 bytes,
									u32 copy_flags,
									struct dma_resv *resv,
									struct dma_fence **fence_out);

/*
 * Submit one copy either in parallel (when the multi-ring entity is ready)
 * or on the legacy SDMA-0 path.  On error @err is set to the errno and
 * @fence_var unchanged.  On success @fence_var receives a ref-counted fence.
 */
#define TTM_DO_COPY_PARALLEL(adev, src, dst, sz, flags, resv, fence_var, err) \
do {								\
	if (amdgpu_ttm_entity_initialized &&			\
		(adev)->sdma.num_instances > 1) {			\
			err = amdgpu_copy_buffer_multi(		\
			(adev), amdgpu_ttm_scheds,		\
			(adev)->sdma.num_instances,		\
			(src), (dst), (sz), (flags), (resv),	\
			&(fence_var));				\
		} else {						\
			err = amdgpu_copy_buffer(			\
			(adev)->mman.buffer_funcs_ring,		\
			(src), (dst), (u32)(sz), (resv),		\
			&(fence_var), false, true, (flags));	\
		}							\
} while (0)

#define AMDGPU_TTM_CHUNK_VEGA64	(512ULL << 20)	/* 512 MiB burst size */
#define AMDGPU_TTM_VRAM_MAX_DW_READ	((size_t)128)
#define STRIPE_THRESHOLD	(128ULL << 20)	/* 128 MiB */

static struct drm_sched_entity amdgpu_ttm_sched_entity;
static struct drm_gpu_scheduler **amdgpu_ttm_scheds;
static bool amdgpu_ttm_entity_initialized;

static int amdgpu_ttm_backend_bind(struct ttm_device *bdev,
				   struct ttm_tt *ttm,
				   struct ttm_resource *bo_mem);
static void amdgpu_ttm_backend_unbind(struct ttm_device *bdev,
				      struct ttm_tt *ttm);

static int amdgpu_ttm_init_on_chip(struct amdgpu_device *adev,
				    unsigned int type,
				    uint64_t size_in_page)
{
	return ttm_range_man_init(&adev->mman.bdev, type,
				  false, size_in_page);
}

static __always_inline u64 vega_ttm_chunk_bytes(struct amdgpu_device *adev)
{
	return (adev->asic_type == CHIP_VEGA10) ? (512ULL << 20) : (256ULL << 20);
}

static __always_inline void gtt_window_lock_fast(struct amdgpu_device *adev)
{
	/* uncontended: take mutex immediately */
	if (likely(mutex_trylock(&adev->mman.gtt_window_lock)))
		return;

	/* contended: sleep until owner releases */
	mutex_lock(&adev->mman.gtt_window_lock);
}

/*
 * Make sure SDMA “buffer functions” are enabled when possible.
 * Preconditions now include:
 *   – buffer_funcs_ring exists
 *   – its scheduler reports at least one run‑queue
 *   – the ring is marked ready
 * This guarantees drm_sched_entity_init() will succeed.
 */
static void amdgpu_ttm_auto_enable_buffer_funcs(struct amdgpu_device *adev)
{
	struct amdgpu_ring *ring;

	if (adev->mman.buffer_funcs_enabled || (adev->flags & AMD_IS_APU))
		return;

	ring = READ_ONCE(adev->mman.buffer_funcs_ring);
	if (ring && READ_ONCE(ring->sched.ready)) {
		DRM_DEBUG_DRIVER("amdgpu_ttm: enabling SDMA buffer functions\n");
		amdgpu_ttm_set_buffer_funcs_status(adev, true);
	}
}

/**
 * amdgpu_evict_flags - Compute placement flags
 *
 * @bo: The buffer object to evict
 * @placement: Possible destination(s) for evicted BO
 *
 * Fill in placement data when ttm_bo_evict() is called
 */
static void amdgpu_evict_flags(struct ttm_buffer_object *bo,
				struct ttm_placement *placement)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_bo *abo;
	static const struct ttm_place placements = {
		.fpfn = 0,
		.lpfn = 0,
		.mem_type = TTM_PL_SYSTEM,
		.flags = 0
	};

	/* Don't handle scatter gather BOs */
	if (bo->type == ttm_bo_type_sg) {
		placement->num_placement = 0;
		return;
	}

	/* Object isn't an AMDGPU object so ignore */
	if (!amdgpu_bo_is_amdgpu_bo(bo)) {
		placement->placement = &placements;
		placement->num_placement = 1;
		return;
	}

	abo = ttm_to_amdgpu_bo(bo);
	if (abo->flags & AMDGPU_GEM_CREATE_DISCARDABLE) {
		placement->num_placement = 0;
		return;
	}

	switch (bo->resource->mem_type) {
	case AMDGPU_PL_GDS:
	case AMDGPU_PL_GWS:
	case AMDGPU_PL_OA:
	case AMDGPU_PL_DOORBELL:
		placement->num_placement = 0;
		return;

	case TTM_PL_VRAM:
		if (!adev->mman.buffer_funcs_enabled) {
			/* Move to system memory */
			amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_CPU);

		} else if (!amdgpu_gmc_vram_full_visible(&adev->gmc) &&
			   !(abo->flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) &&
			   amdgpu_res_cpu_visible(adev, bo->resource)) {

			/* Try evicting to the CPU inaccessible part of VRAM
			 * first, but only set GTT as busy placement, so this
			 * BO will be evicted to GTT rather than causing other
			 * BOs to be evicted from VRAM
			 */
			amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_VRAM |
							AMDGPU_GEM_DOMAIN_GTT |
							AMDGPU_GEM_DOMAIN_CPU);
			abo->placements[0].fpfn = adev->gmc.visible_vram_size >> PAGE_SHIFT;
			abo->placements[0].lpfn = 0;
			abo->placements[0].flags |= TTM_PL_FLAG_DESIRED;
		} else {
			/* Move to GTT memory */
			amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_GTT |
							AMDGPU_GEM_DOMAIN_CPU);
		}
		break;
	case TTM_PL_TT:
	case AMDGPU_PL_PREEMPT:
	default:
		amdgpu_bo_placement_from_domain(abo, AMDGPU_GEM_DOMAIN_CPU);
		break;
	}
	*placement = abo->placement;
}

/**
 * amdgpu_ttm_map_buffer - Map memory into the GART windows
 * @bo: buffer object to map
 * @mem: memory object to map
 * @mm_cur: range to map
 * @window: which GART window to use
 * @ring: DMA ring to use for the copy
 * @tmz: if we should setup a TMZ enabled mapping
 * @size: in number of bytes to map, out number of bytes mapped
 * @addr: resulting address inside the MC address space
 *
 * Setup one of the GART windows to access a specific piece of memory or return
 * the physical address for local memory.
 */
/* completely replace the existing body */
static int amdgpu_ttm_map_buffer(struct ttm_buffer_object *bo,
								 struct ttm_resource *mem,
								 struct amdgpu_res_cursor *mm_cur,
								 unsigned int window,
								 struct amdgpu_ring *ring,
								 bool tmz,          /* TMZ mapping wanted   */
								 u64  *size,        /* in/out: bytes        */
								 u64  *addr)        /* out  : gpu address   */
{
	struct amdgpu_device *adev = ring->adev;
	unsigned int off_in_pg, num_pages, num_dw;
	u64 flags, src_addr, dst_addr;
	struct amdgpu_job *job        = NULL;
	void *cpu_addr;
	int r;

	/* ------------------------------------------------------------------
	 * Preconditions
	 * ------------------------------------------------------------------
	 */
	if (WARN_ON(adev->mman.buffer_funcs->copy_max_bytes <
		AMDGPU_GTT_MAX_TRANSFER_SIZE * 8))
		return -EINVAL;

	if (WARN_ON(mem->mem_type == AMDGPU_PL_PREEMPT))
		return -EINVAL;

	/* VRAM that is directly visible → no GART window needed          */
	if (!tmz && mem->start != AMDGPU_BO_INVALID_OFFSET) {
		*addr = amdgpu_ttm_domain_start(adev, mem->mem_type) +
		mm_cur->start;
		return 0;
	}

	/* ------------------------------------------------------------------
	 * Window size / page calculations
	 * ------------------------------------------------------------------
	 */
	off_in_pg = mm_cur->start & ~PAGE_MASK;
	num_pages = PFN_UP(*size + off_in_pg);
	num_pages = min_t(unsigned int, num_pages,
					  AMDGPU_GTT_MAX_TRANSFER_SIZE);

	/* shrink @size when the window cannot map it completely          */
	*size     = min(*size,
					(u64)num_pages * PAGE_SIZE - off_in_pg);
	if (WARN_ON(*size == 0))
		return -EINVAL;

	if (WARN_ON(*size > U32_MAX))
		return -E2BIG;

	/* ------------------------------------------------------------------
	 * Allocate temporary IB that carries the PTEs + copy packet
	 *   bytes   = job header             (num_dw * 4)
	 *            + num_pages PTEs        ( 8 bytes / page)
	 * ------------------------------------------------------------------
	 */
	num_dw = ALIGN(adev->mman.buffer_funcs->copy_num_dw, 8);
	r = amdgpu_job_alloc_with_ib(adev, &adev->mman.high_pr,
								 AMDGPU_FENCE_OWNER_UNDEFINED,
							  num_dw * 4 + num_pages * 8,
							  AMDGPU_IB_POOL_DELAYED, &job);
	if (r)
		return r;

	src_addr = job->ibs[0].gpu_addr + num_dw * 4;
	dst_addr = amdgpu_bo_gpu_offset(adev->gart.bo) +
	window * AMDGPU_GTT_MAX_TRANSFER_SIZE * 8;

	amdgpu_emit_copy_buffer(adev, &job->ibs[0],
							src_addr, dst_addr,
						 (u32)*size, 0);

	amdgpu_ring_pad_ib(ring, &job->ibs[0]);
	WARN_ON(job->ibs[0].length_dw > num_dw);

	/* ------------------------------------------------------------------
	 * Build PTEs
	 * ------------------------------------------------------------------
	 */
	flags = amdgpu_ttm_tt_pte_flags(adev, bo->ttm, mem);
	if (tmz)
		flags |= AMDGPU_PTE_TMZ;

	cpu_addr = &job->ibs[0].ptr[num_dw];

	if (mem->mem_type == TTM_PL_TT) {
		dma_addr_t *daddr = &bo->ttm->dma_address[mm_cur->start >>
		PAGE_SHIFT];
		amdgpu_gart_map(adev, 0, num_pages, daddr, flags, cpu_addr);
	} else { /* VRAM – build linear DMA addresses                       */
		dma_addr_t dma = mm_cur->start + adev->vm_manager.vram_base_offset;
		unsigned int i;

		for (i = 0; i < num_pages; ++i) {
			amdgpu_gart_map(adev, i << PAGE_SHIFT, 1, &dma,
							flags, cpu_addr);
			dma += PAGE_SIZE;
		}
	}

	dma_fence_put(amdgpu_job_submit(job));

	/* final GPU address inside the chosen window                     */
	*addr = adev->gmc.gart_start +
	(u64)window * AMDGPU_GTT_MAX_TRANSFER_SIZE *
	AMDGPU_GPU_PAGE_SIZE + off_in_pg;
	return 0;
}

int amdgpu_ttm_copy_mem_to_mem(struct amdgpu_device *adev,
							   const struct amdgpu_copy_mem *src,
							   const struct amdgpu_copy_mem *dst,
							   u64 size, bool tmz,
							   struct dma_resv *resv,
							   struct dma_fence **fence_out)
{
	struct amdgpu_ring *ring = adev->mman.buffer_funcs_ring;
	struct amdgpu_res_cursor src_mm, dst_mm;
	struct dma_fence *fence = NULL;
	u32 copy_flags = 0;
	int r = 0;

	/* Sanity ---------------------------------------------------------------- */
	if (!ring || !ring->sched.ready || !adev->mman.buffer_funcs_enabled)
		return -EINVAL;

	if (!src || !dst || !src->bo || !dst->bo || !src->mem || !dst->mem)
		return -EINVAL;

	/* Flags ----------------------------------------------------------------- */
	if (tmz)
		copy_flags |= AMDGPU_COPY_FLAGS_TMZ;

	if ((ttm_to_amdgpu_bo(src->bo)->flags & AMDGPU_GEM_CREATE_GFX12_DCC) &&
		src->mem->mem_type == TTM_PL_VRAM)
		copy_flags |= AMDGPU_COPY_FLAGS_READ_DECOMPRESSED;

	if ((ttm_to_amdgpu_bo(dst->bo)->flags & AMDGPU_GEM_CREATE_GFX12_DCC) &&
		dst->mem->mem_type == TTM_PL_VRAM) {
		u64 tiling_flags;
	u32 mc, nt, df, wcd;

	amdgpu_bo_get_tiling_flags(ttm_to_amdgpu_bo(dst->bo),
							   &tiling_flags);
	mc  = AMDGPU_TILING_GET(tiling_flags,
							GFX12_DCC_MAX_COMPRESSED_BLOCK);
	nt  = AMDGPU_TILING_GET(tiling_flags,
							GFX12_DCC_NUMBER_TYPE);
	df  = AMDGPU_TILING_GET(tiling_flags,
							GFX12_DCC_DATA_FORMAT);
	wcd = AMDGPU_TILING_GET(tiling_flags,
							GFX12_DCC_WRITE_COMPRESS_DISABLE);

	copy_flags |= AMDGPU_COPY_FLAGS_WRITE_COMPRESSED           |
	AMDGPU_COPY_FLAGS_SET(MAX_COMPRESSED, mc)     |
	AMDGPU_COPY_FLAGS_SET(NUMBER_TYPE,  nt)       |
	AMDGPU_COPY_FLAGS_SET(DATA_FORMAT,  df)       |
	AMDGPU_COPY_FLAGS_SET(WRITE_COMPRESS_DISABLE, wcd);
		}

		/* Chunked processing ---------------------------------------------------- */
		amdgpu_res_first(src->mem, src->offset, size, &src_mm);
		amdgpu_res_first(dst->mem, dst->offset, size, &dst_mm);

		gtt_window_lock_fast(adev);

		while (src_mm.remaining) {
			u64 cur_size = min3(src_mm.size, dst_mm.size, 256ULL << 20);
			u64 from, to;
			struct dma_fence *next = NULL;

			/* map source / destination windows */
			r = amdgpu_ttm_map_buffer(src->bo, src->mem, &src_mm, 0, ring,
									  tmz, &cur_size, &from);
			if (r)
				break;

			r = amdgpu_ttm_map_buffer(dst->bo, dst->mem, &dst_mm, 1, ring,
									  tmz, &cur_size, &to);
			if (r)
				break;

			/* issue copy (parallel when possible) */
			TTM_DO_COPY_PARALLEL(adev, from, to, cur_size,
								 copy_flags, resv, next, r);
			if (r)
				break;

			dma_fence_put(fence);
			fence = next;

			amdgpu_res_next(&src_mm, cur_size);
			amdgpu_res_next(&dst_mm, cur_size);
		}

		mutex_unlock(&adev->mman.gtt_window_lock);

		if (fence_out)
			*fence_out = dma_fence_get(fence);
	dma_fence_put(fence);
	return r;
}

/*
 * amdgpu_move_blit - Copy an entire buffer to another buffer
 *
 * This is a helper called by amdgpu_bo_move() and amdgpu_move_vram_ram() to
 * help move buffers to and from VRAM.
 */
static int amdgpu_move_blit(struct ttm_buffer_object *bo,
							bool evict,
							struct ttm_resource *new_mem,
							struct ttm_resource *old_mem)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_ring   *ring = adev->mman.buffer_funcs_ring;
	struct amdgpu_res_cursor src_mm, dst_mm;
	struct dma_fence *fence = NULL;
	u32 copy_flags = 0;
	int r = 0;

	if (!adev->mman.buffer_funcs_enabled || !ring || !ring->sched.ready)
		return -EINVAL;

	if (amdgpu_bo_encrypted(ttm_to_amdgpu_bo(bo)))
		copy_flags |= AMDGPU_COPY_FLAGS_TMZ;

	amdgpu_res_first(old_mem, 0, bo->base.size, &src_mm);
	amdgpu_res_first(new_mem, 0, bo->base.size, &dst_mm);

	gtt_window_lock_fast(adev);

	while (src_mm.remaining) {
		u64 cur_size = min3(src_mm.size, dst_mm.size, 256ULL << 20);
		u64 from, to;
		struct dma_fence *next = NULL;

		r = amdgpu_ttm_map_buffer(bo, old_mem, &src_mm, 0, ring,
								  amdgpu_bo_encrypted(ttm_to_amdgpu_bo(bo)),
								  &cur_size, &from);
		if (r)
			break;

		r = amdgpu_ttm_map_buffer(bo, new_mem, &dst_mm, 1, ring,
								  amdgpu_bo_encrypted(ttm_to_amdgpu_bo(bo)),
								  &cur_size, &to);
		if (r)
			break;

		TTM_DO_COPY_PARALLEL(adev, from, to, cur_size,
							 copy_flags, bo->base.resv,
					   next, r);
		if (r)
			break;

		dma_fence_put(fence);
		fence = next;

		amdgpu_res_next(&src_mm, cur_size);
		amdgpu_res_next(&dst_mm, cur_size);
	}

	mutex_unlock(&adev->mman.gtt_window_lock);

	if (!r) {
		if (bo->type == ttm_bo_type_kernel)
			r = ttm_bo_move_accel_cleanup(bo, fence,
										  true, false, new_mem);
			else
				r = ttm_bo_move_accel_cleanup(bo, fence,
											  evict, true, new_mem);
	}

	dma_fence_put(fence);
	return r;
}

/**
 * amdgpu_res_cpu_visible - Check that resource can be accessed by CPU
 * @adev: amdgpu device
 * @res: the resource to check
 *
 * Returns: true if the full resource is CPU visible, false otherwise.
 */
bool amdgpu_res_cpu_visible(struct amdgpu_device *adev,
			    struct ttm_resource *res)
{
	struct amdgpu_res_cursor cursor;

	if (!res)
		return false;

	if (res->mem_type == TTM_PL_SYSTEM || res->mem_type == TTM_PL_TT ||
	    res->mem_type == AMDGPU_PL_PREEMPT || res->mem_type == AMDGPU_PL_DOORBELL)
		return true;

	if (res->mem_type != TTM_PL_VRAM)
		return false;

	amdgpu_res_first(res, 0, res->size, &cursor);
	while (cursor.remaining) {
		if ((cursor.start + cursor.size) > adev->gmc.visible_vram_size)
			return false;
		amdgpu_res_next(&cursor, cursor.size);
	}

	return true;
}

/*
 * amdgpu_res_copyable - Check that memory can be accessed by ttm_bo_move_memcpy
 *
 * Called by amdgpu_bo_move()
 */
static bool amdgpu_res_copyable(struct amdgpu_device *adev,
				struct ttm_resource *mem)
{
	if (!amdgpu_res_cpu_visible(adev, mem))
		return false;

	/* ttm_resource_ioremap only supports contiguous memory */
	if (mem->mem_type == TTM_PL_VRAM &&
	    !(mem->placement & TTM_PL_FLAG_CONTIGUOUS))
		return false;

	return true;
}

/**
 * amdgpu_bo_move - move a BO to a new memory location
 * @bo: The buffer object to move.
 * @evict: Whether this move is an eviction.
 * @ctx: The TTM operation context.
 * @new_mem: The new memory resource to move to.
 * @hop: TTM placement hop info for multi-hop moves.
 *
 * This function handles the movement of buffer objects between different
 * memory types. It now uses a dedicated, high-priority scheduler entity
 * that can run on both SDMA rings in parallel, allowing the hardware
 * scheduler to manage the copy with maximum efficiency and throughput.
 *
 * Return: 0 on success, negative error code on failure.
 */
static int amdgpu_bo_move(struct ttm_buffer_object *bo,
						  bool evict,
						  struct ttm_operation_ctx *ctx,
						  struct ttm_resource *new_mem,
						  struct ttm_place *hop)
{
	struct amdgpu_device	*adev    = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_bo	*abo     = ttm_to_amdgpu_bo(bo);
	struct ttm_resource	*old_mem = bo->resource;
	bool			 have_sdma;
	int			 r;

	amdgpu_ttm_auto_enable_buffer_funcs(adev);

	/*
	 * Fast paths that involve no data copy
	 */
	if (new_mem->mem_type == TTM_PL_TT ||
		new_mem->mem_type == AMDGPU_PL_PREEMPT) {
		r = amdgpu_ttm_backend_bind(bo->bdev, bo->ttm, new_mem);
	if (r)
		return r;
		}

		if (!old_mem ||
			(old_mem->mem_type == TTM_PL_SYSTEM && !bo->ttm)) {
			amdgpu_bo_move_notify(bo, evict, new_mem);
		ttm_bo_move_null(bo, new_mem);
	return 0;
			}

			if (old_mem->mem_type == TTM_PL_SYSTEM &&
				(new_mem->mem_type == TTM_PL_TT ||
				new_mem->mem_type == AMDGPU_PL_PREEMPT)) {
				amdgpu_bo_move_notify(bo, evict, new_mem);
			ttm_bo_move_null(bo, new_mem);
			return 0;
				}

				if ((old_mem->mem_type == TTM_PL_TT ||
					old_mem->mem_type == AMDGPU_PL_PREEMPT) &&
					new_mem->mem_type == TTM_PL_SYSTEM) {
					r = ttm_bo_wait_ctx(bo, ctx);
				if (r) {
					return r;
				}

				amdgpu_ttm_backend_unbind(bo->bdev, bo->ttm);
				amdgpu_bo_move_notify(bo, evict, new_mem);
				ttm_resource_free(bo, &bo->resource);
				ttm_bo_assign_mem(bo, new_mem);
				return 0;
					}

					if (old_mem->mem_type == AMDGPU_PL_GDS ||
						old_mem->mem_type == AMDGPU_PL_GWS ||
						old_mem->mem_type == AMDGPU_PL_OA  ||
						old_mem->mem_type == AMDGPU_PL_DOORBELL ||
						new_mem->mem_type == AMDGPU_PL_GDS ||
						new_mem->mem_type == AMDGPU_PL_GWS ||
						new_mem->mem_type == AMDGPU_PL_OA  ||
						new_mem->mem_type == AMDGPU_PL_DOORBELL) {
						amdgpu_bo_move_notify(bo, evict, new_mem);
					ttm_bo_move_null(bo, new_mem);
					return 0;
						}

						if (bo->type == ttm_bo_type_device &&
							new_mem->mem_type == TTM_PL_VRAM &&
							old_mem->mem_type != TTM_PL_VRAM)
							abo->flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

						/* visible-VRAM boundary crossing → temporary TT hop */
						if (adev->mman.buffer_funcs_enabled &&
							((old_mem->mem_type == TTM_PL_SYSTEM &&
							new_mem->mem_type == TTM_PL_VRAM) ||
							(old_mem->mem_type == TTM_PL_VRAM &&
							new_mem->mem_type == TTM_PL_SYSTEM))) {
							hop->fpfn	= 0;
						hop->lpfn	= 0;
						hop->mem_type	= TTM_PL_TT;
						hop->flags	= TTM_PL_FLAG_TEMPORARY;
						return -EMULTIHOP;
							}

							amdgpu_bo_move_notify(bo, evict, new_mem);

							have_sdma = adev->mman.buffer_funcs_enabled;
							if (have_sdma)
								r = amdgpu_move_blit(bo, evict, new_mem, old_mem);
	else
		r = -ENODEV;

	/* memcpy fallback */
	if (r) {
		if (!amdgpu_res_copyable(adev, old_mem) ||
			!amdgpu_res_copyable(adev, new_mem)) {
			pr_err("amdgpu: BO move failed – SDMA unavailable and memcpy not possible\n");
		return r;
			}

			r = ttm_bo_move_memcpy(bo, ctx, new_mem);
			if (r)
				return r;
	}

	if (evict)
		atomic64_inc(&adev->num_evictions);

	atomic64_add(bo->base.size, &adev->num_bytes_moved);
	return 0;
}

/*
 * amdgpu_ttm_io_mem_reserve - Reserve a block of memory during a fault
 *
 * Called by ttm_mem_io_reserve() ultimately via ttm_bo_vm_fault()
 */
static int amdgpu_ttm_io_mem_reserve(struct ttm_device *bdev,
				     struct ttm_resource *mem)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);

	switch (mem->mem_type) {
	case TTM_PL_SYSTEM:
		/* system memory */
		return 0;
	case TTM_PL_TT:
	case AMDGPU_PL_PREEMPT:
		break;
	case TTM_PL_VRAM:
		mem->bus.offset = mem->start << PAGE_SHIFT;

		if (adev->mman.aper_base_kaddr &&
		    mem->placement & TTM_PL_FLAG_CONTIGUOUS)
			mem->bus.addr = (u8 *)adev->mman.aper_base_kaddr +
					mem->bus.offset;

		mem->bus.offset += adev->gmc.aper_base;
		mem->bus.is_iomem = true;
		break;
	case AMDGPU_PL_DOORBELL:
		mem->bus.offset = mem->start << PAGE_SHIFT;
		mem->bus.offset += adev->doorbell.base;
		mem->bus.is_iomem = true;
		mem->bus.caching = ttm_uncached;
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

static unsigned long amdgpu_ttm_io_mem_pfn(struct ttm_buffer_object *bo,
					   unsigned long page_offset)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_res_cursor cursor;

	amdgpu_res_first(bo->resource, (u64)page_offset << PAGE_SHIFT, 0,
			 &cursor);

	if (bo->resource->mem_type == AMDGPU_PL_DOORBELL)
		return ((uint64_t)(adev->doorbell.base + cursor.start)) >> PAGE_SHIFT;

	return (adev->gmc.aper_base + cursor.start) >> PAGE_SHIFT;
}

/**
 * amdgpu_ttm_domain_start - Returns GPU start address
 * @adev: amdgpu device object
 * @type: type of the memory
 *
 * Returns:
 * GPU start address of a memory domain
 */

uint64_t amdgpu_ttm_domain_start(struct amdgpu_device *adev, uint32_t type)
{
	switch (type) {
	case TTM_PL_TT:
		return adev->gmc.gart_start;
	case TTM_PL_VRAM:
		return adev->gmc.vram_start;
	}

	return 0;
}

/*
 * TTM backend functions.
 */
struct amdgpu_ttm_tt {
	struct ttm_tt	ttm;
	struct drm_gem_object	*gobj;
	u64			offset;
	uint64_t		userptr;
	struct task_struct	*usertask;
	uint32_t		userflags;
	bool			bound;
	int32_t			pool_id;
};

#define ttm_to_amdgpu_ttm_tt(ptr)	container_of(ptr, struct amdgpu_ttm_tt, ttm)

#ifdef CONFIG_DRM_AMDGPU_USERPTR
/*
 * amdgpu_ttm_tt_get_user_pages - get device accessible pages that back user
 * memory and start HMM tracking CPU page table update
 *
 * Calling function must call amdgpu_ttm_tt_userptr_range_done() once and only
 * once afterwards to stop HMM tracking
 */
int amdgpu_ttm_tt_get_user_pages(struct amdgpu_bo *bo, struct page **pages,
				 struct hmm_range **range)
{
	struct ttm_tt *ttm = bo->tbo.ttm;
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	unsigned long start = gtt->userptr;
	struct vm_area_struct *vma;
	struct mm_struct *mm;
	bool readonly;
	int r = 0;

	/* Make sure get_user_pages_done() can cleanup gracefully */
	*range = NULL;

	mm = bo->notifier.mm;
	if (unlikely(!mm)) {
		DRM_DEBUG_DRIVER("BO is not registered?\n");
		return -EFAULT;
	}

	if (!mmget_not_zero(mm)) /* Happens during process shutdown */
		return -ESRCH;

	mmap_read_lock(mm);
	vma = vma_lookup(mm, start);
	if (unlikely(!vma)) {
		r = -EFAULT;
		goto out_unlock;
	}
	if (unlikely((gtt->userflags & AMDGPU_GEM_USERPTR_ANONONLY) &&
		vma->vm_file)) {
		r = -EPERM;
		goto out_unlock;
	}

	readonly = amdgpu_ttm_tt_is_readonly(ttm);
	r = amdgpu_hmm_range_get_pages(&bo->notifier, start, ttm->num_pages,
				       readonly, NULL, pages, range);
out_unlock:
	mmap_read_unlock(mm);
	if (r)
		pr_debug("failed %d to get user pages 0x%lx\n", r, start);

	mmput(mm);

	return r;
}

/* amdgpu_ttm_tt_discard_user_pages - Discard range and pfn array allocations
 */
void amdgpu_ttm_tt_discard_user_pages(struct ttm_tt *ttm,
				      struct hmm_range *range)
{
	struct amdgpu_ttm_tt *gtt = (void *)ttm;

	if (gtt && gtt->userptr && range)
		amdgpu_hmm_range_get_pages_done(range);
}

/*
 * amdgpu_ttm_tt_get_user_pages_done - stop HMM track the CPU page table change
 * Check if the pages backing this ttm range have been invalidated
 *
 * Returns: true if pages are still valid
 */
bool amdgpu_ttm_tt_get_user_pages_done(struct ttm_tt *ttm,
				       struct hmm_range *range)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (!gtt || !gtt->userptr || !range)
		return false;

	DRM_DEBUG_DRIVER("user_pages_done 0x%llx pages 0x%x\n",
		gtt->userptr, ttm->num_pages);

	WARN_ONCE(!range->hmm_pfns, "No user pages to check\n");

	return !amdgpu_hmm_range_get_pages_done(range);
}
#endif

/*
 * amdgpu_ttm_tt_set_user_pages - Copy pages in, putting old pages as necessary.
 *
 * Called by amdgpu_cs_list_validate(). This creates the page list
 * that backs user memory and will ultimately be mapped into the device
 * address space.
 */
void amdgpu_ttm_tt_set_user_pages(struct ttm_tt *ttm, struct page **pages)
{
	unsigned long i;

	for (i = 0; i < ttm->num_pages; ++i)
		ttm->pages[i] = pages ? pages[i] : NULL;
}

/*
 * amdgpu_ttm_tt_pin_userptr - prepare the sg table with the user pages
 *
 * Called by amdgpu_ttm_backend_bind()
 **/
static int amdgpu_ttm_tt_pin_userptr(struct ttm_device *bdev,
				     struct ttm_tt *ttm)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	int write = !(gtt->userflags & AMDGPU_GEM_USERPTR_READONLY);
	enum dma_data_direction direction = write ?
		DMA_BIDIRECTIONAL : DMA_TO_DEVICE;
	int r;

	/* Allocate an SG array and squash pages into it */
	r = sg_alloc_table_from_pages(ttm->sg, ttm->pages, ttm->num_pages, 0,
				      (u64)ttm->num_pages << PAGE_SHIFT,
				      GFP_KERNEL);
	if (r)
		goto release_sg;

	/* Map SG to device */
	r = dma_map_sgtable(adev->dev, ttm->sg, direction, 0);
	if (r)
		goto release_sg_table;

	/* convert SG to linear array of pages and dma addresses */
	drm_prime_sg_to_dma_addr_array(ttm->sg, gtt->ttm.dma_address,
				       ttm->num_pages);

	return 0;

release_sg_table:
	sg_free_table(ttm->sg);
release_sg:
	kfree(ttm->sg);
	ttm->sg = NULL;
	return r;
}

/*
 * amdgpu_ttm_tt_unpin_userptr - Unpin and unmap userptr pages
 */
static void amdgpu_ttm_tt_unpin_userptr(struct ttm_device *bdev,
					struct ttm_tt *ttm)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	int write = !(gtt->userflags & AMDGPU_GEM_USERPTR_READONLY);
	enum dma_data_direction direction = write ?
		DMA_BIDIRECTIONAL : DMA_TO_DEVICE;

	/* double check that we don't free the table twice */
	if (!ttm->sg || !ttm->sg->sgl)
		return;

	/* unmap the pages mapped to the device */
	dma_unmap_sgtable(adev->dev, ttm->sg, direction, 0);
	sg_free_table(ttm->sg);
}

/*
 * total_pages is constructed as MQD0+CtrlStack0 + MQD1+CtrlStack1 + ...
 * MQDn+CtrlStackn where n is the number of XCCs per partition.
 * pages_per_xcc is the size of one MQD+CtrlStack. The first page is MQD
 * and uses memory type default, UC. The rest of pages_per_xcc are
 * Ctrl stack and modify their memory type to NC.
 */
static void amdgpu_ttm_gart_bind_gfx9_mqd(struct amdgpu_device *adev,
				struct ttm_tt *ttm, uint64_t flags)
{
	struct amdgpu_ttm_tt *gtt = (void *)ttm;
	uint64_t total_pages = ttm->num_pages;
	int num_xcc = max(1U, adev->gfx.num_xcc_per_xcp);
	uint64_t page_idx, pages_per_xcc;
	int i;
	uint64_t ctrl_flags = AMDGPU_PTE_MTYPE_VG10(flags, AMDGPU_MTYPE_NC);

	pages_per_xcc = total_pages;
	do_div(pages_per_xcc, num_xcc);

	for (i = 0, page_idx = 0; i < num_xcc; i++, page_idx += pages_per_xcc) {
		/* MQD page: use default flags */
		amdgpu_gart_bind(adev,
				gtt->offset + (page_idx << PAGE_SHIFT),
				1, &gtt->ttm.dma_address[page_idx], flags);
		/*
		 * Ctrl pages - modify the memory type to NC (ctrl_flags) from
		 * the second page of the BO onward.
		 */
		amdgpu_gart_bind(adev,
				gtt->offset + ((page_idx + 1) << PAGE_SHIFT),
				pages_per_xcc - 1,
				&gtt->ttm.dma_address[page_idx + 1],
				ctrl_flags);
	}
}

static void amdgpu_ttm_gart_bind(struct amdgpu_device *adev,
				 struct ttm_buffer_object *tbo,
				 uint64_t flags)
{
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(tbo);
	struct ttm_tt *ttm = tbo->ttm;
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (amdgpu_bo_encrypted(abo))
		flags |= AMDGPU_PTE_TMZ;

	if (abo->flags & AMDGPU_GEM_CREATE_CP_MQD_GFX9) {
		amdgpu_ttm_gart_bind_gfx9_mqd(adev, ttm, flags);
	} else {
		amdgpu_gart_bind(adev, gtt->offset, ttm->num_pages,
				 gtt->ttm.dma_address, flags);
	}
	gtt->bound = true;
}

/*
 * amdgpu_ttm_backend_bind - Bind GTT memory
 *
 * Called by ttm_tt_bind() on behalf of ttm_bo_handle_move_mem().
 * This handles binding GTT memory to the device address space.
 */
static int amdgpu_ttm_backend_bind(struct ttm_device *bdev,
				   struct ttm_tt *ttm,
				   struct ttm_resource *bo_mem)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	uint64_t flags;
	int r;

	if (!bo_mem)
		return -EINVAL;

	if (gtt->bound)
		return 0;

	if (gtt->userptr) {
		r = amdgpu_ttm_tt_pin_userptr(bdev, ttm);
		if (r) {
			DRM_ERROR("failed to pin userptr\n");
			return r;
		}
	} else if (ttm->page_flags & TTM_TT_FLAG_EXTERNAL) {
		if (!ttm->sg) {
			struct dma_buf_attachment *attach;
			struct sg_table *sgt;

			attach = gtt->gobj->import_attach;
			sgt = dma_buf_map_attachment(attach, DMA_BIDIRECTIONAL);
			if (IS_ERR(sgt))
				return PTR_ERR(sgt);

			ttm->sg = sgt;
		}

		drm_prime_sg_to_dma_addr_array(ttm->sg, gtt->ttm.dma_address,
					       ttm->num_pages);
	}

	if (!ttm->num_pages) {
		WARN(1, "nothing to bind %u pages for mreg %p back %p!\n",
		     ttm->num_pages, bo_mem, ttm);
	}

	if (bo_mem->mem_type != TTM_PL_TT ||
	    !amdgpu_gtt_mgr_has_gart_addr(bo_mem)) {
		gtt->offset = AMDGPU_BO_INVALID_OFFSET;
		return 0;
	}

	/* compute PTE flags relevant to this BO memory */
	flags = amdgpu_ttm_tt_pte_flags(adev, ttm, bo_mem);

	/* bind pages into GART page tables */
	gtt->offset = (u64)bo_mem->start << PAGE_SHIFT;
	amdgpu_gart_bind(adev, gtt->offset, ttm->num_pages,
			 gtt->ttm.dma_address, flags);
	gtt->bound = true;
	return 0;
}

/*
 * amdgpu_ttm_alloc_gart - Make sure buffer object is accessible either
 * through AGP or GART aperture.
 *
 * If bo is accessible through AGP aperture, then use AGP aperture
 * to access bo; otherwise allocate logical space in GART aperture
 * and map bo to GART aperture.
 */
int amdgpu_ttm_alloc_gart(struct ttm_buffer_object *bo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct ttm_operation_ctx ctx = { false, false };
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(bo->ttm);
	struct ttm_placement placement;
	struct ttm_place placements;
	struct ttm_resource *tmp;
	uint64_t addr, flags;
	int r;

	if (bo->resource->start != AMDGPU_BO_INVALID_OFFSET)
		return 0;

	addr = amdgpu_gmc_agp_addr(bo);
	if (addr != AMDGPU_BO_INVALID_OFFSET)
		return 0;

	/* allocate GART space */
	placement.num_placement = 1;
	placement.placement = &placements;
	placements.fpfn = 0;
	placements.lpfn = adev->gmc.gart_size >> PAGE_SHIFT;
	placements.mem_type = TTM_PL_TT;
	placements.flags = bo->resource->placement;

	r = ttm_bo_mem_space(bo, &placement, &tmp, &ctx);
	if (unlikely(r))
		return r;

	/* compute PTE flags for this buffer object */
	flags = amdgpu_ttm_tt_pte_flags(adev, bo->ttm, tmp);

	/* Bind pages */
	gtt->offset = (u64)tmp->start << PAGE_SHIFT;
	amdgpu_ttm_gart_bind(adev, bo, flags);
	amdgpu_gart_invalidate_tlb(adev);
	ttm_resource_free(bo, &bo->resource);
	ttm_bo_assign_mem(bo, tmp);

	return 0;
}

void amdgpu_ttm_recover_gart(struct ttm_buffer_object *tbo)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(tbo->bdev);
	uint64_t flags;

	if (!tbo->ttm)
		return;

	flags = amdgpu_ttm_tt_pte_flags(adev, tbo->ttm, tbo->resource);
	amdgpu_ttm_gart_bind(adev, tbo, flags);

	/* Re‑arm SDMA buffer functions after GPU reset. */
	amdgpu_ttm_auto_enable_buffer_funcs(adev);
}

/*
 * amdgpu_ttm_backend_unbind - Unbind GTT mapped pages
 *
 * Called by ttm_tt_unbind() on behalf of ttm_bo_move_ttm() and
 * ttm_tt_destroy().
 */
static void amdgpu_ttm_backend_unbind(struct ttm_device *bdev,
				      struct ttm_tt *ttm)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	/* if the pages have userptr pinning then clear that first */
	if (gtt->userptr) {
		amdgpu_ttm_tt_unpin_userptr(bdev, ttm);
	} else if (ttm->sg && gtt->gobj->import_attach) {
		struct dma_buf_attachment *attach;

		attach = gtt->gobj->import_attach;
		dma_buf_unmap_attachment(attach, ttm->sg, DMA_BIDIRECTIONAL);
		ttm->sg = NULL;
	}

	if (!gtt->bound)
		return;

	if (gtt->offset == AMDGPU_BO_INVALID_OFFSET)
		return;

	/* unbind shouldn't be done for GDS/GWS/OA in ttm_bo_clean_mm */
	amdgpu_gart_unbind(adev, gtt->offset, ttm->num_pages);
	gtt->bound = false;
}

static void amdgpu_ttm_backend_destroy(struct ttm_device *bdev,
				       struct ttm_tt *ttm)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (gtt->usertask)
		put_task_struct(gtt->usertask);

	ttm_tt_fini(&gtt->ttm);
	kfree(gtt);
}

/**
 * amdgpu_ttm_tt_create - Create a ttm_tt object for a given BO
 *
 * @bo: The buffer object to create a GTT ttm_tt object around
 * @page_flags: Page flags to be added to the ttm_tt object
 *
 * Called by ttm_tt_create().
 */
static struct ttm_tt *amdgpu_ttm_tt_create(struct ttm_buffer_object *bo,
					   uint32_t page_flags)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->bdev);
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	struct amdgpu_ttm_tt *gtt;
	enum ttm_caching caching;

	gtt = kzalloc(sizeof(struct amdgpu_ttm_tt), GFP_KERNEL);
	if (!gtt)
		return NULL;

	gtt->gobj = &bo->base;
	if (adev->gmc.mem_partitions && abo->xcp_id >= 0)
		gtt->pool_id = KFD_XCP_MEM_ID(adev, abo->xcp_id);
	else
		gtt->pool_id = abo->xcp_id;

	if (abo->flags & AMDGPU_GEM_CREATE_CPU_GTT_USWC)
		caching = ttm_write_combined;
	else
		caching = ttm_cached;

	/* allocate space for the uninitialized page entries */
	if (ttm_sg_tt_init(&gtt->ttm, bo, page_flags, caching)) {
		kfree(gtt);
		return NULL;
	}
	return &gtt->ttm;
}

/*
 * amdgpu_ttm_tt_populate - Map GTT pages visible to the device
 *
 * Map the pages of a ttm_tt object to an address space visible
 * to the underlying device.
 */
static int amdgpu_ttm_tt_populate(struct ttm_device *bdev,
				  struct ttm_tt *ttm,
				  struct ttm_operation_ctx *ctx)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bdev);
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	struct ttm_pool *pool;
	pgoff_t i;
	int ret;

	/* user pages are bound by amdgpu_ttm_tt_pin_userptr() */
	if (gtt->userptr) {
		ttm->sg = kzalloc(sizeof(struct sg_table), GFP_KERNEL);
		if (!ttm->sg)
			return -ENOMEM;
		return 0;
	}

	if (ttm->page_flags & TTM_TT_FLAG_EXTERNAL)
		return 0;

	if (adev->mman.ttm_pools && gtt->pool_id >= 0)
		pool = &adev->mman.ttm_pools[gtt->pool_id];
	else
		pool = &adev->mman.bdev.pool;
	ret = ttm_pool_alloc(pool, ttm, ctx);
	if (ret)
		return ret;

	for (i = 0; i < ttm->num_pages; ++i)
		ttm->pages[i]->mapping = bdev->dev_mapping;

	return 0;
}

/*
 * amdgpu_ttm_tt_unpopulate - unmap GTT pages and unpopulate page arrays
 *
 * Unmaps pages of a ttm_tt object from the device address space and
 * unpopulates the page array backing it.
 */
static void amdgpu_ttm_tt_unpopulate(struct ttm_device *bdev,
				     struct ttm_tt *ttm)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	struct amdgpu_device *adev;
	struct ttm_pool *pool;
	pgoff_t i;

	amdgpu_ttm_backend_unbind(bdev, ttm);

	if (gtt->userptr) {
		amdgpu_ttm_tt_set_user_pages(ttm, NULL);
		kfree(ttm->sg);
		ttm->sg = NULL;
		return;
	}

	if (ttm->page_flags & TTM_TT_FLAG_EXTERNAL)
		return;

	for (i = 0; i < ttm->num_pages; ++i)
		ttm->pages[i]->mapping = NULL;

	adev = amdgpu_ttm_adev(bdev);

	if (adev->mman.ttm_pools && gtt->pool_id >= 0)
		pool = &adev->mman.ttm_pools[gtt->pool_id];
	else
		pool = &adev->mman.bdev.pool;

	return ttm_pool_free(pool, ttm);
}

/**
 * amdgpu_ttm_tt_get_userptr - Return the userptr GTT ttm_tt for the current
 * task
 *
 * @tbo: The ttm_buffer_object that contains the userptr
 * @user_addr:  The returned value
 */
int amdgpu_ttm_tt_get_userptr(const struct ttm_buffer_object *tbo,
			      uint64_t *user_addr)
{
	struct amdgpu_ttm_tt *gtt;

	if (!tbo->ttm)
		return -EINVAL;

	gtt = (void *)tbo->ttm;
	*user_addr = gtt->userptr;
	return 0;
}

/**
 * amdgpu_ttm_tt_set_userptr - Initialize userptr GTT ttm_tt for the current
 * task
 *
 * @bo: The ttm_buffer_object to bind this userptr to
 * @addr:  The address in the current tasks VM space to use
 * @flags: Requirements of userptr object.
 *
 * Called by amdgpu_gem_userptr_ioctl() and kfd_ioctl_alloc_memory_of_gpu() to
 * bind userptr pages to current task and by kfd_ioctl_acquire_vm() to
 * initialize GPU VM for a KFD process.
 */
int amdgpu_ttm_tt_set_userptr(struct ttm_buffer_object *bo,
			      uint64_t addr, uint32_t flags)
{
	struct amdgpu_ttm_tt *gtt;

	if (!bo->ttm) {
		/* TODO: We want a separate TTM object type for userptrs */
		bo->ttm = amdgpu_ttm_tt_create(bo, 0);
		if (bo->ttm == NULL)
			return -ENOMEM;
	}

	/* Set TTM_TT_FLAG_EXTERNAL before populate but after create. */
	bo->ttm->page_flags |= TTM_TT_FLAG_EXTERNAL;

	gtt = ttm_to_amdgpu_ttm_tt(bo->ttm);
	gtt->userptr = addr;
	gtt->userflags = flags;

	if (gtt->usertask)
		put_task_struct(gtt->usertask);
	gtt->usertask = current->group_leader;
	get_task_struct(gtt->usertask);

	return 0;
}

/*
 * amdgpu_ttm_tt_get_usermm - Return memory manager for ttm_tt object
 */
struct mm_struct *amdgpu_ttm_tt_get_usermm(struct ttm_tt *ttm)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (gtt == NULL)
		return NULL;

	if (gtt->usertask == NULL)
		return NULL;

	return gtt->usertask->mm;
}

/*
 * amdgpu_ttm_tt_affect_userptr - Determine if a ttm_tt object lays inside an
 * address range for the current task.
 *
 */
bool amdgpu_ttm_tt_affect_userptr(struct ttm_tt *ttm, unsigned long start,
				  unsigned long end, unsigned long *userptr)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);
	unsigned long size;

	if (gtt == NULL || !gtt->userptr)
		return false;

	/* Return false if no part of the ttm_tt object lies within
	 * the range
	 */
	size = (unsigned long)gtt->ttm.num_pages * PAGE_SIZE;
	if (gtt->userptr > end || gtt->userptr + size <= start)
		return false;

	if (userptr)
		*userptr = gtt->userptr;
	return true;
}

/*
 * amdgpu_ttm_tt_is_userptr - Have the pages backing by userptr?
 */
bool amdgpu_ttm_tt_is_userptr(struct ttm_tt *ttm)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (gtt == NULL || !gtt->userptr)
		return false;

	return true;
}

/*
 * amdgpu_ttm_tt_is_readonly - Is the ttm_tt object read only?
 */
bool amdgpu_ttm_tt_is_readonly(struct ttm_tt *ttm)
{
	struct amdgpu_ttm_tt *gtt = ttm_to_amdgpu_ttm_tt(ttm);

	if (gtt == NULL)
		return false;

	return !!(gtt->userflags & AMDGPU_GEM_USERPTR_READONLY);
}

/**
 * amdgpu_ttm_tt_pde_flags - Compute PDE flags for ttm_tt object
 *
 * @ttm: The ttm_tt object to compute the flags for
 * @mem: The memory registry backing this ttm_tt object
 *
 * Figure out the flags to use for a VM PDE (Page Directory Entry).
 */
uint64_t amdgpu_ttm_tt_pde_flags(struct ttm_tt *ttm, struct ttm_resource *mem)
{
	uint64_t flags = 0;

	if (mem && mem->mem_type != TTM_PL_SYSTEM)
		flags |= AMDGPU_PTE_VALID;

	if (mem && (mem->mem_type == TTM_PL_TT ||
		    mem->mem_type == AMDGPU_PL_DOORBELL ||
		    mem->mem_type == AMDGPU_PL_PREEMPT)) {
		flags |= AMDGPU_PTE_SYSTEM;

		if (ttm->caching == ttm_cached)
			flags |= AMDGPU_PTE_SNOOPED;
	}

	if (mem && mem->mem_type == TTM_PL_VRAM &&
			mem->bus.caching == ttm_cached)
		flags |= AMDGPU_PTE_SNOOPED;

	return flags;
}

/**
 * amdgpu_ttm_tt_pte_flags - Compute PTE flags for ttm_tt object
 *
 * @adev: amdgpu_device pointer
 * @ttm: The ttm_tt object to compute the flags for
 * @mem: The memory registry backing this ttm_tt object
 *
 * Figure out the flags to use for a VM PTE (Page Table Entry).
 */
uint64_t amdgpu_ttm_tt_pte_flags(struct amdgpu_device *adev, struct ttm_tt *ttm,
				 struct ttm_resource *mem)
{
	uint64_t flags = amdgpu_ttm_tt_pde_flags(ttm, mem);

	flags |= adev->gart.gart_pte_flags;
	flags |= AMDGPU_PTE_READABLE;

	if (!amdgpu_ttm_tt_is_readonly(ttm))
		flags |= AMDGPU_PTE_WRITEABLE;

	return flags;
}

/*
 * amdgpu_ttm_bo_eviction_valuable - Check to see if we can evict a buffer
 * object.
 *
 * Return true if eviction is sensible. Called by ttm_mem_evict_first() on
 * behalf of ttm_bo_mem_force_space() which tries to evict buffer objects until
 * it can find space for a new object and by ttm_bo_force_list_clean() which is
 * used to clean out a memory space.
 */
static bool amdgpu_ttm_bo_eviction_valuable(struct ttm_buffer_object *bo,
					    const struct ttm_place *place)
{
	struct dma_resv_iter resv_cursor;
	struct dma_fence *f;

	if (!amdgpu_bo_is_amdgpu_bo(bo))
		return ttm_bo_eviction_valuable(bo, place);

	/* Swapout? */
	if (bo->resource->mem_type == TTM_PL_SYSTEM)
		return true;

	if (bo->type == ttm_bo_type_kernel &&
	    !amdgpu_vm_evictable(ttm_to_amdgpu_bo(bo)))
		return false;

	/* If bo is a KFD BO, check if the bo belongs to the current process.
	 * If true, then return false as any KFD process needs all its BOs to
	 * be resident to run successfully
	 */
	dma_resv_for_each_fence(&resv_cursor, bo->base.resv,
				DMA_RESV_USAGE_BOOKKEEP, f) {
		if (amdkfd_fence_check_mm(f, current->mm) &&
		    !(place->flags & TTM_PL_FLAG_CONTIGUOUS))
			return false;
	}

	/* Preemptible BOs don't own system resources managed by the
	 * driver (pages, VRAM, GART space). They point to resources
	 * owned by someone else (e.g. pageable memory in user mode
	 * or a DMABuf). They are used in a preemptible context so we
	 * can guarantee no deadlocks and good QoS in case of MMU
	 * notifiers or DMABuf move notifiers from the resource owner.
	 */
	if (bo->resource->mem_type == AMDGPU_PL_PREEMPT)
		return false;

	if (bo->resource->mem_type == TTM_PL_TT &&
	    amdgpu_bo_encrypted(ttm_to_amdgpu_bo(bo)))
		return false;

	return ttm_bo_eviction_valuable(bo, place);
}

static void amdgpu_ttm_vram_mm_access(struct amdgpu_device *adev, loff_t pos,
				      void *buf, size_t size, bool write)
{
	while (size) {
		uint64_t aligned_pos = ALIGN_DOWN(pos, 4);
		uint64_t bytes = 4 - (pos & 0x3);
		uint32_t shift = (pos & 0x3) * 8;
		uint32_t mask = 0xffffffff << shift;
		uint32_t value = 0;

		if (size < bytes) {
			mask &= 0xffffffff >> (bytes - size) * 8;
			bytes = size;
		}

		if (mask != 0xffffffff) {
			amdgpu_device_mm_access(adev, aligned_pos, &value, 4, false);
			if (write) {
				value &= ~mask;
				value |= (*(uint32_t *)buf << shift) & mask;
				amdgpu_device_mm_access(adev, aligned_pos, &value, 4, true);
			} else {
				value = (value & mask) >> shift;
				memcpy(buf, &value, bytes);
			}
		} else {
			amdgpu_device_mm_access(adev, aligned_pos, buf, 4, write);
		}

		pos += bytes;
		buf += bytes;
		size -= bytes;
	}
}

static int amdgpu_ttm_access_memory_sdma(struct ttm_buffer_object *bo,
					unsigned long offset, void *buf,
					int len, int write)
{
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct amdgpu_res_cursor src_mm;
	struct amdgpu_job *job;
	struct dma_fence *fence;
	uint64_t src_addr, dst_addr;
	unsigned int num_dw;
	int r, idx;

	if (len != PAGE_SIZE)
		return -EINVAL;

	if (!adev->mman.sdma_access_ptr)
		return -EACCES;

	if (!drm_dev_enter(adev_to_drm(adev), &idx))
		return -ENODEV;

	if (write)
		memcpy(adev->mman.sdma_access_ptr, buf, len);

	num_dw = ALIGN(adev->mman.buffer_funcs->copy_num_dw, 8);
	r = amdgpu_job_alloc_with_ib(adev, &adev->mman.high_pr,
				     AMDGPU_FENCE_OWNER_UNDEFINED,
				     num_dw * 4, AMDGPU_IB_POOL_DELAYED,
				     &job);
	if (r)
		goto out;

	amdgpu_res_first(abo->tbo.resource, offset, len, &src_mm);
	src_addr = amdgpu_ttm_domain_start(adev, bo->resource->mem_type) +
		src_mm.start;
	dst_addr = amdgpu_bo_gpu_offset(adev->mman.sdma_access_bo);
	if (write)
		swap(src_addr, dst_addr);

	amdgpu_emit_copy_buffer(adev, &job->ibs[0], src_addr, dst_addr,
				PAGE_SIZE, 0);

	amdgpu_ring_pad_ib(adev->mman.buffer_funcs_ring, &job->ibs[0]);
	WARN_ON(job->ibs[0].length_dw > num_dw);

	fence = amdgpu_job_submit(job);

	if (!dma_fence_wait_timeout(fence, false, adev->sdma_timeout))
		r = -ETIMEDOUT;
	dma_fence_put(fence);

	if (!(r || write))
		memcpy(buf, adev->mman.sdma_access_ptr, len);
out:
	drm_dev_exit(idx);
	return r;
}

/**
 * amdgpu_ttm_access_memory - Read or Write memory that backs a buffer object.
 *
 * @bo:  The buffer object to read/write
 * @offset:  Offset into buffer object
 * @buf:  Secondary buffer to write/read from
 * @len: Length in bytes of access
 * @write:  true if writing
 *
 * This is used to access VRAM that backs a buffer object via MMIO
 * access for debugging purposes.
 */
static int amdgpu_ttm_access_memory(struct ttm_buffer_object *bo,
				    unsigned long offset, void *buf, int len,
				    int write)
{
	struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct amdgpu_res_cursor cursor;
	int ret = 0;

	if (bo->resource->mem_type != TTM_PL_VRAM)
		return -EIO;

	if (amdgpu_device_has_timeouts_enabled(adev) &&
			!amdgpu_ttm_access_memory_sdma(bo, offset, buf, len, write))
		return len;

	amdgpu_res_first(bo->resource, offset, len, &cursor);
	while (cursor.remaining) {
		size_t count, size = cursor.size;
		loff_t pos = cursor.start;

		count = amdgpu_device_aper_access(adev, pos, buf, size, write);
		size -= count;
		if (size) {
			/* using MM to access rest vram and handle un-aligned address */
			pos += count;
			buf += count;
			amdgpu_ttm_vram_mm_access(adev, pos, buf, size, write);
		}

		ret += cursor.size;
		buf += cursor.size;
		amdgpu_res_next(&cursor, cursor.size);
	}

	return ret;
}

static void
amdgpu_bo_delete_mem_notify(struct ttm_buffer_object *bo)
{
	amdgpu_bo_move_notify(bo, false, NULL);
}

static struct ttm_device_funcs amdgpu_bo_driver = {
	.ttm_tt_create = &amdgpu_ttm_tt_create,
	.ttm_tt_populate = &amdgpu_ttm_tt_populate,
	.ttm_tt_unpopulate = &amdgpu_ttm_tt_unpopulate,
	.ttm_tt_destroy = &amdgpu_ttm_backend_destroy,
	.eviction_valuable = amdgpu_ttm_bo_eviction_valuable,
	.evict_flags = &amdgpu_evict_flags,
	.move = &amdgpu_bo_move,
	.delete_mem_notify = &amdgpu_bo_delete_mem_notify,
	.release_notify = &amdgpu_bo_release_notify,
	.io_mem_reserve = &amdgpu_ttm_io_mem_reserve,
	.io_mem_pfn = amdgpu_ttm_io_mem_pfn,
	.access_memory = &amdgpu_ttm_access_memory,
};

/*
 * Firmware Reservation functions
 */
/**
 * amdgpu_ttm_fw_reserve_vram_fini - free fw reserved vram
 *
 * @adev: amdgpu_device pointer
 *
 * free fw reserved vram if it has been reserved.
 */
static void amdgpu_ttm_fw_reserve_vram_fini(struct amdgpu_device *adev)
{
	amdgpu_bo_free_kernel(&adev->mman.fw_vram_usage_reserved_bo,
		NULL, &adev->mman.fw_vram_usage_va);
}

/*
 * Driver Reservation functions
 */
/**
 * amdgpu_ttm_drv_reserve_vram_fini - free drv reserved vram
 *
 * @adev: amdgpu_device pointer
 *
 * free drv reserved vram if it has been reserved.
 */
static void amdgpu_ttm_drv_reserve_vram_fini(struct amdgpu_device *adev)
{
	amdgpu_bo_free_kernel(&adev->mman.drv_vram_usage_reserved_bo,
						  NULL,
						  &adev->mman.drv_vram_usage_va);
}

/**
 * amdgpu_ttm_fw_reserve_vram_init - create bo vram reservation from fw
 *
 * @adev: amdgpu_device pointer
 *
 * create bo vram reservation from fw.
 */
static int amdgpu_ttm_fw_reserve_vram_init(struct amdgpu_device *adev)
{
	uint64_t vram_size = adev->gmc.visible_vram_size;

	adev->mman.fw_vram_usage_va = NULL;
	adev->mman.fw_vram_usage_reserved_bo = NULL;

	if (adev->mman.fw_vram_usage_size == 0 ||
	    adev->mman.fw_vram_usage_size > vram_size)
		return 0;

	return amdgpu_bo_create_kernel_at(adev,
					  adev->mman.fw_vram_usage_start_offset,
					  adev->mman.fw_vram_usage_size,
					  &adev->mman.fw_vram_usage_reserved_bo,
					  &adev->mman.fw_vram_usage_va);
}

/**
 * amdgpu_ttm_drv_reserve_vram_init - create bo vram reservation from driver
 *
 * @adev: amdgpu_device pointer
 *
 * create bo vram reservation from drv.
 */
static int amdgpu_ttm_drv_reserve_vram_init(struct amdgpu_device *adev)
{
	u64 vram_size = adev->gmc.visible_vram_size;

	adev->mman.drv_vram_usage_va = NULL;
	adev->mman.drv_vram_usage_reserved_bo = NULL;

	if (adev->mman.drv_vram_usage_size == 0 ||
	    adev->mman.drv_vram_usage_size > vram_size)
		return 0;

	return amdgpu_bo_create_kernel_at(adev,
					  adev->mman.drv_vram_usage_start_offset,
					  adev->mman.drv_vram_usage_size,
					  &adev->mman.drv_vram_usage_reserved_bo,
					  &adev->mman.drv_vram_usage_va);
}

/*
 * Memoy training reservation functions
 */

/**
 * amdgpu_ttm_training_reserve_vram_fini - free memory training reserved vram
 *
 * @adev: amdgpu_device pointer
 *
 * free memory training reserved vram if it has been reserved.
 */
static int amdgpu_ttm_training_reserve_vram_fini(struct amdgpu_device *adev)
{
	struct psp_memory_training_context *ctx = &adev->psp.mem_train_ctx;

	ctx->init = PSP_MEM_TRAIN_NOT_SUPPORT;
	amdgpu_bo_free_kernel(&ctx->c2p_bo, NULL, NULL);
	ctx->c2p_bo = NULL;

	return 0;
}

static void amdgpu_ttm_training_data_block_init(struct amdgpu_device *adev,
						uint32_t reserve_size)
{
	struct psp_memory_training_context *ctx = &adev->psp.mem_train_ctx;

	memset(ctx, 0, sizeof(*ctx));

	ctx->c2p_train_data_offset =
		ALIGN((adev->gmc.mc_vram_size - reserve_size - SZ_1M), SZ_1M);
	ctx->p2c_train_data_offset =
		(adev->gmc.mc_vram_size - GDDR6_MEM_TRAINING_OFFSET);
	ctx->train_data_size =
		GDDR6_MEM_TRAINING_DATA_SIZE_IN_BYTES;

	DRM_DEBUG("train_data_size:%llx,p2c_train_data_offset:%llx,c2p_train_data_offset:%llx.\n",
			ctx->train_data_size,
			ctx->p2c_train_data_offset,
			ctx->c2p_train_data_offset);
}

/*
 * reserve TMR memory at the top of VRAM which holds
 * IP Discovery data and is protected by PSP.
 */
static int amdgpu_ttm_reserve_tmr(struct amdgpu_device *adev)
{
	struct psp_memory_training_context *ctx = &adev->psp.mem_train_ctx;
	bool mem_train_support = false;
	uint32_t reserve_size = 0;
	int ret;

	if (adev->bios && !amdgpu_sriov_vf(adev)) {
		if (amdgpu_atomfirmware_mem_training_supported(adev))
			mem_train_support = true;
		else
			DRM_DEBUG("memory training does not support!\n");
	}

	/*
	 * Query reserved tmr size through atom firmwareinfo for Sienna_Cichlid and onwards for all
	 * the use cases (IP discovery/G6 memory training/profiling/diagnostic data.etc)
	 *
	 * Otherwise, fallback to legacy approach to check and reserve tmr block for ip
	 * discovery data and G6 memory training data respectively
	 */
	if (adev->bios)
		reserve_size =
			amdgpu_atomfirmware_get_fw_reserved_fb_size(adev);

	if (!adev->bios &&
	    (amdgpu_ip_version(adev, GC_HWIP, 0) == IP_VERSION(9, 4, 3) ||
	     amdgpu_ip_version(adev, GC_HWIP, 0) == IP_VERSION(9, 4, 4) ||
	     amdgpu_ip_version(adev, GC_HWIP, 0) == IP_VERSION(9, 5, 0)))
		reserve_size = max(reserve_size, (uint32_t)280 << 20);
	else if (!reserve_size)
		reserve_size = DISCOVERY_TMR_OFFSET;

	if (mem_train_support) {
		/* reserve vram for mem train according to TMR location */
		amdgpu_ttm_training_data_block_init(adev, reserve_size);
		ret = amdgpu_bo_create_kernel_at(adev,
						 ctx->c2p_train_data_offset,
						 ctx->train_data_size,
						 &ctx->c2p_bo,
						 NULL);
		if (ret) {
			DRM_ERROR("alloc c2p_bo failed(%d)!\n", ret);
			amdgpu_ttm_training_reserve_vram_fini(adev);
			return ret;
		}
		ctx->init = PSP_MEM_TRAIN_RESERVE_SUCCESS;
	}

	if (!adev->gmc.is_app_apu) {
		ret = amdgpu_bo_create_kernel_at(
			adev, adev->gmc.real_vram_size - reserve_size,
			reserve_size, &adev->mman.fw_reserved_memory, NULL);
		if (ret) {
			DRM_ERROR("alloc tmr failed(%d)!\n", ret);
			amdgpu_bo_free_kernel(&adev->mman.fw_reserved_memory,
					      NULL, NULL);
			return ret;
		}
	} else {
		DRM_DEBUG_DRIVER("backdoor fw loading path for PSP TMR, no reservation needed\n");
	}

	return 0;
}

static int amdgpu_ttm_pools_init(struct amdgpu_device *adev)
{
	int i;

	if (!adev->gmc.is_app_apu || !adev->gmc.num_mem_partitions)
		return 0;

	adev->mman.ttm_pools = kcalloc(adev->gmc.num_mem_partitions,
				       sizeof(*adev->mman.ttm_pools),
				       GFP_KERNEL);
	if (!adev->mman.ttm_pools)
		return -ENOMEM;

	for (i = 0; i < adev->gmc.num_mem_partitions; i++) {
		ttm_pool_init(&adev->mman.ttm_pools[i], adev->dev,
			      adev->gmc.mem_partitions[i].numa.node,
			      false, false);
	}
	return 0;
}

static void amdgpu_ttm_pools_fini(struct amdgpu_device *adev)
{
	int i;

	if (!adev->gmc.is_app_apu || !adev->mman.ttm_pools)
		return;

	for (i = 0; i < adev->gmc.num_mem_partitions; i++)
		ttm_pool_fini(&adev->mman.ttm_pools[i]);

	kfree(adev->mman.ttm_pools);
	adev->mman.ttm_pools = NULL;
}

/*
 * amdgpu_ttm_init - Init the memory management (ttm) as well as various
 * gtt/vram related fields.
 *
 * This initializes all of the memory space pools that the TTM layer
 * will need such as the GTT space (system memory mapped to the device),
 * VRAM (on-board memory), and on-chip memories (GDS, GWS, OA) which
 * can be mapped per VMID.
 */
int amdgpu_ttm_init(struct amdgpu_device *adev)
{
	uint64_t gtt_size;
	int r, i;

	/* ----------------------------------------------------------------
	 * Generic TTM & pool initialisation
	 * ----------------------------------------------------------------
	 */
	mutex_init(&adev->mman.gtt_window_lock);
	dma_set_max_seg_size(adev->dev, UINT_MAX);

	r = ttm_device_init(&adev->mman.bdev, &amdgpu_bo_driver,
						adev->dev,
					 adev_to_drm(adev)->anon_inode->i_mapping,
						adev_to_drm(adev)->vma_offset_manager,
						adev->need_swiotlb,
					 dma_addressing_limited(adev->dev));
	if (r) {
		DRM_ERROR("TTM: device-init failed (%d)\n", r);
		return r;
	}

	r = amdgpu_ttm_pools_init(adev);
	if (r) {
		DRM_ERROR("TTM: pool-init failed (%d)\n", r);
		return r;
	}
	adev->mman.initialized = true;

	/* ----------------------------------------------------------------
	 * VRAM / GTT managers
	 * ----------------------------------------------------------------
	 */
	r = amdgpu_vram_mgr_init(adev);
	if (r)
		return r;

	amdgpu_ttm_set_buffer_funcs_status(adev, false);

	#ifdef CONFIG_64BIT
	#ifdef CONFIG_X86
	if (adev->gmc.xgmi.connected_to_cpu)
		adev->mman.aper_base_kaddr =
		ioremap_cache(adev->gmc.aper_base,
					  adev->gmc.visible_vram_size);
		else if (!adev->gmc.is_app_apu)
			adev->mman.aper_base_kaddr =
			ioremap_wc(adev->gmc.aper_base,
					   adev->gmc.visible_vram_size);
			#endif
			#endif

			/* reserved VRAM regions (fw, driver, TMR, stolen …) */
			if (amdgpu_ttm_fw_reserve_vram_init(adev))
				return -ENOMEM;
	if (amdgpu_ttm_drv_reserve_vram_init(adev))
		return -ENOMEM;
	if (adev->mman.discovery_bin && amdgpu_ttm_reserve_tmr(adev))
		return -ENOMEM;
	if (!adev->gmc.is_app_apu) {
		if (amdgpu_bo_create_kernel_at(adev, 0,
			adev->mman.stolen_vga_size,
			&adev->mman.stolen_vga_memory,
			NULL))
			return -ENOMEM;
		if (amdgpu_bo_create_kernel_at(adev, adev->mman.stolen_vga_size,
			adev->mman.stolen_extended_size,
			&adev->mman.stolen_extended_memory,
			NULL))
			return -ENOMEM;
		if (amdgpu_bo_create_kernel_at(adev,
			adev->mman.stolen_reserved_offset,
			adev->mman.stolen_reserved_size,
			&adev->mman.stolen_reserved_memory,
			NULL))
			return -ENOMEM;
	}

	DRM_INFO("amdgpu: %uM of VRAM ready\n",
			 (unsigned)(adev->gmc.real_vram_size >> 20));

	/* ---------------- GTT size ------------------------------------- */
	gtt_size = (amdgpu_gtt_size == -1) ?
	(u64)ttm_tt_pages_limit() << PAGE_SHIFT :
	((u64)amdgpu_gtt_size << 20);

	r = amdgpu_gtt_mgr_init(adev, gtt_size);
	if (r)
		return r;

	DRM_INFO("amdgpu: %uM of GTT ready\n",
			 (unsigned)(gtt_size >> 20));

	/* ---------------- on-chip heaps, doorbells, preempt ------------ */
	if (amdgpu_ttm_init_on_chip(adev, AMDGPU_PL_DOORBELL,
		adev->doorbell.size >> PAGE_SHIFT))
		return -ENOMEM;

	if (amdgpu_doorbell_create_kernel_doorbells(adev))
		return -ENOMEM;

	if (amdgpu_preempt_mgr_init(adev))
		return -ENOMEM;

	if (amdgpu_ttm_init_on_chip(adev, AMDGPU_PL_GDS, adev->gds.gds_size) ||
		amdgpu_ttm_init_on_chip(adev, AMDGPU_PL_GWS, adev->gds.gws_size) ||
		amdgpu_ttm_init_on_chip(adev, AMDGPU_PL_OA,  adev->gds.oa_size))
		return -ENOMEM;

	/* ---------------- debug helper BO for SDMA VRAM access --------- */
	if (amdgpu_bo_create_kernel(adev, PAGE_SIZE, PAGE_SIZE,
		AMDGPU_GEM_DOMAIN_GTT,
		&adev->mman.sdma_access_bo, NULL,
		&adev->mman.sdma_access_ptr))
		DRM_WARN("Debug VRAM access will use slowpath MM access\n");

	/* ----------------------------------------------------------------
	 * Enable buffer-funcs and set up copy scheduler entity
	 * ----------------------------------------------------------------
	 */
	amdgpu_ttm_auto_enable_buffer_funcs(adev);

	if (adev->mman.buffer_funcs_enabled &&
		adev->sdma.num_instances >= 1) {

		amdgpu_ttm_scheds = kcalloc(adev->sdma.num_instances,
									sizeof(*amdgpu_ttm_scheds),
									GFP_KERNEL);
		if (!amdgpu_ttm_scheds) {
			DRM_WARN("TTM: no memory for SDMA entity array – "
			"parallel copies disabled\n");
			goto done;	/* still succeed with single SDMA */
		}

		for (i = 0; i < adev->sdma.num_instances; ++i)
			amdgpu_ttm_scheds[i] =
			&adev->sdma.instance[i].ring.sched;

		r = drm_sched_entity_init(&amdgpu_ttm_sched_entity,
								  DRM_SCHED_PRIORITY_HIGH,
							amdgpu_ttm_scheds,
							adev->sdma.num_instances, NULL);
		if (r) {
			kfree(amdgpu_ttm_scheds);
			amdgpu_ttm_scheds = NULL;
			DRM_WARN("TTM: parallel SDMA disabled "
			"(entity-init failed %d)\n", r);
		} else {
			amdgpu_ttm_entity_initialized = true;
			DRM_INFO("TTM: parallel SDMA (%u engines) enabled\n",
					 adev->sdma.num_instances);
		}
		}

		done:
		return 0;
}

/*
 * amdgpu_ttm_fini - De-initialize the TTM memory pools
 */
void amdgpu_ttm_fini(struct amdgpu_device *adev)
{
	int idx;

	if (!adev->mman.initialized)
		return;

	/* Per-partition page-pools (APUs / XGMI) */
	amdgpu_ttm_pools_fini(adev);
	amdgpu_ttm_training_reserve_vram_fini(adev);

	/* Free stolen and reserved VRAM regions (dGPU only) */
	if (!adev->gmc.is_app_apu) {
		amdgpu_bo_free_kernel(&adev->mman.stolen_vga_memory,
							  NULL, NULL);
		amdgpu_bo_free_kernel(&adev->mman.stolen_extended_memory,
							  NULL, NULL);
		amdgpu_bo_free_kernel(&adev->mman.fw_reserved_memory,
							  NULL, NULL);
		if (adev->mman.stolen_reserved_size)
			amdgpu_bo_free_kernel(&adev->mman.stolen_reserved_memory,
								  NULL, NULL);
	}

	/* SDMA access helper buffer                                   */
	amdgpu_bo_free_kernel(&adev->mman.sdma_access_bo, NULL,
						  &adev->mman.sdma_access_ptr);

	/* Reserved VRAM regions from firmware & driver parameters     */
	amdgpu_ttm_fw_reserve_vram_fini(adev);
	amdgpu_ttm_drv_reserve_vram_fini(adev);

	/* I/O-remapped visible VRAM aperture                          */
	if (drm_dev_enter(adev_to_drm(adev), &idx)) {
		if (adev->mman.aper_base_kaddr)
			iounmap(adev->mman.aper_base_kaddr);
		adev->mman.aper_base_kaddr = NULL;
		drm_dev_exit(idx);
	}

	/* Resource managers                                            */
	amdgpu_vram_mgr_fini(adev);
	amdgpu_gtt_mgr_fini(adev);
	amdgpu_preempt_mgr_fini(adev);
	ttm_range_man_fini(&adev->mman.bdev, AMDGPU_PL_GDS);
	ttm_range_man_fini(&adev->mman.bdev, AMDGPU_PL_GWS);
	ttm_range_man_fini(&adev->mman.bdev, AMDGPU_PL_OA);
	ttm_range_man_fini(&adev->mman.bdev, AMDGPU_PL_DOORBELL);

	/* Core TTM device                                              */
	ttm_device_fini(&adev->mman.bdev);
	adev->mman.initialized = false;

	/* Dedicated scheduler entity for parallel TTM moves            */
	if (amdgpu_ttm_entity_initialized) {
		drm_sched_entity_destroy(&amdgpu_ttm_sched_entity);
		kfree(amdgpu_ttm_scheds);
		amdgpu_ttm_scheds           = NULL;
		amdgpu_ttm_entity_initialized = false;
	}

	DRM_INFO("amdgpu: ttm finalized\n");
}

/**
 * amdgpu_ttm_set_buffer_funcs_status - enable/disable use of buffer functions
 *
 * @adev: amdgpu_device pointer
 * @enable: true when we can use buffer functions.
 *
 * Enable/disable use of buffer functions during suspend/resume. This should
 * only be called at bootup or when userspace isn't running.
 */
void amdgpu_ttm_set_buffer_funcs_status(struct amdgpu_device *adev, bool enable)
{
	struct ttm_resource_manager *man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	uint64_t size;
	int r;

	if (!adev->mman.initialized || amdgpu_in_reset(adev) ||
	    adev->mman.buffer_funcs_enabled == enable || adev->gmc.is_app_apu)
		return;

	if (enable) {
		struct amdgpu_ring *ring;
		struct drm_gpu_scheduler *sched;

		ring = adev->mman.buffer_funcs_ring;
		sched = &ring->sched;
		r = drm_sched_entity_init(&adev->mman.high_pr,
					  DRM_SCHED_PRIORITY_KERNEL, &sched,
					  1, NULL);
		if (r) {
			DRM_ERROR("Failed setting up TTM BO move entity (%d)\n",
				  r);
			return;
		}

		r = drm_sched_entity_init(&adev->mman.low_pr,
					  DRM_SCHED_PRIORITY_NORMAL, &sched,
					  1, NULL);
		if (r) {
			DRM_ERROR("Failed setting up TTM BO move entity (%d)\n",
				  r);
			goto error_free_entity;
		}
	} else {
		drm_sched_entity_destroy(&adev->mman.high_pr);
		drm_sched_entity_destroy(&adev->mman.low_pr);
		dma_fence_put(man->move);
		man->move = NULL;
	}

	/* this just adjusts TTM size idea, which sets lpfn to the correct value */
	if (enable)
		size = adev->gmc.real_vram_size;
	else
		size = adev->gmc.visible_vram_size;
	man->size = size;
	adev->mman.buffer_funcs_enabled = enable;

	return;

error_free_entity:
	drm_sched_entity_destroy(&adev->mman.high_pr);
}

static int amdgpu_ttm_prepare_job(struct amdgpu_device *adev,
				  bool direct_submit,
				  unsigned int num_dw,
				  struct dma_resv *resv,
				  bool vm_needs_flush,
				  struct amdgpu_job **job,
				  bool delayed)
{
	enum amdgpu_ib_pool_type pool = direct_submit ?
		AMDGPU_IB_POOL_DIRECT :
		AMDGPU_IB_POOL_DELAYED;
	int r;
	struct drm_sched_entity *entity = delayed ? &adev->mman.low_pr :
						    &adev->mman.high_pr;
	r = amdgpu_job_alloc_with_ib(adev, entity,
				     AMDGPU_FENCE_OWNER_UNDEFINED,
				     num_dw * 4, pool, job);
	if (r)
		return r;

	if (vm_needs_flush) {
		(*job)->vm_pd_addr = amdgpu_gmc_pd_addr(adev->gmc.pdb0_bo ?
							adev->gmc.pdb0_bo :
							adev->gart.bo);
		(*job)->vm_needs_flush = true;
	}
	if (!resv)
		return 0;

	return drm_sched_job_add_resv_dependencies(&(*job)->base, resv,
						   DMA_RESV_USAGE_BOOKKEEP);
}

int amdgpu_copy_buffer(struct amdgpu_ring *ring, uint64_t src_offset,
		       uint64_t dst_offset, uint32_t byte_count,
		       struct dma_resv *resv,
		       struct dma_fence **fence, bool direct_submit,
		       bool vm_needs_flush, uint32_t copy_flags)
{
	struct amdgpu_device *adev = ring->adev;
	unsigned int num_loops, num_dw;
	struct amdgpu_job *job;
	uint32_t max_bytes;
	unsigned int i;
	int r;

	if (!direct_submit && !ring->sched.ready) {
		DRM_ERROR("Trying to move memory with ring turned off.\n");
		return -EINVAL;
	}

	max_bytes = adev->mman.buffer_funcs->copy_max_bytes;
	num_loops = DIV_ROUND_UP(byte_count, max_bytes);
	num_dw = ALIGN(num_loops * adev->mman.buffer_funcs->copy_num_dw, 8);
	r = amdgpu_ttm_prepare_job(adev, direct_submit, num_dw,
				   resv, vm_needs_flush, &job, false);
	if (r)
		return r;

	for (i = 0; i < num_loops; i++) {
		uint32_t cur_size_in_bytes = min(byte_count, max_bytes);

		amdgpu_emit_copy_buffer(adev, &job->ibs[0], src_offset,
					dst_offset, cur_size_in_bytes, copy_flags);
		src_offset += cur_size_in_bytes;
		dst_offset += cur_size_in_bytes;
		byte_count -= cur_size_in_bytes;
	}

	amdgpu_ring_pad_ib(ring, &job->ibs[0]);
	WARN_ON(job->ibs[0].length_dw > num_dw);
	if (direct_submit)
		r = amdgpu_job_submit_direct(job, ring, fence);
	else
		*fence = amdgpu_job_submit(job);
	if (r)
		goto error_free;

	return r;

error_free:
	amdgpu_job_free(job);
	DRM_ERROR("Error scheduling IBs (%d)\n", r);
	return r;
}

static int amdgpu_ttm_fill_mem(struct amdgpu_ring *ring,
							   u32 src_data, u64 dst_addr, u32 byte_count,
							   struct dma_resv *resv,
							   struct dma_fence **fence,
							   bool vm_needs_flush, bool delayed)
{
	struct amdgpu_device *adev = ring->adev;
	u32 max_bytes  = adev->mman.buffer_funcs->fill_max_bytes;
	unsigned int num_loops, num_dw;
	struct amdgpu_job *job;
	int r;

	if (WARN_ON(byte_count == 0))
		return -EINVAL;

	num_loops = DIV_ROUND_UP(byte_count, max_bytes);
	num_dw    = ALIGN(num_loops * adev->mman.buffer_funcs->fill_num_dw, 8);

	r = amdgpu_ttm_prepare_job(adev, false, num_dw, resv,
							   vm_needs_flush, &job, delayed);
	if (r)
		return r;

	while (byte_count) {
		u32 cur = min(byte_count, max_bytes);

		amdgpu_emit_fill_buffer(adev, &job->ibs[0],
								src_data, dst_addr, cur);

		dst_addr   += cur;
		byte_count -= cur;
	}

	amdgpu_ring_pad_ib(ring, &job->ibs[0]);
	WARN_ON(job->ibs[0].length_dw > num_dw);

	*fence = amdgpu_job_submit(job);
	return 0;
}

/*
 * Submit one large copy as N independent jobs that the drm-scheduler
 * can execute on several SDMA rings in parallel.
 *
 * scheds[]      : array of <=16 scheduler pointers (at least one)
 * numscheds     : number of valid entries in scheds[]
 * fence_out     : optional; combined fence is returned when !NULL
 */
static int amdgpu_copy_buffer_multi(struct amdgpu_device *adev,
									struct drm_gpu_scheduler **scheds,
									unsigned int numscheds,
									u64  src, u64 dst, u64 bytes,
									u32  copy_flags,
									struct dma_resv *resv,
									struct dma_fence **fence_out)
{
	const u32 pkt_max  = adev->mman.buffer_funcs->copy_max_bytes;
	const u32 stripe   = 256u << 20;          /* 256 MiB per job      */
	unsigned int njobs = DIV_ROUND_UP_ULL(bytes, stripe);
	struct dma_fence **farr = NULL;
	struct dma_fence_array *fa;
	unsigned int i;
	int r = 0;

	if (WARN_ON(!numscheds || !scheds))
		return -EINVAL;
	if (njobs > 16)                            /* sanity for stack use */
		return -E2BIG;

	farr = kcalloc(njobs, sizeof(*farr), GFP_KERNEL);
	if (!farr)
		return -ENOMEM;

	for (i = 0; i < njobs; ++i) {
		u64 this_sz   = min_t(u64, bytes, stripe);
		struct drm_gpu_scheduler *sched = scheds[i % numscheds];
		struct amdgpu_ring *ring =
		container_of(sched, struct amdgpu_ring, sched);
		struct drm_sched_entity ent;
		struct amdgpu_job *job;
		u32 loops = DIV_ROUND_UP(this_sz, pkt_max);
		u32 ndw   = ALIGN(loops *
		adev->mman.buffer_funcs->copy_num_dw, 8);
		int tmp;

		/* transient entity pointing at exactly one scheduler       */
		tmp = drm_sched_entity_init(&ent, DRM_SCHED_PRIORITY_KERNEL,
									&sched, 1, NULL);
		if (tmp) { r = tmp; goto err_loop; }

		tmp = amdgpu_job_alloc_with_ib(adev, &ent,
									   AMDGPU_FENCE_OWNER_UNDEFINED,
								 ndw * 4,
								 AMDGPU_IB_POOL_DELAYED, &job);
		if (tmp) {
			r = tmp;
			drm_sched_entity_fini(&ent);
			goto err_loop;
		}

		/* dependencies                                              */
		if (resv) {
			tmp = drm_sched_job_add_resv_dependencies(&job->base,
													  resv,
											 DMA_RESV_USAGE_BOOKKEEP);
			if (tmp) {
				amdgpu_job_free(job);
				drm_sched_entity_fini(&ent);
				r = tmp;
				goto err_loop;
			}
		}

		/* build IB                                                 */
		while (this_sz) {
			u32 cur = min_t(u64, this_sz, pkt_max);

			amdgpu_emit_copy_buffer(adev, &job->ibs[0],
									src, dst, cur, copy_flags);
			src     += cur;
			dst     += cur;
			this_sz -= cur;
		}

		amdgpu_ring_pad_ib(ring, &job->ibs[0]);
		WARN_ON(job->ibs[0].length_dw > ndw);

		farr[i] = amdgpu_job_submit(job);
		bytes  -= min_t(u64, stripe, bytes);

		/* entity will be destroyed when all queued jobs finished  */
		drm_sched_entity_fini(&ent);
	}

	fa = dma_fence_array_create(njobs, farr, 0, GFP_KERNEL, false);
	if (!fa)
		r = -ENOMEM;
	else if (fence_out)
		*fence_out = &fa->base;

	err_loop:
	while (i--)
		dma_fence_put(farr[i]);
	kfree(farr);
	return r;
}

/**
 * amdgpu_ttm_clear_buffer - clear memory buffers
 * @bo: amdgpu buffer object
 * @resv: reservation object
 * @fence: dma_fence associated with the operation
 *
 * Clear the memory buffer resource.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_ttm_clear_buffer(struct amdgpu_bo *bo, struct dma_resv *resv,
							struct dma_fence **fence_out)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_ring   *ring = adev->mman.buffer_funcs_ring;
	struct amdgpu_res_cursor cur;
	u64 chunk = vega_ttm_chunk_bytes(adev);
	struct dma_fence *fence = NULL;
	int r = 0;

	if (!adev->mman.buffer_funcs_enabled || !ring || !ring->sched.ready)
		return -EINVAL;

	amdgpu_res_first(bo->tbo.resource, 0,
					 amdgpu_bo_size(bo), &cur);

	gtt_window_lock_fast(adev);

	while (cur.remaining) {
		u64 size = min(cur.size, chunk);
		u64 addr;
		struct dma_fence *next = NULL;

		if (amdgpu_res_cleared(&cur)) {
			amdgpu_res_next(&cur, cur.size);
			continue;
		}

		r = amdgpu_ttm_map_buffer(&bo->tbo, bo->tbo.resource,
								  &cur, 1, ring, false,
							&size, &addr);
		if (r)
			break;

		r = amdgpu_ttm_fill_mem(ring, 0, addr, (u32)size,
								resv, &next, true, true);
		if (r) {
			/* fill_mem already cleaned up its job */
			break;
		}

		dma_fence_put(fence);
		fence = next;
		amdgpu_res_next(&cur, size);
	}

	mutex_unlock(&adev->mman.gtt_window_lock);

	if (!r && fence_out)
		*fence_out = dma_fence_get(fence);

	dma_fence_put(fence);
	return r;
}

int amdgpu_fill_buffer(struct amdgpu_bo *bo, u32 pattern,
					   struct dma_resv *resv, struct dma_fence **fence_out,
					   bool delayed)
{
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_ring   *ring = adev->mman.buffer_funcs_ring;
	struct amdgpu_res_cursor dst;
	struct dma_fence *fence = NULL;
	u64 chunk = vega_ttm_chunk_bytes(adev);
	int r = 0;

	if (!adev->mman.buffer_funcs_enabled || !ring || !ring->sched.ready)
		return -EINVAL;

	amdgpu_res_first(bo->tbo.resource, 0, amdgpu_bo_size(bo), &dst);

	gtt_window_lock_fast(adev);

	while (dst.remaining) {
		u64 cur_size = min(dst.size, chunk);
		u64 addr;
		struct dma_fence *next = NULL;

		r = amdgpu_ttm_map_buffer(&bo->tbo, bo->tbo.resource,
								  &dst, 1, ring, false,
							&cur_size, &addr);
		if (r)
			break;

		/* Use existing helper – it already supports delayed flag   */
		r = amdgpu_ttm_fill_mem(ring, pattern, addr,
								(u32)cur_size, resv, &next,
								true, delayed);
		if (r)
			break;

		dma_fence_put(fence);
		fence = next;

		amdgpu_res_next(&dst, cur_size);
	}

	mutex_unlock(&adev->mman.gtt_window_lock);

	if (fence_out)
		*fence_out = dma_fence_get(fence);
	dma_fence_put(fence);
	return r;
}

/**
 * amdgpu_ttm_evict_resources - evict memory buffers
 * @adev: amdgpu device object
 * @mem_type: evicted BO's memory type
 *
 * Evicts all @mem_type buffers on the lru list of the memory type.
 *
 * Returns:
 * 0 for success or a negative error code on failure.
 */
int amdgpu_ttm_evict_resources(struct amdgpu_device *adev, int mem_type)
{
	struct ttm_resource_manager *man;

	switch (mem_type) {
	case TTM_PL_VRAM:
	case TTM_PL_TT:
	case AMDGPU_PL_GWS:
	case AMDGPU_PL_GDS:
	case AMDGPU_PL_OA:
		man = ttm_manager_type(&adev->mman.bdev, mem_type);
		break;
	default:
		DRM_ERROR("Trying to evict invalid memory type\n");
		return -EINVAL;
	}

	return ttm_resource_manager_evict_all(&adev->mman.bdev, man);
}

#if defined(CONFIG_DEBUG_FS)

static int amdgpu_ttm_page_pool_show(struct seq_file *m, void *unused)
{
	struct amdgpu_device *adev = m->private;

	return ttm_pool_debugfs(&adev->mman.bdev.pool, m);
}

DEFINE_SHOW_ATTRIBUTE(amdgpu_ttm_page_pool);

/*
 * amdgpu_ttm_vram_read - Linear read access to VRAM
 *
 * Accesses VRAM via MMIO for debugging purposes.
 */
static ssize_t amdgpu_ttm_vram_read(struct file *f, char __user *buf,
				    size_t size, loff_t *pos)
{
	struct amdgpu_device *adev = file_inode(f)->i_private;
	ssize_t result = 0;

	if (size & 0x3 || *pos & 0x3)
		return -EINVAL;

	if (*pos >= adev->gmc.mc_vram_size)
		return -ENXIO;

	size = min(size, (size_t)(adev->gmc.mc_vram_size - *pos));
	while (size) {
		size_t bytes = min(size, AMDGPU_TTM_VRAM_MAX_DW_READ * 4);
		uint32_t value[AMDGPU_TTM_VRAM_MAX_DW_READ];

		amdgpu_device_vram_access(adev, *pos, value, bytes, false);
		if (copy_to_user(buf, value, bytes))
			return -EFAULT;

		result += bytes;
		buf += bytes;
		*pos += bytes;
		size -= bytes;
	}

	return result;
}

/*
 * amdgpu_ttm_vram_write - Linear write access to VRAM
 *
 * Accesses VRAM via MMIO for debugging purposes.
 */
static ssize_t amdgpu_ttm_vram_write(struct file *f, const char __user *buf,
				    size_t size, loff_t *pos)
{
	struct amdgpu_device *adev = file_inode(f)->i_private;
	ssize_t result = 0;
	int r;

	if (size & 0x3 || *pos & 0x3)
		return -EINVAL;

	if (*pos >= adev->gmc.mc_vram_size)
		return -ENXIO;

	while (size) {
		uint32_t value;

		if (*pos >= adev->gmc.mc_vram_size)
			return result;

		r = get_user(value, (uint32_t *)buf);
		if (r)
			return r;

		amdgpu_device_mm_access(adev, *pos, &value, 4, true);

		result += 4;
		buf += 4;
		*pos += 4;
		size -= 4;
	}

	return result;
}

static const struct file_operations amdgpu_ttm_vram_fops = {
	.owner = THIS_MODULE,
	.read = amdgpu_ttm_vram_read,
	.write = amdgpu_ttm_vram_write,
	.llseek = default_llseek,
};

/*
 * amdgpu_iomem_read - Virtual read access to GPU mapped memory
 *
 * This function is used to read memory that has been mapped to the
 * GPU and the known addresses are not physical addresses but instead
 * bus addresses (e.g., what you'd put in an IB or ring buffer).
 */
static ssize_t amdgpu_iomem_read(struct file *f, char __user *buf,
				 size_t size, loff_t *pos)
{
	struct amdgpu_device *adev = file_inode(f)->i_private;
	struct iommu_domain *dom;
	ssize_t result = 0;
	int r;

	/* retrieve the IOMMU domain if any for this device */
	dom = iommu_get_domain_for_dev(adev->dev);

	while (size) {
		phys_addr_t addr = *pos & PAGE_MASK;
		loff_t off = *pos & ~PAGE_MASK;
		size_t bytes = PAGE_SIZE - off;
		unsigned long pfn;
		struct page *p;
		void *ptr;

		bytes = min(bytes, size);

		/* Translate the bus address to a physical address.  If
		 * the domain is NULL it means there is no IOMMU active
		 * and the address translation is the identity
		 */
		addr = dom ? iommu_iova_to_phys(dom, addr) : addr;

		pfn = addr >> PAGE_SHIFT;
		if (!pfn_valid(pfn))
			return -EPERM;

		p = pfn_to_page(pfn);
		if (p->mapping != adev->mman.bdev.dev_mapping)
			return -EPERM;

		ptr = kmap_local_page(p);
		r = copy_to_user(buf, ptr + off, bytes);
		kunmap_local(ptr);
		if (r)
			return -EFAULT;

		size -= bytes;
		*pos += bytes;
		result += bytes;
	}

	return result;
}

/*
 * amdgpu_iomem_write - Virtual write access to GPU mapped memory
 *
 * This function is used to write memory that has been mapped to the
 * GPU and the known addresses are not physical addresses but instead
 * bus addresses (e.g., what you'd put in an IB or ring buffer).
 */
static ssize_t amdgpu_iomem_write(struct file *f, const char __user *buf,
				 size_t size, loff_t *pos)
{
	struct amdgpu_device *adev = file_inode(f)->i_private;
	struct iommu_domain *dom;
	ssize_t result = 0;
	int r;

	dom = iommu_get_domain_for_dev(adev->dev);

	while (size) {
		phys_addr_t addr = *pos & PAGE_MASK;
		loff_t off = *pos & ~PAGE_MASK;
		size_t bytes = PAGE_SIZE - off;
		unsigned long pfn;
		struct page *p;
		void *ptr;

		bytes = min(bytes, size);

		addr = dom ? iommu_iova_to_phys(dom, addr) : addr;

		pfn = addr >> PAGE_SHIFT;
		if (!pfn_valid(pfn))
			return -EPERM;

		p = pfn_to_page(pfn);
		if (p->mapping != adev->mman.bdev.dev_mapping)
			return -EPERM;

		ptr = kmap_local_page(p);
		r = copy_from_user(ptr + off, buf, bytes);
		kunmap_local(ptr);
		if (r)
			return -EFAULT;

		size -= bytes;
		*pos += bytes;
		result += bytes;
	}

	return result;
}

static const struct file_operations amdgpu_ttm_iomem_fops = {
	.owner = THIS_MODULE,
	.read = amdgpu_iomem_read,
	.write = amdgpu_iomem_write,
	.llseek = default_llseek
};

#endif

void amdgpu_ttm_debugfs_init(struct amdgpu_device *adev)
{
#if defined(CONFIG_DEBUG_FS)
	struct drm_minor *minor = adev_to_drm(adev)->primary;
	struct dentry *root = minor->debugfs_root;

	debugfs_create_file_size("amdgpu_vram", 0444, root, adev,
				 &amdgpu_ttm_vram_fops, adev->gmc.mc_vram_size);
	debugfs_create_file("amdgpu_iomem", 0444, root, adev,
			    &amdgpu_ttm_iomem_fops);
	debugfs_create_file("ttm_page_pool", 0444, root, adev,
			    &amdgpu_ttm_page_pool_fops);
	ttm_resource_manager_create_debugfs(ttm_manager_type(&adev->mman.bdev,
							     TTM_PL_VRAM),
					    root, "amdgpu_vram_mm");
	ttm_resource_manager_create_debugfs(ttm_manager_type(&adev->mman.bdev,
							     TTM_PL_TT),
					    root, "amdgpu_gtt_mm");
	ttm_resource_manager_create_debugfs(ttm_manager_type(&adev->mman.bdev,
							     AMDGPU_PL_GDS),
					    root, "amdgpu_gds_mm");
	ttm_resource_manager_create_debugfs(ttm_manager_type(&adev->mman.bdev,
							     AMDGPU_PL_GWS),
					    root, "amdgpu_gws_mm");
	ttm_resource_manager_create_debugfs(ttm_manager_type(&adev->mman.bdev,
							     AMDGPU_PL_OA),
					    root, "amdgpu_oa_mm");

#endif
}
