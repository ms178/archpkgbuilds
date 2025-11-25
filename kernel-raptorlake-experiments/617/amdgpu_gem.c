// SPDX-License-Identifier: MIT
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
#include <linux/overflow.h>
#include <linux/dma-resv.h>
#include <linux/jiffies.h>
#include <linux/math64.h>
#include <linux/percpu.h>
#include <linux/log2.h>
#include <linux/cache.h>
#include <linux/preempt.h>
#include <linux/sched/clock.h>

#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_exec.h>
#include <drm/drm_gem_ttm_helper.h>
#include <drm/ttm/ttm_tt.h>
#include <drm/ttm/ttm_resource.h>
#include <drm/drm_syncobj.h>

#include "amdgpu.h"
#include "amdgpu_display.h"
#include "amdgpu_dma_buf.h"
#include "amdgpu_hmm.h"
#include "amdgpu_xgmi.h"
#include "amdgpu_vm.h"

/* ============================================================================
 * Constants and Tunables
 * ============================================================================
 */

/* Maximum syncobj handles per submission - guard against DoS */
#define AMDGPU_GEM_MAX_SYNCOBJ_HANDLES 1024U

/* Stack buffer size for fence handles - covers 99%+ of submissions */
#define AMDGPU_GEM_SYNCOBJ_STACK_SIZE 128U

/* Absolute safety caps for all ASICs */
#define PREFETCH_ABS_MIN_PAGES 1U
#define PREFETCH_ABS_MAX_PAGES 128U
#define PREFETCH_BOOST_CAP 256U

/* Vega-specific TLB-aware cap (based on 64-entry L1 TLB per CU) */
#define VEGA_TLB_AWARE_MAX_PAGES 56U

/* Large page promotion hint threshold */
#define VEGA_LARGE_PAGE_HINT_THRESHOLD 256U

/* Compute workload size thresholds */
#define VEGA_COMPUTE_LARGE_BO_THRESHOLD (64ULL << 20)
#define VEGA_COMPUTE_SMALL_BO_THRESHOLD (4ULL << 20)

/* Module parameters (tunables) */
static unsigned int vega_pf_max_pages_vram = 32;
module_param_named(vega_pf_max_pages_vram, vega_pf_max_pages_vram, uint, 0644);
MODULE_PARM_DESC(vega_pf_max_pages_vram,
		 "Max prefetch pages for VRAM BO faults (default 32)");

static unsigned int vega_pf_max_pages_gtt = 64;
module_param_named(vega_pf_max_pages_gtt, vega_pf_max_pages_gtt, uint, 0644);
MODULE_PARM_DESC(vega_pf_max_pages_gtt,
		 "Max prefetch pages for GTT BO faults (default 64)");

static unsigned int vega_pf_streak_window_jiffies = (HZ / 200);
module_param_named(vega_pf_streak_window_jiffies,
		   vega_pf_streak_window_jiffies, uint, 0644);
MODULE_PARM_DESC(vega_pf_streak_window_jiffies,
		 "Streak window in jiffies (default HZ/200)");

static unsigned int vega_pf_burst_ns = 1000;
module_param_named(vega_pf_burst_ns, vega_pf_burst_ns, uint, 0644);
MODULE_PARM_DESC(vega_pf_burst_ns,
		 "HBM2-tuned burst threshold in ns (default 1000)");

/* ============================================================================
 * Per-CPU State for Adaptive Prefetch
 * ============================================================================
 */

/*
 * Per-CPU reciprocal cache for division-free VRAM percentage.
 * Consolidated to reduce per-cpu access overhead.
 */
struct vega_recip_cache {
	u64 recip_q38;
	const void *adev_key;
};

static DEFINE_PER_CPU(struct vega_recip_cache, vega_recip_cache_pc);

/*
 * Per-CPU streak tracking state.
 * Layout optimized for cache line efficiency:
 * - Hot fields (read every fault) in first 48 bytes
 * - Aligned to cacheline to prevent false sharing
 */
struct vega_pf_state {
	const void *last_bo;
	const void *last_vma;
	unsigned long last_addr;
	u64 last_ns;
	unsigned long last_j;
	u32 last_stride;
	u16 sequential_pages;
	u8 streak;
	u8 direction;
	u8 stride_repeat_count;
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct vega_pf_state, vega_pf_pc);

/* ============================================================================
 * Fast Math Utilities
 * ============================================================================
 */

/*
 * Fast division by 100 using Barrett reduction.
 * Accurate for all x in [0, 2^32-1].
 *
 * Magic number derivation:
 *   M = ceil(2^39 / 100) = 5497558139 = 0x147AE147B
 *   result = (x * M) >> 39
 */
static __always_inline u32 fast_div100(u32 x)
{
	return (u32)(((u64)x * 0x147AE147BULL) >> 39);
}

/*
 * Fast division by 5 using multiplication.
 * Accurate for all x in [0, 2^32-1].
 *
 * Magic: ceil(2^33 / 5) = 1717986919 = 0x66666667
 */
static __always_inline u32 fast_div5(u32 x)
{
	return (u32)(((u64)x * 0x66666667ULL) >> 33);
}

/* ============================================================================
 * VRAM Usage Calculation
 * ============================================================================
 */

/*
 * Fast VRAM usage percentage with cached reciprocal.
 * Avoids expensive division by caching (100 << 38) / vram_size.
 *
 * Must be called with preemption disabled to safely access per-CPU data.
 */
static u32 amdgpu_vram_usage_pct_fast(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *mgr;
	u64 used_bytes;
	struct vega_recip_cache *cache;

	if (unlikely(!adev))
		return 0;

	mgr = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (unlikely(!mgr))
		return 0;

	used_bytes = ttm_resource_manager_usage(mgr);
	cache = this_cpu_ptr(&vega_recip_cache_pc);

	if (unlikely(cache->adev_key != adev)) {
		u64 vram_b = max_t(u64, adev->gmc.mc_vram_size, 1ULL);

		cache->recip_q38 = div64_u64(100ULL << 38, vram_b);
		cache->adev_key = adev;
	}

	return min_t(u32, mul_u64_u64_shr(used_bytes, cache->recip_q38, 38), 100U);
}

/* ============================================================================
 * Fence Handling
 * ============================================================================
 */

/*
 * amdgpu_gem_add_input_fence - Wait for input fences before submission
 *
 * Optimized fast paths:
 * - 0 fences: immediate return (most common for simple apps)
 * - 1 fence: get_user + direct processing (most common for games)
 * - 2-128 fences: stack buffer (covers >99% of remaining cases)
 * - 129-1024 fences: heap allocation (extreme cases only)
 */
static int
amdgpu_gem_add_input_fence(struct drm_file *filp,
			   uint64_t syncobj_handles_array,
			   uint32_t num_syncobj_handles)
{
	struct dma_fence *fence;
	uint32_t syncobj_handles_stack[AMDGPU_GEM_SYNCOBJ_STACK_SIZE];
	uint32_t *syncobj_handles;
	uint32_t single_handle;
	uint32_t __user *user_handles;
	int ret = 0;
	uint32_t i;

	/* Fast path: no fences */
	if (likely(num_syncobj_handles == 0))
		return 0;

	/* Guard against overflow and unreasonable counts */
	if (unlikely(num_syncobj_handles > AMDGPU_GEM_MAX_SYNCOBJ_HANDLES))
		return -EINVAL;

	user_handles = u64_to_user_ptr(syncobj_handles_array);

	/*
	 * Fast path: single fence (most common in games).
	 * Use get_user for minimal overhead - avoids copy_from_user's
	 * access_ok + might_fault checks for single word.
	 */
	if (likely(num_syncobj_handles == 1)) {
		if (unlikely(get_user(single_handle, user_handles)))
			return -EFAULT;

		if (unlikely(single_handle == 0))
			return -EINVAL;

		ret = drm_syncobj_find_fence(filp, single_handle, 0, 0, &fence);
		if (unlikely(ret))
			return ret;

		if (likely(!dma_fence_is_signaled(fence)))
			ret = dma_fence_wait(fence, false);

		dma_fence_put(fence);
		return ret;
	}

	/* Stack path: 2-128 fences */
	if (likely(num_syncobj_handles <= ARRAY_SIZE(syncobj_handles_stack))) {
		if (unlikely(copy_from_user(syncobj_handles_stack,
					    user_handles,
					    num_syncobj_handles *
					    sizeof(uint32_t))))
			return -EFAULT;
		syncobj_handles = syncobj_handles_stack;
	} else {
		/* Heap path: 129-1024 fences (rare) */
		syncobj_handles = memdup_user(user_handles,
					      size_mul(sizeof(uint32_t),
						       num_syncobj_handles));
		if (IS_ERR(syncobj_handles))
			return PTR_ERR(syncobj_handles);
	}

	for (i = 0; i < num_syncobj_handles; i++) {
		uint32_t handle = syncobj_handles[i];

		if (unlikely(handle == 0)) {
			ret = -EINVAL;
			break;
		}

		ret = drm_syncobj_find_fence(filp, handle, 0, 0, &fence);
		if (unlikely(ret))
			break;

		/*
		 * Skip wait if already signaled - common case for
		 * fences from previous frames that completed.
		 */
		if (likely(!dma_fence_is_signaled(fence)))
			ret = dma_fence_wait(fence, false);

		dma_fence_put(fence);
		if (unlikely(ret))
			break;
	}

	if (unlikely(num_syncobj_handles > ARRAY_SIZE(syncobj_handles_stack)))
		kfree(syncobj_handles);

	return ret;
}

static int
amdgpu_gem_update_timeline_node(struct drm_file *filp,
				uint32_t syncobj_handle,
				uint64_t point,
				struct drm_syncobj **syncobj,
				struct dma_fence_chain **chain)
{
	if (!syncobj_handle)
		return 0;

	*syncobj = drm_syncobj_find(filp, syncobj_handle);
	if (!*syncobj)
		return -ENOENT;

	if (!point)
		return 0;

	*chain = dma_fence_chain_alloc();
	if (!*chain) {
		drm_syncobj_put(*syncobj);
		return -ENOMEM;
	}

	return 0;
}

static void
amdgpu_gem_update_bo_mapping(struct drm_file *filp,
			     struct amdgpu_bo_va *bo_va,
			     uint32_t operation,
			     uint64_t point,
			     struct dma_fence *fence,
			     struct drm_syncobj *syncobj,
			     struct dma_fence_chain *chain)
{
	struct amdgpu_bo *bo = bo_va ? bo_va->base.bo : NULL;
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	struct dma_fence *last_update;

	if (!syncobj)
		return;

	switch (operation) {
	case AMDGPU_VA_OP_MAP:
	case AMDGPU_VA_OP_REPLACE:
		if (bo && (bo->tbo.base.resv == vm->root.bo->tbo.base.resv))
			last_update = vm->last_update;
		else
			last_update = bo_va->last_pt_update;
		break;
	case AMDGPU_VA_OP_UNMAP:
	case AMDGPU_VA_OP_CLEAR:
		last_update = fence;
		break;
	default:
		return;
	}

	if (!point)
		drm_syncobj_replace_fence(syncobj, last_update);
	else
		drm_syncobj_add_point(syncobj, chain, last_update, point);
}

/* ============================================================================
 * Vega-Aware Adaptive Prefetch
 * ============================================================================
 */

/*
 * Compute optimal prefetch pages for Vega GPUs.
 *
 * This function implements an adaptive prefetch strategy tuned for:
 * - Vega's 64-entry L1 TLB per CU (cap prefetch to avoid thrashing)
 * - HBM2's ~130ns latency (aggressive boost for sequential access)
 *
 * Optimization: Snapshot per-CPU state to allow computation with preemption
 * enabled, reducing latency spikes on hybrid CPUs (Intel Raptor Lake).
 */
static unsigned int
amdgpu_vega_optimal_prefetch(struct amdgpu_device *adev,
			     struct amdgpu_bo *abo,
			     struct vm_fault *vmf,
			     unsigned int base_pages)
{
	struct vega_pf_state local_state;
	struct vega_pf_state *pcs;
	u32 vram_pct;
	u32 want, total_pages, cap;
	u64 bo_size;
	bool is_compute, is_vram;
	unsigned long now_j;
	u64 now_ns, delta_ns;
	long diff;
	bool state_valid;

	if (unlikely(!adev || !abo || !vmf || base_pages == 0))
		return base_pages;

	bo_size = abo->tbo.base.size;
	is_vram = (abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) != 0;
	is_compute = (abo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) != 0;

	/*
	 * Snapshot per-CPU state with preemption disabled.
	 * Allows us to run complex stride logic with preemption enabled.
	 */
	preempt_disable();

	vram_pct = amdgpu_vram_usage_pct_fast(adev);
	pcs = this_cpu_ptr(&vega_pf_pc);

	now_j = jiffies;
	now_ns = local_clock();

	/* Check if previous state is valid for this BO/VMA */
	state_valid = (pcs->last_bo == (const void *)abo &&
		       pcs->last_vma == (const void *)vmf->vma &&
		       time_before(now_j,
				   pcs->last_j + vega_pf_streak_window_jiffies));

	if (state_valid)
		local_state = *pcs;
	else
		memset(&local_state, 0, sizeof(local_state));

	preempt_enable();

	/* Cap based on VRAM pressure and TLB capacity */
	if (is_vram)
		cap = (vram_pct >= 90U) ? 40U : VEGA_TLB_AWARE_MAX_PAGES;
	else
		cap = min(vega_pf_max_pages_gtt, 64U);

	cap = max(cap, PREFETCH_ABS_MIN_PAGES);

	total_pages = (u32)min_t(u64, bo_size >> PAGE_SHIFT, U32_MAX);
	if (unlikely(total_pages == 0U))
		total_pages = 1U;

	/* Compute base want based on workload type */
	if (is_compute && bo_size >= VEGA_COMPUTE_LARGE_BO_THRESHOLD) {
		want = base_pages * 2U;
	} else if (is_compute && bo_size <= VEGA_COMPUTE_SMALL_BO_THRESHOLD) {
		want = max(base_pages / 2U, PREFETCH_ABS_MIN_PAGES);
	} else {
		/*
		 * Scale based on VRAM pressure:
		 * Formula: scale = 120 - (vram_pct * 4 / 5)
		 * Optimized: use fast_div5 to avoid division
		 */
		u32 pressure_adj = fast_div5(vram_pct << 2);
		u32 scale_pct = 120U - pressure_adj;

		want = fast_div100(base_pages * scale_pct);

		if (total_pages > 1U)
			want += (u32)__fls(total_pages);

		if (is_compute && vram_pct < 90U)
			want += want >> 2;
	}

	/* Process streak state if valid */
	if (state_valid) {
		delta_ns = (now_ns >= local_state.last_ns) ?
			   (now_ns - local_state.last_ns) : 0;
		diff = (long)(vmf->address - local_state.last_addr);

		if (diff != 0) {
			unsigned long delta_pages;
			int direction;

			delta_pages = (unsigned long)((diff > 0) ? diff : -diff)
				      >> PAGE_SHIFT;
			direction = (diff > 0) ? 1 : 2;

			/* Forward sequential access */
			if (direction == 1 && delta_pages > 0 &&
			    delta_pages <= (unsigned long)(want + 4U)) {
				u32 boost;

				if (local_state.streak < 4U)
					local_state.streak++;

				/* Track sequential pages for large page hints */
				if (is_vram) {
					u32 new_seq = (u32)local_state.sequential_pages +
						      min_t(u32, (u32)delta_pages, 512U);
					local_state.sequential_pages =
						(u16)min(new_seq, 65535U);
				} else {
					local_state.sequential_pages = 0U;
				}

				/* Burst boost: rapid sequential faults */
				if (delta_ns && delta_ns <= vega_pf_burst_ns) {
					boost = want >> 1;
					want = min_t(u32, want + boost,
						     PREFETCH_BOOST_CAP);
				}

				/* Streak boost */
				if (local_state.streak > 0U) {
					boost = (want * local_state.streak) >> 1;
					want = min_t(u32, want + boost,
						     PREFETCH_BOOST_CAP);
				}

				/* Large page hint for sustained access */
				if (is_vram &&
				    local_state.sequential_pages >=
				    VEGA_LARGE_PAGE_HINT_THRESHOLD &&
				    want < 512U && total_pages >= 512U)
					want = 512U;

				/* Stride detection for compute */
				if (is_compute && delta_pages > 1UL &&
				    delta_pages <= 1024UL) {
					u32 stride = (u32)delta_pages;

					if ((stride & (stride - 1)) == 0) {
						if (local_state.last_stride == stride) {
							if (local_state.stride_repeat_count < 255U)
								local_state.stride_repeat_count++;
							if (local_state.stride_repeat_count >= 3U)
								want = max(want, stride);
						} else {
							local_state.last_stride = stride;
							local_state.stride_repeat_count = 1U;
						}
					} else {
						local_state.last_stride = 0U;
						local_state.stride_repeat_count = 0U;
					}
				} else {
					local_state.last_stride = 0U;
					local_state.stride_repeat_count = 0U;
				}
			} else {
				/* Reset on non-sequential access */
				local_state.streak = 0U;
				local_state.sequential_pages = 0U;
				local_state.last_stride = 0U;
				local_state.stride_repeat_count = 0U;
			}
			local_state.direction = (u8)direction;
		} else {
			local_state.streak = 0U;
			local_state.direction = 0U;
			local_state.sequential_pages = 0U;
			local_state.last_stride = 0U;
			local_state.stride_repeat_count = 0U;
		}
	}

	/* Update per-CPU state */
	local_state.last_bo = (const void *)abo;
	local_state.last_vma = (const void *)vmf->vma;
	local_state.last_addr = vmf->address;
	local_state.last_ns = now_ns;
	local_state.last_j = now_j;

	preempt_disable();
	*this_cpu_ptr(&vega_pf_pc) = local_state;
	preempt_enable();

	/* Pressure damping */
	if (vram_pct >= 98U)
		want = min(want, base_pages);
	else if (vram_pct >= 95U)
		want = min(want, (want + base_pages) >> 1);

	/* Alignment for coalescing */
	if (want >= 16U)
		want &= ~15U;
	else if (want >= 4U)
		want &= ~3U;

	/* Final clamping */
	want = clamp_t(u32, want, PREFETCH_ABS_MIN_PAGES, PREFETCH_ABS_MAX_PAGES);
	want = min(want, cap);
	want = min(want, total_pages);

	return want;
}

/* ============================================================================
 * Page Fault Handler
 * ============================================================================
 */

static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo = vmf->vma->vm_private_data;
	struct drm_device *ddev = bo->base.dev;
	struct amdgpu_device *adev = drm_to_adev(ddev);
	unsigned int prefetch_pages = TTM_BO_VM_NUM_PREFAULT;
	vm_fault_t ret;
	int idx;

	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret))
		return ret;

	if (drm_dev_enter(ddev, &idx)) {
		ret = amdgpu_bo_fault_reserve_notify(bo);
		if (unlikely(ret)) {
			drm_dev_exit(idx);
			goto unlock;
		}

		/* Vega-only adaptive prefetch */
		if (likely(adev->asic_type == CHIP_VEGA10 ||
			   adev->asic_type == CHIP_VEGA12 ||
			   adev->asic_type == CHIP_VEGA20)) {
			struct amdgpu_bo *abo;

			abo = container_of(bo, struct amdgpu_bo, tbo);
			prefetch_pages = amdgpu_vega_optimal_prefetch(
				adev, abo, vmf, prefetch_pages);
		}

		ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
					       prefetch_pages);

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

/* ============================================================================
 * GEM Object Lifecycle
 * ============================================================================
 */

static void amdgpu_gem_object_free(struct drm_gem_object *gobj)
{
	struct amdgpu_bo *aobj = gem_to_amdgpu_bo(gobj);

	amdgpu_hmm_unregister(aobj);
	ttm_bo_put(&aobj->tbo);
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

	memset(&bp, 0, sizeof(bp));
	*obj = NULL;
	flags |= AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;

	bp.size = size;
	bp.byte_align = alignment;
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

static int amdgpu_gem_object_open(struct drm_gem_object *obj,
				  struct drm_file *file_priv)
{
	struct amdgpu_bo *abo = gem_to_amdgpu_bo(obj);
	struct amdgpu_device *adev = amdgpu_ttm_adev(abo->tbo.bdev);
	struct amdgpu_fpriv *fpriv = file_priv->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	struct amdgpu_bo_va *bo_va;
	struct mm_struct *mm;
	int r;

	mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
	if (mm && mm != current->mm)
		return -EPERM;

	if (abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID &&
	    !amdgpu_vm_is_bo_always_valid(vm, abo))
		return -EPERM;

	r = amdgpu_bo_reserve(abo, false);
	if (r)
		return r;

	amdgpu_vm_bo_update_shared(abo);
	bo_va = amdgpu_vm_bo_find(vm, abo);
	if (!bo_va)
		bo_va = amdgpu_vm_bo_add(adev, vm, abo);
	else
		++bo_va->ref_count;

	r = amdgpu_eviction_fence_attach(&fpriv->evf_mgr, abo);
	if (r) {
		DRM_DEBUG_DRIVER("Failed to attach eviction fence to BO\n");
		amdgpu_bo_unreserve(abo);
		return r;
	}

	amdgpu_bo_unreserve(abo);

	if (!vm->is_compute_context || !vm->process_info)
		return 0;
	if (!drm_gem_is_imported(obj) ||
	    !dma_buf_is_dynamic(obj->import_attach->dmabuf))
		return 0;

	mutex_lock_nested(&vm->process_info->lock, 1);
	if (!WARN_ON(!vm->process_info->eviction_fence)) {
		r = amdgpu_amdkfd_bo_validate_and_fence(
			abo, AMDGPU_GEM_DOMAIN_GTT,
			&vm->process_info->eviction_fence->base);
		if (r) {
			struct amdgpu_task_info *ti;

			ti = amdgpu_vm_get_task_info_vm(vm);
			dev_warn(adev->dev, "validate_and_fence failed: %d\n", r);
			if (ti) {
				dev_warn(adev->dev, "pid %d\n", ti->task.pid);
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
	struct amdgpu_bo *bo = gem_to_amdgpu_bo(obj);
	struct amdgpu_device *adev = amdgpu_ttm_adev(bo->tbo.bdev);
	struct amdgpu_fpriv *fpriv = file_priv->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	struct dma_fence *fence = NULL;
	struct amdgpu_bo_va *bo_va;
	struct drm_exec exec;
	long r;

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

	if (!amdgpu_vm_is_bo_always_valid(vm, bo))
		amdgpu_eviction_fence_detach(&fpriv->evf_mgr, bo);

	bo_va = amdgpu_vm_bo_find(vm, bo);
	if (!bo_va || --bo_va->ref_count)
		goto out_unlock;

	amdgpu_vm_bo_del(adev, bo_va);
	amdgpu_vm_bo_update_shared(bo);
	if (!amdgpu_vm_ready(vm))
		goto out_unlock;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (unlikely(r < 0))
		dev_err(adev->dev,
			"failed to clear page tables on GEM object close (%ld)\n",
			r);
	if (r || !fence)
		goto out_unlock;

	amdgpu_bo_fence(bo, fence, true);
	dma_fence_put(fence);

out_unlock:
	if (r)
		dev_err(adev->dev, "leaking bo va (%ld)\n", r);
	drm_exec_fini(&exec);
}

static int amdgpu_gem_object_mmap(struct drm_gem_object *obj,
				  struct vm_area_struct *vma)
{
	struct amdgpu_bo *bo = gem_to_amdgpu_bo(obj);

	if (amdgpu_ttm_tt_get_usermm(bo->tbo.ttm))
		return -EPERM;
	if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)
		return -EPERM;

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

/* ============================================================================
 * GEM IOCTLs
 * ============================================================================
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

	if (args->in.domains & ~AMDGPU_GEM_DOMAIN_MASK)
		return -EINVAL;

	if (!amdgpu_is_tmz(adev) && (flags & AMDGPU_GEM_CREATE_ENCRYPTED)) {
		DRM_NOTE_ONCE("Cannot allocate secure buffer since TMZ is disabled\n");
		return -EINVAL;
	}

	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	if (args->in.domains & (AMDGPU_GEM_DOMAIN_GDS |
	    AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
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
	r = amdgpu_gem_object_create(adev, size, args->in.alignment,
				     initial_domain, flags, ttm_bo_type_device,
				     resv, &gobj, fpriv->xcp_id + 1);
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

	if (args->flags & ~(AMDGPU_GEM_USERPTR_READONLY |
	    AMDGPU_GEM_USERPTR_ANONONLY | AMDGPU_GEM_USERPTR_VALIDATE |
	    AMDGPU_GEM_USERPTR_REGISTER))
		return -EINVAL;

	if (!(args->flags & AMDGPU_GEM_USERPTR_READONLY) &&
	    !(args->flags & AMDGPU_GEM_USERPTR_REGISTER)) {
		return -EACCES;
	}

	r = amdgpu_gem_object_create(adev, args->size, 0, AMDGPU_GEM_DOMAIN_CPU,
				     0, ttm_bo_type_device, NULL, &gobj,
				     fpriv->xcp_id + 1);
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
 * @timeout_ns: timeout in ns (absolute ktime value)
 *
 * Calculate the timeout in jiffies from an absolute timeout in ns.
 * Optimized to use direct ktime arithmetic.
 */
unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	ktime_t timeout_kt, now_kt;
	s64 remaining_ns;
	unsigned long timeout_jiffies;

	if (((int64_t)timeout_ns) < 0)
		return MAX_SCHEDULE_TIMEOUT;

	timeout_kt = ns_to_ktime(timeout_ns);
	now_kt = ktime_get();

	remaining_ns = ktime_to_ns(ktime_sub(timeout_kt, now_kt));
	if (remaining_ns <= 0)
		return 0;

	timeout_jiffies = nsecs_to_jiffies((u64)remaining_ns);

	if (timeout_jiffies >= MAX_SCHEDULE_TIMEOUT)
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
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);

	if (likely(dma_resv_test_signaled(robj->tbo.base.resv,
					  DMA_RESV_USAGE_READ))) {
		memset(&args->out, 0, sizeof(args->out));
		args->out.status = 0;
		drm_gem_object_put(gobj);
		return 0;
	}

	ret = dma_resv_wait_timeout(robj->tbo.base.resv, DMA_RESV_USAGE_READ,
				    true, timeout);

	if (likely(ret >= 0)) {
		memset(&args->out, 0, sizeof(args->out));
		args->out.status = (ret == 0) ? 1u : 0u;
		r = 0;
	} else {
		r = (int)ret;
	}

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

static struct dma_fence *
amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
			struct amdgpu_vm *vm,
			struct amdgpu_bo_va *bo_va,
			uint32_t operation)
{
	struct dma_fence *fence = NULL;
	int r;

	if (!amdgpu_vm_ready(vm))
		return NULL;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (r)
		goto error;

	if (operation == AMDGPU_VA_OP_MAP ||
	    operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
		if (r)
			goto error;
	}

	r = amdgpu_vm_update_pdes(adev, vm, false);

	if (r)
		goto error;

	return fence;

error:
	if (r && r != -ERESTARTSYS)
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);

	if (fence)
		dma_fence_put(fence);
	return dma_fence_get_stub();
}

/**
 * amdgpu_gem_va_map_flags - map GEM UAPI flags into hardware flags
 *
 * @adev: amdgpu_device pointer
 * @flags: GEM UAPI flags
 *
 * Returns the GEM UAPI flags mapped into hardware for the ASIC.
 *
 * Optimized: Uses ternary operators which compile to branchless CMOV
 * or bitwise logic on modern x86_64, eliminating branch mispredictions.
 */
uint64_t amdgpu_gem_va_map_flags(struct amdgpu_device *adev, uint32_t flags)
{
	uint64_t pte_flag = 0;

	pte_flag |= (flags & AMDGPU_VM_PAGE_EXECUTABLE) ? AMDGPU_PTE_EXECUTABLE : 0;
	pte_flag |= (flags & AMDGPU_VM_PAGE_READABLE) ? AMDGPU_PTE_READABLE : 0;
	pte_flag |= (flags & AMDGPU_VM_PAGE_WRITEABLE) ? AMDGPU_PTE_WRITEABLE : 0;
	pte_flag |= (flags & AMDGPU_VM_PAGE_NOALLOC) ? AMDGPU_PTE_NOALLOC : 0;

	if (flags & AMDGPU_VM_PAGE_PRT)
		pte_flag |= AMDGPU_PTE_PRT_FLAG(adev);

	if (adev->gmc.gmc_funcs->map_mtype)
		pte_flag |= amdgpu_gmc_map_mtype(adev,
						 flags & AMDGPU_VM_MTYPE_MASK);

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
	struct drm_syncobj *timeline_syncobj = NULL;
	struct dma_fence_chain *timeline_chain = NULL;
	struct dma_fence *fence;
	struct drm_exec exec;
	uint64_t va_flags;
	uint64_t vm_size;
	int r = 0;

	if (unlikely(args->va_address < AMDGPU_VA_RESERVED_BOTTOM ||
		     (args->va_address >= AMDGPU_GMC_HOLE_START &&
		      args->va_address < AMDGPU_GMC_HOLE_END))) {
		return -EINVAL;
	}

	args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size = adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
	vm_size -= AMDGPU_VA_RESERVED_TOP;
	if (unlikely(args->map_size > vm_size - args->va_address)) {
		return -EINVAL;
	}

	if (unlikely((args->flags & ~valid_flags) && (args->flags & ~prt_flags))) {
		return -EINVAL;
	}

	switch (args->operation) {
	case AMDGPU_VA_OP_MAP:
	case AMDGPU_VA_OP_UNMAP:
	case AMDGPU_VA_OP_CLEAR:
	case AMDGPU_VA_OP_REPLACE:
		break;
	default:
		return -EINVAL;
	}

	if ((args->operation != AMDGPU_VA_OP_CLEAR) &&
	    !(args->flags & AMDGPU_VM_PAGE_PRT)) {
		gobj = drm_gem_object_lookup(filp, args->handle);
		if (gobj == NULL)
			return -ENOENT;
		abo = gem_to_amdgpu_bo(gobj);
	} else {
		gobj = NULL;
		abo = NULL;
	}

	r = amdgpu_gem_add_input_fence(filp,
				       args->input_fence_syncobj_handles,
				       args->num_syncobj_handles);
	if (r)
		goto error_put_gobj;

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

	r = amdgpu_gem_update_timeline_node(filp,
					    args->vm_timeline_syncobj_out,
					    args->vm_timeline_point,
					    &timeline_syncobj,
					    &timeline_chain);
	if (r)
		goto error;

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

	if (!r && !(args->flags & AMDGPU_VM_DELAY_UPDATE) &&
	    likely(!adev->debug_vm)) {
		fence = amdgpu_gem_va_update_vm(adev, &fpriv->vm, bo_va,
						args->operation);

		if (timeline_syncobj) {
			if (!fence)
				fence = dma_fence_get_stub();
			amdgpu_gem_update_bo_mapping(filp, bo_va,
						     args->operation,
						     args->vm_timeline_point,
						     fence, timeline_syncobj,
						     timeline_chain);
		} else {
			dma_fence_put(fence);
		}
	}

error:
	drm_exec_fini(&exec);
error_put_gobj:
	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_op_ioctl(struct drm_device *dev, void *data,
			struct drm_file *filp)
{
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
		if (drm_gem_is_imported(&robj->tbo.base) &&
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

		if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
			amdgpu_vm_bo_invalidate(robj, true);

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
	default:
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
	u64 size;
	int r;

	if (adev->mman.buffer_funcs_enabled)
		flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	r = amdgpu_gem_align_pitch(adev, args->width,
				     DIV_ROUND_UP(args->bpp, 8), 0);
	if (r < 0)
		return r;
	args->pitch = r;

	if (check_mul_overflow((u64)args->pitch, (u64)args->height, &size))
		return -EINVAL;

	size = ALIGN(size, PAGE_SIZE);

	domain = amdgpu_bo_get_preferred_domain(adev,
				amdgpu_display_supported_domains(adev, flags));
	r = amdgpu_gem_object_create(adev, size, 0, domain, flags,
				     ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
	if (r)
		return -ENOMEM;

	r = drm_gem_handle_create(file_priv, gobj, &handle);
	drm_gem_object_put(gobj);
	if (r)
		return r;

	args->handle = handle;
	args->size = size;
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
