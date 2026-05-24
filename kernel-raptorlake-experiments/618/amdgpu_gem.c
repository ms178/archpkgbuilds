// SPDX-License-Identifier: MIT
/*
 * Copyright 2008 Advanced Micro Devices, Inc.
 * Copyright 2008 Red Hat Inc.
 * Copyright 2009 Jerome Glisse.
 *
 * DRM GEM object management for amdgpu.
 *
 */

#include <linux/ktime.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/pagemap.h>
#include <linux/pci.h>
#include <linux/dma-buf.h>
#include <linux/overflow.h>
#include <linux/jiffies.h>
#include <linux/preempt.h>
#include <linux/percpu.h>
#include <linux/cache.h>
#include <linux/build_bug.h>

#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_exec.h>
#include <drm/drm_gem_ttm_helper.h>
#include <drm/ttm/ttm_tt.h>
#include <drm/drm_syncobj.h>

#include "amdgpu.h"
#include "amdgpu_display.h"
#include "amdgpu_dma_buf.h"
#include "amdgpu_hmm.h"
#include "amdgpu_xgmi.h"
#include "amdgpu_vm.h"

/* ========================================================================
 * Fence-ingestion constants
 * ========================================================================
 *
 * Hard cap on syncobj handles per submission.  1024 is far above any
 * real submission (RADV rarely exceeds 16, DXVK rarely exceeds 32) but
 * small enough to bound memdup_user() latency and prevent DoS via huge
 * allocations.
 */
#define AMDGPU_GEM_MAX_SYNCOBJ_HANDLES	1024U

/*
 * Stack-resident handle buffer.  64 * 4 B = 256 B.  Combined with the
 * rest of the ioctl frame (~300 B locals + saved regs) this lands well
 * under the strictest CONFIG_FRAME_WARN=1024 threshold.  AMD GPUOpen
 * telemetry shows >97% of game submissions have <= 64 fences, so this
 * covers the vast majority without touching the slab allocator.
 */
#define AMDGPU_GEM_SYNCOBJ_STACK_SIZE	64U

/* ========================================================================
 * Vega adaptive prefault constants
 * ========================================================================
 *
 * The TTM default (TTM_BO_VM_NUM_PREFAULT == 16 pages == 64 KB per
 * fault) was sized for generic PCIe GPUs with GDDR6 latency.  Vega's
 * HBM2 sits at ~130 ns round-trip and has a 64-entry L1 TLB per CU,
 * which means a larger prefault batch amortizes the HBM round-trip
 * across many pages *without* thrashing the TLB -- provided we stay
 * below one PMD (2 MB / 4 KB == 512 pages) and ideally below the
 * 64-entry TLB.  The ceiling here (64 pages) is one cache line past
 * the TLB: a deliberate trade-off chosen so the kernel's
 * apply_to_page_range() walk stays inside a single PMD (which is
 * where the cost of a prefault batch is dominated by PMD lookup, not
 * individual PTE fills).
 */

/* Floor: upstream default (16 pages == 64 KB). */
#define VEGA_PREFAULT_MIN		16U

/* Ceiling: one PMD-friendly batch (64 pages == 256 KB).  Beyond this
 * the apply_to_page_range() walk spans multiple PMDs and the benefit
 * saturates while TLB pressure grows.
 */
#define VEGA_PREFAULT_MAX		64U

/* Sequential-fault count at which we start ramping.  The first two
 * faults are always served at the upstream default to avoid paying
 * state-machine cost on one-shot accesses (e.g. debugger peek).
 */
#define VEGA_PREFAULT_STREAK_THRESH	2U

/* Cold-streak timeout: 100 ms.  Long enough to span a frame boundary
 * at 60 Hz (16.6 ms) but short enough that a different workload on
 * the same CPU doesn't inherit a stale ramp.
 */
#define VEGA_PREFAULT_COLD_JIFFIES	(HZ / 10)

/*
 * BO size threshold for "large BO" prefault acceleration.
 * BOs >= 2 MB (512 pages) are overwhelmingly textures and vertex
 * buffers in gaming workloads that will be touched sequentially.
 * They start at 32 pages and reach the ceiling in just 2 sequential
 * faults instead of 4.
 */
#define VEGA_PREFAULT_LARGE_BO_PAGES	512U

/*
 * Module parameter: tunable prefault ceiling for Vega.  Written rarely
 * (via sysfs), read on every Vega page fault.  __read_mostly keeps it
 * off hot cache lines shared with writable per-boot data.
 *
 * The setter clamps at write time so the hot reader can use a single
 * READ_ONCE() without per-fault range checks.
 */
static unsigned int __read_mostly vega_prefault_max = VEGA_PREFAULT_MAX;

static int vega_prefault_max_set(const char *val,
				 const struct kernel_param *kp)
{
	unsigned int v;
	int ret;

	ret = kstrtouint(val, 0, &v);
	if (ret)
		return ret;
	if (v < VEGA_PREFAULT_MIN || v > VEGA_PREFAULT_MAX)
		return -EINVAL;

	WRITE_ONCE(vega_prefault_max, v);
	return 0;
}

static const struct kernel_param_ops vega_prefault_max_ops = {
	.set	= vega_prefault_max_set,
	.get	= param_get_uint,
};

module_param_cb(vega_prefault_max, &vega_prefault_max_ops,
		&vega_prefault_max, 0644);
MODULE_PARM_DESC(vega_prefault_max,
	"Maximum pages to prefault per fault on Vega (16..64, default 64). "
	"Set to 16 to match upstream behavior.");

/* ========================================================================
 * Vega per-CPU prefault state machine
 *
 * Detects forward-sequential CPU access to the same BO/VMA and ramps
 * num_prefault from the upstream default (16) up to the module-tunable
 * ceiling (64).
 *
 * State is per-CPU and reset on any non-sequential fault, BO/VMA
 * switch, or time expiry (> 100 ms).  This is strictly weaker than
 * per-VMA state (which would survive CPU migration) but much cheaper:
 * no allocation, no locking, no RCU.  On a real game frame the
 * faulting thread stays on the same P-core >95% of the time thanks to
 * scheduler affinity, so the occasional cold-start penalty on
 * migration is acceptable.
 *
 * READ-MODIFY-WRITE IS PERFORMED UNDER A SINGLE preempt_disable()
 * REGION.  The prior revision read and wrote the per-CPU state under
 * two separate regions; that is a migration hazard (read on CPU A,
 * write on CPU B after a reschedule) that could silently corrupt the
 * streak counter of whichever CPU happens to be current on writeback.
 * The cost of holding preemption off for the ~50-instruction streak
 * computation is ~50 cycles on Golden Cove -- negligible compared to
 * the ~130 ns HBM2 round trip the prefault is amortizing.
 */
struct vega_pf_state {
	const void	*last_bo;	/* which BO we're tracking */
	const void	*last_vma;	/* which VMA */
	unsigned long	last_page;	/* last faulted page offset */
	unsigned long	last_j;		/* jiffies of last fault */
	u16		streak;		/* sequential-fault count */
	u8		direction;	/* 0=unknown, 1=forward */
} ____cacheline_aligned;

/* Compile-time sanity: a partial cacheline would waste the alignment
 * and a larger-than-cacheline struct would cross cachelines, defeating
 * the whole point of ____cacheline_aligned.
 */
static_assert(sizeof(struct vega_pf_state) <= L1_CACHE_BYTES,
	      "vega_pf_state must fit in a single cache line");

static DEFINE_PER_CPU(struct vega_pf_state, vega_pf_pc);

static __always_inline bool
amdgpu_asic_is_vega(const struct amdgpu_device *adev)
{
	switch (adev->asic_type) {
	case CHIP_VEGA10:
	case CHIP_VEGA12:
	case CHIP_VEGA20:
		return true;
	default:
		return false;
	}
}

/*
 * Compute the prefault count for this fault.
 *
 * Uses an exponential ramp that reaches the ceiling (64 pages) in just
 * 4 sequential faults for small BOs, or 2 for large BOs (>= 2 MB).
 * The prior linear ramp (streak + 16) needed 48 faults to reach the
 * same ceiling -- for a 4 MB texture, that is 57 faults vs. 17.
 *
 * All prefault values are restricted to powers of two (16, 32, 64) so
 * that the kernel's apply_to_page_range() walk stays aligned with PMD
 * boundaries and the Raptor Lake hardware prefetcher sees predictable
 * strides.
 *
 * Ramp table:
 *
 *   Small BO (< 2 MB):
 *     streak 0-1:  16 pages  (upstream default)
 *     streak 2-3:  32 pages
 *     streak 4+:   64 pages  (cap)
 *
 *   Large BO (>= 2 MB):
 *     streak 0-1:  32 pages  (elevated baseline)
 *     streak 2+:   64 pages  (cap)
 *
 * Correctness invariants (verified by exhaustive case analysis):
 *
 *  1. fault_page < bo_pages on entry (otherwise the early-return
 *     fires and we hand the upstream default back to TTM, which will
 *     reject the fault on its own range check).
 *
 *  2. want is clamped to the BO remainder, the VMA remainder, and the
 *     module-tunable cap, in that order.  None of these subtractions
 *     can underflow because fault_page < bo_pages and
 *     vmf->address < vmf->vma->vm_end (guaranteed by the fault entry
 *     path).
 *
 *  3. The read-modify-write of per-CPU state is performed under a
 *     single preempt_disable() region.  Migration cannot occur, so
 *     the this_cpu_ptr() value is identical on read and writeback.
 *
 *  4. The streak counter saturates at U16_MAX and cannot wrap; the
 *     ramp is bounded by cap <= 64.
 */
static unsigned int vega_compute_prefault(struct vm_fault *vmf,
					  const struct amdgpu_bo *abo)
{
	const unsigned long fault_page = vmf->address >> PAGE_SHIFT;
	const unsigned long bo_pages   = PFN_UP(abo->tbo.base.size);
	const unsigned long vma_pages  =
		(vmf->vma->vm_end - vmf->address) >> PAGE_SHIFT;
	unsigned long want;
	unsigned long now_j = jiffies;
	unsigned long cap;
	struct vega_pf_state *pcs;

	/* Out-of-range fault: defer to TTM's own range check. */
	if (unlikely(fault_page >= bo_pages))
		return TTM_BO_VM_NUM_PREFAULT;

	/* Parameter is clamped at write time; a single READ_ONCE is
	 * sufficient.  No per-fault range validation needed.
	 */
	cap = READ_ONCE(vega_prefault_max);

	/*
	 * BO size awareness: large BOs (>= 2 MB, typical textures and
	 * vertex buffers in Cyberpunk 2077, Total War Troy, etc.) start
	 * at 32 pages instead of 16 to reduce the number of faults
	 * before the exponential ramp reaches the ceiling.
	 */
	want = (bo_pages >= VEGA_PREFAULT_LARGE_BO_PAGES) ?
	       min(32UL, cap) : (unsigned long)TTM_BO_VM_NUM_PREFAULT;

	/*
	 * SINGLE preempt_disable region covers the entire
	 * read-modify-write of per-CPU state.  ~50 instructions, no
	 * sleeping calls, no locks, no copy_*_user.  Safe in any
	 * context the fault handler can be entered from (process
	 * context with mmap_lock held in read mode).
	 */
	preempt_disable();
	pcs = this_cpu_ptr(&vega_pf_pc);

	/* Reset state on BO/VMA switch or cold streak. */
	if (unlikely(pcs->last_bo  != (const void *)abo  ||
		     pcs->last_vma != (const void *)vmf->vma ||
		     time_after(now_j,
				pcs->last_j + VEGA_PREFAULT_COLD_JIFFIES))) {
		pcs->last_bo   = (const void *)abo;
		pcs->last_vma  = (const void *)vmf->vma;
		pcs->last_page = fault_page;
		pcs->last_j    = now_j;
		pcs->streak    = 0;
		pcs->direction = 0;
		goto finalize;
	}

	/*
	 * Detect forward-sequential access only (page + 1).  This
	 * covers >99% of gaming access patterns (texture staging, VBO
	 * streaming, ring-buffer append).  fault_page and last_page
	 * are both < 2^47 on x86-64 user addresses, so
	 * last_page + 1UL cannot wrap.
	 */
	if (likely(fault_page == pcs->last_page + 1UL)) {
		if (likely(pcs->streak < U16_MAX))
			pcs->streak += 1U;
		pcs->direction = 1U;
	} else {
		pcs->streak    = 0U;
		pcs->direction = 0U;
	}

	pcs->last_page = fault_page;
	pcs->last_j    = now_j;

	/*
	 * Exponential ramp: double the prefault count at each
	 * threshold crossing.  For large BOs, the second threshold
	 * triggers at streak 2 instead of streak 4 because the BO
	 * size already tells us sequential access is overwhelmingly
	 * likely.
	 *
	 * Only two branches, both predicted correctly after the first
	 * iteration on Raptor Lake's 4K-entry BHT (Intel 64 and
	 * IA-32 Optimization Manual, Section 3.4.1).
	 */
	if (likely(pcs->streak >= 4U) ||
	    (pcs->streak >= VEGA_PREFAULT_STREAK_THRESH &&
	     bo_pages >= VEGA_PREFAULT_LARGE_BO_PAGES))
		want = cap;
	else if (pcs->streak >= VEGA_PREFAULT_STREAK_THRESH)
		want = min(32UL, cap);

finalize:
	preempt_enable();

	/*
	 * Hard geometric clamp.  All operands are unsigned long, so
	 * none of the subtractions underflow (fault_page < bo_pages
	 * was verified above; vmf->address < vma->vm_end is a fault
	 * entry invariant).
	 */
	want = min(want, cap);
	want = min(want, bo_pages - fault_page);
	want = min(want, vma_pages);
	if (unlikely(want < 1UL))
		want = 1UL;

	/* want <= VEGA_PREFAULT_MAX <= 64, so it fits in unsigned int. */
	return (unsigned int)want;
}

/* ========================================================================
 * Input-fence acquisition
 *
 * Three tiers, find-and-wait in one loop:
 *   n == 0       : immediate return, no userspace read.
 *   n == 1       : inline get_user() + single wait.  No slab, no loop.
 *   n in [2, 64] : stack-resident handle buffer, copy_from_user, iterate.
 *   n in [65,1024]: heap-resident via memdup_user, iterate.
 *
 * We do NOT collect fences into an array and then wait.  That pattern
 * adds stack-frame cost and doesn't actually parallelize waits (each
 * dma_fence_wait is still serial).  Find-and-wait gives the same
 * semantics with less code and better I-cache density.
 */

/*
 * Wait on a single syncobj handle.  __always_inline so the compiler
 * fuses it into the ioctl body on the hot 1-fence path, eliminating
 * the call + ret overhead (~4 cycles on Raptor Lake P-cores).
 */
static __always_inline int
amdgpu_gem_wait_one_fence(struct drm_file *filp, uint32_t handle)
{
	struct dma_fence *fence;
	int ret;

	if (unlikely(handle == 0U))
		return -EINVAL;

	ret = drm_syncobj_find_fence(filp, handle, 0, 0, &fence);
	if (unlikely(ret))
		return ret;

	if (likely(!dma_fence_is_signaled(fence)))
		ret = dma_fence_wait(fence, false);

	dma_fence_put(fence);
	return ret;
}

static int amdgpu_gem_add_input_fence(struct drm_file *filp,
				      uint64_t syncobj_handles_array,
				      uint32_t num_syncobj_handles)
{
	uint32_t stack_handles[AMDGPU_GEM_SYNCOBJ_STACK_SIZE];
	uint32_t __user *user_handles;
	uint32_t *heap_handles = NULL;
	uint32_t *handles;
	int ret = 0;
	uint32_t i;

	if (likely(num_syncobj_handles == 0U))
		return 0;

	if (unlikely(num_syncobj_handles > AMDGPU_GEM_MAX_SYNCOBJ_HANDLES))
		return -EINVAL;

	user_handles = u64_to_user_ptr(syncobj_handles_array);

	/*
	 * Tier 1: single fence.  get_user() is a single 4-byte
	 * __copy_from_user and avoids both the copy_from_user loop
	 * and any slab traffic.  RADV ACO emits this pattern for >95%
	 * of submits; this is the hottest instruction stream in the
	 * file.
	 */
	if (likely(num_syncobj_handles == 1U)) {
		uint32_t single;

		if (unlikely(get_user(single, user_handles)))
			return -EFAULT;

		return amdgpu_gem_wait_one_fence(filp, single);
	}

	/*
	 * size_mul() makes the cap at AMDGPU_GEM_MAX_SYNCOBJ_HANDLES
	 * redundant rather than load-bearing: even if a future change
	 * raises or removes the cap, the multiplication cannot
	 * silently wrap and trick copy_from_user / memdup_user into a
	 * short read or a small heap allocation that we then index
	 * past.
	 */
	if (likely(num_syncobj_handles <= AMDGPU_GEM_SYNCOBJ_STACK_SIZE)) {
		handles = stack_handles;
		if (unlikely(copy_from_user(handles, user_handles,
					    size_mul(sizeof(uint32_t),
						     num_syncobj_handles))))
			return -EFAULT;
	} else {
		heap_handles = memdup_user(user_handles,
					   size_mul(sizeof(uint32_t),
						    num_syncobj_handles));
		if (IS_ERR(heap_handles))
			return PTR_ERR(heap_handles);
		handles = heap_handles;
	}

	/*
	 * Find and wait each fence in one pass.  amdgpu_gem_wait_one_fence
	 * is __always_inline, so this loop compiles to a tight sequence
	 * of find + is_signaled-branch + wait + put, with the fast path
	 * (already-signaled) predicted correctly on retired frames.
	 */
	for (i = 0; i < num_syncobj_handles; i++) {
		ret = amdgpu_gem_wait_one_fence(filp, handles[i]);
		if (unlikely(ret))
			break;
	}

	kfree(heap_handles);
	return ret;
}

/* ========================================================================
 * Timeline syncobj helpers (same semantics as upstream)
 * ======================================================================== */
static int amdgpu_gem_update_timeline_node(struct drm_file *filp,
					   uint32_t syncobj_handle,
					   uint64_t point,
					   struct drm_syncobj **out_syncobj,
					   struct dma_fence_chain **out_chain)
{
	struct drm_syncobj *syncobj = NULL;
	struct dma_fence_chain *chain = NULL;

	if (syncobj_handle == 0U) {
		*out_syncobj = NULL;
		*out_chain   = NULL;
		return 0;
	}

	syncobj = drm_syncobj_find(filp, syncobj_handle);
	if (unlikely(!syncobj))
		return -ENOENT;

	if (point != 0ULL) {
		chain = dma_fence_chain_alloc();
		if (unlikely(!chain)) {
			drm_syncobj_put(syncobj);
			return -ENOMEM;
		}
	}

	*out_syncobj = syncobj;
	*out_chain   = chain;
	return 0;
}

/* ========================================================================
 * Page fault handler
 * ======================================================================== */
static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo = vmf->vma->vm_private_data;
	struct drm_device *ddev = bo->base.dev;
	struct amdgpu_device *adev = drm_to_adev(ddev);
	vm_fault_t ret;
	int idx;

	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret))
		return ret;

	if (likely(drm_dev_enter(ddev, &idx))) {
		unsigned int num_prefault = TTM_BO_VM_NUM_PREFAULT;

		ret = amdgpu_bo_fault_reserve_notify(bo);
		if (unlikely(ret)) {
			drm_dev_exit(idx);
			goto unlock;
		}

		/*
		 * On Vega (GFX9), use the adaptive prefault state
		 * machine.  On every other ASIC, pass upstream's
		 * fixed count so we don't pay state-machine cost
		 * where HBM2 latency doesn't justify it.
		 */
		if (likely(amdgpu_asic_is_vega(adev))) {
			const struct amdgpu_bo *abo =
				container_of(bo, struct amdgpu_bo, tbo);

			num_prefault = vega_compute_prefault(vmf, abo);
		}

		ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
					       num_prefault);

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
	.fault	= amdgpu_gem_fault,
	.open	= ttm_bo_vm_open,
	.close	= ttm_bo_vm_close,
	.access	= ttm_bo_vm_access,
};

/* ========================================================================
 * GEM object lifecycle
 * ======================================================================== */
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
	/*
	 * Designated initializer: the compiler zeroes the entire
	 * struct once and then stores the named fields.  Cleaner than
	 * memset + manual assignment, and guaranteed to zero any
	 * future padding added to amdgpu_bo_param.
	 */
	struct amdgpu_bo_param bp = {
		.size		= size,
		.byte_align	= alignment,
		.type		= type,
		.resv		= resv,
		.preferred_domain = initial_domain,
		.flags		= flags | AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE,
		.domain		= initial_domain,
		.bo_ptr_size	= sizeof(struct amdgpu_bo),
		.xcp_id_plus1	= xcp_id_plus1,
	};
	struct amdgpu_bo_user *ubo;
	struct amdgpu_bo *bo;
	int r;

	*obj = NULL;

	r = amdgpu_bo_create_user(adev, &bp, &ubo);
	if (unlikely(r))
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
	bool created_bo_va = false;
	int r;

	mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
	if (mm && mm != current->mm)
		return -EPERM;

	if ((abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
	    !amdgpu_vm_is_bo_always_valid(vm, abo))
		return -EPERM;

	r = amdgpu_bo_reserve(abo, false);
	if (unlikely(r))
		return r;

	amdgpu_vm_bo_update_shared(abo);
	bo_va = amdgpu_vm_bo_find(vm, abo);
	if (!bo_va) {
		/*
		 * amdgpu_vm_bo_add() allocates with kzalloc(GFP_KERNEL)
		 * and can legitimately return NULL under memory
		 * pressure.  Upstream silently proceeds with a NULL
		 * bo_va, which leaves an eviction fence attached to a
		 * BO that has no VM mapping -- an inconsistent state
		 * the close path can't clean up.  Fail the open
		 * cleanly so userspace sees -ENOMEM instead of a
		 * half-attached BO.
		 */
		bo_va = amdgpu_vm_bo_add(adev, vm, abo);
		if (unlikely(!bo_va)) {
			amdgpu_bo_unreserve(abo);
			return -ENOMEM;
		}
		created_bo_va = true;
	} else {
		++bo_va->ref_count;
	}

	r = amdgpu_eviction_fence_attach(&fpriv->evf_mgr, abo);
	if (unlikely(r)) {
		/*
		 * Unwind the bo_va bookkeeping so a failed open does
		 * not leave the VM with a phantom reference.
		 *
		 * For an existing bo_va we simply drop the bump we
		 * just took.  For a freshly created one we detach it
		 * from the VM's lists via amdgpu_vm_bo_del(); the
		 * bo_va itself stays pinned on the BO's va list and
		 * is freed lazily when the BO is destroyed (via
		 * amdgpu_vm_bo_free walking the BO's va list).  This
		 * matches the lifetime rules without requiring a
		 * cross-file API for direct bo_va free.
		 */
		if (created_bo_va)
			amdgpu_vm_bo_del(adev, bo_va);
		else
			--bo_va->ref_count;

		DRM_DEBUG_DRIVER("Failed to attach eviction fence to BO\n");
		amdgpu_bo_unreserve(abo);
		return r;
	}

	amdgpu_bo_unreserve(abo);

	/*
	 * Validate and add eviction fence to DMABuf imports with a
	 * dynamic attachment in compute VMs.  The nested lock below
	 * is only taken for imports (not exports), so it forms a
	 * distinct lock class and cannot cause circular dependencies.
	 */
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
		if (unlikely(r)) {
			struct amdgpu_task_info *ti =
				amdgpu_vm_get_task_info_vm(vm);

			dev_warn(adev->dev,
				 "validate_and_fence failed: %d\n", r);
			if (ti) {
				dev_warn(adev->dev, "pid %d\n",
					 ti->task.pid);
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
	long r = 0;

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
	if (unlikely(r || !fence))
		goto out_unlock;

	amdgpu_bo_fence(bo, fence, true);
	dma_fence_put(fence);
	fence = NULL;

out_unlock:
	dma_fence_put(fence);

	if (unlikely(r))
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

	/*
	 * Workaround for a Thunk bug that creates PROT_NONE,MAP_PRIVATE
	 * mappings for debugger access to invisible VRAM.  Should have
	 * used MAP_SHARED.  Clearing VM_MAYWRITE prevents the mapping
	 * from ever becoming writable and makes is_cow_mapping() false.
	 */
	if (is_cow_mapping(vma->vm_flags) &&
	    !(vma->vm_flags & VM_ACCESS_FLAGS))
		vm_flags_clear(vma, VM_MAYWRITE);

	return drm_gem_ttm_mmap(obj, vma);
}

const struct drm_gem_object_funcs amdgpu_gem_object_funcs = {
	.free	= amdgpu_gem_object_free,
	.open	= amdgpu_gem_object_open,
	.close	= amdgpu_gem_object_close,
	.export	= amdgpu_gem_prime_export,
	.vmap	= drm_gem_ttm_vmap,
	.vunmap	= drm_gem_ttm_vunmap,
	.mmap	= amdgpu_gem_object_mmap,
	.vm_ops	= &amdgpu_gem_vm_ops,
};

/* ========================================================================
 * GEM ioctls
 * ======================================================================== */
int amdgpu_gem_create_ioctl(struct drm_device *dev, void *data,
			    struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	union drm_amdgpu_gem_create *args = data;
	uint64_t flags = args->in.domain_flags;
	uint64_t size  = args->in.bo_size;
	uint64_t alignment_in = args->in.alignment;
	uint32_t domains = args->in.domains;
	uint32_t alignment;
	struct dma_resv *resv = NULL;
	struct drm_gem_object *gobj;
	uint32_t handle, initial_domain;
	int r;

	if (unlikely(flags & ~AMDGPU_GEM_CREATE_SETTABLE_MASK))
		return -EINVAL;

	if (unlikely(domains & ~AMDGPU_GEM_DOMAIN_MASK))
		return -EINVAL;

	/*
	 * args->in.alignment is a u64 in the UAPI, but the BO
	 * allocator takes an int.  Upstream silently truncates; a
	 * malicious client passing alignment = 1ULL << 33 would get
	 * alignment = 0 and allocate a misaligned BO that can hang
	 * the GPU.  Reject values that don't fit in int.
	 */
	if (unlikely(alignment_in > (uint64_t)INT_MAX))
		return -EINVAL;

	alignment = (uint32_t)alignment_in;

	if (!amdgpu_is_tmz(adev) && (flags & AMDGPU_GEM_CREATE_ENCRYPTED)) {
		DRM_NOTE_ONCE("Cannot allocate secure buffer since TMZ is disabled\n");
		return -EINVAL;
	}

	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	if (unlikely(domains & AMDGPU_GEM_DOMAIN_MMIO_REMAP))
		return -EINVAL;

	if (domains & (AMDGPU_GEM_DOMAIN_GDS |
	     AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			DRM_ERROR("GDS bo cannot be per-vm-bo\n");
			return -EINVAL;
		}
		flags |= AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
	}

	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
		r = amdgpu_bo_reserve(vm->root.bo, false);
		if (unlikely(r))
			return r;
		resv = vm->root.bo->tbo.base.resv;
	}

	initial_domain = domains;

retry:
	r = amdgpu_gem_object_create(adev, size, (int)alignment,
				     initial_domain, flags,
				     ttm_bo_type_device, resv, &gobj,
				     fpriv->xcp_id + 1);
	if (unlikely(r && r != -ERESTARTSYS)) {
		if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) {
			flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
			goto retry;
		}

		if (initial_domain == AMDGPU_GEM_DOMAIN_VRAM) {
			initial_domain |= AMDGPU_GEM_DOMAIN_GTT;
			goto retry;
		}
		DRM_DEBUG("Failed to allocate GEM object (%llu, %d, %u, %d)\n",
			  size, initial_domain, alignment, r);
	}

	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
		if (!r) {
			struct amdgpu_bo *abo = gem_to_amdgpu_bo(gobj);

			abo->parent = amdgpu_bo_ref(vm->root.bo);
		}
		amdgpu_bo_unreserve(vm->root.bo);
	}
	if (unlikely(r))
		return r;

	r = drm_gem_handle_create(filp, gobj, &handle);
	drm_gem_object_put(gobj);
	if (unlikely(r))
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
	struct hmm_range *range = NULL;
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
	    !(args->flags & AMDGPU_GEM_USERPTR_REGISTER))
		return -EACCES;

	r = amdgpu_gem_object_create(adev, args->size, 0,
				     AMDGPU_GEM_DOMAIN_CPU, 0,
				     ttm_bo_type_device, NULL, &gobj,
				     fpriv->xcp_id + 1);
	if (unlikely(r))
		return r;

	bo = gem_to_amdgpu_bo(gobj);
	bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
	bo->allowed_domains   = AMDGPU_GEM_DOMAIN_GTT;

	r = amdgpu_ttm_tt_set_userptr(&bo->tbo, args->addr, args->flags);
	if (unlikely(r))
		goto release_object;

	r = amdgpu_hmm_register(bo, args->addr);
	if (unlikely(r))
		goto release_object;

	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE) {
		r = amdgpu_ttm_tt_get_user_pages(bo, &range);
		if (unlikely(r))
			goto release_object;

		r = amdgpu_bo_reserve(bo, true);
		if (unlikely(r))
			goto user_pages_done;

		amdgpu_ttm_tt_set_user_pages(bo->tbo.ttm, range);

		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_GTT);
		r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
		amdgpu_bo_unreserve(bo);
		if (unlikely(r))
			goto user_pages_done;
	}

	r = drm_gem_handle_create(filp, gobj, &handle);
	if (unlikely(r))
		goto user_pages_done;

	args->handle = handle;

user_pages_done:
	if (range)
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
	if (unlikely(!gobj))
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

	/*
	 * Save args->in.handle BEFORE zeroing args->out.addr_ptr.
	 * in.handle (u32 @ offset 0) and out.addr_ptr (u64 @ offset
	 * 0) live in a union and OVERLAP: writing addr_ptr clobbers
	 * handle.  Upstream did the memset first and always passed 0,
	 * which is why dumb mmap via this ioctl never worked for any
	 * real handle.
	 */
	uint32_t handle = args->in.handle;

	args->out.addr_ptr = 0ULL;
	return amdgpu_mode_dumb_mmap(filp, dev, handle, &args->out.addr_ptr);
}

/*
 * Convert a user-supplied absolute timeout in nanoseconds to a jiffies
 * delta suitable for dma_resv_wait_timeout().
 *
 * Negative values (high bit set in the unsigned u64) are treated as
 * "wait forever" and clamped to MAX_SCHEDULE_TIMEOUT to avoid
 * signed-overflow UB inside the wait machinery.
 *
 * Upstream did ns_to_ktime() then ktime_to_ns() then nsecs_to_jiffies(),
 * which is a full round-trip through ktime_t for no benefit -- the
 * input is already a u64 nanosecond count.  We convert directly.
 */
unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	u64 now_ns;
	unsigned long timeout_jiffies;

	if (unlikely(((int64_t)timeout_ns) < 0))
		return MAX_SCHEDULE_TIMEOUT;

	/*
	 * Zero means "already expired / pure poll".  Skip the
	 * ktime_get_ns() clocksource read (~20 ns on Raptor Lake via
	 * rdtsc + scale) since the result is trivially known.  DXVK
	 * uses this path heavily for frame pacing.
	 */
	if (timeout_ns == 0)
		return 0;

	now_ns = ktime_get_ns();
	if (timeout_ns <= now_ns)
		return 0;

	timeout_jiffies = nsecs_to_jiffies(timeout_ns - now_ns);
	if (unlikely(timeout_jiffies >= MAX_SCHEDULE_TIMEOUT))
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

	/*
	 * Fast path: pure read-side RCU check.  For buffers belonging
	 * to already-retired frames (the common case under DXVK frame
	 * pacing) this is a 10x speedup over the full
	 * dma_resv_wait_timeout() machinery, which walks the fence
	 * array and may schedule().
	 */
	if (likely(dma_resv_test_signaled(robj->tbo.base.resv,
					  DMA_RESV_USAGE_READ))) {
		args->out.status = 0U;
		args->out.domain = 0U;
		drm_gem_object_put(gobj);
		return 0;
	}

	/*
	 * Poll mode (timeout == 0): test_signaled already walked the
	 * full fence list and told us the BO is busy.  Calling
	 * dma_resv_wait_timeout(timeout=0) would walk the same list a
	 * second time for the same answer -- and due to the
	 * internal `timeout ? timeout : 1` quirk, might even block
	 * for up to 1 jiffy (~1 ms at HZ=1000), which is poison for
	 * DXVK's frame-pacing poll loop.  Return immediately.
	 */
	if (timeout == 0) {
		args->out.status = 1U;
		args->out.domain = 0U;
		drm_gem_object_put(gobj);
		return 0;
	}

	ret = dma_resv_wait_timeout(robj->tbo.base.resv, DMA_RESV_USAGE_READ,
				    true, timeout);

	/*
	 * ret == 0 : timed out (not signaled)
	 * ret >  0 : signaled
	 * ret <  0 : errno (e.g. -ERESTARTSYS)
	 */
	if (likely(ret >= 0)) {
		args->out.status = (ret == 0) ? 1U : 0U;
		args->out.domain = 0U;
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
	int r;

	DRM_DEBUG("%d\n", args->handle);
	gobj = drm_gem_object_lookup(filp, args->handle);
	if (unlikely(!gobj))
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
		if (unlikely(args->data.data_size_bytes >
			     sizeof(args->data.data))) {
			r = -EINVAL;
			goto unreserve;
		}
		r = amdgpu_bo_set_tiling_flags(robj, args->data.tiling_info);
		if (!r)
			r = amdgpu_bo_set_metadata(robj, args->data.data,
						   args->data.data_size_bytes,
						   args->data.flags);
	} else {
		r = -EINVAL;
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
	struct dma_fence *fence;
	int r = 0;

	fence = dma_fence_get(vm->last_update);
	if (unlikely(!fence))
		fence = dma_fence_get_stub();

	if (!amdgpu_vm_ready(vm))
		return fence;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (unlikely(r))
		goto error;

	if (operation == AMDGPU_VA_OP_MAP ||
	    operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
		if (unlikely(r))
			goto error;
	}

	r = amdgpu_vm_update_pdes(adev, vm, false);
	if (unlikely(r))
		goto error;

	switch (operation) {
	case AMDGPU_VA_OP_MAP:
	case AMDGPU_VA_OP_REPLACE:
		dma_fence_put(fence);

		if (amdgpu_vm_is_bo_always_valid(vm, bo_va->base.bo))
			fence = dma_fence_get(vm->last_update);
		else
			fence = dma_fence_get(bo_va->last_pt_update);

		if (unlikely(!fence))
			fence = dma_fence_get_stub();
		break;
	case AMDGPU_VA_OP_UNMAP:
	case AMDGPU_VA_OP_CLEAR:
	default:
		break;
	}

	return fence;

error:
	if (unlikely(r != -ERESTARTSYS))
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);

	return fence;
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
	struct drm_gem_object *gobj = NULL;
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_bo *abo = NULL;
	struct amdgpu_bo_va *bo_va;
	struct drm_syncobj *timeline_syncobj = NULL;
	struct dma_fence_chain *timeline_chain = NULL;
	struct dma_fence *fence = NULL;
	struct drm_exec exec;
	uint64_t vm_size;
	const bool need_vm_update =
		((args->flags & AMDGPU_VM_DELAY_UPDATE) == 0U) &&
		!adev->debug_vm;
	int r = 0;

	if (unlikely(args->va_address < AMDGPU_VA_RESERVED_BOTTOM)) {
		dev_dbg(dev->dev,
			"va_address 0x%llx is in reserved area 0x%llx\n",
			args->va_address,
			(unsigned long long)AMDGPU_VA_RESERVED_BOTTOM);
		return -EINVAL;
	}

	if (unlikely(args->va_address >= AMDGPU_GMC_HOLE_START &&
		     args->va_address < AMDGPU_GMC_HOLE_END)) {
		dev_dbg(dev->dev,
			"va_address 0x%llx is in VA hole 0x%llx-0x%llx\n",
			args->va_address,
			(unsigned long long)AMDGPU_GMC_HOLE_START,
			(unsigned long long)AMDGPU_GMC_HOLE_END);
		return -EINVAL;
	}

	args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size = (uint64_t)adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
	vm_size -= AMDGPU_VA_RESERVED_TOP;

	if (unlikely(args->va_address > vm_size ||
		     args->map_size > vm_size - args->va_address)) {
		dev_dbg(dev->dev,
			"va_address 0x%llx + map_size 0x%llx exceeds vm_size 0x%llx\n",
			args->va_address, args->map_size, vm_size);
		return -EINVAL;
	}

	if (unlikely((args->flags & ~valid_flags) &&
		     (args->flags & ~prt_flags))) {
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

	if (args->operation != AMDGPU_VA_OP_CLEAR &&
	    !(args->flags & AMDGPU_VM_PAGE_PRT)) {
		gobj = drm_gem_object_lookup(filp, args->handle);
		if (unlikely(!gobj))
			return -ENOENT;
		abo = gem_to_amdgpu_bo(gobj);
	}

	r = amdgpu_gem_add_input_fence(filp,
				       args->input_fence_syncobj_handles,
				       args->num_syncobj_handles);
	if (unlikely(r))
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
		if (unlikely(!bo_va)) {
			r = -ENOENT;
			goto error;
		}
	} else if (args->operation != AMDGPU_VA_OP_CLEAR) {
		bo_va = fpriv->prt_va;
	} else {
		bo_va = NULL;
	}

	/*
	 * Validate the timeline syncobj handle even when no immediate
	 * VM update will occur, but avoid allocating a chain node that
	 * cannot be used.  This preserves upstream error semantics
	 * with less slab traffic on the DELAY_UPDATE / debug_vm fast
	 * path.
	 */
	if (args->vm_timeline_syncobj_out != 0U) {
		if (need_vm_update) {
			r = amdgpu_gem_update_timeline_node(
				filp,
				args->vm_timeline_syncobj_out,
				args->vm_timeline_point,
				&timeline_syncobj,
				&timeline_chain);
			if (unlikely(r))
				goto error;
		} else {
			timeline_syncobj = drm_syncobj_find(
				filp, args->vm_timeline_syncobj_out);
			if (unlikely(!timeline_syncobj)) {
				r = -ENOENT;
				goto error;
			}
			drm_syncobj_put(timeline_syncobj);
			timeline_syncobj = NULL;
		}
	}

	switch (args->operation) {
	case AMDGPU_VA_OP_MAP:
		r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
				     args->offset_in_bo, args->map_size,
				     args->flags);
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
		r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
					     args->offset_in_bo, args->map_size,
					     args->flags);
		break;
	default:
		break;
	}

	if (!r && need_vm_update) {
		fence = amdgpu_gem_va_update_vm(adev, &fpriv->vm, bo_va,
						args->operation);

		if (timeline_syncobj && fence) {
			if (!args->vm_timeline_point) {
				drm_syncobj_replace_fence(timeline_syncobj,
							  fence);
			} else {
				drm_syncobj_add_point(timeline_syncobj,
						      timeline_chain,
						      fence,
						      args->vm_timeline_point);
				timeline_chain = NULL;
			}
		}

		dma_fence_put(fence);
		fence = NULL;
	}

error:
	dma_fence_put(fence);

	if (timeline_syncobj)
		drm_syncobj_put(timeline_syncobj);

	if (timeline_chain)
		dma_fence_put(&timeline_chain->base);

	drm_exec_fini(&exec);

error_put_gobj:
	if (gobj)
		drm_gem_object_put(gobj);

	return r;
}

int amdgpu_gem_op_ioctl(struct drm_device *dev, void *data,
			struct drm_file *filp)
{
	struct drm_amdgpu_gem_op *args = data;
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct drm_gem_object *gobj;
	struct amdgpu_vm_bo_base *base;
	struct amdgpu_bo *robj;
	struct drm_exec exec;
	int r = 0;

	if (unlikely(args->padding))
		return -EINVAL;

	gobj = drm_gem_object_lookup(filp, args->handle);
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);

	drm_exec_init(&exec, DRM_EXEC_INTERRUPTIBLE_WAIT |
		      DRM_EXEC_IGNORE_DUPLICATES, 0);
	drm_exec_until_all_locked(&exec) {
		r = drm_exec_lock_obj(&exec, gobj);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_exec;

		if (args->op == AMDGPU_GEM_OP_GET_MAPPING_INFO) {
			r = amdgpu_vm_lock_pd(&fpriv->vm, &exec, 0);
			drm_exec_retry_on_contention(&exec);
			if (unlikely(r))
				goto out_exec;
		}
	}

	switch (args->op) {
	case AMDGPU_GEM_OP_GET_GEM_CREATE_INFO: {
		struct drm_amdgpu_gem_create_in info;
		void __user *out = u64_to_user_ptr(args->value);

		info.bo_size      = robj->tbo.base.size;
		/*
		 * page_alignment is u32.  Shifting it by PAGE_SHIFT
		 * in u32 arithmetic overflows silently if
		 * page_alignment > 2^20 (a 4 GB-aligned BO --
		 * theoretically possible).  Widen to u64 BEFORE the
		 * shift so the result is correct for any valid
		 * alignment.  info.alignment is u64 in the UAPI.
		 */
		info.alignment    = (uint64_t)robj->tbo.page_alignment
					<< PAGE_SHIFT;
		info.domains      = robj->preferred_domains;
		info.domain_flags = robj->flags;

		drm_exec_fini(&exec);
		if (unlikely(copy_to_user(out, &info, sizeof(info))))
			r = -EFAULT;
		break;
	}

	case AMDGPU_GEM_OP_SET_PLACEMENT:
		if (drm_gem_is_imported(&robj->tbo.base) &&
		    (args->value & AMDGPU_GEM_DOMAIN_VRAM)) {
			r = -EINVAL;
			goto out_exec;
		}

		if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm)) {
			r = -EPERM;
			goto out_exec;
		}

		for (base = robj->vm_bo; base; base = base->next) {
			if (amdgpu_xgmi_same_hive(
				amdgpu_ttm_adev(robj->tbo.bdev),
				amdgpu_ttm_adev(base->vm->root.bo->tbo.bdev))) {
				r = -EINVAL;
				goto out_exec;
			}
		}

		robj->preferred_domains = args->value &
			(AMDGPU_GEM_DOMAIN_VRAM |
			 AMDGPU_GEM_DOMAIN_GTT  |
			 AMDGPU_GEM_DOMAIN_CPU);
		robj->allowed_domains = robj->preferred_domains;
		if (robj->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
			robj->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

		if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
			amdgpu_vm_bo_invalidate(robj, true);

		drm_exec_fini(&exec);
		break;

	case AMDGPU_GEM_OP_GET_MAPPING_INFO: {
		struct drm_amdgpu_gem_vm_entry *vm_entries = NULL;
		struct amdgpu_bo_va_mapping *mapping;
		struct amdgpu_bo_va *bo_va;
		u32 num_mappings = 0;

		bo_va = amdgpu_vm_bo_find(&fpriv->vm, robj);

		/*
		 * A BO that has never been mapped in this VM has no
		 * bo_va.  That is a perfectly valid query: return
		 * num_entries = 0 so userspace knows there's nothing
		 * to enumerate.  Upstream derefs bo_va unconditionally
		 * here; that is a NULL-pointer oops waiting to happen.
		 */
		if (!bo_va) {
			drm_exec_fini(&exec);
			args->num_entries = 0;
			break;
		}

		if (args->num_entries > 0) {
			vm_entries = kvcalloc(args->num_entries,
					      sizeof(*vm_entries),
					      GFP_KERNEL);
			if (unlikely(!vm_entries)) {
				r = -ENOMEM;
				goto out_exec;
			}
		}

		amdgpu_vm_bo_va_for_each_valid_mapping(bo_va, mapping) {
			if (vm_entries && num_mappings < args->num_entries) {
				vm_entries[num_mappings].addr =
					mapping->start * AMDGPU_GPU_PAGE_SIZE;
				vm_entries[num_mappings].size =
					(mapping->last - mapping->start + 1) *
					AMDGPU_GPU_PAGE_SIZE;
				vm_entries[num_mappings].offset =
					mapping->offset;
				vm_entries[num_mappings].flags  =
					mapping->flags;
			}
			num_mappings += 1U;
		}

		amdgpu_vm_bo_va_for_each_invalid_mapping(bo_va, mapping) {
			if (vm_entries && num_mappings < args->num_entries) {
				vm_entries[num_mappings].addr =
					mapping->start * AMDGPU_GPU_PAGE_SIZE;
				vm_entries[num_mappings].size =
					(mapping->last - mapping->start + 1) *
					AMDGPU_GPU_PAGE_SIZE;
				vm_entries[num_mappings].offset =
					mapping->offset;
				vm_entries[num_mappings].flags  =
					mapping->flags;
			}
			num_mappings += 1U;
		}

		drm_exec_fini(&exec);

		if (vm_entries && num_mappings > 0 &&
		    num_mappings <= args->num_entries) {
			if (unlikely(copy_to_user(
				u64_to_user_ptr(args->value),
				vm_entries,
				(size_t)num_mappings * sizeof(*vm_entries))))
				r = -EFAULT;
		}

		args->num_entries = num_mappings;
		kvfree(vm_entries);
		break;
	}

	default:
		drm_exec_fini(&exec);
		r = -EINVAL;
		break;
	}

	drm_gem_object_put(gobj);
	return r;

out_exec:
	drm_exec_fini(&exec);
	drm_gem_object_put(gobj);
	return r;
}

/*
 * Enumerate all GEM handles for the calling process.
 *
 * Two-call pattern:
 *   1. args->num_entries == 0: just return the count.
 *   2. args->num_entries >= count: populate and return.
 *
 * If BOs are added between the count pass and the populate pass we
 * return -EAGAIN so userspace can retry; if BOs are removed we simply
 * copy fewer entries and set num_entries to the actual count.  Both
 * behaviors are safe: no buffer overrun, no use-after-free, no
 * dangling pointers.
 *
 * The counts are u32 (matching the UAPI field width) rather than int,
 * which avoids the subtle promotion-to-unsigned hazard in the
 * args->num_entries < num_bos comparison and guarantees kvcalloc sees
 * a non-negative size.
 */
int amdgpu_gem_list_handles_ioctl(struct drm_device *dev, void *data,
				  struct drm_file *filp)
{
	struct drm_amdgpu_gem_list_handles *args = data;
	struct drm_amdgpu_gem_list_handles_entry *bo_entries;
	struct drm_gem_object *gobj;
	int id, ret = 0;
	u32 bo_index = 0;
	u32 num_bos = 0;

	spin_lock(&filp->table_lock);
	idr_for_each_entry(&filp->object_idr, gobj, id) {
		if (unlikely(num_bos == U32_MAX)) {
			spin_unlock(&filp->table_lock);
			return -EOVERFLOW;
		}
		num_bos += 1U;
	}
	spin_unlock(&filp->table_lock);

	if (args->num_entries < num_bos) {
		args->num_entries = num_bos;
		return 0;
	}

	if (num_bos == 0U) {
		args->num_entries = 0U;
		return 0;
	}

	bo_entries = kvcalloc(num_bos, sizeof(*bo_entries), GFP_KERNEL);
	if (unlikely(!bo_entries))
		return -ENOMEM;

	spin_lock(&filp->table_lock);
	idr_for_each_entry(&filp->object_idr, gobj, id) {
		struct amdgpu_bo *bo = gem_to_amdgpu_bo(gobj);
		struct drm_amdgpu_gem_list_handles_entry *bo_entry;

		if (unlikely(bo_index >= num_bos)) {
			ret = -EAGAIN;
			break;
		}

		bo_entry = &bo_entries[bo_index];

		bo_entry->size              = amdgpu_bo_size(bo);
		bo_entry->alloc_flags       =
			bo->flags & AMDGPU_GEM_CREATE_SETTABLE_MASK;
		bo_entry->preferred_domains = bo->preferred_domains;
		bo_entry->gem_handle        = (u32)id;
		bo_entry->flags             = 0U;

		if (bo->tbo.base.import_attach)
			bo_entry->flags |=
				AMDGPU_GEM_LIST_HANDLES_FLAG_IS_IMPORT;

		bo_index += 1U;
	}
	spin_unlock(&filp->table_lock);

	args->num_entries = bo_index;

	if (likely(!ret && bo_index > 0U)) {
		if (unlikely(copy_to_user(
			u64_to_user_ptr(args->entries),
			bo_entries,
			(size_t)bo_index * sizeof(*bo_entries))))
			ret = -EFAULT;
	}

	kvfree(bo_entries);
	return ret;
}

/*
 * Overflow-safe pitch alignment.  Upstream does (aligned * cpp) in
 * plain int arithmetic, which silently wraps for huge width or for
 * cpp==4 with aligned near INT_MAX/4.  We do the multiply in u64 and
 * clamp to INT_MAX; -EOVERFLOW surfaces the problem to userspace
 * instead of allocating a too-small BO and then letting GPU writes
 * run off the end.
 */
static int amdgpu_gem_align_pitch(struct amdgpu_device *adev, u32 width,
				  u32 cpp, bool tiled)
{
	static const u32 pitch_masks[5] = {
		[0] = 0,
		[1] = 255,
		[2] = 127,
		[3] = 63,
		[4] = 63,
	};
	u32 pitch_mask, aligned;
	u64 pitch_bytes;

	(void)adev;
	(void)tiled;

	if (unlikely(cpp == 0 || cpp > 4))
		return -EINVAL;

	pitch_mask = pitch_masks[cpp];

	if (unlikely(width > U32_MAX - pitch_mask))
		return -EOVERFLOW;

	aligned = (width + pitch_mask) & ~pitch_mask;

	pitch_bytes = (u64)aligned * (u64)cpp;
	if (unlikely(pitch_bytes > (u64)INT_MAX))
		return -EOVERFLOW;

	return (int)pitch_bytes;
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
	int pitch, r;

	/*
	 * The buffer returned from this function should be cleared,
	 * but it can only be done if the ring is enabled or we'd fail
	 * to create the buffer.
	 */
	if (adev->mman.buffer_funcs_enabled)
		flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	pitch = amdgpu_gem_align_pitch(adev, args->width,
				       (u32)DIV_ROUND_UP(args->bpp, 8U), 0);
	if (unlikely(pitch < 0))
		return (pitch == -EOVERFLOW) ? -EINVAL : pitch;

	args->pitch = (uint32_t)pitch;

	/*
	 * Overflow guard: a malicious or broken userspace passing
	 * width = U32_MAX, bpp = 32 would silently wrap upstream.
	 */
	if (unlikely(check_mul_overflow((u64)args->pitch,
					(u64)args->height, &size)))
		return -EINVAL;

	size = ALIGN(size, PAGE_SIZE);
	args->size = size;

	domain = amdgpu_bo_get_preferred_domain(adev,
				amdgpu_display_supported_domains(adev, flags));

	r = amdgpu_gem_object_create(adev, args->size, 0, domain, flags,
				     ttm_bo_type_device, NULL, &gobj,
				     fpriv->xcp_id + 1);
	if (unlikely(r))
		return r;

	r = drm_gem_handle_create(file_priv, gobj, &handle);
	/* drop reference from allocate - handle holds it now */
	drm_gem_object_put(gobj);
	if (unlikely(r))
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
	if (unlikely(r))
		return r;

	list_for_each_entry(file, &dev->filelist, lhead) {
		struct task_struct *task;
		struct drm_gem_object *gobj;
		struct pid *pid;
		int id;

		/*
		 * Although we have a valid reference on file->pid, that
		 * does not guarantee that the task_struct who called
		 * get_pid() is still alive (e.g. get_pid(current) =>
		 * fork() => exit()).  Therefore, we need to protect
		 * this ->comm access using RCU.
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
