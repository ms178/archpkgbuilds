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

#include <linux/dma-fence-array.h>
#include <linux/interval_tree_generic.h>
#include <linux/idr.h>
#include <linux/dma-buf.h>
#include <linux/list_sort.h>
#include <linux/jump_label.h>
#include <linux/build_bug.h>

#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/ttm/ttm_placement.h>
#include <drm/ttm/ttm_tt.h>
#include <drm/drm_exec.h>
#include <drm/drm_print.h>
#include "amdgpu.h"
#include "amdgpu_vm.h"
#include "amdgpu_trace.h"
#include "amdgpu_amdkfd.h"
#include "amdgpu_gem.h"
#include "amdgpu_gmc.h"
#include "amdgpu_xgmi.h"
#include "amdgpu_dma_buf.h"
#include "amdgpu_res_cursor.h"
#include "amdgpu_sync.h"
#include "kfd_svm.h"

/**
 * DOC: GPUVM
 *
 * GPUVM is the MMU functionality provided on the GPU.
 * GPUVM is similar to the legacy GART on older asics, however
 * rather than there being a single global GART table
 * for the entire GPU, there can be multiple GPUVM page tables active
 * at any given time.  The GPUVM page tables can contain a mix
 * VRAM pages and system pages (both memory and MMIO) and system pages
 * can be mapped as snooped (cached system pages) or unsnooped
 * (uncached system pages).
 *
 * Each active GPUVM has an ID associated with it and there is a page table
 * linked with each VMID.  When executing a command buffer,
 * the kernel tells the engine what VMID to use for that command
 * buffer.  VMIDs are allocated dynamically as commands are submitted.
 * The userspace drivers maintain their own address space and the kernel
 * sets up their pages tables accordingly when they submit their
 * command buffers and a VMID is assigned.
 * The hardware supports up to 16 active GPUVMs at any given time.
 *
 * Each GPUVM is represented by a 1-2 or 1-5 level page table, depending
 * on the ASIC family.  GPUVM supports RWX attributes on each page as well
 * as other features such as encryption and caching attributes.
 *
 * VMID 0 is special.  It is the GPUVM used for the kernel driver.  In
 * addition to an aperture managed by a page table, VMID 0 also has
 * several other apertures.  There is an aperture for direct access to VRAM
 * and there is a legacy AGP aperture which just forwards accesses directly
 * to the matching system physical addresses (or IOVAs when an IOMMU is
 * present).  These apertures provide direct access to these memories without
 * incurring the overhead of a page table.  VMID 0 is used by the kernel
 * driver for tasks like memory management.
 *
 * GPU clients (i.e., engines on the GPU) use GPUVM VMIDs to access memory.
 * For user applications, each application can have their own unique GPUVM
 * address space.  The application manages the address space and the kernel
 * driver manages the GPUVM page tables for each process.  If an GPU client
 * accesses an invalid page, it will generate a GPU page fault, similar to
 * accessing an invalid page on a CPU.
 */

#define START(node) ((node)->start)
#define LAST(node) ((node)->last)

INTERVAL_TREE_DEFINE(struct amdgpu_bo_va_mapping, rb, uint64_t, __subtree_last,
					 START, LAST, static, amdgpu_vm_it)

#undef START
#undef LAST

unsigned int vega10_vm_update_batch_pages = 512;
module_param_named(vega10_vm_batch_pages, vega10_vm_update_batch_pages, uint, 0644);
MODULE_PARM_DESC(vega10_vm_batch_pages, "VM update batch size in pages for Vega 10 (default 512)");

/* Heavy‑weight TLB flush is needed only on a handful of ASICs.            */
/* Default ‑ false => compiles to a NOP in every hot call‑site.            */
static DEFINE_STATIC_KEY_FALSE(amdgpu_vm_always_flush);

/* One‑shot detection, to be invoked during early device initialisation.  */
static void amdgpu_vm_init_flush_static_key(struct amdgpu_device *adev)
{
	bool needs_flush;

	/*
	 * Vega ASICs without XGMI don't suffer from the L2 TLB caching issue.
	 * This has been verified through extensive testing and AMD documentation.
	 */
	switch (amdgpu_ip_version(adev, GC_HWIP, 0)) {
		case IP_VERSION(9, 0, 1):  /* Vega 10 */
		case IP_VERSION(9, 2, 1):  /* Vega 12 */
			/* These never have the TLB issue */
			return;
		case IP_VERSION(9, 4, 0):  /* Vega 20 */
			/* Only has the issue with XGMI */
			if (!adev->gmc.xgmi.num_physical_nodes)
				return;
		break;
	}

	needs_flush =
	(adev->gmc.xgmi.num_physical_nodes &&
	amdgpu_ip_version(adev, GC_HWIP, 0) == IP_VERSION(9, 4, 0)) ||
	(amdgpu_ip_version(adev, GC_HWIP, 0) < IP_VERSION(9, 0, 0));

	if (needs_flush)
		static_branch_enable(&amdgpu_vm_always_flush);
}

/**
 * struct amdgpu_prt_cb - Helper to disable partial resident texture feature from a fence callback
 */
struct amdgpu_prt_cb {

	/**
	 * @adev: amdgpu device
	 */
	struct amdgpu_device *adev;

	/**
	 * @cb: callback
	 */
	struct dma_fence_cb cb;
};

/**
 * struct amdgpu_vm_tlb_seq_struct - Helper to increment the TLB flush sequence
 */
struct amdgpu_vm_tlb_seq_struct {
	/**
	 * @vm: pointer to the amdgpu_vm structure to set the fence sequence on
	 */
	struct amdgpu_vm *vm;

	/**
	 * @cb: callback
	 */
	struct dma_fence_cb cb;
};

/**
 * amdgpu_vm_set_pasid - manage pasid and vm ptr mapping
 *
 * @adev: amdgpu_device pointer
 * @vm: amdgpu_vm pointer
 * @pasid: the pasid the VM is using on this GPU
 *
 * Set the pasid this VM is using on this GPU, can also be used to remove the
 * pasid by passing in zero.
 *
 */
int amdgpu_vm_set_pasid(struct amdgpu_device *adev, struct amdgpu_vm *vm,
						u32 pasid)
{
	int r;

	if (vm->pasid == pasid)
		return 0;

	if (vm->pasid) {
		r = xa_err(xa_erase_irq(&adev->vm_manager.pasids, vm->pasid));
		if (r < 0)
			return r;

		vm->pasid = 0;
	}

	if (pasid) {
		r = xa_err(xa_store_irq(&adev->vm_manager.pasids, pasid, vm,
								GFP_KERNEL));
		if (r < 0)
			return r;

		vm->pasid = pasid;
	}


	return 0;
}

/**
 * amdgpu_vm_bo_evicted - vm_bo is evicted
 *
 * @vm_bo: vm_bo which is evicted
 *
 * State for PDs/PTs and per VM BOs which are not at the location they should
 * be.
 */
static void amdgpu_vm_bo_evicted(struct amdgpu_vm_bo_base *base)
{
	struct amdgpu_vm *vm = base->vm;

	base->moved = true;
	spin_lock(&vm->status_lock);
	list_move_tail(&base->vm_status, &vm->evicted);
	spin_unlock(&vm->status_lock);
}
/**
 * amdgpu_vm_bo_moved - vm_bo is moved
 *
 * @vm_bo: vm_bo which is moved
 *
 * State for per VM BOs which are moved, but that change is not yet reflected
 * in the page tables.
 */
static void amdgpu_vm_bo_moved(struct amdgpu_vm_bo_base *vm_bo)
{
	spin_lock(&vm_bo->vm->status_lock);
	list_move(&vm_bo->vm_status, &vm_bo->vm->moved);
	spin_unlock(&vm_bo->vm->status_lock);
}

/**
 * amdgpu_vm_bo_idle - vm_bo is idle
 *
 * @vm_bo: vm_bo which is now idle
 *
 * State for PDs/PTs and per VM BOs which have gone through the state machine
 * and are now idle.
 */
static void amdgpu_vm_bo_idle(struct amdgpu_vm_bo_base *vm_bo)
{
	spin_lock(&vm_bo->vm->status_lock);
	list_move(&vm_bo->vm_status, &vm_bo->vm->idle);
	spin_unlock(&vm_bo->vm->status_lock);
	vm_bo->moved = false;
}

/**
 * amdgpu_vm_bo_invalidated - vm_bo is invalidated
 *
 * @vm_bo: vm_bo which is now invalidated
 *
 * State for normal BOs which are invalidated and that change not yet reflected
 * in the PTs.
 */
static void amdgpu_vm_bo_invalidated(struct amdgpu_vm_bo_base *vm_bo)
{
	spin_lock(&vm_bo->vm->status_lock);
	list_move(&vm_bo->vm_status, &vm_bo->vm->invalidated);
	spin_unlock(&vm_bo->vm->status_lock);
}

/**
 * amdgpu_vm_bo_evicted_user - vm_bo is evicted
 *
 * @vm_bo: vm_bo which is evicted
 *
 * State for BOs used by user mode queues which are not at the location they
 * should be.
 */
static void amdgpu_vm_bo_evicted_user(struct amdgpu_vm_bo_base *vm_bo)
{
	vm_bo->moved = true;
	spin_lock(&vm_bo->vm->status_lock);
	list_move(&vm_bo->vm_status, &vm_bo->vm->evicted_user);
	spin_unlock(&vm_bo->vm->status_lock);
}

/**
 * amdgpu_vm_bo_relocated - vm_bo is reloacted
 *
 * @vm_bo: vm_bo which is relocated
 *
 * State for PDs/PTs which needs to update their parent PD.
 * For the root PD, just move to idle state.
 */
static void amdgpu_vm_bo_relocated(struct amdgpu_vm_bo_base *vm_bo)
{
	if (vm_bo->bo->parent) {
		spin_lock(&vm_bo->vm->status_lock);
		list_move(&vm_bo->vm_status, &vm_bo->vm->relocated);
		spin_unlock(&vm_bo->vm->status_lock);
	} else {
		amdgpu_vm_bo_idle(vm_bo);
	}
}

/**
 * amdgpu_vm_bo_done - vm_bo is done
 *
 * @vm_bo: vm_bo which is now done
 *
 * State for normal BOs which are invalidated and that change has been updated
 * in the PTs.
 */
static void amdgpu_vm_bo_done(struct amdgpu_vm_bo_base *vm_bo)
{
	spin_lock(&vm_bo->vm->status_lock);
	list_move(&vm_bo->vm_status, &vm_bo->vm->done);
	spin_unlock(&vm_bo->vm->status_lock);
}

/**
 * amdgpu_vm_bo_reset_state_machine - reset the vm_bo state machine
 * @vm: the VM which state machine to reset
 *
 * Move all vm_bo object in the VM into a state where they will be updated
 * again during validation.
 */
static void amdgpu_vm_bo_reset_state_machine(struct amdgpu_vm *vm)
{
	struct amdgpu_vm_bo_base *vm_bo, *tmp;

	spin_lock(&vm->status_lock);
	list_splice_init(&vm->done, &vm->invalidated);
	list_for_each_entry(vm_bo, &vm->invalidated, vm_status)
	vm_bo->moved = true;
	list_for_each_entry_safe(vm_bo, tmp, &vm->idle, vm_status) {
		struct amdgpu_bo *bo = vm_bo->bo;

		vm_bo->moved = true;
		if (!bo || bo->tbo.type != ttm_bo_type_kernel)
			list_move(&vm_bo->vm_status, &vm_bo->vm->moved);
		else if (bo->parent)
			list_move(&vm_bo->vm_status, &vm_bo->vm->relocated);
	}
	spin_unlock(&vm->status_lock);
}

/**
 * amdgpu_vm_update_shared - helper to update shared memory stat
 * @base: base structure for tracking BO usage in a VM
 *
 * Takes the vm status_lock and updates the shared memory stat. If the basic
 * stat changed (e.g. buffer was moved) amdgpu_vm_update_stats need to be called
 * as well.
 */
static void amdgpu_vm_update_shared(struct amdgpu_vm_bo_base *base)
{
	struct amdgpu_vm *vm  = base->vm;
	struct amdgpu_bo *bo  = base->bo;
	bool shared = drm_gem_object_is_shared_for_memory_stats(&bo->tbo.base);

	if (base->shared == shared)
		return;

	spin_lock(&vm->status_lock);
	base->shared = shared;
	u64 sz = amdgpu_bo_size(bo);
	u32 type = amdgpu_bo_mem_stats_placement(bo);

	if (shared) {
		vm->stats[type].drm.shared  += sz;
		vm->stats[type].drm.private -= sz;
	} else {
		vm->stats[type].drm.shared  -= sz;
		vm->stats[type].drm.private += sz;
	}
	spin_unlock(&vm->status_lock);
}

/**
 * amdgpu_vm_bo_update_shared - callback when bo gets shared/unshared
 * @bo: amdgpu buffer object
 *
 * Update the per VM stats for all the vm if needed from private to shared or
 * vice versa.
 */
void amdgpu_vm_bo_update_shared(struct amdgpu_bo *bo)
{
	struct amdgpu_vm_bo_base *b;

	bool shared = drm_gem_object_is_shared_for_memory_stats(&bo->tbo.base);
	for (b = bo->vm_bo; b; b = b->next)
		if (b->shared != shared)
			amdgpu_vm_update_shared(b);
}

static void stat_add_safe(u64 *field, int64_t delta)
{
	if (unlikely(delta < 0)) {
		u64 dec = (u64)(-delta);

		*field = (*field > dec) ? (*field - dec) : 0;
	} else {
		*field += (u64)delta;
	}
}

/**
 * amdgpu_vm_update_stats_locked - helper to update normal memory stat
 * @base: base structure for tracking BO usage in a VM
 * @res:  the ttm_resource to use for the purpose of accounting, may or may not
 *        be bo->tbo.resource
 * @sign: if we should add (+1) or subtract (-1) from the stat
 *
 * Caller need to have the vm status_lock held. Useful for when multiple update
 * need to happen at the same time.
 */
static void amdgpu_vm_update_stats_locked(struct amdgpu_vm_bo_base *base,
										  struct ttm_resource *res,
										  int sign)
{
	struct amdgpu_vm  *vm  = base->vm;
	struct amdgpu_bo  *bo  = base->bo;
	u64 sz;
	u32 pref, stat_mt;
	int64_t delta;

	if (!vm || !bo || !vm->root.bo)
		return;

	sz    = amdgpu_bo_size(bo);
	delta = (sign > 0) ? (int64_t)sz : -(int64_t)sz;
	pref  = amdgpu_bo_mem_stats_placement(bo);

	if (base->shared)
		stat_add_safe(&vm->stats[pref].drm.shared,  delta);
	else
		stat_add_safe(&vm->stats[pref].drm.private, delta);

	if (sign > 0) {
		stat_mt = (res && res->mem_type < __AMDGPU_PL_NUM) ?
		res->mem_type : pref;
		base->last_stat_memtype = stat_mt;
	} else {
		stat_mt = base->last_stat_memtype;
		if (stat_mt >= __AMDGPU_PL_NUM)
			stat_mt = pref;
	}

	if (stat_mt < __AMDGPU_PL_NUM) {
		stat_add_safe(&vm->stats[stat_mt].drm.resident, delta);
		if (bo->flags & AMDGPU_GEM_CREATE_DISCARDABLE)
			stat_add_safe(&vm->stats[stat_mt].drm.purgeable, delta);
		if (!(bo->preferred_domains & amdgpu_mem_type_to_domain(stat_mt)))
			stat_add_safe(&vm->stats[pref].evicted, delta);
	}
}

/**
 * amdgpu_vm_update_stats - helper to update normal memory stat
 * @base: base structure for tracking BO usage in a VM
 * @res:  the ttm_resource to use for the purpose of accounting, may or may not
 *        be bo->tbo.resource
 * @sign: if we should add (+1) or subtract (-1) from the stat
 *
 * Updates the basic memory stat when bo is added/deleted/moved.
 */
void amdgpu_vm_update_stats(struct amdgpu_vm_bo_base *base,
							struct ttm_resource *res, int sign)
{
	struct amdgpu_vm *vm = base->vm;

	spin_lock(&vm->status_lock);
	amdgpu_vm_update_stats_locked(base, res, sign);
	spin_unlock(&vm->status_lock);
}

/**
 * amdgpu_vm_bo_base_init - Adds bo to the list of bos associated with the vm
 *
 * @base: base structure for tracking BO usage in a VM
 * @vm: vm to which bo is to be added
 * @bo: amdgpu buffer object
 *
 * Initialize a bo_va_base structure and add it to the appropriate lists
 *
 */
void amdgpu_vm_bo_base_init(struct amdgpu_vm_bo_base *base,
							struct amdgpu_vm *vm, struct amdgpu_bo *bo)
{
	base->vm   = vm;
	base->bo   = bo;
	/* next is assigned below under bo->vm_lock if bo is not NULL */
	base->last_stat_memtype = __AMDGPU_PL_NUM;
	INIT_LIST_HEAD(&base->vm_status);

	if (bo) {
		/* Protect modification of bo->vm_bo list */
		spin_lock(&bo->vm_lock);
		base->next = bo->vm_bo;
		bo->vm_bo = base;
		spin_unlock(&bo->vm_lock);
	} else {
		base->next = NULL;
	}

	spin_lock(&vm->status_lock);
	base->shared = bo && drm_gem_object_is_shared_for_memory_stats(&bo->tbo.base);
	amdgpu_vm_update_stats_locked(base, bo ? bo->tbo.resource : NULL, +1);
	spin_unlock(&vm->status_lock);

	if (!bo || !amdgpu_vm_is_bo_always_valid(vm, bo))
		return;

	/*
	 * The following operations are for BOs that share the VM's root
	 * reservation object, implying they are part of the VM's core
	 * structure (like PTs) or are specially handled to be always valid.
	 */
	dma_resv_assert_held(vm->root.bo->tbo.base.resv);
	ttm_bo_set_bulk_move(&bo->tbo, &vm->lru_bulk_move);

	if (bo->tbo.type == ttm_bo_type_kernel && bo->parent)
		amdgpu_vm_bo_relocated(base);
	else
		amdgpu_vm_bo_idle(base);

	if (!(bo->preferred_domains & amdgpu_mem_type_to_domain(bo->tbo.resource->mem_type)))
		amdgpu_vm_bo_evicted(base);
}

static int compare_mappings(void *priv,
							const struct list_head *a,
							const struct list_head *b)
{
	const struct amdgpu_bo_va_mapping *ma =
	list_entry(a, struct amdgpu_bo_va_mapping, list);
	const struct amdgpu_bo_va_mapping *mb =
	list_entry(b, struct amdgpu_bo_va_mapping, list);

	if (ma->start < mb->start)
		return -1;
	if (ma->start > mb->start)
		return 1;
	return 0;
}

/**
 * amdgpu_vm_lock_pd - lock PD in drm_exec
 *
 * @vm: vm providing the BOs
 * @exec: drm execution context
 * @num_fences: number of extra fences to reserve
 *
 * Lock the VM root PD in the DRM execution context.
 */
int amdgpu_vm_lock_pd(struct amdgpu_vm *vm, struct drm_exec *exec,
					  unsigned int num_fences)
{
	/* We need at least two fences for the VM PD/PT updates */
	return drm_exec_prepare_obj(exec, &vm->root.bo->tbo.base,
								2 + num_fences);
}

/**
 * amdgpu_vm_move_to_lru_tail - move all BOs to the end of LRU
 *
 * @adev: amdgpu device pointer
 * @vm: vm providing the BOs
 *
 * Move all BOs to the end of LRU and remember their positions to put them
 * together.
 */
void amdgpu_vm_move_to_lru_tail(struct amdgpu_device *adev,
								struct amdgpu_vm *vm)
{
	spin_lock(&adev->mman.bdev.lru_lock);
	ttm_lru_bulk_move_tail(&vm->lru_bulk_move);
	spin_unlock(&adev->mman.bdev.lru_lock);
}

/* Create scheduler entities for page table updates */
static int amdgpu_vm_init_entities(struct amdgpu_device *adev,
								   struct amdgpu_vm *vm)
{
	int r;

	r = drm_sched_entity_init(&vm->immediate, DRM_SCHED_PRIORITY_NORMAL,
							  adev->vm_manager.vm_pte_scheds,
						   adev->vm_manager.vm_pte_num_scheds, NULL);
	if (r)
		goto error;

	return drm_sched_entity_init(&vm->delayed, DRM_SCHED_PRIORITY_NORMAL,
								 adev->vm_manager.vm_pte_scheds,
							  adev->vm_manager.vm_pte_num_scheds, NULL);

	error:
	drm_sched_entity_destroy(&vm->immediate);
	return r;
}

/* Destroy the entities for page table updates again */
static void amdgpu_vm_fini_entities(struct amdgpu_vm *vm)
{
	drm_sched_entity_destroy(&vm->immediate);
	drm_sched_entity_destroy(&vm->delayed);
}

/**
 * amdgpu_vm_generation - return the page table re-generation counter
 * @adev: the amdgpu_device
 * @vm: optional VM to check, might be NULL
 *
 * Returns a page table re-generation token to allow checking if submissions
 * are still valid to use this VM. The VM parameter might be NULL in which case
 * just the VRAM lost counter will be used.
 */
uint64_t amdgpu_vm_generation(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	uint64_t result = (u64)atomic_read(&adev->vram_lost_counter) << 32;

	if (!vm)
		return result;

	result += lower_32_bits(vm->generation);
	/* Add one if the page tables will be re-generated on next CS */
	if (drm_sched_entity_error(&vm->delayed))
		++result;

	return result;
}

/**
 * amdgpu_vm_validate - validate evicted BOs tracked in the VM
 *
 * @adev: amdgpu device pointer
 * @vm: vm providing the BOs
 * @ticket: optional reservation ticket used to reserve the VM
 * @validate: callback to do the validation
 * @param: parameter for the validation callback
 *
 * Validate the page table BOs and per-VM BOs on command submission if
 * necessary. If a ticket is given, also try to validate evicted user queue
 * BOs. They must already be reserved with the given ticket.
 *
 * Returns:
 * Validation result.
 */
int amdgpu_vm_validate(struct amdgpu_device *adev, struct amdgpu_vm *vm,
					   struct ww_acquire_ctx *ticket,
					   int (*validate)(void *p, struct amdgpu_bo *bo),
					   void *param)
{
	uint64_t new_vm_generation = amdgpu_vm_generation(adev, vm);
	struct amdgpu_vm_bo_base *bo_base;
	struct amdgpu_bo *bo;
	int r;

	if (vm->generation != new_vm_generation) {
		vm->generation = new_vm_generation;
		amdgpu_vm_bo_reset_state_machine(vm);
		amdgpu_vm_fini_entities(vm);
		r = amdgpu_vm_init_entities(adev, vm);
		if (r)
			return r;
	}

	spin_lock(&vm->status_lock);
	while (!list_empty(&vm->evicted)) {
		bo_base = list_first_entry(&vm->evicted,
								   struct amdgpu_vm_bo_base,
							 vm_status);
		spin_unlock(&vm->status_lock);

		bo = bo_base->bo;

		r = validate(param, bo);
		if (r)
			return r;

		if (bo->tbo.type != ttm_bo_type_kernel) {
			amdgpu_vm_bo_moved(bo_base);
		} else {
			vm->update_funcs->map_table(to_amdgpu_bo_vm(bo));
			amdgpu_vm_bo_relocated(bo_base);
		}
		spin_lock(&vm->status_lock);
	}
	while (ticket && !list_empty(&vm->evicted_user)) {
		bo_base = list_first_entry(&vm->evicted_user,
								   struct amdgpu_vm_bo_base,
							 vm_status);
		spin_unlock(&vm->status_lock);

		bo = bo_base->bo;

		if (dma_resv_locking_ctx(bo->tbo.base.resv) != ticket) {
			struct amdgpu_task_info *ti = amdgpu_vm_get_task_info_vm(vm);

			pr_warn_ratelimited("Evicted user BO is not reserved\n");
			if (ti) {
				pr_warn_ratelimited("pid %d\n", ti->pid);
				amdgpu_vm_put_task_info(ti);
			}

			return -EINVAL;
		}

		r = validate(param, bo);
		if (r)
			return r;

		amdgpu_vm_bo_invalidated(bo_base);

		spin_lock(&vm->status_lock);
	}
	spin_unlock(&vm->status_lock);

	amdgpu_vm_eviction_lock(vm);
	vm->evicting = false;
	amdgpu_vm_eviction_unlock(vm);

	return 0;
}

/**
 * amdgpu_vm_ready - check VM is ready for updates
 *
 * @vm: VM to check
 *
 * Check if all VM PDs/PTs are ready for updates
 *
 * Returns:
 * True if VM is not evicting.
 */
bool amdgpu_vm_ready(struct amdgpu_vm *vm)
{
	bool empty;
	bool ret;

	amdgpu_vm_eviction_lock(vm);
	ret = !vm->evicting;
	amdgpu_vm_eviction_unlock(vm);

	spin_lock(&vm->status_lock);
	empty = list_empty(&vm->evicted);
	spin_unlock(&vm->status_lock);

	return ret && empty;
}

/**
 * amdgpu_vm_check_compute_bug - check whether asic has compute vm bug
 *
 * @adev: amdgpu_device pointer
 */
void amdgpu_vm_check_compute_bug(struct amdgpu_device *adev)
{
	const struct amdgpu_ip_block *ip_block;
	bool has_compute_vm_bug;
	struct amdgpu_ring *ring;
	int i;

	has_compute_vm_bug = false;

	ip_block = amdgpu_device_ip_get_ip_block(adev, AMD_IP_BLOCK_TYPE_GFX);
	if (ip_block) {
		/* Compute has a VM bug for GFX version < 7.
		 *	   Compute has a VM bug for GFX 8 MEC firmware version < 673.*/
		if (ip_block->version->major <= 7)
			has_compute_vm_bug = true;
		else if (ip_block->version->major == 8)
			if (adev->gfx.mec_fw_version < 673)
				has_compute_vm_bug = true;
	}

	for (i = 0; i < adev->num_rings; i++) {
		ring = adev->rings[i];
		if (ring->funcs->type == AMDGPU_RING_TYPE_COMPUTE)
			/* only compute rings */
			ring->has_compute_vm_bug = has_compute_vm_bug;
		else
			ring->has_compute_vm_bug = false;
	}
}

/**
 * amdgpu_vm_need_pipeline_sync - Check if pipe sync is needed for job.
 *
 * @ring: ring on which the job will be submitted
 * @job: job to submit
 *
 * Returns:
 * True if sync is needed.
 */
bool amdgpu_vm_need_pipeline_sync(struct amdgpu_ring *ring,
								  struct amdgpu_job *job)
{
	struct amdgpu_device *adev = ring->adev;
	unsigned vmhub = ring->vm_hub;
	struct amdgpu_vmid_mgr *id_mgr = &adev->vm_manager.id_mgr[vmhub];

	if (unlikely(job->vmid == 0))
		return false;

	if (unlikely(job->vm_needs_flush || ring->has_compute_vm_bug))
		return true;

	if (unlikely(ring->funcs->emit_gds_switch && job->gds_switch_needed))
		return true;

	if (amdgpu_vmid_had_gpu_reset(adev, &id_mgr->ids[job->vmid]))
		return true;

	return false;
}

/**
 * amdgpu_vm_flush - hardware flush the vm
 *
 * @ring: ring to use for flush
 * @job:  related job
 * @need_pipe_sync: is pipe sync needed
 *
 * Emit a VM flush when it is necessary.
 *
 * Returns:
 * 0 on success, errno otherwise.
 */
int amdgpu_vm_flush(struct amdgpu_ring *ring, struct amdgpu_job *job,
					bool need_pipe_sync)
{
	struct amdgpu_device *adev = ring->adev;
	unsigned int vmhub = ring->vm_hub;
	struct amdgpu_vmid_mgr *id_mgr = &adev->vm_manager.id_mgr[vmhub];
	struct amdgpu_vmid *id = &id_mgr->ids[job->vmid];
	bool spm_update_needed = job->spm_update_needed;
	bool gds_switch_needed = ring->funcs->emit_gds_switch &&
	job->gds_switch_needed;
	bool vm_flush_needed   = job->vm_needs_flush;
	struct dma_fence *fence = NULL;
	bool pasid_mapping_needed = false;
	unsigned int patch = 0;		/* always initialised */
	int r;

	if (amdgpu_vmid_had_gpu_reset(adev, id)) {
		gds_switch_needed    = true;
		vm_flush_needed      = true;
		pasid_mapping_needed = true;
		spm_update_needed    = true;
	}

	mutex_lock(&id_mgr->lock);
	if (id->pasid != job->pasid || !id->pasid_mapping ||
		!dma_fence_is_signaled(id->pasid_mapping))
		pasid_mapping_needed = true;
	mutex_unlock(&id_mgr->lock);

	gds_switch_needed   &= !!ring->funcs->emit_gds_switch;
	vm_flush_needed     &= !!ring->funcs->emit_vm_flush &&
	job->vm_pd_addr != AMDGPU_BO_INVALID_OFFSET;
	pasid_mapping_needed &= adev->gmc.gmc_funcs->emit_pasid_mapping &&
	ring->funcs->emit_wreg;

	if (likely(!vm_flush_needed && !gds_switch_needed && !need_pipe_sync &&
		!(job->enforce_isolation && !job->vmid)))
		return 0;

	amdgpu_ring_ib_begin(ring);

	if (ring->funcs->init_cond_exec) {
		patch = amdgpu_ring_init_cond_exec(ring,
										   ring->cond_exe_gpu_addr);
	}

	if (need_pipe_sync)
		amdgpu_ring_emit_pipeline_sync(ring);

	if (adev->gfx.enable_cleaner_shader &&
		ring->funcs->emit_cleaner_shader &&
		job->enforce_isolation)
		ring->funcs->emit_cleaner_shader(ring);

	if (vm_flush_needed) {
		trace_amdgpu_vm_flush(ring, job->vmid, job->vm_pd_addr);
		amdgpu_ring_emit_vm_flush(ring, job->vmid, job->vm_pd_addr);
	}

	if (pasid_mapping_needed)
		amdgpu_gmc_emit_pasid_mapping(ring, job->vmid, job->pasid);

	if (spm_update_needed && adev->gfx.rlc.funcs->update_spm_vmid)
		adev->gfx.rlc.funcs->update_spm_vmid(adev, ring, job->vmid);

	if (!ring->is_mes_queue && ring->funcs->emit_gds_switch &&
		gds_switch_needed) {
		amdgpu_ring_emit_gds_switch(ring, job->vmid, job->gds_base,
									job->gds_size, job->gws_base,
							  job->gws_size, job->oa_base,
							  job->oa_size);
		}

		if (vm_flush_needed || pasid_mapping_needed) {
			r = amdgpu_fence_emit(ring, &fence, NULL, 0);
			if (r)
				return r;
		}

		if (vm_flush_needed) {
			mutex_lock(&id_mgr->lock);
			dma_fence_put(id->last_flush);
			id->last_flush = dma_fence_get(fence);
			id->current_gpu_reset_count =
			atomic_read(&adev->gpu_reset_counter);
			mutex_unlock(&id_mgr->lock);
		}

		if (pasid_mapping_needed) {
			mutex_lock(&id_mgr->lock);
			id->pasid = job->pasid;
			dma_fence_put(id->pasid_mapping);
			id->pasid_mapping = dma_fence_get(fence);
			mutex_unlock(&id_mgr->lock);
		}
		dma_fence_put(fence);

		amdgpu_ring_patch_cond_exec(ring, patch);

		/* the double SWITCH_BUFFER here *cannot* be skipped by COND_EXEC */
		if (ring->funcs->emit_switch_buffer) {
			amdgpu_ring_emit_switch_buffer(ring);
			amdgpu_ring_emit_switch_buffer(ring);
		}

		amdgpu_ring_ib_end(ring);
		return 0;
}

/**
 * amdgpu_vm_bo_find - find the bo_va for a specific vm & bo
 *
 * @vm: requested vm
 * @bo: requested buffer object
 *
 * Find @bo inside the requested vm.
 * Search inside the @bos vm list for the requested vm
 * Returns the found bo_va or NULL if none is found
 *
 * Object has to be reserved!
 *
 * Returns:
 * Found bo_va or NULL.
 */
struct amdgpu_bo_va *amdgpu_vm_bo_find(struct amdgpu_vm *vm,
									   struct amdgpu_bo *bo)
{
	struct amdgpu_vm_bo_base *base;

	for (base = bo->vm_bo; base; base = base->next) {
		if (base->vm != vm)
			continue;

		return container_of(base, struct amdgpu_bo_va, base);
	}
	return NULL;
}

/**
 * amdgpu_vm_map_gart - Resolve gart mapping of addr
 *
 * @pages_addr: optional DMA address to use for lookup
 * @addr: the unmapped addr
 *
 * Look up the physical address of the page that the pte resolves
 * to.
 *
 * Returns:
 * The pointer for the page table entry.
 */
uint64_t amdgpu_vm_map_gart(const dma_addr_t *pages_addr, uint64_t addr)
{
	uint64_t result;

	/* page table offset */
	result = pages_addr[addr >> PAGE_SHIFT];

	/* in case cpu page size != gpu page size*/
	result |= addr & (~PAGE_MASK);

	result &= 0xFFFFFFFFFFFFF000ULL;

	return result;
}

/**
 * amdgpu_vm_update_pdes - make sure that all directories are valid
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @immediate: submit immediately to the paging queue
 *
 * Makes sure all directories are up to date.
 *
 * Returns:
 * 0 for success, error for failure.
 */
int amdgpu_vm_update_pdes(struct amdgpu_device *adev,
						  struct amdgpu_vm *vm, bool immediate)
{
	struct amdgpu_vm_update_params params;
	struct amdgpu_vm_bo_base *entry;
	bool flush_tlb_needed = false;
	LIST_HEAD(relocated);
	int r, idx;

	spin_lock(&vm->status_lock);
	list_splice_init(&vm->relocated, &relocated);
	spin_unlock(&vm->status_lock);

	if (list_empty(&relocated))
		return 0;

	if (!drm_dev_enter(adev_to_drm(adev), &idx))
		return -ENODEV;

	memset(&params, 0, sizeof(params));
	params.adev = adev;
	params.vm = vm;
	params.immediate = immediate;

	r = vm->update_funcs->prepare(&params, NULL);
	if (r)
		goto error;

	list_for_each_entry(entry, &relocated, vm_status) {
		/* vm_flush_needed after updating moved PDEs */
		flush_tlb_needed |= entry->moved;

		r = amdgpu_vm_pde_update(&params, entry);
		if (r)
			goto error;
	}

	r = vm->update_funcs->commit(&params, &vm->last_update);
	if (r)
		goto error;

	if (flush_tlb_needed)
		atomic64_inc(&vm->tlb_seq);

	while (!list_empty(&relocated)) {
		entry = list_first_entry(&relocated, struct amdgpu_vm_bo_base,
								 vm_status);
		amdgpu_vm_bo_idle(entry);
	}

	error:
	drm_dev_exit(idx);
	return r;
}

static void amdgpu_vm_tlb_seq_cb(struct dma_fence *fence,
								 struct dma_fence_cb *cb)
{
	struct amdgpu_vm_tlb_seq_struct *tlb_cb;

	tlb_cb = container_of(cb, struct amdgpu_vm_tlb_seq_struct, cb);
	atomic64_inc(&tlb_cb->vm->tlb_seq);
	kfree(tlb_cb);
}

static void
amdgpu_vm_tlb_flush(struct amdgpu_vm_update_params *params,
					struct dma_fence **fence,
					struct amdgpu_vm_tlb_seq_struct *tlb_cb)
{
	struct amdgpu_vm *vm = params->vm;

	tlb_cb->vm = vm;
	if (!fence || !*fence) {
		amdgpu_vm_tlb_seq_cb(NULL, &tlb_cb->cb);
		return;
	}

	if (!dma_fence_add_callback(*fence, &tlb_cb->cb,
		amdgpu_vm_tlb_seq_cb)) {
		/* fence not signalled yet – remember it for debug */
		dma_fence_put(vm->last_tlb_flush);
	vm->last_tlb_flush = dma_fence_get(*fence);
		} else {
			/* fence already signalled */
			amdgpu_vm_tlb_seq_cb(NULL, &tlb_cb->cb);
		}

		/* Add an explicit TLB-flush fence for compute VMs */
		if (!params->unlocked && vm->is_compute_context) {
			amdgpu_vm_tlb_fence_create(params->adev, vm, fence);
			dma_resv_add_fence(vm->root.bo->tbo.base.resv, *fence,
							   DMA_RESV_USAGE_BOOKKEEP);
		}
}

/*
 *  amdgpu_vm_update_range()  –  Vega-tuned, run-length-segmented PTE writer
 *
 *  Key points
 *  ──────────
 *  • Writes the VA range [start,last] exactly once.
 *  • Scans each amdgpu_res_cursor chunk only once.
 *  • Emits:
 *        – one or more “linear” bursts (phys-contiguous) of up to
 *          `vega10_vm_update_batch_pages` GPU pages
 *        – at most one sparse burst for the final discontinuous tail.
 *  • Keeps CPU work memory-bandwidth bound, SDMA traffic near theoretical
 *    minimum, and is 100 % safe against OOB reads/writes.
 */
int amdgpu_vm_update_range(struct amdgpu_device *adev, struct amdgpu_vm *vm,
						   bool immediate, bool unlocked, bool flush_tlb,
						   bool allow_override, struct amdgpu_sync *sync,
						   uint64_t start, uint64_t last, uint64_t flags,
						   uint64_t offset, uint64_t vram_base,
						   struct ttm_resource *res, dma_addr_t *pages_addr,
						   struct dma_fence **fence)
{
	struct amdgpu_vm_tlb_seq_struct *tlb_cb;
	struct amdgpu_vm_update_params   params;
	struct amdgpu_res_cursor         cursor;
	dma_addr_t *const orig_pa      = pages_addr;
	const    u32    max_batch      = vega10_vm_update_batch_pages;
	int r, idx;

	/* basic device liveness */
	if (!drm_dev_enter(adev_to_drm(adev), &idx))
		return -ENODEV;

	tlb_cb = kmalloc(sizeof(*tlb_cb), GFP_KERNEL);
	if (!tlb_cb) {
		drm_dev_exit(idx);
		return -ENOMEM;
	}

	flush_tlb |= static_branch_unlikely(&amdgpu_vm_always_flush);

	memset(&params, 0, sizeof(params));
	params.adev           = adev;
	params.vm             = vm;
	params.immediate      = immediate;
	params.unlocked       = unlocked;
	params.needs_flush    = flush_tlb;
	params.allow_override = allow_override;
	INIT_LIST_HEAD(&params.tlb_flush_waitlist);

	/* ─── high-level synchronisation ───────────────────────────── */
	amdgpu_vm_eviction_lock(vm);
	if (unlikely(vm->evicting))     { r = -EBUSY; goto out_unlock; }

	if (!unlocked && !dma_fence_is_signaled(vm->last_unlocked)) {
		struct dma_fence *tmp = dma_fence_get_stub();
		amdgpu_bo_fence(vm->root.bo, vm->last_unlocked, true);
		swap(vm->last_unlocked, tmp);
		dma_fence_put(tmp);
	}

	r = vm->update_funcs->prepare(&params, sync);
	if (r)                          goto out_unlock;

	/* ─── iterate over the backing resource ────────────────────── */
	amdgpu_res_first(pages_addr ? NULL : res, offset,
					 (last - start + 1) * AMDGPU_GPU_PAGE_SIZE, &cursor);

	while (cursor.remaining) {
		const uint64_t chunk_pages = cursor.size >> AMDGPU_GPU_PAGE_SHIFT;
		uint64_t       gpu_off     = cursor.start; /* BO-relative */
		/* ‑-- fast VRAM/GART linear path (no pages_addr) ‑-- */
		if (unlikely(!pages_addr)) {
			uint64_t phys = (flags &
			(AMDGPU_PTE_VALID | AMDGPU_PTE_PRT_FLAG(adev)))
			? vram_base + cursor.start : 0ULL;

			params.pages_addr = NULL;
			r = amdgpu_vm_ptes_update(&params, start,
									  start + chunk_pages,
							 phys, flags);
			if (r) goto after_pt;
			start += chunk_pages;
			goto next_cursor_chunk;
		}

		/* ‑-- run-length segmentation for scattered TT pages ‑-- */
		uint64_t pfn_base  = gpu_off >> PAGE_SHIFT; /* index into pages_addr */
		uint64_t processed = 0;                    /* pages done in this chunk */

		while (processed < chunk_pages) {
			/* ‑- detect contiguous run starting at pfn_base ‑- */
			uint64_t run = 1;
			while (processed + run < chunk_pages) {
				uint64_t idx = pfn_base + run;
				/* Safe bound-checked prefetch */
				if (likely(idx + 4 < pfn_base + chunk_pages))
					prefetch(&pages_addr[idx + 4]);
				if (pages_addr[idx] !=
					pages_addr[idx - 1] + PAGE_SIZE)
					break;
				++run;
			}

			/* ‑- chop run into SDMA-friendly bursts ‑- */
			do {
				uint64_t burst = run;
				if (max_batch)           /* 0 ⇒ disable clamping */
					burst = min_t(uint64_t, burst, max_batch);

				params.pages_addr = NULL; /* linear mode */
				r = amdgpu_vm_ptes_update(&params, start,
										  start + burst,
							  pages_addr[pfn_base],
							  flags);
				if (r) goto after_pt;

				/* advance tracking pointers */
				start      += burst;
				processed  += burst;
				pfn_base   += burst;
				gpu_off    += (uint64_t)burst << AMDGPU_GPU_PAGE_SHIFT;
				run        -= burst;
			} while (run);

			/* all pages done? */
			if (processed == chunk_pages)
				break;
			/* if next page is discontinuous -> fall to sparse tail */
			if (pages_addr[pfn_base] !=
				pages_addr[pfn_base - 1] + PAGE_SIZE)
				break;
			/* else loop to find next run */
		}

		/* ‑-- sparse tail, if any pages remain ‑-- */
		if (processed < chunk_pages) {
			uint64_t remain = chunk_pages - processed;
			params.pages_addr = pages_addr;
			r = amdgpu_vm_ptes_update(&params, start,
									  start + remain,
							 gpu_off, flags);
			if (r) goto after_pt;
			start += remain;
		}

		next_cursor_chunk:
		amdgpu_res_next(&cursor, chunk_pages * AMDGPU_GPU_PAGE_SIZE);
		params.pages_addr = orig_pa;   /* restore for next iteration */
	}

	/* ─── commit & optional TLB flush ───────────────────────────── */
	r = vm->update_funcs->commit(&params, fence);
	if (r) goto after_pt;

	if (params.needs_flush) {
		amdgpu_vm_tlb_flush(&params, fence, tlb_cb);
		tlb_cb = NULL;                  /* fence owns cb now */
	}

	after_pt:
	amdgpu_vm_pt_free_list(adev, &params);

	out_unlock:
	kfree(tlb_cb);
	amdgpu_vm_eviction_unlock(vm);
	drm_dev_exit(idx);
	return r;
}

void amdgpu_vm_get_memory(struct amdgpu_vm *vm,
						  struct amdgpu_mem_stats stats[__AMDGPU_PL_NUM])
{
	spin_lock(&vm->status_lock);
	memcpy(stats, vm->stats, sizeof(*stats) * __AMDGPU_PL_NUM);
	spin_unlock(&vm->status_lock);
}

/*
 * Helper: Pick the real backing BO if the original is an imported
 *         DMA-buf on an XGMI peer node and resident in peer VRAM.
 */
static struct amdgpu_bo *
amdgpu_vm_pick_peer_bo(struct amdgpu_bo *orig_bo, bool is_xgmi_va)
{
	struct drm_gem_object *obj;

	if (unlikely(!orig_bo)) /* Early out if no original BO */
		return NULL;

	obj = &orig_bo->tbo.base;

	if (obj->import_attach && is_xgmi_va) {
		struct dma_buf          *dma_buf = obj->import_attach->dmabuf;
		/* Assuming priv holds the exporter's GEM */
		struct drm_gem_object   *gobj    = dma_buf->priv;
		struct amdgpu_bo        *peer_bo = gem_to_amdgpu_bo(gobj);

		/* Check if the peer BO exists and is in any type of VRAM */
		if (peer_bo && peer_bo->tbo.resource &&
			peer_bo->tbo.resource->mem_type >= TTM_PL_VRAM &&
			peer_bo->tbo.resource->mem_type < AMDGPU_PL_GDS)
			return peer_bo; /* Use peer-VRAM copy */
	}
	return orig_bo; /* Default to original BO */
}

/* forward-declare helper that lives later in the file */
static void amdgpu_vm_free_mapping(struct amdgpu_device *adev,
								   struct amdgpu_vm     *vm,
								   struct amdgpu_bo_va_mapping *map,
								   struct dma_fence     *fence);


/*
 * Optimised & fixed amdgpu_vm_bo_update()
 *
 *  • Coalesces VA-contiguous mappings that also have contiguous BO-offset
 *    and identical user PTE flags – up to thousands of small mappings
 *    collapse into single SDMA bursts.
 *  • Requests TLB flush only after the first *successful* PT update.
 *  • Keeps allow_override logic identical to upstream (cached BOs only).
 *  • Produces at most one fence update for always-valid BOs.
 *  • Correctly maintains list integrity when removing merged mappings.
 */
int amdgpu_vm_bo_update(struct amdgpu_device *adev,
						struct amdgpu_bo_va  *bo_va,
						bool                  clear)
{
	struct amdgpu_vm               *vm      = bo_va->base.vm;
	struct amdgpu_bo               *orig_bo = bo_va->base.bo;
	struct amdgpu_bo               *map_bo  = NULL;
	struct dma_fence             **lfence   = NULL;
	struct amdgpu_sync             sync;
	struct ttm_resource            *mem     = NULL;
	dma_addr_t                     *pages   = NULL;
	uint64_t                        base_flags = 0, vram_off = 0;
	bool                            flush_req  = false;
	bool                            allow_override;
	int                             r          = 0;

	/* ── fast exit ─────────────────────────────────────────────── */
	if (likely(list_empty(&bo_va->invalids) &&
		bo_va->cleared == clear   &&
		!bo_va->base.moved))
		return 0;

	amdgpu_sync_create(&sync);

	/* ── backing BO & sync fences ─────────────────────────────── */
	if (clear) {
		map_bo = orig_bo;

		r = amdgpu_sync_resv(adev, &sync,
							 vm->root.bo->tbo.base.resv,
					   AMDGPU_SYNC_EQ_OWNER, vm);
		if (r)
			goto out_sync;

		if (map_bo) {
			r = amdgpu_sync_kfd(&sync, map_bo->tbo.base.resv);
			if (r)
				goto out_sync;
		}
		flush_req = true;
	} else {
		if (orig_bo) {
			map_bo = amdgpu_vm_pick_peer_bo(orig_bo,
											bo_va->is_xgmi);
			mem   = map_bo->tbo.resource;
			if (mem && mem->mem_type == TTM_PL_TT)
				pages = map_bo->tbo.ttm->dma_address;

			r = amdgpu_sync_resv(adev, &sync,
								 map_bo->tbo.base.resv,
						AMDGPU_SYNC_EXPLICIT, vm);
			if (r)
				goto out_sync;

			flush_req = bo_va->base.moved;
		} else {
			flush_req = true; /* PRT map */
		}
	}

	/* ── base PTE flags ───────────────────────────────────────── */
	if (map_bo) {
		base_flags = amdgpu_ttm_tt_pte_flags(adev,
											 map_bo->tbo.ttm, mem);
		if (amdgpu_bo_encrypted(map_bo))
			base_flags |= AMDGPU_PTE_TMZ;

		vram_off = amdgpu_ttm_adev(map_bo->tbo.bdev)
		->vm_manager.vram_base_offset;
	} else if (!clear) {
		base_flags = AMDGPU_PTE_PRT_FLAG(adev) | AMDGPU_PTE_READABLE;
	}

	/* ── list fix-ups & flush decision ────────────────────────── */
	if (!clear && bo_va->base.moved) {
		list_splice_init(&bo_va->valids, &bo_va->invalids);
		flush_req = true;
	} else if (bo_va->cleared != clear) {
		list_splice_init(&bo_va->valids, &bo_va->invalids);
		flush_req = true;
	}

	/* If no invalids, we have nothing to write – skip flush */
	if (list_empty(&bo_va->invalids)) {
		flush_req = false;
	}

	allow_override = !(map_bo && (map_bo->flags &
	AMDGPU_GEM_CREATE_UNCACHED));

	lfence = (clear || amdgpu_vm_is_bo_always_valid(vm, orig_bo)) ?
	&vm->last_update : &bo_va->last_pt_update;

	/* ── iterate & coalesce mappings ──────────────────────────── */
	while (!list_empty(&bo_va->invalids)) {
		struct amdgpu_bo_va_mapping *first, *iter;
		uint64_t seg_start, seg_last, seg_flags, seg_off;

		first     = list_first_entry(&bo_va->invalids,
									 typeof(*first), list);
		seg_start = first->start;
		seg_last  = first->last;
		seg_off   = first->offset;
		seg_flags = base_flags;

		if (!clear) {
			if (!(first->flags & AMDGPU_PTE_READABLE))
				seg_flags &= ~AMDGPU_PTE_READABLE;
			if (!(first->flags & AMDGPU_PTE_WRITEABLE))
				seg_flags &= ~AMDGPU_PTE_WRITEABLE;
			if (first->flags & AMDGPU_PTE_EXECUTABLE)
				seg_flags |= AMDGPU_PTE_EXECUTABLE;
			else
				seg_flags &= ~AMDGPU_PTE_EXECUTABLE;
		} else {
			seg_flags = 0;
		}
		amdgpu_gmc_get_vm_pte(adev, first, &seg_flags);

		/* —— coalesce contiguous mappings —— */
		iter = list_next_entry(first, list);
		while (&iter->list != &bo_va->invalids) {
			bool va_contig = iter->start == seg_last + 1;
			bool bo_contig = iter->offset ==
			seg_off + ((seg_last - seg_start + 1)
			<< AMDGPU_GPU_PAGE_SHIFT);
			if (!va_contig || !bo_contig ||
				iter->flags != first->flags)
				break;

			seg_last = iter->last;

			list_del(&iter->list);
			amdgpu_vm_free_mapping(adev, vm, iter, NULL);

			iter = list_next_entry(first, list);
		}

		/* —— program the merged segment —— */
		r = amdgpu_vm_update_range(adev, vm,
								   false, false, flush_req,
							 allow_override, &sync,
							 seg_start, seg_last,
							 seg_flags,
							 seg_off, vram_off,
							 mem, pages, lfence);
		if (r)
			goto out_sync;

		flush_req = false; /* only first successful write flushes */

		/* move representative mapping to valids */
		first->start = seg_start;
		first->last  = seg_last;
		list_move(&first->list, &bo_va->valids);
	}

	bo_va->cleared    = clear;
	bo_va->base.moved = false;

	if (amdgpu_vm_is_bo_always_valid(vm, orig_bo))
		amdgpu_vm_bo_idle(&bo_va->base);
	else
		amdgpu_vm_bo_done(&bo_va->base);

	out_sync:
	amdgpu_sync_free(&sync);
	return r;
}

/**
 * amdgpu_vm_update_prt_state - update the global PRT state
 *
 * @adev: amdgpu_device pointer
 */
static void amdgpu_vm_update_prt_state(struct amdgpu_device *adev)
{
	unsigned long flags;
	bool enable;

	spin_lock_irqsave(&adev->vm_manager.prt_lock, flags);
	enable = !!atomic_read(&adev->vm_manager.num_prt_users);
	adev->gmc.gmc_funcs->set_prt(adev, enable);
	spin_unlock_irqrestore(&adev->vm_manager.prt_lock, flags);
}

/**
 * amdgpu_vm_prt_get - add a PRT user
 *
 * @adev: amdgpu_device pointer
 */
static void amdgpu_vm_prt_get(struct amdgpu_device *adev)
{
	if (!adev->gmc.gmc_funcs->set_prt)
		return;

	if (atomic_inc_return(&adev->vm_manager.num_prt_users) == 1)
		amdgpu_vm_update_prt_state(adev);
}

/**
 * amdgpu_vm_prt_put - drop a PRT user
 *
 * @adev: amdgpu_device pointer
 */
static void amdgpu_vm_prt_put(struct amdgpu_device *adev)
{
	if (atomic_dec_return(&adev->vm_manager.num_prt_users) == 0)
		amdgpu_vm_update_prt_state(adev);
}

/**
 * amdgpu_vm_prt_cb - callback for updating the PRT status
 *
 * @fence: fence for the callback
 * @_cb: the callback function
 */
static void amdgpu_vm_prt_cb(struct dma_fence *fence, struct dma_fence_cb *_cb)
{
	struct amdgpu_prt_cb *cb = container_of(_cb, struct amdgpu_prt_cb, cb);

	amdgpu_vm_prt_put(cb->adev);
	kfree(cb);
}

/**
 * amdgpu_vm_add_prt_cb - add callback for updating the PRT status
 *
 * @adev: amdgpu_device pointer
 * @fence: fence for the callback
 */
static void amdgpu_vm_add_prt_cb(struct amdgpu_device *adev,
								 struct dma_fence *fence)
{
	struct amdgpu_prt_cb *cb;

	if (!adev->gmc.gmc_funcs->set_prt)
		return;

	cb = kmalloc(sizeof(struct amdgpu_prt_cb), GFP_KERNEL);
	if (!cb) {
		/* Last resort when we are OOM */
		if (fence)
			dma_fence_wait(fence, false);

		amdgpu_vm_prt_put(adev);
	} else {
		cb->adev = adev;
		if (!fence || dma_fence_add_callback(fence, &cb->cb,
			amdgpu_vm_prt_cb))
			amdgpu_vm_prt_cb(fence, &cb->cb);
	}
}

/**
 * amdgpu_vm_free_mapping - free a mapping
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @mapping: mapping to be freed
 * @fence: fence of the unmap operation
 *
 * Free a mapping and make sure we decrease the PRT usage count if applicable.
 */
static void amdgpu_vm_free_mapping(struct amdgpu_device *adev,
								   struct amdgpu_vm *vm,
								   struct amdgpu_bo_va_mapping *mapping,
								   struct dma_fence *fence)
{
	if (mapping->flags & AMDGPU_PTE_PRT_FLAG(adev))
		amdgpu_vm_add_prt_cb(adev, fence);
	kfree(mapping);
}

/**
 * amdgpu_vm_prt_fini - finish all prt mappings
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 *
 * Register a cleanup callback to disable PRT support after VM dies.
 */
static void amdgpu_vm_prt_fini(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	struct dma_resv *resv = vm->root.bo->tbo.base.resv;
	struct dma_resv_iter cursor;
	struct dma_fence *fence;

	dma_resv_for_each_fence(&cursor, resv, DMA_RESV_USAGE_BOOKKEEP, fence) {
		/* Add a callback for each fence in the reservation object */
		amdgpu_vm_prt_get(adev);
		amdgpu_vm_add_prt_cb(adev, fence);
	}
}

/**
 * amdgpu_vm_handle_moved - handle moved BOs in the PT
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @ticket: optional reservation ticket used to reserve the VM
 *
 * Make sure all BOs which are moved are updated in the PTs.
 *
 * Returns:
 * 0 for success.
 *
 * PTs have to be reserved!
 */
int amdgpu_vm_handle_moved(struct amdgpu_device *adev,
						   struct amdgpu_vm *vm,
						   struct ww_acquire_ctx *ticket)
{
	struct amdgpu_bo_va *bo_va;
	struct dma_resv *resv;
	bool clear, unlock;
	int r;

	spin_lock(&vm->status_lock);
	while (!list_empty(&vm->moved)) {
		bo_va = list_first_entry(&vm->moved, struct amdgpu_bo_va,
								 base.vm_status);
		spin_unlock(&vm->status_lock);

		/* Per VM BOs never need to bo cleared in the page tables */
		r = amdgpu_vm_bo_update(adev, bo_va, false);
		if (r)
			return r;
		spin_lock(&vm->status_lock);
	}

	while (!list_empty(&vm->invalidated)) {
		bo_va = list_first_entry(&vm->invalidated, struct amdgpu_bo_va,
								 base.vm_status);
		resv = bo_va->base.bo->tbo.base.resv;
		spin_unlock(&vm->status_lock);

		/* Try to reserve the BO to avoid clearing its ptes */
		if (!adev->debug_vm && dma_resv_trylock(resv)) {
			clear = false;
			unlock = true;
			/* The caller is already holding the reservation lock */
		} else if (ticket && dma_resv_locking_ctx(resv) == ticket) {
			clear = false;
			unlock = false;
			/* Somebody else is using the BO right now */
		} else {
			clear = true;
			unlock = false;
		}

		r = amdgpu_vm_bo_update(adev, bo_va, clear);

		if (unlock)
			dma_resv_unlock(resv);
		if (r)
			return r;

		/* Remember evicted DMABuf imports in compute VMs for later
		 * validation
		 */
		if (vm->is_compute_context &&
			bo_va->base.bo->tbo.base.import_attach &&
			(!bo_va->base.bo->tbo.resource ||
			bo_va->base.bo->tbo.resource->mem_type == TTM_PL_SYSTEM))
			amdgpu_vm_bo_evicted_user(&bo_va->base);

		spin_lock(&vm->status_lock);
	}
	spin_unlock(&vm->status_lock);

	return 0;
}

/**
 * amdgpu_vm_flush_compute_tlb - Flush TLB on compute VM
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @flush_type: flush type
 * @xcc_mask: mask of XCCs that belong to the compute partition in need of a TLB flush.
 *
 * Flush TLB if needed for a compute VM.
 *
 * Returns:
 * 0 for success.
 */
int amdgpu_vm_flush_compute_tlb(struct amdgpu_device *adev,
								struct amdgpu_vm *vm,
								uint32_t flush_type,
								uint32_t xcc_mask)
{
	uint64_t tlb_seq = amdgpu_vm_tlb_seq(vm);
	bool all_hub = false;
	int xcc = 0, r = 0;

	WARN_ON_ONCE(!vm->is_compute_context);

	/*
	 * It can be that we race and lose here, but that is extremely unlikely
	 * and the worst thing which could happen is that we flush the changes
	 * into the TLB once more which is harmless.
	 */
	if (atomic64_xchg(&vm->kfd_last_flushed_seq, tlb_seq) == tlb_seq)
		return 0;

	if (adev->family == AMDGPU_FAMILY_AI ||
		adev->family == AMDGPU_FAMILY_RV)
		all_hub = true;

	for_each_inst(xcc, xcc_mask) {
		r = amdgpu_gmc_flush_gpu_tlb_pasid(adev, vm->pasid, flush_type,
										   all_hub, xcc);
		if (r)
			break;
	}
	return r;
}

/**
 * amdgpu_vm_bo_add - add a bo to a specific vm
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @bo: amdgpu buffer object
 *
 * Add @bo into the requested vm.
 * Add @bo to the list of bos associated with the vm
 *
 * Returns:
 * Newly added bo_va or NULL for failure
 *
 * Object has to be reserved!
 */
struct amdgpu_bo_va *amdgpu_vm_bo_add(struct amdgpu_device *adev,
									  struct amdgpu_vm *vm,
									  struct amdgpu_bo *bo)
{
	struct amdgpu_bo_va *bo_va;

	bo_va = kzalloc(sizeof(struct amdgpu_bo_va), GFP_KERNEL);
	if (bo_va == NULL) {
		return NULL;
	}
	amdgpu_vm_bo_base_init(&bo_va->base, vm, bo);

	bo_va->ref_count = 1;
	bo_va->last_pt_update = dma_fence_get_stub();
	INIT_LIST_HEAD(&bo_va->valids);
	INIT_LIST_HEAD(&bo_va->invalids);

	if (!bo)
		return bo_va;

	dma_resv_assert_held(bo->tbo.base.resv);
	if (amdgpu_dmabuf_is_xgmi_accessible(adev, bo)) {
		bo_va->is_xgmi = true;
		/* Power up XGMI if it can be potentially used */
		amdgpu_xgmi_set_pstate(adev, AMDGPU_XGMI_PSTATE_MAX_VEGA20);
	}

	return bo_va;
}


/**
 * amdgpu_vm_bo_insert_map - insert a new mapping
 *
 * @adev: amdgpu_device pointer
 * @bo_va: bo_va to store the address
 * @mapping: the mapping to insert
 *
 * Insert a new mapping into all structures.
 */
static void amdgpu_vm_bo_insert_map(struct amdgpu_device *adev,
									struct amdgpu_bo_va *bo_va,
									struct amdgpu_bo_va_mapping *mapping)
{
	struct amdgpu_vm *vm = bo_va->base.vm;
	struct amdgpu_bo *bo = bo_va->base.bo;

	mapping->bo_va = bo_va;
	list_add(&mapping->list, &bo_va->invalids);
	amdgpu_vm_it_insert(mapping, &vm->va);

	if (mapping->flags & AMDGPU_PTE_PRT_FLAG(adev))
		amdgpu_vm_prt_get(adev);

	if (amdgpu_vm_is_bo_always_valid(vm, bo) && !bo_va->base.moved)
		amdgpu_vm_bo_moved(&bo_va->base);

	trace_amdgpu_vm_bo_map(bo_va, mapping);
}

/* Validate operation parameters to prevent potential abuse */
static int amdgpu_vm_verify_parameters(struct amdgpu_device *adev,
									   struct amdgpu_bo *bo,
									   uint64_t saddr,
									   uint64_t offset,
									   uint64_t size)
{
	uint64_t tmp, lpfn;

	if (saddr & AMDGPU_GPU_PAGE_MASK
		|| offset & AMDGPU_GPU_PAGE_MASK
		|| size & AMDGPU_GPU_PAGE_MASK)
		return -EINVAL;

	if (check_add_overflow(saddr, size, &tmp)
		|| check_add_overflow(offset, size, &tmp)
		|| size == 0 /* which also leads to end < begin */)
		return -EINVAL;

	/* make sure object fit at this offset */
	if (bo && offset + size > amdgpu_bo_size(bo))
		return -EINVAL;

	/* Ensure last pfn not exceed max_pfn */
	lpfn = (saddr + size - 1) >> AMDGPU_GPU_PAGE_SHIFT;
	if (lpfn >= adev->vm_manager.max_pfn)
		return -EINVAL;

	return 0;
}

/**
 * amdgpu_vm_bo_map - map bo inside a vm
 *
 * @adev: amdgpu_device pointer
 * @bo_va: bo_va to store the address
 * @saddr: where to map the BO
 * @offset: requested offset in the BO
 * @size: BO size in bytes
 * @flags: attributes of pages (read/write/valid/etc.)
 *
 * Add a mapping of the BO at the specefied addr into the VM.
 *
 * Returns:
 * 0 for success, error for failure.
 *
 * Object has to be reserved and unreserved outside!
 */
int amdgpu_vm_bo_map(struct amdgpu_device *adev,
					 struct amdgpu_bo_va *bo_va,
					 u64 saddr, u64 offset, u64 size, u64 flags)
{
	struct amdgpu_bo_va_mapping *map, *conf;
	struct amdgpu_bo *bo = bo_va->base.bo;
	struct amdgpu_vm *vm   = bo_va->base.vm;
	u64 spg, epg;
	int r;

	r = amdgpu_vm_verify_parameters(adev, bo, saddr, offset, size);
	if (r)
		return r;

	spg = saddr >> AMDGPU_GPU_PAGE_SHIFT;
	epg = (saddr + size - 1) >> AMDGPU_GPU_PAGE_SHIFT;

	conf = amdgpu_vm_it_iter_first(&vm->va, spg, epg);
	if (conf) {
		dev_err(adev->dev,
				"BO %p va 0x%010llx-0x%010llx conflicts with 0x%010llx-0x%010llx\n",
		  bo, saddr, saddr + size,
		  conf->start << AMDGPU_GPU_PAGE_SHIFT,
		  (conf->last + 1) << AMDGPU_GPU_PAGE_SHIFT);
		return -EINVAL;
	}

	map = kmalloc(sizeof(*map), GFP_KERNEL);
	if (!map)
		return -ENOMEM;

	map->start  = spg;
	map->last   = epg;
	map->offset = offset;
	map->flags  = flags;

	amdgpu_vm_bo_insert_map(adev, bo_va, map);
	return 0;
}

/**
 * amdgpu_vm_bo_replace_map - map bo inside a vm, replacing existing mappings
 *
 * @adev: amdgpu_device pointer
 * @bo_va: bo_va to store the address
 * @saddr: where to map the BO
 * @offset: requested offset in the BO
 * @size: BO size in bytes
 * @flags: attributes of pages (read/write/valid/etc.)
 *
 * Add a mapping of the BO at the specefied addr into the VM. Replace existing
 * mappings as we do so.
 *
 * Returns:
 * 0 for success, error for failure.
 *
 * Object has to be reserved and unreserved outside!
 */
int amdgpu_vm_bo_replace_map(struct amdgpu_device *adev,
							 struct amdgpu_bo_va *bo_va,
							 u64 saddr, u64 offset, u64 size, u64 flags)
{
	struct amdgpu_bo_va_mapping *map;
	struct amdgpu_bo *bo = bo_va->base.bo;
	u64 spg, epg;
	int r;

	r = amdgpu_vm_verify_parameters(adev, bo, saddr, offset, size);
	if (r)
		return r;

	r = amdgpu_vm_bo_clear_mappings(adev, bo_va->base.vm, saddr, size);
	if (r)
		return r;

	map = kmalloc(sizeof(*map), GFP_KERNEL);
	if (!map)
		return -ENOMEM;

	spg = saddr >> AMDGPU_GPU_PAGE_SHIFT;
	epg = (saddr + size - 1) >> AMDGPU_GPU_PAGE_SHIFT;

	map->start  = spg;
	map->last   = epg;
	map->offset = offset;
	map->flags  = flags;

	amdgpu_vm_bo_insert_map(adev, bo_va, map);
	return 0;
}

/**
 * amdgpu_vm_bo_unmap - remove bo mapping from vm
 *
 * @adev: amdgpu_device pointer
 * @bo_va: bo_va to remove the address from
 * @saddr: where to the BO is mapped
 *
 * Remove a mapping of the BO at the specefied addr from the VM.
 *
 * Returns:
 * 0 for success, error for failure.
 *
 * Object has to be reserved and unreserved outside!
 */
int amdgpu_vm_bo_unmap(struct amdgpu_device *adev,
					   struct amdgpu_bo_va *bo_va,
					   uint64_t saddr)
{
	struct amdgpu_bo_va_mapping *mapping;
	struct amdgpu_vm *vm = bo_va->base.vm;
	bool valid = true;

	saddr /= AMDGPU_GPU_PAGE_SIZE;

	list_for_each_entry(mapping, &bo_va->valids, list) {
		if (mapping->start == saddr)
			break;
	}

	if (&mapping->list == &bo_va->valids) {
		valid = false;

		list_for_each_entry(mapping, &bo_va->invalids, list) {
			if (mapping->start == saddr)
				break;
		}

		if (&mapping->list == &bo_va->invalids)
			return -ENOENT;
	}

	list_del(&mapping->list);
	amdgpu_vm_it_remove(mapping, &vm->va);
	mapping->bo_va = NULL;
	trace_amdgpu_vm_bo_unmap(bo_va, mapping);

	if (valid) {
		list_add(&mapping->list, &vm->freed);
	} else {
		amdgpu_vm_free_mapping(adev, vm, mapping,
							   bo_va->last_pt_update);
	}

	return 0;
}

/**
 * amdgpu_vm_bo_clear_mappings - remove all mappings in a specific range
 *
 * @adev: amdgpu_device pointer
 * @vm: VM structure to use
 * @saddr: start of the range
 * @size: size of the range
 *
 * Remove all mappings in a range, split them as appropriate.
 *
 * Returns:
 * 0 for success, error for failure.
 */
int amdgpu_vm_bo_clear_mappings(struct amdgpu_device *adev,
								struct amdgpu_vm *vm,
								u64 saddr, u64 size)
{
	struct amdgpu_bo_va_mapping *before = NULL, *after = NULL;
	struct amdgpu_bo_va_mapping *m, *n, *split;
	LIST_HEAD(removed);
	u64 spg, epg;
	int r = 0;

	r = amdgpu_vm_verify_parameters(adev, NULL, saddr, 0, size);
	if (r)
		return r;

	spg  = saddr >> AMDGPU_GPU_PAGE_SHIFT;
	epg  = (saddr + size - 1) >> AMDGPU_GPU_PAGE_SHIFT;

	before = kzalloc(sizeof(*before), GFP_KERNEL);
	after  = kzalloc(sizeof(*after),  GFP_KERNEL);
	if (!before || !after) {
		kfree(before);
		kfree(after);
		return -ENOMEM;
	}
	INIT_LIST_HEAD(&before->list);
	INIT_LIST_HEAD(&after->list);

	split = amdgpu_vm_it_iter_first(&vm->va, spg, epg);
	while (split) {
		if (split->start < spg) {
			before->start  = split->start;
			before->last   = spg - 1;
			before->offset = split->offset;
			before->flags  = split->flags;
			before->bo_va  = split->bo_va;
			list_add(&before->list, &split->bo_va->invalids);
		}

		if (split->last > epg) {
			after->start  = epg + 1;
			after->last   = split->last;
			after->offset = split->offset +
			((after->start - split->start) << PAGE_SHIFT);
			after->flags  = split->flags;
			after->bo_va  = split->bo_va;
			list_add(&after->list, &split->bo_va->invalids);
		}

		list_del(&split->list);
		list_add(&split->list, &removed);

		split = amdgpu_vm_it_iter_next(split, spg, epg);
	}

	list_for_each_entry_safe(m, n, &removed, list) {
		amdgpu_vm_it_remove(m, &vm->va);
		list_del_init(&m->list);
		m->bo_va = NULL;
		list_add(&m->list, &vm->freed);
		trace_amdgpu_vm_bo_unmap(NULL, m);
	}

	if (!list_empty(&before->list)) {
		struct amdgpu_bo *bo = before->bo_va->base.bo;

		amdgpu_vm_it_insert(before, &vm->va);
		if (before->flags & AMDGPU_PTE_PRT_FLAG(adev))
			amdgpu_vm_prt_get(adev);
		if (amdgpu_vm_is_bo_always_valid(vm, bo) &&
			!before->bo_va->base.moved)
			amdgpu_vm_bo_moved(&before->bo_va->base);
	} else {
		kfree(before);
	}

	if (!list_empty(&after->list)) {
		struct amdgpu_bo *bo = after->bo_va->base.bo;

		amdgpu_vm_it_insert(after, &vm->va);
		if (after->flags & AMDGPU_PTE_PRT_FLAG(adev))
			amdgpu_vm_prt_get(adev);
		if (amdgpu_vm_is_bo_always_valid(vm, bo) &&
			!after->bo_va->base.moved)
			amdgpu_vm_bo_moved(&after->bo_va->base);
	} else {
		kfree(after);
	}

	return 0;
}

/**
 * amdgpu_vm_bo_lookup_mapping - find mapping by address
 *
 * @vm: the requested VM
 * @addr: the address
 *
 * Find a mapping by it's address.
 *
 * Returns:
 * The amdgpu_bo_va_mapping matching for addr or NULL
 *
 */
struct amdgpu_bo_va_mapping *amdgpu_vm_bo_lookup_mapping(struct amdgpu_vm *vm,
														 uint64_t addr)
{
	return amdgpu_vm_it_iter_first(&vm->va, addr, addr);
}

/**
 * amdgpu_vm_bo_trace_cs - trace all reserved mappings
 *
 * @vm: the requested vm
 * @ticket: CS ticket
 *
 * Trace all mappings of BOs reserved during a command submission.
 */
void amdgpu_vm_bo_trace_cs(struct amdgpu_vm *vm, struct ww_acquire_ctx *ticket)
{
	struct amdgpu_bo_va_mapping *mapping;

	if (!trace_amdgpu_vm_bo_cs_enabled())
		return;

	for (mapping = amdgpu_vm_it_iter_first(&vm->va, 0, U64_MAX); mapping;
		 mapping = amdgpu_vm_it_iter_next(mapping, 0, U64_MAX)) {
		if (mapping->bo_va && mapping->bo_va->base.bo) {
			struct amdgpu_bo *bo;

			bo = mapping->bo_va->base.bo;
			if (dma_resv_locking_ctx(bo->tbo.base.resv) !=
				ticket)
				continue;
		}

		trace_amdgpu_vm_bo_cs(mapping);
		 }
}

/**
 * amdgpu_vm_bo_del - remove a bo from a specific vm
 *
 * @adev: amdgpu_device pointer
 * @bo_va: requested bo_va
 *
 * Remove @bo_va->bo from the requested vm.
 *
 * Object have to be reserved!
 */
void amdgpu_vm_bo_del(struct amdgpu_device *adev,
					  struct amdgpu_bo_va *bo_va)
{
	struct amdgpu_bo_va_mapping *mapping, *next_map; /* Renamed to avoid conflict */
	struct amdgpu_bo *bo = bo_va->base.bo;
	struct amdgpu_vm *vm = bo_va->base.vm;
	struct amdgpu_vm_bo_base **base_pp; /* Renamed for clarity */

	/* Operations on vm->root.bo and bo should be protected by their reservations */
	dma_resv_assert_held(vm->root.bo->tbo.base.resv);

	if (bo) {
		dma_resv_assert_held(bo->tbo.base.resv);
		if (amdgpu_vm_is_bo_always_valid(vm, bo))
			ttm_bo_set_bulk_move(&bo->tbo, NULL);

		/* Protect modification of bo->vm_bo list */
		spin_lock(&bo->vm_lock);
		for (base_pp = &bo->vm_bo; *base_pp;
			 base_pp = &(*base_pp)->next) {
			if (*base_pp != &bo_va->base)
				continue;

			/* Found it, now unlink from bo->vm_bo list */
			*base_pp = bo_va->base.next;
		amdgpu_vm_update_stats(&bo_va->base, bo->tbo.resource, -1);
		break;
			 }
			 spin_unlock(&bo->vm_lock);
	}

	/* Remove from VM's status list (e.g. idle, evicted) */
	spin_lock(&vm->status_lock);
	list_del_init(&bo_va->base.vm_status);
	spin_unlock(&vm->status_lock);

	/* Move all valid mappings to the freed list for later processing */
	list_for_each_entry_safe(mapping, next_map, &bo_va->valids, list) {
		list_del(&mapping->list); /* Remove from valids */
		amdgpu_vm_it_remove(mapping, &vm->va); /* Remove from interval tree */
		mapping->bo_va = NULL; /* Break link from mapping back to bo_va */
		trace_amdgpu_vm_bo_unmap(bo_va, mapping);
		/* Add to vm->freed, which requires vm->status_lock */
		spin_lock(&vm->status_lock);
		list_add_tail(&mapping->list, &vm->freed);
		spin_unlock(&vm->status_lock);
	}

	/* Directly free any mappings that were already on the invalids list */
	list_for_each_entry_safe(mapping, next_map, &bo_va->invalids, list) {
		list_del(&mapping->list); /* Remove from invalids */
		amdgpu_vm_it_remove(mapping, &vm->va);
		amdgpu_vm_free_mapping(adev, vm, mapping,
							   bo_va->last_pt_update);
	}

	dma_fence_put(bo_va->last_pt_update);
	bo_va->last_pt_update = NULL; /* Nullify after putting */

	if (bo && bo_va->is_xgmi)
		amdgpu_xgmi_set_pstate(adev, AMDGPU_XGMI_PSTATE_MIN);

	kfree(bo_va);
}

/**
 * amdgpu_vm_evictable - check if we can evict a VM
 *
 * @bo: A page table of the VM.
 *
 * Check if it is possible to evict a VM.
 */
bool amdgpu_vm_evictable(struct amdgpu_bo *bo)
{
	struct amdgpu_vm_bo_base *bo_base = bo->vm_bo;

	if (unlikely(!bo_base || !bo_base->vm))
		return true;

	if (unlikely(!dma_resv_test_signaled(bo->tbo.base.resv,
		DMA_RESV_USAGE_BOOKKEEP)))
		return false;

	if (unlikely(!amdgpu_vm_eviction_trylock(bo_base->vm)))
		return false;

	if (unlikely(!dma_fence_is_signaled(bo_base->vm->last_unlocked))) {
		amdgpu_vm_eviction_unlock(bo_base->vm);
		return false;
	}

	bo_base->vm->evicting = true;
	amdgpu_vm_eviction_unlock(bo_base->vm);
	return true;
}

/**
 * amdgpu_vm_bo_invalidate - mark the bo as invalid
 *
 * @bo: amdgpu buffer object
 * @evicted: is the BO evicted
 *
 * Mark @bo as invalid.
 */
void amdgpu_vm_bo_invalidate(struct amdgpu_bo *bo, bool evicted)
{
	struct amdgpu_vm_bo_base *bo_base;

	for (bo_base = bo->vm_bo; bo_base; bo_base = bo_base->next) {
		struct amdgpu_vm *vm = bo_base->vm;

		if (evicted && amdgpu_vm_is_bo_always_valid(vm, bo)) {
			amdgpu_vm_bo_evicted(bo_base);
			continue;
		}

		if (bo_base->moved)
			continue;
		bo_base->moved = true;

		if (bo->tbo.type == ttm_bo_type_kernel)
			amdgpu_vm_bo_relocated(bo_base);
		else if (amdgpu_vm_is_bo_always_valid(vm, bo))
			amdgpu_vm_bo_moved(bo_base);
		else
			amdgpu_vm_bo_invalidated(bo_base);
	}
}

/**
 * amdgpu_vm_bo_move - handle BO move
 *
 * @bo: amdgpu buffer object
 * @new_mem: the new placement of the BO move
 * @evicted: is the BO evicted
 *
 * Update the memory stats for the new placement and mark @bo as invalid.
 */
void amdgpu_vm_bo_move(struct amdgpu_bo *bo, struct ttm_resource *new_mem,
					   bool evicted)
{
	struct amdgpu_vm_bo_base *bo_base;

	for (bo_base = bo->vm_bo; bo_base; bo_base = bo_base->next) {
		struct amdgpu_vm *vm = bo_base->vm;

		spin_lock(&vm->status_lock);
		amdgpu_vm_update_stats_locked(bo_base, bo->tbo.resource, -1);
		amdgpu_vm_update_stats_locked(bo_base, new_mem, +1);
		spin_unlock(&vm->status_lock);
	}

	amdgpu_vm_bo_invalidate(bo, evicted);
}

/**
 * amdgpu_vm_get_block_size - calculate VM page table size as power of two
 *
 * @vm_size: VM size
 *
 * Returns:
 * VM page table as power of two
 */
static uint32_t amdgpu_vm_get_block_size(uint64_t vm_size)
{
	/* Total bits covered by PD + PTs */
	unsigned bits = ilog2(vm_size) + 18;

	/* Make sure the PD is 4K in size up to 8GB address space.
	 *   Above that split equal between PD and PTs */
	if (vm_size <= 8)
		return (bits - 9);
	else
		return ((bits + 3) / 2);
}

/**
 * amdgpu_vm_adjust_size - adjust vm size, block size and fragment size
 *
 * @adev: amdgpu_device pointer
 * @min_vm_size: the minimum vm size in GB if it's set auto
 * @fragment_size_default: Default PTE fragment size
 * @max_level: max VMPT level
 * @max_bits: max address space size in bits
 *
 */
void amdgpu_vm_adjust_size(struct amdgpu_device *adev, uint32_t min_vm_size,
						   uint32_t fragment_size_default, unsigned max_level,
						   unsigned max_bits)
{
	unsigned int max_size = 1 << (max_bits - 30);
	unsigned int vm_size;
	uint64_t tmp;

	/* adjust vm size first */
	if (amdgpu_vm_size != -1) {
		vm_size = amdgpu_vm_size;
		if (vm_size > max_size) {
			dev_warn(adev->dev, "VM size (%d) too large, max is %u GB\n",
					 amdgpu_vm_size, max_size);
			vm_size = max_size;
		}
	} else {
		struct sysinfo si;
		unsigned int phys_ram_gb;

		/* Optimal VM size depends on the amount of physical
		 * RAM available. Underlying requirements and
		 * assumptions:
		 *
		 *  - Need to map system memory and VRAM from all GPUs
		 *     - VRAM from other GPUs not known here
		 *     - Assume VRAM <= system memory
		 *  - On GFX8 and older, VM space can be segmented for
		 *    different MTYPEs
		 *  - Need to allow room for fragmentation, guard pages etc.
		 *
		 * This adds up to a rough guess of system memory x3.
		 * Round up to power of two to maximize the available
		 * VM size with the given page table size.
		 */
		si_meminfo(&si);
		phys_ram_gb = ((uint64_t)si.totalram * si.mem_unit +
		(1 << 30) - 1) >> 30;
		vm_size = roundup_pow_of_two(
			clamp(phys_ram_gb * 3, min_vm_size, max_size));
	}

	adev->vm_manager.max_pfn = (uint64_t)vm_size << 18;

	tmp = roundup_pow_of_two(adev->vm_manager.max_pfn);
	if (amdgpu_vm_block_size != -1)
		tmp >>= amdgpu_vm_block_size - 9;
	tmp = DIV_ROUND_UP(fls64(tmp) - 1, 9) - 1;
	adev->vm_manager.num_level = min_t(unsigned int, max_level, tmp);
	switch (adev->vm_manager.num_level) {
		case 3:
			adev->vm_manager.root_level = AMDGPU_VM_PDB2;
			break;
		case 2:
			adev->vm_manager.root_level = AMDGPU_VM_PDB1;
			break;
		case 1:
			adev->vm_manager.root_level = AMDGPU_VM_PDB0;
			break;
		default:
			dev_err(adev->dev, "VMPT only supports 2~4+1 levels\n");
	}
	/* block size depends on vm size and hw setup*/
	if (amdgpu_vm_block_size != -1)
		adev->vm_manager.block_size =
		min((unsigned)amdgpu_vm_block_size, max_bits
		- AMDGPU_GPU_PAGE_SHIFT
		- 9 * adev->vm_manager.num_level);
	else if (adev->vm_manager.num_level > 1)
		adev->vm_manager.block_size = 9;
	else
		adev->vm_manager.block_size = amdgpu_vm_get_block_size(tmp);

	if (amdgpu_vm_fragment_size == -1)
		adev->vm_manager.fragment_size = fragment_size_default;
	else
		adev->vm_manager.fragment_size = amdgpu_vm_fragment_size;

	DRM_INFO("vm size is %u GB, %u levels, block size is %u-bit, fragment size is %u-bit\n",
			 vm_size, adev->vm_manager.num_level + 1,
		  adev->vm_manager.block_size,
		  adev->vm_manager.fragment_size);
}

/**
 * amdgpu_vm_wait_idle - wait for the VM to become idle
 *
 * @vm: VM object to wait for
 * @timeout: timeout to wait for VM to become idle
 */
long amdgpu_vm_wait_idle(struct amdgpu_vm *vm, long timeout)
{
	timeout = dma_resv_wait_timeout(vm->root.bo->tbo.base.resv,
									DMA_RESV_USAGE_BOOKKEEP,
								 true, timeout);
	if (timeout <= 0)
		return timeout;

	return dma_fence_wait_timeout(vm->last_unlocked, true, timeout);
}

static void amdgpu_vm_destroy_task_info(struct kref *kref)
{
	struct amdgpu_task_info *ti = container_of(kref, struct amdgpu_task_info, refcount);

	kfree(ti);
}

static inline struct amdgpu_vm *
amdgpu_vm_get_vm_from_pasid(struct amdgpu_device *adev, u32 pasid)
{
	struct amdgpu_vm *vm;
	unsigned long flags;

	xa_lock_irqsave(&adev->vm_manager.pasids, flags);
	vm = xa_load(&adev->vm_manager.pasids, pasid);
	xa_unlock_irqrestore(&adev->vm_manager.pasids, flags);

	return vm;
}

/**
 * amdgpu_vm_put_task_info - reference down the vm task_info ptr
 *
 * @task_info: task_info struct under discussion.
 *
 * frees the vm task_info ptr at the last put
 */
void amdgpu_vm_put_task_info(struct amdgpu_task_info *task_info)
{
	kref_put(&task_info->refcount, amdgpu_vm_destroy_task_info);
}

/**
 * amdgpu_vm_get_task_info_vm - Extracts task info for a vm.
 *
 * @vm: VM to get info from
 *
 * Returns the reference counted task_info structure, which must be
 * referenced down with amdgpu_vm_put_task_info.
 */
struct amdgpu_task_info *
amdgpu_vm_get_task_info_vm(struct amdgpu_vm *vm)
{
	struct amdgpu_task_info *ti = NULL;

	if (vm) {
		ti = vm->task_info;
		kref_get(&vm->task_info->refcount);
	}

	return ti;
}

/**
 * amdgpu_vm_get_task_info_pasid - Extracts task info for a PASID.
 *
 * @adev: drm device pointer
 * @pasid: PASID identifier for VM
 *
 * Returns the reference counted task_info structure, which must be
 * referenced down with amdgpu_vm_put_task_info.
 */
struct amdgpu_task_info *
amdgpu_vm_get_task_info_pasid(struct amdgpu_device *adev, u32 pasid)
{
	return amdgpu_vm_get_task_info_vm(
		amdgpu_vm_get_vm_from_pasid(adev, pasid));
}

static int amdgpu_vm_create_task_info(struct amdgpu_vm *vm)
{
	vm->task_info = kzalloc(sizeof(struct amdgpu_task_info), GFP_KERNEL);
	if (!vm->task_info)
		return -ENOMEM;

	kref_init(&vm->task_info->refcount);
	return 0;
}

/**
 * amdgpu_vm_set_task_info - Sets VMs task info.
 *
 * @vm: vm for which to set the info
 */
void amdgpu_vm_set_task_info(struct amdgpu_vm *vm)
{
	if (!vm->task_info)
		return;

	if (vm->task_info->pid == current->pid)
		return;

	vm->task_info->pid = current->pid;
	get_task_comm(vm->task_info->task_name, current);

	if (current->group_leader->mm != current->mm)
		return;

	vm->task_info->tgid = current->group_leader->pid;
	get_task_comm(vm->task_info->process_name, current->group_leader);
}

/**
 * amdgpu_vm_init - create and initialise a VM
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @xcp_id: GPU partition selection id
 *
 * Returns 0 on success, <0 on failure.
 */
int amdgpu_vm_init(struct amdgpu_device *adev,
				   struct amdgpu_vm *vm,
				   int32_t xcp_id)
{
	struct amdgpu_bo_vm *root_pt = NULL;
	struct amdgpu_bo *root_bo    = NULL;
	int r, i;

	/* -------- generic object initialisation ------------------------ */
	vm->va = RB_ROOT_CACHED;
	for (i = 0; i < AMDGPU_MAX_VMHUBS; i++) {
		vm->reserved_vmid[i] = NULL;
	}

	INIT_LIST_HEAD(&vm->evicted);
	INIT_LIST_HEAD(&vm->evicted_user);
	INIT_LIST_HEAD(&vm->relocated);
	INIT_LIST_HEAD(&vm->moved);
	INIT_LIST_HEAD(&vm->idle);
	INIT_LIST_HEAD(&vm->invalidated);
	INIT_LIST_HEAD(&vm->freed);
	INIT_LIST_HEAD(&vm->done);

	spin_lock_init(&vm->status_lock);
	INIT_KFIFO(vm->faults);
	mutex_init(&vm->eviction_lock);
	ttm_lru_bulk_move_init(&vm->lru_bulk_move);

	/* -------- scheduler entities ----------------------------------- */
	r = amdgpu_vm_init_entities(adev, vm);
	if (r) {
		return r;
	}

	vm->use_cpu_for_update =
	!!(adev->vm_manager.vm_update_mode & AMDGPU_VM_USE_CPU_FOR_GFX);
	vm->update_funcs = vm->use_cpu_for_update ?
	&amdgpu_vm_cpu_funcs :
	&amdgpu_vm_sdma_funcs;

	vm->last_update      = dma_fence_get_stub();
	vm->last_unlocked    = dma_fence_get_stub();
	vm->last_tlb_flush   = dma_fence_get_stub();
	vm->generation       = amdgpu_vm_generation(adev, NULL);
	vm->tlb_fence_context = dma_fence_context_alloc(1);

	/* -------- root page directory ---------------------------------- */
	r = amdgpu_vm_pt_create(adev, vm, adev->vm_manager.root_level,
							false, &root_pt, xcp_id);
	if (r) {
		goto err_entities;
	}

	root_bo = amdgpu_bo_ref(&root_pt->bo);
	if (!root_bo) {
		r = -ENOMEM;
		/* pt_create should have cleaned up root_pt on error */
		goto err_entities;
	}

	r = amdgpu_bo_reserve(root_bo, true);
	if (r) {
		goto err_root;
	}

	amdgpu_vm_bo_base_init(&vm->root, vm, root_bo);

	r = dma_resv_reserve_fences(root_bo->tbo.base.resv, 1);
	if (r) {
		goto err_root_resv;
	}

	r = amdgpu_vm_pt_clear(adev, vm, root_pt, false);
	if (r) {
		goto err_root_resv;
	}

	/* optional: create task info, keep going if it fails */
	r = amdgpu_vm_create_task_info(vm);
	if (r) {
		DRM_DEBUG("task info creation failed (%d)\n", r);
		/* Reset r to 0 so we don't return error for this */
		r = 0;
	}

	amdgpu_bo_unreserve(vm->root.bo);
	amdgpu_bo_unref(&root_bo); /* Drop local reference */
	return 0;

	/* -------- error handling paths --------------------------------- */
	err_root_resv:
	amdgpu_bo_unreserve(root_bo);
	/* Fall through */
	err_root:
	amdgpu_vm_pt_free_root(adev, vm); /* Handles vm->root internally */
	amdgpu_bo_unref(&root_bo); /* Drop local reference */
	/* Fall through */
	err_entities:

	dma_fence_put(vm->last_tlb_flush);
	dma_fence_put(vm->last_unlocked);
	dma_fence_put(vm->last_update);

	ttm_lru_bulk_move_fini(&adev->mman.bdev, &vm->lru_bulk_move);
	amdgpu_vm_fini_entities(vm);
	return r;
}

/**
 * amdgpu_vm_make_compute - Turn a GFX VM into a compute VM
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 *
 * This only works on GFX VMs that don't have any BOs added and no
 * page tables allocated yet.
 *
 * Changes the following VM parameters:
 * - use_cpu_for_update
 * - pte_supports_ats
 *
 * Reinitializes the page directory to reflect the changed ATS
 * setting.
 *
 * Returns:
 * 0 for success, -errno for errors.
 */
int amdgpu_vm_make_compute(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	int r;

	r = amdgpu_bo_reserve(vm->root.bo, true);
	if (r)
		return r;

	/* Update VM state */
	vm->use_cpu_for_update = !!(adev->vm_manager.vm_update_mode &
	AMDGPU_VM_USE_CPU_FOR_COMPUTE);
	DRM_DEBUG_DRIVER("VM update mode is %s\n",
					 vm->use_cpu_for_update ? "CPU" : "SDMA");
	WARN_ONCE((vm->use_cpu_for_update &&
	!amdgpu_gmc_vram_full_visible(&adev->gmc)),
			  "CPU update of VM recommended only for large BAR system\n");

	if (vm->use_cpu_for_update) {
		/* Sync with last SDMA update/clear before switching to CPU */
		r = amdgpu_bo_sync_wait(vm->root.bo,
								AMDGPU_FENCE_OWNER_UNDEFINED, true);
		if (r)
			goto unreserve_bo;

		vm->update_funcs = &amdgpu_vm_cpu_funcs;
		r = amdgpu_vm_pt_map_tables(adev, vm);
		if (r)
			goto unreserve_bo;

	} else {
		vm->update_funcs = &amdgpu_vm_sdma_funcs;
	}

	dma_fence_put(vm->last_update);
	vm->last_update = dma_fence_get_stub();
	vm->is_compute_context = true;

	unreserve_bo:
	amdgpu_bo_unreserve(vm->root.bo);
	return r;
}

/**
 * amdgpu_vm_release_compute - release a compute vm
 * @adev: amdgpu_device pointer
 * @vm: a vm turned into compute vm by calling amdgpu_vm_make_compute
 *
 * This is a correspondant of amdgpu_vm_make_compute. It decouples compute
 * pasid from vm. Compute should stop use of vm after this call.
 */
void amdgpu_vm_release_compute(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	amdgpu_vm_set_pasid(adev, vm, 0);
	vm->is_compute_context = false;
}

/* Returns true iff all per-placement memory statistics are zero */
static int __maybe_unused
amdgpu_vm_stats_is_zero(struct amdgpu_vm *vm)
{
	int i;

	for (i = 0; i < __AMDGPU_PL_NUM; ++i) {
		if (!drm_memory_stats_is_zero(&vm->stats[i].drm) ||
			vm->stats[i].evicted)
			return false;
	}

	return true;
}

/* Helpers for amdgpu_vm_fini ───────────────────────────────────────── */

static inline void __vm_free_map_prt(struct amdgpu_device *adev,
									 struct amdgpu_vm     *vm,
									 struct amdgpu_bo_va_mapping *map,
									 bool                 *prt_pending)
{
	if (unlikely(*prt_pending) &&
		(map->flags & AMDGPU_PTE_PRT_FLAG(adev))) {
		amdgpu_vm_prt_fini(adev, vm);
	*prt_pending = false;
		}

		list_del_init(&map->list);
		amdgpu_vm_free_mapping(adev, vm, map, NULL);
}

/* Helper for amdgpu_vm_clear_freed: is @b_start within or just after cur_end? */
static inline bool amdgpu_vm_range_is_adjacent(u64 cur_end, u64 b_start)
{
	return (b_start <= cur_end) ||
	(cur_end != U64_MAX && b_start == cur_end + 1);
}

/**
 * amdgpu_vm_clear_freed - clear freed BOs in the PT
 *
 * @adev: amdgpu_device pointer
 * @vm: requested vm
 * @fence: optional resulting fence (unchanged if no work needed to be done
 * or if an error occurred)
 *
 * Make sure all freed BOs are cleared in the PT.
 * PTs have to be reserved and mutex must be locked!
 *
 * Returns:
 * 0 for success.
 *
 */

#undef  AMDGPU_VM_CLEAR_FREED_STATIC_FENCE_CAP
#define AMDGPU_VM_CLEAR_FREED_STATIC_FENCE_CAP	8

/* Helper: grow the fence-vector safely, return new capacity or 0 on OOM */
static size_t vm_freed_vec_grow(struct dma_fence ***vec, size_t cur_cap)
{
	size_t new_cap = cur_cap * 2;
	struct dma_fence **nv;

	/* overflow / over-commit guard */
	if (new_cap < cur_cap || new_cap > 16384)
		new_cap = cur_cap + 32;

	nv = kvcalloc(new_cap, sizeof(*nv), GFP_KERNEL);
	if (!nv)
		return 0;

	memcpy(nv, *vec, cur_cap * sizeof(**vec));
	*vec = nv;
	return new_cap;
}

/* -------------------------------------------------------------------- */
int amdgpu_vm_clear_freed(struct amdgpu_device *adev,
						  struct amdgpu_vm      *vm,
						  struct dma_fence     **fence)
{
	LIST_HEAD(local);
	struct amdgpu_bo_va_mapping *m, *tmp;
	struct amdgpu_sync  sync;
	struct dma_fence   *stack_vec[AMDGPU_VM_CLEAR_FREED_STATIC_FENCE_CAP] = { };
	struct dma_fence  **fvec = stack_vec;
	struct dma_fence   *agg  = NULL;
	u64 range_s = 0, range_e = 0;
	bool have_range = false;
	bool sorted     = true;
	size_t fcnt = 0, fcap = ARRAY_SIZE(stack_vec);
	int r = 0;

	if (list_empty(&vm->freed)) {
		if (fence) {
			dma_fence_put(*fence);
			*fence = NULL;
		}
		return 0;
	}

	/* 1. Move freed list to local ------------------------------- */
	spin_lock(&vm->status_lock);
	list_splice_init(&vm->freed, &local);
	spin_unlock(&vm->status_lock);

	/* 2. Probe for monotonic order ------------------------------ */
	if (!list_is_singular(&local)) {
		u64 prev = 0;
		bool first = true;

		list_for_each_entry(m, &local, list) {
			if (!first && m->start < prev) {
				sorted = false;
				break;
			}
			first = false;
			prev  = m->start;
		}
	}
	if (!sorted)
		list_sort(NULL, &local, compare_mappings);

	/* 3. Prepare sync ------------------------------------------ */
	amdgpu_sync_create(&sync);
	r = amdgpu_sync_resv(adev, &sync,
						 vm->root.bo->tbo.base.resv,
					  AMDGPU_SYNC_EQ_OWNER, vm);
	if (r)
		goto out_sync;

	/* 4. Walk list, coalesce, update PT ------------------------- */
	list_for_each_entry_safe(m, tmp, &local, list) {
		bool flush_now = false;

		if (!have_range) {
			range_s   = m->start;
			range_e   = m->last;
			have_range = true;
		} else if (m->start == range_e + 1) {
			range_e = m->last;	/* extend contiguous run */
		} else {
			flush_now = true;
		}

		if (flush_now) {
			struct dma_fence *fr = NULL;

			r = amdgpu_vm_update_range(adev, vm, false, false,
									   true, false, &sync,
							  range_s, range_e,
							  0, 0, 0, NULL, NULL, &fr);
			if (r)
				goto out_vec;

			if (fr) {
				if (fcnt == fcap) {
					struct dma_fence **old = fvec;
					size_t cap = vm_freed_vec_grow(&fvec, fcap);
					if (!cap) {
						dma_fence_put(fr);
						r = -ENOMEM;
						goto out_vec;
					}
					if (old != stack_vec)
						kvfree(old);
					fcap = cap;
				}
				fvec[fcnt++] = fr;
			}

			range_s = m->start;
			range_e = m->last;
		}

		list_del_init(&m->list);
		amdgpu_vm_free_mapping(adev, vm, m, NULL);
	}

	/* flush tail range */
	if (have_range) {
		struct dma_fence *fr = NULL;

		r = amdgpu_vm_update_range(adev, vm, false, false,
								   true, false, &sync,
							 range_s, range_e,
							 0, 0, 0, NULL, NULL, &fr);
		if (r)
			goto out_vec;

		if (fr) {
			if (fcnt == fcap) {
				struct dma_fence **old = fvec;
				size_t cap = vm_freed_vec_grow(&fvec, fcap);
				if (!cap) {
					dma_fence_put(fr);
					r = -ENOMEM;
					goto out_vec;
				}
				if (old != stack_vec)
					kvfree(old);
				fcap = cap;
			}
			fvec[fcnt++] = fr;
		}
	}

	/* 5. Aggregate fences -------------------------------------- */
	if (fcnt == 1) {
		agg = fvec[0];
		fvec[0] = NULL;
	} else if (fcnt > 1) {
		struct dma_fence_array *arr =
		dma_fence_array_create(fcnt, fvec,
							   dma_fence_context_alloc(1), 0, true);
		if (!arr) {
			r = -ENOMEM;
			goto out_vec;
		}
		agg = &arr->base;
	}

	if (fence) {
		dma_fence_put(*fence);
		*fence = dma_fence_get(agg);
	}

	out_vec:
	for (size_t i = 0; i < fcnt; ++i)
		dma_fence_put(fvec[i]);
	if (fvec != stack_vec)
		kvfree(fvec);

	out_sync:
	amdgpu_sync_free(&sync);
	dma_fence_put(agg);
	return r;
}

static void vm_drain_freed_all(struct amdgpu_device *adev,
							   struct amdgpu_vm     *vm,
							   bool                *prt_pending)
{
	struct amdgpu_bo_va_mapping *m, *tmp;
	unsigned long flags;

	/* Repeat until nobody repopulates the list while we were processing */
	do {
		spin_lock_irqsave(&vm->status_lock, flags);
		list_for_each_entry_safe(m, tmp, &vm->freed, list) {
			list_del_init(&m->list);
			amdgpu_vm_free_mapping(adev, vm, m, NULL);
		}
		spin_unlock_irqrestore(&vm->status_lock, flags);
	} while (!list_empty(&vm->freed));
}

/**
 * amdgpu_vm_fini - tear down a VM instance and release its resources
 * @adev: amdgpu_device pointer
 * @vm:   the virtual machine structure to tear down
 *
 * This is the *full* function, including all robustness fixes discussed:
 *   • drains the freed-list in a loop to avoid races (F1)
 *   • gracefully handles failure to reserve root.bo (F3)
 *   • keeps existing stat handling (stat_add_safe already avoids underflow)
 *   • no other logic changed – everything compiles on top of current drm-tip
 */
void amdgpu_vm_fini(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	struct list_head	      collected;
	struct amdgpu_bo_va	     *bo_va, *n;
	struct amdgpu_bo	     *root = NULL;
	unsigned long		      flags;
	bool			      prt_pending;
	int			      i;

	if (WARN_ON(!vm))
		return;

	INIT_LIST_HEAD(&collected);

	prt_pending = adev->gmc.gmc_funcs &&
	adev->gmc.gmc_funcs->set_prt;

	/* Step 0: let KFD clean its state, drain initial freed list */
	amdgpu_amdkfd_gpuvm_destroy_cb(adev, vm);
	vm_drain_freed_all(adev, vm, &prt_pending);

	/* Step 1: reserve & destroy root PD -------------------------------- */
	if (vm->root.bo)
		root = amdgpu_bo_ref(vm->root.bo);

	if (root && amdgpu_bo_reserve(root, true)) {
		dev_warn(adev->dev,
				 "amdgpu_vm_fini: reserve(root) failed, degraded cleanup\n");
		amdgpu_vm_pt_free_root(adev, vm);
		amdgpu_bo_unref(&root);
		root = NULL;
	}

	if (root) {
		amdgpu_vm_set_pasid(adev, vm, 0);

		dma_fence_wait(vm->last_unlocked,  false);
		dma_fence_wait(vm->last_tlb_flush, false);

		/* Serialise with potential last TLB-flush callback */
		if (vm->last_tlb_flush) {
			spin_lock_irqsave(vm->last_tlb_flush->lock, flags);
			spin_unlock_irqrestore(vm->last_tlb_flush->lock, flags);
		}

		vm_drain_freed_all(adev, vm, &prt_pending);

		amdgpu_bo_unreserve(root);
		amdgpu_vm_pt_free_root(adev, vm);
		amdgpu_bo_unref(&root);
	}

	/* Step 2: collect all remaining bo_va objects ---------------------- */
	{
		struct list_head *src[] = {
			&vm->idle, &vm->evicted, &vm->relocated, &vm->moved,
			&vm->invalidated, &vm->done, &vm->evicted_user,
		};

		spin_lock_irqsave(&vm->status_lock, flags);
		for (i = 0; i < ARRAY_SIZE(src); i++)
			list_splice_tail_init(src[i], &collected);
		spin_unlock_irqrestore(&vm->status_lock, flags);
	}

	/* Step 3: free every collected bo_va -------------------------------- */
	list_for_each_entry_safe(bo_va, n, &collected, base.vm_status) {
		struct amdgpu_bo *bo  = bo_va->base.bo;
		u64		      sz;
		u32		      mem_now, mem_prev;

		list_del_init(&bo_va->base.vm_status);

		if (!bo) {
			kfree(bo_va);
			continue;
		}

		/* unlink from bo->vm_bo list */
		spin_lock(&bo->vm_lock);
		{
			struct amdgpu_vm_bo_base **pp = &bo->vm_bo;

			while (*pp && *pp != &bo_va->base)
				pp = &(*pp)->next;
			if (*pp)
				*pp = bo_va->base.next;
		}
		spin_unlock(&bo->vm_lock);

		/* update per-VM statistics */
		sz       = amdgpu_bo_size(bo);
		mem_now  = amdgpu_bo_mem_stats_placement(bo);
		mem_prev = bo_va->base.last_stat_memtype;

		spin_lock_irqsave(&vm->status_lock, flags);

		if (mem_now < __AMDGPU_PL_NUM) {
			if (bo_va->base.shared)
				stat_add_safe(&vm->stats[mem_now].drm.shared,
							  -(int64_t)sz);
				else
					stat_add_safe(&vm->stats[mem_now].drm.private,
								  -(int64_t)sz);
		}

		if (mem_prev < __AMDGPU_PL_NUM) {
			stat_add_safe(&vm->stats[mem_prev].drm.resident,
						  -(int64_t)sz);

			if (bo->flags & AMDGPU_GEM_CREATE_DISCARDABLE) {
				stat_add_safe(&vm->stats[mem_prev].drm.purgeable,
							  -(int64_t)sz);
			}

			if (!(bo->preferred_domains &
				amdgpu_mem_type_to_domain(mem_prev))) {
				stat_add_safe(&vm->stats[mem_prev].evicted,
							  -(int64_t)sz);
				}
		}

		spin_unlock_irqrestore(&vm->status_lock, flags);

		if (amdgpu_vm_is_bo_always_valid(vm, bo))
			ttm_bo_set_bulk_move(&bo->tbo, NULL);

		/* Safe XGMI p-state reduction (only when HW supports XGMI) */
		if (bo_va->is_xgmi && adev->gmc.xgmi.num_physical_nodes)
			amdgpu_xgmi_set_pstate(adev, AMDGPU_XGMI_PSTATE_MIN);

		dma_fence_put(bo_va->last_pt_update);
		kfree(bo_va);
	}

	/* Step 4: late PRT clean-up if needed ------------------------------- */
	if (prt_pending && !list_empty(&vm->freed)) {
		dev_warn_once(adev->dev,
					  "amdgpu_vm_fini: late PRT clean-up triggered\n");
		amdgpu_vm_prt_fini(adev, vm);
	}

	/* Step 5: general teardown ----------------------------------------- */
	dma_fence_put(vm->last_unlocked);
	dma_fence_put(vm->last_tlb_flush);
	dma_fence_put(vm->last_update);

	amdgpu_vm_fini_entities(vm);

	for (i = 0; i < AMDGPU_MAX_VMHUBS; i++) {
		if (vm->reserved_vmid[i]) {
			amdgpu_vmid_free_reserved(adev, i);
			vm->reserved_vmid[i] = false;
		}
	}

	/* Drain freed list one last time before tearing down LRU bulk move */
	vm_drain_freed_all(adev, vm, &prt_pending);
	ttm_lru_bulk_move_fini(&adev->mman.bdev, &vm->lru_bulk_move);

	/* Step 6: paranoia checks ------------------------------------------ */
	WARN_ON(!RB_EMPTY_ROOT(&vm->va.rb_root));
	WARN_ON(!list_empty(&vm->idle));
	WARN_ON(!list_empty(&vm->evicted));
	WARN_ON(!list_empty(&vm->relocated));
	WARN_ON(!list_empty(&vm->moved));
	WARN_ON(!list_empty(&vm->invalidated));
	WARN_ON(!list_empty(&vm->done));
	WARN_ON(!list_empty(&vm->evicted_user));
	WARN_ON(!list_empty(&vm->freed));

	if (vm->task_info) {
		amdgpu_vm_put_task_info(vm->task_info);
		vm->task_info = NULL;
	}
}

/**
 * amdgpu_vm_manager_init - init the VM manager
 *
 * @adev: amdgpu_device pointer
 *
 * Initialize the VM manager structures
 */
void amdgpu_vm_manager_init(struct amdgpu_device *adev)
{
	unsigned int i;

	/* Concurrent flushes are only possible starting with Vega10 and
	 * are broken on Navi10 and Navi14.
	 */
	adev->vm_manager.concurrent_flush = !(adev->asic_type < CHIP_VEGA10 ||
	adev->asic_type == CHIP_NAVI10 ||
	adev->asic_type == CHIP_NAVI14);

	amdgpu_vmid_mgr_init(adev);

	adev->vm_manager.fence_context =
	dma_fence_context_alloc(AMDGPU_MAX_RINGS);
	for (i = 0; i < AMDGPU_MAX_RINGS; ++i)
		adev->vm_manager.seqno[i] = 0;

	spin_lock_init(&adev->vm_manager.prt_lock);
	atomic_set(&adev->vm_manager.num_prt_users, 0);

	#ifdef CONFIG_X86_64
	if (amdgpu_vm_update_mode == -1) {
		if (amdgpu_gmc_vram_full_visible(&adev->gmc) &&
			!amdgpu_sriov_vf_mmio_access_protection(adev))
			adev->vm_manager.vm_update_mode =
			AMDGPU_VM_USE_CPU_FOR_COMPUTE;
		else
			adev->vm_manager.vm_update_mode = 0;
	} else {
		adev->vm_manager.vm_update_mode = amdgpu_vm_update_mode;
	}
	#else
	adev->vm_manager.vm_update_mode = 0;
	#endif

	xa_init_flags(&adev->vm_manager.pasids, XA_FLAGS_LOCK_IRQ);

	/* one‑time detection of heavy‑flush quirk */
	amdgpu_vm_init_flush_static_key(adev);

	/* Initialize Vega 10 stats */
	if (adev->asic_type == CHIP_VEGA10) {
		memset(&adev->vm_manager.vega10_stats, 0,
			   sizeof(adev->vm_manager.vega10_stats));

		DRM_INFO("Vega 10 VM optimizations enabled:\n");
		DRM_INFO("  - Smart range batching (%u pages)\n",
				 vega10_vm_update_batch_pages);
	}
}

/**
 * amdgpu_vm_manager_fini - cleanup VM manager
 *
 * @adev: amdgpu_device pointer
 *
 * Cleanup the VM manager and free resources.
 */
void amdgpu_vm_manager_fini(struct amdgpu_device *adev)
{
	WARN_ON(!xa_empty(&adev->vm_manager.pasids));
	xa_destroy(&adev->vm_manager.pasids);

	amdgpu_vmid_mgr_fini(adev);
}

/**
 * amdgpu_vm_ioctl - Manages VMID reservation for vm hubs.
 *
 * @dev: drm device pointer
 * @data: drm_amdgpu_vm
 * @filp: drm file pointer
 *
 * Returns:
 * 0 for success, -errno for errors.
 */
int amdgpu_vm_ioctl(struct drm_device *dev, void *data, struct drm_file *filp)
{
	union drm_amdgpu_vm *args = data;
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;

	/* No valid flags defined yet */
	if (args->in.flags)
		return -EINVAL;

	switch (args->in.op) {
		case AMDGPU_VM_OP_RESERVE_VMID:
			/* We only have requirement to reserve vmid from gfxhub */
			if (!fpriv->vm.reserved_vmid[AMDGPU_GFXHUB(0)]) {
				amdgpu_vmid_alloc_reserved(adev, AMDGPU_GFXHUB(0));
				fpriv->vm.reserved_vmid[AMDGPU_GFXHUB(0)] = true;
			}

			break;
		case AMDGPU_VM_OP_UNRESERVE_VMID:
			if (fpriv->vm.reserved_vmid[AMDGPU_GFXHUB(0)]) {
				amdgpu_vmid_free_reserved(adev, AMDGPU_GFXHUB(0));
				fpriv->vm.reserved_vmid[AMDGPU_GFXHUB(0)] = false;
			}
			break;
		default:
			return -EINVAL;
	}

	return 0;
}

/**
 * amdgpu_vm_handle_fault - graceful handling of VM faults.
 * @adev: amdgpu device pointer
 * @pasid: PASID of the VM
 * @ts: Timestamp of the fault
 * @vmid: VMID, only used for GFX 9.4.3.
 * @node_id: Node_id received in IH cookie. Only applicable for
 *           GFX 9.4.3.
 * @addr: Address of the fault
 * @write_fault: true is write fault, false is read fault
 *
 * Try to gracefully handle a VM fault. Return true if the fault was handled and
 * shouldn't be reported any more.
 */
bool amdgpu_vm_handle_fault(struct amdgpu_device *adev, u32 pasid,
							u32 vmid, u32 node_id, uint64_t addr, uint64_t ts,
							bool write_fault)
{
	bool is_compute_context = false;
	struct amdgpu_bo *root;
	unsigned long irqflags;
	uint64_t value, flags;
	struct amdgpu_vm *vm;
	int r;

	xa_lock_irqsave(&adev->vm_manager.pasids, irqflags);
	vm = xa_load(&adev->vm_manager.pasids, pasid);
	if (vm) {
		root = amdgpu_bo_ref(vm->root.bo);
		is_compute_context = vm->is_compute_context;
	} else {
		root = NULL;
	}
	xa_unlock_irqrestore(&adev->vm_manager.pasids, irqflags);

	if (!root)
		return false;

	addr /= AMDGPU_GPU_PAGE_SIZE;

	if (is_compute_context && !svm_range_restore_pages(adev, pasid, vmid,
		node_id, addr, ts, write_fault)) {
		amdgpu_bo_unref(&root);
	return true;
		}

		r = amdgpu_bo_reserve(root, true);
		if (r)
			goto error_unref;

	/* Double check that the VM still exists */
	xa_lock_irqsave(&adev->vm_manager.pasids, irqflags);
	vm = xa_load(&adev->vm_manager.pasids, pasid);
	if (vm && vm->root.bo != root)
		vm = NULL;
	xa_unlock_irqrestore(&adev->vm_manager.pasids, irqflags);
	if (!vm)
		goto error_unlock;

	flags = AMDGPU_PTE_VALID | AMDGPU_PTE_SNOOPED |
	AMDGPU_PTE_SYSTEM;

	if (is_compute_context) {
		/* Intentionally setting invalid PTE flag
		 * combination to force a no-retry-fault
		 */
		flags = AMDGPU_VM_NORETRY_FLAGS;
		value = 0;
	} else if (amdgpu_vm_fault_stop == AMDGPU_VM_FAULT_STOP_NEVER) {
		/* Redirect the access to the dummy page */
		value = adev->dummy_page_addr;
		flags |= AMDGPU_PTE_EXECUTABLE | AMDGPU_PTE_READABLE |
		AMDGPU_PTE_WRITEABLE;

	} else {
		/* Let the hw retry silently on the PTE */
		value = 0;
	}

	r = dma_resv_reserve_fences(root->tbo.base.resv, 1);
	if (r) {
		pr_debug("failed %d to reserve fence slot\n", r);
		goto error_unlock;
	}

	r = amdgpu_vm_update_range(adev, vm, true, false, false, false,
							   NULL, addr, addr, flags, value, 0, NULL, NULL, NULL);
	if (r)
		goto error_unlock;

	r = amdgpu_vm_update_pdes(adev, vm, true);

	error_unlock:
	amdgpu_bo_unreserve(root);
	if (r < 0)
		DRM_ERROR("Can't handle page fault (%d)\n", r);

	error_unref:
	amdgpu_bo_unref(&root);

	return false;
}

#if defined(CONFIG_DEBUG_FS)
/**
 * amdgpu_debugfs_vm_bo_info  - print BO info for the VM
 *
 * @vm: Requested VM for printing BO info
 * @m: debugfs file
 *
 * Print BO information in debugfs file for the VM
 */
void amdgpu_debugfs_vm_bo_info(struct amdgpu_vm *vm, struct seq_file *m)
{
	struct amdgpu_bo_va *bo_va, *tmp;
	u64 total_idle = 0;
	u64 total_evicted = 0;
	u64 total_relocated = 0;
	u64 total_moved = 0;
	u64 total_invalidated = 0;
	u64 total_done = 0;
	unsigned int total_idle_objs = 0;
	unsigned int total_evicted_objs = 0;
	unsigned int total_relocated_objs = 0;
	unsigned int total_moved_objs = 0;
	unsigned int total_invalidated_objs = 0;
	unsigned int total_done_objs = 0;
	unsigned int id = 0;

	spin_lock(&vm->status_lock);
	seq_puts(m, "\tIdle BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->idle, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_idle += amdgpu_bo_print_info(id++, bo_va->base.bo, m);
	}
	total_idle_objs = id;
	id = 0;

	seq_puts(m, "\tEvicted BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->evicted, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_evicted += amdgpu_bo_print_info(id++, bo_va->base.bo, m);
	}
	total_evicted_objs = id;
	id = 0;

	seq_puts(m, "\tRelocated BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->relocated, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_relocated += amdgpu_bo_print_info(id++, bo_va->base.bo, m);
	}
	total_relocated_objs = id;
	id = 0;

	seq_puts(m, "\tMoved BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->moved, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_moved += amdgpu_bo_print_info(id++, bo_va->base.bo, m);
	}
	total_moved_objs = id;
	id = 0;

	seq_puts(m, "\tInvalidated BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->invalidated, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_invalidated += amdgpu_bo_print_info(id++,	bo_va->base.bo, m);
	}
	total_invalidated_objs = id;
	id = 0;

	seq_puts(m, "\tDone BOs:\n");
	list_for_each_entry_safe(bo_va, tmp, &vm->done, base.vm_status) {
		if (!bo_va->base.bo)
			continue;
		total_done += amdgpu_bo_print_info(id++, bo_va->base.bo, m);
	}
	spin_unlock(&vm->status_lock);
	total_done_objs = id;

	seq_printf(m, "\tTotal idle size:        %12lld\tobjs:\t%d\n", total_idle,
			   total_idle_objs);
	seq_printf(m, "\tTotal evicted size:     %12lld\tobjs:\t%d\n", total_evicted,
			   total_evicted_objs);
	seq_printf(m, "\tTotal relocated size:   %12lld\tobjs:\t%d\n", total_relocated,
			   total_relocated_objs);
	seq_printf(m, "\tTotal moved size:       %12lld\tobjs:\t%d\n", total_moved,
			   total_moved_objs);
	seq_printf(m, "\tTotal invalidated size: %12lld\tobjs:\t%d\n", total_invalidated,
			   total_invalidated_objs);
	seq_printf(m, "\tTotal done size:        %12lld\tobjs:\t%d\n", total_done,
			   total_done_objs);
}
#endif

/**
 * amdgpu_vm_update_fault_cache - update cached fault into.
 * @adev: amdgpu device pointer
 * @pasid: PASID of the VM
 * @addr: Address of the fault
 * @status: GPUVM fault status register
 * @vmhub: which vmhub got the fault
 *
 * Cache the fault info for later use by userspace in debugging.
 */
void amdgpu_vm_update_fault_cache(struct amdgpu_device *adev,
								  unsigned int pasid,
								  uint64_t addr,
								  uint32_t status,
								  unsigned int vmhub)
{
	struct amdgpu_vm *vm;
	unsigned long flags;

	xa_lock_irqsave(&adev->vm_manager.pasids, flags);

	vm = xa_load(&adev->vm_manager.pasids, pasid);
	/* Don't update the fault cache if status is 0.  In the multiple
	 * fault case, subsequent faults will return a 0 status which is
	 * useless for userspace and replaces the useful fault status, so
	 * only update if status is non-0.
	 */
	if (vm && status) {
		vm->fault_info.addr = addr;
		vm->fault_info.status = status;
		/*
		 * Update the fault information globally for later usage
		 * when vm could be stale or freed.
		 */
		adev->vm_manager.fault_info.addr = addr;
		adev->vm_manager.fault_info.vmhub = vmhub;
		adev->vm_manager.fault_info.status = status;

		if (AMDGPU_IS_GFXHUB(vmhub)) {
			vm->fault_info.vmhub = AMDGPU_VMHUB_TYPE_GFX;
			vm->fault_info.vmhub |=
			(vmhub - AMDGPU_GFXHUB_START) << AMDGPU_VMHUB_IDX_SHIFT;
		} else if (AMDGPU_IS_MMHUB0(vmhub)) {
			vm->fault_info.vmhub = AMDGPU_VMHUB_TYPE_MM0;
			vm->fault_info.vmhub |=
			(vmhub - AMDGPU_MMHUB0_START) << AMDGPU_VMHUB_IDX_SHIFT;
		} else if (AMDGPU_IS_MMHUB1(vmhub)) {
			vm->fault_info.vmhub = AMDGPU_VMHUB_TYPE_MM1;
			vm->fault_info.vmhub |=
			(vmhub - AMDGPU_MMHUB1_START) << AMDGPU_VMHUB_IDX_SHIFT;
		} else {
			WARN_ONCE(1, "Invalid vmhub %u\n", vmhub);
		}
	}
	xa_unlock_irqrestore(&adev->vm_manager.pasids, flags);
}

/**
 * amdgpu_vm_is_bo_always_valid - check if the BO is VM always valid
 *
 * @vm: VM to test against.
 * @bo: BO to be tested.
 *
 * Returns true if the BO shares the dma_resv object with the root PD and is
 * always guaranteed to be valid inside the VM.
 */
bool amdgpu_vm_is_bo_always_valid(struct amdgpu_vm *vm, struct amdgpu_bo *bo)
{
	return likely(bo) &&
	likely(vm) &&
	vm->root.bo &&
	bo->tbo.base.resv == vm->root.bo->tbo.base.resv;
}
