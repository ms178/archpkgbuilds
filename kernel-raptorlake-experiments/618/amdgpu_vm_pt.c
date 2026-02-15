// SPDX-License-Identifier: GPL-2.0 OR MIT
/*
 * Copyright 2022 Advanced Micro Devices, Inc.
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
 */

#include <drm/drm_drv.h>
#include <linux/prefetch.h>
#include <linux/bitops.h>
#include "amdgpu.h"
#include "amdgpu_trace.h"
#include "amdgpu_vm.h"
#include "amdgpu_job.h"

#ifdef CONFIG_X86
# define fast_ctz64(x) ((unsigned int)__builtin_ctzll((unsigned long long)(x)))
#else
# define fast_ctz64(x) ((unsigned int)(ffsll((x)) - 1))
#endif

#define PT_UPDATE_BULK	256u

/*
 * amdgpu_vm_pt_cursor - state for for_each_amdgpu_vm_pt
 */
struct amdgpu_vm_pt_cursor {
	u64				pfn;
	struct amdgpu_vm_bo_base	*parent;
	struct amdgpu_vm_bo_base	*entry;
	unsigned int			level;
};

static unsigned int amdgpu_vm_pt_level_shift(struct amdgpu_device *adev,
											 unsigned int level)
{
	switch (level) {
		case AMDGPU_VM_PDB2:
		case AMDGPU_VM_PDB1:
		case AMDGPU_VM_PDB0:
			/* 9 bits per level, plus block_size for the root offset */
			return 9 * (AMDGPU_VM_PDB0 - level) +
			adev->vm_manager.block_size;
		case AMDGPU_VM_PTB:
			return 0;
		default:
			WARN_ON_ONCE(1);
			return ~0U;
	}
}

static unsigned int amdgpu_vm_pt_num_entries(struct amdgpu_device *adev,
											 unsigned int level)
{
	unsigned int shift =
	amdgpu_vm_pt_level_shift(adev, adev->vm_manager.root_level);

	if (level == adev->vm_manager.root_level)
		return round_up(adev->vm_manager.max_pfn, 1ULL << shift) >>
		shift;
	if (level != AMDGPU_VM_PTB)
		return 512;

	return AMDGPU_VM_PTE_COUNT(adev);
}

static u32 amdgpu_vm_pt_entries_mask(struct amdgpu_device *adev,
									 unsigned int level)
{
	if (level <= adev->vm_manager.root_level)
		return 0xffffffff;
	else if (level != AMDGPU_VM_PTB)
		return 0x1ff;

	return AMDGPU_VM_PTE_COUNT(adev) - 1;
}

static unsigned int amdgpu_vm_pt_size(struct amdgpu_device *adev,
									  unsigned int level)
{
	return AMDGPU_GPU_PAGE_ALIGN(
		amdgpu_vm_pt_num_entries(adev, level) * 8);
}

static struct amdgpu_vm_bo_base *
amdgpu_vm_pt_parent(struct amdgpu_vm_bo_base *pt)
{
	struct amdgpu_bo *par = pt->bo->parent;

	return par ? par->vm_bo : NULL;
}

/**
 * amdgpu_vm_pt_start - start PD/PT walk
 *
 * @adev: amdgpu_device pointer
 * @vm: amdgpu_vm structure
 * @start: start address of the walk
 * @cursor: state to initialize
 *
 * Initialize a amdgpu_vm_pt_cursor to start a walk.
 */
static void amdgpu_vm_pt_start(struct amdgpu_device *adev,
							   struct amdgpu_vm *vm, u64 start_pfn,
							   struct amdgpu_vm_pt_cursor *cur)
{
	cur->pfn    = start_pfn;
	cur->parent = NULL;
	cur->entry  = &vm->root;
	cur->level  = adev->vm_manager.root_level;
}

static bool amdgpu_vm_pt_descendant(struct amdgpu_device       *adev,
									struct amdgpu_vm_pt_cursor *cursor)
{
	unsigned int mask, shift, idx;
	struct amdgpu_bo_vm *next;

	if (unlikely(cursor->level == AMDGPU_VM_PTB) ||
		unlikely(!cursor->entry) ||
		unlikely(!cursor->entry->bo))
		return false;

	mask  = amdgpu_vm_pt_entries_mask(adev, cursor->level);
	shift = amdgpu_vm_pt_level_shift(adev, cursor->level);

	++cursor->level;
	idx         = (cursor->pfn >> shift) & mask;
	cursor->parent = cursor->entry;

	next = to_amdgpu_bo_vm(cursor->entry->bo);

	/* Prefetch the child entry itself plus the BO pointer it holds so
	 * that by the time we dereference them they are already in L2/L3.
	 */
	prefetch(&next->entries[idx]);
	prefetch(&next->entries[idx].bo);

	cursor->entry  = &next->entries[idx];
	return true;
}

static bool amdgpu_vm_pt_sibling(struct amdgpu_device *adev,
								 struct amdgpu_vm_pt_cursor *cur)
{
	unsigned int shift, num_entries;
	struct amdgpu_bo_vm *par;

	if (!cur->parent)
		return false;

	shift       = amdgpu_vm_pt_level_shift(adev, cur->level - 1);
	num_entries = amdgpu_vm_pt_num_entries(adev, cur->level - 1);
	par         = to_amdgpu_bo_vm(cur->parent->bo);

	if (cur->entry == &par->entries[num_entries - 1])
		return false;

	cur->pfn += 1ULL << shift;
	cur->pfn &= ~((1ULL << shift) - 1);
	++cur->entry;
	return true;
}

static bool amdgpu_vm_pt_ancestor(struct amdgpu_vm_pt_cursor *cur)
{
	if (!cur->parent)
		return false;

	--cur->level;
	cur->entry  = cur->parent;
	cur->parent = amdgpu_vm_pt_parent(cur->parent);
	return true;
}

static void amdgpu_vm_pt_next(struct amdgpu_device *adev,
							  struct amdgpu_vm_pt_cursor *cur)
{
	if (amdgpu_vm_pt_descendant(adev, cur))
		return;

	while (!amdgpu_vm_pt_sibling(adev, cur))
		if (!amdgpu_vm_pt_ancestor(cur)) {
			cur->pfn = ~0ULL;
			return;
		}
}

/* Depth-first search wrappers */

static void amdgpu_vm_pt_first_dfs(struct amdgpu_device *adev,
								   struct amdgpu_vm *vm,
								   struct amdgpu_vm_pt_cursor *start,
								   struct amdgpu_vm_pt_cursor *cur)
{
	if (start)
		*cur = *start;
	else
		amdgpu_vm_pt_start(adev, vm, 0, cur);

	while (amdgpu_vm_pt_descendant(adev, cur))
		;
}

static bool amdgpu_vm_pt_continue_dfs(struct amdgpu_vm_pt_cursor *start,
									  struct amdgpu_vm_bo_base *entry)
{
	return entry && (!start || entry != start->entry);
}

static void amdgpu_vm_pt_next_dfs(struct amdgpu_device *adev,
								  struct amdgpu_vm_pt_cursor *cur)
{
	if (!cur->entry)
		return;

	if (!cur->parent)
		cur->entry = NULL;
	else if (amdgpu_vm_pt_sibling(adev, cur))
		while (amdgpu_vm_pt_descendant(adev, cur))
			;
	else
		amdgpu_vm_pt_ancestor(cur);
}

/*
 * for_each_amdgpu_vm_pt_dfs_safe - safe deep first search of all PDs/PTs
 */
#define for_each_amdgpu_vm_pt_dfs_safe(adev, vm, start, cursor, entry)	\
for (amdgpu_vm_pt_first_dfs((adev), (vm), (start), &(cursor)),	\
	(entry) = (cursor).entry,					\
	amdgpu_vm_pt_next_dfs((adev), &(cursor));			\
	amdgpu_vm_pt_continue_dfs((start), (entry));		\
	(entry) = (cursor).entry,					\
	amdgpu_vm_pt_next_dfs((adev), &(cursor)))

/**
 * amdgpu_vm_pt_clear - initially clear the PDs/PTs
 *
 * @adev: amdgpu_device pointer
 * @vm: VM to clear BO from
 * @vmbo: BO to clear
 * @immediate: use an immediate update
 *
 * Root PD needs to be reserved when calling this.
 *
 * Returns:
 * 0 on success, errno otherwise.
 */
int amdgpu_vm_pt_clear(struct amdgpu_device *adev, struct amdgpu_vm *vm,
		       struct amdgpu_bo_vm *vmbo, bool immediate)
{
	unsigned int level = adev->vm_manager.root_level;
	struct ttm_operation_ctx ctx = { true, false };
	struct amdgpu_vm_update_params params;
	struct amdgpu_bo *ancestor = &vmbo->bo;
	unsigned int entries;
	struct amdgpu_bo *bo = &vmbo->bo;
	uint64_t addr;
	int r, idx;

	/* Figure out our place in the hierarchy */
	if (ancestor->parent) {
		++level;
		while (ancestor->parent->parent) {
			++level;
			ancestor = ancestor->parent;
		}
	}

	entries = amdgpu_bo_size(bo) / 8;

	r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
	if (r)
		return r;

	if (!drm_dev_enter(adev_to_drm(adev), &idx))
		return -ENODEV;

	r = vm->update_funcs->map_table(vmbo);
	if (r)
		goto exit;

	memset(&params, 0, sizeof(params));
	params.adev = adev;
	params.vm = vm;
	params.immediate = immediate;

	r = vm->update_funcs->prepare(&params, NULL,
				      AMDGPU_KERNEL_JOB_ID_VM_PT_CLEAR);
	if (r)
		goto exit;

	addr = 0;

	uint64_t value = 0, flags = 0;
	if (adev->asic_type >= CHIP_VEGA10) {
		if (level != AMDGPU_VM_PTB) {
			/* Handle leaf PDEs as PTEs */
			flags |= AMDGPU_PDE_PTE_FLAG(adev);
			amdgpu_gmc_get_vm_pde(adev, level,
					      &value, &flags);
		} else {
			/* Workaround for fault priority problem on GMC9 */
			flags = AMDGPU_PTE_EXECUTABLE;
		}
	}

	r = vm->update_funcs->update(&params, vmbo, addr, 0, entries,
				     value, flags);
	if (r)
		goto exit;

	r = vm->update_funcs->commit(&params, NULL);
exit:
	drm_dev_exit(idx);
	return r;
}

/**
 * amdgpu_vm_pt_create - create bo for PD/PT
 *
 * @adev: amdgpu_device pointer
 * @vm: requesting vm
 * @level: the page table level
 * @immediate: use a immediate update
 * @vmbo: pointer to the buffer object pointer
 * @xcp_id: GPU partition id
 */
int amdgpu_vm_pt_create(struct amdgpu_device *adev, struct amdgpu_vm *vm,
			int level, bool immediate, struct amdgpu_bo_vm **vmbo,
			int32_t xcp_id)
{
	struct amdgpu_bo_param bp;
	unsigned int num_entries;

	memset(&bp, 0, sizeof(bp));

	bp.size = amdgpu_vm_pt_size(adev, level);
	bp.byte_align = AMDGPU_GPU_PAGE_SIZE;

	if (!adev->gmc.is_app_apu)
		bp.domain = AMDGPU_GEM_DOMAIN_VRAM;
	else
		bp.domain = AMDGPU_GEM_DOMAIN_GTT;

	bp.domain = amdgpu_bo_get_preferred_domain(adev, bp.domain);
	bp.flags = AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS |
		AMDGPU_GEM_CREATE_CPU_GTT_USWC;

	if (level < AMDGPU_VM_PTB)
		num_entries = amdgpu_vm_pt_num_entries(adev, level);
	else
		num_entries = 0;

	bp.bo_ptr_size = struct_size((*vmbo), entries, num_entries);

	if (vm->use_cpu_for_update)
		bp.flags |= AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;

	bp.type = ttm_bo_type_kernel;
	bp.no_wait_gpu = immediate;
	bp.xcp_id_plus1 = xcp_id + 1;

	if (vm->root.bo)
		bp.resv = vm->root.bo->tbo.base.resv;

	return amdgpu_bo_create_vm(adev, &bp, vmbo);
}

/**
 * amdgpu_vm_pt_alloc - make sure a specific PD/PT exists
 *
 * @adev:      amdgpu_device pointer
 * @vm:        VM to allocate page tables for
 * @cursor:    points to the PD/PT that has to be present
 * @immediate: true if the update must be executed immediately
 *
 * Returns 0 on success, negative errno on failure.
 */
static int amdgpu_vm_pt_alloc(struct amdgpu_device            *adev,
							  struct amdgpu_vm                *vm,
							  struct amdgpu_vm_pt_cursor      *cursor,
							  bool                             immediate)
{
	struct amdgpu_bo_vm *pt  = NULL;
	struct amdgpu_bo    *pt_bo;
	int                   r;

	for (;;) {
		struct amdgpu_vm_bo_base *entry = cursor->entry;

		/* Fast path – somebody already created the PT. */
		if (entry->bo)
			return 0;

		/* Slow path – create a brand-new PT (drop eviction lock!). */
		amdgpu_vm_eviction_unlock(vm);
		r = amdgpu_vm_pt_create(adev, vm, cursor->level, immediate,
								&pt, vm->root.bo->xcp_id);
		amdgpu_vm_eviction_lock(vm);
		if (r)
			return r;

		pt_bo         = &pt->bo;

		/* Re-check after reacquiring the lock (race window closed). */
		if (entry->bo) {
			amdgpu_bo_unref(&pt_bo);	/* someone else won */
			continue;
		}

		/* Link new PT into the hierarchy. */
		pt_bo->parent = amdgpu_bo_ref(cursor->parent->bo);
		amdgpu_vm_bo_base_init(entry, vm, pt_bo);

		/* Clear freshly created PT. */
		r = amdgpu_vm_pt_clear(adev, vm, pt, immediate);
		if (!r) {
			return 0;			/* success */
		} else {
			/* ---------------- error rollback ---------------- */
			amdgpu_vm_update_stats(entry, pt_bo->tbo.resource, -1);
			pt_bo->vm_bo = NULL;

			spin_lock(&vm->status_lock);
			list_del_init(&entry->vm_status);
			spin_unlock(&vm->status_lock);

			entry->bo = NULL;

			amdgpu_bo_unref(&pt_bo->parent);
			amdgpu_bo_unref(&pt_bo);
			return r;
		}
	}
}

/**
 * amdgpu_vm_pt_free - free one PD/PT
 *
 * @entry: PDE to free
 */
static void amdgpu_vm_pt_free(struct amdgpu_vm_bo_base *entry)
{
	if (!entry->bo)
		return;

	amdgpu_vm_update_stats(entry, entry->bo->tbo.resource, -1);
	entry->bo->vm_bo = NULL;
	ttm_bo_set_bulk_move(&entry->bo->tbo, NULL);

	spin_lock(&entry->vm->status_lock);
	list_del(&entry->vm_status);
	spin_unlock(&entry->vm->status_lock);
	amdgpu_bo_unref(&entry->bo);
}

/**
 * amdgpu_vm_pt_free_list - free PD/PT levels
 *
 * @adev: amdgpu device structure
 * @params: see amdgpu_vm_update_params definition
 *
 * Free the page directory objects saved in the flush list
 */
void amdgpu_vm_pt_free_list(struct amdgpu_device *adev,
			    struct amdgpu_vm_update_params *params)
{
	struct amdgpu_vm_bo_base *entry, *next;
	bool unlocked = params->unlocked;

	if (list_empty(&params->tlb_flush_waitlist))
		return;

	/*
	 * unlocked unmap clear page table leaves, warning to free the page entry.
	 */
	WARN_ON(unlocked);

	list_for_each_entry_safe(entry, next, &params->tlb_flush_waitlist, vm_status)
		amdgpu_vm_pt_free(entry);
}

/**
 * amdgpu_vm_pt_add_list - add PD/PT level to the flush list
 *
 * @params: parameters for the update
 * @cursor: first PT entry to start DF search from, non NULL
 *
 * This list will be freed after TLB flush.
 */
static void amdgpu_vm_pt_add_list(struct amdgpu_vm_update_params *params,
				  struct amdgpu_vm_pt_cursor *cursor)
{
	struct amdgpu_vm_pt_cursor seek;
	struct amdgpu_vm_bo_base *entry;

	spin_lock(&params->vm->status_lock);
	for_each_amdgpu_vm_pt_dfs_safe(params->adev, params->vm, cursor, seek, entry) {
		if (entry && entry->bo)
			list_move(&entry->vm_status, &params->tlb_flush_waitlist);
	}

	/* enter start node now */
	list_move(&cursor->entry->vm_status, &params->tlb_flush_waitlist);
	spin_unlock(&params->vm->status_lock);
}

/**
 * amdgpu_vm_pt_free_root - free root PD
 * @adev: amdgpu device structure
 * @vm: amdgpu vm structure
 *
 * Free the root page directory and everything below it.
 */
void amdgpu_vm_pt_free_root(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	struct amdgpu_vm_pt_cursor cursor;
	struct amdgpu_vm_bo_base *entry;

	for_each_amdgpu_vm_pt_dfs_safe(adev, vm, NULL, cursor, entry) {
		if (entry)
			amdgpu_vm_pt_free(entry);
	}
}

/**
 * amdgpu_vm_pde_update - update a single level in the hierarchy
 *
 * @params: parameters for the update
 * @entry: entry to update
 *
 * Makes sure the requested entry in parent is up to date.
 */
int amdgpu_vm_pde_update(struct amdgpu_vm_update_params *params,
			 struct amdgpu_vm_bo_base *entry)
{
	struct amdgpu_vm_bo_base *parent = amdgpu_vm_pt_parent(entry);
	struct amdgpu_bo *bo, *pbo;
	struct amdgpu_vm *vm = params->vm;
	uint64_t pde, pt, flags;
	unsigned int level;

	if (WARN_ON(!parent))
		return -EINVAL;

	bo = parent->bo;
	for (level = 0, pbo = bo->parent; pbo; ++level)
		pbo = pbo->parent;

	level += params->adev->vm_manager.root_level;
	amdgpu_gmc_get_pde_for_bo(entry->bo, level, &pt, &flags);
	pde = (entry - to_amdgpu_bo_vm(parent->bo)->entries) * 8;
	return vm->update_funcs->update(params, to_amdgpu_bo_vm(bo), pde, pt,
					1, 0, flags);
}

/**
 * amdgpu_vm_pte_update_noretry_flags - Update PTE no-retry flags
 *
 * @adev: amdgpu_device pointer
 * @flags: pointer to PTE flags
 *
 * Update PTE no-retry flags when TF is enabled.
 */
static void amdgpu_vm_pte_update_noretry_flags(struct amdgpu_device *adev,
						uint64_t *flags)
{
	/*
	 * Update no-retry flags with the corresponding TF
	 * no-retry combination.
	 */
	if ((*flags & AMDGPU_VM_NORETRY_FLAGS) == AMDGPU_VM_NORETRY_FLAGS) {
		*flags &= ~AMDGPU_VM_NORETRY_FLAGS;
		*flags |= adev->gmc.noretry_flags;
	}
}

/*
 * amdgpu_vm_pte_update_flags - figure out flags for PTE updates
 *
 * Make sure to set the right flags for the PTEs at the desired level.
 */
static void amdgpu_vm_pte_update_flags(struct amdgpu_vm_update_params *params,
				       struct amdgpu_bo_vm *pt,
				       unsigned int level,
				       uint64_t pe, uint64_t addr,
				       unsigned int count, uint32_t incr,
				       uint64_t flags)
{
	struct amdgpu_device *adev = params->adev;

	if (level != AMDGPU_VM_PTB) {
		flags |= AMDGPU_PDE_PTE_FLAG(params->adev);
		amdgpu_gmc_get_vm_pde(adev, level, &addr, &flags);

	} else if (adev->asic_type >= CHIP_VEGA10 &&
		   !(flags & AMDGPU_PTE_VALID) &&
		   !(flags & AMDGPU_PTE_PRT_FLAG(params->adev))) {

		/* Workaround for fault priority problem on GMC9 */
		flags |= AMDGPU_PTE_EXECUTABLE;
	}

	/*
	 * Update no-retry flags to use the no-retry flag combination
	 * with TF enabled. The AMDGPU_VM_NORETRY_FLAGS flag combination
	 * does not work when TF is enabled. So, replace them with
	 * AMDGPU_VM_NORETRY_FLAGS_TF flag combination which works for
	 * all cases.
	 */
	if (level == AMDGPU_VM_PTB)
		amdgpu_vm_pte_update_noretry_flags(adev, &flags);

	/* APUs mapping system memory may need different MTYPEs on different
	 * NUMA nodes. Only do this for contiguous ranges that can be assumed
	 * to be on the same NUMA node.
	 */
	if ((flags & AMDGPU_PTE_SYSTEM) && (adev->flags & AMD_IS_APU) &&
	    adev->gmc.gmc_funcs->override_vm_pte_flags &&
	    num_possible_nodes() > 1 && !params->pages_addr && params->allow_override)
		amdgpu_gmc_override_vm_pte_flags(adev, params->vm, addr, &flags);

	params->vm->update_funcs->update(params, pt, pe, addr, count, incr,
					 flags);
}

/**
 * amdgpu_vm_pte_fragment - get fragment for PTEs
 *
 * @params: see amdgpu_vm_update_params definition
 * @start: first PTE to handle
 * @end: last PTE to handle
 * @flags: hw mapping flags
 * @frag: resulting fragment size
 * @frag_end: end of this fragment
 *
 * Returns the first possible fragment for the start and end address.
 */
static void amdgpu_vm_pte_fragment(struct amdgpu_vm_update_params *params,
								   u64 start, u64 end, u64 flags,
								   unsigned int *frag, u64 *frag_end)
{
	unsigned int max_frag;

	if (params->adev->asic_type < CHIP_VEGA10) {
		max_frag = params->adev->vm_manager.fragment_size;
	} else {
		max_frag = 31;
	}

	/* system pages are non-continuous */
	if (params->pages_addr) {
		*frag     = 0;
		*frag_end = end;
		return;
	}

	/* This calculation intentionally wraps if no bit is set.        */
	*frag = min_t(unsigned int,
				  fast_ctz64(start),           /* alignment restriction */
				  fls64(end - start) - 1);      /* span restriction      */

	if (*frag >= max_frag) {
		*frag     = max_frag;
		*frag_end = end & ~((1ULL << max_frag) - 1);
	} else {
		*frag_end = start + (1ULL << *frag);
	}
}

/**
 * amdgpu_vm_ptes_update - make sure that page tables are valid
 *
 * @params: see amdgpu_vm_update_params definition
 * @start: start of GPU address range
 * @end: end of GPU address range
 * @dst: destination address to map to, the next dst inside the function
 * @flags: mapping flags
 *
 * Update the page tables in the range @start - @end.
 *
 * Returns:
 * 0 for success, -EINVAL for failure.
 */
int amdgpu_vm_ptes_update(struct amdgpu_vm_update_params *params,
						  u64 start, u64 end,
						  u64 dst,   u64 flags)
{
	struct amdgpu_device *adev = params->adev;
	struct amdgpu_vm_pt_cursor cursor;
	u64 frag_start = start, frag_end;
	unsigned int frag;
	int r;

	/* first fragment */
	amdgpu_vm_pte_fragment(params, frag_start, end, flags,
						   &frag, &frag_end);

	amdgpu_vm_pt_start(adev, params->vm, start, &cursor);

	while (cursor.pfn < end) {
		unsigned int shift, parent_shift, mask;
		u64 incr, entry_end, pe_start;
		struct amdgpu_bo *pt;

		if (!params->unlocked) {
			r = amdgpu_vm_pt_alloc(adev, params->vm,
								   &cursor, params->immediate);
			if (r) {
				return r;
			}
		}

		shift        = amdgpu_vm_pt_level_shift(adev, cursor.level);
		parent_shift = amdgpu_vm_pt_level_shift(adev,
												cursor.level - 1);

		if (params->unlocked) {
			if (amdgpu_vm_pt_descendant(adev, &cursor)) {
				continue;
			}
		} else if ((adev->asic_type < CHIP_VEGA10) &&
			(flags & AMDGPU_PTE_VALID)) {
			if (cursor.level != AMDGPU_VM_PTB) {
				if (!amdgpu_vm_pt_descendant(adev, &cursor)) {
					return -ENOENT;
				}
				continue;
			}
			} else if (frag < shift) {
				if (amdgpu_vm_pt_descendant(adev, &cursor)) {
					continue;
				}
			} else if (frag >= parent_shift) {
				if (!amdgpu_vm_pt_ancestor(&cursor)) {
					return -EINVAL;
				}
				continue;
			}

			pt = cursor.entry->bo;
			if (!pt) {
				if (flags & AMDGPU_PTE_VALID) {
					return -ENOENT;
				}
				if (!amdgpu_vm_pt_ancestor(&cursor)) {
					return -EINVAL;
				}

				pt       = cursor.entry->bo;
				shift    = parent_shift;
				frag_end = max(frag_end,
							   ALIGN(frag_start + 1, 1ULL << shift));
			}

			incr     = (u64)AMDGPU_GPU_PAGE_SIZE << shift;
			mask     = amdgpu_vm_pt_entries_mask(adev, cursor.level);
			pe_start = ((cursor.pfn >> shift) & mask) * 8;

			if ((cursor.level < AMDGPU_VM_PTB) && params->unlocked) {
				entry_end = 1ULL << shift;
			} else {
				entry_end = ((u64)mask + 1) << shift;
			}
			entry_end += cursor.pfn & ~(entry_end - 1);
			entry_end  = min(entry_end, end);

			do {
				struct amdgpu_vm *vm = params->vm;
				u64 upd_end      = min(entry_end, frag_end);
				unsigned int nptes =
				(upd_end - frag_start) >> shift;
				u64 upd_flags   = flags | AMDGPU_PTE_FRAG(frag);

				nptes = max(nptes, 1u);

				trace_amdgpu_vm_update_ptes(params, frag_start,
											upd_end,
								min(nptes, 32u),
											dst, incr, upd_flags,
								vm->task_info ?
								vm->task_info->tgid : 0,
								vm->immediate.fence_context);

				amdgpu_vm_pte_update_flags(params,
										   to_amdgpu_bo_vm(pt),
										   cursor.level,
							   pe_start, dst,
							   nptes, incr, upd_flags);

				pe_start  += nptes * 8;
				dst       += nptes * incr;
				frag_start = upd_end;

				if (frag_start >= frag_end) {
					amdgpu_vm_pte_fragment(params, frag_start, end,
										   flags,
							&frag, &frag_end);
					if (frag < shift) {
						break;
					}
				}
			} while (frag_start < entry_end);

			if (amdgpu_vm_pt_descendant(adev, &cursor)) {
				while (cursor.pfn < frag_start) {
					if (cursor.entry->bo) {
						params->needs_flush = true;
						amdgpu_vm_pt_add_list(params, &cursor);
					}
					amdgpu_vm_pt_next(adev, &cursor);
				}
			} else if (frag >= shift) {
				amdgpu_vm_pt_next(adev, &cursor);
			}
	}

	return 0;
}

/**
 * amdgpu_vm_pt_map_tables - have bo of root PD cpu accessible
 * @adev: amdgpu device structure
 * @vm: amdgpu vm structure
 *
 * make root page directory and everything below it cpu accessible.
 */
int amdgpu_vm_pt_map_tables(struct amdgpu_device *adev, struct amdgpu_vm *vm)
{
	struct amdgpu_vm_pt_cursor cursor;
	struct amdgpu_vm_bo_base *entry;

	for_each_amdgpu_vm_pt_dfs_safe(adev, vm, NULL, cursor, entry) {

		struct amdgpu_bo_vm *bo;
		int r;

		if (entry->bo) {
			bo = to_amdgpu_bo_vm(entry->bo);
			r = vm->update_funcs->map_table(bo);
			if (r)
				return r;
		}
	}

	return 0;
}
