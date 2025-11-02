/*
 * Copyright © 2014 Advanced Micro Devices, Inc.
 * All Rights Reserved.
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

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdbool.h>

#include "amdgpu.h"
#include "amdgpu_drm.h"
#include "amdgpu_internal.h"
#include "util_math.h"

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

drm_public int amdgpu_va_range_query(amdgpu_device_handle dev,
				     enum amdgpu_gpu_va_range type,
				     uint64_t *start,
				     uint64_t *end)
{
	if (unlikely(!dev || !start || !end)) {
		return -EINVAL;
	}

	if (type != amdgpu_gpu_va_range_general) {
		return -EINVAL;
	}

	*start = dev->dev_info.virtual_address_offset;
	*end = dev->dev_info.virtual_address_max;
	return 0;
}

drm_private void amdgpu_vamgr_init(struct amdgpu_bo_va_mgr *mgr,
				   uint64_t start,
				   uint64_t max,
				   uint64_t alignment)
{
	struct amdgpu_bo_va_hole *n;

	if (unlikely(!mgr)) {
		return;
	}

	if (unlikely(max <= start)) {
		mgr->va_max = 0;
		mgr->va_alignment = alignment;
		list_inithead(&mgr->va_holes);
		pthread_mutex_init(&mgr->bo_va_mutex, NULL);
		return;
	}

	mgr->va_max = max;
	mgr->va_alignment = alignment;

	list_inithead(&mgr->va_holes);
	pthread_mutex_init(&mgr->bo_va_mutex, NULL);

	n = calloc(1, sizeof(struct amdgpu_bo_va_hole));
	if (unlikely(!n)) {
		return;
	}

	n->size = max - start;
	n->offset = start;

	pthread_mutex_lock(&mgr->bo_va_mutex);
	list_add(&n->list, &mgr->va_holes);
	pthread_mutex_unlock(&mgr->bo_va_mutex);
}

drm_private void amdgpu_vamgr_deinit(struct amdgpu_bo_va_mgr *mgr)
{
	struct amdgpu_bo_va_hole *hole;
	struct amdgpu_bo_va_hole *tmp;

	if (unlikely(!mgr)) {
		return;
	}

	LIST_FOR_EACH_ENTRY_SAFE(hole, tmp, &mgr->va_holes, list) {
		list_del(&hole->list);
		free(hole);
	}

	pthread_mutex_destroy(&mgr->bo_va_mutex);
}

static drm_private int
amdgpu_vamgr_subtract_hole(struct amdgpu_bo_va_hole *hole,
			   uint64_t start_va,
			   uint64_t end_va)
{
	struct amdgpu_bo_va_hole *n;

	if (unlikely(!hole)) {
		return -EINVAL;
	}

	if (unlikely(end_va <= start_va)) {
		return -EINVAL;
	}

	if (unlikely(start_va >= (hole->offset + hole->size))) {
		return -EINVAL;
	}

	if (unlikely(end_va <= hole->offset)) {
		return -EINVAL;
	}

	if (start_va > hole->offset && end_va < (hole->offset + hole->size)) {
		n = calloc(1, sizeof(struct amdgpu_bo_va_hole));
		if (unlikely(!n)) {
			return -ENOMEM;
		}

		n->size = start_va - hole->offset;
		n->offset = hole->offset;
		list_add(&n->list, &hole->list);

		hole->size -= (end_va - hole->offset);
		hole->offset = end_va;
	} else if (start_va > hole->offset) {
		hole->size = start_va - hole->offset;
	} else if (end_va < (hole->offset + hole->size)) {
		hole->size -= (end_va - hole->offset);
		hole->offset = end_va;
	} else {
		list_del(&hole->list);
		free(hole);
	}

	return 0;
}

static drm_private int
amdgpu_vamgr_find_va(struct amdgpu_bo_va_mgr *mgr,
		     uint64_t size,
		     uint64_t alignment,
		     uint64_t base_required,
		     bool search_from_top,
		     uint64_t *va_out)
{
	struct amdgpu_bo_va_hole *hole;
	struct amdgpu_bo_va_hole *n;
	uint64_t offset;
	int ret;

	if (unlikely(!mgr || !va_out)) {
		return -EINVAL;
	}

	if (unlikely(size == 0)) {
		return -EINVAL;
	}

	if (unlikely(alignment == 0)) {
		return -EINVAL;
	}

	if (unlikely(alignment & (alignment - 1))) {
		return -EINVAL;
	}

	alignment = MAX2(alignment, mgr->va_alignment);
	size = ALIGN(size, mgr->va_alignment);

	if (unlikely(size > mgr->va_max)) {
		return -ENOMEM;
	}

	if (base_required != 0 && unlikely(base_required % alignment != 0)) {
		return -EINVAL;
	}

	pthread_mutex_lock(&mgr->bo_va_mutex);

	if (!search_from_top) {
		LIST_FOR_EACH_ENTRY_SAFE_REV(hole, n, &mgr->va_holes, list) {
			if (base_required != 0) {
				if (hole->offset > base_required ||
				    (hole->offset + hole->size) < (base_required + size)) {
					continue;
				}
				offset = base_required;
			} else {
				uint64_t waste;

				if (unlikely(__builtin_add_overflow(hole->offset, alignment - 1, &waste))) {
					continue;
				}

				waste = hole->offset % alignment;
				waste = (waste != 0) ? (alignment - waste) : 0;

				if (unlikely(__builtin_add_overflow(hole->offset, waste, &offset))) {
					continue;
				}

				if (offset >= (hole->offset + hole->size)) {
					continue;
				}

				if (size > (hole->offset + hole->size - offset)) {
					continue;
				}
			}

			ret = amdgpu_vamgr_subtract_hole(hole, offset, offset + size);
			pthread_mutex_unlock(&mgr->bo_va_mutex);

			if (unlikely(ret)) {
				return ret;
			}

			*va_out = offset;
			return 0;
		}
	} else {
		LIST_FOR_EACH_ENTRY_SAFE(hole, n, &mgr->va_holes, list) {
			if (base_required != 0) {
				if (hole->offset > base_required ||
				    (hole->offset + hole->size) < (base_required + size)) {
					continue;
				}
				offset = base_required;
			} else {
				if (size > hole->size) {
					continue;
				}

				offset = hole->offset + hole->size - size;
				offset -= offset % alignment;

				if (offset < hole->offset) {
					continue;
				}
			}

			ret = amdgpu_vamgr_subtract_hole(hole, offset, offset + size);
			pthread_mutex_unlock(&mgr->bo_va_mutex);

			if (unlikely(ret)) {
				return ret;
			}

			*va_out = offset;
			return 0;
		}
	}

	pthread_mutex_unlock(&mgr->bo_va_mutex);
	return -ENOMEM;
}

static drm_private void
amdgpu_vamgr_free_va(struct amdgpu_bo_va_mgr *mgr,
		     uint64_t va,
		     uint64_t size)
{
	struct amdgpu_bo_va_hole *hole;
	struct amdgpu_bo_va_hole *next;
	struct amdgpu_bo_va_hole *new_hole;
	bool found_next;

	if (unlikely(!mgr)) {
		return;
	}

	if (va == AMDGPU_INVALID_VA_ADDRESS) {
		return;
	}

	if (unlikely(size == 0)) {
		return;
	}

	size = ALIGN(size, mgr->va_alignment);

	pthread_mutex_lock(&mgr->bo_va_mutex);

	hole = NULL;
	next = NULL;
	found_next = false;

	LIST_FOR_EACH_ENTRY(next, &mgr->va_holes, list) {
		if (next->offset < va) {
			found_next = true;
			break;
		}
		hole = next;
	}

	if (hole != NULL && hole->offset == va + size) {
		hole->offset = va;
		hole->size += size;

		if (found_next && next->offset + next->size == va) {
			next->size += hole->size;
			list_del(&hole->list);
			free(hole);
		}

		pthread_mutex_unlock(&mgr->bo_va_mutex);
		return;
	}

	if (found_next && next->offset + next->size == va) {
		next->size += size;
		pthread_mutex_unlock(&mgr->bo_va_mutex);
		return;
	}

	new_hole = calloc(1, sizeof(struct amdgpu_bo_va_hole));
	if (likely(new_hole != NULL)) {
		new_hole->size = size;
		new_hole->offset = va;

		if (hole != NULL) {
			list_add(&new_hole->list, &hole->list);
		} else {
			list_add(&new_hole->list, &mgr->va_holes);
		}
	}

	pthread_mutex_unlock(&mgr->bo_va_mutex);
}

drm_public int amdgpu_va_range_alloc(amdgpu_device_handle dev,
				     enum amdgpu_gpu_va_range va_range_type,
				     uint64_t size,
				     uint64_t va_base_alignment,
				     uint64_t va_base_required,
				     uint64_t *va_base_allocated,
				     amdgpu_va_handle *va_range_handle,
				     uint64_t flags)
{
	if (unlikely(!dev)) {
		return -EINVAL;
	}

	return amdgpu_va_range_alloc2(&dev->va_mgr, va_range_type, size,
				      va_base_alignment, va_base_required,
				      va_base_allocated, va_range_handle,
				      flags);
}

drm_public int amdgpu_va_range_alloc2(amdgpu_va_manager_handle va_mgr,
				      enum amdgpu_gpu_va_range va_range_type,
				      uint64_t size,
				      uint64_t va_base_alignment,
				      uint64_t va_base_required,
				      uint64_t *va_base_allocated,
				      amdgpu_va_handle *va_range_handle,
				      uint64_t flags)
{
	struct amdgpu_bo_va_mgr *vamgr;
	struct amdgpu_va *va;
	bool search_from_top;
	int ret;

	if (unlikely(!va_mgr || !va_base_allocated || !va_range_handle)) {
		return -EINVAL;
	}

	if (unlikely(size == 0)) {
		return -EINVAL;
	}

	search_from_top = !!(flags & AMDGPU_VA_RANGE_REPLAYABLE);

	if ((flags & AMDGPU_VA_RANGE_HIGH) && !va_mgr->vamgr_high_32.va_max) {
		flags &= ~AMDGPU_VA_RANGE_HIGH;
	}

	if (flags & AMDGPU_VA_RANGE_HIGH) {
		if (flags & AMDGPU_VA_RANGE_32_BIT) {
			vamgr = &va_mgr->vamgr_high_32;
		} else {
			vamgr = &va_mgr->vamgr_high;
		}
	} else {
		if (flags & AMDGPU_VA_RANGE_32_BIT) {
			vamgr = &va_mgr->vamgr_32;
		} else {
			vamgr = &va_mgr->vamgr_low;
		}
	}

	va_base_alignment = MAX2(va_base_alignment, vamgr->va_alignment);
	size = ALIGN(size, vamgr->va_alignment);

	ret = amdgpu_vamgr_find_va(vamgr, size, va_base_alignment,
				   va_base_required, search_from_top,
				   va_base_allocated);

	if (!(flags & AMDGPU_VA_RANGE_32_BIT) && ret) {
		if (flags & AMDGPU_VA_RANGE_HIGH) {
			vamgr = &va_mgr->vamgr_high_32;
		} else {
			vamgr = &va_mgr->vamgr_32;
		}

		ret = amdgpu_vamgr_find_va(vamgr, size, va_base_alignment,
					   va_base_required, search_from_top,
					   va_base_allocated);
	}

	if (unlikely(ret)) {
		return ret;
	}

	va = calloc(1, sizeof(struct amdgpu_va));
	if (unlikely(!va)) {
		amdgpu_vamgr_free_va(vamgr, *va_base_allocated, size);
		return -ENOMEM;
	}

	va->address = *va_base_allocated;
	va->size = size;
	va->range = va_range_type;
	va->vamgr = vamgr;
	*va_range_handle = va;

	return 0;
}

drm_public int amdgpu_va_range_free(amdgpu_va_handle va_range_handle)
{
	if (!va_range_handle) {
		return 0;
	}

	if (!va_range_handle->address) {
		free(va_range_handle);
		return 0;
	}

	if (unlikely(!va_range_handle->vamgr)) {
		free(va_range_handle);
		return -EINVAL;
	}

	amdgpu_vamgr_free_va(va_range_handle->vamgr,
			     va_range_handle->address,
			     va_range_handle->size);

	free(va_range_handle);
	return 0;
}

drm_public uint64_t amdgpu_va_get_start_addr(amdgpu_va_handle va_handle)
{
	if (unlikely(!va_handle)) {
		return 0;
	}

	return va_handle->address;
}

drm_public amdgpu_va_manager_handle amdgpu_va_manager_alloc(void)
{
	amdgpu_va_manager_handle r;

	r = calloc(1, sizeof(struct amdgpu_va_manager));
	return r;
}

drm_public void amdgpu_va_manager_init(struct amdgpu_va_manager *va_mgr,
				       uint64_t low_va_offset,
				       uint64_t low_va_max,
				       uint64_t high_va_offset,
				       uint64_t high_va_max,
				       uint32_t virtual_address_alignment)
{
	uint64_t start;
	uint64_t max;

	if (unlikely(!va_mgr)) {
		return;
	}

	start = low_va_offset;
	max = MIN2(low_va_max, 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_32, start, max,
			  virtual_address_alignment);

	start = max;
	max = MAX2(low_va_max, 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_low, start, max,
			  virtual_address_alignment);

	start = high_va_offset;
	max = MIN2(high_va_max, (start & ~0xffffffffULL) + 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_high_32, start, max,
			  virtual_address_alignment);

	start = max;
	max = MAX2(high_va_max, (start & ~0xffffffffULL) + 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_high, start, max,
			  virtual_address_alignment);
}

drm_public void amdgpu_va_manager_deinit(struct amdgpu_va_manager *va_mgr)
{
	if (unlikely(!va_mgr)) {
		return;
	}

	amdgpu_vamgr_deinit(&va_mgr->vamgr_32);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_low);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_high_32);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_high);
}
