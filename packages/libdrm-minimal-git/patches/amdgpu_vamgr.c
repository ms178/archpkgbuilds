/*
 * Copyright 2014 Advanced Micro Devices, Inc.
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
 */

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include "amdgpu.h"
#include "amdgpu_drm.h"
#include "amdgpu_internal.h"
#include "util_math.h"

#ifndef likely
#define likely(x)   __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif

static drm_private bool amdgpu_is_power_of_two_u64(uint64_t value)
{
	return value != 0 && (value & (value - 1)) == 0;
}

static drm_private uint64_t amdgpu_next_power_of_two_u64(uint64_t value)
{
	if (value <= 1)
		return 1;

	value--;
	value |= value >> 1;
	value |= value >> 2;
	value |= value >> 4;
	value |= value >> 8;
	value |= value >> 16;
	value |= value >> 32;

	if (unlikely(value == UINT64_MAX))
		return 0;

	return value + 1;
}

static drm_private int amdgpu_align_up_u64(uint64_t value,
					    uint64_t alignment,
					    uint64_t *out)
{
	uint64_t tmp;

	if (unlikely(!out || !amdgpu_is_power_of_two_u64(alignment)))
		return -EINVAL;

	if (unlikely(__builtin_add_overflow(value, alignment - 1, &tmp)))
		return -EOVERFLOW;

	*out = tmp & ~(alignment - 1);
	return 0;
}

drm_public int amdgpu_va_range_query(amdgpu_device_handle dev,
				     enum amdgpu_gpu_va_range type,
				     uint64_t *start, uint64_t *end)
{
	if (unlikely(!dev || !start || !end))
		return -EINVAL;

	if (type != amdgpu_gpu_va_range_general)
		return -EINVAL;

	*start = dev->dev_info.virtual_address_offset;
	*end = dev->dev_info.virtual_address_max;
	return 0;
}

drm_private void amdgpu_vamgr_init(struct amdgpu_bo_va_mgr *mgr, uint64_t start,
				   uint64_t max, uint64_t alignment)
{
	struct amdgpu_bo_va_hole *n;

	if (unlikely(!mgr))
		return;

	alignment = amdgpu_next_power_of_two_u64(alignment);
	if (unlikely(!alignment))
		alignment = 1;

	mgr->va_max = 0;
	mgr->va_alignment = alignment;

	list_inithead(&mgr->va_holes);
	pthread_mutex_init(&mgr->bo_va_mutex, NULL);

	if (unlikely(max <= start))
		return;

	n = malloc(sizeof(*n));
	if (unlikely(!n))
		return;

	n->size = max - start;
	n->offset = start;
	list_add(&n->list, &mgr->va_holes);
	mgr->va_max = max;
}

drm_private void amdgpu_vamgr_deinit(struct amdgpu_bo_va_mgr *mgr)
{
	struct amdgpu_bo_va_hole *hole, *tmp;

	if (unlikely(!mgr))
		return;

	LIST_FOR_EACH_ENTRY_SAFE(hole, tmp, &mgr->va_holes, list) {
		list_del(&hole->list);
		free(hole);
	}
	pthread_mutex_destroy(&mgr->bo_va_mutex);
}

static drm_private int
amdgpu_vamgr_subtract_hole(struct amdgpu_bo_va_hole *hole, uint64_t start_va,
			   uint64_t end_va, struct amdgpu_bo_va_hole **spare)
{
	struct amdgpu_bo_va_hole *n;
	uint64_t hole_end;

	if (unlikely(!hole || end_va <= start_va))
		return -EINVAL;

	if (unlikely(__builtin_add_overflow(hole->offset, hole->size, &hole_end)))
		return -EOVERFLOW;

	if (unlikely(start_va < hole->offset || end_va > hole_end))
		return -EINVAL;

	if (start_va > hole->offset && end_va < hole_end) {
		if (unlikely(!spare || !*spare))
			return -ENOMEM;

		n = *spare;
		*spare = NULL;
		n->size = start_va - hole->offset;
		n->offset = hole->offset;
		list_add(&n->list, &hole->list);

		hole->size = hole_end - end_va;
		hole->offset = end_va;
	} else if (start_va > hole->offset) {
		hole->size = start_va - hole->offset;
	} else if (end_va < hole_end) {
		hole->size = hole_end - end_va;
		hole->offset = end_va;
	} else {
		list_del(&hole->list);
		free(hole);
	}

	return 0;
}

static drm_private int
amdgpu_vamgr_find_va(struct amdgpu_bo_va_mgr *mgr, uint64_t size,
		     uint64_t alignment, uint64_t base_required,
		     bool search_from_top, uint64_t *va_out)
{
	struct amdgpu_bo_va_hole *hole, *tmp;
	struct amdgpu_bo_va_hole *spare = NULL;
	uint64_t aligned_size;
	uint64_t required_end = 0;
	uint64_t mgr_alignment;
	uint64_t offset;
	int ret;

	if (unlikely(!mgr || !va_out || size == 0))
		return -EINVAL;

	mgr_alignment = mgr->va_alignment ? mgr->va_alignment : 1;
	if (unlikely(!amdgpu_is_power_of_two_u64(mgr_alignment)))
		return -EINVAL;

	alignment = MAX2(alignment, mgr_alignment);
	if (unlikely(!amdgpu_is_power_of_two_u64(alignment)))
		return -EINVAL;

	ret = amdgpu_align_up_u64(size, mgr_alignment, &aligned_size);
	if (unlikely(ret))
		return ret;

	if (base_required) {
		if (unlikely(base_required & (alignment - 1)))
			return -EINVAL;

		if (unlikely(__builtin_add_overflow(base_required, aligned_size,
							&required_end)))
			return -EOVERFLOW;
	}

retry:
	pthread_mutex_lock(&mgr->bo_va_mutex);
	if (!search_from_top) {
		LIST_FOR_EACH_ENTRY_SAFE_REV(hole, tmp, &mgr->va_holes, list) {
			uint64_t hole_end;
			uint64_t end_va;
			bool needs_spare;

			if (unlikely(__builtin_add_overflow(hole->offset, hole->size,
							&hole_end)))
				continue;

			if (base_required) {
				if (hole->offset > base_required || hole_end < required_end)
					continue;

				offset = base_required;
			} else {
				uint64_t waste = hole->offset & (alignment - 1);

				waste = waste ? alignment - waste : 0;
				if (unlikely(__builtin_add_overflow(hole->offset, waste,
								&offset)))
					continue;

				if (offset >= hole_end || aligned_size > hole_end - offset)
					continue;
			}

			end_va = offset + aligned_size;
			needs_spare = offset > hole->offset && end_va < hole_end;
			if (unlikely(needs_spare && !spare)) {
				pthread_mutex_unlock(&mgr->bo_va_mutex);
				spare = malloc(sizeof(*spare));
				if (unlikely(!spare))
					return -ENOMEM;
				goto retry;
			}

			ret = amdgpu_vamgr_subtract_hole(hole, offset, end_va, &spare);
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (spare)
				free(spare);
			if (unlikely(ret))
				return ret;

			*va_out = offset;
			return 0;
		}
	} else {
		LIST_FOR_EACH_ENTRY_SAFE(hole, tmp, &mgr->va_holes, list) {
			uint64_t hole_end;
			uint64_t end_va;
			bool needs_spare;

			if (unlikely(__builtin_add_overflow(hole->offset, hole->size,
							&hole_end)))
				continue;

			if (base_required) {
				if (hole->offset > base_required || hole_end < required_end)
					continue;

				offset = base_required;
			} else {
				if (aligned_size > hole->size)
					continue;

				offset = (hole_end - aligned_size) & ~(alignment - 1);
				if (offset < hole->offset || aligned_size > hole_end - offset)
					continue;
			}

			end_va = offset + aligned_size;
			needs_spare = offset > hole->offset && end_va < hole_end;
			if (unlikely(needs_spare && !spare)) {
				pthread_mutex_unlock(&mgr->bo_va_mutex);
				spare = malloc(sizeof(*spare));
				if (unlikely(!spare))
					return -ENOMEM;
				goto retry;
			}

			ret = amdgpu_vamgr_subtract_hole(hole, offset, end_va, &spare);
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (spare)
				free(spare);
			if (unlikely(ret))
				return ret;

			*va_out = offset;
			return 0;
		}
	}

	pthread_mutex_unlock(&mgr->bo_va_mutex);
	free(spare);
	return -ENOMEM;
}

static drm_private void
amdgpu_vamgr_free_va(struct amdgpu_bo_va_mgr *mgr, uint64_t va, uint64_t size)
{
	struct amdgpu_bo_va_hole *upper, *lower, *cur;
	struct amdgpu_bo_va_hole *new_hole = NULL;
	uint64_t aligned_size;
	uint64_t lower_end = 0;
	uint64_t upper_size;
	uint64_t va_end;
	bool has_lower;

	if (unlikely(!mgr || size == 0 || va == AMDGPU_INVALID_VA_ADDRESS))
		return;

	if (unlikely(amdgpu_align_up_u64(size,
					mgr->va_alignment ? mgr->va_alignment : 1,
					&aligned_size)))
		return;

	if (unlikely(__builtin_add_overflow(va, aligned_size, &va_end)))
		return;

	if (unlikely(mgr->va_max && va_end > mgr->va_max))
		return;

retry:
	pthread_mutex_lock(&mgr->bo_va_mutex);

	upper = NULL;
	lower = NULL;
	has_lower = false;
	LIST_FOR_EACH_ENTRY(cur, &mgr->va_holes, list) {
		if (cur->offset < va) {
			lower = cur;
			has_lower = true;
			break;
		}
		upper = cur;
	}

	if (has_lower) {
		if (unlikely(__builtin_add_overflow(lower->offset, lower->size,
							&lower_end))) {
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (new_hole)
				if (new_hole)
				free(new_hole);
			return;
		}

		if (unlikely(lower_end > va)) {
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (new_hole)
				if (new_hole)
				free(new_hole);
			return;
		}
	}

	if (upper) {
		if (unlikely(va_end > upper->offset)) {
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (new_hole)
				if (new_hole)
				free(new_hole);
			return;
		}

		if (upper->offset == va_end) {
			if (unlikely(__builtin_add_overflow(upper->size, aligned_size,
								&upper_size))) {
				pthread_mutex_unlock(&mgr->bo_va_mutex);
				free(new_hole);
				return;
			}

			if (has_lower && lower_end == va) {
				uint64_t lower_size;

				if (unlikely(__builtin_add_overflow(lower->size, upper_size,
									&lower_size))) {
					pthread_mutex_unlock(&mgr->bo_va_mutex);
					free(new_hole);
					return;
				}

				lower->size = lower_size;
				list_del(&upper->list);
				free(upper);
			} else {
				upper->offset = va;
				upper->size = upper_size;
			}

			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (new_hole)
				if (new_hole)
				free(new_hole);
			return;
		}
	}

	if (has_lower && lower_end == va) {
		uint64_t lower_size;

		if (unlikely(__builtin_add_overflow(lower->size, aligned_size,
							&lower_size))) {
			pthread_mutex_unlock(&mgr->bo_va_mutex);
			if (new_hole)
				if (new_hole)
				free(new_hole);
			return;
		}

		lower->size = lower_size;
		pthread_mutex_unlock(&mgr->bo_va_mutex);
		if (new_hole)
			free(new_hole);
		return;
	}

	if (unlikely(!new_hole)) {
		pthread_mutex_unlock(&mgr->bo_va_mutex);
		new_hole = malloc(sizeof(*new_hole));
		if (unlikely(!new_hole))
			return;
		goto retry;
	}

	new_hole->size = aligned_size;
	new_hole->offset = va;
	if (upper)
		list_add(&new_hole->list, &upper->list);
	else
		list_add(&new_hole->list, &mgr->va_holes);
	new_hole = NULL;

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
	if (unlikely(!dev))
		return -EINVAL;

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
	bool search_from_top = !!(flags & AMDGPU_VA_RANGE_REPLAYABLE);
	int ret;

	if (unlikely(!va_mgr || !va_base_allocated || !va_range_handle || size == 0))
		return -EINVAL;

	/* Clear the flag when the high VA manager is not initialized */
	if (flags & AMDGPU_VA_RANGE_HIGH && !va_mgr->vamgr_high_32.va_max)
		flags &= ~AMDGPU_VA_RANGE_HIGH;

	if (flags & AMDGPU_VA_RANGE_HIGH) {
		if (flags & AMDGPU_VA_RANGE_32_BIT)
			vamgr = &va_mgr->vamgr_high_32;
		else
			vamgr = &va_mgr->vamgr_high;
	} else {
		if (flags & AMDGPU_VA_RANGE_32_BIT)
			vamgr = &va_mgr->vamgr_32;
		else
			vamgr = &va_mgr->vamgr_low;
	}

	va_base_alignment = MAX2(va_base_alignment, vamgr->va_alignment);
	ret = amdgpu_align_up_u64(size, vamgr->va_alignment, &size);
	if (unlikely(ret))
		return ret;

	ret = amdgpu_vamgr_find_va(vamgr, size,
				   va_base_alignment, va_base_required,
				   search_from_top, va_base_allocated);

	if (!(flags & AMDGPU_VA_RANGE_32_BIT) && ret) {
		/* fallback to 32bit address */
		if (flags & AMDGPU_VA_RANGE_HIGH)
			vamgr = &va_mgr->vamgr_high_32;
		else
			vamgr = &va_mgr->vamgr_32;
		ret = amdgpu_vamgr_find_va(vamgr, size,
					   va_base_alignment, va_base_required,
					   search_from_top, va_base_allocated);
	}

	if (ret)
		return ret;

	va = malloc(sizeof(*va));
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
	if (!va_range_handle)
		return 0;

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
	if (unlikely(!va_handle))
		return 0;

	return va_handle->address;
}

drm_public amdgpu_va_manager_handle amdgpu_va_manager_alloc(void)
{
	amdgpu_va_manager_handle r = calloc(1, sizeof(struct amdgpu_va_manager));
	return r;
}

drm_public void amdgpu_va_manager_init(struct amdgpu_va_manager *va_mgr,
				       uint64_t low_va_offset, uint64_t low_va_max,
				       uint64_t high_va_offset, uint64_t high_va_max,
				       uint32_t virtual_address_alignment)
{
	amdgpu_va_manager_init2(va_mgr, low_va_offset, low_va_max,
				high_va_offset, high_va_max,
				virtual_address_alignment, 0);
}

drm_public void amdgpu_va_manager_init2(struct amdgpu_va_manager *va_mgr,
					uint64_t low_va_offset, uint64_t low_va_max,
					uint64_t high_va_offset, uint64_t high_va_max,
					uint32_t virtual_address_alignment,
					uint32_t flags)
{
	uint64_t start, max;
	uint64_t split_xor;

	if (unlikely(!va_mgr))
		return;

	va_mgr->address_prt_wa_control_bit = ~0u;

	start = low_va_offset;
	max = MIN2(low_va_max, 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_32, start, max,
			  virtual_address_alignment);

	start = max;
	if ((flags & AMDGPU_VA_MGR_RESERVE_HALF_VA_FOR_PRT) && !high_va_max) {
		/* Reserve the half VA range for PRT by splitting it in two
		 * equal halves where one bit controls whether it's the LOW or
		 * HIGH half.
		 */
		split_xor = low_va_offset ^ low_va_max;
		if (split_xor) {
			va_mgr->address_prt_wa_control_bit = util_last_bit64(split_xor) - 1;
			max = low_va_max ^ (1ull << va_mgr->address_prt_wa_control_bit);
		} else {
			max = MAX2(low_va_max, 0x100000000ULL);
		}
	} else {
		max = MAX2(low_va_max, 0x100000000ULL);
	}

	amdgpu_vamgr_init(&va_mgr->vamgr_low, start, max,
			  virtual_address_alignment);

	start = high_va_offset;
	max = MIN2(high_va_max, (start & ~0xffffffffULL) + 0x100000000ULL);
	amdgpu_vamgr_init(&va_mgr->vamgr_high_32, start, max,
			  virtual_address_alignment);

	start = max;
	if ((flags & AMDGPU_VA_MGR_RESERVE_HALF_VA_FOR_PRT) && high_va_max) {
		/* Reserve the half VA range for PRT by splitting it in two
		 * equal halves where one bit controls whether it's the LOW or
		 * HIGH half.
		 */
		split_xor = high_va_offset ^ high_va_max;
		if (split_xor) {
			va_mgr->address_prt_wa_control_bit = util_last_bit64(split_xor) - 1;
			max = high_va_max ^ (1ull << va_mgr->address_prt_wa_control_bit);
		} else {
			max = MAX2(high_va_max, (start & ~0xffffffffULL) + 0x100000000ULL);
		}
	} else {
		max = MAX2(high_va_max, (start & ~0xffffffffULL) + 0x100000000ULL);
	}

	amdgpu_vamgr_init(&va_mgr->vamgr_high, start, max,
			  virtual_address_alignment);
}

drm_public void amdgpu_va_manager_deinit(struct amdgpu_va_manager *va_mgr)
{
	if (unlikely(!va_mgr))
		return;

	amdgpu_vamgr_deinit(&va_mgr->vamgr_32);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_low);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_high_32);
	amdgpu_vamgr_deinit(&va_mgr->vamgr_high);
}

drm_public int amdgpu_va_manager_query_sw_info(struct amdgpu_va_manager *va_mgr,
					       enum amdgpu_va_manager_sw_info info,
					       void *value)
{
	uint32_t *val32 = (uint32_t*)value;

	if (unlikely(!va_mgr || !value))
		return -EINVAL;

	switch (info) {
	case amdgpu_va_manager_sw_info_address_prt_wa_control_bit:
		*val32 = va_mgr->address_prt_wa_control_bit;
		return 0;
	}
	return -EINVAL;
}
