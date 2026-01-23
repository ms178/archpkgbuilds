/* ----------------------------------------------------------------------------
Copyright (c) 2018-2024, Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/
#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/atomic.h"

#include <string.h>  // memset
#include <stdio.h>

/* Forward declaration */
static void mi_segment_try_purge(mi_segment_t* segment, bool force);


/* ---------------------------------------------------------------------------
  Commit Mask Operations
  Optimized for Raptor Lake using hardware bit manipulation (BMI1/BMI2)
--------------------------------------------------------------------------- */

static bool mi_commit_mask_all_set(const mi_commit_mask_t* commit, const mi_commit_mask_t* cm) {
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    if ((commit->mask[i] & cm->mask[i]) != cm->mask[i]) return false;
  }
  return true;
}

static bool mi_commit_mask_any_set(const mi_commit_mask_t* commit, const mi_commit_mask_t* cm) {
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    if ((commit->mask[i] & cm->mask[i]) != 0) return true;
  }
  return false;
}

static void mi_commit_mask_create_intersect(const mi_commit_mask_t* commit, const mi_commit_mask_t* cm, mi_commit_mask_t* res) {
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    res->mask[i] = (commit->mask[i] & cm->mask[i]);
  }
}

static void mi_commit_mask_clear(mi_commit_mask_t* res, const mi_commit_mask_t* cm) {
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    res->mask[i] &= ~(cm->mask[i]);
  }
}

static void mi_commit_mask_set(mi_commit_mask_t* res, const mi_commit_mask_t* cm) {
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    res->mask[i] |= cm->mask[i];
  }
}

static void mi_commit_mask_create(size_t bitidx, size_t bitcount, mi_commit_mask_t* cm) {
  mi_assert_internal(bitidx < MI_COMMIT_MASK_BITS);
  mi_assert_internal((bitidx + bitcount) <= MI_COMMIT_MASK_BITS);
  if (bitcount == MI_COMMIT_MASK_BITS) {
    mi_assert_internal(bitidx == 0);
    mi_commit_mask_create_full(cm);
  }
  else if (bitcount == 0) {
    mi_commit_mask_create_empty(cm);
  }
  else {
    mi_commit_mask_create_empty(cm);
    size_t i = bitidx / MI_COMMIT_MASK_FIELD_BITS;
    size_t ofs = bitidx % MI_COMMIT_MASK_FIELD_BITS;
    while (bitcount > 0) {
      mi_assert_internal(i < MI_COMMIT_MASK_FIELD_COUNT);
      const size_t avail = MI_COMMIT_MASK_FIELD_BITS - ofs;
      const size_t count = (bitcount > avail ? avail : bitcount);
      const size_t mask = (count >= MI_COMMIT_MASK_FIELD_BITS
                           ? ~((size_t)0)
                           : (((size_t)1 << count) - 1) << ofs);
      cm->mask[i] = mask;
      bitcount -= count;
      ofs = 0;
      i++;
    }
  }
}

size_t _mi_commit_mask_committed_size(const mi_commit_mask_t* cm, size_t total) {
  mi_assert_internal((total % MI_COMMIT_MASK_BITS) == 0);
  size_t count = 0;
  for (size_t i = 0; i < MI_COMMIT_MASK_FIELD_COUNT; i++) {
    count += mi_popcount(cm->mask[i]);
  }
  return ((total / MI_COMMIT_MASK_BITS) * count);
}

size_t _mi_commit_mask_next_run(const mi_commit_mask_t* cm, size_t* idx) {
  size_t i = (*idx) / MI_COMMIT_MASK_FIELD_BITS;
  size_t ofs = (*idx) % MI_COMMIT_MASK_FIELD_BITS;
  size_t mask = 0;

  // Find first field with set bit
  while (i < MI_COMMIT_MASK_FIELD_COUNT) {
    mask = cm->mask[i];
    mask >>= ofs;
    if (mask != 0) {
      const size_t trailing = mi_ctz(mask);
      mask >>= trailing;
      ofs += trailing;
      break;
    }
    i++;
    ofs = 0;
  }

  if (i >= MI_COMMIT_MASK_FIELD_COUNT) {
    *idx = MI_COMMIT_MASK_BITS;
    return 0;
  }

  // Found start bit, count consecutive ones
  *idx = (i * MI_COMMIT_MASK_FIELD_BITS) + ofs;
  size_t count = 0;

  // Count trailing ones in the current field
  // ~mask has 0s where mask has 1s
  size_t field_ones = mi_ctz(~mask);
  count += field_ones;

  // If we consumed the entire remainder of this field, check next fields
  if ((ofs + count) == MI_COMMIT_MASK_FIELD_BITS) {
     i++;
     while (i < MI_COMMIT_MASK_FIELD_COUNT && cm->mask[i] == ~((size_t)0)) {
       count += MI_COMMIT_MASK_FIELD_BITS;
       i++;
     }
     if (i < MI_COMMIT_MASK_FIELD_COUNT) {
       count += mi_ctz(~cm->mask[i]);
     }
  }

  mi_assert_internal(count > 0);
  return count;
}


/* --------------------------------------------------------------------------------
  Segment Allocation
-------------------------------------------------------------------------------- */

static const mi_slice_t* mi_segment_slices_end(const mi_segment_t* segment) {
  return &segment->slices[segment->slice_entries];
}

static uint8_t* mi_slice_start(const mi_slice_t* slice) {
  mi_segment_t* segment = _mi_ptr_segment(slice);
  mi_assert_internal(slice >= segment->slices && slice < mi_segment_slices_end(segment));
  return ((uint8_t*)segment + ((slice - segment->slices) * MI_SEGMENT_SLICE_SIZE));
}


/* ---------------------------------------------------------------------------
  Bins
--------------------------------------------------------------------------- */

static inline size_t mi_slice_bin8(size_t slice_count) {
  if (slice_count <= 1) return slice_count;
  mi_assert_internal(slice_count <= MI_SLICES_PER_SEGMENT);
  slice_count--;
  const size_t s = mi_bsr(slice_count);
  if (s <= 2) return slice_count + 1;
  const size_t bin = ((s << 2) | ((slice_count >> (s - 2)) & 0x03)) - 4;
  return bin;
}

static inline size_t mi_slice_bin(size_t slice_count) {
  mi_assert_internal(slice_count * MI_SEGMENT_SLICE_SIZE <= MI_SEGMENT_SIZE);
  mi_assert_internal(mi_slice_bin8(MI_SLICES_PER_SEGMENT) <= MI_SEGMENT_BIN_MAX);
  const size_t bin = mi_slice_bin8(slice_count);
  mi_assert_internal(bin <= MI_SEGMENT_BIN_MAX);
  return bin;
}

static inline size_t mi_slice_index(const mi_slice_t* slice) {
  mi_segment_t* segment = _mi_ptr_segment(slice);
  const ptrdiff_t index = slice - segment->slices;
  mi_assert_internal(index >= 0 && index < (ptrdiff_t)segment->slice_entries);
  return (size_t)index;
}


/* ---------------------------------------------------------------------------
  Slice Span Queues
--------------------------------------------------------------------------- */

static void mi_span_queue_push(mi_span_queue_t* sq, mi_slice_t* slice) {
  mi_assert_internal(slice->prev == NULL && slice->next == NULL);
  slice->prev = NULL;
  slice->next = sq->first;
  sq->first = slice;
  if (slice->next != NULL) {
    slice->next->prev = slice;
  }
  else {
    sq->last = slice;
  }
  slice->block_size = 0;  /* Mark as free */
}

static mi_span_queue_t* mi_span_queue_for(size_t slice_count, mi_segments_tld_t* tld) {
  const size_t bin = mi_slice_bin(slice_count);
  mi_span_queue_t* sq = &tld->spans[bin];
  mi_assert_internal(sq->slice_count >= slice_count);
  return sq;
}

static void mi_span_queue_delete(mi_span_queue_t* sq, mi_slice_t* slice) {
  mi_assert_internal(slice->block_size == 0 && slice->slice_count > 0 && slice->slice_offset == 0);
  if (slice->prev != NULL) slice->prev->next = slice->next;
  if (slice == sq->first) sq->first = slice->next;
  if (slice->next != NULL) slice->next->prev = slice->prev;
  if (slice == sq->last)  sq->last = slice->prev;
  slice->prev = NULL;
  slice->next = NULL;
  slice->block_size = 1;  /* Mark as no longer free */
}


/* ---------------------------------------------------------------------------
  Invariant Checking
--------------------------------------------------------------------------- */

static bool mi_slice_is_used(const mi_slice_t* slice) {
  return (slice->block_size > 0);
}

#if (MI_DEBUG >= 3)
static bool mi_span_queue_contains(mi_span_queue_t* sq, mi_slice_t* slice) {
  for (mi_slice_t* s = sq->first; s != NULL; s = s->next) {
    if (s == slice) return true;
  }
  return false;
}

static bool mi_segment_is_valid(mi_segment_t* segment, mi_segments_tld_t* tld) {
  mi_assert_internal(segment != NULL);
  mi_assert_internal(_mi_ptr_cookie(segment) == segment->cookie);
  mi_assert_internal(segment->abandoned <= segment->used);
  mi_assert_internal(segment->thread_id == 0 || segment->thread_id == _mi_thread_id());
  mi_assert_internal(mi_commit_mask_all_set(&segment->commit_mask, &segment->purge_mask));

  mi_slice_t* slice = &segment->slices[0];
  const mi_slice_t* end = mi_segment_slices_end(segment);
  size_t used_count = 0;
  mi_span_queue_t* sq;

  while (slice < end) {
    mi_assert_internal(slice->slice_count > 0);
    mi_assert_internal(slice->slice_offset == 0);
    const size_t index = mi_slice_index(slice);
    size_t maxindex = (index + slice->slice_count >= segment->slice_entries
                       ? segment->slice_entries
                       : index + slice->slice_count) - 1;

    if (mi_slice_is_used(slice)) {
      used_count++;
      mi_assert_internal(slice->is_huge == (segment->kind == MI_SEGMENT_HUGE));
      for (size_t i = 0; i <= MI_MAX_SLICE_OFFSET_COUNT && index + i <= maxindex; i++) {
        mi_assert_internal(segment->slices[index + i].slice_offset == i * sizeof(mi_slice_t));
        mi_assert_internal(i == 0 || segment->slices[index + i].slice_count == 0);
        mi_assert_internal(i == 0 || segment->slices[index + i].block_size == 1);
      }
      const mi_slice_t* last = slice + slice->slice_count - 1;
      if (last > slice && last < mi_segment_slices_end(segment)) {
        mi_assert_internal(last->slice_offset == (slice->slice_count - 1) * sizeof(mi_slice_t));
        mi_assert_internal(last->slice_count == 0);
        mi_assert_internal(last->block_size == 1);
      }
    }
    else {
      mi_slice_t* last = &segment->slices[maxindex];
      if (segment->kind != MI_SEGMENT_HUGE ||
          slice->slice_count <= (segment->slice_entries - segment->segment_info_slices)) {
        mi_assert_internal((uint8_t*)slice == (uint8_t*)last - last->slice_offset);
      }
      mi_assert_internal(slice == last || last->slice_count == 0);
      mi_assert_internal(last->block_size == 0 ||
                         (segment->kind == MI_SEGMENT_HUGE && last->block_size == 1));
      if (segment->kind != MI_SEGMENT_HUGE && segment->thread_id != 0) {
        sq = mi_span_queue_for(slice->slice_count, tld);
        mi_assert_internal(mi_span_queue_contains(sq, slice));
      }
    }
    slice = &segment->slices[maxindex + 1];
  }
  mi_assert_internal(slice == end);
  mi_assert_internal(used_count == segment->used + 1);
  return true;
}
#endif


/* ---------------------------------------------------------------------------
  Segment Size Calculations
--------------------------------------------------------------------------- */

static size_t mi_segment_info_size(mi_segment_t* segment) {
  return segment->segment_info_slices * MI_SEGMENT_SLICE_SIZE;
}

static uint8_t* _mi_segment_page_start_from_slice(const mi_segment_t* segment,
                                                   const mi_slice_t* slice,
                                                   size_t block_size,
                                                   size_t* page_size)
{
  const ptrdiff_t idx = slice - segment->slices;
  const size_t psize = (size_t)slice->slice_count * MI_SEGMENT_SLICE_SIZE;
  uint8_t* const pstart = (uint8_t*)segment + ((size_t)idx * MI_SEGMENT_SLICE_SIZE);

  size_t start_offset = 0;
  if (block_size > 0 && block_size <= MI_MAX_ALIGN_GUARANTEE) {
    const size_t adjust = block_size - ((uintptr_t)pstart % block_size);
    if (adjust < block_size && psize >= block_size + adjust) {
      start_offset += adjust;
    }
  }
  if (block_size >= MI_INTPTR_SIZE) {
    if (block_size <= 64) {
      start_offset += 3 * block_size;
    }
    else if (block_size <= 512) {
      start_offset += block_size;
    }
  }
  start_offset = _mi_align_up(start_offset, MI_MAX_ALIGN_SIZE);
  mi_assert_internal(_mi_is_aligned(pstart + start_offset, MI_MAX_ALIGN_SIZE));
  mi_assert_internal(block_size == 0 || block_size > MI_MAX_ALIGN_GUARANTEE ||
                     _mi_is_aligned(pstart + start_offset, block_size));
  if (page_size != NULL) {
    *page_size = psize - start_offset;
  }
  return (pstart + start_offset);
}

uint8_t* _mi_segment_page_start(const mi_segment_t* segment, const mi_page_t* page, size_t* page_size) {
  const mi_slice_t* slice = mi_page_to_slice((mi_page_t*)page);
  uint8_t* p = _mi_segment_page_start_from_slice(segment, slice, mi_page_block_size(page), page_size);
  mi_assert_internal(mi_page_block_size(page) > 0 || _mi_ptr_page(p) == page);
  mi_assert_internal(_mi_ptr_segment(p) == segment);
  return p;
}

static size_t mi_segment_calculate_slices(size_t required, size_t* info_slices) {
  const size_t page_size = _mi_os_page_size();
  size_t isize = _mi_align_up(sizeof(mi_segment_t), page_size);
  size_t guardsize = 0;

  if (MI_SECURE > 0) {
    guardsize = page_size;
    if (required > 0) {
      required = _mi_align_up(required, MI_SEGMENT_SLICE_SIZE) + page_size;
    }
  }

  isize = _mi_align_up(isize + guardsize, MI_SEGMENT_SLICE_SIZE);
  if (info_slices != NULL) {
    *info_slices = isize / MI_SEGMENT_SLICE_SIZE;
  }
  const size_t segment_size = (required == 0
                               ? MI_SEGMENT_SIZE
                               : _mi_align_up(required + isize + guardsize, MI_SEGMENT_SLICE_SIZE));
  mi_assert_internal(segment_size % MI_SEGMENT_SLICE_SIZE == 0);
  return (segment_size / MI_SEGMENT_SLICE_SIZE);
}


/* ---------------------------------------------------------------------------
  Segment Cache and Tracking
--------------------------------------------------------------------------- */

static void mi_segments_track_size(long segment_size, mi_segments_tld_t* tld) {
  if (segment_size >= 0) {
    _mi_stat_increase(&tld->stats->segments, 1);
  }
  else {
    _mi_stat_decrease(&tld->stats->segments, 1);
  }
  tld->count += (segment_size >= 0 ? 1 : -1);
  if (tld->count > tld->peak_count) {
    tld->peak_count = tld->count;
  }
  tld->current_size += segment_size;
  if (tld->current_size > tld->peak_size) {
    tld->peak_size = tld->current_size;
  }
}

static void mi_segment_os_free(mi_segment_t* segment, mi_segments_tld_t* tld) {
  segment->thread_id = 0;
  _mi_segment_map_freed_at(segment);
  mi_segments_track_size(-((long)mi_segment_size(segment)), tld);

  if (segment->was_reclaimed) {
    tld->reclaim_count--;
    segment->was_reclaimed = false;
  }

  if (MI_SECURE > 0) {
    const size_t os_pagesize = _mi_os_page_size();
    _mi_os_unprotect((uint8_t*)segment + mi_segment_info_size(segment) - os_pagesize, os_pagesize);
    uint8_t* end = (uint8_t*)segment + mi_segment_size(segment) - os_pagesize;
    _mi_os_unprotect(end, os_pagesize);
  }

  const size_t size = mi_segment_size(segment);
  const size_t csize = _mi_commit_mask_committed_size(&segment->commit_mask, size);
  _mi_arena_free(segment, mi_segment_size(segment), csize, segment->memid);
}


/* ---------------------------------------------------------------------------
  Commit/Decommit Ranges
--------------------------------------------------------------------------- */

static void mi_segment_commit_mask(mi_segment_t* segment, bool conservative,
                                    uint8_t* p, size_t size,
                                    uint8_t** start_p, size_t* full_size,
                                    mi_commit_mask_t* cm)
{
  mi_assert_internal(_mi_ptr_segment(p + 1) == segment);
  mi_assert_internal(segment->kind != MI_SEGMENT_HUGE);
  mi_commit_mask_create_empty(cm);

  if (size == 0 || size > MI_SEGMENT_SIZE || segment->kind == MI_SEGMENT_HUGE) {
    *start_p = NULL;
    *full_size = 0;
    return;
  }

  const size_t segstart = mi_segment_info_size(segment);
  const size_t segsize = mi_segment_size(segment);
  if (p >= (uint8_t*)segment + segsize) {
    *start_p = NULL;
    *full_size = 0;
    return;
  }

  size_t pstart = (size_t)(p - (uint8_t*)segment);
  mi_assert_internal(pstart + size <= segsize);

  size_t start;
  size_t end;
  if (conservative) {
    start = _mi_align_up(pstart, MI_COMMIT_SIZE);
    end   = _mi_align_down(pstart + size, MI_COMMIT_SIZE);
    mi_assert_internal(start >= segstart);
    mi_assert_internal(end <= segsize);
  }
  else {
    start = _mi_align_down(pstart, MI_MINIMAL_COMMIT_SIZE);
    end   = _mi_align_up(pstart + size, MI_MINIMAL_COMMIT_SIZE);
  }

  if (pstart >= segstart && start < segstart) {
    start = segstart;
  }
  if (end > segsize) {
    end = segsize;
  }

  mi_assert_internal(start <= pstart && (pstart + size) <= end);
  mi_assert_internal(start % MI_COMMIT_SIZE == 0 && end % MI_COMMIT_SIZE == 0);
  *start_p   = (uint8_t*)segment + start;
  *full_size = (end > start ? end - start : 0);
  if (*full_size == 0) return;

  const size_t bitidx = start / MI_COMMIT_SIZE;
  mi_assert_internal(bitidx < MI_COMMIT_MASK_BITS);

  const size_t bitcount = *full_size / MI_COMMIT_SIZE;
  if (bitidx + bitcount > MI_COMMIT_MASK_BITS) {
    _mi_warning_message("commit mask overflow: idx=%zu count=%zu start=%zx end=%zx p=0x%p size=%zu fullsize=%zu\n",
                        bitidx, bitcount, start, end, p, size, *full_size);
  }
  mi_assert_internal((bitidx + bitcount) <= MI_COMMIT_MASK_BITS);
  mi_commit_mask_create(bitidx, bitcount, cm);
}

static bool mi_segment_commit(mi_segment_t* segment, uint8_t* p, size_t size) {
  mi_assert_internal(mi_commit_mask_all_set(&segment->commit_mask, &segment->purge_mask));

  uint8_t* start = NULL;
  size_t   full_size = 0;
  mi_commit_mask_t mask;
  mi_segment_commit_mask(segment, false /* conservative? */, p, size, &start, &full_size, &mask);

  if (mi_commit_mask_is_empty(&mask) || full_size == 0) {
    return true;
  }

  if (!mi_commit_mask_all_set(&segment->commit_mask, &mask)) {
    bool is_zero = false;
    mi_commit_mask_t cmask;
    mi_commit_mask_create_intersect(&segment->commit_mask, &mask, &cmask);
    _mi_stat_decrease(&_mi_stats_main.committed,
                      _mi_commit_mask_committed_size(&cmask, MI_SEGMENT_SIZE));
    if (!_mi_os_commit(start, full_size, &is_zero)) {
      return false;
    }
    mi_commit_mask_set(&segment->commit_mask, &mask);
  }

  if (mi_commit_mask_any_set(&segment->purge_mask, &mask)) {
    segment->purge_expire = _mi_clock_now() + mi_option_get(mi_option_purge_delay);
  }

  mi_commit_mask_clear(&segment->purge_mask, &mask);
  return true;
}

static bool mi_segment_ensure_committed(mi_segment_t* segment, uint8_t* p, size_t size) {
  mi_assert_internal(mi_commit_mask_all_set(&segment->commit_mask, &segment->purge_mask));
  if (mi_likely(mi_commit_mask_is_full(&segment->commit_mask) &&
                mi_commit_mask_is_empty(&segment->purge_mask))) {
    return true;
  }
  mi_assert_internal(segment->kind != MI_SEGMENT_HUGE);
  return mi_segment_commit(segment, p, size);
}

static bool mi_segment_purge(mi_segment_t* segment, uint8_t* p, size_t size) {
  mi_assert_internal(mi_commit_mask_all_set(&segment->commit_mask, &segment->purge_mask));
  if (!segment->allow_purge) return true;

  uint8_t* start = NULL;
  size_t   full_size = 0;
  mi_commit_mask_t mask;
  mi_segment_commit_mask(segment, true /* conservative? */, p, size, &start, &full_size, &mask);

  if (mi_commit_mask_is_empty(&mask) || full_size == 0) {
    return true;
  }

  if (mi_commit_mask_any_set(&segment->commit_mask, &mask)) {
    mi_assert_internal((void*)start != (void*)segment);
    mi_assert_internal(segment->allow_decommit);
    const bool decommitted = _mi_os_purge(start, full_size);
    if (decommitted) {
      mi_commit_mask_t cmask;
      mi_commit_mask_create_intersect(&segment->commit_mask, &mask, &cmask);
      _mi_stat_increase(&_mi_stats_main.committed,
                        full_size - _mi_commit_mask_committed_size(&cmask, MI_SEGMENT_SIZE));
      mi_commit_mask_clear(&segment->commit_mask, &mask);
    }
  }

  mi_commit_mask_clear(&segment->purge_mask, &mask);
  return true;
}

static void mi_segment_schedule_purge(mi_segment_t* segment, uint8_t* p, size_t size) {
  if (!segment->allow_purge) return;

  if (mi_option_get(mi_option_purge_delay) == 0) {
    mi_segment_purge(segment, p, size);
  }
  else {
    uint8_t* start = NULL;
    size_t   full_size = 0;
    mi_commit_mask_t mask;
    mi_segment_commit_mask(segment, true /* conservative */, p, size, &start, &full_size, &mask);

    if (mi_commit_mask_is_empty(&mask) || full_size == 0) {
      return;
    }

    mi_assert_internal(segment->purge_expire > 0 || mi_commit_mask_is_empty(&segment->purge_mask));
    mi_commit_mask_t cmask;
    mi_commit_mask_create_intersect(&segment->commit_mask, &mask, &cmask);
    mi_commit_mask_set(&segment->purge_mask, &cmask);

    const mi_msecs_t now = _mi_clock_now();
    if (segment->purge_expire == 0) {
      segment->purge_expire = now + mi_option_get(mi_option_purge_delay);
    }
    else if (segment->purge_expire <= now) {
      if (segment->purge_expire + mi_option_get(mi_option_purge_extend_delay) <= now) {
        mi_segment_try_purge(segment, true);
      }
      else {
        segment->purge_expire = now + mi_option_get(mi_option_purge_extend_delay);
      }
    }
    else {
      segment->purge_expire += mi_option_get(mi_option_purge_extend_delay);
    }
  }
}

static void mi_segment_try_purge(mi_segment_t* segment, bool force) {
  if (!segment->allow_purge || segment->purge_expire == 0 ||
      mi_commit_mask_is_empty(&segment->purge_mask)) {
    return;
  }

  const mi_msecs_t now = _mi_clock_now();
  if (!force && now < segment->purge_expire) {
    return;
  }

  mi_commit_mask_t mask = segment->purge_mask;
  segment->purge_expire = 0;
  mi_commit_mask_create_empty(&segment->purge_mask);

  size_t idx;
  size_t count;
  mi_commit_mask_foreach(&mask, idx, count) {
    if (count > 0) {
      uint8_t* p = (uint8_t*)segment + (idx * MI_COMMIT_SIZE);
      const size_t psize = count * MI_COMMIT_SIZE;
      mi_segment_purge(segment, p, psize);
    }
  }
  mi_commit_mask_foreach_end()
  mi_assert_internal(mi_commit_mask_is_empty(&segment->purge_mask));
}

void _mi_segment_collect(mi_segment_t* segment, bool force) {
  mi_segment_try_purge(segment, force);
}


/* ---------------------------------------------------------------------------
  Span Free
--------------------------------------------------------------------------- */

static bool mi_segment_is_abandoned(mi_segment_t* segment) {
  return (mi_atomic_load_relaxed(&segment->thread_id) == 0);
}

static void mi_segment_span_free(mi_segment_t* segment, size_t slice_index, size_t slice_count,
                                  bool allow_purge, mi_segments_tld_t* tld)
{
  mi_assert_internal(slice_index < segment->slice_entries);
  mi_span_queue_t* sq = (segment->kind == MI_SEGMENT_HUGE || mi_segment_is_abandoned(segment)
                          ? NULL
                          : mi_span_queue_for(slice_count, tld));
  if (slice_count == 0) slice_count = 1;
  mi_assert_internal(slice_index + slice_count - 1 < segment->slice_entries);

  mi_slice_t* slice = &segment->slices[slice_index];
  slice->slice_count = (uint32_t)slice_count;
  mi_assert_internal(slice->slice_count == slice_count);
  slice->slice_offset = 0;

  if (slice_count > 1) {
    mi_slice_t* last = slice + slice_count - 1;
    mi_slice_t* end  = (mi_slice_t*)mi_segment_slices_end(segment);
    if (last > end) {
      last = end;
    }
    if (last > slice) {
      last->slice_count = 0;
      last->slice_offset = (uint32_t)(sizeof(mi_page_t) * (slice_count - 1));
      last->block_size = 0;
    }
  }

  if (allow_purge) {
    mi_segment_schedule_purge(segment, mi_slice_start(slice), slice_count * MI_SEGMENT_SLICE_SIZE);
  }

  if (sq != NULL) {
    mi_span_queue_push(sq, slice);
  }
  else {
    slice->block_size = 0;  /* Mark huge page as free */
  }
}

static void mi_segment_span_remove_from_queue(mi_slice_t* slice, mi_segments_tld_t* tld) {
  mi_assert_internal(slice->slice_count > 0 && slice->slice_offset == 0 && slice->block_size == 0);
  mi_assert_internal(_mi_ptr_segment(slice)->kind != MI_SEGMENT_HUGE);
  mi_span_queue_t* sq = mi_span_queue_for(slice->slice_count, tld);
  mi_span_queue_delete(sq, slice);
}

static mi_slice_t* mi_segment_span_free_coalesce(mi_slice_t* slice, mi_segments_tld_t* tld) {
  mi_assert_internal(slice != NULL && slice->slice_count > 0 && slice->slice_offset == 0);
  mi_segment_t* const segment = _mi_ptr_segment(slice);

  if (mi_unlikely(segment->kind == MI_SEGMENT_HUGE)) {
    mi_assert_internal((segment->used == 0 && slice->block_size == 0) || segment->used == 1);
    slice->block_size = 0;
    return slice;
  }

  const bool is_abandoned = (segment->thread_id == 0);
  size_t slice_count = slice->slice_count;
  mi_slice_t* next = slice + slice->slice_count;
  mi_assert_internal(next <= mi_segment_slices_end(segment));

  if (next < mi_segment_slices_end(segment) && next->block_size == 0) {
    mi_assert_internal(next->slice_count > 0 && next->slice_offset == 0);
    slice_count += next->slice_count;
    if (!is_abandoned) {
      mi_segment_span_remove_from_queue(next, tld);
    }
  }

  if (slice > segment->slices) {
    mi_slice_t* prev = mi_slice_first(slice - 1);
    mi_assert_internal(prev >= segment->slices);
    if (prev->block_size == 0) {
      mi_assert_internal(prev->slice_count > 0 && prev->slice_offset == 0);
      slice_count += prev->slice_count;
      slice->slice_count = 0;
      slice->slice_offset = (uint32_t)((uint8_t*)slice - (uint8_t*)prev);
      if (!is_abandoned) {
        mi_segment_span_remove_from_queue(prev, tld);
      }
      slice = prev;
    }
  }

  mi_segment_span_free(segment, mi_slice_index(slice), slice_count, true, tld);
  return slice;
}


/* ---------------------------------------------------------------------------
  Page Allocation
--------------------------------------------------------------------------- */

static mi_page_t* mi_segment_span_allocate(mi_segment_t* segment, size_t slice_index, size_t slice_count) {
  mi_assert_internal(slice_index < segment->slice_entries);
  mi_slice_t* const slice = &segment->slices[slice_index];
  mi_assert_internal(slice->block_size == 0 || slice->block_size == 1);

  if (!mi_segment_ensure_committed(segment,
                                    _mi_segment_page_start_from_slice(segment, slice, 0, NULL),
                                    slice_count * MI_SEGMENT_SLICE_SIZE)) {
    return NULL;
  }

  slice->slice_offset = 0;
  slice->slice_count = (uint32_t)slice_count;
  mi_assert_internal(slice->slice_count == slice_count);
  const size_t bsize = slice_count * MI_SEGMENT_SLICE_SIZE;
  slice->block_size = bsize;
  mi_page_t* page = mi_slice_to_page(slice);
  mi_assert_internal(mi_page_block_size(page) == bsize);

  size_t extra = slice_count - 1;
  if (extra > MI_MAX_SLICE_OFFSET_COUNT) {
    extra = MI_MAX_SLICE_OFFSET_COUNT;
  }
  if (slice_index + extra >= segment->slice_entries) {
    extra = segment->slice_entries - slice_index - 1;
  }

  mi_slice_t* slice_next = slice + 1;
  for (size_t i = 1; i <= extra; i++, slice_next++) {
    slice_next->slice_offset = (uint32_t)(sizeof(mi_slice_t) * i);
    slice_next->slice_count = 0;
    slice_next->block_size = 1;
  }

  mi_slice_t* last = slice + slice_count - 1;
  mi_slice_t* end = (mi_slice_t*)mi_segment_slices_end(segment);
  if (last > end) last = end;
  if (last > slice) {
    last->slice_offset = (uint32_t)(sizeof(mi_slice_t) * (size_t)(last - slice));
    last->slice_count = 0;
    last->block_size = 1;
  }

  page->is_committed = true;
  page->is_zero_init = segment->free_is_zero;
  page->is_huge = (segment->kind == MI_SEGMENT_HUGE);
  segment->used++;
  return page;
}

static void mi_segment_slice_split(mi_segment_t* segment, mi_slice_t* slice,
                                    size_t slice_count, mi_segments_tld_t* tld)
{
  mi_assert_internal(_mi_ptr_segment(slice) == segment);
  mi_assert_internal(slice->slice_count >= slice_count);
  mi_assert_internal(slice->block_size > 0);

  if (slice->slice_count <= slice_count) return;
  mi_assert_internal(segment->kind != MI_SEGMENT_HUGE);

  const size_t next_index = mi_slice_index(slice) + slice_count;
  const size_t next_count = slice->slice_count - slice_count;
  mi_segment_span_free(segment, next_index, next_count, false /* don't purge left-over */, tld);
  slice->slice_count = (uint32_t)slice_count;
}

static mi_page_t* mi_segments_page_find_and_allocate(size_t slice_count, mi_arena_id_t req_arena_id,
                                                      mi_segments_tld_t* tld)
{
  mi_assert_internal(slice_count * MI_SEGMENT_SLICE_SIZE <= MI_LARGE_OBJ_SIZE_MAX);

  mi_span_queue_t* sq = mi_span_queue_for(slice_count, tld);
  if (slice_count == 0) slice_count = 1;

  while (sq <= &tld->spans[MI_SEGMENT_BIN_MAX]) {
    for (mi_slice_t* slice = sq->first; slice != NULL; slice = slice->next) {
      if (mi_likely(slice->slice_count >= slice_count)) {
        mi_segment_t* segment = _mi_ptr_segment(slice);
        if (mi_likely(_mi_arena_memid_is_suitable(segment->memid, req_arena_id))) {
          mi_span_queue_delete(sq, slice);

          if (slice->slice_count > slice_count) {
            mi_segment_slice_split(segment, slice, slice_count, tld);
          }
          mi_assert_internal(slice != NULL && slice->slice_count == slice_count && slice->block_size > 0);

          mi_page_t* page = mi_segment_span_allocate(segment, mi_slice_index(slice), slice->slice_count);
          if (mi_unlikely(page == NULL)) {
            mi_segment_span_free_coalesce(slice, tld);
            return NULL;
          }
          return page;
        }
      }
    }
    sq++;
  }
  return NULL;
}

static mi_segment_t* mi_segment_os_alloc(size_t required, size_t page_alignment,
                                          bool eager_delayed, mi_arena_id_t req_arena_id,
                                          size_t* psegment_slices, size_t* pinfo_slices,
                                          bool commit, mi_segments_tld_t* tld)
{
  mi_memid_t memid;
  bool allow_large = (!eager_delayed && (MI_SECURE == 0));
  size_t align_offset = 0;
  size_t alignment = MI_SEGMENT_ALIGN;

  if (page_alignment > 0) {
    /*
      CRITICAL FIX: Force segment alignment to be at least MI_SEGMENT_ALIGN (32 MiB).
      If the user requests small alignment for a huge allocation (e.g., 8 bytes),
      we must still align the segment base to 32 MiB for _mi_ptr_segment() to work.
      But we must NOT set align_offset for small alignments, or the payload will
      be pushed 32MB into the segment, causing slice index overflow.
    */
    alignment = (page_alignment > MI_SEGMENT_ALIGN) ? page_alignment : MI_SEGMENT_ALIGN;

    if (page_alignment > MI_SEGMENT_ALIGN) {
        // Large alignment: use OS to align payload via align_offset
        const size_t info_size = (*pinfo_slices) * MI_SEGMENT_SLICE_SIZE;
        align_offset = _mi_align_up(info_size, alignment);
        const size_t extra = align_offset - info_size;
        *psegment_slices = mi_segment_calculate_slices(required + extra, pinfo_slices);
    } else {
        // Small/Normal alignment (<= 32 MiB):
        // Ensure segment is 32 MiB aligned (done by alignment = 32MB).
        // align_offset = 0 keeps the payload close to the header.
        const size_t info_size = (*pinfo_slices) * MI_SEGMENT_SLICE_SIZE;
        const size_t aligned_info = _mi_align_up(info_size, page_alignment);
        const size_t extra = aligned_info - info_size;
        *psegment_slices = mi_segment_calculate_slices(required + extra, pinfo_slices);
        align_offset = 0;
    }

    mi_assert_internal(*psegment_slices > 0 && *psegment_slices <= UINT32_MAX);
  }

  const size_t segment_size = (*psegment_slices) * MI_SEGMENT_SLICE_SIZE;
  mi_segment_t* segment = (mi_segment_t*)_mi_arena_alloc_aligned(
      segment_size, alignment, align_offset, commit, allow_large, req_arena_id, &memid);

  if (segment == NULL) {
    return NULL;
  }

  mi_commit_mask_t commit_mask;
  if (memid.initially_committed) {
    mi_commit_mask_create_full(&commit_mask);
  }
  else {
    const size_t commit_needed = _mi_divide_up((*pinfo_slices) * MI_SEGMENT_SLICE_SIZE, MI_COMMIT_SIZE);
    mi_assert_internal(commit_needed > 0);
    mi_commit_mask_create(0, commit_needed, &commit_mask);
    mi_assert_internal(commit_needed * MI_COMMIT_SIZE >= (*pinfo_slices) * MI_SEGMENT_SLICE_SIZE);
    if (!_mi_os_commit(segment, commit_needed * MI_COMMIT_SIZE, NULL)) {
      _mi_arena_free(segment, segment_size, 0, memid);
      return NULL;
    }
  }

  mi_assert_internal(segment != NULL && (uintptr_t)segment % MI_SEGMENT_ALIGN == 0);

  segment->memid = memid;
  segment->allow_decommit = !memid.is_pinned;
  segment->allow_purge = segment->allow_decommit && (mi_option_get(mi_option_purge_delay) >= 0);
  segment->segment_size = segment_size;
  segment->subproc = tld->subproc;
  segment->commit_mask = commit_mask;
  segment->purge_expire = 0;
  segment->free_is_zero = memid.initially_zero;
  mi_commit_mask_create_empty(&segment->purge_mask);

  mi_segments_track_size((long)(segment_size), tld);
  _mi_segment_map_allocated_at(segment);
  return segment;
}

static mi_segment_t* mi_segment_alloc(size_t required, size_t page_alignment,
                                       mi_arena_id_t req_arena_id,
                                       mi_segments_tld_t* tld, mi_page_t** huge_page)
{
  mi_assert_internal((required == 0 && huge_page == NULL) || (required > 0 && huge_page != NULL));

  size_t info_slices;
  size_t segment_slices = mi_segment_calculate_slices(required, &info_slices);
  mi_assert_internal(segment_slices > 0 && segment_slices <= UINT32_MAX);

  const bool eager_delay = (_mi_current_thread_count() > 1 &&
                            tld->peak_count < (size_t)mi_option_get(mi_option_eager_commit_delay));
  const bool eager = !eager_delay && mi_option_is_enabled(mi_option_eager_commit);
  bool commit = eager || (required > 0);

  mi_segment_t* segment = mi_segment_os_alloc(required, page_alignment, eager_delay, req_arena_id,
                                               &segment_slices, &info_slices, commit, tld);
  if (segment == NULL) return NULL;

  if (!segment->memid.initially_zero) {
    const ptrdiff_t ofs = offsetof(mi_segment_t, next);
    const size_t prefix = offsetof(mi_segment_t, slices) - (size_t)ofs;
    const size_t zsize = prefix + (sizeof(mi_slice_t) * (segment_slices + 1));
    _mi_memzero((uint8_t*)segment + ofs, zsize);
  }

  const size_t slice_entries = (segment_slices > MI_SLICES_PER_SEGMENT
                                ? MI_SLICES_PER_SEGMENT
                                : segment_slices);
  segment->segment_slices = segment_slices;
  segment->segment_info_slices = info_slices;
  segment->thread_id = _mi_thread_id();
  segment->cookie = _mi_ptr_cookie(segment);
  segment->slice_entries = slice_entries;
  segment->kind = (required == 0 ? MI_SEGMENT_NORMAL : MI_SEGMENT_HUGE);

  _mi_stat_increase(&tld->stats->page_committed, mi_segment_info_size(segment));

  size_t guard_slices = 0;
  if (MI_SECURE > 0) {
    const size_t os_pagesize = _mi_os_page_size();
    _mi_os_protect((uint8_t*)segment + mi_segment_info_size(segment) - os_pagesize, os_pagesize);
    uint8_t* end = (uint8_t*)segment + mi_segment_size(segment) - os_pagesize;
    mi_segment_ensure_committed(segment, end, os_pagesize);
    _mi_os_protect(end, os_pagesize);
    if (slice_entries == segment_slices) {
      segment->slice_entries--;
    }
    guard_slices = 1;
  }

  mi_page_t* page0 = mi_segment_span_allocate(segment, 0, info_slices);
  mi_assert_internal(page0 != NULL);
  if (page0 == NULL) return NULL;
  mi_assert_internal(segment->used == 1);
  segment->used = 0;

  if (segment->kind == MI_SEGMENT_NORMAL) {
    mi_assert_internal(huge_page == NULL);
    mi_segment_span_free(segment, info_slices, segment->slice_entries - info_slices, false, tld);
  }
  else {
    mi_assert_internal(huge_page != NULL);
    mi_assert_internal(mi_commit_mask_is_empty(&segment->purge_mask));
    mi_assert_internal(mi_commit_mask_is_full(&segment->commit_mask));
    *huge_page = mi_segment_span_allocate(segment, info_slices,
                                           segment_slices - info_slices - guard_slices);
    mi_assert_internal(*huge_page != NULL);
  }

  mi_assert_expensive(mi_segment_is_valid(segment, tld));
  return segment;
}

static void mi_segment_free(mi_segment_t* segment, bool force, mi_segments_tld_t* tld) {
  MI_UNUSED(force);
  mi_assert_internal(segment != NULL);
  mi_assert_internal(segment->next == NULL);
  mi_assert_internal(segment->used == 0);

  if (segment->dont_free) return;

  mi_slice_t* slice = &segment->slices[0];
  const mi_slice_t* end = mi_segment_slices_end(segment);
  #if MI_DEBUG > 1
  size_t page_count = 0;
  #endif

  while (slice < end) {
    mi_assert_internal(slice->slice_count > 0);
    mi_assert_internal(slice->slice_offset == 0);
    mi_assert_internal(mi_slice_index(slice) == 0 || slice->block_size == 0);
    if (slice->block_size == 0 && segment->kind != MI_SEGMENT_HUGE) {
      mi_segment_span_remove_from_queue(slice, tld);
    }
    #if MI_DEBUG > 1
    page_count++;
    #endif
    slice = slice + slice->slice_count;
  }
  mi_assert_internal(page_count == 2);

  mi_segment_os_free(segment, tld);
}


/* ---------------------------------------------------------------------------
  Page Free
--------------------------------------------------------------------------- */

static void mi_segment_abandon(mi_segment_t* segment, mi_segments_tld_t* tld);

static mi_slice_t* mi_segment_page_clear(mi_page_t* page, mi_segments_tld_t* tld) {
  mi_assert_internal(page->block_size > 0);
  mi_assert_internal(mi_page_all_free(page));
  mi_segment_t* segment = _mi_ptr_segment(page);
  mi_assert_internal(segment->used > 0);

  const size_t inuse = page->capacity * mi_page_block_size(page);
  _mi_stat_decrease(&tld->stats->page_committed, inuse);
  _mi_stat_decrease(&tld->stats->pages, 1);
  _mi_stat_decrease(&tld->stats->page_bins[_mi_page_stats_bin(page)], 1);

  if (segment->allow_decommit && mi_option_is_enabled(mi_option_deprecated_page_reset)) {
    size_t psize;
    uint8_t* start = _mi_segment_page_start(segment, page, &psize);
    _mi_os_reset(start, psize);
  }

  page->is_zero_init = false;
  const uint8_t heap_tag = page->heap_tag;
  const ptrdiff_t ofs = offsetof(mi_page_t, capacity);
  _mi_memzero((uint8_t*)page + ofs, sizeof(*page) - (size_t)ofs);
  page->block_size = 1;
  page->heap_tag = heap_tag;

  mi_slice_t* slice = mi_segment_span_free_coalesce(mi_page_to_slice(page), tld);
  segment->used--;
  segment->free_is_zero = false;

  return slice;
}

void _mi_segment_page_free(mi_page_t* page, bool force, mi_segments_tld_t* tld) {
  mi_assert(page != NULL);
  mi_segment_t* segment = _mi_page_segment(page);
  mi_assert_expensive(mi_segment_is_valid(segment, tld));

  mi_segment_page_clear(page, tld);
  mi_assert_expensive(mi_segment_is_valid(segment, tld));

  if (segment->used == 0) {
    mi_segment_free(segment, force, tld);
  }
  else if (segment->used == segment->abandoned) {
    mi_segment_abandon(segment, tld);
  }
  else {
    mi_segment_try_purge(segment, false);
  }
}


/* ---------------------------------------------------------------------------
  Abandonment
--------------------------------------------------------------------------- */

static void mi_segment_abandon(mi_segment_t* segment, mi_segments_tld_t* tld) {
  mi_assert_internal(segment->used == segment->abandoned);
  mi_assert_internal(segment->used > 0);
  mi_assert_internal(segment->abandoned_visits == 0);
  mi_assert_expensive(mi_segment_is_valid(segment, tld));

  mi_slice_t* slice = &segment->slices[0];
  const mi_slice_t* end = mi_segment_slices_end(segment);
  while (slice < end) {
    mi_assert_internal(slice->slice_count > 0);
    mi_assert_internal(slice->slice_offset == 0);
    if (slice->block_size == 0) {
      mi_segment_span_remove_from_queue(slice, tld);
      slice->block_size = 0;  /* Keep marked as free */
    }
    slice = slice + slice->slice_count;
  }

  const bool force_purge = (segment->memid.memkind != MI_MEM_ARENA) ||
                           mi_option_is_enabled(mi_option_abandoned_page_purge);
  mi_segment_try_purge(segment, force_purge);

  _mi_stat_increase(&tld->stats->segments_abandoned, 1);
  mi_segments_track_size(-((long)mi_segment_size(segment)), tld);
  segment->thread_id = 0;
  segment->abandoned_visits = 1;
  if (segment->was_reclaimed) {
    tld->reclaim_count--;
    segment->was_reclaimed = false;
  }
  _mi_arena_segment_mark_abandoned(segment);
}

void _mi_segment_page_abandon(mi_page_t* page, mi_segments_tld_t* tld) {
  mi_assert(page != NULL);
  mi_assert_internal(mi_page_thread_free_flag(page) == MI_NEVER_DELAYED_FREE);
  mi_assert_internal(mi_page_heap(page) == NULL);
  mi_segment_t* segment = _mi_page_segment(page);

  mi_assert_expensive(mi_segment_is_valid(segment, tld));
  segment->abandoned++;

  _mi_stat_increase(&tld->stats->pages_abandoned, 1);
  mi_assert_internal(segment->abandoned <= segment->used);
  if (segment->used == segment->abandoned) {
    mi_segment_abandon(segment, tld);
  }
}


/* ---------------------------------------------------------------------------
  Reclaim Abandoned Segments
--------------------------------------------------------------------------- */

static mi_slice_t* mi_slices_start_iterate(mi_segment_t* segment, const mi_slice_t** end) {
  mi_slice_t* slice = &segment->slices[0];
  *end = mi_segment_slices_end(segment);
  mi_assert_internal(slice->slice_count > 0 && slice->block_size > 0);
  slice = slice + slice->slice_count;  /* Skip segment info page */
  return slice;
}

static bool mi_segment_check_free(mi_segment_t* segment, size_t slices_needed,
                                   size_t block_size, mi_segments_tld_t* tld)
{
  mi_assert_internal(mi_segment_is_abandoned(segment));
  bool has_page = false;

  const mi_slice_t* end;
  mi_slice_t* slice = mi_slices_start_iterate(segment, &end);
  while (slice < end) {
    mi_assert_internal(slice->slice_count > 0);
    mi_assert_internal(slice->slice_offset == 0);
    if (mi_slice_is_used(slice)) {
      mi_page_t* const page = mi_slice_to_page(slice);
      _mi_page_free_collect(page, false);
      if (mi_page_all_free(page)) {
        mi_assert_internal(page->next == NULL && page->prev == NULL);
        _mi_stat_decrease(&tld->stats->pages_abandoned, 1);
        segment->abandoned--;
        slice = mi_segment_page_clear(page, tld);
        mi_assert_internal(!mi_slice_is_used(slice));
        if (slice->slice_count >= slices_needed) {
          has_page = true;
        }
      }
      else if (mi_page_block_size(page) == block_size && mi_page_has_any_available(page)) {
        has_page = true;
      }
    }
    else {
      if (slice->slice_count >= slices_needed) {
        has_page = true;
      }
    }
    slice = slice + slice->slice_count;
  }
  return has_page;
}

static mi_segment_t* mi_segment_reclaim(mi_segment_t* segment, mi_heap_t* heap,
                                         size_t requested_block_size,
                                         bool* right_page_reclaimed,
                                         mi_segments_tld_t* tld)
{
  if (right_page_reclaimed != NULL) {
    *right_page_reclaimed = false;
  }

  mi_assert_internal(mi_atomic_load_relaxed(&segment->thread_id) == 0 ||
                     mi_atomic_load_relaxed(&segment->thread_id) == _mi_thread_id());
  mi_assert_internal(segment->subproc == heap->tld->segments.subproc);

  mi_atomic_store_release(&segment->thread_id, _mi_thread_id());
  segment->abandoned_visits = 0;
  segment->was_reclaimed = true;
  tld->reclaim_count++;
  mi_segments_track_size((long)mi_segment_size(segment), tld);
  mi_assert_internal(segment->next == NULL);
  _mi_stat_decrease(&tld->stats->segments_abandoned, 1);

  const mi_slice_t* end;
  mi_slice_t* slice = mi_slices_start_iterate(segment, &end);
  while (slice < end) {
    mi_assert_internal(slice->slice_count > 0);
    mi_assert_internal(slice->slice_offset == 0);
    if (mi_slice_is_used(slice)) {
      mi_page_t* page = mi_slice_to_page(slice);
      mi_assert_internal(page->is_committed);
      mi_assert_internal(mi_page_thread_free_flag(page) == MI_NEVER_DELAYED_FREE);
      mi_assert_internal(mi_page_heap(page) == NULL);
      mi_assert_internal(page->next == NULL && page->prev == NULL);
      _mi_stat_decrease(&tld->stats->pages_abandoned, 1);
      segment->abandoned--;

      mi_heap_t* target_heap = _mi_heap_by_tag(heap, page->heap_tag);
      if (target_heap == NULL) {
        target_heap = heap;
        _mi_error_message(EFAULT, "page with tag %u cannot be reclaimed by a heap with the same tag (using heap tag %u instead)\n",
                          page->heap_tag, heap->tag);
      }

      mi_page_set_heap(page, target_heap);
      _mi_page_use_delayed_free(page, MI_USE_DELAYED_FREE, true);
      _mi_page_free_collect(page, false);

      if (mi_page_all_free(page)) {
        slice = mi_segment_page_clear(page, tld);
      }
      else {
        _mi_page_reclaim(target_heap, page);
        if (requested_block_size == mi_page_block_size(page) &&
            mi_page_has_any_available(page) && heap == target_heap) {
          if (right_page_reclaimed != NULL) {
            *right_page_reclaimed = true;
          }
        }
      }
    }
    else {
      slice = mi_segment_span_free_coalesce(slice, tld);
    }
    mi_assert_internal(slice->slice_count > 0 && slice->slice_offset == 0);
    slice = slice + slice->slice_count;
  }

  mi_assert(segment->abandoned == 0);
  mi_assert_expensive(mi_segment_is_valid(segment, tld));

  if (segment->used == 0) {
    mi_assert_internal(right_page_reclaimed == NULL || !(*right_page_reclaimed));
    mi_segment_free(segment, false, tld);
    return NULL;
  }
  return segment;
}

bool _mi_segment_attempt_reclaim(mi_heap_t* heap, mi_segment_t* segment) {
  if (mi_atomic_load_relaxed(&segment->thread_id) != 0) return false;
  if (segment->subproc != heap->tld->segments.subproc) return false;
  if (!_mi_heap_memid_is_suitable(heap, segment->memid)) return false;

  const long target = _mi_option_get_fast(mi_option_target_segments_per_thread);
  if (target > 0 && (size_t)target <= heap->tld->segments.count) return false;

  if (segment->memid.memkind == MI_MEM_ARENA &&
      heap->tld->segments.reclaim_count * 2 > heap->tld->segments.count) {
    return false;
  }

  if (_mi_arena_segment_clear_abandoned(segment)) {
    mi_segment_t* res = mi_segment_reclaim(segment, heap, 0, NULL, &heap->tld->segments);
    mi_assert_internal(res == segment);
    return (res != NULL);
  }
  return false;
}

void _mi_abandoned_reclaim_all(mi_heap_t* heap, mi_segments_tld_t* tld) {
  mi_segment_t* segment;
  mi_arena_field_cursor_t current;
  _mi_arena_field_cursor_init(heap, tld->subproc, true, &current);
  while ((segment = _mi_arena_segment_clear_abandoned_next(&current)) != NULL) {
    mi_segment_reclaim(segment, heap, 0, NULL, tld);
  }
  _mi_arena_field_cursor_done(&current);
}

static bool segment_count_is_within_target(mi_segments_tld_t* tld, size_t* ptarget) {
    const size_t target = (size_t)mi_option_get_clamp(mi_option_target_segments_per_thread, 0, 1024);
    if (ptarget != NULL) {
        *ptarget = target;
    }
    return (target == 0 || tld->count < target);
}

static long mi_segment_get_reclaim_tries(mi_segments_tld_t* tld) {
    const size_t perc = (size_t)mi_option_get_clamp(mi_option_max_segment_reclaim, 0, 100);
    if (perc == 0) return 0;

    const size_t total_count = mi_atomic_load_relaxed(&tld->subproc->abandoned_count);
    if (total_count == 0) return 0;

    size_t relative_count;
    if (total_count > 10000) {
        relative_count = (total_count / 100) * perc;
    }
    else {
        relative_count = (total_count * perc) / 100;
    }

    long max_tries;
    if (relative_count <= 1) {
        max_tries = 1;
    }
    else if (relative_count > 1024) {
        max_tries = 1024;
    }
    else {
        max_tries = (long)relative_count;
    }

    if (max_tries < 8 && total_count > 8) {
        max_tries = 8;
    }

    return max_tries;
}


static mi_segment_t* mi_segment_try_reclaim(mi_heap_t* heap, size_t needed_slices,
                                             size_t block_size, bool* reclaimed,
                                             mi_segments_tld_t* tld)
{
    *reclaimed = false;
    long max_tries = mi_segment_get_reclaim_tries(tld);
    if (max_tries <= 0) {
        return NULL;
    }

    mi_segment_t* result = NULL;
    mi_segment_t* segment = NULL;
    mi_arena_field_cursor_t current;

    _mi_arena_field_cursor_init(heap, tld->subproc, false /* non-blocking */, &current);

    while (segment_count_is_within_target(tld, NULL) &&
           (max_tries-- > 0) &&
           ((segment = _mi_arena_segment_clear_abandoned_next(&current)) != NULL))
    {
        mi_assert(segment->subproc == heap->tld->segments.subproc);
        segment->abandoned_visits++;

        const bool is_suitable = _mi_heap_memid_is_suitable(heap, segment->memid);
        const bool has_page = mi_segment_check_free(segment, needed_slices, block_size, tld);

        if (segment->used == 0) {
            mi_segment_reclaim(segment, heap, 0, NULL, tld);
        }
        else if (has_page && is_suitable) {
            result = mi_segment_reclaim(segment, heap, block_size, reclaimed, tld);
            break;
        }
        else if (segment->abandoned_visits > 3 && is_suitable) {
            mi_segment_reclaim(segment, heap, 0, NULL, tld);
        }
        else {
            if (!is_suitable) {
                max_tries++;
            }
            mi_segment_try_purge(segment, false);
            _mi_arena_segment_mark_abandoned(segment);
        }
    }

    _mi_arena_field_cursor_done(&current);
    return result;
}


void _mi_abandoned_collect(mi_heap_t* heap, bool force, mi_segments_tld_t* tld) {
    mi_segment_t* segment;
    mi_arena_field_cursor_t current;

    _mi_arena_field_cursor_init(heap, tld->subproc, force /* blocking? */, &current);

    long max_tries = force
        ? (long)mi_atomic_load_relaxed(&tld->subproc->abandoned_count)
        : 1024;

    while ((max_tries-- > 0) &&
           ((segment = _mi_arena_segment_clear_abandoned_next(&current)) != NULL))
    {
        mi_segment_check_free(segment, 0, 0, tld);

        if (segment->used == 0) {
            mi_segment_reclaim(segment, heap, 0, NULL, tld);
        }
        else {
            mi_segment_try_purge(segment, force);
            _mi_arena_segment_mark_abandoned(segment);
        }
    }

    _mi_arena_field_cursor_done(&current);
}


static void mi_segment_force_abandon(mi_segment_t* segment, mi_segments_tld_t* tld) {
    mi_assert_internal(!mi_segment_is_abandoned(segment));
    mi_assert_internal(!segment->dont_free);

    segment->dont_free = true;

    const mi_slice_t* end;
    mi_slice_t* slice = mi_slices_start_iterate(segment, &end);

    while (slice < end) {
        mi_assert_internal(slice->slice_count > 0);
        mi_assert_internal(slice->slice_offset == 0);

        if (mi_slice_is_used(slice)) {
            mi_page_t* const page = mi_slice_to_page(slice);
            _mi_page_free_collect(page, false);

            mi_assert_internal(segment->used > 0);

            if (segment->used == segment->abandoned + 1) {
                segment->dont_free = false;
                _mi_page_force_abandon(page);
                return;
            }
            else {
                _mi_page_force_abandon(page);
                slice = mi_slice_first(slice);
            }
        }

        slice = slice + slice->slice_count;
    }

    segment->dont_free = false;

    mi_assert_internal(segment->used == segment->abandoned);

    if (segment->used == 0) {
        mi_segment_free(segment, false, tld);
    }
    else {
        mi_segment_try_purge(segment, false);
    }
}

static void mi_segments_try_abandon_to_target(mi_heap_t* heap, size_t target,
                                               mi_segments_tld_t* tld)
{
    if (target <= 1) return;

    const size_t min_target = (target > 4) ? ((target * 3) / 4) : target;

    for (int i = 0; i < 64 && tld->count >= min_target; i++) {
        mi_page_t* page = heap->pages[MI_BIN_FULL].first;
        while (page != NULL && mi_page_block_size(page) > MI_LARGE_OBJ_SIZE_MAX) {
            page = page->next;
        }

        if (page == NULL) break;

        mi_segment_t* segment = _mi_page_segment(page);
        mi_segment_force_abandon(segment, tld);
        mi_assert_internal(page != heap->pages[MI_BIN_FULL].first);
    }
}

static void mi_segments_try_abandon(mi_heap_t* heap, mi_segments_tld_t* tld) {
    size_t target = 0;
    if (segment_count_is_within_target(tld, &target)) {
        return;
    }
    mi_segments_try_abandon_to_target(heap, target, tld);
}

void mi_collect_reduce(size_t target_size) mi_attr_noexcept {
    mi_collect(true);

    mi_heap_t* heap = mi_heap_get_default();
    if (mi_unlikely(heap == NULL)) return;

    mi_segments_tld_t* tld = &heap->tld->segments;
    size_t target;
    if (target_size == 0) {
        target = (size_t)mi_option_get_clamp(mi_option_target_segments_per_thread, 1, 1024);
    }
    else {
        target = target_size / MI_SEGMENT_SIZE;
        if (target == 0) target = 1;
    }

    mi_segments_try_abandon_to_target(heap, target, tld);
}

static mi_segment_t* mi_segment_reclaim_or_alloc(mi_heap_t* heap, size_t needed_slices,
                                                  size_t block_size, mi_segments_tld_t* tld)
{
    mi_assert_internal(block_size <= MI_LARGE_OBJ_SIZE_MAX);

    mi_segments_try_abandon(heap, tld);

    bool reclaimed;
    mi_segment_t* segment = mi_segment_try_reclaim(heap, needed_slices, block_size, &reclaimed, tld);

    if (reclaimed) {
        mi_assert_internal(segment != NULL);
        return NULL;
    }

    if (segment != NULL) {
        return segment;
    }

    return mi_segment_alloc(0, 0, heap->arena_id, tld, NULL);
}

static mi_page_t* mi_segments_page_alloc(mi_heap_t* heap, mi_page_kind_t page_kind,
                                          size_t required, size_t block_size,
                                          mi_segments_tld_t* tld)
{
    mi_assert_internal(required <= MI_LARGE_OBJ_SIZE_MAX);
    mi_assert_internal(page_kind <= MI_PAGE_LARGE);

    const size_t page_size = _mi_align_up(required,
        (required > MI_MEDIUM_PAGE_SIZE ? MI_MEDIUM_PAGE_SIZE : MI_SEGMENT_SLICE_SIZE));
    const size_t slices_needed = page_size / MI_SEGMENT_SLICE_SIZE;
    mi_assert_internal(slices_needed * MI_SEGMENT_SLICE_SIZE == page_size);

    mi_page_t* page = mi_segments_page_find_and_allocate(slices_needed, heap->arena_id, tld);

    if (page == NULL) {
        if (mi_segment_reclaim_or_alloc(heap, slices_needed, block_size, tld) == NULL) {
            return NULL;
        }
        return mi_segments_page_alloc(heap, page_kind, required, block_size, tld);
    }

    mi_assert_internal(page != NULL);
    mi_assert_internal(page->slice_count * MI_SEGMENT_SLICE_SIZE == page_size);
    mi_assert_internal(_mi_ptr_segment(page)->thread_id == _mi_thread_id());

    mi_segment_try_purge(_mi_ptr_segment(page), false);
    return page;
}

static mi_page_t* mi_segment_huge_page_alloc(size_t size, size_t page_alignment,
                                              mi_arena_id_t req_arena_id,
                                              mi_segments_tld_t* tld)
{
    mi_page_t* page = NULL;
    mi_segment_t* segment = mi_segment_alloc(size, page_alignment, req_arena_id, tld, &page);

    if (segment == NULL || page == NULL) {
        return NULL;
    }

    mi_assert_internal(segment->used == 1);
    mi_assert_internal(mi_page_block_size(page) >= size);

#if MI_HUGE_PAGE_ABANDON
    segment->thread_id = 0;
#endif

    size_t psize;
    uint8_t* start = _mi_segment_page_start(segment, page, &psize);
    page->block_size = psize;
    mi_assert_internal(page->is_huge);

    if (page_alignment > 0 && segment->allow_decommit) {
        uint8_t* aligned_p = (uint8_t*)_mi_align_up((uintptr_t)start, page_alignment);
        mi_assert_internal(_mi_is_aligned(aligned_p, page_alignment));
        mi_assert_internal(psize >= (size_t)(aligned_p - start) + size);

        uint8_t* decommit_start = start + sizeof(mi_block_t);
        const ptrdiff_t decommit_size_signed = aligned_p - decommit_start;

        if (decommit_size_signed > 0) {
            const size_t decommit_size = (size_t)decommit_size_signed;
            _mi_os_reset(decommit_start, decommit_size);
        }
    }

    return page;
}


#if MI_HUGE_PAGE_ABANDON

void _mi_segment_huge_page_free(mi_segment_t* segment, mi_page_t* page, mi_block_t* block) {
    mi_assert_internal(segment->kind == MI_SEGMENT_HUGE);
    mi_assert_internal(segment == _mi_page_segment(page));
    mi_assert_internal(mi_atomic_load_relaxed(&segment->thread_id) == 0);

    mi_heap_t* heap = mi_heap_get_default();
    if (mi_unlikely(heap == NULL)) {
        return;
    }

    size_t expected_tid = 0;
    if (mi_atomic_cas_strong_acq_rel(&segment->thread_id, &expected_tid, heap->thread_id)) {
        mi_block_set_next(page, block, page->free);
        page->free = block;
        page->used--;
        page->is_zero_init = false;
        mi_assert_internal(page->used == 0);

        mi_tld_t* tld = heap->tld;
        _mi_segment_page_free(page, true, &tld->segments);
    }
}

#else

void _mi_segment_huge_page_reset(mi_segment_t* segment, mi_page_t* page, mi_block_t* block) {
    mi_assert_internal(segment->kind == MI_SEGMENT_HUGE);
    mi_assert_internal(segment == _mi_page_segment(page));
    mi_assert_internal(page->used == 1);
    mi_assert_internal(page->free == NULL);

    MI_UNUSED(page);

    if (segment->allow_decommit) {
        const size_t csize = mi_usable_size(block);
        if (csize > sizeof(mi_block_t)) {
            const size_t reset_size = csize - sizeof(mi_block_t);
            uint8_t* p = (uint8_t*)block + sizeof(mi_block_t);
            _mi_os_reset(p, reset_size);
        }
    }
}
#endif

mi_page_t* _mi_segment_page_alloc(mi_heap_t* heap, size_t block_size,
                                   size_t page_alignment, mi_segments_tld_t* tld)
{
    mi_page_t* page;

    if (mi_unlikely(page_alignment > MI_BLOCK_ALIGNMENT_MAX)) {
        mi_assert_internal(_mi_is_power_of_two(page_alignment));
        if (page_alignment < MI_SEGMENT_SIZE) {
            page_alignment = MI_SEGMENT_SIZE;
        }
        page = mi_segment_huge_page_alloc(block_size, page_alignment, heap->arena_id, tld);
    }
    else if (block_size <= MI_SMALL_OBJ_SIZE_MAX) {
        page = mi_segments_page_alloc(heap, MI_PAGE_SMALL, block_size, block_size, tld);
    }
    else if (block_size <= MI_MEDIUM_OBJ_SIZE_MAX) {
        page = mi_segments_page_alloc(heap, MI_PAGE_MEDIUM, MI_MEDIUM_PAGE_SIZE, block_size, tld);
    }
    else if (block_size <= MI_LARGE_OBJ_SIZE_MAX) {
        page = mi_segments_page_alloc(heap, MI_PAGE_LARGE, block_size, block_size, tld);
    }
    else {
        page = mi_segment_huge_page_alloc(block_size, page_alignment, heap->arena_id, tld);
    }

    mi_assert_internal(page == NULL ||
                       _mi_heap_memid_is_suitable(heap, _mi_page_segment(page)->memid));
    mi_assert_expensive(page == NULL ||
                        mi_segment_is_valid(_mi_page_segment(page), tld));
    mi_assert_internal(page == NULL ||
                       _mi_page_segment(page)->subproc == tld->subproc);

    return page;
}

static bool mi_segment_visit_page(mi_page_t* page, bool visit_blocks,
                                   mi_block_visit_fun* visitor, void* arg)
{
    mi_heap_area_t area;
    _mi_heap_area_init(&area, page);
    if (!visitor(NULL, &area, NULL, area.block_size, arg)) {
        return false;
    }
    if (visit_blocks) {
        return _mi_heap_area_visit_blocks(&area, page, visitor, arg);
    }
    return true;
}

bool _mi_segment_visit_blocks(mi_segment_t* segment, int heap_tag, bool visit_blocks,
                               mi_block_visit_fun* visitor, void* arg)
{
    const mi_slice_t* end;
    mi_slice_t* slice = mi_slices_start_iterate(segment, &end);

    while (slice < end) {
        if (mi_slice_is_used(slice)) {
            mi_page_t* const page = mi_slice_to_page(slice);
            if (heap_tag < 0 || (int)page->heap_tag == heap_tag) {
                if (!mi_segment_visit_page(page, visit_blocks, visitor, arg)) {
                    return false;
                }
            }
        }
        slice = slice + slice->slice_count;
    }

    return true;
}
