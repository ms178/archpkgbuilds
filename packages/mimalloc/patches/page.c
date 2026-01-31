/*----------------------------------------------------------------------------
Copyright (c) 2018-2024, Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/

#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/atomic.h"

#if defined(__GNUC__) || defined(__clang__)
  #if defined(__x86_64__) || defined(__i386__)
    #define mi_spin_pause() __builtin_ia32_pause()
  #elif defined(__aarch64__)
    #define mi_spin_pause() __asm__ volatile("yield" ::: "memory")
  #else
    #define mi_spin_pause() ((void)0)
  #endif
  #define mi_prefetch_r(addr)  __builtin_prefetch((addr), 0, 3)
  #define mi_prefetch_w(addr)  __builtin_prefetch((addr), 1, 3)
  #define mi_prefetch_r_l2(addr) __builtin_prefetch((addr), 0, 2)
#else
  #define mi_spin_pause() ((void)0)
  #define mi_prefetch_r(addr) ((void)(addr))
  #define mi_prefetch_w(addr) ((void)(addr))
  #define mi_prefetch_r_l2(addr) ((void)(addr))
#endif

#define MI_IN_PAGE_C
#include "page-queue.c"
#undef MI_IN_PAGE_C

static inline mi_block_t* mi_page_block_at(const mi_page_t* page, void* page_start, size_t block_size, size_t i) {
  MI_UNUSED(page);
  mi_assert_internal(page != NULL);
  mi_assert_internal(i <= page->reserved);
  return (mi_block_t*)((uint8_t*)page_start + (i * block_size));
}

static void mi_page_init(mi_heap_t* heap, mi_page_t* page, size_t size, mi_tld_t* tld);
static bool mi_page_extend_free(mi_heap_t* heap, mi_page_t* page, mi_tld_t* tld);

#if (MI_DEBUG>=3)
static size_t mi_page_list_count(mi_page_t* page, mi_block_t* head) {
  size_t count = 0;
  while (head != NULL) {
    mi_assert_internal(page == _mi_ptr_page(head));
    count++;
    head = mi_block_next(page, head);
  }
  return count;
}

static bool mi_page_list_is_valid(mi_page_t* page, mi_block_t* p) {
  size_t psize;
  uint8_t* page_area = _mi_segment_page_start(_mi_page_segment(page), page, &psize);
  mi_block_t* start = (mi_block_t*)page_area;
  mi_block_t* end   = (mi_block_t*)(page_area + psize);
  while (p != NULL) {
    if (p < start || p >= end) return false;
    p = mi_block_next(page, p);
  }
#if MI_DEBUG>3
  if (page->free_is_zero) {
    const size_t ubsize = mi_page_usable_block_size(page);
    for (mi_block_t* block = page->free; block != NULL; block = mi_block_next(page, block)) {
      mi_assert_expensive(mi_mem_is_zero(block + 1, ubsize - sizeof(mi_block_t)));
    }
  }
#endif
  return true;
}

static bool mi_page_is_valid_init(mi_page_t* page) {
  mi_assert_internal(mi_page_block_size(page) > 0);
  mi_assert_internal(page->used <= page->capacity);
  mi_assert_internal(page->capacity <= page->reserved);

  uint8_t* start = mi_page_start(page);
  mi_assert_internal(start == _mi_segment_page_start(_mi_page_segment(page), page, NULL));
  mi_assert_internal(page->is_huge == (_mi_page_segment(page)->kind == MI_SEGMENT_HUGE));

  mi_assert_internal(mi_page_list_is_valid(page, page->free));
  mi_assert_internal(mi_page_list_is_valid(page, page->local_free));

#if MI_DEBUG>3
  if (page->free_is_zero) {
    const size_t ubsize = mi_page_usable_block_size(page);
    for (mi_block_t* block = page->free; block != NULL; block = mi_block_next(page, block)) {
      mi_assert_expensive(mi_mem_is_zero(block + 1, ubsize - sizeof(mi_block_t)));
    }
  }
#endif

#if !MI_TRACK_ENABLED && !MI_TSAN
  mi_block_t* tfree = mi_page_thread_free(page);
  mi_assert_internal(mi_page_list_is_valid(page, tfree));
#endif

  size_t free_count = mi_page_list_count(page, page->free) + mi_page_list_count(page, page->local_free);
  mi_assert_internal(page->used + free_count == page->capacity);

  return true;
}

extern mi_decl_hidden bool _mi_process_is_initialized;

bool _mi_page_is_valid(mi_page_t* page) {
  mi_assert_internal(mi_page_is_valid_init(page));
#if MI_SECURE
  mi_assert_internal(page->keys[0] != 0);
#endif
  if (mi_page_heap(page) != NULL) {
    mi_segment_t* segment = _mi_page_segment(page);

    mi_assert_internal(!_mi_process_is_initialized || segment->thread_id == 0 || segment->thread_id == mi_page_heap(page)->thread_id);
#if MI_HUGE_PAGE_ABANDON
    if (segment->kind != MI_SEGMENT_HUGE)
#endif
    {
      mi_page_queue_t* pq = mi_page_queue_of(page);
      mi_assert_internal(mi_page_queue_contains(pq, page));
      mi_assert_internal(pq->block_size == mi_page_block_size(page) || mi_page_block_size(page) > MI_MEDIUM_OBJ_SIZE_MAX || mi_page_is_in_full(page));
      mi_assert_internal(mi_heap_contains_queue(mi_page_heap(page), pq));
    }
  }
  return true;
}
#endif

void _mi_page_use_delayed_free(mi_page_t* page, mi_delayed_t delay, bool override_never) {
  while (!_mi_page_try_use_delayed_free(page, delay, override_never)) {
    mi_spin_pause();
    mi_atomic_yield();
  }
}

bool _mi_page_try_use_delayed_free(mi_page_t* page, mi_delayed_t delay, bool override_never) {
  mi_thread_free_t tfreex;
  mi_delayed_t old_delay;
  mi_thread_free_t tfree;
  size_t yield_count = 0;

  do {
    tfree = mi_atomic_load_acquire(&page->xthread_free);
    tfreex = mi_tf_set_delayed(tfree, delay);
    old_delay = mi_tf_delayed(tfree);

    if mi_unlikely(old_delay == MI_DELAYED_FREEING) {
      if (yield_count >= 4) return false;
      yield_count++;
      mi_spin_pause();
      mi_atomic_yield();
    }
    else if (delay == old_delay) {
      break;
    }
    else if (!override_never && old_delay == MI_NEVER_DELAYED_FREE) {
      break;
    }
  } while ((old_delay == MI_DELAYED_FREEING) ||
           !mi_atomic_cas_weak_release(&page->xthread_free, &tfree, tfreex));

  return true;
}

static void _mi_page_thread_free_collect(mi_page_t* page) {
  mi_block_t* head;
  mi_thread_free_t tfreex;
  mi_thread_free_t tfree = mi_atomic_load_relaxed(&page->xthread_free);
  size_t spin_count = 0;

  do {
    head = mi_tf_block(tfree);
    tfreex = mi_tf_set_block(tfree, NULL);
    if mi_unlikely(!mi_atomic_cas_weak_acq_rel(&page->xthread_free, &tfree, tfreex)) {
      if (++spin_count <= 8) {
        mi_spin_pause();
      } else {
        mi_atomic_yield();
        spin_count = 0;
      }
      continue;
    }
    break;
  } while (true);

  if (head == NULL) return;

  mi_prefetch_r(head);

  size_t max_count = page->capacity;
  size_t count = 1;
  mi_block_t* tail = head;
  mi_block_t* next;

  while ((next = mi_block_next(page, tail)) != NULL && count <= max_count) {
    mi_prefetch_r(next);
    count++;
    tail = next;
  }

  if (count > max_count) {
    _mi_error_message(EFAULT, "corrupted thread-free list\n");
    return;
  }

  mi_block_set_next(page, tail, page->local_free);
  page->local_free = head;
  page->used -= (uint16_t)count;
}

void _mi_page_free_collect(mi_page_t* page, bool force) {
  mi_assert_internal(page != NULL);

  if (force || mi_page_thread_free(page) != NULL) {
    _mi_page_thread_free_collect(page);
  }

  if (page->local_free != NULL) {
    if mi_likely(page->free == NULL) {
      page->free = page->local_free;
      page->local_free = NULL;
      page->free_is_zero = false;
    }
    else if (force) {
      mi_block_t* tail = page->local_free;
      mi_block_t* next;
      while ((next = mi_block_next(page, tail)) != NULL) {
        tail = next;
      }
      mi_block_set_next(page, tail, page->free);
      page->free = page->local_free;
      page->local_free = NULL;
      page->free_is_zero = false;
    }
  }

  mi_assert_internal(!force || page->local_free == NULL);
}

void _mi_page_reclaim(mi_heap_t* heap, mi_page_t* page) {
  mi_assert_expensive(mi_page_is_valid_init(page));
  mi_assert_internal(mi_page_heap(page) == heap);
  mi_assert_internal(mi_page_thread_free_flag(page) != MI_NEVER_DELAYED_FREE);
#if MI_HUGE_PAGE_ABANDON
  mi_assert_internal(_mi_page_segment(page)->kind != MI_SEGMENT_HUGE);
#endif

  mi_page_queue_t* pq = mi_page_queue(heap, mi_page_block_size(page));
  mi_page_queue_push(heap, pq, page);
  mi_assert_expensive(_mi_page_is_valid(page));
}

static mi_page_t* mi_page_fresh_alloc(mi_heap_t* heap, mi_page_queue_t* pq, size_t block_size, size_t page_alignment) {
#if !MI_HUGE_PAGE_ABANDON
  mi_assert_internal(pq != NULL);
  mi_assert_internal(mi_heap_contains_queue(heap, pq));
  mi_assert_internal(page_alignment > 0 || block_size > MI_MEDIUM_OBJ_SIZE_MAX || block_size == pq->block_size);
#endif
  mi_page_t* page = _mi_segment_page_alloc(heap, block_size, page_alignment, &heap->tld->segments);
  if mi_unlikely(page == NULL) {
    return NULL;
  }
#if MI_HUGE_PAGE_ABANDON
  mi_assert_internal(pq == NULL || _mi_page_segment(page)->page_kind != MI_PAGE_HUGE);
#endif
  mi_assert_internal(page_alignment > 0 || block_size > MI_MEDIUM_OBJ_SIZE_MAX || _mi_page_segment(page)->kind != MI_SEGMENT_HUGE);
  mi_assert_internal(pq != NULL || mi_page_block_size(page) >= block_size);

  const size_t full_block_size = (pq == NULL || mi_page_is_huge(page) ? mi_page_block_size(page) : block_size);
  mi_assert_internal(full_block_size >= block_size);
  mi_page_init(heap, page, full_block_size, heap->tld);
  mi_heap_stat_increase(heap, pages, 1);
  mi_heap_stat_increase(heap, page_bins[_mi_page_stats_bin(page)], 1);
  if (pq != NULL) {
    mi_page_queue_push(heap, pq, page);
  }
  mi_assert_expensive(_mi_page_is_valid(page));
  return page;
}

static mi_page_t* mi_page_fresh(mi_heap_t* heap, mi_page_queue_t* pq) {
  mi_assert_internal(mi_heap_contains_queue(heap, pq));
  mi_page_t* page = mi_page_fresh_alloc(heap, pq, pq->block_size, 0);
  if mi_unlikely(page == NULL) return NULL;
  mi_assert_internal(pq->block_size == mi_page_block_size(page));
  mi_assert_internal(pq == mi_page_queue(heap, mi_page_block_size(page)));
  return page;
}

void _mi_heap_delayed_free_all(mi_heap_t* heap) {
  while (!_mi_heap_delayed_free_partial(heap)) {
    mi_spin_pause();
    mi_atomic_yield();
  }
}

bool _mi_heap_delayed_free_partial(mi_heap_t* heap) {
  mi_block_t* block = mi_atomic_load_ptr_relaxed(mi_block_t, &heap->thread_delayed_free);

  if mi_likely(block == NULL) {
    return true;
  }

  size_t spin_count = 0;
  while (!mi_atomic_cas_ptr_weak_acq_rel(mi_block_t, &heap->thread_delayed_free, &block, NULL)) {
    if (block == NULL) {
      return true;
    }
    if (++spin_count <= 8) {
      mi_spin_pause();
    } else {
      mi_atomic_yield();
      spin_count = 0;
    }
  }

  bool all_freed = true;

  mi_block_t* prefetch_block = block;
  for (int i = 0; i < 4 && prefetch_block != NULL; i++) {
    mi_prefetch_r(prefetch_block);
    prefetch_block = mi_block_nextx(heap, prefetch_block, heap->keys);
  }

  while (block != NULL) {
    mi_block_t* next = mi_block_nextx(heap, block, heap->keys);

    if mi_likely(next != NULL) {
      mi_prefetch_r(next);
    }

    if mi_likely(_mi_free_delayed_block(block)) {
    }
    else {
      all_freed = false;
      mi_block_t* dfree = mi_atomic_load_ptr_relaxed(mi_block_t, &heap->thread_delayed_free);
      do {
        mi_block_set_nextx(heap, block, dfree, heap->keys);
      } while (!mi_atomic_cas_ptr_weak_release(mi_block_t, &heap->thread_delayed_free, &dfree, block));
    }
    block = next;
  }
  return all_freed;
}

void _mi_page_unfull(mi_page_t* page) {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(mi_page_is_in_full(page));
  if (!mi_page_is_in_full(page)) return;

  mi_heap_t* heap = mi_page_heap(page);
  mi_page_queue_t* pqfull = &heap->pages[MI_BIN_FULL];
  mi_page_set_in_full(page, false);
  mi_page_queue_t* pq = mi_heap_page_queue_of(heap, page);
  mi_page_set_in_full(page, true);
  mi_page_queue_enqueue_from_full(pq, pqfull, page);
}

static void mi_page_to_full(mi_page_t* page, mi_page_queue_t* pq) {
  mi_assert_internal(pq == mi_page_queue_of(page));
  mi_assert_internal(!mi_page_immediate_available(page));
  mi_assert_internal(!mi_page_is_in_full(page));

  if (mi_page_is_in_full(page)) return;
  mi_page_queue_enqueue_from(&mi_page_heap(page)->pages[MI_BIN_FULL], pq, page);
  _mi_page_free_collect(page, false);
}

void _mi_page_abandon(mi_page_t* page, mi_page_queue_t* pq) {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(pq == mi_page_queue_of(page));
  mi_assert_internal(mi_page_heap(page) != NULL);

  mi_heap_t* pheap = mi_page_heap(page);

  mi_segments_tld_t* segments_tld = &pheap->tld->segments;
  mi_page_queue_remove(pq, page);

  mi_assert_internal(mi_page_thread_free_flag(page) == MI_NEVER_DELAYED_FREE);
  mi_page_set_heap(page, NULL);

#if (MI_DEBUG>1) && !MI_TRACK_ENABLED
  for (mi_block_t* block = (mi_block_t*)pheap->thread_delayed_free; block != NULL; block = mi_block_nextx(pheap, block, pheap->keys)) {
    mi_assert_internal(_mi_ptr_page(block) != page);
  }
#endif

  mi_assert_internal(mi_page_heap(page) == NULL);
  _mi_segment_page_abandon(page, segments_tld);
}

void _mi_page_force_abandon(mi_page_t* page) {
  mi_heap_t* heap = mi_page_heap(page);
  _mi_page_use_delayed_free(page, MI_NEVER_DELAYED_FREE, false);
  _mi_heap_delayed_free_all(heap);

  if (page->capacity == 0) return;

  mi_page_queue_t* pq = mi_heap_page_queue_of(heap, page);
  if (mi_page_all_free(page)) {
    _mi_page_free(page, pq, false);
  }
  else {
    _mi_page_abandon(page, pq);
  }
}

void _mi_page_free(mi_page_t* page, mi_page_queue_t* pq, bool force) {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(pq == mi_page_queue_of(page));
  mi_assert_internal(mi_page_all_free(page));
  mi_assert_internal(mi_page_thread_free_flag(page) != MI_DELAYED_FREEING);

  mi_page_set_has_aligned(page, false);

  mi_heap_t* heap = mi_page_heap(page);
  mi_segments_tld_t* segments_tld = &heap->tld->segments;
  mi_page_queue_remove(pq, page);

  mi_page_set_heap(page, NULL);
  _mi_segment_page_free(page, force, segments_tld);
}

#define MI_MAX_RETIRE_SIZE    MI_MEDIUM_OBJ_SIZE_MAX
#define MI_RETIRE_CYCLES      (16)

void _mi_page_retire(mi_page_t* page) mi_attr_noexcept {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(mi_page_all_free(page));

  mi_page_set_has_aligned(page, false);

  mi_page_queue_t* pq = mi_page_queue_of(page);
#if MI_RETIRE_CYCLES > 0
  const size_t bsize = mi_page_block_size(page);
  if mi_likely(!mi_page_queue_is_special(pq)) {
    if mi_unlikely(pq->last == page && pq->first == page) {
      mi_stat_counter_increase(_mi_stats_main.pages_retire, 1);
      page->retire_expire = (bsize <= MI_SMALL_OBJ_SIZE_MAX ? MI_RETIRE_CYCLES : MI_RETIRE_CYCLES / 4);
      mi_heap_t* heap = mi_page_heap(page);
      mi_assert_internal(pq >= heap->pages);
      const size_t index = (size_t)(pq - heap->pages);
      mi_assert_internal(index < MI_BIN_FULL && index < MI_BIN_HUGE);
      if (index < heap->page_retired_min) heap->page_retired_min = index;
      if (index > heap->page_retired_max) heap->page_retired_max = index;
      mi_assert_internal(mi_page_all_free(page));
      return;
    }
  }
#endif
  _mi_page_free(page, pq, false);
}

void _mi_heap_collect_retired(mi_heap_t* heap, bool force) {
  size_t min = MI_BIN_FULL;
  size_t max = 0;
  for (size_t bin = heap->page_retired_min; bin <= heap->page_retired_max; bin++) {
    mi_page_queue_t* pq = &heap->pages[bin];
    mi_page_t* page = pq->first;
    if (page != NULL && page->retire_expire != 0) {
      if (mi_page_all_free(page)) {
        page->retire_expire--;
        if (force || page->retire_expire == 0) {
          _mi_page_free(pq->first, pq, force);
        }
        else {
          if (bin < min) min = bin;
          if (bin > max) max = bin;
        }
      }
      else {
        page->retire_expire = 0;
      }
    }
  }
  heap->page_retired_min = min;
  heap->page_retired_max = max;
}

#define MI_MAX_SLICE_SHIFT  (6)
#define MI_MAX_SLICES       (1UL << MI_MAX_SLICE_SHIFT)
#define MI_MIN_SLICES       (2)

static void mi_page_free_list_extend_secure(mi_heap_t* const heap, mi_page_t* const page, const size_t bsize, const size_t extend, mi_stats_t* const stats) {
  MI_UNUSED(stats);
#if (MI_SECURE<=2)
  mi_assert_internal(page->free == NULL);
  mi_assert_internal(page->local_free == NULL);
#endif
  mi_assert_internal(page->capacity + extend <= page->reserved);
  mi_assert_internal(bsize == mi_page_block_size(page));
  void* const page_area = mi_page_start(page);

  size_t shift = MI_MAX_SLICE_SHIFT;
  while ((extend >> shift) == 0) {
    shift--;
  }
  const size_t slice_count = (size_t)1U << shift;
  const size_t slice_extend = extend / slice_count;
  mi_assert_internal(slice_extend >= 1);
  mi_block_t* blocks[MI_MAX_SLICES];
  size_t counts[MI_MAX_SLICES];

  for (size_t i = 0; i < slice_count; i++) {
    blocks[i] = mi_page_block_at(page, page_area, bsize, page->capacity + i * slice_extend);
    counts[i] = slice_extend;
  }
  counts[slice_count - 1] += (extend % slice_count);

  const uintptr_t r = _mi_heap_random_next(heap);
  size_t current = r % slice_count;
  counts[current]--;
  mi_block_t* const free_start = blocks[current];

  uintptr_t rnd = _mi_random_shuffle(r | 1);
  for (size_t i = 1; i < extend; i++) {
    const size_t round = i % MI_INTPTR_SIZE;
    if (round == 0) rnd = _mi_random_shuffle(rnd);
    size_t next = ((rnd >> 8 * round) & (slice_count - 1));
    while (counts[next] == 0) {
      next++;
      if (next == slice_count) next = 0;
    }
    counts[next]--;
    mi_block_t* const block = blocks[current];
    blocks[current] = (mi_block_t*)((uint8_t*)block + bsize);
    mi_block_set_next(page, block, blocks[next]);
    current = next;
  }

  mi_block_set_next(page, blocks[current], page->free);
  page->free = free_start;
}

static mi_decl_noinline void mi_page_free_list_extend(mi_page_t* const page, const size_t bsize, const size_t extend, mi_stats_t* const stats) {
  MI_UNUSED(stats);
#if (MI_SECURE <= 2)
  mi_assert_internal(page->free == NULL);
  mi_assert_internal(page->local_free == NULL);
#endif
  mi_assert_internal(page->capacity + extend <= page->reserved);
  mi_assert_internal(bsize == mi_page_block_size(page));
  mi_assert_internal(extend >= 1);

  void* const page_area = mi_page_start(page);
  mi_block_t* const start = mi_page_block_at(page, page_area, bsize, page->capacity);
  mi_block_t* const last = mi_page_block_at(page, page_area, bsize, page->capacity + extend - 1);
  mi_block_t* block = start;

  size_t remaining = extend;

  while (remaining >= 4) {
    if (remaining > 4) {
      mi_prefetch_w((uint8_t*)block + 4 * bsize);
    }

    mi_block_t* const b0 = block;
    mi_block_t* const b1 = (mi_block_t*)((uint8_t*)b0 + bsize);
    mi_block_t* const b2 = (mi_block_t*)((uint8_t*)b1 + bsize);
    mi_block_t* const b3 = (mi_block_t*)((uint8_t*)b2 + bsize);
    mi_block_t* const next_block = (mi_block_t*)((uint8_t*)b3 + bsize);

    mi_block_set_next(page, b0, b1);
    mi_block_set_next(page, b1, b2);
    mi_block_set_next(page, b2, b3);
    mi_block_set_next(page, b3, next_block);

    block = next_block;
    remaining -= 4;
  }

  while (block <= last) {
    mi_block_t* next = (mi_block_t*)((uint8_t*)block + bsize);
    mi_block_set_next(page, block, next);
    block = next;
  }

  mi_block_set_next(page, last, page->free);
  page->free = start;
}

#define MI_MAX_EXTEND_SIZE    (4*1024)
#if (MI_SECURE>0)
#define MI_MIN_EXTEND         (8*MI_SECURE)
#else
#define MI_MIN_EXTEND         (4)
#endif

static bool mi_page_extend_free(mi_heap_t* heap, mi_page_t* page, mi_tld_t* tld) {
  mi_assert_expensive(mi_page_is_valid_init(page));
#if (MI_SECURE<=2)
  mi_assert(page->free == NULL);
  mi_assert(page->local_free == NULL);
  if (page->free != NULL) return true;
#endif
  if (page->capacity >= page->reserved) return true;

  mi_stat_counter_increase(tld->stats.pages_extended, 1);

  const size_t bsize = mi_page_block_size(page);
  size_t extend = page->reserved - page->capacity;
  mi_assert_internal(extend > 0);

  size_t max_extend = (bsize >= MI_MAX_EXTEND_SIZE ? MI_MIN_EXTEND : MI_MAX_EXTEND_SIZE / bsize);
  if (max_extend < MI_MIN_EXTEND) {
    max_extend = MI_MIN_EXTEND;
  }
  mi_assert_internal(max_extend > 0);

  if (extend > max_extend) {
    extend = max_extend;
  }

  mi_assert_internal(extend > 0 && extend + page->capacity <= page->reserved);
  mi_assert_internal(extend < (1UL << 16));

  if (extend < MI_MIN_SLICES || MI_SECURE == 0) {
    mi_page_free_list_extend(page, bsize, extend, &tld->stats);
  }
  else {
    mi_page_free_list_extend_secure(heap, page, bsize, extend, &tld->stats);
  }

  page->capacity += (uint16_t)extend;
  mi_stat_increase(tld->stats.page_committed, extend * bsize);
  mi_assert_expensive(mi_page_is_valid_init(page));
  return true;
}

static void mi_page_init(mi_heap_t* heap, mi_page_t* page, size_t block_size, mi_tld_t* tld) {
  mi_assert(page != NULL);
  mi_segment_t* segment = _mi_page_segment(page);
  mi_assert(segment != NULL);
  mi_assert_internal(block_size > 0);

  mi_page_set_heap(page, heap);
  page->block_size = block_size;

  size_t page_size;
  page->page_start = _mi_segment_page_start(segment, page, &page_size);
  mi_track_mem_noaccess(page->page_start, page_size);
  mi_assert_internal(mi_page_block_size(page) <= page_size);
  mi_assert_internal(page_size <= page->slice_count * MI_SEGMENT_SLICE_SIZE);
  mi_assert_internal(page_size / block_size < (1L << 16));
  page->reserved = (uint16_t)(page_size / block_size);
  mi_assert_internal(page->reserved > 0);

#if (MI_PADDING || MI_ENCODE_FREELIST)
  page->keys[0] = _mi_heap_random_next(heap);
  page->keys[1] = _mi_heap_random_next(heap);
#endif
  page->free_is_zero = page->is_zero_init;

#if MI_DEBUG>2
  if (page->is_zero_init) {
    mi_track_mem_defined(page->page_start, page_size);
    mi_assert_expensive(mi_mem_is_zero(page->page_start, page_size));
  }
#endif
  mi_assert_internal(page->is_committed);

  if (block_size > 0 && _mi_is_power_of_two(block_size)) {
    page->block_size_shift = (uint8_t)(mi_ctz((uintptr_t)block_size));
  }
  else {
    page->block_size_shift = 0;
  }

  mi_assert_internal(page->capacity == 0);
  mi_assert_internal(page->free == NULL);
  mi_assert_internal(page->used == 0);
  mi_assert_internal(page->xthread_free == 0);
  mi_assert_internal(page->next == NULL);
  mi_assert_internal(page->prev == NULL);
  mi_assert_internal(page->retire_expire == 0);
  mi_assert_internal(!mi_page_has_aligned(page));
#if (MI_PADDING || MI_ENCODE_FREELIST)
  mi_assert_internal(page->keys[0] != 0);
  mi_assert_internal(page->keys[1] != 0);
#endif
  mi_assert_internal(page->block_size_shift == 0 || (block_size == ((size_t)1 << page->block_size_shift)));
  mi_assert_expensive(mi_page_is_valid_init(page));

  if (mi_page_extend_free(heap, page, tld)) {
    mi_assert(mi_page_immediate_available(page));
  }
  return;
}

#define MI_MAX_CANDIDATE_SEARCH  (4)

static bool mi_page_is_expandable(const mi_page_t* page) {
  mi_assert_internal(page != NULL);
  mi_assert_internal(page->capacity <= page->reserved);
  return (page->capacity < page->reserved);
}

static mi_page_t* mi_page_queue_find_free_ex(mi_heap_t* heap, mi_page_queue_t* pq, bool first_try) {
#if MI_STAT
  size_t count = 0;
#endif
  size_t candidate_count = 0;
  mi_page_t* page_candidate = NULL;
  mi_page_t* page = pq->first;

  while (page != NULL) {
    mi_page_t* next = page->next;

    if mi_likely(next != NULL) {
      mi_prefetch_r_l2(next);
    }

#if MI_STAT
    count++;
#endif
    candidate_count++;

    _mi_page_free_collect(page, false);

#if MI_MAX_CANDIDATE_SEARCH > 1
    const bool immediate_available = mi_page_immediate_available(page);

    if mi_unlikely(!immediate_available && !mi_page_is_expandable(page)) {
      mi_assert_internal(!mi_page_is_in_full(page) && !mi_page_immediate_available(page));
      mi_page_to_full(page, pq);
    }
    else {
      if (page_candidate == NULL) {
        page_candidate = page;
        candidate_count = 0;
      }
      else if (page->used >= page_candidate->used && !mi_page_is_mostly_used(page) && !mi_page_is_expandable(page)) {
        page_candidate = page;
      }

      if mi_likely(immediate_available || candidate_count > MI_MAX_CANDIDATE_SEARCH) {
        mi_assert_internal(page_candidate != NULL);
        break;
      }
    }
#else
    if mi_likely(mi_page_immediate_available(page) || mi_page_is_expandable(page)) {
      break;
    }

    mi_assert_internal(!mi_page_is_in_full(page) && !mi_page_immediate_available(page));
    mi_page_to_full(page, pq);
#endif

    page = next;
  }

  mi_heap_stat_counter_increase(heap, page_searches, count);
  mi_heap_stat_counter_increase(heap, page_searches_count, 1);

  if (page_candidate != NULL) {
    page = page_candidate;
  }

  if mi_likely(page != NULL) {
    if mi_unlikely(!mi_page_immediate_available(page)) {
      mi_assert_internal(mi_page_is_expandable(page));
      if (!mi_page_extend_free(heap, page, heap->tld)) {
        page = NULL;
      }
    }
    mi_assert_internal(page == NULL || mi_page_immediate_available(page));
  }

  if mi_unlikely(page == NULL) {
    _mi_heap_collect_retired(heap, false);
    page = mi_page_fresh(heap, pq);
    if (page == NULL && first_try) {
      page = mi_page_queue_find_free_ex(heap, pq, false);
    }
  }
  else {
    mi_page_queue_move_to_front(heap, pq, page);
    page->retire_expire = 0;
  }

  mi_assert_internal(page == NULL || mi_page_immediate_available(page));

  return page;
}

static inline mi_page_t* mi_find_free_page(mi_heap_t* heap, size_t size) {
  mi_page_queue_t* pq = mi_page_queue(heap, size);

  mi_page_t* page = pq->first;
  if mi_likely(page != NULL) {
#if (MI_SECURE>=3)
    if (page->capacity < page->reserved && ((_mi_heap_random_next(heap) & 1) == 1)) {
      mi_page_extend_free(heap, page, heap->tld);
      mi_assert_internal(mi_page_immediate_available(page));
    }
    else
#endif
    {
      _mi_page_free_collect(page, false);
    }

    if mi_likely(mi_page_immediate_available(page)) {
      page->retire_expire = 0;
      return page;
    }
  }

  return mi_page_queue_find_free_ex(heap, pq, true);
}

static mi_deferred_free_fun* volatile deferred_free = NULL;
static _Atomic(void*) deferred_arg;

void _mi_deferred_free(mi_heap_t* heap, bool force) {
  heap->tld->heartbeat++;
  if (deferred_free != NULL && !heap->tld->recurse) {
    heap->tld->recurse = true;
    deferred_free(force, heap->tld->heartbeat, mi_atomic_load_ptr_relaxed(void, &deferred_arg));
    heap->tld->recurse = false;
  }
}

void mi_register_deferred_free(mi_deferred_free_fun* fn, void* arg) mi_attr_noexcept {
  deferred_free = fn;
  mi_atomic_store_ptr_release(void, &deferred_arg, arg);
}

static mi_page_t* mi_large_huge_page_alloc(mi_heap_t* heap, size_t size, size_t page_alignment) {
  size_t block_size = _mi_os_good_alloc_size(size);
  mi_assert_internal(mi_bin(block_size) == MI_BIN_HUGE || page_alignment > 0);
  bool is_huge = (block_size > MI_LARGE_OBJ_SIZE_MAX || page_alignment > 0);

#if MI_HUGE_PAGE_ABANDON
  mi_page_queue_t* pq = (is_huge ? NULL : mi_page_queue(heap, block_size));
#else
  mi_page_queue_t* pq = mi_page_queue(heap, is_huge ? MI_LARGE_OBJ_SIZE_MAX + 1 : block_size);
  mi_assert_internal(!is_huge || mi_page_queue_is_huge(pq));
#endif

  mi_page_t* page = mi_page_fresh_alloc(heap, pq, block_size, page_alignment);
  if mi_likely(page != NULL) {
    mi_assert_internal(mi_page_immediate_available(page));

    if (is_huge) {
      mi_assert_internal(mi_page_is_huge(page));
      mi_assert_internal(_mi_page_segment(page)->kind == MI_SEGMENT_HUGE);
      mi_assert_internal(_mi_page_segment(page)->used == 1);
#if MI_HUGE_PAGE_ABANDON
      mi_assert_internal(_mi_page_segment(page)->thread_id == 0);
      mi_page_set_heap(page, NULL);
#endif
    }
    else {
      mi_assert_internal(!mi_page_is_huge(page));
    }

    const size_t bsize = mi_page_usable_block_size(page);
    {
      _mi_stat_increase(&heap->tld->stats.malloc_huge, bsize);
      _mi_stat_counter_increase(&heap->tld->stats.malloc_huge_count, 1);
    }
  }
  return page;
}

static mi_page_t* mi_find_page(mi_heap_t* heap, size_t size, size_t huge_alignment) mi_attr_noexcept {
  const size_t req_size = size - MI_PADDING_SIZE;
  if mi_unlikely(req_size > (MI_MEDIUM_OBJ_SIZE_MAX - MI_PADDING_SIZE) || huge_alignment > 0) {
    if mi_unlikely(req_size > MI_MAX_ALLOC_SIZE) {
      _mi_error_message(EOVERFLOW, "allocation request is too large (%zu bytes)\n", req_size);
      return NULL;
    }
    else {
      return mi_large_huge_page_alloc(heap, size, huge_alignment);
    }
  }
  else {
#if MI_PADDING
    mi_assert_internal(size >= MI_PADDING_SIZE);
#endif
    return mi_find_free_page(heap, size);
  }
}

void* _mi_malloc_generic(mi_heap_t* heap, size_t size, bool zero, size_t huge_alignment, size_t* usable) mi_attr_noexcept {
  mi_assert_internal(heap != NULL);

  if mi_unlikely(!mi_heap_is_initialized(heap)) {
    heap = mi_heap_get_default();
    if mi_unlikely(!mi_heap_is_initialized(heap)) {
      return NULL;
    }
  }
  mi_assert_internal(mi_heap_is_initialized(heap));

  if mi_unlikely(++heap->generic_count >= 100) {
    heap->generic_collect_count += heap->generic_count;
    heap->generic_count = 0;
    _mi_deferred_free(heap, false);
    _mi_heap_delayed_free_partial(heap);

    const long generic_collect = mi_option_get_clamp(mi_option_generic_collect, 1, 1000000L);
    if mi_unlikely(heap->generic_collect_count >= generic_collect) {
      heap->generic_collect_count = 0;
      mi_heap_collect(heap, false);
    }
  }

  mi_page_t* page = mi_find_page(heap, size, huge_alignment);
  if mi_unlikely(page == NULL) {
    mi_heap_collect(heap, true);
    page = mi_find_page(heap, size, huge_alignment);
  }

  if mi_unlikely(page == NULL) {
    const size_t req_size = size - MI_PADDING_SIZE;
    _mi_error_message(ENOMEM, "unable to allocate memory (%zu bytes)\n", req_size);
    return NULL;
  }

  mi_assert_internal(mi_page_immediate_available(page));
  mi_assert_internal(mi_page_block_size(page) >= size);

  void* const p = _mi_page_malloc_zero(heap, page, size, zero, usable);
  mi_assert_internal(p != NULL);

  if mi_unlikely(page->reserved == page->used) {
    mi_page_to_full(page, mi_page_queue_of(page));
  }
  return p;
}
