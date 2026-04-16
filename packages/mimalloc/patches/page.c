/*----------------------------------------------------------------------------
Copyright (c) 2018-2024, Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/

/* -----------------------------------------------------------
  The core of the allocator. Every segment contains
  pages of a certain block size. The main function
  exported is `mi_malloc_generic`.
----------------------------------------------------------- */

#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/atomic.h"
#include "mimalloc/prim.h"

/* -----------------------------------------------------------
  Definition of page queues for each block size
----------------------------------------------------------- */

#define MI_IN_PAGE_C
#include "page-queue.c"
#undef MI_IN_PAGE_C

static mi_decl_forceinline void mi_prefetch_read(const void* p) {
#if defined(__GNUC__) || defined(__clang__)
  if (p != NULL) {
    __builtin_prefetch(p, 0, 3);
  }
#else
  MI_UNUSED(p);
#endif
}


/* -----------------------------------------------------------
  Page helpers
----------------------------------------------------------- */

// Index a block in a page
static inline mi_block_t* mi_page_block_at(const mi_page_t* page, void* page_start, size_t block_size, size_t i) {
  MI_UNUSED(page);
  mi_assert_internal(page != NULL);
  mi_assert_internal(i <= page->reserved);
  return (mi_block_t*)((uint8_t*)page_start + (i * block_size));
}

static bool mi_page_extend_free(mi_theap_t* theap, mi_page_t* page);

#if (MI_DEBUG>=3)
static size_t mi_page_list_count(mi_page_t* page, mi_block_t* head) {
  mi_assert_internal(_mi_ptr_page(page->page_start) == page);
  const uint8_t* slice_start = mi_page_slice_start(page);
  mi_assert_internal(_mi_is_aligned(slice_start,MI_PAGE_ALIGN));
  size_t count = 0;
  while (head != NULL) {
    mi_assert_internal((uint8_t*)head - slice_start > (ptrdiff_t)MI_LARGE_PAGE_SIZE || page == _mi_ptr_page(head));
    count++;
    head = mi_block_next(page, head);
  }
  return count;
}

/*
// Start of the page available memory
static inline uint8_t* mi_page_area(const mi_page_t* page) {
  return _mi_page_start(_mi_page_segment(page), page, NULL);
}
*/

static bool mi_page_list_is_valid(mi_page_t* page, mi_block_t* p) {
  size_t psize;
  uint8_t* page_area = mi_page_area(page, &psize);
  mi_block_t* start = (mi_block_t*)page_area;
  mi_block_t* end   = (mi_block_t*)(page_area + psize);
  while(p != NULL) {
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

  mi_assert_internal(mi_page_list_is_valid(page,page->free));
  mi_assert_internal(mi_page_list_is_valid(page,page->local_free));

  #if MI_DEBUG>3
  if (page->free_is_zero) {
    const size_t ubsize = mi_page_usable_block_size(page);
    for(mi_block_t* block = page->free; block != NULL; block = mi_block_next(page,block)) {
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
  if (!mi_page_is_abandoned(page)) {
    {
      mi_page_queue_t* pq = mi_page_queue_of(page);
      mi_assert_internal(mi_page_queue_contains(pq, page));
      mi_assert_internal(pq->block_size==mi_page_block_size(page) || mi_page_is_huge(page) || mi_page_is_in_full(page));
    }
  }
  return true;
}
#endif


/* -----------------------------------------------------------
  Page collect the `local_free` and `thread_free` lists
----------------------------------------------------------- */

static void mi_page_thread_collect_to_local(mi_page_t* page, mi_block_t* head)
{
  if (head == NULL) return;

  size_t max_count = page->capacity;
  size_t count = 1;
  mi_block_t* last = head;
  for (;;) {
    mi_block_t* const next = mi_block_next(page, last);
    if (next == NULL) break;
    count++;
    if mi_unlikely(count > max_count) {
      _mi_error_message(EFAULT, "corrupted thread-free list\n");
      return;
    }
    last = next;
    mi_prefetch_read(mi_block_next(page, last));
  }

  mi_block_set_next(page, last, page->local_free);
  page->local_free = head;

  mi_assert_internal(count <= UINT16_MAX);
  mi_assert_internal(page->used >= (uint16_t)count);
  page->used = (uint16_t)(page->used - (uint16_t)count);
}

// Collect the local `thread_free` list using an atomic exchange.
static void mi_page_thread_free_collect(mi_page_t* page)
{
  mi_block_t* head;
  mi_thread_free_t tfreex;
  mi_thread_free_t tfree = mi_atomic_load_relaxed(&page->xthread_free);
  do {
    head = mi_tf_block(tfree);
    if mi_likely(head == NULL) return;
    tfreex = mi_tf_create(NULL,mi_tf_is_owned(tfree));
  } while (!mi_atomic_cas_weak_acq_rel(&page->xthread_free, &tfree, tfreex));
  mi_assert_internal(head != NULL);

  mi_page_thread_collect_to_local(page, head);
}

// returns `true` if after collection `mi_page_immediate_available` is true.
static mi_decl_forceinline bool mi_page_free_quick_collect(mi_page_t* page) {
  if (page->free != NULL) return true;
  if (page->local_free == NULL) return false;
  page->free = page->local_free;
  page->local_free = NULL;
  page->free_is_zero = false;
  return true;
}

void _mi_page_free_collect(mi_page_t* page, bool force) {
  mi_assert_internal(page!=NULL);

  mi_page_thread_free_collect(page);

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

void _mi_page_free_collect_partly(mi_page_t* page, mi_block_t* head) {
  if (head == NULL) return;
  mi_block_t* next = mi_block_next(page,head);
  if (next != NULL) {
    mi_block_set_next(page, head, NULL);
    mi_page_thread_collect_to_local(page, next);
    if (page->local_free != NULL && page->free == NULL) {
      page->free = page->local_free;
      page->local_free = NULL;
      page->free_is_zero = false;
    }
  }
  if (page->used == 1) {
    mi_assert_internal(mi_tf_block(mi_atomic_load_relaxed(&page->xthread_free)) == head);
    mi_assert_internal(mi_block_next(page,head) == NULL);
    _mi_page_free_collect(page, false);
  }
}


/* -----------------------------------------------------------
  Page fresh and retire
----------------------------------------------------------- */

void _mi_theap_page_reclaim(mi_theap_t* theap, mi_page_t* page)
{
  mi_assert_internal(_mi_is_aligned(mi_page_slice_start(page), MI_PAGE_ALIGN));
  mi_assert_internal(_mi_ptr_page(mi_page_start(page))==page);
  mi_assert_internal(mi_page_is_owned(page));
  mi_assert_internal(mi_page_is_abandoned(page));

  mi_page_set_theap(page,theap);
  _mi_page_free_collect(page, false);
  mi_page_queue_t* pq = mi_theap_page_queue_of(theap, page);
  mi_page_queue_push_at_end(theap, pq, page);
  mi_assert_expensive(_mi_page_is_valid(page));
}

void _mi_page_abandon(mi_page_t* page, mi_page_queue_t* pq) {
  _mi_page_free_collect(page, false);
  if (mi_page_all_free(page)) {
    _mi_page_free(page, pq);
  }
  else {
    mi_page_queue_remove(pq, page);
    mi_theap_t* theap = page->theap;
    mi_page_set_theap(page, NULL);
    page->theap = theap;
    _mi_arenas_page_abandon(page, theap);
    _mi_arenas_collect(false, false, theap->tld);
  }
}


// allocate a fresh page from an arena
static mi_page_t* mi_page_fresh_alloc(mi_theap_t* theap, mi_page_queue_t* pq, size_t block_size, size_t page_alignment) {
  #if !MI_HUGE_PAGE_ABANDON
  mi_assert_internal(pq != NULL);
  mi_assert_internal(mi_theap_contains_queue(theap, pq));
  mi_assert_internal(page_alignment > 0 || block_size > MI_LARGE_MAX_OBJ_SIZE || block_size == pq->block_size);
  #endif
  mi_page_t* page = _mi_arenas_page_alloc(theap, block_size, page_alignment);
  if (page == NULL) {
    return NULL;
  }
  if (mi_page_is_abandoned(page)) {
    _mi_theap_page_reclaim(theap, page);
    if (!mi_page_immediate_available(page)) {
      if (mi_page_is_expandable(page)) {
        if (!mi_page_extend_free(theap, page)) {
          return NULL;
        }
      }
      else {
        mi_assert(false);
        return NULL;
      }
    }
  }
  else if (pq != NULL) {
    mi_page_queue_push(theap, pq, page);
  }
  mi_assert_internal(pq!=NULL || mi_page_block_size(page) >= block_size);
  mi_assert_expensive(_mi_page_is_valid(page));
  return page;
}

// Get a fresh page to use
static mi_page_t* mi_page_fresh(mi_theap_t* theap, mi_page_queue_t* pq) {
  mi_assert_internal(mi_theap_contains_queue(theap, pq));
  mi_page_t* page = mi_page_fresh_alloc(theap, pq, pq->block_size, 0);
  if (page==NULL) return NULL;
  mi_assert_internal(pq->block_size==mi_page_block_size(page));
  mi_assert_internal(pq==mi_theap_page_queue_of(theap, page));
  return page;
}


/* -----------------------------------------------------------
  Unfull, abandon, free and retire
----------------------------------------------------------- */

// Move a page from the full list back to a regular list (called from thread-local mi_free)
void _mi_page_unfull(mi_page_t* page) {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(mi_page_is_in_full(page));
  mi_assert_internal(!mi_page_theap(page)->allow_page_abandon);
  if (!mi_page_is_in_full(page)) return;

  mi_theap_t* theap = mi_page_theap(page);
  mi_page_queue_t* pqfull = &theap->pages[MI_BIN_FULL];
  mi_page_set_in_full(page, false);
  mi_page_queue_t* pq = mi_theap_page_queue_of(theap, page);
  mi_page_set_in_full(page, true);
  mi_page_queue_enqueue_from_full(pq, pqfull, page);
}

static void mi_page_to_full(mi_page_t* page, mi_page_queue_t* pq) {
  mi_assert_internal(pq == mi_page_queue_of(page));
  mi_assert_internal(!mi_page_immediate_available(page));
  mi_assert_internal(!mi_page_is_in_full(page));

  mi_theap_t* theap = mi_page_theap(page);
  if (theap->allow_page_abandon) {
    _mi_page_abandon(page, pq);
  }
  else if (!mi_page_is_in_full(page)) {
    mi_page_queue_enqueue_from(&mi_page_theap(page)->pages[MI_BIN_FULL], pq, page);
    _mi_page_free_collect(page, false);
  }
}


// Free a page with no more free blocks
void _mi_page_free(mi_page_t* page, mi_page_queue_t* pq) {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(pq == mi_page_queue_of(page));
  mi_assert_internal(mi_page_all_free(page));

  mi_page_set_has_interior_pointers(page, false);

  mi_page_queue_remove(pq, page);

  mi_theap_t* theap = mi_page_theap(page); mi_assert_internal(theap!=NULL);
  mi_page_set_theap(page,NULL);
  _mi_arenas_page_free(page, theap);
  _mi_arenas_collect(false, false, theap->tld);
}

#define MI_MAX_RETIRE_SIZE    MI_LARGE_OBJ_SIZE_MAX
#define MI_RETIRE_CYCLES      (16)

void _mi_page_retire(mi_page_t* page) mi_attr_noexcept {
  mi_assert_internal(page != NULL);
  mi_assert_expensive(_mi_page_is_valid(page));
  mi_assert_internal(mi_page_all_free(page));

  if (page->retire_expire!=0) return;
  mi_page_set_has_interior_pointers(page, false);

  mi_page_queue_t* pq = mi_page_queue_of(page);
  #if MI_RETIRE_CYCLES > 0
  const size_t bsize = mi_page_block_size(page);
  if mi_likely(!mi_page_queue_is_special(pq)) {
    if (pq->last==page && pq->first==page) {
      mi_theap_t* theap = mi_page_theap(page);
      #if MI_STAT>0
      mi_theap_stat_counter_increase(theap, pages_retire, 1);
      #endif
      page->retire_expire = (uint8_t)(bsize <= MI_SMALL_MAX_OBJ_SIZE ? MI_RETIRE_CYCLES : MI_RETIRE_CYCLES/4);
      mi_assert_internal(pq >= theap->pages);
      const size_t index = (size_t)(pq - theap->pages);
      mi_assert_internal(index < MI_BIN_FULL && index < MI_BIN_HUGE);
      if (index < theap->page_retired_min) theap->page_retired_min = index;
      if (index > theap->page_retired_max) theap->page_retired_max = index;
      mi_assert_internal(mi_page_all_free(page));
      return;
    }
  }
  #endif
  _mi_page_free(page, pq);
}

// free retired pages: we don't need to look at the entire queues
// since we only retire pages that are at the head position in a queue.
void _mi_theap_collect_retired(mi_theap_t* theap, bool force) {
  size_t min = MI_BIN_FULL;
  size_t max = 0;
  for(size_t bin = theap->page_retired_min; bin <= theap->page_retired_max; bin++) {
    mi_page_queue_t* pq   = &theap->pages[bin];
    mi_page_t*       page = pq->first;
    if (page != NULL && page->retire_expire != 0) {
      if (mi_page_all_free(page)) {
        page->retire_expire--;
        if (page->retire_expire == 0 || force) {
          _mi_page_free(page, pq);
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
  theap->page_retired_min = min;
  theap->page_retired_max = max;
}


/* -----------------------------------------------------------
  Initialize the initial free list in a page.
  In secure mode we initialize a randomized list by
  alternating between slices.
----------------------------------------------------------- */

#define MI_MAX_SLICE_SHIFT  (6)
#define MI_MAX_SLICES       (1UL << MI_MAX_SLICE_SHIFT)
#define MI_MIN_SLICES       (2)

static void mi_page_free_list_extend_secure(mi_theap_t* const theap, mi_page_t* const page, const size_t bsize, const size_t extend) {
  #if (MI_SECURE<3)
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
  size_t      counts[MI_MAX_SLICES];
  for (size_t i = 0; i < slice_count; i++) {
    blocks[i] = mi_page_block_at(page, page_area, bsize, page->capacity + i*slice_extend);
    counts[i] = slice_extend;
  }
  counts[slice_count-1] += (extend % slice_count);

  const uintptr_t r = _mi_theap_random_next(theap);
  size_t current = (size_t)(r % slice_count);
  counts[current]--;
  mi_block_t* const free_start = blocks[current];

  uintptr_t rnd = _mi_random_shuffle(r|1);
  for (size_t i = 1; i < extend; i++) {
    const size_t round = i%MI_INTPTR_SIZE;
    if (round == 0) rnd = _mi_random_shuffle(rnd);
    size_t next = (size_t)((rnd >> (8*round)) & (slice_count-1));
    while (counts[next]==0) {
      next++;
      if (next==slice_count) next = 0;
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

static mi_decl_noinline void mi_page_free_list_extend(mi_page_t* const page, const size_t bsize, const size_t extend)
{
  #if (MI_SECURE<3)
  mi_assert_internal(page->free == NULL);
  mi_assert_internal(page->local_free == NULL);
  #endif
  mi_assert_internal(page->capacity + extend <= page->reserved);
  mi_assert_internal(bsize == mi_page_block_size(page));
  mi_assert_internal(extend > 0);

  void* const page_area = mi_page_start(page);
  mi_block_t* const start = mi_page_block_at(page, page_area, bsize, page->capacity);

  mi_block_t* block = start;
  for (size_t i = 1; i < extend; i++) {
    mi_block_t* const next = (mi_block_t*)((uint8_t*)block + bsize);
    mi_block_set_next(page, block, next);
    block = next;
  }

  mi_block_set_next(page, block, page->free);
  page->free = start;
}

/* -----------------------------------------------------------
  Page initialize and extend the capacity
----------------------------------------------------------- */

#define MI_MAX_EXTEND_SIZE    (4*1024)
#if (MI_SECURE>=3)
#define MI_MIN_EXTEND         (8*MI_SECURE)
#else
#define MI_MIN_EXTEND         (1)
#endif

static bool mi_page_extend_free(mi_theap_t* theap, mi_page_t* page) {
  mi_assert_expensive(mi_page_is_valid_init(page));
  #if (MI_SECURE<3)
  mi_assert(page->free == NULL);
  mi_assert(page->local_free == NULL);
  if (page->free != NULL) return true;
  #endif
  if (page->capacity >= page->reserved) return true;

  size_t page_size;
  mi_page_area(page, &page_size);
  MI_UNUSED(page_size);
  #if MI_STAT>0
  mi_theap_stat_counter_increase(theap, pages_extended, 1);
  #endif

  const size_t bsize = mi_page_block_size(page);
  size_t extend = (size_t)page->reserved - page->capacity;
  mi_assert_internal(extend > 0);

  size_t max_extend = (bsize >= MI_MAX_EXTEND_SIZE ? MI_MIN_EXTEND : MI_MAX_EXTEND_SIZE/bsize);
  if (max_extend < MI_MIN_EXTEND) { max_extend = MI_MIN_EXTEND; }
  mi_assert_internal(max_extend > 0);

  if (extend > max_extend) {
    extend = max_extend;
  }

  mi_assert_internal(extend > 0 && extend + page->capacity <= page->reserved);
  mi_assert_internal(extend < (1UL<<16));

  if (page->slice_committed > 0) {
    const size_t needed_size = (page->capacity + extend)*bsize;
    const size_t needed_commit = _mi_align_up(mi_page_slice_offset_of(page, needed_size), MI_PAGE_MIN_COMMIT_SIZE);
    if (needed_commit > page->slice_committed) {
      mi_assert_internal(((needed_commit - page->slice_committed) % _mi_os_page_size()) == 0);
      if (!_mi_os_commit(mi_page_slice_start(page) + page->slice_committed, needed_commit - page->slice_committed, NULL)) {
        return false;
      }
      page->slice_committed = needed_commit;
    }
  }

  if (extend < MI_MIN_SLICES || MI_SECURE<3) {
    mi_page_free_list_extend(page, bsize, extend);
  }
  else {
    mi_page_free_list_extend_secure(theap, page, bsize, extend);
  }

  page->capacity = (uint16_t)(page->capacity + (uint16_t)extend);
  #if MI_STAT>0
  mi_theap_stat_increase(theap, page_committed, extend * bsize);
  #endif
  mi_assert_expensive(mi_page_is_valid_init(page));
  return true;
}

// Initialize a fresh page (that is already partially initialized)
mi_decl_nodiscard bool _mi_page_init(mi_theap_t* theap, mi_page_t* page) {
  mi_assert(page != NULL);
  mi_assert(theap!=NULL);
  page->heap = (_mi_is_heap_main(_mi_theap_heap(theap)) ? NULL : _mi_theap_heap(theap));
  mi_page_set_theap(page, theap);

  size_t page_size;
  uint8_t* page_start = mi_page_area(page, &page_size); MI_UNUSED(page_start);
  mi_track_mem_noaccess(page_start,page_size);
  mi_assert_internal(page_size / mi_page_block_size(page) < (1L<<16));
  mi_assert_internal(page->reserved > 0);
  #if (MI_PADDING || MI_ENCODE_FREELIST)
  page->keys[0] = _mi_theap_random_next(theap);
  page->keys[1] = _mi_theap_random_next(theap);
  #endif
  #if MI_DEBUG>2
  if (page->memid.initially_zero) {
    mi_track_mem_defined(page->page_start, mi_page_committed(page));
    mi_assert_expensive(mi_mem_is_zero(page_start, mi_page_committed(page)));
  }
  #endif

  mi_assert_internal(page->theap!=NULL);
  mi_assert_internal(page->theap == mi_page_theap(page));
  mi_assert_internal(page->capacity == 0);
  mi_assert_internal(page->free == NULL);
  mi_assert_internal(page->used == 0);
  mi_assert_internal(mi_page_is_owned(page));
  mi_assert_internal(page->xthread_free == 1);
  mi_assert_internal(page->next == NULL);
  mi_assert_internal(page->prev == NULL);
  mi_assert_internal(page->retire_expire == 0);
  mi_assert_internal(!mi_page_has_interior_pointers(page));
  #if (MI_PADDING || MI_ENCODE_FREELIST)
  mi_assert_internal(page->keys[0] != 0);
  mi_assert_internal(page->keys[1] != 0);
  #endif
  mi_assert_expensive(mi_page_is_valid_init(page));

  if (!mi_page_extend_free(theap,page)) return false;
  mi_assert(mi_page_immediate_available(page));
  return true;
}


/* -----------------------------------------------------------
  Find pages with free blocks
-------------------------------------------------------------*/

// Find a page with free blocks of `page->block_size`.
static mi_decl_noinline mi_page_t* mi_page_queue_find_free_ex(mi_theap_t* theap, mi_page_queue_t* pq, bool first_try)
{
  size_t count = 0;
  long candidate_limit = 0;
  const long max_candidates = _mi_option_get_fast(mi_option_page_max_candidates);
  long page_full_retain = (pq->block_size > MI_SMALL_MAX_OBJ_SIZE ? 0 : theap->page_full_retain);
  mi_page_t* page_candidate = NULL;
  mi_page_t* page = pq->first;

  while (page != NULL)
  {
    mi_page_t* const next = page->next;
    mi_prefetch_read(next);
    count++;
    candidate_limit--;

    bool immediate_available = mi_page_immediate_available(page);
    if (!immediate_available) {
      _mi_page_free_collect(page, false);
      immediate_available = mi_page_immediate_available(page);
    }

    if (!immediate_available && !mi_page_is_expandable(page)) {
      page_full_retain--;
      if (page_full_retain < 0) {
        mi_assert_internal(!mi_page_is_in_full(page) && !mi_page_immediate_available(page));
        mi_page_to_full(page, pq);
      }
    }
    else {
      if (page_candidate == NULL) {
        page_candidate = page;
        candidate_limit = max_candidates;
      }
      else if (mi_page_all_free(page_candidate)) {
        _mi_page_free(page_candidate, pq);
        page_candidate = page;
      }
      else if (page->used >= page_candidate->used && !mi_page_is_mostly_used(page)) {
        page_candidate = page;
      }

      if (immediate_available || candidate_limit <= 0) {
        mi_assert_internal(page_candidate!=NULL);
        break;
      }
    }

    page = next;
  }

  mi_theap_stat_counter_increase(theap, page_searches, count);
  mi_theap_stat_counter_increase(theap, page_searches_count, 1);

  if (page_candidate != NULL) {
    page = page_candidate;
  }
  if (page != NULL) {
    if (!mi_page_immediate_available(page)) {
      mi_assert_internal(mi_page_is_expandable(page));
      if (!mi_page_extend_free(theap, page)) {
        page = NULL;
      }
    }
    mi_assert_internal(page == NULL || mi_page_immediate_available(page));
  }

  if (page == NULL) {
    _mi_theap_collect_retired(theap, false);
    page = mi_page_fresh(theap, pq);
    mi_assert_internal(page == NULL || mi_page_immediate_available(page));
    if (page == NULL && first_try) {
      page = mi_page_queue_find_free_ex(theap, pq, false);
      mi_assert_internal(page == NULL || mi_page_immediate_available(page));
    }
  }
  else {
    mi_assert_internal(page == NULL || mi_page_immediate_available(page));
    mi_page_queue_move_to_front(theap, pq, page);
    page->retire_expire = 0;
  }
  mi_assert_internal(page == NULL || mi_page_immediate_available(page));

  return page;
}



// Find a page with free blocks of `size`.
static mi_page_t* mi_find_free_page(mi_theap_t* theap, mi_page_queue_t* pq) {
  mi_assert_internal(!mi_page_queue_is_huge(pq));

  mi_page_t* page = pq->first;
  if mi_likely(page != NULL && mi_page_free_quick_collect(page)) {
    mi_prefetch_read(page->free);
    #if (MI_SECURE>=3)
    if (page->capacity < page->reserved && ((_mi_theap_random_next(theap) & 1) == 1)) {
      (void)mi_page_extend_free(theap, page);
      mi_assert_internal(mi_page_immediate_available(page));
    }
    #endif
    page->retire_expire = 0;
    return page;
  }
  else {
    return mi_page_queue_find_free_ex(theap, pq, true);
  }
}


/* -----------------------------------------------------------
  Users can register a deferred free function called
  when the `free` list is empty. Since the `local_free`
  is separate this is deterministically called after
  a certain number of allocations.
----------------------------------------------------------- */

static mi_deferred_free_fun* volatile deferred_free = NULL;
static _Atomic(void*) deferred_arg; // = NULL

void _mi_deferred_free(mi_theap_t* theap, bool force) {
  theap->heartbeat++;
  if (deferred_free != NULL && !theap->tld->recurse) {
    theap->tld->recurse = true;
    deferred_free(force, theap->heartbeat, mi_atomic_load_ptr_relaxed(void,&deferred_arg));
    theap->tld->recurse = false;
  }
}

void mi_register_deferred_free(mi_deferred_free_fun* fn, void* arg) mi_attr_noexcept {
  deferred_free = fn;
  mi_atomic_store_ptr_release(void,&deferred_arg, arg);
}


/* -----------------------------------------------------------
  General allocation
----------------------------------------------------------- */

// Huge pages contain just one block, and the segment contains just that page.
// Huge pages are also use if the requested alignment is very large (> MI_BLOCK_ALIGNMENT_MAX)
// so their size is not always `> MI_LARGE_OBJ_SIZE_MAX`.
static mi_page_t* mi_huge_page_alloc(mi_theap_t* theap, size_t size, size_t page_alignment, mi_page_queue_t* pq) {
  const size_t block_size = _mi_os_good_alloc_size(size);
  #if MI_HUGE_PAGE_ABANDON
  #error todo.
  #else
  mi_assert_internal(mi_page_queue_is_huge(pq));
  #endif
  mi_page_t* page = mi_page_fresh_alloc(theap, pq, block_size, page_alignment);
  if (page != NULL) {
    mi_assert_internal(mi_page_block_size(page) >= size);
    mi_assert_internal(mi_page_immediate_available(page));
    mi_assert_internal(mi_page_is_huge(page));
    mi_assert_internal(mi_page_is_singleton(page));
    #if MI_HUGE_PAGE_ABANDON
    mi_assert_internal(mi_page_is_abandoned(page));
    mi_page_set_theap(page, NULL);
    #endif
    mi_theap_stat_increase(theap, malloc_huge, mi_page_block_size(page));
    mi_theap_stat_counter_increase(theap, malloc_huge_count, 1);
  }
  return page;
}


// Allocate a page
// Note: in debug mode the size includes MI_PADDING_SIZE and might have overflowed.
static mi_page_t* mi_find_page(mi_theap_t* theap, size_t size, size_t huge_alignment) mi_attr_noexcept {
  const size_t req_size = size - MI_PADDING_SIZE;
  if mi_unlikely(req_size > MI_MAX_ALLOC_SIZE) {
    _mi_error_message(EOVERFLOW, "allocation request is too large (%zu bytes)\n", req_size);
    return NULL;
  }
  mi_page_queue_t* pq = mi_page_queue(theap, (huge_alignment > 0 ? MI_LARGE_MAX_OBJ_SIZE+1 : size));
  if mi_unlikely(mi_page_queue_is_huge(pq)) {
    return mi_huge_page_alloc(theap,size,huge_alignment,pq);
  }
  else {
    #if MI_PADDING
    mi_assert_internal(size >= MI_PADDING_SIZE);
    #endif
    return mi_find_free_page(theap, pq);
  }
}


// Generic allocation routine if the fast path (`alloc.c:mi_page_malloc`) does not succeed.
// Note: in debug mode the size includes MI_PADDING_SIZE and might have overflowed.
// The `huge_alignment` is normally 0 but is set to a multiple of MI_SLICE_SIZE for
// very large requested alignments in which case we use a huge singleton page.
// Note: we put `bool zero, size_t huge_alignment` into one parameter (with zero in the low bit)
// to use 4 parameters which compiles better on msvc for the malloc fast path.
void* _mi_malloc_generic(mi_theap_t* theap, size_t size, size_t zero_huge_alignment, size_t* usable) mi_attr_noexcept
{
  const bool zero = ((zero_huge_alignment & 1) != 0);
  const size_t huge_alignment = (zero_huge_alignment & ~1);

  #if !MI_THEAP_INITASNULL
  mi_assert_internal(theap != NULL);
  #endif

  if mi_unlikely(!mi_theap_is_initialized(theap)) {
    if (theap==&_mi_theap_empty_wrong) {
      return NULL;
    }
    mi_thread_init();
    theap = _mi_theap_default();
    if mi_unlikely(!mi_theap_is_initialized(theap)) { return NULL; }
    mi_assert_internal(_mi_theap_default()==theap);
  }
  mi_assert_internal(mi_theap_is_initialized(theap));

  if mi_unlikely(++theap->generic_count >= 1000) {
    theap->generic_collect_count += theap->generic_count;
    theap->generic_count = 0;
    _mi_deferred_free(theap, false);
    _mi_theap_collect_retired(theap, false);

    const long generic_collect = mi_option_get_clamp(mi_option_generic_collect, 1, 1000000L);
    if (theap->generic_collect_count >= generic_collect) {
      theap->generic_collect_count = 0;
      mi_theap_collect(theap, false);
    }
  }

  mi_page_t* page = mi_find_page(theap, size, huge_alignment);
  if mi_unlikely(page == NULL) {
    mi_theap_collect(theap, true);
    page = mi_find_page(theap, size, huge_alignment);
  }

  if mi_unlikely(page == NULL) {
    const size_t req_size = size - MI_PADDING_SIZE;
    _mi_error_message(ENOMEM, "unable to allocate memory (%zu bytes)\n", req_size);
    return NULL;
  }

  mi_assert_internal(mi_page_immediate_available(page));
  mi_assert_internal(mi_page_block_size(page) >= size);
  mi_assert_internal(_mi_is_aligned(mi_page_slice_start(page), MI_PAGE_ALIGN));
  mi_assert_internal(_mi_ptr_page(mi_page_start(page))==page);

  if (usable!=NULL) { *usable = mi_page_usable_block_size(page); }
  mi_prefetch_read(page->free);
  void* const p = _mi_page_malloc_zero(theap,page,size,zero);
  mi_assert_internal(p != NULL);

  if (mi_page_block_size(page) > MI_SMALL_MAX_OBJ_SIZE && mi_page_is_full(page)) {
    mi_page_to_full(page, mi_page_queue_of(page));
  }
  return p;
}
