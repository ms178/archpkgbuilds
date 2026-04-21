/* ----------------------------------------------------------------------------
Copyright (c) 2019-2024 Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/

/* ----------------------------------------------------------------------------
Concurrent bitmap that can set/reset sequences of bits atomically
---------------------------------------------------------------------------- */

#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/bits.h"
#include "mimalloc/prim.h"  // _mi_prim_thread_yield
#include "bitmap.h"

#ifndef MI_OPT_SIMD
#define MI_OPT_SIMD   0
#endif

#if defined(__x86_64__)
#include <immintrin.h>
#endif

/* --------------------------------------------------------------------------------
  bfields
-------------------------------------------------------------------------------- */

static inline size_t mi_bfield_ctz(mi_bfield_t x) {
  return mi_ctz(x);
}

static inline size_t mi_bfield_clz(mi_bfield_t x) {
  return mi_clz(x);
}

static inline size_t mi_bfield_popcount(mi_bfield_t x) {
  return mi_popcount(x);
}

static inline mi_bfield_t mi_bfield_clear_least_bit(mi_bfield_t x) {
#if defined(__x86_64__) && defined(__BMI__)
  #if MI_BFIELD_BITS == 64
  return (mi_bfield_t)_blsr_u64((uint64_t)x);
  #elif MI_BFIELD_BITS == 32
  return (mi_bfield_t)_blsr_u32((uint32_t)x);
  #else
  return (x & (x - 1));
  #endif
#else
  return (x & (x - 1));
#endif
}

// return false if x==0, true with idx set otherwise.
static inline bool mi_bfield_find_least_bit(mi_bfield_t x, size_t* idx) {
  if (x == 0) return false;
#if defined(__x86_64__) && defined(__BMI__)
  #if MI_BFIELD_BITS == 64
  *idx = (size_t)_tzcnt_u64((uint64_t)x);
  #elif MI_BFIELD_BITS == 32
  *idx = (size_t)_tzcnt_u32((uint32_t)x);
  #else
  *idx = mi_ctz(x);
  #endif
#else
  *idx = mi_ctz(x);
#endif
  return true;
}

// return false if x==0, true with idx set otherwise.
static inline bool mi_bfield_find_highest_bit(mi_bfield_t x, size_t* idx) {
  if (x == 0) return false;
#if defined(__x86_64__) && defined(__LZCNT__)
  #if MI_BFIELD_BITS == 64
  *idx = (size_t)(63u - (unsigned)_lzcnt_u64((uint64_t)x));
  #elif MI_BFIELD_BITS == 32
  *idx = (size_t)(31u - (unsigned)_lzcnt_u32((uint32_t)x));
  #else
  *idx = mi_clz(x);
  #endif
#else
  *idx = mi_bsr(x, idx) ? *idx : 0;
  if (*idx == 0 && x != 1) {
    size_t t = 0;
    (void)mi_bsr(x, &t);
    *idx = t;
  }
#endif
  return true;
}

static inline bool mi_bfield_foreach_bit(mi_bfield_t* x, size_t* idx) {
  const mi_bfield_t v = *x;
  if (v == 0) return false;
  (void)mi_bfield_find_least_bit(v, idx);
  *x = mi_bfield_clear_least_bit(v);
  return true;
}

static inline mi_bfield_t mi_bfield_zero(void) {
  return 0;
}

static inline mi_bfield_t mi_bfield_one(void) {
  return 1;
}

static inline mi_bfield_t mi_bfield_all_set(void) {
  return ~((mi_bfield_t)0);
}

// mask of `bit_count` bits set shifted to the left by `shiftl`
static inline mi_bfield_t mi_bfield_mask(size_t bit_count, size_t shiftl) {
  mi_assert_internal(bit_count > 0);
  mi_assert_internal(bit_count + shiftl <= MI_BFIELD_BITS);
  mi_assert_internal(shiftl < MI_BFIELD_BITS);

#if defined(__x86_64__) && defined(__BMI2__)
  #if MI_BFIELD_BITS == 64
  const mi_bfield_t low = (mi_bfield_t)_bzhi_u64(~UINT64_C(0), (unsigned int)bit_count);
  #elif MI_BFIELD_BITS == 32
  const mi_bfield_t low = (mi_bfield_t)_bzhi_u32(~UINT32_C(0), (unsigned int)bit_count);
  #else
  const mi_bfield_t low = (bit_count < MI_BFIELD_BITS ? ((mi_bfield_t)1 << bit_count) - 1 : ~((mi_bfield_t)0));
  #endif
  return (mi_bfield_t)(low << shiftl);
#else
  const mi_bfield_t mask0 = (bit_count < MI_BFIELD_BITS ? (((mi_bfield_t)1 << bit_count) - 1) : ~((mi_bfield_t)0));
  return (mi_bfield_t)(mask0 << shiftl);
#endif
}


// ------- mi_bfield_atomic_set ---------------------------------------

// Set a bit atomically. Returns `true` if the bit transitioned from 0 to 1
static inline bool mi_bfield_atomic_set(_Atomic(mi_bfield_t)*b, size_t idx) {
  mi_assert_internal(idx < MI_BFIELD_BITS);
  const mi_bfield_t mask = mi_bfield_mask(1, idx);
  const mi_bfield_t old = mi_atomic_or_acq_rel(b, mask);
  return ((old&mask) == 0);
}

// Clear a bit atomically. Returns `true` if the bit transitioned from 1 to 0.
// `all_clear` is set if the new bfield is zero.
static inline bool mi_bfield_atomic_clear(_Atomic(mi_bfield_t)*b, size_t idx, bool* all_clear) {
  mi_assert_internal(idx < MI_BFIELD_BITS);
  const mi_bfield_t mask = mi_bfield_mask(1, idx);
  mi_bfield_t old = mi_atomic_and_acq_rel(b, ~mask);
  if (all_clear != NULL) { *all_clear = ((old&~mask)==0); }
  return ((old&mask) == mask);
}

// Clear a bit but only when/once it is set.
static inline void mi_bfield_atomic_clear_once_set(_Atomic(mi_bfield_t)*b, size_t idx) {
  mi_assert_internal(idx < MI_BFIELD_BITS);
  const mi_bfield_t mask = mi_bfield_mask(1, idx);
  mi_bfield_t old = mi_atomic_load_relaxed(b);
  do {
    if mi_unlikely((old&mask) == 0) {
      old = mi_atomic_load_acquire(b);
      if ((old&mask)==0) {
        mi_subproc_stat_counter_increase(_mi_subproc(), pages_unabandon_busy_wait, 1);
      }
      while ((old&mask)==0) { // busy wait
        _mi_prim_thread_yield();
        old = mi_atomic_load_acquire(b);
      }
    }
  } while (!mi_atomic_cas_weak_acq_rel(b,&old, (old&~mask)));
  mi_assert_internal((old&mask)==mask);
}

static inline bool mi_bfield_atomic_set_mask(_Atomic(mi_bfield_t)* b, mi_bfield_t mask, size_t* already_set) {
  mi_assert_internal(mask != 0);
  const mi_bfield_t old = mi_atomic_or_acq_rel(b, mask);
  if (already_set != NULL) { *already_set = mi_bfield_popcount(old & mask); }
  return ((old & mask) == 0);
}

static inline bool mi_bfield_atomic_clear_mask(_Atomic(mi_bfield_t)* b, mi_bfield_t mask, bool* all_clear) {
  mi_assert_internal(mask != 0);
  const mi_bfield_t old = mi_atomic_and_acq_rel(b, ~mask);
  if (all_clear != NULL) { *all_clear = ((old & ~mask) == 0); }
  return ((old & mask) == mask);
}

static inline bool mi_bfield_atomic_setX(_Atomic(mi_bfield_t)*b, size_t* already_set) {
  const mi_bfield_t old = mi_atomic_exchange_release(b, mi_bfield_all_set());
  if (already_set!=NULL) { *already_set = mi_bfield_popcount(old); }
  return (old==0);
}


// ------- mi_bfield_atomic_try_clear ---------------------------------------

static inline bool mi_bfield_atomic_try_clear_mask_of(_Atomic(mi_bfield_t)*b, mi_bfield_t mask, mi_bfield_t expect, bool* all_clear) {
  mi_assert_internal(mask != 0);
  do {
    if ((expect & mask) != mask) {
      if (all_clear != NULL) { *all_clear = (expect == 0); }
      return false;
    }
  } while (!mi_atomic_cas_weak_acq_rel(b, &expect, expect & ~mask));
  if (all_clear != NULL) { *all_clear = ((expect & ~mask) == 0);  }
  return true;
}

static inline bool mi_bfield_atomic_try_clear_mask(_Atomic(mi_bfield_t)* b, mi_bfield_t mask, bool* all_clear) {
  mi_assert_internal(mask != 0);
  const mi_bfield_t expect = mi_atomic_load_relaxed(b);
  return mi_bfield_atomic_try_clear_mask_of(b, mask, expect, all_clear);
}

mi_decl_maybe_unused static inline bool mi_bfield_atomic_try_clear(_Atomic(mi_bfield_t)* b, size_t idx, bool* all_clear) {
  mi_assert_internal(idx < MI_BFIELD_BITS);
  const mi_bfield_t mask = mi_bfield_one()<<idx;
  return mi_bfield_atomic_try_clear_mask(b, mask, all_clear);
}

mi_decl_maybe_unused static inline bool mi_bfield_atomic_try_clear8(_Atomic(mi_bfield_t)*b, size_t idx, bool* all_clear) {
  mi_assert_internal(idx < MI_BFIELD_BITS);
  mi_assert_internal((idx%8)==0);
  const mi_bfield_t mask = ((mi_bfield_t)0xFF)<<idx;
  return mi_bfield_atomic_try_clear_mask(b, mask, all_clear);
}

static inline bool mi_bfield_atomic_try_clearX(_Atomic(mi_bfield_t)*b, bool* all_clear) {
  mi_bfield_t old = mi_bfield_all_set();
  if (mi_atomic_cas_strong_acq_rel(b, &old, mi_bfield_zero())) {
    if (all_clear != NULL) { *all_clear = true; }
    return true;
  }
  else return false;
}


// ------- mi_bfield_atomic_is_set ---------------------------------------

static inline bool mi_bfield_atomic_is_set(const _Atomic(mi_bfield_t)*b, const size_t idx) {
  const mi_bfield_t x = mi_atomic_load_relaxed(b);
  return ((x & mi_bfield_mask(1,idx)) != 0);
}

static inline bool mi_bfield_atomic_is_clear(const _Atomic(mi_bfield_t)*b, const size_t idx) {
  const mi_bfield_t x = mi_atomic_load_relaxed(b);
  return ((x & mi_bfield_mask(1, idx)) == 0);
}

static inline bool mi_bfield_atomic_is_xset(mi_xset_t set, const _Atomic(mi_bfield_t)*b, const size_t idx) {
  if (set) return mi_bfield_atomic_is_set(b, idx);
      else return mi_bfield_atomic_is_clear(b, idx);
}

static inline bool mi_bfield_atomic_is_set_mask(const _Atomic(mi_bfield_t)* b, mi_bfield_t mask) {
  mi_assert_internal(mask != 0);
  const mi_bfield_t x = mi_atomic_load_relaxed(b);
  return ((x & mask) == mask);
}

static inline bool mi_bfield_atomic_is_clear_mask(const _Atomic(mi_bfield_t)* b, mi_bfield_t mask) {
  mi_assert_internal(mask != 0);
  const mi_bfield_t x = mi_atomic_load_relaxed(b);
  return ((x & mask) == 0);
}

static inline bool mi_bfield_atomic_is_xset_mask(mi_xset_t set, const _Atomic(mi_bfield_t)* b, mi_bfield_t mask) {
  mi_assert_internal(mask != 0);
  if (set) return mi_bfield_atomic_is_set_mask(b, mask);
      else return mi_bfield_atomic_is_clear_mask(b, mask);
}

static inline size_t mi_bfield_atomic_popcount_mask(_Atomic(mi_bfield_t)*b, mi_bfield_t mask) {
  const mi_bfield_t x = mi_atomic_load_relaxed(b);
  return mi_bfield_popcount(x & mask);
}


/* --------------------------------------------------------------------------------
 bitmap chunks
-------------------------------------------------------------------------------- */

// ------- mi_bchunk_set ---------------------------------------

static inline bool mi_bchunk_set(mi_bchunk_t* chunk, size_t cidx, size_t* already_set) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  const bool was_clear = mi_bfield_atomic_set(&chunk->bfields[i], idx);
  if (already_set != NULL) { *already_set = (was_clear ? 0 : 1); }
  return was_clear;
}

static inline bool mi_bchunk_setNX(mi_bchunk_t* chunk, size_t cidx, size_t n, size_t* already_set) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  mi_assert_internal(n > 0 && n <= MI_BFIELD_BITS);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  if mi_likely(idx + n <= MI_BFIELD_BITS) {
    return mi_bfield_atomic_set_mask(&chunk->bfields[i], mi_bfield_mask(n,idx), already_set);
  }
  else {
    const size_t m = MI_BFIELD_BITS - idx;
    mi_assert_internal(m < n);
    mi_assert_internal(i < MI_BCHUNK_FIELDS - 1);
    mi_assert_internal(idx + m <= MI_BFIELD_BITS);
    size_t already_set1;
    const bool all_set1 = mi_bfield_atomic_set_mask(&chunk->bfields[i], mi_bfield_mask(m, idx), &already_set1);
    mi_assert_internal(n - m > 0);
    mi_assert_internal(n - m < MI_BFIELD_BITS);
    size_t already_set2;
    const bool all_set2 = mi_bfield_atomic_set_mask(&chunk->bfields[i+1], mi_bfield_mask(n - m, 0), &already_set2);
    if (already_set != NULL) { *already_set = already_set1 + already_set2; }
    return (all_set1 && all_set2);
  }
}

mi_decl_noinline static bool mi_bchunk_xsetNC(mi_xset_t set, mi_bchunk_t* chunk, size_t cidx, size_t n, size_t* palready_set, bool* pmaybe_all_clear) {
  mi_assert_internal(cidx + n <= MI_BCHUNK_BITS);
  mi_assert_internal(n>0);
  bool all_transition = true;
  bool maybe_all_clear = true;
  size_t total_already_set = 0;
  size_t idx   = cidx % MI_BFIELD_BITS;
  size_t field = cidx / MI_BFIELD_BITS;
  while (n > 0) {
    size_t m = MI_BFIELD_BITS - idx;
    if (m > n) { m = n; }
    mi_assert_internal(idx + m <= MI_BFIELD_BITS);
    mi_assert_internal(field < MI_BCHUNK_FIELDS);
    const mi_bfield_t mask = mi_bfield_mask(m, idx);
    size_t already_set = 0;
    bool all_clear = false;
    const bool transition = (set ? mi_bfield_atomic_set_mask(&chunk->bfields[field], mask, &already_set)
                                 : mi_bfield_atomic_clear_mask(&chunk->bfields[field], mask, &all_clear));
    mi_assert_internal((transition && already_set == 0) || (!transition && already_set > 0));
    all_transition = all_transition && transition;
    total_already_set += already_set;
    maybe_all_clear = maybe_all_clear && all_clear;
    field++;
    idx = 0;
    mi_assert_internal(m <= n);
    n -= m;
  }
  if (palready_set!=NULL) { *palready_set = total_already_set; }
  if (pmaybe_all_clear!=NULL) { *pmaybe_all_clear = maybe_all_clear; }
  return all_transition;
}

static inline bool mi_bchunk_setN(mi_bchunk_t* chunk, size_t cidx, size_t n, size_t* already_set) {
  mi_assert_internal(n>0 && n <= MI_BCHUNK_BITS);
  if (n==1) return mi_bchunk_set(chunk, cidx, already_set);
  if (n<=MI_BFIELD_BITS) return mi_bchunk_setNX(chunk, cidx, n, already_set);
  return mi_bchunk_xsetNC(MI_BIT_SET, chunk, cidx, n, already_set, NULL);
}

// ------- mi_bchunk_clear ---------------------------------------

static inline bool mi_bchunk_clear(mi_bchunk_t* chunk, size_t cidx, bool* all_clear) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  return mi_bfield_atomic_clear(&chunk->bfields[i], idx, all_clear);
}

static inline bool mi_bchunk_clearNX(mi_bchunk_t* chunk, size_t cidx, size_t n, bool* pmaybe_all_clear) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  mi_assert_internal(n > 0 && n <= MI_BFIELD_BITS);

  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;

  if mi_likely(idx + n <= MI_BFIELD_BITS) {
    return mi_bfield_atomic_clear_mask(&chunk->bfields[i], mi_bfield_mask(n, idx), pmaybe_all_clear);
  }
  else {
    const size_t m = MI_BFIELD_BITS - idx;
    mi_assert_internal(m < n);
    mi_assert_internal(i < MI_BCHUNK_FIELDS - 1);

    bool field1_is_clear = false;
    const bool all_set1 = mi_bfield_atomic_clear_mask(&chunk->bfields[i], mi_bfield_mask(m, idx), &field1_is_clear);

    bool field2_is_clear = false;
    const bool all_set2 = mi_bfield_atomic_clear_mask(&chunk->bfields[i + 1], mi_bfield_mask(n - m, 0), &field2_is_clear);

    if (pmaybe_all_clear != NULL) {
      *pmaybe_all_clear = (field1_is_clear && field2_is_clear);
    }
    return (all_set1 && all_set2);
  }
}

static inline bool mi_bchunk_clearN(mi_bchunk_t* chunk, size_t cidx, size_t n, bool* maybe_all_clear) {
  mi_assert_internal(n > 0 && n <= MI_BCHUNK_BITS);
  if (n == 1) return mi_bchunk_clear(chunk, cidx, maybe_all_clear);
  if (n <= MI_BFIELD_BITS) return mi_bchunk_clearNX(chunk, cidx, n, maybe_all_clear);
  return mi_bchunk_xsetNC(MI_BIT_CLEAR, chunk, cidx, n, NULL, maybe_all_clear);
}

// Check if a sequence of `n` bits within a chunk are all set/cleared.
mi_decl_noinline static size_t mi_bchunk_popcountNC(mi_bchunk_t* chunk, size_t field_idx, size_t idx, size_t n) {
  mi_assert_internal((field_idx*MI_BFIELD_BITS) + idx + n <= MI_BCHUNK_BITS);
  size_t count = 0;
  while (n > 0) {
    size_t m = MI_BFIELD_BITS - idx;
    if (m > n) { m = n; }
    mi_assert_internal(idx + m <= MI_BFIELD_BITS);
    mi_assert_internal(field_idx < MI_BCHUNK_FIELDS);
    const mi_bfield_t mask = mi_bfield_mask(m, idx);
    count += mi_bfield_atomic_popcount_mask(&chunk->bfields[field_idx], mask);
    field_idx++;
    idx = 0;
    n -= m;
  }
  return count;
}

static inline size_t mi_bchunk_popcountN(mi_bchunk_t* chunk, size_t cidx, size_t n) {
  mi_assert_internal(cidx + n <= MI_BCHUNK_BITS);
  mi_assert_internal(n>0);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  if (n==1) { return (mi_bfield_atomic_is_set(&chunk->bfields[i], idx) ? 1 : 0); }
  if (idx + n <= MI_BFIELD_BITS) { return mi_bfield_atomic_popcount_mask(&chunk->bfields[i], mi_bfield_mask(n, idx)); }
  return mi_bchunk_popcountNC(chunk, i, idx, n);
}

mi_decl_noinline static bool mi_bchunk_is_xsetNC(mi_xset_t set, const mi_bchunk_t* chunk, size_t field_idx, size_t idx, size_t n) {
  mi_assert_internal((field_idx*MI_BFIELD_BITS) + idx + n <= MI_BCHUNK_BITS);
  while (n > 0) {
    size_t m = MI_BFIELD_BITS - idx;
    if (m > n) { m = n; }
    mi_assert_internal(idx + m <= MI_BFIELD_BITS);
    mi_assert_internal(field_idx < MI_BCHUNK_FIELDS);
    const mi_bfield_t mask = mi_bfield_mask(m, idx);
    if (!mi_bfield_atomic_is_xset_mask(set, &chunk->bfields[field_idx], mask)) {
      return false;
    }
    field_idx++;
    idx = 0;
    n -= m;
  }
  return true;
}

static inline bool mi_bchunk_is_xsetN(mi_xset_t set, const mi_bchunk_t* chunk, size_t cidx, size_t n) {
  mi_assert_internal(cidx + n <= MI_BCHUNK_BITS);
  mi_assert_internal(n>0);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  if (n==1) { return mi_bfield_atomic_is_xset(set, &chunk->bfields[i], idx); }
  if (idx + n <= MI_BFIELD_BITS) { return mi_bfield_atomic_is_xset_mask(set, &chunk->bfields[i], mi_bfield_mask(n, idx)); }
  return mi_bchunk_is_xsetNC(set, chunk, i, idx, n);
}

// ------- mi_bchunk_try_clear  ---------------------------------------

// Clear `0 < n <= MI_BFIELD_BITS`. Can cross over a bfield boundary.
static inline bool mi_bchunk_try_clearNX(mi_bchunk_t* chunk, size_t cidx, size_t n, bool* pmaybe_all_clear) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  mi_assert_internal(n <= MI_BFIELD_BITS);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  if mi_likely(idx + n <= MI_BFIELD_BITS) {
    return mi_bfield_atomic_try_clear_mask(&chunk->bfields[i], mi_bfield_mask(n, idx), pmaybe_all_clear);
  }
  else {
    const size_t m = MI_BFIELD_BITS - idx;
    mi_assert_internal(m < n);
    mi_assert_internal(i < MI_BCHUNK_FIELDS - 1);
    bool field1_is_clear;
    if (!mi_bfield_atomic_try_clear_mask(&chunk->bfields[i], mi_bfield_mask(m, idx), &field1_is_clear)) return false;

    mi_assert_internal(n - m > 0);
    mi_assert_internal(n - m < MI_BFIELD_BITS);
    bool field2_is_clear;
    if (!mi_bfield_atomic_try_clear_mask(&chunk->bfields[i+1], mi_bfield_mask(n - m, 0), &field2_is_clear)) {
      mi_bfield_atomic_set_mask(&chunk->bfields[i], mi_bfield_mask(m, idx), NULL);
      return false;
    }
    if (pmaybe_all_clear != NULL) { *pmaybe_all_clear = field1_is_clear && field2_is_clear; }
    return true;
  }
}

mi_decl_noinline static bool mi_bchunk_try_clearNC(mi_bchunk_t* chunk, size_t cidx, size_t n, bool* pmaybe_all_clear) {
  mi_assert_internal(cidx + n <= MI_BCHUNK_BITS);
  mi_assert_internal(n > 0);
  if (pmaybe_all_clear != NULL) { *pmaybe_all_clear = true; }

  const size_t start_idx = cidx % MI_BFIELD_BITS;
  const size_t start_field = cidx / MI_BFIELD_BITS;
  size_t field = start_field;
  size_t m = MI_BFIELD_BITS - start_idx;
  if (m > n) { m = n; }

  mi_assert_internal(start_idx + m <= MI_BFIELD_BITS);
  mi_assert_internal(start_field < MI_BCHUNK_FIELDS);

  const mi_bfield_t mask_start = mi_bfield_mask(m, start_idx);
  bool maybe_all_clear;
  if (!mi_bfield_atomic_try_clear_mask(&chunk->bfields[field], mask_start, &maybe_all_clear)) return false;

  n -= m;

  while (n >= MI_BFIELD_BITS) {
    field++;
    mi_assert_internal(field < MI_BCHUNK_FIELDS);
    bool field_is_clear;
    if (!mi_bfield_atomic_try_clearX(&chunk->bfields[field], &field_is_clear)) goto restore;
    maybe_all_clear = maybe_all_clear && field_is_clear;
    n -= MI_BFIELD_BITS;
  }

  if (n > 0) {
    mi_assert_internal(n < MI_BFIELD_BITS);
    field++;
    mi_assert_internal(field < MI_BCHUNK_FIELDS);
    const mi_bfield_t mask_end = mi_bfield_mask(n, 0);
    bool field_is_clear;
    if (!mi_bfield_atomic_try_clear_mask(&chunk->bfields[field], mask_end, &field_is_clear)) goto restore;
    maybe_all_clear = maybe_all_clear && field_is_clear;
  }

  if (pmaybe_all_clear != NULL) { *pmaybe_all_clear = maybe_all_clear; }
  return true;

restore:
  mi_assert_internal(field > start_field);
  while (field > start_field) {
    field--;
    if (field == start_field) {
      mi_bfield_atomic_set_mask(&chunk->bfields[field], mask_start, NULL);
    }
    else {
      mi_bfield_atomic_setX(&chunk->bfields[field], NULL);
    }
  }
  return false;
}

static inline bool mi_bchunk_try_clearN(mi_bchunk_t* chunk, size_t cidx, size_t n, bool* maybe_all_clear) {
  mi_assert_internal(n > 0);
  if (n <= MI_BFIELD_BITS) return mi_bchunk_try_clearNX(chunk, cidx, n, maybe_all_clear);
  return mi_bchunk_try_clearNC(chunk, cidx, n, maybe_all_clear);
}


// ------- mi_bchunk_try_find_and_clear ---------------------------------------

#if MI_OPT_SIMD && defined(__AVX2__)
mi_decl_maybe_unused static inline __m256i mi_mm256_zero(void) {
  return _mm256_setzero_si256();
}
mi_decl_maybe_unused static inline __m256i mi_mm256_ones(void) {
  return _mm256_set1_epi64x(~0);
}
mi_decl_maybe_unused static inline bool mi_mm256_is_ones(__m256i vec) {
  return _mm256_testc_si256(vec, _mm256_cmpeq_epi32(vec, vec));
}
mi_decl_maybe_unused static inline bool mi_mm256_is_zero(__m256i vec) {
  return _mm256_testz_si256(vec, vec);
}
#endif

static inline bool mi_bchunk_try_find_and_clear_at(mi_bchunk_t* chunk, size_t chunk_idx, size_t* pidx) {
  mi_assert_internal(chunk_idx < MI_BCHUNK_FIELDS);
  const mi_bfield_t b = mi_atomic_load_acquire(&chunk->bfields[chunk_idx]);
  size_t idx;
  if (mi_bfield_find_least_bit(b, &idx)) {
    if mi_likely(mi_bfield_atomic_try_clear_mask_of(&chunk->bfields[chunk_idx], mi_bfield_mask(1, idx), b, NULL)) {
      *pidx = (chunk_idx * MI_BFIELD_BITS) + idx;
      mi_assert_internal(*pidx < MI_BCHUNK_BITS);
      return true;
    }
  }
  return false;
}

static inline bool mi_bchunk_try_find_and_clear(mi_bchunk_t* chunk, size_t* pidx) {
  #if MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==256)
  for (int tries = 0; tries < 4; tries++) {
    const __m256i vec = _mm256_load_si256((const __m256i*)chunk->bfields);
    const __m256i vcmp = _mm256_cmpeq_epi64(vec, mi_mm256_zero());
    const uint32_t mask = ~_mm256_movemask_epi8(vcmp);
    if (mask == 0) return false;
    mi_assert_internal((_tzcnt_u32(mask) % 8) == 0);
    const size_t chunk_idx = _tzcnt_u32(mask) / 8;
    if (mi_bchunk_try_find_and_clear_at(chunk, chunk_idx, pidx)) return true;
    #if defined(__GNUC__)
    __asm __volatile ("" : : "g"(chunk) : "memory");
    #endif
  }
  #elif MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==512)
  for (int tries = 0; tries < 4; tries++) {
    const __m256i vec1  = _mm256_load_si256((const __m256i*)chunk->bfields);
    const __m256i vec2  = _mm256_load_si256(((const __m256i*)chunk->bfields) + 1);
    const __m256i cmpv  = mi_mm256_zero();
    const __m256i vcmp1 = _mm256_cmpeq_epi64(vec1, cmpv);
    const __m256i vcmp2 = _mm256_cmpeq_epi64(vec2, cmpv);
    const uint32_t mask1 = ~_mm256_movemask_epi8(vcmp1);
    const uint32_t mask2 = ~_mm256_movemask_epi8(vcmp2);
    const uint64_t mask = ((uint64_t)mask2 << 32) | mask1;
    if (mask == 0) return false;
    mi_assert_internal((_tzcnt_u64(mask) % 8) == 0);
    const size_t chunk_idx = mi_ctz(mask) / 8;
    if (mi_bchunk_try_find_and_clear_at(chunk, chunk_idx, pidx)) return true;
    #if defined(__GNUC__)
    __asm __volatile ("" : : "g"(chunk) : "memory");
    #endif
  }
  #else
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    if (mi_bchunk_try_find_and_clear_at(chunk, i, pidx)) return true;
  }
  #endif
  return false;
}

static inline bool mi_bchunk_try_find_and_clear_1(mi_bchunk_t* chunk, size_t n, size_t* pidx) {
  mi_assert_internal(n == 1); MI_UNUSED(n);
  return mi_bchunk_try_find_and_clear(chunk, pidx);
}

mi_decl_maybe_unused static inline bool mi_bchunk_try_find_and_clear8_at(mi_bchunk_t* chunk, size_t chunk_idx, size_t* pidx) {
  const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[chunk_idx]);
  const mi_bfield_t has_set8 =
    ((~b - MI_BFIELD_LO_BIT8) &
     (b  & MI_BFIELD_HI_BIT8)) >> 7;
  size_t idx;
  if (mi_bfield_find_least_bit(has_set8, &idx)) {
    mi_assert_internal(idx <= (MI_BFIELD_BITS - 8));
    mi_assert_internal((idx % 8) == 0);
    if mi_likely(mi_bfield_atomic_try_clear_mask_of(&chunk->bfields[chunk_idx], ((mi_bfield_t)0xFF) << idx, b, NULL)) {
      *pidx = (chunk_idx * MI_BFIELD_BITS) + idx;
      mi_assert_internal(*pidx + 8 <= MI_BCHUNK_BITS);
      return true;
    }
  }
  return false;
}

static mi_decl_noinline bool mi_bchunk_try_find_and_clear8(mi_bchunk_t* chunk, size_t* pidx) {
  #if MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==512)
  while (true) {
    const __m256i vec1 = _mm256_load_si256((const __m256i*)chunk->bfields);
    const __m256i vec2 = _mm256_load_si256(((const __m256i*)chunk->bfields) + 1);
    const __m256i cmpv = mi_mm256_ones();
    const __m256i vcmp1 = _mm256_cmpeq_epi8(vec1, cmpv);
    const __m256i vcmp2 = _mm256_cmpeq_epi8(vec2, cmpv);
    const uint32_t mask1 = _mm256_movemask_epi8(vcmp1);
    const uint32_t mask2 = _mm256_movemask_epi8(vcmp2);
    const uint64_t mask = ((uint64_t)mask2 << 32) | mask1;
    if (mask == 0) return false;
    const size_t bidx = _tzcnt_u64(mask);
    const size_t chunk_idx = bidx / 8;
    const size_t idx = (bidx % 8) * 8;
    mi_assert_internal(chunk_idx < MI_BCHUNK_FIELDS);
    if mi_likely(mi_bfield_atomic_try_clear8(&chunk->bfields[chunk_idx], idx, NULL)) {
      *pidx = (chunk_idx * MI_BFIELD_BITS) + idx;
      mi_assert_internal(*pidx + 8 <= MI_BCHUNK_BITS);
      return true;
    }
    #if defined(__GNUC__)
    __asm __volatile ("" : : "g"(chunk) : "memory");
    #endif
  }
  #else
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    if (mi_bchunk_try_find_and_clear8_at(chunk, i, pidx)) return true;
  }
  return false;
  #endif
}

static inline bool mi_bchunk_try_find_and_clear_8(mi_bchunk_t* chunk, size_t n, size_t* pidx) {
  mi_assert_internal(n == 8); MI_UNUSED(n);
  return mi_bchunk_try_find_and_clear8(chunk, pidx);
}

mi_decl_noinline static bool mi_bchunk_try_find_and_clearNX(mi_bchunk_t* chunk, size_t n, size_t* pidx) {
  if (n == 0 || n > MI_BFIELD_BITS) return false;
  const mi_bfield_t mask = mi_bfield_mask(n, 0);
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    mi_bfield_t b0 = mi_atomic_load_relaxed(&chunk->bfields[i]);
    mi_bfield_t b = b0;
    size_t idx;

    while (mi_bfield_find_least_bit(b, &idx)) {
      if (idx + n > MI_BFIELD_BITS) break;

      const mi_bfield_t bmask = mask << idx;
      mi_assert_internal((bmask >> idx) == mask);
      if ((b & bmask) == bmask) {
        if mi_likely(mi_bfield_atomic_try_clear_mask_of(&chunk->bfields[i], bmask, b0, NULL)) {
          *pidx = (i * MI_BFIELD_BITS) + idx;
          mi_assert_internal(*pidx < MI_BCHUNK_BITS);
          mi_assert_internal(*pidx + n <= MI_BCHUNK_BITS);
          return true;
        }
        else {
          b = b0 = mi_atomic_load_acquire(&chunk->bfields[i]);
        }
      }
      else {
        b = b & (b + (mi_bfield_one() << idx));
      }
    }

    if (b != 0 && i < MI_BCHUNK_FIELDS - 1) {
      const size_t post = mi_bfield_clz(~b);
      if (post > 0) {
        const size_t pre = mi_bfield_ctz(~mi_atomic_load_relaxed(&chunk->bfields[i+1]));
        if (post + pre >= n) {
          const size_t cidx = (i * MI_BFIELD_BITS) + (MI_BFIELD_BITS - post);
          if (mi_bchunk_try_clearNX(chunk, cidx, n, NULL)) {
            *pidx = cidx;
            mi_assert_internal(*pidx < MI_BCHUNK_BITS);
            mi_assert_internal(*pidx + n <= MI_BCHUNK_BITS);
            return true;
          }
        }
      }
    }
  }
  return false;
}

static mi_decl_noinline bool mi_bchunk_try_find_and_clearNC(mi_bchunk_t* chunk, size_t n, size_t* pidx) {
  if (n == 0 || n > MI_BCHUNK_BITS) return false;

  const size_t skip_count = (n - 1) / MI_BFIELD_BITS;
  size_t cidx;
  for (size_t i = 0; i < MI_BCHUNK_FIELDS - skip_count; i++) {
    size_t m = n;

    mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[i]);
    size_t ones = mi_bfield_clz(~b);

    cidx = (i * MI_BFIELD_BITS) + (MI_BFIELD_BITS - ones);
    if (ones >= m) {
      m = 0;
    }
    else if (ones > 0) {
      m -= ones;
      size_t j = 1;
      while (i + j < MI_BCHUNK_FIELDS) {
        mi_assert_internal(m > 0);
        b = mi_atomic_load_relaxed(&chunk->bfields[i + j]);
        ones = mi_bfield_ctz(~b);
        if (ones >= m) {
          m = 0;
          break;
        }
        else if (ones == MI_BFIELD_BITS) {
          j++;
          m -= MI_BFIELD_BITS;
        }
        else {
          i = i + j - 1;
          break;
        }
      }
    }

    if (m == 0) {
      if (mi_bchunk_try_clearN(chunk, cidx, n, NULL)) {
        *pidx = cidx;
        mi_assert_internal(*pidx < MI_BCHUNK_BITS);
        mi_assert_internal(*pidx + n <= MI_BCHUNK_BITS);
        return true;
      }
    }
  }
  return false;
}


// ------- mi_bchunk_clear_once_set ---------------------------------------

static inline void mi_bchunk_clear_once_set(mi_bchunk_t* chunk, size_t cidx) {
  mi_assert_internal(cidx < MI_BCHUNK_BITS);
  const size_t i = cidx / MI_BFIELD_BITS;
  const size_t idx = cidx % MI_BFIELD_BITS;
  mi_bfield_atomic_clear_once_set(&chunk->bfields[i], idx);
}


// ------- all-are-clear/set ---------------------------------------

static inline bool mi_bchunk_all_are_clear_relaxed(mi_bchunk_t* chunk) {
  #if MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==256)
  const __m256i vec = _mm256_load_si256((const __m256i*)chunk->bfields);
  return mi_mm256_is_zero(vec);
  #elif MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==512)
  const __m256i vec1 = _mm256_load_si256((const __m256i*)chunk->bfields);
  const __m256i vec2 = _mm256_load_si256(((const __m256i*)chunk->bfields)+1);
  return (mi_mm256_is_zero(_mm256_or_si256(vec1,vec2)));
  #else
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    if (mi_atomic_load_relaxed(&chunk->bfields[i]) != 0) return false;
  }
  return true;
  #endif
}

static inline bool mi_bchunk_all_are_set_relaxed(mi_bchunk_t* chunk) {
  #if MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==256)
  const __m256i vec = _mm256_load_si256((const __m256i*)chunk->bfields);
  return mi_mm256_is_ones(vec);
  #elif MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==512)
  const __m256i vec1 = _mm256_load_si256((const __m256i*)chunk->bfields);
  const __m256i vec2 = _mm256_load_si256(((const __m256i*)chunk->bfields)+1);
  return (mi_mm256_is_ones(_mm256_and_si256(vec1, vec2)));
  #else
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    if (~mi_atomic_load_relaxed(&chunk->bfields[i]) != 0) return false;
  }
  return true;
  #endif
}

static bool mi_bchunk_bsr(mi_bchunk_t* chunk, size_t* pidx) {
  for (size_t i = MI_BCHUNK_FIELDS; i > 0; ) {
    i--;
    const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[i]);
    size_t idx;
    if (mi_bsr(b, &idx)) {
      *pidx = (i * MI_BFIELD_BITS) + idx;
      return true;
    }
  }
  return false;
}

// Scan a chunk for the highest clear bit (inverse bsr).
static bool mi_bchunk_bsr_inv(mi_bchunk_t* chunk, size_t* pidx) {
#if MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==512)
  const __m256i ones = _mm256_set1_epi64x(~INT64_C(0));
  const __m256i v1 = _mm256_load_si256((const __m256i*)chunk->bfields);
  const __m256i v2 = _mm256_load_si256(((const __m256i*)chunk->bfields) + 1);

  // lane == all-ones => FF bytes; invert movemask => bytes FF where lane has at least one clear bit
  const uint32_t m1 = (uint32_t)~_mm256_movemask_epi8(_mm256_cmpeq_epi64(v1, ones));
  const uint32_t m2 = (uint32_t)~_mm256_movemask_epi8(_mm256_cmpeq_epi64(v2, ones));
  const uint64_t m = ((uint64_t)m2 << 32) | (uint64_t)m1;

  if (m != 0) {
    size_t hi;
    (void)mi_bfield_find_highest_bit((mi_bfield_t)m, &hi);
    const size_t field_hint = hi / 8; // 8 mask-bits per 64-bit field
    if (field_hint < MI_BCHUNK_FIELDS) {
      const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[field_hint]);
      size_t idx;
      if (mi_bfield_find_highest_bit(~b, &idx)) {
        *pidx = (field_hint * MI_BFIELD_BITS) + idx;
        return true;
      }
    }
  }
#elif MI_OPT_SIMD && defined(__AVX2__) && (MI_BCHUNK_BITS==256)
  const __m256i ones = _mm256_set1_epi64x(~INT64_C(0));
  const __m256i v1 = _mm256_load_si256((const __m256i*)chunk->bfields);
  const uint32_t m = (uint32_t)~_mm256_movemask_epi8(_mm256_cmpeq_epi64(v1, ones));
  if (m != 0) {
    size_t hi;
    (void)mi_bfield_find_highest_bit((mi_bfield_t)m, &hi);
    const size_t field_hint = hi / 8;
    if (field_hint < MI_BCHUNK_FIELDS) {
      const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[field_hint]);
      size_t idx;
      if (mi_bfield_find_highest_bit(~b, &idx)) {
        *pidx = (field_hint * MI_BFIELD_BITS) + idx;
        return true;
      }
    }
  }
#endif

  // Scalar fallback / race-safe fallback
  for (size_t i = MI_BCHUNK_FIELDS; i > 0; ) {
    i--;
    const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[i]);
    size_t idx;
    if (mi_bfield_find_highest_bit(~b, &idx)) {
      *pidx = (i * MI_BFIELD_BITS) + idx;
      return true;
    }
  }
  return false;
}

static size_t mi_bchunk_popcount(mi_bchunk_t* chunk) {
  size_t popcount = 0;
  for (size_t i = 0; i < MI_BCHUNK_FIELDS; i++) {
    const mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[i]);
    popcount += mi_bfield_popcount(b);
  }
  return popcount;
}


/* --------------------------------------------------------------------------------
 bitmap chunkmap
-------------------------------------------------------------------------------- */

static void mi_bitmap_chunkmap_set(mi_bitmap_t* bitmap, size_t chunk_idx) {
  mi_assert(chunk_idx < mi_bitmap_chunk_count(bitmap));
  mi_bchunk_set(&bitmap->chunkmap, chunk_idx, NULL);
}

static bool mi_bitmap_chunkmap_try_clear(mi_bitmap_t* bitmap, size_t chunk_idx) {
  mi_assert(chunk_idx < mi_bitmap_chunk_count(bitmap));
  if (!mi_bchunk_all_are_clear_relaxed(&bitmap->chunks[chunk_idx])) return false;
  mi_bchunk_clear(&bitmap->chunkmap, chunk_idx, NULL);
  if (!mi_bchunk_all_are_clear_relaxed(&bitmap->chunks[chunk_idx])) {
    mi_bchunk_set(&bitmap->chunkmap, chunk_idx, NULL);
    return false;
  }
  return true;
}


/* --------------------------------------------------------------------------------
  bitmap
-------------------------------------------------------------------------------- */

size_t mi_bitmap_size(size_t bit_count, size_t* pchunk_count) {
  bit_count = _mi_align_up(bit_count, MI_BCHUNK_BITS);
  mi_assert_internal(bit_count <= MI_BITMAP_MAX_BIT_COUNT);
  mi_assert_internal(bit_count > 0);
  const size_t chunk_count = bit_count / MI_BCHUNK_BITS;
  mi_assert_internal(chunk_count >= 1);
  const size_t size = offsetof(mi_bitmap_t,chunks) + (chunk_count * MI_BCHUNK_SIZE);
  mi_assert_internal((size%MI_BCHUNK_SIZE) == 0);
  if (pchunk_count != NULL) { *pchunk_count = chunk_count; }
  return size;
}

size_t mi_bitmap_init(mi_bitmap_t* bitmap, size_t bit_count, bool already_zero) {
  size_t chunk_count;
  const size_t size = mi_bitmap_size(bit_count, &chunk_count);
  if (!already_zero) {
    _mi_memzero_aligned(bitmap, size);
  }
  mi_atomic_store_release(&bitmap->chunk_count, chunk_count);
  mi_assert_internal(mi_atomic_load_relaxed(&bitmap->chunk_count) <= MI_BITMAP_MAX_CHUNK_COUNT);
  return size;
}

static void mi_bchunks_unsafe_setN(mi_bchunk_t* chunks, mi_bchunkmap_t* cmap, size_t idx, size_t n) {
  mi_assert_internal(n>0);

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  const size_t cidx = idx % MI_BCHUNK_BITS;
  const size_t ccount = _mi_divide_up(n, MI_BCHUNK_BITS);

  mi_bchunk_setN(cmap, chunk_idx, ccount, NULL);

  size_t m = MI_BCHUNK_BITS - cidx;
  if (m > n) { m = n; }
  mi_bchunk_setN(&chunks[chunk_idx], cidx, m, NULL);

  chunk_idx++;
  n -= m;
  const size_t mid_chunks = n / MI_BCHUNK_BITS;
  if (mid_chunks > 0) {
    _mi_memset(&chunks[chunk_idx], ~0, mid_chunks * MI_BCHUNK_SIZE);
    chunk_idx += mid_chunks;
    n -= (mid_chunks * MI_BCHUNK_BITS);
  }

  if (n > 0) {
    mi_assert_internal(n < MI_BCHUNK_BITS);
    mi_bchunk_setN(&chunks[chunk_idx], 0, n, NULL);
  }
}

void mi_bitmap_unsafe_setN(mi_bitmap_t* bitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  mi_assert_internal(idx + n <= mi_bitmap_max_bits(bitmap));
  mi_bchunks_unsafe_setN(&bitmap->chunks[0], &bitmap->chunkmap, idx, n);
}

bool mi_bitmap_setN(mi_bitmap_t* bitmap, size_t idx, size_t n, size_t* palready_set) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bitmap_max_bits(bitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return false;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  bool were_allclear = true;
  size_t already_set = 0;
  while (n > 0) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    size_t _already_set = 0;
    were_allclear = mi_bchunk_setN(&bitmap->chunks[chunk_idx], cidx, m, &_already_set) && were_allclear;
    already_set += _already_set;
    mi_bitmap_chunkmap_set(bitmap, chunk_idx);
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  if (palready_set != NULL) { *palready_set = already_set; }
  return were_allclear;
}

bool mi_bitmap_clearN(mi_bitmap_t* bitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bitmap_max_bits(bitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return false;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  bool were_allset = true;
  while (n > 0) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    bool maybe_all_clear = false;
    were_allset = mi_bchunk_clearN(&bitmap->chunks[chunk_idx], cidx, m, &maybe_all_clear) && were_allset;
    if (maybe_all_clear) { mi_bitmap_chunkmap_try_clear(bitmap, chunk_idx); }
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  return were_allset;
}

size_t mi_bitmap_popcountN(mi_bitmap_t* bitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bitmap_max_bits(bitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return 0;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  size_t popcount = 0;
  while (n > 0) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    popcount += mi_bchunk_popcountN(&bitmap->chunks[chunk_idx], cidx, m);
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  return popcount;
}

bool mi_bitmap_set(mi_bitmap_t* bitmap, size_t idx) {
  return mi_bitmap_setN(bitmap, idx, 1, NULL);
}

bool mi_bitmap_clear(mi_bitmap_t* bitmap, size_t idx) {
  return mi_bitmap_clearN(bitmap, idx, 1);
}

bool mi_bitmap_is_xsetN(mi_xset_t set, mi_bitmap_t* bitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bitmap_max_bits(bitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return false;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  bool xset = true;
  while (n > 0 && xset) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    xset = mi_bchunk_is_xsetN(set, &bitmap->chunks[chunk_idx], cidx, m) && xset;
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  return xset;
}

bool mi_bitmap_is_all_clear(mi_bitmap_t* bitmap) {
  return mi_bitmap_is_xsetN(MI_BIT_CLEAR, bitmap, 0, mi_bitmap_max_bits(bitmap));
}


/* --------------------------------------------------------------------------------
  Iterate through a bfield
-------------------------------------------------------------------------------- */

#define mi_bfield_iterate(bfield,start,cycle,name_idx,SUF) { \
  mi_assert_internal(start <= cycle); \
  mi_assert_internal(start < MI_BFIELD_BITS); \
  mi_assert_internal(cycle <= MI_BFIELD_BITS); \
  const mi_bfield_t _cycle_mask##SUF = mi_bfield_mask(cycle - start, start); \
  size_t _bcount##SUF = mi_bfield_popcount(bfield); \
  mi_bfield_t _b##SUF = bfield & _cycle_mask##SUF; \
  while(_bcount##SUF > 0) { \
    _bcount##SUF--; \
    if (_b##SUF==0) { _b##SUF = bfield & ~_cycle_mask##SUF; } \
    const bool _found##SUF = mi_bfield_find_least_bit(_b##SUF,&name_idx); \
    _b##SUF = mi_bfield_clear_least_bit(_b##SUF); \
    mi_assert_internal(_found##SUF); MI_UNUSED(_found##SUF); \
    {

#define mi_bfield_iterate_end(SUF) \
    } \
  } \
}

#define mi_bfield_cycle_iterate(bfield,tseq,cycle,name_idx,SUF) { \
  const size_t _start##SUF = (uint32_t)(tseq) % (uint32_t)(cycle); \
  mi_bfield_iterate(bfield,_start##SUF,cycle,name_idx,SUF)

#define mi_bfield_cycle_iterate_end(SUF) \
  mi_bfield_iterate_end(SUF); \
}


/* --------------------------------------------------------------------------------
  mi_bitmap_find
-------------------------------------------------------------------------------- */

typedef bool (mi_bitmap_visit_fun_t)(mi_bitmap_t* bitmap, size_t chunk_idx, size_t n, size_t* idx, void* arg1, void* arg2);

static inline bool mi_bitmap_find(mi_bitmap_t* bitmap, size_t tseq, size_t n, size_t* pidx, mi_bitmap_visit_fun_t* on_find, void* arg1, void* arg2)
{
  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for (size_t i = 0; i < chunkmap_max; i++) {
    const mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t hi;
    if (mi_bfield_find_highest_bit(cmap_entry, &hi)) {
      size_t eidx = 0;
      mi_bfield_cycle_iterate(cmap_entry, tseq%8, hi+1, eidx, Y)
      {
        const size_t chunk_idx = i*MI_BFIELD_BITS + eidx;
        mi_assert_internal(chunk_idx < mi_bitmap_chunk_count(bitmap));
        if ((*on_find)(bitmap, chunk_idx, n, pidx, arg1, arg2)) {
          return true;
        }
      }
      mi_bfield_cycle_iterate_end(Y);
    }
  }
  return false;
}

typedef struct mi_claim_fun_data_s {
  mi_arena_t* arena;
} mi_claim_fun_data_t;

static bool mi_bitmap_try_find_and_claim_visit(mi_bitmap_t* bitmap, size_t chunk_idx, size_t n, size_t* pidx, void* arg1, void* arg2)
{
  mi_assert_internal(n==1); MI_UNUSED(n);
  mi_claim_fun_t* claim_fun = (mi_claim_fun_t*)arg1;
  mi_claim_fun_data_t* claim_data = (mi_claim_fun_data_t*)arg2;
  size_t cidx;
  if mi_likely(mi_bchunk_try_find_and_clear(&bitmap->chunks[chunk_idx], &cidx)) {
    const size_t slice_index = (chunk_idx * MI_BCHUNK_BITS) + cidx;
    mi_assert_internal(slice_index < mi_bitmap_max_bits(bitmap));
    bool keep_set = true;
    if ((*claim_fun)(slice_index, claim_data->arena, &keep_set)) {
      mi_assert_internal(!keep_set);
      *pidx = slice_index;
      return true;
    }
    else if (keep_set) {
      const bool wasclear = mi_bchunk_set(&bitmap->chunks[chunk_idx], cidx, NULL);
      mi_assert_internal(wasclear); MI_UNUSED(wasclear);
    }
  }
  else {
    mi_bitmap_chunkmap_try_clear(bitmap, chunk_idx);
  }
  return false;
}

mi_decl_nodiscard bool mi_bitmap_try_find_and_claim(mi_bitmap_t* bitmap, size_t tseq, size_t* pidx,
  mi_claim_fun_t* claim, mi_arena_t* arena )
{
  mi_claim_fun_data_t claim_data = { arena };
  return mi_bitmap_find(bitmap, tseq, 1, pidx, &mi_bitmap_try_find_and_claim_visit, (void*)claim, &claim_data);
}

bool mi_bitmap_bsr(mi_bitmap_t* bitmap, size_t* idx) {
  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for (size_t i = chunkmap_max; i > 0; ) {
    i--;
    const mi_bfield_t cmap = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t cmap_idx;
    if (mi_bsr(cmap,&cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      size_t cidx;
      if (mi_bchunk_bsr(&bitmap->chunks[chunk_idx], &cidx)) {
        *idx = (chunk_idx * MI_BCHUNK_BITS) + cidx;
        return true;
      }
    }
  }
  return false;
}

size_t mi_bitmap_popcount(mi_bitmap_t* bitmap) {
  size_t popcount = 0;
  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for (size_t i = 0; i < chunkmap_max; i++) {
    mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t cmap_idx;
    while (mi_bfield_foreach_bit(&cmap_entry, &cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      popcount += mi_bchunk_popcount(&bitmap->chunks[chunk_idx]);
    }
  }
  return popcount;
}

void mi_bitmap_clear_once_set(mi_bitmap_t* bitmap, size_t idx) {
  mi_assert_internal(idx < mi_bitmap_max_bits(bitmap));
  const size_t chunk_idx = idx / MI_BCHUNK_BITS;
  const size_t cidx = idx % MI_BCHUNK_BITS;
  mi_assert_internal(chunk_idx < mi_bitmap_chunk_count(bitmap));
  mi_bchunk_clear_once_set(&bitmap->chunks[chunk_idx], cidx);
}

bool _mi_bitmap_forall_set(mi_bitmap_t* bitmap, mi_forall_set_fun_t* visit, mi_arena_t* arena, void* arg) {
  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for(size_t i = 0; i < chunkmap_max; i++) {
    mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t cmap_idx;
    while (mi_bfield_foreach_bit(&cmap_entry, &cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      mi_bchunk_t* const chunk = &bitmap->chunks[chunk_idx];
      for (size_t j = 0; j < MI_BCHUNK_FIELDS; j++) {
        const size_t base_idx = (chunk_idx*MI_BCHUNK_BITS) + (j*MI_BFIELD_BITS);
        mi_bfield_t b = mi_atomic_load_relaxed(&chunk->bfields[j]);
        size_t bidx;
        while (mi_bfield_foreach_bit(&b, &bidx)) {
          const size_t idx = base_idx + bidx;
          if (!visit(idx, 1, arena, arg)) return false;
        }
      }
    }
  }
  return true;
}

bool _mi_bitmap_forall_setc_ranges(mi_bitmap_t* bitmap, mi_forall_set_fun_t* visit, mi_arena_t* arena, void* arg) {
  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for (size_t i = 0; i < chunkmap_max; i++) {
    mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t cmap_idx;
    while (mi_bfield_foreach_bit(&cmap_entry, &cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      mi_bchunk_t* const chunk = &bitmap->chunks[chunk_idx];
      for (size_t j = 0; j < MI_BCHUNK_FIELDS; j++) {
        const size_t base_idx = (chunk_idx*MI_BCHUNK_BITS) + (j*MI_BFIELD_BITS);
        mi_bfield_t b = mi_atomic_exchange_relaxed(&chunk->bfields[j], (mi_bfield_t)0);
        #if MI_DEBUG > 1
        const size_t bpopcount = mi_popcount(b);
        size_t rngcount = 0;
        #endif
        size_t bidx;
        while (mi_bfield_find_least_bit(b, &bidx)) {
          size_t rng = mi_ctz(~(b>>bidx));
          #if MI_DEBUG > 1
          rngcount += rng;
          #endif
          const size_t idx = base_idx + bidx;
          mi_assert_internal(rng>=1 && rng<=MI_BFIELD_BITS);
          mi_assert_internal((idx % MI_BFIELD_BITS) + rng <= MI_BFIELD_BITS);
          mi_assert_internal((idx / MI_BCHUNK_BITS) < mi_bitmap_chunk_count(bitmap));
          if (!visit(idx, rng, arena, arg)) {
            if (b != 0) {
              mi_atomic_or_relaxed(&chunk->bfields[j], b);
            }
            return false;
          }
          b = b & ~mi_bfield_mask(rng, bidx);
        }
        #if MI_DEBUG > 1
        mi_assert_internal(rngcount == bpopcount);
        #endif
      }
    }
  }
  return true;
}

bool _mi_bitmap_forall_setc_rangesn(mi_bitmap_t* bitmap, size_t rngslices, mi_forall_set_fun_t* visit, mi_arena_t* arena, void* arg)
{
  if (rngslices<=1) {
    return _mi_bitmap_forall_setc_ranges(bitmap, visit, arena, arg);
  }
  if (rngslices > MI_BFIELD_BITS) { rngslices = MI_BFIELD_BITS; }

  const size_t chunkmap_max = _mi_divide_up(mi_bitmap_chunk_count(bitmap), MI_BFIELD_BITS);
  for (size_t i = 0; i < chunkmap_max; i++) {
    mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bitmap->chunkmap.bfields[i]);
    size_t cmap_idx;
    while (mi_bfield_foreach_bit(&cmap_entry, &cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      mi_bchunk_t* const chunk = &bitmap->chunks[chunk_idx];
      for (size_t j = 0; j < MI_BCHUNK_FIELDS; j++) {
        const size_t base_idx = (chunk_idx*MI_BCHUNK_BITS) + (j*MI_BFIELD_BITS);
        const mi_bfield_t b = mi_atomic_exchange_relaxed(&chunk->bfields[j], (mi_bfield_t)0);
        mi_bfield_t skipped = 0;
        for (size_t shift = 0; rngslices + shift <= MI_BFIELD_BITS; shift += rngslices) {
          const mi_bfield_t rngmask = mi_bfield_mask(rngslices, shift);
          if ((b & rngmask) == rngmask) {
            const size_t idx = base_idx + shift;
            if (!visit(idx, rngslices, arena, arg)) {
              mi_bfield_t notyet_visited = 0;
              if (shift + rngslices < MI_BFIELD_BITS) {
                notyet_visited = (b & (~(mi_bfield_t)0 << (shift + rngslices)));
              }
              mi_assert_internal((notyet_visited & skipped) == 0);
              if ((notyet_visited | skipped) != 0) {
                mi_atomic_or_relaxed(&chunk->bfields[j], notyet_visited | skipped);
              }
              return false;
            }
          }
          else {
            skipped |= (b & rngmask);
          }
        }

        if (skipped != 0) {
          mi_atomic_or_relaxed(&chunk->bfields[j], skipped);
        }
      }
    }
  }
  return true;
}


/* --------------------------------------------------------------------------------
  binned bitmap's
-------------------------------------------------------------------------------- */

size_t mi_bbitmap_size(size_t bit_count, size_t* pchunk_count) {
  bit_count = _mi_align_up(bit_count, MI_BCHUNK_BITS);
  mi_assert_internal(bit_count <= MI_BITMAP_MAX_BIT_COUNT);
  mi_assert_internal(bit_count > 0);
  const size_t chunk_count = bit_count / MI_BCHUNK_BITS;
  mi_assert_internal(chunk_count >= 1);
  const size_t size = offsetof(mi_bbitmap_t,chunks) + (chunk_count * MI_BCHUNK_SIZE);
  mi_assert_internal((size%MI_BCHUNK_SIZE) == 0);
  if (pchunk_count != NULL) { *pchunk_count = chunk_count; }
  return size;
}

size_t mi_bbitmap_init(mi_bbitmap_t* bbitmap, size_t bit_count, bool already_zero) {
  size_t chunk_count;
  const size_t size = mi_bbitmap_size(bit_count, &chunk_count);
  if (!already_zero) {
    _mi_memzero_aligned(bbitmap, size);
  }
  mi_atomic_store_release(&bbitmap->chunk_count, chunk_count);
  mi_assert_internal(mi_atomic_load_relaxed(&bbitmap->chunk_count) <= MI_BITMAP_MAX_CHUNK_COUNT);
  return size;
}

void mi_bbitmap_unsafe_setN(mi_bbitmap_t* bbitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  mi_assert_internal(idx + n <= mi_bbitmap_max_bits(bbitmap));
  mi_bchunks_unsafe_setN(&bbitmap->chunks[0], &bbitmap->chunkmap, idx, n);
}

static void mi_bbitmap_set_chunk_bin(mi_bbitmap_t* bbitmap, size_t chunk_idx, mi_chunkbin_t bin) {
  mi_assert_internal(chunk_idx < mi_bbitmap_chunk_count(bbitmap));
  for (mi_chunkbin_t ibin = MI_CBIN_SMALL; ibin < MI_CBIN_NONE; ibin = mi_chunkbin_inc(ibin)) {
    if (ibin == bin) {
      const bool was_clear = mi_bchunk_set(&bbitmap->chunkmap_bins[ibin], chunk_idx, NULL);
      if (was_clear) { mi_os_stat_increase(chunk_bins[ibin],1); }
    }
    else {
      const bool was_set = mi_bchunk_clear(&bbitmap->chunkmap_bins[ibin], chunk_idx, NULL);
      if (was_set) { mi_os_stat_decrease(chunk_bins[ibin],1); }
    }
  }
}

mi_chunkbin_t mi_bbitmap_debug_get_bin(const mi_bchunkmap_t* chunkmap_bins, size_t chunk_idx) {
  for (mi_chunkbin_t ibin = MI_CBIN_SMALL; ibin < MI_CBIN_NONE; ibin = mi_chunkbin_inc(ibin)) {
    if (mi_bchunk_is_xsetN(MI_BIT_SET, &chunkmap_bins[ibin], chunk_idx, 1)) {
      return ibin;
    }
  }
  return MI_CBIN_NONE;
}

static void mi_bbitmap_chunkmap_set_max(mi_bbitmap_t* bbitmap, size_t chunk_idx) {
  size_t oldmax = mi_atomic_load_relaxed(&bbitmap->chunk_max_accessed);
  if mi_unlikely(chunk_idx > oldmax) {
    mi_atomic_cas_strong_relaxed(&bbitmap->chunk_max_accessed, &oldmax, chunk_idx);
  }
}

static void mi_bbitmap_chunkmap_set(mi_bbitmap_t* bbitmap, size_t chunk_idx, bool check_all_set) {
  mi_assert(chunk_idx < mi_bbitmap_chunk_count(bbitmap));
  if (check_all_set) {
    if (mi_bchunk_all_are_set_relaxed(&bbitmap->chunks[chunk_idx])) {
      mi_bbitmap_set_chunk_bin(bbitmap, chunk_idx, MI_CBIN_NONE);
    }
  }
  mi_bchunk_set(&bbitmap->chunkmap, chunk_idx, NULL);
  mi_bbitmap_chunkmap_set_max(bbitmap, chunk_idx);
}

static bool mi_bbitmap_chunkmap_try_clear(mi_bbitmap_t* bbitmap, size_t chunk_idx) {
  mi_assert(chunk_idx < mi_bbitmap_chunk_count(bbitmap));
  if (!mi_bchunk_all_are_clear_relaxed(&bbitmap->chunks[chunk_idx])) return false;
  mi_bchunk_clear(&bbitmap->chunkmap, chunk_idx, NULL);
  if (!mi_bchunk_all_are_clear_relaxed(&bbitmap->chunks[chunk_idx])) {
    mi_bchunk_set(&bbitmap->chunkmap, chunk_idx, NULL);
    return false;
  }
  return true;
}

bool mi_bbitmap_setN(mi_bbitmap_t* bbitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bbitmap_max_bits(bbitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return false;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  bool were_allclear = true;
  while (n > 0) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    were_allclear = mi_bchunk_setN(&bbitmap->chunks[chunk_idx], cidx, m, NULL) && were_allclear;
    mi_bbitmap_chunkmap_set(bbitmap, chunk_idx, true);
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  return were_allclear;
}

bool mi_bbitmap_try_clearNC(mi_bbitmap_t* bbitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  mi_assert_internal(n <= MI_BCHUNK_BITS);
  mi_assert_internal(idx + n <= mi_bbitmap_max_bits(bbitmap));

  const size_t chunk_idx = idx / MI_BCHUNK_BITS;
  const size_t cidx = idx % MI_BCHUNK_BITS;
  mi_assert_internal(cidx + n <= MI_BCHUNK_BITS);
  mi_assert_internal(chunk_idx < mi_bbitmap_chunk_count(bbitmap));
  if (cidx + n > MI_BCHUNK_BITS) return false;
  bool maybe_all_clear = false;
  const bool cleared = mi_bchunk_try_clearN(&bbitmap->chunks[chunk_idx], cidx, n, &maybe_all_clear);
  if (cleared && maybe_all_clear) { mi_bbitmap_chunkmap_try_clear(bbitmap, chunk_idx); }
  return cleared;
}

bool mi_bbitmap_is_xsetN(mi_xset_t set, mi_bbitmap_t* bbitmap, size_t idx, size_t n) {
  mi_assert_internal(n > 0);
  const size_t maxbits = mi_bbitmap_max_bits(bbitmap);
  mi_assert_internal(idx + n <= maxbits);
  if (idx + n > maxbits) {
    if (idx >= maxbits) return false;
    n = maxbits - idx;
  }

  size_t chunk_idx = idx / MI_BCHUNK_BITS;
  size_t cidx = idx % MI_BCHUNK_BITS;
  bool xset = true;
  while (n > 0 && xset) {
    const size_t m = (cidx + n > MI_BCHUNK_BITS ? MI_BCHUNK_BITS - cidx : n);
    xset = mi_bchunk_is_xsetN(set, &bbitmap->chunks[chunk_idx], cidx, m) && xset;
    n -= m;
    cidx = 0;
    chunk_idx++;
  }
  return xset;
}


/* --------------------------------------------------------------------------------
  mi_bbitmap_find
-------------------------------------------------------------------------------- */

typedef bool (mi_bchunk_try_find_and_clear_fun_t)(mi_bchunk_t* chunk, size_t n, size_t* idx);

static inline bool mi_bbitmap_try_find_and_clear_generic(mi_bbitmap_t* bbitmap, size_t tseq, size_t n, size_t* pidx, mi_bchunk_try_find_and_clear_fun_t* on_find)
{
  const size_t cmap_max_count  = _mi_divide_up(mi_bbitmap_chunk_count(bbitmap),MI_BFIELD_BITS);
  const size_t chunk_acc       = mi_atomic_load_relaxed(&bbitmap->chunk_max_accessed);
  const size_t cmap_acc        = chunk_acc / MI_BFIELD_BITS;
  const size_t cmap_acc_bits   = 1 + (chunk_acc % MI_BFIELD_BITS);

  mi_assert_internal(MI_BFIELD_BITS >= MI_BCHUNK_FIELDS);
  const mi_bfield_t cmap_mask  = mi_bfield_mask(cmap_max_count,0);
  const size_t cmap_cycle      = cmap_acc+1;
  const mi_chunkbin_t bbin = mi_chunkbin_of(n);

  size_t cmap_idx = 0;
  mi_bfield_cycle_iterate(cmap_mask, tseq, cmap_cycle, cmap_idx, X)
  {
    const mi_bfield_t cmap_entry = mi_atomic_load_relaxed(&bbitmap->chunkmap.bfields[cmap_idx]);
    const size_t cmap_entry_cycle = (cmap_idx != cmap_acc ? MI_BFIELD_BITS : cmap_acc_bits);
    if (cmap_entry == 0) {
      continue;
    }

    mi_bfield_t cmap_bins[MI_CBIN_COUNT] = { 0 };
    cmap_bins[MI_CBIN_NONE] = cmap_entry;
    for (mi_chunkbin_t ibin = MI_CBIN_SMALL; ibin < MI_CBIN_NONE; ibin = mi_chunkbin_inc(ibin)) {
      const mi_bfield_t cmap_bin = mi_atomic_load_relaxed(&bbitmap->chunkmap_bins[ibin].bfields[cmap_idx]);
      cmap_bins[ibin] = cmap_bin & cmap_entry;
      cmap_bins[MI_CBIN_NONE] &= ~cmap_bin;
    }

    mi_assert_internal(bbin < MI_CBIN_NONE);
    for (mi_chunkbin_t ibin = MI_CBIN_SMALL; ibin <= MI_CBIN_NONE;
         ibin = (ibin == bbin ? MI_CBIN_NONE : mi_chunkbin_inc(ibin)))
    {
      const mi_bfield_t cmap_bin = cmap_bins[ibin];
      size_t eidx = 0;
      mi_bfield_cycle_iterate(cmap_bin, tseq, cmap_entry_cycle, eidx, Y)
      {
        const size_t chunk_idx = cmap_idx*MI_BFIELD_BITS + eidx;
        mi_bchunk_t* chunk = &bbitmap->chunks[chunk_idx];

        size_t cidx;
        if ((*on_find)(chunk, n, &cidx)) {
          if (cidx==0 && ibin == MI_CBIN_NONE) {
            mi_bbitmap_set_chunk_bin(bbitmap, chunk_idx, bbin);
          }
          *pidx = (chunk_idx * MI_BCHUNK_BITS) + cidx;
          mi_assert_internal(*pidx + n <= mi_bbitmap_max_bits(bbitmap));
          return true;
        }
        else {
          mi_bbitmap_chunkmap_try_clear(bbitmap, chunk_idx);
        }
      }
      mi_bfield_cycle_iterate_end(Y);
    }
  }
  mi_bfield_cycle_iterate_end(X);
  return false;
}

bool mi_bbitmap_try_find_and_clear(mi_bbitmap_t* bbitmap, size_t tseq, size_t* pidx) {
  return mi_bbitmap_try_find_and_clear_generic(bbitmap, tseq, 1, pidx, &mi_bchunk_try_find_and_clear_1);
}

bool mi_bbitmap_try_find_and_clear8(mi_bbitmap_t* bbitmap, size_t tseq, size_t* pidx) {
  return mi_bbitmap_try_find_and_clear_generic(bbitmap, tseq, 8, pidx, &mi_bchunk_try_find_and_clear_8);
}

bool mi_bbitmap_try_find_and_clearNX(mi_bbitmap_t* bbitmap, size_t tseq, size_t n, size_t* pidx) {
  mi_assert_internal(n <= MI_BFIELD_BITS);
  return mi_bbitmap_try_find_and_clear_generic(bbitmap, tseq, n, pidx, &mi_bchunk_try_find_and_clearNX);
}

bool mi_bbitmap_try_find_and_clearNC(mi_bbitmap_t* bbitmap, size_t tseq, size_t n, size_t* pidx) {
  mi_assert_internal(n <= MI_BCHUNK_BITS);
  return mi_bbitmap_try_find_and_clear_generic(bbitmap, tseq, n, pidx, &mi_bchunk_try_find_and_clearNC);
}


// Try to atomically clear `n` bits starting at `chunk_idx` where `n` can span over multiple chunks
static bool mi_bchunk_try_clearN_(mi_bbitmap_t* bbitmap, size_t chunk_idx, size_t n) {
  mi_assert_internal((chunk_idx * MI_BCHUNK_BITS) + n <= mi_bbitmap_max_bits(bbitmap));

  size_t m = n;
  size_t count = 0;
  while (m > 0) {
    mi_bchunk_t* chunk = &bbitmap->chunks[chunk_idx + count];
    if (!mi_bchunk_try_clearN(chunk, 0, (m > MI_BCHUNK_BITS ? MI_BCHUNK_BITS : m), NULL)) {
      goto rollback;
    }
    m = (m <= MI_BCHUNK_BITS ? 0 : m - MI_BCHUNK_BITS);
    count++;
  }
  return true;

rollback:
  while (count > 0) {
    count--;
    mi_bchunk_t* chunk = &bbitmap->chunks[chunk_idx + count];
    mi_bchunk_setN(chunk, 0, MI_BCHUNK_BITS, NULL);
  }
  return false;
}

bool mi_bbitmap_try_find_and_clearN_(mi_bbitmap_t* bbitmap, size_t tseq, size_t n, size_t* pidx) {
  MI_UNUSED(tseq);
  mi_assert(n > 0);
  if (n == 0) return false;

  const size_t chunk_max = mi_bbitmap_chunk_count(bbitmap);
  const size_t full_req = n / MI_BCHUNK_BITS;
  const size_t tail_req = n % MI_BCHUNK_BITS;
  const size_t chunk_req = full_req + (tail_req > 0 ? 1 : 0);

  if (chunk_max < chunk_req) return false;

  size_t chunk_idx = 0;
  while (chunk_idx <= chunk_max - chunk_req) {
    size_t count = 0;

    while (count < full_req) {
      mi_bchunk_t* const chunk = &bbitmap->chunks[chunk_idx + count];
      if (!mi_bchunk_all_are_set_relaxed(chunk)) break;
      count++;
    }

    if (count < full_req) {
      chunk_idx += count + 1;
      continue;
    }

    if (tail_req > 0) {
      mi_bchunk_t* const tail_chunk = &bbitmap->chunks[chunk_idx + full_req];
      if (!mi_bchunk_is_xsetN(MI_BIT_SET, tail_chunk, 0, tail_req)) {
        chunk_idx += full_req + 1;
        continue;
      }
    }

    if (mi_bchunk_try_clearN_(bbitmap, chunk_idx, n)) {
      *pidx = (chunk_idx * MI_BCHUNK_BITS);
      for (size_t i = 0; i < chunk_req; i++) {
        mi_bbitmap_set_chunk_bin(bbitmap, chunk_idx + i, MI_CBIN_HUGE);
      }
      mi_assert_internal(*pidx + n <= mi_bbitmap_max_bits(bbitmap));
      return true;
    }

    chunk_idx++;
  }
  return false;
}


// Highest clear-bit query used by arena debug/inspection paths.
// Returns false if there is no clear bit.
bool mi_bbitmap_bsr_inv(mi_bbitmap_t* bbitmap, size_t* idx) {
  const size_t chunk_count = mi_bbitmap_chunk_count(bbitmap);
  const size_t chunkmap_max = _mi_divide_up(chunk_count, MI_BFIELD_BITS);
  const size_t valid_top_bits = (chunk_count % MI_BFIELD_BITS); // valid low bits in top map entry if non-zero

  for (size_t i = chunkmap_max; i > 0; ) {
    i--;
    mi_bfield_t cmap = mi_atomic_load_relaxed(&bbitmap->chunkmap.bfields[i]);

    // In the top entry, bits >= valid_top_bits are out-of-range chunks: force them to 1
    // so `mi_bsr(~cmap,...)` cannot select them.
    if (i == (chunkmap_max - 1) && valid_top_bits != 0) {
      const mi_bfield_t invalid_mask = ~mi_bfield_mask(valid_top_bits, 0);
      cmap |= invalid_mask;
    }

    size_t cmap_idx;
    if (mi_bsr(~cmap, &cmap_idx)) {
      const size_t chunk_idx = i*MI_BFIELD_BITS + cmap_idx;
      if (chunk_idx >= chunk_count) continue; // extra hardening
      size_t cidx;
      if (mi_bchunk_bsr_inv(&bbitmap->chunks[chunk_idx], &cidx)) {
        *idx = (chunk_idx * MI_BCHUNK_BITS) + cidx;
        return true;
      }
    }
  }
  return false;
}
