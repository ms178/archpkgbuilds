/* Fast, page-safe strchr() for x86-64 – AVX2 + SWAR fallback.
 Copyright (C) 1991-2025 Free Software Foundation, Inc.
 SPDX-License-Identifier: LGPL-2.1-or-later                               */

#include <string.h>
#include <stdint.h>

#ifdef __AVX2__
# include <immintrin.h>
#endif

#undef  strchr
#undef  index
#ifdef  STRCHR
# define strchr STRCHR
#endif

/* ------------------------------------------------------------------ */
/* 64-bit SWAR helper – finds first target or NUL with one ctz.       */
/* ------------------------------------------------------------------ */
static __attribute__((always_inline))
char *swar_strchr64 (const char *s, int c_in)
{
  #if __SIZEOF_POINTER__ == 8
  const uint64_t K1   = 0x0101010101010101ULL;
  const uint64_t K128 = 0x8080808080808080ULL;
  const uint64_t VCHR = (uint8_t)c_in * K1;

  for (;; s += 8)
  {
    const uint64_t w = *(const uint64_t *)s;
    const uint64_t nulmask = (w - K1) & ~w & K128;
    const uint64_t eqmask  = ((w ^ VCHR) - K1) & ~(w ^ VCHR) & K128;
    const uint64_t hit     = nulmask | eqmask;

    if (__glibc_unlikely (hit))
    {
      /* A NUL or target is in this word. `ctz(0)` is undef, so guard. */
      const unsigned pos_chr = eqmask  ? __builtin_ctzll (eqmask)  : 64;
      const unsigned pos_nul = nulmask ? __builtin_ctzll (nulmask) : 64;
      if (pos_chr < pos_nul)
        return (char *)s + (pos_chr >> 3);
      return ((uint8_t)c_in == 0)
      ? (char *)s + (pos_nul >> 3)
      : NULL;
    }
  }
  #else
  char *r = __strchrnul (s, c_in);
  return (*(unsigned char *)r == (unsigned char)c_in) ? r : NULL;
  #endif
}

/* ------------------------------------------------------------------ */
/* AVX2 fast path – page-safe & ABI-clean.                            */
/* ------------------------------------------------------------------ */
#ifdef __AVX2__
static __attribute__((target("avx2"), hot))
char *strchr_avx2 (const char *p, int c_in)
{
  const __m256i VCHR  = _mm256_set1_epi8 ((char)c_in);
  const __m256i VZERO = _mm256_setzero_si256 ();
  const uintptr_t mis = (uintptr_t)p & 31u;

  /* --- Prologue: handle initial misaligned bytes efficiently --- */
  /* A single unaligned load fetches the first 32 bytes of the string.
   T *his vector is then used for two checks: for a NUL (short string)
   and for the target character (hit in the prologue). */
  const __m256i first = _mm256_loadu_si256 ((const __m256i *)p);

  /* Check 1: Is there a NUL in the valid part of the initial block?
   I*f so, the string is short; delegate to the SWAR helper. */
  const unsigned nulmsk = (unsigned)_mm256_movemask_epi8(
    _mm256_cmpeq_epi8 (first, VZERO));
  if (__glibc_unlikely (nulmsk >> mis))
  { _mm256_zeroupper(); return swar_strchr64 (p, c_in); }

  /* Check 2: Is the target in the misaligned prefix? If so, we're done.
   W*e already know no NUL is present in this section. */
  if (mis)
  {
    unsigned tmask = (unsigned)_mm256_movemask_epi8(
      _mm256_cmpeq_epi8 (first, VCHR)) >> mis;
      if (__glibc_unlikely (tmask))
      { _mm256_zeroupper(); return (char *)p + __builtin_ctz (tmask); }
      p = (const char *)(((uintptr_t)p + 31) & ~31ul);
  }

  /* --- Main Aligned Loop --- */
  for (;; p += 32)
  {
    /* Guard: next load must stay inside the current 4 KiB page. */
    if (__glibc_unlikely (((uintptr_t)p & 0xFFF) > 0xFE0))
    { char *r = swar_strchr64 (p, c_in); _mm256_zeroupper(); return r; }

    const __m256i blk = _mm256_load_si256 ((const __m256i *)p);
    const __m256i hit = _mm256_or_si256 (_mm256_cmpeq_epi8 (blk, VCHR),
                                         _mm256_cmpeq_epi8 (blk, VZERO));
    const unsigned m  = (unsigned)_mm256_movemask_epi8 (hit);
    if (__glibc_unlikely (m))
    {
      const unsigned idx = __builtin_ctz (m);
      const char ch = p[idx];
      _mm256_zeroupper();
      return (ch == (char)c_in) ? (char *)(p + idx) : NULL;
    }
  }
}
#endif /* __AVX2__ */

/* ------------------------------------------------------------------ */
/* Public wrapper – no special-case for NUL (vector path handles it). */
/* ------------------------------------------------------------------ */
char *strchr (const char *s, int c_in)
{
  #ifdef __AVX2__
  return strchr_avx2 (s, c_in);
  #else
  return swar_strchr64 (s, c_in);
  #endif
}

#ifndef STRCHR
weak_alias (strchr, index)
libc_hidden_builtin_def (strchr)
#endif
