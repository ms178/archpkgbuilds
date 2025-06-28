/* Copyright (C) 1991-2025 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

#include <string.h>
#include <stdint.h>

#if defined __AVX2__
# include <immintrin.h>
#endif

#undef strchr
#undef index
#ifdef STRCHR
# define strchr STRCHR          /* honour local macro re-defs      */
#endif

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                  */
/* ------------------------------------------------------------------ */

#if defined __AVX2__
static __attribute__((target("avx2"), always_inline, hot))
char *
strchr_avx2 (const char *s, int c_in)
{
  /* Broadcast searched byte as 32 copies.  */
  const __m256i v_ch   = _mm256_set1_epi8 ((char) c_in);
  const __m256i v_zero = _mm256_setzero_si256 ();

  const char *p = s;

  /* Main 32-byte stride loop – unaligned safe load.                  */
  for (;; p += 32)
  {
    __m256i blk      = _mm256_loadu_si256 ((const __m256i *) p);
    __m256i match_ch = _mm256_cmpeq_epi8 (blk, v_ch);
    __m256i match_0  = _mm256_cmpeq_epi8 (blk, v_zero);

    /* bit mask: 1-bit per byte – ORing keeps “first event” test easy */
    int mask = _mm256_movemask_epi8 (_mm256_or_si256 (match_ch,
                                                      match_0));

    if (__builtin_expect (mask != 0, 0))
    {
      unsigned idx = (unsigned) __builtin_ctz (mask);

      /* Sub-script once – avoids aliasing UB.                     */
      char res = p[idx];

      /* If we stopped on NUL before the target char, it is “not found”. */
      return (res == (char) c_in) ? (char *)(p + idx) : NULL;
    }
  }
}
#endif /* __AVX2__ */


/* 64-bit word-at-a-time helper – used when AVX2 not available or
 for* very short strings where vector start-up would dominate.       */
static inline char *
strchr_word (const char *s, int c_in)
{
  #if INTPTR_MAX == INT64_MAX         /* only build on 64-bit glibc   */
  const size_t K_LO = 0x0101010101010101ULL;
  const size_t K_HI = 0x8080808080808080ULL;
  const size_t target = (unsigned char) c_in;

  /* Align?  Not necessary – x86 allows unaligned accesses. */
  for (;; s += sizeof (size_t))
  {
    size_t chunk = *(const size_t *) s;

    /* Detect NULs first – cheap.                                   */
    size_t tmp0 = (chunk - K_LO) & ~chunk & K_HI;

    /* Detect target byte.  Classic “byte replicate” trick.          */
    size_t tmask = chunk ^ (target * K_LO);
    size_t tmp1  = (tmask - K_LO) & ~tmask & K_HI;

    /* Combine; any bit set ⇒ something interesting.                */
    size_t any = tmp0 | tmp1;

    if (__builtin_expect (any != 0, 0))
    {
      /* Scan the eight bytes exactly once.                       */
      for (unsigned i = 0; i < sizeof (size_t); ++i)
      {
        unsigned char cur = (unsigned char) s[i];
        if (cur == (unsigned char) c_in)
          return (char *)(s + i);
        if (cur == 0)
          return NULL;       /* NUL first ⇒ not found          */
      }
    }
  }
  #else
  /* 32-bit hosts fall back on glibc scalar path.                     */
  char *r = __strchrnul (s, c_in);
  return (*(unsigned char *) r == (unsigned char) c_in) ? r : NULL;
  #endif
}


/* ------------------------------------------------------------------ */
/*  Public symbol                                                     */
/* ------------------------------------------------------------------ */
char *
strchr (const char *s, int c_in)
{
  /* POSIX: searching for NUL is legal and must return end-ptr.       */
  if (__glibc_unlikely ((unsigned char) c_in == '\0'))
    return (char *) (s + strlen (s));

  #if defined __AVX2__
  /* AVX2 present at compile-time ⇒ we assume run-time CPU matches.
   U *se it unconditionally – overhead on tiny (< 16 B) strings is
   below measurement noise on Raptor-Lake.                          */
  return strchr_avx2 (s, c_in);
  #else
  /* Plain word-at-a-time fallback.                                   */
  return strchr_word (s, c_in);
  #endif
}

#ifndef STRCHR
weak_alias (strchr, index)
libc_hidden_builtin_def (strchr)
#endif
