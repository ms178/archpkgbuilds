/**************************************************************************
 *
 * Copyright 2008 VMware, Inc.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL VMWARE AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 **************************************************************************/

#ifndef BITSCAN_H
#define BITSCAN_H

#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#if defined(_MSC_VER)
#include <intrin.h>
#endif

#if defined(__POPCNT__)
/* _mm_popcnt_u32/_mm_popcnt_u64 intrinsics */
#include <popcntintrin.h>
#endif

#if (defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)) && defined(__BMI2__)
/* For _pdep_u32 (BMI2) */
#include <immintrin.h>
#endif

#include "util/detect_arch.h"
#include "util/detect_cc.h"
#include "util/macros.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Find first bit set in word.  Least significant bit is 1.
 * Return 0 if no bits set.
 */
#ifdef HAVE___BUILTIN_FFS
#define ffs __builtin_ffs
#elif defined(_MSC_VER) && (_M_IX86 || _M_ARM || _M_AMD64 || _M_IA64)
static inline int
ffs(int i)
{
   unsigned long index;
   if (_BitScanForward(&index, (unsigned long)i))
      return (int)index + 1;
   else
      return 0;
}
#else
extern int
ffs(int i);
#endif

#ifdef HAVE___BUILTIN_FFSLL
#define ffsll __builtin_ffsll
#elif defined(_MSC_VER) && (_M_AMD64 || _M_ARM64 || _M_IA64)
static inline int
ffsll(long long int i)
{
   unsigned long index;
   if (_BitScanForward64(&index, (unsigned long long)i))
      return (int)index + 1;
   else
      return 0;
}
#else
extern int
ffsll(long long int val);
#endif

/* Destructively loop over all of the bits in a mask as in:
 *
 * while (mymask) {
 *   int i = u_bit_scan(&mymask);
 *   ... process element i
 * }
 *
 * Note: u_bit_scan() asserts mask != 0 (debug) to prevent UB (shift by -1).
 */
static inline int
u_bit_scan(unsigned *mask)
{
   assert(mask && *mask);
#if defined(__GNUC__) || defined(__clang__)
   /* Fast path: ctz → clear LSB (mask &= mask - 1) */
   const unsigned m = *mask;
   const int i = __builtin_ctz(m);
   *mask = m & (m - 1);
   return i;
#else
   const int i = ffs(*mask) - 1;
   *mask &= ~(1u << i);
   return i;
#endif
}

/* Iterate over set bits in a 32-bit mask.
 * 'b' is assigned the bit index on each iteration.
 */
#define u_foreach_bit(b, dword)                                  \
   for (uint32_t __dword = (uint32_t)(dword), b;                  \
        ((b) = ffs(__dword) - 1, __dword);                        \
        __dword &= ~(1u << (b)))

static inline int
u_bit_scan64(uint64_t *mask)
{
   assert(mask && *mask);
#if defined(__GNUC__) || defined(__clang__)
   const uint64_t m = *mask;
   const int i = __builtin_ctzll(m);
   *mask = m & (m - 1);
   return i;
#else
   const int i = ffsll(*mask) - 1;
   *mask &= ~(1ull << i);
   return i;
#endif
}

/* Iterate over set bits in a 64-bit mask.
 * 'b' is assigned the bit index on each iteration.
 */
#define u_foreach_bit64(b, dword)                                \
   for (uint64_t __dword = (uint64_t)(dword), b;                  \
        ((b) = ffsll(__dword) - 1, __dword);                      \
        __dword &= ~(1ull << (b)))

/* Given two bitmasks, loop over all bits of both of them.
 * Bits of mask1 are: b = ffsll(mask1) - 1;
 * Bits of mask2 are: b = offset + (ffsll(mask2) - 1);
 */
#define u_foreach_bit64_two_masks(b, mask1, offset, mask2)                             \
   for (uint64_t __mask1 = (uint64_t)(mask1), __mask2 = (uint64_t)(mask2), b;          \
        (__mask1 ? ((b) = ffsll(__mask1) - 1)                                          \
                 : ((b) = ffsll(__mask2) - 1 + (offset)), __mask1 || __mask2);         \
        __mask1 ? (__mask1 &= ~(1ull << (b))) : (__mask2 &= ~(1ull << ((b) - (offset)))))

/* Determine if an uint32_t value is a power of two.
 * Zero is treated as a power of two.
 */
static inline bool
util_is_power_of_two_or_zero(uint32_t v)
{
   return IS_POT(v);
}

/* Determine if an uint64_t value is a power of two.
 * Zero is treated as a power of two.
 */
static inline bool
util_is_power_of_two_or_zero64(uint64_t v)
{
   return IS_POT(v);
}

/* Determine if an uint32_t value is a power of two.
 * Zero is not treated as a power of two.
 *
 * Branchless bit trick (fast on all CPUs; no popcnt required).
 */
static inline bool
util_is_power_of_two_nonzero(uint32_t v)
{
   return IS_POT_NONZERO(v);
}

/* Determine if an uint64_t value is a power of two.
 * Zero is not treated as a power of two.
 */
static inline bool
util_is_power_of_two_nonzero64(uint64_t v)
{
   return IS_POT_NONZERO(v);
}

/* Determine if a uintptr_t value is a power of two.
 * Zero is not treated as a power of two.
 */
static inline bool
util_is_power_of_two_nonzero_uintptr(uintptr_t v)
{
   return IS_POT_NONZERO(v);
}

/* Loop over a bitmask yielding ranges of consecutive bits. */
static inline void
u_bit_scan_consecutive_range(unsigned *mask, int *start, int *count)
{
   if (*mask == 0xffffffffu) {
      *start = 0;
      *count = 32;
      *mask  = 0;
      return;
   }
   *start = ffs(*mask) - 1;
   *count = ffs(~(*mask >> *start)) - 1;
   *mask &= ~(((1u << (unsigned)*count) - 1u) << (unsigned)*start);
}

static inline void
u_bit_scan_consecutive_range64(uint64_t *mask, int *start, int *count)
{
   if (*mask == UINT64_MAX) {
      *start = 0;
      *count = 64;
      *mask  = 0;
      return;
   }
   *start = ffsll(*mask) - 1;
   *count = ffsll(~(*mask >> *start)) - 1;
   *mask &= ~(((((uint64_t)1) << (unsigned)*count) - 1ull) << (unsigned)*start);
}

/**
 * Find last bit set in a word. Least significant bit is 1.
 * Return 0 if no bits are set. (Essentially ffs() in reverse.)
 */
static inline unsigned
util_last_bit(unsigned u)
{
#if defined(HAVE___BUILTIN_CLZ)
   return u == 0 ? 0u : 32u - (unsigned)__builtin_clz(u);
#elif defined(_MSC_VER) && (_M_IX86 || _M_ARM || _M_AMD64 || _M_IA64)
   unsigned long index;
   if (_BitScanReverse(&index, (unsigned long)u))
      return (unsigned)index + 1u;
   else
      return 0u;
#else
   unsigned r = 0;
   while (u) {
      r++;
      u >>= 1;
   }
   return r;
#endif
}

/**
 * Find last bit set in a 64-bit word. Least significant bit is 1.
 * Return 0 if no bits are set. (Essentially ffsll() in reverse.)
 */
static inline unsigned
util_last_bit64(uint64_t u)
{
#if defined(HAVE___BUILTIN_CLZLL)
   return u == 0 ? 0u : 64u - (unsigned)__builtin_clzll(u);
#elif defined(_MSC_VER) && (_M_AMD64 || _M_ARM64 || _M_IA64)
   unsigned long index;
   if (_BitScanReverse64(&index, (unsigned long long)u))
      return (unsigned)index + 1u;
   else
      return 0u;
#else
   unsigned r = 0;
   while (u) {
      r++;
      u >>= 1;
   }
   return r;
#endif
}

/**
 * Find last bit in a word that does not match the sign bit.
 * The least significant bit is 1. Return 0 if no bits are set.
 */
static inline unsigned
util_last_bit_signed(int i)
{
   if (i >= 0)
      return util_last_bit((unsigned)i);
   else
      return util_last_bit(~(unsigned)i);
}

/* Returns a bitfield in which the first count bits starting at start are set. */
static inline unsigned
u_bit_consecutive(unsigned start, unsigned count)
{
   assert(start + count <= 32);
   if (count == 32)
      return ~0u;
   return ((1u << count) - 1u) << start;
}

static inline uint64_t
u_bit_consecutive64(unsigned start, unsigned count)
{
   assert(start + count <= 64);
   if (count == 64)
      return ~(uint64_t)0;
   return (((uint64_t)1 << count) - 1ull) << start;
}

/**
 * Return number of bits set in n (32-bit).
 */
static inline unsigned
util_bitcount(unsigned n)
{
#if defined(HAVE___BUILTIN_POPCOUNT)
   return (unsigned)__builtin_popcount(n);
#elif __OPENCL_VERSION__
   return (unsigned)popcount(n);
#else
   /* K&R bitcount: clear lowest set bit per iteration. */
   unsigned bits = 0;
   while (n) {
      bits++;
      n &= n - 1;
   }
   return bits;
#endif
}

/**
 * Return the number of bits set in n using the native popcnt instruction.
 * The caller is responsible for ensuring that popcnt is supported by the CPU.
 * gcc won't auto-emit popcnt unless -mpopcnt or suitable -march is used.
 */
static inline unsigned
util_popcnt_inline_asm(unsigned n)
{
#if (DETECT_ARCH_X86 || DETECT_ARCH_X86_64) && DETECT_CC_GCC
   uint32_t out;
   __asm volatile("popcnt %1, %0" : "=r"(out) : "r"(n));
   return out;
#else
   /* Fallback safely to software popcount. */
   return util_bitcount(n);
#endif
}

/**
 * Return number of bits set in n (64-bit).
 */
static inline unsigned
util_bitcount64(uint64_t n)
{
#ifdef HAVE___BUILTIN_POPCOUNTLL
   return (unsigned)__builtin_popcountll(n);
#elif __OPENCL_VERSION__
   return (unsigned)popcount(n);
#else
   /* Portable fallback via two 32-bit popcounts. */
   return util_bitcount((unsigned)n) + util_bitcount((unsigned)(n >> 32));
#endif
}

/**
 * Widens the given bit mask by a multiplier, replicating each bit by that count.
 * Example: 0b101 widened by 2 -> 0b110011
 */
static inline uint32_t
util_widen_mask(uint32_t mask, unsigned multiplier)
{
   if (!mask || multiplier == 0)
      return 0u;

#if (defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)) && defined(__BMI2__)
   /* Fast path for the most common case: multiplier == 2.
    * Use BMI2 PDEP to interleave zeros between bits, then OR with a shift to replicate.
    */
   if (multiplier == 2) {
      /* Spread source bits into odd positions. */
      uint32_t spread = _pdep_u32(mask, 0x55555555u);
      return spread | (spread << 1);
   }
#endif

   /* Generic path for any multiplier. */
   uint32_t new_mask = 0;
   u_foreach_bit(i, mask) {
      new_mask |= ((1u << multiplier) - 1u) << (i * multiplier);
   }
   return new_mask;
}

#ifdef __cplusplus
} /* extern "C" */

/* util_bitcount has measurable overhead (~2%) compared to popcnt,
 * so prefer inline assembly if the CPU supports it.
 */
enum util_popcnt {
   POPCNT_NO,
   POPCNT_YES,
   POPCNT_INVALID,
};

/* Convenient function to select popcnt through a C++ template argument.
 * This should be used as part of larger functions optimized as a whole.
 */
template<util_popcnt POPCNT> inline unsigned
util_bitcount_fast(unsigned n)
{
   if (POPCNT == POPCNT_YES)
      return util_popcnt_inline_asm(n);
   else
      return util_bitcount(n);
}

#endif /* __cplusplus */

#endif /* BITSCAN_H */
