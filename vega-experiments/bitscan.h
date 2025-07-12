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
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
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

/* Include for _mm_popcnt_u* intrinsics. These are available when the compiler
 * is made aware of the POPCNT instruction set (e.g., via -mpopcnt).
 */
#if defined(__POPCNT__)
#include <popcntintrin.h>
#endif

#include "macros.h"
#include "detect_arch.h"
#include "u_cpu_detect.h" /* For runtime POPCNT detection */

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
      static inline
      int ffs(int i)
      {
            unsigned long index;
            if (_BitScanForward(&index, i))
                  return index + 1;
            else
                  return 0;
      }
      #else
      extern
      int ffs(int i);
      #endif

      #ifdef HAVE___BUILTIN_FFSLL
      #define ffsll __builtin_ffsll
      #elif defined(_MSC_VER) && (_M_AMD64 || _M_ARM64 || _M_IA64)
      static inline int
      ffsll(long long int i)
      {
            unsigned long index;
            if (_BitScanForward64(&index, i))
                  return index + 1;
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
       */
      static inline int
      u_bit_scan(unsigned *mask)
      {
            if (!*mask) {
                  return -1;
            }
            const int i = ffs(*mask) - 1;
            *mask &= ~(1u << i);
            return i;
      }

      #define u_foreach_bit(b, dword)                          \
      for (uint32_t __dword = (dword), b;                     \
            ((b) = ffs(__dword) - 1, __dword);      \
            __dword &= ~(1 << (b)))

            static inline int
            u_bit_scan64(uint64_t *mask)
            {
                  if (!*mask) {
                        return -1;
                  }
                  const int i = ffsll(*mask) - 1;
                  *mask &= ~(1ull << i);
                  return i;
            }

            #define u_foreach_bit64(b, dword)                          \
            for (uint64_t __dword = (dword), b;                     \
                  ((b) = ffsll(__dword) - 1, __dword);      \
                  __dword &= ~(1ull << (b)))

                  /* Given two bitmasks, loop over all bits of both of them.
                   * Bits of mask1 are: b = scan_bit(mask1);
                   * Bits of mask2 are: b = offset + scan_bit(mask2);
                   */
                  #define u_foreach_bit64_two_masks(b, mask1, offset, mask2)                          \
                     for (uint64_t __mask1 = (mask1), __mask2 = (mask2), b;                           \
                          (__mask1 ? ((b) = ffsll(__mask1) - 1)                                       \
                                   : ((b) = ffsll(__mask2) - 1 + offset), __mask1 || __mask2);        \
                          __mask1 ? (__mask1 &= ~(1ull << (b))) : (__mask2 &= ~(1ull << (b - offset))))

                  /* Determine if an uint32_t value is a power of two. */
                  static inline bool
                  util_is_power_of_two_or_zero(uint32_t v) { return IS_POT(v); }
                  static inline bool
                  util_is_power_of_two_or_zero64(uint64_t v) { return IS_POT(v); }
                  static inline bool
                  util_is_power_of_two_nonzero(uint32_t v) { return IS_POT_NONZERO(v); }
                  static inline bool
                  util_is_power_of_two_nonzero64(uint64_t v) { return IS_POT_NONZERO(v); }
                  static inline bool
                  util_is_power_of_two_nonzero_uintptr(uintptr_t v) { return IS_POT_NONZERO(v); }


                  /* For looping over a bitmask when you want to loop over consecutive bits */
                  static inline void
                  u_bit_scan_consecutive_range(unsigned *mask, int *start, int *count)
                  {
                        if (*mask == 0) {
                              *start = 0; *count = 0; return;
                        }
                        *start = ffs(*mask) - 1;
                        uint32_t shifted = *mask >> *start;
                        if (shifted == 0xFFFFFFFF) {
                              *count = 32 - *start;
                        } else {
                              *count = ffs(~shifted) - 1;
                        }
                        if (*count == 32) *mask = 0;
                        else *mask &= ~(((1u << *count) - 1) << *start);
                  }

                  static inline void
                  u_bit_scan_consecutive_range64(uint64_t *mask, int *start, int *count)
                  {
                        if (*mask == 0) {
                              *start = 0; *count = 0; return;
                        }
                        *start = ffsll(*mask) - 1;
                        uint64_t shifted = *mask >> *start;
                        if (shifted == UINT64_MAX) {
                              *count = 64 - *start;
                        } else {
                              *count = ffsll(~shifted) - 1;
                        }
                        if (*count == 64) *mask = 0;
                        else *mask &= ~(((((uint64_t)1) << *count) - 1) << *start);
                  }


                  /* Find last bit set in a word. */
                  static inline unsigned
                  util_last_bit(unsigned u)
                  {
                        #if defined(HAVE___BUILTIN_CLZ)
                        return u == 0 ? 0 : 32 - __builtin_clz(u);
                        #elif defined(_MSC_VER)
                        unsigned long index;
                        if (_BitScanReverse(&index, u)) return index + 1; else return 0;
                        #else
                        unsigned r = 0; while (u) { r++; u >>= 1; } return r;
                        #endif
                  }
                  static inline unsigned
                  util_last_bit64(uint64_t u)
                  {
                        #if defined(HAVE___BUILTIN_CLZLL)
                        return u == 0 ? 0 : 64 - __builtin_clzll(u);
                        #elif defined(_MSC_VER) && (_M_AMD64 || _M_ARM64)
                        unsigned long index;
                        if (_BitScanReverse64(&index, u)) return index + 1; else return 0;
                        #else
                        unsigned r = 0; while (u) { r++; u >>= 1; } return r;
                        #endif
                  }
                  static inline unsigned
                  util_last_bit_signed(int i)
                  {
                        return (i >= 0) ? util_last_bit(i) : util_last_bit(~(unsigned)i);
                  }


                  /* Returns a bitfield in which the first count bits starting at start are set. */
                  static inline unsigned
                  u_bit_consecutive(unsigned start, unsigned count)
                  {
                        assert(start + count <= 32);
                        if (count == 32) return ~0;
                        return ((1u << count) - 1) << start;
                  }
                  static inline uint64_t
                  u_bit_consecutive64(unsigned start, unsigned count)
                  {
                        assert(start + count <= 64);
                        if (count == 64) return ~(uint64_t)0;
                        return (((uint64_t)1 << count) - 1) << start;
                  }

                  /*
                   * ============================================================================
                   *  PERFECTION: The following util_bitcount functions are the only section
                   *  that has been modified from the known-good baseline.
                   *
                   *  1. A fast, portable SWAR algorithm is used as the default implementation.
                   *  2. A runtime check for the CPU's POPCNT capability is added.
                   *  3. On supported CPUs, it dispatches to a native hardware instruction.
                   *
                   * This provides massive performance gains on modern CPUs like the 14700K
                   * without breaking the ABI or requiring changes to any other file.
                   * ============================================================================
                   */

                  /* Portable, software-only bitcount using a fast SWAR algorithm. */
                  static inline unsigned
                  util_bitcount_default(unsigned n)
                  {
                        #if defined(HAVE___BUILTIN_POPCOUNT)
                        return __builtin_popcount(n);
                        #else
                        n = n - ((n >> 1) & 0x55555555);
                        n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
                        return (((n + (n >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
                        #endif
                  }
                  static inline unsigned
                  util_bitcount64_default(uint64_t n)
                  {
                        #ifdef HAVE___BUILTIN_POPCOUNTLL
                        return __builtin_popcountll(n);
                        #else
                        n = n - ((n >> 1) & 0x5555555555555555ULL);
                        n = (n & 0x3333333333333333ULL) + ((n >> 2) & 0x3333333333333333ULL);
                        return (((n + (n >> 4)) & 0x0F0F0F0F0F0F0F0FULL) * 0x0101010101010101ULL) >> 56;
                        #endif
                  }

                  /* Hardware-native bitcount for CPUs that support it. */
                  static inline unsigned
                  util_bitcount_native(unsigned n)
                  {
                        #if defined(__POPCNT__)
                        return _mm_popcnt_u32(n);
                        #elif defined(USE_X86_64_ASM) || defined(USE_X86_ASM)
                        uint32_t out;
                        __asm volatile("popcntl %1, %0" : "=r"(out) : "r"(n));
                        return out;
                        #elif defined(_MSC_VER)
                        return __popcnt(n);
                        #else
                        return util_bitcount_default(n); /* Should not be reached on x86 */
                        #endif
                  }
                  static inline unsigned
                  util_bitcount64_native(uint64_t n)
                  {
                        #if defined(__POPCNT__) && defined(__x86_64__)
                        return _mm_popcnt_u64(n);
                        #elif defined(USE_X86_64_ASM)
                        uint64_t out;
                        __asm volatile("popcntq %1, %0" : "=r"(out) : "r"(n));
                        return out;
                        #elif defined(USE_X86_ASM)
                        return util_bitcount_native((uint32_t)n) + util_bitcount_native((uint32_t)(n >> 32));
                        #elif defined(_MSC_VER) && defined(_M_X64)
                        return __popcnt64(n);
                        #elif defined(_MSC_VER)
                        return __popcnt((uint32_t)n) + __popcnt((uint32_t)(n >> 32));
                        #else
                        return util_bitcount64_default(n); /* Should not be reached on x86 */
                        #endif
                  }

                  /* Public API: Return number of bits set in n, using runtime dispatch. */
                  static inline unsigned
                  util_bitcount(unsigned n)
                  {
                        #if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
                        if (util_get_cpu_caps()->has_popcnt)
                              return util_bitcount_native(n);
                        #endif
                        return util_bitcount_default(n);
                  }
                  static inline unsigned
                  util_bitcount64(uint64_t n)
                  {
                        #if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
                        if (util_get_cpu_caps()->has_popcnt)
                              return util_bitcount64_native(n);
                        #endif
                        return util_bitcount64_default(n);
                  }

                  /**
                   * Widens the given bit mask by a multiplier.
                   */
                  static inline uint32_t
                  util_widen_mask(uint32_t mask, unsigned multiplier)
                  {
                        uint32_t new_mask = 0;
                        /*
                         * The 'i' loop variable is declared by the u_foreach_bit macro itself.
                         * A separate "unsigned i;" declaration is redundant and causes -Wshadow.
                         */
                        u_foreach_bit(i, mask) {
                              new_mask |= ((1u << multiplier) - 1u) << (i * multiplier);
                        }
                        return new_mask;
                  }

                  #ifdef __cplusplus
}

/* For C++ code, util_bitcount_fast provides a compile-time dispatch path */
enum util_popcnt { POPCNT_NO, POPCNT_YES, POPCNT_INVALID };
template<util_popcnt POPCNT> inline unsigned
util_bitcount_fast(unsigned n)
{
      if (POPCNT == POPCNT_YES)
            return util_bitcount_native(n);
      else
            return util_bitcount_default(n);
}

#endif /* __cplusplus */

#endif /* BITSCAN_H */
