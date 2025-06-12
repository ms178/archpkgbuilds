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
            const int i = ffs(*mask) - 1;
            *mask ^= (1u << i);
            return i;
      }

      #define u_foreach_bit(b, dword)                          \
      for (uint32_t __dword = (dword), b;                     \
            ((b) = ffs(__dword) - 1, __dword);      \
            __dword &= ~(1 << (b)))

            static inline int
            u_bit_scan64(uint64_t *mask)
            {
                  const int i = ffsll(*mask) - 1;
                  *mask ^= (((uint64_t)1) << i);
                  return i;
            }

            #define u_foreach_bit64(b, dword)                          \
            for (uint64_t __dword = (dword), b;                     \
                  ((b) = ffsll(__dword) - 1, __dword);      \
                  __dword &= ~(1ull << (b)))

                  /* Forward declaration needed by util_is_power_of_two_nonzero */
                  static inline unsigned util_bitcount_native(unsigned n);
            static inline unsigned util_bitcount64_native(uint64_t n);

            /* Determine if an uint32_t value is a power of two.
             *
             * \note
             * Zero is treated as a power of two.
             */
            static inline bool
            util_is_power_of_two_or_zero(uint32_t v)
            {
                  return IS_POT(v);
            }

            /* Determine if an uint64_t value is a power of two.
             *
             * \note
             * Zero is treated as a power of two.
             */
            static inline bool
            util_is_power_of_two_or_zero64(uint64_t v)
            {
                  return IS_POT(v);
            }

            /* Determine if an uint32_t value is a power of two.
             *
             * \note
             * Zero is \b not treated as a power of two.
             */
            static inline bool
            util_is_power_of_two_nonzero(uint32_t v)
            {
                  #ifdef __POPCNT__
                  return _mm_popcnt_u32(v) == 1;
                  #else
                  /* Fallback for generic builds - this avoids needing a runtime check here. */
                  return IS_POT_NONZERO(v);
                  #endif
            }

            /* Determine if an uint64_t value is a power of two.
             *
             * \note
             * Zero is \b not treated as a power of two.
             */
            static inline bool
            util_is_power_of_two_nonzero64(uint64_t v)
            {
                  #if defined(__POPCNT__) && defined(USE_X86_64_ASM)
                  return _mm_popcnt_u64(v) == 1;
                  #else
                  return IS_POT_NONZERO(v);
                  #endif
            }

            /* Determine if an size_t/uintptr_t/intptr_t value is a power of two.
             *
             * \note
             * Zero is \b not treated as a power of two.
             */
            static inline bool
            util_is_power_of_two_nonzero_uintptr(uintptr_t v)
            {
                  return IS_POT_NONZERO(v);
            }

            /* For looping over a bitmask when you want to loop over consecutive bits
             * manually.
             */
            static inline void
            u_bit_scan_consecutive_range(unsigned *mask, int *start, int *count)
            {
                  if (*mask == 0) {
                        *start = 0;
                        *count = 0;
                        return;
                  }
                  *start = ffs(*mask) - 1;

                  uint32_t shifted = *mask >> *start;
                  if (shifted == 0xFFFFFFFF) {
                        *count = 32 - *start;
                  } else {
                        *count = ffs(~shifted) - 1;
                  }

                  *mask &= ~(((1u << *count) - 1) << *start);
            }

            static inline void
            u_bit_scan_consecutive_range64(uint64_t *mask, int *start, int *count)
            {
                  if (*mask == 0) {
                        *start = 0;
                        *count = 0;
                        return;
                  }
                  *start = ffsll(*mask) - 1;

                  uint64_t shifted = *mask >> *start;
                  if (shifted == UINT64_MAX) {
                        *count = 64 - *start;
                  } else {
                        *count = ffsll(~shifted) - 1;
                  }

                  *mask &= ~(((((uint64_t)1) << *count) - 1) << *start);
            }


            /**
             * Find last bit set in a word.  The least significant bit is 1.
             * Return 0 if no bits are set.
             */
            static inline unsigned
            util_last_bit(unsigned u)
            {
                  #if defined(HAVE___BUILTIN_CLZ)
                  return u == 0 ? 0 : 32 - __builtin_clz(u);
                  #elif defined(_MSC_VER) && (_M_IX86 || _M_ARM || _M_AMD64 || _M_IA64)
                  unsigned long index;
                  if (_BitScanReverse(&index, u))
                        return index + 1;
                  else
                        return 0;
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
             * Find last bit set in a word.  The least significant bit is 1.
             * Return 0 if no bits are set.
             */
            static inline unsigned
            util_last_bit64(uint64_t u)
            {
                  #if defined(HAVE___BUILTIN_CLZLL)
                  return u == 0 ? 0 : 64 - __builtin_clzll(u);
                  #elif defined(_MSC_VER) && (_M_AMD64 || _M_ARM64 || _M_IA64)
                  unsigned long index;
                  if (_BitScanReverse64(&index, u))
                        return index + 1;
                  else
                        return 0;
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
             * Find last bit in a word that does not match the sign bit. The least
             * significant bit is 1.
             * Return 0 if no bits are set.
             */
            static inline unsigned
            util_last_bit_signed(int i)
            {
                  if (i >= 0)
                        return util_last_bit(i);
                  else
                        return util_last_bit(~(unsigned)i);
            }

            /* Returns a bitfield in which the first count bits starting at start are
             * set.
             */
            static inline unsigned
            u_bit_consecutive(unsigned start, unsigned count)
            {
                  assert(start + count <= 32);
                  if (count == 32)
                        return ~0;
                  return ((1u << count) - 1) << start;
            }

            static inline uint64_t
            u_bit_consecutive64(unsigned start, unsigned count)
            {
                  assert(start + count <= 64);
                  if (count == 64)
                        return ~(uint64_t)0;
                  return (((uint64_t)1 << count) - 1) << start;
            }

            /**
             * Portable, software-only bitcount implementations.
             * These use a fast, branchless, constant-time SWAR algorithm.
             */
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

            /**
             * Return the number of bits set in n using the native popcnt instruction.
             * This is the workhorse for util_bitcount on x86, using the best available method.
             */
            static inline unsigned
            util_bitcount_native(unsigned n)
            {
                  #if defined(__POPCNT__)
                  /* Optimal path for modern GCC/Clang when compiling with -mpopcnt. */
                  return _mm_popcnt_u32(n);
                  #elif defined(USE_X86_64_ASM) || defined(USE_X86_ASM)
                  /* Fallback for generic builds to force popcnt via inline asm. */
                  uint32_t out;
                  __asm volatile("popcntl %1, %0" : "=r"(out) : "r"(n));
                  return out;
                  #elif defined(_MSC_VER) && (DETECT_ARCH_X86_64 || DETECT_ARCH_X86)
                  return __popcnt(n);
                  #else
                  /* This path should not be taken on x86 if has_popcnt is true. */
                  return util_bitcount_default(n);
                  #endif
            }

            static inline unsigned
            util_bitcount64_native(uint64_t n)
            {
                  #if defined(__POPCNT__) && defined(USE_X86_64_ASM)
                  /* Optimal path for modern GCC/Clang on 64-bit when compiling with -mpopcnt. */
                  return _mm_popcnt_u64(n);
                  #elif defined(USE_X86_64_ASM)
                  /* Fallback for generic 64-bit builds to force popcntq via inline asm. */
                  uint64_t out;
                  __asm volatile("popcntq %1, %0" : "=r"(out) : "r"(n));
                  return out;
                  #elif defined(USE_X86_ASM)
                  /* On 32-bit x86, use two 32-bit native popcounts. */
                  return util_bitcount_native((uint32_t)n) +
                  util_bitcount_native((uint32_t)(n >> 32));
                  #elif defined(_MSC_VER) && DETECT_ARCH_X86_64
                  return __popcnt64(n);
                  #elif defined(_MSC_VER) && DETECT_ARCH_X86
                  return __popcnt((uint32_t)n) + __popcnt((uint32_t)(n >> 32));
                  #else
                  /* This path should not be taken on x86 if has_popcnt is true. */
                  return util_bitcount64_default(n);
                  #endif
            }

            /**
             * Return number of bits set in n.
             *
             * This function performs a runtime check on x86 to use the fast native
             * POPCNT instruction if available. This is the preferred general-purpose API.
             */
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
             * Widens the given bit mask by a multiplier, meaning that it will
             * replicate each bit by that amount.
             */
            static inline uint32_t
            util_widen_mask(uint32_t mask, unsigned multiplier)
            {
                  uint32_t new_mask = 0;
                  u_foreach_bit(i, mask)
                  new_mask |= ((1u << multiplier) - 1u) << (i * multiplier);
                  return new_mask;
            }

            #ifdef __cplusplus
}

/* For C++ code, util_bitcount_fast provides a compile-time dispatch path
 * where CPU features are known ahead of time, avoiding the runtime check.
 */
enum util_popcnt {
      POPCNT_NO,
      POPCNT_YES,
      POPCNT_INVALID,
};

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
