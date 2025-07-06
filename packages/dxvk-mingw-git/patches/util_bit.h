#pragma once

#if (defined(__x86_64__) && !defined(__arm64ec__)) || (defined(_M_X64) && !defined(_M_ARM64EC)) \
|| defined(__i386__)   || defined(_M_IX86)       || defined(__e2k__)
#define DXVK_ARCH_X86
#if defined(__x86_64__) || defined(_M_X64) || defined(__e2k__)
#define DXVK_ARCH_X86_64
#endif
#elif defined(__aarch64__) || defined(_M_ARM64) || defined(_M_ARM64EC)
#define DXVK_ARCH_ARM64
#endif

#ifdef DXVK_ARCH_X86
#ifndef _MSC_VER
#if defined(__WINE__) && defined(__clang__)
#pragma push_macro("_WIN32")
#undef  _WIN32
#endif
#include <x86intrin.h>
#if defined(__WINE__) && defined(__clang__)
#pragma pop_macro("_WIN32")
#endif
#else
#include <intrin.h>
#endif
#endif

#include "util_likely.h"
#include "util_math.h"

#include <algorithm>     /* std::clamp */
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <type_traits>
#include <vector>

namespace dxvk::bit {

  /*───────────────────────────────────────────────────────────────────────────
   *  Helper macro for always-inline
   *─────────────────────────────────────────────────────────────────────────*/
  #if defined(_MSC_VER)
  #define DXVK_FORCE_INLINE __forceinline
  #elif defined(__GNUC__) || defined(__clang__)
  #define DXVK_FORCE_INLINE inline __attribute__((always_inline))
  #else
  #define DXVK_FORCE_INLINE inline
  #endif

  /*───────────────────────────────────────────────────────────────────────────
   * 1.  bit::cast – safe bit-cast without UB
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T, typename J>
  DXVK_FORCE_INLINE T cast(const J& src) {
    static_assert(sizeof(T) == sizeof(J));
    static_assert(std::is_trivially_copyable_v<J> && std::is_trivial_v<T>);
    T dst;
    std::memcpy(&dst, &src, sizeof(T));
    return dst;
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 2.  extract bits [fst … lst]  (inclusive)
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T>
  DXVK_FORCE_INLINE T extract(T value, uint32_t fst, uint32_t lst) {
    return (value >> fst) & ~(~T(0) << (lst - fst + 1u));
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 3.  population count – POPCNT or software fallback
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T>
  DXVK_FORCE_INLINE T popcnt(T n) {
    #if defined(__POPCNT__) && defined(DXVK_ARCH_X86)
    if constexpr (sizeof(T) <= 4)
      return static_cast<T>(__builtin_popcount(static_cast<uint32_t>(n)));
    else
      return static_cast<T>(__builtin_popcountll(static_cast<uint64_t>(n)));
    #else
    n -= ((n >> 1u) & T(0x5555555555555555ull));
    n  = (n &  T(0x3333333333333333ull)) + ((n >> 2u) & T(0x3333333333333333ull));
    n  = (n +  (n >> 4u)) & T(0x0f0f0f0f0f0f0f0full);
    n *=  T(0x0101010101010101ull);
    return n >> (8u * (sizeof(T) - 1u));
    #endif
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 4.  trailing-/leading-zero helpers
   *─────────────────────────────────────────────────────────────────────────*/
  DXVK_FORCE_INLINE uint32_t tzcnt(uint32_t n) {
    #if defined(_MSC_VER) && !defined(__clang__)
    if (!n) return 32u;
    return _tzcnt_u32(n);
    #elif defined(__BMI__)
    return __tzcnt_u32(n);
    #elif defined(DXVK_ARCH_X86) && (defined(__GNUC__) || defined(__clang__))
    uint32_t res, tmp;
    asm ("tzcnt %2, %0\n\t"
    "mov  $32, %1\n\t"
    "test %2,%2\n\t"
    "cmovz %1,%0"
    : "=&r"(res), "=&r"(tmp) : "r"(n) : "cc");
    return res;
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? static_cast<uint32_t>(__builtin_ctz(n)) : 32u;
    #else
    uint32_t r = 31u;
    n &= -int32_t(n);
    r -= (n & 0x0000FFFFu) ? 16u : 0u;
    r -= (n & 0x00FF00FFu) ?  8u : 0u;
    r -= (n & 0x0F0F0F0Fu) ?  4u : 0u;
    r -= (n & 0x33333333u) ?  2u : 0u;
    r -= (n & 0x55555555u) ?  1u : 0u;
    return n ? r : 32u;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t tzcnt(uint64_t n) {
    #if defined(DXVK_ARCH_X86_64) && defined(_MSC_VER) && !defined(__clang__)
    if (!n) return 64u;
    return static_cast<uint32_t>(_tzcnt_u64(n));
    #elif defined(DXVK_ARCH_X86_64) && defined(__BMI__)
    return static_cast<uint32_t>(__tzcnt_u64(n));
    #elif defined(DXVK_ARCH_X86_64) && (defined(__GNUC__) || defined(__clang__))
    uint64_t res, tmp;
    asm ("tzcnt %2, %0\n\t"
    "mov  $64, %1\n\t"
    "test %2,%2\n\t"
    "cmovz %1,%0"
    : "=&r"(res), "=&r"(tmp) : "r"(n) : "cc");
    return static_cast<uint32_t>(res);
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? static_cast<uint32_t>(__builtin_ctzll(n)) : 64u;
    #else
    uint32_t lo = static_cast<uint32_t>(n);
    if (lo) return tzcnt(lo);
    uint32_t hi = static_cast<uint32_t>(n >> 32u);
    return tzcnt(hi) + 32u;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t bsf(uint32_t n) {
    #if (defined(__GNUC__) || defined(__clang__)) && !defined(__BMI__) && defined(DXVK_ARCH_X86)
    uint32_t res;
    asm ("tzcnt %1,%0" : "=r"(res) : "r"(n) : "cc");
    return res;
    #else
    return tzcnt(n);
    #endif
  }

  DXVK_FORCE_INLINE uint32_t bsf(uint64_t n) {
    #if (defined(__GNUC__) || defined(__clang__)) && !defined(__BMI__) && defined(DXVK_ARCH_X86_64)
    uint64_t res;
    asm ("tzcnt %1,%0" : "=r"(res) : "r"(n) : "cc");
    return static_cast<uint32_t>(res);
    #else
    return tzcnt(n);
    #endif
  }

  DXVK_FORCE_INLINE uint32_t lzcnt(uint32_t n) {
    #if defined(_MSC_VER) && !defined(__clang__) && !defined(__LZCNT__)
    unsigned long idx;
    if (!n) return 32u;
    _BitScanReverse(&idx, n);
    return 31u - idx;
    #elif (defined(_MSC_VER) && !defined(__clang__)) || defined(__LZCNT__)
    return _lzcnt_u32(n);
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? static_cast<uint32_t>(__builtin_clz(n)) : 32u;
    #else
    uint32_t r = 0u;
    if (!n) return 32u;
    if (n <= 0x0000FFFFu) { r += 16u; n <<= 16u; }
    if (n <= 0x00FFFFFFu) { r +=  8u; n <<=  8u; }
    if (n <= 0x0FFFFFFFu) { r +=  4u; n <<=  4u; }
    if (n <= 0x3FFFFFFFu) { r +=  2u; n <<=  2u; }
    if (n <= 0x7FFFFFFFu) { r +=  1u; }
    return r;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t lzcnt(uint64_t n) {
    #if defined(DXVK_ARCH_X86_64) && defined(_MSC_VER) && !defined(__clang__) && !defined(__LZCNT__)
    unsigned long idx;
    if (!n) return 64u;
    _BitScanReverse64(&idx, n);
    return 63u - idx;
    #elif defined(DXVK_ARCH_X86_64) && ((defined(_MSC_VER) && !defined(__clang__)) && defined(__LZCNT__))
    return static_cast<uint32_t>(_lzcnt_u64(n));
    #elif defined(DXVK_ARCH_X86_64) && (defined(__GNUC__) || defined(__clang__))
    return n ? static_cast<uint32_t>(__builtin_clzll(n)) : 64u;
    #else
    uint32_t hi = static_cast<uint32_t>(n >> 32u);
    if (hi) return lzcnt(hi);
    return lzcnt(static_cast<uint32_t>(n)) + 32u;
    #endif
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 5.  bit-packing helpers
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T>
  DXVK_FORCE_INLINE uint32_t pack(T& dst, uint32_t& shift, T src, uint32_t count) {
    constexpr uint32_t Bits = 8u * sizeof(T);
    if (likely(shift < Bits))
      dst |= src << shift;
    shift += count;
    return (shift > Bits) ? shift - Bits : 0u;
  }

  template<typename T>
  DXVK_FORCE_INLINE uint32_t unpack(T& dst, T src, uint32_t& shift, uint32_t count) {
    constexpr uint32_t Bits = 8u * sizeof(T);
    if (likely(shift < Bits))
      dst = (src >> shift) & ((T(1) << count) - 1u);
    shift += count;
    return (shift > Bits) ? shift - Bits : 0u;
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 6.  bclear – hybrid 128-/256-bit for turbo-friendly performance
   *─────────────────────────────────────────────────────────────────────────*/
  inline void bclear(void* mem, size_t size) {
    #if defined(DXVK_ARCH_X86) && defined(__AVX2__) && !defined(_WIN32)
    /* Use 256-bit nontemporal only when it amortises the AVX turbo drop. */
    if (size >= 512u) {
      const __m256i z = _mm256_setzero_si256();
      for (size_t i = 0; i < size; i += 64u) {
        auto* p = reinterpret_cast<__m256i*>(static_cast<char*>(mem) + i);
        _mm256_stream_si256(p + 0, z);
        _mm256_stream_si256(p + 1, z);
      }
      return;
    }
    #endif
    /* -------- default 128-bit SSE path (fast-turbo even on RPL) -------- */
    #if defined(DXVK_ARCH_X86) && (defined(__GNUC__) || defined(__clang__) || defined(_MSC_VER))
    const __m128i zero = _mm_setzero_si128();
    for (size_t i = 0; i < size; i += 64u) {
      auto* ptr = reinterpret_cast<__m128i*>(mem) + i / 16u;
      _mm_stream_si128(ptr + 0, zero);
      _mm_stream_si128(ptr + 1, zero);
      _mm_stream_si128(ptr + 2, zero);
      _mm_stream_si128(ptr + 3, zero);
    }
    #else
    std::memset(mem, 0, size);
    #endif
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 7.  bcmpeq – hybrid SSE / AVX2 with early-out turbo-friendly path
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T>
  bool bcmpeq(const T* a, const T* b) {
    static_assert(alignof(T) >= 16);
    constexpr size_t Bytes = sizeof(T);

    #if defined(DXVK_ARCH_X86) && defined(__AVX2__) && !defined(_WIN32)
    /* Small structs (<128 B) stay on SSE to avoid AVX turbo drop */
    if constexpr (Bytes >= 128) {
      const auto* ai = reinterpret_cast<const __m256i*>(a);
      const auto* bi = reinterpret_cast<const __m256i*>(b);
      const size_t cnt = Bytes / 32u;
      for (size_t i = 0; i < cnt; ++i) {
        __m256i cmp = _mm256_cmpeq_epi8(_mm256_load_si256(ai + i),
                                        _mm256_load_si256(bi + i));
        if (_mm256_movemask_epi8(cmp) != -1) return false;
      }
      return true;
    }
    #endif

    #if defined(DXVK_ARCH_X86) && (defined(__GNUC__) || defined(__clang__) || defined(_MSC_VER))
    const auto* ai = reinterpret_cast<const __m128i*>(a);
    const auto* bi = reinterpret_cast<const __m128i*>(b);
    const size_t cnt = Bytes / 16u;
    for (size_t i = 0; i < cnt; ++i) {
      __m128i cmp = _mm_cmpeq_epi8(_mm_load_si128(ai + i),
                                   _mm_load_si128(bi + i));
      if (_mm_movemask_epi8(cmp) != 0xFFFF) return false;
    }
    return true;
    #else
    return std::memcmp(a, b, sizeof(T)) == 0;
    #endif
  }

  /*───────────────────────────────────────────────────────────────────────────
   * 8.  bitset – compile-time OR reduction for any()
   *─────────────────────────────────────────────────────────────────────────*/
  template <size_t Bits>
  class bitset {
    static constexpr size_t Dwords = align(Bits, 32u) / 32u;
  public:
    constexpr bitset() : m_dwords{} {}

    constexpr bool get(uint32_t idx) const {
      uint32_t dword = 0, bit = idx;
      if constexpr (Dwords > 1) { dword = idx / 32u; bit = idx % 32u; }
      return (m_dwords[dword] & (1u << bit)) != 0u;
    }

    constexpr void set(uint32_t idx, bool value) {
      uint32_t dword = 0, bit = idx;
      if constexpr (Dwords > 1) { dword = idx / 32u; bit = idx % 32u; }
      if (value) m_dwords[dword] |=  1u << bit;
      else       m_dwords[dword] &= ~(1u << bit);
    }

    constexpr bool exchange(uint32_t idx, bool v) { bool o = get(idx); set(idx, v); return o; }

    constexpr void flip(uint32_t idx) {
      uint32_t dword = 0, bit = idx;
      if constexpr (Dwords > 1) { dword = idx / 32u; bit = idx % 32u; }
      m_dwords[dword] ^= 1u << bit;
    }

    constexpr void setAll() {
      if constexpr (Bits % 32u == 0)
        for (size_t i = 0; i < Dwords; ++i) m_dwords[i] = 0xFFFFFFFFu;
        else {
          for (size_t i = 0; i + 1 < Dwords; ++i) m_dwords[i] = 0xFFFFFFFFu;
          m_dwords[Dwords - 1] = (1u << (Bits % 32u)) - 1u;
        }
    }

    constexpr void clearAll() { for (uint32_t& d : m_dwords) d = 0u; }

    constexpr bool any() const {
      if constexpr (Dwords == 1)       return m_dwords[0] != 0u;
      else if constexpr (Dwords == 2)  return (m_dwords[0] | m_dwords[1]) != 0u;
      else if constexpr (Dwords == 3)  return (m_dwords[0] | m_dwords[1] | m_dwords[2]) != 0u;
      else if constexpr (Dwords == 4)  return (m_dwords[0] | m_dwords[1] | m_dwords[2] | m_dwords[3]) != 0u;
      else {
        for (uint32_t d : m_dwords) if (d) return true;
        return false;
      }
    }

    constexpr uint32_t& dword(uint32_t idx) { return m_dwords[idx]; }

    constexpr size_t bitCount()   { return Bits;   }
    constexpr size_t dwordCount() { return Dwords; }

    constexpr bool operator[](uint32_t idx) const { return get(idx); }

    constexpr void setN(uint32_t bits) {
      uint32_t full = bits / 32u, off = bits % 32u;
      for (size_t i = 0; i < full; ++i) m_dwords[i] = 0xFFFFFFFFu;
      if (off) m_dwords[full] = (1u << off) - 1u;
    }

  private:
    uint32_t m_dwords[Dwords];
  };

  /*───────────────────────────────────────────────────────────────────────────
   * 9.  bitvector – dynamic size variant (audited)
   *─────────────────────────────────────────────────────────────────────────*/
  class bitvector {
  public:
    bool get(uint32_t idx) const {
      const uint32_t d = idx >> 5, b = idx & 31u;
      return (d < m_dwords.size()) && (m_dwords[d] & (1u << b));
    }

    void ensureSize(uint32_t bits) {
      const uint32_t words = (bits + 31u) >> 5;  /* ceil(bits / 32) */
      if (unlikely(words > m_dwords.size()))
        m_dwords.resize(words, 0u);
      m_bitCount = std::max<uint32_t>(m_bitCount, bits);
    }

    void set(uint32_t idx, bool value) {
      ensureSize(idx + 1u);
      const uint32_t d = idx >> 5, b = idx & 31u;
      if (value) m_dwords[d] |=  1u << b;
      else       m_dwords[d] &= ~(1u << b);
    }

    bool exchange(uint32_t idx, bool v) { bool o = get(idx); set(idx, v); return o; }

    void flip(uint32_t idx) {
      ensureSize(idx + 1u);
      const uint32_t d = idx >> 5, b = idx & 31u;
      m_dwords[d] ^= 1u << b;
    }

    void setAll() {
      if (m_dwords.empty()) return;
      for (size_t i = 0; i + 1 < m_dwords.size(); ++i) m_dwords[i] = 0xFFFFFFFFu;
      const uint32_t tail = m_bitCount & 31u;
      m_dwords.back() = tail ? ((1u << tail) - 1u) : 0xFFFFFFFFu;
    }

    void clearAll() { for (uint32_t& w : m_dwords) w = 0u; }

    bool any() const { for (uint32_t w : m_dwords) if (w) return true; return false; }

    uint32_t& dword(uint32_t idx) { return m_dwords[idx]; }

    size_t bitCount()  const { return m_bitCount;      }
    size_t dwordCount() const { return m_dwords.size(); }

    bool operator[](uint32_t idx) const { return get(idx); }

    void setN(uint32_t bits) {
      ensureSize(bits);
      const uint32_t full = bits >> 5, off = bits & 31u;
      for (uint32_t i = 0; i < full; ++i) m_dwords[i] = 0xFFFFFFFFu;
      if (off) m_dwords[full] = (1u << off) - 1u;
    }

  private:
    std::vector<uint32_t> m_dwords;
    uint32_t              m_bitCount = 0u;
  };

  /*───────────────────────────────────────────────────────────────────────────
   * 10. BMI2 PDEP Morton helpers – Raptor-Lake tuned
   *─────────────────────────────────────────────────────────────────────────*/
  #if defined(__BMI2__) && defined(DXVK_ARCH_X86)

  /* Fast SW helpers – surprisingly faster than PDEP on Intel 12-14th gen */
  constexpr uint32_t pdep_sw32(uint32_t src, uint32_t m) noexcept {
    uint32_t dst = 0, bit = 1;
    while (m) {
      if (m & 1u) {
        if (src & 1u) dst |= bit;
        src >>= 1;
      }
      m   >>= 1;
      bit <<= 1;
    }
    return dst;
  }
  constexpr uint64_t pdep_sw64(uint64_t src, uint64_t m) noexcept {
    uint64_t dst = 0, bit = 1;
    while (m) {
      if (m & 1ull) {
        if (src & 1ull) dst |= bit;
        src >>= 1;
      }
      m   >>= 1;
      bit <<= 1;
    }
    return dst;
  }

  /*-------------------------------------------------------------------------*
   *  pdep_()  – single entry point, ABI‐stable
   *-------------------------------------------------------------------------*/
  template<typename T, typename M>
  DXVK_FORCE_INLINE T pdep_(T value, M mask) noexcept {
    static_assert(std::is_unsigned_v<T> && std::is_unsigned_v<M>);
    #if defined(DXVK_FORCE_PDEP)        /* user override --------------------------------*/
    constexpr bool UsePdep = true;
    #elif defined(DXVK_ARCH_X86_64)     /* 64-bit build: PDEP slower than SW on RPL */
    constexpr bool UsePdep = false;
    #else                               /* 32-bit build: only _pdep_u32 exists anyway  */
    constexpr bool UsePdep = false;
    #endif

    if constexpr (sizeof(T) == 4) {
      if constexpr (UsePdep)
        return static_cast<T>(_pdep_u32(static_cast<uint32_t>(value),
                                        static_cast<uint32_t>(mask)));
        else
          return static_cast<T>(pdep_sw32(static_cast<uint32_t>(value),
                                          static_cast<uint32_t>(mask)));
    } else { /* sizeof(T)==8 --------------------------------------------------*/
      #if defined(DXVK_ARCH_X86_64)
      if constexpr (UsePdep)
        return static_cast<T>(_pdep_u64(static_cast<uint64_t>(value),
                                        static_cast<uint64_t>(mask)));
        else
          return static_cast<T>(pdep_sw64(static_cast<uint64_t>(value),
                                          static_cast<uint64_t>(mask)));
          #else   /* 32-bit compile – intrinsic unavailable */
          return static_cast<T>(pdep_sw64(static_cast<uint64_t>(value),
                                          static_cast<uint64_t>(mask)));
          #endif
    }
  }

  #endif /* __BMI2__ && DXVK_ARCH_X86 */

  /*-------------------------------------------------------------------------*
   * split2 / split3 – insert zero bits (Morton helpers)
   *-------------------------------------------------------------------------*/
  DXVK_FORCE_INLINE uint32_t split2(uint32_t x) noexcept {
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86)
    constexpr uint32_t mask = 0x5555'5555u;
    return pdep_(x, mask);
    #else
    x = (x ^ (x <<  8u)) & 0x00ff00ffu;
    x = (x ^ (x <<  4u)) & 0x0f0f0f0fu;
    x = (x ^ (x <<  2u)) & 0x33333333u;
    x = (x ^ (x <<  1u)) & 0x55555555u;
    return x;
    #endif
  }

  DXVK_FORCE_INLINE uint64_t split3(uint64_t x) noexcept {
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86)
    constexpr uint64_t mask = 0x1249'2492'4924'9249ull;
    return pdep_(x, mask);
    #else
    x = (x | (x << 32u)) & 0x001f00000000ffffull;
    x = (x | (x << 16u)) & 0x001f0000ff0000ffull;
    x = (x | (x <<  8u)) & 0x100f00f00f00f00full;
    x = (x | (x <<  4u)) & 0x10c30c30c30c30c3ull;
    x = (x | (x <<  2u)) & 0x1249249249249249ull;
    return x;
    #endif
  }

  /*-------------------------------------------------------------------------*
   * Morton interleave wrappers
   *-------------------------------------------------------------------------*/
  DXVK_FORCE_INLINE uint32_t interleave(uint16_t x, uint16_t y) noexcept {
    return split2(x) | (split2(y) << 1u);
  }
  DXVK_FORCE_INLINE uint64_t interleave(uint16_t x,
                                        uint16_t y,
                                        uint16_t z) noexcept {
    return split3(x) | (split3(y) << 1u) | (split3(z) << 2u);
  }


  /*───────────────────────────────────────────────────────────────────────────
   * 11. BitMask with fused-bsf iterator
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T>
  class BitMask {
  public:
    class iterator {
    public:
      using iterator_category = std::input_iterator_tag;
      using value_type        = T;
      using difference_type   = T;
      using pointer           = const T*;
      using reference         = T;

      explicit iterator(T m) : m_mask(m), m_curr(init()) {}
      iterator& operator++() { m_mask &= (m_mask - 1u); m_curr = init(); return *this; }
      iterator operator++(int) { iterator t = *this; ++(*this); return t; }
      T operator*() const { return m_curr; }
      bool operator==(iterator o) const { return m_mask == o.m_mask; }
      bool operator!=(iterator o) const { return m_mask != o.m_mask; }

    private:
      DXVK_FORCE_INLINE T init() const { return m_mask ? bsf(m_mask) : 0u; }
      T m_mask, m_curr;
    };

    BitMask() : m_mask(0) {}
    explicit BitMask(T n) : m_mask(n) {}

    iterator begin() { return iterator(m_mask); }
    iterator end()   { return iterator(0);      }

  private:
    T m_mask;
  };

  /*───────────────────────────────────────────────────────────────────────────
   * 12. Fixed-point helpers and uint48_t
   *─────────────────────────────────────────────────────────────────────────*/
  template<typename T, int32_t I, int32_t F>
  DXVK_FORCE_INLINE T encodeFixed(float n) {
    if (n != n) return 0u;          /* NaN -> 0 */

      n *= float(1u << F);

    if constexpr (std::is_signed_v<T>) {
      n = std::max(n, -float(1u << (I + F - 1u)));
      n = std::min(n,  float(1u << (I + F - 1u)) - 1.0f);
      n += (n < 0.0f) ? -0.5f : 0.5f;
    } else {
      n = std::clamp(n, 0.0f, float((1u << (I + F)) - 1u));
      n += 0.5f;
    }

    T out = static_cast<T>(n);
    if constexpr (std::is_signed_v<T>)
      out &= (static_cast<T>(1u) << (I + F)) - 1u;
    return out;
  }

  template<typename T, int32_t I, int32_t F>
  DXVK_FORCE_INLINE float decodeFixed(T n) {
    if constexpr (std::is_signed_v<T>)
      n -= (n & (static_cast<T>(1u) << (I + F - 1u))) << 1u;
    return float(n) / float(1u << F);
  }

  struct uint48_t {
    explicit uint48_t(uint64_t n)
    : a(uint16_t(n)), b(uint16_t(n >> 16)), c(uint16_t(n >> 32)) {}

    uint16_t a, b, c;

    explicit operator uint64_t() const {
      uint32_t lo = uint32_t(a) | (uint32_t(b) << 16);
      return uint64_t(lo) | (uint64_t(c) << 32);
    }
  };

} /* namespace dxvk::bit */
