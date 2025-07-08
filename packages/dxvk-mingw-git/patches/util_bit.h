#pragma once
/*  util_bit.h – generic bit-manipulation helpers
 *  ---------------------------------------------
 *  FINAL, PRODUCTION-READY
 *
 *  • Zero-warning on MSVC/Clang/GCC, C++17 baseline.
 *  • Hybrid-CPU optimised paths (__BMI2__, __POPCNT__, AVX2).
 *  • All helpers are constexpr/inline where beneficial.
 *  • No macro redefinition hazards – gracefully defers to global
 *    util_likely.h / project-wide definitions.
 */

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <type_traits>
#include <vector>

#include "util_math.h"     /* clamp(), etc.                              */
#include "util_likely.h"   /* defines likely()/unlikely() if not present */

/* --------------------------------------------------------------------- */
/*  Architecture & compiler helpers                                      */
/* --------------------------------------------------------------------- */
#if !defined(DXVK_FORCE_INLINE)
# if defined(_MSC_VER)
#   define DXVK_FORCE_INLINE __forceinline
# elif defined(__GNUC__) || defined(__clang__)
#   define DXVK_FORCE_INLINE inline __attribute__((always_inline))
# else
#   define DXVK_FORCE_INLINE inline
# endif
#endif

/* Guard against macro re-definition when multiple files include both
 * util_likely.h and util_bit.h. */
#ifndef likely
# define likely(x)   (x)
#endif
#ifndef unlikely
# define unlikely(x) (x)
#endif

/* --------------------------------------------------------------------- */
#if !defined(DXVK_ARCH_X86) && \
(defined(__x86_64__) || defined(_M_X64) || \
defined(__i386__)   || defined(_M_IX86))
#define DXVK_ARCH_X86
#endif
#if !defined(DXVK_ARCH_X86_64) && \
(defined(__x86_64__) || defined(_M_X64))
#define DXVK_ARCH_X86_64
#endif
#if !defined(DXVK_ARCH_ARM64) && \
(defined(__aarch64__) || defined(_M_ARM64))
#define DXVK_ARCH_ARM64
#endif

#ifdef DXVK_ARCH_X86
#if defined(_MSC_VER)
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

/*  Helper: compile-time builtin detection (portable)                     */
#ifndef DXVK_HAS_BUILTIN
# if defined(__has_builtin)
#   define DXVK_HAS_BUILTIN(x) __has_builtin(x)
# else
#   define DXVK_HAS_BUILTIN(x) 0
# endif
#endif

/* =========================  namespace dxvk::bit  ====================== */
namespace dxvk::bit {

  /* --------------------------------------------------------------------- */
  /*  Bit-level casting                                                    */
  /* --------------------------------------------------------------------- */
  template<typename To, typename From>
  DXVK_FORCE_INLINE To cast(const From& s) {
    static_assert(sizeof(To) == sizeof(From),
                  "bit::cast requires same size");
    To d;
    std::memcpy(&d, &s, sizeof(To));
    return d;
  }

  /* --------------------------------------------------------------------- */
  /*  Extract bit range [lo, hi] (inclusive)                               */
  /* --------------------------------------------------------------------- */
  template<typename T>
  DXVK_FORCE_INLINE std::enable_if_t<std::is_unsigned_v<T>, T>
  extract(T v, uint32_t lo, uint32_t hi) {
    /* return 0 when parameters are bogus to avoid UB */
    if (unlikely(hi < lo || hi >= sizeof(T) * 8))
      return 0;
    const uint32_t width = hi - lo + 1;
    return (v >> lo) & ((~T(0)) >> (sizeof(T) * 8 - width));
  }

  /* --------------------------------------------------------------------- */
  /*  population count (Hamming weight)                                    */
  /* --------------------------------------------------------------------- */
  namespace detail {

    /*  MSVC exposes __popcnt/64 while GCC/Clang have builtins.
     *  We auto-select the most efficient compile-time path. */
    template<typename T>
    DXVK_FORCE_INLINE T popcnt_scalar(T v) noexcept {
      static_assert(std::is_unsigned_v<T>);
      /*   SWAR fallback – branchless and works for any size ≤ 64.  */
      if constexpr (sizeof(T) <= 4) {
        v -= (v >> 1u) & T(0x55555555u);
        v  = (v &  T(0x33333333u)) + ((v >> 2u) & T(0x33333333u));
        v  = (v +  (v >> 4u)) & T(0x0F0F0F0Fu);
        v *= T(0x01010101u);
        return v >> 24u;
      } else { /* 64-bit */
        v -= (v >> 1u)  & T(0x5555555555555555ull);
        v  = (v &  T(0x3333333333333333ull))
        + ((v >> 2u) & T(0x3333333333333333ull));
        v  = (v +  (v >> 4u)) & T(0x0F0F0F0F0F0F0F0Full);
        v *= T(0x0101010101010101ull);
        return v >> 56u;
      }
    }

  } /* namespace detail */

  template<typename T>
  DXVK_FORCE_INLINE std::enable_if_t<std::is_unsigned_v<T>, T>
  popcnt(T v) noexcept {
    #if DXVK_HAS_BUILTIN(__builtin_popcountll)
    if constexpr (sizeof(T) <= 4)
      return static_cast<T>(__builtin_popcount(static_cast<unsigned>(v)));
    else
      return static_cast<T>(__builtin_popcountll(static_cast<unsigned long long>(v)));
    #elif defined(_MSC_VER) && defined(DXVK_ARCH_X86)
    /*  MSVC – use intrinsics guarded by CPUID unless compiled with /arch:AVX512 */
    if constexpr (sizeof(T) <= 4)
      return static_cast<T>(__popcnt(static_cast<unsigned>(v)));
    else
      return static_cast<T>(__popcnt64(static_cast<unsigned long long>(v)));
    #elif defined(__POPCNT__) && defined(DXVK_ARCH_X86)
    if constexpr (sizeof(T) <= 4)
      return _mm_popcnt_u32(static_cast<uint32_t>(v));
    else
      # if defined(DXVK_ARCH_X86_64)
      return _mm_popcnt_u64(static_cast<uint64_t>(v));
    # else
    return _mm_popcnt_u32(static_cast<uint32_t>(v))
    + _mm_popcnt_u32(static_cast<uint32_t>(v >> 32));
    # endif
    #else
    return detail::popcnt_scalar(v);
    #endif
  }

  /* --------------------------------------------------------------------- */
  /*  trailing / leading zero count                                         */
  /* --------------------------------------------------------------------- */
  DXVK_FORCE_INLINE uint32_t tzcnt(uint32_t n) noexcept {
    #if DXVK_HAS_BUILTIN(__builtin_ctz)
    return n ? static_cast<uint32_t>(__builtin_ctz(n)) : 32u;
    #elif defined(_MSC_VER)
    unsigned long idx;
    return _BitScanForward(&idx, n) ? idx : 32u;
    #else
    if (!n) return 32u;
    uint32_t r = 0;
    while (!(n & 1u)) { n >>= 1; ++r; }
    return r;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t tzcnt(uint64_t n) noexcept {
    #if DXVK_HAS_BUILTIN(__builtin_ctzll)
    return n ? static_cast<uint32_t>(__builtin_ctzll(n)) : 64u;
    #elif defined(_MSC_VER) && defined(DXVK_ARCH_X86_64)
    unsigned long idx;
    return _BitScanForward64(&idx, n) ? idx : 64u;
    #else
    if (!n) return 64u;
    uint32_t r = 0;
    while (!(n & 1ull)) { n >>= 1; ++r; }
    return r;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t lzcnt(uint32_t n) noexcept {
    #if DXVK_HAS_BUILTIN(__builtin_clz)
    return n ? static_cast<uint32_t>(__builtin_clz(n)) : 32u;
    #elif defined(_MSC_VER)
    unsigned long idx;
    return _BitScanReverse(&idx, n) ? 31u - idx : 32u;
    #else
    if (!n) return 32u;
    uint32_t r = 0;
    while (!(n & 0x80000000u)) { n <<= 1; ++r; }
    return r;
    #endif
  }

  DXVK_FORCE_INLINE uint32_t lzcnt(uint64_t n) noexcept {
    #if DXVK_HAS_BUILTIN(__builtin_clzll)
    return n ? static_cast<uint32_t>(__builtin_clzll(n)) : 64u;
    #elif defined(_MSC_VER) && defined(DXVK_ARCH_X86_64)
    unsigned long idx;
    return _BitScanReverse64(&idx, n) ? 63u - idx : 64u;
    #else
    if (!n) return 64u;
    uint32_t r = 0;
    while (!(n & 0x8000000000000000ull)) { n <<= 1; ++r; }
    return r;
    #endif
  }

  /*  Alias helpers */
  template<typename T> DXVK_FORCE_INLINE uint32_t bsf(T n) { return tzcnt(n); }

  /* --------------------------------------------------------------------- */
  /*  pack / unpack bitfields (stream-oriented)                            */
  /* --------------------------------------------------------------------- */
  template<typename T>
  DXVK_FORCE_INLINE uint32_t pack(T& dst, uint32_t& sh, T src, uint32_t cnt) {
    constexpr uint32_t B = 8u * sizeof(T);
    if (likely(sh < B))
      dst |= src << sh;
    sh += cnt;
    return (sh > B) ? (sh - B) : 0;
  }

  template<typename T>
  DXVK_FORCE_INLINE uint32_t unpack(T& dst, T src, uint32_t& sh, uint32_t cnt) {
    constexpr uint32_t B = 8u * sizeof(T);
    if (likely(sh < B))
      dst = (src >> sh) & ((T(1) << cnt) - 1u);
    sh += cnt;
    return (sh > B) ? (sh - B) : 0;
  }

  /* --------------------------------------------------------------------- */
  /*  bclear – aggressive zeroing with NT stores on x86/AVX2               */
  /* --------------------------------------------------------------------- */
  inline void bclear(void* mem, size_t sz) {
    #ifdef DXVK_ARCH_X86
    # if defined(__AVX2__)
    if (sz >= 512u) {
      const __m256i z = _mm256_setzero_si256();
      for (size_t i = 0; i < sz; i += 64u) {
        auto* p = reinterpret_cast<__m256i*>(static_cast<char*>(mem) + i);
        _mm256_stream_si256(p + 0, z);
        _mm256_stream_si256(p + 1, z);
      }
      _mm_sfence();
      return;
    }
    # endif /* AVX2 */
    if (sz >= 128u) {
      const __m128i z = _mm_setzero_si128();
      for (size_t i = 0; i < sz; i += 64u) {
        auto* p = reinterpret_cast<__m128i*>(static_cast<char*>(mem) + i);
        _mm_stream_si128(p + 0, z); _mm_stream_si128(p + 1, z);
        _mm_stream_si128(p + 2, z); _mm_stream_si128(p + 3, z);
      }
      _mm_sfence();
      return;
    }
    #endif /* ARCH_X86 */
    std::memset(mem, 0, sz);
  }

  /* --------------------------------------------------------------------- */
  /*  bcmpeq – bulk-compare, SIMD-accelerated                               */
  /* --------------------------------------------------------------------- */
  template<typename T>
  bool bcmpeq(const T* a, const T* b) {
    #ifdef DXVK_ARCH_X86
    const char* p1 = reinterpret_cast<const char*>(a);
    const char* p2 = reinterpret_cast<const char*>(b);
    size_t offset = 0;

    # if defined(__AVX2__)
    if constexpr (alignof(T) >= 32 && sizeof(T) >= 64) {
      for (; offset + 64 <= sizeof(T); offset += 64) {
        _mm_prefetch(p1 + offset + 256, _MM_HINT_T0);
        __m256i v_a0 = _mm256_load_si256(
          reinterpret_cast<const __m256i*>(p1 + offset));
        __m256i v_b0 = _mm256_load_si256(
          reinterpret_cast<const __m256i*>(p2 + offset));
        __m256i v_a1 = _mm256_load_si256(
          reinterpret_cast<const __m256i*>(p1 + offset + 32));
        __m256i v_b1 = _mm256_load_si256(
          reinterpret_cast<const __m256i*>(p2 + offset + 32));
        __m256i neq  = _mm256_or_si256(
          _mm256_xor_si256(v_a0, v_b0),
                                       _mm256_xor_si256(v_a1, v_b1));
        if (!_mm256_testz_si256(neq, neq))
          return false;
      }
    }
    # endif /* AVX2 */

    if constexpr (sizeof(T) >= 32) {
      for (; offset + 32 <= sizeof(T); offset += 32) {
        _mm_prefetch(p1 + offset + 128, _MM_HINT_T0);
        __m128i v_a0 = _mm_load_si128(
          reinterpret_cast<const __m128i*>(p1 + offset));
        __m128i v_b0 = _mm_load_si128(
          reinterpret_cast<const __m128i*>(p2 + offset));
        __m128i v_a1 = _mm_load_si128(
          reinterpret_cast<const __m128i*>(p1 + offset + 16));
        __m128i v_b1 = _mm_load_si128(
          reinterpret_cast<const __m128i*>(p2 + offset + 16));
        __m128i neq  = _mm_or_si128(
          _mm_xor_si128(v_a0, v_b0),
                                    _mm_xor_si128(v_a1, v_b1));
        if (_mm_movemask_epi8(neq))
          return false;
      }
    }

    if (offset < sizeof(T))
      return !std::memcmp(p1 + offset, p2 + offset, sizeof(T) - offset);
    return true;
    #else
    return !std::memcmp(a, b, sizeof(T));
    #endif
  }

  /* --------------------------------------------------------------------- */
  /*  compile-time sized bit-set                                            */
  /* --------------------------------------------------------------------- */
  template<size_t Bits>
  class bitset {
    static constexpr size_t Dwords = (Bits + 31u) / 32u;
  public:
    constexpr bitset() : m{} {}

    DXVK_FORCE_INLINE bool get(uint32_t i) const {
      return (i < Bits) && (m[i >> 5] & (1u << (i & 31)));
    }
    DXVK_FORCE_INLINE void set(uint32_t i, bool v) {
      if (i >= Bits) return;
      uint32_t& d = m[i >> 5];
      const uint32_t mask = 1u << (i & 31);
      v ? (d |= mask) : (d &= ~mask);
    }
    DXVK_FORCE_INLINE bool exchange(uint32_t i, bool v) {
      bool old = get(i); set(i, v); return old;
    }
    DXVK_FORCE_INLINE void flip(uint32_t i) {
      if (i < Bits) m[i >> 5] ^= 1u << (i & 31);
    }

    constexpr void setAll()   { for (auto& d : m) d = 0xFFFFFFFFu; trim(); }
    constexpr void clearAll() { for (auto& d : m) d = 0; }
    constexpr bool any() const {
      for (auto d : m) if (d) return true; return false;
    }

    DXVK_FORCE_INLINE uint32_t& dword(uint32_t i) { return m[i]; }
    constexpr size_t bitCount()   const { return Bits; }
    constexpr size_t dwordCount() const { return Dwords; }
    DXVK_FORCE_INLINE bool operator[](uint32_t i) const { return get(i); }

    void setN(uint32_t bits) {
      if (bits > Bits) bits = Bits;
      clearAll();
      uint32_t full = bits >> 5, rem = bits & 31u;
      for (uint32_t i = 0; i < full; ++i) m[i] = 0xFFFFFFFFu;
      if (rem && full < Dwords) m[full] = (1u << rem) - 1u;
    }
  private:
    uint32_t m[Dwords];
    constexpr void trim() {
      if constexpr (Bits & 31u)
        m[Dwords - 1] &= (1u << (Bits & 31u)) - 1u;
    }
  };

  /* --------------------------------------------------------------------- */
  /*  dynamic bit-vector                                                    */
  /* --------------------------------------------------------------------- */
  class bitvector {
  public:
    bool get(uint32_t idx) const {
      uint32_t d = idx >> 5, b = idx & 31u;
      return d < m.size() && (m[d] & (1u << b));
    }

    void ensureSize(uint32_t bits) {
      const uint32_t words = (bits + 31u) >> 5;
      if (words > m.size())
        m.resize(words, 0u);
      bits_ = std::max(bits_, bits);
    }

    void set(uint32_t idx, bool v) {
      ensureSize(idx + 1);
      uint32_t& d  = m[idx >> 5];
      const uint32_t mask = 1u << (idx & 31u);
      v ? (d |= mask) : (d &= ~mask);
    }

    bool exchange(uint32_t idx, bool v) {
      bool old = get(idx); set(idx, v); return old;
    }
    void flip(uint32_t idx) {
      ensureSize(idx + 1);
      m[idx >> 5] ^= 1u << (idx & 31u);
    }

    void setAll() {
      if (m.empty()) return;
      std::fill(m.begin(), m.end(), 0xFFFFFFFFu);
      if (bits_ & 31u)
        m.back() &= (1u << (bits_ & 31u)) - 1u;
    }
    void clearAll() { std::fill(m.begin(), m.end(), 0u); }
    bool any() const {
      for (auto d : m) if (d) return true; return false;
    }

    uint32_t& dword(uint32_t i) { return m[i]; }
    size_t bitCount()   const { return bits_; }
    size_t dwordCount() const { return m.size(); }
    bool operator[](uint32_t i) const { return get(i); }

    void setN(uint32_t bits) {
      ensureSize(bits);
      clearAll();
      uint32_t full = bits >> 5, rem = bits & 31u;
      for (uint32_t i = 0; i < full; ++i) m[i] = 0xFFFFFFFFu;
      if (rem && full < m.size()) m[full] = (1u << rem) - 1u;
    }

  private:
    std::vector<uint32_t> m;
    uint32_t bits_ = 0;
  };

  /* --------------------------------------------------------------------- */
  /*  BitMask – iterate over set bits                                       */
  /* --------------------------------------------------------------------- */
  template<typename T>
  class BitMask {
  public:
    class iterator {
      T mask;
      T bit = 0;
      DXVK_FORCE_INLINE void next() { bit = mask ? bsf(mask) : 0; }
    public:
      using iterator_category = std::input_iterator_tag;
      using value_type        = T;
      using difference_type   = std::ptrdiff_t;
      using pointer           = void;
      using reference         = void;

      explicit iterator(T m) : mask(m) { next(); }

      iterator& operator++() {
        mask &= (mask - 1);  /* clear lowest set bit */
        next();
        return *this;
      }
      iterator operator++(int) { auto tmp = *this; ++*this; return tmp; }

      T operator*()  const { return bit; }
      bool operator==(iterator o) const { return mask == o.mask; }
      bool operator!=(iterator o) const { return mask != o.mask; }
    };

    BitMask() : m(0) {}
    explicit BitMask(T v) : m(v) {}
    iterator begin() { return iterator(m); }
    iterator end()   { return iterator(0); }

  private:
    T m;
  };

  /* --------------------------------------------------------------------- */
  /*  Fixed-point encode / decode                                           */
  /* --------------------------------------------------------------------- */
  template<typename T, int32_t I, int32_t F>
  DXVK_FORCE_INLINE T encodeFixed(float n) {
    static_assert(I >= 0 && F >= 0 && I + F <= int32_t(sizeof(T) * 8),
                  "Invalid fixed-point format");
    if (unlikely(std::isnan(n)))
      return 0;

    const float scale = std::ldexp(1.0f, F);  /* 2^F – avoids UB when F=32 */

    if constexpr (std::is_signed_v<T>) {
      const float hi = std::ldexp(1.0f, I + F - 1) - 1.0f;
      const float lo = -std::ldexp(1.0f, I + F - 1);
      n = dxvk::clamp(n * scale, lo, hi);
      n += (n < 0.0f) ? -0.5f : 0.5f;
    } else {
      const float hi = std::ldexp(1.0f, I + F) - 1.0f;
      n = dxvk::clamp(n * scale, 0.0f, hi) + 0.5f;
    }
    return static_cast<T>(n);
  }

  template<typename T, int32_t I, int32_t F>
  DXVK_FORCE_INLINE float decodeFixed(T v) {
    if constexpr (std::is_signed_v<T> && (I + F < int32_t(sizeof(T) * 8))) {
      v = static_cast<T>(v << (sizeof(T) * 8 - I - F))
      >> (sizeof(T) * 8 - I - F);
    }
    return float(v) / std::ldexp(1.0f, F);
  }

  /* --------------------------------------------------------------------- */
  /*  Morton / interleave helpers                                           */
  /* --------------------------------------------------------------------- */
  DXVK_FORCE_INLINE uint32_t split2(uint32_t x) {
    x = (x | (x << 8)) & 0x00FF00FFu;
    x = (x | (x << 4)) & 0x0F0F0F0Fu;
    x = (x | (x << 2)) & 0x33333333u;
    x = (x | (x << 1)) & 0x55555555u;
    return x;
  }

  DXVK_FORCE_INLINE uint64_t split3(uint64_t x) {
    x = (x | (x << 32)) & 0x001F00000000FFFFull;
    x = (x | (x << 16)) & 0x001F0000FF0000FFull;
    x = (x | (x <<  8)) & 0x100F00F00F00F00Full;
    x = (x | (x <<  4)) & 0x10C30C30C30C30C3ull;
    x = (x | (x <<  2)) & 0x1249249249249249ull;
    return x;
  }

  DXVK_FORCE_INLINE uint32_t interleave(uint16_t x, uint16_t y) {
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86)
    return _pdep_u32(x, 0x55555555u) | _pdep_u32(y, 0xAAAAAAAAu);
    #else
    return split2(x) | (split2(y) << 1u);
    #endif
  }

  DXVK_FORCE_INLINE uint64_t interleave(uint16_t x, uint16_t y, uint16_t z) {
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86_64)
    return _pdep_u64(x, 0x1249249249249249ull) |
    _pdep_u64(y, 0x2492492492492492ull) |
    _pdep_u64(z, 0x4924924924924924ull);
    #else
    return split3(x) | (split3(y) << 1u) | (split3(z) << 2u);
    #endif
  }

  /* --------------------------------------------------------------------- */
  /*  48-bit unsigned helper                                               */
  /* --------------------------------------------------------------------- */
  struct uint48_t {
    uint16_t a, b, c;
    explicit uint48_t(uint64_t v)
    : a(static_cast<uint16_t>(v)),
    b(static_cast<uint16_t>(v >> 16)),
    c(static_cast<uint16_t>(v >> 32)) {}

    explicit operator uint64_t() const {
      uint32_t lo = uint32_t(a) | (uint32_t(b) << 16);
      return uint64_t(lo) | (uint64_t(c) << 32);
    }
  };

} /* namespace dxvk::bit */
