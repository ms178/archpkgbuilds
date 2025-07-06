#pragma once

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <type_traits>
#include <vector>
#include <algorithm>

#include "util_math.h"
#include "util_likely.h"

/*── branch hints ──*/
#ifndef likely
# if defined(__GNUC__) || defined(__clang__)
#  define likely(x)   __builtin_expect(!!(x),1)
#  define unlikely(x) __builtin_expect(!!(x),0)
# else
#  define likely(x)   (x)
#  define unlikely(x) (x)
# endif
#endif

/*── architecture ──*/
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
#define DXVK_ARCH_X86
#if defined(__x86_64__) || defined(_M_X64)
#define DXVK_ARCH_X86_64
#endif
#elif defined(__aarch64__) || defined(_M_ARM64)
#define DXVK_ARCH_ARM64
#endif

#ifdef DXVK_ARCH_X86
#if !defined(_MSC_VER)
#include <x86intrin.h>
#else
#include <intrin.h>
#endif
#endif

/*── force-inline ──*/
#if defined(_MSC_VER)
#define DXVK_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define DXVK_FORCE_INLINE inline __attribute__((always_inline))
#else
#define DXVK_FORCE_INLINE inline
#endif

namespace dxvk::bit {

  /*============================================================================
   * 1.  bit::cast
   *==========================================================================*/
  template<typename To, typename From>
  DXVK_FORCE_INLINE To cast(const From& s) {
    static_assert(sizeof(To) == sizeof(From));
    To d; std::memcpy(&d, &s, sizeof(To)); return d;
  }

  /*============================================================================
   * 2.  extract bits
   *==========================================================================*/
  template<typename T>
  DXVK_FORCE_INLINE T extract(T v, uint32_t lo, uint32_t hi) {
    return unlikely(hi < lo) ? 0 : (v >> lo) & ~(~T(0) << (hi - lo + 1));
  }

  /*============================================================================
   * 3.  popcnt
   *==========================================================================*/
  template<typename T>
  DXVK_FORCE_INLINE T popcnt(T v) {
    if (__builtin_constant_p(v))
      return static_cast<T>(__builtin_popcountll(v));
    #if defined(__POPCNT__) && defined(DXVK_ARCH_X86)
    if constexpr (sizeof(T) <= 4)
      return static_cast<T>(__builtin_popcount(static_cast<uint32_t>(v)));
    else
      return static_cast<T>(__builtin_popcountll(static_cast<uint64_t>(v)));
    #else
    v -= (v >> 1u) & T(0x5555555555555555ull);
    v  = (v &  T(0x3333333333333333ull)) + ((v >> 2u) & T(0x3333333333333333ull));
    v  = (v +  (v >> 4u)) & T(0x0F0F0F0F0F0F0F0Full);
    v *= T(0x0101010101010101ull);
    return v >> (8u * (sizeof(T) - 1u));
    #endif
  }

  /*============================================================================
   * 4.  tzcnt / lzcnt / bsf helpers
   *==========================================================================*/
  DXVK_FORCE_INLINE uint32_t tzcnt(uint32_t n) {
    #if defined(_MSC_VER) && !defined(__clang__)
    unsigned long i; return _BitScanForward(&i,n)?i:32u;
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? __builtin_ctz(n) : 32u;
    #else
    if(!n) return 32u; uint32_t r=0; while(!(n&1)){n>>=1;++r;} return r;
    #endif
  }
  DXVK_FORCE_INLINE uint32_t tzcnt(uint64_t n) {
    #if defined(DXVK_ARCH_X86_64) && defined(_MSC_VER) && !defined(__clang__)
    unsigned long i; return _BitScanForward64(&i,n)?i:64u;
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? __builtin_ctzll(n) : 64u;
    #else
    if(!n) return 64u; uint32_t r=0; while(!(n&1ull)){n>>=1;++r;} return r;
    #endif
  }
  DXVK_FORCE_INLINE uint32_t bsf(uint32_t n){ return tzcnt(n); }
  DXVK_FORCE_INLINE uint32_t bsf(uint64_t n){ return tzcnt(n); }

  DXVK_FORCE_INLINE uint32_t lzcnt(uint32_t n) {
    #if defined(_MSC_VER) && !defined(__clang__)
    unsigned long i; return _BitScanReverse(&i,n)?31u-i:32u;
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? __builtin_clz(n) : 32u;
    #else
    if(!n) return 32u; uint32_t r=0; while(!(n&0x80000000u)){n<<=1;++r;} return r;
    #endif
  }
  DXVK_FORCE_INLINE uint32_t lzcnt(uint64_t n) {
    #if defined(DXVK_ARCH_X86_64) && defined(_MSC_VER) && !defined(__clang__)
    unsigned long i; return _BitScanReverse64(&i,n)?63u-i:64u;
    #elif defined(__GNUC__) || defined(__clang__)
    return n ? __builtin_clzll(n) : 64u;
    #else
    if(!n) return 64u; uint32_t r=0; while(!(n&0x8000000000000000ull)){n<<=1;++r;} return r;
    #endif
  }

  /*============================================================================
   * 5.  pack / unpack
   *==========================================================================*/
  template<typename T>
  DXVK_FORCE_INLINE uint32_t pack(T& dst,uint32_t& sh,T src,uint32_t cnt){
    constexpr uint32_t B=8u*sizeof(T);
    if(likely(sh<B)) dst|=src<<sh;
    sh+=cnt; return sh>B?sh-B:0;
  }
  template<typename T>
  DXVK_FORCE_INLINE uint32_t unpack(T& dst,T src,uint32_t& sh,uint32_t cnt){
    constexpr uint32_t B=8u*sizeof(T);
    if(likely(sh<B)) dst=(src>>sh)&((T(1)<<cnt)-1u);
    sh+=cnt; return sh>B?sh-B:0;
  }

  /*============================================================================
   * 6.  bclear – NT streaming zero, turbo-aware thresholds
   *==========================================================================*/
  inline void bclear(void* mem, size_t sz) {
    #if defined(DXVK_ARCH_X86) && defined(__AVX2__)
    if (sz >= 512u) {
      const __m256i z = _mm256_setzero_si256();
      for (size_t i = 0; i < sz; i += 64u) {
        auto* p = reinterpret_cast<__m256i*>(static_cast<char*>(mem) + i);
        _mm256_stream_si256(p+0, z); _mm256_stream_si256(p+1, z);
      }
      _mm_sfence(); return;
    }
    #endif
    #if defined(DXVK_ARCH_X86)
    if (sz >= 128u) {
      const __m128i z = _mm_setzero_si128();
      for (size_t i = 0; i < sz; i += 64u) {
        auto* p = reinterpret_cast<__m128i*>(static_cast<char*>(mem) + i);
        _mm_stream_si128(p+0, z); _mm_stream_si128(p+1, z);
        _mm_stream_si128(p+2, z); _mm_stream_si128(p+3, z);
      }
      _mm_sfence(); return;
    }
    #endif
    std::memset(mem, 0, sz);
  }

  /*============================================================================
   * 7.  bcmpeq – adaptive compare
   *==========================================================================*/
  template<typename T>
  bool bcmpeq(const T* a, const T* b) {
    constexpr size_t N = sizeof(T);

    if constexpr (N <= 16u) {          /* tiny structs: scalar cmp */
      uint64_t l1, l2; std::memcpy(&l1, a, sizeof(uint64_t));
      std::memcpy(&l2, b, sizeof(uint64_t));
      if (l1 != l2) return false;
      if constexpr (N > 8u)
        return std::memcmp(reinterpret_cast<const char*>(a) + 8,
                           reinterpret_cast<const char*>(b) + 8,
                           N - 8) == 0;
                           else
                             return true;
    }
    if constexpr (N <= 32u)            /* small: libc memcmp */
      return std::memcmp(a, b, N) == 0;

    #if defined(DXVK_ARCH_X86) && defined(__AVX2__) && !defined(_WIN32)
    if constexpr (N >= 128u && alignof(T) >= 32) {
      auto* pa = reinterpret_cast<const __m256i*>(a);
      auto* pb = reinterpret_cast<const __m256i*>(b);
      for (size_t i = 0; i < N / 32u; ++i) {
        __m256i cmp = _mm256_cmpeq_epi8(_mm256_load_si256(pa+i),
                                        _mm256_load_si256(pb+i));
        if (_mm256_movemask_epi8(cmp) != -1) return false;
      }
      return true;
    }
    #endif
    #if defined(DXVK_ARCH_X86)
    auto* pa = reinterpret_cast<const __m128i*>(a);
    auto* pb = reinterpret_cast<const __m128i*>(b);
    for (size_t i = 0; i < N / 16u; ++i) {
      __m128i cmp = _mm_cmpeq_epi8(_mm_load_si128(pa+i),
                                   _mm_load_si128(pb+i));
      if (_mm_movemask_epi8(cmp) != 0xFFFF) return false;
    }
    return true;
    #else
    return std::memcmp(a, b, N) == 0;
    #endif
  }

  /*============================================================================
   * 8.  fixed-size bitset   (unchanged logic)
   *==========================================================================*/
  template<size_t Bits>
  class bitset {
    static constexpr size_t Dwords=(Bits+31u)/32u;
  public:
    constexpr bitset():m{}{}
    DXVK_FORCE_INLINE bool get(uint32_t i)const{
      return (i<Bits)&&(m[i>>5]&(1u<<(i&31)));
    }
    DXVK_FORCE_INLINE void set(uint32_t i,bool v){
      if(i>=Bits) return;
      uint32_t& d=m[i>>5]; uint32_t msk=1u<<(i&31);
      v? d|=msk : d&=~msk;
    }
    DXVK_FORCE_INLINE bool exchange(uint32_t i,bool v){
      bool o=get(i); set(i,v); return o;
    }
    DXVK_FORCE_INLINE void flip(uint32_t i){ if(i<Bits) m[i>>5]^=1u<<(i&31); }

    constexpr void setAll(){for(auto&d:m)d=0xFFFFFFFFu; trim();}
    constexpr void clearAll(){for(auto&d:m)d=0;}
    constexpr bool any()const{for(auto d:m) if(d) return true; return false;}

    DXVK_FORCE_INLINE uint32_t& dword(uint32_t i){return m[i];}
    constexpr size_t bitCount()const{return Bits;}
    constexpr size_t dwordCount()const{return Dwords;}
    DXVK_FORCE_INLINE bool operator[](uint32_t i)const{return get(i);}

    void setN(uint32_t bits){
      if(bits>Bits) bits=Bits;
      uint32_t full=bits>>5, rem=bits&31u;
      for(auto&d:m)d=0;
      for(uint32_t i=0;i<full;++i) m[i]=0xFFFFFFFFu;
      if(rem&&full<Dwords) m[full]=(1u<<rem)-1u;
    }
  private:
    uint32_t m[Dwords];
    constexpr void trim(){ if constexpr(Bits&31u) m[Dwords-1]&=(1u<<(Bits&31u))-1u; }
  };

  /*============================================================================
   * 9.  bitvector, BitMask, fixed-point, Morton helpers, uint48_t
   *     (unchanged – identical to V4)
   *============================================================================*/

  #include <vector>
  #include <iterator>

  class bitvector {
  public:
    bool get(uint32_t idx)const{
      uint32_t d=idx>>5,b=idx&31;
      return d<m.size() && (m[d]&(1u<<b));
    }
    void ensureSize(uint32_t bits){
      uint32_t words=(bits+31)>>5;
      if(words>m.size()) m.resize(words,0);
      bits_=std::max(bits_,bits);
    }
    void set(uint32_t idx,bool v){
      ensureSize(idx+1);
      uint32_t& d=m[idx>>5]; uint32_t msk=1u<<(idx&31);
      v? d|=msk : d&=~msk;
    }
    bool exchange(uint32_t i,bool v){bool o=get(i); set(i,v); return o;}
    void flip(uint32_t i){ensureSize(i+1); m[i>>5]^=1u<<(i&31);}

    void setAll(){
      std::fill(m.begin(),m.end(),0xFFFFFFFFu);
      if(bits_&31u) m.back()&=(1u<<(bits_&31u))-1u;
    }
    void clearAll(){std::fill(m.begin(),m.end(),0);}
    bool any()const{for(auto d:m) if(d) return true; return false;}

    uint32_t& dword(uint32_t i){return m[i];}
    size_t bitCount()const{return bits_;}
    size_t dwordCount()const{return m.size();}
    bool operator[](uint32_t i)const{return get(i);}

    void setN(uint32_t bits){
      ensureSize(bits);
      std::fill(m.begin(),m.end(),0);
      uint32_t full=bits>>5,rem=bits&31;
      for(uint32_t i=0;i<full;++i) m[i]=0xFFFFFFFFu;
      if(rem&&full<m.size()) m[full]=(1u<<rem)-1u;
    }
  private:
    std::vector<uint32_t> m;
    uint32_t bits_=0;
  };

  /*============================================================================
   * 10. BitMask iterator helper
   *==========================================================================*/
  template<typename T>
  class BitMask {
  public:
    class iterator {
      T mask, bit=0;
      DXVK_FORCE_INLINE void next(){bit=mask? bsf(mask):0;}
    public:
      using iterator_category=std::input_iterator_tag;
      using value_type=T; using difference_type=T;
      explicit iterator(T m):mask(m){next();}
      iterator& operator++(){mask&=(mask-1); next(); return *this;}
      iterator operator++(int){auto t=*this;++(*this); return t;}
      T operator*()const{return bit;}
      bool operator==(iterator o)const{return mask==o.mask;}
      bool operator!=(iterator o)const{return mask!=o.mask;}
    };

    BitMask() : m(0) {}
    explicit BitMask(T v) : m(v) {}
    iterator begin(){return iterator(m);}
    iterator end(){return iterator(0);}
    private: T m;
  };

  /*============================================================================
   * 11. Fixed-point helpers
   *==========================================================================*/
  template<typename T,int32_t I,int32_t F>
  DXVK_FORCE_INLINE T encodeFixed(float n){
    static_assert(I>=0&&F>=0&&I+F<=int32_t(sizeof(T)*8));
    if(std::isnan(n)) return 0;
    const float scale=float(1u<<F);
    if constexpr (std::is_signed_v<T>) {
      const float hi=float((1ull<<(I+F-1))-1), lo=-float(1ull<<(I+F-1));
      n = dxvk::clamp(n*scale, lo, hi);
      n += n<0 ? -0.5f : 0.5f;
    } else {
      const float hi=float((1ull<<(I+F))-1);
      n = dxvk::clamp(n*scale, 0.f, hi)+0.5f;
    }
    return static_cast<T>(n);
  }

  template<typename T,int32_t I,int32_t F>
  DXVK_FORCE_INLINE float decodeFixed(T v){
    if constexpr(std::is_signed_v<T>)
      v = static_cast<T>(v<<(sizeof(T)*8-I-F))>>(sizeof(T)*8-I-F);
    return float(v)/float(1u<<F);
  }

  /*============================================================================
   * 12. Morton helpers
   *==========================================================================*/
  DXVK_FORCE_INLINE uint32_t split2(uint32_t x){
    x=(x|(x<<8)) &0x00FF00FFu;
    x=(x|(x<<4)) &0x0F0F0F0Fu;
    x=(x|(x<<2)) &0x33333333u;
    x=(x|(x<<1)) &0x55555555u;
    return x;
  }
  DXVK_FORCE_INLINE uint64_t split3(uint64_t x){
    x=(x|(x<<32))&0x001F00000000FFFFull;
    x=(x|(x<<16))&0x001F0000FF0000FFull;
    x=(x|(x<< 8))&0x100F00F00F00F00Full;
    x=(x|(x<< 4))&0x10C30C30C30C30C3ull;
    x=(x|(x<< 2))&0x1249249249249249ull;
    return x;
  }

  DXVK_FORCE_INLINE uint32_t interleave(uint16_t x,uint16_t y){
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86)
    return _pdep_u32(x,0x55555555u)|_pdep_u32(y,0xAAAAAAAAu);
    #else
    return split2(x)|(split2(y)<<1u);
    #endif
  }
  DXVK_FORCE_INLINE uint64_t interleave(uint16_t x,uint16_t y,uint16_t z){
    #if defined(__BMI2__) && defined(DXVK_ARCH_X86_64)
    return _pdep_u64(x,0x1249249249249249ull)
    | _pdep_u64(y,0x2492492492492492ull)
    | _pdep_u64(z,0x4924924924924924ull);
    #else
    return split3(x)|(split3(y)<<1u)|(split3(z)<<2u);
    #endif
  }

  /*============================================================================
   * 13. uint48_t helper
   *==========================================================================*/
  struct uint48_t{
    uint16_t a,b,c;
    explicit uint48_t(uint64_t v):a(v),b(v>>16),c(v>>32){}
    explicit operator uint64_t()const{
      uint32_t lo=uint32_t(a)|(uint32_t(b)<<16);
      return uint64_t(lo)|(uint64_t(c)<<32);
    }
  };

} /* namespace dxvk::bit */
