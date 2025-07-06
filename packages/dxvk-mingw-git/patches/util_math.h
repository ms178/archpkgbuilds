#pragma once

#include <cmath>
#include <cstdint>
#include <type_traits>

/* fast-inline + branch hint -------------------------------------------*/
#if defined(_MSC_VER)
#define DXVK_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define DXVK_FORCE_INLINE inline __attribute__((always_inline))
#else
#define DXVK_FORCE_INLINE inline
#endif
#if defined(__GNUC__) || defined(__clang__)
#define DXVK_LIKELY(x)   __builtin_expect(!!(x),1)
#define DXVK_UNLIKELY(x) __builtin_expect(!!(x),0)
#else
#define DXVK_LIKELY(x)   (x)
#define DXVK_UNLIKELY(x) (x)
#endif

namespace dxvk {

  /* ------------------------------------------------------------------ */
  constexpr std::size_t CACHE_LINE_SIZE = 64;

  inline constexpr long double pi_ld = 3.141592653589793238462643383279502884L;
  inline constexpr double      pi    = static_cast<double>(pi_ld);
  inline constexpr float       pi_f  = static_cast<float>(pi_ld);

  namespace spirv {                    /* visible to SPIR-V code-gen unit */
    inline constexpr double pi   = dxvk::pi;
    inline constexpr float  pi_f = dxvk::pi_f;
  }

  /* clamp with branch hints --------------------------------------------*/
  template<class T>
  DXVK_FORCE_INLINE constexpr T clamp(T v, T lo, T hi) {
    return DXVK_LIKELY(v >= lo)
    ? (DXVK_LIKELY(v <= hi) ? v : hi)
    : lo;
  }

  /* align helpers -------------------------------------------------------*/
  template<class T, class U = T>
  DXVK_FORCE_INLINE constexpr T align(T val, U to) {
    static_assert(std::is_integral_v<T> && std::is_integral_v<U>);
    #if defined(__GNUC__) || defined(__clang__)
    if (__builtin_constant_p(to) && (to & (to - 1)) == 0)
      return (val + to - 1) & ~(to - 1);          /* power-of-two CTZ path */
      #endif
      return (val + to - 1) / to * to;
  }

  template<class T, class U = T>
  DXVK_FORCE_INLINE constexpr T alignDown(T val, U to) {
    static_assert(std::is_integral_v<T> && std::is_integral_v<U>);
    #if defined(__GNUC__) || defined(__clang__)
    if (__builtin_constant_p(to) && (to & (to - 1)) == 0)
      return val & ~(to - 1);
    #endif
    return val / to * to;
  }

  /* NaN-safe float clamp -----------------------------------------------*/
  DXVK_FORCE_INLINE float fclamp(float v, float lo, float hi) {
    if (DXVK_UNLIKELY(std::isnan(v))) return lo;
    #if defined(__AVX__)               /*  single cmp+blend on AVX-capable CPUs */
    float t = std::fmax(v, lo);
    return std::fmin(t, hi);
    #else
    return std::fmin(std::fmax(v, lo), hi);
    #endif
  }

  /* ceil-div ------------------------------------------------------------*/
  template<typename T>
  DXVK_FORCE_INLINE constexpr T divCeil(T n, T d) {
    static_assert(std::is_integral_v<T>);
    return (n + d - 1) / d;
  }

} /* namespace dxvk */
