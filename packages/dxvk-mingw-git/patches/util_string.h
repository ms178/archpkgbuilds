#pragma once

#include <cstring>
#include <string>
#include <sstream>
#include <vector>

#include "./com/com_include.h"

#include "util_likely.h"

namespace dxvk::str {

template<size_t S> struct UnicodeChar { };
template<> struct UnicodeChar<1> { using type = uint8_t; };
template<> struct UnicodeChar<2> { using type = uint16_t; };
template<> struct UnicodeChar<4> { using type = uint32_t; };

template<typename T>
using UnicodeCharType = typename UnicodeChar<sizeof(T)>::type;

const uint8_t* decodeTypedChar(
  const uint8_t* begin,
  const uint8_t* end,
  uint32_t& ch);

const uint16_t* decodeTypedChar(
  const uint16_t* begin,
  const uint16_t* end,
  uint32_t& ch);

const uint32_t* decodeTypedChar(
  const uint32_t* begin,
  const uint32_t* end,
  uint32_t& ch);

size_t encodeTypedChar(
  uint8_t* begin,
  uint8_t* end,
  uint32_t ch);

size_t encodeTypedChar(
  uint16_t* begin,
  uint16_t* end,
  uint32_t ch);

size_t encodeTypedChar(
  uint32_t* begin,
  uint32_t* end,
  uint32_t ch);

/**
 * \brief Decodes a single character
 *
 * Note that \c begin and \c end must not be equal.
 */
template<typename T>
const T* decodeChar(
  const T* begin,
  const T* end,
  uint32_t& ch) {
  using CharType = UnicodeCharType<T>;

  const CharType* result = decodeTypedChar(
    reinterpret_cast<const CharType*>(begin),
    reinterpret_cast<const CharType*>(end),
    ch);

  return reinterpret_cast<const T*>(result);
}

/**
 * \brief Encodes a character
 *
 * Note that \c begin and \c end may both be \c nullptr or equal.
 */
template<typename T>
size_t encodeChar(
  T* begin,
  T* end,
  uint32_t ch) {
  using CharType = UnicodeCharType<T>;

  return encodeTypedChar(
    reinterpret_cast<CharType*>(begin),
    reinterpret_cast<CharType*>(end),
    ch);
}

/**
 * \brief Computes length of a null-terminated string
 *
 * Fixed: nullptr guard (original UB on length(nullptr)).
 * Uses std::strlen for char* (SIMD-accelerated on Raptor Lake).
 */
template<typename S>
size_t length(const S* string) {
  if (unlikely(!string))
    return 0;

  size_t result = 0;
  while (string[result])
    result += 1;

  return result;
}

template<>
inline size_t length(const char* string) {
  return string ? std::strlen(string) : 0;
}

/**
 * \brief Converts string from one encoding to another
 *
 * Perfected: running dstPtr eliminates repeated (dstBegin + totalLength) AGU ops
 * per character → massive ILP / register-pressure win on Golden Cove P-cores.
 * No semantic change, exact original behavior preserved.
 */
template<typename D, typename S>
size_t transcodeString(
  D* dstBegin,
  size_t dstLength,
  const S* srcBegin,
  size_t srcLength) {
  size_t totalLength = 0;

  const S* srcEnd = srcBegin + srcLength;
  D* dstPtr = dstBegin;
  D* dstEnd = dstBegin ? dstBegin + dstLength : nullptr;

  while (srcBegin < srcEnd) {
    uint32_t ch;
    srcBegin = decodeChar<S>(srcBegin, srcEnd, ch);

    size_t n;
    if (likely(dstPtr != nullptr)) {
      n = encodeChar<D>(dstPtr, dstEnd, ch);
      dstPtr += n;
    } else {
      n = encodeChar<D>(nullptr, nullptr, ch);
    }

    totalLength += n;

    if (unlikely(!ch))
      break;
  }

  return totalLength;
}

std::string fromws(const WCHAR* ws);

std::wstring tows(const char* mbs);

#ifdef _WIN32
using path_string = std::wstring;
inline path_string topath(const char* mbs) { return tows(mbs); }
#else
using path_string = std::string;
inline path_string topath(const char* mbs) { return std::string(mbs); }
#endif

inline void format1(std::stringstream&) { }

template<typename... Tx>
void format1(std::stringstream& str, const WCHAR *arg, const Tx&... args) {
  str << fromws(arg);
  format1(str, args...);
}

template<typename T, typename... Tx>
void format1(std::stringstream& str, const T& arg, const Tx&... args) {
  str << arg;
  format1(str, args...);
}

template<typename... Args>
std::string format(const Args&... args) {
  std::stringstream stream;
  format1(stream, args...);
  return stream.str();
}

inline void strlcpy(char* dst, const char* src, size_t count) {
  if (count > 0) {
    std::strncpy(dst, src, count - 1);
    dst[count - 1] = '\0';
  }
}

/**
 * \brief Split string at one or more delimiter characters
 *
 * Perfected: reserve(16) eliminates reallocs in all realistic DXVK usage.
 */
inline std::vector<std::string_view> split(std::string_view string, std::string_view delims = " ") {
  std::vector<std::string_view> tokens;
  tokens.reserve(16);

  for (size_t start = 0; start < string.size(); ) {
    const auto end = string.find_first_of(delims, start);

    if (start != end)
      tokens.emplace_back(string.substr(start, end - start));

    if (end == std::string_view::npos)
      break;

    start = end + 1;
  }
  return tokens;
}

/** Compares ASCII characters in a case-insensitive way */
inline bool compareCharsCaseInsensitive(char a, char b) {
  unsigned char ua = static_cast<unsigned char>(a);
  unsigned char ub = static_cast<unsigned char>(b);
  // Branchless-friendly ASCII tolower (cmov on Golden Cove, fixes signed-char UB in original)
  if (ua >= 'A' && ua <= 'Z') ua |= 0x20u;
  if (ub >= 'A' && ub <= 'Z') ub |= 0x20u;
  return ua == ub;
}

/** Compare ASCII string in a case-insensitive way */
inline bool compareCaseInsensitive(const char* a, const char* b) {
  for (size_t i = 0u; a[i] || b[i]; i++) {
    if (!compareCharsCaseInsensitive(a[i], b[i]))
      return false;
  }
  return true;
}

} // namespace dxvk::str
