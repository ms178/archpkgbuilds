#pragma once

#include <cstring>
#include <sstream>
#include <string>
#include <string_view>
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

template<typename D, typename S>
size_t transcodeString(
  D* dstBegin,
  size_t dstLength,
  const S* srcBegin,
  size_t srcLength) {
  if (unlikely(!srcBegin || !srcLength))
    return 0;

  size_t totalLength = 0;

  const S* srcEnd = srcBegin + srcLength;
  D* dstPtr = dstBegin;
  D* dstEnd = dstBegin ? dstBegin + dstLength : nullptr;

  while (srcBegin < srcEnd) {
    uint32_t ch = 0u;
    srcBegin = decodeChar<S>(srcBegin, srcEnd, ch);

    const size_t n = likely(dstPtr != nullptr)
      ? encodeChar<D>(dstPtr, dstEnd, ch)
      : encodeChar<D>(nullptr, nullptr, ch);

    if (likely(dstPtr != nullptr))
      dstPtr += n;

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
inline path_string topath(const char* mbs) { return mbs ? tows(mbs) : path_string(); }
#else
using path_string = std::string;
inline path_string topath(const char* mbs) { return mbs ? std::string(mbs) : path_string(); }
#endif

inline void format1(std::stringstream&) { }

template<typename... Tx>
void format1(std::stringstream& str, const WCHAR* arg, const Tx&... args) {
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
  if (!count || !dst)
    return;

  if (!src) {
    dst[0] = '\0';
    return;
  }

  size_t i = 0u;
  for (; i + 1u < count && src[i]; i++)
    dst[i] = src[i];

  dst[i] = '\0';
}

inline std::vector<std::string_view> split(std::string_view string, std::string_view delims = " ") {
  std::vector<std::string_view> tokens;
  tokens.reserve(16);

  size_t start = string.find_first_not_of(delims, 0u);

  while (start != std::string_view::npos) {
    const size_t end = string.find_first_of(delims, start);

    if (end == std::string_view::npos) {
      tokens.emplace_back(string.substr(start));
      break;
    }

    tokens.emplace_back(string.substr(start, end - start));
    start = string.find_first_not_of(delims, end + 1u);
  }

  return tokens;
}

inline bool compareCharsCaseInsensitive(char a, char b) {
  uint32_t ua = static_cast<uint8_t>(a);
  uint32_t ub = static_cast<uint8_t>(b);

  ua |= (uint32_t(ua - uint32_t('A')) <= uint32_t('Z' - 'A')) ? 0x20u : 0u;
  ub |= (uint32_t(ub - uint32_t('A')) <= uint32_t('Z' - 'A')) ? 0x20u : 0u;
  return ua == ub;
}

inline bool compareCaseInsensitive(const char* a, const char* b) {
  if (a == b)
    return true;

  if (!a || !b)
    return false;

  for (size_t i = 0u; a[i] || b[i]; i++) {
    if (!compareCharsCaseInsensitive(a[i], b[i]))
      return false;
  }

  return true;
}

inline std::string tolower(std::string str) {
  for (char& c : str) {
    if (c >= 'A' && c <= 'Z')
      c = char(c + ('a' - 'A'));
  }

  return str;
}

} // namespace dxvk::str
