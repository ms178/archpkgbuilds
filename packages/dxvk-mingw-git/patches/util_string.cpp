#include "util_string.h"

namespace dxvk::str {

  namespace {

    constexpr uint32_t ReplacementChar = uint32_t('?');

  }


  const uint8_t* decodeTypedChar(
    const uint8_t*  begin,
    const uint8_t*  end,
          uint32_t& ch) {
    const uint32_t first = begin[0];

    if (likely(first < 0x80u)) {
      ch = first;
      return begin + 1;
    }

    if (unlikely(first < 0xC0u)) {
      while ((begin < end) && (((*begin) & 0xC0u) == 0x80u))
        begin += 1;

      ch = ReplacementChar;
      return begin;
    }

    uint32_t length = 0u;
    uint32_t code = 0u;
    uint32_t minCode = 0u;

    if (first < 0xE0u) {
      length = 2u;
      code = first & 0x1Fu;
      minCode = 0x80u;
    } else if (first < 0xF0u) {
      length = 3u;
      code = first & 0x0Fu;
      minCode = 0x800u;
    } else if (first < 0xF8u) {
      length = 4u;
      code = first & 0x07u;
      minCode = 0x10000u;
    } else {
      ch = ReplacementChar;
      return begin + 1;
    }

    const size_t remaining = size_t(end - begin);

    if (unlikely(remaining < length)) {
      ch = ReplacementChar;
      return end;
    }

    for (uint32_t i = 1u; i < length; i++) {
      const uint32_t next = begin[i];

      if (unlikely((next & 0xC0u) != 0x80u)) {
        ch = ReplacementChar;
        return begin + 1;
      }

      code = (code << 6) | (next & 0x3Fu);
    }

    if (unlikely(code < minCode || code > 0x10FFFFu || (code >= 0xD800u && code <= 0xDFFFu))) {
      ch = ReplacementChar;
      return begin + length;
    }

    ch = code;
    return begin + length;
  }


  const uint16_t* decodeTypedChar(
    const uint16_t* begin,
    const uint16_t* end,
          uint32_t& ch) {
    const uint32_t first = begin[0];

    if (likely(first < 0xD800u || first >= 0xE000u)) {
      ch = first;
      return begin + 1;
    }

    if (unlikely(first >= 0xDC00u)) {
      ch = ReplacementChar;
      return begin + 1;
    }

    if (unlikely(begin + 2 > end)) {
      ch = ReplacementChar;
      return end;
    }

    const uint32_t second = begin[1];

    if (unlikely(second < 0xDC00u || second >= 0xE000u)) {
      ch = ReplacementChar;
      return begin + 1;
    }

    ch = 0x10000u
       + ((first  & 0x3FFu) << 10)
       +  (second & 0x3FFu);
    return begin + 2;
  }


  const uint32_t* decodeTypedChar(
    const uint32_t* begin,
    const uint32_t* end,
          uint32_t& ch) {
    (void)end;
    ch = begin[0];
    return begin + 1;
  }


  size_t encodeTypedChar(
          uint8_t*  begin,
          uint8_t*  end,
          uint32_t  ch) {
    if (unlikely(ch > 0x10FFFFu || (ch >= 0xD800u && ch <= 0xDFFFu)))
      return 0;

    if (likely(ch < 0x80u)) {
      if (begin) {
        if (unlikely(begin + 1 > end))
          return 0;
        begin[0] = uint8_t(ch);
      }
      return 1;
    }

    if (ch < 0x800u) {
      if (begin) {
        if (unlikely(begin + 2 > end))
          return 0;
        begin[0] = uint8_t(0xC0u | (ch >> 6));
        begin[1] = uint8_t(0x80u | (ch & 0x3Fu));
      }
      return 2;
    }

    if (ch < 0x10000u) {
      if (begin) {
        if (unlikely(begin + 3 > end))
          return 0;
        begin[0] = uint8_t(0xE0u | (ch >> 12));
        begin[1] = uint8_t(0x80u | ((ch >> 6) & 0x3Fu));
        begin[2] = uint8_t(0x80u | (ch & 0x3Fu));
      }
      return 3;
    }

    if (begin) {
      if (unlikely(begin + 4 > end))
        return 0;
      begin[0] = uint8_t(0xF0u | (ch >> 18));
      begin[1] = uint8_t(0x80u | ((ch >> 12) & 0x3Fu));
      begin[2] = uint8_t(0x80u | ((ch >> 6) & 0x3Fu));
      begin[3] = uint8_t(0x80u | (ch & 0x3Fu));
    }

    return 4;
  }


  size_t encodeTypedChar(
          uint16_t* begin,
          uint16_t* end,
          uint32_t  ch) {
    if (unlikely(ch > 0x10FFFFu || (ch >= 0xD800u && ch < 0xE000u)))
      return 0;

    if (likely(ch < 0x10000u)) {
      if (begin) {
        if (unlikely(begin + 1 > end))
          return 0;
        begin[0] = uint16_t(ch);
      }
      return 1;
    }

    if (begin) {
      if (unlikely(begin + 2 > end))
        return 0;

      ch -= 0x10000u;
      begin[0] = uint16_t(0xD800u + (ch >> 10));
      begin[1] = uint16_t(0xDC00u + (ch & 0x3FFu));
    }

    return 2;
  }


  size_t encodeTypedChar(
          uint32_t* begin,
          uint32_t* end,
          uint32_t  ch) {
    if (unlikely(ch > 0x10FFFFu || (ch >= 0xD800u && ch < 0xE000u)))
      return 0;

    if (begin) {
      if (unlikely(begin + 1 > end))
        return 0;
      begin[0] = ch;
    }

    return 1;
  }


  std::string fromws(const WCHAR* ws) {
    const size_t srcLen = length(ws);

    if (!srcLen)
      return std::string();

    const size_t dstLen = transcodeString<char>(nullptr, 0u, ws, srcLen);

    std::string result;
    result.resize(dstLen);
    transcodeString(result.data(), dstLen, ws, srcLen);
    return result;
  }


  std::wstring tows(const char* mbs) {
    const size_t srcLen = length(mbs);

    if (!srcLen)
      return std::wstring();

    const size_t dstLen = transcodeString<wchar_t>(nullptr, 0u, mbs, srcLen);

    std::wstring result;
    result.resize(dstLen);
    transcodeString(result.data(), dstLen, mbs, srcLen);
    return result;
  }

} // namespace dxvk::str
