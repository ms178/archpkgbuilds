#include "util_string.h"

namespace dxvk::str {

namespace {

// Lookup table replacing bit::lzcnt((~first) << 24).
// Indexed by the raw byte value (0x00-0xFF).
// Only entries 0xC0-0xFF are accessed at runtime; lower entries are
// never reached because the if/else-if chain handles them first.
// Using [] (no explicit size) so the static_assert below catches
// any accidental insertion or deletion of initializers.
constexpr uint8_t s_utf8LengthTable[] = {
  // 0x00-0x7F: ASCII single-byte (128 entries, never accessed via table)
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x00-0x0F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x10-0x1F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x20-0x2F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x30-0x3F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x40-0x4F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x50-0x5F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x60-0x6F
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0x70-0x7F
  // 0x80-0xBF: Continuation bytes, invalid as lead (64 entries, never accessed)
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 0x80-0x8F
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 0x90-0x9F
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 0xA0-0xAF
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 0xB0-0xBF
  // 0xC0-0xDF: 2-byte sequence leads (32 entries)
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // 0xC0-0xCF
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // 0xD0-0xDF
  // 0xE0-0xEF: 3-byte sequence leads (16 entries)
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // 0xE0-0xEF
  // 0xF0-0xFF: 4+ byte leads, matches lzcnt(~first << 24u) (16 entries)
  4,4,4,4,4,4,4,4,5,5,5,5,6,6,7,32  // 0xF0-0xFF
};
static_assert(sizeof(s_utf8LengthTable) == 256,
  "UTF-8 length table must have exactly 256 entries");

} // anonymous namespace


const uint8_t* decodeTypedChar(
  const uint8_t* begin,
  const uint8_t* end,
  uint32_t& ch) {
  uint32_t first = begin[0];

  if (likely(first < 0x80)) {
    ch = first;
    return begin + 1;
  } else if (unlikely(first < 0xC0)) {
    while ((begin < end) && (((*begin) & 0xC0) == 0x80))
      begin += 1;

    ch = uint32_t('?');
    return begin;
  } else {
    uint8_t length = s_utf8LengthTable[first];

    if (unlikely(static_cast<size_t>(end - begin) < length)) {
      ch = uint32_t('?');
      return end;
    }

    if (first < 0xE0) {
      ch = ((uint32_t(begin[0]) & 0x1F) << 6)
         | ((uint32_t(begin[1]) & 0x3F));
    } else if (first < 0xF0) {
      ch = ((uint32_t(begin[0]) & 0x0F) << 12)
         | ((uint32_t(begin[1]) & 0x3F) << 6)
         | ((uint32_t(begin[2]) & 0x3F));
    } else if (first < 0xF8) {
      ch = ((uint32_t(begin[0]) & 0x07) << 18)
         | ((uint32_t(begin[1]) & 0x3F) << 12)
         | ((uint32_t(begin[2]) & 0x3F) << 6)
         | ((uint32_t(begin[3]) & 0x3F));
    } else {
      ch = uint32_t('?');
    }

    return begin + length;
  }
}


const uint16_t* decodeTypedChar(
  const uint16_t* begin,
  const uint16_t* end,
  uint32_t& ch) {
  uint32_t first = begin[0];

  if (likely(first < 0xD800)) {
    ch = first;
    return begin + 1;
  } else if (first < 0xDC00) {
    if (unlikely(begin + 2 > end)) {
      ch = uint32_t('?');
      return end;
    }

    ch = 0x10000
       + ((uint32_t(begin[0]) & 0x3FF) << 10)
       + ((uint32_t(begin[1]) & 0x3FF));
    return begin + 2;
  } else if (unlikely(first < 0xE000)) {
    ch = uint32_t('?');
    return begin + 1;
  } else {
    ch = first;
    return begin + 1;
  }
}


const uint32_t* decodeTypedChar(
  const uint32_t* begin,
  const uint32_t* end,
  uint32_t& ch) {
  ch = begin[0];
  return begin + 1;
}


size_t encodeTypedChar(
  uint8_t* begin,
  uint8_t* end,
  uint32_t ch) {
  if (likely(ch < 0x80)) {
    if (begin) {
      if (unlikely(begin + 1 > end))
        return 0;

      begin[0] = uint8_t(ch);
    }
    return 1;
  } else if (ch < 0x800) {
    if (begin) {
      if (unlikely(begin + 2 > end))
        return 0;

      begin[0] = uint8_t(0xC0 | (ch >> 6));
      begin[1] = uint8_t(0x80 | (ch & 0x3F));
    }
    return 2;
  } else if (ch < 0x10000) {
    if (begin) {
      if (unlikely(begin + 3 > end))
        return 0;

      begin[0] = uint8_t(0xE0 | (ch >> 12));
      begin[1] = uint8_t(0x80 | ((ch >> 6) & 0x3F));
      begin[2] = uint8_t(0x80 | (ch & 0x3F));
    }
    return 3;
  } else if (ch < 0x200000) {
    if (begin) {
      if (unlikely(begin + 4 > end))
        return 0;

      begin[0] = uint8_t(0xF0 | (ch >> 18));
      begin[1] = uint8_t(0x80 | ((ch >> 12) & 0x3F));
      begin[2] = uint8_t(0x80 | ((ch >> 6) & 0x3F));
      begin[3] = uint8_t(0x80 | (ch & 0x3F));
    }
    return 4;
  } else {
    return 0;
  }
}


size_t encodeTypedChar(
  uint16_t* begin,
  uint16_t* end,
  uint32_t ch) {
  if (likely(ch < 0xD800)) {
    if (begin) {
      if (unlikely(begin + 1 > end))
        return 0;

      begin[0] = static_cast<uint16_t>(ch);
    }
    return 1;
  } else if (ch < 0xE000) {
    return 0;
  } else if (ch < 0x10000) {
    if (begin) {
      if (unlikely(begin + 1 > end))
        return 0;

      begin[0] = static_cast<uint16_t>(ch);
    }
    return 1;
  } else if (ch < 0x110000) {
    if (begin) {
      if (unlikely(begin + 2 > end))
        return 0;

      ch -= 0x10000;
      begin[0] = static_cast<uint16_t>(0xD800 + (ch >> 10));
      begin[1] = static_cast<uint16_t>(0xDC00 + (ch & 0x3FF));
    }
    return 2;
  } else {
    return 0;
  }
}


size_t encodeTypedChar(
  uint32_t* begin,
  uint32_t* end,
  uint32_t ch) {
  if (begin) {
    if (unlikely(begin + 1 > end))
      return 0;

    begin[0] = ch;
  }
  return 1;
}


std::string fromws(const WCHAR* ws) {
  size_t srcLen = length(ws);
  size_t dstLen = transcodeString<char>(nullptr, 0, ws, srcLen);

  std::string result;
  result.resize(dstLen);

  transcodeString(result.data(), dstLen, ws, srcLen);
  return result;
}


std::wstring tows(const char* mbs) {
  size_t srcLen = length(mbs);
  size_t dstLen = transcodeString<wchar_t>(nullptr, 0, mbs, srcLen);

  std::wstring result;
  result.resize(dstLen);

  transcodeString(result.data(), dstLen, mbs, srcLen);
  return result;
}

} // namespace dxvk::str
