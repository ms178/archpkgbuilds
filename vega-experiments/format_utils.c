/*
 * Mesa 3-D graphics library
 *
 * Copyright (C) 2014  Intel Corporation  All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
#include <immintrin.h>
#endif

#include "errors.h"
#include "format_utils.h"
#include "glformats.h"
#include "format_pack.h"
#include "format_unpack.h"
#include "util/macros.h"

/* ======================================================================
 * Configuration Constants
 * ====================================================================== */

/*
 * Stack buffer threshold for temporary row buffers.
 * 256 pixels × 16 bytes/pixel = 4KB per buffer.
 * Three buffers = 12KB max stack usage, well within typical L1D cache and stack limits.
 * Covers common tile sizes (64×64, 128×128, 256×256 widths) without malloc overhead.
 */
#define FORMAT_CONVERT_STACK_THRESHOLD 256

/* ======================================================================
 * Compiler Feature Detection and Portability Macros
 * ====================================================================== */

#if defined(__GNUC__) || defined(__clang__)
   #define MESA_ALWAYS_INLINE __attribute__((always_inline)) inline
   #define MESA_LIKELY(x)     __builtin_expect(!!(x), 1)
   #define MESA_UNLIKELY(x)   __builtin_expect(!!(x), 0)
#else
   #define MESA_ALWAYS_INLINE inline
   #define MESA_LIKELY(x)     (x)
   #define MESA_UNLIKELY(x)   (x)
#endif

/* Detect AVX2 support at compile time */
#if defined(__AVX2__)
   #define HAVE_AVX2 1
#else
   #define HAVE_AVX2 0
#endif

/* ======================================================================
 * Standard Array Format Definitions
 * ====================================================================== */

const mesa_array_format RGBA32_FLOAT =
   MESA_ARRAY_FORMAT(MESA_ARRAY_FORMAT_BASE_FORMAT_RGBA_VARIANTS,
                     4, 1, 1, 1, 4, 0, 1, 2, 3);

const mesa_array_format RGBA8_UBYTE =
   MESA_ARRAY_FORMAT(MESA_ARRAY_FORMAT_BASE_FORMAT_RGBA_VARIANTS,
                     1, 0, 0, 1, 4, 0, 1, 2, 3);

const mesa_array_format BGRA8_UBYTE =
   MESA_ARRAY_FORMAT(MESA_ARRAY_FORMAT_BASE_FORMAT_RGBA_VARIANTS,
                     1, 0, 0, 1, 4, 2, 1, 0, 3);

const mesa_array_format RGBA32_UINT =
   MESA_ARRAY_FORMAT(MESA_ARRAY_FORMAT_BASE_FORMAT_RGBA_VARIANTS,
                     4, 0, 0, 0, 4, 0, 1, 2, 3);

const mesa_array_format RGBA32_INT =
   MESA_ARRAY_FORMAT(MESA_ARRAY_FORMAT_BASE_FORMAT_RGBA_VARIANTS,
                     4, 1, 0, 0, 4, 0, 1, 2, 3);

/* ======================================================================
 * AVX2 Optimized Kernels
 * ====================================================================== */

#if HAVE_AVX2

/*
 * 16-bit Left Rotation Kernel (A1B5G5R5 -> B5G5R5A1)
 *
 * Rotates each 16-bit value left by 1 bit position.
 * Processes 16 pixels (32 bytes) per AVX2 iteration.
 *
 * Performance: ~0.5 cycles per pixel.
 */
static MESA_ALWAYS_INLINE void
convert_16bit_rotate_left_avx2(uint16_t *restrict dst,
                               const uint16_t *restrict src,
                               size_t n)
{
   size_t i = 0;

   /* AVX2 main loop: 16 pixels per iteration */
   if (MESA_LIKELY(n >= 16)) {
      const size_t limit = n - 15;
      for (; i < limit; i += 16) {
         /* Load 16 pixels (32 bytes) - unaligned load is safe on AVX2 */
         __m256i in = _mm256_loadu_si256((const __m256i *)(src + i));

         /* Rotate left by 1: (x << 1) | (x >> 15) */
         __m256i lo = _mm256_slli_epi16(in, 1);
         __m256i hi = _mm256_srli_epi16(in, 15);
         __m256i out = _mm256_or_si256(lo, hi);

         _mm256_storeu_si256((__m256i *)(dst + i), out);
      }
   }

   /* Scalar tail: remaining 0-15 pixels */
   for (; i < n; i++) {
      uint16_t val = src[i];
      dst[i] = (uint16_t)((val << 1) | (val >> 15));
   }
}

/*
 * 16-bit Right Rotation Kernel (B5G5R5A1 -> A1B5G5R5)
 *
 * Rotates each 16-bit value right by 1 bit position.
 * Mirror of left rotation for reverse conversion.
 */
static MESA_ALWAYS_INLINE void
convert_16bit_rotate_right_avx2(uint16_t *restrict dst,
                                const uint16_t *restrict src,
                                size_t n)
{
   size_t i = 0;

   /* AVX2 main loop: 16 pixels per iteration */
   if (MESA_LIKELY(n >= 16)) {
      const size_t limit = n - 15;
      for (; i < limit; i += 16) {
         __m256i in = _mm256_loadu_si256((const __m256i *)(src + i));

         /* Rotate right by 1: (x >> 1) | (x << 15) */
         __m256i lo = _mm256_slli_epi16(in, 15);
         __m256i hi = _mm256_srli_epi16(in, 1);
         __m256i out = _mm256_or_si256(lo, hi);

         _mm256_storeu_si256((__m256i *)(dst + i), out);
      }
   }

   /* Scalar tail */
   for (; i < n; i++) {
      uint16_t val = src[i];
      dst[i] = (uint16_t)((val >> 1) | (val << 15));
   }
}

/*
 * RGBA to BGRA Byte Swizzle (AVX2)
 *
 * Swaps bytes 0 and 2 within each 4-byte pixel using vpshufb.
 * Processes 8 RGBA pixels (32 bytes) per iteration.
 */
static void
convert_ubyte_rgba_to_bgra_avx2(size_t width, size_t height,
                                const uint8_t *restrict src, size_t src_stride,
                                uint8_t *restrict dst, size_t dst_stride)
{
   /*
    * Shuffle mask: swap bytes 0↔2 within each 4-byte group
    * Layout: {2,1,0,3, 6,5,4,7, 10,9,8,11, 14,13,12,15} repeated for 256-bit
    */
   const __m256i shuffle_mask = _mm256_setr_epi8(
       2,  1,  0,  3,   6,  5,  4,  7,  10,  9,  8, 11,  14, 13, 12, 15,
       2,  1,  0,  3,   6,  5,  4,  7,  10,  9,  8, 11,  14, 13, 12, 15
   );

   for (size_t row = 0; row < height; row++) {
      const uint8_t *s = src + row * src_stride;
      uint8_t *d = dst + row * dst_stride;
      size_t i = 0;

      /* AVX2 main loop: 8 pixels per iteration */
      if (MESA_LIKELY(width >= 8)) {
         const size_t limit = width - 7;
         for (; i < limit; i += 8) {
            __m256i v = _mm256_loadu_si256((const __m256i *)(s + i * 4));
            v = _mm256_shuffle_epi8(v, shuffle_mask);
            _mm256_storeu_si256((__m256i *)(d + i * 4), v);
         }
      }

      /* Scalar tail: remaining 0-7 pixels */
      for (; i < width; i++) {
         uint32_t pixel;
         /* Use memcpy to safely handle potentially unaligned tail pointers */
         memcpy(&pixel, s + i * 4, sizeof(pixel));

         /*
          * RGBA (little-endian memory order) -> BGRA
          * Bit layout: 0xAARRGGBB -> 0xAABBGGRR
          */
         pixel = (pixel & 0xFF00FF00u) |
                 ((pixel & 0x00FF0000u) >> 16) |
                 ((pixel & 0x000000FFu) << 16);

         memcpy(d + i * 4, &pixel, sizeof(pixel));
      }
   }
}

#endif /* HAVE_AVX2 */

/* ======================================================================
 * Swizzle Logic
 * ====================================================================== */

/*
 * Compute the inverse of a swizzle mapping.
 *
 * Given src[j] = i means "output channel j gets input channel i".
 * We compute dst[i] = j meaning "input channel i goes to output channel j".
 *
 * For channels not used in src (SWIZZLE_ZERO, SWIZZLE_ONE), dst is set to
 * SWIZZLE_NONE.
 */
static void
invert_swizzle(uint8_t dst[4], const uint8_t src[4])
{
   /* Initialize all outputs to NONE (not mapped) */
   dst[0] = MESA_FORMAT_SWIZZLE_NONE;
   dst[1] = MESA_FORMAT_SWIZZLE_NONE;
   dst[2] = MESA_FORMAT_SWIZZLE_NONE;
   dst[3] = MESA_FORMAT_SWIZZLE_NONE;

   /* For each output channel, find where its input came from */
   for (int i = 0; i < 4; ++i) {
      for (int j = 0; j < 4; ++j) {
         if (src[j] == (uint8_t)i && dst[i] == MESA_FORMAT_SWIZZLE_NONE) {
            dst[i] = (uint8_t)j;
            break;  /* First match wins */
         }
      }
   }
}

/*
 * Apply rebase_swizzle to src2rgba mapping.
 *
 * If rebase_swizzle is non-NULL, composes the mappings:
 *   rebased_src2rgba[i] = src2rgba[rebase_swizzle[i]]
 * Unless rebase_swizzle[i] is a special value (ZERO, ONE, etc.).
 */
static void
compute_rebased_rgba_component_mapping(const uint8_t *src2rgba,
                                       const uint8_t *rebase_swizzle,
                                       uint8_t *rebased_src2rgba)
{
   if (rebase_swizzle != NULL) {
      for (int i = 0; i < 4; i++) {
         uint8_t rb = rebase_swizzle[i];
         if (rb > MESA_FORMAT_SWIZZLE_W) {
            rebased_src2rgba[i] = rb;  /* Pass through special values */
         } else {
            rebased_src2rgba[i] = src2rgba[rb];
         }
      }
   } else {
      memcpy(rebased_src2rgba, src2rgba, 4);
   }
}

/*
 * Compute direct source-to-destination channel mapping.
 *
 * Composes: src -> rgba (via src2rgba) -> dst (via rgba2dst)
 * With optional rebase applied to rgba stage.
 */
static void
compute_src2dst_component_mapping(const uint8_t *src2rgba,
                                  const uint8_t *rgba2dst,
                                  const uint8_t *rebase_swizzle,
                                  uint8_t *src2dst)
{
   if (rebase_swizzle == NULL) {
      for (int i = 0; i < 4; i++) {
         uint8_t r2d = rgba2dst[i];
         if (r2d > MESA_FORMAT_SWIZZLE_W) {
            src2dst[i] = r2d;
         } else {
            src2dst[i] = src2rgba[r2d];
         }
      }
   } else {
      for (int i = 0; i < 4; i++) {
         uint8_t r2d = rgba2dst[i];
         if (r2d > MESA_FORMAT_SWIZZLE_W) {
            src2dst[i] = r2d;
         } else {
            uint8_t rb = rebase_swizzle[r2d];
            if (rb > MESA_FORMAT_SWIZZLE_W) {
               src2dst[i] = rb;
            } else {
               src2dst[i] = src2rgba[rb];
            }
         }
      }
   }
}

/*
 * Compute RGBA to base format to RGBA mapping.
 *
 * Returns true if rebasing is actually needed (non-identity mapping).
 */
bool
_mesa_compute_rgba2base2rgba_component_mapping(GLenum baseFormat, uint8_t *map)
{
   uint8_t rgba2base[6], base2rgba[6];
   bool needRebase = false;

   switch (baseFormat) {
   case GL_ALPHA:
   case GL_RED:
   case GL_GREEN:
   case GL_BLUE:
   case GL_RG:
   case GL_RGB:
   case GL_BGR:
   case GL_RGBA:
   case GL_BGRA:
   case GL_ABGR_EXT:
   case GL_LUMINANCE:
   case GL_INTENSITY:
   case GL_LUMINANCE_ALPHA:
      _mesa_compute_component_mapping(GL_RGBA, baseFormat, rgba2base);
      _mesa_compute_component_mapping(baseFormat, GL_RGBA, base2rgba);

      for (int i = 0; i < 4; i++) {
         if (base2rgba[i] > MESA_FORMAT_SWIZZLE_W) {
            map[i] = base2rgba[i];
         } else {
            map[i] = rgba2base[base2rgba[i]];
         }
         if (map[i] != (uint8_t)i) {
            needRebase = true;
         }
      }
      return needRebase;

   default:
      UNREACHABLE("Unexpected base format");
   }
}

/* ======================================================================
 * RGBA to BGRA Conversion (with scalar fallback)
 * ====================================================================== */

static void
convert_ubyte_rgba_to_bgra(size_t width, size_t height,
                           const uint8_t *src, size_t src_stride,
                           uint8_t *dst, size_t dst_stride)
{
#if HAVE_AVX2
   convert_ubyte_rgba_to_bgra_avx2(width, height, src, src_stride, dst, dst_stride);
#else
   /*
    * Scalar fallback with SWAR optimization for 64-bit systems.
    * Processes 2 pixels per 64-bit operation when alignment permits.
    */
   const bool use_swar = (sizeof(void *) == 8) &&
                         (src_stride % 8 == 0) &&
                         (dst_stride % 8 == 0) &&
                         (((uintptr_t)src & 7) == 0) &&
                         (((uintptr_t)dst & 7) == 0);

   if (use_swar) {
      for (size_t row = 0; row < height; row++) {
         const uint64_t *s = (const uint64_t *)src;
         uint64_t *d = (uint64_t *)dst;
         size_t i;

         /* Process 2 pixels (8 bytes) at a time */
         for (i = 0; i < width / 2; i++) {
            uint64_t v = s[i];
            d[i] = (v & 0xFF00FF00FF00FF00ULL) |
                   ((v & 0x00000000000000FFULL) << 16) |
                   ((v & 0x0000000000FF0000ULL) >> 16) |
                   ((v & 0x000000FF00000000ULL) << 16) |
                   ((v & 0x00FF000000000000ULL) >> 16);
         }

         /* Handle odd pixel */
         if (width & 1) {
            const uint32_t *s32 = (const uint32_t *)src;
            uint32_t *d32 = (uint32_t *)dst;
            uint32_t v = s32[width - 1];
            d32[width - 1] = (v & 0xFF00FF00u) |
                             ((v & 0x000000FFu) << 16) |
                             ((v & 0x00FF0000u) >> 16);
         }

         src += src_stride;
         dst += dst_stride;
      }
   } else {
      /* Unaligned path: process one pixel at a time */
      for (size_t row = 0; row < height; row++) {
         const uint32_t *s = (const uint32_t *)src;
         uint32_t *d = (uint32_t *)dst;

         for (size_t i = 0; i < width; i++) {
            uint32_t v = s[i];
            d[i] = (v & 0xFF00FF00u) |
                   ((v & 0x000000FFu) << 16) |
                   ((v & 0x00FF0000u) >> 16);
         }

         src += src_stride;
         dst += dst_stride;
      }
   }
#endif /* HAVE_AVX2 */
}

/* ======================================================================
 * Generic Swizzle Fast Paths
 * ====================================================================== */

/*
 * Try fast memcpy path for identity swizzle.
 *
 * Identity swizzle is {0,1,2,3} for the active channels.
 * SWIZZLE_NONE is accepted as "don't care".
 */
static bool
swizzle_convert_try_memcpy(void *restrict dst,
                           enum mesa_array_format_datatype dst_type,
                           int num_dst_channels,
                           const void *restrict src,
                           enum mesa_array_format_datatype src_type,
                           int num_src_channels,
                           const uint8_t swizzle[4],
                           bool normalized,
                           int count)
{
   (void)normalized;  /* Unused in memcpy path */

   if (src_type != dst_type)
      return false;
   if (num_src_channels != num_dst_channels)
      return false;
   if (count <= 0)
      return true;  /* Trivially successful */

   /* Check for identity swizzle on active channels */
   for (int i = 0; i < num_dst_channels; ++i) {
      uint8_t s = swizzle[i];
      /* Accept identity (s == i) or don't-care (SWIZZLE_NONE) */
      if (s != (uint8_t)i && s != MESA_FORMAT_SWIZZLE_NONE)
         return false;
   }

   size_t elem_size = _mesa_array_format_datatype_get_size(src_type);
   size_t total_bytes = (size_t)count * (size_t)num_src_channels * elem_size;
   memcpy(dst, src, total_bytes);
   return true;
}

/*
 * AVX2-accelerated swizzle conversion for common types.
 *
 * Supports:
 *   - UBYTE: 4→4 channel swizzle with ZERO/ONE constants
 *   - USHORT: 4→4 channel swizzle with ZERO/ONE constants
 *   - FLOAT: 4→4 channel swizzle with ZERO/ONE constants
 *
 * Returns true if handled, false to fall back to scalar.
 */
static bool
swizzle_convert_try_avx2(void *restrict dst,
                         enum mesa_array_format_datatype dst_type,
                         int num_dst_channels,
                         const void *restrict src,
                         enum mesa_array_format_datatype src_type,
                         int num_src_channels,
                         const uint8_t swizzle[4],
                         bool normalized,
                         int count)
{
#if HAVE_AVX2
   /* Only handle same-type 4→4 swizzles */
   if (src_type != dst_type)
      return false;
   if (num_src_channels != 4 || num_dst_channels != 4)
      return false;
   if (count <= 0)
      return true;

   /*
    * UBYTE path: Uses vpshufb for byte-level swizzle.
    * Processes 8 pixels (32 bytes) per iteration.
    */
   if (src_type == MESA_ARRAY_FORMAT_TYPE_UBYTE) {
      uint8_t shuf[32];
      uint8_t one_mask_arr[32];
      const uint8_t one_val = normalized ? 255 : 1;

      /* Build shuffle mask for low 16 bytes (4 pixels) */
      for (int px = 0; px < 4; px++) {
         for (int ch = 0; ch < 4; ch++) {
            int byte_idx = px * 4 + ch;
            uint8_t s = swizzle[ch];

            if (s <= 3) {
               shuf[byte_idx] = (uint8_t)(px * 4 + s);
               one_mask_arr[byte_idx] = 0;
            } else if (s == MESA_FORMAT_SWIZZLE_ZERO) {
               shuf[byte_idx] = 0x80;  /* vpshufb zeros on high bit */
               one_mask_arr[byte_idx] = 0;
            } else if (s == MESA_FORMAT_SWIZZLE_ONE) {
               shuf[byte_idx] = 0x80;
               one_mask_arr[byte_idx] = one_val;
            } else {
               return false;  /* Unknown swizzle type */
            }
         }
      }
      /* Replicate to high lane (vpshufb is per-128-bit lane) */
      memcpy(shuf + 16, shuf, 16);
      memcpy(one_mask_arr + 16, one_mask_arr, 16);

      __m256i vshuf = _mm256_loadu_si256((const __m256i *)shuf);
      __m256i vone = _mm256_loadu_si256((const __m256i *)one_mask_arr);

      const uint8_t *s_ptr = (const uint8_t *)src;
      uint8_t *d_ptr = (uint8_t *)dst;
      int i = 0;

      /* Main loop: 8 pixels per iteration */
      if (MESA_LIKELY(count >= 8)) {
         const int limit = count - 7;
         for (; i < limit; i += 8) {
            __m256i v = _mm256_loadu_si256((const __m256i *)(s_ptr + i * 4));
            v = _mm256_shuffle_epi8(v, vshuf);
            v = _mm256_or_si256(v, vone);
            _mm256_storeu_si256((__m256i *)(d_ptr + i * 4), v);
         }
      }

      /* Scalar tail */
      for (; i < count; i++) {
         for (int c = 0; c < 4; c++) {
            uint8_t val;
            uint8_t s = swizzle[c];
            if (s <= 3)
               val = s_ptr[i * 4 + s];
            else if (s == MESA_FORMAT_SWIZZLE_ZERO)
               val = 0;
            else
               val = one_val;
            d_ptr[i * 4 + c] = val;
         }
      }
      return true;
   }

   /*
    * USHORT path: Uses vpshufb on 16-bit data (2 bytes per channel).
    * Processes 4 pixels (32 bytes) per iteration.
    *
    * Layout per 256-bit register:
    *   Lane 0 (bytes 0-15): Pixels 0-1
    *   Lane 1 (bytes 16-31): Pixels 2-3
    *
    * vpshufb uses lane-relative indices (0-15) for both lanes.
    */
   if (src_type == MESA_ARRAY_FORMAT_TYPE_USHORT) {
      uint8_t shuf[32];
      uint16_t one_mask_arr[16];
      const uint16_t one_val = normalized ? UINT16_MAX : 1;

      for (int px = 0; px < 4; px++) {
         int lane = px / 2;               /* 0 or 1 */
         int px_in_lane = px % 2;         /* 0 or 1 */
         int lane_base = lane * 16;       /* Byte offset of lane start */

         for (int ch = 0; ch < 4; ch++) {
            int out_byte_abs = lane_base + px_in_lane * 8 + ch * 2;
            uint8_t s = swizzle[ch];

            if (s <= 3) {
               /* Source byte relative to lane (0-15) */
               int src_byte_rel = px_in_lane * 8 + s * 2;
               shuf[out_byte_abs] = (uint8_t)src_byte_rel;
               shuf[out_byte_abs + 1] = (uint8_t)(src_byte_rel + 1);
               one_mask_arr[px * 4 + ch] = 0;
            } else if (s == MESA_FORMAT_SWIZZLE_ZERO) {
               shuf[out_byte_abs] = 0x80;
               shuf[out_byte_abs + 1] = 0x80;
               one_mask_arr[px * 4 + ch] = 0;
            } else if (s == MESA_FORMAT_SWIZZLE_ONE) {
               shuf[out_byte_abs] = 0x80;
               shuf[out_byte_abs + 1] = 0x80;
               one_mask_arr[px * 4 + ch] = one_val;
            } else {
               return false;
            }
         }
      }

      __m256i vshuf = _mm256_loadu_si256((const __m256i *)shuf);
      __m256i vone = _mm256_loadu_si256((const __m256i *)one_mask_arr);

      const uint16_t *s_ptr = (const uint16_t *)src;
      uint16_t *d_ptr = (uint16_t *)dst;
      int i = 0;

      /* Main loop: 4 pixels per iteration */
      if (MESA_LIKELY(count >= 4)) {
         const int limit = count - 3;
         for (; i < limit; i += 4) {
            __m256i v = _mm256_loadu_si256((const __m256i *)(s_ptr + i * 4));
            v = _mm256_shuffle_epi8(v, vshuf);
            v = _mm256_or_si256(v, vone);
            _mm256_storeu_si256((__m256i *)(d_ptr + i * 4), v);
         }
      }

      /* Scalar tail */
      for (; i < count; i++) {
         for (int c = 0; c < 4; c++) {
            uint16_t val;
            uint8_t s = swizzle[c];
            if (s <= 3)
               val = s_ptr[i * 4 + s];
            else if (s == MESA_FORMAT_SWIZZLE_ZERO)
               val = 0;
            else
               val = one_val;
            d_ptr[i * 4 + c] = val;
         }
      }
      return true;
   }

   /*
    * FLOAT path: Uses vpermilps for intra-lane float swizzle.
    * Processes 2 pixels (32 bytes) per iteration.
    *
    * Layout: Lane 0 = Pixel 0, Lane 1 = Pixel 1
    * vpermilps permutes within each 128-bit lane independently.
    */
   if (src_type == MESA_ARRAY_FORMAT_TYPE_FLOAT) {
      int perm[8];
      float ones[8];
      uint32_t blend_mask_arr[8];
      int needs_blend = 0;

      for (int k = 0; k < 8; k++) {
         int ch = k % 4;
         uint8_t s = swizzle[ch];

         if (s <= 3) {
            perm[k] = (int)s;
            ones[k] = 0.0f;
            blend_mask_arr[k] = 0;
         } else if (s == MESA_FORMAT_SWIZZLE_ZERO) {
            perm[k] = 0;  /* Dummy, will be blended */
            ones[k] = 0.0f;
            blend_mask_arr[k] = 0x80000000u;
            needs_blend = 1;
         } else if (s == MESA_FORMAT_SWIZZLE_ONE) {
            perm[k] = 0;
            ones[k] = 1.0f;
            blend_mask_arr[k] = 0x80000000u;
            needs_blend = 1;
         } else {
            return false;
         }
      }

      __m256i vperm = _mm256_loadu_si256((const __m256i *)perm);
      __m256 vone = _mm256_loadu_ps(ones);
      __m256 vblend = _mm256_castsi256_ps(
         _mm256_loadu_si256((const __m256i *)blend_mask_arr));

      const float *s_ptr = (const float *)src;
      float *d_ptr = (float *)dst;
      int i = 0;

      /* Main loop: 2 pixels per iteration */
      if (MESA_LIKELY(count >= 2)) {
         const int limit = count - 1;
         for (; i < limit; i += 2) {
            __m256 v = _mm256_loadu_ps(s_ptr + i * 4);
            __m256 p = _mm256_permutevar_ps(v, vperm);
            if (needs_blend)
               p = _mm256_blendv_ps(p, vone, vblend);
            _mm256_storeu_ps(d_ptr + i * 4, p);
         }
      }

      /* Scalar tail */
      for (; i < count; i++) {
         for (int c = 0; c < 4; c++) {
            float val;
            uint8_t s = swizzle[c];
            if (s <= 3)
               val = s_ptr[i * 4 + s];
            else if (s == MESA_FORMAT_SWIZZLE_ZERO)
               val = 0.0f;
            else
               val = 1.0f;
            d_ptr[i * 4 + c] = val;
         }
      }
      return true;
   }

#else
   /* Suppress unused parameter warnings when AVX2 not available */
   (void)dst; (void)dst_type; (void)num_dst_channels;
   (void)src; (void)src_type; (void)num_src_channels;
   (void)swizzle; (void)normalized; (void)count;
#endif /* HAVE_AVX2 */

   return false;
}

/* ======================================================================
 * Scalar Swizzle Loop Macros
 * ====================================================================== */

#define SWIZZLE_CONVERT_LOOP(DST_TYPE, DST_CHANS, SRC_TYPE, SRC_CHANS, CONV) \
   do {                                                                      \
      int s, j;                                                              \
      for (s = 0; s + 3 < count; s += 4) {                                   \
         for (int u = 0; u < 4; u++) {                                       \
            for (j = 0; j < SRC_CHANS; ++j) {                                \
               SRC_TYPE src = typed_src[j];                                  \
               tmp[j] = CONV;                                                \
            }                                                                \
            typed_dst[0] = tmp[swizzle_x];                                   \
            if (DST_CHANS > 1) typed_dst[1] = tmp[swizzle_y];                \
            if (DST_CHANS > 2) typed_dst[2] = tmp[swizzle_z];                \
            if (DST_CHANS > 3) typed_dst[3] = tmp[swizzle_w];                \
            typed_src += SRC_CHANS;                                          \
            typed_dst += DST_CHANS;                                          \
         }                                                                   \
      }                                                                      \
      for (; s < count; ++s) {                                               \
         for (j = 0; j < SRC_CHANS; ++j) {                                   \
            SRC_TYPE src = typed_src[j];                                     \
            tmp[j] = CONV;                                                   \
         }                                                                   \
         typed_dst[0] = tmp[swizzle_x];                                      \
         if (DST_CHANS > 1) typed_dst[1] = tmp[swizzle_y];                   \
         if (DST_CHANS > 2) typed_dst[2] = tmp[swizzle_z];                   \
         if (DST_CHANS > 3) typed_dst[3] = tmp[swizzle_w];                   \
         typed_src += SRC_CHANS;                                             \
         typed_dst += DST_CHANS;                                             \
      }                                                                      \
   } while (0)

#define SWIZZLE_CONVERT(DST_TYPE, SRC_TYPE, CONV)                            \
   do {                                                                      \
      const uint8_t swizzle_x = swizzle[0];                                  \
      const uint8_t swizzle_y = swizzle[1];                                  \
      const uint8_t swizzle_z = swizzle[2];                                  \
      const uint8_t swizzle_w = swizzle[3];                                  \
      const SRC_TYPE *typed_src = void_src;                                  \
      DST_TYPE *typed_dst = void_dst;                                        \
      DST_TYPE tmp[7];                                                       \
      tmp[4] = 0;                                                            \
      tmp[5] = one;                                                          \
      tmp[6] = 0;                                                            \
      switch (num_dst_channels) {                                            \
      case 1:                                                                \
         switch (num_src_channels) {                                         \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 1, CONV); break;\
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 2, CONV); break;\
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 3, CONV); break;\
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 4, CONV); break;\
         } break;                                                            \
      case 2:                                                                \
         switch (num_src_channels) {                                         \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 1, CONV); break;\
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 2, CONV); break;\
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 3, CONV); break;\
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 4, CONV); break;\
         } break;                                                            \
      case 3:                                                                \
         switch (num_src_channels) {                                         \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 1, CONV); break;\
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 2, CONV); break;\
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 3, CONV); break;\
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 4, CONV); break;\
         } break;                                                            \
      case 4:                                                                \
         switch (num_src_channels) {                                         \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 1, CONV); break;\
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 2, CONV); break;\
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 3, CONV); break;\
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 4, CONV); break;\
         } break;                                                            \
      }                                                                      \
   } while (0)

/* ======================================================================
 * Scalar Type Conversion Functions
 * ====================================================================== */

static void
convert_float(void *void_dst, int num_dst_channels,
              const void *void_src, GLenum src_type, int num_src_channels,
              const uint8_t swizzle[4], bool normalized, int count)
{
   const float one = 1.0f;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      SWIZZLE_CONVERT(float, float, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      SWIZZLE_CONVERT(float, uint16_t, _mesa_half_to_float(src));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) {
         SWIZZLE_CONVERT(float, uint8_t, _mesa_unorm_to_float(src, 8));
      } else {
         SWIZZLE_CONVERT(float, uint8_t, src);
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) {
         SWIZZLE_CONVERT(float, int8_t, _mesa_snorm_to_float(src, 8));
      } else {
         SWIZZLE_CONVERT(float, int8_t, src);
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) {
         SWIZZLE_CONVERT(float, uint16_t, _mesa_unorm_to_float(src, 16));
      } else {
         SWIZZLE_CONVERT(float, uint16_t, src);
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) {
         SWIZZLE_CONVERT(float, int16_t, _mesa_snorm_to_float(src, 16));
      } else {
         SWIZZLE_CONVERT(float, int16_t, src);
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) {
         SWIZZLE_CONVERT(float, uint32_t, _mesa_unorm_to_float(src, 32));
      } else {
         SWIZZLE_CONVERT(float, uint32_t, src);
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) {
         SWIZZLE_CONVERT(float, int32_t, _mesa_snorm_to_float(src, 32));
      } else {
         SWIZZLE_CONVERT(float, int32_t, src);
      }
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_half_float(void *void_dst, int num_dst_channels,
                   const void *void_src, GLenum src_type, int num_src_channels,
                   const uint8_t swizzle[4], bool normalized, int count)
{
   const uint16_t one = _mesa_float_to_half(1.0f);

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_half(src));
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      SWIZZLE_CONVERT(uint16_t, uint16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_unorm_to_half(src, 8));
      } else {
         SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_float_to_half(src));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_snorm_to_half(src, 8));
      } else {
         SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_float_to_half(src));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_unorm_to_half(src, 16));
      } else {
         SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_float_to_half(src));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_snorm_to_half(src, 16));
      } else {
         SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_float_to_half(src));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unorm_to_half(src, 32));
      } else {
         SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_float_to_half(src));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) {
         SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_snorm_to_half(src, 32));
      } else {
         SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_float_to_half(src));
      }
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_ubyte(void *void_dst, int num_dst_channels,
              const void *void_src, GLenum src_type, int num_src_channels,
              const uint8_t swizzle[4], bool normalized, int count)
{
   const uint8_t one = normalized ? UINT8_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_unorm(src, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_unorm(src, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      SWIZZLE_CONVERT(uint8_t, uint8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, int8_t, _mesa_snorm_to_unorm(src, 8, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, int8_t, _mesa_unsigned_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_unorm_to_unorm(src, 16, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_unsigned_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, int16_t, _mesa_snorm_to_unorm(src, 16, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, int16_t, _mesa_unsigned_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, uint32_t, _mesa_unorm_to_unorm(src, 32, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, uint32_t, _mesa_unsigned_to_unsigned(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) {
         SWIZZLE_CONVERT(uint8_t, int32_t, _mesa_snorm_to_unorm(src, 32, 8));
      } else {
         SWIZZLE_CONVERT(uint8_t, int32_t, _mesa_signed_to_unsigned(src, 8));
      }
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_byte(void *void_dst, int num_dst_channels,
             const void *void_src, GLenum src_type, int num_src_channels,
             const uint8_t swizzle[4], bool normalized, int count)
{
   const int8_t one = normalized ? INT8_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, float, _mesa_float_to_snorm(src, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, float, _mesa_float_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_half_to_snorm(src, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_half_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, uint8_t, _mesa_unsigned_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      SWIZZLE_CONVERT(int8_t, int8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_unsigned_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, int16_t, _mesa_snorm_to_snorm(src, 16, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, int16_t, _mesa_signed_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, uint32_t, _mesa_unsigned_to_signed(src, 8));
      }
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) {
         SWIZZLE_CONVERT(int8_t, int32_t, _mesa_snorm_to_snorm(src, 32, 8));
      } else {
         SWIZZLE_CONVERT(int8_t, int32_t, _mesa_signed_to_signed(src, 8));
      }
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_ushort(void *void_dst, int num_dst_channels,
               const void *void_src, GLenum src_type, int num_src_channels,
               const uint8_t swizzle[4], bool normalized, int count)
{
   const uint16_t one = normalized ? UINT16_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_unorm(src, 16));
      else
         SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_unsigned(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_unorm(src, 16));
      else
         SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_unsigned(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_unorm_to_unorm(src, 8, 16));
      else
         SWIZZLE_CONVERT(uint16_t, uint8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_snorm_to_unorm(src, 8, 16));
      else
         SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_signed_to_unsigned(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      SWIZZLE_CONVERT(uint16_t, uint16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_snorm_to_unorm(src, 16, 16));
      else
         SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_signed_to_unsigned(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unorm_to_unorm(src, 32, 16));
      else
         SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unsigned_to_unsigned(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized)
         SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_snorm_to_unorm(src, 32, 16));
      else
         SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_signed_to_unsigned(src, 16));
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_short(void *void_dst, int num_dst_channels,
              const void *void_src, GLenum src_type, int num_src_channels,
              const uint8_t swizzle[4], bool normalized, int count)
{
   const int16_t one = normalized ? INT16_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, float, _mesa_float_to_snorm(src, 16));
      else
         SWIZZLE_CONVERT(int16_t, float, _mesa_float_to_signed(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_half_to_snorm(src, 16));
      else
         SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_half_to_signed(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 16));
      else
         SWIZZLE_CONVERT(int16_t, uint8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, int8_t, _mesa_snorm_to_snorm(src, 8, 16));
      else
         SWIZZLE_CONVERT(int16_t, int8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 16));
      else
         SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_unsigned_to_signed(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      SWIZZLE_CONVERT(int16_t, int16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 16));
      else
         SWIZZLE_CONVERT(int16_t, uint32_t, _mesa_unsigned_to_signed(src, 16));
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized)
         SWIZZLE_CONVERT(int16_t, int32_t, _mesa_snorm_to_snorm(src, 32, 16));
      else
         SWIZZLE_CONVERT(int16_t, int32_t, _mesa_signed_to_signed(src, 16));
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_uint(void *void_dst, int num_dst_channels,
             const void *void_src, GLenum src_type, int num_src_channels,
             const uint8_t swizzle[4], bool normalized, int count)
{
   const uint32_t one = normalized ? UINT32_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_unorm(src, 32));
      else
         SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_unsigned(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_unorm(src, 32));
      else
         SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_unsigned(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, uint8_t, _mesa_unorm_to_unorm(src, 8, 32));
      else
         SWIZZLE_CONVERT(uint32_t, uint8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, int8_t, _mesa_snorm_to_unorm(src, 8, 32));
      else
         SWIZZLE_CONVERT(uint32_t, int8_t, _mesa_signed_to_unsigned(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_unorm_to_unorm(src, 16, 32));
      else
         SWIZZLE_CONVERT(uint32_t, uint16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, int16_t, _mesa_snorm_to_unorm(src, 16, 32));
      else
         SWIZZLE_CONVERT(uint32_t, int16_t, _mesa_signed_to_unsigned(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      SWIZZLE_CONVERT(uint32_t, uint32_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized)
         SWIZZLE_CONVERT(uint32_t, int32_t, _mesa_snorm_to_unorm(src, 32, 32));
      else
         SWIZZLE_CONVERT(uint32_t, int32_t, _mesa_signed_to_unsigned(src, 32));
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

static void
convert_int(void *void_dst, int num_dst_channels,
            const void *void_src, GLenum src_type, int num_src_channels,
            const uint8_t swizzle[4], bool normalized, int count)
{
   const int32_t one = normalized ? INT32_MAX : 1;

   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, float, _mesa_float_to_snorm(src, 32));
      else
         SWIZZLE_CONVERT(int32_t, float, _mesa_float_to_signed(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, uint16_t, _mesa_half_to_snorm(src, 32));
      else
         SWIZZLE_CONVERT(int32_t, uint16_t, _mesa_half_to_signed(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 32));
      else
         SWIZZLE_CONVERT(int32_t, uint8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, int8_t, _mesa_snorm_to_snorm(src, 8, 32));
      else
         SWIZZLE_CONVERT(int32_t, int8_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 32));
      else
         SWIZZLE_CONVERT(int32_t, uint16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, int16_t, _mesa_snorm_to_snorm(src, 16, 32));
      else
         SWIZZLE_CONVERT(int32_t, int16_t, src);
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized)
         SWIZZLE_CONVERT(int32_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 32));
      else
         SWIZZLE_CONVERT(int32_t, uint32_t, _mesa_unsigned_to_signed(src, 32));
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      SWIZZLE_CONVERT(int32_t, int32_t, src);
      break;
   default:
      assert(!"Invalid channel type combination");
   }
}

void
_mesa_swizzle_and_convert(void *void_dst, enum mesa_array_format_datatype dst_type,
                          int num_dst_channels,
                          const void *void_src, enum mesa_array_format_datatype src_type,
                          int num_src_channels,
                          const uint8_t swizzle[4], bool normalized, int count)
{
   if (count <= 0)
      return;

   /* Fast path 1: Direct memcpy for identical format and identity swizzle */
   if (swizzle_convert_try_memcpy(void_dst, dst_type, num_dst_channels,
                                  void_src, src_type, num_src_channels,
                                  swizzle, normalized, count))
      return;

   /* Fast path 2: AVX2-optimized swizzle for supported formats */
   if (swizzle_convert_try_avx2(void_dst, dst_type, num_dst_channels,
                                void_src, src_type, num_src_channels,
                                swizzle, normalized, count))
      return;

   /* Scalar fallback path */
   switch (dst_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:
      convert_float(void_dst, num_dst_channels, void_src, src_type,
                    num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      convert_half_float(void_dst, num_dst_channels, void_src, src_type,
                         num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      convert_ubyte(void_dst, num_dst_channels, void_src, src_type,
                    num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      convert_byte(void_dst, num_dst_channels, void_src, src_type,
                   num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      convert_ushort(void_dst, num_dst_channels, void_src, src_type,
                     num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      convert_short(void_dst, num_dst_channels, void_src, src_type,
                    num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      convert_uint(void_dst, num_dst_channels, void_src, src_type,
                   num_src_channels, swizzle, normalized, count);
      break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      convert_int(void_dst, num_dst_channels, void_src, src_type,
                  num_src_channels, swizzle, normalized, count);
      break;
   default:
      assert(!"Invalid destination channel type");
   }
}

void
_mesa_format_convert(void *void_dst, uint32_t dst_format, size_t dst_stride,
                     void *void_src, uint32_t src_format, size_t src_stride,
                     size_t width, size_t height, uint8_t *rebase_swizzle)
{
   uint8_t *dst = (uint8_t *)void_dst;
   uint8_t *src = (uint8_t *)void_src;
   mesa_array_format src_array_format, dst_array_format;
   bool src_format_is_mesa_array_format, dst_format_is_mesa_array_format;
   uint8_t src2dst[4], src2rgba[4], rgba2dst[4], dst2rgba[4];
   uint8_t rebased_src2rgba[4];
   enum mesa_array_format_datatype src_type = 0, dst_type = 0, common_type;
   bool normalized, dst_integer, src_integer, is_signed;
   int src_num_channels = 0, dst_num_channels = 0;
   int bits;
   size_t row;

   if (width == 0 || height == 0)
      return;

   if (_mesa_format_is_mesa_array_format(src_format)) {
      src_format_is_mesa_array_format = true;
      src_array_format = src_format;
   } else {
      assert(_mesa_is_format_color_format(src_format));
      src_format_is_mesa_array_format = false;
      src_array_format = _mesa_format_to_array_format(src_format);
   }

   if (_mesa_format_is_mesa_array_format(dst_format)) {
      dst_format_is_mesa_array_format = true;
      dst_array_format = dst_format;
   } else {
      assert(_mesa_is_format_color_format(dst_format));
      dst_format_is_mesa_array_format = false;
      dst_array_format = _mesa_format_to_array_format(dst_format);
   }

   /* --------------------------------------------------------------
    * Fast Paths (no rebasing required)
    * -------------------------------------------------------------- */
   if (!rebase_swizzle) {
      /* Direct memcpy for identical formats */
      if ((dst_format_is_mesa_array_format &&
           src_format_is_mesa_array_format &&
           src_array_format == dst_array_format) ||
          src_format == dst_format) {

         int format_size = _mesa_get_format_bytes(src_format);
         size_t row_bytes = width * (size_t)format_size;

         /* Contiguous block optimization */
         if (src_stride == row_bytes && dst_stride == row_bytes) {
            memcpy(dst, src, height * row_bytes);
            return;
         }

         for (row = 0; row < height; row++) {
            memcpy(dst, src, row_bytes);
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }

#if HAVE_AVX2
      /* 16-bit rotation: A1B5G5R5 <-> B5G5R5A1 */
      if (src_format == MESA_FORMAT_A1B5G5R5_UNORM &&
          dst_format == MESA_FORMAT_B5G5R5A1_UNORM) {
         for (row = 0; row < height; row++) {
            convert_16bit_rotate_left_avx2((uint16_t *)dst,
                                           (const uint16_t *)src, width);
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }
      if (src_format == MESA_FORMAT_B5G5R5A1_UNORM &&
          dst_format == MESA_FORMAT_A1B5G5R5_UNORM) {
         for (row = 0; row < height; row++) {
            convert_16bit_rotate_right_avx2((uint16_t *)dst,
                                            (const uint16_t *)src, width);
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }
#endif

      if (!src_format_is_mesa_array_format) {
         if (dst_array_format == RGBA32_FLOAT) {
            for (row = 0; row < height; ++row) {
               _mesa_unpack_rgba_row(src_format, width, src, (float (*)[4])dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
         } else if (dst_array_format == RGBA8_UBYTE) {
            assert(!_mesa_is_format_integer_color(src_format));
            for (row = 0; row < height; ++row) {
               _mesa_unpack_ubyte_rgba_row(src_format, width, src, (uint8_t (*)[4])dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
#if UTIL_ARCH_LITTLE_ENDIAN
         } else if (dst_array_format == BGRA8_UBYTE &&
                    src_format == MESA_FORMAT_R8G8B8A8_UNORM) {
            convert_ubyte_rgba_to_bgra(width, height, src, src_stride, dst, dst_stride);
            return;
#endif
         } else if (dst_array_format == RGBA32_UINT &&
                    _mesa_is_format_unsigned(src_format)) {
            assert(_mesa_is_format_integer_color(src_format));
            for (row = 0; row < height; ++row) {
               _mesa_unpack_uint_rgba_row(src_format, width, src, (uint32_t (*)[4])dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
         }
      }

      if (!dst_format_is_mesa_array_format) {
         if (src_array_format == RGBA32_FLOAT) {
            for (row = 0; row < height; ++row) {
               _mesa_pack_float_rgba_row(dst_format, width, (const float (*)[4])src, dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
         } else if (src_array_format == RGBA8_UBYTE) {
            assert(!_mesa_is_format_integer_color(dst_format));
#if UTIL_ARCH_LITTLE_ENDIAN
            if (dst_format == MESA_FORMAT_B8G8R8A8_UNORM) {
               convert_ubyte_rgba_to_bgra(width, height, src, src_stride, dst, dst_stride);
            } else
#endif
            {
               for (row = 0; row < height; ++row) {
                  _mesa_pack_ubyte_rgba_row(dst_format, width, src, dst);
                  src += src_stride;
                  dst += dst_stride;
               }
            }
            return;
         } else if (src_array_format == RGBA32_UINT &&
                    _mesa_is_format_unsigned(dst_format)) {
            assert(_mesa_is_format_integer_color(dst_format));
            for (row = 0; row < height; ++row) {
               _mesa_pack_uint_rgba_row(dst_format, width, (const uint32_t (*)[4])src, dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
         }
      }
   }

   /* --------------------------------------------------------------
    * General Path: Row-Based Processing
    * -------------------------------------------------------------- */
   normalized = false;

   if (src_array_format) {
      src_type = _mesa_array_format_get_datatype(src_array_format);
      src_num_channels = _mesa_array_format_get_num_channels(src_array_format);
      _mesa_array_format_get_swizzle(src_array_format, src2rgba);
      normalized = _mesa_array_format_is_normalized(src_array_format);
   }

   if (dst_array_format) {
      dst_type = _mesa_array_format_get_datatype(dst_array_format);
      dst_num_channels = _mesa_array_format_get_num_channels(dst_array_format);
      _mesa_array_format_get_swizzle(dst_array_format, dst2rgba);
      invert_swizzle(rgba2dst, dst2rgba);
      normalized |= _mesa_array_format_is_normalized(dst_array_format);
   }

   /* Both formats are array formats: direct conversion */
   if (src_array_format && dst_array_format) {
      assert(_mesa_array_format_is_normalized(src_array_format) ==
             _mesa_array_format_is_normalized(dst_array_format));

      compute_src2dst_component_mapping(src2rgba, rgba2dst, rebase_swizzle, src2dst);

      for (row = 0; row < height; ++row) {
         _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                   src, src_type, src_num_channels,
                                   src2dst, normalized, (int)width);
         src += src_stride;
         dst += dst_stride;
      }
      return;
   }

   /* Determine integer vs. floating-point path */
   dst_integer = false;
   src_integer = false;

   if (src_array_format) {
      if (!_mesa_array_format_is_float(src_array_format) &&
          !_mesa_array_format_is_normalized(src_array_format))
         src_integer = true;
   } else {
      switch (_mesa_get_format_datatype(src_format)) {
      case GL_UNSIGNED_INT:
      case GL_INT:
         src_integer = true;
         break;
      }
   }

   is_signed = false;
   bits = 8;

   if (dst_array_format) {
      if (!_mesa_array_format_is_float(dst_array_format) &&
          !_mesa_array_format_is_normalized(dst_array_format))
         dst_integer = true;
      is_signed = _mesa_array_format_is_signed(dst_array_format);
      bits = 8 * _mesa_array_format_get_type_size(dst_array_format);
   } else {
      switch (_mesa_get_format_datatype(dst_format)) {
      case GL_UNSIGNED_NORMALIZED:
         is_signed = false;
         break;
      case GL_SIGNED_NORMALIZED:
         is_signed = true;
         break;
      case GL_FLOAT:
         is_signed = true;
         break;
      case GL_UNSIGNED_INT:
         is_signed = false;
         dst_integer = true;
         break;
      case GL_INT:
         is_signed = true;
         dst_integer = true;
         break;
      }
      bits = _mesa_get_format_max_bits(dst_format);
   }

   assert(src_integer == dst_integer);

   /* --------------------------------------------------------------
    * Tiled Processing with Stack/Heap Buffer Selection
    * -------------------------------------------------------------- */
   if (src_integer && dst_integer) {
      common_type = is_signed ? MESA_ARRAY_FORMAT_TYPE_INT : MESA_ARRAY_FORMAT_TYPE_UINT;

      /* Stack allocation for common sizes */
      uint32_t stack_buffer[FORMAT_CONVERT_STACK_THRESHOLD][4];
      uint32_t (*tmp_uint)[4];
      bool heap_allocated = false;

      if (width <= FORMAT_CONVERT_STACK_THRESHOLD) {
         tmp_uint = stack_buffer;
      } else {
         tmp_uint = malloc(width * sizeof(*tmp_uint));
         if (!tmp_uint)
            return;
         heap_allocated = true;
      }

      if (src_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_array_format) {
            _mesa_swizzle_and_convert(tmp_uint, common_type, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, (int)width);
         } else {
            _mesa_unpack_uint_rgba_row(src_format, width, src, tmp_uint);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_uint, common_type, 4,
                                         tmp_uint, common_type, 4,
                                         rebase_swizzle, false, (int)width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_uint, common_type, 4,
                                      rgba2dst, normalized, (int)width);
         } else {
            _mesa_pack_uint_rgba_row(dst_format, width, (const uint32_t (*)[4])tmp_uint, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }

      if (heap_allocated)
         free(tmp_uint);

   } else if (is_signed || bits > 8) {
      /* Float intermediate path */
      float stack_buffer[FORMAT_CONVERT_STACK_THRESHOLD][4];
      float (*tmp_float)[4];
      bool heap_allocated = false;

      if (width <= FORMAT_CONVERT_STACK_THRESHOLD) {
         tmp_float = stack_buffer;
      } else {
         tmp_float = malloc(width * sizeof(*tmp_float));
         if (!tmp_float)
            return;
         heap_allocated = true;
      }

      if (src_format_is_mesa_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, (int)width);
         } else {
            _mesa_unpack_rgba_row(src_format, width, src, tmp_float);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                         tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                         rebase_swizzle, normalized, (int)width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                      rgba2dst, normalized, (int)width);
         } else {
            _mesa_pack_float_rgba_row(dst_format, width, (const float (*)[4])tmp_float, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }

      if (heap_allocated)
         free(tmp_float);

   } else {
      /* Unsigned 8-bit intermediate path */
      uint8_t stack_buffer[FORMAT_CONVERT_STACK_THRESHOLD][4];
      uint8_t (*tmp_ubyte)[4];
      bool heap_allocated = false;

      if (width <= FORMAT_CONVERT_STACK_THRESHOLD) {
         tmp_ubyte = stack_buffer;
      } else {
         tmp_ubyte = malloc(width * sizeof(*tmp_ubyte));
         if (!tmp_ubyte)
            return;
         heap_allocated = true;
      }

      if (src_format_is_mesa_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, (int)width);
         } else {
            _mesa_unpack_ubyte_rgba_row(src_format, width, src, tmp_ubyte);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                         tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                         rebase_swizzle, normalized, (int)width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                      rgba2dst, normalized, (int)width);
         } else {
            _mesa_pack_ubyte_rgba_row(dst_format, width, (const uint8_t *)tmp_ubyte, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }

      if (heap_allocated)
         free(tmp_ubyte);
   }
}
