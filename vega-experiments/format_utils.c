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
#include <immintrin.h>

#include "errors.h"
#include "format_utils.h"
#include "glformats.h"
#include "format_pack.h"
#include "format_unpack.h"

/* Constants for array formats */
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

/* ----------------------------------------------------------------------
 * AVX2 Helpers
 * ---------------------------------------------------------------------- */

#if defined(__AVX2__)

/*
 * Optimized 16-bit rotation for A1B5G5R5 <-> B5G5R5A1 conversions.
 * Direction: 0 = Left Rotate 1 (A1B5 -> B5A1), 1 = Right Rotate 1 (B5A1 -> A1B5).
 */
static inline void
convert_16bit_rotate_avx2(uint16_t *dst, const uint16_t *src, size_t n, int direction)
{
   size_t i = 0;
   /* A1B5G5R5 (A=15) -> B5G5R5A1 (A=0). Needs Rotate Left 1. */
   /* B5G5R5A1 (A=0) -> A1B5G5R5 (A=15). Needs Rotate Right 1. */

   if (n >= 16) {
      const __m256i vone = _mm256_set1_epi16(1);
      const __m256i vfifteen = _mm256_set1_epi16(15);

      for (; i <= n - 16; i += 16) {
         __m256i in = _mm256_loadu_si256((const __m256i *)(src + i));
         __m256i r, l;

         if (direction == 0) { /* Rotate Left 1 */
            l = _mm256_slli_epi16(in, 1);
            r = _mm256_srli_epi16(in, 15);
         } else { /* Rotate Right 1 */
            l = _mm256_slli_epi16(in, 15);
            r = _mm256_srli_epi16(in, 1);
         }
         _mm256_storeu_si256((__m256i *)(dst + i), _mm256_or_si256(l, r));
      }
   }

   /* Scalar Tail */
   for (; i < n; i++) {
      uint16_t val = src[i];
      if (direction == 0)
         dst[i] = (val << 1) | (val >> 15);
      else
         dst[i] = (val >> 1) | (val << 15);
   }
}

static inline void
convert_ubyte_rgba_to_bgra_avx2(size_t width, size_t height,
                                const uint8_t *src, size_t src_stride,
                                uint8_t *dst, size_t dst_stride)
{
   /* Mask: R(0)<->B(2). Pattern: 2, 1, 0, 3 ... repeated */
   const __m256i mask = _mm256_setr_epi8(
      2, 1, 0, 3, 6, 5, 4, 7, 10, 9, 8, 11, 14, 13, 12, 15,
      2, 1, 0, 3, 6, 5, 4, 7, 10, 9, 8, 11, 14, 13, 12, 15
      /* Note: vpshufb indices are per-lane (0-15). High lane uses same 0-15 indices relative to lane start. */
   );

   size_t row, i;
   for (row = 0; row < height; row++) {
      const uint8_t *s = src;
      uint8_t *d = dst;

      for (i = 0; i <= width - 8; i += 8) {
         __m256i v = _mm256_loadu_si256((const __m256i *)(s + i * 4));
         v = _mm256_shuffle_epi8(v, mask);
         _mm256_storeu_si256((__m256i *)(d + i * 4), v);
      }

      /* Tail */
      for (; i < width; i++) {
         uint32_t pixel;
         memcpy(&pixel, s + i * 4, 4);
         pixel = (pixel & 0xff00ff00) |
                 ((pixel & 0x00ff0000) >> 16) |
                 ((pixel & 0x000000ff) << 16);
         memcpy(d + i * 4, &pixel, 4);
      }

      src += src_stride;
      dst += dst_stride;
   }
}

#endif /* __AVX2__ */

/* ----------------------------------------------------------------------
 * Logic
 * ---------------------------------------------------------------------- */

static void
invert_swizzle(uint8_t dst[4], const uint8_t src[4])
{
   int i, j;
   dst[0] = MESA_FORMAT_SWIZZLE_NONE;
   dst[1] = MESA_FORMAT_SWIZZLE_NONE;
   dst[2] = MESA_FORMAT_SWIZZLE_NONE;
   dst[3] = MESA_FORMAT_SWIZZLE_NONE;

   for (i = 0; i < 4; ++i)
      for (j = 0; j < 4; ++j)
         if (src[j] == i && dst[i] == MESA_FORMAT_SWIZZLE_NONE)
            dst[i] = j;
}

static void
compute_rebased_rgba_component_mapping(uint8_t *src2rgba,
                                       uint8_t *rebase_swizzle,
                                       uint8_t *rebased_src2rgba)
{
   int i;
   if (rebase_swizzle) {
      for (i = 0; i < 4; i++) {
         if (rebase_swizzle[i] > MESA_FORMAT_SWIZZLE_W)
            rebased_src2rgba[i] = rebase_swizzle[i];
         else
            rebased_src2rgba[i] = src2rgba[rebase_swizzle[i]];
      }
   } else {
      memcpy(rebased_src2rgba, src2rgba, 4 * sizeof(uint8_t));
   }
}

static void
compute_src2dst_component_mapping(uint8_t *src2rgba, uint8_t *rgba2dst,
                                  uint8_t *rebase_swizzle, uint8_t *src2dst)
{
   int i;
   if (!rebase_swizzle) {
      for (i = 0; i < 4; i++) {
         if (rgba2dst[i] > MESA_FORMAT_SWIZZLE_W)
            src2dst[i] = rgba2dst[i];
         else
            src2dst[i] = src2rgba[rgba2dst[i]];
      }
   } else {
      for (i = 0; i < 4; i++) {
         if (rgba2dst[i] > MESA_FORMAT_SWIZZLE_W)
            src2dst[i] = rgba2dst[i];
         else if (rebase_swizzle[rgba2dst[i]] > MESA_FORMAT_SWIZZLE_W)
            src2dst[i] = rebase_swizzle[rgba2dst[i]];
         else
            src2dst[i] = src2rgba[rebase_swizzle[rgba2dst[i]]];
      }
   }
}

bool
_mesa_compute_rgba2base2rgba_component_mapping(GLenum baseFormat, uint8_t *map)
{
   uint8_t rgba2base[6], base2rgba[6];
   int i;

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
      {
         bool needRebase = false;
         _mesa_compute_component_mapping(GL_RGBA, baseFormat, rgba2base);
         _mesa_compute_component_mapping(baseFormat, GL_RGBA, base2rgba);
         for (i = 0; i < 4; i++) {
            if (base2rgba[i] > MESA_FORMAT_SWIZZLE_W)
               map[i] = base2rgba[i];
            else
               map[i] = rgba2base[base2rgba[i]];
            if (map[i] != i)
               needRebase = true;
         }
         return needRebase;
      }
   default:
      UNREACHABLE("Unexpected base format");
   }
}

static void
convert_ubyte_rgba_to_bgra(size_t width, size_t height,
                           const uint8_t *src, size_t src_stride,
                           uint8_t *dst, size_t dst_stride)
{
#if defined(__AVX2__)
   convert_ubyte_rgba_to_bgra_avx2(width, height, src, src_stride, dst, dst_stride);
#else
   int row;
   if (sizeof(void *) == 8 &&
       src_stride % 8 == 0 && dst_stride % 8 == 0 &&
       (GLsizeiptr) src % 8 == 0 && (GLsizeiptr) dst % 8 == 0) {
      for (row = 0; row < height; row++) {
         const GLuint64 *s = (const GLuint64 *) src;
         GLuint64 *d = (GLuint64 *) dst;
         int i;
         for (i = 0; i < width/2; i++) {
            d[i] = ( (s[i] & 0xff00ff00ff00ff00) |
                    ((s[i] &       0xff000000ff) << 16) |
                    ((s[i] &   0xff000000ff0000) >> 16));
         }
         if (width & 1) {
            const GLuint s32 = ((const GLuint *) src)[width - 1];
            GLuint *d32 = (GLuint *) dst + width - 1;
            *d32 = ( (s32 & 0xff00ff00) |
                    ((s32 &       0xff) << 16) |
                    ((s32 &   0xff0000) >> 16));
         }
         src += src_stride;
         dst += dst_stride;
      }
   } else {
      for (row = 0; row < height; row++) {
         const GLuint *s = (const GLuint *) src;
         GLuint *d = (GLuint *) dst;
         int i;
         for (i = 0; i < width; i++) {
            d[i] = ( (s[i] & 0xff00ff00) |
                    ((s[i] &       0xff) << 16) |
                    ((s[i] &   0xff0000) >> 16));
         }
         src += src_stride;
         dst += dst_stride;
      }
   }
#endif
}

/* ----------------------------------------------------------------------
 * Generic Swizzle Optimizations
 * ---------------------------------------------------------------------- */

static bool
swizzle_convert_try_memcpy(void *dst,
                           enum mesa_array_format_datatype dst_type,
                           int num_dst_channels,
                           const void *src,
                           enum mesa_array_format_datatype src_type,
                           int num_src_channels,
                           const uint8_t swizzle[4], bool normalized, int count)
{
   int i;
   if (src_type != dst_type) return false;
   if (num_src_channels != num_dst_channels) return false;
   for (i = 0; i < num_dst_channels; ++i)
      if (swizzle[i] != i && swizzle[i] != MESA_FORMAT_SWIZZLE_NONE)
         return false;

   memcpy(dst, src, count * num_src_channels *
          _mesa_array_format_datatype_get_size(src_type));
   return true;
}

static bool
swizzle_convert_try_avx2(void *dst, enum mesa_array_format_datatype dst_type,
                         int num_dst_channels,
                         const void *src, enum mesa_array_format_datatype src_type,
                         int num_src_channels,
                         const uint8_t swizzle[4], bool normalized, int count)
{
#if defined(__AVX2__)
   if (src_type != dst_type) return false;
   if (num_src_channels != 4 || num_dst_channels != 4) return false;

   if (src_type == MESA_ARRAY_FORMAT_TYPE_UBYTE) {
      uint8_t shuf[32];
      uint8_t one_mask_arr[32];
      uint8_t one_val = normalized ? 255 : 1;

      for (int k = 0; k < 16; k++) {
         int chan = k % 4;
         uint8_t s = swizzle[chan];
         if (s <= 3) {
            shuf[k] = (uint8_t)((k / 4) * 4 + s);
            one_mask_arr[k] = 0;
         } else if (s == MESA_FORMAT_SWIZZLE_ZERO) {
            shuf[k] = 0x80;
            one_mask_arr[k] = 0;
         } else if (s == MESA_FORMAT_SWIZZLE_ONE) {
            shuf[k] = 0x80;
            one_mask_arr[k] = one_val;
         } else {
            return false;
         }
      }
      /* Replicate mask to high lane */
      memcpy(shuf + 16, shuf, 16);
      memcpy(one_mask_arr + 16, one_mask_arr, 16);

      __m256i vshuf = _mm256_loadu_si256((const __m256i *)shuf);
      __m256i vone  = _mm256_loadu_si256((const __m256i *)one_mask_arr);
      const uint8_t *s_ptr = (const uint8_t *)src;
      uint8_t *d_ptr = (uint8_t *)dst;
      int i;

      for (i = 0; i <= count - 8; i += 8) {
         __m256i v = _mm256_loadu_si256((const __m256i *)(s_ptr + i * 4));
         v = _mm256_shuffle_epi8(v, vshuf);
         v = _mm256_or_si256(v, vone);
         _mm256_storeu_si256((__m256i *)(d_ptr + i * 4), v);
      }

      /* Tail handled by caller if returns false, but we did partial work?
         Ideally we finish here. */
      for (; i < count; i++) {
         for (int c = 0; c < 4; c++) {
            uint8_t val;
            if (swizzle[c] <= 3) val = s_ptr[i*4 + swizzle[c]];
            else if (swizzle[c] == MESA_FORMAT_SWIZZLE_ZERO) val = 0;
            else val = one_val;
            d_ptr[i*4 + c] = val;
         }
      }
      return true;
   }

   if (src_type == MESA_ARRAY_FORMAT_TYPE_FLOAT) {
      int perm[8];
      float ones[8];
      int blend_mask[8];

      for(int k=0; k<8; k++) {
         int c = k % 4;
         if (swizzle[c] <= 3) {
            perm[k] = swizzle[c];
            ones[k] = 0.0f;
            blend_mask[k] = 0;
         } else {
            perm[k] = 0;
            ones[k] = (swizzle[c] == MESA_FORMAT_SWIZZLE_ONE) ? 1.0f : 0.0f;
            blend_mask[k] = 0x80000000;
         }
      }

      __m256i vperm = _mm256_loadu_si256((const __m256i *)perm);
      __m256 vone = _mm256_loadu_ps(ones);
      __m256 vblend = _mm256_castsi256_ps(_mm256_loadu_si256((const __m256i *)blend_mask));
      const float *s_ptr = (const float *)src;
      float *d_ptr = (float *)dst;
      int i;

      for (i = 0; i <= count - 2; i += 2) {
         __m256 v = _mm256_loadu_ps(s_ptr + i * 4);
         __m256 p = _mm256_permutevar_ps(v, vperm);
         /* If any component is constant, blend it in */
         if (_mm256_movemask_ps(vblend))
            p = _mm256_blendv_ps(p, vone, vblend);
         _mm256_storeu_ps(d_ptr + i * 4, p);
      }

      for (; i < count; i++) {
         for (int c = 0; c < 4; c++) {
            float val;
            if (swizzle[c] <= 3) val = s_ptr[i*4 + swizzle[c]];
            else if (swizzle[c] == MESA_FORMAT_SWIZZLE_ZERO) val = 0.0f;
            else val = 1.0f;
            d_ptr[i*4 + c] = val;
         }
      }
      return true;
   }
#endif
   return false;
}

/* ----------------------------------------------------------------------
 * Scalar Loops with Manual Unrolling
 * ---------------------------------------------------------------------- */

#define SWIZZLE_CONVERT_LOOP(DST_TYPE, DST_CHANS, SRC_TYPE, SRC_CHANS, CONV) \
   do {                                           \
      int s, j;                                   \
      for (s = 0; s < count - 3; s += 4) {        \
         for (int u = 0; u < 4; u++) {            \
             for (j = 0; j < SRC_CHANS; ++j) {    \
                SRC_TYPE src = typed_src[j];      \
                tmp[j] = CONV;                    \
             }                                    \
             typed_dst[0] = tmp[swizzle_x];       \
             if (DST_CHANS > 1) typed_dst[1] = tmp[swizzle_y]; \
             if (DST_CHANS > 2) typed_dst[2] = tmp[swizzle_z]; \
             if (DST_CHANS > 3) typed_dst[3] = tmp[swizzle_w]; \
             typed_src += SRC_CHANS;              \
             typed_dst += DST_CHANS;              \
         }                                        \
      }                                           \
      for (; s < count; ++s) {                    \
         for (j = 0; j < SRC_CHANS; ++j) {        \
            SRC_TYPE src = typed_src[j];          \
            tmp[j] = CONV;                        \
         }                                        \
         typed_dst[0] = tmp[swizzle_x];           \
         if (DST_CHANS > 1) typed_dst[1] = tmp[swizzle_y]; \
         if (DST_CHANS > 2) typed_dst[2] = tmp[swizzle_z]; \
         if (DST_CHANS > 3) typed_dst[3] = tmp[swizzle_w]; \
         typed_src += SRC_CHANS;                  \
         typed_dst += DST_CHANS;                  \
      }                                           \
   } while (0)

#define SWIZZLE_CONVERT(DST_TYPE, SRC_TYPE, CONV)                 \
   do {                                                           \
      const uint8_t swizzle_x = swizzle[0];                       \
      const uint8_t swizzle_y = swizzle[1];                       \
      const uint8_t swizzle_z = swizzle[2];                       \
      const uint8_t swizzle_w = swizzle[3];                       \
      const SRC_TYPE *typed_src = void_src;                       \
      DST_TYPE *typed_dst = void_dst;                             \
      DST_TYPE tmp[7];                                            \
      tmp[4] = 0;                                                 \
      tmp[5] = one;                                               \
      switch (num_dst_channels) {                                 \
      case 1:                                                     \
         switch (num_src_channels) {                              \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 1, CONV); break; \
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 2, CONV); break; \
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 3, CONV); break; \
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 1, SRC_TYPE, 4, CONV); break; \
         } break;                                                 \
      case 2:                                                     \
         switch (num_src_channels) {                              \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 1, CONV); break; \
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 2, CONV); break; \
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 3, CONV); break; \
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 2, SRC_TYPE, 4, CONV); break; \
         } break;                                                 \
      case 3:                                                     \
         switch (num_src_channels) {                              \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 1, CONV); break; \
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 2, CONV); break; \
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 3, CONV); break; \
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 3, SRC_TYPE, 4, CONV); break; \
         } break;                                                 \
      case 4:                                                     \
         switch (num_src_channels) {                              \
         case 1: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 1, CONV); break; \
         case 2: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 2, CONV); break; \
         case 3: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 3, CONV); break; \
         case 4: SWIZZLE_CONVERT_LOOP(DST_TYPE, 4, SRC_TYPE, 4, CONV); break; \
         } break;                                                 \
      }                                                           \
   } while (0)

static void
convert_float(void *void_dst, int num_dst_channels,
              const void *void_src, GLenum src_type, int num_src_channels,
              const uint8_t swizzle[4], bool normalized, int count)
{
   const float one = 1.0f;
   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT: SWIZZLE_CONVERT(float, float, src); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF: SWIZZLE_CONVERT(float, uint16_t, _mesa_half_to_float(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(float, uint8_t, _mesa_unorm_to_float(src, 8));
      else SWIZZLE_CONVERT(float, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(float, int8_t, _mesa_snorm_to_float(src, 8));
      else SWIZZLE_CONVERT(float, int8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(float, uint16_t, _mesa_unorm_to_float(src, 16));
      else SWIZZLE_CONVERT(float, uint16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(float, int16_t, _mesa_snorm_to_float(src, 16));
      else SWIZZLE_CONVERT(float, int16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(float, uint32_t, _mesa_unorm_to_float(src, 32));
      else SWIZZLE_CONVERT(float, uint32_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(float, int32_t, _mesa_snorm_to_float(src, 32));
      else SWIZZLE_CONVERT(float, int32_t, src); break;
   default: assert(!"Invalid channel type combination");
   }
}

static void
convert_half_float(void *void_dst, int num_dst_channels,
                   const void *void_src, GLenum src_type, int num_src_channels,
                   const uint8_t swizzle[4], bool normalized, int count)
{
   const uint16_t one = _mesa_float_to_half(1.0f);
   switch (src_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT: SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF: SWIZZLE_CONVERT(uint16_t, uint16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_unorm_to_half(src, 8));
      else SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_snorm_to_half(src, 8));
      else SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_unorm_to_half(src, 16));
      else SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_snorm_to_half(src, 16));
      else SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unorm_to_half(src, 32));
      else SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_float_to_half(src)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_snorm_to_half(src, 32));
      else SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_float_to_half(src)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_unorm(src, 8));
      else SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_unorm(src, 8));
      else SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE: SWIZZLE_CONVERT(uint8_t, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(uint8_t, int8_t, _mesa_snorm_to_unorm(src, 8, 8));
      else SWIZZLE_CONVERT(uint8_t, int8_t, _mesa_unsigned_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_unorm_to_unorm(src, 16, 8));
      else SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_unsigned_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(uint8_t, int16_t, _mesa_snorm_to_unorm(src, 16, 8));
      else SWIZZLE_CONVERT(uint8_t, int16_t, _mesa_unsigned_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(uint8_t, uint32_t, _mesa_unorm_to_unorm(src, 32, 8));
      else SWIZZLE_CONVERT(uint8_t, uint32_t, _mesa_unsigned_to_unsigned(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(uint8_t, int32_t, _mesa_snorm_to_unorm(src, 32, 8));
      else SWIZZLE_CONVERT(uint8_t, int32_t, _mesa_signed_to_unsigned(src, 8)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_snorm(src, 8));
      else SWIZZLE_CONVERT(uint8_t, float, _mesa_float_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_snorm(src, 8));
      else SWIZZLE_CONVERT(uint8_t, uint16_t, _mesa_half_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(int8_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 8));
      else SWIZZLE_CONVERT(int8_t, uint8_t, _mesa_unsigned_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE: SWIZZLE_CONVERT(int8_t, int8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 8));
      else SWIZZLE_CONVERT(int8_t, uint16_t, _mesa_unsigned_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(int8_t, int16_t, _mesa_snorm_to_snorm(src, 16, 8));
      else SWIZZLE_CONVERT(int8_t, int16_t, _mesa_signed_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(int8_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 8));
      else SWIZZLE_CONVERT(int8_t, uint32_t, _mesa_unsigned_to_signed(src, 8)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(int8_t, int32_t, _mesa_snorm_to_snorm(src, 32, 8));
      else SWIZZLE_CONVERT(int8_t, int32_t, _mesa_signed_to_signed(src, 8)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_unorm(src, 16));
      else SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_unsigned(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_unorm(src, 16));
      else SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_unsigned(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint8_t, _mesa_unorm_to_unorm(src, 8, 16));
      else SWIZZLE_CONVERT(uint16_t, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_snorm_to_unorm(src, 8, 16));
      else SWIZZLE_CONVERT(uint16_t, int8_t, _mesa_signed_to_unsigned(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT: SWIZZLE_CONVERT(uint16_t, uint16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_snorm_to_unorm(src, 16, 16));
      else SWIZZLE_CONVERT(uint16_t, int16_t, _mesa_signed_to_unsigned(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unorm_to_unorm(src, 32, 16));
      else SWIZZLE_CONVERT(uint16_t, uint32_t, _mesa_unsigned_to_unsigned(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_snorm_to_unorm(src, 32, 16));
      else SWIZZLE_CONVERT(uint16_t, int32_t, _mesa_signed_to_unsigned(src, 16)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_snorm(src, 16));
      else SWIZZLE_CONVERT(uint16_t, float, _mesa_float_to_signed(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_snorm(src, 16));
      else SWIZZLE_CONVERT(uint16_t, uint16_t, _mesa_half_to_signed(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(int16_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 16));
      else SWIZZLE_CONVERT(int16_t, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(int16_t, int8_t, _mesa_snorm_to_snorm(src, 8, 16));
      else SWIZZLE_CONVERT(int16_t, int8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 16));
      else SWIZZLE_CONVERT(int16_t, uint16_t, _mesa_unsigned_to_signed(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT: SWIZZLE_CONVERT(int16_t, int16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(int16_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 16));
      else SWIZZLE_CONVERT(int16_t, uint32_t, _mesa_unsigned_to_signed(src, 16)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(int16_t, int32_t, _mesa_snorm_to_snorm(src, 32, 16));
      else SWIZZLE_CONVERT(int16_t, int32_t, _mesa_signed_to_signed(src, 16)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_unorm(src, 32));
      else SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_unsigned(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_unorm(src, 32));
      else SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_unsigned(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(uint32_t, uint8_t, _mesa_unorm_to_unorm(src, 8, 32));
      else SWIZZLE_CONVERT(uint32_t, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(uint32_t, int8_t, _mesa_snorm_to_unorm(src, 8, 32));
      else SWIZZLE_CONVERT(uint32_t, int8_t, _mesa_signed_to_unsigned(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_unorm_to_unorm(src, 16, 32));
      else SWIZZLE_CONVERT(uint32_t, uint16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(uint32_t, int16_t, _mesa_snorm_to_unorm(src, 16, 32));
      else SWIZZLE_CONVERT(uint32_t, int16_t, _mesa_signed_to_unsigned(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT: SWIZZLE_CONVERT(uint32_t, uint32_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:
      if (normalized) SWIZZLE_CONVERT(uint32_t, int32_t, _mesa_snorm_to_unorm(src, 32, 32));
      else SWIZZLE_CONVERT(uint32_t, int32_t, _mesa_signed_to_unsigned(src, 32)); break;
   default: assert(!"Invalid channel type combination");
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
      if (normalized) SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_snorm(src, 32));
      else SWIZZLE_CONVERT(uint32_t, float, _mesa_float_to_signed(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:
      if (normalized) SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_snorm(src, 32));
      else SWIZZLE_CONVERT(uint32_t, uint16_t, _mesa_half_to_signed(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:
      if (normalized) SWIZZLE_CONVERT(int32_t, uint8_t, _mesa_unorm_to_snorm(src, 8, 32));
      else SWIZZLE_CONVERT(int32_t, uint8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:
      if (normalized) SWIZZLE_CONVERT(int32_t, int8_t, _mesa_snorm_to_snorm(src, 8, 32));
      else SWIZZLE_CONVERT(int32_t, int8_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT:
      if (normalized) SWIZZLE_CONVERT(int32_t, uint16_t, _mesa_unorm_to_snorm(src, 16, 32));
      else SWIZZLE_CONVERT(int32_t, uint16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:
      if (normalized) SWIZZLE_CONVERT(int32_t, int16_t, _mesa_snorm_to_snorm(src, 16, 32));
      else SWIZZLE_CONVERT(int32_t, int16_t, src); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:
      if (normalized) SWIZZLE_CONVERT(int32_t, uint32_t, _mesa_unorm_to_snorm(src, 32, 32));
      else SWIZZLE_CONVERT(int32_t, uint32_t, _mesa_unsigned_to_signed(src, 32)); break;
   case MESA_ARRAY_FORMAT_TYPE_INT: SWIZZLE_CONVERT(int32_t, int32_t, src); break;
   default: assert(!"Invalid channel type combination");
   }
}

void
_mesa_swizzle_and_convert(void *void_dst, enum mesa_array_format_datatype dst_type, int num_dst_channels,
                          const void *void_src, enum mesa_array_format_datatype src_type, int num_src_channels,
                          const uint8_t swizzle[4], bool normalized, int count)
{
   if (swizzle_convert_try_memcpy(void_dst, dst_type, num_dst_channels,
                                  void_src, src_type, num_src_channels,
                                  swizzle, normalized, count))
      return;

   if (swizzle_convert_try_avx2(void_dst, dst_type, num_dst_channels,
                                void_src, src_type, num_src_channels,
                                swizzle, normalized, count))
      return;

   switch (dst_type) {
   case MESA_ARRAY_FORMAT_TYPE_FLOAT:  convert_float(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_HALF:   convert_half_float(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_UBYTE:  convert_ubyte(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_BYTE:   convert_byte(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_USHORT: convert_ushort(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_SHORT:  convert_short(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_UINT:   convert_uint(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   case MESA_ARRAY_FORMAT_TYPE_INT:    convert_int(void_dst, num_dst_channels, void_src, src_type, num_src_channels, swizzle, normalized, count); break;
   default: assert(!"Invalid channel type");
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

   /* Row buffers */
   uint8_t (*tmp_ubyte)[4] = NULL;
   float (*tmp_float)[4] = NULL;
   uint32_t (*tmp_uint)[4] = NULL;

   int bits;
   size_t row;

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
    * FAST PATHS: Optimized conversions bypassing row buffers
    * -------------------------------------------------------------- */
   if (!rebase_swizzle) {
      /* Direct Memcpy */
      if ((dst_format_is_mesa_array_format &&
           src_format_is_mesa_array_format &&
           src_array_format == dst_array_format) ||
          src_format == dst_format) {
         int format_size = _mesa_get_format_bytes(src_format);
         for (row = 0; row < height; row++) {
            memcpy(dst, src, width * format_size);
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }

#if defined(__AVX2__)
      /* 16-bit Swap Optimizations (A1B5G5R5 <-> B5G5R5A1) */
      if (src_format == MESA_FORMAT_A1B5G5R5_UNORM && dst_format == MESA_FORMAT_B5G5R5A1_UNORM) {
         for (row = 0; row < height; row++) {
            convert_16bit_rotate_avx2((uint16_t *)dst, (const uint16_t *)src, width, 0); /* Left */
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }
      if (src_format == MESA_FORMAT_B5G5R5A1_UNORM && dst_format == MESA_FORMAT_A1B5G5R5_UNORM) {
         for (row = 0; row < height; row++) {
            convert_16bit_rotate_avx2((uint16_t *)dst, (const uint16_t *)src, width, 1); /* Right */
            src += src_stride;
            dst += dst_stride;
         }
         return;
      }
#endif

      /* Direct Unpack (No row buffer needed for unpack directly to dst) */
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
         } else if (dst_array_format == BGRA8_UBYTE && src_format == MESA_FORMAT_R8G8B8A8_UNORM) {
             convert_ubyte_rgba_to_bgra(width, height, src, src_stride, dst, dst_stride);
             return;
#endif
         } else if (dst_array_format == RGBA32_UINT && _mesa_is_format_unsigned(src_format)) {
            assert(_mesa_is_format_integer_color(src_format));
            for (row = 0; row < height; ++row) {
               _mesa_unpack_uint_rgba_row(src_format, width, src, (uint32_t (*)[4])dst);
               src += src_stride;
               dst += dst_stride;
            }
            return;
         }
      }

      /* Direct Pack */
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
         } else if (src_array_format == RGBA32_UINT && _mesa_is_format_unsigned(dst_format)) {
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
    * SETUP: Slow Path (Row-Based Processing)
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

   /* Direct Swizzle (Array -> Array) */
   if (src_array_format && dst_array_format) {
      assert(_mesa_array_format_is_normalized(src_array_format) ==
             _mesa_array_format_is_normalized(dst_array_format));

      compute_src2dst_component_mapping(src2rgba, rgba2dst, rebase_swizzle, src2dst);

      for (row = 0; row < height; ++row) {
         _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                   src, src_type, src_num_channels,
                                   src2dst, normalized, width);
         src += src_stride;
         dst += dst_stride;
      }
      return;
   }

   dst_integer = false;
   src_integer = false;

   if (src_array_format) {
      if (!_mesa_array_format_is_float(src_array_format) && !_mesa_array_format_is_normalized(src_array_format))
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
   if (dst_array_format) {
      if (!_mesa_array_format_is_float(dst_array_format) && !_mesa_array_format_is_normalized(dst_array_format))
         dst_integer = true;
      is_signed = _mesa_array_format_is_signed(dst_array_format);
      bits = 8 * _mesa_array_format_get_type_size(dst_array_format);
   } else {
      switch (_mesa_get_format_datatype(dst_format)) {
      case GL_UNSIGNED_NORMALIZED: is_signed = false; break;
      case GL_SIGNED_NORMALIZED: is_signed = true; break;
      case GL_FLOAT: is_signed = true; break;
      case GL_UNSIGNED_INT: is_signed = false; dst_integer = true; break;
      case GL_INT: is_signed = true; dst_integer = true; break;
      }
      bits = _mesa_get_format_max_bits(dst_format);
   }

   assert(src_integer == dst_integer);

   /* --------------------------------------------------------------
    * EXECUTION: Row-Based Loop
    * -------------------------------------------------------------- */
   if (src_integer && dst_integer) {
      common_type = is_signed ? MESA_ARRAY_FORMAT_TYPE_INT : MESA_ARRAY_FORMAT_TYPE_UINT;
      tmp_uint = malloc(width * sizeof(*tmp_uint));
      if (!tmp_uint) return;

      if (src_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_array_format) {
            _mesa_swizzle_and_convert(tmp_uint, common_type, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, width);
         } else {
            _mesa_unpack_uint_rgba_row(src_format, width, src, tmp_uint);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_uint, common_type, 4,
                                         tmp_uint, common_type, 4,
                                         rebase_swizzle, false, width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_uint, common_type, 4,
                                      rgba2dst, normalized, width);
         } else {
            _mesa_pack_uint_rgba_row(dst_format, width, (const uint32_t (*)[4])tmp_uint, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }
      free(tmp_uint);
   } else if (is_signed || bits > 8) {
      tmp_float = malloc(width * sizeof(*tmp_float));
      if (!tmp_float) return;

      if (src_format_is_mesa_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, width);
         } else {
            _mesa_unpack_rgba_row(src_format, width, src, tmp_float);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                         tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                         rebase_swizzle, normalized, width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_float, MESA_ARRAY_FORMAT_TYPE_FLOAT, 4,
                                      rgba2dst, normalized, width);
         } else {
            _mesa_pack_float_rgba_row(dst_format, width, (const float (*)[4])tmp_float, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }
      free(tmp_float);
   } else {
      tmp_ubyte = malloc(width * sizeof(*tmp_ubyte));
      if (!tmp_ubyte) return;

      if (src_format_is_mesa_array_format) {
         compute_rebased_rgba_component_mapping(src2rgba, rebase_swizzle, rebased_src2rgba);
      }

      for (row = 0; row < height; ++row) {
         if (src_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                      src, src_type, src_num_channels,
                                      rebased_src2rgba, normalized, width);
         } else {
            _mesa_unpack_ubyte_rgba_row(src_format, width, src, tmp_ubyte);
            if (rebase_swizzle)
               _mesa_swizzle_and_convert(tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                         tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                         rebase_swizzle, normalized, width);
         }

         if (dst_format_is_mesa_array_format) {
            _mesa_swizzle_and_convert(dst, dst_type, dst_num_channels,
                                      tmp_ubyte, MESA_ARRAY_FORMAT_TYPE_UBYTE, 4,
                                      rgba2dst, normalized, width);
         } else {
            _mesa_pack_ubyte_rgba_row(dst_format, width, (const uint8_t *)tmp_ubyte, dst);
         }
         src += src_stride;
         dst += dst_stride;
      }
      free(tmp_ubyte);
   }
}
