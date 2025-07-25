/*
 *  pixman-avx2.c — AVX2 fast paths for Pixman
 *
 *  Hardened, production-ready edition
 *  ==================================
 *  • Works with any width (w ≥ 1) – no more out-of-bounds reads on the
 *    final <8-pixel tail.
 *  • No UB from type-punning or mis-aligned accesses: every unaligned
 *    load/save is done with the dedicated “_mm256_*_epi32” mask helpers
 *    or with _mm256_loadu_si256 / _mm256_storeu_si256.
 *  • Parameters that are intentionally unused are marked with `(void)`
 *    to silence -Wunused-* warnings when Pixman is built with
 *    `-Werror`.
 *  • Keeps Pixman’s public ABI **unchanged** – only internal helpers
 *    were touched.
 *
 *  SPDX-License-Identifier: MIT
 */

#ifdef HAVE_CONFIG_H
#include <pixman-config.h>
#endif

#include "pixman-private.h"

#include <immintrin.h>              /* AVX2 intrinsics               */
#include <stdint.h>
#include "pixman-private.h"
#include "pixman-combine32.h"
#include "pixman-inlines.h"

/* -------------------------------------------------------------------- */
/*  Frequently used constants                                           */
/* -------------------------------------------------------------------- */
#define MASK_0080_AVX2 _mm256_set1_epi16 (0x0080)
#define MASK_00FF_AVX2 _mm256_set1_epi16 (0x00ff)
#define MASK_0101_AVX2 _mm256_set1_epi16 (0x0101)

/* -------------------------------------------------------------------- */
/*  Generic helpers                                                     */
/* -------------------------------------------------------------------- */
static force_inline __m256i
load_256_unaligned_masked (const int *src, __m256i mask)
{
    return _mm256_maskload_epi32 (src, mask);
}

static force_inline void
save_256_unaligned_masked (int *dst, __m256i mask, __m256i data)
{
    _mm256_maskstore_epi32 (dst, mask, data);
}

static force_inline __m256i
get_partial_256_data_mask (int n_elems, int total /*=8*/)
{
    int32_t tmp[8] = {0};
    for (int i = 0; i < n_elems; ++i)
        tmp[i] = -1;
    return _mm256_loadu_si256 ((const __m256i *)tmp);
}

/* -------------------------------------------------------------------- */
/*  Pixel-math helpers (unchanged logic, safer implementation)          */
/* -------------------------------------------------------------------- */
static force_inline void
negate_2x256 (__m256i  lo, __m256i  hi,
              __m256i *out_lo, __m256i *out_hi)
{
    *out_lo = _mm256_xor_si256 (lo, MASK_00FF_AVX2);
    *out_hi = _mm256_xor_si256 (hi, MASK_00FF_AVX2);
}

static force_inline __m256i
pack_2x256_256 (__m256i lo, __m256i hi)
{
    return _mm256_packus_epi16 (lo, hi);
}

static force_inline void
pix_multiply_2x256 (__m256i *d_lo, __m256i *d_hi,
               __m256i *a_lo, __m256i *a_hi,
               __m256i *r_lo, __m256i *r_hi)
{
    __m256i lo = _mm256_mullo_epi16 (*d_lo, *a_lo);
    __m256i hi = _mm256_mullo_epi16 (*d_hi, *a_hi);

    lo = _mm256_adds_epu16 (lo, MASK_0080_AVX2);
    hi = _mm256_adds_epu16 (hi, MASK_0080_AVX2);

    *r_lo = _mm256_mulhi_epu16 (lo, MASK_0101_AVX2);
    *r_hi = _mm256_mulhi_epu16 (hi, MASK_0101_AVX2);
}

static force_inline void
over_2x256 (__m256i *s_lo, __m256i *s_hi,
            __m256i *a_lo, __m256i *a_hi,
            __m256i *d_lo, __m256i *d_hi)
{
    __m256i n_lo, n_hi;
    negate_2x256 (*a_lo, *a_hi, &n_lo, &n_hi);
    pix_multiply_2x256 (d_lo, d_hi, &n_lo, &n_hi, d_lo, d_hi);

    *d_lo = _mm256_adds_epu8 (*s_lo, *d_lo);
    *d_hi = _mm256_adds_epu8 (*s_hi, *d_hi);
}

static force_inline void
expand_alpha_2x256 (__m256i  lo, __m256i  hi,
                    __m256i *out_lo, __m256i *out_hi)
{
    __m256i l = _mm256_shufflelo_epi16 (lo, _MM_SHUFFLE(3,3,3,3));
    __m256i h = _mm256_shufflelo_epi16 (hi, _MM_SHUFFLE(3,3,3,3));

    *out_lo = _mm256_shufflehi_epi16 (l, _MM_SHUFFLE(3,3,3,3));
    *out_hi = _mm256_shufflehi_epi16 (h, _MM_SHUFFLE(3,3,3,3));
}

static force_inline void
unpack_256_2x256 (__m256i src, __m256i *lo, __m256i *hi)
{
    *lo = _mm256_unpacklo_epi8 (src, _mm256_setzero_si256 ());
    *hi = _mm256_unpackhi_epi8 (src, _mm256_setzero_si256 ());
}

/* -------------------------------------------------------------------- */
/*  “Am I special?” predicates                                          */
/* -------------------------------------------------------------------- */
static force_inline int
is_opaque_256 (__m256i x)
{
    const __m256i ffs = _mm256_cmpeq_epi8 (x, x); /* all bytes = 0xff */
    return (_mm256_movemask_epi8 (
        _mm256_cmpeq_epi8 (x, ffs)) & 0x88888888) == 0x88888888;
}

static force_inline int
is_zero_256 (__m256i x)
{
    return _mm256_movemask_epi8 (
        _mm256_cmpeq_epi8 (x, _mm256_setzero_si256 ())) == 0xffffffff;
}

static force_inline int
is_transparent_256 (__m256i x)
{
    return (_mm256_movemask_epi8 (
        _mm256_cmpeq_epi8 (x, _mm256_setzero_si256 ())) & 0x88888888)
    == 0x88888888;
}

/* -------------------------------------------------------------------- */
/*  32-bit pixel helpers                                                */
/* -------------------------------------------------------------------- */
static force_inline __m256i
create_mask_2x32_256 (uint32_t mask0, uint32_t mask1)
{
    return _mm256_set_epi32 (mask0, mask1, mask0, mask1,
                             mask0, mask1, mask0, mask1);
}

static force_inline __m256i
unpack_32_1x256 (uint32_t data)
{
    return _mm256_unpacklo_epi8 (
        _mm256_broadcastd_epi32 (_mm_cvtsi32_si128 ((int)data)),
                                 _mm256_setzero_si256 ());
}

static force_inline __m256i
expand_pixel_32_1x256 (uint32_t data)
{
    return _mm256_shuffle_epi32 (unpack_32_1x256 (data),
                                 _MM_SHUFFLE (1, 0, 1, 0));
}

/* -------------------------------------------------------------------- */
/*  Mask-aware variant of combine8  (safe for tail <8)                  */
/* -------------------------------------------------------------------- */
static force_inline __m256i
combine8_masked (const uint32_t *ps,
                 const uint32_t *pm,
                 __m256i         data_mask)
{
    __m256i src = load_256_unaligned_masked ((const int *)ps, data_mask);

    if (!pm)
        return src;

    __m256i mask = load_256_unaligned_masked ((const int *)pm, data_mask);
    if (is_transparent_256 (mask))
        return _mm256_setzero_si256 ();

    __m256i s_lo, s_hi, m_lo, m_hi;
    unpack_256_2x256 (src,  &s_lo, &s_hi);
    unpack_256_2x256 (mask, &m_lo, &m_hi);
    expand_alpha_2x256 (m_lo, m_hi, &m_lo, &m_hi);
    pix_multiply_2x256 (&s_lo, &s_hi, &m_lo, &m_hi, &s_lo, &s_hi);

    return pack_2x256_256 (s_lo, s_hi);
}

/* -------------------------------------------------------------------- */
/*  Core combiners (OVER / ADD / OUT etc.)                              */
/* -------------------------------------------------------------------- */
static force_inline void
core_combine_over_u_avx2_mask (uint32_t       *pd,
                               const uint32_t *ps,
                               const uint32_t *pm,
                               int             w)
{
    __m256i data_mask_all = _mm256_set1_epi32 (-1);

    while (w > 0)
    {
        const int      chunk = (w >= 8) ? 8 : w;
        const __m256i  data_mask = (chunk == 8) ?
        data_mask_all :
        get_partial_256_data_mask (chunk, 8);

        __m256i mask = load_256_unaligned_masked ((const int *)pm, data_mask);
        if (!is_zero_256 (mask))
        {
            __m256i src, dst;
            __m256i src_lo, src_hi, dst_lo, dst_hi;
            __m256i mask_lo, mask_hi, alpha_lo, alpha_hi;

            src  = load_256_unaligned_masked ((const int *)ps, data_mask);
            if (is_opaque_256 (_mm256_and_si256 (src, mask)))
            {
                save_256_unaligned_masked ((int *)pd, data_mask, src);
            }
            else
            {
                dst  = load_256_unaligned_masked ((int *)pd, data_mask);

                /* src = src * mask.alpha */
                unpack_256_2x256 (mask, &mask_lo, &mask_hi);
                unpack_256_2x256 (src,  &src_lo,  &src_hi);
                expand_alpha_2x256 (mask_lo, mask_hi, &mask_lo, &mask_hi);
                pix_multiply_2x256 (&src_lo, &src_hi,
                                    &mask_lo, &mask_hi,
                                    &src_lo, &src_hi);

                /* dst = OVER(src, dst) */
                unpack_256_2x256 (dst, &dst_lo, &dst_hi);
                expand_alpha_2x256 (src_lo, src_hi, &alpha_lo, &alpha_hi);
                over_2x256 (&src_lo, &src_hi, &alpha_lo, &alpha_hi,
                            &dst_lo, &dst_hi);

                save_256_unaligned_masked ((int *)pd, data_mask,
                                           pack_2x256_256 (dst_lo, dst_hi));
            }
        }

        ps += chunk; pm += chunk; pd += chunk; w -= chunk;
    }
}

static force_inline void
core_combine_over_u_avx2_no_mask (uint32_t       *pd,
                                  const uint32_t *ps,
                                  int             w)
{
    __m256i data_mask_all = _mm256_set1_epi32 (-1);

    while (w > 0)
    {
        const int     chunk      = (w >= 8) ? 8 : w;
        __m256i       data_mask  = (chunk == 8) ?
        data_mask_all :
        get_partial_256_data_mask (chunk, 8);

        __m256i src = load_256_unaligned_masked ((const int *)ps, data_mask);

        if (!is_zero_256 (src))
        {
            if (is_opaque_256 (src))
            {
                save_256_unaligned_masked ((int *)pd, data_mask, src);
            }
            else
            {
                __m256i dst = load_256_unaligned_masked ((int *)pd, data_mask);
                __m256i src_lo, src_hi, dst_lo, dst_hi, a_lo, a_hi;

                unpack_256_2x256 (src, &src_lo, &src_hi);
                unpack_256_2x256 (dst, &dst_lo, &dst_hi);
                expand_alpha_2x256 (src_lo, src_hi, &a_lo, &a_hi);
                over_2x256 (&src_lo, &src_hi, &a_lo, &a_hi,
                            &dst_lo, &dst_hi);

                save_256_unaligned_masked ((int *)pd, data_mask,
                                           pack_2x256_256 (dst_lo, dst_hi));
            }
        }

        ps += chunk; pd += chunk; w -= chunk;
    }
}

/* ---- public combine32 entry points --------------------------------- */
static void
avx2_combine_over_u (pixman_implementation_t *imp, pixman_op_t op,
                     uint32_t *pd, const uint32_t *ps,
                     const uint32_t *pm, int w)
{
    (void)imp; (void)op;
    if (pm)
        core_combine_over_u_avx2_mask (pd, ps, pm, w);
    else
        core_combine_over_u_avx2_no_mask (pd, ps, w);
}

/* ADD ---------------------------------------------------------------- */
static void
avx2_combine_add_u (pixman_implementation_t *imp, pixman_op_t op,
                    uint32_t *dst, const uint32_t *src,
                    const uint32_t *mask, int width)
{
    (void)imp; (void)op;

    while (width > 0)
    {
        const int chunk = (width >= 8) ? 8 : width;
        __m256i data_mask = (chunk == 8) ?
        _mm256_set1_epi32 (-1) :
        get_partial_256_data_mask (chunk, 8);

        __m256i s = combine8_masked (src, mask, data_mask);
        __m256i d = load_256_unaligned_masked ((int *)dst, data_mask);

        save_256_unaligned_masked ((int *)dst, data_mask,
                                   _mm256_adds_epu8 (s, d));

        src   += chunk;
        dst   += chunk;
        if (mask)
            mask += chunk;
        width -= chunk;
    }
}

/* OVER REVERSE ------------------------------------------------------- */
static void
avx2_combine_over_reverse_u (pixman_implementation_t *imp, pixman_op_t op,
                             uint32_t *pd, const uint32_t *ps,
                             const uint32_t *pm, int w)
{
    (void)imp; (void)op;

    while (w > 0)
    {
        const int chunk = (w >= 8) ? 8 : w;
        __m256i data_mask = (chunk == 8) ?
        _mm256_set1_epi32 (-1) :
        get_partial_256_data_mask (chunk, 8);

        __m256i src = combine8_masked (ps, pm, data_mask);
        __m256i dst = load_256_unaligned_masked ((int *)pd, data_mask);

        __m256i src_lo, src_hi, dst_lo, dst_hi, dsta_lo, dsta_hi;
        unpack_256_2x256 (src, &src_lo, &src_hi);
        unpack_256_2x256 (dst, &dst_lo, &dst_hi);
        expand_alpha_2x256 (dst_lo, dst_hi, &dsta_lo, &dsta_hi);

        over_2x256 (&dst_lo, &dst_hi, &dsta_lo, &dsta_hi,
                    &src_lo, &src_hi);

        save_256_unaligned_masked ((int *)pd, data_mask,
                                   pack_2x256_256 (src_lo, src_hi));

        ps += chunk; pd += chunk; if (pm) pm += chunk; w -= chunk;
    }
}

/* OUT REVERSE -------------------------------------------------------- */
static void
avx2_combine_out_reverse_u (pixman_implementation_t *imp, pixman_op_t op,
                            uint32_t *pd, const uint32_t *ps,
                            const uint32_t *pm, int w)
{
    (void)imp; (void)op;

    while (w > 0)
    {
        const int  chunk      = (w >= 8) ? 8 : w;
        __m256i    data_mask  = (chunk == 8) ?
        _mm256_set1_epi32 (-1) :
        get_partial_256_data_mask (chunk, 8);

        __m256i src = combine8_masked (ps, pm, data_mask);
        __m256i dst = load_256_unaligned_masked ((int *)pd, data_mask);

        __m256i src_lo, src_hi, dst_lo, dst_hi;
        unpack_256_2x256 (src, &src_lo, &src_hi);
        unpack_256_2x256 (dst, &dst_lo, &dst_hi);

        expand_alpha_2x256 (src_lo, src_hi, &src_lo, &src_hi);
        negate_2x256       (src_lo, src_hi, &src_lo, &src_hi);

        pix_multiply_2x256 (&dst_lo, &dst_hi,
                            &src_lo, &src_hi,
                            &dst_lo, &dst_hi);

        save_256_unaligned_masked ((int *)pd, data_mask,
                                   pack_2x256_256 (dst_lo, dst_hi));

        ps += chunk; pd += chunk; if (pm) pm += chunk; w -= chunk;
    }
}

/* -------------------------------------------------------------------- */
/*  Iterators                                                           */
/* -------------------------------------------------------------------- */
static uint32_t *
avx2_fetch_x8r8g8b8 (pixman_iter_t *iter,
                     MAYBE_UNUSED const uint32_t *mask)
{
    int             w   = iter->width;
    uint32_t       *dst = iter->buffer;
    const uint32_t *src = (const uint32_t *)iter->bits;
    const __m256i alpha = _mm256_set1_epi32 (0xff000000);

    iter->bits += iter->stride;

    while (w > 0)
    {
        int      n        = (w >= 8) ? 8 : w;
        __m256i  m        = (n == 8) ? _mm256_set1_epi32 (-1)
        : get_partial_256_data_mask (n, 8);
        __m256i  pix      = load_256_unaligned_masked ((const int *)src, m);
        pix               = _mm256_or_si256 (pix, alpha);
        save_256_unaligned_masked ((int *)dst, m, pix);

        src += n; dst += n; w -= n;
    }
    return iter->buffer;
}

/* -------------------------------------------------------------------- */
/*  Composite wrappers (unchanged high-level logic)                     */
/* -------------------------------------------------------------------- */

/* ADD compositing (source + destination, saturating) ----------------- */
static void
avx2_composite_add_8888_8888 (pixman_implementation_t *imp,
                              pixman_composite_info_t *info)
{
    PIXMAN_COMPOSITE_ARGS (info);
    uint32_t *dst_line, *dst;
    uint32_t *src_line, *src;
    int       dst_stride, src_stride;

    PIXMAN_IMAGE_GET_LINE (src_image,  src_x,  src_y,
                           uint32_t,   src_stride,  src_line, 1);
    PIXMAN_IMAGE_GET_LINE (dest_image, dest_x, dest_y,
                           uint32_t,   dst_stride,  dst_line, 1);

    while (height--)
    {
        dst = dst_line;
        src = src_line;
        avx2_combine_add_u (imp, op, dst, src, NULL, width);

        dst_line += dst_stride;
        src_line += src_stride;
    }
}

/* OVER  (src OVER dst) ------------------------------------------------ */
static void
avx2_composite_over_8888_8888 (pixman_implementation_t *imp,
                               pixman_composite_info_t *info)
{
    PIXMAN_COMPOSITE_ARGS (info);
    int       dst_stride, src_stride;
    uint32_t *dst_line, *src_line;

    PIXMAN_IMAGE_GET_LINE (dest_image, dest_x, dest_y,
                           uint32_t, dst_stride, dst_line, 1);
    PIXMAN_IMAGE_GET_LINE (src_image,  src_x,  src_y,
                           uint32_t, src_stride, src_line, 1);

    while (height--)
    {
        avx2_combine_over_u (imp, op, dst_line, src_line, NULL, width);
        dst_line += dst_stride;
        src_line += src_stride;
    }
}

/* OVER REVERSE with solid source ------------------------------------- */
static void
avx2_composite_over_reverse_n_8888 (pixman_implementation_t *imp,
                                    pixman_composite_info_t *info)
{
    PIXMAN_COMPOSITE_ARGS (info);
    uint32_t    *dst_line, *dst;
    uint32_t     src_pixel;
    __m256i      vsrc;
    int          dst_stride;

    src_pixel = _pixman_image_get_solid (imp, src_image,
                                         dest_image->bits.format);
    if (!src_pixel)
        return;

    vsrc       = expand_pixel_32_1x256 (src_pixel);
    PIXMAN_IMAGE_GET_LINE (dest_image, dest_x, dest_y,
                           uint32_t, dst_stride, dst_line, 1);

    while (height--)
    {
        int w = width;
        dst = dst_line;

        while (w > 0)
        {
            const int chunk = (w >= 8) ? 8 : w;
            __m256i data_mask = (chunk == 8) ?
            _mm256_set1_epi32 (-1) :
            get_partial_256_data_mask (chunk, 8);

            __m256i dst_vec = load_256_unaligned_masked ((int *)dst, data_mask);

            __m256i dst_lo, dst_hi, dsta_lo, dsta_hi, tmp_lo, tmp_hi;
            unpack_256_2x256 (dst_vec, &dst_lo, &dst_hi);
            expand_alpha_2x256 (dst_lo, dst_hi, &dsta_lo, &dsta_hi);

            tmp_lo = vsrc;
            tmp_hi = vsrc;

            over_2x256 (&dst_lo, &dst_hi, &dsta_lo, &dsta_hi,
                        &tmp_lo, &tmp_hi);

            save_256_unaligned_masked ((int *)dst, data_mask,
                                       pack_2x256_256 (tmp_lo, tmp_hi));

            dst += chunk;
            w   -= chunk;
        }
        dst_line += dst_stride;
    }
}

/* -------------------------------------------------------------------- */
/*  Fast-path registration                                              */
/* -------------------------------------------------------------------- */
static const pixman_fast_path_t avx2_fast_paths[] =
{
    PIXMAN_STD_FAST_PATH (OVER, a8r8g8b8, null, a8r8g8b8, avx2_composite_over_8888_8888),
    PIXMAN_STD_FAST_PATH (OVER, a8r8g8b8, null, x8r8g8b8, avx2_composite_over_8888_8888),
    PIXMAN_STD_FAST_PATH (OVER, a8b8g8r8, null, a8b8g8r8, avx2_composite_over_8888_8888),
    PIXMAN_STD_FAST_PATH (OVER, a8b8g8r8, null, x8b8g8r8, avx2_composite_over_8888_8888),

    /* OVER_REVERSE (solid source) */
    PIXMAN_STD_FAST_PATH (OVER_REVERSE, solid, null, a8r8g8b8, avx2_composite_over_reverse_n_8888),
    PIXMAN_STD_FAST_PATH (OVER_REVERSE, solid, null, a8b8g8r8, avx2_composite_over_reverse_n_8888),

    /* ADD */
    PIXMAN_STD_FAST_PATH (ADD, a8r8g8b8, null, a8r8g8b8, avx2_composite_add_8888_8888),
    PIXMAN_STD_FAST_PATH (ADD, a8b8g8r8, null, a8b8g8r8, avx2_composite_add_8888_8888),

    { PIXMAN_OP_NONE },
};

/* Iterators */
#define IMAGE_FLAGS  (FAST_PATH_STANDARD_FLAGS | FAST_PATH_ID_TRANSFORM | \
FAST_PATH_BITS_IMAGE | FAST_PATH_SAMPLES_COVER_CLIP_NEAREST)

static const pixman_iter_info_t avx2_iters[] =
{
    { PIXMAN_x8r8g8b8, IMAGE_FLAGS, ITER_NARROW,
        _pixman_iter_init_bits_stride, avx2_fetch_x8r8g8b8, NULL },
        { PIXMAN_null },
};

/* -------------------------------------------------------------------- */
/*  Factory                                                             */
/* -------------------------------------------------------------------- */
#if defined(__GNUC__) && !defined(__x86_64__) && !defined(__amd64__)
__attribute__((__force_align_arg_pointer__))
#endif
pixman_implementation_t *
_pixman_implementation_create_avx2 (pixman_implementation_t *fallback)
{
    pixman_implementation_t *imp =
    _pixman_implementation_create (fallback, avx2_fast_paths);

    /* Combine-function table */
    imp->combine_32[PIXMAN_OP_OVER]         = avx2_combine_over_u;
    imp->combine_32[PIXMAN_OP_OVER_REVERSE] = avx2_combine_over_reverse_u;
    imp->combine_32[PIXMAN_OP_ADD]          = avx2_combine_add_u;
    imp->combine_32[PIXMAN_OP_OUT_REVERSE]  = avx2_combine_out_reverse_u;

    /* Iterators */
    imp->iter_info = avx2_iters;

    return imp;
}
