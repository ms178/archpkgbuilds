/* adler32_avx2.c -- compute the Adler-32 checksum of a data stream
 * Copyright (C) 1995-2011 Mark Adler
 * Copyright (C) 2022 Adam Stylinski
 * Authors:
 *   Brian Bockelman <bockelman@gmail.com>
 *   Adam Stylinski <kungfujesus06@gmail.com>
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

#ifdef X86_AVX2

#include "zbuild.h"
#include <immintrin.h>
#include <stdint.h>
#include "adler32_p.h"
#include "adler32_avx2_p.h"
#include "x86_intrins.h"

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
_Static_assert(NMAX >= 64, "NMAX must accommodate at least one AVX2 block");
_Static_assert(NMAX < 5553, "NMAX must prevent 32-bit overflow in s2");
_Static_assert(BASE == 65521, "BASE must be largest prime below 2^16");
#endif

extern uint32_t adler32_fold_copy_sse42(uint32_t adler, uint8_t *dst,
                                        const uint8_t *src, size_t len);
extern uint32_t adler32_ssse3(uint32_t adler, const uint8_t *src, size_t len);

static inline uint32_t adler32_fold_copy_impl(uint32_t adler, uint8_t *dst,
                                              const uint8_t *src, size_t len,
                                              const int COPY) {
    if (src == NULL)
        return 1U;
    if (len == 0)
        return adler;
    if (COPY && dst == NULL)
        return 1U;

    uint32_t adler0 = adler & 0xFFFFU;
    uint32_t adler1 = (adler >> 16) & 0xFFFFU;

    const __m256i dot2v = _mm256_setr_epi8(
        64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49,
        48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33);
    const __m256i dot2v_0 = _mm256_setr_epi8(
        32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17,
        16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1);
    const __m256i dot3v = _mm256_set1_epi16(1);
    const __m256i zero = _mm256_setzero_si256();

    while (len >= 32) {
        __m256i vs1, vs2, vs1_0, vs2_0, vs3;

        vs1 = _mm256_zextsi128_si256(_mm_cvtsi32_si128((int)adler0));
        vs2 = _mm256_zextsi128_si256(_mm_cvtsi32_si128((int)adler1));
        vs1_0 = vs1;
        vs3 = _mm256_setzero_si256();
        vs2_0 = _mm256_setzero_si256();

        size_t k = (len < NMAX) ? len : NMAX;
        k &= ~(size_t)31;
        len -= k;

        while (k >= 64) {
            __m256i vbuf = _mm256_loadu_si256((const __m256i *)src);
            __m256i vbuf_0 = _mm256_loadu_si256((const __m256i *)(src + 32));
            __m256i vs1_sad = _mm256_sad_epu8(vbuf, zero);
            __m256i vs1_sad2 = _mm256_sad_epu8(vbuf_0, zero);

            if (COPY) {
                _mm256_storeu_si256((__m256i *)dst, vbuf);
                _mm256_storeu_si256((__m256i *)(dst + 32), vbuf_0);
                dst += 64;
            }

            vs3 = _mm256_add_epi32(vs3, vs1_0);
            vs1 = _mm256_add_epi32(vs1, vs1_sad);

            __m256i v_short_sum2 = _mm256_maddubs_epi16(vbuf, dot2v);
            __m256i v_short_sum2_0 = _mm256_maddubs_epi16(vbuf_0, dot2v_0);
            __m256i vsum2 = _mm256_madd_epi16(v_short_sum2, dot3v);
            __m256i vsum2_0 = _mm256_madd_epi16(v_short_sum2_0, dot3v);

            vs1 = _mm256_add_epi32(vs1, vs1_sad2);
            vs2 = _mm256_add_epi32(vs2, vsum2);
            vs2_0 = _mm256_add_epi32(vs2_0, vsum2_0);

            vs1_0 = vs1;
            src += 64;
            k -= 64;
        }

        vs2 = _mm256_add_epi32(vs2, vs2_0);
        vs3 = _mm256_slli_epi32(vs3, 6);
        vs2 = _mm256_add_epi32(vs2, vs3);

        vs3 = _mm256_setzero_si256();

        while (k >= 32) {
            __m256i vbuf = _mm256_loadu_si256((const __m256i *)src);
            __m256i vs1_sad = _mm256_sad_epu8(vbuf, zero);

            if (COPY) {
                _mm256_storeu_si256((__m256i *)dst, vbuf);
                dst += 32;
            }

            vs3 = _mm256_add_epi32(vs3, vs1_0);
            vs1 = _mm256_add_epi32(vs1, vs1_sad);

            __m256i v_short_sum2 = _mm256_maddubs_epi16(vbuf, dot2v_0);
            __m256i vsum2 = _mm256_madd_epi16(v_short_sum2, dot3v);
            vs2 = _mm256_add_epi32(vs2, vsum2);

            vs1_0 = vs1;
            src += 32;
            k -= 32;
        }

        vs3 = _mm256_slli_epi32(vs3, 5);
        vs2 = _mm256_add_epi32(vs2, vs3);

        adler0 = partial_hsum256(vs1) % BASE;
        adler1 = hsum256(vs2) % BASE;
    }

    if (len > 0) {
        uint32_t adler_combined = adler0 | (adler1 << 16);
        _mm256_zeroupper();

        if (len < 16) {
            if (COPY)
                return adler32_copy_len_16(adler0, src, dst, len, adler1);
            return adler32_len_16(adler0, src, len, adler1);
        }

        if (COPY)
            return adler32_fold_copy_sse42(adler_combined, dst, src, len);
        return adler32_ssse3(adler_combined, src, len);
    }

    _mm256_zeroupper();
    return adler0 | (adler1 << 16);
}

Z_INTERNAL uint32_t adler32_avx2(uint32_t adler, const uint8_t *src, size_t len) {
    return adler32_fold_copy_impl(adler, NULL, src, len, 0);
}

Z_INTERNAL uint32_t adler32_fold_copy_avx2(uint32_t adler, uint8_t *dst,
                                           const uint8_t *src, size_t len) {
    return adler32_fold_copy_impl(adler, dst, src, len, 1);
}

#endif
