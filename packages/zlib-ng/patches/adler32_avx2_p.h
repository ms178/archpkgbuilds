/* adler32_avx2_p.h -- Adler-32 AVX2 utility functions
 * Copyright (C) 2022 Adam Stylinski
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

#ifndef ZLIB_ADLER32_AVX2_P_H_
#define ZLIB_ADLER32_AVX2_P_H_

#if defined(X86_AVX2) || defined(X86_AVX512VNNI)

#include <immintrin.h>
#include <stdint.h>

/*
 * hsum256 - Horizontal sum of 8 x 32-bit integers in YMM register
 *
 * Algorithm (adapted from Agner Fog's vector library):
 *   1. Add high 128-bit lane to low lane
 *   2. Add high 64 bits to low 64 bits
 *   3. Add element 1 to element 0
 *
 * Latency: ~6 cycles on Skylake+
 * Throughput: ~2 cycles (limited by shuffle port)
 */
static inline uint32_t hsum256(__m256i x) {
    /* Step 1: Fold 256 -> 128 bits */
    __m128i sum128 = _mm_add_epi32(
        _mm256_extracti128_si256(x, 1),
        _mm256_castsi256_si128(x)
    );

    /* Step 2: Fold 128 -> 64 bits
     * unpackhi_epi64 duplicates high 64 bits: [a,b,c,d] -> [c,d,c,d] */
    __m128i sum64 = _mm_add_epi32(
        sum128,
        _mm_unpackhi_epi64(sum128, sum128)
    );

    /* Step 3: Fold 64 -> 32 bits
     * shuffle with imm=1 (0b00000001) gives [elem1, elem0, elem0, elem0] */
    __m128i sum32 = _mm_add_epi32(
        sum64,
        _mm_shuffle_epi32(sum64, 1)
    );

    return (uint32_t)_mm_cvtsi128_si32(sum32);
}

/*
 * partial_hsum256 - Horizontal sum of SAD results in YMM register
 *
 * SAD (vpsadbw) produces 4 x 64-bit results in a 256-bit register:
 *   - Bits [63:0]:    sum of bytes 0-7
 *   - Bits [127:64]:  zero (high part of first 64-bit element)
 *   - Bits [191:128]: sum of bytes 8-15
 *   - etc.
 *
 * When viewed as 8 x 32-bit elements, meaningful values are in
 * positions 0, 2, 4, 6 (the low 32 bits of each 64-bit slot).
 *
 * This function extracts and sums those four values.
 *
 * Latency: ~8 cycles on Skylake+ (dominated by cross-lane permutevar)
 */
static inline uint32_t partial_hsum256(__m256i x) {
    /*
     * Permutation to gather elements 0, 2, 4, 6 into contiguous positions.
     * permutevar8x32 has 3-cycle latency on Skylake+.
     *
     * Static would be ideal but creates ODR issues in headers.
     * Compiler will hoist this constant out of any loop.
     */
    const __m256i perm_vec = _mm256_setr_epi32(0, 2, 4, 6, 1, 1, 1, 1);

    /* Gather the four SAD results into low 128 bits */
    __m256i gathered = _mm256_permutevar8x32_epi32(x, perm_vec);

    /* Extract low 128 bits containing our 4 values */
    __m128i values = _mm256_castsi256_si128(gathered);

    /* Standard 4-element horizontal sum */
    __m128i sum64 = _mm_add_epi32(
        values,
        _mm_unpackhi_epi64(values, values)
    );

    __m128i sum32 = _mm_add_epi32(
        sum64,
        _mm_shuffle_epi32(sum64, 1)
    );

    return (uint32_t)_mm_cvtsi128_si32(sum32);
}

#endif /* X86_AVX2 || X86_AVX512VNNI */

#endif /* ZLIB_ADLER32_AVX2_P_H_ */
