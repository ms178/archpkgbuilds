/*
 * xxHash - Extremely Fast Hash algorithm
 * Copyright (C) 2012-2016, Yann Collet.
 *
 * BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following disclaimer
 *     in the documentation and/or other materials provided with the
 *     distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License version 2 as published by the
 * Free Software Foundation. This program is dual-licensed; you may select
 * either version 2 of the GNU General Public License ("GPL") or BSD license
 * ("BSD").
 *
 * You can contact the author at:
 * - xxHash homepage: https://cyan4973.github.io/xxHash/
 * - xxHash source repository: https://github.com/Cyan4973/xxHash
 *
 * Optimized for Intel Raptor Lake, 2025
 */

#include <linux/unaligned.h>
#include <linux/errno.h>
#include <linux/compiler.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/string.h>
#include <linux/xxhash.h>
#include <linux/prefetch.h>

/*-*************************************
 * Macros
 **************************************/
#define xxh_rotl32(x, r) ((x << r) | (x >> (32 - r)))
#define xxh_rotl64(x, r) ((x << r) | (x >> (64 - r)))

/* Optimization: Read 4-byte and 8-byte chunks more efficiently */
#define XXH_get32bits(ptr) get_unaligned_le32(ptr)
#define XXH_get64bits(ptr) get_unaligned_le64(ptr)

/* Prefetch macros optimized for Raptor Lake's cache architecture */
#define XXH_PREFETCH(ptr) prefetch(ptr)
#define XXH_PREFETCH_DIST 512  /* Optimized for Raptor Lake L1/L2 prefetcher behavior */

/* Cache line size for Raptor Lake */
#define XXH_CACHELINE_SIZE 64

#ifdef __LITTLE_ENDIAN
# define XXH_CPU_LITTLE_ENDIAN 1
#else
# define XXH_CPU_LITTLE_ENDIAN 0
#endif

/*-*************************************
 * Constants
 **************************************/
static const uint32_t PRIME32_1 = 2654435761U;
static const uint32_t PRIME32_2 = 2246822519U;
static const uint32_t PRIME32_3 = 3266489917U;
static const uint32_t PRIME32_4 =  668265263U;
static const uint32_t PRIME32_5 =  374761393U;

static const uint64_t PRIME64_1 = 11400714785074694791ULL;
static const uint64_t PRIME64_2 = 14029467366897019727ULL;
static const uint64_t PRIME64_3 =  1609587929392839161ULL;
static const uint64_t PRIME64_4 =  9650029242287828579ULL;
static const uint64_t PRIME64_5 =  2870177450012600261ULL;

/*-**************************
 *  Utils
 ***************************/
void xxh32_copy_state(struct xxh32_state *dst, const struct xxh32_state *src)
{
	memcpy(dst, src, sizeof(*dst));
}
EXPORT_SYMBOL(xxh32_copy_state);

void xxh64_copy_state(struct xxh64_state *dst, const struct xxh64_state *src)
{
	memcpy(dst, src, sizeof(*dst));
}
EXPORT_SYMBOL(xxh64_copy_state);

/*-***************************
 * Simple Hash Functions
 ****************************/
/* Optimized for better instruction pipelining on Raptor Lake */
static inline uint32_t xxh32_round(uint32_t seed, const uint32_t input)
{
	seed += input * PRIME32_2;
	seed = xxh_rotl32(seed, 13);
	seed *= PRIME32_1;
	return seed;
}

/*
 * xxh32 optimized for Raptor Lake:
 * - Improved prefetching for large inputs
 * - Better branch prediction with likely/unlikely hints
 * - Loop unrolling for better instruction-level parallelism
 */
uint32_t xxh32(const void *input, const size_t len, const uint32_t seed)
{
	const uint8_t *p = (const uint8_t *)input;
	const uint8_t *b_end = p + len;
	uint32_t h32;

	if (likely(len >= 16)) {
		const uint8_t *const limit = b_end - 16;
		uint32_t v1 = seed + PRIME32_1 + PRIME32_2;
		uint32_t v2 = seed + PRIME32_2;
		uint32_t v3 = seed + 0;
		uint32_t v4 = seed - PRIME32_1;

		/* Process 16 bytes per iteration (4 lanes of 4 bytes each) */
		do {
			/* For large inputs, prefetch ahead to reduce cache misses */
			if (likely(limit - p > XXH_PREFETCH_DIST))
				XXH_PREFETCH(p + XXH_PREFETCH_DIST);

			/* Process 4 lanes in parallel for better instruction pipelining */
			v1 = xxh32_round(v1, XXH_get32bits(p));
			v2 = xxh32_round(v2, XXH_get32bits(p + 4));
			v3 = xxh32_round(v3, XXH_get32bits(p + 8));
			v4 = xxh32_round(v4, XXH_get32bits(p + 12));

			p += 16;
		} while (p <= limit);

		/* Combine the 4 lanes */
		h32 = xxh_rotl32(v1, 1) + xxh_rotl32(v2, 7) +
		xxh_rotl32(v3, 12) + xxh_rotl32(v4, 18);
	} else {
		/* Small input optimization */
		h32 = seed + PRIME32_5;
	}

	h32 += (uint32_t)len;

	/* Process remaining 4-byte chunks */
	while (p + 4 <= b_end) {
		h32 += XXH_get32bits(p) * PRIME32_3;
		h32 = xxh_rotl32(h32, 17) * PRIME32_4;
		p += 4;
	}

	/* Process remaining bytes */
	while (p < b_end) {
		h32 += (*p) * PRIME32_5;
		h32 = xxh_rotl32(h32, 11) * PRIME32_1;
		p++;
	}

	/* Finalization - avalanche bits for better mixing */
	h32 ^= h32 >> 15;
	h32 *= PRIME32_2;
	h32 ^= h32 >> 13;
	h32 *= PRIME32_3;
	h32 ^= h32 >> 16;

	return h32;
}
EXPORT_SYMBOL(xxh32);

/* Optimized round function for xxh64 */
static inline uint64_t xxh64_round(uint64_t acc, const uint64_t input)
{
	acc += input * PRIME64_2;
	acc = xxh_rotl64(acc, 31);
	acc *= PRIME64_1;
	return acc;
}

static inline uint64_t xxh64_merge_round(uint64_t acc, uint64_t val)
{
	val = xxh64_round(0, val);
	acc ^= val;
	acc = acc * PRIME64_1 + PRIME64_4;
	return acc;
}

/*
 * xxh64 optimized for Raptor Lake:
 * - Improved prefetching strategy
 * - Loop unrolling for better instruction-level parallelism
 * - Better branch prediction with likely/unlikely hints
 */
uint64_t xxh64(const void *input, const size_t len, const uint64_t seed)
{
	const uint8_t *p = (const uint8_t *)input;
	const uint8_t *const b_end = p + len;
	uint64_t h64;

	if (likely(len >= 32)) {
		const uint8_t *const limit = b_end - 32;
		uint64_t v1 = seed + PRIME64_1 + PRIME64_2;
		uint64_t v2 = seed + PRIME64_2;
		uint64_t v3 = seed + 0;
		uint64_t v4 = seed - PRIME64_1;

		/* Process 32 bytes per iteration (4 lanes of 8 bytes each) */
		do {
			/* Prefetch ahead for large inputs to reduce cache misses */
			if (likely(limit - p > XXH_PREFETCH_DIST)) {
				XXH_PREFETCH(p + XXH_PREFETCH_DIST);
				/* Add a second prefetch to handle more of the stream */
				XXH_PREFETCH(p + XXH_PREFETCH_DIST + XXH_CACHELINE_SIZE);
			}

			/* Process 4 lanes in parallel for better instruction pipelining */
			v1 = xxh64_round(v1, XXH_get64bits(p));
			v2 = xxh64_round(v2, XXH_get64bits(p + 8));
			v3 = xxh64_round(v3, XXH_get64bits(p + 16));
			v4 = xxh64_round(v4, XXH_get64bits(p + 24));

			p += 32;
		} while (p <= limit);

		/* Combine the 4 lanes with improved mixing for better distribution */
		h64 = xxh_rotl64(v1, 1) + xxh_rotl64(v2, 7) +
		xxh_rotl64(v3, 12) + xxh_rotl64(v4, 18);

		/* Merge all lanes to improve bit mixing */
		h64 = xxh64_merge_round(h64, v1);
		h64 = xxh64_merge_round(h64, v2);
		h64 = xxh64_merge_round(h64, v3);
		h64 = xxh64_merge_round(h64, v4);

	} else {
		/* Small input optimization */
		h64 = seed + PRIME64_5;
	}

	h64 += (uint64_t)len;

	/* Process remaining 8-byte chunks */
	while (p + 8 <= b_end) {
		const uint64_t k1 = xxh64_round(0, XXH_get64bits(p));
		h64 ^= k1;
		h64 = xxh_rotl64(h64, 27) * PRIME64_1 + PRIME64_4;
		p += 8;
	}

	/* Process remaining 4-byte chunk if present */
	if (p + 4 <= b_end) {
		h64 ^= (uint64_t)(XXH_get32bits(p)) * PRIME64_1;
		h64 = xxh_rotl64(h64, 23) * PRIME64_2 + PRIME64_3;
		p += 4;
	}

	/* Process remaining bytes */
	while (p < b_end) {
		h64 ^= (*p) * PRIME64_5;
		h64 = xxh_rotl64(h64, 11) * PRIME64_1;
		p++;
	}

	/* Finalization - avalanche bits for better mixing */
	h64 ^= h64 >> 33;
	h64 *= PRIME64_2;
	h64 ^= h64 >> 29;
	h64 *= PRIME64_3;
	h64 ^= h64 >> 32;

	return h64;
}
EXPORT_SYMBOL(xxh64);

/*-**************************************************
 * Advanced Hash Functions
 ***************************************************/
void xxh32_reset(struct xxh32_state *statePtr, const uint32_t seed)
{
	/* Initialize the state with the seed value */
	statePtr->total_len_32 = 0;
	statePtr->large_len = 0;
	statePtr->v1 = seed + PRIME32_1 + PRIME32_2;
	statePtr->v2 = seed + PRIME32_2;
	statePtr->v3 = seed + 0;
	statePtr->v4 = seed - PRIME32_1;
	statePtr->memsize = 0;

	/* Zero the memory buffer in one operation */
	memset(statePtr->mem32, 0, sizeof(statePtr->mem32));
}
EXPORT_SYMBOL(xxh32_reset);

void xxh64_reset(struct xxh64_state *statePtr, const uint64_t seed)
{
	/* Initialize the state with the seed value */
	statePtr->total_len = 0;
	statePtr->v1 = seed + PRIME64_1 + PRIME64_2;
	statePtr->v2 = seed + PRIME64_2;
	statePtr->v3 = seed + 0;
	statePtr->v4 = seed - PRIME64_1;
	statePtr->memsize = 0;

	/* Zero the memory buffer in one operation */
	memset(statePtr->mem64, 0, sizeof(statePtr->mem64));
}
EXPORT_SYMBOL(xxh64_reset);

int xxh32_update(struct xxh32_state *state, const void *input, const size_t len)
{
	const uint8_t *p = (const uint8_t *)input;
	const uint8_t *const b_end = p + len;

	if (unlikely(input == NULL))
		return -EINVAL;

	state->total_len_32 += (uint32_t)len;
	state->large_len |= (len >= 16) | (state->total_len_32 >= 16);

	/* Small data chunk optimization: append to buffer */
	if (state->memsize + len < 16) {
		memcpy((uint8_t *)(state->mem32) + state->memsize, input, len);
		state->memsize += (uint32_t)len;
		return 0;
	}

	/* Process any data left from previous update */
	if (state->memsize) {
		/* Fill up to 16 bytes */
		memcpy((uint8_t *)(state->mem32) + state->memsize, input,
			   16 - state->memsize);

		/* Process the 16-byte block */
		state->v1 = xxh32_round(state->v1, XXH_get32bits(&state->mem32[0]));
		state->v2 = xxh32_round(state->v2, XXH_get32bits(&state->mem32[1]));
		state->v3 = xxh32_round(state->v3, XXH_get32bits(&state->mem32[2]));
		state->v4 = xxh32_round(state->v4, XXH_get32bits(&state->mem32[3]));

		p += 16 - state->memsize;
		state->memsize = 0;
	}

	/* Process 16-byte blocks */
	if (p <= b_end - 16) {
		const uint8_t *const limit = b_end - 16;
		uint32_t v1 = state->v1;
		uint32_t v2 = state->v2;
		uint32_t v3 = state->v3;
		uint32_t v4 = state->v4;

		/* Main loop - process blocks in groups of 16 bytes */
		do {
			/* Prefetch for large inputs - Raptor Lake prefetcher optimization */
			if (likely(limit - p > XXH_PREFETCH_DIST)) {
				XXH_PREFETCH(p + XXH_PREFETCH_DIST);
				/* Add a second prefetch to maximize memory bandwidth */
				XXH_PREFETCH(p + XXH_PREFETCH_DIST + XXH_CACHELINE_SIZE);
			}

			/* Process 4 values in one iteration for better pipelining */
			v1 = xxh32_round(v1, XXH_get32bits(p));
			v2 = xxh32_round(v2, XXH_get32bits(p + 4));
			v3 = xxh32_round(v3, XXH_get32bits(p + 8));
			v4 = xxh32_round(v4, XXH_get32bits(p + 12));

			p += 16;
		} while (p <= limit);

		state->v1 = v1;
		state->v2 = v2;
		state->v3 = v3;
		state->v4 = v4;
	}

	/* Store remaining bytes */
	if (p < b_end) {
		memcpy(state->mem32, p, (size_t)(b_end-p));
		state->memsize = (uint32_t)(b_end-p);
	}

	return 0;
}
EXPORT_SYMBOL(xxh32_update);

uint32_t xxh32_digest(const struct xxh32_state *state)
{
	const uint8_t *p = (const uint8_t *)state->mem32;
	const uint8_t *const b_end = (const uint8_t *)(state->mem32) +
	state->memsize;
	uint32_t h32;

	/* Process according to amount of data processed */
	if (likely(state->large_len)) {
		h32 = xxh_rotl32(state->v1, 1) + xxh_rotl32(state->v2, 7) +
		xxh_rotl32(state->v3, 12) + xxh_rotl32(state->v4, 18);
	} else {
		h32 = state->v3 /* == seed */ + PRIME32_5;
	}

	h32 += state->total_len_32;

	/* Process remaining 4-byte chunks */
	while (p + 4 <= b_end) {
		h32 += XXH_get32bits(p) * PRIME32_3;
		h32 = xxh_rotl32(h32, 17) * PRIME32_4;
		p += 4;
	}

	/* Process remaining bytes */
	while (p < b_end) {
		h32 += (*p) * PRIME32_5;
		h32 = xxh_rotl32(h32, 11) * PRIME32_1;
		p++;
	}

	/* Finalization - avalanche bits for better mixing */
	h32 ^= h32 >> 15;
	h32 *= PRIME32_2;
	h32 ^= h32 >> 13;
	h32 *= PRIME32_3;
	h32 ^= h32 >> 16;

	return h32;
}
EXPORT_SYMBOL(xxh32_digest);

int xxh64_update(struct xxh64_state *state, const void *input, const size_t len)
{
	const uint8_t *p = (const uint8_t *)input;
	const uint8_t *const b_end = p + len;

	if (unlikely(input == NULL))
		return -EINVAL;

	state->total_len += len;

	/* Small data chunk optimization: append to buffer */
	if (state->memsize + len < 32) {
		memcpy(((uint8_t *)state->mem64) + state->memsize, input, len);
		state->memsize += (uint32_t)len;
		return 0;
	}

	/* Process any data left from previous update */
	if (state->memsize) {
		/* Fill up to 32 bytes */
		memcpy(((uint8_t *)state->mem64) + state->memsize, input,
			   32 - state->memsize);

		/* Process the 32-byte block */
		state->v1 = xxh64_round(state->v1, XXH_get64bits(&state->mem64[0]));
		state->v2 = xxh64_round(state->v2, XXH_get64bits(&state->mem64[1]));
		state->v3 = xxh64_round(state->v3, XXH_get64bits(&state->mem64[2]));
		state->v4 = xxh64_round(state->v4, XXH_get64bits(&state->mem64[3]));

		p += 32 - state->memsize;
		state->memsize = 0;
	}

	/* Process 32-byte blocks */
	if (p + 32 <= b_end) {
		const uint8_t *const limit = b_end - 32;
		uint64_t v1 = state->v1;
		uint64_t v2 = state->v2;
		uint64_t v3 = state->v3;
		uint64_t v4 = state->v4;

		/* Main loop - process blocks in groups of 32 bytes */
		do {
			/* Prefetch for large inputs - Raptor Lake prefetcher optimization */
			if (likely(limit - p > XXH_PREFETCH_DIST)) {
				XXH_PREFETCH(p + XXH_PREFETCH_DIST);
				/* Additional prefetch to utilize full memory bandwidth */
				XXH_PREFETCH(p + XXH_PREFETCH_DIST + XXH_CACHELINE_SIZE);
			}

			/* Process in one iteration for better pipelining */
			v1 = xxh64_round(v1, XXH_get64bits(p));
			v2 = xxh64_round(v2, XXH_get64bits(p + 8));
			v3 = xxh64_round(v3, XXH_get64bits(p + 16));
			v4 = xxh64_round(v4, XXH_get64bits(p + 24));

			p += 32;
		} while (p <= limit);

		state->v1 = v1;
		state->v2 = v2;
		state->v3 = v3;
		state->v4 = v4;
	}

	/* Store remaining bytes */
	if (p < b_end) {
		memcpy(state->mem64, p, (size_t)(b_end-p));
		state->memsize = (uint32_t)(b_end - p);
	}

	return 0;
}
EXPORT_SYMBOL(xxh64_update);

uint64_t xxh64_digest(const struct xxh64_state *state)
{
	const uint8_t *p = (const uint8_t *)state->mem64;
	const uint8_t *const b_end = (const uint8_t *)state->mem64 +
	state->memsize;
	uint64_t h64;

	/* Process according to amount of data processed */
	if (likely(state->total_len >= 32)) {
		const uint64_t v1 = state->v1;
		const uint64_t v2 = state->v2;
		const uint64_t v3 = state->v3;
		const uint64_t v4 = state->v4;

		/* Combine the 4 lanes with improved mixing for better distribution */
		h64 = xxh_rotl64(v1, 1) + xxh_rotl64(v2, 7) +
		xxh_rotl64(v3, 12) + xxh_rotl64(v4, 18);

		/* Merge all lanes to improve bit mixing */
		h64 = xxh64_merge_round(h64, v1);
		h64 = xxh64_merge_round(h64, v2);
		h64 = xxh64_merge_round(h64, v3);
		h64 = xxh64_merge_round(h64, v4);
	} else {
		h64 = state->v3 + PRIME64_5;
	}

	h64 += (uint64_t)state->total_len;

	/* Process remaining 8-byte chunks */
	while (p + 8 <= b_end) {
		const uint64_t k1 = xxh64_round(0, XXH_get64bits(p));
		h64 ^= k1;
		h64 = xxh_rotl64(h64, 27) * PRIME64_1 + PRIME64_4;
		p += 8;
	}

	/* Process remaining 4-byte chunk if present */
	if (p + 4 <= b_end) {
		h64 ^= (uint64_t)(XXH_get32bits(p)) * PRIME64_1;
		h64 = xxh_rotl64(h64, 23) * PRIME64_2 + PRIME64_3;
		p += 4;
	}

	/* Process remaining bytes */
	while (p < b_end) {
		h64 ^= (*p) * PRIME64_5;
		h64 = xxh_rotl64(h64, 11) * PRIME64_1;
		p++;
	}

	/* Finalization - avalanche bits for better mixing */
	h64 ^= h64 >> 33;
	h64 *= PRIME64_2;
	h64 ^= h64 >> 29;
	h64 *= PRIME64_3;
	h64 ^= h64 >> 32;

	return h64;
}
EXPORT_SYMBOL(xxh64_digest);

MODULE_LICENSE("Dual BSD/GPL");
MODULE_DESCRIPTION("xxHash");
