// SPDX-License-Identifier: BSD-2-Clause OR GPL-2.0-only
/*
 * xxHash - Extremely Fast Hash algorithm
 *
 * Optimized scalar implementation for Linux kernel use on 64-bit little-endian
 * systems. This file intentionally keeps the public API and ABI unchanged.
 */

#include <linux/bitops.h>
#include <linux/compiler.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/string.h>
#include <linux/types.h>
#include <linux/unaligned.h>
#include <linux/xxhash.h>

/*
 * Keep forward declarations local as a fallback for trees where xxhash.h lacks
 * some streaming prototypes; this also silences -Wmissing-prototypes cleanly.
 */
void xxh32_copy_state(struct xxh32_state *dst, const struct xxh32_state *src);
void xxh64_copy_state(struct xxh64_state *dst, const struct xxh64_state *src);
uint32_t xxh32(const void *input, size_t len, uint32_t seed);
uint64_t xxh64(const void *input, size_t len, uint64_t seed);
void xxh32_reset(struct xxh32_state *state, uint32_t seed);
void xxh64_reset(struct xxh64_state *state, uint64_t seed);
int xxh32_update(struct xxh32_state *state, const void *input, size_t len);
uint32_t xxh32_digest(const struct xxh32_state *state);
int xxh64_update(struct xxh64_state *state, const void *input, size_t len);
uint64_t xxh64_digest(const struct xxh64_state *state);

#define XXH_PRIME32_1 0x9E3779B1U
#define XXH_PRIME32_2 0x85EBCA77U
#define XXH_PRIME32_3 0xC2B2AE3DU
#define XXH_PRIME32_4 0x27D4EB2FU
#define XXH_PRIME32_5 0x165667B1U

#define XXH_PRIME64_1 0x9E3779B185EBCA87ULL
#define XXH_PRIME64_2 0xC2B2AE3D27D4EB4FULL
#define XXH_PRIME64_3 0x165667B19E3779F9ULL
#define XXH_PRIME64_4 0x85EBCA77C2B2AE63ULL
#define XXH_PRIME64_5 0x27D4EB2F165667C5ULL

static __always_inline uint32_t xxh32_round(uint32_t acc, uint32_t input)
{
	acc += input * XXH_PRIME32_2;
	acc = rol32(acc, 13);
	acc *= XXH_PRIME32_1;
	return acc;
}

static __always_inline uint64_t xxh64_round(uint64_t acc, uint64_t input)
{
	acc += input * XXH_PRIME64_2;
	acc = rol64(acc, 31);
	acc *= XXH_PRIME64_1;
	return acc;
}

static __always_inline uint64_t xxh64_merge_round(uint64_t acc, uint64_t val)
{
	val = xxh64_round(0, val);
	acc ^= val;
	acc = acc * XXH_PRIME64_1 + XXH_PRIME64_4;
	return acc;
}

static __always_inline uint32_t xxh32_avalanche(uint32_t h32)
{
	h32 ^= h32 >> 15;
	h32 *= XXH_PRIME32_2;
	h32 ^= h32 >> 13;
	h32 *= XXH_PRIME32_3;
	h32 ^= h32 >> 16;
	return h32;
}

static __always_inline uint64_t xxh64_avalanche(uint64_t h64)
{
	h64 ^= h64 >> 33;
	h64 *= XXH_PRIME64_2;
	h64 ^= h64 >> 29;
	h64 *= XXH_PRIME64_3;
	h64 ^= h64 >> 32;
	return h64;
}

static __always_inline uint32_t xxh32_finalize(uint32_t h32, const uint8_t *p,
					       size_t len)
{
	while (len >= 4) {
		h32 += get_unaligned_le32(p) * XXH_PRIME32_3;
		h32 = rol32(h32, 17) * XXH_PRIME32_4;
		p += 4;
		len -= 4;
	}

	switch (len) {
	case 3:
		h32 += (uint32_t)(*p++) * XXH_PRIME32_5;
		h32 = rol32(h32, 11) * XXH_PRIME32_1;
		fallthrough;
	case 2:
		h32 += (uint32_t)(*p++) * XXH_PRIME32_5;
		h32 = rol32(h32, 11) * XXH_PRIME32_1;
		fallthrough;
	case 1:
		h32 += (uint32_t)(*p) * XXH_PRIME32_5;
		h32 = rol32(h32, 11) * XXH_PRIME32_1;
		break;
	default:
		break;
	}

	return xxh32_avalanche(h32);
}

static __always_inline uint64_t xxh64_finalize(uint64_t h64, const uint8_t *p,
					       size_t len)
{
	while (len >= 8) {
		const uint64_t k1 = xxh64_round(0, get_unaligned_le64(p));

		h64 ^= k1;
		h64 = rol64(h64, 27) * XXH_PRIME64_1 + XXH_PRIME64_4;
		p += 8;
		len -= 8;
	}

	if (len >= 4) {
		h64 ^= (uint64_t)get_unaligned_le32(p) * XXH_PRIME64_1;
		h64 = rol64(h64, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
		p += 4;
		len -= 4;
	}

	switch (len) {
	case 3:
		h64 ^= (uint64_t)(*p++) * XXH_PRIME64_5;
		h64 = rol64(h64, 11) * XXH_PRIME64_1;
		fallthrough;
	case 2:
		h64 ^= (uint64_t)(*p++) * XXH_PRIME64_5;
		h64 = rol64(h64, 11) * XXH_PRIME64_1;
		fallthrough;
	case 1:
		h64 ^= (uint64_t)(*p) * XXH_PRIME64_5;
		h64 = rol64(h64, 11) * XXH_PRIME64_1;
		break;
	default:
		break;
	}

	return xxh64_avalanche(h64);
}

void xxh32_copy_state(struct xxh32_state *dst, const struct xxh32_state *src)
{
	*dst = *src;
}
EXPORT_SYMBOL(xxh32_copy_state);

void xxh64_copy_state(struct xxh64_state *dst, const struct xxh64_state *src)
{
	*dst = *src;
}
EXPORT_SYMBOL(xxh64_copy_state);

uint32_t xxh32(const void *input, const size_t len, const uint32_t seed)
{
	const uint8_t *p = (const uint8_t *)input;
	uint32_t h32;

	if (!len)
		return xxh32_avalanche(seed + XXH_PRIME32_5);

	if (len >= 16) {
		const uint8_t *const b_end = p + len;
		const uint8_t *const limit = b_end - 16;
		uint32_t v1 = seed + XXH_PRIME32_1 + XXH_PRIME32_2;
		uint32_t v2 = seed + XXH_PRIME32_2;
		uint32_t v3 = seed;
		uint32_t v4 = seed - XXH_PRIME32_1;

		do {
			v1 = xxh32_round(v1, get_unaligned_le32(p));
			p += 4;
			v2 = xxh32_round(v2, get_unaligned_le32(p));
			p += 4;
			v3 = xxh32_round(v3, get_unaligned_le32(p));
			p += 4;
			v4 = xxh32_round(v4, get_unaligned_le32(p));
			p += 4;
		} while (p <= limit);

		h32 = rol32(v1, 1) + rol32(v2, 7) +
		      rol32(v3, 12) + rol32(v4, 18);
		return xxh32_finalize(h32 + (uint32_t)len, p,
				      (size_t)(b_end - p));
	}

	h32 = seed + XXH_PRIME32_5 + (uint32_t)len;
	return xxh32_finalize(h32, p, len);
}
EXPORT_SYMBOL(xxh32);

uint64_t xxh64(const void *input, const size_t len, const uint64_t seed)
{
	const uint8_t *p = (const uint8_t *)input;
	uint64_t h64;

	if (!len)
		return xxh64_avalanche(seed + XXH_PRIME64_5);

	if (len >= 32) {
		const uint8_t *const b_end = p + len;
		const uint8_t *const limit = b_end - 32;
		uint64_t v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2;
		uint64_t v2 = seed + XXH_PRIME64_2;
		uint64_t v3 = seed;
		uint64_t v4 = seed - XXH_PRIME64_1;

		while (p + 64 <= b_end) {
			v1 = xxh64_round(v1, get_unaligned_le64(p));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 8));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 16));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 24));
			v1 = xxh64_round(v1, get_unaligned_le64(p + 32));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 40));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 48));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 56));
			p += 64;
		}

		if (p <= limit) {
			v1 = xxh64_round(v1, get_unaligned_le64(p));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 8));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 16));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 24));
			p += 32;
		}

		h64 = rol64(v1, 1) + rol64(v2, 7) +
		      rol64(v3, 12) + rol64(v4, 18);
		h64 = xxh64_merge_round(h64, v1);
		h64 = xxh64_merge_round(h64, v2);
		h64 = xxh64_merge_round(h64, v3);
		h64 = xxh64_merge_round(h64, v4);
		return xxh64_finalize(h64 + (uint64_t)len, p,
				      (size_t)(b_end - p));
	}

	h64 = seed + XXH_PRIME64_5 + (uint64_t)len;
	return xxh64_finalize(h64, p, len);
}
EXPORT_SYMBOL(xxh64);

void xxh32_reset(struct xxh32_state *state, const uint32_t seed)
{
	memset(state, 0, sizeof(*state));
	state->v1 = seed + XXH_PRIME32_1 + XXH_PRIME32_2;
	state->v2 = seed + XXH_PRIME32_2;
	state->v3 = seed;
	state->v4 = seed - XXH_PRIME32_1;
}
EXPORT_SYMBOL(xxh32_reset);

void xxh64_reset(struct xxh64_state *state, const uint64_t seed)
{
	memset(state, 0, sizeof(*state));
	state->v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2;
	state->v2 = seed + XXH_PRIME64_2;
	state->v3 = seed;
	state->v4 = seed - XXH_PRIME64_1;
}
EXPORT_SYMBOL(xxh64_reset);

int xxh32_update(struct xxh32_state *state, const void *input, const size_t len)
{
	const uint8_t *p;
	const uint8_t *b_end;
	uint32_t v1, v2, v3, v4;

	if (unlikely(input == NULL))
		return -EINVAL;
	if (!len)
		return 0;

	p = (const uint8_t *)input;
	b_end = p + len;
	state->total_len_32 += (uint32_t)len;

	if (state->memsize + len < 16) {
		memcpy((uint8_t *)state->mem32 + state->memsize, input, len);
		state->memsize += (uint32_t)len;
		return 0;
	}

	v1 = state->v1;
	v2 = state->v2;
	v3 = state->v3;
	v4 = state->v4;

	if (state->memsize) {
		const uint32_t fill = 16 - state->memsize;

		memcpy((uint8_t *)state->mem32 + state->memsize, input, fill);
		v1 = xxh32_round(v1, get_unaligned_le32(state->mem32));
		v2 = xxh32_round(v2, get_unaligned_le32(state->mem32 + 1));
		v3 = xxh32_round(v3, get_unaligned_le32(state->mem32 + 2));
		v4 = xxh32_round(v4, get_unaligned_le32(state->mem32 + 3));
		p += fill;
		state->memsize = 0;
		state->large_len = 1;
	}

	if (p + 16 <= b_end) {
		const uint8_t *const limit = b_end - 16;

		state->large_len = 1;
		do {
			v1 = xxh32_round(v1, get_unaligned_le32(p));
			p += 4;
			v2 = xxh32_round(v2, get_unaligned_le32(p));
			p += 4;
			v3 = xxh32_round(v3, get_unaligned_le32(p));
			p += 4;
			v4 = xxh32_round(v4, get_unaligned_le32(p));
			p += 4;
		} while (p <= limit);
	}

	state->v1 = v1;
	state->v2 = v2;
	state->v3 = v3;
	state->v4 = v4;

	if (p < b_end) {
		memcpy(state->mem32, p, (size_t)(b_end - p));
		state->memsize = (uint32_t)(b_end - p);
	}

	return 0;
}
EXPORT_SYMBOL(xxh32_update);

uint32_t xxh32_digest(const struct xxh32_state *state)
{
	const uint8_t *p = (const uint8_t *)state->mem32;
	uint32_t h32;

	if (state->large_len) {
		h32 = rol32(state->v1, 1) + rol32(state->v2, 7) +
		      rol32(state->v3, 12) + rol32(state->v4, 18);
	} else {
		h32 = state->v3 + XXH_PRIME32_5;
	}

	h32 += state->total_len_32;
	return xxh32_finalize(h32, p, state->memsize);
}
EXPORT_SYMBOL(xxh32_digest);

int xxh64_update(struct xxh64_state *state, const void *input, const size_t len)
{
	const uint8_t *p;
	const uint8_t *b_end;
	uint64_t v1, v2, v3, v4;

	if (unlikely(input == NULL))
		return -EINVAL;
	if (!len)
		return 0;

	p = (const uint8_t *)input;
	b_end = p + len;
	state->total_len += len;

	if (state->memsize + len < 32) {
		memcpy((uint8_t *)state->mem64 + state->memsize, input, len);
		state->memsize += (uint32_t)len;
		return 0;
	}

	v1 = state->v1;
	v2 = state->v2;
	v3 = state->v3;
	v4 = state->v4;

	if (state->memsize) {
		const uint32_t fill = 32 - state->memsize;

		memcpy((uint8_t *)state->mem64 + state->memsize, input, fill);
		v1 = xxh64_round(v1, get_unaligned_le64(state->mem64));
		v2 = xxh64_round(v2, get_unaligned_le64(state->mem64 + 1));
		v3 = xxh64_round(v3, get_unaligned_le64(state->mem64 + 2));
		v4 = xxh64_round(v4, get_unaligned_le64(state->mem64 + 3));
		p += fill;
		state->memsize = 0;
	}

	if (p + 32 <= b_end) {
		const uint8_t *const limit = b_end - 32;

		while (p + 64 <= b_end) {
			v1 = xxh64_round(v1, get_unaligned_le64(p));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 8));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 16));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 24));
			v1 = xxh64_round(v1, get_unaligned_le64(p + 32));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 40));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 48));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 56));
			p += 64;
		}

		if (p <= limit) {
			v1 = xxh64_round(v1, get_unaligned_le64(p));
			v2 = xxh64_round(v2, get_unaligned_le64(p + 8));
			v3 = xxh64_round(v3, get_unaligned_le64(p + 16));
			v4 = xxh64_round(v4, get_unaligned_le64(p + 24));
			p += 32;
		}
	}

	state->v1 = v1;
	state->v2 = v2;
	state->v3 = v3;
	state->v4 = v4;

	if (p < b_end) {
		memcpy(state->mem64, p, (size_t)(b_end - p));
		state->memsize = (uint32_t)(b_end - p);
	}

	return 0;
}
EXPORT_SYMBOL(xxh64_update);

uint64_t xxh64_digest(const struct xxh64_state *state)
{
	const uint8_t *p = (const uint8_t *)state->mem64;
	uint64_t h64;

	if (state->total_len >= 32) {
		h64 = rol64(state->v1, 1) + rol64(state->v2, 7) +
		      rol64(state->v3, 12) + rol64(state->v4, 18);
		h64 = xxh64_merge_round(h64, state->v1);
		h64 = xxh64_merge_round(h64, state->v2);
		h64 = xxh64_merge_round(h64, state->v3);
		h64 = xxh64_merge_round(h64, state->v4);
	} else {
		h64 = state->v3 + XXH_PRIME64_5;
	}

	h64 += state->total_len;
	return xxh64_finalize(h64, p, state->memsize);
}
EXPORT_SYMBOL(xxh64_digest);

MODULE_LICENSE("Dual BSD/GPL");
MODULE_DESCRIPTION("xxHash - Extremely Fast Hash algorithm");
MODULE_AUTHOR("Yann Collet");
