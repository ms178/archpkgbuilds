/*
 * Copyright © 2023 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <string.h>
#include <stdbool.h>
#include <limits.h>

#include "util/u_vector.h"
#include "util/macros.h"
#include "util/u_math.h"

/* ---------------------------------------------------------------------- */
/* Tunables                                                               */
/* ---------------------------------------------------------------------- */
#ifndef U_VECTOR_GROWTH_NUM   /* geometric grow factor NUM / DEN          */
#define U_VECTOR_GROWTH_NUM 2u
#endif
#ifndef U_VECTOR_GROWTH_DEN
#define U_VECTOR_GROWTH_DEN 1u
#endif

#ifndef likely
#  define likely(x)   __builtin_expect(!!(x), 1)
#  define unlikely(x) __builtin_expect(!!(x), 0)
#endif

/* ---------------------------------------------------------------------- */
/* Portable helpers                                                       */
/* ---------------------------------------------------------------------- */
static inline uint64_t
next_pow2_u64(uint64_t x)
{
    if (x <= 1)
        return 1;

    #if defined(__GNUC__) || defined(__clang__)
    return 1ull << (64 - __builtin_clzll(x - 1));
    #elif defined(_MSC_VER) && defined(_M_X64)
    unsigned long idx;
    _BitScanReverse64(&idx, (unsigned long long)(x - 1));
    return 1ull << (idx + 1);
    #else
    /* generic SW fallback (branchless) */
    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    x |= x >> 32;
    return x + 1;
    #endif
}

/* Return next capacity in BYTES that is:
 *   • strictly larger than cur_cap,
 *   • a power-of-two,
 *   • a multiple of elem_sz.
 * Return 0 on overflow. */
static uint32_t
next_capacity(uint32_t cur_cap, uint32_t elem_sz)
{
    uint64_t target = (uint64_t)cur_cap * U_VECTOR_GROWTH_NUM / U_VECTOR_GROWTH_DEN;
    if (target <= cur_cap)
        target = (uint64_t)cur_cap + elem_sz;

    target = next_pow2_u64(target);

    /* Round up to multiple of elem_sz */
    uint64_t rem = target & (elem_sz - 1u);
    if (rem)
        target += elem_sz - rem;

    /* Ensure still power-of-two (elem_sz may not be) */
    while (target & (target - 1))
        target <<= 1;

    if (target > UINT32_MAX)
        return 0;

    return (uint32_t)target;
}

/* ---------------------------------------------------------------------- */
/* Internal grow                                                          */
/* ---------------------------------------------------------------------- */
static bool
u_vector_grow(struct u_vector *q)
{
    const uint32_t old_cap     = q->size;
    const uint32_t live_elems  = u_vector_length(q);
    const uint32_t live_bytes  = live_elems * q->element_size;

    const uint32_t new_cap = next_capacity(old_cap, q->element_size);
    if (unlikely(new_cap == 0))
        return false; /* overflow or OOM */

        /* Fast path: tail is at offset 0 => memory is already linear. */
        if ((q->tail & (old_cap - 1u)) == 0u) {
            void *ptr = realloc(q->data, new_cap);
            if (!ptr)
                return false;
            q->data = ptr;
            q->size = new_cap;
            return true;
        }

        /* Slow path: wrapped buffer => two-part copy */
        void *new_data = malloc(new_cap);
        if (!new_data)
            return false;

    const uint32_t tail_off = q->tail & (old_cap - 1u);
    const uint32_t first    = MIN2(old_cap - tail_off, live_bytes);
    const uint32_t second   = live_bytes - first;

    /* Sanity: first + second == live_bytes */
    assert(first + second == live_bytes);

    memcpy(new_data,                       (char *)q->data + tail_off, first);
    if (second)
        memcpy((char *)new_data + first,    q->data,                     second);

    free(q->data);
    q->data = new_data;
    q->size = new_cap;
    q->tail = 0;
    q->head = live_bytes;  /* contiguous payload from offset 0 */
    return true;
}

/* ---------------------------------------------------------------------- */
/* Public API                                                             */
/* ---------------------------------------------------------------------- */
int
u_vector_init(struct u_vector *q,
              uint32_t         initial_elems,
              uint32_t         elem_sz)
{
    if (unlikely(!q) || unlikely(elem_sz == 0))
        return 0;

    elem_sz       = util_next_power_of_two(elem_sz);
    initial_elems = initial_elems ? util_next_power_of_two(initial_elems) : 16;

    uint64_t cap_bytes = (uint64_t)elem_sz * initial_elems;
    if (cap_bytes > UINT32_MAX)
        return 0;

    q->data = calloc(1, (size_t)cap_bytes);
    if (!q->data)
        return 0;

    q->head = q->tail = 0;
    q->element_size = elem_sz;
    q->size         = (uint32_t)cap_bytes;
    return 1;
}

int
u_vector_init_pow2(struct u_vector *q,
                   uint32_t         cnt,
                   uint32_t         sz)
{
    return u_vector_init(q, cnt, sz);
}

/* ---------------------------------------------------------------------- */
/* Hot-path push / pop                                                    */
/* ---------------------------------------------------------------------- */
ALWAYS_INLINE void *
u_vector_add(struct u_vector *q)
{
    if (unlikely((q->head - q->tail) == q->size))
        if (unlikely(!u_vector_grow(q)))
            return NULL; /* OOM */

            const uint32_t off = q->head & (q->size - 1u);
        q->head += q->element_size;

    #if defined(__GNUC__) || defined(__clang__)
    __builtin_prefetch((char *)q->data +
    (q->head & (q->size - 1u)), 1, 3);
    #endif
    return (char *)q->data + off;
}

ALWAYS_INLINE void *
u_vector_remove(struct u_vector *q)
{
    if (unlikely(q->head == q->tail))
        return NULL; /* empty */

        const uint32_t off = q->tail & (q->size - 1u);
    q->tail += q->element_size;
    return (char *)q->data + off;
}
