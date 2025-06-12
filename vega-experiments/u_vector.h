/*
 * Copyright © 2015 Intel Corporation
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

#ifndef U_VECTOR_H
#define U_VECTOR_H

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "util/macros.h"
#include "util/u_math.h"

#ifdef __cplusplus
extern "C" {
        #endif

        /* ------------------------------------------------------------------ */
        /* Data structure (unchanged ABI)                                     */
        /* ------------------------------------------------------------------ */
        struct u_vector {
                uint32_t head;          /* first free slot (push position)   */
                uint32_t tail;          /* first live element (pop position) */
                uint32_t element_size;  /* element size (power-of-two)       */
                uint32_t size;          /* capacity in bytes (power-of-two)  */
                void    *data;
        };

        /* ------------------------------------------------------------------ */
        /* Initialisation                                                     */
        /* ------------------------------------------------------------------ */
        int  u_vector_init      (struct u_vector *q,
                                 uint32_t        initial_elems,
                                 uint32_t        element_size);

        int  u_vector_init_pow2 (struct u_vector *q,
                                 uint32_t        initial_elems,
                                 uint32_t        element_size);

        /* ------------------------------------------------------------------ */
        /* Push / Pop                                                         */
        /* ------------------------------------------------------------------ */
        void *u_vector_add   (struct u_vector *q);   /* push, returns new slot */
        void *u_vector_remove(struct u_vector *q);   /* pop,  returns oldest   */

        /* ------------------------------------------------------------------ */
        /* Helpers                                                            */
        /* ------------------------------------------------------------------ */
        static inline uint32_t
        u_vector_length(const struct u_vector *q)
        {
                #if defined(__GNUC__) || defined(__clang__)
                return ((q->head - q->tail) >> __builtin_ctz(q->element_size));
                #else
                return (q->head - q->tail) / q->element_size;
                #endif
        }

        static inline int
        u_vector_is_empty(const struct u_vector *q)
        {
                return q->head == q->tail;
        }

        static inline void *
        u_vector_head(struct u_vector *q)   /* newest element */
        {
                assert(!u_vector_is_empty(q));
                return (void *)((char *)q->data +
                (((q->head - q->element_size) & (q->size - 1))));
        }

        static inline void *
        u_vector_tail(struct u_vector *q)   /* oldest element */
        {
                assert(!u_vector_is_empty(q));
                return (void *)((char *)q->data + (q->tail & (q->size - 1)));
        }

        static inline void
        u_vector_finish(struct u_vector *q)
        {
                free(q->data);
                memset(q, 0, sizeof(*q));
        }

        /* ------------------------------------------------------------------ */
        /* foreach macro – legacy 2-param signature                           */
        /* ------------------------------------------------------------------ */
        #ifdef __cplusplus
        #  define _u_vec_cast(e) (decltype(e))
        #else
        #  define _u_vec_cast(e) (void *)
        #endif

        #define u_vector_foreach(elem, q)                                           \
        STATIC_ASSERT(__builtin_types_compatible_p(__typeof__(q), struct u_vector *)); \
        for (uint32_t __u_vec_off = (q)->tail, __u_vec_end = (q)->head;          \
                __u_vec_off != __u_vec_end &&                                       \
                ((elem) = _u_vec_cast(elem)((char *)(q)->data +                     \
                (__u_vec_off & ((q)->size - 1))), true);                 \
                __u_vec_off += (q)->element_size)

                /* ------------------------------------------------------------------ */
                #ifdef __cplusplus
}  /* extern "C" */
#endif
#endif /* U_VECTOR_H */
