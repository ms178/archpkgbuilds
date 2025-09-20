/*
 * Copyright © 2014 Intel Corporation
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
#include <assert.h>
#include <stdint.h>
#include <limits.h>
#include <stdlib.h>

#include "blob.h"

#ifdef HAVE_VALGRIND
#include <valgrind.h>
#include <memcheck.h>
#define VG(x) x
#else
#define VG(x)
#endif

/* Portable feature detection */
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

/* Branch prediction hints */
#if __has_builtin(__builtin_expect)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
#endif

/* Prefetch hint */
#if __has_builtin(__builtin_prefetch)
#define PREFETCH_READ(addr, locality) __builtin_prefetch((addr), 0, (locality))
#else
#define PREFETCH_READ(addr, locality) ((void)0)
#endif

#define BLOB_INITIAL_SIZE 4096
#define CACHE_LINE_SIZE 64

/* Align x up to 'alignment' with overflow safety.
 * Treat alignment <= 1 as a no-op. Return false on overflow.
 */
static inline bool
align_up_safe(size_t x, size_t alignment, size_t *out)
{
   if (alignment <= 1) {
      *out = x;
      return true;
   }

   /* Power-of-two fast path */
   if ((alignment & (alignment - 1)) == 0) {
      const size_t mask = alignment - 1;
      if (UNLIKELY(x > SIZE_MAX - mask)) {
         return false;
      }
      *out = (x + mask) & ~mask;
      return true;
   }

   /* Generic path */
   const size_t rem = x % alignment;
   if (rem == 0) {
      *out = x;
      return true;
   }
   const size_t add = alignment - rem;
   if (UNLIKELY(x > SIZE_MAX - add)) {
      return false;
   }
   *out = x + add;
   return true;
}

/* Ensure that 'blob' can fit 'additional' more bytes. Grow by doubling.
 * Returns false on out-of-memory or overflow. Never calls realloc(..., 0).
 */
static bool
grow_to_fit(struct blob *blob, size_t additional)
{
   size_t required_size;
   size_t to_allocate;

   if (UNLIKELY(blob->out_of_memory)) {
      return false;
   }

   /* Overflow check for size + additional */
   if (UNLIKELY(additional > SIZE_MAX - blob->size)) {
      blob->out_of_memory = true;
      return false;
   }

   required_size = blob->size + additional;

   if (LIKELY(required_size <= blob->allocated)) {
      return true;
   }

   if (UNLIKELY(blob->fixed_allocation)) {
      blob->out_of_memory = true;
      return false;
   }

   /* Compute new allocation size: double, with overflow guard */
   if (blob->allocated == 0) {
      to_allocate = BLOB_INITIAL_SIZE;
   } else {
      if (blob->allocated >= SIZE_MAX / 2) {
         to_allocate = SIZE_MAX;
      } else {
         to_allocate = blob->allocated * 2;
      }
   }

   /* Ensure at least what's required */
   if (to_allocate < required_size) {
      to_allocate = required_size;
   }

   /* Optionally round to cache line; never reduce below required_size, never overflow */
   if (CACHE_LINE_SIZE > 1) {
      size_t rounded;
      if (align_up_safe(to_allocate, CACHE_LINE_SIZE, &rounded)) {
         if (rounded >= required_size) {
            to_allocate = rounded;
         }
      }
      /* If rounding failed or would reduce size, keep to_allocate as-is */
   }

   /* Never realloc with size 0; required_size > 0 if we got here */
   if (UNLIKELY(to_allocate == 0)) {
      blob->out_of_memory = true;
      return false;
   }

   uint8_t *new_data = (uint8_t *)realloc(blob->data, to_allocate);
   if (UNLIKELY(new_data == NULL)) {
      blob->out_of_memory = true;
      return false;
   }

   blob->data = new_data;
   blob->allocated = to_allocate;

   return true;
}

/* Internal: Reserve 'write_size' bytes aligned to 'alignment'.
 * - Inserts zero padding as needed for alignment.
 * - On success, sets *out_offset to the aligned write position and advances blob->size.
 */
static bool
writer_reserve_aligned(struct blob *blob, size_t alignment, size_t write_size, size_t *out_offset)
{
   size_t aligned_size;
   size_t pad;

   /* Compute aligned position for the write */
   if (!align_up_safe(blob->size, alignment, &aligned_size)) {
      blob->out_of_memory = true;
      return false;
   }

   pad = aligned_size - blob->size;

   /* Overflow check for total additional */
   if (UNLIKELY(write_size > SIZE_MAX - pad)) {
      blob->out_of_memory = true;
      return false;
   }

   if (!grow_to_fit(blob, pad + write_size)) {
      return false;
   }

   /* Zero any padding bytes to preserve previous blob_align semantics */
   if (pad > 0 && blob->data != NULL) {
      memset(blob->data + blob->size, 0, pad);
   }

   *out_offset = aligned_size;
   blob->size = aligned_size + write_size;
   return true;
}

/* Align the blob->size to a granularity of 'alignment' bytes.
 * Any padding bytes are zeroed.
 */
bool
blob_align(struct blob *blob, size_t alignment)
{
   size_t new_size;
   size_t pad;

   if (alignment <= 1) {
      return true;
   }

   if (!align_up_safe(blob->size, alignment, &new_size)) {
     blob->out_of_memory = true;
     return false;
   }

   if (blob->size < new_size) {
      pad = new_size - blob->size;
      if (!grow_to_fit(blob, pad)) {
         return false;
      }

      if (blob->data != NULL) {
         memset(blob->data + blob->size, 0, pad);
      }
      blob->size = new_size;
   }

   return true;
}

void
blob_reader_align(struct blob_reader *blob, size_t alignment)
{
   /* If we're already in overrun, or alignment is no-op, bail early. */
   if (blob->overrun || alignment <= 1) {
      return;
   }

   /* If data is NULL, reader cannot meaningfully align; mark overrun. */
   if (blob->data == NULL) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   /* Validate current within [data, end] before doing pointer math. */
   if (UNLIKELY(blob->current < blob->data || blob->current > blob->end)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   size_t off = (size_t)(blob->current - blob->data);
   size_t new_off;

   if (!align_up_safe(off, alignment, &new_off)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   /* If aligning would move past end, it's an overrun. */
   if (UNLIKELY(new_off > (size_t)(blob->end - blob->data))) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   blob->current = blob->data + new_off;
}

void
blob_init(struct blob *blob)
{
   blob->data = NULL;
   blob->allocated = 0;
   blob->size = 0;
   blob->fixed_allocation = false;
   blob->out_of_memory = false;
}

void
blob_init_fixed(struct blob *blob, void *data, size_t size)
{
   blob->data = (uint8_t *)data;
   blob->allocated = size;
   blob->size = 0;
   blob->fixed_allocation = true;
   blob->out_of_memory = false;
}

void
blob_finish_get_buffer(struct blob *blob, void **buffer, size_t *size)
{
   *buffer = blob->data;
   *size = blob->size;
   blob->data = NULL;

   /* Trim the buffer - but don't lose data on failure */
   if (*size > 0 && *buffer != NULL) {
      void *trimmed = realloc(*buffer, *size);
      if (trimmed != NULL) {
         *buffer = trimmed;
      }
   }
}

bool
blob_overwrite_bytes(struct blob *blob,
                     size_t offset,
                     const void *bytes,
                     size_t to_write)
{
   /* Validate overwrite region: offset + to_write <= size (with overflow safety) */
   if (UNLIKELY(offset > blob->size)) {
      return false;
   }
   if (UNLIKELY(to_write > blob->size - offset)) {
      return false;
   }

   VG(VALGRIND_CHECK_MEM_IS_DEFINED(bytes, to_write));

   if (blob->data != NULL && to_write > 0) {
      memcpy(blob->data + offset, bytes, to_write);
   }

   return true;
}

bool
blob_write_bytes(struct blob *blob, const void *bytes, size_t to_write)
{
   if (to_write == 0) {
      return true;
   }

   if (!grow_to_fit(blob, to_write)) {
      return false;
   }

   if (blob->data != NULL) {
      VG(VALGRIND_CHECK_MEM_IS_DEFINED(bytes, to_write));
      memcpy(blob->data + blob->size, bytes, to_write);
   }
   blob->size += to_write;

   return true;
}

intptr_t
blob_reserve_bytes(struct blob *blob, size_t to_write)
{
   intptr_t ret;

   /* Ensure the returned offset can fit in intptr_t */
   if (UNLIKELY(blob->size > (size_t)INTPTR_MAX ||
                to_write > (size_t)(INTPTR_MAX - blob->size))) {
      blob->out_of_memory = true;
      return -1;
   }

   if (!grow_to_fit(blob, to_write)) {
      return -1;
   }

   ret = (intptr_t)blob->size;
   blob->size += to_write;

   return ret;
}

intptr_t
blob_reserve_uint32(struct blob *blob)
{
   if (!blob_align(blob, sizeof(uint32_t))) {
      return -1;
   }
   return blob_reserve_bytes(blob, sizeof(uint32_t));
}

intptr_t
blob_reserve_intptr(struct blob *blob)
{
   if (!blob_align(blob, sizeof(intptr_t))) {
      return -1;
   }
   return blob_reserve_bytes(blob, sizeof(intptr_t));
}

/* Optimized typed writes: fused align + grow + zero-pad + write. */
#define BLOB_WRITE_TYPE(name, type)                                    \
bool                                                                   \
name(struct blob *blob, type value)                                    \
{                                                                      \
   size_t off;                                                         \
   if (!writer_reserve_aligned(blob, sizeof(type), sizeof(type), &off)) { \
      return false;                                                    \
   }                                                                   \
   if (blob->data != NULL) {                                           \
      memcpy(blob->data + off, &value, sizeof(type));                  \
   }                                                                   \
   return true;                                                        \
}

BLOB_WRITE_TYPE(blob_write_uint8, uint8_t)
BLOB_WRITE_TYPE(blob_write_uint16, uint16_t)
BLOB_WRITE_TYPE(blob_write_uint32, uint32_t)
BLOB_WRITE_TYPE(blob_write_uint64, uint64_t)
BLOB_WRITE_TYPE(blob_write_intptr, intptr_t)

#define ASSERT_ALIGNED(_offset, _align)                                \
   do {                                                                \
      size_t __aligned;                                                \
      assert(align_up_safe((_offset), (_align), &__aligned) &&         \
             __aligned == (_offset));                                  \
   } while (0)

bool
blob_overwrite_uint8(struct blob *blob,
                     size_t offset,
                     uint8_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

bool
blob_overwrite_uint32(struct blob *blob,
                      size_t offset,
                      uint32_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

bool
blob_overwrite_intptr(struct blob *blob,
                      size_t offset,
                      intptr_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

bool
blob_write_string(struct blob *blob, const char *str)
{
   /* str must be non-NULL; enforce via assert in debug builds. */
   assert(str != NULL);

   /* Include NUL terminator; detect overflow if strlen returns SIZE_MAX */
   size_t len = strlen(str);
   if (UNLIKELY(len == SIZE_MAX)) {
      blob->out_of_memory = true;
      return false;
   }
   len += 1;

   if (!grow_to_fit(blob, len)) {
      return false;
   }

   if (blob->data != NULL) {
      VG(VALGRIND_CHECK_MEM_IS_DEFINED(str, len));
      memcpy(blob->data + blob->size, str, len);
   }
   blob->size += len;

   return true;
}

void
blob_reader_init(struct blob_reader *blob, const void *data, size_t size)
{
   blob->data = (const uint8_t *)data;

   /* If size==0, it's valid to have data==NULL; start/end/current equal. */
   if (size == 0) {
      blob->end = blob->data;
      blob->current = blob->data;
      blob->overrun = false;
      return;
   }

   /* If size>0 but data==NULL, this is invalid input; fail safely. */
   if (blob->data == NULL) {
      blob->end = (const uint8_t *)NULL;
      blob->current = (const uint8_t *)NULL;
      blob->overrun = true;
      return;
   }

   blob->end = blob->data + size;
   blob->current = blob->data;
   blob->overrun = false;

   /* Prefetch first cache lines for sequential read pattern, safely. */
   PREFETCH_READ(blob->current, 3);
   if (size > CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);
   }
   if (size > 2 * CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + 2 * CACHE_LINE_SIZE, 1);
   }
}

/* Check that 'size' bytes can be read from current without overrun.
 * Sets blob->overrun on failure and never performs invalid pointer arithmetic.
 */
static bool
ensure_can_read(struct blob_reader *blob, size_t size)
{
   if (UNLIKELY(blob->overrun)) {
      return false;
   }

   /* Special-case zero-length reads: always okay if within bounds. */
   if (size == 0) {
      /* If data is NULL with size==0 (init path), remain okay. For size>0, we
       * would have set overrun in init. Just require current and end to be sane. */
      return true;
   }

   /* If data is NULL here, we cannot read any bytes safely. */
   if (UNLIKELY(blob->data == NULL)) {
      blob->overrun = true;
      return false;
   }

   /* Validate current within [data, end] before subtraction. */
   if (UNLIKELY(blob->current < blob->data || blob->current > blob->end)) {
      blob->overrun = true;
      return false;
   }

   const size_t remaining = (size_t)(blob->end - blob->current);
   if (LIKELY(remaining >= size)) {
      return true;
   }

   blob->overrun = true;
   return false;
}

const void *
blob_read_bytes(struct blob_reader *blob, size_t size)
{
   const void *ret;

   if (!ensure_can_read(blob, size)) {
      return NULL;
   }

   ret = blob->current;
   blob->current += size;

   /* Prefetch ahead without creating invalid pointers */
   size_t remain = (size_t)(blob->end - blob->current);
   if (remain > CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);
   }

   return ret;
}

void
blob_copy_bytes(struct blob_reader *blob, void *dest, size_t size)
{
   const void *bytes = blob_read_bytes(blob, size);
   if (bytes == NULL) {
      return;
   }

   if (size > 0) {
      memcpy(dest, bytes, size);
   }
}

void
blob_skip_bytes(struct blob_reader *blob, size_t size)
{
   if (ensure_can_read(blob, size)) {
      blob->current += size;
   }
}

/* Optimized typed reads with safe alignment and bounds checks. */
#define BLOB_READ_TYPE(name, type)                                     \
type                                                                   \
name(struct blob_reader *blob)                                         \
{                                                                      \
   type ret = (type)0;                                                 \
   blob_reader_align(blob, sizeof(type));                              \
   if (ensure_can_read(blob, sizeof(type))) {                          \
      memcpy(&ret, blob->current, sizeof(type));                       \
      blob->current += sizeof(type);                                   \
      size_t remain = (size_t)(blob->end - blob->current);             \
      if (remain > CACHE_LINE_SIZE) {                                  \
         PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);            \
      }                                                                \
   }                                                                   \
   return ret;                                                         \
}

BLOB_READ_TYPE(blob_read_uint8, uint8_t)
BLOB_READ_TYPE(blob_read_uint16, uint16_t)
BLOB_READ_TYPE(blob_read_uint32, uint32_t)
BLOB_READ_TYPE(blob_read_uint64, uint64_t)
BLOB_READ_TYPE(blob_read_intptr, intptr_t)

char *
blob_read_string(struct blob_reader *blob)
{
   /* If we're already at the end, then this is an overrun. */
   if (UNLIKELY(blob->current >= blob->end)) {
      blob->overrun = true;
      return NULL;
   }

   /* data==NULL implies invalid reader; fail safely */
   if (UNLIKELY(blob->data == NULL)) {
      blob->overrun = true;
      return NULL;
   }

   /* Search for NUL within remaining bytes */
   size_t remain = (size_t)(blob->end - blob->current);
   const uint8_t *nul = (const uint8_t *)memchr(blob->current, 0, remain);

   if (UNLIKELY(nul == NULL)) {
      blob->overrun = true;
      return NULL;
   }

   size_t size = (size_t)(nul - blob->current) + 1;

   /* We already know we can read this much */
   char *ret = (char *)blob->current;
   blob->current += size;

   return ret;
}
