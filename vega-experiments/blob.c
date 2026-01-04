/*
 * Copyright © 2016 Intel Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * SPDX-License-Identifier: MIT
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
#define VG(x) ((void)0)
#endif

/*
 * Portable feature detection macros.
 */
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

/*
 * Branch prediction hints - critical for performance in hot paths.
 * These help the CPU's branch predictor and compiler's code layout.
 */
#if __has_builtin(__builtin_expect)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
#endif

/*
 * Prefetch hints for sequential read patterns.
 * Locality: 0=non-temporal, 1=low, 2=moderate, 3=high (keep in all caches).
 */
#if __has_builtin(__builtin_prefetch)
#define PREFETCH_READ(addr, locality) __builtin_prefetch((addr), 0, (locality))
#else
#define PREFETCH_READ(addr, locality) ((void)0)
#endif

/*
 * Warn if return value is ignored - critical for error handling.
 */
#if __has_attribute(warn_unused_result)
#define MUST_CHECK __attribute__((warn_unused_result))
#else
#define MUST_CHECK
#endif

/*
 * Mark functions as pure (no side effects, result depends only on args).
 */
#if __has_attribute(pure)
#define ATTRIBUTE_PURE __attribute__((pure))
#else
#define ATTRIBUTE_PURE
#endif

/*
 * Configuration constants.
 * BLOB_INITIAL_SIZE: Starting allocation for dynamic blobs (4KB = typical page size).
 * CACHE_LINE_SIZE: x86-64 cache line size for alignment optimization.
 */
#define BLOB_INITIAL_SIZE 4096
#define CACHE_LINE_SIZE   64

/*
 * Compile-time validation of assumptions.
 */
_Static_assert(BLOB_INITIAL_SIZE > 0, "BLOB_INITIAL_SIZE must be positive");
_Static_assert(CACHE_LINE_SIZE > 0, "CACHE_LINE_SIZE must be positive");
_Static_assert((CACHE_LINE_SIZE & (CACHE_LINE_SIZE - 1)) == 0,
               "CACHE_LINE_SIZE must be power of 2");
_Static_assert(sizeof(size_t) >= sizeof(uint32_t),
               "size_t must be at least 32 bits");

/**
 * Align x up to 'alignment' with overflow safety.
 *
 * Handles both power-of-two alignments (fast bit masking) and arbitrary
 * alignments (modulo arithmetic). Returns false on overflow.
 *
 * @param x          Value to align
 * @param alignment  Alignment granularity (0 and 1 are treated as no-op)
 * @param out        Output pointer for aligned value (must not be NULL)
 * @return           True on success, false on overflow
 */
static inline bool
align_up_safe(size_t x, size_t alignment, size_t *out)
{
   assert(out != NULL);

   /* Alignment of 0 or 1 is a no-op */
   if (alignment <= 1) {
      *out = x;
      return true;
   }

   /* Power-of-two fast path: use bit masking */
   if ((alignment & (alignment - 1)) == 0) {
      const size_t mask = alignment - 1;
      if (UNLIKELY(x > SIZE_MAX - mask)) {
         return false;
      }
      *out = (x + mask) & ~mask;
      return true;
   }

   /* Generic path for non-power-of-two alignment */
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

/**
 * Ensure that 'blob' can fit 'additional' more bytes.
 *
 * Growth strategy: double the allocation size to amortize realloc cost.
 * Allocations are rounded up to cache line boundaries for better memory
 * access patterns on x86-64.
 *
 * @param blob       The blob to potentially grow
 * @param additional Number of additional bytes needed
 * @return           True if space is available, false on OOM or overflow
 */
static bool
grow_to_fit(struct blob *blob, size_t additional)
{
   size_t required_size;
   size_t to_allocate;

   /* Early exit if already in error state */
   if (UNLIKELY(blob->out_of_memory)) {
      return false;
   }

   /* Overflow check: size + additional must not wrap */
   if (UNLIKELY(additional > SIZE_MAX - blob->size)) {
      blob->out_of_memory = true;
      return false;
   }

   required_size = blob->size + additional;

   /* Fast path: already have enough space */
   if (LIKELY(required_size <= blob->allocated)) {
      return true;
   }

   /* Fixed allocations cannot grow */
   if (UNLIKELY(blob->fixed_allocation)) {
      blob->out_of_memory = true;
      return false;
   }

   /*
    * Compute new allocation size using doubling strategy.
    * This gives O(1) amortized cost per byte written.
    */
   if (blob->allocated == 0) {
      to_allocate = BLOB_INITIAL_SIZE;
   } else if (blob->allocated >= SIZE_MAX / 2) {
      /* Prevent overflow in doubling */
      to_allocate = SIZE_MAX;
   } else {
      to_allocate = blob->allocated * 2;
   }

   /* Ensure we allocate at least what's required */
   if (to_allocate < required_size) {
      to_allocate = required_size;
   }

   /*
    * Round up to cache line boundary for better memory access patterns.
    * Skip if rounding would overflow or reduce allocation below required.
    */
   {
      size_t rounded;
      if (align_up_safe(to_allocate, CACHE_LINE_SIZE, &rounded)) {
         if (rounded >= required_size) {
            to_allocate = rounded;
         }
      }
   }

   /* Sanity check: never call realloc with size 0 */
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

/**
 * Reserve aligned space for a write operation.
 *
 * This is an internal helper that handles alignment, padding, and growth
 * in a single operation, avoiding multiple passes over the same logic.
 *
 * @param blob        The blob to write to
 * @param alignment   Required alignment for the write
 * @param write_size  Number of bytes to reserve
 * @param out_offset  Output: offset where data should be written
 * @return            True on success, false on error
 */
static bool
writer_reserve_aligned(struct blob *blob, size_t alignment, size_t write_size,
                       size_t *out_offset)
{
   size_t aligned_offset;
   size_t pad;

   assert(out_offset != NULL);

   /* Compute aligned position for the write */
   if (!align_up_safe(blob->size, alignment, &aligned_offset)) {
      blob->out_of_memory = true;
      return false;
   }

   pad = aligned_offset - blob->size;

   /* Overflow check: pad + write_size must not wrap */
   if (UNLIKELY(write_size > SIZE_MAX - pad)) {
      blob->out_of_memory = true;
      return false;
   }

   if (!grow_to_fit(blob, pad + write_size)) {
      return false;
   }

   /* Zero padding bytes for deterministic output (helps with hashing/comparison) */
   if (pad > 0 && blob->data != NULL) {
      memset(blob->data + blob->size, 0, pad);
   }

   *out_offset = aligned_offset;
   blob->size = aligned_offset + write_size;
   return true;
}

/**
 * Align the blob's write position to a granularity of 'alignment' bytes.
 *
 * Any padding bytes inserted are zeroed for deterministic output.
 *
 * @param blob       The blob to align
 * @param alignment  Alignment granularity (1 or 0 is a no-op)
 * @return           True on success, false on OOM or overflow
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

/**
 * Align the reader's current position.
 *
 * If alignment would move past the end of data, sets overrun flag.
 *
 * @param blob       The blob reader
 * @param alignment  Alignment granularity
 */
void
blob_reader_align(struct blob_reader *blob, size_t alignment)
{
   /* Early exit conditions */
   if (blob->overrun || alignment <= 1) {
      return;
   }

   /* NULL data means reader is in invalid state */
   if (UNLIKELY(blob->data == NULL)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   /* Validate current pointer is within valid range before arithmetic */
   if (UNLIKELY(blob->current < blob->data || blob->current > blob->end)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   const size_t offset = (size_t)(blob->current - blob->data);
   size_t new_offset;

   if (!align_up_safe(offset, alignment, &new_offset)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   /* Check if aligned position exceeds available data */
   const size_t total_size = (size_t)(blob->end - blob->data);
   if (UNLIKELY(new_offset > total_size)) {
      blob->overrun = true;
      blob->current = blob->end;
      return;
   }

   blob->current = blob->data + new_offset;
}

/**
 * Initialize a dynamically-growing blob.
 *
 * @param blob  The blob to initialize
 */
void
blob_init(struct blob *blob)
{
   blob->data = NULL;
   blob->allocated = 0;
   blob->size = 0;
   blob->fixed_allocation = false;
   blob->out_of_memory = false;
}

/**
 * Initialize a blob with a fixed-size buffer.
 *
 * The blob will use the provided buffer and cannot grow.
 * Writes beyond the buffer size will set the out_of_memory flag.
 *
 * @param blob  The blob to initialize
 * @param data  Buffer to use (may be NULL if size is 0)
 * @param size  Size of the buffer
 */
void
blob_init_fixed(struct blob *blob, void *data, size_t size)
{
   blob->data = (uint8_t *)data;
   blob->allocated = size;
   blob->size = 0;
   blob->fixed_allocation = true;
   blob->out_of_memory = false;
}

/**
 * Finish a blob and extract its buffer.
 *
 * The blob is reset and ownership of the buffer is transferred to the caller.
 * The buffer is trimmed to the actual size (realloc to shrink).
 *
 * @param blob    The blob to finish
 * @param buffer  Output: pointer to the data buffer (caller must free)
 * @param size    Output: size of data written
 */
void
blob_finish_get_buffer(struct blob *blob, void **buffer, size_t *size)
{
   assert(buffer != NULL);
   assert(size != NULL);

   *buffer = blob->data;
   *size = blob->size;
   blob->data = NULL;

   /*
    * Attempt to shrink the buffer to actual size.
    * On failure, keep the original (larger) buffer.
    */
   if (*size > 0 && *buffer != NULL) {
      void *trimmed = realloc(*buffer, *size);
      if (trimmed != NULL) {
         *buffer = trimmed;
      }
   }
}

/**
 * Overwrite bytes at a specific offset in the blob.
 *
 * The offset + to_write must not exceed the current blob size.
 * This does NOT grow the blob.
 *
 * @param blob      The blob to modify
 * @param offset    Offset from start of blob
 * @param bytes     Data to write (must not be NULL if to_write > 0)
 * @param to_write  Number of bytes to write
 * @return          True on success, false if region is out of bounds
 */
bool
blob_overwrite_bytes(struct blob *blob, size_t offset, const void *bytes,
                     size_t to_write)
{
   /* Validate overwrite region with overflow-safe bounds check */
   if (UNLIKELY(offset > blob->size)) {
      return false;
   }
   if (UNLIKELY(to_write > blob->size - offset)) {
      return false;
   }

   if (to_write > 0) {
      assert(bytes != NULL);
      VG(VALGRIND_CHECK_MEM_IS_DEFINED(bytes, to_write));
      if (blob->data != NULL) {
         memcpy(blob->data + offset, bytes, to_write);
      }
   }

   return true;
}

/**
 * Write bytes to the end of the blob.
 *
 * @param blob      The blob to write to
 * @param bytes     Data to write (must not be NULL if to_write > 0)
 * @param to_write  Number of bytes to write
 * @return          True on success, false on OOM
 */
MUST_CHECK bool
blob_write_bytes(struct blob *blob, const void *bytes, size_t to_write)
{
   if (to_write == 0) {
      return true;
   }

   assert(bytes != NULL);

   if (!grow_to_fit(blob, to_write)) {
      return false;
   }

   VG(VALGRIND_CHECK_MEM_IS_DEFINED(bytes, to_write));
   if (blob->data != NULL) {
      memcpy(blob->data + blob->size, bytes, to_write);
   }
   blob->size += to_write;

   return true;
}

/**
 * Reserve space for future writes without initializing.
 *
 * Returns the offset where reserved bytes start, or -1 on error.
 * The caller can later write to blob->data + offset.
 *
 * @param blob      The blob to reserve in
 * @param to_write  Number of bytes to reserve
 * @return          Offset of reserved space, or -1 on error
 */
MUST_CHECK intptr_t
blob_reserve_bytes(struct blob *blob, size_t to_write)
{
   /*
    * Ensure the returned offset can fit in intptr_t.
    * Both current size and size after reservation must fit.
    */
   if (UNLIKELY(blob->size > (size_t)INTPTR_MAX)) {
      blob->out_of_memory = true;
      return -1;
   }
   if (UNLIKELY(to_write > (size_t)(INTPTR_MAX - (intptr_t)blob->size))) {
      blob->out_of_memory = true;
      return -1;
   }

   if (!grow_to_fit(blob, to_write)) {
      return -1;
   }

   const intptr_t offset = (intptr_t)blob->size;
   blob->size += to_write;

   return offset;
}

/**
 * Reserve aligned space for a uint32_t.
 *
 * @param blob  The blob to reserve in
 * @return      Offset of reserved space, or -1 on error
 */
MUST_CHECK intptr_t
blob_reserve_uint32(struct blob *blob)
{
   if (!blob_align(blob, sizeof(uint32_t))) {
      return -1;
   }
   return blob_reserve_bytes(blob, sizeof(uint32_t));
}

/**
 * Reserve aligned space for an intptr_t.
 *
 * @param blob  The blob to reserve in
 * @return      Offset of reserved space, or -1 on error
 */
MUST_CHECK intptr_t
blob_reserve_intptr(struct blob *blob)
{
   if (!blob_align(blob, sizeof(intptr_t))) {
      return -1;
   }
   return blob_reserve_bytes(blob, sizeof(intptr_t));
}

/*
 * Macro to generate optimized typed write functions.
 *
 * Each function:
 * 1. Aligns to natural alignment of the type
 * 2. Reserves space with padding
 * 3. Copies the value
 *
 * The fused operation is more efficient than separate align + write calls.
 */
#define BLOB_WRITE_TYPE(name, type)                                            \
MUST_CHECK bool                                                                \
name(struct blob *blob, type value)                                            \
{                                                                              \
   size_t offset;                                                              \
   if (!writer_reserve_aligned(blob, sizeof(type), sizeof(type), &offset)) {   \
      return false;                                                            \
   }                                                                           \
   if (blob->data != NULL) {                                                   \
      memcpy(blob->data + offset, &value, sizeof(type));                       \
   }                                                                           \
   return true;                                                                \
}

BLOB_WRITE_TYPE(blob_write_uint8, uint8_t)
BLOB_WRITE_TYPE(blob_write_uint16, uint16_t)
BLOB_WRITE_TYPE(blob_write_uint32, uint32_t)
BLOB_WRITE_TYPE(blob_write_uint64, uint64_t)
BLOB_WRITE_TYPE(blob_write_intptr, intptr_t)

#undef BLOB_WRITE_TYPE

/*
 * Debug assertion to verify offset is properly aligned.
 * Only active in debug builds; compiles to nothing in release.
 */
#define ASSERT_ALIGNED(offset, align)                                          \
   do {                                                                        \
      size_t __aligned;                                                        \
      assert(align_up_safe((offset), (align), &__aligned) &&                   \
             __aligned == (offset));                                           \
   } while (0)

/**
 * Overwrite a uint8_t at a specific offset.
 */
bool
blob_overwrite_uint8(struct blob *blob, size_t offset, uint8_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

/**
 * Overwrite a uint32_t at a specific offset.
 */
bool
blob_overwrite_uint32(struct blob *blob, size_t offset, uint32_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

/**
 * Overwrite an intptr_t at a specific offset.
 */
bool
blob_overwrite_intptr(struct blob *blob, size_t offset, intptr_t value)
{
   ASSERT_ALIGNED(offset, sizeof(value));
   return blob_overwrite_bytes(blob, offset, &value, sizeof(value));
}

/**
 * Write a NUL-terminated string to the blob.
 *
 * The NUL terminator is included in the written data.
 *
 * @param blob  The blob to write to
 * @param str   String to write (must not be NULL)
 * @return      True on success, false on OOM
 */
MUST_CHECK bool
blob_write_string(struct blob *blob, const char *str)
{
   assert(str != NULL);

   const size_t len = strlen(str);

   /*
    * Paranoid overflow check: strlen returning SIZE_MAX would indicate
    * a string of impossible length, but guard against it anyway.
    */
   if (UNLIKELY(len == SIZE_MAX)) {
      blob->out_of_memory = true;
      return false;
   }

   /* Include NUL terminator */
   const size_t total = len + 1;

   if (!grow_to_fit(blob, total)) {
      return false;
   }

   VG(VALGRIND_CHECK_MEM_IS_DEFINED(str, total));
   if (blob->data != NULL) {
      memcpy(blob->data + blob->size, str, total);
   }
   blob->size += total;

   return true;
}

/**
 * Initialize a blob reader.
 *
 * The reader provides a view over existing data; it does not own the data.
 * The data pointer must remain valid for the lifetime of the reader.
 *
 * @param blob  The reader to initialize
 * @param data  Pointer to data (may be NULL only if size is 0)
 * @param size  Size of data in bytes
 */
void
blob_reader_init(struct blob_reader *blob, const void *data, size_t size)
{
   blob->data = (const uint8_t *)data;
   blob->overrun = false;

   /* Handle empty blob: all pointers equal, valid but nothing to read */
   if (size == 0) {
      blob->end = blob->data;
      blob->current = blob->data;
      return;
   }

   /* Non-empty blob with NULL data is invalid */
   if (UNLIKELY(data == NULL)) {
      blob->end = NULL;
      blob->current = NULL;
      blob->overrun = true;
      return;
   }

   blob->end = blob->data + size;
   blob->current = blob->data;

   /*
    * Prefetch first cache lines for sequential read pattern.
    * Use decreasing locality hints for prefetch distance:
    * - First line: locality 3 (keep in all cache levels)
    * - Second line: locality 2 (keep in L2+)
    * - Third line: locality 1 (keep in L3 only)
    */
   PREFETCH_READ(blob->current, 3);
   if (size > CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);
   }
   if (size > 2 * CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + 2 * CACHE_LINE_SIZE, 1);
   }
}

/**
 * Check if 'size' bytes can be read from current position.
 *
 * Sets overrun flag on failure; never performs invalid pointer arithmetic.
 *
 * @param blob  The blob reader
 * @param size  Number of bytes to check
 * @return      True if readable, false otherwise
 */
static bool
ensure_can_read(struct blob_reader *blob, size_t size)
{
   if (UNLIKELY(blob->overrun)) {
      return false;
   }

   /* Zero-length reads are always valid */
   if (size == 0) {
      return true;
   }

   /* NULL data means reader is in invalid state */
   if (UNLIKELY(blob->data == NULL)) {
      blob->overrun = true;
      return false;
   }

   /* Validate current pointer before subtraction */
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

/**
 * Read bytes from the blob without copying.
 *
 * Returns a pointer into the blob's data; the pointer is valid as long
 * as the underlying data remains valid.
 *
 * @param blob  The blob reader
 * @param size  Number of bytes to read
 * @return      Pointer to data, or NULL on overrun
 */
const void *
blob_read_bytes(struct blob_reader *blob, size_t size)
{
   if (!ensure_can_read(blob, size)) {
      return NULL;
   }

   const void *ret = blob->current;
   blob->current += size;

   /* Prefetch ahead for sequential access pattern */
   const size_t remaining = (size_t)(blob->end - blob->current);
   if (remaining > CACHE_LINE_SIZE) {
      PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);
   }

   return ret;
}

/**
 * Read and copy bytes from the blob.
 *
 * Unlike blob_read_bytes, this copies data to caller's buffer.
 *
 * @param blob  The blob reader
 * @param dest  Destination buffer (must not be NULL if size > 0)
 * @param size  Number of bytes to copy
 */
void
blob_copy_bytes(struct blob_reader *blob, void *dest, size_t size)
{
   const void *bytes = blob_read_bytes(blob, size);
   if (bytes == NULL) {
      return;
   }

   if (size > 0) {
      assert(dest != NULL);
      memcpy(dest, bytes, size);
   }
}

/**
 * Skip bytes in the reader without reading them.
 *
 * @param blob  The blob reader
 * @param size  Number of bytes to skip
 */
void
blob_skip_bytes(struct blob_reader *blob, size_t size)
{
   if (ensure_can_read(blob, size)) {
      blob->current += size;
   }
}

/*
 * Macro to generate optimized typed read functions.
 *
 * Each function:
 * 1. Aligns to natural alignment of the type
 * 2. Checks bounds
 * 3. Copies the value out
 * 4. Prefetches ahead
 *
 * Returns 0 on overrun.
 */
#define BLOB_READ_TYPE(name, type)                                             \
type                                                                           \
name(struct blob_reader *blob)                                                 \
{                                                                              \
   type ret = (type)0;                                                         \
   blob_reader_align(blob, sizeof(type));                                      \
   if (ensure_can_read(blob, sizeof(type))) {                                  \
      memcpy(&ret, blob->current, sizeof(type));                               \
      blob->current += sizeof(type);                                           \
      const size_t remaining = (size_t)(blob->end - blob->current);            \
      if (remaining > CACHE_LINE_SIZE) {                                       \
         PREFETCH_READ(blob->current + CACHE_LINE_SIZE, 2);                    \
      }                                                                        \
   }                                                                           \
   return ret;                                                                 \
}

BLOB_READ_TYPE(blob_read_uint8, uint8_t)
BLOB_READ_TYPE(blob_read_uint16, uint16_t)
BLOB_READ_TYPE(blob_read_uint32, uint32_t)
BLOB_READ_TYPE(blob_read_uint64, uint64_t)
BLOB_READ_TYPE(blob_read_intptr, intptr_t)

#undef BLOB_READ_TYPE

/**
 * Read a NUL-terminated string from the blob.
 *
 * Returns a pointer into the blob's data; does not copy.
 * The NUL terminator must be present within the remaining data.
 *
 * @param blob  The blob reader
 * @return      Pointer to string, or NULL on overrun/missing NUL
 */
char *
blob_read_string(struct blob_reader *blob)
{
   /* Check for empty/exhausted reader */
   if (UNLIKELY(blob->current >= blob->end)) {
      blob->overrun = true;
      return NULL;
   }

   /* NULL data means invalid reader state */
   if (UNLIKELY(blob->data == NULL)) {
      blob->overrun = true;
      return NULL;
   }

   /* Search for NUL terminator within remaining data */
   const size_t remaining = (size_t)(blob->end - blob->current);
   const uint8_t *nul = (const uint8_t *)memchr(blob->current, 0, remaining);

   if (UNLIKELY(nul == NULL)) {
      blob->overrun = true;
      return NULL;
   }

   /* Calculate string size including NUL, advance reader */
   const size_t size = (size_t)(nul - blob->current) + 1;
   char *ret = (char *)blob->current;
   blob->current += size;

   return ret;
}
