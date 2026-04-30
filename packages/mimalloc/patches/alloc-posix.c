/* ----------------------------------------------------------------------------
Copyright (c) 2018-2026, Microsoft Research, Daan Leijen
This is free software; you can redistribute it and/or modify it under the
terms of the MIT license. A copy of the license can be found in the file
"LICENSE" at the root of this distribution.
-----------------------------------------------------------------------------*/

// ------------------------------------------------------------------------
// mi-prefixed public definitions of various Posix, Unix, and C++ functions
// for convenience and use when overriding these functions.
// ------------------------------------------------------------------------

#include "mimalloc.h"
#include "mimalloc/internal.h"

// ------------------------------------------------------
// Posix & Unix function definitions
// ------------------------------------------------------

#include <errno.h>
#include <stddef.h>   // ptrdiff_t, size_t
#include <stdint.h>   // SIZE_MAX
#include <stdlib.h>   // getenv
#include <string.h>   // memset
#include <wchar.h>    // wchar_t, wcslen

#ifdef _MSC_VER
#pragma warning(disable:4996)  // getenv, _wgetenv
#endif

#ifndef EINVAL
#define EINVAL 22
#endif
#ifndef ENOMEM
#define ENOMEM 12
#endif

static inline bool mi_alignment_is_valid(size_t alignment) {
  return (alignment != 0 &&
          _mi_is_power_of_two(alignment) &&
          (alignment % sizeof(void*)) == 0);
}

mi_decl_nodiscard size_t mi_malloc_size(const void* p) mi_attr_noexcept {
  return mi_usable_size(p);
}

mi_decl_nodiscard size_t mi_malloc_usable_size(const void* p) mi_attr_noexcept {
  return mi_usable_size(p);
}

mi_decl_nodiscard size_t mi_malloc_good_size(size_t size) mi_attr_noexcept {
  return mi_good_size(size);
}

void mi_cfree(void* p) mi_attr_noexcept {
  // Keep behavior conservative for foreign pointers.
  if (mi_is_in_heap_region(p)) {
    mi_free(p);
  }
}

int mi_posix_memalign(void** p, size_t alignment, size_t size) mi_attr_noexcept {
  // Spec says: do not modify `*p` on error.
  if (p == NULL) return EINVAL;
  if (!mi_alignment_is_valid(alignment)) return EINVAL;

  void* const q = mi_malloc_aligned(size, alignment);
  if (q == NULL && size != 0) return ENOMEM;

  mi_assert_internal(_mi_is_aligned(q, alignment));
  *p = q;
  return 0;
}

mi_decl_nodiscard mi_decl_restrict void* mi_memalign(size_t alignment, size_t size) mi_attr_noexcept {
  void* const p = mi_malloc_aligned(size, alignment);
  mi_assert_internal(_mi_is_aligned(p, alignment));
  return p;
}

mi_decl_nodiscard mi_decl_restrict void* mi_valloc(size_t size) mi_attr_noexcept {
  return mi_memalign(_mi_os_page_size(), size);
}

mi_decl_nodiscard mi_decl_restrict void* mi_pvalloc(size_t size) mi_attr_noexcept {
  const size_t psize = _mi_os_page_size();
  if (size > (SIZE_MAX - psize)) return NULL;  // overflow guard for align-up arithmetic
  const size_t asize = _mi_align_up(size, psize);
  return mi_malloc_aligned(asize, psize);
}

mi_decl_nodiscard mi_decl_restrict void* mi_aligned_alloc(size_t alignment, size_t size) mi_attr_noexcept {
  // C11 requires size to be a multiple of alignment; many programs violate this in practice.
  // We intentionally keep mimalloc's permissive behavior.
  void* const p = mi_malloc_aligned(size, alignment);
  mi_assert_internal(_mi_is_aligned(p, alignment));
  return p;
}

mi_decl_nodiscard void* mi_reallocarray(void* p, size_t count, size_t size) mi_attr_noexcept {  // BSD
  size_t total = 0;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) {
    errno = EOVERFLOW;
    return NULL;
  }
  void* const newp = mi_realloc(p, total);
  if (newp == NULL) { errno = ENOMEM; }
  return newp;
}

mi_decl_nodiscard int mi_reallocarr(void* ptrp, size_t count, size_t size) mi_attr_noexcept { // NetBSD
  mi_assert(ptrp != NULL);
  mi_assert(size != 0);

  if (ptrp == NULL || size == 0) {
    errno = EINVAL;
    return EINVAL;
  }

  size_t total = 0;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) {
    errno = EOVERFLOW;
    return EOVERFLOW;
  }

  void** const op = (void**)ptrp;
  if (total == 0) {
    free(*op);   // POSIX-compatible behavior for foreign pointers
    *op = NULL;
    return 0;
  }

  void* const newp = mi_realloc(*op, total);
  if (newp == NULL) {
    errno = ENOMEM;
    return ENOMEM;
  }

  *op = newp;
  return 0;
}

void* mi__expand(void* p, size_t newsize) mi_attr_noexcept {  // Microsoft
  void* const res = mi_expand(p, newsize);
  if (res == NULL) { errno = ENOMEM; }
  return res;
}

mi_decl_nodiscard mi_decl_restrict wchar_t* mi_wcsdup(const wchar_t* s) mi_attr_noexcept {
  if (s == NULL) return NULL;

  // Cap scan by allocator limit to avoid overflow in `(wlen+1)*sizeof(wchar_t)`.
  const size_t wmax = (MI_MAX_ALLOC_SIZE / sizeof(wchar_t));
  if (wmax == 0) return NULL;

  size_t wlen = 0;
  while (s[wlen] != L'\0') {
    if mi_unlikely(wlen >= (wmax - 1)) return NULL;
    wlen++;
  }

  const size_t bytes = (wlen + 1) * sizeof(wchar_t);
  wchar_t* const p = (wchar_t*)mi_malloc(bytes);
  if (p != NULL) {
    _mi_memcpy(p, s, bytes);
  }
  return p;
}

mi_decl_nodiscard mi_decl_restrict unsigned char* mi_mbsdup(const unsigned char* s) mi_attr_noexcept {
  return (unsigned char*)mi_strdup((const char*)s);
}

int mi_dupenv_s(char** buf, size_t* size, const char* name) mi_attr_noexcept {
  if (size != NULL) *size = 0;
  if (buf == NULL || name == NULL) return EINVAL;

  char* const p = getenv(name);
  if (p == NULL) {
    *buf = NULL;
    return 0;
  }

  char* const dup = mi_strdup(p);
  if (dup == NULL) return ENOMEM;

  *buf = dup;
  if (size != NULL) {
    *size = _mi_strlen(p) + 1; // bounded by mi_strdup internal limits
  }
  return 0;
}

int mi_wdupenv_s(wchar_t** buf, size_t* size, const wchar_t* name) mi_attr_noexcept {
  if (size != NULL) *size = 0;
  if (buf == NULL || name == NULL) return EINVAL;

#if !defined(_WIN32) || (defined(WINAPI_FAMILY) && (WINAPI_FAMILY != WINAPI_FAMILY_DESKTOP_APP))
  // Not supported on non-desktop Windows families and non-Windows.
  *buf = NULL;
  return EINVAL;
#else
  wchar_t* const p = (wchar_t*)_wgetenv(name);
  if (p == NULL) {
    *buf = NULL;
    return 0;
  }

  wchar_t* const dup = mi_wcsdup(p);
  if (dup == NULL) return ENOMEM;

  *buf = dup;
  if (size != NULL) {
    *size = wcslen(p) + 1; // bounded by mi_wcsdup limits
  }
  return 0;
#endif
}

mi_decl_nodiscard void* mi_aligned_offset_recalloc(void* p, size_t newcount, size_t size, size_t alignment, size_t offset) mi_attr_noexcept { // Microsoft
  return mi_recalloc_aligned_at(p, newcount, size, alignment, offset);
}

mi_decl_nodiscard void* mi_aligned_recalloc(void* p, size_t newcount, size_t size, size_t alignment) mi_attr_noexcept { // Microsoft
  return mi_recalloc_aligned(p, newcount, size, alignment);
}
