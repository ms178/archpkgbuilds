#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "mimalloc/atomic.h"
#include "mimalloc/prim.h"

#include <string.h>
#include <stdlib.h>

#if defined(__GNUC__) || defined(__clang__)
  #if defined(__x86_64__) || defined(__i386__)
    #define mi_prefetch_block(addr) __builtin_prefetch((addr), 0, 3)
  #elif defined(__aarch64__)
    #define mi_prefetch_block(addr) __builtin_prefetch((addr), 0, 3)
  #else
    #define mi_prefetch_block(addr) ((void)(addr))
  #endif
#else
  #define mi_prefetch_block(addr) ((void)(addr))
#endif

#define MI_IN_ALLOC_C
#include "alloc-override.c"
#include "free.c"
#undef MI_IN_ALLOC_C

extern inline void* _mi_page_malloc_zero(mi_heap_t* heap, mi_page_t* page, size_t size, bool zero, size_t* usable) mi_attr_noexcept
{
  mi_assert_internal(size >= MI_PADDING_SIZE);
  mi_assert_internal(page->block_size == 0 || mi_page_block_size(page) >= size);

  mi_block_t* const block = page->free;
  if mi_unlikely(block == NULL) {
    return _mi_malloc_generic(heap, size, zero, 0, usable);
  }

  mi_assert_internal(block != NULL && _mi_ptr_page(block) == page);

  const size_t bsize = mi_page_usable_block_size(page);

  if (usable != NULL) {
    *usable = bsize;
  }

  mi_block_t* const next = mi_block_next(page, block);
  page->free = next;
  page->used++;

  if mi_likely(next != NULL) {
    mi_prefetch_block(next);
  }

  mi_assert_internal(page->free == NULL || _mi_ptr_page(page->free) == page);
  mi_assert_internal(page->block_size < MI_MAX_ALIGN_SIZE || _mi_is_aligned(block, MI_MAX_ALIGN_SIZE));

#if MI_DEBUG>3
  if (page->free_is_zero && size > sizeof(*block)) {
    mi_assert_expensive(mi_mem_is_zero(block + 1, size - sizeof(*block)));
  }
#endif

  mi_track_mem_undefined(block, bsize);

  if mi_unlikely(zero) {
    mi_assert_internal(page->block_size != 0);
#if MI_PADDING
    mi_assert_internal(page->block_size >= MI_PADDING_SIZE);
#endif
    const size_t zsize = page->block_size - MI_PADDING_SIZE;
    if (page->free_is_zero) {
      block->next = 0;
      mi_track_mem_defined(block, zsize);
    }
    else {
      _mi_memzero_aligned(block, zsize);
    }
  }

#if (MI_DEBUG>0) && !MI_TRACK_ENABLED && !MI_TSAN
  if (!zero && !mi_page_is_huge(page)) {
    memset(block, MI_DEBUG_UNINIT, bsize);
  }
#elif (MI_SECURE!=0)
  if (!zero) {
    block->next = 0;
  }
#endif

#if (MI_STAT>0)
  if (bsize <= MI_MEDIUM_OBJ_SIZE_MAX) {
    mi_heap_stat_increase(heap, malloc_normal, bsize);
    mi_heap_stat_counter_increase(heap, malloc_normal_count, 1);
#if (MI_STAT>1)
    const size_t bin = _mi_bin(bsize);
    mi_heap_stat_increase(heap, malloc_bins[bin], 1);
    mi_heap_stat_increase(heap, malloc_requested, size - MI_PADDING_SIZE);
#endif
  }
#endif

#if MI_PADDING
  mi_padding_t* const padding = (mi_padding_t*)((uint8_t*)block + bsize);
  ptrdiff_t delta = ((uint8_t*)padding - (uint8_t*)block - (size - MI_PADDING_SIZE));
#if (MI_DEBUG>=2)
  mi_assert_internal(delta >= 0 && bsize >= (size - MI_PADDING_SIZE + delta));
#endif
  mi_track_mem_defined(padding, sizeof(mi_padding_t));
  padding->canary = mi_ptr_encode_canary(page, block, page->keys);
  padding->delta  = (uint32_t)(delta);
#if MI_PADDING_CHECK
  if (!mi_page_is_huge(page)) {
    uint8_t* fill = (uint8_t*)padding - delta;
    const size_t maxpad = (delta > MI_MAX_ALIGN_SIZE ? MI_MAX_ALIGN_SIZE : (size_t)delta);
    for (size_t i = 0; i < maxpad; i++) {
      fill[i] = MI_DEBUG_PADDING;
    }
  }
#endif
#endif

  return block;
}

extern void* _mi_page_malloc(mi_heap_t* heap, mi_page_t* page, size_t size) mi_attr_noexcept {
  return _mi_page_malloc_zero(heap, page, size, false, NULL);
}

extern void* _mi_page_malloc_zeroed(mi_heap_t* heap, mi_page_t* page, size_t size) mi_attr_noexcept {
  return _mi_page_malloc_zero(heap, page, size, true, NULL);
}

#if MI_GUARDED
mi_decl_restrict void* _mi_heap_malloc_guarded(mi_heap_t* heap, size_t size, bool zero) mi_attr_noexcept;
#endif

static inline mi_decl_restrict void* mi_heap_malloc_small_zero(mi_heap_t* heap, size_t size, bool zero, size_t* usable) mi_attr_noexcept {
  mi_assert(heap != NULL);
  mi_assert(size <= MI_SMALL_SIZE_MAX);
#if MI_DEBUG
  const uintptr_t tid = _mi_thread_id();
  mi_assert(heap->thread_id == 0 || heap->thread_id == tid);
#endif
#if (MI_PADDING || MI_GUARDED)
  if (size == 0) {
    size = sizeof(void*);
  }
#endif
#if MI_GUARDED
  if (mi_heap_malloc_use_guarded(heap, size)) {
    return _mi_heap_malloc_guarded(heap, size, zero);
  }
#endif

  mi_page_t* page = _mi_heap_get_free_small_page(heap, size + MI_PADDING_SIZE);
  void* const p = _mi_page_malloc_zero(heap, page, size + MI_PADDING_SIZE, zero, usable);
  mi_track_malloc(p, size, zero);

#if MI_DEBUG>3
  if (p != NULL && zero) {
    mi_assert_expensive(mi_mem_is_zero(p, size));
  }
#endif
  return p;
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_heap_malloc_small(mi_heap_t* heap, size_t size) mi_attr_noexcept {
  return mi_heap_malloc_small_zero(heap, size, false, NULL);
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_malloc_small(size_t size) mi_attr_noexcept {
  return mi_heap_malloc_small(mi_prim_get_default_heap(), size);
}

extern inline void* _mi_heap_malloc_zero_ex(mi_heap_t* heap, size_t size, bool zero, size_t huge_alignment, size_t* usable) mi_attr_noexcept {
  if mi_likely(size <= MI_SMALL_SIZE_MAX) {
    mi_assert_internal(huge_alignment == 0);
    return mi_heap_malloc_small_zero(heap, size, zero, usable);
  }
#if MI_GUARDED
  else if (huge_alignment == 0 && mi_heap_malloc_use_guarded(heap, size)) {
    return _mi_heap_malloc_guarded(heap, size, zero);
  }
#endif
  else {
    mi_assert(heap != NULL);
    mi_assert(heap->thread_id == 0 || heap->thread_id == _mi_thread_id());
    void* const p = _mi_malloc_generic(heap, size + MI_PADDING_SIZE, zero, huge_alignment, usable);
    mi_track_malloc(p, size, zero);

#if MI_DEBUG>3
    if (p != NULL && zero) {
      mi_assert_expensive(mi_mem_is_zero(p, size));
    }
#endif
    return p;
  }
}

extern inline void* _mi_heap_malloc_zero(mi_heap_t* heap, size_t size, bool zero) mi_attr_noexcept {
  return _mi_heap_malloc_zero_ex(heap, size, zero, 0, NULL);
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_heap_malloc(mi_heap_t* heap, size_t size) mi_attr_noexcept {
  return _mi_heap_malloc_zero(heap, size, false);
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_malloc(size_t size) mi_attr_noexcept {
  return mi_heap_malloc(mi_prim_get_default_heap(), size);
}

mi_decl_nodiscard mi_decl_restrict void* mi_zalloc_small(size_t size) mi_attr_noexcept {
  return mi_heap_malloc_small_zero(mi_prim_get_default_heap(), size, true, NULL);
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_heap_zalloc(mi_heap_t* heap, size_t size) mi_attr_noexcept {
  return _mi_heap_malloc_zero(heap, size, true);
}

mi_decl_nodiscard mi_decl_restrict void* mi_zalloc(size_t size) mi_attr_noexcept {
  return mi_heap_zalloc(mi_prim_get_default_heap(), size);
}

mi_decl_nodiscard extern inline mi_decl_restrict void* mi_heap_calloc(mi_heap_t* heap, size_t count, size_t size) mi_attr_noexcept {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) return NULL;
  return mi_heap_zalloc(heap, total);
}

mi_decl_nodiscard mi_decl_restrict void* mi_calloc(size_t count, size_t size) mi_attr_noexcept {
  return mi_heap_calloc(mi_prim_get_default_heap(), count, size);
}

mi_decl_nodiscard mi_decl_restrict void* mi_umalloc_small(size_t size, size_t* usable) mi_attr_noexcept {
  return mi_heap_malloc_small_zero(mi_prim_get_default_heap(), size, false, usable);
}

mi_decl_nodiscard mi_decl_restrict void* mi_heap_umalloc(mi_heap_t* heap, size_t size, size_t* usable) mi_attr_noexcept {
  return _mi_heap_malloc_zero_ex(heap, size, false, 0, usable);
}

mi_decl_nodiscard mi_decl_restrict void* mi_umalloc(size_t size, size_t* usable) mi_attr_noexcept {
  return mi_heap_umalloc(mi_prim_get_default_heap(), size, usable);
}

mi_decl_nodiscard mi_decl_restrict void* mi_uzalloc(size_t size, size_t* usable) mi_attr_noexcept {
  return _mi_heap_malloc_zero_ex(mi_prim_get_default_heap(), size, true, 0, usable);
}

mi_decl_nodiscard mi_decl_restrict void* mi_ucalloc(size_t count, size_t size, size_t* usable) mi_attr_noexcept {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) return NULL;
  return mi_uzalloc(total, usable);
}

mi_decl_nodiscard extern mi_decl_restrict void* mi_heap_mallocn(mi_heap_t* heap, size_t count, size_t size) mi_attr_noexcept {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) return NULL;
  return mi_heap_malloc(heap, total);
}

mi_decl_nodiscard mi_decl_restrict void* mi_mallocn(size_t count, size_t size) mi_attr_noexcept {
  return mi_heap_mallocn(mi_prim_get_default_heap(), count, size);
}

void* mi_expand(void* p, size_t newsize) mi_attr_noexcept {
#if MI_PADDING
  MI_UNUSED(p);
  MI_UNUSED(newsize);
  return NULL;
#else
  if mi_unlikely(p == NULL) return NULL;
  const mi_page_t* const page = mi_validate_ptr_page(p, "mi_expand");
  const size_t size = _mi_usable_size(p, page);
  if mi_unlikely(newsize > size) return NULL;
  return p;
#endif
}

void* _mi_heap_realloc_zero(mi_heap_t* heap, void* p, size_t newsize, bool zero, size_t* usable_pre, size_t* usable_post) mi_attr_noexcept {
  const mi_page_t* page;
  size_t size;

  if mi_unlikely(p == NULL) {
    page = NULL;
    size = 0;
    if (usable_pre != NULL) {
      *usable_pre = 0;
    }
  }
  else {
    page = mi_validate_ptr_page(p, "mi_realloc");
    size = _mi_usable_size(p, page);
    if (usable_pre != NULL) {
      *usable_pre = mi_page_usable_block_size(page);
    }
    mi_prefetch_block(p);
  }

  if mi_unlikely(newsize <= size && newsize >= (size / 2) && newsize > 0) {
    mi_assert_internal(p != NULL);
    if (usable_post != NULL) {
      *usable_post = mi_page_usable_block_size(page);
    }
    return p;
  }

  void* newp = mi_heap_umalloc(heap, newsize, usable_post);
  if mi_likely(newp != NULL) {
    if (zero && newsize > size) {
      const size_t start = (size >= sizeof(intptr_t) ? size - sizeof(intptr_t) : 0);
      _mi_memzero((uint8_t*)newp + start, newsize - start);
    }
    else if mi_unlikely(newsize == 0) {
      ((uint8_t*)newp)[0] = 0;
    }
    if mi_likely(p != NULL) {
      const size_t copysize = (newsize > size ? size : newsize);
      mi_track_mem_defined(p, copysize);
      _mi_memcpy(newp, p, copysize);
      mi_free(p);
    }
  }
  return newp;
}

mi_decl_nodiscard void* mi_heap_realloc(mi_heap_t* heap, void* p, size_t newsize) mi_attr_noexcept {
  return _mi_heap_realloc_zero(heap, p, newsize, false, NULL, NULL);
}

mi_decl_nodiscard void* mi_heap_reallocn(mi_heap_t* heap, void* p, size_t count, size_t size) mi_attr_noexcept {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) return NULL;
  return mi_heap_realloc(heap, p, total);
}

mi_decl_nodiscard void* mi_heap_reallocf(mi_heap_t* heap, void* p, size_t newsize) mi_attr_noexcept {
  void* newp = mi_heap_realloc(heap, p, newsize);
  if (newp == NULL && p != NULL) mi_free(p);
  return newp;
}

mi_decl_nodiscard void* mi_heap_rezalloc(mi_heap_t* heap, void* p, size_t newsize) mi_attr_noexcept {
  return _mi_heap_realloc_zero(heap, p, newsize, true, NULL, NULL);
}

mi_decl_nodiscard void* mi_heap_recalloc(mi_heap_t* heap, void* p, size_t count, size_t size) mi_attr_noexcept {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) return NULL;
  return mi_heap_rezalloc(heap, p, total);
}

mi_decl_nodiscard void* mi_realloc(void* p, size_t newsize) mi_attr_noexcept {
  return mi_heap_realloc(mi_prim_get_default_heap(), p, newsize);
}

mi_decl_nodiscard void* mi_reallocn(void* p, size_t count, size_t size) mi_attr_noexcept {
  return mi_heap_reallocn(mi_prim_get_default_heap(), p, count, size);
}

mi_decl_nodiscard void* mi_urealloc(void* p, size_t newsize, size_t* usable_pre, size_t* usable_post) mi_attr_noexcept {
  return _mi_heap_realloc_zero(mi_prim_get_default_heap(), p, newsize, false, usable_pre, usable_post);
}

mi_decl_nodiscard void* mi_reallocf(void* p, size_t newsize) mi_attr_noexcept {
  return mi_heap_reallocf(mi_prim_get_default_heap(), p, newsize);
}

mi_decl_nodiscard void* mi_rezalloc(void* p, size_t newsize) mi_attr_noexcept {
  return mi_heap_rezalloc(mi_prim_get_default_heap(), p, newsize);
}

mi_decl_nodiscard void* mi_recalloc(void* p, size_t count, size_t size) mi_attr_noexcept {
  return mi_heap_recalloc(mi_prim_get_default_heap(), p, count, size);
}

mi_decl_nodiscard mi_decl_restrict char* mi_heap_strdup(mi_heap_t* heap, const char* s) mi_attr_noexcept {
  if mi_unlikely(s == NULL) return NULL;
  size_t len = _mi_strlen(s);
  char* t = (char*)mi_heap_malloc(heap, len + 1);
  if mi_unlikely(t == NULL) return NULL;
  _mi_memcpy(t, s, len);
  t[len] = 0;
  return t;
}

mi_decl_nodiscard mi_decl_restrict char* mi_strdup(const char* s) mi_attr_noexcept {
  return mi_heap_strdup(mi_prim_get_default_heap(), s);
}

mi_decl_nodiscard mi_decl_restrict char* mi_heap_strndup(mi_heap_t* heap, const char* s, size_t n) mi_attr_noexcept {
  if mi_unlikely(s == NULL) return NULL;
  const size_t len = _mi_strnlen(s, n);
  char* t = (char*)mi_heap_malloc(heap, len + 1);
  if mi_unlikely(t == NULL) return NULL;
  _mi_memcpy(t, s, len);
  t[len] = 0;
  return t;
}

mi_decl_nodiscard mi_decl_restrict char* mi_strndup(const char* s, size_t n) mi_attr_noexcept {
  return mi_heap_strndup(mi_prim_get_default_heap(), s, n);
}

#ifndef __wasi__
#ifdef _WIN32
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif

mi_decl_nodiscard mi_decl_restrict char* mi_heap_realpath(mi_heap_t* heap, const char* fname, char* resolved_name) mi_attr_noexcept {
  char buf[PATH_MAX];
  DWORD res = GetFullPathNameA(fname, PATH_MAX, (resolved_name == NULL ? buf : resolved_name), NULL);
  if (res == 0) {
    errno = GetLastError();
    return NULL;
  }
  else if (res > PATH_MAX) {
    errno = EINVAL;
    return NULL;
  }
  else if (resolved_name != NULL) {
    return resolved_name;
  }
  else {
    return mi_heap_strndup(heap, buf, PATH_MAX);
  }
}
#else
char* mi_heap_realpath(mi_heap_t* heap, const char* fname, char* resolved_name) mi_attr_noexcept {
  if (resolved_name != NULL) {
    return realpath(fname, resolved_name);
  }
  else {
    char* rname = realpath(fname, NULL);
    if (rname == NULL) return NULL;
    char* result = mi_heap_strdup(heap, rname);
    mi_cfree(rname);
    return result;
  }
}
#endif

mi_decl_nodiscard mi_decl_restrict char* mi_realpath(const char* fname, char* resolved_name) mi_attr_noexcept {
  return mi_heap_realpath(mi_prim_get_default_heap(), fname, resolved_name);
}
#endif

#ifdef __cplusplus
#include <new>
static bool mi_try_new_handler(bool nothrow) {
#if defined(_MSC_VER) || (__cplusplus >= 201103L)
  std::new_handler h = std::get_new_handler();
#else
  std::new_handler h = std::set_new_handler();
  std::set_new_handler(h);
#endif
  if (h == NULL) {
    _mi_error_message(ENOMEM, "out of memory in 'new'");
#if defined(_CPPUNWIND) || defined(__cpp_exceptions)
    if (!nothrow) {
      throw std::bad_alloc();
    }
#else
    MI_UNUSED(nothrow);
#endif
    return false;
  }
  else {
    h();
    return true;
  }
}
#else
typedef void (*std_new_handler_t)(void);

#if (defined(__GNUC__) || (defined(__clang__) && !defined(_MSC_VER)))
std_new_handler_t __attribute__((weak)) _ZSt15get_new_handlerv(void) {
  return NULL;
}
static std_new_handler_t mi_get_new_handler(void) {
  return _ZSt15get_new_handlerv();
}
#else
static std_new_handler_t mi_get_new_handler(void) {
  return NULL;
}
#endif

static bool mi_try_new_handler(bool nothrow) {
  std_new_handler_t h = mi_get_new_handler();
  if (h == NULL) {
    _mi_error_message(ENOMEM, "out of memory in 'new'");
    if (!nothrow) {
      abort();
    }
    return false;
  }
  else {
    h();
    return true;
  }
}
#endif

mi_decl_export mi_decl_noinline void* mi_heap_try_new(mi_heap_t* heap, size_t size, bool nothrow) {
  void* p = NULL;
  while (p == NULL && mi_try_new_handler(nothrow)) {
    p = mi_heap_malloc(heap, size);
  }
  return p;
}

static mi_decl_noinline void* mi_try_new(size_t size, bool nothrow) {
  return mi_heap_try_new(mi_prim_get_default_heap(), size, nothrow);
}

mi_decl_nodiscard mi_decl_restrict void* mi_heap_alloc_new(mi_heap_t* heap, size_t size) {
  void* p = mi_heap_malloc(heap, size);
  if mi_unlikely(p == NULL) return mi_heap_try_new(heap, size, false);
  return p;
}

mi_decl_nodiscard mi_decl_restrict void* mi_new(size_t size) {
  return mi_heap_alloc_new(mi_prim_get_default_heap(), size);
}

mi_decl_nodiscard mi_decl_restrict void* mi_heap_alloc_new_n(mi_heap_t* heap, size_t count, size_t size) {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(count, size, &total)) {
    mi_try_new_handler(false);
    return NULL;
  }
  else {
    return mi_heap_alloc_new(heap, total);
  }
}

mi_decl_nodiscard mi_decl_restrict void* mi_new_n(size_t count, size_t size) {
  return mi_heap_alloc_new_n(mi_prim_get_default_heap(), count, size);
}

mi_decl_nodiscard mi_decl_restrict void* mi_new_nothrow(size_t size) mi_attr_noexcept {
  void* p = mi_malloc(size);
  if mi_unlikely(p == NULL) return mi_try_new(size, true);
  return p;
}

mi_decl_nodiscard mi_decl_restrict void* mi_new_aligned(size_t size, size_t alignment) {
  void* p;
  do {
    p = mi_malloc_aligned(size, alignment);
  } while (p == NULL && mi_try_new_handler(false));
  return p;
}

mi_decl_nodiscard mi_decl_restrict void* mi_new_aligned_nothrow(size_t size, size_t alignment) mi_attr_noexcept {
  void* p;
  do {
    p = mi_malloc_aligned(size, alignment);
  } while (p == NULL && mi_try_new_handler(true));
  return p;
}

mi_decl_nodiscard void* mi_new_realloc(void* p, size_t newsize) {
  void* q;
  do {
    q = mi_realloc(p, newsize);
  } while (q == NULL && mi_try_new_handler(false));
  return q;
}

mi_decl_nodiscard void* mi_new_reallocn(void* p, size_t newcount, size_t size) {
  size_t total;
  if mi_unlikely(mi_count_size_overflow(newcount, size, &total)) {
    mi_try_new_handler(false);
    return NULL;
  }
  else {
    return mi_new_realloc(p, total);
  }
}

#if MI_GUARDED
static void* mi_block_ptr_set_guarded(mi_block_t* block, size_t obj_size) {
  mi_page_t* const page = _mi_ptr_page(block);
  mi_page_set_has_aligned(page, true);
  block->next = MI_BLOCK_TAG_GUARDED;

  mi_segment_t* const segment = _mi_page_segment(page);
  const size_t block_size = mi_page_block_size(page);
  const size_t os_page_size = _mi_os_page_size();

  mi_assert_internal(block_size >= obj_size + os_page_size + sizeof(mi_block_t));
  if mi_unlikely(block_size < obj_size + os_page_size + sizeof(mi_block_t)) {
    mi_free(block);
    return NULL;
  }

  uint8_t* guard_page = (uint8_t*)block + block_size - os_page_size;
  mi_assert_internal(_mi_is_aligned(guard_page, os_page_size));

  if mi_likely(segment->allow_decommit && _mi_is_aligned(guard_page, os_page_size)) {
    const bool ok = _mi_os_protect(guard_page, os_page_size);
    if mi_unlikely(!ok) {
      _mi_warning_message("failed to set a guard page behind an object (object %p of size %zu)\n", block, block_size);
    }
  }
  else {
    _mi_warning_message("unable to set a guard page behind an object due to pinned memory (large OS pages?) (object %p of size %zu)\n", block, block_size);
  }

  size_t offset = block_size - os_page_size - obj_size;
  mi_assert_internal(offset > sizeof(mi_block_t));
  if (offset > MI_BLOCK_ALIGNMENT_MAX) {
    offset = MI_BLOCK_ALIGNMENT_MAX;
  }
  void* p = (uint8_t*)block + offset;
  mi_track_align(block, p, offset, obj_size);
  mi_track_mem_defined(block, sizeof(mi_block_t));
  return p;
}

mi_decl_restrict void* _mi_heap_malloc_guarded(mi_heap_t* heap, size_t size, bool zero) mi_attr_noexcept
{
#if defined(MI_PADDING_SIZE)
  mi_assert(MI_PADDING_SIZE == 0);
#endif
  const size_t os_page_size = _mi_os_page_size();
  const size_t obj_size = (mi_option_is_enabled(mi_option_guarded_precise) ? size : _mi_align_up(size, MI_MAX_ALIGN_SIZE));
  const size_t bsize    = _mi_align_up(_mi_align_up(obj_size, MI_MAX_ALIGN_SIZE) + sizeof(mi_block_t), MI_MAX_ALIGN_SIZE);
  const size_t req_size = _mi_align_up(bsize + os_page_size, os_page_size);

  mi_block_t* const block = (mi_block_t*)_mi_malloc_generic(heap, req_size, zero, 0, NULL);
  if mi_unlikely(block == NULL) return NULL;

  void* const p = mi_block_ptr_set_guarded(block, obj_size);

  mi_track_malloc(p, size, zero);
  if mi_likely(p != NULL) {
    if (!mi_heap_is_initialized(heap)) {
      heap = mi_prim_get_default_heap();
    }
#if MI_STAT>1
    mi_heap_stat_adjust_decrease(heap, malloc_requested, req_size);
    mi_heap_stat_increase(heap, malloc_requested, size);
#endif
    _mi_stat_counter_increase(&heap->tld->stats.malloc_guarded_count, 1);
  }
#if MI_DEBUG>3
  if (p != NULL && zero) {
    mi_assert_expensive(mi_mem_is_zero(p, size));
  }
#endif
  return p;
}
#endif

#ifdef __cplusplus
void* _mi_externs[] = {
  (void*)&_mi_page_malloc,
  (void*)&_mi_page_malloc_zero,
  (void*)&_mi_heap_malloc_zero,
  (void*)&_mi_heap_malloc_zero_ex,
  (void*)&mi_malloc,
  (void*)&mi_malloc_small,
  (void*)&mi_zalloc_small,
  (void*)&mi_heap_malloc,
  (void*)&mi_heap_zalloc,
  (void*)&mi_heap_malloc_small,
};
#endif
