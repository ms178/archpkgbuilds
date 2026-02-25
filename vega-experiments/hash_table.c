/*
 * Copyright © 2009,2012 Intel Corporation
 * Copyright © 1988-2004 Keith Packard and Bart Massey.
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors
 * or their institutions shall not be used in advertising or
 * otherwise to promote the sale, use or other dealings in this
 * Software without prior written authorization from the
 * authors.
 *
 * Authors:
 *   Eric Anholt <eric@anholt.net>
 *   Keith Packard <keithp@keithp.com>
 *
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <immintrin.h>

#include "hash_table.h"
#include "ralloc.h"
#include "macros.h"
#include "u_memory.h"
#include "fast_urem_by_const.h"
#include "util/u_memory.h"

#define XXH_INLINE_ALL
#include "xxhash.h"

#define DELETED_KEY_VALUE 1

static inline void *
uint_key(unsigned id)
{
   return (void *)(uintptr_t)id;
}

static const uint32_t deleted_key_value;

static const struct {
   uint32_t max_entries, size, rehash;
   uint64_t size_magic, rehash_magic;
} hash_sizes[] = {
#define ENTRY(max_entries, size, rehash) \
   { max_entries, size, rehash, REMAINDER_MAGIC(size), REMAINDER_MAGIC(rehash) }

   ENTRY(16, 19, 17 ),
   ENTRY(32, 43, 41 ),
   ENTRY(64, 73, 71 ),
   ENTRY(128, 151, 149 ),
   ENTRY(256, 283, 281 ),
   ENTRY(512, 571, 569 ),
   ENTRY(1024, 1153, 1151 ),
   ENTRY(2048, 2269, 2267 ),
   ENTRY(4096, 4519, 4517 ),
   ENTRY(8192, 9013, 9011 ),
   ENTRY(16384, 18043, 18041 ),
   ENTRY(32768, 36109, 36107 ),
   ENTRY(65536, 72091, 72089 ),
   ENTRY(131072, 144409, 144407 ),
   ENTRY(262144, 288361, 288359 ),
   ENTRY(524288, 576883, 576881 ),
   ENTRY(1048576, 1153459, 1153457 ),
   ENTRY(2097152, 2307163, 2307161 ),
   ENTRY(4194304, 4613893, 4613891 ),
   ENTRY(8388608, 9227641, 9227639 ),
   ENTRY(16777216, 18455029, 18455027 ),
   ENTRY(33554432, 36911011, 36911009 ),
   ENTRY(67108864, 73819861, 73819859 ),
   ENTRY(134217728, 147639589, 147639587 ),
   ENTRY(268435456, 295279081, 295279079 ),
   ENTRY(536870912, 590559793, 590559791 ),
   ENTRY(1073741824, 1181116273, 1181116271 ),
   ENTRY(2147483648ul, 2362232233ul, 2362232231ul )

#undef ENTRY
};

_Static_assert(ARRAY_SIZE(hash_sizes) == 28, "hash_sizes table size mismatch");

static inline bool
key_pointer_is_reserved(const struct hash_table *ht, const void *key)
{
   return key == NULL || key == ht->deleted_key;
}

static inline bool
entry_is_free(const struct hash_entry *entry)
{
   return entry->key == NULL;
}

static inline bool
entry_is_deleted(const struct hash_table *ht, const struct hash_entry *entry)
{
   return entry->key == ht->deleted_key;
}

static inline bool
entry_is_present(const struct hash_table *ht, const struct hash_entry *entry)
{
   return entry->key != NULL && entry->key != ht->deleted_key;
}

static void
_mesa_hash_table_initial_storage_destructor(void *data)
{
   struct hash_table *ht = (struct hash_table *)data;
   if (!ht->table)
      return;
   if (ht->table_destructor)
      ht->table_destructor(ht);
   ht->table = NULL;
}

static void
_mesa_hash_table_ralloc_table_destructor(void *data)
{
   void *parent = ralloc_parent(data);
   _mesa_hash_table_initial_storage_destructor(parent);
}

void
_mesa_hash_table_init(struct hash_table *ht,
                      void *mem_ctx,
                      uint32_t (*key_hash_function)(const void *key),
                      bool (*key_equals_function)(const void *a,
                                                  const void *b))
{
   ht->mem_ctx = mem_ctx;
   ht->size_index = 0;
   ht->size = hash_sizes[0].size;
   ht->rehash = hash_sizes[0].rehash;
   ht->size_magic = hash_sizes[0].size_magic;
   ht->rehash_magic = hash_sizes[0].rehash_magic;
   ht->max_entries = hash_sizes[0].max_entries;
   ht->key_hash_function = key_hash_function;
   ht->key_equals_function = key_equals_function;
   ht->table_destructor = NULL;
   assert(ht->size == ARRAY_SIZE(ht->_initial_storage));
   ht->table = ht->_initial_storage;
   memset(ht->table, 0, sizeof(ht->_initial_storage));
   ht->entries = 0;
   ht->deleted_entries = 0;
   ht->deleted_key = &deleted_key_value;
}

void
_mesa_hash_table_set_destructor(struct hash_table *ht,
                                void (*table_destructor)(void *data))
{
   ht->table_destructor = table_destructor;
   ralloc_set_destructor(ht, _mesa_hash_table_initial_storage_destructor);
   if (ht->table != ht->_initial_storage)
      ralloc_set_destructor(ht->table, _mesa_hash_table_ralloc_table_destructor);
}

struct hash_table *
_mesa_hash_table_create(void *mem_ctx,
                        uint32_t (*key_hash_function)(const void *key),
                        bool (*key_equals_function)(const void *a,
                                                    const void *b))
{
   struct hash_table *ht = ralloc(mem_ctx, struct hash_table);
   if (ht == NULL)
      return NULL;
   _mesa_hash_table_init(ht, ht, key_hash_function, key_equals_function);
   return ht;
}

static inline struct hash_entry *
hash_table_search_pointer(const struct hash_table *ht, uint32_t hash, const void *key)
{
   const uint32_t size = ht->size;
   const uint32_t start = util_fast_urem32(hash, size, ht->size_magic);
   const uint32_t step = 1 + util_fast_urem32(hash, ht->rehash, ht->rehash_magic);
   struct hash_entry * const table = ht->table;
   uint32_t addr = start;

   do {
      struct hash_entry *entry = table + addr;

      uint32_t next1 = addr + step;
      if (next1 >= size) next1 -= size;
      __builtin_prefetch(table + next1, 0, 3);

      uint32_t next2 = next1 + step;
      if (next2 >= size) next2 -= size;
      __builtin_prefetch(table + next2, 0, 2);

      if (likely(entry->key == NULL))
         return NULL;

      if (entry->hash == hash && entry->key != ht->deleted_key && entry->key == key)
         return entry;

      addr = next1;
   } while (addr != start);

   return NULL;
}

static struct hash_entry *
hash_table_search(const struct hash_table *ht, uint32_t hash, const void *key)
{
   assert(!key_pointer_is_reserved(ht, key));

   if (ht->key_equals_function == _mesa_key_pointer_equal)
      return hash_table_search_pointer(ht, hash, key);

   const uint32_t size = ht->size;
   const uint32_t start_hash_address = util_fast_urem32(hash, size, ht->size_magic);
   const uint32_t double_hash = 1 + util_fast_urem32(hash, ht->rehash, ht->rehash_magic);
   uint32_t hash_address = start_hash_address;

   struct hash_entry * const table = ht->table;
   const void * const deleted_key = ht->deleted_key;
   bool (* const key_equals)(const void *, const void *) = ht->key_equals_function;

   do {
      struct hash_entry *entry = table + hash_address;

      uint32_t next1 = hash_address + double_hash;
      if (next1 >= size) next1 -= size;
      __builtin_prefetch(table + next1, 0, 3);

      uint32_t next2 = next1 + double_hash;
      if (next2 >= size) next2 -= size;
      __builtin_prefetch(table + next2, 0, 2);

      const void *entry_key = entry->key;
      if (likely(entry_key == NULL))
         return NULL;

      if (entry->hash == hash &&
          entry_key != deleted_key &&
          key_equals(key, entry_key))
         return entry;

      hash_address = next1;
   } while (hash_address != start_hash_address);

   return NULL;
}

struct hash_entry *
_mesa_hash_table_search(const struct hash_table *ht, const void *key)
{
   assert(ht->key_hash_function != NULL);
   return hash_table_search(ht, ht->key_hash_function(key), key);
}

struct hash_entry *
_mesa_hash_table_search_pre_hashed(struct hash_table *ht, uint32_t hash,
                                   const void *key)
{
   assert(ht->key_hash_function == NULL || hash == ht->key_hash_function(key));
   return hash_table_search(ht, hash, key);
}

static void
hash_table_insert_rehash(struct hash_table *ht, uint32_t hash,
                         const void *key, void *data)
{
   const uint32_t size = ht->size;
   const uint32_t start = util_fast_urem32(hash, size, ht->size_magic);
   const uint32_t step = 1 + util_fast_urem32(hash, ht->rehash, ht->rehash_magic);
   struct hash_entry * const table = ht->table;
   uint32_t addr = start;

   do {
      struct hash_entry *entry = table + addr;
      if (likely(entry->key == NULL)) {
         entry->hash = hash;
         entry->key = key;
         entry->data = data;
         return;
      }
      addr += step;
      if (addr >= size) addr -= size;
   } while (true);
}

static void
hash_table_clear_fast(struct hash_table *ht)
{
   size_t bytes = (size_t)ht->size * sizeof(struct hash_entry);
   if (bytes >= 256 && __builtin_cpu_supports("avx2")) {
      __m256i zero = _mm256_setzero_si256();
      char *p = (char *)ht->table;
      size_t i;
      for (i = 0; i + 32 <= bytes; i += 32)
         _mm256_storeu_si256((__m256i *)(p + i), zero);
      if (i < bytes)
         memset(p + i, 0, bytes - i);
   } else {
      memset(ht->table, 0, bytes);
   }
   ht->entries = 0;
   ht->deleted_entries = 0;
}

void
_mesa_hash_table_clear(struct hash_table *ht,
                       void (*delete_function)(struct hash_entry *entry))
{
   if (ht == NULL)
      return;

   if (delete_function != NULL) {
      struct hash_entry * const table_end = ht->table + ht->size;
      for (struct hash_entry *entry = ht->table; entry != table_end; ++entry) {
         if (entry_is_present(ht, entry))
            delete_function(entry);
         entry->key = NULL;
      }
      ht->entries = 0;
      ht->deleted_entries = 0;
   } else {
      hash_table_clear_fast(ht);
   }
}

void
_mesa_hash_table_set_deleted_key(struct hash_table *ht, const void *deleted_key)
{
   ht->deleted_key = deleted_key;
}

static void
_mesa_hash_table_rehash(struct hash_table *ht, unsigned new_size_index)
{
   struct hash_table old_ht;
   struct hash_entry *table;

   if (ht->size_index == new_size_index && ht->deleted_entries == ht->max_entries) {
      hash_table_clear_fast(ht);
      assert(!ht->entries);
      return;
   }

   if (new_size_index >= ARRAY_SIZE(hash_sizes))
      return;

   table = rzalloc_array(ht->mem_ctx, struct hash_entry,
                         hash_sizes[new_size_index].size);
   if (table == NULL)
      return;

   if (ht->table_destructor != NULL)
      ralloc_set_destructor(table, _mesa_hash_table_ralloc_table_destructor);

   if (ht->table == ht->_initial_storage) {
      old_ht = *ht;
      old_ht.table = old_ht._initial_storage;
   } else {
      memcpy(&old_ht, ht, offsetof(struct hash_table, _initial_storage));
   }

   ht->table = table;
   ht->size_index = new_size_index;
   ht->size = hash_sizes[new_size_index].size;
   ht->rehash = hash_sizes[new_size_index].rehash;
   ht->size_magic = hash_sizes[new_size_index].size_magic;
   ht->rehash_magic = hash_sizes[new_size_index].rehash_magic;
   ht->max_entries = hash_sizes[new_size_index].max_entries;
   ht->entries = 0;
   ht->deleted_entries = 0;

   hash_table_foreach(&old_ht, entry) {
      hash_table_insert_rehash(ht, entry->hash, entry->key, entry->data);
   }

   ht->entries = old_ht.entries;

   if (old_ht.table != old_ht._initial_storage) {
      if (ht->table_destructor != NULL)
         ralloc_set_destructor(old_ht.table, NULL);
      ralloc_free(old_ht.table);
   }
}

static struct hash_entry *
hash_table_get_entry(struct hash_table *ht, uint32_t hash, const void *key)
{
   struct hash_entry *available_entry = NULL;

   assert(!key_pointer_is_reserved(ht, key));

   if (ht->entries >= ht->max_entries) {
      _mesa_hash_table_rehash(ht, ht->size_index + 1);
   } else if (ht->deleted_entries + ht->entries >= ht->max_entries) {
      _mesa_hash_table_rehash(ht, ht->size_index);
   }

   const uint32_t size = ht->size;
   const uint32_t start_hash_address = util_fast_urem32(hash, size, ht->size_magic);
   const uint32_t double_hash = 1 + util_fast_urem32(hash, ht->rehash,
                                                     ht->rehash_magic);
   uint32_t hash_address = start_hash_address;

   struct hash_entry * const table = ht->table;
   const void * const deleted_key = ht->deleted_key;
   bool (* const key_equals)(const void *, const void *) = ht->key_equals_function;

   do {
      struct hash_entry *entry = table + hash_address;

      uint32_t next1 = hash_address + double_hash;
      if (next1 >= size) next1 -= size;
      __builtin_prefetch(table + next1, 1, 3);

      uint32_t next2 = next1 + double_hash;
      if (next2 >= size) next2 -= size;
      __builtin_prefetch(table + next2, 1, 2);

      const void *entry_key = entry->key;

      if (entry_key == NULL || entry_key == deleted_key) {
         if (available_entry == NULL)
            available_entry = entry;
         if (likely(entry_key == NULL))
            break;
      } else if (entry->hash == hash && key_equals(key, entry_key)) {
         return entry;
      }

      hash_address = next1;
   } while (hash_address != start_hash_address);

   if (available_entry != NULL) {
      if (available_entry->key == deleted_key)
         ht->deleted_entries--;
      available_entry->hash = hash;
      ht->entries++;
      return available_entry;
   }

   return NULL;
}

static struct hash_entry *
hash_table_insert(struct hash_table *ht, uint32_t hash,
                  const void *key, void *data)
{
   struct hash_entry *entry = hash_table_get_entry(ht, hash, key);
   if (entry != NULL) {
      entry->key = key;
      entry->data = data;
   }
   return entry;
}

struct hash_entry *
_mesa_hash_table_insert(struct hash_table *ht, const void *key, void *data)
{
   assert(ht->key_hash_function != NULL);
   return hash_table_insert(ht, ht->key_hash_function(key), key, data);
}

struct hash_entry *
_mesa_hash_table_insert_pre_hashed(struct hash_table *ht, uint32_t hash,
                                   const void *key, void *data)
{
   assert(ht->key_hash_function == NULL || hash == ht->key_hash_function(key));
   return hash_table_insert(ht, hash, key, data);
}

void
_mesa_hash_table_remove(struct hash_table *ht, struct hash_entry *entry)
{
   if (entry == NULL)
      return;
   entry->key = ht->deleted_key;
   ht->entries--;
   ht->deleted_entries++;
}

void
_mesa_hash_table_remove_key(struct hash_table *ht, const void *key)
{
   _mesa_hash_table_remove(ht, _mesa_hash_table_search(ht, key));
}

struct hash_entry *
_mesa_hash_table_next_entry_unsafe(const struct hash_table *ht,
                                   struct hash_entry *entry)
{
   assert(!ht->deleted_entries);
   if (ht->entries == 0)
      return NULL;
   if (entry == NULL)
      entry = ht->table;
   else
      entry++;
   struct hash_entry * const end = ht->table + ht->size;
   while (entry != end) {
      if (entry->key != NULL)
         return entry;
      entry++;
   }
   return NULL;
}

struct hash_entry *
_mesa_hash_table_next_entry(struct hash_table *ht, struct hash_entry *entry)
{
   if (entry == NULL)
      entry = ht->table;
   else
      entry++;
   struct hash_entry * const end = ht->table + ht->size;
   for (; entry != end; ++entry) {
      if (entry_is_present(ht, entry))
         return entry;
   }
   return NULL;
}

struct hash_entry *
_mesa_hash_table_random_entry(struct hash_table *ht,
                              bool (*predicate)(struct hash_entry *entry))
{
   if (ht->entries == 0)
      return NULL;

   uint32_t i = (uint32_t)rand() % ht->size;
   struct hash_entry *entry;

   for (entry = ht->table + i; entry != ht->table + ht->size; ++entry) {
      if (entry_is_present(ht, entry) &&
          (predicate == NULL || predicate(entry)))
         return entry;
   }
   for (entry = ht->table; entry != ht->table + i; ++entry) {
      if (entry_is_present(ht, entry) &&
          (predicate == NULL || predicate(entry)))
         return entry;
   }
   return NULL;
}

uint32_t
_mesa_hash_data(const void *data, size_t size)
{
   return XXH32(data, size, 0);
}

uint32_t
_mesa_hash_data_with_seed(const void *data, size_t size, uint32_t seed)
{
   return XXH32(data, size, seed);
}

uint32_t
_mesa_hash_int(const void *key)
{
   uint32_t x;
   memcpy(&x, key, sizeof(x));
   x ^= x >> 16;
   x *= 0x85ebca6bU;
   x ^= x >> 13;
   x *= 0xc2b2ae35U;
   x ^= x >> 16;
   return x;
}

uint32_t _mesa_hash_uint(const void *key) { return _mesa_hash_int(key); }
uint32_t _mesa_hash_u32(const void *key)  { return _mesa_hash_int(key); }

uint32_t
_mesa_hash_u64(const void *key)
{
   uint64_t x;
   memcpy(&x, key, 8);
   x ^= x >> 30;
   x *= 0xbf58476d1ce4e5b9ULL;
   x ^= x >> 27;
   x *= 0x94d049bb133111ebULL;
   x ^= x >> 31;
   return (uint32_t)(x ^ (x >> 32));
}

uint32_t
_mesa_hash_string(const void *_key)
{
   return _mesa_hash_string_with_length(_key, (unsigned)strlen((const char *)_key));
}

uint32_t
_mesa_hash_string_with_length(const void *_key, unsigned length)
{
   const char *key = _key;
#if defined(_WIN64) || defined(__x86_64__)
   return (uint32_t)XXH64(key, length, 0);
#else
   return XXH32(key, length, 0);
#endif
}

uint32_t
_mesa_hash_pointer(const void *pointer)
{
   uintptr_t num = (uintptr_t)pointer;
#if UINTPTR_MAX == UINT64_MAX
   if (__builtin_cpu_supports("bmi2")) {
      uint64_t x = _pext_u64(num, 0xAAAAAAAAAAAAAAAAULL);
      x ^= x >> 30;
      x *= 0xbf58476d1ce4e5b9ULL;
      x ^= x >> 27;
      x *= 0x94d049bb133111ebULL;
      x ^= x >> 31;
      return (uint32_t)(x ^ (x >> 32));
   }
#endif
   uint64_t x = (uint64_t)num;
   x ^= x >> 30;
   x *= 0xbf58476d1ce4e5b9ULL;
   x ^= x >> 27;
   x *= 0x94d049bb133111ebULL;
   x ^= x >> 31;
   return (uint32_t)(x ^ (x >> 32));
}

bool _mesa_key_int_equal(const void *a, const void *b) { return *((const int *)a) == *((const int *)b); }
bool _mesa_key_uint_equal(const void *a, const void *b) { return *((const unsigned *)a) == *((const unsigned *)b); }
bool _mesa_key_u32_equal(const void *a, const void *b) { return *((const uint32_t *)a) == *((const uint32_t *)b); }
bool _mesa_key_u64_equal(const void *a, const void *b) { return *((const uint64_t *)a) == *((const uint64_t *)b); }
bool _mesa_key_string_equal(const void *a, const void *b) { return strcmp(a, b) == 0; }
bool _mesa_key_pointer_equal(const void *a, const void *b) { return a == b; }

struct hash_table *
_mesa_pointer_hash_table_create(void *mem_ctx)
{
   return _mesa_hash_table_create(mem_ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
}

void
_mesa_pointer_hash_table_init(struct hash_table *ht, void *mem_ctx)
{
   _mesa_hash_table_init(ht, mem_ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
}

struct hash_table *
_mesa_string_hash_table_create(void *mem_ctx)
{
   return _mesa_hash_table_create(mem_ctx, _mesa_hash_string, _mesa_key_string_equal);
}

void
_mesa_string_hash_table_init(struct hash_table *ht, void *mem_ctx)
{
   _mesa_hash_table_init(ht, mem_ctx, _mesa_hash_string, _mesa_key_string_equal);
}

bool
_mesa_hash_table_reserve(struct hash_table *ht, unsigned size)
{
   if (size <= ht->max_entries)
      return true;
   for (unsigned i = ht->size_index + 1; i < ARRAY_SIZE(hash_sizes); ++i) {
      if (hash_sizes[i].max_entries >= size) {
         _mesa_hash_table_rehash(ht, i);
         break;
      }
   }
   return ht->max_entries >= size;
}

/* === MISSING FUNCTIONS RESTORED (fixes linker error) === */

bool
_mesa_hash_table_copy(struct hash_table *dst, struct hash_table *src,
                      void *dst_mem_ctx)
{
   memcpy(dst, src, offsetof(struct hash_table, _initial_storage));
   dst->mem_ctx = dst_mem_ctx;
   dst->table_destructor = NULL;

   if (src->table != src->_initial_storage) {
      dst->table = ralloc_array(dst_mem_ctx, struct hash_entry, dst->size);
      if (dst->table == NULL)
         return false;
      memcpy(dst->table, src->table, (size_t)dst->size * sizeof(struct hash_entry));
   } else {
      dst->table = dst->_initial_storage;
      memcpy(dst->table, src->_initial_storage, sizeof(src->_initial_storage));
   }
   return true;
}

struct hash_table *
_mesa_hash_table_clone(struct hash_table *src, void *dst_mem_ctx)
{
   struct hash_table *ht = ralloc(dst_mem_ctx, struct hash_table);
   if (ht == NULL)
      return NULL;
   if (!_mesa_hash_table_copy(ht, src, dst_mem_ctx)) {
      ralloc_free(ht);
      return NULL;
   }
   return ht;
}

void
_mesa_hash_table_fini(struct hash_table *ht,
                      void (*delete_function)(struct hash_entry *entry))
{
   if (delete_function != NULL) {
      hash_table_foreach(ht, entry) {
         delete_function(entry);
      }
   }
   if (ht->table != ht->_initial_storage) {
      if (ht->table_destructor != NULL)
         ralloc_set_destructor(ht->table, NULL);
      ralloc_free(ht->table);
   }
   ht->table = NULL;
}

void
_mesa_hash_table_destroy(struct hash_table *ht,
                         void (*delete_function)(struct hash_entry *entry))
{
   if (ht == NULL)
      return;
   _mesa_hash_table_fini(ht, delete_function);
   ralloc_free(ht);
}

/* === u64 wrapper (upstream-patch exact) === */

struct hash_key_u64 { uint64_t value; };

static uint32_t key_u64_hash(const void *key) { return _mesa_hash_data(key, sizeof(struct hash_key_u64)); }
static bool key_u64_equals(const void *a, const void *b)
{
   const struct hash_key_u64 *aa = a, *bb = b;
   return aa->value == bb->value;
}

#define FREED_KEY_VALUE 0

static void
_mesa_hash_table_u64_delete_keys(void *data)
{
   struct hash_table_u64 *ht = (struct hash_table_u64 *)data;
   _mesa_hash_table_u64_clear(ht);
}

struct hash_table_u64 *
_mesa_hash_table_u64_create(void *mem_ctx)
{
   _Static_assert(FREED_KEY_VALUE != DELETED_KEY_VALUE, "key collision");
   struct hash_table_u64 *ht = rzalloc(mem_ctx, struct hash_table_u64);
   if (ht == NULL)
      return NULL;

   if (sizeof(void *) == 8) {
      _mesa_hash_table_init(&ht->table, ht, _mesa_hash_pointer, _mesa_key_pointer_equal);
   } else {
      _mesa_hash_table_init(&ht->table, ht, key_u64_hash, key_u64_equals);
      _mesa_hash_table_set_destructor(&ht->table, _mesa_hash_table_u64_delete_keys);
   }

   _mesa_hash_table_set_deleted_key(&ht->table, uint_key(DELETED_KEY_VALUE));
   return ht;
}

static void
_mesa_hash_table_u64_delete_key(struct hash_entry *entry)
{
   if (sizeof(void *) == 8)
      return;
   struct hash_key_u64 *_key = (struct hash_key_u64 *)entry->key;
   FREE(_key);
}

void
_mesa_hash_table_u64_clear(struct hash_table_u64 *ht)
{
   if (ht == NULL)
      return;
   _mesa_hash_table_clear(&ht->table, _mesa_hash_table_u64_delete_key);
   ht->freed_key_data = NULL;
   ht->deleted_key_data = NULL;
}

void
_mesa_hash_table_u64_destroy(struct hash_table_u64 *ht)
{
   ralloc_free(ht);
}

void
_mesa_hash_table_u64_insert(struct hash_table_u64 *ht, uint64_t key, void *data)
{
   if (key == FREED_KEY_VALUE) { ht->freed_key_data = data; return; }
   if (key == DELETED_KEY_VALUE) { ht->deleted_key_data = data; return; }

   if (sizeof(void *) == 8) {
      _mesa_hash_table_insert(&ht->table, (void *)(uintptr_t)key, data);
   } else {
      struct hash_key_u64 *_key = CALLOC_STRUCT(hash_key_u64);
      if (_key == NULL) return;
      _key->value = key;
      struct hash_entry *entry = hash_table_get_entry(&ht->table, key_u64_hash(_key), _key);
      if (entry == NULL) { FREE(_key); return; }
      entry->data = data;
      if (!entry_is_present(&ht->table, entry))
         entry->key = _key;
      else
         FREE(_key);
   }
}

static struct hash_entry *
hash_table_u64_search(struct hash_table_u64 *ht, uint64_t key)
{
   if (sizeof(void *) == 8)
      return _mesa_hash_table_search(&ht->table, (void *)(uintptr_t)key);
   struct hash_key_u64 _key = { .value = key };
   return _mesa_hash_table_search(&ht->table, &_key);
}

void *
_mesa_hash_table_u64_search(struct hash_table_u64 *ht, uint64_t key)
{
   struct hash_entry *entry;
   if (key == FREED_KEY_VALUE) return ht->freed_key_data;
   if (key == DELETED_KEY_VALUE) return ht->deleted_key_data;
   entry = hash_table_u64_search(ht, key);
   return entry ? entry->data : NULL;
}

void
_mesa_hash_table_u64_remove(struct hash_table_u64 *ht, uint64_t key)
{
   struct hash_entry *entry;
   if (key == FREED_KEY_VALUE) { ht->freed_key_data = NULL; return; }
   if (key == DELETED_KEY_VALUE) { ht->deleted_key_data = NULL; return; }
   entry = hash_table_u64_search(ht, key);
   if (entry == NULL) return;
   if (sizeof(void *) == 8) {
      _mesa_hash_table_remove(&ht->table, entry);
   } else {
      struct hash_key_u64 *_key = (struct hash_key_u64 *)entry->key;
      _mesa_hash_table_remove(&ht->table, entry);
      FREE(_key);
   }
}

struct hash_entry_u64
_mesa_hash_table_u64_next_entry(struct hash_table_u64 *ht,
                                struct hash_entry_u64 *ent)
{
   if (ent == NULL && ht->freed_key_data != NULL)
      return (struct hash_entry_u64){ .key = FREED_KEY_VALUE, .data = ht->freed_key_data, ._entry = NULL };

   if ((ent == NULL || ent->key == FREED_KEY_VALUE) && ht->deleted_key_data != NULL)
      return (struct hash_entry_u64){ .key = DELETED_KEY_VALUE, .data = ht->deleted_key_data, ._entry = NULL };

   struct hash_entry *next = _mesa_hash_table_next_entry(&ht->table,
                                                         ent != NULL ? ent->_entry : NULL);
   if (next == NULL)
      return (struct hash_entry_u64){ .key = 0, .data = NULL, ._entry = NULL };

   uint64_t key = (sizeof(void *) == 8) ? (uintptr_t)next->key : ((const struct hash_key_u64 *)next->key)->value;
   return (struct hash_entry_u64){ .key = key, .data = next->data, ._entry = next };
}

void
_mesa_hash_table_u64_replace(struct hash_table_u64 *ht,
                             const struct hash_entry_u64 *ent,
                             void *new_data)
{
   if (ent->_entry != NULL) {
      ent->_entry->data = new_data;
   } else if (ent->key == FREED_KEY_VALUE) {
      ht->freed_key_data = new_data;
   } else {
      assert(ent->key == DELETED_KEY_VALUE);
      ht->deleted_key_data = new_data;
   }
}
