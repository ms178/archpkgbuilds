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

#ifdef ENABLE_SHADER_CACHE

#include <ctype.h>
#include <ftw.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <inttypes.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#if !defined(_WIN32) && !defined(__CYGWIN__)
#include <unistd.h>
#include <pthread.h>
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#if (defined(__x86_64__) || defined(_M_X64))
#include <emmintrin.h>
#include <immintrin.h>
#if defined(__GNUC__) || defined(__clang__)
#include <x86intrin.h>
#endif
#endif

#include "util/compress.h"
#include "util/crc32.h"
#include "util/u_debug.h"
#include "util/rand_xor.h"
#include "util/u_atomic.h"
#include "util/mesa-sha1.h"
#include "util/perf/cpu_trace.h"
#include "util/ralloc.h"
#include "util/compiler.h"
#include "util/log.h"

#include "disk_cache.h"
#include "disk_cache_os.h"

#define CACHE_VERSION 1

#define DRV_KEY_CPY(_dst, _src, _src_size) \
do {                                       \
   memcpy(_dst, _src, _src_size);          \
   _dst += _src_size;                      \
} while (0)

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#if __has_builtin(__builtin_prefetch)
#define PREFETCH_R(addr) __builtin_prefetch((addr), 0, 1)
#else
#define PREFETCH_R(addr) ((void)0)
#endif

#if defined(__GNUC__) || defined(__clang__)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
#endif

/* -----------------------------------------------------------------------------
 * Thread-local buffer management
 * ---------------------------------------------------------------------------*/
struct tls_in_buf {
   uint8_t *p;
   size_t cap;
};

struct tls_out_buf {
   uint8_t *p;
   size_t cap;
};

#if !defined(_WIN32) && !defined(__CYGWIN__)
static pthread_key_t g_tls_in_key;
static pthread_key_t g_tls_out_key;
static pthread_once_t g_tls_once = PTHREAD_ONCE_INIT;

static void
tls_in_destructor(void *ptr)
{
   struct tls_in_buf *b = (struct tls_in_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static void
tls_out_destructor(void *ptr)
{
   struct tls_out_buf *b = (struct tls_out_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static void
tls_make_keys(void)
{
   int r1 = pthread_key_create(&g_tls_in_key, tls_in_destructor);
   int r2 = pthread_key_create(&g_tls_out_key, tls_out_destructor);
   (void)r1;
   (void)r2;
}

static struct tls_in_buf *
tls_get_in(void)
{
   pthread_once(&g_tls_once, tls_make_keys);
   struct tls_in_buf *b = (struct tls_in_buf *)pthread_getspecific(g_tls_in_key);
   if (UNLIKELY(!b)) {
      b = (struct tls_in_buf *)malloc(sizeof(*b));
      if (UNLIKELY(!b)) {
         return NULL;
      }
      b->cap = 128 * 1024;
      b->p = (uint8_t *)malloc(b->cap);
      if (UNLIKELY(!b->p)) {
         free(b);
         return NULL;
      }
      if (UNLIKELY(pthread_setspecific(g_tls_in_key, b) != 0)) {
         free(b->p);
         free(b);
         return NULL;
      }
   }
   return b;
}

static struct tls_out_buf *
tls_get_out(void)
{
   pthread_once(&g_tls_once, tls_make_keys);
   struct tls_out_buf *b = (struct tls_out_buf *)pthread_getspecific(g_tls_out_key);
   if (UNLIKELY(!b)) {
      b = (struct tls_out_buf *)malloc(sizeof(*b));
      if (UNLIKELY(!b)) {
         return NULL;
      }
      b->p = NULL;
      b->cap = 0;
      if (UNLIKELY(pthread_setspecific(g_tls_out_key, b) != 0)) {
         free(b);
         return NULL;
      }
   }
   return b;
}

#else /* Windows */

static DWORD g_tls_in_index = TLS_OUT_OF_INDEXES;
static DWORD g_tls_out_index = TLS_OUT_OF_INDEXES;
static INIT_ONCE g_tls_once = INIT_ONCE_STATIC_INIT;
static bool g_use_fls = false;

static void
tls_in_destructor_win(void *ptr)
{
   struct tls_in_buf *b = (struct tls_in_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static void
tls_out_destructor_win(void *ptr)
{
   struct tls_out_buf *b = (struct tls_out_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static BOOL CALLBACK
tls_make_keys_win(PINIT_ONCE once, PVOID param, PVOID *ctx)
{
   (void)once;
   (void)param;
   (void)ctx;

#if (_WIN32_WINNT >= 0x0600)
   DWORD fls_in = FlsAlloc((PFLS_CALLBACK_FUNCTION)tls_in_destructor_win);
   DWORD fls_out = FlsAlloc((PFLS_CALLBACK_FUNCTION)tls_out_destructor_win);

   if (fls_in != FLS_OUT_OF_INDEXES && fls_out != FLS_OUT_OF_INDEXES) {
      g_tls_in_index = fls_in;
      g_tls_out_index = fls_out;
      g_use_fls = true;
      return TRUE;
   }

   if (fls_in != FLS_OUT_OF_INDEXES) {
      FlsFree(fls_in);
   }
   if (fls_out != FLS_OUT_OF_INDEXES) {
      FlsFree(fls_out);
   }
#endif

   g_tls_in_index = TlsAlloc();
   g_tls_out_index = TlsAlloc();
   g_use_fls = false;

   return TRUE;
}

static struct tls_in_buf *
tls_get_in(void)
{
   InitOnceExecuteOnce(&g_tls_once, tls_make_keys_win, NULL, NULL);
   if (UNLIKELY(g_tls_in_index == TLS_OUT_OF_INDEXES)) {
      return NULL;
   }

   void *ptr;
#if (_WIN32_WINNT >= 0x0600)
   if (g_use_fls) {
      ptr = FlsGetValue(g_tls_in_index);
   } else {
      ptr = TlsGetValue(g_tls_in_index);
   }
#else
   ptr = TlsGetValue(g_tls_in_index);
#endif

   struct tls_in_buf *b = (struct tls_in_buf *)ptr;
   if (UNLIKELY(!b)) {
      b = (struct tls_in_buf *)malloc(sizeof(*b));
      if (UNLIKELY(!b)) {
         return NULL;
      }
      b->cap = 128 * 1024;
      b->p = (uint8_t *)malloc(b->cap);
      if (UNLIKELY(!b->p)) {
         free(b);
         return NULL;
      }

      BOOL ok;
#if (_WIN32_WINNT >= 0x0600)
      if (g_use_fls) {
         ok = FlsSetValue(g_tls_in_index, b);
      } else {
         ok = TlsSetValue(g_tls_in_index, b);
      }
#else
      ok = TlsSetValue(g_tls_in_index, b);
#endif

      if (UNLIKELY(!ok)) {
         free(b->p);
         free(b);
         return NULL;
      }
   }
   return b;
}

static struct tls_out_buf *
tls_get_out(void)
{
   InitOnceExecuteOnce(&g_tls_once, tls_make_keys_win, NULL, NULL);
   if (UNLIKELY(g_tls_out_index == TLS_OUT_OF_INDEXES)) {
      return NULL;
   }

   void *ptr;
#if (_WIN32_WINNT >= 0x0600)
   if (g_use_fls) {
      ptr = FlsGetValue(g_tls_out_index);
   } else {
      ptr = TlsGetValue(g_tls_out_index);
   }
#else
   ptr = TlsGetValue(g_tls_out_index);
#endif

   struct tls_out_buf *b = (struct tls_out_buf *)ptr;
   if (UNLIKELY(!b)) {
      b = (struct tls_out_buf *)malloc(sizeof(*b));
      if (UNLIKELY(!b)) {
         return NULL;
      }
      b->p = NULL;
      b->cap = 0;

      BOOL ok;
#if (_WIN32_WINNT >= 0x0600)
      if (g_use_fls) {
         ok = FlsSetValue(g_tls_out_index, b);
      } else {
         ok = TlsSetValue(g_tls_out_index, b);
      }
#else
      ok = TlsSetValue(g_tls_out_index, b);
#endif

      if (UNLIKELY(!ok)) {
         free(b);
         return NULL;
      }
   }
   return b;
}

#endif /* !POSIX / Windows */

/* -----------------------------------------------------------------------------
 * Queue initialization
 * ---------------------------------------------------------------------------*/
static bool
disk_cache_init_queue(struct disk_cache *cache)
{
   if (LIKELY(util_queue_is_initialized(&cache->cache_queue))) {
      return true;
   }

   int logical = 4;
#if defined(_SC_NPROCESSORS_ONLN)
   long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
   if (nprocs > 0 && nprocs < INT_MAX) {
      logical = (int)nprocs;
   }
#elif defined(_WIN32)
   SYSTEM_INFO sysinfo;
   GetSystemInfo(&sysinfo);
   logical = (int)sysinfo.dwNumberOfProcessors;
   if (logical > 128) {
      logical = 128;
   }
#endif

   int threads = (logical * 3 + 1) / 4;
   if (threads < 4) {
      threads = 4;
   }
   if (threads > 16) {
      threads = 16;
   }

   unsigned queue_depth = 128;

   return util_queue_init(&cache->cache_queue, "disk$", queue_depth, threads,
                          UTIL_QUEUE_INIT_RESIZE_IF_FULL |
                          UTIL_QUEUE_INIT_USE_MINIMUM_PRIORITY |
                          UTIL_QUEUE_INIT_SET_FULL_THREAD_AFFINITY, NULL);
}

/* -----------------------------------------------------------------------------
 * Cache creation and destruction
 * ---------------------------------------------------------------------------*/
static struct disk_cache *
disk_cache_type_create(const char *gpu_name,
                       const char *driver_id,
                       const char *cache_dir_name,
                       uint64_t driver_flags,
                       enum disk_cache_type cache_type,
                       uint64_t max_size)
{
   void *local;
   struct disk_cache *cache = NULL;

   uint8_t cache_version = CACHE_VERSION;
   size_t cv_size = sizeof(cache_version);

   local = ralloc_context(NULL);
   if (UNLIKELY(local == NULL)) {
      goto fail;
   }

   cache = rzalloc(NULL, struct disk_cache);
   if (UNLIKELY(cache == NULL)) {
      goto fail;
   }

   cache->path_init_failed = true;
   cache->type = DISK_CACHE_NONE;

   if (!disk_cache_enabled()) {
      goto path_fail;
   }

   const char *path =
      disk_cache_generate_cache_dir(local, gpu_name, driver_id, cache_dir_name, cache_type, true);
   if (!path) {
      goto path_fail;
   }

   cache->path = ralloc_strdup(cache, path);
   if (UNLIKELY(cache->path == NULL)) {
      goto path_fail;
   }

   if (strcmp(driver_id, "make_check_uncompressed") == 0) {
      cache->compression_disabled = true;
   }

   if (cache_type == DISK_CACHE_SINGLE_FILE) {
      if (!disk_cache_load_cache_index_foz(local, cache)) {
         goto path_fail;
      }
   } else if (cache_type == DISK_CACHE_DATABASE) {
      if (!disk_cache_db_load_cache_index(local, cache)) {
         goto path_fail;
      }
   }

   if (!os_get_option("MESA_SHADER_CACHE_DIR") && !os_get_option("MESA_GLSL_CACHE_DIR")) {
      disk_cache_touch_cache_user_marker(cache->path);
   }

   cache->type = cache_type;

   cache->stats.enabled = debug_get_bool_option("MESA_SHADER_CACHE_SHOW_STATS", false);

   if (!disk_cache_mmap_cache_index(local, cache)) {
      goto path_fail;
   }

   cache->max_size = max_size;

   if (cache->type == DISK_CACHE_DATABASE) {
      mesa_cache_db_multipart_set_size_limit(&cache->cache_db, cache->max_size);
   }

   if (!disk_cache_init_queue(cache)) {
      goto fail;
   }

   cache->path_init_failed = false;

 path_fail:

   cache->driver_keys_blob_size = cv_size;

   size_t id_size = strlen(driver_id) + 1;
   size_t gpu_name_size = strlen(gpu_name) + 1;
   cache->driver_keys_blob_size += id_size;
   cache->driver_keys_blob_size += gpu_name_size;

   uint8_t ptr_size = sizeof(void *);
   size_t ptr_size_size = sizeof(ptr_size);
   cache->driver_keys_blob_size += ptr_size_size;

   size_t driver_flags_size = sizeof(driver_flags);
   cache->driver_keys_blob_size += driver_flags_size;

   cache->driver_keys_blob = ralloc_size(cache, cache->driver_keys_blob_size);
   if (UNLIKELY(!cache->driver_keys_blob)) {
      goto fail;
   }

   uint8_t *drv_key_blob = cache->driver_keys_blob;
   DRV_KEY_CPY(drv_key_blob, &cache_version, cv_size);
   DRV_KEY_CPY(drv_key_blob, driver_id, id_size);
   DRV_KEY_CPY(drv_key_blob, gpu_name, gpu_name_size);
   DRV_KEY_CPY(drv_key_blob, &ptr_size, ptr_size_size);
   DRV_KEY_CPY(drv_key_blob, &driver_flags, driver_flags_size);

   s_rand_xorshift128plus(cache->seed_xorshift128plus, true);

   ralloc_free(local);

   return cache;

 fail:
   if (cache) {
      if (cache->type == DISK_CACHE_SINGLE_FILE) {
         foz_destroy(&cache->foz_db);
      } else if (cache->type == DISK_CACHE_DATABASE) {
         mesa_cache_db_multipart_close(&cache->cache_db);
      }
      disk_cache_destroy_mmap(cache);
      ralloc_free(cache);
   }
   ralloc_free(local);

   return NULL;
}

static void
background_delete_old_cache(void *job, void *gdata, int thread_index)
{
   (void)job;
   (void)gdata;
   (void)thread_index;
   disk_cache_delete_old_cache();
}

struct fence_job {
   struct util_queue_fence fence;
};

static void
trivial_job_free(void *job, void *gdata, int thread_index)
{
   (void)gdata;
   (void)thread_index;
   free(job);
}

struct disk_cache *
disk_cache_create(const char *gpu_name, const char *driver_id,
                  uint64_t driver_flags)
{
   enum disk_cache_type cache_type;
   struct disk_cache *cache;
   uint64_t max_size = 0;
   const char *max_size_str;

   if (debug_get_bool_option("MESA_DISK_CACHE_SINGLE_FILE", false)) {
      cache_type = DISK_CACHE_SINGLE_FILE;
   } else if (debug_get_bool_option("MESA_DISK_CACHE_DATABASE", false)) {
      cache_type = DISK_CACHE_DATABASE;
   } else if (debug_get_bool_option("MESA_DISK_CACHE_MULTI_FILE", true)) {
      cache_type = DISK_CACHE_MULTI_FILE;
   } else {
      return NULL;
   }

   bool need_delete_old_cache = false;
   if (cache_type == DISK_CACHE_DATABASE) {
      if (!os_get_option("MESA_SHADER_CACHE_DIR") && !os_get_option("MESA_GLSL_CACHE_DIR") &&
          disk_cache_enabled()) {
         need_delete_old_cache = true;
      }
   }

   max_size_str = os_get_option("MESA_SHADER_CACHE_MAX_SIZE");

   if (!max_size_str) {
      max_size_str = os_get_option("MESA_GLSL_CACHE_MAX_SIZE");
      if (max_size_str) {
         fprintf(stderr,
                 "*** MESA_GLSL_CACHE_MAX_SIZE is deprecated; "
                 "use MESA_SHADER_CACHE_MAX_SIZE instead ***\n");
      }
   }

#ifdef MESA_SHADER_CACHE_MAX_SIZE
   if (!max_size_str) {
      max_size_str = MESA_SHADER_CACHE_MAX_SIZE;
   }
#endif

   if (max_size_str) {
      char *end;
      max_size = strtoul(max_size_str, &end, 10);
      if (end == max_size_str) {
         max_size = 0;
      } else {
         switch (*end) {
         case 'K':
         case 'k':
            max_size *= 1024;
            break;
         case 'M':
         case 'm':
            max_size *= 1024 * 1024;
            break;
         case '\0':
         case 'G':
         case 'g':
         default:
            max_size *= 1024 * 1024 * 1024;
            break;
         }
      }
   }

   if (max_size == 0) {
      max_size = 1024 * 1024 * 1024;
   }

   cache = disk_cache_type_create(gpu_name, driver_id, NULL, driver_flags,
                                  cache_type, max_size);
   if (!cache) {
      return NULL;
   }

   if (need_delete_old_cache && !cache->path_init_failed &&
       util_queue_is_initialized(&cache->cache_queue)) {
      struct fence_job *job = (struct fence_job *)malloc(sizeof(*job));
      if (job) {
         util_queue_fence_init(&job->fence);
         util_queue_add_job(&cache->cache_queue, job, &job->fence,
                            background_delete_old_cache, trivial_job_free, 0);
      }
   }

   if (cache_type != DISK_CACHE_SINGLE_FILE && !cache->path_init_failed &&
       debug_get_bool_option("MESA_DISK_CACHE_COMBINE_RW_WITH_RO_FOZ", false)) {

      cache->foz_ro_cache = disk_cache_type_create(gpu_name, driver_id, NULL,
                                                   driver_flags,
                                                   DISK_CACHE_SINGLE_FILE,
                                                   max_size);
   }

   return cache;
}

struct disk_cache *
disk_cache_create_custom(const char *gpu_name, const char *driver_id,
                         uint64_t driver_flags, const char *cache_dir_name,
                         uint32_t max_size)
{
   return disk_cache_type_create(gpu_name, driver_id, cache_dir_name, driver_flags,
                                 DISK_CACHE_DATABASE, max_size);
}

void
disk_cache_destroy(struct disk_cache *cache)
{
   if (!cache) {
      return;
   }

   if (UNLIKELY(cache->stats.enabled)) {
      mesa_logi("disk shader cache:  hits = %u, misses = %u\n",
                cache->stats.hits,
                cache->stats.misses);
   }

   if (util_queue_is_initialized(&cache->cache_queue)) {
      util_queue_finish(&cache->cache_queue);
      util_queue_destroy(&cache->cache_queue);
   }

   if (cache->foz_ro_cache) {
      disk_cache_destroy(cache->foz_ro_cache);
      cache->foz_ro_cache = NULL;
   }

   if (cache->type == DISK_CACHE_SINGLE_FILE) {
      foz_destroy(&cache->foz_db);
   }

   if (cache->type == DISK_CACHE_DATABASE) {
      mesa_cache_db_multipart_close(&cache->cache_db);
   }

   disk_cache_destroy_mmap(cache);

   ralloc_free(cache);
}

void
disk_cache_wait_for_idle(struct disk_cache *cache)
{
   if (cache && util_queue_is_initialized(&cache->cache_queue)) {
      util_queue_finish(&cache->cache_queue);
   }
}

void
disk_cache_remove(struct disk_cache *cache, const cache_key key)
{
   if (!cache) {
      return;
   }

   if (cache->type == DISK_CACHE_DATABASE) {
      mesa_cache_db_multipart_entry_remove(&cache->cache_db, key);
      return;
   }

   char *filename = disk_cache_get_cache_filename(cache, key);
   if (filename == NULL) {
      return;
   }

   disk_cache_evict_item(cache, filename);
}

/* -----------------------------------------------------------------------------
 * Put job creation/destruction
 * ---------------------------------------------------------------------------*/
static struct disk_cache_put_job *
create_put_job(struct disk_cache *cache, const cache_key key,
               void *data, size_t size,
               struct cache_item_metadata *cache_item_metadata,
               bool take_ownership)
{
   size_t job_size = sizeof(struct disk_cache_put_job);
   size_t payload_size = (!take_ownership) ? size : 0;

   if (!take_ownership && (payload_size > SIZE_MAX - job_size)) {
      return NULL;
   }

   size_t nkeys = 0;
   size_t keys_size = 0;
   bool inline_keys = false;

   if (cache_item_metadata &&
       cache_item_metadata->type == CACHE_ITEM_TYPE_GLSL &&
       cache_item_metadata->num_keys > 0) {
      nkeys = cache_item_metadata->num_keys;
      if (nkeys > SIZE_MAX / sizeof(cache_key)) {
         return NULL;
      }
      keys_size = nkeys * sizeof(cache_key);
      inline_keys = true;
   }

   size_t total_alloc_size = job_size;
   if (inline_keys) {
      if (keys_size > SIZE_MAX - total_alloc_size) {
         return NULL;
      }
      total_alloc_size += keys_size;
   }
   if (!take_ownership) {
      if (payload_size > SIZE_MAX - total_alloc_size) {
         return NULL;
      }
      total_alloc_size += payload_size;
   }

   struct disk_cache_put_job *dc_job =
      (struct disk_cache_put_job *)malloc(total_alloc_size);
   if (UNLIKELY(!dc_job)) {
      return NULL;
   }

   dc_job->cache = cache;
   memcpy(dc_job->key, key, sizeof(cache_key));
   dc_job->size = size;

   uint8_t *p_after_job = (uint8_t *)(dc_job + 1);

   if (take_ownership) {
      dc_job->data = data;
   } else {
      dc_job->data = p_after_job + (inline_keys ? keys_size : 0);
      if (size > 0) {
         memcpy(dc_job->data, data, size);
      }
   }

   if (cache_item_metadata) {
      dc_job->cache_item_metadata.type = cache_item_metadata->type;
      if (cache_item_metadata->type == CACHE_ITEM_TYPE_GLSL) {
         dc_job->cache_item_metadata.num_keys = nkeys;
         if (nkeys > 0) {
            dc_job->cache_item_metadata.keys = (cache_key *)p_after_job;
            memcpy(dc_job->cache_item_metadata.keys,
                   cache_item_metadata->keys,
                   keys_size);
         } else {
            dc_job->cache_item_metadata.keys = NULL;
         }
      } else {
         dc_job->cache_item_metadata.keys = NULL;
         dc_job->cache_item_metadata.num_keys = 0;
      }
   } else {
      dc_job->cache_item_metadata.type = CACHE_ITEM_TYPE_UNKNOWN;
      dc_job->cache_item_metadata.keys = NULL;
      dc_job->cache_item_metadata.num_keys = 0;
   }

   return dc_job;
}

static void
destroy_put_job(void *job, void *gdata, int thread_index)
{
   (void)gdata;
   (void)thread_index;
   free(job);
}

static void
destroy_put_job_nocopy(void *job, void *gdata, int thread_index)
{
   (void)gdata;
   (void)thread_index;

   struct disk_cache_put_job *dc_job = (struct disk_cache_put_job *) job;
   free(dc_job->data);
   destroy_put_job(job, gdata, thread_index);
}

/* -----------------------------------------------------------------------------
 * Blob callback compression format
 * ---------------------------------------------------------------------------*/
struct blob_cache_entry {
   uint32_t uncompressed_size;
   uint8_t compressed_data[];
};

static void
blob_put_compressed(struct disk_cache *cache, const cache_key key,
                    const void *data, size_t size)
{
   MESA_TRACE_FUNC();

   if (UNLIKELY(size > UINT32_MAX)) {
      return;
   }

   size_t max_buf = util_compress_max_compressed_len(size);
   if (UNLIKELY(max_buf > SIZE_MAX - sizeof(struct blob_cache_entry))) {
      return;
   }

   struct tls_out_buf *tls_out = tls_get_out();
   if (UNLIKELY(!tls_out)) {
      return;
   }

   size_t need = sizeof(struct blob_cache_entry) + max_buf;
   if (tls_out->cap < need) {
      size_t new_cap = tls_out->cap + (tls_out->cap / 2);
      if (new_cap < need) {
         new_cap = need;
      }
      if (new_cap > 16 * 1024 * 1024) {
         new_cap = need;
         if (new_cap > 16 * 1024 * 1024) {
            return;
         }
      }

      uint8_t *newp = (uint8_t *)realloc(tls_out->p, new_cap);
      if (UNLIKELY(!newp)) {
         return;
      }
      tls_out->p = newp;
      tls_out->cap = new_cap;
   }

   struct blob_cache_entry *entry = (struct blob_cache_entry *)tls_out->p;
   entry->uncompressed_size = (uint32_t)size;

   size_t compressed_size =
         util_compress_deflate(data, size, entry->compressed_data, max_buf);
   if (UNLIKELY(!compressed_size)) {
      return;
   }

   size_t entry_size_sz = compressed_size + sizeof(*entry);
   if (UNLIKELY(entry_size_sz > (size_t)LONG_MAX)) {
      return;
   }

   signed long entry_size = (signed long)entry_size_sz;
   {
      MESA_TRACE_SCOPE("blob_put");
      cache->blob_put_cb(key, CACHE_KEY_SIZE, entry, entry_size);
   }
}

static void *
blob_get_compressed(struct disk_cache *cache, const cache_key key,
                    size_t *size)
{
   MESA_TRACE_FUNC();

   const signed long max_blob_size = 128 * 1024;

   struct tls_in_buf *tls_in = tls_get_in();
   if (UNLIKELY(!tls_in || tls_in->cap < (size_t)max_blob_size)) {
      return NULL;
   }

   struct blob_cache_entry *entry = (struct blob_cache_entry *)tls_in->p;

   signed long entry_size;
   {
      MESA_TRACE_SCOPE("blob_get");
      entry_size = cache->blob_get_cb(key, CACHE_KEY_SIZE, entry, max_blob_size);
   }

   if (UNLIKELY(entry_size <= 0 ||
       entry_size > max_blob_size ||
       entry_size < (signed long)sizeof(*entry))) {
      return NULL;
   }

   size_t sz_entry = (size_t)entry_size;
   size_t hdr = sizeof(*entry);
   size_t compressed_size = sz_entry - hdr;

   size_t uncomp = (size_t)entry->uncompressed_size;
   if (UNLIKELY(uncomp == 0 || uncomp > 256 * 1024 * 1024)) {
      return NULL;
   }

   void *data = malloc(uncomp);
   if (UNLIKELY(!data)) {
      return NULL;
   }

   bool ret = util_compress_inflate(entry->compressed_data, compressed_size,
                                    data, uncomp);
   if (UNLIKELY(!ret)) {
      free(data);
      return NULL;
   }

   if (size) {
      *size = uncomp;
   }

   return data;
}

/* -----------------------------------------------------------------------------
 * Queue job function
 * ---------------------------------------------------------------------------*/
static void
cache_put(void *job, void *gdata, int thread_index)
{
   (void)gdata;
   (void)thread_index;

   assert(job);

   unsigned i = 0;
   char *filename = NULL;
   struct disk_cache_put_job *dc_job = (struct disk_cache_put_job *) job;

   if (dc_job->cache->blob_put_cb) {
      blob_put_compressed(dc_job->cache, dc_job->key, dc_job->data, dc_job->size);
   } else if (dc_job->cache->type == DISK_CACHE_SINGLE_FILE) {
      disk_cache_write_item_to_disk_foz(dc_job);
   } else if (dc_job->cache->type == DISK_CACHE_DATABASE) {
      disk_cache_db_write_item_to_disk(dc_job);
   } else if (dc_job->cache->type == DISK_CACHE_MULTI_FILE) {
      filename = disk_cache_get_cache_filename(dc_job->cache, dc_job->key);
      if (filename == NULL) {
         goto done;
      }

      while (p_atomic_read_relaxed(&dc_job->cache->size->value) + dc_job->size > dc_job->cache->max_size &&
             i < 8) {
         disk_cache_evict_lru_item(dc_job->cache);
         i++;
      }

      disk_cache_write_item_to_disk(dc_job, filename);

done:
      free(filename);
   }
}

/* -----------------------------------------------------------------------------
 * Public API: put/get
 * ---------------------------------------------------------------------------*/
void
disk_cache_put(struct disk_cache *cache, const cache_key key,
               const void *data, size_t size,
               struct cache_item_metadata *cache_item_metadata)
{
   if (UNLIKELY(size == 0)) {
      return;
   }

   if (!util_queue_is_initialized(&cache->cache_queue)) {
      return;
   }

   struct disk_cache_put_job *dc_job =
      create_put_job(cache, key, (void*)data, size, cache_item_metadata, false);

   if (dc_job) {
      util_queue_fence_init(&dc_job->fence);
      util_queue_add_job(&cache->cache_queue, dc_job, &dc_job->fence,
                         cache_put, destroy_put_job, dc_job->size);
   }
}

void
disk_cache_put_nocopy(struct disk_cache *cache, const cache_key key,
                      void *data, size_t size,
                      struct cache_item_metadata *cache_item_metadata)
{
   if (!util_queue_is_initialized(&cache->cache_queue)) {
      free(data);
      return;
   }

   if (UNLIKELY(size == 0)) {
      free(data);
      return;
   }

   struct disk_cache_put_job *dc_job =
      create_put_job(cache, key, data, size, cache_item_metadata, true);

   if (dc_job) {
      util_queue_fence_init(&dc_job->fence);
      util_queue_add_job(&cache->cache_queue, dc_job, &dc_job->fence,
                         cache_put, destroy_put_job_nocopy, dc_job->size);
   } else {
      free(data);
   }
}

void *
disk_cache_get(struct disk_cache *cache, const cache_key key, size_t *size)
{
   void *buf = NULL;

   if (size) {
      *size = 0;
   }

   if (cache->foz_ro_cache) {
      buf = disk_cache_load_item_foz(cache->foz_ro_cache, key, size);
   }

   if (!buf) {
      if (cache->blob_get_cb) {
         buf = blob_get_compressed(cache, key, size);
      } else if (cache->type == DISK_CACHE_SINGLE_FILE) {
         buf = disk_cache_load_item_foz(cache, key, size);
      } else if (cache->type == DISK_CACHE_DATABASE) {
         buf = disk_cache_db_load_item(cache, key, size);
      } else if (cache->type == DISK_CACHE_MULTI_FILE) {
         char *filename = disk_cache_get_cache_filename(cache, key);
         if (filename) {
            buf = disk_cache_load_item(cache, filename, size);
         }
      }
   }

   if (UNLIKELY(cache->stats.enabled)) {
      if (buf) {
         p_atomic_inc(&cache->stats.hits);
      } else {
         p_atomic_inc(&cache->stats.misses);
      }
   }

   return buf;
}

/* -----------------------------------------------------------------------------
 * Fast key compare
 * ---------------------------------------------------------------------------*/

_Static_assert(CACHE_KEY_SIZE == 20, "Optimized key comparison assumes 20-byte SHA1 key");

#if defined(CACHE_KEY_SIZE) && (CACHE_KEY_SIZE != 20)
#  define CACHE_KEY_FASTCMP(a,b) (memcmp((a),(b),CACHE_KEY_SIZE) == 0)
#else

#if (defined(__x86_64__) || defined(_M_X64))
static inline bool
cache_key_equals_fast_sse2(const unsigned char *a, const unsigned char *b)
{
   __m128i va = _mm_loadu_si128((const __m128i *)a);
   __m128i vb = _mm_loadu_si128((const __m128i *)b);
   __m128i cmp_res = _mm_cmpeq_epi8(va, vb);

   if (UNLIKELY(_mm_movemask_epi8(cmp_res) != 0xFFFF)) {
      return false;
   }

   uint32_t tail_a, tail_b;
   memcpy(&tail_a, a + 16, sizeof(tail_a));
   memcpy(&tail_b, b + 16, sizeof(tail_b));
   return tail_a == tail_b;
}
#endif

static inline bool
cache_key_equals_fast(const unsigned char *a, const unsigned char *b)
{
#if (defined(__x86_64__) || defined(_M_X64))
   return cache_key_equals_fast_sse2(a, b);
#else
   uint64_t a0, a1, b0, b1;
   uint32_t a2, b2;
   memcpy(&a0, a + 0,  sizeof(a0));
   memcpy(&a1, a + 8,  sizeof(a1));
   memcpy(&a2, a + 16, sizeof(a2));
   memcpy(&b0, b + 0,  sizeof(b0));
   memcpy(&b1, b + 8,  sizeof(b1));
   memcpy(&b2, b + 16, sizeof(b2));
   return (a0 == b0) & (a1 == b1) & (a2 == b2);
#endif
}
#  define CACHE_KEY_FASTCMP(a,b) cache_key_equals_fast((a),(b))
#endif

/* -----------------------------------------------------------------------------
 * In-memory key table helpers
 * ---------------------------------------------------------------------------*/
void
disk_cache_put_key(struct disk_cache *cache, const cache_key key)
{
   uint32_t first_word;
   int i;
   unsigned char *entry;

   memcpy(&first_word, key, sizeof(first_word));
   i = CPU_TO_LE32(first_word) & CACHE_INDEX_KEY_MASK;

   if (cache->blob_put_cb) {
      cache->blob_put_cb(key, CACHE_KEY_SIZE, &first_word, (signed long)sizeof(uint32_t));
      return;
   }

   if (cache->path_init_failed) {
      return;
   }

   entry = &cache->stored_keys[i * CACHE_KEY_SIZE];

   memcpy(entry, key, CACHE_KEY_SIZE);
}

bool
disk_cache_has_key(struct disk_cache *cache, const cache_key key)
{
   uint32_t first_word;
   int i;
   unsigned char *entry;

   memcpy(&first_word, key, sizeof(first_word));

   if (cache->blob_get_cb) {
      uint32_t blob = 0;
      return cache->blob_get_cb(key, CACHE_KEY_SIZE, &blob, (signed long)sizeof(uint32_t)) != 0;
   }

   if (cache->path_init_failed) {
      return false;
   }

   i = CPU_TO_LE32(first_word) & CACHE_INDEX_KEY_MASK;
   entry = &cache->stored_keys[i * CACHE_KEY_SIZE];

   PREFETCH_R(entry);

   return CACHE_KEY_FASTCMP(entry, key);
}

/* -----------------------------------------------------------------------------
 * Hashing / callbacks
 * ---------------------------------------------------------------------------*/

#if (defined(__x86_64__) || defined(_M_X64)) && (defined(__SHA__) || (defined(_MSC_VER) && !defined(__clang__)))
#define HAVE_SHA_NI 1

static inline uint32_t
bswap32_portable(uint32_t val)
{
#if defined(__GNUC__) || defined(__clang__)
   return __builtin_bswap32(val);
#elif defined(_MSC_VER)
   return _byteswap_ulong(val);
#else
   return ((val & 0xff000000) >> 24) |
          ((val & 0x00ff0000) >>  8) |
          ((val & 0x0000ff00) <<  8) |
          ((val & 0x000000ff) << 24);
#endif
}

static inline uint64_t
bswap64_portable(uint64_t val)
{
#if defined(__GNUC__) || defined(__clang__)
   return __builtin_bswap64(val);
#elif defined(_MSC_VER)
   return _byteswap_uint64(val);
#else
   return ((uint64_t)bswap32_portable((uint32_t)(val & 0xFFFFFFFF)) << 32) |
          (bswap32_portable((uint32_t)(val >> 32)));
#endif
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((target("sha")))
#endif
static void
process_block_sha_ni(uint32_t h[5], const uint8_t* block_ptr)
{
    const __m128i endian_shuffle = _mm_set_epi8(12, 13, 14, 15, 8, 9, 10, 11, 4, 5, 6, 7, 0, 1, 2, 3);
    __m128i abcd, e0, e1;
    uint32_t e_s;

    abcd = _mm_loadu_si128((const __m128i*)h);
    e_s = h[4];
    abcd = _mm_shuffle_epi32(abcd, 0x1B);

    __m128i w0 = _mm_loadu_si128((const __m128i*)(block_ptr + 0));
    __m128i w1 = _mm_loadu_si128((const __m128i*)(block_ptr + 16));
    __m128i w2 = _mm_loadu_si128((const __m128i*)(block_ptr + 32));
    __m128i w3 = _mm_loadu_si128((const __m128i*)(block_ptr + 48));

    w0 = _mm_shuffle_epi8(w0, endian_shuffle);
    w1 = _mm_shuffle_epi8(w1, endian_shuffle);
    w2 = _mm_shuffle_epi8(w2, endian_shuffle);
    w3 = _mm_shuffle_epi8(w3, endian_shuffle);
    e0 = _mm_set_epi32((int)e_s, 0, 0, 0);

    e0 = _mm_sha1rnds4_epu32(e0, abcd, 0); abcd = _mm_sha1nexte_epu32(abcd, w0);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 0); abcd = _mm_sha1nexte_epu32(abcd, w1);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 0); abcd = _mm_sha1nexte_epu32(abcd, w2);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 0); abcd = _mm_sha1nexte_epu32(abcd, w3);
    w0 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w0, w1), w2);

    e0 = _mm_sha1rnds4_epu32(e1, abcd, 0); abcd = _mm_sha1nexte_epu32(abcd, w0);
    w1 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w1, w2), w3);

    e1 = _mm_sha1rnds4_epu32(e0, abcd, 1); abcd = _mm_sha1nexte_epu32(abcd, w1);
    w2 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w2, w3), w0);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 1); abcd = _mm_sha1nexte_epu32(abcd, w2);
    w3 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w3, w0), w1);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 1); abcd = _mm_sha1nexte_epu32(abcd, w3);
    w0 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w0, w1), w2);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 1); abcd = _mm_sha1nexte_epu32(abcd, w0);
    w1 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w1, w2), w3);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 1); abcd = _mm_sha1nexte_epu32(abcd, w1);
    w2 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w2, w3), w0);

    e0 = _mm_sha1rnds4_epu32(e1, abcd, 2); abcd = _mm_sha1nexte_epu32(abcd, w2);
    w3 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w3, w0), w1);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 2); abcd = _mm_sha1nexte_epu32(abcd, w3);
    w0 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w0, w1), w2);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 2); abcd = _mm_sha1nexte_epu32(abcd, w0);
    w1 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w1, w2), w3);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 2); abcd = _mm_sha1nexte_epu32(abcd, w1);
    w2 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w2, w3), w0);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 2); abcd = _mm_sha1nexte_epu32(abcd, w2);
    w3 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w3, w0), w1);

    e1 = _mm_sha1rnds4_epu32(e0, abcd, 3); abcd = _mm_sha1nexte_epu32(abcd, w3);
    w0 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w0, w1), w2);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 3); abcd = _mm_sha1nexte_epu32(abcd, w0);
    w1 = _mm_sha1msg2_epu32(_mm_sha1msg1_epu32(w1, w2), w3);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 3); abcd = _mm_sha1nexte_epu32(abcd, w1);
    e0 = _mm_sha1rnds4_epu32(e1, abcd, 3); abcd = _mm_sha1nexte_epu32(abcd, w2);
    e1 = _mm_sha1rnds4_epu32(e0, abcd, 3);

    e_s = (uint32_t)_mm_extract_epi32(e1, 0);
    abcd = _mm_shuffle_epi32(abcd, 0x1B);
    _mm_storeu_si128((__m128i*)h, _mm_add_epi32(abcd, _mm_loadu_si128((const __m128i*)h)));
    h[4] += e_s;
}

static bool
disk_cache_compute_key_sha_ni(const void *data1, size_t size1,
                              const void *data2, size_t size2,
                              cache_key key)
{
   uint32_t h[5] = {
      0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0
   };

   uint8_t current_block[64];
   size_t total_size = size1 + size2;
   size_t offset = 0;
   const uint8_t *p1 = (const uint8_t *)data1;
   const uint8_t *p2 = (const uint8_t *)data2;

   while (offset + 64 <= total_size) {
      const uint8_t* block_ptr;
      if (offset < size1 && (size1 - offset) >= 64) {
         block_ptr = p1 + offset;
      } else if (offset >= size1) {
         block_ptr = p2 + (offset - size1);
      } else {
         size_t remaining_in_p1 = size1 - offset;
         memcpy(current_block, p1 + offset, remaining_in_p1);
         memcpy(current_block + remaining_in_p1, p2, 64 - remaining_in_p1);
         block_ptr = current_block;
      }
      process_block_sha_ni(h, block_ptr);
      offset += 64;
   }

   size_t remaining = total_size - offset;
   if (offset < size1) {
      size_t remaining_in_p1 = size1 - offset;
      if (remaining_in_p1 > 0) {
         memcpy(current_block, p1 + offset, remaining_in_p1);
      }
      if (remaining > remaining_in_p1) {
         memcpy(current_block + remaining_in_p1, p2, remaining - remaining_in_p1);
      }
   } else {
      if (remaining > 0) {
         memcpy(current_block, p2 + (offset - size1), remaining);
      }
   }

   current_block[remaining] = 0x80;
   remaining++;

   if (remaining > 56) {
      memset(current_block + remaining, 0, 64 - remaining);
      process_block_sha_ni(h, current_block);
      memset(current_block, 0, 56);
   } else {
      memset(current_block + remaining, 0, 56 - remaining);
   }

   uint64_t total_bits = total_size * 8;
   ((uint64_t*)current_block)[7] = bswap64_portable(total_bits);
   process_block_sha_ni(h, current_block);

   h[0] = bswap32_portable(h[0]);
   h[1] = bswap32_portable(h[1]);
   h[2] = bswap32_portable(h[2]);
   h[3] = bswap32_portable(h[3]);
   h[4] = bswap32_portable(h[4]);
   memcpy(key, h, 20);

   return true;
}
#endif /* HAVE_SHA_NI */

void
disk_cache_compute_key(struct disk_cache *cache, const void *data, size_t size,
                       cache_key key)
{
#if HAVE_SHA_NI
   static uint32_t sha_support_state = 0;

   uint32_t state = p_atomic_read(&sha_support_state);
   if (UNLIKELY(state == 0)) {
      bool has_sha;
#if defined(__GNUC__) || defined(__clang__)
      has_sha = __builtin_cpu_supports("sha");
#elif defined(_MSC_VER)
      int info[4];
      __cpuidex(info, 7, 0);
      has_sha = (info[1] & (1 << 29)) != 0;
#else
      has_sha = false;
#endif
      p_atomic_set(&sha_support_state, has_sha ? 2 : 1);
      state = has_sha ? 2 : 1;
   }

   if (state == 2) {
      disk_cache_compute_key_sha_ni(cache->driver_keys_blob,
                                     cache->driver_keys_blob_size,
                                     data, size, key);
      return;
   }
#endif

   struct mesa_sha1 ctx;
   _mesa_sha1_init(&ctx);
   _mesa_sha1_update(&ctx, cache->driver_keys_blob,
                     cache->driver_keys_blob_size);
   _mesa_sha1_update(&ctx, data, size);
   _mesa_sha1_final(&ctx, key);
}

void
disk_cache_set_callbacks(struct disk_cache *cache, disk_cache_put_cb put,
                         disk_cache_get_cb get)
{
   cache->blob_put_cb = put;
   cache->blob_get_cb = get;
   disk_cache_init_queue(cache);
}

#endif /* ENABLE_SHADER_CACHE */
