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
#if !defined(_WIN32) && !defined(__CYGWIN__)
#include <unistd.h> /* sysconf for CPU count on POSIX */
#include <pthread.h> /* dynamic TLS for POSIX */
#else
/* Windows dynamic TLS (FLS) */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
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

/* The cache version should be bumped whenever a change is made to the
 * structure of cache entries or the index. This will give any 3rd party
 * applications reading the cache entries a chance to adjust to the changes.
 *
 * - The cache version is checked internally when reading a cache entry. If we
 *   ever have a mismatch we are in big trouble as this means we had a cache
 *   collision. In case of such an event please check the skys for giant
 *   asteroids and that the entire Mesa team hasn't been eaten by wolves.
 *
 * - There is no strict requirement that cache versions be backwards
 *   compatible but effort should be taken to limit disruption where possible.
 */
#define CACHE_VERSION 1

/* Copy src bytes into dst and bump dst pointer by src_size. */
#define DRV_KEY_CPY(_dst, _src, _src_size) \
do {                                       \
   memcpy(_dst, _src, _src_size);          \
   _dst += _src_size;                      \
} while (0)

/* Portable feature detection for prefetch */
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#if __has_builtin(__builtin_prefetch)
#define PREFETCH_R(addr) __builtin_prefetch((addr), 0, 1)
#else
#define PREFETCH_R(addr) ((void)0)
#endif

/* -----------------------------------------------------------------------------
 * Thread-local buffer management (portable, minimal static TLS footprint)
 * We avoid large static TLS arrays to prevent "no memory in static TLS block"
 * loader failures (observed with many DSOs + big TLS). We use dynamic TLS:
 *  - POSIX: pthread_key_t (+ destructor) per-thread allocation and cleanup
 *  - Windows: FLS (Fiber-local storage) with destructor; fallback to TlsAlloc
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

static void tls_in_destructor(void *ptr)
{
   struct tls_in_buf *b = (struct tls_in_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static void tls_out_destructor(void *ptr)
{
   struct tls_out_buf *b = (struct tls_out_buf *)ptr;
   if (b) {
      free(b->p);
      free(b);
   }
}

static void tls_make_keys(void)
{
   int r1 = pthread_key_create(&g_tls_in_key, tls_in_destructor);
   int r2 = pthread_key_create(&g_tls_out_key, tls_out_destructor);
   (void)r1;
   (void)r2;
   /* If either fails, keys will be zero-initialized and pthread_getspecific
    * will return NULL; the code will behave as if allocation failed.
    */
}

static struct tls_in_buf *tls_get_in(void)
{
   pthread_once(&g_tls_once, tls_make_keys);
   struct tls_in_buf *b = (struct tls_in_buf *)pthread_getspecific(g_tls_in_key);
   if (!b) {
      b = (struct tls_in_buf *)malloc(sizeof(*b));
      if (!b) return NULL;
      b->cap = 64 * 1024;
      b->p = (uint8_t *)malloc(b->cap);
      if (!b->p) {
         free(b);
         return NULL;
      }
      if (pthread_setspecific(g_tls_in_key, b) != 0) {
         free(b->p);
         free(b);
         return NULL;
      }
   }
   return b;
}

static struct tls_out_buf *tls_get_out(void)
{
   pthread_once(&g_tls_once, tls_make_keys);
   struct tls_out_buf *b = (struct tls_out_buf *)pthread_getspecific(g_tls_out_key);
   if (!b) {
      b = (struct tls_out_buf *)malloc(sizeof(*b));
      if (!b) return NULL;
      b->p = NULL;
      b->cap = 0;
      if (pthread_setspecific(g_tls_out_key, b) != 0) {
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

static BOOL CALLBACK tls_make_keys_win(PINIT_ONCE once, PVOID param, PVOID *ctx)
{
   (void)once; (void)param; (void)ctx;
   /* Try to allocate FLS indexes with destructor if available (Windows Vista+) */
#if (_WIN32_WINNT >= 0x0600)
   g_tls_in_index = FlsAlloc([](void *ptr){
      struct tls_in_buf *b = (struct tls_in_buf *)ptr;
      if (b) {
         free(b->p);
         free(b);
      }
   });
   g_tls_out_index = FlsAlloc([](void *ptr){
      struct tls_out_buf *b = (struct tls_out_buf *)ptr;
      if (b) {
         free(b->p);
         free(b);
      }
   });
   if (g_tls_in_index == FLS_OUT_OF_INDEXES || g_tls_out_index == FLS_OUT_OF_INDEXES) {
      if (g_tls_in_index != FLS_OUT_OF_INDEXES) FlsFree(g_tls_in_index);
      if (g_tls_out_index != FLS_OUT_OF_INDEXES) FlsFree(g_tls_out_index);
      g_tls_in_index = TLS_OUT_OF_INDEXES;
      g_tls_out_index = TLS_OUT_OF_INDEXES;
   }
#endif
   /* Fallback to TlsAlloc (no destructor). Not ideal but avoids static TLS. */
   if (g_tls_in_index == TLS_OUT_OF_INDEXES) {
      g_tls_in_index = TlsAlloc();
   }
   if (g_tls_out_index == TLS_OUT_OF_INDEXES) {
      g_tls_out_index = TlsAlloc();
   }
   return TRUE;
}

static struct tls_in_buf *tls_get_in(void)
{
   InitOnceExecuteOnce(&g_tls_once, tls_make_keys_win, NULL, NULL);
   if (g_tls_in_index == TLS_OUT_OF_INDEXES) return NULL;

#if (_WIN32_WINNT >= 0x0600)
   void *ptr = (g_tls_in_index != FLS_OUT_OF_INDEXES) ? FlsGetValue(g_tls_in_index) : TlsGetValue(g_tls_in_index);
#else
   void *ptr = TlsGetValue(g_tls_in_index);
#endif
   struct tls_in_buf *b = (struct tls_in_buf *)ptr;
   if (!b) {
      b = (struct tls_in_buf *)malloc(sizeof(*b));
      if (!b) return NULL;
      b->cap = 64 * 1024;
      b->p = (uint8_t *)malloc(b->cap);
      if (!b->p) {
         free(b);
         return NULL;
      }
#if (_WIN32_WINNT >= 0x0600)
      BOOL ok = (g_tls_in_index != FLS_OUT_OF_INDEXES) ? FlsSetValue(g_tls_in_index, b) : TlsSetValue(g_tls_in_index, b);
#else
      BOOL ok = TlsSetValue(g_tls_in_index, b);
#endif
      if (!ok) {
         free(b->p);
         free(b);
         return NULL;
      }
   }
   return b;
}

static struct tls_out_buf *tls_get_out(void)
{
   InitOnceExecuteOnce(&g_tls_once, tls_make_keys_win, NULL, NULL);
   if (g_tls_out_index == TLS_OUT_OF_INDEXES) return NULL;

#if (_WIN32_WINNT >= 0x0600)
   void *ptr = (g_tls_out_index != FLS_OUT_OF_INDEXES) ? FlsGetValue(g_tls_out_index) : TlsGetValue(g_tls_out_index);
#else
   void *ptr = TlsGetValue(g_tls_out_index);
#endif
   struct tls_out_buf *b = (struct tls_out_buf *)ptr;
   if (!b) {
      b = (struct tls_out_buf *)malloc(sizeof(*b));
      if (!b) return NULL;
      b->p = NULL;
      b->cap = 0;
#if (_WIN32_WINNT >= 0x0600)
      BOOL ok = (g_tls_out_index != FLS_OUT_OF_INDEXES) ? FlsSetValue(g_tls_out_index, b) : TlsSetValue(g_tls_out_index, b);
#else
      BOOL ok = TlsSetValue(g_tls_out_index, b);
#endif
      if (!ok) {
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
   if (util_queue_is_initialized(&cache->cache_queue))
      return true;

   /* Dynamically scale worker threads to hide I/O latency without oversubscription.
    * Heuristic: half the logical CPUs clamped to [4, 8]. Default to 4 if unknown.
    */
   int logical = 4;
#if defined(_SC_NPROCESSORS_ONLN)
   long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
   if (nprocs > 0 && nprocs < INT_MAX)
      logical = (int)nprocs;
#endif
   int threads = logical / 2;
   if (threads < 4) threads = 4;
   if (threads > 8) threads = 8;

   /* Slightly deeper queue to reduce producer stalls under heavy load. */
   unsigned queue_depth = 64;

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

   /* A ralloc context for transient data during this invocation. */
   local = ralloc_context(NULL);
   if (local == NULL)
      goto fail;

   cache = rzalloc(NULL, struct disk_cache);
   if (cache == NULL)
      goto fail;

   /* Assume failure. */
   cache->path_init_failed = true;
   cache->type = DISK_CACHE_NONE;

   if (!disk_cache_enabled())
      goto path_fail;

   const char *path =
      disk_cache_generate_cache_dir(local, gpu_name, driver_id, cache_dir_name, cache_type, true);
   if (!path)
      goto path_fail;

   cache->path = ralloc_strdup(cache, path);
   if (cache->path == NULL)
      goto path_fail;

   /* Cache tests that want to have a disabled cache compression are using
    * the "make_check_uncompressed" for the driver_id name.  Hence here we
    * disable disk cache compression when mesa's build tests require it.
    */
   if (strcmp(driver_id, "make_check_uncompressed") == 0)
      cache->compression_disabled = true;

   if (cache_type == DISK_CACHE_SINGLE_FILE) {
      if (!disk_cache_load_cache_index_foz(local, cache))
         goto path_fail;
   } else if (cache_type == DISK_CACHE_DATABASE) {
      if (!disk_cache_db_load_cache_index(local, cache))
         goto path_fail;
   }

   if (!os_get_option("MESA_SHADER_CACHE_DIR") && !os_get_option("MESA_GLSL_CACHE_DIR"))
      disk_cache_touch_cache_user_marker(cache->path);

   cache->type = cache_type;

   cache->stats.enabled = debug_get_bool_option("MESA_SHADER_CACHE_SHOW_STATS",
                                                false);

   if (!disk_cache_mmap_cache_index(local, cache))
      goto path_fail;

   cache->max_size = max_size;

   if (cache->type == DISK_CACHE_DATABASE)
      mesa_cache_db_multipart_set_size_limit(&cache->cache_db, cache->max_size);

   if (!disk_cache_init_queue(cache))
      goto fail;

   cache->path_init_failed = false;

 path_fail:

   cache->driver_keys_blob_size = cv_size;

   /* Create driver id keys */
   size_t id_size = strlen(driver_id) + 1;
   size_t gpu_name_size = strlen(gpu_name) + 1;
   cache->driver_keys_blob_size += id_size;
   cache->driver_keys_blob_size += gpu_name_size;

   /* We sometimes store entire structs that contains a pointers in the cache,
    * use pointer size as a key to avoid hard to debug issues.
    */
   uint8_t ptr_size = sizeof(void *);
   size_t ptr_size_size = sizeof(ptr_size);
   cache->driver_keys_blob_size += ptr_size_size;

   size_t driver_flags_size = sizeof(driver_flags);
   cache->driver_keys_blob_size += driver_flags_size;

   cache->driver_keys_blob =
      ralloc_size(cache, cache->driver_keys_blob_size);
   if (!cache->driver_keys_blob)
      goto fail;

   uint8_t *drv_key_blob = cache->driver_keys_blob;
   DRV_KEY_CPY(drv_key_blob, &cache_version, cv_size);
   DRV_KEY_CPY(drv_key_blob, driver_id, id_size);
   DRV_KEY_CPY(drv_key_blob, gpu_name, gpu_name_size);
   DRV_KEY_CPY(drv_key_blob, &ptr_size, ptr_size_size);
   DRV_KEY_CPY(drv_key_blob, &driver_flags, driver_flags_size);

   /* Seed our rand function */
   s_rand_xorshift128plus(cache->seed_xorshift128plus, true);

   ralloc_free(local);

   return cache;

 fail:
   if (cache) {
      /* Clean up any partially initialized backend resources to avoid leaks. */
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

/* Background job to delete old cache folder to avoid startup stalls. */
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

   /* Compute whether we want to delete old cache directory (if default path). */
   bool need_delete_old_cache = false;
   if (cache_type == DISK_CACHE_DATABASE) {
      /* Since switching the default cache to <mesa_shader_cache_db>, remove the
       * old cache folder if it hasn't been modified for more than 7 days.
       */
      if (!os_get_option("MESA_SHADER_CACHE_DIR") && !os_get_option("MESA_GLSL_CACHE_DIR") &&
          disk_cache_enabled()) {
         need_delete_old_cache = true;
      }
   }

   max_size_str = os_get_option("MESA_SHADER_CACHE_MAX_SIZE");

   if (!max_size_str) {
      max_size_str = os_get_option("MESA_GLSL_CACHE_MAX_SIZE");
      if (max_size_str)
         fprintf(stderr,
                 "*** MESA_GLSL_CACHE_MAX_SIZE is deprecated; "
                 "use MESA_SHADER_CACHE_MAX_SIZE instead ***\n");
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
            max_size *= 1024*1024;
            break;
         case '\0':
         case 'G':
         case 'g':
         default:
            max_size *= 1024*1024*1024;
            break;
         }
      }
   }

   /* Default to 1GB for maximum cache size. */
   if (max_size == 0) {
      max_size = 1024*1024*1024;
   }

   /* Create main writable cache. */
   cache = disk_cache_type_create(gpu_name, driver_id, NULL, driver_flags,
                                  cache_type, max_size);
   if (!cache)
      return NULL;

   /* If configured, schedule deletion of old <mesa_shader_cache> dir in the
    * background to avoid blocking startup (prevents visible freezes).
    */
   if (need_delete_old_cache && !cache->path_init_failed &&
       util_queue_is_initialized(&cache->cache_queue)) {
      struct fence_job *job = (struct fence_job *)malloc(sizeof(*job));
      if (job) {
         util_queue_fence_init(&job->fence);
         util_queue_add_job(&cache->cache_queue, job, &job->fence,
                            background_delete_old_cache, trivial_job_free, 0);
         /* No waiting on fence: fire-and-forget cleanup. */
      }
   }

   /* If MESA_DISK_CACHE_SINGLE_FILE is unset and MESA_DISK_CACHE_COMBINE_RW_WITH_RO_FOZ
    * is set, then enable additional Fossilize RO caches together with the RW
    * cache.  At first we will check cache entry presence in the RO caches and
    * if entry isn't found there, then we'll fall back to the RW cache.
    */
   if (cache_type != DISK_CACHE_SINGLE_FILE && !cache->path_init_failed &&
       debug_get_bool_option("MESA_DISK_CACHE_COMBINE_RW_WITH_RO_FOZ", false)) {

      /* Create read-only cache used for sharing prebuilt shaders.
       * If cache entry will be found in this cache, then the main cache
       * will be bypassed.
       */
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

   if (unlikely(cache->stats.enabled)) {
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

   /* Ownership of filename transfers to disk_cache_evict_item */
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

   /* For copy case, we inline the payload (and optionally keys) after the job. */
   size_t payload_size = (!take_ownership) ? size : 0;

   /* Prevent overflow in size calculation */
   if (!take_ownership) {
      if (payload_size > SIZE_MAX - job_size)
         return NULL;
   }

   size_t nkeys = 0;
   size_t keys_size = 0;
   bool want_inline_keys = false;

   if (cache_item_metadata &&
       cache_item_metadata->type == CACHE_ITEM_TYPE_GLSL &&
       cache_item_metadata->num_keys > 0) {
      nkeys = cache_item_metadata->num_keys;
      if (nkeys > SIZE_MAX / sizeof(cache_key)) {
         return NULL;
      }
      keys_size = nkeys * sizeof(cache_key);
      /* Only inline keys when we also inline data (copy case). */
      want_inline_keys = !take_ownership;
   }

   size_t base = job_size + payload_size;
   if (want_inline_keys) {
      if (keys_size > SIZE_MAX - base)
         return NULL;
   }
   size_t total = base + (want_inline_keys ? keys_size : 0);

   struct disk_cache_put_job *dc_job =
      (struct disk_cache_put_job *)malloc(total);
   if (!dc_job)
      return NULL;

   dc_job->cache = cache;
   memcpy(dc_job->key, key, sizeof(cache_key));
   if (take_ownership) {
      dc_job->data = data;
   } else {
      dc_job->data = dc_job + 1;
      if (size > 0)
         memcpy(dc_job->data, data, size);
   }
   dc_job->size = size;

   /* Copy the cache item metadata */
   if (cache_item_metadata) {
      dc_job->cache_item_metadata.type = cache_item_metadata->type;
      if (cache_item_metadata->type == CACHE_ITEM_TYPE_GLSL) {
         dc_job->cache_item_metadata.num_keys = cache_item_metadata->num_keys;

         if (nkeys == 0) {
            dc_job->cache_item_metadata.keys = NULL;
         } else if (want_inline_keys) {
            /* Embed keys right after the embedded payload */
            uint8_t *basep = (uint8_t *)dc_job->data;
            uint8_t *inline_keys = basep + payload_size;
            dc_job->cache_item_metadata.keys = (cache_key *)inline_keys;
            memcpy(dc_job->cache_item_metadata.keys,
                   cache_item_metadata->keys,
                   keys_size);
         } else {
            dc_job->cache_item_metadata.keys =
               (cache_key *)malloc(keys_size);
            if (!dc_job->cache_item_metadata.keys) {
               free(dc_job);
               return NULL;
            }
            memcpy(dc_job->cache_item_metadata.keys,
                   cache_item_metadata->keys,
                   keys_size);
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

   if (job) {
      struct disk_cache_put_job *dc_job = (struct disk_cache_put_job *) job;

      /* If we inlined keys after the payload (copy case), don't free twice. */
      bool keys_embedded = false;
      if (dc_job->cache_item_metadata.keys) {
         /* Copy case if data == (dc_job + 1). */
         if (dc_job->data == (void *)(dc_job + 1)) {
            uint8_t *embedded = (uint8_t *)dc_job->data + dc_job->size;
            if ((void *)dc_job->cache_item_metadata.keys == (void *)embedded)
               keys_embedded = true;
         }
      }

      if (!keys_embedded)
         free(dc_job->cache_item_metadata.keys);

      free(job);
   }
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
/* TLS scratch buffers to avoid per-call heap churn in blob callbacks.
 * Safe, per-thread, bounded:
 *  - get: per-thread 64 KiB input buffer (Android egl_cache_t limit) via dynamic TLS
 *  - put: per-thread growable buffer for compressed payload + header via dynamic TLS
 */
struct blob_cache_entry {
   uint32_t uncompressed_size;
   uint8_t compressed_data[];
};

static void
blob_put_compressed(struct disk_cache *cache, const cache_key key,
                    const void *data, size_t size)
{
   MESA_TRACE_FUNC();

   /* We encode uncompressed_size as 32-bit; reject larger inputs. */
   if (size > UINT32_MAX) {
      return;
   }

   size_t max_buf = util_compress_max_compressed_len(size);
   if (max_buf > SIZE_MAX - sizeof(struct blob_cache_entry)) {
      return;
   }

   /* Dynamic per-thread reusable output buffer */
   struct tls_out_buf *tls_out = tls_get_out();
   if (!tls_out) {
      return;
   }

   size_t need = sizeof(struct blob_cache_entry) + max_buf;
   if (tls_out->cap < need) {
      uint8_t *newp = (uint8_t *)realloc(tls_out->p, need);
      if (!newp) {
         return;
      }
      tls_out->p = newp;
      tls_out->cap = need;
   }

   struct blob_cache_entry *entry = (struct blob_cache_entry *)tls_out->p;
   entry->uncompressed_size = (uint32_t)size;

   size_t compressed_size =
         util_compress_deflate(data, size, entry->compressed_data, max_buf);
   if (!compressed_size) {
      return;
   }

   size_t entry_size_sz = compressed_size + sizeof(*entry);
   if (entry_size_sz > (size_t)LONG_MAX) {
      return;
   }

   signed long entry_size = (signed long)entry_size_sz;
   /* The curly brackets are here to only trace the blob_put_cb call */
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

   /* This is what Android EGL defines as the maxValueSize in egl_cache_t */
   const signed long max_blob_size = 64 * 1024;

   /* Dynamic per-thread reusable input buffer */
   struct tls_in_buf *tls_in = tls_get_in();
   if (!tls_in || tls_in->cap < (size_t)max_blob_size) {
      return NULL;
   }

   struct blob_cache_entry *entry = (struct blob_cache_entry *)tls_in->p;

   signed long entry_size;
   /* The curly brackets are here to only trace the blob_get_cb call */
   {
      MESA_TRACE_SCOPE("blob_get");
      entry_size = cache->blob_get_cb(key, CACHE_KEY_SIZE, entry, max_blob_size);
   }

   if (entry_size <= 0 ||
       entry_size > max_blob_size ||
       entry_size < (signed long)sizeof(*entry)) {
      return NULL;
   }

   size_t sz_entry = (size_t)entry_size;
   size_t hdr = sizeof(*entry);
   size_t compressed_size = sz_entry - hdr;

   size_t uncomp = (size_t)entry->uncompressed_size;
   if (uncomp == 0) {
      return NULL;
   }

   void *data = malloc(uncomp);
   if (!data) {
      return NULL;
   }

   bool ret = util_compress_inflate(entry->compressed_data, compressed_size,
                                    data, uncomp);
   if (!ret) {
      free(data);
      return NULL;
   }

   if (size)
      *size = uncomp;

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
      if (filename == NULL)
         goto done;

      /* If the cache is too large, evict something else first. */
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
   /* Fast exit: zero-sized entries are pointless and can be skipped */
   if (unlikely(size == 0))
      return;

   if (!util_queue_is_initialized(&cache->cache_queue))
      return;

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

   /* Fast exit for zero-length data; free handed ownership */
   if (unlikely(size == 0)) {
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
      /* Ownership transfer failed; free caller-provided data to avoid leak. */
      free(data);
   }
}

void *
disk_cache_get(struct disk_cache *cache, const cache_key key, size_t *size)
{
   void *buf = NULL;

   if (size)
      *size = 0;

   if (cache->foz_ro_cache)
      buf = disk_cache_load_item_foz(cache->foz_ro_cache, key, size);

   if (!buf) {
      if (cache->blob_get_cb) {
         buf = blob_get_compressed(cache, key, size);
      } else if (cache->type == DISK_CACHE_SINGLE_FILE) {
         buf = disk_cache_load_item_foz(cache, key, size);
      } else if (cache->type == DISK_CACHE_DATABASE) {
         buf = disk_cache_db_load_item(cache, key, size);
      } else if (cache->type == DISK_CACHE_MULTI_FILE) {
         char *filename = disk_cache_get_cache_filename(cache, key);
         if (filename)
            buf = disk_cache_load_item(cache, filename, size);
         /* Ownership of filename is managed by disk_cache_load_item */
      }
   }

   if (unlikely(cache->stats.enabled)) {
      if (buf)
         p_atomic_inc(&cache->stats.hits);
      else
         p_atomic_inc(&cache->stats.misses);
   }

   return buf;
}

/* -----------------------------------------------------------------------------
 * Fast key compare
 * ---------------------------------------------------------------------------*/
/* Fast equality for 20-byte keys without function-call overhead.
 * Falls back to memcmp if CACHE_KEY_SIZE changes.
 */
#if defined(CACHE_KEY_SIZE) && (CACHE_KEY_SIZE != 20)
#  define CACHE_KEY_FASTCMP(a,b) (memcmp((a),(b),CACHE_KEY_SIZE) == 0)
#else
static inline bool
cache_key_equals_fast(const unsigned char *a, const unsigned char *b)
{
   uint64_t a0, a1, b0, b1;
   uint32_t a2, b2;
   memcpy(&a0, a + 0,  sizeof(a0));
   memcpy(&a1, a + 8,  sizeof(a1));
   memcpy(&a2, a + 16, sizeof(a2));
   memcpy(&b0, b + 0,  sizeof(b0));
   memcpy(&b1, b + 8,  sizeof(b1));
   memcpy(&b2, b + 16, sizeof(b2));
   /* Use bitwise AND to avoid early-branching; compilers emit set/and/test */
   return (a0 == b0) & (a1 == b1) & (a2 == b2);
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
      /* Only store the first word to satisfy has_key via blob_get_cb fast path. */
      cache->blob_put_cb(key, CACHE_KEY_SIZE, &first_word, (signed long)sizeof(uint32_t));
      return;
   }

   if (cache->path_init_failed)
      return;

   entry = &cache->stored_keys[i * CACHE_KEY_SIZE];

   memcpy(entry, key, CACHE_KEY_SIZE);
}

/* This function lets us test whether a given key was previously
 * stored in the cache with disk_cache_put_key(). The implement is
 * efficient by not using syscalls or hitting the disk. It's not
 * race-free, but the races are benign. If we race with someone else
 * calling disk_cache_put_key, then that's just an extra cache miss and an
 * extra recompile.
 */
bool
disk_cache_has_key(struct disk_cache *cache, const cache_key key)
{
   uint32_t first_word;
   int i;
   unsigned char *entry;

   memcpy(&first_word, key, sizeof(first_word));

   if (cache->blob_get_cb) {
      uint32_t blob = 0;
      /* Non-zero return means found (callback returns bytes read). */
      return cache->blob_get_cb(key, CACHE_KEY_SIZE, &blob, (signed long)sizeof(uint32_t)) != 0;
   }

   if (cache->path_init_failed)
      return false;

   i = CPU_TO_LE32(first_word) & CACHE_INDEX_KEY_MASK;
   entry = &cache->stored_keys[i * CACHE_KEY_SIZE];

   /* Prefetch the entry to hide L1 miss latency on the hot path */
   PREFETCH_R(entry);

   return CACHE_KEY_FASTCMP(entry, key);
}

/* -----------------------------------------------------------------------------
 * Hashing / callbacks
 * ---------------------------------------------------------------------------*/
void
disk_cache_compute_key(struct disk_cache *cache, const void *data, size_t size,
                       cache_key key)
{
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
