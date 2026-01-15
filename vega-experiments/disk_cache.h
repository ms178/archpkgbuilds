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

#ifndef DISK_CACHE_H
#define DISK_CACHE_H

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#include <stdio.h>
#include "util/build_id.h"
#endif
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/stat.h>
#include "util/mesa-sha1.h"
#include "util/detect_os.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Size of cache keys in bytes - now uses BLAKE3 (32 bytes) via SHA1_DIGEST_LENGTH alias */
#define CACHE_KEY_SIZE SHA1_DIGEST_LENGTH

#define CACHE_DIR_NAME "mesa_shader_cache"
#define CACHE_DIR_NAME_SF "mesa_shader_cache_sf"
#define CACHE_DIR_NAME_DB "mesa_shader_cache_db"

typedef uint8_t cache_key[CACHE_KEY_SIZE];

/* WARNING: 3rd party applications might be reading the cache item metadata.
 * Do not change these values without making the change widely known.
 * Please contact Valve developers and make them aware of this change.
 */
#define CACHE_ITEM_TYPE_UNKNOWN  0x0
#define CACHE_ITEM_TYPE_GLSL     0x1

typedef void
(*disk_cache_put_cb) (const void *key, signed long keySize,
                      const void *value, signed long valueSize);

typedef signed long
(*disk_cache_get_cb) (const void *key, signed long keySize,
                      void *value, signed long valueSize);

struct cache_item_metadata {
   /**
    * The cache item type. This could be used to identify a GLSL cache item,
    * a certain type of IR (tgsi, nir, etc), or signal that it is the final
    * binary form of the shader.
    */
   uint32_t type;

   /** GLSL cache item metadata */
   cache_key *keys;   /* sha1 list of shaders that make up the cache item */
   uint32_t num_keys;
};

struct disk_cache;

#ifdef HAVE_DLADDR
static inline bool
disk_cache_get_function_timestamp(void *ptr, uint32_t* timestamp)
{
   Dl_info info;
   struct stat st;
   if (!dladdr(ptr, &info) || !info.dli_fname) {
      return false;
   }
   if (stat(info.dli_fname, &st)) {
      return false;
   }

   if (!st.st_mtime) {
      fprintf(stderr, "Mesa: The provided filesystem timestamp for the cache "
              "is bogus! Disabling On-disk cache.\n");
      return false;
   }

   *timestamp = (uint32_t)st.st_mtime;

   return true;
}

static inline bool
disk_cache_get_function_identifier(void *ptr, struct mesa_sha1 *ctx)
{
   uint32_t timestamp;

#if defined(HAVE_BUILD_ID) && HAVE_BUILD_ID
   const struct build_id_note *note = build_id_find_nhdr_for_addr(ptr);
   if (note) {
      _mesa_sha1_update(ctx, build_id_data(note), build_id_length(note));
      return true;
   }
#endif

   if (disk_cache_get_function_timestamp(ptr, &timestamp)) {
      _mesa_sha1_update(ctx, &timestamp, sizeof(timestamp));
      return true;
   }

   return false;
}
#elif DETECT_OS_WINDOWS
bool
disk_cache_get_function_identifier(void *ptr, struct mesa_sha1 *ctx);
#else
static inline bool
disk_cache_get_function_identifier(void *ptr, struct mesa_sha1 *ctx)
{
   (void)ptr;
   (void)ctx;
   return false;
}
#endif

/* Provide inlined stub functions if the shader cache is disabled. */

#ifdef ENABLE_SHADER_CACHE

struct disk_cache *
disk_cache_create(const char *gpu_name, const char *timestamp,
                  uint64_t driver_flags);

struct disk_cache *
disk_cache_create_custom(const char *gpu_name, const char *driver_id,
                         uint64_t driver_flags, const char *cache_dir_name,
                         uint32_t max_size);

void
disk_cache_destroy(struct disk_cache *cache);

void
disk_cache_wait_for_idle(struct disk_cache *cache);

void
disk_cache_remove(struct disk_cache *cache, const cache_key key);

void
disk_cache_put(struct disk_cache *cache, const cache_key key,
               const void *data, size_t size,
               struct cache_item_metadata *cache_item_metadata);

void
disk_cache_put_nocopy(struct disk_cache *cache, const cache_key key,
                      void *data, size_t size,
                      struct cache_item_metadata *cache_item_metadata);

void *
disk_cache_get(struct disk_cache *cache, const cache_key key, size_t *size);

void
disk_cache_put_key(struct disk_cache *cache, const cache_key key);

bool
disk_cache_has_key(struct disk_cache *cache, const cache_key key);

void
disk_cache_compute_key(struct disk_cache *cache, const void *data, size_t size,
                       cache_key key);

void
disk_cache_set_callbacks(struct disk_cache *cache, disk_cache_put_cb put,
                         disk_cache_get_cb get);

#else

static inline struct disk_cache *
disk_cache_create(const char *gpu_name, const char *timestamp,
                  uint64_t driver_flags)
{
   (void)gpu_name;
   (void)timestamp;
   (void)driver_flags;
   return NULL;
}

static inline struct disk_cache *
disk_cache_create_custom(const char *gpu_name, const char *driver_id,
                         uint64_t driver_flags, const char *cache_dir_name,
                         uint32_t max_size)
{
   (void)gpu_name;
   (void)driver_id;
   (void)driver_flags;
   (void)cache_dir_name;
   (void)max_size;
   return NULL;
}

static inline void
disk_cache_destroy(struct disk_cache *cache)
{
   (void)cache;
}

static inline void
disk_cache_wait_for_idle(struct disk_cache *cache)
{
   (void)cache;
}

static inline void
disk_cache_put(struct disk_cache *cache, const cache_key key,
               const void *data, size_t size,
               struct cache_item_metadata *cache_item_metadata)
{
   (void)cache;
   (void)key;
   (void)data;
   (void)size;
   (void)cache_item_metadata;
}

static inline void
disk_cache_put_nocopy(struct disk_cache *cache, const cache_key key,
                      void *data, size_t size,
                      struct cache_item_metadata *cache_item_metadata)
{
   (void)cache;
   (void)key;
   (void)data;
   (void)size;
   (void)cache_item_metadata;
}

static inline void
disk_cache_remove(struct disk_cache *cache, const cache_key key)
{
   (void)cache;
   (void)key;
}

static inline uint8_t *
disk_cache_get(struct disk_cache *cache, const cache_key key, size_t *size)
{
   (void)cache;
   (void)key;
   (void)size;
   return NULL;
}

static inline void
disk_cache_put_key(struct disk_cache *cache, const cache_key key)
{
   (void)cache;
   (void)key;
}

static inline bool
disk_cache_has_key(struct disk_cache *cache, const cache_key key)
{
   (void)cache;
   (void)key;
   return false;
}

static inline void
disk_cache_compute_key(struct disk_cache *cache, const void *data, size_t size,
                       cache_key key)
{
   (void)cache;
   (void)data;
   (void)size;
   (void)key;
}

static inline void
disk_cache_set_callbacks(struct disk_cache *cache, disk_cache_put_cb put,
                         disk_cache_get_cb get)
{
   (void)cache;
   (void)put;
   (void)get;
}

#endif /* ENABLE_SHADER_CACHE */

#ifdef __cplusplus
}
#endif

#endif /* DISK_CACHE_H */
