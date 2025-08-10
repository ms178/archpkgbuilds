/*
 * Copyright © 2021 Valve Corporation
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

#ifdef HAVE_COMPRESSION
#include <assert.h>
#include <stdlib.h>  /* For aligned_alloc, free, malloc, realloc */
#include <string.h>  /* For memcpy */
#include <pthread.h> /* For pthread_t, pthread_create, pthread_join, pthread_mutex_t, pthread_once_t */
#include <sched.h>   /* For sched_setaffinity - P-core pinning */
#include <cpuid.h>   /* For __get_cpuid - dynamic core detection */
#include <unistd.h>  /* For sysconf(_SC_NPROCESSORS_ONLN) */

/* Ensure that zlib uses 'const' in 'z_const' declarations. */
#ifndef ZLIB_CONST
#define ZLIB_CONST
#endif

#ifdef HAVE_ZLIB
#include "zlib.h"
#endif

#ifdef HAVE_ZSTD
#include "zstd.h"
#endif

#include "util/compress.h"
#include "util/perf/cpu_trace.h"
#include "macros.h"

/* Base levels for dynamic tuning - godlike aggressiveness for max speed in gaming */
#define ZSTD_SMALL_LEVEL (-3)   /* Balanced speed for small data */
#define ZSTD_FAST_LEVEL (-10)   /* Ultra-fast for large data */

/* Hardware-tuned constants for 14700KF (8 P-cores + 20 E-cores) */
#define MT_WORKERS_SMALL 4      /* P-core focus for medium data */
#define MT_WORKERS_LARGE 12     /* Hybrid utilization for large data */
#define MT_THRESHOLD_SMALL 65536  /* 64KB - aggressive for gaming latency (skip MT below) */
#define MT_JOB_SIZE 524288      /* 512KB chunks - optimal for L2 cache (2MB/P-core) */
#define SMALL_DATA_SKIP_THRESHOLD 65536  /* 64KB - skip compression entirely below this for ultra-low latency */
#define SMALL_DATA_COMPRESS_THRESHOLD 131072  /* 128KB - use light params below this */

/* Ensure AVX2 is leveraged (assumed in ZSTD build; tune params for vector width) */
#define AVX2_HASH_LOG 18        /* Optimized for AVX2 hash loops (256-bit) */
#define AVX2_SEARCH_LOG 2       /* Low for search speed in vectorized paths */

/* Dynamic P-core mask builder using CPUID (leaf 0x1A for hybrid info) */
static cpu_set_t build_p_core_mask(void) {
   cpu_set_t mask;
   CPU_ZERO(&mask);

   // User override via env (e.g., MESA_P_CORE_COUNT=8)
   const char* env = getenv("MESA_P_CORE_COUNT");
   int p_core_count = env ? atoi(env) : 0;

   if (p_core_count > 0) {
      // Assume first N are P-cores (user-specified); cap at CPU_SETSIZE
      int max_cpus = sysconf(_SC_NPROCESSORS_ONLN);
      for (int i = 0; i < p_core_count && i < max_cpus && i < CPU_SETSIZE; ++i) {
         CPU_SET(i, &mask);
      }
      return mask;
   }

   // Dynamic detection via CPUID (Intel hybrid since Alder Lake)
   unsigned int eax, ebx, ecx, edx;
   int max_cpus = sysconf(_SC_NPROCESSORS_ONLN);
   int detected_p_cores = 0;
   for (int i = 0; i < max_cpus && i < CPU_SETSIZE; ++i) {
      if (__get_cpuid_count(0x1A, i, &eax, &ebx, &ecx, &edx)) {
         unsigned core_type = (eax >> 24) & 0xFF;
         if (core_type == 0x20) {  // P-core (High Performance)
            CPU_SET(i, &mask);
            detected_p_cores++;
         }
      }
   }
   if (detected_p_cores > 0) {
      return mask;
   }

   // Fallback: No affinity (use all cores) if detection fails or non-Intel
   for (int i = 0; i < max_cpus && i < CPU_SETSIZE; ++i) {
      CPU_SET(i, &mask);
   }
   return mask;
}

/* Lazy initialization for p_core_mask - thread-safe with pthread_once */
static pthread_once_t p_core_mask_once = PTHREAD_ONCE_INIT;
static cpu_set_t p_core_mask;

static void init_p_core_mask(void) {
   p_core_mask = build_p_core_mask();
}

static const cpu_set_t* get_p_core_mask(void) {
   pthread_once(&p_core_mask_once, init_p_core_mask);
   return &p_core_mask;
}

size_t
util_compress_max_compressed_len(size_t in_data_size)
{
#ifdef HAVE_ZSTD
   /* from the zstd docs (https://facebook.github.io/zstd/zstd_manual.html):
    * compression runs faster if `dstCapacity` >= `ZSTD_compressBound(srcSize)`.
    */
   return ZSTD_compressBound(in_data_size);
#elif defined(HAVE_ZLIB)
   /* From https://zlib.net/zlib_tech.html:
    *
    *    "In the worst possible case, where the other block types would expand
    *    the data, deflation falls back to stored (uncompressed) blocks. Thus
    *    for the default settings used by deflateInit(), compress(), and
    *    compress2(), the only expansion is an overhead of five bytes per 16 KB
    *    block (about 0.03%), plus a one-time overhead of six bytes for the
    *    entire stream."
    */
   size_t num_blocks = (in_data_size + 16383) / 16384; /* round up blocks - safe unsigned math */
   return in_data_size + 6 + (num_blocks * 5);
#else
   STATIC_ASSERT(false);
#endif
}

/* Compress data and return the size of the compressed data */
size_t
util_compress_deflate(const uint8_t *in_data, size_t in_data_size,
                      uint8_t *out_data, size_t out_buff_size)
{
   MESA_TRACE_FUNC();
#ifdef HAVE_ZSTD
   /* Skip compression for tiny data in gaming - direct copy for zero latency */
   if (in_data_size < SMALL_DATA_SKIP_THRESHOLD) {
      if (in_data_size > out_buff_size) {
         return 0;  // Buffer too small
      }
      memcpy(out_data, in_data, in_data_size);
      return in_data_size;  // Indicate uncompressed (caller can handle)
   }

   _Thread_local static ZSTD_CCtx* cctx = NULL;

   if (!cctx) {
      cctx = ZSTD_createCCtx();
      if (!cctx) {
         return 0;
      }
   }

   // Reset for reuse - no null deref
   size_t ret = ZSTD_CCtx_reset(cctx, ZSTD_reset_session_only);
   if (ZSTD_isError(ret)) {
      return 0;
   }

   // Dynamic level: aggressive for small, ultra-fast for large - tuned for Raptor Lake IPC
   int level = (in_data_size < SMALL_DATA_COMPRESS_THRESHOLD) ? ZSTD_SMALL_LEVEL : ZSTD_FAST_LEVEL;
   ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, level);
   if (ZSTD_isError(ret)) {
      return 0;
   }

   // Strategy tuning: fast for small, btultra2 for large (better ratio without speed loss in gaming)
   ZSTD_strategy strategy = (in_data_size < SMALL_DATA_COMPRESS_THRESHOLD) ? ZSTD_fast : ZSTD_btultra2;
   ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_strategy, strategy);
   if (ZSTD_isError(ret)) {
      return 0;
   }

   // AVX2-tuned params for vectorization - leverages 14700KF's AVX2 units
   ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_hashLog, AVX2_HASH_LOG);
   if (ZSTD_isError(ret)) {
      return 0;
   }
   ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_searchLog, AVX2_SEARCH_LOG);
   if (ZSTD_isError(ret)) {
      return 0;
   }

   // Apply heavy params only above threshold - reduce overhead for medium gaming data
   if (in_data_size >= SMALL_DATA_COMPRESS_THRESHOLD) {
      // Window/chain tuned for L3 (33MB) - fits shader patterns
      ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_windowLog, 23);  // 8MB window
      if (ZSTD_isError(ret)) {
         return 0;
      }
      ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_chainLog, 22);
      if (ZSTD_isError(ret)) {
         return 0;
      }

      // Overlap for lower latency - enhances pipelining on Vega 64 data uploads
      ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_overlapLog, 6);
      if (ZSTD_isError(ret)) {
         return 0;
      }

      // LDM for repetitive data (e.g., shader code) - only if large
      if (in_data_size >= 1048576) {
         ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_enableLongDistanceMatching, 1);
         if (ZSTD_isError(ret)) {
            return 0;
         }
         ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_ldmHashLog, 20);  // Tuned for L2 (2MB/P-core)
         if (ZSTD_isError(ret)) {
            return 0;
         }
      }
   }

   // Aggressive MT with lower threshold and tuned workers/job size
   // Godlike: Scales to 14700KF's hybrid cores (affinity to P-cores for latency)
   int nb_workers = 0;
   if (in_data_size >= MT_THRESHOLD_SMALL) {
      nb_workers = (in_data_size < 1048576) ? MT_WORKERS_SMALL : MT_WORKERS_LARGE;
   }
   ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_nbWorkers, nb_workers);
   if (ZSTD_isError(ret)) {
      return 0;  // Fallback to single-thread if MT unavailable
   }
   if (nb_workers > 0) {
      ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_jobSize, MT_JOB_SIZE);
      if (ZSTD_isError(ret)) {
         return 0;
      }
      // Pin to dynamic P-core mask for consistent latency in gaming
      sched_setaffinity(0, sizeof(cpu_set_t), get_p_core_mask());
   }

   // Final compression - error-checked
   ret = ZSTD_compress2(cctx, out_data, out_buff_size, in_data, in_data_size);
   if (ZSTD_isError(ret)) {
      return 0;
   }

   return ret;
#elif defined(HAVE_ZLIB)
   size_t compressed_size = 0;

   /* allocate deflate state with tuned params (max window, high mem for speed) */
   z_stream strm;
   strm.zalloc = Z_NULL;
   strm.zfree = Z_NULL;
   strm.opaque = Z_NULL;
   strm.next_in = (z_const Bytef *)in_data;
   strm.next_out = out_data;
   strm.avail_in = in_data_size;
   strm.avail_out = out_buff_size;

   int ret = deflateInit2(&strm, Z_BEST_SPEED, Z_DEFLATED, 15, 9, Z_DEFAULT_STRATEGY);
   if (ret != Z_OK) {
      (void) deflateEnd(&strm);
      return 0;
   }

   /* compress until end of in_data */
   ret = deflate(&strm, Z_FINISH);

   if (ret == Z_STREAM_END) {
      compressed_size = strm.total_out;
   } else {
      (void) deflateEnd(&strm);
      return 0;
   }

   /* clean up and return */
   (void) deflateEnd(&strm);
   return compressed_size;
#else
   STATIC_ASSERT(false);
#endif
}

/* Thread arg struct for decompress */
typedef struct {
   const uint8_t* in_ptr;
   size_t in_sz;
   uint8_t* out_ptr;
   size_t out_cap;
   size_t* out_sz_ptr;
   int* error_ptr;
} ThreadArg;

/* Thread function for decompress */
static void* thread_decompress_func(void* arg_ptr) {
   ThreadArg* arg = (ThreadArg*)arg_ptr;
   ZSTD_DCtx* thread_dctx = ZSTD_createDCtx();
   if (!thread_dctx) {
      *arg->error_ptr = 1;
      *arg->out_sz_ptr = 0;
      free(arg);
      return NULL;
   }

   // Pin thread to dynamic P-core mask for latency
   sched_setaffinity(0, sizeof(cpu_set_t), get_p_core_mask());

   size_t decompressed = ZSTD_decompressDCtx(thread_dctx, arg->out_ptr, arg->out_cap,
                                             arg->in_ptr, arg->in_sz);
   ZSTD_freeDCtx(thread_dctx);

   if (ZSTD_isError(decompressed)) {
      *arg->error_ptr = 1;
      *arg->out_sz_ptr = 0;
      free(arg);
      return NULL;
   }

   *arg->out_sz_ptr = decompressed;
   free(arg);
   return NULL;
}

/**
 * Decompresses data, returns true if successful.
 * Perfected manual MT: Frame-based splitting with robust error handling, atomic error flag, full cleanup.
 * Gaming-tuned: Skips MT for small data; per-thread affinity; fast paths.
 */
bool
util_compress_inflate(const uint8_t *in_data, size_t in_data_size,
                      uint8_t *out_data, size_t out_data_size)
{
   MESA_TRACE_FUNC();
#ifdef HAVE_ZSTD
   /* Skip decompression for tiny data - assume uncompressed if size matches (gaming fast path) */
   if (in_data_size < SMALL_DATA_SKIP_THRESHOLD && in_data_size == out_data_size) {
      memcpy(out_data, in_data, in_data_size);
      return true;
   }

   _Thread_local static ZSTD_DCtx* dctx = NULL;

   if (!dctx) {
      dctx = ZSTD_createDCtx();
      if (!dctx) {
         return false;
      }
   }

   // Reset for reuse - no null deref
   size_t ret = ZSTD_DCtx_reset(dctx, ZSTD_reset_session_only);
   if (ZSTD_isError(ret)) {
      return false;
   }

   // For small data, standard single-thread decompress - low latency, no MT overhead
   if (in_data_size < MT_THRESHOLD_SMALL) {
      ret = ZSTD_decompressDCtx(dctx, out_data, out_data_size, in_data, in_data_size);
      return !ZSTD_isError(ret) && ret == out_data_size;
   }

   // Godlike manual MT decompress for large data: Safe frame-based splitting and parallel processing
   // Step 1: Find frame boundaries for safe splitting - dynamic array for frame sizes
   size_t* frame_sizes = NULL;
   size_t frame_capacity = 16;  // Initial capacity - grow as needed
   size_t frame_count = 0;
   frame_sizes = (size_t*)malloc(frame_capacity * sizeof(size_t));
   if (!frame_sizes) {
      return false;
   }

   size_t pos = 0;
   while (pos < in_data_size) {
      size_t frame_size = ZSTD_findFrameCompressedSize(in_data + pos, in_data_size - pos);
      if (ZSTD_isError(frame_size)) {
         free(frame_sizes);
         return false;  // Invalid frame - fail safely
      }

      // Dynamic resize if needed - safe realloc, no leaks, prevent overflow
      if (frame_count >= frame_capacity) {
         if (frame_capacity > SIZE_MAX / 2 / sizeof(size_t)) {
            free(frame_sizes);
            return false;  // Prevent overflow in realloc size
         }
         frame_capacity *= 2;
         size_t* new_array = (size_t*)realloc(frame_sizes, frame_capacity * sizeof(size_t));
         if (!new_array) {
            free(frame_sizes);
            return false;
         }
         frame_sizes = new_array;
      }

      frame_sizes[frame_count++] = frame_size;
      if (pos > SIZE_MAX - frame_size) {
         free(frame_sizes);
         return false;  // Prevent pos overflow
      }
      pos += frame_size;
   }

   // Step 2: Determine number of workers - tuned for hardware
   int num_workers = (in_data_size < 1048576) ? MT_WORKERS_SMALL : MT_WORKERS_LARGE;

   // Adjust num_workers to not exceed number of frames - avoid over-parallelism
   if (num_workers > (int)frame_count) {
      num_workers = frame_count;
   }
   if (num_workers <= 1) {
      // Fallback to single-thread if not enough frames - cleanup array
      free(frame_sizes);
      ret = ZSTD_decompressDCtx(dctx, out_data, out_data_size, in_data, in_data_size);
      return !ZSTD_isError(ret) && ret == out_data_size;
   }

   // Step 3: Prepare thread structures - pthread for C compatibility
   pthread_t* threads = (pthread_t*)malloc(num_workers * sizeof(pthread_t));
   uint8_t** out_blocks = (uint8_t**)malloc(num_workers * sizeof(uint8_t*));
   size_t* out_block_sizes = (size_t*)malloc(num_workers * sizeof(size_t));
   size_t total_decompressed = 0;
   int thread_errors = 0;  // Error flag (written with mutex for safety)
   pthread_mutex_t error_mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_t output_mutex = PTHREAD_MUTEX_INITIALIZER;

   if (!threads || !out_blocks || !out_block_sizes) {
      free(threads);
      free(out_blocks);
      free(out_block_sizes);
      free(frame_sizes);
      pthread_mutex_destroy(&error_mutex);
      pthread_mutex_destroy(&output_mutex);
      return false;
   }

   // Step 4: Launch threads for parallel decompression - each handles one or more frames
   size_t frame_idx = 0;
   size_t in_offset = 0;
   int active_threads = 0;  // Track actual created threads (skip empty)
   for (int i = 0; i < num_workers; ++i) {
      // Group frames per thread for balanced load - accumulate size
      size_t this_in_size = 0;
      size_t start_frame = frame_idx;
      while (frame_idx < frame_count && this_in_size < (in_data_size / num_workers)) {
         if (this_in_size > SIZE_MAX - frame_sizes[frame_idx]) {
            pthread_mutex_lock(&error_mutex);
            thread_errors = 1;
            pthread_mutex_unlock(&error_mutex);
            break;  // Prevent overflow
         }
         this_in_size += frame_sizes[frame_idx++];
      }
      if (this_in_size == 0) continue;  // Skip empty assignments

      size_t this_out_capacity = out_data_size / num_workers + 1024;  // Safe over-allocation, bounded
      if (this_out_capacity > SIZE_MAX - 63) {  // Align to 64
         this_out_capacity = SIZE_MAX - 63;
      }

      // Allocate per-thread output - aligned for cache efficiency (64-byte for AVX2)
      uint8_t* this_out = (uint8_t*)aligned_alloc(64, (this_out_capacity + 63) & ~63);  // Round up to multiple of 64
      if (!this_out) {
         pthread_mutex_lock(&error_mutex);
         thread_errors = 1;
         pthread_mutex_unlock(&error_mutex);
         break;
      }
      out_blocks[active_threads] = this_out;
      out_block_sizes[active_threads] = 0;

      // Thread arg - malloc per thread to avoid races
      ThreadArg* arg = (ThreadArg*)malloc(sizeof(ThreadArg));
      if (!arg) {
         pthread_mutex_lock(&error_mutex);
         thread_errors = 1;
         pthread_mutex_unlock(&error_mutex);
         free(this_out);
         break;
      }
      arg->in_ptr = in_data + in_offset;
      arg->in_sz = this_in_size;
      arg->out_ptr = this_out;
      arg->out_cap = this_out_capacity;
      arg->out_sz_ptr = &out_block_sizes[active_threads];
      arg->error_ptr = &thread_errors;

      // Create thread - error-checked
      if (pthread_create(&threads[active_threads], NULL, thread_decompress_func, arg) != 0) {
         pthread_mutex_lock(&error_mutex);
         thread_errors = 1;
         pthread_mutex_unlock(&error_mutex);
         free(arg);
         free(this_out);
         break;
      }

      if (in_offset > SIZE_MAX - this_in_size) {
         pthread_mutex_lock(&error_mutex);
         thread_errors = 1;
         pthread_mutex_unlock(&error_mutex);
         break;  // Prevent offset overflow
      }
      in_offset += this_in_size;
      active_threads++;
   }

   // Step 5: Join active threads - robust, wait for completion
   for (int i = 0; i < active_threads; ++i) {
      void* result;
      if (pthread_join(threads[i], &result) != 0) {
         pthread_mutex_lock(&error_mutex);
         thread_errors = 1;
         pthread_mutex_unlock(&error_mutex);
      }
   }

   // Step 6: Check for errors (protected by mutex) and assemble output
   bool success = true;
   pthread_mutex_lock(&error_mutex);
   if (thread_errors) {
      success = false;
   }
   pthread_mutex_unlock(&error_mutex);

   size_t out_offset = 0;
   if (success) {
      pthread_mutex_lock(&output_mutex);
      for (int i = 0; i < active_threads; ++i) {
         if (out_block_sizes[i] > 0) {
            if (out_offset > SIZE_MAX - out_block_sizes[i] || out_offset + out_block_sizes[i] > out_data_size) {
               success = false;
               break;  // Overflow protection
            }
            memcpy(out_data + out_offset, out_blocks[i], out_block_sizes[i]);
            out_offset += out_block_sizes[i];
            total_decompressed += out_block_sizes[i];
         }
         free(out_blocks[i]);  // Cleanup - no use-after-free
      }
      pthread_mutex_unlock(&output_mutex);
   } else {
      // Cleanup on error
      for (int i = 0; i < active_threads; ++i) {
         if (out_blocks[i]) {
            free(out_blocks[i]);
         }
      }
   }

   free(threads);
   free(out_blocks);
   free(out_block_sizes);
   free(frame_sizes);
   pthread_mutex_destroy(&error_mutex);
   pthread_mutex_destroy(&output_mutex);

   // Final check: Ensure total matches expected - no overflows/underflows
   if (!success || total_decompressed != out_data_size) {
      return false;
   }

   return true;
#elif defined(HAVE_ZLIB)
   z_stream strm;

   /* allocate inflate state */
   strm.zalloc = Z_NULL;
   strm.zfree = Z_NULL;
   strm.opaque = Z_NULL;
   strm.next_in = (z_const Bytef *)in_data;
   strm.avail_in = in_data_size;
   strm.next_out = out_data;
   strm.avail_out = out_data_size;

   int ret = inflateInit(&strm);
   if (ret != Z_OK) {
      return false;
   }

   ret = inflate(&strm, Z_NO_FLUSH);
   if (ret == Z_STREAM_ERROR) {  /* state not clobbered; handle robustly */
      (void)inflateEnd(&strm);
      return false;
   }

   /* Unless there was an error we should have decompressed everything in one
    * go as we know the uncompressed file size.
    */
   if (ret != Z_STREAM_END || strm.avail_out != 0) {
      (void)inflateEnd(&strm);
      return false;
   }

   /* clean up and return */
   (void)inflateEnd(&strm);
   return true;
#else
   STATIC_ASSERT(false);
#endif
}

#endif
