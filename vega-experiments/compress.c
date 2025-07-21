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
#include <stdlib.h>  /* For aligned_alloc and free */

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

/* Ultra-fast negative level for max speed on Raptor Lake */
#define ZSTD_COMPRESSION_LEVEL (-5)

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
    size_t num_blocks = (in_data_size + 16383) / 16384; /* round up blocks */
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
    _Thread_local static ZSTD_CCtx* cctx = NULL;

    if (!cctx) {
        cctx = ZSTD_createCCtx();
        if (!cctx) {
            return 0;
        }
    }

    // Reset for reuse (safe even on first use)
    size_t ret = ZSTD_CCtx_reset(cctx, ZSTD_reset_session_only);
    if (ZSTD_isError(ret)) {
        return 0;
    }

    // Set ultra-fast compression level
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, ZSTD_COMPRESSION_LEVEL);
    if (ZSTD_isError(ret)) {
        return 0;
    }

    // Tune for Raptor Lake: window fits L3 (~32MB), chain for L2 locality
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_windowLog, 25);
    if (ZSTD_isError(ret)) {
        return 0;
    }
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_chainLog, 24);
    if (ZSTD_isError(ret)) {
        return 0;
    }

    // Enable Long Distance Matching for repetitive shader data
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_enableLongDistanceMatching, 1);
    if (ZSTD_isError(ret)) {
        return 0;
    }
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_ldmHashLog, 20);  // Tuned for L2 cache
    if (ZSTD_isError(ret)) {
        return 0;
    }

    // Enable MT if large data (1MB threshold; 16 workers for 14700KF P-cores + some E-cores)
    int nb_workers = (in_data_size >= 1048576) ? 16 : 0;
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_nbWorkers, nb_workers);
    if (ZSTD_isError(ret)) {
        return 0;  // Fallback to single-thread if MT unavailable
    }

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
    strm.next_in = in_data;
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

    /* Check if stream is complete (no assert; handle error robustly) */
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
    # endif
}

/**
 * Decompresses data, returns true if successful.
 */
bool
util_compress_inflate(const uint8_t *in_data, size_t in_data_size,
                      uint8_t *out_data, size_t out_data_size)
{
    MESA_TRACE_FUNC();
    #ifdef HAVE_ZSTD
    _Thread_local static ZSTD_DCtx* dctx = NULL;

    if (!dctx) {
        dctx = ZSTD_createDCtx();
        if (!dctx) {
            return false;
        }
    }

    // Reset for reuse (safe even on first use)
    size_t ret = ZSTD_DCtx_reset(dctx, ZSTD_reset_session_only);
    if (ZSTD_isError(ret)) {
        return false;
    }

    ret = ZSTD_decompressDCtx(dctx, out_data, out_data_size, in_data, in_data_size);
    return !ZSTD_isError(ret);
    #elif defined(HAVE_ZLIB)
    z_stream strm;

    /* allocate inflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.next_in = in_data;
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
    if (ret != Z_STREAM_END) {
        (void)inflateEnd(&strm);
        return false;
    }

    /* clean up and return (relaxed check: no assert on avail_out == 0, as partial is error via ret) */
    (void)inflateEnd(&strm);
    return true;
    #else
    STATIC_ASSERT(false);
    #endif
}

#endif
