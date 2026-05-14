/*
 * SPDX-License-Identifier: MIT
 *
 * glamor_image.c - accelerated PutImage/GetImage paths
 *
 * Optimized single-file variant with guarded PBO ring uploads and AVX2
 * plane-mask handling.
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_transform.h"
#include "servermd.h"

#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if defined(__AVX2__) && (defined(__GNUC__) || defined(__clang__))
#include <immintrin.h>
#define GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS 1
#else
#define GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS 0
#endif

#if defined(__BMI__) && (defined(__GNUC__) || defined(__clang__))
#include <x86intrin.h>
#define GLAMOR_IMAGE_HAVE_BMI_INTRINSICS 1
#else
#define GLAMOR_IMAGE_HAVE_BMI_INTRINSICS 0
#endif

#if defined(__BMI2__) && (defined(__GNUC__) || defined(__clang__))
#include <x86intrin.h>
#define GLAMOR_IMAGE_HAVE_BMI2_INTRINSICS 1
#else
#define GLAMOR_IMAGE_HAVE_BMI2_INTRINSICS 0
#endif

#if defined(__GNUC__) || defined(__clang__)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#define COLD        __attribute__((cold))
#define HOT         __attribute__((hot))
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
#define COLD
#define HOT
#endif

#ifndef GLAMOR_PIXMAP_PRIV_HAS_FBO
#define GLAMOR_PIXMAP_PRIV_HAS_FBO(priv) \
    ((priv) != NULL && (priv)->gl_fbo == GLAMOR_FBO_NORMAL)
#endif

#define GLAMOR_IMAGE_MAX_SCREENS 16

#define GLAMOR_IMAGE_PBO_RING_SIZE 4
#define GLAMOR_IMAGE_PBO_MIN_SIZE (64u * 1024u)
#define GLAMOR_IMAGE_PBO_MAX_SIZE (16u * 1024u * 1024u)
#define GLAMOR_IMAGE_PBO_THRESHOLD (64u * 1024u)

typedef enum {
    CPU_FEATURE_NONE = 0,
    CPU_FEATURE_AVX2 = 1u << 0,
    CPU_FEATURE_BMI  = 1u << 1,
    CPU_FEATURE_BMI2 = 1u << 2,
} cpu_feature_level;

typedef struct glamor_image_pbo_ring {
    Bool initialized;
    GLuint pbo[GLAMOR_IMAGE_PBO_RING_SIZE];
    GLsync fence[GLAMOR_IMAGE_PBO_RING_SIZE];
    Bool fence_valid[GLAMOR_IMAGE_PBO_RING_SIZE];
    int current_idx;
    size_t current_size;
} glamor_image_pbo_ring;

static glamor_image_pbo_ring glamor_image_pbo_rings[GLAMOR_IMAGE_MAX_SCREENS];

static cpu_feature_level
get_cpu_features(void)
{
    static cpu_feature_level cached = CPU_FEATURE_NONE;
    static int initialized = 0;

    if (!initialized) {
#if defined(__GNUC__) || defined(__clang__)
#if defined(__x86_64__) || defined(__i386__)
        __builtin_cpu_init();
#endif
#if GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS
        if (__builtin_cpu_supports("avx2"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_AVX2);
#endif
#if GLAMOR_IMAGE_HAVE_BMI_INTRINSICS
        if (__builtin_cpu_supports("bmi"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_BMI);
#endif
#if GLAMOR_IMAGE_HAVE_BMI2_INTRINSICS
        if (__builtin_cpu_supports("bmi2"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_BMI2);
#endif
#endif
        initialized = 1;
    }

    return cached;
}

static inline Bool
glamor_image_can_fast_upload(const GCPtr gc)
{
    return gc && gc->alu == GXcopy &&
        glamor_pm_is_solid(gc->depth, gc->planemask);
}

static inline size_t
glamor_image_safe_mul_size(size_t a, size_t b)
{
    if (a == 0 || b == 0)
        return 0;

    if (UNLIKELY(a > SIZE_MAX / b))
        return 0;

    return a * b;
}

static int
glamor_image_get_screen_index(ScreenPtr screen)
{
    int idx;

    if (!screen)
        return -1;

    idx = screen->myNum;
    if (idx < 0 || idx >= GLAMOR_IMAGE_MAX_SCREENS)
        return -1;

    return idx;
}

static void
glamor_image_pbo_ring_init(glamor_image_pbo_ring *ring)
{
    if (!ring)
        return;

    memset(ring, 0, sizeof(*ring));
    ring->current_idx = -1;
    ring->current_size = GLAMOR_IMAGE_PBO_MIN_SIZE;
    ring->initialized = TRUE;
}

static Bool
glamor_image_pbo_ring_acquire(int screen_idx, size_t required,
                              GLuint *out_pbo, void **out_map)
{
    glamor_image_pbo_ring *ring;
    int slot;
    Bool need_realloc = FALSE;

    if (screen_idx < 0 || screen_idx >= GLAMOR_IMAGE_MAX_SCREENS ||
        !out_pbo || !out_map)
        return FALSE;

    ring = &glamor_image_pbo_rings[screen_idx];
    if (!ring->initialized)
        glamor_image_pbo_ring_init(ring);

    if (required > ring->current_size) {
        if (required > GLAMOR_IMAGE_PBO_MAX_SIZE)
            return FALSE;
        ring->current_size = required;
        need_realloc = TRUE;
    }

    slot = (ring->current_idx + 1) % GLAMOR_IMAGE_PBO_RING_SIZE;

    if (ring->fence_valid[slot] && ring->fence[slot]) {
        GLenum status = glClientWaitSync(ring->fence[slot], 0, 0);
        if (status == GL_TIMEOUT_EXPIRED || status == GL_WAIT_FAILED)
            need_realloc = TRUE;

        glDeleteSync(ring->fence[slot]);
        ring->fence[slot] = NULL;
        ring->fence_valid[slot] = FALSE;
    }

    if (ring->pbo[slot] == 0) {
        glGenBuffers(1, &ring->pbo[slot]);
        if (ring->pbo[slot] == 0)
            return FALSE;
        need_realloc = TRUE;
    }

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, ring->pbo[slot]);

    if (need_realloc) {
        glBufferData(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr) ring->current_size,
                     NULL, GL_STREAM_DRAW);
    }

    *out_map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr) required,
                                GL_MAP_WRITE_BIT | GL_MAP_UNSYNCHRONIZED_BIT);
    if (!*out_map) {
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        return FALSE;
    }

    ring->current_idx = slot;
    *out_pbo = ring->pbo[slot];
    return TRUE;
}

static void
glamor_image_pbo_ring_record_fence(int screen_idx)
{
    glamor_image_pbo_ring *ring;
    int slot;

    if (screen_idx < 0 || screen_idx >= GLAMOR_IMAGE_MAX_SCREENS)
        return;

    ring = &glamor_image_pbo_rings[screen_idx];
    if (!ring->initialized || ring->current_idx < 0)
        return;

    slot = ring->current_idx;

    if (ring->fence_valid[slot] && ring->fence[slot])
        glDeleteSync(ring->fence[slot]);

    ring->fence[slot] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
    ring->fence_valid[slot] = ring->fence[slot] ? TRUE : FALSE;
}

static inline void
glamor_image_memcpy_streaming(void * restrict dst,
                              const void * restrict src,
                              size_t n)
{
    if (UNLIKELY(n == 0))
        return;

#if GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS
    if ((get_cpu_features() & CPU_FEATURE_AVX2) != 0 &&
        n >= (size_t) (256 * 1024) &&
        (((uintptr_t) dst & 63u) == 0u)) {
        const char *src_ptr = (const char *) src;
        char *dst_ptr = (char *) dst;
        size_t chunks = n / 64u;
        size_t rem = n % 64u;

        for (size_t i = 0; i < chunks; i++) {
            size_t off = i * 64u;

            if ((i & 3u) == 0u)
                __builtin_prefetch(src_ptr + off + 512u, 0, 1);

            __m256i v0 = _mm256_loadu_si256((const __m256i *) (const void *) (src_ptr + off));
            __m256i v1 = _mm256_loadu_si256((const __m256i *) (const void *) (src_ptr + off + 32u));

            _mm256_stream_si256((__m256i *) (void *) (dst_ptr + off), v0);
            _mm256_stream_si256((__m256i *) (void *) (dst_ptr + off + 32u), v1);
        }

        _mm_sfence();

        if (rem)
            memcpy(dst_ptr + chunks * 64u, src_ptr + chunks * 64u, rem);
        return;
    }
#endif

    memcpy(dst, src, n);
}

static Bool
glamor_image_build_put_region(DrawablePtr drawable, GCPtr gc,
                              PixmapPtr pixmap, int x, int y, int w, int h,
                              RegionPtr region)
{
    int off_x = 0, off_y = 0;
    BoxRec image_box;
    BoxRec pixmap_box;
    RegionRec pixmap_region;

    if (UNLIKELY(!drawable || !pixmap || !region || w <= 0 || h <= 0))
        return FALSE;

    glamor_get_drawable_deltas(drawable, pixmap, &off_x, &off_y);

    image_box.x1 = x + off_x;
    image_box.y1 = y + off_y;
    image_box.x2 = x + off_x + w;
    image_box.y2 = y + off_y + h;

    RegionInit(region, &image_box, 1);

    pixmap_box.x1 = 0;
    pixmap_box.y1 = 0;
    pixmap_box.x2 = pixmap->drawable.width;
    pixmap_box.y2 = pixmap->drawable.height;

    RegionInit(&pixmap_region, &pixmap_box, 1);
    RegionIntersect(region, region, &pixmap_region);
    RegionUninit(&pixmap_region);

    if (gc && gc->pCompositeClip) {
        RegionRec clip_region;

        RegionNull(&clip_region);
        RegionCopy(&clip_region, gc->pCompositeClip);
        RegionTranslate(&clip_region, off_x, off_y);
        RegionIntersect(region, region, &clip_region);
        RegionUninit(&clip_region);
    }

    return RegionNotEmpty(region) ? TRUE : FALSE;
}

static Bool
glamor_put_image_zpixmap_gl(DrawablePtr drawable, GCPtr gc, int depth,
                            int x, int y, int w, int h, const char *bits)
{
    ScreenPtr screen;
    glamor_screen_private *priv;
    PixmapPtr dst_pixmap;
    glamor_pixmap_private *dst_priv;
    uint32_t byte_stride;
    size_t transfer_size;
    RegionRec region;
    Bool have_region = FALSE;
    int screen_idx;

    (void) depth;

    if (UNLIKELY(!drawable || !gc || !bits || w <= 0 || h <= 0))
        return FALSE;

    if (!glamor_image_can_fast_upload(gc))
        return FALSE;

    screen = drawable->pScreen;
    priv = glamor_get_screen_private(screen);
    dst_pixmap = glamor_get_drawable_pixmap(drawable);

    if (UNLIKELY(!priv || !dst_pixmap))
        return FALSE;

    dst_priv = glamor_get_pixmap_private(dst_pixmap);
    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv)))
        return FALSE;

    byte_stride = PixmapBytePad(w, drawable->depth);
    transfer_size = glamor_image_safe_mul_size((size_t) h,
                                               (size_t) byte_stride);
    if (UNLIKELY(transfer_size == 0 || transfer_size > (size_t) INT_MAX))
        return FALSE;

    have_region = glamor_image_build_put_region(drawable, gc, dst_pixmap,
                                                x, y, w, h, &region);
    if (!have_region) {
        RegionUninit(&region);
        return TRUE;
    }

    glamor_make_current(priv);

    screen_idx = glamor_image_get_screen_index(screen);

    if (transfer_size >= GLAMOR_IMAGE_PBO_THRESHOLD && screen_idx >= 0) {
        GLuint pbo_id = 0;
        void *map = NULL;

        if (glamor_image_pbo_ring_acquire(screen_idx, transfer_size,
                                          &pbo_id, &map)) {
            (void) pbo_id;
            glamor_image_memcpy_streaming(map, bits, transfer_size);
            glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);

            glamor_upload_region(drawable, &region, x, y,
                                 (const uint8_t *) (uintptr_t) 0,
                                 byte_stride);

            glamor_image_pbo_ring_record_fence(screen_idx);
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

            RegionUninit(&region);
            return TRUE;
        }
    }

    glamor_upload_region(drawable, &region, x, y,
                         (const uint8_t *) bits, byte_stride);

    RegionUninit(&region);
    return TRUE;
}

static Bool
glamor_put_image_xy_gl(DrawablePtr drawable, GCPtr gc, int depth,
                       int x, int y, int w, int h, int leftPad,
                       int format, const char *bits)
{
    ScreenPtr screen;
    PixmapPtr dst_pixmap;
    glamor_pixmap_private *dst_priv;
    PixmapPtr tmp_pixmap;
    GCPtr tmp_gc;
    ChangeGCVal gcv[3];

    if (UNLIKELY(!drawable || !gc || !bits || w <= 0 || h <= 0))
        return FALSE;

    screen = drawable->pScreen;
    dst_pixmap = glamor_get_drawable_pixmap(drawable);
    if (UNLIKELY(!dst_pixmap))
        return FALSE;

    dst_priv = glamor_get_pixmap_private(dst_pixmap);
    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv)))
        return FALSE;

    tmp_pixmap = screen->CreatePixmap(screen, w, h, drawable->depth,
                                      GLAMOR_CREATE_PIXMAP_CPU);
    if (UNLIKELY(!tmp_pixmap))
        return FALSE;

    tmp_gc = GetScratchGC(tmp_pixmap->drawable.depth, screen);
    if (UNLIKELY(!tmp_gc)) {
        screen->DestroyPixmap(tmp_pixmap);
        return FALSE;
    }

    gcv[0].val = GXcopy;
    gcv[1].val = gc->fgPixel;
    gcv[2].val = gc->bgPixel;

    ChangeGC(NullClient, tmp_gc, GCFunction | GCForeground | GCBackground,
             gcv);
    ValidateGC(&tmp_pixmap->drawable, tmp_gc);

    tmp_gc->ops->PutImage(&tmp_pixmap->drawable, tmp_gc, depth,
                          0, 0, w, h, leftPad, format, (char *) bits);

    gc->ops->CopyArea(&tmp_pixmap->drawable, drawable, gc,
                      0, 0, w, h, x, y);

    FreeScratchGC(tmp_gc);
    screen->DestroyPixmap(tmp_pixmap);

    return TRUE;
}

static void COLD
glamor_put_image_bail(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                      int w, int h, int leftPad, int format, const char *bits)
{
    if (UNLIKELY(!drawable || !gc || !bits || w <= 0 || h <= 0))
        return;

    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RW, x, y, w, h))
        fbPutImage(drawable, gc, depth, x, y, w, h, leftPad, format,
                   (char *) bits);

    glamor_finish_access(drawable);
}

void
glamor_put_image(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                 int w, int h, int leftPad, int format, char *bits)
{
    if (UNLIKELY(!drawable || !gc || !bits || w <= 0 || h <= 0))
        return;

    switch (format) {
    case ZPixmap:
        if (glamor_put_image_zpixmap_gl(drawable, gc, depth,
                                        x, y, w, h, bits))
            return;
        break;
    case XYBitmap:
    case XYPixmap:
        if (glamor_put_image_xy_gl(drawable, gc, depth,
                                   x, y, w, h, leftPad, format, bits))
            return;
        break;
    default:
        break;
    }

    glamor_put_image_bail(drawable, gc, depth, x, y, w, h,
                          leftPad, format, bits);
}

#if GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS
static void
glamor_apply_plane_mask_32_avx2(char *dst, size_t byte_stride,
                                int w, int h, uint32_t plane_mask)
{
    const __m256i mask = _mm256_set1_epi32((int) plane_mask);
    size_t row_bytes = (size_t) w * 4u;

    for (int y = 0; y < h; y++) {
        char *row = dst + (size_t) y * byte_stride;
        size_t x = 0;

        for (; x + 32u <= row_bytes; x += 32u) {
            __m256i v = _mm256_loadu_si256((const __m256i *) (const void *) (row + x));
            v = _mm256_and_si256(v, mask);
            _mm256_storeu_si256((__m256i *) (void *) (row + x), v);
        }

        for (; x + 4u <= row_bytes; x += 4u) {
            uint32_t v;

            memcpy(&v, row + x, sizeof(v));
            v &= plane_mask;
            memcpy(row + x, &v, sizeof(v));
        }
    }
}
#endif

static void
glamor_apply_plane_mask_32_scalar(char *dst, size_t byte_stride,
                                  int w, int h, uint32_t plane_mask)
{
    size_t row_words = (size_t) w;

    for (int y = 0; y < h; y++) {
        char *row = dst + (size_t) y * byte_stride;

        for (size_t x = 0; x < row_words; x++) {
            uint32_t v;

            memcpy(&v, row + x * 4u, sizeof(v));
            v &= plane_mask;
            memcpy(row + x * 4u, &v, sizeof(v));
        }
    }
}

static void
glamor_apply_plane_mask_16(char *dst, size_t byte_stride,
                           int w, int h, uint16_t plane_mask)
{
    for (int y = 0; y < h; y++) {
        char *row = dst + (size_t) y * byte_stride;

        for (int x = 0; x < w; x++) {
            uint16_t v;

            memcpy(&v, row + (size_t) x * 2u, sizeof(v));
            v &= plane_mask;
            memcpy(row + (size_t) x * 2u, &v, sizeof(v));
        }
    }
}

static void
glamor_apply_plane_mask_8(char *dst, size_t byte_stride,
                          int w, int h, uint8_t plane_mask)
{
    for (int y = 0; y < h; y++) {
        uint8_t *row = (uint8_t *) dst + (size_t) y * byte_stride;

        for (int x = 0; x < w; x++)
            row[x] &= plane_mask;
    }
}

static Bool
glamor_can_apply_plane_mask_zpixmap(DrawablePtr drawable,
                                    unsigned long plane_mask)
{
    unsigned int bpp;

    if (glamor_pm_is_solid(glamor_drawable_effective_depth(drawable),
                           plane_mask))
        return TRUE;

    bpp = drawable->bitsPerPixel;
    return bpp == 1 || bpp == 8 || bpp == 16 || bpp == 32;
}

static Bool
glamor_apply_plane_mask_zpixmap(DrawablePtr drawable, int w, int h,
                                unsigned long plane_mask, char *dst,
                                uint32_t byte_stride)
{
    unsigned int bpp;
    uint32_t replicated;

    if (glamor_pm_is_solid(glamor_drawable_effective_depth(drawable),
                           plane_mask))
        return TRUE;

    bpp = drawable->bitsPerPixel;

    if (bpp == 1) {
        if ((plane_mask & 1ul) == 0)
            memset(dst, 0, (size_t) byte_stride * (size_t) h);
        return TRUE;
    }

    replicated = (uint32_t) fbReplicatePixel(plane_mask, bpp);

    switch (bpp) {
    case 8:
        glamor_apply_plane_mask_8(dst, byte_stride, w, h,
                                  (uint8_t) replicated);
        return TRUE;
    case 16:
        glamor_apply_plane_mask_16(dst, byte_stride, w, h,
                                   (uint16_t) replicated);
        return TRUE;
    case 32:
#if GLAMOR_IMAGE_HAVE_AVX2_INTRINSICS
        if (LIKELY((get_cpu_features() & CPU_FEATURE_AVX2) != 0)) {
            glamor_apply_plane_mask_32_avx2(dst, byte_stride, w, h,
                                            replicated);
            return TRUE;
        }
#endif
        glamor_apply_plane_mask_32_scalar(dst, byte_stride, w, h,
                                          replicated);
        return TRUE;
    default:
        return FALSE;
    }
}

static Bool
glamor_get_image_zpixmap_gl(DrawablePtr drawable, int x, int y, int w, int h,
                            unsigned int format, unsigned long plane_mask,
                            char *dst)
{
    PixmapPtr pixmap;
    glamor_pixmap_private *priv;
    uint32_t byte_stride;
    int off_x = 0, off_y = 0;
    BoxRec box;

    if (UNLIKELY(!drawable || !dst || format != ZPixmap || w <= 0 || h <= 0))
        return FALSE;

    pixmap = glamor_get_drawable_pixmap(drawable);
    if (UNLIKELY(!pixmap))
        return FALSE;

    priv = glamor_get_pixmap_private(pixmap);
    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(priv)))
        return FALSE;

    byte_stride = PixmapBytePad(w, drawable->depth);
    if (UNLIKELY(byte_stride == 0))
        return FALSE;

    if (!glamor_can_apply_plane_mask_zpixmap(drawable, plane_mask))
        return FALSE;

    glamor_get_drawable_deltas(drawable, pixmap, &off_x, &off_y);

    box.x1 = x + off_x;
    box.y1 = y + off_y;
    box.x2 = x + off_x + w;
    box.y2 = y + off_y + h;

    glamor_download_boxes(drawable, &box, 1,
                          x + off_x, y + off_y,
                          -x, -y, (uint8_t *) dst, byte_stride);

    if (!glamor_apply_plane_mask_zpixmap(drawable, w, h, plane_mask,
                                         dst, byte_stride))
        return FALSE;

    return TRUE;
}

static void COLD
glamor_get_image_bail(DrawablePtr drawable, int x, int y, int w, int h,
                      unsigned int format, unsigned long plane_mask, char *d)
{
    if (UNLIKELY(!drawable || !d || w <= 0 || h <= 0))
        return;

    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RO, x, y, w, h))
        fbGetImage(drawable, x, y, w, h, format, plane_mask, d);

    glamor_finish_access(drawable);
}

void
glamor_get_image(DrawablePtr drawable, int x, int y, int w, int h,
                 unsigned int format, unsigned long plane_mask, char *d)
{
    if (UNLIKELY(!drawable || !d || w <= 0 || h <= 0))
        return;

    if (glamor_get_image_zpixmap_gl(drawable, x, y, w, h,
                                    format, plane_mask, d))
        return;

    glamor_get_image_bail(drawable, x, y, w, h, format, plane_mask, d);
}
