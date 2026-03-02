/*
 * SPDX-License-Identifier: MIT
 *
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_transform.h"
#include "servermd.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Hardware Feature Detection */
#if defined(__AVX2__) && (defined(__GNUC__) || defined(__clang__))
#include <immintrin.h>
#define HAVE_AVX2_INTRINSICS 1
#else
#define HAVE_AVX2_INTRINSICS 0
#endif

/* Branch Prediction Hints */
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

/* GLAMOR_FBO_NORMAL means it has actual backing storage on the GPU */
#define GLAMOR_PIXMAP_PRIV_HAS_FBO(priv) \
    ((priv) != NULL && ((priv)->gl_fbo == GLAMOR_FBO_NORMAL))

/* Per-Screen PBO Ring Buffer Configuration */
#define PBO_RING_SIZE 4
#define PBO_MIN_SIZE (64u * 1024u)
#define PBO_MAX_SIZE (16u * 1024u * 1024u)
#define PBO_THRESHOLD (64u * 1024u)

/* Maximum supported screens (Xorg typical limit) */
#define MAX_SCREENS 16

typedef enum {
    CPU_FEATURE_NONE = 0,
    CPU_FEATURE_AVX2 = 1,
} cpu_feature_level;

static cpu_feature_level
get_cpu_features(void)
{
    static cpu_feature_level cached = CPU_FEATURE_NONE;
    static int initialized = 0;

    if (!initialized) {
#if HAVE_AVX2_INTRINSICS && (defined(__GNUC__) && __GNUC__ >= 9 || defined(__clang__))
        if (__builtin_cpu_supports("avx2")) {
            cached = CPU_FEATURE_AVX2;
        }
#endif
        initialized = 1;
    }
    return cached;
}

/* Per-screen PBO ring state (replaces global static) */
typedef struct {
    GLuint pbo[PBO_RING_SIZE];
    GLsync fence[PBO_RING_SIZE];
    int current_idx;
    size_t current_size;
    int fence_valid[PBO_RING_SIZE];
    int initialized;
} glamor_pbo_ring_per_screen;

static glamor_pbo_ring_per_screen g_pbo_rings[MAX_SCREENS];

/* Fast upload check (GXcopy with solid planemask) */
static inline bool
glamor_can_fast_upload(const GCPtr gc)
{
    return gc && (gc->alu == GXcopy) && glamor_pm_is_solid(gc->depth, gc->planemask);
}

/* Safe multiplication with overflow check */
static inline size_t
safe_mul_size(size_t a, size_t b)
{
    if (a == 0 || b == 0)
        return 0;
    if (UNLIKELY(a > SIZE_MAX / b))
        return 0;

    size_t result = a * b;

    /* Sanity bound against extreme X11 client requests */
    if (UNLIKELY(result > (size_t)(INT_MAX)))
        return 0;

    return result;
}

/* AVX2 Non-Temporal Streaming Memcpy */
/* Bypasses CPU caches for large transfers to GPU-mapped memory */
static inline void
memcpy_streaming(void * restrict dst, const void * restrict src, size_t n)
{
    if (UNLIKELY(n == 0))
        return;

    /* Software prefetching for very large transfers */
    if (n >= (size_t)(512 * 1024)) {
        const char *s = (const char *)src;
        for (size_t i = 0; i < n; i += 65536) {
            __builtin_prefetch(s + i, 0, 0);
        }
    }

    cpu_feature_level features = get_cpu_features();

    if (features >= CPU_FEATURE_AVX2 && n >= (size_t)(256 * 1024)) {
        /* Destination (PBO Map) is page-aligned by GL driver */
        if (((uintptr_t)dst & 31u) == 0u) {
            const char *src_ptr = (const char *)src;
            char *dst_ptr = (char *)dst;

            size_t vec_count = n / 32;
            size_t remainder = n % 32;

            for (size_t i = 0; i < vec_count; i++) {
                /* Unaligned load (safe for random X11 heap pointers) */
                __m256i data = _mm256_loadu_si256((const __m256i *)(src_ptr + (i * 32)));

                /* Non-temporal store (bypasses L1/L2/L3, writes directly to WC memory) */
                _mm256_stream_si256((__m256i *)(dst_ptr + (i * 32)), data);
            }

            /* Memory fence: ensures WC buffers are flushed before GPU access */
            _mm_sfence();

            if (remainder > 0) {
                memcpy(dst_ptr + (vec_count * 32), src_ptr + (vec_count * 32), remainder);
            }
            return;
        }
    }

    memcpy(dst, src, n);
}

/* Initialize per-screen PBO ring */
static void
pbo_ring_init(int screen_index)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return;

    glamor_pbo_ring_per_screen *ring = &g_pbo_rings[screen_index];
    memset(ring, 0, sizeof(*ring));
    ring->current_size = PBO_MIN_SIZE;
    ring->initialized = 1;
}

/* Cleanup per-screen PBO ring */
static void
pbo_ring_cleanup(int screen_index)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return;

    glamor_pbo_ring_per_screen *ring = &g_pbo_rings[screen_index];

    if (ring->pbo[0]) {
        glDeleteBuffers(PBO_RING_SIZE, ring->pbo);
    }

    for (int i = 0; i < PBO_RING_SIZE; i++) {
        if (ring->fence_valid[i] && ring->fence[i]) {
            glDeleteSync(ring->fence[i]);
        }
    }

    memset(ring, 0, sizeof(*ring));
}

/* Get screen index for PBO ring */
static int
get_screen_index(ScreenPtr screen)
{
    if (!screen)
        return -1;

    /* Use screen index as array index (bounded) */
    int idx = screen->myNum;
    if (idx < 0 || idx >= MAX_SCREENS)
        return -1;

    if (!g_pbo_rings[idx].initialized)
        pbo_ring_init(idx);

    return idx;
}

/* Per-screen PBO acquisition with fence tracking */
static bool
pbo_acquire_upload_per_screen(int screen_index, size_t required,
                               GLuint *out_pbo, void **out_map)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return false;

    glamor_pbo_ring_per_screen *ring = &g_pbo_rings[screen_index];

    /* Grow buffer if needed (with upper bound) */
    if (required > ring->current_size && required <= PBO_MAX_SIZE) {
        if (ring->pbo[0]) {
            glDeleteBuffers(PBO_RING_SIZE, ring->pbo);
            for (int i = 0; i < PBO_RING_SIZE; i++) {
                if (ring->fence_valid[i] && ring->fence[i]) {
                    glDeleteSync(ring->fence[i]);
                    ring->fence[i] = NULL;
                    ring->fence_valid[i] = 0;
                }
            }
        }
        ring->current_size = required;
        glGenBuffers(PBO_RING_SIZE, ring->pbo);
    }

    int next_idx = (ring->current_idx + 1) % PBO_RING_SIZE;
    GLuint pbo = ring->pbo[next_idx];

    if (UNLIKELY(pbo == 0)) {
        glGenBuffers(1, &pbo);
        ring->pbo[next_idx] = pbo;
    }

    /* Wait for GPU if this slot is still in use (with timeout) */
    if (ring->fence_valid[next_idx] && ring->fence[next_idx]) {
        GLenum wait = glClientWaitSync(ring->fence[next_idx], 0, 1000000); /* 1ms timeout */
        if (wait == GL_TIMEOUT_EXPIRED) {
            /* GPU still busy, orphan this slot */
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);
            glBufferData(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr)ring->current_size,
                        NULL, GL_STREAM_DRAW);
        }
        glDeleteSync(ring->fence[next_idx]);
        ring->fence[next_idx] = NULL;
        ring->fence_valid[next_idx] = 0;
    }

    ring->current_idx = next_idx;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);

    /* Orphaning: discard old backing store, allocate fresh memory */
    glBufferData(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr)required, NULL, GL_STREAM_DRAW);

    /* Map unsynchronized (we just orphaned, so no GPU access) */
    *out_map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required,
                                GL_MAP_WRITE_BIT | GL_MAP_UNSYNCHRONIZED_BIT);

    if (UNLIKELY(*out_map == NULL)) {
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        return false;
    }

    *out_pbo = pbo;
    return true;
}

/* Record fence after PBO upload */
static void
pbo_record_fence(int screen_index)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return;

    glamor_pbo_ring_per_screen *ring = &g_pbo_rings[screen_index];
    int idx = ring->current_idx;

    if (ring->fence_valid[idx] && ring->fence[idx]) {
        glDeleteSync(ring->fence[idx]);
    }

    ring->fence[idx] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
    ring->fence_valid[idx] = 1;
}

/* ZPixmap PutImage (Hot Path) */
static Bool
glamor_put_image_zpixmap_gl(DrawablePtr drawable, GCPtr gc, int depth,
                            int x, int y, int w, int h, const char *bits)
{
    (void)depth;

    if (UNLIKELY(!drawable || !bits || w <= 0 || h <= 0))
        return FALSE;

    ScreenPtr screen = drawable->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(drawable);

    if (UNLIKELY(!priv || !dst_pixmap))
        return FALSE;

    glamor_pixmap_private *dst_priv = glamor_get_pixmap_private(dst_pixmap);

    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv) || !glamor_can_fast_upload(gc)))
        return FALSE;

    uint32_t byte_stride = PixmapBytePad(w, drawable->depth);
    size_t transfer_size = safe_mul_size((size_t)h, (size_t)byte_stride);

    if (UNLIKELY(transfer_size == 0))
        return FALSE;

    /* Convert client coordinates to pixmap coordinates */
    int off_x = 0, off_y = 0;
    glamor_get_drawable_deltas(drawable, dst_pixmap, &off_x, &off_y);

    /* Build box in pixmap coordinates */
    BoxRec box = {
        .x1 = (int16_t)(x + off_x),
        .y1 = (int16_t)(y + off_y),
        .x2 = (int16_t)(x + off_x + w),
        .y2 = (int16_t)(y + off_y + h)
    };

    /* Boundary safety check */
    if (box.x1 < 0 || box.y1 < 0 ||
        box.x2 > dst_pixmap->drawable.width ||
        box.y2 > dst_pixmap->drawable.height) {
        return FALSE;
    }

    RegionRec region;
    RegionInit(&region, &box, 1);

    if (gc && gc->pCompositeClip) {
        RegionRec clip_region;
        RegionNull(&clip_region);
        RegionCopy(&clip_region, gc->pCompositeClip);
        RegionTranslate(&clip_region, off_x, off_y);
        RegionIntersect(&region, &region, &clip_region);
        RegionUninit(&clip_region);
    }

    if (!RegionNotEmpty(&region)) {
        RegionUninit(&region);
        return TRUE;
    }

    glamor_make_current(priv);

    int screen_index = get_screen_index(screen);
    GLuint pbo_id = 0;
    void *map = NULL;

    /* Use PBO for payloads > 64KB */
    if (transfer_size >= PBO_THRESHOLD && screen_index >= 0 &&
        pbo_acquire_upload_per_screen(screen_index, transfer_size, &pbo_id, &map)) {

        memcpy_streaming(map, bits, transfer_size);
        glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);

        /* Upload from PBO (NULL pointer means use bound PBO) */
        glamor_upload_region(drawable, &region, x, y, (const uint8_t *)(uintptr_t)0, byte_stride);

        /* Record fence for GPU-CPU synchronization */
        pbo_record_fence(screen_index);

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    } else {
        /* Direct upload for small transfers */
        glamor_upload_region(drawable, &region, x, y, (const uint8_t *)bits, byte_stride);
    }

    RegionUninit(&region);
    return TRUE;
}

/* XYBitmap/XYPixmap CPU Fallback */
static Bool
glamor_put_image_xy_gl(DrawablePtr drawable, GCPtr gc, int depth,
                       int x, int y, int w, int h, int leftPad,
                       int format, const char *bits)
{
    if (UNLIKELY(!drawable || !bits || w <= 0 || h <= 0))
        return FALSE;

    ScreenPtr screen = drawable->pScreen;
    PixmapPtr dst_pix = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private *dst_priv = glamor_get_pixmap_private(dst_pix);

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv))
        return FALSE;

    PixmapPtr tmp_pix = screen->CreatePixmap(screen, w, h, drawable->depth, GLAMOR_CREATE_PIXMAP_CPU);
    if (!tmp_pix)
        return FALSE;

    GCPtr tmp_gc = GetScratchGC(tmp_pix->drawable.depth, screen);
    if (!tmp_gc) {
        screen->DestroyPixmap(tmp_pix);
        return FALSE;
    }

    ChangeGCVal gcv[3] = {
        { .val = GXcopy },
        { .val = gc ? gc->fgPixel : 0 },
        { .val = gc ? gc->bgPixel : 0 }
    };
    ChangeGC(NullClient, tmp_gc, GCFunction | GCForeground | GCBackground, gcv);
    ValidateGC(&tmp_pix->drawable, tmp_gc);

    tmp_gc->ops->PutImage(&tmp_pix->drawable, tmp_gc, depth, 0, 0, w, h, leftPad, format, (char *)bits);

    if (gc) {
        gc->ops->CopyArea(&tmp_pix->drawable, drawable, gc, 0, 0, w, h, x, y);
    }

    FreeScratchGC(tmp_gc);
    screen->DestroyPixmap(tmp_pix);
    return TRUE;
}

/* PutImage Bail (CPU Fallback) */
static void COLD
glamor_put_image_bail(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                      int w, int h, int leftPad, int format, const char *bits)
{
    if (w <= 0 || h <= 0)
        return;

    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RW, x, y, w, h)) {
        fbPutImage(drawable, gc, depth, x, y, w, h, leftPad, format, (char *)bits);
    }
    glamor_finish_access(drawable);
}

/* PutImage Dispatcher */
void
glamor_put_image(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                 int w, int h, int leftPad, int format, char *bits)
{
    if (UNLIKELY(!drawable || !gc || !bits || w <= 0 || h <= 0))
        return;

    switch (format) {
    case ZPixmap:
        if (glamor_put_image_zpixmap_gl(drawable, gc, depth, x, y, w, h, bits))
            return;
        break;

    case XYBitmap:
    case XYPixmap:
        if (glamor_put_image_xy_gl(drawable, gc, depth, x, y, w, h, leftPad, format, bits))
            return;
        break;
    }

    glamor_put_image_bail(drawable, gc, depth, x, y, w, h, leftPad, format, bits);
}

/* GetImage ZPixmap (with plane mask handling) */
static Bool
glamor_get_image_zpixmap_gl(DrawablePtr drawable, int x, int y, int w, int h,
                            unsigned int format, unsigned long plane_mask, char *dst)
{
    if (UNLIKELY(!drawable || !dst || format != ZPixmap || w <= 0 || h <= 0))
        return FALSE;

    PixmapPtr pixmap = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private *priv = glamor_get_pixmap_private(pixmap);

    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(priv)))
        return FALSE;

    uint32_t byte_stride = PixmapBytePad(w, drawable->depth);
    int off_x = 0, off_y = 0;
    glamor_get_drawable_deltas(drawable, pixmap, &off_x, &off_y);

    BoxRec box = {
        .x1 = (int16_t)(x + off_x),
        .y1 = (int16_t)(y + off_y),
        .x2 = (int16_t)(x + off_x + w),
        .y2 = (int16_t)(y + off_y + h)
    };

    glamor_download_boxes(drawable, &box, 1,
                          x + off_x, y + off_y,
                          -x, -y, (uint8_t *)dst, byte_stride);

    /* Apply plane mask if not solid */
    if (!glamor_pm_is_solid(glamor_drawable_effective_depth(drawable), plane_mask)) {
        uint32_t pm32 = (uint32_t)fbReplicatePixel(plane_mask, drawable->bitsPerPixel);
        uint32_t *dst32 = (uint32_t *)dst;
        size_t total_words = ((size_t)byte_stride / 4u) * (size_t)h;

        for (size_t i = 0; i < total_words; i++) {
            dst32[i] &= pm32;
        }
    }

    return TRUE;
}

/* GetImage Bail */
static void COLD
glamor_get_image_bail(DrawablePtr drawable, int x, int y, int w, int h,
                      unsigned int format, unsigned long plane_mask, char *d)
{
    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RO, x, y, w, h)) {
        fbGetImage(drawable, x, y, w, h, format, plane_mask, d);
    }
    glamor_finish_access(drawable);
}

/* GetImage Dispatcher */
void
glamor_get_image(DrawablePtr drawable, int x, int y, int w, int h,
                 unsigned int format, unsigned long plane_mask, char *d)
{
    if (UNLIKELY(!drawable || !d || w <= 0 || h <= 0))
        return;

    if (glamor_get_image_zpixmap_gl(drawable, x, y, w, h, format, plane_mask, d))
        return;

    glamor_get_image_bail(drawable, x, y, w, h, format, plane_mask, d);
}
