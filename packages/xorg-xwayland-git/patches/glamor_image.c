/* SPDX-License-Identifier: MIT
 *
 * glamor_image.c – PutImage / GetImage fast paths
 *
 * High-performance, production-ready implementation tuned for:
 * - AMD Vega 64 (GFX9, HBM2) – prefers persistent non-coherent PBO + explicit flush
 * - Intel Raptor Lake – prefers persistent coherent PBO mapping
 *
 * Key improvements:
 * - Persistent-mapped PBO ring (upload/download) with GLsync fences to avoid stalls/overwrites.
 * - Vendor-aware mapping (coherent on Intel, non-coherent + flush on AMD).
 * - Adaptive thresholds to bypass PBO for small transfers; aligned PBO sizes.
 * - XYBitmap texture reuse and uniform location caching to cut driver CPU overhead.
 * - Strict GL state hygiene and null-safe clipping (fixes reported segfault).
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_transform.h"
#include "servermd.h"

#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <strings.h> /* strncasecmp */
#include <stdlib.h>

/* ---------------------------------------------------------------------------
 * Helpers and small utilities
 * -------------------------------------------------------------------------*/

#if defined(__GNUC__)
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

/* True when the pixmap’s private indicates a texture/renderable FBO. */
#define GLAMOR_PIXMAP_PRIV_HAS_FBO(priv) \
    ((priv) && ((priv)->gl_fbo == GLAMOR_FBO_NORMAL))

/* Fast GC check: alu must be copy and planemask must be solid. */
static inline Bool
glamor_can_fast_upload(const GCPtr gc)
{
    return gc && (gc->alu == GXcopy) &&
           glamor_pm_is_solid(gc->depth, gc->planemask);
}

/* Safe multiply and bound check; returns 0 on overflow/zero. */
static inline size_t
safe_mul_size(size_t a, size_t b)
{
    if (a == 0 || b == 0)
        return 0;
    if (a > SIZE_MAX / b)
        return 0;
    return a * b;
}

/* Round up to next multiple of 'align' (power of two preferred). */
static inline size_t
round_up(size_t n, size_t align)
{
    return (n + (align - 1)) / align * align;
}

/* For large copies, prefetch and call memcpy. Keeps code simple and portable. */
static inline void
memcpy_streaming(void *dst, const void *src, size_t n)
{
    if (unlikely(n == 0))
        return;

    /* For very large copies, modest prefetching can help on some CPUs. */
    if (n >= (size_t)(512 * 1024)) {
        const char *s = (const char *)src;
        for (size_t i = 0; i < n; i += 4096)
            __builtin_prefetch(s + i, 0, 0);
    }
    memcpy(dst, src, n);
}

/* ---------------------------------------------------------------------------
 * PBO pool (small ring) – optimized for streaming uploads/downloads
 * -------------------------------------------------------------------------*/

typedef struct glamor_pbo_slot {
    GLuint  id;
    void   *map;      /* Non-NULL when persistently mapped */
    size_t  size;     /* Allocated buffer storage size */
    Bool    persistent;
    Bool    coherent;
    GLsync  fence;    /* GPU fence from last use */
} glamor_pbo_slot;

typedef struct glamor_pbo_pool {
    Bool    inited;
    Bool    have_storage;     /* ARB_buffer_storage */
    Bool    want_persistent;  /* Enabled via ext + not disabled by env */
    Bool    prefer_coherent;  /* Vendor heuristic (Intel: TRUE, AMD: FALSE) */

    unsigned upload_index;
    unsigned download_index;

    glamor_pbo_slot upload[4];   /* Small ring */
    glamor_pbo_slot download[2];
} glamor_pbo_pool;

static glamor_pbo_pool g_pbo_pool;

static Bool
str_contains_nocase(const char *hay, const char *needle)
{
    if (!hay || !needle || !*needle)
        return FALSE;

    const size_t nlen = strlen(needle);
    for (const char *p = hay; *p; p++) {
        if (strncasecmp(p, needle, nlen) == 0)
            return TRUE;
    }
    return FALSE;
}

static Bool
gl_has_extension(const char *ext)
{
    GLint major = 0, minor = 0, n = 0;

    if (!ext || !*ext)
        return FALSE;

    glGetIntegerv(GL_MAJOR_VERSION, &major);
    glGetIntegerv(GL_MINOR_VERSION, &minor);

    if (major >= 3) {
        glGetIntegerv(GL_NUM_EXTENSIONS, &n);
        for (GLint i = 0; i < n; i++) {
            const char *e = (const char *)glGetStringi(GL_EXTENSIONS, (GLuint)i);
            if (e && strcmp(e, ext) == 0)
                return TRUE;
        }
        return FALSE;
    }

    const char *exts = (const char *)glGetString(GL_EXTENSIONS);
    if (!exts)
        return FALSE;

    const char *p = exts;
    size_t elen = strlen(ext);
    while ((p = strstr(p, ext)) != NULL) {
        if ((p == exts || p[-1] == ' ') &&
            (p[elen] == 0 || p[elen] == ' '))
            return TRUE;
        p += elen;
    }
    return FALSE;
}

/* Initialize GL capability info and vendor heuristics once. */
static void
glamor_pbo_pool_init(void)
{
    if (g_pbo_pool.inited)
        return;

    const char *vendor   = (const char *)glGetString(GL_VENDOR);
    const char *renderer = (const char *)glGetString(GL_RENDERER);

    g_pbo_pool.have_storage = gl_has_extension("GL_ARB_buffer_storage");
    g_pbo_pool.want_persistent = g_pbo_pool.have_storage;

    /* Honor environment override. */
    const char *env = getenv("GLAMOR_NO_PERSISTENT_PBO");
    if (env && atoi(env) != 0)
        g_pbo_pool.want_persistent = FALSE;

    /* Vendor heuristic: Intel prefers coherent; AMD prefers non-coherent (explicit flush). */
    g_pbo_pool.prefer_coherent =
        (str_contains_nocase(vendor, "intel") ||
         str_contains_nocase(renderer, "intel"));

    if (str_contains_nocase(vendor, "amd") || str_contains_nocase(renderer, "radeon"))
        g_pbo_pool.prefer_coherent = FALSE;

    g_pbo_pool.upload_index = 0;
    g_pbo_pool.download_index = 0;
    for (unsigned i = 0; i < 4; i++) {
        g_pbo_pool.upload[i].id = 0;
        g_pbo_pool.upload[i].map = NULL;
        g_pbo_pool.upload[i].size = 0;
        g_pbo_pool.upload[i].persistent = FALSE;
        g_pbo_pool.upload[i].coherent = FALSE;
        g_pbo_pool.upload[i].fence = 0;
    }
    for (unsigned i = 0; i < 2; i++) {
        g_pbo_pool.download[i].id = 0;
        g_pbo_pool.download[i].map = NULL;
        g_pbo_pool.download[i].size = 0;
        g_pbo_pool.download[i].persistent = FALSE;
        g_pbo_pool.download[i].coherent = FALSE;
        g_pbo_pool.download[i].fence = 0;
    }

    g_pbo_pool.inited = TRUE;
}

static inline void
glamor_pbo_clear_fence(glamor_pbo_slot *slot)
{
    if (slot->fence) {
        glDeleteSync(slot->fence);
        slot->fence = 0;
    }
}

/* Wait for a fence to signal (if present), then clear it. */
static inline void
glamor_pbo_wait(glamor_pbo_slot *slot)
{
    if (!slot->fence)
        return;

    GLenum r = glClientWaitSync(slot->fence, GL_SYNC_FLUSH_COMMANDS_BIT, 0);
    if (r == GL_TIMEOUT_EXPIRED || r == GL_WAIT_FAILED) {
        /* Hard wait if not yet ready */
        glClientWaitSync(slot->fence, GL_SYNC_FLUSH_COMMANDS_BIT, GL_TIMEOUT_IGNORED);
    }
    glDeleteSync(slot->fence);
    slot->fence = 0;
}

/* Acquire an upload slot large enough and not in use; waits only if all busy. */
static Bool
glamor_pbo_upload_acquire(size_t required, glamor_pbo_slot **out)
{
    glamor_pbo_slot *best_wait = NULL;

    /* Try each slot once, prefer not to wait. */
    for (unsigned tries = 0; tries < 4; tries++) {
        glamor_pbo_slot *slot = &g_pbo_pool.upload[g_pbo_pool.upload_index];
        g_pbo_pool.upload_index = (g_pbo_pool.upload_index + 1) & 3;

        /* If there is a fence, test without blocking. */
        if (slot->fence) {
            GLenum r = glClientWaitSync(slot->fence, 0, 0);
            if (r == GL_ALREADY_SIGNALED || r == GL_CONDITION_SATISFIED) {
                glamor_pbo_clear_fence(slot);
            } else {
                if (!best_wait)
                    best_wait = slot;
                continue;
            }
        }

        /* Persistent desired path */
        if (g_pbo_pool.want_persistent) {
            const size_t alloc = round_up(required, (size_t)256 * 1024);
            const Bool need_new = (slot->id == 0) || (slot->size < alloc) || !slot->persistent;

            if (need_new) {
                if (slot->id) {
                    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);
                    if (slot->map)
                        glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
                    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
                    glDeleteBuffers(1, &slot->id);
                    slot->id = 0;
                    slot->map = NULL;
                    slot->size = 0;
                    slot->persistent = FALSE;
                    slot->coherent = FALSE;
                }

                glGenBuffers(1, &slot->id);
                if (!slot->id)
                    return FALSE;

                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);

                const GLbitfield storage_flags =
                    GL_MAP_WRITE_BIT |
                    GL_MAP_PERSISTENT_BIT |
                    (g_pbo_pool.prefer_coherent ? GL_MAP_COHERENT_BIT : 0);

                glBufferStorage(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr)alloc, NULL, storage_flags);

                const GLbitfield map_flags =
                    GL_MAP_WRITE_BIT |
                    GL_MAP_PERSISTENT_BIT |
                    (g_pbo_pool.prefer_coherent ? GL_MAP_COHERENT_BIT : GL_MAP_FLUSH_EXPLICIT_BIT);

                slot->map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)alloc, map_flags);
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

                if (!slot->map) {
                    glDeleteBuffers(1, &slot->id);
                    slot->id = 0;
                    return FALSE;
                }

                slot->size = alloc;
                slot->persistent = TRUE;
                slot->coherent = g_pbo_pool.prefer_coherent;
            }

            *out = slot;
            return TRUE;
        }

        /* Non-persistent path: allocate or orphan via BufferData */
        if (slot->id == 0) {
            glGenBuffers(1, &slot->id);
            if (!slot->id)
                return FALSE;
            slot->size = 0;
            slot->persistent = FALSE;
            slot->coherent = FALSE;
        }

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);
        if (slot->size < required) {
            glBufferData(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr)required, NULL, GL_STREAM_DRAW);
            slot->size = required;
        }
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        *out = slot;
        return TRUE;
    }

    /* All slots busy: wait on the first candidate, then return it. */
    if (best_wait) {
        glamor_pbo_wait(best_wait);
        *out = best_wait;
        return TRUE;
    }

    return FALSE;
}

/* Acquire a download slot large enough and not in use; similar to upload. */
static Bool
glamor_pbo_download_acquire(size_t required, glamor_pbo_slot **out)
{
    glamor_pbo_slot *best_wait = NULL;

    for (unsigned tries = 0; tries < 2; tries++) {
        glamor_pbo_slot *slot = &g_pbo_pool.download[g_pbo_pool.download_index];
        g_pbo_pool.download_index = (g_pbo_pool.download_index + 1) & 1;

        if (slot->fence) {
            GLenum r = glClientWaitSync(slot->fence, 0, 0);
            if (r == GL_ALREADY_SIGNALED || r == GL_CONDITION_SATISFIED) {
                glamor_pbo_clear_fence(slot);
            } else {
                if (!best_wait)
                    best_wait = slot;
                continue;
            }
        }

        if (g_pbo_pool.want_persistent) {
            const size_t alloc = round_up(required, (size_t)256 * 1024);
            const Bool need_new = (slot->id == 0) || (slot->size < alloc) || !slot->persistent;
            if (need_new) {
                if (slot->id) {
                    glBindBuffer(GL_PIXEL_PACK_BUFFER, slot->id);
                    if (slot->map)
                        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
                    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
                    glDeleteBuffers(1, &slot->id);
                    slot->id = 0;
                    slot->map = NULL;
                    slot->size = 0;
                    slot->persistent = FALSE;
                    slot->coherent = FALSE;
                }

                glGenBuffers(1, &slot->id);
                if (!slot->id)
                    return FALSE;

                glBindBuffer(GL_PIXEL_PACK_BUFFER, slot->id);

                const GLbitfield storage_flags =
                    GL_MAP_READ_BIT |
                    GL_MAP_PERSISTENT_BIT;

                glBufferStorage(GL_PIXEL_PACK_BUFFER, (GLsizeiptr)alloc, NULL, storage_flags);

                const GLbitfield map_flags =
                    GL_MAP_READ_BIT |
                    GL_MAP_PERSISTENT_BIT;

                slot->map = glMapBufferRange(GL_PIXEL_PACK_BUFFER, 0, (GLsizeiptr)alloc, map_flags);
                glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

                if (!slot->map) {
                    glDeleteBuffers(1, &slot->id);
                    slot->id = 0;
                    return FALSE;
                }

                slot->size = alloc;
                slot->persistent = TRUE;
                slot->coherent = TRUE;
            }

            *out = slot;
            return TRUE;
        }

        if (slot->id == 0) {
            glGenBuffers(1, &slot->id);
            if (!slot->id)
                return FALSE;
            slot->size = 0;
            slot->persistent = FALSE;
            slot->coherent = FALSE;
        }

        glBindBuffer(GL_PIXEL_PACK_BUFFER, slot->id);
        if (slot->size < required) {
            glBufferData(GL_PIXEL_PACK_BUFFER, (GLsizeiptr)required, NULL, GL_STREAM_READ);
            slot->size = required;
        }
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

        *out = slot;
        return TRUE;
    }

    if (best_wait) {
        glamor_pbo_wait(best_wait);
        *out = best_wait;
        return TRUE;
    }

    return FALSE;
}

/* ---------------------------------------------------------------------------
 * ZPixmap upload – fast path (PBO optimized)
 * -------------------------------------------------------------------------*/

static Bool
glamor_put_image_zpixmap_gl(DrawablePtr drawable, GCPtr gc, int depth,
                            int x, int y, int w, int h,
                            const char *bits /* never NULL */)
{
    ScreenPtr               screen       = drawable->pScreen;
    glamor_screen_private  *glamor_priv  = glamor_get_screen_private(screen);
    PixmapPtr               dst_pixmap   = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private  *dst_priv     = glamor_get_pixmap_private(dst_pixmap);
    const uint32_t          byte_stride  = PixmapBytePad(w, drawable->depth);

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv))
        return FALSE;
    if (!glamor_can_fast_upload(gc))
        return FALSE;
    if (w <= 0 || h <= 0 || w > glamor_priv->max_fbo_size || h > glamor_priv->max_fbo_size)
        return FALSE;

    /* Clip region in destination pixmap coordinates. */
    RegionRec region;
    BoxRec    box = { .x1 = x + drawable->x,
                      .y1 = y + drawable->y,
                      .x2 = x + drawable->x + w,
                      .y2 = y + drawable->y + h };
    RegionInit(&region, &box, 1);
    if (gc && gc->pCompositeClip) {
        RegionIntersect(&region, &region, gc->pCompositeClip);
    }

    int off_x = 0, off_y = 0;
    glamor_get_drawable_deltas(drawable, dst_pixmap, &off_x, &off_y);
    if (off_x || off_y) {
        RegionTranslate(&region, off_x, off_y);
        x += off_x;
        y += off_y;
    }

    glamor_make_current(glamor_priv);

    /* Compute required bytes and choose path. */
    const size_t required = safe_mul_size((size_t)h, (size_t)byte_stride);
    if (required == 0 || required > INT_MAX) {
        RegionUninit(&region);
        return FALSE;
    }

    /* For very small uploads, CPU pointer path tends to be fastest. */
    if (likely(required < (size_t)32768)) {
        glamor_upload_region(drawable, &region, x, y,
                             (const uint8_t *)bits, byte_stride);
        RegionUninit(&region);
        return TRUE;
    }

    glamor_pbo_pool_init();

    glamor_pbo_slot *slot = NULL;
    if (!glamor_pbo_upload_acquire(required, &slot)) {
        /* Fallback to CPU pointer path. */
        glamor_upload_region(drawable, &region, x, y,
                             (const uint8_t *)bits, byte_stride);
        RegionUninit(&region);
        return TRUE;
    }

    if (slot->persistent) {
        /* Persistent path: copy to mapped pointer, flush if needed. */
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);

        memcpy_streaming(slot->map, bits, required);
        if (!slot->coherent) {
            glFlushMappedBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required);
        }

        glamor_upload_region(drawable, &region, x, y,
                             (const uint8_t *)(uintptr_t)0, byte_stride);

        /* Fence to protect this slot's memory until GPU read completes. */
        glamor_pbo_clear_fence(slot);
        slot->fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    } else {
        /* Transient path: MapRange UNSYNCHRONIZED + INVALIDATE. */
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);

        GLbitfield map_flags = GL_MAP_WRITE_BIT |
                               GL_MAP_INVALIDATE_BUFFER_BIT |
                               GL_MAP_UNSYNCHRONIZED_BIT;

        void *map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required, map_flags);
        if (map) {
            memcpy_streaming(map, bits, required);
            glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);

            glamor_upload_region(drawable, &region, x, y,
                                 (const uint8_t *)(uintptr_t)0, byte_stride);

            glamor_pbo_clear_fence(slot);
            slot->fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
        } else {
            /* Fallback to CPU pointer path if mapping fails. */
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
            glamor_upload_region(drawable, &region, x, y,
                                 (const uint8_t *)bits, byte_stride);
            RegionUninit(&region);
            return TRUE;
        }

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    }

    RegionUninit(&region);
    return TRUE;
}

/* ---------------------------------------------------------------------------
 * XY / XYPixmap – CPU temp pixmap path (robust and often optimal)
 * -------------------------------------------------------------------------*/

static Bool
glamor_put_image_xy_gl(DrawablePtr drawable, GCPtr gc, int depth,
                       int x, int y, int w, int h,
                       int leftPad, int format, const char *bits)
{
    ScreenPtr              screen   = drawable->pScreen;
    PixmapPtr              dst_pix  = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private *dst_priv = glamor_get_pixmap_private(dst_pix);

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv))
        return FALSE;

    if (w <= 0 || h <= 0)
        return TRUE;

    /* CPU temporary pixmap to avoid recursion and to leverage fb’s XY->Z conversion. */
    PixmapPtr tmp_pix = screen->CreatePixmap(screen, w, h, drawable->depth,
                                             GLAMOR_CREATE_PIXMAP_CPU);
    if (!tmp_pix)
        return FALSE;

    DrawablePtr tmp_draw = &tmp_pix->drawable;
    GCPtr       tmp_gc   = GetScratchGC(tmp_draw->depth, screen);
    if (!tmp_gc) {
        screen->DestroyPixmap(tmp_pix);
        return FALSE;
    }

    ChangeGCVal gcv[3] = {
        { .val = GXcopy },
        { .val = gc ? gc->fgPixel : 0 },
        { .val = gc ? gc->bgPixel : 0 }
    };
    ChangeGC(NullClient, tmp_gc,
             GCFunction | GCForeground | GCBackground, gcv);
    ValidateGC(tmp_draw, tmp_gc);

    tmp_gc->ops->PutImage(tmp_draw, tmp_gc,
                          depth, 0, 0, w, h,
                          leftPad, format, (char *)bits);

    gc->ops->CopyArea(tmp_draw, drawable, gc,
                      0, 0, w, h, x, y);

    FreeScratchGC(tmp_gc);
    screen->DestroyPixmap(tmp_pix);
    return TRUE;
}

/* ---------------------------------------------------------------------------
 * XYBitmap – large mono bitmap shader path
 * -------------------------------------------------------------------------*/

static const char vs_vars_put_bitmap[] =
"in  vec4 primitive;\n"
"in  vec2 source;\n"
"out vec2 img_pos;\n";

static const char vs_exec_put_bitmap[] =
"vec2 p = primitive.zw * vec2(gl_VertexID & 1, (gl_VertexID & 2) >> 1);\n"
GLAMOR_POS(gl_Position, (primitive.xy + p))
"img_pos = source + p;\n";

static const char fs_vars_put_bitmap[] =
"in  vec2 img_pos;\n"
"uniform usampler2D font;\n"
"uniform vec4 fg;\n"
"uniform vec4 bg;\n"
"uniform int bitorder;\n";

static Bool
put_bitmap_use(DrawablePtr draw, GCPtr gc,
               glamor_program *prog, void *unused)
{
    if (!glamor_set_solid(draw, gc, TRUE, prog->fg_uniform))
        return FALSE;
    glamor_set_color(draw, gc->bgPixel, prog->bg_uniform);
    return TRUE;
}

static const char fs_exec_put_bitmap[] =
"ivec2 t = ivec2(img_pos);\n"
"uint x = uint(t.x & 7u);\n"
"if (bitorder == 1) x = 7u - x;\n"
"t.x >>= 3;\n"
"uint tex = texelFetch(font, t, 0).x;\n"
"frag_color = ((tex >> x) & 1u) == 0u ? bg : fg;\n";

static const glamor_facet facet_put_bitmap = {
    .name      = "put_bitmap",
    .version   = 130,
    .vs_vars   = vs_vars_put_bitmap,
    .vs_exec   = vs_exec_put_bitmap,
    .fs_vars   = fs_vars_put_bitmap,
    .fs_exec   = fs_exec_put_bitmap,
    .locations = glamor_program_location_fg |
                 glamor_program_location_bg |
                 glamor_program_location_font,
    .use       = put_bitmap_use,
};

static Bool
glamor_put_image_xybitmap_gl(DrawablePtr drawable, GCPtr gc,
                             int x, int y, int w, int h,
                             int leftPad, const char *bits)
{
    ScreenPtr               screen      = drawable->pScreen;
    glamor_screen_private  *glamor_priv = glamor_get_screen_private(screen);
    PixmapPtr               dst_pixmap  = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private  *dst_priv    = glamor_get_pixmap_private(dst_pixmap);
    glamor_program         *prog        = &glamor_priv->put_bitmap_prog;
    uint32_t                stride      = PixmapBytePad(w + leftPad, 1);
    static GLuint           s_bitmap_tex = 0;
    static GLsizei          s_tex_w = 0, s_tex_h = 0;
    static GLuint           s_last_prog = 0;
    static GLint            s_bitorder_loc = -1;
    Bool                    ok          = FALSE;

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv))
        return FALSE;
    if (!glamor_can_fast_upload(gc))
        return FALSE;

    if (w <= 0 || h <= 0 || leftPad < 0 || leftPad > 32767)
        return FALSE;

    glamor_make_current(glamor_priv);

    /* Compile program on first use */
    if (!prog->prog && !prog->failed) {
        if (!glamor_build_program(screen, prog,
                                  &facet_put_bitmap,
                                  NULL, NULL, NULL))
            return FALSE;
    }
    if (prog->failed || !prog->prog)
        return FALSE;

    if (!glamor_use_program(&dst_pixmap->drawable, gc, prog, NULL))
        return FALSE;

    /* Cache bitorder uniform location per program */
    if (s_last_prog != prog->prog) {
        s_bitorder_loc = glGetUniformLocation(prog->prog, "bitorder");
        s_last_prog = prog->prog;
    }
    if (s_bitorder_loc != -1) {
        const int bitorder = (BITMAP_BIT_ORDER == MSBFirst) ? 1 : 0;
        glUniform1i(s_bitorder_loc, bitorder);
    }

    /* Create or reuse cached texture for bitmap upload. */
    if (!s_bitmap_tex) {
        glGenTextures(1, &s_bitmap_tex);
        s_tex_w = 0;
        s_tex_h = 0;
    }
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, s_bitmap_tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    /* (Re)allocate texture storage if size changed (round up to reduce reallocs) */
    GLsizei alloc_w = (GLsizei)round_up((size_t)stride, 256);
    GLsizei alloc_h = (GLsizei)round_up((size_t)h, 64);
    if (alloc_w != s_tex_w || alloc_h != s_tex_h) {
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R8UI,
                     alloc_w, alloc_h, 0,
                     GL_RED_INTEGER, GL_UNSIGNED_BYTE, NULL);
        s_tex_w = alloc_w;
        s_tex_h = alloc_h;
    }

    /* Upload the actual bitmap into the top-left sub-rectangle */
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, (GLsizei)stride, (GLsizei)h,
                    GL_RED_INTEGER, GL_UNSIGNED_BYTE, bits);

    glUniform1i(prog->font_uniform, 1);

    /* Build one instance vertex (x,y,w,h, leftPad, 0) ------------------- */
    char *vbo_offset = NULL;
    GLshort *vbo = glamor_get_vbo_space(screen,
                                        6 * sizeof(GLshort),
                                        &vbo_offset);
    if (!vbo) {
        /* Very unlikely; bail to CPU path */
        glBindTexture(GL_TEXTURE_2D, 0);
        glActiveTexture(GL_TEXTURE0);
        glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
        return FALSE;
    }
    vbo[0] = (GLshort)x; vbo[1] = (GLshort)y;
    vbo[2] = (GLshort)w; vbo[3] = (GLshort)h;
    vbo[4] = (GLshort)leftPad; vbo[5] = 0;
    glamor_put_vbo_space(screen);

    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 4, GL_SHORT, GL_FALSE,
                          6 * sizeof(GLshort), vbo_offset);
    glVertexAttribDivisor(GLAMOR_VERTEX_POS, 1);

    glEnableVertexAttribArray(GLAMOR_VERTEX_SOURCE);
    glVertexAttribPointer(GLAMOR_VERTEX_SOURCE, 2, GL_SHORT, GL_FALSE,
                          6 * sizeof(GLshort),
                          vbo_offset + 4 * sizeof(GLshort));
    glVertexAttribDivisor(GLAMOR_VERTEX_SOURCE, 1);

    /* Draw once for each clip rectangle --------------------------------- */
    glEnable(GL_SCISSOR_TEST);
    int off_x = 0, off_y = 0, box_index;
    glamor_pixmap_loop(dst_priv, box_index) {
        glamor_set_destination_drawable(drawable, box_index,
                                        TRUE, FALSE,
                                        prog->matrix_uniform,
                                        &off_x, &off_y);

        if (gc && gc->pCompositeClip) {
            int nbox = RegionNumRects(gc->pCompositeClip);
            const BoxPtr boxes = RegionRects(gc->pCompositeClip);

            for (int i = 0; i < nbox; i++) {
                const BoxRec *b = &boxes[i];
                glScissor(b->x1 + off_x, b->y1 + off_y,
                          b->x2 - b->x1, b->y2 - b->y1);
                glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, 1);
            }
        } else {
            /* No clip; draw once with full drawable scissor */
            glScissor(drawable->x + off_x, drawable->y + off_y,
                      drawable->width, drawable->height);
            glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, 1);
        }
    }
    glDisable(GL_SCISSOR_TEST);

    /* Clean vertex attrib state ---------------------------------------- */
    glVertexAttribDivisor(GLAMOR_VERTEX_SOURCE, 0);
    glDisableVertexAttribArray(GLAMOR_VERTEX_SOURCE);
    glVertexAttribDivisor(GLAMOR_VERTEX_POS, 0);
    glDisableVertexAttribArray(GLAMOR_VERTEX_POS);

    ok = TRUE;

    /* Restore all GL state we touched */
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE0);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);   /* X server default */
    return ok;
}

/* ---------------------------------------------------------------------------
 * Fallbacks to fb/CPU
 * -------------------------------------------------------------------------*/

static void
glamor_put_image_bail(DrawablePtr drawable, GCPtr gc, int depth,
                      int x, int y, int w, int h,
                      int leftPad, int format, const char *bits)
{
    if (w <= 0 || h <= 0)
        return;

    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RW,
                                  x, y, w, h)) {
        fbPutImage(drawable, gc, depth, x, y, w, h,
                   leftPad, format, (char *)bits);
    }
    glamor_finish_access(drawable);
}

/* ---------------------------------------------------------------------------
 * Public entry point – PutImage
 * -------------------------------------------------------------------------*/

void
glamor_put_image(DrawablePtr drawable, GCPtr gc, int depth,
                 int x, int y, int w, int h,
                 int leftPad, int format, char *bits)
{
    if (w <= 0 || h <= 0)
        return;

    switch (format) {
    case ZPixmap:
        if (glamor_put_image_zpixmap_gl(drawable, gc, depth,
                                        x, y, w, h, bits))
            return;
        break;

    case XYPixmap:
        if (glamor_put_image_xy_gl(drawable, gc, depth,
                                   x, y, w, h, leftPad, format, bits))
            return;
        break;

    case XYBitmap:
        /* Large bitmaps: shader path avoids huge CPU scratch pixmaps. */
        if ((size_t)w * (size_t)h >= (size_t)(100 * 100)) {
            if (glamor_put_image_xybitmap_gl(drawable, gc,
                                             x, y, w, h, leftPad, bits))
                return;
        }
        if (glamor_put_image_xy_gl(drawable, gc, depth,
                                   x, y, w, h, leftPad, format, bits))
            return;
        break;
    }

    /* If all accelerated paths fail, fall back to fb. */
    glamor_put_image_bail(drawable, gc, depth,
                          x, y, w, h, leftPad, format, bits);
}

/* ===========================================================================
 *                               GetImage paths
 * ======================================================================== */

/* ZPixmap download -------------------------------------------------------- */

static Bool
glamor_get_image_zpixmap_gl(DrawablePtr drawable,
                            int x, int y, int w, int h,
                            unsigned int img_format,
                            unsigned long plane_mask,
                            char *dst /* out-buffer, never NULL */)
{
    PixmapPtr               src_pixmap  = glamor_get_drawable_pixmap(drawable);
    glamor_pixmap_private  *src_priv    = glamor_get_pixmap_private(src_pixmap);
    ScreenPtr               screen      = drawable->pScreen;
    glamor_screen_private  *glamor_priv = glamor_get_screen_private(screen);

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(src_priv))
        return FALSE;
    if (img_format != ZPixmap)
        return FALSE;

    if (w <= 0 || h <= 0 || w > glamor_priv->max_fbo_size || h > glamor_priv->max_fbo_size)
        return FALSE;

    int off_x = 0, off_y = 0;
    glamor_get_drawable_deltas(drawable, src_pixmap, &off_x, &off_y);

    glamor_make_current(glamor_priv);

    BoxRec box = { .x1 = x + drawable->x + off_x,
                   .y1 = y + drawable->y + off_y,
                   .x2 = x + drawable->x + off_x + w,
                   .y2 = y + drawable->y + off_y + h };

    const struct glamor_format *format = glamor_format_for_pixmap(src_pixmap);
    if (unlikely(!format))
        return FALSE;

    /* Compute client stride and required size. */
    const uint32_t byte_stride = PixmapBytePad(w, drawable->depth);
    const size_t   required = safe_mul_size((size_t)h, (size_t)byte_stride);
    if (required == 0 || required > INT_MAX)
        return FALSE;

    /* Heuristic: for small downloads, prefer glamor_download_boxes; for larger, try PBO readback if supported. */
    Bool use_pbo = (required >= (size_t)65536); /* >= 64 KiB */

    if (use_pbo) {
        glamor_pbo_pool_init();
        glamor_pbo_slot *slot = NULL;
        if (glamor_pbo_download_acquire(required, &slot)) {
            glBindBuffer(GL_PIXEL_PACK_BUFFER, slot->id);

            /* Pack pixels into rows of 'byte_stride'. Convert bytes_per_row to pixels. */
            const int bpp = drawable->bitsPerPixel;
            const int bytes_per_pixel = (bpp >> 3) ? (bpp >> 3) : 1;
            const int pack_row_length = (int)(byte_stride / (uint32_t)bytes_per_pixel);

            glPixelStorei(GL_PACK_ALIGNMENT, 4);
            glPixelStorei(GL_PACK_ROW_LENGTH, pack_row_length);

            glamor_set_destination_pixmap_priv_nc(glamor_priv, src_pixmap, src_priv);
            glReadPixels(box.x1, box.y1, w, h, format->format, format->type, (void *)0);

            /* Fence to ensure GPU completed the transfer to the PBO */
            glamor_pbo_clear_fence(slot);
            slot->fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
            glamor_pbo_wait(slot);

            /* For persistent mapping, slot->map already points to buffer; else, map once. */
            const uint8_t *src;
            void *temp_map = NULL;
            if (slot->persistent) {
                glMemoryBarrier(GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT);
                src = (const uint8_t *)slot->map;
            } else {
                temp_map = glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
                if (!temp_map) {
                    /* Fallback if map fails, unbind and use CPU download. */
                    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
                    glPixelStorei(GL_PACK_ROW_LENGTH, 0);
                    glPixelStorei(GL_PACK_ALIGNMENT, 4);
                    use_pbo = FALSE;
                    goto cpu_download;
                }
                src = (const uint8_t *)temp_map;
            }

            /* Copy to destination buffer row by row (already in requested stride). */
            memcpy_streaming(dst, src, required);

            if (temp_map)
                glUnmapBuffer(GL_PIXEL_PACK_BUFFER);

            glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
            glPixelStorei(GL_PACK_ROW_LENGTH, 0);
            glPixelStorei(GL_PACK_ALIGNMENT, 4);

            goto mask_and_done;
        }
        /* If acquisition failed, continue to CPU path. */
    }

cpu_download:
    /* Robust CPU download path provided by glamor. */
    glamor_download_boxes(drawable, &box, 1,
                          drawable->x + off_x, drawable->y + off_y,
                          -x, -y,
                          (uint8_t *)dst, byte_stride);

mask_and_done:
    /* Apply plane-mask if not solid (all bits 1). */
    if (!glamor_pm_is_solid(glamor_drawable_effective_depth(drawable),
                            plane_mask))
    {
        const FbStip mask = fbReplicatePixel(plane_mask,
                                             drawable->bitsPerPixel);
        FbStip       *d   = (FbStip *)dst;
        const size_t  n   = ((size_t)byte_stride / sizeof(FbStip)) * (size_t)h;
        for (size_t i = 0; i < n; i++)
            d[i] &= mask;
    }

    return TRUE;
}

/* fb fallback */
static void
glamor_get_image_bail(DrawablePtr drawable,
                      int x, int y, int w, int h,
                      unsigned int format,
                      unsigned long plane_mask,
                      char *dst)
{
    if (w <= 0 || h <= 0)
        return;

    if (glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RO,
                                  x, y, w, h))
    {
        fbGetImage(drawable, x, y, w, h,
                   format, plane_mask, dst);
    }
    glamor_finish_access(drawable);
}

/* Public entry */
void
glamor_get_image(DrawablePtr drawable,
                 int x, int y, int w, int h,
                 unsigned int format,
                 unsigned long plane_mask,
                 char *dst)
{
    if (w <= 0 || h <= 0)
        return;

    /* Only ZPixmap has a fast path; XY formats require complex conversions. */
    if (glamor_get_image_zpixmap_gl(drawable, x, y, w, h,
                                    format, plane_mask, dst))
        return;

    glamor_get_image_bail(drawable, x, y, w, h,
                          format, plane_mask, dst);
}
