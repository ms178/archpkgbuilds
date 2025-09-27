/* SPDX-License-Identifier: MIT
 *
 * glamor_image.c – PutImage / GetImage fast paths
 *
 * Copyright © 2012–2024  The X.Org Foundation
 *
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_transform.h"
#include "servermd.h"

#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* ---------------------------------------------------------------------------
 * Helpers
 * -------------------------------------------------------------------------*/

/* True when the pixmap’s private indicates a texture/renderable FBO. */
#define GLAMOR_PIXMAP_PRIV_HAS_FBO(priv) \
    ((priv) && ((priv)->gl_fbo == GLAMOR_FBO_NORMAL))

/* Fast GC check: alu must be copy and planemask must be solid. */
static inline Bool
glamor_can_fast_upload(const GCPtr gc)
{
    return (gc->alu == GXcopy) &&
           glamor_pm_is_solid(gc->depth, gc->planemask);
}

/* Safe multiply and bound check; returns 0 on overflow/zero. */
static size_t
safe_mul_size(size_t a, size_t b)
{
    if (a == 0 || b == 0)
        return 0;
    if (a > SIZE_MAX / b)
        return 0;
    return a * b;
}

/* Case-insensitive substring test. */
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

/* GL extension detection with runtime robustness. */
static Bool
gl_has_extension(const char *ext)
{
    GLint major = 0, minor = 0, n = 0;

    if (!ext || !*ext)
        return FALSE;

    /* Prefer GL 3.0+ method */
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

    /* Fallback to legacy */
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

/* ---------------------------------------------------------------------------
 * PBO pool (small ring) – optimized for streaming uploads/downloads
 * -------------------------------------------------------------------------*/

typedef struct glamor_pbo_slot {
    GLuint id;
    void   *map;      /* Non-NULL when persistently mapped */
    size_t  size;     /* Allocated buffer storage size */
    Bool    persistent;
    Bool    coherent;
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
    }
    for (unsigned i = 0; i < 2; i++) {
        g_pbo_pool.download[i].id = 0;
        g_pbo_pool.download[i].map = NULL;
        g_pbo_pool.download[i].size = 0;
        g_pbo_pool.download[i].persistent = FALSE;
        g_pbo_pool.download[i].coherent = FALSE;
    }

    g_pbo_pool.inited = TRUE;
}

/* Ensure upload PBO slot is available and large enough. */
static Bool
glamor_pbo_upload_acquire(size_t required, glamor_pbo_slot **out)
{
    glamor_pbo_slot *slot = &g_pbo_pool.upload[g_pbo_pool.upload_index];
    g_pbo_pool.upload_index = (g_pbo_pool.upload_index + 1) & 3;

    /* If persistent desired but slot not created or too small, (re)create with BufferStorage. */
    if (g_pbo_pool.want_persistent) {
        const Bool need_new = (slot->id == 0) || (slot->size < required) || !slot->persistent;

        if (need_new) {
            if (slot->id) {
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);
                if (slot->map)
                    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER); /* In case mapped via non-persistent path */
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

            glBufferStorage(GL_PIXEL_UNPACK_BUFFER, (GLsizeiptr)required, NULL, storage_flags);

            const GLbitfield map_flags =
                GL_MAP_WRITE_BIT |
                GL_MAP_PERSISTENT_BIT |
                (g_pbo_pool.prefer_coherent ? GL_MAP_COHERENT_BIT : GL_MAP_FLUSH_EXPLICIT_BIT);

            slot->map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required, map_flags);
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

            if (!slot->map) {
                glDeleteBuffers(1, &slot->id);
                slot->id = 0;
                return FALSE;
            }

            slot->size = required;
            slot->persistent = TRUE;
            slot->coherent = g_pbo_pool.prefer_coherent;
        }

        *out = slot;
        return TRUE;
    }

    /* Non-persistent path: allocate (or reuse) buffer with BufferData (orphaning). */
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

/* Ensure download PBO slot is available and large enough. */
static Bool
glamor_pbo_download_acquire(size_t required, glamor_pbo_slot **out)
{
    glamor_pbo_slot *slot = &g_pbo_pool.download[g_pbo_pool.download_index];
    g_pbo_pool.download_index = (g_pbo_pool.download_index + 1) & 1;

    if (g_pbo_pool.want_persistent) {
        const Bool need_new = (slot->id == 0) || (slot->size < required) || !slot->persistent;
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
                GL_MAP_PERSISTENT_BIT; /* COHERENT not required; we'll fence for completion */

            glBufferStorage(GL_PIXEL_PACK_BUFFER, (GLsizeiptr)required, NULL, storage_flags);

            const GLbitfield map_flags =
                GL_MAP_READ_BIT |
                GL_MAP_PERSISTENT_BIT;

            slot->map = glMapBufferRange(GL_PIXEL_PACK_BUFFER, 0, (GLsizeiptr)required, map_flags);
            glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

            if (!slot->map) {
                glDeleteBuffers(1, &slot->id);
                slot->id = 0;
                return FALSE;
            }

            slot->size = required;
            slot->persistent = TRUE;
            slot->coherent = TRUE; /* For read, coherence semantics are simpler; we still fence. */
        }

        *out = slot;
        return TRUE;
    }

    /* Non-persistent path */
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
    RegionIntersect(&region, &region, gc->pCompositeClip);

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
    if (required < (size_t)32768) {
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

        memcpy(slot->map, bits, required);
        if (!slot->coherent) {
            glFlushMappedBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required);
        }

        glamor_upload_region(drawable, &region, x, y,
                             (const uint8_t *)(uintptr_t)0, byte_stride);

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    } else {
        /* Transient path: MapRange UNSYNCHRONIZED + INVALIDATE. */
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, slot->id);

        GLbitfield map_flags = GL_MAP_WRITE_BIT |
                               GL_MAP_INVALIDATE_BUFFER_BIT |
                               GL_MAP_UNSYNCHRONIZED_BIT;

        void *map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, (GLsizeiptr)required, map_flags);
        if (map) {
            memcpy(map, bits, required);
            glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);

            glamor_upload_region(drawable, &region, x, y,
                                 (const uint8_t *)(uintptr_t)0, byte_stride);
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
        { .val = gc->fgPixel },
        { .val = gc->bgPixel }
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
    GLuint                  tex         = 0;
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

    /* Set bitorder uniform from server compile-time constant. */
    GLint bitorder_loc = glGetUniformLocation(prog->prog, "bitorder");
    if (bitorder_loc != -1) {
        const int bitorder = (BITMAP_BIT_ORDER == MSBFirst) ? 1 : 0;
        glUniform1i(bitorder_loc, bitorder);
    }

    /* Upload bitmap as R8UI texture: width in bytes = stride. */
    glGenTextures(1, &tex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8UI,
                 (GLsizei)stride, (GLsizei)h, 0,
                 GL_RED_INTEGER, GL_UNSIGNED_BYTE, bits);

    glUniform1i(prog->font_uniform, 1);

    /* Build one instance vertex (x,y,w,h, leftPad, 0) ------------------- */
    char *vbo_offset = NULL;
    GLshort *vbo = glamor_get_vbo_space(screen,
                                        6 * sizeof(GLshort),
                                        &vbo_offset);
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

        int nbox = RegionNumRects(gc->pCompositeClip);
        const BoxPtr boxes = RegionRects(gc->pCompositeClip);

        for (int i = 0; i < nbox; i++) {
            const BoxRec *b = &boxes[i];
            glScissor(b->x1 + off_x, b->y1 + off_y,
                      b->x2 - b->x1, b->y2 - b->y1);
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
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE0);
    if (tex)
        glDeleteTextures(1, &tex);
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
            const int bytes_per_pixel = bpp >> 3;
            int pack_row_length = 0;
            if (bytes_per_pixel > 0)
                pack_row_length = (int)(byte_stride / (uint32_t)bytes_per_pixel);

            glPixelStorei(GL_PACK_ALIGNMENT, 4);
            glPixelStorei(GL_PACK_ROW_LENGTH, pack_row_length);

            glamor_set_destination_pixmap_priv_nc(glamor_priv, src_pixmap, src_priv);
            glReadPixels(box.x1, box.y1, w, h, format->format, format->type, (void *)0);

            /* Ensure GPU completed the transfer; then ensure CPU visibility for persistent mapping. */
            GLsync sync = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
            if (sync) {
                glClientWaitSync(sync, GL_SYNC_FLUSH_COMMANDS_BIT, GL_TIMEOUT_IGNORED);
                glDeleteSync(sync);
            }

            /* For persistent mapping, slot->map already points to buffer; else, map once. */
            const uint8_t *src;
            void *temp_map = NULL;
            if (slot->persistent) {
                /* Memory barrier so CPU sees GPU writes (esp. on non-coherent devices). */
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
            uint8_t *dst_row = (uint8_t *)dst;
            const uint8_t *src_row = src;

            if (byte_stride * (size_t)h <= required) {
                memcpy(dst_row, src_row, required);
            } else {
                /* Conservative copy (should not happen with our PACK_ROW_LENGTH), keep robust. */
                for (int i = 0; i < h; i++) {
                    memcpy(dst_row, src_row, byte_stride);
                    src_row += byte_stride;
                    dst_row += byte_stride;
                }
            }

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
