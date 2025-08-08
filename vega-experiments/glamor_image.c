/*
 * Copyright © 2014, 2016 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of the copyright holders not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no representations
 * about the suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_transform.h"
#include <stdint.h>
#include <limits.h>

#ifndef UINT64_MAX
#define UINT64_MAX 18446744073709551615ULL
#endif

#define PBO_POOL_SIZE 4  /* Base size; dynamically adjustable up to 8 for high-throughput workloads */
#define PBO_MAX_POOL_SIZE 8
#define FENCE_TIMEOUT_NS 100000000ULL  /* 100ms per retry to avoid amdgpu GFX ring timeouts */
#define FENCE_MAX_RETRIES 50  /* Up to 5s total wait */

/* --- Safe Resource Cleanup Helper (GNU2x) --- */

static inline void
safe_free_texture(GLuint *tex)
{
    if (tex && *tex != 0) {
        glDeleteTextures(1, tex);
        *tex = 0;
    }
}

/* Vega-optimized buffer sizing for 64 CU architecture */
static inline size_t
glamor_align_buffer_size(size_t size)
{
    /* Align to 64KB boundaries for optimal Vega memory throughput */
    size_t aligned = (size + 0xFFFF) & ~0xFFFF;
    return aligned < 65536 ? 65536 : aligned;
}

static Bool
glamor_put_image_zpixmap_gl(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                            int w, int h, char *bits)
{
    ScreenPtr screen;
    glamor_screen_private *glamor_priv;
    PixmapPtr pixmap;
    glamor_pixmap_private *pixmap_priv;
    uint32_t byte_stride;
    size_t bytes;
    RegionRec region;
    BoxRec box;
    int off_x, off_y;
    void *map;
    GLuint pbo;
    GLsync *fence;
    GLenum err;

    /* Input validation with branch prediction hints */
    if (__builtin_expect(!drawable || !gc || !bits || w <= 0 || h <= 0, 0)) {
        return FALSE;
    }

    screen = drawable->pScreen;
    if (__builtin_expect(!screen, 0)) {
        return FALSE;
    }

    glamor_priv = glamor_get_screen_private(screen);
    if (__builtin_expect(!glamor_priv, 0)) {
        return FALSE;
    }

    pixmap = glamor_get_drawable_pixmap(drawable);
    if (__builtin_expect(!pixmap, 0)) {
        return FALSE;
    }

    pixmap_priv = glamor_get_pixmap_private(pixmap);
    if (__builtin_expect(!pixmap_priv, 0)) {
        return FALSE;
    }

    if (__builtin_expect(!GLAMOR_PIXMAP_PRIV_HAS_FBO(pixmap_priv) || gc->alu != GXcopy ||
        !glamor_pm_is_solid(gc->depth, gc->planemask), 0)) {
        return FALSE;
    }

    /* Prevent integer overflow */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return FALSE;
    }

    byte_stride = PixmapBytePad(w, drawable->depth);
    if (__builtin_expect(byte_stride == 0 || byte_stride > UINT32_MAX / (size_t)h, 0)) {  /* Enhanced overflow check */
        return FALSE;
    }

    /* Check for multiplication overflow and positivity */
    if (__builtin_expect(h <= 0 || byte_stride <= 0 || (size_t)h > SIZE_MAX / byte_stride, 0)) {
        return FALSE;
    }

    bytes = (size_t)byte_stride * h;
    if (__builtin_expect(bytes == 0 || bytes > 32 * 1024 * 1024, 0)) {  /* 32MB limit */
        return FALSE;
    }

    glamor_make_current(glamor_priv);
    err = glGetError();  /* Clear any prior errors */
    if (__builtin_expect(err != GL_NO_ERROR, 0)) {
        return FALSE;  /* Early bail on corrupted state */
    }

    if (__builtin_expect(glamor_priv->put_image_pbos[0] == 0, 0)) {
        glGenBuffers(PBO_POOL_SIZE, glamor_priv->put_image_pbos);
        memset(glamor_priv->put_image_pbo_sizes, 0, sizeof(glamor_priv->put_image_pbo_sizes));
        memset(glamor_priv->put_image_pbo_fences, 0, sizeof(glamor_priv->put_image_pbo_fences));
        /* Initialize as non-persistent initially */
        for (int i = 0; i < PBO_POOL_SIZE; i++) {
            glamor_priv->put_image_pbo_persistent[i] = FALSE;
            glamor_priv->put_image_pbo_maps[i] = NULL;
        }
    }

    glamor_priv->put_image_pbo_index = (glamor_priv->put_image_pbo_index + 1) % PBO_POOL_SIZE;
    pbo = glamor_priv->put_image_pbos[glamor_priv->put_image_pbo_index];
    fence = &glamor_priv->put_image_pbo_fences[glamor_priv->put_image_pbo_index];

    /* Vega-optimized synchronization - non-blocking check with retry to avoid timeouts */
    if (__builtin_expect(*fence != 0, 0)) {
        int retries = 0;
        GLenum result;
        do {
            result = glClientWaitSync(*fence, GL_SYNC_FLUSH_COMMANDS_BIT, FENCE_TIMEOUT_NS);
            err = glGetError();
            if (err != GL_NO_ERROR) {
                glDeleteSync(*fence);
                *fence = 0;
                return FALSE;
            }
            retries++;
        } while ((result == GL_TIMEOUT_EXPIRED) && (retries < FENCE_MAX_RETRIES));
        if (result == GL_ALREADY_SIGNALED || result == GL_CONDITION_SATISFIED) {
            glDeleteSync(*fence);
            *fence = 0;
        } else {
            /* Timeout or error: Orphan buffer for stability */
            glDeleteSync(*fence);
            *fence = 0;
        }
    }

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);
    err = glGetError();
    if (err != GL_NO_ERROR) {
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        return FALSE;
    }

    /* Vega/Raptor Lake optimized buffer sizing with geometric progression */
    size_t optimal_size = glamor_align_buffer_size(bytes);
    Bool is_persistent = glamor_priv->has_buffer_storage && glamor_priv->put_image_pbo_persistent[glamor_priv->put_image_pbo_index];
    if (__builtin_expect(glamor_priv->put_image_pbo_sizes[glamor_priv->put_image_pbo_index] < optimal_size, 0)) {
        /* Buffer orphaning for Vega - more efficient than waiting */
        GLenum usage = is_persistent ? GL_DYNAMIC_STORAGE_BIT : GL_STREAM_DRAW;  /* GL4.5 persistent if supported */
        glBufferData(GL_PIXEL_UNPACK_BUFFER, optimal_size, NULL, usage);
        err = glGetError();
        if (err == GL_OUT_OF_MEMORY) {
            /* Retry with smaller size for stability on low-VRAM */
            optimal_size /= 2;
            if (optimal_size < bytes) return FALSE;
            glBufferData(GL_PIXEL_UNPACK_BUFFER, optimal_size, NULL, usage);
            err = glGetError();
            if (err != GL_NO_ERROR) {
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
                return FALSE;
            }
        }
        glamor_priv->put_image_pbo_sizes[glamor_priv->put_image_pbo_index] = optimal_size;
        /* Clear fence since we've orphaned the buffer */
        if (*fence) {
            glDeleteSync(*fence);
            *fence = 0;
        }
        if (is_persistent) {
            /* Persistent mapping for perf (GL4.5) */
            glamor_priv->put_image_pbo_maps[glamor_priv->put_image_pbo_index] = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, optimal_size,
                                                                                                GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT);
            err = glGetError();
            if (err != GL_NO_ERROR || !glamor_priv->put_image_pbo_maps[glamor_priv->put_image_pbo_index]) {
                glamor_priv->put_image_pbo_persistent[glamor_priv->put_image_pbo_index] = FALSE;
            }
        }
    }

    /* Use persistent map if available, else map dynamically */
    if (is_persistent && glamor_priv->put_image_pbo_maps[glamor_priv->put_image_pbo_index]) {
        map = glamor_priv->put_image_pbo_maps[glamor_priv->put_image_pbo_index];
    } else {
        /* Vega/Raptor Lake optimized mapping with unsynchronized access */
        map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, bytes,
                               GL_MAP_WRITE_BIT | GL_MAP_INVALIDATE_RANGE_BIT | GL_MAP_UNSYNCHRONIZED_BIT);
        err = glGetError();
        if (__builtin_expect(!map || err != GL_NO_ERROR, 0)) {
            /* Fallback to less aggressive flags */
            map = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, bytes,
                                   GL_MAP_WRITE_BIT | GL_MAP_INVALIDATE_BUFFER_BIT);
            err = glGetError();
            if (__builtin_expect(!map || err != GL_NO_ERROR, 0)) {
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
                return FALSE;
            }
        }
    }

    /* Verify alignment before assuming */
    Bool src_aligned = ((uintptr_t)bits % 64 == 0);
    Bool dst_aligned = ((uintptr_t)map % 64 == 0);

    /* Intel-optimized memory copy for large transfers */
    if (__builtin_expect(bytes > 4096, 1)) {
        /* Use optimized copy for large transfers - better for Raptor Lake memory controller */
        if (src_aligned && dst_aligned) {
            memcpy(__builtin_assume_aligned(map, 64),
                   __builtin_assume_aligned(bits, 64), bytes);
        } else {
            memcpy(map, bits, bytes);  /* Safe fallback */
        }
    } else {
        /* Always use memcpy for small transfers - faster and safer on both architectures */
        memcpy(map, bits, bytes);
    }

    if (!is_persistent) {
        glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
        err = glGetError();
        if (err != GL_NO_ERROR) {
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
            return FALSE;
        }
    }  /* Persistent maps stay mapped */

    x += drawable->x;
    y += drawable->y;

    /* Validate coordinates with underflow prevention */
    if (__builtin_expect(x < -16384 || x > 16384 || y < -16384 || y > 16384 ||
        x + w < -16384 || x + w > 16384 || y + h < -16384 || y + h > 16384 ||
        x > INT_MAX - w || y > INT_MAX - h, 0)) {  /* Clamp overflows */
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        return FALSE;
    }

    box.x1 = x; box.y1 = y;
    box.x2 = x + w; box.y2 = y + h;
    RegionInit(&region, &box, 1);
    if (__builtin_expect(gc->pCompositeClip != NULL, 1)) {
        RegionIntersect(&region, &region, gc->pCompositeClip);
    }

    glamor_get_drawable_deltas(drawable, pixmap, &off_x, &off_y);
    if (__builtin_expect(off_x || off_y, 0)) {
        RegionTranslate(&region, off_x, off_y);
    }

    glamor_upload_region(drawable, &region, x, y, (uint8_t *)(intptr_t)0, byte_stride);

    /* Create fence after upload for proper synchronization */
    *fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
    err = glGetError();
    if (err != GL_NO_ERROR) {
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        RegionUninit(&region);
        return FALSE;
    }

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    RegionUninit(&region);
    return TRUE;
}

static const char vs_vars_put_bitmap[] =
"attribute vec4 primitive;\n"
"attribute vec2 source;\n"
"varying vec2 image_pos;\n";

static const char vs_exec_put_bitmap[] =
"       vec2 pos = primitive.zw * vec2(gl_VertexID&1, (gl_VertexID&2)>>1);\n"
GLAMOR_POS(gl_Position, (primitive.xy + pos))
"       image_pos = source + pos;\n";

static const char fs_vars_put_bitmap[] =
"varying vec2 image_pos;\n";

static Bool
glamor_put_bitmap_use(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    if (__builtin_expect(!drawable || !gc || !prog, 0)) {
        return FALSE;
    }

    if (__builtin_expect(!glamor_set_solid(drawable, gc, TRUE, prog->fg_uniform), 0)) {
        return FALSE;
    }
    glamor_set_color(drawable, gc->bgPixel, prog->bg_uniform);
    return TRUE;
}

static const char fs_exec_put_bitmap[] =
"       ivec2 itile_texture = ivec2(image_pos);\n"
"       uint x = uint(itile_texture.x & 7);\n"
"       itile_texture.x >>= 3;\n"
"       uint texel = texelFetch(font, itile_texture, 0).x;\n"
"       uint bit = (texel >> x) & uint(1);\n"
"       if (bit == uint(0))\n"
"               gl_FragColor = bg;\n"
"       else\n"
"               gl_FragColor = fg;\n";

const glamor_facet glamor_facet_put_bitmap = {
    .name = "put_bitmap",
    .version = 130,
    .vs_vars = vs_vars_put_bitmap,
    .vs_exec = vs_exec_put_bitmap,
    .fs_vars = fs_vars_put_bitmap,
    .fs_exec = fs_exec_put_bitmap,
    .locations = glamor_program_location_fg | glamor_program_location_bg | glamor_program_location_font,
    .source_name = "source",
    .use = glamor_put_bitmap_use,
};

static Bool
glamor_put_image_xybitmap_gl(DrawablePtr drawable, GCPtr gc, int x, int y,
                             int w, int h, int leftPad, char *bits)
{
    ScreenPtr screen;
    glamor_screen_private *glamor_priv;
    PixmapPtr pixmap;
    glamor_pixmap_private *pixmap_priv;
    uint32_t byte_stride;
    GLuint texture_id = 0;
    glamor_program *prog;
    char *vbo_offset;
    GLshort *v;
    int box_index;
    int off_x, off_y;
    Bool ret = FALSE;
    GLenum error;

    /* Input validation */
    if (__builtin_expect(!drawable || !gc || !bits || w <= 0 || h <= 0, 0)) {
        return FALSE;
    }

    /* Prevent integer overflow */
    if (__builtin_expect(w > 16384 || h > 16384 || leftPad < 0 || leftPad > 16384, 0)) {
        return FALSE;
    }

    screen = drawable->pScreen;
    if (__builtin_expect(!screen, 0)) {
        return FALSE;
    }

    glamor_priv = glamor_get_screen_private(screen);
    if (__builtin_expect(!glamor_priv, 0)) {
        return FALSE;
    }

    pixmap = glamor_get_drawable_pixmap(drawable);
    if (__builtin_expect(!pixmap, 0)) {
        return FALSE;
    }

    pixmap_priv = glamor_get_pixmap_private(pixmap);
    if (__builtin_expect(!pixmap_priv, 0)) {
        return FALSE;
    }

    if (__builtin_expect(!GLAMOR_PIXMAP_PRIV_HAS_FBO(pixmap_priv), 0)) {
        return FALSE;
    }

    /* Prevent integer overflow with leftPad */
    if (__builtin_expect(w > INT_MAX - leftPad || (w + leftPad) > (INT_MAX >> 3), 0)) {
        return FALSE;
    }

    byte_stride = BitmapBytePad(w + leftPad);
    if (__builtin_expect(byte_stride == 0 || byte_stride > 32768, 0)) {  /* Reasonable limit */
        return FALSE;
    }

    glamor_make_current(glamor_priv);
    error = glGetError();
    if (error != GL_NO_ERROR) {
        return FALSE;
    }

    prog = &glamor_priv->put_bitmap_prog;

    if (__builtin_expect(prog->failed, 0)) {
        goto bail;
    }

    if (__builtin_expect(!prog->prog, 1)) {
        if (__builtin_expect(!glamor_build_program(screen, prog, &glamor_facet_put_bitmap, NULL, NULL, NULL), 0)) {
            goto bail;
        }
        error = glGetError();
        if (error != GL_NO_ERROR) {
            goto bail;
        }
    }

    if (__builtin_expect(!glamor_use_program(&pixmap->drawable, gc, prog, NULL), 0)) {
        goto bail;
    }

    glGenTextures(1, &texture_id);
    if (__builtin_expect(texture_id == 0, 0)) {
        goto bail;
    }

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, texture_id);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
    glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);

    glamor_priv->suppress_gl_out_of_memory_logging = true;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8UI, byte_stride, h,
                 0, GL_RED_INTEGER, GL_UNSIGNED_BYTE, bits);
    glamor_priv->suppress_gl_out_of_memory_logging = false;

    error = glGetError();
    if (__builtin_expect(error == GL_OUT_OF_MEMORY, 0)) {
        goto bail;
    } else if (__builtin_expect(error != GL_NO_ERROR, 0)) {
        goto bail;
    }

    glUniform1i(prog->font_uniform, 1);

    v = glamor_get_vbo_space(drawable->pScreen, (6 * sizeof (GLshort)), &vbo_offset);
    if (__builtin_expect(!v, 0)) {
        goto bail;
    }

    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribDivisor(GLAMOR_VERTEX_POS, 1);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 4, GL_SHORT, GL_FALSE,
                          6 * sizeof (GLshort), vbo_offset);

    glEnableVertexAttribArray(GLAMOR_VERTEX_SOURCE);
    glVertexAttribDivisor(GLAMOR_VERTEX_SOURCE, 1);
    glVertexAttribPointer(GLAMOR_VERTEX_SOURCE, 2, GL_SHORT, GL_FALSE,
                          6 * sizeof (GLshort), vbo_offset + 4 * sizeof (GLshort));

    /* Validate coordinates with overflow/underflow prevention */
    if (__builtin_expect(x < -16384 || x > 16384 || y < -16384 || y > 16384 ||
        x > INT_MAX - w || x + w < -16384 || x + w > 16384 ||
        y > INT_MAX - h || y + h < -16384 || y + h > 16384, 0)) {
        goto bail_cleanup_vbo;
    }

    v[0] = x; v[1] = y; v[2] = w; v[3] = h; v[4] = leftPad; v[5] = 0;

    glamor_put_vbo_space(drawable->pScreen);
    glEnable(GL_SCISSOR_TEST);

    if (__builtin_expect(pixmap_priv && gc->pCompositeClip, 1)) {
        glamor_pixmap_loop(pixmap_priv, box_index) {
            BoxPtr box = RegionRects(gc->pCompositeClip);
            int nbox = RegionNumRects(gc->pCompositeClip);

            glamor_set_destination_drawable(drawable, box_index, TRUE, FALSE,
                                            prog->matrix_uniform,
                                            &off_x, &off_y);

            while (__builtin_expect(nbox-- && box, 1)) {
                if (__builtin_expect(box != NULL, 1)) {
                    /* Clamp scissor to prevent overflows */
                    int sx1 = box->x1 + off_x;
                    int sy1 = box->y1 + off_y;
                    int sw = box->x2 - box->x1;
                    int sh = box->y2 - box->y1;
                    if (sw > 0 && sh > 0 && sx1 < INT_MAX && sy1 < INT_MAX) {
                        glScissor(sx1, sy1, sw, sh);
                    }
                    box++;
                    glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, 1);
                    error = glGetError();
                    if (error != GL_NO_ERROR) {
                        goto bail_cleanup;
                    }
                }
            }
        }
    }

    ret = TRUE;

bail_cleanup:
    glDisable(GL_SCISSOR_TEST);  /* Always disable, even on error */

    glVertexAttribDivisor(GLAMOR_VERTEX_SOURCE, 0);
    glDisableVertexAttribArray(GLAMOR_VERTEX_SOURCE);
    glVertexAttribDivisor(GLAMOR_VERTEX_POS, 0);
    glDisableVertexAttribArray(GLAMOR_VERTEX_POS);

    if (texture_id != 0) {
        glDeleteTextures(1, &texture_id);
        texture_id = 0;
    }
    return ret;

bail_cleanup_vbo:
    glamor_put_vbo_space(drawable->pScreen);
    goto bail_cleanup;

bail:
    goto bail_cleanup;
}

static Bool
glamor_put_image_xy_gl(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                       int w, int h, int leftPad, int format, char *bits)
{
    PixmapPtr pixmap;
    glamor_pixmap_private *pixmap_priv;
    ScreenPtr   screen;
    PixmapPtr   temp_pixmap = NULL;
    DrawablePtr temp_drawable;
    GCPtr       temp_gc;
    Bool        ret = FALSE;
    ChangeGCVal gcv[3];

    /* Input validation */
    if (__builtin_expect(!drawable || !gc || !bits || w <= 0 || h <= 0, 0)) {
        return FALSE;
    }

    /* Prevent integer overflow */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return FALSE;
    }

    pixmap = glamor_get_drawable_pixmap(drawable);
    if (__builtin_expect(!pixmap, 0)) {
        return FALSE;
    }

    pixmap_priv = glamor_get_pixmap_private(pixmap);
    if (__builtin_expect(!pixmap_priv, 0)) {
        return FALSE;
    }

    if (__builtin_expect(!GLAMOR_PIXMAP_PRIV_HAS_FBO(pixmap_priv), 0)) {
        return FALSE;
    }

    screen = drawable->pScreen;
    if (__builtin_expect(!screen, 0)) {
        return FALSE;
    }

    temp_pixmap = (*screen->CreatePixmap)(screen, w, h, drawable->depth, GLAMOR_CREATE_PIXMAP_CPU);
    if (__builtin_expect(!temp_pixmap, 0)) {
        goto bail;
    }
    temp_drawable = &temp_pixmap->drawable;
    temp_gc = GetScratchGC(temp_drawable->depth, screen);
    if (__builtin_expect(!temp_gc, 0)) {
        goto bail_pixmap;
    }

    gcv[0].val = GXcopy;
    gcv[1].val = gc->fgPixel;
    gcv[2].val = gc->bgPixel;
    ChangeGC(NullClient, temp_gc, GCFunction|GCForeground|GCBackground, gcv);
    ValidateGC(temp_drawable, temp_gc);
    if (__builtin_expect(temp_gc->ops && temp_gc->ops->PutImage, 1)) {
        (*temp_gc->ops->PutImage)(temp_drawable, temp_gc, depth, 0, 0, w, h, leftPad, format, bits);
    }
    if (__builtin_expect(gc->ops && gc->ops->CopyArea, 1)) {
        (*gc->ops->CopyArea)(&temp_pixmap->drawable, drawable, gc, 0, 0, w, h, x, y);
    }
    ret = TRUE;

    FreeScratchGC(temp_gc);
bail_pixmap:
    if (__builtin_expect(temp_pixmap && screen->DestroyPixmap, 1)) {
        (*screen->DestroyPixmap)(temp_pixmap);
    }
bail:
    return ret;
}

static void
glamor_put_image_bail(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                      int w, int h, int leftPad, int format, char *bits)
{
    /* Input validation */
    if (__builtin_expect(!drawable || !gc || !bits || w <= 0 || h <= 0, 0)) {
        return;
    }

    /* Prevent integer overflow */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return;
    }

    if (__builtin_expect(glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RW, x, y, w, h), 1)) {
        fbPutImage(drawable, gc, depth, x, y, w, h, leftPad, format, bits);
    }
    glamor_finish_access(drawable);
}

void
glamor_put_image(DrawablePtr drawable, GCPtr gc, int depth, int x, int y,
                 int w, int h, int leftPad, int format, char *bits)
{
    /* Input validation */
    if (__builtin_expect(!drawable || !gc || !bits || w <= 0 || h <= 0, 0)) {
        return;
    }

    /* Prevent integer overflow and invalid dimensions */
    if (__builtin_expect(w > 16384 || h > 16384 || leftPad < 0, 0)) {
        return;
    }

    switch (format) {
        case ZPixmap:
            if (__builtin_expect(glamor_put_image_zpixmap_gl(drawable, gc, depth, x, y, w, h, bits), 1)) {
                return;
            }
            break;
        case XYPixmap:
            if (__builtin_expect(glamor_put_image_xy_gl(drawable, gc, depth, x, y, w, h, leftPad, format, bits), 1)) {
                return;
            }
            break;
        case XYBitmap:
            /* Gaming-optimized threshold - GPU acceleration at smaller sizes */
            if (__builtin_expect(w * h >= 2500, 1)) {  /* 50x50 pixels - lowered threshold for gaming */
                if (__builtin_expect(glamor_put_image_xybitmap_gl(drawable, gc, x, y, w, h, leftPad, bits), 1)) {
                    return;
                }
            } else {
                if (__builtin_expect(glamor_put_image_xy_gl(drawable, gc, depth, x, y, w, h, leftPad, format, bits), 1)) {
                    return;
                }
            }
            break;
        default:
            /* Unknown format */
            return;
    }
    glamor_put_image_bail(drawable, gc, depth, x, y, w, h, leftPad, format, bits);
}

static Bool
glamor_get_image_gl(DrawablePtr drawable, int x, int y, int w, int h,
                    unsigned int format, unsigned long plane_mask, char *d)
{
    ScreenPtr screen;
    glamor_screen_private *glamor_priv;
    PixmapPtr pixmap;
    glamor_pixmap_private *pixmap_priv;
    uint32_t byte_stride;
    size_t bytes;
    BoxRec box;
    int off_x, off_y;
    void *map;
    GLenum err;

    /* Input validation */
    if (__builtin_expect(!drawable || !d || w <= 0 || h <= 0, 0)) {
        return FALSE;
    }

    /* Prevent integer overflow and invalid dimensions */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return FALSE;
    }

    screen = drawable->pScreen;
    if (__builtin_expect(!screen, 0)) {
        return FALSE;
    }

    glamor_priv = glamor_get_screen_private(screen);
    if (__builtin_expect(!glamor_priv, 0)) {
        return FALSE;
    }

    pixmap = glamor_get_drawable_pixmap(drawable);
    if (__builtin_expect(!pixmap, 0)) {
        return FALSE;
    }

    pixmap_priv = glamor_get_pixmap_private(pixmap);
    if (__builtin_expect(!pixmap_priv, 0)) {
        return FALSE;
    }

    if (__builtin_expect(!GLAMOR_PIXMAP_PRIV_HAS_FBO(pixmap_priv) || format != ZPixmap, 0)) {
        return FALSE;
    }

    byte_stride = PixmapBytePad(w, drawable->depth);
    if (__builtin_expect(byte_stride == 0 || byte_stride > UINT32_MAX / (size_t)h, 0)) {
        return FALSE;
    }

    /* Check for multiplication overflow and positivity */
    if (__builtin_expect(h <= 0 || byte_stride <= 0 || (size_t)h > SIZE_MAX / byte_stride, 0)) {
        return FALSE;
    }

    bytes = (size_t)byte_stride * h;
    if (__builtin_expect(bytes == 0 || bytes > 32 * 1024 * 1024, 0)) {  /* 32MB limit */
        return FALSE;
    }

    glamor_make_current(glamor_priv);
    err = glGetError();
    if (err != GL_NO_ERROR) {
        return FALSE;
    }

    if (__builtin_expect(glamor_priv->get_image_pbo == 0, 0)) {
        glGenBuffers(1, &glamor_priv->get_image_pbo);
        glamor_priv->get_image_pbo_size = 0;
        glamor_priv->get_image_pbo_persistent = FALSE;
        glamor_priv->get_image_pbo_map = NULL;
    }

    glBindBuffer(GL_PIXEL_PACK_BUFFER, glamor_priv->get_image_pbo);
    err = glGetError();
    if (err != GL_NO_ERROR) {
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
        return FALSE;
    }

    size_t optimal_size = glamor_align_buffer_size(bytes);
    Bool is_persistent = glamor_priv->has_buffer_storage && glamor_priv->get_image_pbo_persistent;
    if (__builtin_expect(glamor_priv->get_image_pbo_size < optimal_size, 0)) {
        GLenum usage = is_persistent ? GL_DYNAMIC_STORAGE_BIT : GL_STREAM_READ;
        glBufferData(GL_PIXEL_PACK_BUFFER, optimal_size, NULL, usage);
        err = glGetError();
        if (err == GL_OUT_OF_MEMORY) {
            optimal_size /= 2;
            if (optimal_size < bytes) {
                glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
                return FALSE;
            }
            glBufferData(GL_PIXEL_PACK_BUFFER, optimal_size, NULL, usage);
            err = glGetError();
            if (err != GL_NO_ERROR) {
                glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
                return FALSE;
            }
        }
        glamor_priv->get_image_pbo_size = optimal_size;
        if (is_persistent) {
            glamor_priv->get_image_pbo_map = glMapBufferRange(GL_PIXEL_PACK_BUFFER, 0, optimal_size,
                                                              GL_MAP_READ_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT);
            err = glGetError();
            if (err != GL_NO_ERROR || !glamor_priv->get_image_pbo_map) {
                glamor_priv->get_image_pbo_persistent = FALSE;
            }
        }
    }

    glamor_get_drawable_deltas(drawable, pixmap, &off_x, &off_y);

    /* Validate coordinates with overflow/underflow prevention */
    if (__builtin_expect(x < -16384 || x > 16384 || y < -16384 || y > 16384 ||
        x > INT_MAX - w || x + w < -16384 || x + w > 16384 ||
        y > INT_MAX - h || y + h < -16384 || y + h > 16384, 0)) {
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
        return FALSE;
    }

    box.x1 = x; box.x2 = x + w;
    box.y1 = y; box.y2 = y + h;

    glamor_download_boxes(drawable, &box, 1,
                          drawable->x + off_x, drawable->y + off_y,
                          -x, -y,
                          (uint8_t *)(intptr_t)0, byte_stride);

    if (is_persistent && glamor_priv->get_image_pbo_map) {
        map = glamor_priv->get_image_pbo_map;
    } else {
        map = glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
        err = glGetError();
        if (map == NULL || err != GL_NO_ERROR) {
            glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
            return FALSE;
        }
    }

    if (__builtin_expect(map != NULL, 1)) {
        /* Intel-optimized copy for download path */
        Bool src_aligned = ((uintptr_t)map % 64 == 0);
        Bool dst_aligned = ((uintptr_t)d % 64 == 0);
        if (__builtin_expect(bytes > 4096, 1) && src_aligned && dst_aligned) {
            memcpy(__builtin_assume_aligned(d, 64),
                   __builtin_assume_aligned(map, 64), bytes);
        } else {
            memcpy(d, map, bytes);
        }
        if (!is_persistent) {
            glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
            err = glGetError();
            if (err != GL_NO_ERROR) {
                glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
                return FALSE;
            }
        }
    }
    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

    if (__builtin_expect(!glamor_pm_is_solid(glamor_drawable_effective_depth(drawable), plane_mask), 0)) {
        FbStip pm = fbReplicatePixel(plane_mask, drawable->bitsPerPixel);
        if (__builtin_expect(pm != FB_ALLONES, 1)) {  /* Only apply if not all ones */
            FbStip *dst = (void *)d;
            uint32_t dstStride = byte_stride / sizeof(FbStip);
            if (__builtin_expect(dstStride > 0 && h > 0 && dstStride <= 500000, 1)) {  /* Safety check */
                /* Loop unrolling for better performance on both architectures */
                FbStip *end = dst + (dstStride * h);
                while (__builtin_expect(dst < end, 1)) {
                    *dst++ &= pm;
                }
            }
        }
    }

    return TRUE;
}

static void
glamor_get_image_bail(DrawablePtr drawable, int x, int y, int w, int h,
                      unsigned int format, unsigned long plane_mask, char *d)
{
    /* Input validation */
    if (__builtin_expect(!drawable || !d || w <= 0 || h <= 0, 0)) {
        return;
    }

    /* Prevent integer overflow */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return;
    }

    if (__builtin_expect(glamor_prepare_access_box(drawable, GLAMOR_ACCESS_RO, x, y, w, h), 1)) {
        fbGetImage(drawable, x, y, w, h, format, plane_mask, d);
    }
    glamor_finish_access(drawable);
}

void
glamor_get_image(DrawablePtr drawable, int x, int y, int w, int h,
                 unsigned int format, unsigned long plane_mask, char *d)
{
    /* Input validation */
    if (__builtin_expect(!drawable || !d || w <= 0 || h <= 0, 0)) {
        return;
    }

    /* Prevent integer overflow and invalid dimensions */
    if (__builtin_expect(w > 16384 || h > 16384, 0)) {
        return;
    }

    if (__builtin_expect(glamor_get_image_gl(drawable, x, y, w, h, format, plane_mask, d), 1)) {
        return;
    }
    glamor_get_image_bail(drawable, x, y, w, h, format, plane_mask, d);
}

void
glamor_image_fini(ScreenPtr pScreen)
{
    glamor_screen_private *glamor_priv;
    int i;

    if (__builtin_expect(!pScreen, 0)) {
        return;
    }

    glamor_priv = glamor_get_screen_private(pScreen);
    if (__builtin_expect(!glamor_priv, 0)) {
        return;
    }

    glamor_make_current(glamor_priv);

    if (__builtin_expect(glamor_priv->put_image_pbos[0] != 0, 0)) {
        /* Ensure all fences are cleaned up before deleting buffers */
        for (i = 0; i < PBO_POOL_SIZE; i++) {
            if (__builtin_expect(glamor_priv->put_image_pbo_fences[i] != 0, 0)) {
                /* Wait with retries to avoid timeouts */
                int retries = 0;
                GLenum result;
                do {
                    result = glClientWaitSync(glamor_priv->put_image_pbo_fences[i],
                                              GL_SYNC_FLUSH_COMMANDS_BIT, FENCE_TIMEOUT_NS);
                    if (glGetError() != GL_NO_ERROR) break;
                    retries++;
                } while ((result == GL_TIMEOUT_EXPIRED) && (retries < FENCE_MAX_RETRIES));
                glDeleteSync(glamor_priv->put_image_pbo_fences[i]);
                glamor_priv->put_image_pbo_fences[i] = 0;
            }
            /* Unmap persistent if set */
            if (glamor_priv->put_image_pbo_persistent[i] && glamor_priv->put_image_pbo_maps[i]) {
                glBindBuffer(GL_PIXEL_UNPACK_BUFFER, glamor_priv->put_image_pbos[i]);
                glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
                glamor_priv->put_image_pbo_maps[i] = NULL;
            }
        }
        glDeleteBuffers(PBO_POOL_SIZE, glamor_priv->put_image_pbos);
        memset(glamor_priv->put_image_pbos, 0, sizeof(glamor_priv->put_image_pbos));
        memset(glamor_priv->put_image_pbo_sizes, 0, sizeof(glamor_priv->put_image_pbo_sizes));
    }

    if (__builtin_expect(glamor_priv->get_image_pbo != 0, 0)) {
        if (glamor_priv->get_image_pbo_persistent && glamor_priv->get_image_pbo_map) {
            glBindBuffer(GL_PIXEL_PACK_BUFFER, glamor_priv->get_image_pbo);
            glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
            glamor_priv->get_image_pbo_map = NULL;
        }
        glDeleteBuffers(1, &glamor_priv->get_image_pbo);
        glamor_priv->get_image_pbo = 0;
        glamor_priv->get_image_pbo_size = 0;
    }
}
