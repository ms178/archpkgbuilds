/*
 * SPDX-License-Identifier: MIT
 *
 * glamor_copy.c - High-performance GPU copy operations
 *
 * Stable AMD/Mesa-focused optimization pass based on the user's last known-good
 * revision.  The goal is to keep the proven control flow and GL state model,
 * while fixing correctness holes and applying only optimizations that should not
 * reduce stability.
 */

#include "glamor_priv.h"
#include "glamor_prepare.h"
#include "glamor_transform.h"
#include "glamor_transfer.h"

#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if defined(__AVX2__) && (defined(__GNUC__) || defined(__clang__))
#include <immintrin.h>
#define HAVE_AVX2_INTRINSICS 1
#else
#define HAVE_AVX2_INTRINSICS 0
#endif

#if defined(__BMI__) && (defined(__GNUC__) || defined(__clang__))
#include <x86intrin.h>
#define HAVE_BMI_INTRINSICS 1
#else
#define HAVE_BMI_INTRINSICS 0
#endif

#if defined(__BMI2__) && (defined(__GNUC__) || defined(__clang__))
#include <x86intrin.h>
#define HAVE_BMI2_INTRINSICS 1
#else
#define HAVE_BMI2_INTRINSICS 0
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

#define LOCAL_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define LOCAL_MAX(a, b) (((a) > (b)) ? (a) : (b))

#define GLAMOR_COPY_MAX_SCREENS            16
#define GLAMOR_COPY_IMAGE_MIN_PIXELS       (64u * 64u)
#define GLAMOR_COPY_IMAGE_MIN_DIM          32
#define GLAMOR_COPY_IMAGE_MAX_BOXES        4
#define GLAMOR_COPY_STACK_BOXES            1024
#define GLAMOR_COPY_TEMP_STACK_BOXES       256
#define GLAMOR_COPY_SCRATCH_VBO_CAPACITY   (1024u * 1024u)
#define GLAMOR_COPY_VBO_ALIGN              64u

#ifndef GL_TILE_RASTER_ORDER_FIXED_MESA
#define GL_TILE_RASTER_ORDER_FIXED_MESA          0x8BB8
#define GL_TILE_RASTER_ORDER_INCREASING_X_MESA   0x8BB9
#define GL_TILE_RASTER_ORDER_INCREASING_Y_MESA   0x8BBA
#endif

typedef enum {
    CPU_FEATURE_NONE = 0,
    CPU_FEATURE_AVX2 = 1u << 0,
    CPU_FEATURE_BMI  = 1u << 1,
    CPU_FEATURE_BMI2 = 1u << 2,
} cpu_feature_level;

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
#if HAVE_AVX2_INTRINSICS
        if (__builtin_cpu_supports("avx2"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_AVX2);
#endif
#if HAVE_BMI_INTRINSICS
        if (__builtin_cpu_supports("bmi"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_BMI);
#endif
#if HAVE_BMI2_INTRINSICS
        if (__builtin_cpu_supports("bmi2"))
            cached = (cpu_feature_level) (cached | CPU_FEATURE_BMI2);
#endif
#endif
        initialized = 1;
    }

    return cached;
}

typedef struct {
    Bool copy_image_initialized;
    Bool copy_image_supported;
} glamor_copy_screen_cache;

static glamor_copy_screen_cache g_screen_cache[GLAMOR_COPY_MAX_SCREENS];

typedef struct {
    GLuint vbo;
    size_t capacity;
    size_t offset;
    int initialized;
} glamor_vbo_per_screen;

static glamor_vbo_per_screen g_vbo_rings[GLAMOR_COPY_MAX_SCREENS];

static int
get_screen_index(ScreenPtr screen)
{
    int idx;

    if (!screen)
        return -1;

    idx = screen->myNum;
    if (idx < 0 || idx >= GLAMOR_COPY_MAX_SCREENS)
        return -1;

    return idx;
}

static Bool
glamor_copy_image_supported_for_screen(ScreenPtr screen)
{
    int idx = get_screen_index(screen);
    glamor_copy_screen_cache *cache;

    if (idx < 0)
        return FALSE;

    cache = &g_screen_cache[idx];
    if (!cache->copy_image_initialized) {
        cache->copy_image_supported =
            epoxy_has_gl_extension("GL_ARB_copy_image") ? TRUE : FALSE;
        cache->copy_image_initialized = TRUE;
    }

    return cache->copy_image_supported;
}

static Bool
glamor_formats_compatible_for_copy(GLenum src_format, GLenum dst_format)
{
    /*
     * Conservative production rule: exact internalformat equality only.
     * ARB_copy_image permits view-compatible classes, but exact equality
     * avoids Mesa-devel corner cases and GL_INVALID_OPERATION surprises.
     */
    return src_format == dst_format;
}

static size_t
glamor_copy_align_up(size_t value, size_t alignment)
{
    return (value + alignment - 1u) & ~(alignment - 1u);
}

static void
vbo_ring_init(int screen_index)
{
    glamor_vbo_per_screen *vbo;

    if (screen_index < 0 || screen_index >= GLAMOR_COPY_MAX_SCREENS)
        return;

    vbo = &g_vbo_rings[screen_index];
    memset(vbo, 0, sizeof(*vbo));
    vbo->capacity = GLAMOR_COPY_SCRATCH_VBO_CAPACITY;
    vbo->initialized = 1;
}

static GLshort *
scratch_vbo_alloc_per_screen(int screen_index, size_t bytes,
                             char **out_offset, Bool allow_orphan)
{
    glamor_vbo_per_screen *vbo;
    GLshort *ptr;
    size_t offset;
    GLbitfield access;

    if (screen_index < 0 || screen_index >= GLAMOR_COPY_MAX_SCREENS ||
        !out_offset)
        return NULL;

    if (UNLIKELY(bytes == 0 || bytes > GLAMOR_COPY_SCRATCH_VBO_CAPACITY))
        return NULL;

    vbo = &g_vbo_rings[screen_index];
    if (!vbo->initialized)
        vbo_ring_init(screen_index);

    if (UNLIKELY(!vbo->vbo)) {
        glGenBuffers(1, &vbo->vbo);
        if (!vbo->vbo)
            return NULL;

        glBindBuffer(GL_ARRAY_BUFFER, vbo->vbo);
        glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) vbo->capacity,
                     NULL, GL_STREAM_DRAW);
    }
    else {
        glBindBuffer(GL_ARRAY_BUFFER, vbo->vbo);
    }

    offset = glamor_copy_align_up(vbo->offset, GLAMOR_COPY_VBO_ALIGN);
    if (UNLIKELY(offset + bytes > vbo->capacity)) {
        if (!allow_orphan)
            return NULL;

        glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) vbo->capacity,
                     NULL, GL_STREAM_DRAW);
        offset = 0;
    }

    access = GL_MAP_WRITE_BIT | GL_MAP_UNSYNCHRONIZED_BIT |
             GL_MAP_INVALIDATE_RANGE_BIT;

    ptr = (GLshort *) glMapBufferRange(GL_ARRAY_BUFFER,
                                       (GLintptr) offset,
                                       (GLsizeiptr) bytes,
                                       access);
    if (UNLIKELY(!ptr))
        return NULL;

    *out_offset = (char *) (uintptr_t) offset;
    vbo->offset = offset + bytes;
    return ptr;
}

static Bool
glamor_unmap_array_buffer(void)
{
    return glUnmapBuffer(GL_ARRAY_BUFFER) == GL_TRUE;
}

static inline void
glamor_generate_box_vertices_scalar(GLshort * restrict vbo,
                                    const BoxPtr restrict boxes,
                                    int nbox)
{
    int n = 0;

    for (; n + 1 < nbox; n += 2) {
        const BoxPtr b0 = &boxes[n];
        const BoxPtr b1 = &boxes[n + 1];

        vbo[0]  = b0->x1; vbo[1]  = b0->y1;
        vbo[2]  = b0->x1; vbo[3]  = b0->y2;
        vbo[4]  = b0->x2; vbo[5]  = b0->y2;
        vbo[6]  = b0->x2; vbo[7]  = b0->y1;

        vbo[8]  = b1->x1; vbo[9]  = b1->y1;
        vbo[10] = b1->x1; vbo[11] = b1->y2;
        vbo[12] = b1->x2; vbo[13] = b1->y2;
        vbo[14] = b1->x2; vbo[15] = b1->y1;

        vbo += 16;
    }

    if (n < nbox) {
        const BoxPtr b = &boxes[n];

        vbo[0] = b->x1; vbo[1] = b->y1;
        vbo[2] = b->x1; vbo[3] = b->y2;
        vbo[4] = b->x2; vbo[5] = b->y2;
        vbo[6] = b->x2; vbo[7] = b->y1;
    }
}

#if HAVE_AVX2_INTRINSICS
static inline void
glamor_generate_box_vertices_avx2(GLshort * restrict vbo,
                                  const BoxPtr restrict boxes,
                                  int nbox)
{
    int n = 0;
    const __m256i shuffle =
        _mm256_setr_epi8(
            0, 1, 2, 3, 0, 1, 6, 7,
            4, 5, 6, 7, 4, 5, 2, 3,
            8, 9, 10, 11, 8, 9, 14, 15,
            12, 13, 14, 15, 12, 13, 10, 11);

    for (; n + 1 < nbox; n += 2) {
        __m128i in;
        __m256i dup;
        __m256i out;

        memcpy(&in, &boxes[n], sizeof(in));
        dup = _mm256_broadcastsi128_si256(in);
        out = _mm256_shuffle_epi8(dup, shuffle);
        memcpy(vbo, &out, sizeof(out));
        vbo += 16;
    }

    if (n < nbox)
        glamor_generate_box_vertices_scalar(vbo, &boxes[n], 1);
}
#endif

static inline void
glamor_generate_box_vertices_batched(GLshort * restrict vbo,
                                     const BoxPtr restrict boxes,
                                     int nbox)
{
#if HAVE_AVX2_INTRINSICS
    if (LIKELY((get_cpu_features() & CPU_FEATURE_AVX2) != 0) && nbox >= 2) {
        glamor_generate_box_vertices_avx2(vbo, boxes, nbox);
        return;
    }
#endif

    glamor_generate_box_vertices_scalar(vbo, boxes, nbox);
}

static Bool
glamor_copy_image_box_valid(const BoxRec *box, int dx, int dy,
                            int src_off_x, int src_off_y,
                            int dst_off_x, int dst_off_y,
                            int sp_w, int sp_h, int dp_w, int dp_h)
{
    int w, h;
    int s_x, s_y;
    int d_x, d_y;

    if (!box)
        return FALSE;

    w = box->x2 - box->x1;
    h = box->y2 - box->y1;
    if (w <= 0 || h <= 0)
        return FALSE;

    s_x = box->x1 + dx + src_off_x;
    s_y = box->y1 + dy + src_off_y;
    d_x = box->x1 + dst_off_x;
    d_y = box->y1 + dst_off_y;

    return s_x >= 0 && s_y >= 0 && d_x >= 0 && d_y >= 0 &&
           w <= sp_w && h <= sp_h && w <= dp_w && h <= dp_h &&
           s_x <= sp_w - w && s_y <= sp_h - h &&
           d_x <= dp_w - w && d_y <= dp_h - h;
}

static void
glamor_issue_copy_image_box(const BoxRec *box,
                            glamor_pixmap_private *spr,
                            glamor_pixmap_private *dpr,
                            int dx, int dy,
                            int src_off_x, int src_off_y,
                            int dst_off_x, int dst_off_y,
                            int sp_h, int dp_h)
{
    int w = box->x2 - box->x1;
    int h = box->y2 - box->y1;
    int s_x = box->x1 + dx + src_off_x;
    int s_y = box->y1 + dy + src_off_y;
    int d_x = box->x1 + dst_off_x;
    int d_y = box->y1 + dst_off_y;

    glCopyImageSubData(spr->fbo->tex, GL_TEXTURE_2D, 0,
                       s_x, sp_h - (s_y + h), 0,
                       dpr->fbo->tex, GL_TEXTURE_2D, 0,
                       d_x, dp_h - (d_y + h), 0,
                       w, h, 1);
}

static Bool
glamor_copy_fbo_fbo_direct(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                           BoxPtr box, int nbox, int dx, int dy,
                           Pixel bitplane)
{
    ScreenPtr screen;
    PixmapPtr spix;
    PixmapPtr dpix;
    glamor_pixmap_private *spr;
    glamor_pixmap_private *dpr;
    glamor_screen_private *priv;
    const struct glamor_format *s_fmt;
    const struct glamor_format *d_fmt;
    int src_off_x = 0, src_off_y = 0;
    int dst_off_x = 0, dst_off_y = 0;
    unsigned long total_pixels = 0;

    if (UNLIKELY(!src || !dst || !box || nbox <= 0 || bitplane ||
                 nbox > GLAMOR_COPY_IMAGE_MAX_BOXES))
        return FALSE;

    if (gc && (gc->alu != GXcopy ||
               !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    screen = dst->pScreen;
    spix = glamor_get_drawable_pixmap(src);
    dpix = glamor_get_drawable_pixmap(dst);
    if (UNLIKELY(!spix || !dpix || spix == dpix))
        return FALSE;

    spr = glamor_get_pixmap_private(spix);
    dpr = glamor_get_pixmap_private(dpix);
    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) ||
                 !GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr) ||
                 !spr->fbo || !dpr->fbo ||
                 spr->fbo->tex == dpr->fbo->tex))
        return FALSE;

    if (!glamor_pixmap_priv_is_small(spr) ||
        !glamor_pixmap_priv_is_small(dpr))
        return FALSE;

    for (int i = 0; i < nbox; i++) {
        int w = box[i].x2 - box[i].x1;
        int h = box[i].y2 - box[i].y1;

        if (UNLIKELY(w <= 0 || h <= 0))
            return FALSE;
        total_pixels += (unsigned long) w * (unsigned long) h;
    }

    if (total_pixels < GLAMOR_COPY_IMAGE_MIN_PIXELS)
        return FALSE;

    if (nbox == 1) {
        int w = box[0].x2 - box[0].x1;
        int h = box[0].y2 - box[0].y1;

        if (w < GLAMOR_COPY_IMAGE_MIN_DIM || h < GLAMOR_COPY_IMAGE_MIN_DIM)
            return FALSE;
    }

    priv = glamor_get_screen_private(screen);
    if (UNLIKELY(!priv))
        return FALSE;

    glamor_make_current(priv);

    if (!glamor_copy_image_supported_for_screen(screen))
        return FALSE;

    s_fmt = glamor_format_for_pixmap(spix);
    d_fmt = glamor_format_for_pixmap(dpix);
    if (UNLIKELY(!s_fmt || !d_fmt ||
                 !glamor_formats_compatible_for_copy(s_fmt->internalformat,
                                                     d_fmt->internalformat)))
        return FALSE;

    glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

    for (int i = 0; i < nbox; i++) {
        if (!glamor_copy_image_box_valid(&box[i], dx, dy,
                                         src_off_x, src_off_y,
                                         dst_off_x, dst_off_y,
                                         spix->drawable.width,
                                         spix->drawable.height,
                                         dpix->drawable.width,
                                         dpix->drawable.height))
            return FALSE;
    }

    for (int i = 0; i < nbox; i++)
        glamor_issue_copy_image_box(&box[i], spr, dpr, dx, dy,
                                    src_off_x, src_off_y,
                                    dst_off_x, dst_off_y,
                                    spix->drawable.height,
                                    dpix->drawable.height);

    return TRUE;
}

static Bool
glamor_copy_needs_temp(DrawablePtr src, DrawablePtr dst,
                       BoxPtr box, int nbox, int dx, int dy)
{
    PixmapPtr src_pixmap;
    PixmapPtr dst_pixmap;
    ScreenPtr screen;
    glamor_screen_private *priv;
    int src_off_x, src_off_y;
    int dst_off_x, dst_off_y;
    BoxRec bounds;

    if (UNLIKELY(!src || !dst || !box || nbox <= 0))
        return FALSE;

    src_pixmap = glamor_get_drawable_pixmap(src);
    dst_pixmap = glamor_get_drawable_pixmap(dst);
    if (!src_pixmap || !dst_pixmap || src_pixmap != dst_pixmap)
        return FALSE;

    screen = dst->pScreen;
    priv = glamor_get_screen_private(screen);
    if (priv && priv->has_nv_texture_barrier)
        return FALSE;

    glamor_get_drawable_deltas(src, src_pixmap, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_off_x, &dst_off_y);

    bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    return bounds.x1 + dst_off_x      < bounds.x2 + dx + src_off_x &&
           bounds.x1 + dx + src_off_x < bounds.x2 + dst_off_x &&
           bounds.y1 + dst_off_y      < bounds.y2 + dy + src_off_y &&
           bounds.y1 + dy + src_off_y < bounds.y2 + dst_off_y;
}

struct copy_args {
    DrawablePtr src_drawable;
    glamor_pixmap_fbo *src;
    uint32_t bitplane;
    int dx, dy;
};

static Bool
use_copyarea(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    struct copy_args *args = (struct copy_args *) arg;
    glamor_screen_private *priv = glamor_get_screen_private(drawable->pScreen);

    (void) gc;

    if (UNLIKELY(!priv || !args || !args->src))
        return FALSE;

    glamor_bind_texture(priv, GL_TEXTURE0, args->src, TRUE);
    glUniform2f(prog->fill_offset_uniform,
                (GLfloat) args->dx, (GLfloat) args->dy);
    glUniform2f(prog->fill_size_inv_uniform,
                1.0f / (GLfloat) args->src->width,
                1.0f / (GLfloat) args->src->height);
    return TRUE;
}

static const glamor_facet glamor_facet_copyarea = {
    .name = "copy_area",
    .vs_vars = "in vec2 primitive;\n",
    .vs_exec = (GLAMOR_POS(gl_Position, primitive.xy)
                " fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = " frag_color = texture(sampler, fill_pos);\n",
    .locations = glamor_program_location_fillsamp |
                 glamor_program_location_fillpos,
    .use = use_copyarea,
};

static Bool
use_copyplane(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    struct copy_args *args = (struct copy_args *) arg;
    glamor_pixmap_fbo *src_fbo;
    glamor_screen_private *priv = glamor_get_screen_private(drawable->pScreen);

    if (UNLIKELY(!priv || !args || !args->src || !gc))
        return FALSE;

    src_fbo = args->src;
    glamor_bind_texture(priv, GL_TEXTURE0, src_fbo, TRUE);
    glUniform2f(prog->fill_offset_uniform,
                (GLfloat) args->dx, (GLfloat) args->dy);
    glUniform2f(prog->fill_size_inv_uniform,
                1.0f / (GLfloat) src_fbo->width,
                1.0f / (GLfloat) src_fbo->height);

    glamor_set_color(drawable, gc->fgPixel, prog->fg_uniform);
    glamor_set_color(drawable, gc->bgPixel, prog->bg_uniform);

    switch (glamor_drawable_effective_depth(args->src_drawable)) {
    case 30:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 20) & 0x3ff,
                     (args->bitplane >> 10) & 0x3ff,
                     args->bitplane & 0x3ff, 0);
        glUniform4f(prog->bitmul_uniform,
                    1023.0f, 1023.0f, 1023.0f, 0.0f);
        break;
    case 24:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 16) & 0xff,
                     (args->bitplane >> 8) & 0xff,
                     args->bitplane & 0xff, 0);
        glUniform4f(prog->bitmul_uniform,
                    255.0f, 255.0f, 255.0f, 0.0f);
        break;
    case 32:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 16) & 0xff,
                     (args->bitplane >> 8) & 0xff,
                     args->bitplane & 0xff,
                     (args->bitplane >> 24) & 0xff);
        glUniform4f(prog->bitmul_uniform,
                    255.0f, 255.0f, 255.0f, 255.0f);
        break;
    case 16:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 11) & 0x1f,
                     (args->bitplane >> 5) & 0x3f,
                     args->bitplane & 0x1f, 0);
        glUniform4f(prog->bitmul_uniform,
                    31.0f, 63.0f, 31.0f, 0.0f);
        break;
    case 15:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 10) & 0x1f,
                     (args->bitplane >> 5) & 0x1f,
                     args->bitplane & 0x1f, 0);
        glUniform4f(prog->bitmul_uniform,
                    31.0f, 31.0f, 31.0f, 0.0f);
        break;
    case 8:
    case 1:
        glUniform4ui(prog->bitplane_uniform, 0, 0, 0, args->bitplane);
        glUniform4f(prog->bitmul_uniform, 0.0f, 0.0f, 0.0f, 255.0f);
        break;
    default:
        return FALSE;
    }

    return TRUE;
}

static const glamor_facet glamor_facet_copyplane = {
    .name = "copy_plane",
    .version = 130,
    .vs_vars = "in vec2 primitive;\n",
    .vs_exec = (GLAMOR_POS(gl_Position, primitive.xy)
                " fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = (" uvec4 bits = uvec4(round(texture(sampler, fill_pos) * bitmul));\n"
                " if ((bits & bitplane) != uvec4(0,0,0,0))\n"
                " frag_color = fg;\n"
                " else\n"
                " frag_color = bg;\n"),
    .locations = glamor_program_location_fillsamp |
                 glamor_program_location_fillpos |
                 glamor_program_location_fg |
                 glamor_program_location_bg |
                 glamor_program_location_bitplane,
    .use = use_copyplane,
};

static Bool
glamor_copy_fbo_fbo_draw(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                         BoxPtr box, int nbox, int dx, int dy,
                         Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    ScreenPtr screen;
    glamor_screen_private *priv;
    PixmapPtr spix;
    PixmapPtr dpix;
    glamor_pixmap_private *spr;
    glamor_pixmap_private *dpr;
    glamor_program *prog;
    struct copy_args args;
    int screen_index;
    size_t vbytes;
    char *vbo_offset = NULL;
    GLshort *vbuf;
    int src_off_x = 0, src_off_y = 0;
    int dst_off_x = 0, dst_off_y = 0;
    Bool mesa_tile_enabled = FALSE;
    int damage_x1, damage_y1, damage_x2, damage_y2;
    BoxRec stack_boxes[GLAMOR_COPY_STACK_BOXES];
    BoxPtr filtered_boxes = stack_boxes;
    Bool allocated = FALSE;
    int src_tile, dst_tile;

    (void) reverse;
    (void) upsidedown;
    (void) closure;

    if (UNLIKELY(!src || !dst || !box || nbox <= 0))
        return FALSE;

    screen = dst->pScreen;
    priv = glamor_get_screen_private(screen);
    if (UNLIKELY(!priv))
        return FALSE;

    if (glamor_copy_fbo_fbo_direct(src, dst, gc, box, nbox,
                                   dx, dy, bitplane))
        return TRUE;

    spix = glamor_get_drawable_pixmap(src);
    dpix = glamor_get_drawable_pixmap(dst);
    if (UNLIKELY(!spix || !dpix))
        return FALSE;

    spr = glamor_get_pixmap_private(spix);
    dpr = glamor_get_pixmap_private(dpix);
    if (UNLIKELY(!GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) ||
                 !GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr)))
        return FALSE;

    glamor_make_current(priv);

    if (gc && !glamor_set_planemask(gc->depth, gc->planemask))
        return FALSE;

    if (!glamor_set_alu(dst, gc ? gc->alu : GXcopy))
        return FALSE;

    if (bitplane && !priv->can_copyplane)
        return FALSE;

    prog = bitplane ? &priv->copy_plane_prog : &priv->copy_area_prog;
    if (UNLIKELY(prog->failed))
        return FALSE;

    if (!prog->prog &&
        !glamor_build_program(screen, prog,
                              bitplane ? &glamor_facet_copyplane :
                                         &glamor_facet_copyarea,
                              NULL, NULL, NULL))
        return FALSE;

    memset(&args, 0, sizeof(args));
    args.src_drawable = src;
    args.bitplane = (uint32_t) bitplane;

    screen_index = get_screen_index(screen);
    if (UNLIKELY(screen_index < 0))
        return FALSE;

    if (UNLIKELY((size_t) nbox > SIZE_MAX / 16u))
        return FALSE;

    vbytes = (size_t) nbox * 16u;
    vbuf = scratch_vbo_alloc_per_screen(screen_index, vbytes,
                                        &vbo_offset, TRUE);
    if (UNLIKELY(!vbuf)) {
        glBindBuffer(GL_ARRAY_BUFFER, priv->vbo);
        return FALSE;
    }

    glamor_generate_box_vertices_batched(vbuf, box, nbox);
    if (UNLIKELY(!glamor_unmap_array_buffer())) {
        glBindBuffer(GL_ARRAY_BUFFER, priv->vbo);
        return FALSE;
    }

    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE,
                          2 * sizeof(GLshort), vbo_offset);

    glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

    damage_x1 = box[0].x1 + dst_off_x;
    damage_y1 = box[0].y1 + dst_off_y;
    damage_x2 = box[0].x2 + dst_off_x;
    damage_y2 = box[0].y2 + dst_off_y;
    for (int i = 1; i < nbox; i++) {
        damage_x1 = LOCAL_MIN(damage_x1, box[i].x1 + dst_off_x);
        damage_y1 = LOCAL_MIN(damage_y1, box[i].y1 + dst_off_y);
        damage_x2 = LOCAL_MAX(damage_x2, box[i].x2 + dst_off_x);
        damage_y2 = LOCAL_MAX(damage_y2, box[i].y2 + dst_off_y);
    }

    glEnable(GL_SCISSOR_TEST);

    if (spix == dpix && bitplane == 0 && priv->has_mesa_tile_raster_order &&
        glamor_pixmap_priv_is_small(spr)) {
        glEnable(GL_TILE_RASTER_ORDER_FIXED_MESA);

        if (dx >= 0)
            glEnable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);
        else
            glDisable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);

        if (dy >= 0)
            glEnable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);
        else
            glDisable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);

        mesa_tile_enabled = TRUE;
    }

    if (UNLIKELY(nbox > GLAMOR_COPY_STACK_BOXES)) {
        filtered_boxes = malloc((size_t) nbox * sizeof(BoxRec));
        if (filtered_boxes)
            allocated = TRUE;
        else
            filtered_boxes = NULL;
    }

    glamor_pixmap_loop(spr, src_tile) {
        BoxPtr sbox = glamor_pixmap_box_at(spr, src_tile);

        args.dx = dx + src_off_x - sbox->x1;
        args.dy = dy + src_off_y - sbox->y1;
        args.src = glamor_pixmap_fbo_at(spr, src_tile);

        if (!glamor_use_program(dst, gc, prog, &args))
            continue;

        glamor_pixmap_loop(dpr, dst_tile) {
            BoxPtr dbox = glamor_pixmap_box_at(dpr, dst_tile);
            int d_off_x = dst_off_x;
            int d_off_y = dst_off_y;
            BoxRec scissor;

            if (glamor_pixmap_priv_is_large(dpr) &&
                (damage_x1 >= dbox->x2 || damage_x2 <= dbox->x1 ||
                 damage_y1 >= dbox->y2 || damage_y2 <= dbox->y1))
                continue;

            if (!glamor_set_destination_drawable(dst, dst_tile, FALSE, FALSE,
                                                 prog->matrix_uniform,
                                                 &d_off_x, &d_off_y))
                continue;

            scissor.x1 = (int16_t) LOCAL_MAX(-args.dx, dbox->x1);
            scissor.y1 = (int16_t) LOCAL_MAX(-args.dy, dbox->y1);
            scissor.x2 = (int16_t) LOCAL_MIN(-args.dx + sbox->x2 - sbox->x1,
                                             dbox->x2);
            scissor.y2 = (int16_t) LOCAL_MIN(-args.dy + sbox->y2 - sbox->y1,
                                             dbox->y2);

            if (scissor.x1 >= scissor.x2 || scissor.y1 >= scissor.y2)
                continue;

            glScissor(scissor.x1 + d_off_x,
                      scissor.y1 + d_off_y,
                      scissor.x2 - scissor.x1,
                      scissor.y2 - scissor.y1);

            if (glamor_pixmap_priv_is_large(dpr) && filtered_boxes &&
                nbox > 8) {
                int count = 0;

                for (int i = 0; i < nbox; i++) {
                    int bx1 = box[i].x1 + dst_off_x;
                    int by1 = box[i].y1 + dst_off_y;
                    int bx2 = box[i].x2 + dst_off_x;
                    int by2 = box[i].y2 + dst_off_y;

                    if (bx1 < dbox->x2 && bx2 > dbox->x1 &&
                        by1 < dbox->y2 && by2 > dbox->y1)
                        filtered_boxes[count++] = box[i];
                }

                if (count > 0) {
                    if (count < nbox) {
                        char *filtered_offset = NULL;
                        GLshort *filtered_vbo;

                        filtered_vbo = scratch_vbo_alloc_per_screen(screen_index,
                                                                    (size_t) count * 16u,
                                                                    &filtered_offset,
                                                                    FALSE);
                        if (filtered_vbo) {
                            glamor_generate_box_vertices_batched(filtered_vbo,
                                                                 filtered_boxes,
                                                                 count);
                            if (LIKELY(glamor_unmap_array_buffer())) {
                                glVertexAttribPointer(GLAMOR_VERTEX_POS, 2,
                                                      GL_SHORT, GL_FALSE,
                                                      2 * sizeof(GLshort),
                                                      filtered_offset);
                                glamor_glDrawArrays_GL_QUADS(priv,
                                                             (unsigned) count);
                                glVertexAttribPointer(GLAMOR_VERTEX_POS, 2,
                                                      GL_SHORT, GL_FALSE,
                                                      2 * sizeof(GLshort),
                                                      vbo_offset);
                            }
                            else {
                                glBindBuffer(GL_ARRAY_BUFFER, g_vbo_rings[screen_index].vbo);
                                glVertexAttribPointer(GLAMOR_VERTEX_POS, 2,
                                                      GL_SHORT, GL_FALSE,
                                                      2 * sizeof(GLshort),
                                                      vbo_offset);
                                glamor_glDrawArrays_GL_QUADS(priv,
                                                             (unsigned) nbox);
                            }
                        }
                        else {
                            glamor_glDrawArrays_GL_QUADS(priv,
                                                         (unsigned) nbox);
                        }
                    }
                    else {
                        glamor_glDrawArrays_GL_QUADS(priv, (unsigned) nbox);
                    }
                }
            }
            else {
                glamor_glDrawArrays_GL_QUADS(priv, (unsigned) nbox);
            }
        }
    }

    if (allocated)
        free(filtered_boxes);

    if (mesa_tile_enabled) {
        glDisable(GL_TILE_RASTER_ORDER_FIXED_MESA);
        glDisable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);
        glDisable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);
    }

    glDisable(GL_SCISSOR_TEST);
    glDisableVertexAttribArray(GLAMOR_VERTEX_POS);
    glBindBuffer(GL_ARRAY_BUFFER, priv->vbo);

    return TRUE;
}

static Bool
glamor_copy_fbo_fbo_temp(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                         BoxPtr box, int nbox, int dx, int dy,
                         Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    ScreenPtr screen;
    BoxRec bounds;
    PixmapPtr tmp_pixmap;
    BoxRec stack_boxes[GLAMOR_COPY_TEMP_STACK_BOXES];
    BoxPtr tmp_box = stack_boxes;
    Bool allocated = FALSE;
    Bool ok;

    (void) reverse;
    (void) upsidedown;

    if (nbox <= 0)
        return TRUE;

    screen = dst->pScreen;

    bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    if (UNLIKELY(bounds.x2 <= bounds.x1 || bounds.y2 <= bounds.y1))
        return TRUE;

    tmp_pixmap = glamor_create_pixmap(screen,
                                      bounds.x2 - bounds.x1,
                                      bounds.y2 - bounds.y1,
                                      glamor_drawable_effective_depth(src),
                                      0);
    if (UNLIKELY(!tmp_pixmap))
        return FALSE;

    if (UNLIKELY(nbox > GLAMOR_COPY_TEMP_STACK_BOXES)) {
        tmp_box = malloc((size_t) nbox * sizeof(BoxRec));
        if (!tmp_box) {
            glamor_destroy_pixmap(tmp_pixmap);
            return FALSE;
        }
        allocated = TRUE;
    }

    for (int n = 0; n < nbox; n++) {
        tmp_box[n].x1 = box[n].x1 - bounds.x1;
        tmp_box[n].y1 = box[n].y1 - bounds.y1;
        tmp_box[n].x2 = box[n].x2 - bounds.x1;
        tmp_box[n].y2 = box[n].y2 - bounds.y1;
    }

    ok = glamor_copy_fbo_fbo_draw(src, &tmp_pixmap->drawable, NULL,
                                  tmp_box, nbox,
                                  dx + bounds.x1, dy + bounds.y1,
                                  FALSE, FALSE, 0, NULL);

    if (LIKELY(ok)) {
        ok = glamor_copy_fbo_fbo_draw(&tmp_pixmap->drawable, dst, gc,
                                      box, nbox,
                                      -bounds.x1, -bounds.y1,
                                      FALSE, FALSE, bitplane, closure);
    }

    if (allocated)
        free(tmp_box);

    glamor_destroy_pixmap(tmp_pixmap);
    return ok;
}

static void COLD
glamor_copy_bail(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                 int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                 Pixel bitplane, void *closure)
{
    if (glamor_prepare_access(dst, GLAMOR_ACCESS_RW) &&
        glamor_prepare_access(src, GLAMOR_ACCESS_RO)) {
        if (bitplane) {
            if (src->bitsPerPixel > 1)
                fbCopyNto1(src, dst, gc, box, nbox, dx, dy,
                           reverse, upsidedown, bitplane, closure);
            else
                fbCopy1toN(src, dst, gc, box, nbox, dx, dy,
                           reverse, upsidedown, bitplane, closure);
        }
        else {
            fbCopyNtoN(src, dst, gc, box, nbox, dx, dy,
                       reverse, upsidedown, bitplane, closure);
        }
    }

    glamor_finish_access(dst);
    glamor_finish_access(src);
}

static Bool
glamor_copy_cpu_fbo(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                    int nbox, int dx, int dy, Bool reverse,
                    Bool upsidedown, Pixel bitplane, void *closure)
{
    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);
    int dst_xoff, dst_yoff;

    if (gc && (gc->alu != GXcopy ||
               !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    if (UNLIKELY(!glamor_priv || !dst_pixmap))
        return FALSE;

    glamor_make_current(glamor_priv);

    if (!glamor_prepare_access(src, GLAMOR_ACCESS_RO))
        return FALSE;

    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_xoff, &dst_yoff);

    if (bitplane) {
        PixmapPtr tmp_pix;
        FbBits *tmp_bits;
        FbStride tmp_stride;
        int tmp_bpp, tmp_xoff, tmp_yoff;

        tmp_pix = fbCreatePixmap(screen,
                                 dst_pixmap->drawable.width,
                                 dst_pixmap->drawable.height,
                                 glamor_drawable_effective_depth(dst), 0);
        if (UNLIKELY(!tmp_pix)) {
            glamor_finish_access(src);
            return FALSE;
        }

        tmp_pix->drawable.x = dst_xoff;
        tmp_pix->drawable.y = dst_yoff;

        fbGetDrawable(&tmp_pix->drawable, tmp_bits, tmp_stride,
                      tmp_bpp, tmp_xoff, tmp_yoff);
        (void) tmp_bpp;

        if (src->bitsPerPixel > 1)
            fbCopyNto1(src, &tmp_pix->drawable, gc, box, nbox,
                       dx, dy, reverse, upsidedown, bitplane, closure);
        else
            fbCopy1toN(src, &tmp_pix->drawable, gc, box, nbox,
                       dx, dy, reverse, upsidedown, bitplane, closure);

        glamor_upload_boxes(dst, box, nbox,
                            tmp_xoff, tmp_yoff,
                            dst_xoff, dst_yoff,
                            (uint8_t *) tmp_bits,
                            tmp_stride * sizeof(FbBits));

        fbDestroyPixmap(tmp_pix);
    }
    else {
        FbBits *src_bits;
        FbStride src_stride;
        int src_bpp, src_xoff, src_yoff;

        fbGetDrawable(src, src_bits, src_stride,
                      src_bpp, src_xoff, src_yoff);
        (void) src_bpp;

        glamor_upload_boxes(dst, box, nbox,
                            src_xoff + dx, src_yoff + dy,
                            dst_xoff, dst_yoff,
                            (uint8_t *) src_bits,
                            src_stride * sizeof(FbBits));
    }

    glamor_finish_access(src);
    return TRUE;
}

static Bool
glamor_copy_fbo_cpu(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                    int nbox, int dx, int dy, Bool reverse,
                    Bool upsidedown, Pixel bitplane, void *closure)
{
    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);
    int src_xoff, src_yoff;
    FbBits *dst_bits;
    FbStride dst_stride;
    int dst_bpp, dst_xoff, dst_yoff;

    (void) reverse;
    (void) upsidedown;
    (void) bitplane;
    (void) closure;

    if (gc && (gc->alu != GXcopy ||
               !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    if (UNLIKELY(!glamor_priv || !src_pixmap))
        return FALSE;

    glamor_make_current(glamor_priv);

    if (!glamor_prepare_access(dst, GLAMOR_ACCESS_RW))
        return FALSE;

    glamor_get_drawable_deltas(src, src_pixmap, &src_xoff, &src_yoff);

    fbGetDrawable(dst, dst_bits, dst_stride, dst_bpp, dst_xoff, dst_yoff);
    (void) dst_bpp;

    glamor_download_boxes(src, box, nbox,
                          src_xoff + dx, src_yoff + dy,
                          dst_xoff, dst_yoff,
                          (uint8_t *) dst_bits,
                          dst_stride * sizeof(FbBits));

    glamor_finish_access(dst);
    return TRUE;
}

void
glamor_copy(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
            int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
            Pixel bitplane, void *closure)
{
    PixmapPtr sp;
    PixmapPtr dp;
    glamor_pixmap_private *spr;
    glamor_pixmap_private *dpr;

    if (UNLIKELY(!src || !dst || !box || nbox <= 0))
        return;

    sp = glamor_get_drawable_pixmap(src);
    dp = glamor_get_drawable_pixmap(dst);
    if (UNLIKELY(!sp || !dp))
        return;

    spr = glamor_get_pixmap_private(sp);
    dpr = glamor_get_pixmap_private(dp);

    if (GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr) &&
        GLAMOR_PIXMAP_PRIV_HAS_FBO(spr)) {
        if (glamor_copy_needs_temp(src, dst, box, nbox, dx, dy)) {
            if (glamor_copy_fbo_fbo_temp(src, dst, gc, box, nbox,
                                         dx, dy, reverse, upsidedown,
                                         bitplane, closure))
                return;
        }
        else {
            if (glamor_copy_fbo_fbo_draw(src, dst, gc, box, nbox,
                                         dx, dy, reverse, upsidedown,
                                         bitplane, closure))
                return;
        }
    }
    else if (GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr)) {
        if (glamor_copy_cpu_fbo(src, dst, gc, box, nbox,
                                dx, dy, reverse, upsidedown,
                                bitplane, closure))
            return;
    }
    else if (GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) &&
             dpr && dpr->type != GLAMOR_DRM_ONLY && bitplane == 0) {
        if (glamor_copy_fbo_cpu(src, dst, gc, box, nbox,
                                dx, dy, reverse, upsidedown,
                                bitplane, closure))
            return;
    }

    glamor_copy_bail(src, dst, gc, box, nbox, dx, dy,
                     reverse, upsidedown, bitplane, closure);
}

RegionPtr
glamor_copy_area(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                 int srcx, int srcy, int width, int height,
                 int dstx, int dsty)
{
    if (UNLIKELY(!src || !dst || !gc))
        return NULL;

    return miDoCopy(src, dst, gc,
                    srcx, srcy, width, height,
                    dstx, dsty, glamor_copy, 0, NULL);
}

RegionPtr
glamor_copy_plane(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                  int srcx, int srcy, int width, int height,
                  int dstx, int dsty, unsigned long bitplane)
{
    if (UNLIKELY(!src || !dst || !gc))
        return NULL;

    if ((bitplane & FbFullMask(glamor_drawable_effective_depth(src))) == 0)
        return miHandleExposures(src, dst, gc,
                                 srcx, srcy, width, height,
                                 dstx, dsty);

    return miDoCopy(src, dst, gc,
                    srcx, srcy, width, height,
                    dstx, dsty, glamor_copy, bitplane, NULL);
}

void
glamor_copy_window(WindowPtr window, DDXPointRec old_origin,
                   RegionPtr src_region)
{
    PixmapPtr pixmap;
    int dx, dy;
    RegionRec dst_region;

    if (UNLIKELY(!window || !src_region))
        return;

    pixmap = glamor_get_drawable_pixmap(&window->drawable);
    if (UNLIKELY(!pixmap))
        return;

    dx = old_origin.x - window->drawable.x;
    dy = old_origin.y - window->drawable.y;

    RegionTranslate(src_region, -dx, -dy);

    RegionNull(&dst_region);
    RegionIntersect(&dst_region, &window->borderClip, src_region);

    if (pixmap->screen_x || pixmap->screen_y)
        RegionTranslate(&dst_region, -pixmap->screen_x, -pixmap->screen_y);

    miCopyRegion(&pixmap->drawable, &pixmap->drawable, NULL,
                 &dst_region, dx, dy, glamor_copy, 0, NULL);

    RegionUninit(&dst_region);
}
