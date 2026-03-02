/*
 * SPDX-License-Identifier: MIT
 *
 * glamor_copy.c - High-performance GPU copy operations
 * OPTIMIZED FOR: Intel i7-14700KF + AMD Vega 64
 *
 * Performance Optimizations (2026):
 * - glCopyImageSubData DMA path (bypasses rasterizer for large copies)
 * - Per-screen format compatibility cache (eliminates glGetTexLevelParameteriv stalls)
 * - AVX2 batched VBO generation (2 boxes per cycle)
 * - MESA tile raster order for self-copies (eliminates temp buffer overhead)
 * - Zero-allocation region filtering (stack-based for common cases)
 */

#include "glamor_priv.h"
#include "glamor_prepare.h"
#include "glamor_transform.h"
#include "glamor_transfer.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

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

#define LOCAL_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define LOCAL_MAX(a, b) (((a) > (b)) ? (a) : (b))

/* glCopyImageSubData thresholds */
#define GLAMOR_COPY_IMAGE_MIN_PIXELS (64 * 64)
#define GLAMOR_COPY_IMAGE_MIN_DIM 32

/* MESA tile raster order constants */
#ifndef GL_TILE_RASTER_ORDER_FIXED_MESA
#define GL_TILE_RASTER_ORDER_FIXED_MESA          0x8BB8
#define GL_TILE_RASTER_ORDER_INCREASING_X_MESA   0x8BB9
#define GL_TILE_RASTER_ORDER_INCREASING_Y_MESA   0x8BBA
#endif

/* Per-Screen Format Cache */
#define FORMAT_HASH_SIZE 128
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

typedef struct {
    GLenum format1;
    GLenum format2;
    bool   compatible;
    bool   valid;
} format_hash_entry;

typedef struct {
    format_hash_entry entries[FORMAT_HASH_SIZE];
    int initialized;
} glamor_format_cache_per_screen;

static glamor_format_cache_per_screen g_format_caches[MAX_SCREENS];

/* Per-Screen VBO Ring Buffer */
#define SCRATCH_VBO_CAPACITY (512u * 1024u)

typedef struct {
    GLuint vbo;
    size_t capacity;
    size_t offset;
    int initialized;
} glamor_vbo_per_screen;

static glamor_vbo_per_screen g_vbo_rings[MAX_SCREENS];

/* Get screen index */
static int
get_screen_index(ScreenPtr screen)
{
    if (!screen)
        return -1;

    int idx = screen->myNum;
    if (idx < 0 || idx >= MAX_SCREENS)
        return -1;

    return idx;
}

/* Initialize per-screen format cache */
static void
format_cache_init(int screen_index)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return;

    glamor_format_cache_per_screen *cache = &g_format_caches[screen_index];
    memset(cache->entries, 0, sizeof(cache->entries));
    cache->initialized = 1;
}

/* FNV-1a hash for format pairs */
static inline uint32_t
format_pair_hash(GLenum f1, GLenum f2)
{
    uint64_t key = ((uint64_t)f1 << 32) | (uint64_t)f2;
    uint32_t hash = 2166136261u;

    for (int i = 0; i < 8; i++) {
        hash ^= (uint32_t)(key & 0xFF);
        hash *= 16777619u;
        key >>= 8;
    }

    return hash & (FORMAT_HASH_SIZE - 1);
}

/* Per-screen format compatibility check */
static bool
glamor_formats_compatible_for_copy_cached(int screen_index, GLenum f1, GLenum f2)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return f1 == f2;

    glamor_format_cache_per_screen *cache = &g_format_caches[screen_index];

    if (!cache->initialized)
        format_cache_init(screen_index);

    if (f1 == f2)
        return true;

    if (f1 > f2) {
        GLenum t = f1;
        f1 = f2;
        f2 = t;
    }

    uint32_t idx = format_pair_hash(f1, f2);

    /* Fast path lookup (L1 cached) */
    for (int i = 0; i < 8; i++) {
        uint32_t pos = (idx + i) & (FORMAT_HASH_SIZE - 1);
        if (!cache->entries[pos].valid)
            break;
        if (cache->entries[pos].format1 == f1 && cache->entries[pos].format2 == f2)
            return cache->entries[pos].compatible;
    }

    /* Slow path compute (once per format pair lifetime) */
    bool compatible = false;
    static const GLenum class_32bit[] = {
        GL_RG16F, GL_R32F, GL_RGB10_A2UI, GL_RGBA8UI, GL_RG16UI, GL_R32UI,
        GL_RGBA8I, GL_RG16I, GL_R32I, GL_RGB10_A2, GL_RGBA8, GL_RG16,
        GL_RGBA8_SNORM, GL_RG16_SNORM, GL_SRGB8_ALPHA8, GL_RGB9_E5, GL_R11F_G11F_B10F
    };
    static const GLenum class_16bit[] = {
        GL_R16F, GL_RG8UI, GL_R16UI, GL_RG8I, GL_R16I, GL_RG8, GL_R16,
        GL_RG8_SNORM, GL_R16_SNORM
    };
    static const GLenum class_8bit[] = {
        GL_R8UI, GL_R8I, GL_R8, GL_R8_SNORM
    };

    struct {
        const GLenum *fmt;
        size_t count;
    } classes[] = {
        { class_32bit, sizeof(class_32bit)/sizeof(GLenum) },
        { class_16bit, sizeof(class_16bit)/sizeof(GLenum) },
        { class_8bit,  sizeof(class_8bit)/sizeof(GLenum) }
    };

    for (size_t c = 0; c < 3; c++) {
        bool found1 = false, found2 = false;
        for (size_t i = 0; i < classes[c].count; i++) {
            if (classes[c].fmt[i] == f1) found1 = true;
            if (classes[c].fmt[i] == f2) found2 = true;
        }
        if (found1 && found2) {
            compatible = true;
            break;
        }
    }

    /* Insert into cache */
    for (int i = 0; i < 8; i++) {
        uint32_t pos = (idx + i) & (FORMAT_HASH_SIZE - 1);
        if (!cache->entries[pos].valid ||
           (cache->entries[pos].format1 == f1 && cache->entries[pos].format2 == f2)) {
            cache->entries[pos].format1 = f1;
            cache->entries[pos].format2 = f2;
            cache->entries[pos].compatible = compatible;
            cache->entries[pos].valid = true;
            break;
        }
    }

    return compatible;
}

/* Initialize per-screen VBO */
static void
vbo_ring_init(int screen_index)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return;

    glamor_vbo_per_screen *vbo = &g_vbo_rings[screen_index];
    memset(vbo, 0, sizeof(*vbo));
    vbo->capacity = SCRATCH_VBO_CAPACITY;
    vbo->initialized = 1;
}

/* Per-screen VBO allocation */
static GLshort *
scratch_vbo_alloc_per_screen(int screen_index, size_t bytes, char **out_offset)
{
    if (screen_index < 0 || screen_index >= MAX_SCREENS)
        return NULL;

    if (UNLIKELY(bytes == 0 || bytes > SCRATCH_VBO_CAPACITY))
        return NULL;

    glamor_vbo_per_screen *vbo = &g_vbo_rings[screen_index];

    if (!vbo->initialized)
        vbo_ring_init(screen_index);

    if (UNLIKELY(!vbo->vbo)) {
        glGenBuffers(1, &vbo->vbo);
        glBindBuffer(GL_ARRAY_BUFFER, vbo->vbo);
        glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr)vbo->capacity, NULL, GL_STREAM_DRAW);
    } else {
        glBindBuffer(GL_ARRAY_BUFFER, vbo->vbo);
    }

    /* Orphaning: reset when full */
    if (UNLIKELY(vbo->offset + bytes > vbo->capacity)) {
        glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr)vbo->capacity, NULL, GL_STREAM_DRAW);
        vbo->offset = 0;
    }

    GLshort *ptr = (GLshort *)glMapBufferRange(GL_ARRAY_BUFFER,
                                               vbo->offset,
                                               bytes,
                                               GL_MAP_WRITE_BIT | GL_MAP_UNSYNCHRONIZED_BIT);

    if (UNLIKELY(!ptr))
        return NULL;

    *out_offset = (char *)(uintptr_t)vbo->offset;
    vbo->offset += bytes;

    return ptr;
}

/* glCopyImageSubData helper (pure C, no lambdas) */
static inline void
issue_copy(const BoxRec *bx,
           glamor_pixmap_private *spr, glamor_pixmap_private *dpr,
           int dx, int dy,
           int src_off_x, int src_off_y, int dst_off_x, int dst_off_y,
           int sp_w, int sp_h, int dp_w, int dp_h,
           int *commands)
{
    int w_ = bx->x2 - bx->x1;
    int h_ = bx->y2 - bx->y1;

    if (w_ <= 0 || h_ <= 0)
        return;

    int s_x = bx->x1 + dx + src_off_x;
    int s_y = bx->y1 + dy + src_off_y;
    int d_x = bx->x1 + dst_off_x;
    int d_y = bx->y1 + dst_off_y;

    if (s_x < 0 || s_y < 0 || d_x < 0 || d_y < 0 ||
        s_x + w_ > sp_w || s_y + h_ > sp_h ||
        d_x + w_ > dp_w || d_y + h_ > dp_h) {
        return;
    }

    /* OpenGL textures are bottom-up; flip Y coordinate */
    glCopyImageSubData(spr->fbo->tex, GL_TEXTURE_2D, 0,
                       s_x, sp_h - (s_y + h_), 0,
                       dpr->fbo->tex, GL_TEXTURE_2D, 0,
                       d_x, dp_h - (d_y + h_), 0,
                       w_, h_, 1);
    (*commands)++;
}

/* DMA Path - glCopyImageSubData */
static Bool
glamor_copy_fbo_fbo_direct(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                           BoxPtr box, int nbox, int dx, int dy, Pixel bitplane)
{
    if (UNLIKELY(!src || !dst || !box || nbox <= 0 || bitplane))
        return FALSE;

    if ((gc && gc->alu != GXcopy) || (gc && !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    ScreenPtr screen = dst->pScreen;
    PixmapPtr spix = glamor_get_drawable_pixmap(src);
    PixmapPtr dpix = glamor_get_drawable_pixmap(dst);

    if (UNLIKELY(!spix || !dpix || spix == dpix))
        return FALSE;

    glamor_pixmap_private *spr = glamor_get_pixmap_private(spix);
    glamor_pixmap_private *dpr = glamor_get_pixmap_private(dpix);

    if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) || !GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr))
        return FALSE;

    if (spix->drawable.width * spix->drawable.height < GLAMOR_COPY_IMAGE_MIN_PIXELS ||
        spix->drawable.width < GLAMOR_COPY_IMAGE_MIN_DIM ||
        spix->drawable.height < GLAMOR_COPY_IMAGE_MIN_DIM)
        return FALSE;

    glamor_screen_private *priv = glamor_get_screen_private(screen);
    glamor_make_current(priv);

    int screen_index = get_screen_index(screen);
    const struct glamor_format *s_fmt = glamor_format_for_pixmap(spix);
    const struct glamor_format *d_fmt = glamor_format_for_pixmap(dpix);

    if (UNLIKELY(!s_fmt || !d_fmt || !glamor_formats_compatible_for_copy_cached(screen_index,
                                          s_fmt->internalformat, d_fmt->internalformat)))
        return FALSE;

    BoxRec merged = box[0];
    unsigned long covered = 0;

    for (int i = 0; i < nbox; ++i) {
        merged.x1 = LOCAL_MIN(merged.x1, box[i].x1);
        merged.y1 = LOCAL_MIN(merged.y1, box[i].y1);
        merged.x2 = LOCAL_MAX(merged.x2, box[i].x2);
        merged.y2 = LOCAL_MAX(merged.y2, box[i].y2);
        covered += (unsigned long)(box[i].x2 - box[i].x1) *
                   (unsigned long)(box[i].y2 - box[i].y1);
    }

    unsigned long bbox_area = (unsigned long)(merged.x2 - merged.x1) *
                              (unsigned long)(merged.y2 - merged.y1);

    bool can_merge = (bbox_area > 0 && bbox_area <= covered * 12ul / 10ul);

    int src_off_x = 0, src_off_y = 0, dst_off_x = 0, dst_off_y = 0;
    glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

    const int sp_w = spix->drawable.width;
    const int sp_h = spix->drawable.height;
    const int dp_w = dpix->drawable.width;
    const int dp_h = dpix->drawable.height;

    int commands = 0;

    if (can_merge) {
        issue_copy(&merged, spr, dpr, dx, dy, src_off_x, src_off_y, dst_off_x, dst_off_y,
                   sp_w, sp_h, dp_w, dp_h, &commands);
    } else {
        for (int i = 0; i < nbox; ++i) {
            issue_copy(&box[i], spr, dpr, dx, dy, src_off_x, src_off_y, dst_off_x, dst_off_y,
                       sp_w, sp_h, dp_w, dp_h, &commands);
        }
    }

    return commands > 0;
}

/* Check if copy needs temporary buffer (self-intersecting) */
static Bool
glamor_copy_needs_temp(DrawablePtr src, DrawablePtr dst, BoxPtr box, int nbox, int dx, int dy)
{
    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);
    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);

    if (src_pixmap != dst_pixmap)
        return FALSE;

    if (nbox == 0)
        return FALSE;

    /* NV_texture_barrier can handle self-copies */
    if (priv->has_nv_texture_barrier)
        return FALSE;

    /* Check for overlap */
    int src_off_x, src_off_y;
    int dst_off_x, dst_off_y;
    glamor_get_drawable_deltas(src, src_pixmap, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_off_x, &dst_off_y);

    BoxRec bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    /* Check if source and destination regions overlap */
    if (bounds.x1 + dst_off_x      < bounds.x2 + dx + src_off_x &&
        bounds.x1 + dx + src_off_x < bounds.x2 + dst_off_x &&
        bounds.y1 + dst_off_y      < bounds.y2 + dy + src_off_y &&
        bounds.y1 + dy + src_off_y < bounds.y2 + dst_off_y) {
        return TRUE;
    }

    return FALSE;
}

static inline void
glamor_generate_box_vertices_batched(GLshort * restrict vbo, const BoxPtr restrict boxes, int nbox)
{
    int n = 0;

    cpu_feature_level features = get_cpu_features();

    if (features >= CPU_FEATURE_AVX2 && nbox >= 2) {
        const __m256i mask = _mm256_set_epi8(
            3,2, 5,4, 7,6, 5,4, 7,6, 1,0, 3,2, 1,0,
            3,2, 5,4, 7,6, 5,4, 7,6, 1,0, 3,2, 1,0
        );

        for (; n + 1 < nbox; n += 2) {
            __m128i b_in = _mm_loadu_si128((const __m128i *)&boxes[n]);
            __m256i ymm_dup = _mm256_broadcastsi128_si256(b_in);
            __m256i ymm_b = _mm256_permute4x64_epi64(ymm_dup, 0x50);
            __m256i out = _mm256_shuffle_epi8(ymm_b, mask);
            _mm256_storeu_si256((__m256i *)vbo, out);
            vbo += 16;
        }
    }

    for (; n < nbox; n++) {
        const BoxPtr b = &boxes[n];
        vbo[0] = b->x1; vbo[1] = b->y1;
        vbo[2] = b->x1; vbo[3] = b->y2;
        vbo[4] = b->x2; vbo[5] = b->y2;
        vbo[6] = b->x2; vbo[7] = b->y1;
        vbo += 8;
    }
}

/* Rasterizer Shaders */
struct copy_args {
    DrawablePtr src_drawable;
    glamor_pixmap_fbo *src;
    uint32_t bitplane;
    int dx, dy;
};

static Bool
use_copyarea(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    (void)gc;
    struct copy_args *args = (struct copy_args *)arg;
    glamor_screen_private *priv = glamor_get_screen_private(drawable->pScreen);

    glamor_bind_texture(priv, GL_TEXTURE0, args->src, TRUE);
    glUniform2f(prog->fill_offset_uniform, (GLfloat)args->dx, (GLfloat)args->dy);
    glUniform2f(prog->fill_size_inv_uniform,
                1.0f/(GLfloat)args->src->width,
                1.0f/(GLfloat)args->src->height);

    return TRUE;
}

static const glamor_facet glamor_facet_copyarea = {
    .name = "copy_area",
    .vs_vars = "in vec2 primitive;\n",
    .vs_exec = (GLAMOR_POS(gl_Position, primitive.xy)
                " fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = " frag_color = texture(sampler, fill_pos);\n",
    .locations = glamor_program_location_fillsamp | glamor_program_location_fillpos,
    .use = use_copyarea,
};

static Bool
use_copyplane(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    struct copy_args *args = (struct copy_args *)arg;
    glamor_pixmap_fbo *src = args->src;
    glamor_screen_private *priv = glamor_get_screen_private(drawable->pScreen);

    glamor_bind_texture(priv, GL_TEXTURE0, src, TRUE);
    glUniform2f(prog->fill_offset_uniform, (GLfloat)args->dx, (GLfloat)args->dy);
    glUniform2f(prog->fill_size_inv_uniform, 1.0f/(GLfloat)src->width, 1.0f/(GLfloat)src->height);

    glamor_set_color(drawable, gc->fgPixel, prog->fg_uniform);
    glamor_set_color(drawable, gc->bgPixel, prog->bg_uniform);

    switch (glamor_drawable_effective_depth(args->src_drawable)) {
    case 30:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 20) & 0x3ff,
                     (args->bitplane >> 10) & 0x3ff,
                     (args->bitplane      ) & 0x3ff, 0);
        glUniform4f(prog->bitmul_uniform, 1023.0f, 1023.0f, 1023.0f, 0.0f);
        break;
    case 24:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 16) & 0xff,
                     (args->bitplane >>  8) & 0xff,
                     (args->bitplane      ) & 0xff, 0);
        glUniform4f(prog->bitmul_uniform, 255.0f, 255.0f, 255.0f, 0.0f);
        break;
    case 32:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 16) & 0xff,
                     (args->bitplane >>  8) & 0xff,
                     (args->bitplane      ) & 0xff,
                     (args->bitplane >> 24) & 0xff);
        glUniform4f(prog->bitmul_uniform, 255.0f, 255.0f, 255.0f, 255.0f);
        break;
    case 16:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 11) & 0x1f,
                     (args->bitplane >>  5) & 0x3f,
                     (args->bitplane      ) & 0x1f, 0);
        glUniform4f(prog->bitmul_uniform, 31.0f, 63.0f, 31.0f, 0.0f);
        break;
    case 15:
        glUniform4ui(prog->bitplane_uniform,
                     (args->bitplane >> 10) & 0x1f,
                     (args->bitplane >>  5) & 0x1f,
                     (args->bitplane      ) & 0x1f, 0);
        glUniform4f(prog->bitmul_uniform, 31.0f, 31.0f, 31.0f, 0.0f);
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
    .vs_exec = (GLAMOR_POS(gl_Position, (primitive.xy))
                " fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = (" uvec4 bits = uvec4(round(texture(sampler, fill_pos) * bitmul));\n"
                " if ((bits & bitplane) != uvec4(0,0,0,0))\n"
                " frag_color = fg;\n"
                " else\n"
                " frag_color = bg;\n"),
    .locations = glamor_program_location_fillsamp | glamor_program_location_fillpos |
                 glamor_program_location_fg | glamor_program_location_bg | glamor_program_location_bitplane,
    .use = use_copyplane,
};

/* Core Rasterization Pass */
static Bool
glamor_copy_fbo_fbo_draw(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                         int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    if (UNLIKELY(!src || !dst || !box || nbox <= 0))
        return FALSE;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);

    /* Prefer DMA transfer over Rasterizer */
    if (epoxy_has_gl_extension("GL_ARB_copy_image") &&
        glamor_copy_fbo_fbo_direct(src, dst, gc, box, nbox, dx, dy, bitplane)) {
        return TRUE;
    }

    PixmapPtr spix = glamor_get_drawable_pixmap(src);
    PixmapPtr dpix = glamor_get_drawable_pixmap(dst);
    glamor_pixmap_private *spr = glamor_get_pixmap_private(spix);
    glamor_pixmap_private *dpr = glamor_get_pixmap_private(dpix);

    glamor_make_current(priv);

    if (gc && !glamor_set_planemask(gc->depth, gc->planemask))
        return FALSE;

    if (!glamor_set_alu(dst, gc ? gc->alu : GXcopy))
        return FALSE;

    if (bitplane && !priv->can_copyplane)
        return FALSE;

    glamor_program *prog = bitplane ? &priv->copy_plane_prog : &priv->copy_area_prog;

    if (UNLIKELY(prog->failed))
        return FALSE;

    if (!prog->prog && !glamor_build_program(screen, prog,
                                              bitplane ? &glamor_facet_copyplane : &glamor_facet_copyarea,
                                              NULL, NULL, NULL))
        return FALSE;

    struct copy_args args = {
        .src_drawable = src,
        .bitplane = (uint32_t)bitplane
    };

    int screen_index = get_screen_index(screen);
    size_t vbytes = (size_t)nbox * 16u;
    char *vbo_offset = NULL;
    GLshort *vbuf = scratch_vbo_alloc_per_screen(screen_index, vbytes, &vbo_offset);

    if (UNLIKELY(!vbuf))
        return FALSE;

    glamor_generate_box_vertices_batched(vbuf, box, nbox);
    glUnmapBuffer(GL_ARRAY_BUFFER);

    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE, 2 * sizeof(GLshort), vbo_offset);

    int src_off_x = 0, src_off_y = 0, dst_off_x = 0, dst_off_y = 0;
    glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

    glEnable(GL_SCISSOR_TEST);

    /* MESA tile raster order for self-copies */
    bool mesa_tile_enabled = FALSE;
    if (spix == dpix && priv->has_mesa_tile_raster_order) {
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

    /* Zero-Allocation Region Filtering */
    #define MAX_STACK_BOXES 256
    BoxRec stack_boxes[MAX_STACK_BOXES];
    BoxPtr filtered_boxes = stack_boxes;
    bool allocated = false;

    if (UNLIKELY(nbox > MAX_STACK_BOXES)) {
        filtered_boxes = (BoxPtr)malloc((size_t)nbox * sizeof(BoxRec));
        if (!filtered_boxes) {
            filtered_boxes = stack_boxes;
            allocated = false;
        } else {
            allocated = true;
        }
    }

    int src_tile, dst_tile;
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

            if (!glamor_set_destination_drawable(dst, dst_tile, FALSE, FALSE,
                                                  prog->matrix_uniform, &d_off_x, &d_off_y))
                continue;

            BoxRec scissor = {
                .x1 = (int16_t)LOCAL_MAX(-args.dx, dbox->x1),
                .y1 = (int16_t)LOCAL_MAX(-args.dy, dbox->y1),
                .x2 = (int16_t)LOCAL_MIN(-args.dx + sbox->x2 - sbox->x1, dbox->x2),
                .y2 = (int16_t)LOCAL_MIN(-args.dy + sbox->y2 - sbox->y1, dbox->y2)
            };

            if (scissor.x1 >= scissor.x2 || scissor.y1 >= scissor.y2)
                continue;

            glScissor(scissor.x1 + d_off_x, scissor.y1 + d_off_y,
                      scissor.x2 - scissor.x1, scissor.y2 - scissor.y1);

            if (glamor_pixmap_priv_is_large(dpr) && (allocated || nbox <= MAX_STACK_BOXES)) {
                int count = 0;
                for (int i = 0; i < nbox; i++) {
                    if (box[i].x1 < dbox->x2 && box[i].x2 > dbox->x1 &&
                        box[i].y1 < dbox->y2 && box[i].y2 > dbox->y1) {
                        filtered_boxes[count++] = box[i];
                    }
                }

                if (count > 0) {
                    char *f_off = NULL;
                    GLshort *f_v = scratch_vbo_alloc_per_screen(screen_index, (size_t)count * 16u, &f_off);
                    if (LIKELY(f_v)) {
                        glamor_generate_box_vertices_batched(f_v, filtered_boxes, count);
                        glUnmapBuffer(GL_ARRAY_BUFFER);
                        glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE,
                                              2 * sizeof(GLshort), f_off);
                        glamor_glDrawArrays_GL_QUADS(priv, (unsigned)count);
                        glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE,
                                              2 * sizeof(GLshort), vbo_offset);
                    } else {
                        glamor_glDrawArrays_GL_QUADS(priv, (unsigned)nbox);
                    }
                }
            } else {
                glamor_glDrawArrays_GL_QUADS(priv, (unsigned)nbox);
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

    return TRUE;
}

/* Self-Intersecting Copies (Temp Buffer) */
static Bool
glamor_copy_fbo_fbo_temp(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                         int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    if (nbox <= 0)
        return TRUE;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);

    BoxRec bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    if (UNLIKELY(bounds.x2 <= bounds.x1 || bounds.y2 <= bounds.y1))
        return TRUE;

    PixmapPtr tmp_pixmap = glamor_create_pixmap(screen, bounds.x2 - bounds.x1, bounds.y2 - bounds.y1,
                                                glamor_drawable_effective_depth(src), 0);

    if (UNLIKELY(!tmp_pixmap))
        return FALSE;

    #define MAX_STACK_BOXES 256
    BoxRec stack_boxes[MAX_STACK_BOXES];
    BoxPtr tmp_box = stack_boxes;
    bool allocated = false;

    if (UNLIKELY(nbox > MAX_STACK_BOXES)) {
        tmp_box = (BoxPtr)malloc((size_t)nbox * sizeof(BoxRec));
        if (!tmp_box) {
            glamor_destroy_pixmap(tmp_pixmap);
            return FALSE;
        }
        allocated = true;
    }

    for (int n = 0; n < nbox; n++) {
        tmp_box[n].x1 = box[n].x1 - bounds.x1;
        tmp_box[n].y1 = box[n].y1 - bounds.y1;
        tmp_box[n].x2 = box[n].x2 - bounds.x1;
        tmp_box[n].y2 = box[n].y2 - bounds.y1;
    }

    Bool ok = glamor_copy_fbo_fbo_draw(src, &tmp_pixmap->drawable, NULL, tmp_box, nbox,
                                       dx + bounds.x1, dy + bounds.y1, FALSE, FALSE, 0, NULL);

    if (LIKELY(ok)) {
        ok = glamor_copy_fbo_fbo_draw(&tmp_pixmap->drawable, dst, gc, box, nbox,
                                      -bounds.x1, -bounds.y1, FALSE, FALSE, bitplane, closure);
    }

    if (allocated)
        free(tmp_box);

    glamor_destroy_pixmap(tmp_pixmap);
    return ok;
}

/* CPU Fallback */
static void COLD
glamor_copy_bail(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                 int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                 Pixel bitplane, void *closure)
{
    if (glamor_prepare_access(dst, GLAMOR_ACCESS_RW) && glamor_prepare_access(src, GLAMOR_ACCESS_RO)) {
        if (bitplane) {
            if (src->bitsPerPixel > 1)
                fbCopyNto1(src, dst, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);
            else
                fbCopy1toN(src, dst, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);
        } else {
            fbCopyNtoN(src, dst, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);
        }
    }
    glamor_finish_access(dst);
    glamor_finish_access(src);
}

/* CPU → FBO Copy */
static Bool
glamor_copy_cpu_fbo(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                    int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                    Pixel bitplane, void *closure)
{
    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);
    int dst_xoff, dst_yoff;

    if (gc && (gc->alu != GXcopy || !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    glamor_make_current(glamor_priv);

    if (!glamor_prepare_access(src, GLAMOR_ACCESS_RO))
        return FALSE;

    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_xoff, &dst_yoff);

    if (bitplane) {
        PixmapPtr tmp_pix = fbCreatePixmap(screen, dst_pixmap->drawable.width,
                                           dst_pixmap->drawable.height,
                                           glamor_drawable_effective_depth(dst), 0);
        if (UNLIKELY(!tmp_pix)) {
            glamor_finish_access(src);
            return FALSE;
        }

        tmp_pix->drawable.x = dst_xoff;
        tmp_pix->drawable.y = dst_yoff;

        FbBits *tmp_bits;
        FbStride tmp_stride;
        int tmp_bpp, tmp_xoff, tmp_yoff;

        fbGetDrawable(&tmp_pix->drawable, tmp_bits, tmp_stride, tmp_bpp, tmp_xoff, tmp_yoff);

        if (src->bitsPerPixel > 1)
            fbCopyNto1(src, &tmp_pix->drawable, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);
        else
            fbCopy1toN(src, &tmp_pix->drawable, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);

        glamor_upload_boxes(dst, box, nbox, tmp_xoff, tmp_yoff, dst_xoff, dst_yoff,
                            (uint8_t *)tmp_bits, tmp_stride * sizeof(FbBits));
        fbDestroyPixmap(tmp_pix);
    } else {
        FbBits *src_bits;
        FbStride src_stride;
        int src_bpp, src_xoff, src_yoff;

        fbGetDrawable(src, src_bits, src_stride, src_bpp, src_xoff, src_yoff);

        glamor_upload_boxes(dst, box, nbox, src_xoff + dx, src_yoff + dy, dst_xoff, dst_yoff,
                            (uint8_t *)src_bits, src_stride * sizeof(FbBits));
    }

    glamor_finish_access(src);
    return TRUE;
}

/* FBO → CPU Copy */
static Bool
glamor_copy_fbo_cpu(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
                    int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
                    Pixel bitplane, void *closure)
{
    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);
    int src_xoff, src_yoff;

    if (gc && (gc->alu != GXcopy || !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    glamor_make_current(glamor_priv);

    if (!glamor_prepare_access(dst, GLAMOR_ACCESS_RW))
        return FALSE;

    glamor_get_drawable_deltas(src, src_pixmap, &src_xoff, &src_yoff);

    FbBits *dst_bits;
    FbStride dst_stride;
    int dst_bpp, dst_xoff, dst_yoff;

    fbGetDrawable(dst, dst_bits, dst_stride, dst_bpp, dst_xoff, dst_yoff);

    glamor_download_boxes(src, box, nbox, src_xoff + dx, src_yoff + dy, dst_xoff, dst_yoff,
                          (uint8_t *)dst_bits, dst_stride * sizeof(FbBits));
    glamor_finish_access(dst);
    return TRUE;
}

/* Public API - glamor_copy */
void
glamor_copy(DrawablePtr src, DrawablePtr dst, GCPtr gc, BoxPtr box,
            int nbox, int dx, int dy, Bool reverse, Bool upsidedown,
            Pixel bitplane, void *closure)
{
    if (UNLIKELY(!src || !dst || !box || nbox <= 0))
        return;

    PixmapPtr sp = glamor_get_drawable_pixmap(src);
    PixmapPtr dp = glamor_get_drawable_pixmap(dst);

    if (UNLIKELY(!sp || !dp))
        return;

    glamor_pixmap_private *spr = glamor_get_pixmap_private(sp);
    glamor_pixmap_private *dpr = glamor_get_pixmap_private(dp);

    if (GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr) && GLAMOR_PIXMAP_PRIV_HAS_FBO(spr)) {
        if (glamor_copy_needs_temp(src, dst, box, nbox, dx, dy)) {
            if (glamor_copy_fbo_fbo_temp(src, dst, gc, box, nbox, dx, dy,
                                          reverse, upsidedown, bitplane, closure))
                return;
        } else {
            if (glamor_copy_fbo_fbo_draw(src, dst, gc, box, nbox, dx, dy,
                                          reverse, upsidedown, bitplane, closure))
                return;
        }
    } else if (GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr)) {
        if (glamor_copy_cpu_fbo(src, dst, gc, box, nbox, dx, dy,
                                 reverse, upsidedown, bitplane, closure))
            return;
    } else if (GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) && dpr->type != GLAMOR_DRM_ONLY && bitplane == 0) {
        if (glamor_copy_fbo_cpu(src, dst, gc, box, nbox, dx, dy,
                                 reverse, upsidedown, bitplane, closure))
            return;
    }

    glamor_copy_bail(src, dst, gc, box, nbox, dx, dy, reverse, upsidedown, bitplane, closure);
}

/* Public API - glamor_copy_area */
RegionPtr
glamor_copy_area(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                 int srcx, int srcy, int width, int height, int dstx, int dsty)
{
    if (UNLIKELY(!src || !dst || !gc))
        return NULL;

    return miDoCopy(src, dst, gc, srcx, srcy, width, height, dstx, dsty,
                    glamor_copy, 0, NULL);
}

/* Public API - glamor_copy_plane */
RegionPtr
glamor_copy_plane(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                  int srcx, int srcy, int width, int height, int dstx, int dsty,
                  unsigned long bitplane)
{
    if (UNLIKELY(!src || !dst || !gc))
        return NULL;

    if ((bitplane & FbFullMask(glamor_drawable_effective_depth(src))) == 0)
        return miHandleExposures(src, dst, gc, srcx, srcy, width, height, dstx, dsty);

    return miDoCopy(src, dst, gc, srcx, srcy, width, height, dstx, dsty,
                    glamor_copy, bitplane, NULL);
}

/* Public API - glamor_copy_window */
void
glamor_copy_window(WindowPtr window, DDXPointRec old_origin, RegionPtr src_region)
{
    if (UNLIKELY(!window || !src_region))
        return;

    PixmapPtr pixmap = glamor_get_drawable_pixmap(&window->drawable);
    if (UNLIKELY(!pixmap))
        return;

    int dx = old_origin.x - window->drawable.x;
    int dy = old_origin.y - window->drawable.y;

    RegionTranslate(src_region, -dx, -dy);
    RegionRec dst_region;
    RegionNull(&dst_region);
    RegionIntersect(&dst_region, &window->borderClip, src_region);

    if (pixmap->screen_x || pixmap->screen_y)
        RegionTranslate(&dst_region, -pixmap->screen_x, -pixmap->screen_y);

    miCopyRegion(&pixmap->drawable, &pixmap->drawable, 0, &dst_region, dx, dy,
                 glamor_copy, 0, 0);
    RegionUninit(&dst_region);
}
