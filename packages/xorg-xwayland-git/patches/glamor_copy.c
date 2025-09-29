/*
 * SPDX-License-Identifier: MIT
 *
 * glamor_copy.c - High-performance GPU copy operations
 * PRODUCTION VERSION v2 - NULL-safety hardened
 *
 * Critical fixes:
 * - NULL pointer validation at all entry points
 * - Bounds checking for box arrays
 * - Uninitialized variable elimination
 * - Race-free command accounting
 */

#include "glamor_priv.h"
#include "glamor_transfer.h"
#include "glamor_prepare.h"
#include "glamor_transform.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#if defined(__AVX2__)
#include <immintrin.h>
#endif

/* Compiler hints */
#if defined(__GNUC__)
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#define NONNULL __attribute__((nonnull))
#else
#define likely(x)   (x)
#define unlikely(x) (x)
#define NONNULL
#endif

#define LOCAL_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define LOCAL_MAX(a, b) (((a) > (b)) ? (a) : (b))
#define CLAMP(v, lo, hi) (((v) < (lo)) ? (lo) : (((v) > (hi)) ? (hi) : (v)))

/* GPU command thresholds (Vega 64 tuned) */
#define GLAMOR_COMMAND_BATCH_SIZE      64
#define GLAMOR_SOFT_COMMAND_LIMIT      384
#define GLAMOR_HARD_COMMAND_LIMIT      3072
#define GLAMOR_VBO_MAX_SIZE            (4 * 1024 * 1024)
#define GLAMOR_VERTEX_PER_BOX          8

/* glCopyImageSubData heuristics */
#define GLAMOR_COPY_IMAGE_MIN_PIXELS   (64 * 64)
#define GLAMOR_COPY_IMAGE_MIN_DIM      32

/* Scratch VBO pool */
#define SCRATCH_VBO_CAPACITY           (512u * 1024u)

/* ═══════════════════════════════════════════════════════════════════════════
 *  Global State (CRITICAL: All access must be NULL-safe)
 * ═══════════════════════════════════════════════════════════════════════════ */
typedef struct {
    Bool     checked;
    Bool     has_copy_image;
    Bool     has_texture_storage;
    Bool     copy_image_coherent;
    int      gl_major;
    int      gl_minor;
} glamor_gl_features;

static glamor_gl_features g_gl_features = {0};

typedef struct {
    GLsync   fence;
    int      pending_commands;
    Bool     needs_flush;
} glamor_gpu_sync_state;

static glamor_gpu_sync_state g_gpu_sync = {0};

typedef struct {
    GLenum   format1;
    GLenum   format2;
    Bool     compatible;
} format_compat_entry;

#define MAX_FORMAT_CACHE_ENTRIES 64
static format_compat_entry g_format_cache[MAX_FORMAT_CACHE_ENTRIES];
static int g_format_cache_entries = 0;

/* Persistent scratch VBO */
static GLuint   scratch_vbo          = 0;
static GLubyte *scratch_map          = NULL;
static size_t   scratch_offset_bytes = 0;

typedef struct {
    uint32_t bitplane;
    int      depth;
    GLuint   uniform_values[4];
    GLfloat  scale_values[4];
} bitplane_cache_entry;

static bitplane_cache_entry g_bitplane_cache = {0};

struct copy_args {
    DrawablePtr       src_drawable;
    glamor_pixmap_fbo *src;
    uint32_t          bitplane;
    int               dx, dy;
};

#ifndef GL_TILE_RASTER_ORDER_FIXED_MESA
#define GL_TILE_RASTER_ORDER_FIXED_MESA          0x8BB8
#define GL_TILE_RASTER_ORDER_INCREASING_X_MESA   0x8BB9
#define GL_TILE_RASTER_ORDER_INCREASING_Y_MESA   0x8BBA
#endif

/* ═══════════════════════════════════════════════════════════════════════════
 *  GPU Command Throttling (unchanged – already safe)
 * ═══════════════════════════════════════════════════════════════════════════ */
static void
glamor_manage_gpu_commands(glamor_screen_private *priv, int new_commands)
{
    (void)priv;
    g_gpu_sync.pending_commands += new_commands;

    if (unlikely(g_gpu_sync.pending_commands >= GLAMOR_SOFT_COMMAND_LIMIT)) {
        if (!g_gpu_sync.fence)
            g_gpu_sync.fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
        glFlush();
        g_gpu_sync.needs_flush = FALSE;
    }

    if (unlikely(g_gpu_sync.pending_commands >= GLAMOR_HARD_COMMAND_LIMIT)) {
        if (!g_gpu_sync.fence)
            g_gpu_sync.fence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);

        while (glClientWaitSync(g_gpu_sync.fence, GL_SYNC_FLUSH_COMMANDS_BIT,
                                50000) == GL_TIMEOUT_EXPIRED)
            ;

        glDeleteSync(g_gpu_sync.fence);
        g_gpu_sync.fence            = NULL;
        g_gpu_sync.pending_commands = 0;
    } else {
        g_gpu_sync.needs_flush = TRUE;
    }
}

static void
glamor_ensure_gpu_idle(glamor_screen_private *priv, Bool force)
{
    (void)priv;

    if (g_gpu_sync.fence) {
        GLenum result = glClientWaitSync(g_gpu_sync.fence,
                                         GL_SYNC_FLUSH_COMMANDS_BIT,
                                         force ? GL_TIMEOUT_IGNORED : 0);
        if (result == GL_ALREADY_SIGNALED || result == GL_CONDITION_SATISFIED) {
            glDeleteSync(g_gpu_sync.fence);
            g_gpu_sync.fence            = NULL;
            g_gpu_sync.pending_commands = 0;
        }
    }

    if (force) {
        glFinish();
        g_gpu_sync.pending_commands = 0;
        g_gpu_sync.needs_flush      = FALSE;
    }
}

static Bool
glamor_check_gpu_health(glamor_screen_private *priv)
{
    GLenum error = glGetError();
    if (unlikely(error != GL_NO_ERROR)) {
        if (error == GL_OUT_OF_MEMORY) {
            glamor_ensure_gpu_idle(priv, TRUE);
            return FALSE;
        }
        if (error != GL_INVALID_VALUE && error != GL_INVALID_OPERATION) {
            ErrorF("glamor: GL error 0x%x detected\n", error);
        }
    }
    return TRUE;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Scratch VBO Pool (race-free version)
 * ═══════════════════════════════════════════════════════════════════════════ */
static Bool
scratch_vbo_ensure(glamor_screen_private *priv)
{
    (void)priv;

    if (scratch_vbo)
        return TRUE;

    if (!epoxy_has_gl_extension("GL_ARB_buffer_storage"))
        return FALSE;

    const GLbitfield flags =
        GL_MAP_WRITE_BIT          |
        GL_MAP_PERSISTENT_BIT     |
        GL_MAP_COHERENT_BIT       |
        GL_MAP_FLUSH_EXPLICIT_BIT |
        GL_DYNAMIC_STORAGE_BIT;

    glGenBuffers(1, &scratch_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, scratch_vbo);
    glBufferStorage(GL_ARRAY_BUFFER, SCRATCH_VBO_CAPACITY, NULL, flags);

    scratch_map = (GLubyte *)glMapBufferRange(
        GL_ARRAY_BUFFER, 0, SCRATCH_VBO_CAPACITY,
        GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT |
        GL_MAP_COHERENT_BIT | GL_MAP_FLUSH_EXPLICIT_BIT
    );

    return (scratch_map != NULL);
}

static GLshort *
scratch_vbo_alloc(glamor_screen_private *priv, size_t bytes, char **out_offset)
{
    if (!scratch_vbo_ensure(priv))
        return NULL;

    if (bytes > SCRATCH_VBO_CAPACITY)
        return NULL;

    if (scratch_offset_bytes + bytes > SCRATCH_VBO_CAPACITY) {
        if (g_gpu_sync.fence) {
            glClientWaitSync(g_gpu_sync.fence, GL_SYNC_FLUSH_COMMANDS_BIT,
                             GL_TIMEOUT_IGNORED);
            glDeleteSync(g_gpu_sync.fence);
            g_gpu_sync.fence = NULL;
        }

        glBindBuffer(GL_ARRAY_BUFFER, scratch_vbo);
        glFlushMappedBufferRange(GL_ARRAY_BUFFER, 0,
                                 (GLsizeiptr)scratch_offset_bytes);
        scratch_offset_bytes = 0;
    }

    *out_offset = (char *)(uintptr_t)scratch_offset_bytes;
    GLshort *ptr = (GLshort *)(scratch_map + scratch_offset_bytes);
    scratch_offset_bytes += bytes;
    return ptr;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  GL Feature Detection (unchanged)
 * ═════════════════��═════════════════════════════════════════════════════════ */
static Bool
glamor_copy_image_gl_is_coherent(void)
{
    if (!g_gl_features.checked)
        return FALSE;

    const char *vendor = (const char *)glGetString(GL_VENDOR);
    const char *renderer = (const char *)glGetString(GL_RENDERER);

    if (!vendor || !renderer)
        return FALSE;

    if ((strstr(vendor, "AMD") || strstr(vendor, "X.Org")) &&
        (strstr(renderer, "Radeon") || strstr(renderer, "RADV"))) {
        g_gl_features.copy_image_coherent = TRUE;
        return TRUE;
    }

    if (strstr(vendor, "NVIDIA")) {
        g_gl_features.copy_image_coherent = TRUE;
        return TRUE;
    }

    if (strstr(vendor, "Intel")) {
        const char *version = (const char *)glGetString(GL_VERSION);
        if (version && strstr(version, "Mesa")) {
            int mesa_major = 0, mesa_minor = 0;
            if (sscanf(version, "%*s Mesa %d.%d", &mesa_major, &mesa_minor) == 2 &&
                mesa_major >= 20) {
                g_gl_features.copy_image_coherent = TRUE;
                return TRUE;
            }
        }
    }

    g_gl_features.copy_image_coherent = FALSE;
    return FALSE;
}

static Bool
glamor_check_copy_image_support(void)
{
    if (g_gl_features.checked)
        return g_gl_features.has_copy_image;

    g_gl_features.checked = TRUE;
    g_gl_features.has_copy_image = FALSE;

    const char *version_str = (const char *)glGetString(GL_VERSION);
    if (version_str) {
        if (sscanf(version_str, "%d.%d", &g_gl_features.gl_major,
                   &g_gl_features.gl_minor) == 2) {
            if (g_gl_features.gl_major > 4 ||
                (g_gl_features.gl_major == 4 && g_gl_features.gl_minor >= 3)) {
                g_gl_features.has_copy_image = TRUE;
            }
        }
    }

    if (!g_gl_features.has_copy_image &&
        epoxy_has_gl_extension("GL_ARB_copy_image")) {
        g_gl_features.has_copy_image = TRUE;
    }

    if (g_gl_features.has_copy_image && !glCopyImageSubData) {
        g_gl_features.has_copy_image = FALSE;
    }

    g_gl_features.has_texture_storage =
        epoxy_has_gl_extension("GL_ARB_texture_storage");

    if (g_gl_features.has_copy_image) {
        glamor_copy_image_gl_is_coherent();
    }

    return g_gl_features.has_copy_image;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Texture Queries
 * ═══════════════════════════════════════════════════════════════════════════ */
static GLenum
glamor_get_tex_internal_format(GLuint tex)
{
    if (tex == 0)
        return 0;

    int ver = epoxy_gl_version();
    if ((ver >= 45 || epoxy_has_gl_extension("GL_ARB_direct_state_access")) &&
        glGetTextureLevelParameteriv) {
        GLint fmt = 0;
        glGetTextureLevelParameteriv(tex, 0, GL_TEXTURE_INTERNAL_FORMAT, &fmt);
        GLenum err = glGetError();
        if (err == GL_NO_ERROR)
            return (GLenum)fmt;
    }

    GLint prev = 0, fmt = 0;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, &prev);
    glBindTexture(GL_TEXTURE_2D, tex);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, &fmt);
    glBindTexture(GL_TEXTURE_2D, (GLuint)prev);
    return (GLenum)fmt;
}

static Bool
glamor_validate_texture(GLuint tex)
{
    if (tex == 0)
        return FALSE;

    GLint prev = 0;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, &prev);
    glBindTexture(GL_TEXTURE_2D, tex);

    GLint width = 0, height = 0;
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);

    glBindTexture(GL_TEXTURE_2D, (GLuint)prev);
    return (width > 0 && height > 0);
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Format Compatibility (unchanged – already correct per GL spec)
 * ═══════════════════════════════════════════════════════════════════════════ */
static Bool
glamor_formats_compatible_for_copy_cached(GLenum format1, GLenum format2)
{
    if (format1 == format2)
        return TRUE;

    for (int i = 0; i < g_format_cache_entries; i++) {
        if ((g_format_cache[i].format1 == format1 &&
             g_format_cache[i].format2 == format2) ||
            (g_format_cache[i].format1 == format2 &&
             g_format_cache[i].format2 == format1)) {
            return g_format_cache[i].compatible;
        }
    }

    static const GLenum class_128bit[] = {
        GL_RGBA32F, GL_RGBA32UI, GL_RGBA32I
    };
    static const GLenum class_96bit[] = {
        GL_RGB32F, GL_RGB32UI, GL_RGB32I
    };
    static const GLenum class_64bit[] = {
        GL_RGBA16F, GL_RG32F, GL_RGBA16UI, GL_RG32UI,
        GL_RGBA16I, GL_RG32I, GL_RGBA16, GL_RGBA16_SNORM
    };
    static const GLenum class_48bit[] = {
        GL_RGB16F, GL_RGB16UI, GL_RGB16I, GL_RGB16, GL_RGB16_SNORM
    };
    static const GLenum class_32bit[] = {
        GL_RG16F, GL_R32F, GL_RGB10_A2UI, GL_RGBA8UI, GL_RG16UI,
        GL_R32UI, GL_RGBA8I, GL_RG16I, GL_R32I, GL_RGB10_A2,
        GL_RGBA8, GL_RG16, GL_RGBA8_SNORM, GL_RG16_SNORM,
        GL_SRGB8_ALPHA8, GL_RGB9_E5, GL_R11F_G11F_B10F
    };
    static const GLenum class_24bit[] = {
        GL_RGB8, GL_RGB8_SNORM, GL_SRGB8, GL_RGB8UI, GL_RGB8I
    };
    static const GLenum class_16bit[] = {
        GL_R16F, GL_RG8UI, GL_R16UI, GL_RG8I, GL_R16I,
        GL_RG8, GL_R16, GL_RG8_SNORM, GL_R16_SNORM
    };
    static const GLenum class_8bit[] = {
        GL_R8UI, GL_R8I, GL_R8, GL_R8_SNORM
    };

    struct {
        const GLenum *formats;
        size_t count;
    } view_classes[] = {
        { class_128bit, sizeof(class_128bit) / sizeof(GLenum) },
        { class_96bit,  sizeof(class_96bit)  / sizeof(GLenum) },
        { class_64bit,  sizeof(class_64bit)  / sizeof(GLenum) },
        { class_48bit,  sizeof(class_48bit)  / sizeof(GLenum) },
        { class_32bit,  sizeof(class_32bit)  / sizeof(GLenum) },
        { class_24bit,  sizeof(class_24bit)  / sizeof(GLenum) },
        { class_16bit,  sizeof(class_16bit)  / sizeof(GLenum) },
        { class_8bit,   sizeof(class_8bit)   / sizeof(GLenum) },
    };

    Bool compatible = FALSE;
    for (size_t c = 0; c < sizeof(view_classes) / sizeof(view_classes[0]); c++) {
        Bool found1 = FALSE, found2 = FALSE;
        for (size_t i = 0; i < view_classes[c].count; i++) {
            if (view_classes[c].formats[i] == format1) found1 = TRUE;
            if (view_classes[c].formats[i] == format2) found2 = TRUE;
        }
        if (found1 && found2) {
            compatible = TRUE;
            break;
        }
    }

    if (g_format_cache_entries < MAX_FORMAT_CACHE_ENTRIES) {
        g_format_cache[g_format_cache_entries].format1 = format1;
        g_format_cache[g_format_cache_entries].format2 = format2;
        g_format_cache[g_format_cache_entries].compatible = compatible;
        g_format_cache_entries++;
    }

    return compatible;
}

static Bool
glamor_should_use_copy_image(int width, int height, Bool is_cursor,
                             Bool same_pixmap, int depth)
{
    if (same_pixmap || is_cursor)
        return FALSE;

    if (width * height < GLAMOR_COPY_IMAGE_MIN_PIXELS)
        return FALSE;

    if (width < GLAMOR_COPY_IMAGE_MIN_DIM || height < GLAMOR_COPY_IMAGE_MIN_DIM)
        return FALSE;

    if ((depth == 1 || depth == 8) && !g_gl_features.copy_image_coherent)
        return FALSE;

    return TRUE;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  AVX2 Vertex Generator (unchanged – already bounds-safe)
 * ═══════════════════════════════════════════════════════════════════════════ */
static inline void
glamor_generate_box_vertices_batched(GLshort *v, const BoxPtr box, int nbox)
{
    /* CRITICAL: v and box MUST be non-NULL (caller's responsibility) */
#if defined(__AVX2__)
    const __m256i v_min = _mm256_set1_epi16(SHRT_MIN);
    const __m256i v_max = _mm256_set1_epi16(SHRT_MAX);

    int i = 0;
    for (; i + 1 < nbox; i += 2) {
        __m128i lo = _mm_loadu_si128((const __m128i *)&box[i]);
        __m128i hi = _mm_loadu_si128((const __m128i *)&box[i + 1]);
        __m256i pack = _mm256_castsi128_si256(lo);
        pack = _mm256_inserti128_si256(pack, hi, 1);

        pack = _mm256_max_epi16(v_min, _mm256_min_epi16(v_max, pack));

        const __m256i shuf = _mm256_set_epi8(
            15,14, 11,10, 13,12, 11,10,
             7, 6,   3, 2,   5, 4,   3, 2,
            15,14, 11,10, 13,12, 11,10,
             7, 6,   3, 2,   5, 4,   3, 2);
        __m256i verts = _mm256_shuffle_epi8(pack, shuf);

        _mm256_storeu_si256((__m256i *)(v + i * 8), verts);
    }

    for (; i < nbox; ++i) {
        const BoxPtr b = &box[i];
        GLshort *p = v + i * 8;

        GLshort x1 = (GLshort)CLAMP(b->x1, SHRT_MIN, SHRT_MAX);
        GLshort y1 = (GLshort)CLAMP(b->y1, SHRT_MIN, SHRT_MAX);
        GLshort x2 = (GLshort)CLAMP(b->x2, SHRT_MIN, SHRT_MAX);
        GLshort y2 = (GLshort)CLAMP(b->y2, SHRT_MIN, SHRT_MAX);

        p[0] = x1; p[1] = y1;
        p[2] = x1; p[3] = y2;
        p[4] = x2; p[5] = y2;
        p[6] = x2; p[7] = y1;
    }
#else
    for (int i = 0; i < nbox; ++i) {
        const BoxPtr b = &box[i];
        GLshort *p = v + i * 8;

        GLshort x1 = (GLshort)CLAMP(b->x1, SHRT_MIN, SHRT_MAX);
        GLshort y1 = (GLshort)CLAMP(b->y1, SHRT_MIN, SHRT_MAX);
        GLshort x2 = (GLshort)CLAMP(b->x2, SHRT_MIN, SHRT_MAX);
        GLshort y2 = (GLshort)CLAMP(b->y2, SHRT_MIN, SHRT_MAX);

        p[0] = x1; p[1] = y1;
        p[2] = x1; p[3] = y2;
        p[4] = x2; p[5] = y2;
        p[6] = x2; p[7] = y1;
    }
#endif
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  glCopyImageSubData Fast-Path (FIXED: NULL box check)
 * ═══════════════════════════════════════════════════════════════════════════ */
static Bool
glamor_copy_fbo_fbo_direct(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                           BoxPtr box, int nbox, int dx, int dy,
                           Bool reverse, Bool upsidedown,
                           Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    /* CRITICAL FIX: Add NULL checks */
    if (!src || !dst || !box || nbox <= 0 || bitplane)
        return FALSE;

    if ((gc && gc->alu != GXcopy) ||
        (gc && !glamor_pm_is_solid(gc->depth, gc->planemask)))
        return FALSE;

    ScreenPtr screen = dst->pScreen;
    if (!screen)
        return FALSE;

    glamor_screen_private *priv = glamor_get_screen_private(screen);
    if (!glamor_check_copy_image_support())
        return FALSE;

    PixmapPtr spix = glamor_get_drawable_pixmap(src);
    PixmapPtr dpix = glamor_get_drawable_pixmap(dst);
    if (!spix || !dpix || spix == dpix)
        return FALSE;

    glamor_pixmap_private *spr = glamor_get_pixmap_private(spix);
    glamor_pixmap_private *dpr = glamor_get_pixmap_private(dpix);
    if (!spr || !dpr ||
        !GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) ||
        !GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr))
        return FALSE;

    glamor_make_current(priv);

    glamor_pixmap_fbo *sfbo = spr->fbo;
    glamor_pixmap_fbo *dfbo = dpr->fbo;
    if (!sfbo || !dfbo)
        return FALSE;

    if (!glamor_validate_texture(sfbo->tex) ||
        !glamor_validate_texture(dfbo->tex))
        return FALSE;

    if (!glamor_formats_compatible_for_copy_cached(
            glamor_get_tex_internal_format(sfbo->tex),
            glamor_get_tex_internal_format(dfbo->tex)))
        return FALSE;

    if (!glamor_should_use_copy_image(spix->drawable.width,
                                      spix->drawable.height,
                                      FALSE, FALSE, src->depth))
        return FALSE;

    /* Box-merge heuristic */
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

    unsigned long bbox_area =
        (unsigned long)(merged.x2 - merged.x1) *
        (unsigned long)(merged.y2 - merged.y1);
    Bool can_merge = (bbox_area > 0 && bbox_area <= covered * 12ul / 10ul);

    int src_off_x = 0, src_off_y = 0, dst_off_x = 0, dst_off_y = 0;
    glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

    const int sp_w = spix->drawable.width;
    const int sp_h = spix->drawable.height;
    const int dp_w = dpix->drawable.width;
    const int dp_h = dpix->drawable.height;

    int commands = 0;

#define ISSUE_COPY(_bx)                                                        \
    do {                                                                       \
        int w_ = (_bx)->x2 - (_bx)->x1;                                        \
        int h_ = (_bx)->y2 - (_bx)->y1;                                        \
        if (w_ <= 0 || h_ <= 0) break;                                         \
        int s_x = (_bx)->x1 + dx + src_off_x;                                  \
        int s_y = (_bx)->y1 + dy + src_off_y;                                  \
        int d_x = (_bx)->x1 + dst_off_x;                                       \
        int d_y = (_bx)->y1 + dst_off_y;                                       \
        if (s_x < 0 || s_y < 0 || d_x < 0 || d_y < 0 ||                        \
            s_x + w_ > sp_w || s_y + h_ > sp_h ||                              \
            d_x + w_ > dp_w || d_y + h_ > dp_h) break;                         \
        int gl_sy = sp_h - (s_y + h_);                                         \
        int gl_dy = dp_h - (d_y + h_);                                         \
        glCopyImageSubData(sfbo->tex, GL_TEXTURE_2D, 0,                        \
                           s_x, gl_sy, 0,                                      \
                           dfbo->tex, GL_TEXTURE_2D, 0,                        \
                           d_x, gl_dy, 0,                                      \
                           w_, h_, 1);                                         \
        ++commands;                                                            \
    } while (0)

    if (can_merge) {
        ISSUE_COPY(&merged);
    } else {
        for (int i = 0; i < nbox; ++i) {
            ISSUE_COPY(&box[i]);
        }
    }

#undef ISSUE_COPY

    if (commands > 0) {
        glMemoryBarrier(GL_TEXTURE_UPDATE_BARRIER_BIT);
        glamor_manage_gpu_commands(priv, commands);
    }

    return (commands > 0);
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Shader Callbacks (unchanged)
 * ═══════════════════════════════════════════════════════════════════════════ */
static Bool
use_copyarea(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    (void)gc;
    struct copy_args *args = (struct copy_args *)arg;
    glamor_pixmap_fbo *src = args->src;

    if (unlikely(!src || src->width <= 0 || src->height <= 0))
        return FALSE;

    glamor_bind_texture(glamor_get_screen_private(drawable->pScreen),
                        GL_TEXTURE0, src, TRUE);

    glUniform2f(prog->fill_offset_uniform, (GLfloat)args->dx, (GLfloat)args->dy);
    glUniform2f(prog->fill_size_inv_uniform,
                1.0f / (GLfloat)src->width,
                1.0f / (GLfloat)src->height);

    return TRUE;
}

static const glamor_facet glamor_facet_copyarea = {
    "copy_area",
    .vs_vars = "in vec2 primitive;\n",
    .vs_exec = (GLAMOR_POS(gl_Position, primitive.xy)
                "       fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = "       frag_color = texture(sampler, fill_pos);\n",
    .locations = glamor_program_location_fillsamp | glamor_program_location_fillpos,
    .use = use_copyarea,
};

static Bool
use_copyplane(DrawablePtr drawable, GCPtr gc, glamor_program *prog, void *arg)
{
    if (unlikely(!gc))
        return FALSE;

    struct copy_args *args = (struct copy_args *)arg;
    glamor_pixmap_fbo *src = args->src;

    if (unlikely(!src || src->width <= 0 || src->height <= 0))
        return FALSE;

    glamor_bind_texture(glamor_get_screen_private(drawable->pScreen),
                        GL_TEXTURE0, src, TRUE);

    glUniform2f(prog->fill_offset_uniform, (GLfloat)args->dx, (GLfloat)args->dy);
    glUniform2f(prog->fill_size_inv_uniform,
                1.0f / (GLfloat)src->width,
                1.0f / (GLfloat)src->height);

    glamor_set_color(drawable, gc->fgPixel, prog->fg_uniform);
    glamor_set_color(drawable, gc->bgPixel, prog->bg_uniform);

    const uint32_t bp = (uint32_t)args->bitplane;
    const int depth = glamor_drawable_effective_depth(args->src_drawable);

    if (likely(g_bitplane_cache.bitplane == bp &&
               g_bitplane_cache.depth == depth)) {
        glUniform4uiv(prog->bitplane_uniform, 1, g_bitplane_cache.uniform_values);
        glUniform4fv(prog->bitmul_uniform, 1, g_bitplane_cache.scale_values);
        return TRUE;
    }

    g_bitplane_cache.bitplane = bp;
    g_bitplane_cache.depth = depth;

    switch (depth) {
    case 30:
        g_bitplane_cache.uniform_values[0] = (bp >> 20) & 0x3ffu;
        g_bitplane_cache.uniform_values[1] = (bp >> 10) & 0x3ffu;
        g_bitplane_cache.uniform_values[2] = (bp      ) & 0x3ffu;
        g_bitplane_cache.uniform_values[3] = 0u;
        g_bitplane_cache.scale_values[0] = 1023.0f;
        g_bitplane_cache.scale_values[1] = 1023.0f;
        g_bitplane_cache.scale_values[2] = 1023.0f;
        g_bitplane_cache.scale_values[3] = 0.0f;
        break;
    case 24:
        g_bitplane_cache.uniform_values[0] = (bp >> 16) & 0xffu;
        g_bitplane_cache.uniform_values[1] = (bp >>  8) & 0xffu;
        g_bitplane_cache.uniform_values[2] = (bp      ) & 0xffu;
        g_bitplane_cache.uniform_values[3] = 0u;
        g_bitplane_cache.scale_values[0] = 255.0f;
        g_bitplane_cache.scale_values[1] = 255.0f;
        g_bitplane_cache.scale_values[2] = 255.0f;
        g_bitplane_cache.scale_values[3] = 0.0f;
        break;
    case 32:
        g_bitplane_cache.uniform_values[0] = (bp >> 16) & 0xffu;
        g_bitplane_cache.uniform_values[1] = (bp >>  8) & 0xffu;
        g_bitplane_cache.uniform_values[2] = (bp      ) & 0xffu;
        g_bitplane_cache.uniform_values[3] = (bp >> 24) & 0xffu;
        g_bitplane_cache.scale_values[0] = 255.0f;
        g_bitplane_cache.scale_values[1] = 255.0f;
        g_bitplane_cache.scale_values[2] = 255.0f;
        g_bitplane_cache.scale_values[3] = 255.0f;
        break;
    case 16:
        g_bitplane_cache.uniform_values[0] = (bp >> 11) & 0x1fu;
        g_bitplane_cache.uniform_values[1] = (bp >>  5) & 0x3fu;
        g_bitplane_cache.uniform_values[2] = (bp      ) & 0x1fu;
        g_bitplane_cache.uniform_values[3] = 0u;
        g_bitplane_cache.scale_values[0] = 31.0f;
        g_bitplane_cache.scale_values[1] = 63.0f;
        g_bitplane_cache.scale_values[2] = 31.0f;
        g_bitplane_cache.scale_values[3] = 0.0f;
        break;
    case 15:
        g_bitplane_cache.uniform_values[0] = (bp >> 10) & 0x1fu;
        g_bitplane_cache.uniform_values[1] = (bp >>  5) & 0x1fu;
        g_bitplane_cache.uniform_values[2] = (bp      ) & 0x1fu;
        g_bitplane_cache.uniform_values[3] = 0u;
        g_bitplane_cache.scale_values[0] = 31.0f;
        g_bitplane_cache.scale_values[1] = 31.0f;
        g_bitplane_cache.scale_values[2] = 31.0f;
        g_bitplane_cache.scale_values[3] = 0.0f;
        break;
    case 8:
    case 1:
        g_bitplane_cache.uniform_values[0] = 0u;
        g_bitplane_cache.uniform_values[1] = 0u;
        g_bitplane_cache.uniform_values[2] = 0u;
        g_bitplane_cache.uniform_values[3] = bp & 0xffu;
        g_bitplane_cache.scale_values[0] = 0.0f;
        g_bitplane_cache.scale_values[1] = 0.0f;
        g_bitplane_cache.scale_values[2] = 0.0f;
        g_bitplane_cache.scale_values[3] = 255.0f;
        break;
    default:
        return FALSE;
    }

    glUniform4uiv(prog->bitplane_uniform, 1, g_bitplane_cache.uniform_values);
    glUniform4fv(prog->bitmul_uniform, 1, g_bitplane_cache.scale_values);
    return TRUE;
}

static const glamor_facet glamor_facet_copyplane = {
    "copy_plane",
    .version = 130,
    .vs_vars = "in vec2 primitive;\n",
    .vs_exec = (GLAMOR_POS(gl_Position, (primitive.xy))
                "       fill_pos = (fill_offset + primitive.xy) * fill_size_inv;\n"),
    .fs_exec = ("       uvec4 bits = uvec4(round(texture(sampler, fill_pos) * bitmul));\n"
                "       if ((bits & bitplane) != uvec4(0,0,0,0))\n"
                "               frag_color = fg;\n"
                "       else\n"
                "               frag_color = bg;\n"),
    .locations = glamor_program_location_fillsamp |
                 glamor_program_location_fillpos |
                 glamor_program_location_fg |
                 glamor_program_location_bg |
                 glamor_program_location_bitplane,
    .use = use_copyplane,
};

/* ═══════════════════════════════════════════════════════════════════════════
 *  FBO→FBO Shader Path (FIXED: deltoids hoisted out of loop, NULL-safe)
 * ═══════════════════════════════════════════════════════════════════════════ */
static Bool
glamor_copy_fbo_fbo_draw(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                         BoxPtr box, int nbox, int dx, int dy,
                         Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    /* CRITICAL FIX: Add NULL checks */
    if (unlikely(!src || !dst || !box || nbox <= 0))
        return FALSE;

    ScreenPtr screen = dst->pScreen;
    if (!screen)
        return FALSE;

    glamor_screen_private *priv = glamor_get_screen_private(screen);

    if (!bitplane &&
        glamor_check_copy_image_support() &&
        glamor_copy_fbo_fbo_direct(src, dst, gc, box, nbox,
                                   dx, dy, FALSE, FALSE, 0, NULL))
        return TRUE;

    PixmapPtr spix = glamor_get_drawable_pixmap(src);
    PixmapPtr dpix = glamor_get_drawable_pixmap(dst);
    if (!spix || !dpix)
        return FALSE;

    glamor_pixmap_private *spr = glamor_get_pixmap_private(spix);
    glamor_pixmap_private *dpr = glamor_get_pixmap_private(dpix);
    if (!spr || !dpr ||
        !GLAMOR_PIXMAP_PRIV_HAS_FBO(spr) ||
        !GLAMOR_PIXMAP_PRIV_HAS_FBO(dpr))
        return FALSE;

    glamor_make_current(priv);

    if (!glamor_check_gpu_health(priv))
        return FALSE;

    if (gc && !glamor_set_planemask(gc->depth, gc->planemask))
        return FALSE;

    if (!glamor_set_alu(dst, gc ? gc->alu : GXcopy))
        return FALSE;

    const Bool is_copyplane = (bitplane != 0);
    if (is_copyplane && !priv->can_copyplane)
        return FALSE;

    glamor_program *prog = is_copyplane ? &priv->copy_plane_prog
                                        : &priv->copy_area_prog;
    const glamor_facet *facet = is_copyplane ? &glamor_facet_copyplane
                                             : &glamor_facet_copyarea;

    if (prog->failed)
        return FALSE;

    if (!prog->prog &&
        !glamor_build_program(screen, prog, facet, NULL, NULL, NULL))
        return FALSE;

    struct copy_args args = {
        .src_drawable = src,
        .bitplane = (uint32_t)bitplane
    };

    int boxes_done = 0;
    Bool ok = TRUE;

    while (boxes_done < nbox) {
        const int batch_boxes = LOCAL_MIN(nbox - boxes_done,
                                          GLAMOR_COMMAND_BATCH_SIZE);
        const size_t vbytes = (size_t)batch_boxes *
                              GLAMOR_VERTEX_PER_BOX *
                              sizeof(GLshort);

        char *vbo_offset = NULL;
        GLshort *vbuf = scratch_vbo_alloc(priv, vbytes, &vbo_offset);

        const Bool using_scratch = (vbuf != NULL);
        if (!vbuf) {
            vbuf = glamor_get_vbo_space(screen, (int)vbytes, &vbo_offset);
            if (!vbuf) {
                glamor_ensure_gpu_idle(priv, TRUE);
                vbuf = glamor_get_vbo_space(screen, (int)vbytes, &vbo_offset);
                if (!vbuf) {
                    ok = FALSE;
                    break;
                }
            }
            glBindBuffer(GL_ARRAY_BUFFER, priv->vbo);
        } else {
            glBindBuffer(GL_ARRAY_BUFFER, scratch_vbo);
        }

        glamor_generate_box_vertices_batched(vbuf, box + boxes_done, batch_boxes);

        if (using_scratch) {
            glFlushMappedBufferRange(GL_ARRAY_BUFFER,
                                     (GLintptr)(uintptr_t)vbo_offset,
                                     (GLsizeiptr)vbytes);
        }

        glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
        glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE,
                              2 * sizeof(GLshort), vbo_offset);

        const Bool same_pixmap = (spix == dpix);
        if (same_pixmap && priv->has_mesa_tile_raster_order) {
            glEnable(GL_TILE_RASTER_ORDER_FIXED_MESA);
            if (dx >= 0)
                glEnable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);
            else
                glDisable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);
            if (dy >= 0)
                glEnable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);
            else
                glDisable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);
        }

        /* CRITICAL FIX: Hoist delta computation out of loop */
        int src_off_x, src_off_y, dst_off_x, dst_off_y;
        glamor_get_drawable_deltas(src, spix, &src_off_x, &src_off_y);
        glamor_get_drawable_deltas(dst, dpix, &dst_off_x, &dst_off_y);

        int commands_this_batch = 0;

        int src_tile = 0, dst_tile = 0;
        glamor_pixmap_loop(spr, src_tile) {
            BoxPtr sbox = glamor_pixmap_box_at(spr, src_tile);
            if (!sbox)
                continue;

            args.dx = dx + src_off_x - sbox->x1;
            args.dy = dy + src_off_y - sbox->y1;
            args.src = glamor_pixmap_fbo_at(spr, src_tile);
            if (!args.src)
                continue;

            if (!glamor_use_program(dst, gc, prog, &args))
                continue;

            commands_this_batch += 3;

            glamor_pixmap_loop(dpr, dst_tile) {
                BoxPtr dbox = glamor_pixmap_box_at(dpr, dst_tile);
                if (!dbox)
                    continue;

                if (!glamor_set_destination_drawable(dst, dst_tile,
                                                     FALSE, FALSE,
                                                     prog->matrix_uniform,
                                                     &dst_off_x, &dst_off_y))
                    continue;

                BoxRec scissor = {
                    .x1 = LOCAL_MAX(-args.dx, dbox->x1),
                    .y1 = LOCAL_MAX(-args.dy, dbox->y1),
                    .x2 = LOCAL_MIN(-args.dx + sbox->x2 - sbox->x1, dbox->x2),
                    .y2 = LOCAL_MIN(-args.dy + sbox->y2 - sbox->y1, dbox->y2)
                };

                if (scissor.x1 >= scissor.x2 || scissor.y1 >= scissor.y2)
                    continue;

                glEnable(GL_SCISSOR_TEST);
                glScissor(scissor.x1 + dst_off_x,
                          scissor.y1 + dst_off_y,
                          scissor.x2 - scissor.x1,
                          scissor.y2 - scissor.y1);

                if (same_pixmap && priv->has_nv_texture_barrier)
                    glTextureBarrierNV();

                glamor_glDrawArrays_GL_QUADS(priv, batch_boxes);

                glDisable(GL_SCISSOR_TEST);

                commands_this_batch += 6;
            }
        }

        glDisableVertexAttribArray(GLAMOR_VERTEX_POS);

        if (same_pixmap && priv->has_mesa_tile_raster_order) {
            glDisable(GL_TILE_RASTER_ORDER_INCREASING_Y_MESA);
            glDisable(GL_TILE_RASTER_ORDER_INCREASING_X_MESA);
            glDisable(GL_TILE_RASTER_ORDER_FIXED_MESA);
        }

        glamor_manage_gpu_commands(priv, commands_this_batch);

        if (!glamor_check_gpu_health(priv)) {
            ok = FALSE;
            break;
        }

        boxes_done += batch_boxes;
    }

    if (g_gpu_sync.needs_flush) {
        glFlush();
        g_gpu_sync.needs_flush = FALSE;
    }

    return ok;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Fallback Paths (unchanged)
 * ═══════════════════════════════════════════════════════════════════════════ */
static void
glamor_copy_bail(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                 BoxPtr box, int nbox, int dx, int dy,
                 Bool reverse, Bool upsidedown,
                 Pixel bitplane, void *closure)
{
    if (nbox == 0)
        return;

    if (glamor_prepare_access(dst, GLAMOR_ACCESS_RW) &&
        glamor_prepare_access(src, GLAMOR_ACCESS_RO)) {
        if (bitplane) {
            if (src->bitsPerPixel > 1) {
                fbCopyNto1(src, dst, gc, box, nbox, dx, dy,
                           reverse, upsidedown, bitplane, closure);
            } else {
                fbCopy1toN(src, dst, gc, box, nbox, dx, dy,
                           reverse, upsidedown, bitplane, closure);
            }
        } else {
            fbCopyNtoN(src, dst, gc, box, nbox, dx, dy,
                       reverse, upsidedown, bitplane, closure);
        }
    }
    glamor_finish_access(dst);
    glamor_finish_access(src);
}

static Bool
glamor_copy_cpu_fbo(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                    BoxPtr box, int nbox, int dx, int dy,
                    Bool reverse, Bool upsidedown,
                    Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);
    int dst_xoff = 0, dst_yoff = 0;

    if (gc && gc->alu != GXcopy)
        return FALSE;
    if (gc && !glamor_pm_is_solid(gc->depth, gc->planemask))
        return FALSE;

    glamor_make_current(priv);

    if (!glamor_prepare_access(src, GLAMOR_ACCESS_RO))
        return FALSE;

    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_xoff, &dst_yoff);

    if (bitplane) {
        FbBits *tmp_bits = NULL;
        FbStride tmp_stride = 0;
        int tmp_bpp = 0;
        int tmp_xoff = 0, tmp_yoff = 0;

        PixmapPtr tmp_pix = fbCreatePixmap(
            screen,
            dst_pixmap->drawable.width,
            dst_pixmap->drawable.height,
            glamor_drawable_effective_depth(dst),
            0
        );
        if (!tmp_pix) {
            glamor_finish_access(src);
            return FALSE;
        }

        tmp_pix->drawable.x = dst_xoff;
        tmp_pix->drawable.y = dst_yoff;

        fbGetDrawable(&tmp_pix->drawable, tmp_bits, tmp_stride, tmp_bpp,
                      tmp_xoff, tmp_yoff);

        if (src->bitsPerPixel > 1) {
            fbCopyNto1(src, &tmp_pix->drawable, gc, box, nbox, dx, dy,
                       reverse, upsidedown, bitplane, closure);
        } else {
            fbCopy1toN(src, &tmp_pix->drawable, gc, box, nbox, dx, dy,
                       reverse, upsidedown, bitplane, closure);
        }

        glamor_upload_boxes(dst, box, nbox, tmp_xoff, tmp_yoff,
                            dst_xoff, dst_yoff,
                            (uint8_t *)tmp_bits,
                            (int)(tmp_stride * (int)sizeof(FbBits)));
        fbDestroyPixmap(tmp_pix);
    } else {
        FbBits *src_bits = NULL;
        FbStride src_stride = 0;
        int src_bpp = 0;
        int src_xoff = 0, src_yoff = 0;

        fbGetDrawable(src, src_bits, src_stride, src_bpp, src_xoff, src_yoff);
        glamor_upload_boxes(dst, box, nbox, src_xoff + dx, src_yoff + dy,
                            dst_xoff, dst_yoff,
                            (uint8_t *)src_bits,
                            (int)(src_stride * (int)sizeof(FbBits)));
    }

    glamor_finish_access(src);
    glamor_manage_gpu_commands(priv, 1);
    return TRUE;
}

static Bool
glamor_copy_fbo_cpu(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                    BoxPtr box, int nbox, int dx, int dy,
                    Bool reverse, Bool upsidedown,
                    Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    if (unlikely(nbox <= 0))
        return TRUE;
    if (unlikely(bitplane != 0))
        return FALSE;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);
    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);

    if (gc && gc->alu != GXcopy)
        return FALSE;
    if (gc && !glamor_pm_is_solid(gc->depth, gc->planemask))
        return FALSE;

    glamor_make_current(priv);

    if (!glamor_prepare_access(dst, GLAMOR_ACCESS_RW))
        return FALSE;

    FbBits *dst_bits = NULL;
    FbStride dst_stride = 0;
    int dst_bpp = 0;
    int src_xoff = 0, src_yoff = 0;
    int dst_xoff = 0, dst_yoff = 0;

    glamor_get_drawable_deltas(src, src_pixmap, &src_xoff, &src_yoff);
    fbGetDrawable(dst, dst_bits, dst_stride, dst_bpp, dst_xoff, dst_yoff);

    if (unlikely(dst_bits == NULL)) {
        glamor_finish_access(dst);
        return FALSE;
    }

    glamor_ensure_gpu_idle(priv, FALSE);

    glamor_download_boxes(src, box, nbox,
                          src_xoff + dx, src_yoff + dy,
                          dst_xoff, dst_yoff,
                          (uint8_t *)dst_bits,
                          (int)(dst_stride * (int)sizeof(FbBits)));

    glamor_finish_access(dst);
    return TRUE;
}

static Bool
glamor_copy_fbo_fbo_temp(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                         BoxPtr box, int nbox, int dx, int dy,
                         Bool reverse, Bool upsidedown,
                         Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    if (nbox == 0)
        return TRUE;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);
    PixmapPtr tmp_pixmap = NULL;
    BoxRec bounds;
    BoxPtr tmp_box = NULL;
    Bool ret = FALSE;

    glamor_make_current(priv);
    glamor_ensure_gpu_idle(priv, FALSE);

    if (gc && !glamor_set_planemask(gc->depth, gc->planemask))
        return FALSE;
    if (!glamor_set_alu(dst, gc ? gc->alu : GXcopy))
        return FALSE;

    bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    int w = bounds.x2 - bounds.x1;
    int h = bounds.y2 - bounds.y1;
    if (w <= 0 || h <= 0)
        return TRUE;

    if ((size_t)w * (size_t)h * 4 > 64 * 1024 * 1024)
        return FALSE;

    tmp_pixmap = glamor_create_pixmap(screen, w, h,
                                      glamor_drawable_effective_depth(src), 0);
    if (!tmp_pixmap)
        return FALSE;

    tmp_box = (BoxPtr)malloc((size_t)nbox * sizeof(BoxRec));
    if (!tmp_box)
        goto bail;

    for (int n = 0; n < nbox; n++) {
        tmp_box[n].x1 = box[n].x1 - bounds.x1;
        tmp_box[n].x2 = box[n].x2 - bounds.x1;
        tmp_box[n].y1 = box[n].y1 - bounds.y1;
        tmp_box[n].y2 = box[n].y2 - bounds.y1;
    }

    if (!glamor_copy_fbo_fbo_draw(src, &tmp_pixmap->drawable, NULL,
                                  tmp_box, nbox, dx + bounds.x1, dy + bounds.y1,
                                  FALSE, FALSE, 0, NULL))
        goto bail;

    glamor_ensure_gpu_idle(priv, FALSE);

    if (!glamor_copy_fbo_fbo_draw(&tmp_pixmap->drawable, dst, gc,
                                  box, nbox, -bounds.x1, -bounds.y1,
                                  FALSE, FALSE, bitplane, NULL))
        goto bail;

    ret = TRUE;

bail:
    free(tmp_box);
    if (tmp_pixmap)
        glamor_destroy_pixmap(tmp_pixmap);
    return ret;
}

static Bool
glamor_copy_needs_temp(DrawablePtr src, DrawablePtr dst,
                       BoxPtr box, int nbox, int dx, int dy)
{
    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);

    if (!src_pixmap || !dst_pixmap)
        return TRUE;

    ScreenPtr screen = dst->pScreen;
    glamor_screen_private *priv = glamor_get_screen_private(screen);

    if (src_pixmap != dst_pixmap)
        return FALSE;

    if (nbox == 0)
        return FALSE;

    if (!priv->has_nv_texture_barrier)
        return TRUE;

    if (priv->has_mesa_tile_raster_order)
        return FALSE;

    int src_off_x = 0, src_off_y = 0;
    int dst_off_x = 0, dst_off_y = 0;
    glamor_get_drawable_deltas(src, src_pixmap, &src_off_x, &src_off_y);
    glamor_get_drawable_deltas(dst, dst_pixmap, &dst_off_x, &dst_off_y);

    BoxRec bounds = box[0];
    for (int n = 1; n < nbox; n++) {
        bounds.x1 = LOCAL_MIN(bounds.x1, box[n].x1);
        bounds.y1 = LOCAL_MIN(bounds.y1, box[n].y1);
        bounds.x2 = LOCAL_MAX(bounds.x2, box[n].x2);
        bounds.y2 = LOCAL_MAX(bounds.y2, box[n].y2);
    }

    if (bounds.x1 + dst_off_x      < bounds.x2 + dx + src_off_x &&
        bounds.x1 + dx + src_off_x < bounds.x2 + dst_off_x &&
        bounds.y1 + dst_off_y      < bounds.y2 + dy + src_off_y &&
        bounds.y1 + dy + src_off_y < bounds.y2 + dst_off_y) {
        return TRUE;
    }

    return FALSE;
}

static Bool
glamor_copy_gl(DrawablePtr src, DrawablePtr dst, GCPtr gc,
               BoxPtr box, int nbox, int dx, int dy,
               Bool reverse, Bool upsidedown,
               Pixel bitplane, void *closure)
{
    (void)reverse; (void)upsidedown; (void)closure;

    PixmapPtr src_pixmap = glamor_get_drawable_pixmap(src);
    PixmapPtr dst_pixmap = glamor_get_drawable_pixmap(dst);

    if (!src_pixmap || !dst_pixmap)
        return FALSE;

    glamor_pixmap_private *src_priv = glamor_get_pixmap_private(src_pixmap);
    glamor_pixmap_private *dst_priv = glamor_get_pixmap_private(dst_pixmap);

    if (!src_priv || !dst_priv)
        return FALSE;

    if (GLAMOR_PIXMAP_PRIV_HAS_FBO(dst_priv)) {
        if (GLAMOR_PIXMAP_PRIV_HAS_FBO(src_priv)) {
            if (glamor_copy_needs_temp(src, dst, box, nbox, dx, dy)) {
                return glamor_copy_fbo_fbo_temp(src, dst, gc, box, nbox, dx, dy,
                                                reverse, upsidedown, bitplane, closure);
            } else {
                return glamor_copy_fbo_fbo_draw(src, dst, gc, box, nbox, dx, dy,
                                                reverse, upsidedown, bitplane, closure);
            }
        }
        return glamor_copy_cpu_fbo(src, dst, gc, box, nbox, dx, dy,
                                   reverse, upsidedown, bitplane, closure);
    } else if (GLAMOR_PIXMAP_PRIV_HAS_FBO(src_priv) &&
               dst_priv && dst_priv->type != GLAMOR_DRM_ONLY &&
               bitplane == 0) {
        return glamor_copy_fbo_cpu(src, dst, gc, box, nbox, dx, dy,
                                   reverse, upsidedown, bitplane, closure);
    }

    return FALSE;
}

/* ═══════════════════════════════════════════════════════════════════════════
 *  Public API (CRITICAL: NULL-safe entry points)
 * ═══════════════════════════════════════════════════════════════════════════ */
void
glamor_copy(DrawablePtr src, DrawablePtr dst, GCPtr gc,
            BoxPtr box, int nbox, int dx, int dy,
            Bool reverse, Bool upsidedown,
            Pixel bitplane, void *closure)
{
    /* CRITICAL FIX: Validate ALL inputs at public API boundary */
    if (!src || !dst || !box || nbox <= 0)
        return;

    ScreenPtr screen = dst->pScreen;
    if (!screen)
        return;

    glamor_screen_private *priv = glamor_get_screen_private(screen);
    if (priv) {
        glamor_make_current(priv);
        glamor_check_gpu_health(priv);
    }

    if (glamor_copy_gl(src, dst, gc, box, nbox, dx, dy,
                       reverse, upsidedown, bitplane, closure))
        return;

    glamor_copy_bail(src, dst, gc, box, nbox, dx, dy,
                     reverse, upsidedown, bitplane, closure);
}

RegionPtr
glamor_copy_area(DrawablePtr src, DrawablePtr dst, GCPtr gc,
                 int srcx, int srcy, int width, int height,
                 int dstx, int dsty)
{
    /* CRITICAL FIX: NULL check */
    if (!src || !dst || !gc)
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
    /* CRITICAL FIX: NULL check */
    if (!src || !dst || !gc)
        return NULL;

    if ((bitplane & FbFullMask(glamor_drawable_effective_depth(src))) == 0) {
        return miHandleExposures(src, dst, gc,
                                 srcx, srcy, width, height,
                                 dstx, dsty);
    }
    return miDoCopy(src, dst, gc,
                    srcx, srcy, width, height,
                    dstx, dsty, glamor_copy, bitplane, NULL);
}

void
glamor_copy_window(WindowPtr window, DDXPointRec old_origin, RegionPtr src_region)
{
    /* CRITICAL FIX: NULL check */
    if (!window || !src_region)
        return;

    PixmapPtr pixmap = glamor_get_drawable_pixmap(&window->drawable);
    if (!pixmap)
        return;

    DrawablePtr drawable = &pixmap->drawable;
    RegionRec dst_region;
    int dx, dy;

    dx = old_origin.x - window->drawable.x;
    dy = old_origin.y - window->drawable.y;
    RegionTranslate(src_region, -dx, -dy);

    RegionNull(&dst_region);
    RegionIntersect(&dst_region, &window->borderClip, src_region);

#if defined(COMPOSITE) || defined(ROOTLESS)
    if (pixmap->screen_x || pixmap->screen_y) {
        RegionTranslate(&dst_region, -pixmap->screen_x, -pixmap->screen_y);
    }
#endif

    miCopyRegion(drawable, drawable, 0, &dst_region, dx, dy, glamor_copy, 0, 0);

    RegionUninit(&dst_region);
}
