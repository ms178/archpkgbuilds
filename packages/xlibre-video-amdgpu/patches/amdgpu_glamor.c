/*
 * amdgpu_glamor.c - GLAMOR (OpenGL 2D acceleration) for AMDGPU
 * Copyright © 2014-2024 Advanced Micro Devices, Inc.
 */

#include "config.h"
#include <xorg-server.h>

#ifdef USE_GLAMOR

#include <xf86.h>
#include <stdint.h>
#include <limits.h>

#include "amdgpu_bo_helper.h"
#include "amdgpu_pixmap.h"
#include "amdgpu_glamor.h"

#include <gbm.h>

DevPrivateKeyRec amdgpu_pixmap_index;

#if defined(__GNUC__) || defined(__clang__)
# define AMDGPU_HOT  __attribute__((hot))
# define AMDGPU_COLD __attribute__((cold))
# define LIKELY(x)   __builtin_expect(!!(x), 1)
# define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
# define AMDGPU_HOT
# define AMDGPU_COLD
# define LIKELY(x)   (x)
# define UNLIKELY(x) (x)
#endif

#define MAX_PIXMAP_DIM 32767
#define SMALL_PIXMAP_THRESHOLD 32

void AMDGPU_HOT
amdgpu_glamor_exchange_buffers(PixmapPtr src, PixmapPtr dst)
{
	AMDGPUInfoPtr info;

	if (UNLIKELY(!src || !dst || !dst->drawable.pScreen))
		return;

	info = AMDGPUPTR(xf86ScreenToScrn(dst->drawable.pScreen));
	if (info && info->use_glamor)
		glamor_egl_exchange_buffers(src, dst);
}

Bool AMDGPU_COLD
amdgpu_glamor_create_screen_resources(ScreenPtr screen)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
	PixmapPtr pixmap;

	if (UNLIKELY(!screen))
		return FALSE;

	scrn = xf86ScreenToScrn(screen);
	info = AMDGPUPTR(scrn);

	if (!info->use_glamor)
		return TRUE;

	pixmap = screen->GetScreenPixmap(screen);
	if (!pixmap)
		return FALSE;

	return amdgpu_glamor_create_textured_pixmap(pixmap, info->front_buffer);
}

Bool AMDGPU_COLD
amdgpu_glamor_pre_init(ScrnInfoPtr scrn)
{
	AMDGPUInfoPtr info;
	AMDGPUEntPtr pAMDGPUEnt;
	void *module;
	CARD32 ver;

	if (UNLIKELY(!scrn))
		return FALSE;

	info = AMDGPUPTR(scrn);
	if (UNLIKELY(!info))
		return FALSE;

	if (scrn->depth < 15) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
		           "Glamor requires depth >= 15 (got %d)\n", scrn->depth);
		return FALSE;
	}

	module = xf86LoadSubModule(scrn, GLAMOR_EGL_MODULE_NAME);
	if (!module) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR, "Failed to load glamor\n");
		return FALSE;
	}

	ver = xf86GetModuleVersion(module);
	if (ver < MODULE_VERSION_NUMERIC(0, 3, 1)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
		           "Glamor version too old (need >= 0.3.1)\n");
		return FALSE;
	}

	if (scrn->depth == 30 && ver < MODULE_VERSION_NUMERIC(1, 0, 1)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
		           "Depth 30 requires glamor >= 1.0.1\n");
		return FALSE;
	}

	pAMDGPUEnt = AMDGPUEntPriv(scrn);
	if (!glamor_egl_init(scrn, pAMDGPUEnt->fd)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR, "glamor_egl_init failed\n");
		return FALSE;
	}

	xf86DrvMsg(scrn->scrnIndex, X_INFO, "Glamor EGL initialized\n");
	info->use_glamor = TRUE;
	return TRUE;
}

Bool AMDGPU_HOT
amdgpu_glamor_create_textured_pixmap(PixmapPtr pixmap, struct amdgpu_buffer *bo)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
	uint32_t handle;

	if (UNLIKELY(!pixmap || !bo))
		return FALSE;

	scrn = xf86ScreenToScrn(pixmap->drawable.pScreen);
	info = AMDGPUPTR(scrn);

	if (!info->use_glamor)
		return TRUE;

	if (bo->flags & AMDGPU_BO_FLAGS_GBM) {
		if (UNLIKELY(!bo->bo.gbm))
			return FALSE;
		return glamor_egl_create_textured_pixmap_from_gbm_bo(
			pixmap, bo->bo.gbm, FALSE);
	}

	if (!amdgpu_bo_get_handle(bo, &handle))
		return FALSE;

	return glamor_egl_create_textured_pixmap(pixmap, handle, pixmap->devKind);
}

static Bool AMDGPU_HOT
amdgpu_glamor_destroy_pixmap(PixmapPtr pixmap)
{
	ScreenPtr screen;
	AMDGPUInfoPtr info;
	Bool ret;

	if (UNLIKELY(!pixmap))
		return TRUE;

	screen = pixmap->drawable.pScreen;
	if (UNLIKELY(!screen))
		return TRUE;

	info = AMDGPUPTR(xf86ScreenToScrn(screen));

	if (pixmap->refcnt == 1) {
		if (pixmap->devPrivate.ptr) {
			struct amdgpu_buffer *bo = amdgpu_get_pixmap_bo(pixmap);
			if (bo)
				amdgpu_bo_unmap(bo);
			pixmap->devPrivate.ptr = NULL;
		}
		amdgpu_set_pixmap_bo(pixmap, NULL);
	}

	screen->DestroyPixmap = info->glamor.SavedDestroyPixmap;
	ret = screen->DestroyPixmap ? screen->DestroyPixmap(pixmap) : TRUE;
	screen->DestroyPixmap = amdgpu_glamor_destroy_pixmap;

	return ret;
}

static PixmapPtr AMDGPU_HOT
amdgpu_glamor_create_pixmap(ScreenPtr screen, int w, int h, int depth,
                             unsigned usage)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
	PixmapFormatPtr fmt;
	PixmapPtr pixmap = NULL;
	struct amdgpu_pixmap *priv = NULL;
	struct amdgpu_buffer *bo = NULL;
	int stride;

	if (UNLIKELY(!screen))
		return NullPixmap;

	scrn = xf86ScreenToScrn(screen);
	info = AMDGPUPTR(scrn);
	fmt = xf86GetPixFormat(scrn, depth);

	if (UNLIKELY(!fmt || depth < 1 || depth > 32))
		return NullPixmap;

	if (UNLIKELY(w > MAX_PIXMAP_DIM || h > MAX_PIXMAP_DIM))
		return NullPixmap;

	if (w > 0 && h > 0) {
		uint64_t bytes;
		if (__builtin_mul_overflow((uint64_t)w, (uint64_t)h, &bytes) ||
		    __builtin_mul_overflow(bytes, (uint64_t)(fmt->bitsPerPixel >> 3), &bytes) ||
		    bytes > (1ULL << 30))
			return NullPixmap;
	}

	if (depth == 1)
		return fbCreatePixmap(screen, w, h, depth, usage);

	if (usage == CREATE_PIXMAP_USAGE_GLYPH_PICTURE &&
	    w <= SMALL_PIXMAP_THRESHOLD && h <= SMALL_PIXMAP_THRESHOLD)
		return fbCreatePixmap(screen, w, h, depth, usage);

	if (w == 0 || h == 0)
		return fbCreatePixmap(screen, 0, 0, depth, usage);

	if (usage != CREATE_PIXMAP_USAGE_BACKING_PIXMAP &&
	    usage != CREATE_PIXMAP_USAGE_SHARED &&
	    !info->shadow_primary &&
	    w >= scrn->virtualX && w <= scrn->displayWidth &&
	    h == scrn->virtualY && fmt->bitsPerPixel == scrn->bitsPerPixel)
		usage |= AMDGPU_CREATE_PIXMAP_SCANOUT;

	if (!(usage & AMDGPU_CREATE_PIXMAP_SCANOUT) &&
	    !AMDGPU_CREATE_PIXMAP_SHARED(usage)) {
		if (info->shadow_primary) {
			if (usage != CREATE_PIXMAP_USAGE_BACKING_PIXMAP)
				return fbCreatePixmap(screen, w, h, depth, usage);
			usage |= AMDGPU_CREATE_PIXMAP_LINEAR | AMDGPU_CREATE_PIXMAP_GTT;
		} else if (usage != CREATE_PIXMAP_USAGE_BACKING_PIXMAP) {
			pixmap = glamor_create_pixmap(screen, w, h, depth, usage);
			if (pixmap)
				return pixmap;
		}
	}

	pixmap = fbCreatePixmap(screen, 0, 0, depth, usage);
	if (!pixmap)
		return NullPixmap;

	priv = calloc(1, sizeof(*priv));
	if (!priv)
		goto fail;

	bo = amdgpu_alloc_pixmap_bo(scrn, w, h, depth, usage,
	                            pixmap->drawable.bitsPerPixel, &stride);
	if (!bo)
		goto fail;

	priv->bo = bo;
	amdgpu_set_pixmap_private(pixmap, priv);
	screen->ModifyPixmapHeader(pixmap, w, h, 0, 0, stride, NULL);
	pixmap->devPrivate.ptr = NULL;

	if (!amdgpu_glamor_create_textured_pixmap(pixmap, bo)) {
		if (AMDGPU_CREATE_PIXMAP_SHARED(usage)) {
			amdgpu_set_pixmap_bo(pixmap, NULL);
			free(priv);
			fbDestroyPixmap(pixmap);
			return NullPixmap;
		}

		amdgpu_set_pixmap_bo(pixmap, NULL);
		free(priv);
		fbDestroyPixmap(pixmap);

		pixmap = glamor_create_pixmap(screen, w, h, depth, usage);
		if (pixmap)
			return pixmap;

		return fbCreatePixmap(screen, w, h, depth, usage);
	}

	return pixmap;

fail:
	if (priv)
		free(priv);
	if (pixmap)
		fbDestroyPixmap(pixmap);
	return NullPixmap;
}

PixmapPtr AMDGPU_COLD
amdgpu_glamor_set_pixmap_bo(DrawablePtr drawable, PixmapPtr pixmap)
{
	PixmapPtr old;
	ScreenPtr screen;
	struct amdgpu_pixmap *priv_old, *priv_new;
	GCPtr gc;

	if (UNLIKELY(!drawable || !pixmap))
		return pixmap;

	old = get_drawable_pixmap(drawable);
	if (!old)
		return pixmap;

	screen = drawable->pScreen;
	priv_new = amdgpu_get_pixmap_private(pixmap);
	priv_old = amdgpu_get_pixmap_private(old);

	gc = GetScratchGC(drawable->depth, screen);
	if (gc) {
		ValidateGC(&pixmap->drawable, gc);
		gc->ops->CopyArea(&old->drawable, &pixmap->drawable, gc,
		                  0, 0, old->drawable.width, old->drawable.height,
		                  0, 0);
		FreeScratchGC(gc);
	}

	glamor_egl_exchange_buffers(old, pixmap);
	amdgpu_set_pixmap_private(pixmap, priv_old);
	amdgpu_set_pixmap_private(old, priv_new);

	screen->ModifyPixmapHeader(old, old->drawable.width, old->drawable.height,
	                           0, 0, pixmap->devKind, NULL);
	old->devPrivate.ptr = NULL;

	dixDestroyPixmap(pixmap, 0);
	return old;
}

static Bool AMDGPU_COLD
amdgpu_glamor_share_pixmap_backing(PixmapPtr pixmap, ScreenPtr slave,
                                   void **handle)
{
	ScreenPtr screen;
	AMDGPUInfoPtr info;
	ScrnInfoPtr scrn;
	uint64_t tiling;
	CARD16 stride;
	CARD32 size;
	Bool linear;
	int fd;

	if (UNLIKELY(!pixmap || !handle))
		return FALSE;

	screen = pixmap->drawable.pScreen;
	scrn = xf86ScreenToScrn(screen);
	info = AMDGPUPTR(scrn);
	tiling = amdgpu_pixmap_get_tiling_info(pixmap);

	if (info->family >= AMDGPU_FAMILY_GC_12_0_0)
		linear = AMDGPU_TILING_GET(tiling, GFX12_SWIZZLE_MODE) == 0;
	else if (info->family >= AMDGPU_FAMILY_AI)
		linear = AMDGPU_TILING_GET(tiling, SWIZZLE_MODE) == 0;
	else
		linear = AMDGPU_TILING_GET(tiling, ARRAY_MODE) == 1;

	if (!linear) {
		PixmapPtr lin;

		if (screen->GetScreenPixmap(screen) == pixmap)
			return FALSE;

		lin = screen->CreatePixmap(screen, pixmap->drawable.width,
		                           pixmap->drawable.height,
		                           pixmap->drawable.depth,
		                           CREATE_PIXMAP_USAGE_SHARED);
		if (!lin)
			return FALSE;

		amdgpu_glamor_set_pixmap_bo(&pixmap->drawable, lin);
	}

	fd = glamor_fd_from_pixmap(screen, pixmap, &stride, &size);
	if (fd < 0)
		return FALSE;

	*handle = (void *)(long)fd;
	return TRUE;
}

static Bool AMDGPU_COLD
amdgpu_glamor_set_shared_pixmap_backing(PixmapPtr pixmap, void *handle)
{
	ScreenPtr screen;
	ScrnInfoPtr scrn;
	struct amdgpu_pixmap *priv;
	int fd = (int)(long)handle;

	if (UNLIKELY(!pixmap))
		return FALSE;

	screen = pixmap->drawable.pScreen;
	scrn = xf86ScreenToScrn(screen);

	if (!amdgpu_set_shared_pixmap_backing(pixmap, handle))
		return FALSE;

	priv = amdgpu_get_pixmap_private(pixmap);
	if (!priv)
		return FALSE;

	if (fd != -1 && !amdgpu_glamor_create_textured_pixmap(pixmap, priv->bo)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
		           "Failed to create textured PRIME pixmap\n");
		return FALSE;
	}

	screen->ModifyPixmapHeader(pixmap, pixmap->drawable.width,
	                           pixmap->drawable.height, 0, 0, 0, NULL);
	return TRUE;
}

Bool AMDGPU_COLD
amdgpu_glamor_init(ScreenPtr screen)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
#ifdef RENDER
	PictureScreenPtr ps = NULL;
	UnrealizeGlyphProcPtr saved_unrealize = NULL;
#endif

	if (UNLIKELY(!screen))
		return FALSE;

	scrn = xf86ScreenToScrn(screen);
	info = AMDGPUPTR(scrn);

#ifdef RENDER
	if (info->shadow_primary) {
		ps = GetPictureScreenIfSet(screen);
		if (ps) {
			saved_unrealize = ps->UnrealizeGlyph;
			info->glamor.SavedGlyphs = ps->Glyphs;
			info->glamor.SavedTriangles = ps->Triangles;
			info->glamor.SavedTrapezoids = ps->Trapezoids;
		}
	}
#endif

	if (!glamor_init(screen, GLAMOR_USE_EGL_SCREEN | GLAMOR_USE_SCREEN |
	                 GLAMOR_USE_PICTURE_SCREEN | GLAMOR_INVERTED_Y_AXIS |
	                 GLAMOR_NO_DRI3)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR, "glamor_init failed\n");
		return FALSE;
	}

	if (!dixRegisterPrivateKey(&amdgpu_pixmap_index, PRIVATE_PIXMAP, 0))
		return FALSE;

	if (info->shadow_primary)
		amdgpu_glamor_screen_init(screen);

#ifdef RENDER
	if (ps && saved_unrealize)
		ps->UnrealizeGlyph = saved_unrealize;
#endif

	info->glamor.SavedCreatePixmap = screen->CreatePixmap;
	screen->CreatePixmap = amdgpu_glamor_create_pixmap;

	info->glamor.SavedDestroyPixmap = screen->DestroyPixmap;
	screen->DestroyPixmap = amdgpu_glamor_destroy_pixmap;

	info->glamor.SavedSharePixmapBacking = screen->SharePixmapBacking;
	screen->SharePixmapBacking = amdgpu_glamor_share_pixmap_backing;

	info->glamor.SavedSetSharedPixmapBacking = screen->SetSharedPixmapBacking;
	screen->SetSharedPixmapBacking = amdgpu_glamor_set_shared_pixmap_backing;

	xf86DrvMsg(scrn->scrnIndex, X_INFO, "GLAMOR acceleration enabled\n");
	return TRUE;
}

void AMDGPU_HOT
amdgpu_glamor_flush(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	if (info->use_glamor) {
		glamor_block_handler(pScrn->pScreen);
		info->gpu_flushed++;
	}
}

void AMDGPU_COLD
amdgpu_glamor_finish(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	if (info->use_glamor) {
		glamor_finish(pScrn->pScreen);
		info->gpu_flushed++;
	}
}

void AMDGPU_COLD
amdgpu_glamor_fini(ScreenPtr screen)
{
	AMDGPUInfoPtr info;

	if (UNLIKELY(!screen))
		return;

	info = AMDGPUPTR(xf86ScreenToScrn(screen));
	if (!info || !info->use_glamor)
		return;

	screen->CreatePixmap = info->glamor.SavedCreatePixmap;
	screen->DestroyPixmap = info->glamor.SavedDestroyPixmap;
	screen->SharePixmapBacking = info->glamor.SavedSharePixmapBacking;
	screen->SetSharedPixmapBacking = info->glamor.SavedSetSharedPixmapBacking;
}

XF86VideoAdaptorPtr AMDGPU_COLD
amdgpu_glamor_xv_init(ScreenPtr pScreen, int num_adapt)
{
	if (UNLIKELY(!pScreen))
		return NULL;

	return glamor_xv_init(pScreen, num_adapt);
}

#endif
