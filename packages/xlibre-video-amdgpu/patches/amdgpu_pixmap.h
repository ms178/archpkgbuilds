/*
 * amdgpu_pixmap.h - Pixmap management for AMDGPU
 * Copyright © 2014-2024 Advanced Micro Devices, Inc.
 */

#ifndef AMDGPU_PIXMAP_H
#define AMDGPU_PIXMAP_H

#include "amdgpu_drv.h"
#include <stdint.h>

#if defined(__GNUC__) || defined(__clang__)
# define AMDGPU_HOT  __attribute__((hot))
# define AMDGPU_COLD __attribute__((cold))
# define LIKELY(x)   __builtin_expect(!!(x), 1)
# define UNLIKELY(x) __builtin_expect(!!(x), 0)
# define AMDGPU_PURE __attribute__((pure))
#else
# define AMDGPU_HOT
# define AMDGPU_COLD
# define LIKELY(x)   (x)
# define UNLIKELY(x) (x)
# define AMDGPU_PURE
#endif

struct amdgpu_pixmap {
	uint_fast32_t gpu_read;
	uint_fast32_t gpu_write;
	uint64_t tiling_info;
	struct amdgpu_buffer *bo;
	struct drmmode_fb *fb;
	Bool fb_failed;
	Bool handle_valid;
	uint32_t handle;
};

enum {
	AMDGPU_CREATE_PIXMAP_FRONT   = 0x10000000,
	AMDGPU_CREATE_PIXMAP_DRI2    = 0x08000000,
	AMDGPU_CREATE_PIXMAP_LINEAR  = 0x04000000,
	AMDGPU_CREATE_PIXMAP_SCANOUT = 0x02000000,
	AMDGPU_CREATE_PIXMAP_GTT     = 0x01000000,
};

extern DevPrivateKeyRec amdgpu_pixmap_index;

Bool amdgpu_pixmap_init(ScreenPtr screen);

static inline struct amdgpu_pixmap* AMDGPU_HOT AMDGPU_PURE
amdgpu_get_pixmap_private(PixmapPtr pixmap)
{
	if (UNLIKELY(!pixmap))
		return NULL;
	return dixGetPrivate(&pixmap->devPrivates, &amdgpu_pixmap_index);
}

static inline void AMDGPU_HOT
amdgpu_set_pixmap_private(PixmapPtr pixmap, struct amdgpu_pixmap *priv)
{
	if (LIKELY(pixmap))
		dixSetPrivate(&pixmap->devPrivates, &amdgpu_pixmap_index, priv);
}

static inline Bool AMDGPU_HOT
amdgpu_set_pixmap_bo(PixmapPtr pPix, struct amdgpu_buffer *bo)
{
	ScrnInfoPtr scrn;
	AMDGPUEntPtr pAMDGPUEnt;
	struct amdgpu_pixmap *priv;

	if (UNLIKELY(!pPix))
		return FALSE;

	scrn = xf86ScreenToScrn(pPix->drawable.pScreen);
	pAMDGPUEnt = AMDGPUEntPriv(scrn);
	priv = amdgpu_get_pixmap_private(pPix);

	if (!priv && !bo)
		return TRUE;

	if (priv) {
		if (priv->bo == bo)
			return TRUE;

		drmmode_fb_reference(pAMDGPUEnt->fd, &priv->fb, NULL);

		if (priv->bo) {
			amdgpu_bo_unref(&priv->bo);
			priv->handle_valid = FALSE;
		}

		if (!bo) {
			free(priv);
			amdgpu_set_pixmap_private(pPix, NULL);
			return TRUE;
		}
	}

	if (bo) {
		if (!priv) {
			priv = calloc(1, sizeof(struct amdgpu_pixmap));
			if (!priv)
				return FALSE;
			amdgpu_set_pixmap_private(pPix, priv);
		}
		amdgpu_bo_ref(bo);
		priv->bo = bo;
	}

	return TRUE;
}

static inline struct amdgpu_buffer* AMDGPU_HOT AMDGPU_PURE
amdgpu_get_pixmap_bo(PixmapPtr pPix)
{
	struct amdgpu_pixmap *priv;
	if (UNLIKELY(!pPix))
		return NULL;
	priv = amdgpu_get_pixmap_private(pPix);
	return priv ? priv->bo : NULL;
}

static inline struct drmmode_fb*
amdgpu_fb_create(ScrnInfoPtr scrn, int drm_fd, uint32_t width, uint32_t height,
                 uint32_t pitch, uint32_t handle)
{
	struct drmmode_fb *fb;

	if (UNLIKELY(!scrn || drm_fd < 0))
		return NULL;

	fb = malloc(sizeof(*fb));
	if (!fb)
		return NULL;

	fb->refcnt = 1;
	if (drmModeAddFB(drm_fd, width, height, scrn->depth, scrn->bitsPerPixel,
	                 pitch, handle, &fb->handle) == 0)
		return fb;

	free(fb);
	return NULL;
}

static inline struct drmmode_fb**
amdgpu_pixmap_get_fb_ptr(PixmapPtr pix)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
	struct amdgpu_pixmap *priv;

	if (UNLIKELY(!pix))
		return NULL;

	scrn = xf86ScreenToScrn(pix->drawable.pScreen);
	info = AMDGPUPTR(scrn);

	if (!info->use_glamor)
		return NULL;

	priv = amdgpu_get_pixmap_private(pix);
	return priv ? &priv->fb : NULL;
}

static inline struct drmmode_fb*
amdgpu_pixmap_get_fb(PixmapPtr pix)
{
	ScrnInfoPtr scrn;
	AMDGPUEntPtr pAMDGPUEnt;
	struct drmmode_fb **fb_ptr;
	uint32_t handle;

	if (UNLIKELY(!pix))
		return NULL;

	fb_ptr = amdgpu_pixmap_get_fb_ptr(pix);
	if (fb_ptr && *fb_ptr)
		return *fb_ptr;

	if (amdgpu_pixmap_get_handle(pix, &handle)) {
		scrn = xf86ScreenToScrn(pix->drawable.pScreen);
		pAMDGPUEnt = AMDGPUEntPriv(scrn);

		if (!fb_ptr)
			fb_ptr = amdgpu_pixmap_get_fb_ptr(pix);

		if (fb_ptr) {
			*fb_ptr = amdgpu_fb_create(scrn, pAMDGPUEnt->fd,
			                           pix->drawable.width,
			                           pix->drawable.height,
			                           pix->devKind, handle);
		}
	}

	return fb_ptr ? *fb_ptr : NULL;
}

#endif
