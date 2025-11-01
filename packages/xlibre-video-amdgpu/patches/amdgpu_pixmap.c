/*
 * amdgpu_pixmap.c - Pixmap management for AMDGPU (non-glamor path)
 * Copyright © 2014-2024 Advanced Micro Devices, Inc.
 */

#include "config.h"
#include <xorg-server.h>
#include <xf86.h>

#include "amdgpu_pixmap.h"
#include "amdgpu_bo_helper.h"

#if defined(__GNUC__) || defined(__clang__)
# define AMDGPU_HOT  __attribute__((hot))
# define LIKELY(x)   __builtin_expect(!!(x), 1)
# define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
# define AMDGPU_HOT
# define LIKELY(x)   (x)
# define UNLIKELY(x) (x)
#endif

#define MAX_PIXMAP_DIM 32767

static PixmapPtr AMDGPU_HOT
amdgpu_pixmap_create(ScreenPtr screen, int w, int h, int depth, unsigned usage)
{
	ScrnInfoPtr scrn;
	AMDGPUInfoPtr info;
	struct amdgpu_pixmap *priv;
	PixmapPtr pixmap;
	int stride;

	if (!(usage & AMDGPU_CREATE_PIXMAP_DRI2))
		return fbCreatePixmap(screen, w, h, depth, usage);

	if (w > MAX_PIXMAP_DIM || h > MAX_PIXMAP_DIM)
		return NullPixmap;

	if (depth == 1)
		return fbCreatePixmap(screen, w, h, depth, usage);

	pixmap = fbCreatePixmap(screen, 0, 0, depth, usage);
	if (pixmap == NullPixmap)
		return pixmap;

	if (w && h) {
		priv = calloc(1, sizeof(struct amdgpu_pixmap));
		if (!priv)
			goto fallback_pixmap;

		scrn = xf86ScreenToScrn(screen);
		info = AMDGPUPTR(scrn);

		if (!info->use_glamor)
			usage |= AMDGPU_CREATE_PIXMAP_LINEAR;

		priv->bo = amdgpu_alloc_pixmap_bo(scrn, w, h, depth, usage,
		                                  pixmap->drawable.bitsPerPixel,
		                                  &stride);
		if (!priv->bo)
			goto fallback_priv;

		amdgpu_set_pixmap_private(pixmap, priv);

		if (amdgpu_bo_map(scrn, priv->bo)) {
			xf86DrvMsg(scrn->scrnIndex, X_ERROR,
			           "Failed to map pixmap BO\n");
			goto fallback_bo;
		}

		screen->ModifyPixmapHeader(pixmap, w, h, 0, 0, stride,
		                           priv->bo->cpu_ptr);
	}

	return pixmap;

fallback_bo:
	amdgpu_bo_unref(&priv->bo);
fallback_priv:
	free(priv);
fallback_pixmap:
	fbDestroyPixmap(pixmap);
	return fbCreatePixmap(screen, w, h, depth, usage);
}

static Bool AMDGPU_HOT
amdgpu_pixmap_destroy(PixmapPtr pixmap)
{
	if (pixmap->refcnt == 1)
		amdgpu_set_pixmap_bo(pixmap, NULL);

	return fbDestroyPixmap(pixmap);
}

Bool
amdgpu_pixmap_init(ScreenPtr screen)
{
	if (!dixRegisterPrivateKey(&amdgpu_pixmap_index, PRIVATE_PIXMAP, 0))
		return FALSE;

	screen->CreatePixmap = amdgpu_pixmap_create;
	screen->DestroyPixmap = amdgpu_pixmap_destroy;

	return TRUE;
}
