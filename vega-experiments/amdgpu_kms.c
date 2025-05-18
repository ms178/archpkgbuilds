/*
 * Copyright © 2009 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Authors:
 *    Dave Airlie <airlied@redhat.com>
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <errno.h>
#include <sys/ioctl.h>
#include <stdlib.h> /* For malloc/free/calloc if not implicitly included */
#include <string.h> /* For strerror if not implicitly included */
/* Driver data structures */
#include "amdgpu_drv.h"
#include "amdgpu_bo_helper.h"
#include "amdgpu_drm_queue.h"
#include "amdgpu_glamor.h"
#include "amdgpu_probe.h"
#include "micmap.h"
#include "mipointrst.h"
#include "regionstr.h" /* For RegionRec, RegionNumRects etc. */
#include "pixman.h"    /* For pixman_f_transform */


#include "amdgpu_version.h"
#include "shadow.h"
#include <xf86Priv.h> /* For xf86GetVerbosity, xf86DrvMsgVerb */

#if HAVE_PRESENT_H
#include <present.h>
#endif

/* DPMS */
#ifdef HAVE_XEXTPROTO_71
#include <X11/extensions/dpmsconst.h>
#else
#define DPMS_SERVER
#include <X11/extensions/dpms.h>
#endif

#include <X11/extensions/damageproto.h>

#include "amdgpu_bo_helper.h"
#include "amdgpu_pixmap.h"

#include <gbm.h>

/* Branch prediction hints */
#if (defined(__GNUC__) && (__GNUC__ >= 3)) || defined(__clang__)
#define likely(x)   __builtin_expect(!!(x),1)
#define unlikely(x) __builtin_expect(!!(x),0)
#else
#define likely(x)   (x)
#define unlikely(x) (x)
#endif


static DevPrivateKeyRec amdgpu_window_private_key;
static DevScreenPrivateKeyRec amdgpu_client_private_key;
DevScreenPrivateKeyRec amdgpu_device_private_key;

static Atom amdgpu_vrr_atom;
static Bool amdgpu_property_vectors_wrapped; /* Used by unwrap, needs to be file-static */
static Bool restore_property_vector;       /* Used by change/delete, needs to be file-static */
static int (*saved_change_property) (ClientPtr client);
static int (*saved_delete_property) (ClientPtr client);

static Bool amdgpu_setup_kernel_mem(ScreenPtr pScreen);

/* GLOBALS for idea #10 – thread-safe property-vector wrapping */
static volatile int vrr_prop_lock = 0;
static inline void vrr_lock  (void) { while (__sync_lock_test_and_set(&vrr_prop_lock,1)); }
static inline void vrr_unlock(void) { __sync_lock_release(&vrr_prop_lock); }


const OptionInfoRec AMDGPUOptions_KMS[] = {
	{OPTION_ACCEL, "Accel", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_SW_CURSOR, "SWcursor", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_PAGE_FLIP, "EnablePageFlip", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_SUBPIXEL_ORDER, "SubPixelOrder", OPTV_ANYSTR, {0}, FALSE},
	{OPTION_ZAPHOD_HEADS, "ZaphodHeads", OPTV_STRING, {0}, FALSE},
	{OPTION_ACCEL_METHOD, "AccelMethod", OPTV_STRING, {0}, FALSE},
	{OPTION_DRI3, "DRI3", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_DRI, "DRI", OPTV_INTEGER, {0}, FALSE},
	{OPTION_SHADOW_PRIMARY, "ShadowPrimary", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_TEAR_FREE, "TearFree", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_DELETE_DP12, "DeleteUnusedDP12Displays", OPTV_BOOLEAN, {0}, FALSE},
	{OPTION_VARIABLE_REFRESH, "VariableRefresh", OPTV_BOOLEAN, {0}, FALSE },
	{OPTION_ASYNC_FLIP_SECONDARIES, "AsyncFlipSecondaries", OPTV_BOOLEAN, {0}, FALSE},
	{-1, NULL, OPTV_NONE, {0}, FALSE}
};

const OptionInfoRec *AMDGPUOptionsWeak(void)
{
	return AMDGPUOptions_KMS;
}

static inline struct amdgpu_window_priv *get_window_priv(WindowPtr win) {
	return dixLookupPrivate(&win->devPrivates, &amdgpu_window_private_key);
}

static void
amdgpu_vrr_property_update(WindowPtr window, Bool variable_refresh)
{
	ScrnInfoPtr scrn = xf86ScreenToScrn(window->drawable.pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);

	get_window_priv(window)->variable_refresh = variable_refresh;

	if (info->flip_window == window &&
		info->drmmode.present_flipping)
		amdgpu_present_set_screen_vrr(scrn, variable_refresh);
}

/* idea #10 – protect property-vector wrappers */
static int amdgpu_change_property(ClientPtr client)
{
	vrr_lock();
	WindowPtr window;
	int ret;

	REQUEST(xChangePropertyReq);

	client->requestVector[X_ChangeProperty] = saved_change_property;
	ret = saved_change_property(client);

	if (!restore_property_vector) /* This check is inside the lock */
		client->requestVector[X_ChangeProperty] = amdgpu_change_property;

	if (ret == Success &&
		dixLookupWindow(&window, stuff->window, client, DixSetPropAccess) == Success &&
		stuff->property == amdgpu_vrr_atom &&
		xf86ScreenToScrn(window->drawable.pScreen)->PreInit == AMDGPUPreInit_KMS &&
		stuff->format == 32 && stuff->nUnits == 1)
	{
		amdgpu_vrr_property_update(window, ((uint32_t *)(stuff + 1))[0] != 0);
	}
	vrr_unlock();
	return ret;
}

static int amdgpu_delete_property(ClientPtr client)
{
	vrr_lock();
	WindowPtr window;
	int ret;

	REQUEST(xDeletePropertyReq);

	client->requestVector[X_DeleteProperty] = saved_delete_property;
	ret = saved_delete_property(client);

	if (!restore_property_vector) /* This check is inside the lock */
		client->requestVector[X_DeleteProperty] = amdgpu_delete_property;

	if (ret == Success &&
		dixLookupWindow(&window, stuff->window, client, DixSetPropAccess) == Success &&
		stuff->property == amdgpu_vrr_atom &&
		xf86ScreenToScrn(window->drawable.pScreen)->PreInit == AMDGPUPreInit_KMS)
	{
		amdgpu_vrr_property_update(window, FALSE);
	}
	vrr_unlock();
	return ret;
}

static void
amdgpu_unwrap_property_requests(ScrnInfoPtr scrn)
{
	vrr_lock();
	/* restore_property_vector logic is mostly handled by the individual wrappers now.
	 * This function ensures all known vectors are reset if wrapping was active.
	 */
	if (amdgpu_property_vectors_wrapped) { /* Check if we ever wrapped them */
		int i;
		if (ProcVector[X_ChangeProperty] == amdgpu_change_property)
			ProcVector[X_ChangeProperty] = saved_change_property;
		/* else: some other module might have wrapped it, or logic error.
		 * restore_property_vector flag should catch mismatches in individual handlers.
		 */

		if (ProcVector[X_DeleteProperty] == amdgpu_delete_property)
			ProcVector[X_DeleteProperty] = saved_delete_property;


		for (i = 0; i < currentMaxClients; i++) {
			if (clients[i]) { /* Check if client slot is active */
				if (clients[i]->requestVector[X_ChangeProperty] == amdgpu_change_property) {
					clients[i]->requestVector[X_ChangeProperty] = saved_change_property;
				}
				if (clients[i]->requestVector[X_DeleteProperty] == amdgpu_delete_property) {
					clients[i]->requestVector[X_DeleteProperty] = saved_delete_property;
				}
			}
		}
		amdgpu_property_vectors_wrapped = FALSE;
		restore_property_vector = FALSE; /* Reset the flag as we've unwrapped */
	}
	vrr_unlock();
}


extern _X_EXPORT int gAMDGPUEntityIndex;

static int getAMDGPUEntityIndex(void)
{
	return gAMDGPUEntityIndex;
}

AMDGPUEntPtr AMDGPUEntPriv(ScrnInfoPtr pScrn)
{
	DevUnion *pPriv;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	pPriv = xf86GetEntityPrivate(info->pEnt->index, getAMDGPUEntityIndex());
	return pPriv->ptr;
}

static Bool AMDGPUGetRec(ScrnInfoPtr pScrn)
{
	if (pScrn->driverPrivate)
		return TRUE;

	pScrn->driverPrivate = XNFcallocarray(sizeof(AMDGPUInfoRec), 1);
	return TRUE;
}

static void AMDGPUFreeRec(ScrnInfoPtr pScrn)
{
	DevUnion *pPriv;
	AMDGPUEntPtr pAMDGPUEnt;
	AMDGPUInfoPtr info;
	EntityInfoPtr pEnt;

	if (!pScrn)
		return;

	pEnt = xf86GetEntityInfo(pScrn->entityList[pScrn->numEntities - 1]);
	pPriv = xf86GetEntityPrivate(pEnt->index, gAMDGPUEntityIndex);
	pAMDGPUEnt = pPriv->ptr;

	info = AMDGPUPTR(pScrn);
	if (info) {
		pAMDGPUEnt->scrn[info->instance_id] = NULL;
		pAMDGPUEnt->num_scrns--;
		free(pScrn->driverPrivate);
		pScrn->driverPrivate = NULL;
	}

	if (pAMDGPUEnt->fd > 0) {
		/* DevUnion *pPriv; This re-declaration shadows. Use the outer one. */
		/* AMDGPUEntPtr pAMDGPUEnt; */
		pPriv = xf86GetEntityPrivate(pScrn->entityList[0],
									 getAMDGPUEntityIndex());

		pAMDGPUEnt = pPriv->ptr;
		pAMDGPUEnt->fd_ref--;
		if (!pAMDGPUEnt->fd_ref) {
			amdgpu_unwrap_property_requests(pScrn); /* Ensure unwrapped on final close */
			amdgpu_device_deinitialize(pAMDGPUEnt->pDev);
			amdgpu_kernel_close_fd(pAMDGPUEnt);
			free(pAMDGPUEnt->busid);
			free(pPriv->ptr);
			pPriv->ptr = NULL;
		}
	}

	free(pEnt);
}

Bool amdgpu_window_has_variable_refresh(WindowPtr win) {
	struct amdgpu_window_priv *priv = get_window_priv(win);
	return priv->variable_refresh;
}

static void *amdgpuShadowWindow(ScreenPtr screen, CARD32 row, CARD32 offset,
								int mode, CARD32 * size, void *closure)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(screen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	int stride;

	stride = (pScrn->displayWidth * pScrn->bitsPerPixel) / 8;
	*size = stride;

	if (unlikely(!info->front_buffer || !info->front_buffer->cpu_ptr)) return NULL;
	return ((uint8_t *) info->front_buffer->cpu_ptr + row * stride + offset);
}

static void
amdgpuUpdatePacked(ScreenPtr pScreen, shadowBufPtr pBuf)
{
	shadowUpdatePacked(pScreen, pBuf);
}

/* idea #5 – make this tiny helper inline + prefetch */
static inline Bool
callback_needs_flush(AMDGPUInfoPtr info,
					 struct amdgpu_client_priv *client_priv)
{
	if (unlikely(!client_priv)) return FALSE; /* Safety check */
		/* Prefetch next cache-line of struct to hide latency */
		__builtin_prefetch(client_priv, 0, 0); /* 0 for read, 0 for low locality - assuming check then potential write soon */
		/* signed difference allows wraparound-safe comparison */
		return (int)(client_priv->needs_flush - info->gpu_flushed) > 0;
}


static void
amdgpu_event_callback(CallbackListPtr *list,
					  pointer user_data, pointer call_data)
{
	EventInfoRec *eventinfo = call_data;
	ScrnInfoPtr pScrn = user_data;
	ScreenPtr pScreen = pScrn->pScreen;
	struct amdgpu_client_priv *client_priv =
	dixLookupScreenPrivate(&eventinfo->client->devPrivates,
						   &amdgpu_client_private_key, pScreen);
	struct amdgpu_client_priv *server_priv =
	dixLookupScreenPrivate(&serverClient->devPrivates,
						   &amdgpu_client_private_key, pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	int i;

	if (callback_needs_flush(info, client_priv) ||
		callback_needs_flush(info, server_priv))
		return;

	client_priv->needs_flush = info->gpu_flushed;
	server_priv->needs_flush = info->gpu_flushed;

	for (i = 0; i < eventinfo->count; i++) {
		if (eventinfo->events[i].u.u.type == info->callback_event_type) {
			client_priv->needs_flush++;
			server_priv->needs_flush++;
			return;
		}
	}
}

static void
amdgpu_flush_callback(CallbackListPtr *list,
					  pointer user_data, pointer call_data)
{
	ScrnInfoPtr pScrn = user_data;
	ScreenPtr pScreen = pScrn->pScreen;
	ClientPtr client = call_data ? call_data : serverClient;
	struct amdgpu_client_priv *client_priv =
	dixLookupScreenPrivate(&client->devPrivates,
						   &amdgpu_client_private_key, pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	if (pScrn->vtSema && callback_needs_flush(info, client_priv))
		amdgpu_glamor_flush(pScrn);
}

static Bool AMDGPUCreateScreenResources_KMS(ScreenPtr pScreen)
{
	ExtensionEntry *damage_ext;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	PixmapPtr pixmap;

	pScreen->CreateScreenResources = info->CreateScreenResources;
	if (unlikely(!(*pScreen->CreateScreenResources) (pScreen)))
		return FALSE;
	pScreen->CreateScreenResources = AMDGPUCreateScreenResources_KMS;

	if (dixPrivateKeyRegistered(rrPrivKey)) {
		rrScrPrivPtr rrScrPriv = rrGetScrPriv(pScreen);
		if (!pScreen->isGPU && !rrScrPriv->primaryOutput) {
			xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
			if (likely(xf86_config->num_output > 0 && xf86_config->output[0])) {
				rrScrPriv->primaryOutput = xf86_config->output[0]->randr_output;
				RROutputChanged(rrScrPriv->primaryOutput, FALSE);
				rrScrPriv->layoutChanged = TRUE;
			}
		}
		drmmode_uevent_init(pScrn, &info->drmmode);
	}

	if (unlikely(!drmmode_set_desired_modes(pScrn, &info->drmmode, pScreen->isGPU)))
		return FALSE;

	if (info->shadow_fb) {
		pixmap = pScreen->GetScreenPixmap(pScreen);
		if (unlikely(!shadowAdd(pScreen, pixmap, amdgpuUpdatePacked,
			amdgpuShadowWindow, 0, NULL)))
			return FALSE;
	}

	if (info->dri2.enabled || info->use_glamor) {
		if (info->front_buffer) {
			PixmapPtr pPix = pScreen->GetScreenPixmap(pScreen);
			if (unlikely(!amdgpu_set_pixmap_bo(pPix, info->front_buffer)))
				return FALSE;
		}
	}

	if (info->use_glamor)
		amdgpu_glamor_create_screen_resources(pScreen);

	info->callback_event_type = -1;
	if (!pScreen->isGPU && (damage_ext = CheckExtension("DAMAGE"))) {
		info->callback_event_type = damage_ext->eventBase + XDamageNotify;
		if (unlikely(!AddCallback(&FlushCallback, amdgpu_flush_callback, pScrn)))
			return FALSE;
		if (unlikely(!AddCallback(&EventCallback, amdgpu_event_callback, pScrn))) {
			DeleteCallback(&FlushCallback, amdgpu_flush_callback, pScrn);
			return FALSE;
		}
		if (unlikely(!dixRegisterScreenPrivateKey(&amdgpu_client_private_key, pScreen,
			PRIVATE_CLIENT, sizeof(struct amdgpu_client_priv)))) {
			DeleteCallback(&FlushCallback, amdgpu_flush_callback, pScrn);
		DeleteCallback(&EventCallback, amdgpu_event_callback, pScrn);
		return FALSE;
			}
	}

	if (info->vrr_support &&
		unlikely(!dixRegisterPrivateKey(&amdgpu_window_private_key,
										PRIVATE_WINDOW, sizeof(struct amdgpu_window_priv))))
		return FALSE;

	return TRUE;
}

static __attribute__((hot)) Bool
amdgpu_scanout_extents_intersect(xf86CrtcPtr xf86_crtc, BoxPtr extents)
{
	/* Coarse reject when no transforms are active and not a PRIME sink */
	if (!xf86_crtc->driverIsPerformingTransform &&
		!xf86_crtc->scrn->is_gpu)
	{
		if (extents->x2 <= xf86_crtc->x ||
			extents->x1 >= xf86_crtc->x + xf86_crtc->mode.HDisplay ||
			extents->y2 <= xf86_crtc->y ||
			extents->y1 >= xf86_crtc->y + xf86_crtc->mode.VDisplay)
			return FALSE;
	}

	/* Original detailed handling */
	if (xf86_crtc->scrn->is_gpu) {
		extents->x1 -= xf86_crtc->x;
		extents->y1 -= xf86_crtc->y;
		extents->x2 -= xf86_crtc->x;
		extents->y2 -= xf86_crtc->y;
	} else {
		extents->x1 -= xf86_crtc->filter_width  >> 1;
		extents->x2 += xf86_crtc->filter_width  >> 1;
		extents->y1 -= xf86_crtc->filter_height >> 1;
		extents->y2 += xf86_crtc->filter_height >> 1;

		if (xf86_crtc->driverIsPerformingTransform) /* e.g. RandR rotation */
			pixman_f_transform_bounds(&xf86_crtc->f_framebuffer_to_crtc, extents);
	}

	/* Clamp to CRTC's local 0,0 to HDisplay, VDisplay space */
	extents->x1 = max(extents->x1, 0);
	extents->y1 = max(extents->y1, 0);
	extents->x2 = min(extents->x2, xf86_crtc->mode.HDisplay);
	extents->y2 = min(extents->y2, xf86_crtc->mode.VDisplay);

	return (extents->x1 < extents->x2 && extents->y1 < extents->y2);
}

static __attribute__((hot)) RegionPtr
transform_region(RegionPtr               region,
				 struct pixman_f_transform *transform,
				 int                      w,
				 int                      h)
{
	const int   nboxes = RegionNumRects(region);
	const BoxPtr boxes  = RegionRects(region);

	/* Fast-path: empty input → empty output */
	if (nboxes == 0)
		return RegionCreate(NULL, 0);

	/* Re-use static scratch for the common case, malloc for large ones */
	enum { SCRATCH_RECTS = 64 };
	static xRectangle rect_static[SCRATCH_RECTS];
	xRectangle       *rects = (nboxes <= SCRATCH_RECTS)
	? rect_static
	: malloc(sizeof(xRectangle) * nboxes);

	if (!rects) {                             /* OOM fallback */
		xf86Msg(X_WARNING,
				"amdgpu: transform_region OOM, using duplicate region\n");
		return RegionDuplicate(region);
	}

	int nrects = 0;
	for (int i = 0; i < nboxes; ++i) {
		BoxRec b = boxes[i];                  /* copy – transform modifies */

		pixman_f_transform_bounds(transform, &b);

		if (b.x1 < 0) b.x1 = 0;
		if (b.y1 < 0) b.y1 = 0;
		if (b.x2 >  w) b.x2 =  w;
		if (b.y2 >  h) b.y2 =  h;

		if (b.x1 >= b.x2 || b.y1 >= b.y2)
			continue;

		rects[nrects].x      = b.x1;
		rects[nrects].y      = b.y1;
		rects[nrects].width  = b.x2 - b.x1;
		rects[nrects].height = b.y2 - b.y1;
		nrects++;
	}

	RegionPtr out = RegionFromRects(nrects, rects, CT_UNSORTED);
	if (rects != rect_static)
		free(rects);

	return out;
}

static void
amdgpu_sync_scanout_pixmaps(xf86CrtcPtr xf86_crtc, RegionPtr new_region,
							int scanout_id)
{
	drmmode_crtc_private_ptr drmmode_crtc = xf86_crtc->driver_private;
	DrawablePtr dst   = &drmmode_crtc->scanout[scanout_id]->drawable;
	DrawablePtr src   = &drmmode_crtc->scanout[scanout_id ^ 1]->drawable;
	RegionPtr   last  = &drmmode_crtc->scanout_last_region;
	ScreenPtr   pScr  = xf86_crtc->scrn->pScreen;
	RegionPtr   clip  = NULL; /* Initialize to NULL */

	RegionRec   remaining;
	RegionNull(&remaining); /* Initializes remaining.extents and remaining.data */
	RegionSubtract(&remaining, last, new_region);

	if (RegionNil(&remaining)) /* Check if there's anything to sync */
		goto done;

	BoxRec ext = *RegionExtents(&remaining); /* Get extents of the difference */
	if (!amdgpu_scanout_extents_intersect(xf86_crtc, &ext)) /* Intersect with CRTC view */
		goto done;

	/* ext is now the CRTC-local area to update */

	if (xf86_crtc->driverIsPerformingTransform) {
		/* 'remaining' is in global framebuffer coordinates. Transform it to CRTC-local. */
		clip = transform_region(&remaining,
								&xf86_crtc->f_framebuffer_to_crtc,
						  dst->width, dst->height);
	} else {
		/* 'remaining' is global, translate to CRTC-local for clipping */
		clip = RegionDuplicate(&remaining);
		if (clip)
			RegionTranslate(clip, -xf86_crtc->x, -xf86_crtc->y);
	}

	if (!clip)
		goto done;

	GCPtr gc = GetScratchGC(dst->depth, pScr);
	if (gc) {
		(*gc->funcs->ChangeClip)(gc, CT_REGION, clip, 0);
		ValidateGC(dst, gc);
		clip = NULL;
		(*gc->ops->CopyArea)(src, dst, gc,
							 0, 0, dst->width, dst->height, 0, 0);
		FreeScratchGC(gc);
	}

	done:
	if (clip)
		RegionDestroy(clip);
	RegionUninit(&remaining);
}

static void
amdgpu_scanout_flip_abort(xf86CrtcPtr crtc, void *event_data)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(crtc->scrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	struct drmmode_fb *fb = event_data;

	drmmode_crtc->scanout_update_pending = 0;

	if (drmmode_crtc->flip_pending == fb) {
		drmmode_fb_reference(pAMDGPUEnt->fd, &drmmode_crtc->flip_pending, NULL);
	}
}

static void
amdgpu_scanout_flip_handler(xf86CrtcPtr crtc, uint32_t msc, uint64_t usec,
							void *event_data)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(crtc->scrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	struct drmmode_fb *fb = event_data;

	drmmode_fb_reference(pAMDGPUEnt->fd, &drmmode_crtc->fb, fb);
	amdgpu_scanout_flip_abort(crtc, event_data); /* This also unsets flip_pending if it was fb */
}


static __attribute__((hot)) RegionPtr
dirty_region(PixmapDirtyUpdatePtr dirty) /* Marked hot */
{
	RegionPtr damageregion = DamageRegion(dirty->damage);
	RegionPtr dstregion;

	if (dirty->rotation != RR_Rotate_0) {
		dstregion = transform_region(damageregion,
									 &dirty->f_inverse,
							   dirty->secondary_dst->drawable.width,
							   dirty->secondary_dst->drawable.height);
	} else {
		RegionRec pixregion;
		dstregion = RegionDuplicate(damageregion);
		if (unlikely(!dstregion)) return NULL; /* Handle OOM from RegionDuplicate */

			RegionTranslate(dstregion, -dirty->x, -dirty->y);
		PixmapRegionInit(&pixregion, dirty->secondary_dst);
		RegionIntersect(dstregion, dstregion, &pixregion);
		RegionUninit(&pixregion);
	}
	return dstregion;
}

static __attribute__((hot)) void /* Marked hot */
redisplay_dirty(PixmapDirtyUpdatePtr dirty, RegionPtr region)
{
	ScrnInfoPtr src_scrn =
	xf86ScreenToScrn(amdgpu_dirty_src_drawable(dirty)->pScreen);

	if (RegionNil(region))
		goto out;

	if (dirty->secondary_dst->primary_pixmap)
		DamageRegionAppend(&dirty->secondary_dst->drawable, region);

	PixmapSyncDirtyHelper(dirty);

	amdgpu_glamor_flush(src_scrn);
	if (dirty->secondary_dst->primary_pixmap)
		DamageRegionProcessPending(&dirty->secondary_dst->drawable);
	out:
	DamageEmpty(dirty->damage);
}

static __attribute__((cold)) void /* Marked cold */
amdgpu_prime_scanout_update_abort(xf86CrtcPtr crtc, void *event_data)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_crtc->scanout_update_pending = 0;
}

void
amdgpu_sync_shared_pixmap(PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr primary_screen = amdgpu_dirty_primary(dirty);
	PixmapDirtyUpdatePtr ent;
	RegionPtr region_obj; /* Renamed */

	xorg_list_for_each_entry(ent, &primary_screen->pixmap_dirty_list, ent) {
		if (!amdgpu_dirty_src_equals(dirty, ent->secondary_dst))
			continue;

		region_obj = dirty_region(ent);
		if (unlikely(!region_obj)) continue; /* Skip if OOM in dirty_region */
			redisplay_dirty(ent, region_obj);
		RegionDestroy(region_obj);
	}
}


#if HAS_SYNC_SHARED_PIXMAP
static Bool
primary_has_sync_shared_pixmap(ScrnInfoPtr scrn, PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr primary_screen = amdgpu_dirty_primary(dirty);
	return primary_screen->SyncSharedPixmap != NULL;
}

static Bool
secondary_has_sync_shared_pixmap(ScrnInfoPtr scrn, PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr secondary_screen = dirty->secondary_dst->drawable.pScreen;
	return secondary_screen->SyncSharedPixmap != NULL;
}

static void
call_sync_shared_pixmap(PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr primary_screen = amdgpu_dirty_primary(dirty);
	primary_screen->SyncSharedPixmap(dirty);
}
#else
static Bool
primary_has_sync_shared_pixmap(ScrnInfoPtr scrn, PixmapDirtyUpdatePtr dirty)
{
	ScrnInfoPtr primary_scrn = xf86ScreenToScrn(amdgpu_dirty_primary(dirty));
	return primary_scrn->driverName == scrn->driverName;
}

static Bool
secondary_has_sync_shared_pixmap(ScrnInfoPtr scrn, PixmapDirtyUpdatePtr dirty)
{
	ScrnInfoPtr secondary_scrn = xf86ScreenToScrn(dirty->secondary_dst->drawable.pScreen);
	return secondary_scrn->driverName == scrn->driverName;
}

static void
call_sync_shared_pixmap(PixmapDirtyUpdatePtr dirty)
{
	amdgpu_sync_shared_pixmap(dirty);
}
#endif


static xf86CrtcPtr
amdgpu_prime_dirty_to_crtc(PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr screen = dirty->secondary_dst->drawable.pScreen;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);
	int c;

	for (c = 0; c < xf86_config->num_crtc; c++) {
		xf86CrtcPtr xf86_crtc = xf86_config->crtc[c];
		if (unlikely(!xf86_crtc)) continue; /* Safety check */
			drmmode_crtc_private_ptr drmmode_crtc = xf86_crtc->driver_private;
		if (unlikely(!drmmode_crtc)) continue; /* Safety check */

			if (amdgpu_dirty_src_equals(dirty, drmmode_crtc->prime_scanout_pixmap))
				return xf86_crtc;
	}
	return NULL;
}

static Bool
amdgpu_prime_scanout_do_update(xf86CrtcPtr crtc, unsigned scanout_id)
{
	ScrnInfoPtr scrn = crtc->scrn;
	ScreenPtr screen = scrn->pScreen;
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	PixmapDirtyUpdatePtr dirty;
	Bool ret = FALSE;

	xorg_list_for_each_entry(dirty, &screen->pixmap_dirty_list, ent) {
		if (amdgpu_dirty_src_equals(dirty, drmmode_crtc->prime_scanout_pixmap)) {
			RegionPtr region_obj; /* Renamed */

			if (primary_has_sync_shared_pixmap(scrn, dirty))
				call_sync_shared_pixmap(dirty);

			region_obj = dirty_region(dirty);
			if (unlikely(!region_obj)) break; /* OOM */

				if (RegionNil(region_obj))
					goto destroy_region;

			if (drmmode_crtc->tear_free) {
				RegionTranslate(region_obj, crtc->x, crtc->y);
				amdgpu_sync_scanout_pixmaps(crtc, region_obj, scanout_id);
				amdgpu_glamor_flush(scrn);
				RegionCopy(&drmmode_crtc->scanout_last_region, region_obj);
				RegionTranslate(region_obj, -crtc->x, -crtc->y);
				dirty->secondary_dst = drmmode_crtc->scanout[scanout_id];
			}
			redisplay_dirty(dirty, region_obj);
			ret = TRUE;
			destroy_region:
			RegionDestroy(region_obj);
			break;
		}
	}
	return ret;
}

static __attribute__((cold)) void /* Marked cold */
amdgpu_prime_scanout_update_handler(xf86CrtcPtr crtc, uint32_t frame, uint64_t usec,
									void *event_data)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	amdgpu_prime_scanout_do_update(crtc, 0);
	drmmode_crtc->scanout_update_pending = 0;
}

static __attribute__((hot)) void /* Marked hot */
amdgpu_prime_scanout_update(PixmapDirtyUpdatePtr dirty)
{
	ScreenPtr screen = dirty->secondary_dst->drawable.pScreen;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	xf86CrtcPtr xf86_crtc = amdgpu_prime_dirty_to_crtc(dirty);
	drmmode_crtc_private_ptr drmmode_crtc;
	uintptr_t drm_queue_seq;

	if (unlikely(!xf86_crtc || !xf86_crtc->enabled))
		return;

	drmmode_crtc = xf86_crtc->driver_private;
	if (unlikely(drmmode_crtc->scanout_update_pending ||
		!drmmode_crtc->scanout[drmmode_crtc->scanout_id] ||
		drmmode_crtc->dpms_mode != DPMSModeOn))
		return;

	drm_queue_seq = amdgpu_drm_queue_alloc(xf86_crtc,
										   AMDGPU_DRM_QUEUE_CLIENT_DEFAULT,
										AMDGPU_DRM_QUEUE_ID_DEFAULT, NULL,
										amdgpu_prime_scanout_update_handler,
										amdgpu_prime_scanout_update_abort,
										FALSE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "amdgpu_drm_queue_alloc failed for PRIME update\n");
		amdgpu_prime_scanout_update_handler(xf86_crtc, 0, 0, NULL);
		return;
	}
	drmmode_crtc->scanout_update_pending = drm_queue_seq;

	if (unlikely(!drmmode_wait_vblank(xf86_crtc, DRM_VBLANK_RELATIVE | DRM_VBLANK_EVENT,
		1, drm_queue_seq, NULL, NULL))) {
		if (!(drmmode_crtc->scanout_status & DRMMODE_SCANOUT_VBLANK_FAILED)) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
					   "drmmode_wait_vblank failed for PRIME update: %s\n",
			  strerror(errno));
			drmmode_crtc->scanout_status |= DRMMODE_SCANOUT_VBLANK_FAILED;
		}
		drmmode_crtc->drmmode->event_context.vblank_handler(pAMDGPUEnt->fd, 0, 0, 0, (void*)drm_queue_seq);
	drmmode_crtc->wait_flip_nesting_level++;
	amdgpu_drm_queue_handle_deferred(xf86_crtc);
	return;
		}

		if (drmmode_crtc->scanout_status == (DRMMODE_SCANOUT_FLIP_FAILED | DRMMODE_SCANOUT_VBLANK_FAILED)) {
			xf86_crtc->funcs->set_mode_major(xf86_crtc, &xf86_crtc->mode,
											 xf86_crtc->rotation, xf86_crtc->x, xf86_crtc->y);
		}
		drmmode_crtc->scanout_status &= ~DRMMODE_SCANOUT_VBLANK_FAILED;
}

static __attribute__((hot)) void /* Marked hot */
amdgpu_prime_scanout_flip(PixmapDirtyUpdatePtr ent)
{
	ScreenPtr screen = ent->secondary_dst->drawable.pScreen;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	xf86CrtcPtr crtc = amdgpu_prime_dirty_to_crtc(ent);
	drmmode_crtc_private_ptr drmmode_crtc;
	uintptr_t drm_queue_seq;
	unsigned scanout_id;
	struct drmmode_fb *fb;

	if (unlikely(!crtc || !crtc->enabled)) return;

	drmmode_crtc = crtc->driver_private;
	scanout_id = drmmode_crtc->scanout_id ^ 1;

	if (unlikely(drmmode_crtc->scanout_update_pending ||
		!drmmode_crtc->scanout[scanout_id] ||
		drmmode_crtc->dpms_mode != DPMSModeOn))
		return;

	if (unlikely(!amdgpu_prime_scanout_do_update(crtc, scanout_id)))
		return;

	fb = amdgpu_pixmap_get_fb(drmmode_crtc->scanout[scanout_id]);
	if (unlikely(!fb)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "Failed to get FB for PRIME flip.\n");
		return;
	}

	drm_queue_seq = amdgpu_drm_queue_alloc(crtc, AMDGPU_DRM_QUEUE_CLIENT_DEFAULT,
										   AMDGPU_DRM_QUEUE_ID_DEFAULT, fb,
										amdgpu_scanout_flip_handler,
										amdgpu_scanout_flip_abort, TRUE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "Allocating DRM event queue entry failed for PRIME flip.\n");
		/* Manually unref fb if queue_alloc failed, as abort handler won't be called */
		if (fb) drmmode_fb_reference(pAMDGPUEnt->fd, &fb, NULL); /* Check if fb is still this one */
			return;
	}

	if (unlikely(drmmode_page_flip_target_relative(pAMDGPUEnt, drmmode_crtc,
		fb->handle, 0, drm_queue_seq, 1) != 0)) {
		if (!(drmmode_crtc->scanout_status & DRMMODE_SCANOUT_FLIP_FAILED)) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
					   "flip queue failed in %s: %s, TearFree inactive\n", __func__, strerror(errno));
			drmmode_crtc->scanout_status |= DRMMODE_SCANOUT_FLIP_FAILED;
		}
		amdgpu_drm_abort_entry(drm_queue_seq); /* This will call amdgpu_scanout_flip_abort, unref'ing fb */
		return;
		}

		if (drmmode_crtc->scanout_status & DRMMODE_SCANOUT_FLIP_FAILED) {
			xf86DrvMsg(scrn->scrnIndex, X_INFO, "TearFree active again\n");
			drmmode_crtc->scanout_status &= ~DRMMODE_SCANOUT_FLIP_FAILED;
		}
		drmmode_crtc->scanout_id = scanout_id;
		drmmode_crtc->scanout_update_pending = drm_queue_seq;
		drmmode_fb_reference(pAMDGPUEnt->fd, &drmmode_crtc->flip_pending, fb);
}

static __attribute__((hot)) void /* Marked hot */
amdgpu_dirty_update(ScrnInfoPtr scrn)
{
	ScreenPtr screen = scrn->pScreen;
	PixmapDirtyUpdatePtr ent;
	RegionPtr region_obj; /* Renamed */

	xorg_list_for_each_entry(ent, &screen->pixmap_dirty_list, ent) {
		if (screen->isGPU) {
			PixmapDirtyUpdatePtr region_ent = ent;
			if (primary_has_sync_shared_pixmap(scrn, ent)) {
				ScreenPtr primary_screen = amdgpu_dirty_primary(ent);
				xorg_list_for_each_entry(region_ent, &primary_screen->pixmap_dirty_list, ent) {
					if (amdgpu_dirty_src_equals(ent, region_ent->secondary_dst))
						break;
				}
			}
			region_obj = dirty_region(region_ent);
			if (unlikely(!region_obj)) continue;

			if (RegionNotEmpty(region_obj)) {
				xf86CrtcPtr crtc = amdgpu_prime_dirty_to_crtc(ent);
				drmmode_crtc_private_ptr drmmode_crtc = NULL;
				if (crtc) drmmode_crtc = crtc->driver_private;

				if (drmmode_crtc && drmmode_crtc->tear_free)
					amdgpu_prime_scanout_flip(ent);
				else
					amdgpu_prime_scanout_update(ent);
			} else {
				DamageEmpty(region_ent->damage);
			}
			RegionDestroy(region_obj);
		} else {
			if (secondary_has_sync_shared_pixmap(scrn, ent)) continue;
			region_obj = dirty_region(ent);
			if (unlikely(!region_obj)) continue;
			redisplay_dirty(ent, region_obj);
			RegionDestroy(region_obj);
		}
	}
}

static void
amdgpuSourceValidate(DrawablePtr draw, int x, int y, int w, int h,
					 unsigned int subWindowMode)
{
}

Bool
amdgpu_scanout_do_update(xf86CrtcPtr xf86_crtc, int scanout_id,
						 PixmapPtr   src_pix, BoxRec extents_fb) /* extents_fb is in framebuffer coords */
{
	drmmode_crtc_private_ptr drmmode_crtc = xf86_crtc->driver_private;
	ScrnInfoPtr scrn = xf86_crtc->scrn;
	ScreenPtr   pScr = scrn->pScreen;
	DrawablePtr dst_draw;
	BoxRec extents_crtc = extents_fb; /* Copy to modify for CRTC-local intersection */

	if (unlikely(!xf86_crtc->enabled || !drmmode_crtc->scanout[scanout_id] ||
		extents_crtc.x1 >= extents_crtc.x2 || extents_crtc.y1 >= extents_crtc.y2))
		return FALSE;

	dst_draw = &drmmode_crtc->scanout[scanout_id]->drawable;

	if (!amdgpu_scanout_extents_intersect(xf86_crtc, &extents_crtc))
		return FALSE;

	/* extents_crtc is now the CRTC-local area to update */

	if (drmmode_crtc->tear_free) {
		RegionRec fb_region_to_sync;
		fb_region_to_sync.extents = extents_crtc; /* Start with CRTC-local intersected rect */
		fb_region_to_sync.data = NULL;

		if (!scrn->is_gpu && !xf86_crtc->driverIsPerformingTransform) {
			/* If primary GPU and no RandR transform, add CRTC offsets */
			fb_region_to_sync.extents.x1 += xf86_crtc->x;
			fb_region_to_sync.extents.x2 += xf86_crtc->x;
			fb_region_to_sync.extents.y1 += xf86_crtc->y;
			fb_region_to_sync.extents.y2 += xf86_crtc->y;
		} else if (xf86_crtc->driverIsPerformingTransform) {
			fb_region_to_sync.extents = extents_fb;
		}

		amdgpu_sync_scanout_pixmaps(xf86_crtc, &(RegionRec){.extents = extents_fb, .data = NULL}, scanout_id);
		RegionCopy(&drmmode_crtc->scanout_last_region, &(RegionRec){.extents = extents_fb, .data = NULL});
	}

	if (xf86_crtc->driverIsPerformingTransform) {
		SourceValidateProcPtr SourceValidate = pScr->SourceValidate;
		PictFormatPtr format = PictureWindowFormat(pScr->root);
		int error;
		PicturePtr src_pic, dst_pic; /* Renamed */

		src_pic = CreatePicture(None, &src_pix->drawable, format, 0L, NULL, serverClient, &error);
		if (unlikely(!src_pic)) { ErrorF("Failed to create source picture\n"); return TRUE; } /* Still an update, but logged */

			dst_pic = CreatePicture(None, dst_draw, format, 0L, NULL, serverClient, &error);
			if (unlikely(!dst_pic)) { ErrorF("Failed to create dest picture\n"); FreePicture(src_pic, None); return TRUE; }

			error = SetPictureTransform(src_pic, &xf86_crtc->crtc_to_framebuffer);
			if (unlikely(error)) { ErrorF("SetPictureTransform failed\n"); goto free_pictures; }

			if (xf86_crtc->filter)
				SetPicturePictFilter(src_pic, xf86_crtc->filter, xf86_crtc->params, xf86_crtc->nparams);

		pScr->SourceValidate = amdgpuSourceValidate;
		CompositePicture(PictOpSrc,
						 src_pic, NULL, dst_pic,
				   extents_crtc.x1, extents_crtc.y1, /* srcX/Y for Composite are after transform */
				   0, 0,                             /* maskX/Y */
				   extents_crtc.x1, extents_crtc.y1, /* dstX/Y */
				   extents_crtc.x2 - extents_crtc.x1, /* width */
				   extents_crtc.y2 - extents_crtc.y1);/* height */
		pScr->SourceValidate = SourceValidate;

		free_pictures:
		FreePicture(dst_pic, None);
		FreePicture(src_pic, None);
	} else { /* Not driverIsPerformingTransform */
		GCPtr gc = GetScratchGC(dst_draw->depth, pScr);
		if (likely(gc)) {
			ValidateGC(dst_draw, gc);
			(*gc->ops->CopyArea)(&src_pix->drawable, dst_draw, gc,
								 xf86_crtc->x + extents_crtc.x1, /* srcX in framebuffer coords */
						xf86_crtc->y + extents_crtc.y1, /* srcY in framebuffer coords */
						extents_crtc.x2 - extents_crtc.x1, /* width */
						extents_crtc.y2 - extents_crtc.y1, /* height */
						extents_crtc.x1, /* dstX in CRTC-local scanout pixmap */
						extents_crtc.y1);/* dstY in CRTC-local scanout pixmap */
			FreeScratchGC(gc);
		}
	}
	return TRUE;
}

static __attribute__((cold)) void /* Marked cold */
amdgpu_scanout_update_abort(xf86CrtcPtr crtc, void *event_data)
{
	drmmode_crtc_private_ptr drmmode_crtc = event_data; /* event_data is drmmode_crtc itself */
	drmmode_crtc->scanout_update_pending = 0;
}

static __attribute__((cold)) void /* Marked cold */
amdgpu_scanout_update_handler(xf86CrtcPtr crtc, uint32_t frame, uint64_t usec, void *event_data)
{
	drmmode_crtc_private_ptr drmmode_crtc = event_data;
	ScreenPtr screen = crtc->scrn->pScreen;
	RegionPtr region_obj = DamageRegion(drmmode_crtc->scanout_damage); /* Renamed */

	if (likely(crtc->enabled && !drmmode_crtc->flip_pending && drmmode_crtc->dpms_mode == DPMSModeOn)) {
		/* Note: RegionExtents(region_obj) returns a pointer to box within region_obj,
		 * amdgpu_scanout_do_update will modify its copy of extents.
		 */
		if (amdgpu_scanout_do_update(crtc, drmmode_crtc->scanout_id,
			screen->GetWindowPixmap(screen->root),
									 region_obj->extents)) { /* Pass extents directly */
										 amdgpu_glamor_flush(crtc->scrn);
										 RegionEmpty(region_obj); /* Region is modified by amdgpu_scanout_extents_intersect via amdgpu_scanout_do_update */
									 }
	}
	amdgpu_scanout_update_abort(crtc, event_data); /* Pass drmmode_crtc as event_data */
}

/* idea #7 – skip vblank ioctl when nothing to do */
static __attribute__((hot)) void
amdgpu_scanout_update(xf86CrtcPtr xf86_crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = xf86_crtc->driver_private;
	ScrnInfoPtr scrn = xf86_crtc->scrn;
	AMDGPUEntPtr pEnt = AMDGPUEntPriv(scrn); /* Renamed pAMDGPUEnt */
	DamagePtr pDamage;
	RegionPtr pRegion;
	BoxRec extents_copy; /* Use a copy for amdgpu_scanout_extents_intersect */

	if (unlikely(!xf86_crtc->enabled || drmmode_crtc->scanout_update_pending ||
		drmmode_crtc->flip_pending || drmmode_crtc->dpms_mode != DPMSModeOn))
		return;

	pDamage = drmmode_crtc->scanout_damage;
	if (unlikely(!pDamage)) return;

	pRegion = DamageRegion(pDamage);
	if (unlikely(!RegionNotEmpty(pRegion))) /* EARLY OUT saves ioctl */
		return;

	extents_copy = *RegionExtents(pRegion); /* Make a copy */
	if (!amdgpu_scanout_extents_intersect(xf86_crtc, &extents_copy)) {
		RegionEmpty(pRegion); /* Original region is emptied if no intersection */
		return;
	}

	uintptr_t drm_queue_seq = amdgpu_drm_queue_alloc(xf86_crtc,
													 AMDGPU_DRM_QUEUE_CLIENT_DEFAULT,
												  AMDGPU_DRM_QUEUE_ID_DEFAULT,
												  drmmode_crtc, /* Pass drmmode_crtc as data */
												  amdgpu_scanout_update_handler,
												  amdgpu_scanout_update_abort,
												  FALSE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "amdgpu_drm_queue_alloc failed for scanout update\n");
		amdgpu_scanout_update_handler(xf86_crtc, 0, 0, drmmode_crtc);
		return;
	}
	drmmode_crtc->scanout_update_pending = drm_queue_seq;

	if (unlikely(!drmmode_wait_vblank(xf86_crtc, DRM_VBLANK_RELATIVE | DRM_VBLANK_EVENT,
		1, drm_queue_seq, NULL, NULL))) {
		if (!(drmmode_crtc->scanout_status & DRMMODE_SCANOUT_VBLANK_FAILED)) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
					   "drmmode_wait_vblank failed for scanout update: %s\n", strerror(errno));
			drmmode_crtc->scanout_status |= DRMMODE_SCANOUT_VBLANK_FAILED;
		}
		drmmode_crtc->drmmode->event_context.vblank_handler(
			pEnt->fd, 0, 0, 0, (void*)drm_queue_seq);
		drmmode_crtc->wait_flip_nesting_level++;
	amdgpu_drm_queue_handle_deferred(xf86_crtc);
	return;
		}

		if (drmmode_crtc->scanout_status ==
			(DRMMODE_SCANOUT_FLIP_FAILED | DRMMODE_SCANOUT_VBLANK_FAILED)) {
			xf86_crtc->funcs->set_mode_major(xf86_crtc, &xf86_crtc->mode,
											 xf86_crtc->rotation, xf86_crtc->x, xf86_crtc->y);
			}
			drmmode_crtc->scanout_status &= ~DRMMODE_SCANOUT_VBLANK_FAILED;
}


static __attribute__((hot)) void
amdgpu_scanout_flip(ScreenPtr pScreen, AMDGPUInfoPtr info,
					xf86CrtcPtr xf86_crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = xf86_crtc->driver_private;
	RegionPtr region_ptr = DamageRegion(drmmode_crtc->scanout_damage); /* Renamed region */
	ScrnInfoPtr scrn = xf86_crtc->scrn;
	AMDGPUEntPtr pEnt = AMDGPUEntPriv(scrn);
	uintptr_t drm_queue_seq;
	unsigned scanout_id;
	struct drmmode_fb *fb;

	if (unlikely(drmmode_crtc->scanout_update_pending ||
		drmmode_crtc->flip_pending ||
		drmmode_crtc->dpms_mode != DPMSModeOn))
		return;

	scanout_id = drmmode_crtc->scanout_id ^ 1;
	if (unlikely(!amdgpu_scanout_do_update(xf86_crtc, scanout_id,
		pScreen->GetWindowPixmap(pScreen->root),
										   region_ptr->extents))) /* Pass the actual extents */
	return;

	amdgpu_glamor_flush(scrn);
	RegionEmpty(region_ptr);

	fb = amdgpu_pixmap_get_fb(drmmode_crtc->scanout[scanout_id]);
	if (unlikely(!fb)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "Failed to get FB for scanout flip.\n");
		return;
	}

	drm_queue_seq = amdgpu_drm_queue_alloc(xf86_crtc,
										   AMDGPU_DRM_QUEUE_CLIENT_DEFAULT,
										AMDGPU_DRM_QUEUE_ID_DEFAULT,
										fb,
										amdgpu_scanout_flip_handler,
										amdgpu_scanout_flip_abort, TRUE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "Allocating DRM event queue entry failed.\n");
		drmmode_fb_reference(pEnt->fd, &fb, NULL); /* Unref since it won't be passed to handler */
		return;
	}

	if (unlikely(drmmode_page_flip_target_relative(pEnt, drmmode_crtc,
		fb->handle, 0,
		drm_queue_seq, 1) != 0)) {
		if (!(drmmode_crtc->scanout_status & DRMMODE_SCANOUT_FLIP_FAILED)) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
					   "flip queue failed in %s: %s, TearFree inactive\n",
			  __func__, strerror(errno));
			drmmode_crtc->scanout_status |= DRMMODE_SCANOUT_FLIP_FAILED;
		}
		amdgpu_drm_abort_entry(drm_queue_seq); /* This will call amdgpu_scanout_flip_abort */
		/* If flip failed, copy current damage to last_region before trying a non-flip update */
		RegionCopy(&drmmode_crtc->scanout_last_region, DamageRegion(drmmode_crtc->scanout_damage));
		RegionEmpty(&drmmode_crtc->scanout_last_region); /* Should be empty before non-flip update */
		amdgpu_scanout_update(xf86_crtc);
		drmmode_crtc_scanout_destroy(&drmmode_crtc->scanout[scanout_id]);
		drmmode_crtc->tear_free = FALSE;
		return;
		}

		if (drmmode_crtc->scanout_status & DRMMODE_SCANOUT_FLIP_FAILED) {
			xf86DrvMsg(scrn->scrnIndex, X_INFO, "TearFree active again\n");
			drmmode_crtc->scanout_status &= ~DRMMODE_SCANOUT_FLIP_FAILED;
		}

		drmmode_crtc->scanout_id            = scanout_id;
		drmmode_crtc->scanout_update_pending = drm_queue_seq;
		drmmode_fb_reference(pEnt->fd, &drmmode_crtc->flip_pending, fb);
}

__attribute__((hot))
static void
AMDGPUBlockHandler_KMS(ScreenPtr pScreen, pointer timeout)
{
	ScrnInfoPtr           pScrn       = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr         info        = AMDGPUPTR(pScrn);
	xf86CrtcConfigPtr     xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	int                   i;

	if (unlikely(info->BlockHandler)) {
		/* Temporarily restore and invoke the original handler. */
		ScreenBlockHandlerProcPtr saved = info->BlockHandler;

		pScreen->BlockHandler = saved;          /* unwrap            */
		saved(pScreen, timeout);                /* … call through …  */
		pScreen->BlockHandler = AMDGPUBlockHandler_KMS; /* re-wrap    */
	}

	if (unlikely(!pScrn->vtSema))
		return;

	if (pScreen->isGPU)
		goto flush_and_dirty;

	for (i = 0; i < xf86_config->num_crtc; ++i) {
		xf86CrtcPtr crtc = xf86_config->crtc[i];
		if (unlikely(!crtc))
			continue;

		drmmode_crtc_private_ptr drmmode = crtc->driver_private;
		if (unlikely(!drmmode))
			continue;

		if (drmmode->rotate)
			continue;

			if (drmmode->tear_free)
				amdgpu_scanout_flip(pScreen, info, crtc);
		else if (drmmode->scanout[drmmode->scanout_id])
			amdgpu_scanout_update(crtc);
	}

	flush_and_dirty:
	#if XORG_VERSION_CURRENT < XORG_VERSION_NUMERIC(1,19,0,0,0)
	if (info->use_glamor)
		amdgpu_glamor_flush(pScrn);
	#endif
	amdgpu_dirty_update(pScrn);
}

static void
AMDGPUBlockHandler_Install(ScreenPtr pScreen)
{
	ScrnInfoPtr   pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info  = AMDGPUPTR(pScrn);

	if (!info->BlockHandler) {
		info->BlockHandler  = pScreen->BlockHandler;
		pScreen->BlockHandler = AMDGPUBlockHandler_KMS;
	}
}

static Bool AMDGPUPreInitVisual(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	if (unlikely(!xf86SetDepthBpp(pScrn, 0, 0, 0, Support32bppFb))) return FALSE;
	switch (pScrn->depth) {
		case 8: case 15: case 16: case 24: case 30: break;
		default:
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Given depth (%d) is not supported\n", pScrn->depth);
			return FALSE;
	}
	xf86PrintDepthBpp(pScrn);
	info->pix24bpp = xf86GetBppFromDepth(pScrn, pScrn->depth);
	info->pixel_bytes = pScrn->bitsPerPixel / 8;
	if (info->pix24bpp == 24) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "24bpp is not supported\n");
		return FALSE;
	}
	xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Pixel depth = %d bits stored in %d byte%s (%d bpp pixmaps)\n",
			   pScrn->depth, info->pixel_bytes, info->pixel_bytes > 1 ? "s" : "", info->pix24bpp);
	if (unlikely(!xf86SetDefaultVisual(pScrn, -1))) return FALSE;
	if (pScrn->depth > 8 && pScrn->defaultVisual != TrueColor) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Default visual (%s) is not supported at depth %d\n",
				   xf86GetVisualName(pScrn->defaultVisual), pScrn->depth);
		return FALSE;
	}
	return TRUE;
}

static Bool AMDGPUPreInitWeight(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	info->dac6bits = FALSE;
	if (pScrn->depth > 8) {
		rgb defaultWeight = { 0, 0, 0 };
		if (unlikely(!xf86SetWeight(pScrn, defaultWeight, defaultWeight))) return FALSE;
	} else {
		pScrn->rgbBits = 8;
	}
	xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Using %d bits per RGB (%d bit DAC)\n",
			   pScrn->rgbBits, info->dac6bits ? 6 : 8);
	return TRUE;
}

static Bool AMDGPUPreInitAccel_KMS(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	if (xf86ReturnOptValBool(info->Options, OPTION_ACCEL, TRUE)) {
		AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
		Bool use_glamor = TRUE;
		#ifdef HAVE_GBM_BO_USE_LINEAR
		const char *accel_method = xf86GetOptValString(info->Options, OPTION_ACCEL_METHOD);
		if (accel_method && !strcmp(accel_method, "none")) use_glamor = FALSE;
		#endif
		#ifdef DRI2
		info->dri2.available = !!xf86LoadSubModule(pScrn, "dri2");
		#endif
		if (info->dri2.available) info->gbm = gbm_create_device(pAMDGPUEnt->fd);
		if (info->gbm) {
			if (use_glamor && amdgpu_glamor_pre_init(pScrn)) return TRUE;
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "Glamor pre-init failed or disabled, using ShadowFB\n");
		} else {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "GBM device creation failed, using ShadowFB\n");
		}
	} else {
		xf86DrvMsg(pScrn->scrnIndex, X_CONFIG, "GPU acceleration disabled, using ShadowFB\n");
	}
	if (unlikely(!xf86LoadSubModule(pScrn, "shadow"))) return FALSE;
	info->dri2.available = FALSE; info->shadow_fb = TRUE;
	return TRUE;
}

static Bool AMDGPUPreInitChipType_KMS(ScrnInfoPtr pScrn, struct amdgpu_gpu_info *gpu_info)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	pScrn->chipset = (char*)amdgpu_get_marketing_name(pAMDGPUEnt->pDev);
	if (!pScrn->chipset) pScrn->chipset = "Unknown AMD Radeon GPU";
	xf86DrvMsg(pScrn->scrnIndex, X_PROBED, "Chipset: \"%s\" (ChipID = 0x%04x)\n",
			   pScrn->chipset, gpu_info->asic_id);
	info->family = gpu_info->family_id;
	return TRUE;
}

static Bool amdgpu_get_tile_config(AMDGPUInfoPtr info, struct amdgpu_gpu_info *gpu_info)
{
	switch ((gpu_info->gb_addr_cfg & 0x70) >> 4) {
		case 0: info->group_bytes = 256; break;
		case 1: info->group_bytes = 512; break;
		default: return FALSE;
	}
	info->have_tiling_info = TRUE;
	return TRUE;
}

static void AMDGPUSetupCapabilities(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	uint64_t value; int ret;
	pScrn->capabilities = 0;
	if (!info->use_glamor) return;
	ret = drmGetCap(pAMDGPUEnt->fd, DRM_CAP_PRIME, &value);
	if (ret == 0) {
		if (value & DRM_PRIME_CAP_EXPORT) pScrn->capabilities |= RR_Capability_SourceOutput | RR_Capability_SourceOffload;
		if (value & DRM_PRIME_CAP_IMPORT) {
			pScrn->capabilities |= RR_Capability_SinkOffload;
			if (info->drmmode.count_crtcs) pScrn->capabilities |= RR_Capability_SinkOutput;
		}
	}
}

static Bool AMDGPUCreateWindow_oneshot(WindowPtr pWin)
{
	ScreenPtr pScreen = pWin->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	Bool ret;
	if (unlikely(pWin != pScreen->root)) ErrorF("%s called for non-root window %p\n", __func__, pWin);
	pScreen->CreateWindow = info->CreateWindow;
	ret = pScreen->CreateWindow(pWin);
	if (ret) drmmode_copy_fb(pScrn, &info->drmmode);
	return ret;
}

static void amdgpu_determine_cursor_size(int fd, AMDGPUInfoPtr info)
{
	uint64_t value;
	if (drmGetCap(fd, DRM_CAP_CURSOR_WIDTH, &value) == 0) info->cursor_w = value;
	else info->cursor_w = (info->family < AMDGPU_FAMILY_CI) ? CURSOR_WIDTH : CURSOR_WIDTH_CIK;
	if (drmGetCap(fd, DRM_CAP_CURSOR_HEIGHT, &value) == 0) info->cursor_h = value;
	else info->cursor_h = (info->family < AMDGPU_FAMILY_CI) ? CURSOR_HEIGHT : CURSOR_HEIGHT_CIK;
}

void AMDGPUWindowExposures_oneshot(WindowPtr pWin, RegionPtr pRegion)
{
	ScreenPtr pScreen = pWin->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	if (unlikely(pWin != pScreen->root)) ErrorF("%s called for non-root window %p\n", __func__, pWin);
	pScreen->WindowExposures = info->WindowExposures;
	pScreen->WindowExposures(pWin, pRegion);
	amdgpu_glamor_finish(pScrn);
	drmmode_set_desired_modes(pScrn, &info->drmmode, TRUE);
}

Bool AMDGPUPreInit_KMS(ScrnInfoPtr pScrn, int flags)
{
	AMDGPUInfoPtr info;
	AMDGPUEntPtr pAMDGPUEnt;
	struct amdgpu_gpu_info gpu_info;
	MessageType from;
	Gamma zeros = { 0.0, 0.0, 0.0 };
	int cpp;
	uint64_t heap_size = 0, max_allocation = 0;

	if (flags & PROBE_DETECT) return TRUE;
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUPreInit_KMS\n");
	if (pScrn->numEntities != 1) return FALSE;
	pAMDGPUEnt = xf86GetEntityPrivate(pScrn->entityList[0], getAMDGPUEntityIndex())->ptr;
	if (!AMDGPUGetRec(pScrn)) return FALSE;
	info = AMDGPUPTR(pScrn);
	info->instance_id = pAMDGPUEnt->num_scrns++;
	pAMDGPUEnt->scrn[info->instance_id] = pScrn;
	info->pEnt = xf86GetEntityInfo(pScrn->entityList[pScrn->numEntities - 1]);
	if (info->pEnt->location.type!=BUS_PCI
		#ifdef XSERVER_PLATFORM_BUS
		&& info->pEnt->location.type!=BUS_PLATFORM
		#endif
	) return FALSE;
	if (xf86IsEntityShared(pScrn->entityList[0]) && info->instance_id == 0)
		xf86SetPrimInitDone(pScrn->entityList[0]);
	pScrn->monitor = pScrn->confScreen->monitor;
	if (!AMDGPUPreInitVisual(pScrn)) return FALSE;
	xf86CollectOptions(pScrn, NULL);
	if (!(info->Options = malloc(sizeof(AMDGPUOptions_KMS)))) return FALSE;
	memcpy(info->Options, AMDGPUOptions_KMS, sizeof(AMDGPUOptions_KMS));
	xf86ProcessOptions(pScrn->scrnIndex, pScrn->options, info->Options);
	if (!AMDGPUPreInitWeight(pScrn)) return FALSE;
	memset(&gpu_info, 0, sizeof(gpu_info));
	amdgpu_query_gpu_info(pAMDGPUEnt->pDev, &gpu_info);
	if (!AMDGPUPreInitChipType_KMS(pScrn, &gpu_info)) return FALSE;
	info->dri2.available = FALSE; info->dri2.enabled = FALSE;
	info->dri2.pKernelDRMVersion = drmGetVersion(pAMDGPUEnt->fd);
	if (unlikely(!info->dri2.pKernelDRMVersion)) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Failed to get DRM version\n");
		return FALSE;
	}
	if (unlikely(!xf86LoadSubModule(pScrn, "fb"))) return FALSE;
	if (!AMDGPUPreInitAccel_KMS(pScrn)) return FALSE;
	amdgpu_drm_queue_init(pScrn);
	if (info->use_glamor) {
		info->group_bytes = 256; info->have_tiling_info = FALSE;
		amdgpu_get_tile_config(info, &gpu_info);
	}
	if (info->use_glamor) {
		from = X_DEFAULT; info->tear_free = 2;
		if (xf86GetOptValBool(info->Options, OPTION_TEAR_FREE, &info->tear_free)) from = X_CONFIG;
		xf86DrvMsg(pScrn->scrnIndex, from, "TearFree property default: %s\n",
				   info->tear_free == 2 ? "auto" : (info->tear_free ? "on" : "off"));
		info->shadow_primary = xf86ReturnOptValBool(info->Options, OPTION_SHADOW_PRIMARY, FALSE);
		if (info->shadow_primary) xf86DrvMsg(pScrn->scrnIndex, X_CONFIG, "ShadowPrimary enabled\n");
		if (!pScrn->is_gpu) {
			from = xf86GetOptValBool(info->Options, OPTION_VARIABLE_REFRESH, &info->vrr_support) ? X_CONFIG : X_DEFAULT;
			if (info->vrr_support && !info->tear_free)
				xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "Enabling VariableRefresh while TearFree is disabled can cause instability!\n");
			xf86DrvMsg(pScrn->scrnIndex, from, "VariableRefresh: %sabled\n", info->vrr_support ? "en" : "dis");
			info->async_flip_secondaries = FALSE;
			from = xf86GetOptValBool(info->Options, OPTION_ASYNC_FLIP_SECONDARIES, &info->async_flip_secondaries) ? X_CONFIG : X_DEFAULT;
			xf86DrvMsg(pScrn->scrnIndex, from, "AsyncFlipSecondaries: %sabled\n", info->async_flip_secondaries ? "en" : "dis");
		}
	}
	if (!pScrn->is_gpu) {
		info->allowPageFlip = xf86ReturnOptValBool(info->Options, OPTION_PAGE_FLIP, TRUE);
		if (info->shadow_primary) {
			xf86DrvMsg(pScrn->scrnIndex, info->allowPageFlip?X_WARNING:X_DEFAULT, "KMS Pageflipping: disabled%s\n",
					   info->allowPageFlip?" because of ShadowPrimary":"");
			info->allowPageFlip = FALSE;
		} else {
			xf86DrvMsg(pScrn->scrnIndex, X_INFO, "KMS Pageflipping: %sabled\n", info->allowPageFlip?"en":"dis");
		}
	}
	if (xf86ReturnOptValBool(info->Options, OPTION_DELETE_DP12, FALSE))
		info->drmmode.delete_dp_12_displays = TRUE;
	if (unlikely(!drmmode_pre_init(pScrn, &info->drmmode, pScrn->bitsPerPixel / 8))) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Kernel modesetting setup failed\n");
		return FALSE;
	}
	AMDGPUSetupCapabilities(pScrn);
	pAMDGPUEnt->HasCRTC2 = (info->drmmode.count_crtcs > 1);
	amdgpu_determine_cursor_size(pAMDGPUEnt->fd, info);
	amdgpu_query_heap_size(pAMDGPUEnt->pDev, AMDGPU_GEM_DOMAIN_GTT, &heap_size, &max_allocation);
	info->gart_size = heap_size;
	amdgpu_query_heap_size(pAMDGPUEnt->pDev, AMDGPU_GEM_DOMAIN_VRAM, &heap_size, &max_allocation);
	info->vram_size = max_allocation;
	xf86DrvMsg(pScrn->scrnIndex, X_INFO, "mem size init: gart size :%llx vram size: s:%llx visible:%llx\n",
			   (unsigned long long)info->gart_size, (unsigned long long)heap_size, (unsigned long long)max_allocation);
	cpp = pScrn->bitsPerPixel / 8;
	pScrn->displayWidth = AMDGPU_ALIGN(pScrn->virtualX, drmmode_get_pitch_align(pScrn, cpp));
	xf86SetDpi(pScrn, 0, 0);
	if (unlikely(!xf86SetGamma(pScrn, zeros))) return FALSE;
	if (!xf86ReturnOptValBool(info->Options, OPTION_SW_CURSOR, FALSE) &&
		unlikely(!xf86LoadSubModule(pScrn, "ramdac"))) return FALSE;
	if (unlikely(!pScrn->modes && !pScrn->is_gpu)) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "No modes.\n");
		return FALSE;
	}
	return TRUE;
}

static Bool AMDGPUCursorInit_KMS(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "Initializing Cursor\n");
	xf86SetSilkenMouse(pScreen);
	miDCInitialize(pScreen, xf86GetPointerScreenFuncs());
	if (info->allowPageFlip) {
		miPointerScreenPtr PointPriv = dixLookupPrivate(&pScreen->devPrivates, miPointerScreenKey);
		if (unlikely(!dixRegisterScreenPrivateKey(&amdgpu_device_private_key, pScreen,
			PRIVATE_DEVICE, sizeof(struct amdgpu_device_priv)))) {
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "dixRegisterScreenPrivateKey failed\n");
		return FALSE;
			}
			info->SpriteFuncs = PointPriv->spriteFuncs;
			PointPriv->spriteFuncs = &drmmode_sprite_funcs;
	}
	if (xf86ReturnOptValBool(info->Options, OPTION_SW_CURSOR, FALSE)) return TRUE;
	if (unlikely(!xf86_cursors_init(pScreen, info->cursor_w, info->cursor_h,
		HARDWARE_CURSOR_TRUECOLOR_AT_8BPP | HARDWARE_CURSOR_AND_SOURCE_WITH_MASK |
		HARDWARE_CURSOR_SOURCE_MASK_INTERLEAVE_1 | HARDWARE_CURSOR_UPDATE_UNHIDDEN |
		HARDWARE_CURSOR_ARGB))) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "xf86_cursors_init failed\n");
	return FALSE;
		}
		return TRUE;
}

void AMDGPUBlank(ScrnInfoPtr pScrn)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	xf86OutputPtr output; xf86CrtcPtr crtc; int o, c;
	for (c = 0; c < xf86_config->num_crtc; c++) {
		crtc = xf86_config->crtc[c];
		if (unlikely(!crtc)) continue;
		for (o = 0; o < xf86_config->num_output; o++) {
			output = xf86_config->output[o];
			if (unlikely(!output)) continue;
			if (output->crtc != crtc) continue;
			output->funcs->dpms(output, DPMSModeOff);
		}
		crtc->funcs->dpms(crtc, DPMSModeOff);
	}
}

void AMDGPUUnblank(ScrnInfoPtr pScrn)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	xf86OutputPtr output; xf86CrtcPtr crtc; int o, c;
	for (c = 0; c < xf86_config->num_crtc; c++) {
		crtc = xf86_config->crtc[c];
		if (unlikely(!crtc || !crtc->enabled)) continue;
		crtc->funcs->dpms(crtc, DPMSModeOn);
		for (o = 0; o < xf86_config->num_output; o++) {
			output = xf86_config->output[o];
			if (unlikely(!output)) continue;
			if (output->crtc != crtc) continue;
			output->funcs->dpms(output, DPMSModeOn);
		}
	}
}

static Bool amdgpu_set_drm_master(ScrnInfoPtr pScrn)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn); int err;
	#ifdef XF86_PDEV_SERVER_FD
	if (pAMDGPUEnt->platform_dev && (pAMDGPUEnt->platform_dev->flags & XF86_PDEV_SERVER_FD)) return TRUE;
	#endif
	err = drmSetMaster(pAMDGPUEnt->fd);
	if (err) ErrorF("Unable to retrieve master\n");
	return err == 0;
}

static void amdgpu_drop_drm_master(ScrnInfoPtr pScrn)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	#ifdef XF86_PDEV_SERVER_FD
	if (pAMDGPUEnt->platform_dev && (pAMDGPUEnt->platform_dev->flags & XF86_PDEV_SERVER_FD)) return;
	#endif
	drmDropMaster(pAMDGPUEnt->fd);
}

static CARD32 cleanup_black_fb(OsTimerPtr timer, CARD32 now, pointer data)
{
	ScreenPtr screen = data;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn); int c;
	if (xf86ScreenToScrn(amdgpu_primary_screen(screen))->vtSema) return 0;
	for (c = 0; c < xf86_config->num_crtc; c++) {
		if (unlikely(!xf86_config->crtc[c])) continue;
		drmmode_crtc_private_ptr drmmode_crtc = xf86_config->crtc[c]->driver_private;
		if (likely(drmmode_crtc))
			drmmode_fb_reference(pAMDGPUEnt->fd, &drmmode_crtc->fb, NULL);
	}
	TimerFree(timer); return 0;
}

static Bool AMDGPUSaveScreen_KMS(ScreenPtr pScreen, int mode)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen); Bool unblank;
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUSaveScreen(%d)\n", mode);
	unblank = xf86IsUnblank(mode);
	if (unblank) SetTimeSinceLastInputEvent();
	if (pScrn && pScrn->vtSema) {
		if (unblank) AMDGPUUnblank(pScrn); else AMDGPUBlank(pScrn);
	}
	return TRUE;
}

static Bool AMDGPUCloseScreen_KMS(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUCloseScreen\n");
	pAMDGPUEnt->assigned_crtcs = 0;
	drmmode_uevent_fini(pScrn, &info->drmmode);
	amdgpu_drm_queue_close(pScrn);
	if (info->callback_event_type != -1) {
		DeleteCallback(&EventCallback, amdgpu_event_callback, pScrn);
		DeleteCallback(&FlushCallback, amdgpu_flush_callback, pScrn);
	}
	amdgpu_sync_close(pScreen);
	amdgpu_drop_drm_master(pScrn);
	drmmode_fini(pScrn, &info->drmmode);
	if (info->dri2.enabled) amdgpu_dri2_close_screen(pScreen);
	amdgpu_glamor_fini(pScreen);
	pScrn->vtSema = FALSE;
	xf86ClearPrimInitDone(info->pEnt->index);
	if (info->allowPageFlip) {
		miPointerScreenPtr PointPriv = dixLookupPrivate(&pScreen->devPrivates, miPointerScreenKey);
		if (PointPriv->spriteFuncs == &drmmode_sprite_funcs) PointPriv->spriteFuncs = info->SpriteFuncs;
	}
	pScreen->BlockHandler = info->BlockHandler;
	pScreen->CloseScreen = info->CloseScreen;
	return pScreen->CloseScreen(pScreen);
}

void AMDGPUFreeScreen_KMS(ScrnInfoPtr pScrn)
{
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUFreeScreen\n");
	AMDGPUFreeRec(pScrn);
}

Bool AMDGPUScreenInit_KMS(ScreenPtr pScreen, int argc, char **argv)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	int subPixelOrder = SubPixelUnknown; MessageType from; Bool value; int driLevel;
	const char *s; void *front_ptr = NULL; /* Initialize */

	pScrn->fbOffset = 0;
	miClearVisualTypes();
	if (unlikely(!miSetVisualTypes(pScrn->depth, miGetDefaultVisualMask(pScrn->depth),
		pScrn->rgbBits, pScrn->defaultVisual))) return FALSE;
	miSetPixmapDepths();
	if (unlikely(!amdgpu_set_drm_master(pScrn))) return FALSE;
	info->directRenderingEnabled = FALSE;
	if (!info->shadow_fb) info->directRenderingEnabled = amdgpu_dri2_screen_init(pScreen);
	if (unlikely(!amdgpu_setup_kernel_mem(pScreen))) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "amdgpu_setup_kernel_mem failed\n");
		return FALSE;
	}
	if (likely(info->front_buffer)) front_ptr = info->front_buffer->cpu_ptr;

	if (info->shadow_fb) {
		if (unlikely(!info->fb_shadow)) { /* Should have been allocated by setup_kernel_mem if needed by then */
			info->fb_shadow = calloc(1, pScrn->displayWidth*pScrn->virtualY*((pScrn->bitsPerPixel+7)>>3));
		}
		if (unlikely(!info->fb_shadow)) {
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Failed to allocate shadow framebuffer\n");
			return FALSE;
		}
		if (unlikely(!fbScreenInit(pScreen,info->fb_shadow,pScrn->virtualX,pScrn->virtualY,
			pScrn->xDpi,pScrn->yDpi,pScrn->displayWidth,pScrn->bitsPerPixel))) return FALSE;
	} else {
		if (unlikely(!fbScreenInit(pScreen,front_ptr,pScrn->virtualX,pScrn->virtualY,
			pScrn->xDpi,pScrn->yDpi,pScrn->displayWidth,pScrn->bitsPerPixel))) return FALSE;
	}
	xf86SetBlackWhitePixels(pScreen);
	if (pScrn->bitsPerPixel > 8) {
		VisualPtr visual = pScreen->visuals + pScreen->numVisuals;
		while (--visual >= pScreen->visuals) {
			if ((visual->class|DynamicClass)==DirectColor) {
				visual->offsetRed = pScrn->offset.red; visual->offsetGreen = pScrn->offset.green;
				visual->offsetBlue = pScrn->offset.blue; visual->redMask = pScrn->mask.red;
				visual->greenMask = pScrn->mask.green; visual->blueMask = pScrn->mask.blue;
			}
		}
	}
	fbPictureInit(pScreen,0,0);
	#ifdef RENDER
	if ((s = xf86GetOptValString(info->Options, OPTION_SUBPIXEL_ORDER))) {
		if(!strcmp(s,"RGB"))subPixelOrder=SubPixelHorizontalRGB; else if(!strcmp(s,"BGR"))subPixelOrder=SubPixelHorizontalBGR;
		else if(!strcmp(s,"NONE"))subPixelOrder=SubPixelNone;
		PictureSetSubpixelOrder(pScreen,subPixelOrder);
	}
	#endif
	value = (XORG_VERSION_CURRENT >= XORG_VERSION_NUMERIC(1,18,3,0,0)) ? info->use_glamor : FALSE;
	from = X_DEFAULT;
	if (info->use_glamor) {
		if (xf86GetOptValBool(info->Options, OPTION_DRI3, &value)) from = X_CONFIG;
		if (xf86GetOptValInteger(info->Options, OPTION_DRI, &driLevel) && (driLevel==2||driLevel==3)) {
			from = X_CONFIG; value = (driLevel==3);
		}
	}
	if (value) {
		value = amdgpu_sync_init(pScreen) && amdgpu_present_screen_init(pScreen) && amdgpu_dri3_screen_init(pScreen);
		if (!value) from = X_WARNING;
	}
	xf86DrvMsg(pScrn->scrnIndex, from, "DRI3 %sabled\n", value?"en":"dis");
	pScrn->vtSema = TRUE; xf86SetBackingStore(pScreen);
	if (info->directRenderingEnabled) xf86DrvMsg(pScrn->scrnIndex,X_INFO,"Direct rendering enabled\n");
	else xf86DrvMsg(pScrn->scrnIndex,X_WARNING,"Direct rendering disabled\n");
	if (info->use_glamor && info->directRenderingEnabled) {
		xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "Initializing Acceleration\n");
		if (amdgpu_glamor_init(pScreen)) xf86DrvMsg(pScrn->scrnIndex,X_INFO,"Acceleration enabled\n");
		else { xf86DrvMsg(pScrn->scrnIndex,X_ERROR,"Acceleration initialization failed\n");
			xf86DrvMsg(pScrn->scrnIndex,X_INFO,"2D and 3D acceleration disabled\n"); info->use_glamor=FALSE; }
	} else if (info->directRenderingEnabled) {
		if (!amdgpu_pixmap_init(pScreen)) xf86DrvMsg(pScrn->scrnIndex,X_INFO,"3D acceleration disabled\n");
		xf86DrvMsg(pScrn->scrnIndex,X_INFO,"2D acceleration disabled\n");
	} else { xf86DrvMsg(pScrn->scrnIndex,X_INFO,"2D and 3D acceleration disabled\n"); }
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "Initializing DPMS\n");
	xf86DPMSInit(pScreen,xf86DPMSSet,0);
	if (unlikely(!AMDGPUCursorInit_KMS(pScreen))) return FALSE;
	if (!info->shadow_fb && !pScreen->isGPU) {
		xf86DrvMsgVerb(pScrn->scrnIndex,X_INFO,AMDGPU_LOGLEVEL_DEBUG,"Initializing Xv\n");
		AMDGPUInitVideo(pScreen);
	}
	if (info->shadow_fb && unlikely(!shadowSetup(pScreen))) {
		xf86DrvMsg(pScrn->scrnIndex,X_ERROR,"Shadowfb initialization failed\n"); return FALSE;
	}
	pScrn->pScreen = pScreen;
	if (!pScreen->isGPU) {
		if (serverGeneration==1 && bgNoneRoot && info->use_glamor) {
			info->CreateWindow = pScreen->CreateWindow; pScreen->CreateWindow = AMDGPUCreateWindow_oneshot;
		}
		info->WindowExposures = pScreen->WindowExposures; pScreen->WindowExposures = AMDGPUWindowExposures_oneshot;
	}
	info->CloseScreen = pScreen->CloseScreen; pScreen->CloseScreen = AMDGPUCloseScreen_KMS;
	pScreen->SaveScreen = AMDGPUSaveScreen_KMS;
	info->BlockHandler = pScreen->BlockHandler; pScreen->BlockHandler = AMDGPUBlockHandler_KMS;
	info->CreateScreenResources = pScreen->CreateScreenResources;
	pScreen->CreateScreenResources = AMDGPUCreateScreenResources_KMS;
	pScreen->StartPixmapTracking = PixmapStartDirtyTracking;
	pScreen->StopPixmapTracking = PixmapStopDirtyTracking;
	#if HAS_SYNC_SHARED_PIXMAP
	pScreen->SyncSharedPixmap = amdgpu_sync_shared_pixmap;
	#endif
	if (unlikely(!xf86CrtcScreenInit(pScreen))) return FALSE;
	if (unlikely(!drmmode_setup_colormap(pScreen, pScrn))) return FALSE;
	if (serverGeneration == 1) xf86ShowUnusedOptions(pScrn->scrnIndex, pScrn->options);
	if (info->vrr_support) {
		vrr_lock(); /* Protect global static variables */
		if (!amdgpu_property_vectors_wrapped) { /* Only wrap if not already done */
			saved_change_property = ProcVector[X_ChangeProperty]; ProcVector[X_ChangeProperty] = amdgpu_change_property;
			saved_delete_property = ProcVector[X_DeleteProperty]; ProcVector[X_DeleteProperty] = amdgpu_delete_property;
			amdgpu_property_vectors_wrapped = TRUE;
		}
		vrr_unlock();
		amdgpu_vrr_atom = MakeAtom("_VARIABLE_REFRESH", strlen("_VARIABLE_REFRESH"), TRUE);
	}
	drmmode_init(pScrn, &info->drmmode);
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUScreenInit finished\n");
	return TRUE;
}

Bool AMDGPUEnterVT_KMS(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPUEnterVT_KMS\n");
	amdgpu_set_drm_master(pScrn);
	if (info->shadow_fb) {
		int pitch;
		struct amdgpu_buffer *front_buffer =
		amdgpu_alloc_pixmap_bo(pScrn, pScrn->virtualX, pScrn->virtualY, pScrn->depth,
							   AMDGPU_CREATE_PIXMAP_SCANOUT | AMDGPU_CREATE_PIXMAP_LINEAR,
						 pScrn->bitsPerPixel, &pitch);
		if (likely(front_buffer)) {
			if (amdgpu_bo_map(pScrn, front_buffer) == 0) {
				if (likely(front_buffer->cpu_ptr))
					memset(front_buffer->cpu_ptr, 0, (size_t)pitch * pScrn->virtualY);
				amdgpu_bo_unref(&info->front_buffer); info->front_buffer = front_buffer;
			} else { amdgpu_bo_unref(&front_buffer); front_buffer = NULL; }
		}
		if (unlikely(!front_buffer))
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "Failed to allocate new scanout BO after VT switch\n");
	}
	pScrn->vtSema = TRUE;
	if (unlikely(!drmmode_set_desired_modes(pScrn, &info->drmmode, TRUE))) return FALSE;
	return TRUE;
}

static void pixmap_unref_fb(PixmapPtr pixmap)
{
	ScrnInfoPtr scrn = xf86ScreenToScrn(pixmap->drawable.pScreen);
	struct drmmode_fb **fb_ptr = amdgpu_pixmap_get_fb_ptr(pixmap);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	if (fb_ptr) drmmode_fb_reference(pAMDGPUEnt->fd, fb_ptr, NULL);
}

static void client_pixmap_unref_fb(void *value, XID id, void *pScreen)
{
	PixmapPtr pixmap = value;
	if (pixmap->drawable.pScreen == pScreen) pixmap_unref_fb(pixmap);
}

void AMDGPULeaveVT_KMS(ScrnInfoPtr pScrn)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn); ScreenPtr pScreen = pScrn->pScreen;
	xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG, "AMDGPULeaveVT_KMS\n");
	if (!info->shadow_fb) {
		AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
		xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
		xf86CrtcPtr crtc; drmmode_crtc_private_ptr drmmode_crtc;
		unsigned w = 0, h = 0; int i;
		if (unlikely(!pScreen->GCperDepth[0])) goto hide_cursors;
		for (i=0; i<xf86_config->num_crtc; i++) {
			if (unlikely(!xf86_config->crtc[i])) continue;
			crtc = xf86_config->crtc[i]; drmmode_crtc = crtc->driver_private;
			if (unlikely(!drmmode_crtc || !drmmode_crtc->fb)) continue;
			w = max(w, crtc->mode.HDisplay); h = max(h, crtc->mode.VDisplay);
		}
		if (w > 0 && h > 0) {
			PixmapPtr black_scanout = pScreen->CreatePixmap(pScreen,w,h,pScrn->depth,AMDGPU_CREATE_PIXMAP_SCANOUT);
			if (likely(black_scanout)) {
				struct drmmode_fb *black_fb = amdgpu_pixmap_get_fb(black_scanout);
				amdgpu_pixmap_clear(black_scanout); amdgpu_glamor_finish(pScrn);
				for (i=0; i<xf86_config->num_crtc; i++) {
					if (unlikely(!xf86_config->crtc[i])) continue;
					crtc = xf86_config->crtc[i]; drmmode_crtc = crtc->driver_private;
					if (unlikely(!drmmode_crtc || !drmmode_crtc->fb)) continue;
					if (black_fb) drmmode_set_mode(crtc,black_fb,&crtc->mode,0,0);
					else { drmModeSetCrtc(pAMDGPUEnt->fd, drmmode_crtc->mode_crtc->crtc_id,0,0,0,NULL,0,NULL);
						drmmode_fb_reference(pAMDGPUEnt->fd, &drmmode_crtc->fb,NULL); }
						if (pScrn->is_gpu) {
							if(drmmode_crtc->scanout[0])pixmap_unref_fb(drmmode_crtc->scanout[0]);
							if(drmmode_crtc->scanout[1])pixmap_unref_fb(drmmode_crtc->scanout[1]);
						} else drmmode_crtc_scanout_free(crtc);
				}
				dixDestroyPixmap(black_scanout,0);
			}
		}
		xf86RotateFreeShadow(pScrn);
		for (i=0; i<currentMaxClients; i++) {
			if (i>0 && (!clients[i] || clients[i]->clientState!=ClientStateRunning)) continue;
			FindClientResourcesByType(clients[i],RT_PIXMAP,client_pixmap_unref_fb,pScreen);
		}
		pixmap_unref_fb(pScreen->GetScreenPixmap(pScreen));
	} else {
		if (likely(info->front_buffer && info->front_buffer->cpu_ptr))
			memset(info->front_buffer->cpu_ptr, 0, (size_t)pScrn->virtualX * info->pixel_bytes * pScrn->virtualY);
	}
	if (pScreen->GCperDepth[0]) TimerSet(NULL,0,1000,cleanup_black_fb,pScreen);
	hide_cursors:
	xf86_hide_cursors(pScrn);
	amdgpu_drop_drm_master(pScrn);
	xf86DrvMsgVerb(pScrn->scrnIndex,X_INFO,AMDGPU_LOGLEVEL_DEBUG,"Ok, leaving now...\n");
}

Bool AMDGPUSwitchMode_KMS(ScrnInfoPtr pScrn, DisplayModePtr mode)
{
	return xf86SetSingleMode(pScrn, mode, RR_Rotate_0);
}

void AMDGPUAdjustFrame_KMS(ScrnInfoPtr pScrn, int x, int y)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	drmmode_adjust_frame(pScrn, &info->drmmode, x, y);
}

static Bool amdgpu_setup_kernel_mem(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	int cpp = info->pixel_bytes; int cursor_size; int c, i;

	cursor_size = info->cursor_w * info->cursor_h * 4;
	cursor_size = AMDGPU_ALIGN(cursor_size, AMDGPU_GPU_PAGE_SIZE);
	for (c = 0; c < xf86_config->num_crtc; c++) {
		if (unlikely(!xf86_config->crtc[c])) continue;
		drmmode_crtc_private_ptr drmmode_crtc = xf86_config->crtc[c]->driver_private;
		if (unlikely(!drmmode_crtc)) continue;
		for (i = 0; i < 2; i++) {
			if (!drmmode_crtc->cursor_buffer[i]) {
				drmmode_crtc->cursor_buffer[i] =
				amdgpu_bo_open(pAMDGPUEnt->pDev, cursor_size,0,AMDGPU_GEM_DOMAIN_VRAM);
				if (unlikely(!drmmode_crtc->cursor_buffer[i])) {
					ErrorF("Failed to allocate cursor buffer memory\n"); return FALSE;
				}
				if (unlikely(amdgpu_bo_cpu_map(drmmode_crtc->cursor_buffer[i]->bo.amdgpu,
					&drmmode_crtc->cursor_buffer[i]->cpu_ptr)))
					ErrorF("Failed to map cursor buffer memory\n"); /* Non-fatal? Continue. */
			}
		}
	}
	if (!info->front_buffer) {
		int pitch; int hint = AMDGPU_CREATE_PIXMAP_SCANOUT | AMDGPU_CREATE_PIXMAP_FRONT;
		if (info->shadow_primary) hint |= AMDGPU_CREATE_PIXMAP_LINEAR | AMDGPU_CREATE_PIXMAP_GTT;
		else if (!info->use_glamor) hint |= AMDGPU_CREATE_PIXMAP_LINEAR;
		info->front_buffer = amdgpu_alloc_pixmap_bo(pScrn, pScrn->virtualX, pScrn->virtualY,
													pScrn->depth, hint, pScrn->bitsPerPixel, &pitch);
		if (unlikely(!info->front_buffer)) {
			ErrorF("Failed to allocate front buffer memory\n"); return FALSE;
		}
		if (!info->use_glamor && unlikely(amdgpu_bo_map(pScrn, info->front_buffer) != 0)) {
			ErrorF("Failed to map front buffer memory\n"); return FALSE;
		}
		pScrn->displayWidth = pitch / cpp;
	}
	xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Front buffer pitch: %d bytes\n", pScrn->displayWidth*cpp);
	return TRUE;
}

ModeStatus AMDGPUValidMode(ScrnInfoPtr pScrn, DisplayModePtr mode, Bool verbose, int flag)
{
	if (mode->Flags & V_DBLSCAN && (mode->CrtcHDisplay >= 1024 || mode->CrtcVDisplay >= 768))
		return MODE_CLOCK_RANGE;
	return MODE_OK;
}
