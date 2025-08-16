/*
 * Copyright 2008 Kristian Høgsberg
 * Copyright 2008 Jérôme Glisse
 *
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation on the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON-INFRINGEMENT.  IN NO EVENT SHALL ATI, VA LINUX SYSTEMS AND/OR
 * THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "amdgpu_drv.h"
#include "amdgpu_dri2.h"
#include "amdgpu_glamor.h"
#include "amdgpu_video.h"
#include "amdgpu_pixmap.h"

#ifdef DRI2

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

#include <gbm.h>

#include "amdgpu_bo_helper.h"
#include "amdgpu_version.h"

#include <list.h> /* From Xorg server includes */
#include <xf86Priv.h>
#include <X11/extensions/dpmsconst.h>
#include <regionstr.h> /* For Region* function prototypes and RegionRec */

#define FALLBACK_SWAP_DELAY 16

/* Branch-prediction helpers */
#if (defined(__GNUC__) && (__GNUC__ >= 3)) || defined(__clang__)
# define likely(x)   __builtin_expect(!!(x), 1)
# define unlikely(x) __builtin_expect(!!(x), 0)
#else
# define likely(x)   (x)
# define unlikely(x) (x)
#endif

/* Hot/Cold function layout hints (advisory to compiler) */
#if defined(__GNUC__) || defined(__clang__)
# ifndef AMDGPU_HOT
#  define AMDGPU_HOT  __attribute__((hot))
# endif
# ifndef AMDGPU_COLD
#  define AMDGPU_COLD __attribute__((cold))
# endif
#else
# ifndef AMDGPU_HOT
#  define AMDGPU_HOT
# endif
# ifndef AMDGPU_COLD
#  define AMDGPU_COLD
# endif
#endif

/* For Scratch GC Cache */
#define MAX_GC_DEPTH 32 /* Max depth value (0-32 needs array size 33) */
#define GC_CACHE_PER_DEPTH 1

typedef DRI2BufferPtr BufferPtr;

struct dri2_buffer_priv {
	PixmapPtr    pixmap;
	unsigned int attachment;
	unsigned int refcnt;
};

struct dri2_window_priv {
	xf86CrtcPtr crtc;
	int         vblank_delta;
};

static DevPrivateKeyRec dri2_window_private_key_rec;
#define dri2_window_private_key (&dri2_window_private_key_rec)
#define get_dri2_window_priv(window) \
((struct dri2_window_priv *) \
dixLookupPrivate(&(window)->devPrivates, dri2_window_private_key))

static int DRI2InfoCnt; /* Declaration for the global counter */

enum DRI2FrameEventType {
	DRI2_SWAP,
	DRI2_FLIP,
	DRI2_WAITMSC,
};

typedef struct _DRI2FrameEvent {
	XID                     drawable_id;
	ClientPtr               client;
	enum DRI2FrameEventType type;
	unsigned                frame;
	xf86CrtcPtr             crtc;
	OsTimerPtr              timer;
	uintptr_t               drm_queue_seq;

	DRI2SwapEventPtr        event_complete;
	void                   *event_data;
	DRI2BufferPtr           front;
	DRI2BufferPtr           back;
	struct _DRI2FrameEvent *next;
} __attribute__((aligned(64))) DRI2FrameEventRec,
*DRI2FrameEventPtr;

/* Simple spinlock for small caches (Xorg is effectively single-threaded, but we keep the lock) */
static volatile int global_spin = 0;
static inline void gl_lock(void)   { while (__sync_lock_test_and_set(&global_spin, 1)) { } }
static inline void gl_unlock(void) { __sync_lock_release(&global_spin); }

#define FRAME_EVENT_CACHE_MAX 256
static DRI2FrameEventPtr fe_free_list        = NULL;
static unsigned          fe_free_list_length = 0;

static AMDGPU_HOT inline DRI2FrameEventPtr frame_event_alloc(void)
{
	DRI2FrameEventPtr ev = NULL;

	gl_lock();
	if (fe_free_list) {
		ev            = fe_free_list;
		fe_free_list  = ev->next;
		fe_free_list_length--;
	}
	gl_unlock();

	if (ev) {
		memset(ev, 0, sizeof(*ev));
		return ev;
	}
	return (DRI2FrameEventPtr)calloc(1, sizeof(DRI2FrameEventRec));
}

static AMDGPU_HOT inline void frame_event_free(DRI2FrameEventPtr ev)
{
	if (!ev)
		return;

	gl_lock();
	if (fe_free_list_length >= FRAME_EVENT_CACHE_MAX) {
		gl_unlock();
		free(ev);
		return;
	}
	ev->next       = fe_free_list;
	fe_free_list   = ev;
	fe_free_list_length++;
	gl_unlock();
}

#define FLINK_CACHE_SIZE 64
typedef struct {
	uint32_t bo_handle;
	uint32_t flink_name;
	Bool     valid;
} FlinkCacheEntry;

static FlinkCacheEntry flink_cache[FLINK_CACHE_SIZE];
static int             flink_cache_idx = 0;
/* MRU cache entry: common case is repeated use of the same BO */
static FlinkCacheEntry flink_mru = { 0, 0, FALSE };

static AMDGPU_HOT Bool
amdgpu_get_flink_name(AMDGPUEntPtr pEnt, PixmapPtr pixmap, uint32_t *name)
{
	struct amdgpu_buffer *bo = amdgpu_get_pixmap_bo(pixmap);
	uint32_t handle;

	if (!amdgpu_pixmap_get_handle(pixmap, &handle))
		return FALSE;

	/* Fast path: cached and non-GBM BO */
	if (bo && !(bo->flags & AMDGPU_BO_FLAGS_GBM)) {
		/* MRU check (benign data race in single-threaded Xorg; helps avoid lock) */
		if (flink_mru.valid && flink_mru.bo_handle == handle) {
			*name = flink_mru.flink_name;
			return TRUE;
		}

		gl_lock();
		for (int i = 0; i < FLINK_CACHE_SIZE; ++i) {
			if (flink_cache[i].valid && flink_cache[i].bo_handle == handle) {
				*name   = flink_cache[i].flink_name;
				flink_mru = flink_cache[i]; /* Update MRU */
				gl_unlock();
				return TRUE;
			}
		}
		gl_unlock();

		if (amdgpu_bo_export(bo->bo.amdgpu,
		                     amdgpu_bo_handle_type_gem_flink_name,
		                     name) == 0) {
			gl_lock();
			flink_cache[flink_cache_idx].bo_handle  = handle;
			flink_cache[flink_cache_idx].flink_name = *name;
			flink_cache[flink_cache_idx].valid      = TRUE;
			flink_mru = flink_cache[flink_cache_idx];
			flink_cache_idx = (flink_cache_idx + 1) % FLINK_CACHE_SIZE;
			gl_unlock();
			return TRUE;
		}
	}

	/* Fallback: direct DRM ioctl */
	struct drm_gem_flink req;
	memset(&req, 0, sizeof(req));
	req.handle = handle;
	if (ioctl(pEnt->fd, DRM_IOCTL_GEM_FLINK, &req) < 0)
		return FALSE;

	*name = req.name;
	/* Update MRU from ioctl path too */
	flink_mru.bo_handle  = handle;
	flink_mru.flink_name = req.name;
	flink_mru.valid      = TRUE;
	return TRUE;
}

static AMDGPU_HOT Bool update_front(DrawablePtr draw, DRI2BufferPtr front)
{
	ScreenPtr s = draw->pScreen;
	AMDGPUEntPtr ent = AMDGPUEntPriv(xf86ScreenToScrn(s));
	struct dri2_buffer_priv *priv = (struct dri2_buffer_priv *)front->driverPrivate;
	PixmapPtr px = NULL;

	if (!priv)
		return FALSE;

	/* Resolve the pixmap for this drawable */
	if (draw->type == DRAWABLE_PIXMAP) {
		px = (PixmapPtr)draw;
	} else {
		px = s->GetWindowPixmap((WindowPtr)draw);
	}

	if (!px)
		return FALSE;

	/* Fast path: same pixmap as before and we have a valid flink name */
	if (priv->pixmap == px && front->name != 0) {
		front->pitch = px->devKind;
		front->cpp   = px->drawable.bitsPerPixel / 8;
		return TRUE;
	}

	if (!amdgpu_get_flink_name(ent, px, &front->name))
		return FALSE;

	if (priv->pixmap)
		dixDestroyPixmap(priv->pixmap, 0);

	front->pitch = px->devKind;
	front->cpp   = px->drawable.bitsPerPixel / 8;
	priv->pixmap = px;
	px->refcnt++;
	return TRUE;
}

static const uint8_t depth_to_cpp_table[33] = {
	[0]  = 0,
	[1 ... 7] = 1,  [8]  = 1,
	[9 ... 14]= 2,  [15] = 2, [16] = 2,
	[17 ... 23]= 3,
	[24] = 4,
	[25 ... 29]= 4, [30] = 4,
	[31 ... 32]= 4
};

static AMDGPU_HOT BufferPtr
amdgpu_dri2_create_buffer2(ScreenPtr      pScreen,
                           DrawablePtr    drawable,
                           unsigned int   attachment,
                           unsigned int   format)
{
	ScrnInfoPtr              pScrn  = xf86ScreenToScrn(pScreen);
	AMDGPUEntPtr             pEnt   = AMDGPUEntPriv(pScrn);
	AMDGPUInfoPtr            info   = AMDGPUPTR(pScrn);
	BufferPtr                buf    = NULL;
	struct dri2_buffer_priv *priv   = NULL;
	PixmapPtr                pixmap = NULL;
	unsigned                 aw     = drawable->width;
	unsigned                 h      = drawable->height;
	unsigned                 front_w= pScreen->GetScreenPixmap(pScreen)->drawable.width;
	Bool                     glamor = FALSE;
	int                      depth, cpp;

	/* Determine depth / cpp */
	if (format) {
		depth = (int)format;
		cpp   = (depth <= 32) ? depth_to_cpp_table[depth] : 0;
	} else {
		depth = (int)drawable->depth;
		cpp   = (depth <= 32) ? depth_to_cpp_table[depth] : 0;
	}
	if (cpp == 0)
		cpp = (depth + 7) / 8;

	/* Choose pixmap for attachment */
	if (attachment == DRI2BufferFrontLeft) {
		uint32_t handle = 0;
		/* Resolve drawable -> pixmap */
		if (drawable->type == DRAWABLE_PIXMAP) {
			pixmap = (PixmapPtr)drawable;
		} else {
			pixmap = pScreen->GetWindowPixmap((WindowPtr)drawable);
		}

		if (pixmap && pScreen == pixmap->drawable.pScreen) {
			/* If glamor and no gpu-handle for this pixmap, redirect */
			if (info->use_glamor && !amdgpu_pixmap_get_handle(pixmap, &handle)) {
				glamor = TRUE;
				aw     = pixmap->drawable.width;
				h      = pixmap->drawable.height;
				pixmap = NULL;
			} else {
				pixmap->refcnt++;
			}
		} else {
			pixmap = NULL;
		}
	}

	if (!pixmap && (glamor || attachment != DRI2BufferFrontLeft)) {
		/* Use virtual width if attachment equals screen width to avoid realloc */
		if (aw == front_w)
			aw = (unsigned)pScrn->virtualX;

		pixmap = (*pScreen->CreatePixmap)(pScreen, aw, h, depth,
		                                  AMDGPU_CREATE_PIXMAP_DRI2);
	}
	if (!pixmap)
		return NULL;

	/* Unified allocation for DRI2BufferRec + private */
	buf  = (BufferPtr)calloc(1, sizeof(DRI2BufferRec) + sizeof(struct dri2_buffer_priv));
	if (!buf)
		goto err;

	priv = (struct dri2_buffer_priv *)((char *)buf + sizeof(DRI2BufferRec));
	buf->driverPrivate = priv;

	/* If glamor redirect was requested, bind BO to drawable */
	if (glamor) {
		PixmapPtr new_pixmap = amdgpu_glamor_set_pixmap_bo(drawable, pixmap);
		if (!new_pixmap)
			goto err;
		pixmap = new_pixmap;
		pixmap->refcnt++;
		amdgpu_glamor_flush(pScrn);
	}

	if (!amdgpu_get_flink_name(pEnt, pixmap, &buf->name))
		goto err;

	buf->attachment = attachment;
	buf->pitch      = (uint32_t)pixmap->devKind;
	buf->cpp        = (uint32_t)cpp;
	buf->format     = format;
	buf->flags      = 0;

	priv->pixmap     = pixmap;
	priv->attachment = attachment;
	priv->refcnt     = 1;
	return buf;

err:
	if (buf)
		free(buf);
	if (pixmap)
		dixDestroyPixmap(pixmap, 0);
	return NULL;
}

static AMDGPU_HOT void
amdgpu_dri2_destroy_buffer2(ScreenPtr pScreen, DrawablePtr drawable, BufferPtr buf)
{
	(void)pScreen;
	(void)drawable;

	if (!buf)
		return;

	struct dri2_buffer_priv *priv = (struct dri2_buffer_priv *)buf->driverPrivate;
	if (!priv)
		return;

	if (priv->refcnt == 0) {
		xf86DrvMsg(xf86ScreenToScrn(((PixmapPtr)NULL)->drawable.pScreen)->scrnIndex,
		           X_WARNING, "DRI2 buffer double-free attempt ignored.\n");
		return;
	}

	if (--priv->refcnt == 0) {
		if (priv->pixmap)
			dixDestroyPixmap(priv->pixmap, 0);
		free(buf);
	}
}

static GCPtr gc_cache[MAX_GC_DEPTH + 1][GC_CACHE_PER_DEPTH];
static int   gc_cache_cnt[MAX_GC_DEPTH + 1];

static AMDGPU_HOT GCPtr cached_GetScratchGC(unsigned depth, ScreenPtr s)
{
	if (depth <= MAX_GC_DEPTH && gc_cache_cnt[depth] > 0)
		return gc_cache[depth][--gc_cache_cnt[depth]];
	return GetScratchGC(depth, s);
}
static AMDGPU_HOT void cached_FreeScratchGC(GCPtr gc)
{
	if (!gc)
		return;
	unsigned d = (unsigned)gc->depth;
	if (d <= MAX_GC_DEPTH && gc_cache_cnt[d] < GC_CACHE_PER_DEPTH) {
		gc_cache[d][gc_cache_cnt[d]++] = gc;
	} else {
		FreeScratchGC(gc);
	}
}

static AMDGPU_HOT inline PixmapPtr GetDrawablePixmap(DrawablePtr d)
{
	if (d->type == DRAWABLE_PIXMAP)
		return (PixmapPtr)d;
	return d->pScreen->GetWindowPixmap((WindowPtr)d);
}

static AMDGPU_HOT void
amdgpu_dri2_copy_region2(ScreenPtr pScreen, DrawablePtr drawable,
                         RegionPtr region_ptr, BufferPtr dst_buf,
                         BufferPtr src_buf)
{
	struct dri2_buffer_priv *srcp = src_buf ? (struct dri2_buffer_priv *)src_buf->driverPrivate : NULL;
	struct dri2_buffer_priv *dstp = dst_buf ? (struct dri2_buffer_priv *)dst_buf->driverPrivate : NULL;
	if (!srcp || !dstp || !srcp->pixmap || !dstp->pixmap)
		return;

	DrawablePtr src_draw = &srcp->pixmap->drawable;
	DrawablePtr dst_draw = &dstp->pixmap->drawable;
	Bool translate = FALSE;
	int  off_x = 0, off_y = 0;

	if (srcp->attachment == DRI2BufferFrontLeft) {
		if (drawable->pScreen != pScreen) {
			src_draw = DRI2UpdatePrime(drawable, src_buf);
			if (!src_draw)
				return;
		} else {
			src_draw = drawable;
		}
	}
	if (dstp->attachment == DRI2BufferFrontLeft) {
		if (drawable->pScreen != pScreen) {
			dst_draw = DRI2UpdatePrime(drawable, dst_buf);
			if (!dst_draw)
				return;
			if (dst_draw != drawable)
				translate = TRUE;
		} else {
			dst_draw = drawable;
		}
	}

	if (translate && drawable->type == DRAWABLE_WINDOW) {
		PixmapPtr winpx = GetDrawablePixmap(drawable);
		off_x = drawable->x - winpx->screen_x;
		off_y = drawable->y - winpx->screen_y;
	}

	/* Fastpath: region is full-surface */
	const BoxPtr ext = RegionExtents(region_ptr);
	const Bool full_surface = (RegionNumRects(region_ptr) == 1) &&
	                          ext->x1 == 0 && ext->y1 == 0 &&
	                          ext->x2 == drawable->width &&
	                          ext->y2 == drawable->height;

	GCPtr gc = cached_GetScratchGC(dst_draw->depth, pScreen);
	if (!gc)
		return;

	if (full_surface) {
		ValidateGC(dst_draw, gc);
		(*gc->ops->CopyArea)(src_draw, dst_draw, gc,
		                     0, 0, drawable->width, drawable->height,
		                     off_x, off_y);
		cached_FreeScratchGC(gc);
		return;
	}

	/* General case: build clip from region */
	RegionPtr clip = RegionCreate(NULL, 0);
	if (!clip) {
		cached_FreeScratchGC(gc);
		return;
	}
	RegionCopy(clip, region_ptr);
	if (translate)
		RegionTranslate(clip, off_x, off_y);

	(*gc->funcs->ChangeClip)(gc, CT_REGION, clip, 0);
	ValidateGC(dst_draw, gc);
	/* ChangeClip may adopt clip; do not destroy clip again here */
	clip = NULL;

	(*gc->ops->CopyArea)(src_draw, dst_draw, gc,
	                     0, 0, drawable->width, drawable->height,
	                     off_x, off_y);

	cached_FreeScratchGC(gc);
	/* If ChangeClip didn't adopt (implementation-dependent), clip is NULL and this is a no-op */
	if (clip)
		RegionDestroy(clip);
}

static AMDGPU_HOT void amdgpu_dri2_ref_buffer(BufferPtr b)
{
	if (likely(b && b->driverPrivate))
		((struct dri2_buffer_priv *)b->driverPrivate)->refcnt++;
}

static AMDGPU_HOT void amdgpu_dri2_unref_buffer(BufferPtr b)
{
	if (unlikely(!b || !b->driverPrivate))
		return;
	struct dri2_buffer_priv *p = (struct dri2_buffer_priv *)b->driverPrivate;
	if (unlikely(!p->pixmap))
		return;
	DrawablePtr d = &p->pixmap->drawable;
	amdgpu_dri2_destroy_buffer2(d->pScreen, d, b);
}

static AMDGPU_COLD void
amdgpu_dri2_frame_event_abort(xf86CrtcPtr crtc, void *event_data)
{
	(void)crtc;

	DRI2FrameEventPtr event = (DRI2FrameEventPtr)event_data;
	if (unlikely(!event))
		return;

	if (event->timer) {
		TimerCancel(event->timer);
		TimerFree(event->timer);
		event->timer = NULL;
	}
	amdgpu_dri2_unref_buffer(event->front);
	amdgpu_dri2_unref_buffer(event->back);
	frame_event_free(event);
}

static AMDGPU_COLD void
amdgpu_dri2_client_state_changed(CallbackListPtr *ClientStateCallback,
                                 pointer data, pointer calldata)
{
	(void)ClientStateCallback;
	(void)data;

	NewClientInfoRec *clientinfo = (NewClientInfoRec *)calldata;
	ClientPtr pClient = clientinfo->client;
	switch (pClient->clientState) {
		case ClientStateRetained:
		case ClientStateGone:
			amdgpu_drm_abort_client(pClient);
			break;
		default:
			break;
	}
}

static AMDGPU_HOT uint32_t amdgpu_get_msc_delta(DrawablePtr pDraw, xf86CrtcPtr crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	if (pDraw && pDraw->type == DRAWABLE_WINDOW) {
		struct dri2_window_priv *wp = get_dri2_window_priv((WindowPtr)pDraw);
		return (uint32_t)(drmmode_crtc->interpolated_vblanks + wp->vblank_delta);
	}
	return (uint32_t)drmmode_crtc->interpolated_vblanks;
}

static AMDGPU_HOT Bool amdgpu_dri2_get_crtc_msc(xf86CrtcPtr crtc, CARD64 *ust, CARD64 *msc)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;

	/* Fast path: enabled CRTC and kernel can supply UST/MSC */
	if (amdgpu_crtc_is_enabled(crtc) &&
	    drmmode_crtc_get_ust_msc(crtc, ust, msc) == Success) {
		*msc += drmmode_crtc->interpolated_vblanks;
		return TRUE;
	}

	/* Fallback: DPMS off or not available; extrapolate from last DPMS timestamps */
	ScrnInfoPtr scrn = crtc->scrn;
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	CARD64 now = 0, delta_t = 0, delta_seq = 0;

	if (unlikely(!drmmode_crtc->dpms_last_ust))
		return FALSE;
	if (unlikely(drmmode_crtc->dpms_last_fps == 0)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "%s: dpms_last_fps is zero.\n", __func__);
		return FALSE;
	}
	if (unlikely(drmmode_get_current_ust(pAMDGPUEnt->fd, &now) != 0)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR, "%s: cannot get current time\n", __func__);
		return FALSE;
	}

	delta_t  = now - drmmode_crtc->dpms_last_ust;
	delta_seq = (delta_t * (CARD64)drmmode_crtc->dpms_last_fps) / 1000000ULL;

	*ust = drmmode_crtc->dpms_last_ust;
	/* Align ust to computed whole frames to keep UST and MSC consistent */
	{
		CARD64 adj_usec = (delta_seq * 1000000ULL) / (CARD64)drmmode_crtc->dpms_last_fps;
		*ust += adj_usec;
	}
	*msc = drmmode_crtc->dpms_last_seq + delta_seq;
	*msc += drmmode_crtc->interpolated_vblanks;
	return TRUE;
}

static AMDGPU_HOT xf86CrtcPtr amdgpu_dri2_drawable_crtc(DrawablePtr pDraw)
{
	ScreenPtr pScreen = pDraw->pScreen;
	xf86CrtcPtr crtc = amdgpu_pick_best_crtc(pScreen,
	                                         pDraw->x, pDraw->x + pDraw->width,
	                                         pDraw->y, pDraw->y + pDraw->height);

	if (pDraw->type == DRAWABLE_WINDOW) {
		struct dri2_window_priv *priv = get_dri2_window_priv((WindowPtr)pDraw);
		if (!crtc) {
			crtc = priv->crtc;
		} else if (priv->crtc && priv->crtc != crtc) {
			CARD64 ust, mscold, mscnew;
			if (amdgpu_dri2_get_crtc_msc(priv->crtc, &ust, &mscold) &&
			    amdgpu_dri2_get_crtc_msc(crtc,      &ust, &mscnew)) {
				priv->vblank_delta += (int)(mscold - mscnew);
			}
		}
		priv->crtc = crtc;
	}
	return crtc;
}

static AMDGPU_COLD void
amdgpu_dri2_flip_event_abort_for_pageflip(xf86CrtcPtr crtc, void *event_data)
{
	if (likely(crtc))
		AMDGPUPTR(crtc->scrn)->drmmode.dri2_flipping = FALSE;

	DRI2FrameEventPtr event = (DRI2FrameEventPtr)event_data;
	if (likely(event)) {
		amdgpu_dri2_unref_buffer(event->front);
		amdgpu_dri2_unref_buffer(event->back);
		frame_event_free(event);
	}
}

static AMDGPU_HOT void
amdgpu_dri2_flip_event_handler(xf86CrtcPtr crtc, uint32_t frame,
                               uint64_t usec, void *event_data)
{
	DRI2FrameEventPtr flip = (DRI2FrameEventPtr)event_data;
	if (!flip)
		return;

	DrawablePtr drawable = NULL;
	if (dixLookupDrawable(&drawable, flip->drawable_id, serverClient,
	                      M_ANY, DixWriteAccess) != Success)
		goto out;

	frame += amdgpu_get_msc_delta(drawable, crtc);
	DRI2SwapComplete(flip->client, drawable, frame,
	                 usec / 1000000, usec % 1000000,
	                 DRI2_FLIP_COMPLETE,
	                 flip->event_complete, flip->event_data);

out:
	amdgpu_dri2_unref_buffer(flip->front);
	amdgpu_dri2_unref_buffer(flip->back);
	AMDGPUPTR(crtc->scrn)->drmmode.dri2_flipping = FALSE;
	frame_event_free(flip);
}

static AMDGPU_HOT Bool
amdgpu_dri2_schedule_flip(xf86CrtcPtr crtc, ClientPtr client,
                          DrawablePtr draw, DRI2BufferPtr front,
                          DRI2BufferPtr back, DRI2SwapEventPtr func,
                          void *data, unsigned int target_msc)
{
	ScrnInfoPtr scrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);
	struct dri2_buffer_priv *bp = (struct dri2_buffer_priv *)back->driverPrivate;

	if (!bp || !bp->pixmap)
		return FALSE;

	DRI2FrameEventPtr ev = frame_event_alloc();
	if (!ev)
		return FALSE;

	ev->drawable_id    = draw->id;
	ev->client         = client;
	ev->type           = DRI2_FLIP;
	ev->event_complete = func;
	ev->event_data     = data;
	ev->frame          = target_msc;
	ev->crtc           = crtc;
	ev->front          = front;
	ev->back           = back;

	if (amdgpu_do_pageflip(scrn, client, bp->pixmap,
	                       AMDGPU_DRM_QUEUE_ID_DEFAULT, ev, crtc,
	                       amdgpu_dri2_flip_event_handler,
	                       amdgpu_dri2_flip_event_abort_for_pageflip,
	                       FLIP_VSYNC,
	                       target_msc - amdgpu_get_msc_delta(draw, crtc))) {
		info->drmmode.dri2_flipping = TRUE;
		return TRUE;
	}

	/* Page-flip submission failed → clean up only our event; caller still owns buffers */
	frame_event_free(ev);
	return FALSE;
}

static AMDGPU_HOT Bool
can_exchange(ScrnInfoPtr pScrn, DrawablePtr draw,
             DRI2BufferPtr front, DRI2BufferPtr back)
{
	struct dri2_buffer_priv *bp = (struct dri2_buffer_priv *)back->driverPrivate;

	if (!bp || !bp->pixmap)
		return FALSE;
	if (!update_front(draw, front))
		return FALSE;

	struct dri2_buffer_priv *fp = (struct dri2_buffer_priv *)front->driverPrivate;
	PixmapPtr fpx = fp->pixmap;
	PixmapPtr bpx = bp->pixmap;

	return fpx->drawable.width  == bpx->drawable.width  &&
	       fpx->drawable.height == bpx->drawable.height &&
	       fpx->drawable.bitsPerPixel == bpx->drawable.bitsPerPixel &&
	       fpx->devKind == bpx->devKind;
}

static AMDGPU_HOT Bool
can_flip(xf86CrtcPtr crtc, DrawablePtr draw,
         DRI2BufferPtr front, DRI2BufferPtr back)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(pScrn);
	int num_crtcs_on = 0;

	if (unlikely(draw->type != DRAWABLE_WINDOW ||
	             !info->allowPageFlip || info->sprites_visible > 0 ||
	             info->drmmode.present_flipping || !pScrn->vtSema ||
	             !DRI2CanFlip(draw)))
		return FALSE;

	for (int i = 0; i < config->num_crtc; i++) {
		if (likely(config->crtc[i] != NULL && drmmode_crtc_can_flip(config->crtc[i])))
			num_crtcs_on++;
	}
	return likely(num_crtcs_on > 0 && can_exchange(pScrn, draw, front, back));
}

static AMDGPU_HOT void
amdgpu_dri2_exchange_buffers(DrawablePtr draw, DRI2BufferPtr front,
                             DRI2BufferPtr back)
{
	struct dri2_buffer_priv *fp = (struct dri2_buffer_priv *)front->driverPrivate;
	struct dri2_buffer_priv *bp = (struct dri2_buffer_priv *)back->driverPrivate;
	if (!fp || !bp || !fp->pixmap || !bp->pixmap)
		return;

	RegionRec region;
	region.extents.x1 = 0;
	region.extents.y1 = 0;
	region.extents.x2 = fp->pixmap->drawable.width;
	region.extents.y2 = fp->pixmap->drawable.height;
	region.data       = NULL;
	DamageRegionAppend(&fp->pixmap->drawable, &region);

	uint32_t tmp = front->name;
	front->name   = back->name;
	back->name    = tmp;

	struct amdgpu_pixmap *fpx = amdgpu_get_pixmap_private(fp->pixmap);
	struct amdgpu_pixmap *bpx = amdgpu_get_pixmap_private(bp->pixmap);
	amdgpu_set_pixmap_private(fp->pixmap, bpx);
	amdgpu_set_pixmap_private(bp->pixmap, fpx);

	ScreenPtr      s    = draw->pScreen;
	AMDGPUInfoPtr  info = AMDGPUPTR(xf86ScreenToScrn(s));
	if (fpx && fpx->bo == info->front_buffer && bpx && bpx->bo) {
		struct amdgpu_pixmap *scrpriv = amdgpu_get_pixmap_private(s->GetScreenPixmap(s));
		amdgpu_bo_ref(bpx->bo);
		amdgpu_bo_unref(&info->front_buffer);
		info->front_buffer = bpx->bo;
		if (scrpriv)
			*scrpriv = *bpx;
	}

	amdgpu_glamor_exchange_buffers(fp->pixmap, bp->pixmap);
	DamageRegionProcessPending(&fp->pixmap->drawable);
}

static AMDGPU_HOT void amdgpu_dri2_frame_event_handler(xf86CrtcPtr crtc, uint32_t seq,
                                                       uint64_t usec, void *event_data)
{
	DRI2FrameEventPtr event = (DRI2FrameEventPtr)event_data;
	ScrnInfoPtr scrn;
	DrawablePtr drawable;
	int status;
	int swap_type;
	BoxRec box;
	RegionRec local_region_obj;

	if (unlikely(!event))
		return;
	scrn = crtc->scrn;

	status = dixLookupDrawable(&drawable, event->drawable_id, serverClient, M_ANY, DixWriteAccess);
	if (unlikely(status != Success))
		goto cleanup_event;

	seq += amdgpu_get_msc_delta(drawable, crtc);

	switch (event->type) {
		case DRI2_FLIP:
			if (likely(can_flip(crtc, drawable, event->front, event->back) &&
			           amdgpu_dri2_schedule_flip(crtc, event->client, drawable,
			                                     event->front, event->back,
			                                     event->event_complete, event->event_data,
			                                     event->frame))) {
				amdgpu_dri2_exchange_buffers(drawable, event->front, event->back);
				break;
			}
			/* fallthrough */
		case DRI2_SWAP:
			if (likely(DRI2CanExchange(drawable) &&
			           can_exchange(scrn, drawable, event->front, event->back))) {
				amdgpu_dri2_exchange_buffers(drawable, event->front, event->back);
				swap_type = DRI2_EXCHANGE_COMPLETE;
			} else {
				box.x1 = 0; box.y1 = 0;
				box.x2 = drawable->width; box.y2 = drawable->height;
				RegionInit(&local_region_obj, &box, 0);
				amdgpu_dri2_copy_region2(drawable->pScreen, drawable, &local_region_obj,
				                         event->front, event->back);
				RegionUninit(&local_region_obj);
				swap_type = DRI2_BLIT_COMPLETE;
			}
			DRI2SwapComplete(event->client, drawable, seq, usec / 1000000,
			                 usec % 1000000, swap_type, event->event_complete, event->event_data);
			break;
		case DRI2_WAITMSC:
			DRI2WaitMSCComplete(event->client, drawable, seq, usec / 1000000, usec % 1000000);
			break;
		default:
			xf86DrvMsg(scrn->scrnIndex, X_WARNING, "%s: unknown vblank event received\n", __func__);
			break;
	}

cleanup_event:
	amdgpu_dri2_frame_event_abort(crtc, event_data);
}

static AMDGPU_HOT CARD32 amdgpu_dri2_extrapolate_msc_delay(xf86CrtcPtr crtc, CARD64 *target_msc,
                                                           CARD64 divisor, CARD64 remainder)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	CARD64 last_vblank_ust = drmmode_crtc->dpms_last_ust;
	uint32_t last_vblank_seq = drmmode_crtc->dpms_last_seq;
	CARD64 now, target_time_calc, delta_t_calc;
	int64_t d, delta_seq_calc_signed;
	int ret;
	CARD32 d_ms;
	uint64_t fps_recip = 0;
	int nominal_frame_rate = drmmode_crtc->dpms_last_fps;

	if (unlikely(!last_vblank_ust)) {
		*target_msc = 0;
		return FALLBACK_SWAP_DELAY;
	}
	if (unlikely(nominal_frame_rate == 0)) {
		xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "%s: nominal_frame_rate is zero.\n", __func__);
		*target_msc = 0;
		return FALLBACK_SWAP_DELAY;
	}
	fps_recip = (1ULL << 32) / (uint64_t)nominal_frame_rate;

	ret = drmmode_get_current_ust(pAMDGPUEnt->fd, &now);
	if (unlikely(ret)) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "%s: cannot get current time\n", __func__);
		*target_msc = 0;
		return FALLBACK_SWAP_DELAY;
	}

	delta_seq_calc_signed = (int64_t)(*target_msc - (CARD64)last_vblank_seq);
	delta_seq_calc_signed *= 1000000;
	target_time_calc = last_vblank_ust + (delta_seq_calc_signed / (CARD64)nominal_frame_rate);
	d = (int64_t)(target_time_calc - now);

	if (d < 0) {
		CARD64 current_msc = last_vblank_seq;
		delta_t_calc = now - last_vblank_ust;
		delta_seq_calc_signed = (int64_t)((delta_t_calc * fps_recip) >> 32);
		current_msc += (CARD64)delta_seq_calc_signed;
		current_msc &= 0xffffffffULL;

		if (divisor == 0) {
			*target_msc = current_msc;
			d = 0;
		} else {
			*target_msc = current_msc - (current_msc % divisor) + remainder;
			if ((current_msc % divisor) >= remainder)
				*target_msc += divisor;
			*target_msc &= 0xffffffffULL;

			delta_seq_calc_signed = (int64_t)(*target_msc - (CARD64)last_vblank_seq);
			delta_seq_calc_signed *= 1000000;
			target_time_calc = last_vblank_ust + (delta_seq_calc_signed / (CARD64)nominal_frame_rate);
			d = (int64_t)(target_time_calc - now);
		}
	}
	d_ms = (CARD32)((uint64_t)(d < 0 ? 0 : d) / 1000ULL);
	/* Ensure we wake a smidge after target (avoid off-by-one frame) */
	if (((CARD32)(d - (int64_t)d_ms * 1000)) > 0)
		d_ms += 2;
	else
		d_ms++;
	return d_ms;
}

static AMDGPU_HOT int amdgpu_dri2_get_msc(DrawablePtr draw, CARD64 *ust, CARD64 *msc)
{
	xf86CrtcPtr crtc = amdgpu_dri2_drawable_crtc(draw);
	if (unlikely(!crtc)) {
		*ust = 0;
		*msc = 0;
		return TRUE;
	}
	if (unlikely(!amdgpu_dri2_get_crtc_msc(crtc, ust, msc)))
		return FALSE;

	if (draw && draw->type == DRAWABLE_WINDOW)
		*msc += get_dri2_window_priv((WindowPtr)draw)->vblank_delta;

	*msc &= 0xffffffffULL;
	return TRUE;
}

static AMDGPU_HOT CARD32 amdgpu_dri2_deferred_event(OsTimerPtr timer, CARD32 now_timer, pointer data)
{
	(void)timer;
	(void)now_timer;

	DRI2FrameEventPtr event_info = (DRI2FrameEventPtr)data;
	xf86CrtcPtr crtc;
	ScrnInfoPtr scrn;
	AMDGPUEntPtr pAMDGPUEnt;
	CARD64 drm_now;
	int ret;
	CARD64 delta_t_calc, frame_val;
	int64_t delta_seq_calc_signed;
	drmmode_crtc_private_ptr drmmode_crtc_priv;
	uint64_t fps_recip = 0;

	if (unlikely(!event_info))
		return 0;
	crtc = event_info->crtc;

	if (unlikely(!crtc)) {
		ErrorF("%s: no crtc\n", __func__);
		if (event_info->drm_queue_seq)
			amdgpu_drm_abort_entry(event_info->drm_queue_seq);
		else
			amdgpu_dri2_frame_event_abort(NULL, data);
		return 0;
	}
	scrn = crtc->scrn;
	pAMDGPUEnt = AMDGPUEntPriv(scrn);
	drmmode_crtc_priv = crtc->driver_private;

	ret = drmmode_get_current_ust(pAMDGPUEnt->fd, &drm_now);
	if (unlikely(ret)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR, "%s: cannot get current time\n", __func__);
		if (event_info->drm_queue_seq) {
			drmmode_crtc_priv->drmmode->event_context.vblank_handler(pAMDGPUEnt->fd,
			                                                         0, 0, 0,
			                                                         (void*)event_info->drm_queue_seq);
			drmmode_crtc_priv->wait_flip_nesting_level++;
			amdgpu_drm_queue_handle_deferred(crtc);
		} else {
			amdgpu_dri2_frame_event_handler(crtc, 0, 0, data);
		}
		return 0;
	}

	if (unlikely(drmmode_crtc_priv->dpms_last_fps == 0)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "%s: dpms_last_fps is zero.\n", __func__);
		frame_val = (CARD64)drmmode_crtc_priv->dpms_last_seq;
	} else {
		fps_recip = (1ULL << 32) / (uint64_t)drmmode_crtc_priv->dpms_last_fps;
		delta_t_calc = drm_now - (CARD64)drmmode_crtc_priv->dpms_last_ust;
		delta_seq_calc_signed = (int64_t)((delta_t_calc * fps_recip) >> 32);
		frame_val = (CARD64)drmmode_crtc_priv->dpms_last_seq + (CARD64)delta_seq_calc_signed;
	}

	if (event_info->drm_queue_seq) {
		drmmode_crtc_priv->drmmode->event_context.vblank_handler(pAMDGPUEnt->fd,
		                                                         frame_val,
		                                                         drm_now / 1000000ULL,
		                                                         drm_now % 1000000ULL,
		                                                         (void*)event_info->drm_queue_seq);
		drmmode_crtc_priv->wait_flip_nesting_level++;
		amdgpu_drm_queue_handle_deferred(crtc);
	} else {
		amdgpu_dri2_frame_event_handler(crtc, (uint32_t)frame_val, drm_now, data);
	}
	return 0;
}

static AMDGPU_HOT void amdgpu_dri2_schedule_event(CARD32 delay, DRI2FrameEventPtr event_info)
{
	event_info->timer = TimerSet(NULL, 0, delay, amdgpu_dri2_deferred_event, event_info);
	if (delay == 0) {
		CARD32 current_time_ms = GetTimeInMillis();
		amdgpu_dri2_deferred_event(event_info->timer, current_time_ms, event_info);
	}
}

static AMDGPU_HOT int amdgpu_dri2_schedule_wait_msc(ClientPtr client, DrawablePtr draw,
                                                    CARD64 target_msc_in, CARD64 divisor_in,
                                                    CARD64 remainder_in)
{
	ScreenPtr screen = draw->pScreen;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	DRI2FrameEventPtr wait_info = NULL;
	uintptr_t drm_queue_seq = 0;
	xf86CrtcPtr crtc = amdgpu_dri2_drawable_crtc(draw);
	uint32_t msc_delta;
	uint32_t seq;
	CARD64 current_msc;
	CARD64 target_msc = target_msc_in;
	CARD64 divisor    = divisor_in;
	CARD64 remainder  = remainder_in;

	target_msc &= 0xffffffffULL;
	divisor    &= 0xffffffffULL;
	remainder  &= 0xffffffffULL;

	if (unlikely(!crtc))
		goto out_complete_no_alloc;

	msc_delta = amdgpu_get_msc_delta(draw, crtc);
	wait_info = frame_event_alloc();
	if (unlikely(!wait_info))
		goto out_complete_no_alloc;

	wait_info->drawable_id = draw->id;
	wait_info->client      = client;
	wait_info->type        = DRI2_WAITMSC;
	wait_info->crtc        = crtc;

	if (unlikely(!amdgpu_crtc_is_enabled(crtc))) {
		CARD32 delay;
		target_msc -= msc_delta;
		delay = amdgpu_dri2_extrapolate_msc_delay(crtc, &target_msc, divisor, remainder);
		amdgpu_dri2_schedule_event(delay, wait_info);
		DRI2BlockClient(client, draw);
		return TRUE;
	}
	if (unlikely(!drmmode_wait_vblank(crtc, DRM_VBLANK_RELATIVE, 0, 0, NULL, &seq))) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "get vblank counter failed: %s\n", strerror(errno));
		goto out_complete_with_alloc;
	}
	current_msc = (CARD64)seq + (CARD64)msc_delta;
	current_msc &= 0xffffffffULL;

	drm_queue_seq = amdgpu_drm_queue_alloc(crtc, client, AMDGPU_DRM_QUEUE_ID_DEFAULT,
	                                       wait_info, amdgpu_dri2_frame_event_handler,
	                                       amdgpu_dri2_frame_event_abort, FALSE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "Allocating DRM queue event entry failed.\n");
		goto out_complete_with_alloc;
	}
	wait_info->drm_queue_seq = drm_queue_seq;

	if (divisor == 0 || current_msc < target_msc) {
		if (current_msc >= target_msc)
			target_msc = current_msc;

		if (unlikely(!drmmode_wait_vblank(crtc, DRM_VBLANK_ABSOLUTE | DRM_VBLANK_EVENT,
		                                  (uint32_t)(target_msc - (CARD64)msc_delta),
		                                  drm_queue_seq, NULL, NULL))) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING, "get vblank counter failed: %s\n", strerror(errno));
			amdgpu_drm_abort_entry(drm_queue_seq);
			wait_info = NULL;
			goto out_complete_no_alloc;
		}
		DRI2BlockClient(client, draw);
		return TRUE;
	}

	target_msc = current_msc - (current_msc % divisor) + remainder - msc_delta;
	if ((current_msc % divisor) >= remainder)
		target_msc += divisor;

	if (unlikely(!drmmode_wait_vblank(crtc, DRM_VBLANK_ABSOLUTE | DRM_VBLANK_EVENT,
	                                  (uint32_t)target_msc, drm_queue_seq, NULL, NULL))) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "get vblank counter failed: %s\n", strerror(errno));
		amdgpu_drm_abort_entry(drm_queue_seq);
		wait_info = NULL;
		goto out_complete_no_alloc;
	}
	DRI2BlockClient(client, draw);
	return TRUE;

out_complete_with_alloc:
	if (wait_info)
		amdgpu_dri2_deferred_event(NULL, 0, wait_info);
	return TRUE;

out_complete_no_alloc:
	if (!wait_info)
		DRI2WaitMSCComplete(client, draw, 0, 0, 0);
	return TRUE;
}

static AMDGPU_HOT int amdgpu_dri2_schedule_swap(ClientPtr client, DrawablePtr draw,
                                                DRI2BufferPtr front, DRI2BufferPtr back,
                                                CARD64 *target_msc_inout, CARD64 divisor_in,
                                                CARD64 remainder_in, DRI2SwapEventPtr func, void *data)
{
	ScreenPtr screen = draw->pScreen;
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	xf86CrtcPtr crtc = amdgpu_dri2_drawable_crtc(draw);
	uint32_t msc_delta;
	drmVBlankSeqType type;
	uint32_t seq;
	int flip = 0;
	DRI2FrameEventPtr swap_info = NULL;
	uintptr_t drm_queue_seq = 0;
	CARD64 current_msc, event_msc;
	BoxRec box;
	RegionRec local_region_obj;
	CARD64 target_msc_local = *target_msc_inout;
	CARD64 divisor   = divisor_in;
	CARD64 remainder = remainder_in;

	target_msc_local &= 0xffffffffULL;
	divisor          &= 0xffffffffULL;
	remainder        &= 0xffffffffULL;

	amdgpu_dri2_ref_buffer(front);
	amdgpu_dri2_ref_buffer(back);

	if (unlikely(!crtc))
		goto blit_fallback_no_alloc;

	msc_delta = amdgpu_get_msc_delta(draw, crtc);
	swap_info = frame_event_alloc();
	if (unlikely(!swap_info))
		goto blit_fallback_no_alloc;

	swap_info->type           = DRI2_SWAP;
	swap_info->drawable_id    = draw->id;
	swap_info->client         = client;
	swap_info->event_complete = func;
	swap_info->event_data     = data;
	swap_info->front          = front;
	swap_info->back           = back;
	swap_info->crtc           = crtc;

	drm_queue_seq = amdgpu_drm_queue_alloc(crtc, client, AMDGPU_DRM_QUEUE_ID_DEFAULT,
	                                       swap_info, amdgpu_dri2_frame_event_handler,
	                                       amdgpu_dri2_frame_event_abort, FALSE);
	if (unlikely(drm_queue_seq == AMDGPU_DRM_QUEUE_ERROR)) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "Allocating DRM queue event entry failed.\n");
		goto blit_fallback_with_alloc;
	}
	swap_info->drm_queue_seq = drm_queue_seq;

	if (unlikely(!amdgpu_crtc_is_enabled(crtc))) {
		CARD32 delay;
		target_msc_local -= msc_delta;
		delay = amdgpu_dri2_extrapolate_msc_delay(crtc, &target_msc_local, divisor, remainder);
		target_msc_local += msc_delta;
		target_msc_local &= 0xffffffffULL;
		amdgpu_dri2_schedule_event(delay, swap_info);
		*target_msc_inout = target_msc_local;
		return TRUE;
	}
	if (unlikely(!drmmode_wait_vblank(crtc, DRM_VBLANK_RELATIVE, 0, 0, NULL, &seq))) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "first get vblank counter failed: %s\n", strerror(errno));
		amdgpu_drm_abort_entry(drm_queue_seq);
		swap_info = NULL;
		goto blit_fallback_no_alloc;
	}
	current_msc = (CARD64)seq + (CARD64)msc_delta;
	current_msc &= 0xffffffffULL;

	if (likely(can_flip(crtc, draw, front, back))) {
		swap_info->type = DRI2_FLIP;
		flip = 1;
	}

	if (target_msc_local > 0)
		target_msc_local -= (CARD64)flip;

	if (divisor == 0 || current_msc < target_msc_local) {
		type = DRM_VBLANK_ABSOLUTE | DRM_VBLANK_EVENT;
		if (flip == 0)
			type |= DRM_VBLANK_NEXTONMISS;

		if (current_msc >= target_msc_local)
			target_msc_local = current_msc;

		if (unlikely(!drmmode_wait_vblank(crtc, type,
		                                  (uint32_t)(target_msc_local - (CARD64)msc_delta),
		                                  drm_queue_seq, NULL, &seq))) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING, "divisor 0 get vblank counter failed: %s\n", strerror(errno));
			amdgpu_drm_abort_entry(drm_queue_seq);
			swap_info = NULL;
			goto blit_fallback_no_alloc;
		}
		target_msc_local = (CARD64)seq + (CARD64)flip + (CARD64)msc_delta;
		target_msc_local &= 0xffffffffULL;
		swap_info->frame = (unsigned)target_msc_local;
		*target_msc_inout = target_msc_local;
		return TRUE;
	}

	type = DRM_VBLANK_ABSOLUTE | DRM_VBLANK_EVENT;
	if (flip == 0)
		type |= DRM_VBLANK_NEXTONMISS;

	event_msc = current_msc - (current_msc % divisor) + remainder - msc_delta;
	if (event_msc <= current_msc)
		event_msc += divisor;
	event_msc -= (CARD64)flip;

	if (unlikely(!drmmode_wait_vblank(crtc, type, (uint32_t)event_msc,
	                                  drm_queue_seq, NULL, &seq))) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING, "final get vblank counter failed: %s\n", strerror(errno));
		amdgpu_drm_abort_entry(drm_queue_seq);
		swap_info = NULL;
		goto blit_fallback_no_alloc;
	}
	target_msc_local = (CARD64)seq + (CARD64)flip + (CARD64)msc_delta;
	target_msc_local &= 0xffffffffULL;
	swap_info->frame = (unsigned)target_msc_local;
	*target_msc_inout = target_msc_local;
	return TRUE;

blit_fallback_with_alloc:
	if (swap_info) {
		/* We have an allocated event; schedule a short deferred event to complete via BLIT */
		swap_info->type = DRI2_SWAP;
		amdgpu_dri2_schedule_event(FALLBACK_SWAP_DELAY, swap_info);
	}
	goto end_blit_fallback;

blit_fallback_no_alloc:
	box.x1 = 0; box.y1 = 0; box.x2 = draw->width; box.y2 = draw->height;
	RegionInit(&local_region_obj, &box, 0);
	amdgpu_dri2_copy_region2(draw->pScreen, draw, &local_region_obj, front, back);
	RegionUninit(&local_region_obj);
	DRI2SwapComplete(client, draw, 0, 0, 0, DRI2_BLIT_COMPLETE, func, data);
	amdgpu_dri2_unref_buffer(front);
	amdgpu_dri2_unref_buffer(back);

end_blit_fallback:
	*target_msc_inout = 0;
	return TRUE;
}

AMDGPU_COLD Bool amdgpu_dri2_screen_init(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	DRI2InfoRec dri2_info;
	const char *driverNames[2];
	Bool scheduling_works = TRUE;

	if (unlikely(!info->dri2.available))
		return FALSE;

	info->dri2.device_name = drmGetDeviceNameFromFd(pAMDGPUEnt->fd);
	if (unlikely(!info->dri2.device_name)) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Failed to get device name from DRM FD.\n");
		return FALSE;
	}

	memset(&dri2_info, 0, sizeof(dri2_info));
	dri2_info.driverName = SI_DRIVER_NAME;
	dri2_info.fd = pAMDGPUEnt->fd;
	dri2_info.deviceName = info->dri2.device_name;

	if (info->drmmode.count_crtcs > 2) {
		uint64_t cap_value = 0;
		if (unlikely(drmGetCap(pAMDGPUEnt->fd, DRM_CAP_VBLANK_HIGH_CRTC, &cap_value))) {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
			           "You need a newer kernel for VBLANKs on CRTC > 1\n");
			scheduling_works = FALSE;
		} else if (unlikely(!cap_value)) {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
			           "Your kernel does not handle VBLANKs on CRTC > 1\n");
			scheduling_works = FALSE;
		}
	}
	if (likely(scheduling_works)) {
		dri2_info.ScheduleSwap   = amdgpu_dri2_schedule_swap;
		dri2_info.GetMSC         = amdgpu_dri2_get_msc;
		dri2_info.ScheduleWaitMSC= amdgpu_dri2_schedule_wait_msc;
		dri2_info.numDrivers     = 2;
		dri2_info.driverNames    = driverNames;
		driverNames[0] = driverNames[1] = dri2_info.driverName;

		if (unlikely(DRI2InfoCnt == 0)) {
			if (unlikely(!dixRegisterPrivateKey(dri2_window_private_key, PRIVATE_WINDOW,
			                                    sizeof(struct dri2_window_priv)))) {
				xf86DrvMsg(pScrn->scrnIndex, X_WARNING, "Failed to get DRI2 window private\n");
				return FALSE;
			}
			AddCallback(&ClientStateCallback, amdgpu_dri2_client_state_changed, 0);
		}
		DRI2InfoCnt++;
	}
	dri2_info.version      = 9;
	dri2_info.CreateBuffer2= amdgpu_dri2_create_buffer2;
	dri2_info.DestroyBuffer2= amdgpu_dri2_destroy_buffer2;
	dri2_info.CopyRegion2  = amdgpu_dri2_copy_region2;

	info->dri2.enabled = DRI2ScreenInit(pScreen, &dri2_info);
	return info->dri2.enabled;
}

AMDGPU_COLD void amdgpu_dri2_close_screen(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	if (--DRI2InfoCnt == 0)
		DeleteCallback(&ClientStateCallback, amdgpu_dri2_client_state_changed, 0);

	/* Free frame event free-list */
	gl_lock();
	DRI2FrameEventPtr curr = fe_free_list;
	while (curr) {
		DRI2FrameEventPtr next_ev = curr->next;
		free(curr);
		curr = next_ev;
	}
	fe_free_list = NULL;
	fe_free_list_length = 0;
	gl_unlock();

	/* Free any cached GCs */
	for (int d = 0; d <= MAX_GC_DEPTH; ++d) {
		for (int i = 0; i < gc_cache_cnt[d]; ++i) {
			if (gc_cache[d][i])
				FreeScratchGC(gc_cache[d][i]);
		}
		gc_cache_cnt[d] = 0;
	}

	/* Reset flink cache */
	gl_lock();
	memset(flink_cache, 0, sizeof(flink_cache));
	flink_cache_idx = 0;
	flink_mru.valid = FALSE;
	gl_unlock();

	DRI2CloseScreen(pScreen);
	if (info->dri2.device_name) {
		drmFree(info->dri2.device_name);
		info->dri2.device_name = NULL;
	}
}

#endif /* DRI2 */
