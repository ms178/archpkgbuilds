/*
 * Copyright © 2013-2024 Advanced Micro Devices, Inc.
 *
 * DRI3 support for AMDGPU Xorg driver.
 * Optimized for minimal latency and maximal throughput on modern CPUs.
 */

#include "config.h"

#include <xorg-server.h>
#include <amdgpu_drv.h>
#include <amdgpu_glamor.h>
#include <amdgpu_pixmap.h>
#include <dri3.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>

/* ========================================================================
 * Compiler hints for better code generation and branch prediction
 * ======================================================================== */
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

/* ========================================================================
 * RAII-style automatic file descriptor cleanup
 *
 * This uses GCC/Clang's cleanup attribute to guarantee FD closure on all
 * code paths (normal return, early return, goto error labels). This
 * prevents FD leaks which would exhaust the process FD limit over time.
 *
 * Usage:
 *   auto_close_fd int fd = open(...);
 *   // fd automatically closed when it goes out of scope
 *   // To "move" ownership out, set fd = -1 before returning
 * ======================================================================== */
static inline void auto_close_fd_helper(int *fd)
{
	if (*fd >= 0) {
		(void)close(*fd);
	}
}

#define auto_close_fd __attribute__((cleanup(auto_close_fd_helper)))

/* ========================================================================
 * DRI3 node opening functions
 * ======================================================================== */

/*
 * open_render_node: Open the render node for unprivileged rendering.
 *
 * This is the fast path for modern systems with render nodes. It avoids
 * the legacy DRM authentication protocol, saving two ioctls per client
 * connection (drmGetMagic + drmAuthMagic).
 *
 * Marked COLD because it's called once per client connection, not per-frame.
 */
static int AMDGPU_COLD
open_render_node(ScreenPtr screen, int *out_fd)
{
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	int fd;

	if (UNLIKELY(!pAMDGPUEnt->render_node)) {
		return BadMatch;
	}

	fd = open(pAMDGPUEnt->render_node, O_RDWR | O_CLOEXEC);
	if (UNLIKELY(fd < 0)) {
		return BadAlloc;
	}

	*out_fd = fd;
	return Success;
}

/*
 * open_card_node: Open the legacy card node and perform DRM authentication.
 *
 * This is the fallback path for:
 * - Systems without render nodes (old kernels)
 * - Scenarios requiring master privileges
 *
 * The authentication dance (drmGetMagic + drmAuthMagic) adds two syscalls
 * but is necessary for security on legacy setups.
 *
 * Marked COLD as it's a fallback and only called during connection setup.
 */
static int AMDGPU_COLD
open_card_node(ScreenPtr screen, int *out_fd)
{
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);
	drm_magic_t magic;
	auto_close_fd int fd = -1;

	fd = open(info->dri2.device_name, O_RDWR | O_CLOEXEC);
	if (UNLIKELY(fd < 0)) {
		return BadAlloc;
	}

	/*
	 * Try to get the DRM magic token. If this fails with EACCES, the
	 * device is likely a render node or otherwise doesn't require
	 * authentication, so we can proceed directly.
	 */
	if (UNLIKELY(drmGetMagic(fd, &magic) < 0)) {
		if (errno == EACCES) {
			/* No authentication needed; move FD ownership to caller */
			*out_fd = fd;
			fd = -1; /* Disarm auto-closer */
			return Success;
		}
		/* Other errors are fatal */
		return BadMatch;
	}

	/*
	 * Authenticate the FD with the server's master FD.
	 * This is the legacy security model for GPU access control.
	 */
	if (UNLIKELY(drmAuthMagic(pAMDGPUEnt->fd, magic) < 0)) {
		return BadMatch;
	}

	/* Move FD ownership to caller */
	*out_fd = fd;
	fd = -1; /* Disarm auto-closer */
	return Success;
}

/*
 * amdgpu_dri3_open: Main entry point for DRI3 FD passing.
 *
 * Called by the DRI3 extension when a client requests a DRM FD.
 * Priority order:
 *   1. Render node (fast, modern, unprivileged)
 *   2. Card node (slow, legacy, requires authentication)
 *
 * Marked COLD as it's only called once per client connection.
 */
static int AMDGPU_COLD
amdgpu_dri3_open(ScreenPtr screen, RRProviderPtr provider, int *out_fd)
{
	(void)provider; /* Unused parameter */

	/*
	 * Try render node first. On a typical modern system with render
	 * nodes, this succeeds immediately and we avoid the auth overhead.
	 */
	if (LIKELY(open_render_node(screen, out_fd) == Success)) {
		return Success;
	}

	/* Fallback to card node for compatibility */
	return open_card_node(screen, out_fd);
}

/* ========================================================================
 * DRI3 pixmap validation and conversion functions
 * ======================================================================== */

/*
 * validate_pixmap_dims: Fast validation of pixmap parameters.
 *
 * This is a critical security check to reject malformed client requests
 * before they reach expensive code paths (BO allocation, GPU setup).
 *
 * Validates:
 * - Non-zero dimensions
 * - Canonical depth/bpp combinations
 * - Stride large enough to hold a scanline
 * - No integer overflow in stride calculation
 *
 * Marked inline to reduce call overhead in the hot path.
 * Marked HOT as it's called for every DRI3 pixmap import/export.
 */
static inline Bool AMDGPU_HOT
validate_pixmap_dims(CARD16 width, CARD16 height, CARD16 stride,
                     CARD8 depth, CARD8 bpp)
{
	uint32_t min_stride;

	/* Reject zero-sized pixmaps */
	if (UNLIKELY(width == 0 || height == 0)) {
		return FALSE;
	}

	/*
	 * Validate depth/bpp combinations.
	 * Only allow formats that are universally supported by X11, DRM, and GBM.
	 */
	switch (bpp) {
	case 8:
		if (UNLIKELY(depth != 8)) return FALSE;
		break;
	case 16:
		if (UNLIKELY(depth != 15 && depth != 16)) return FALSE;
		break;
	case 32:
		/*
		 * Allow depth 24 (RGB) and 32 (RGBA).
		 * Reject lower depths to catch malformed requests.
		 */
		if (UNLIKELY(depth < 24)) return FALSE;
		break;
	default:
		/* Reject uncommon bpp values */
		return FALSE;
	}

	/*
	 * Check stride >= width * bytes_per_pixel.
	 * Use uint32_t arithmetic to avoid overflow (width and bpp are 16-bit).
	 * This prevents integer overflow attacks where stride wraps around.
	 */
	min_stride = (uint32_t)width * ((uint32_t)bpp >> 3);
	if (UNLIKELY(stride < min_stride)) {
		return FALSE;
	}

	return TRUE;
}

/*
 * amdgpu_dri3_pixmap_from_fd: Import a DMA-BUF FD as a pixmap.
 *
 * This is one of the two hot paths in DRI3. It's called every time a client
 * shares a rendered buffer with the compositor or X server.
 *
 * Optimization strategy:
 * 1. Validate parameters early (fail-fast)
 * 2. Use glamor path when available (common case on modern systems)
 * 3. Fall back to DDX path only when necessary
 * 4. Mark all error paths with UNLIKELY to keep happy path linear
 *
 * Marked HOT as it's called frequently during rendering and compositing.
 */
static PixmapPtr AMDGPU_HOT
amdgpu_dri3_pixmap_from_fd(ScreenPtr screen,
                           int fd,
                           CARD16 width,
                           CARD16 height,
                           CARD16 stride,
                           CARD8 depth,
                           CARD8 bpp)
{
	PixmapPtr pixmap;

	/*
	 * Validate early to avoid expensive operations on bad input.
	 * This prevents DoS via malformed client requests.
	 */
	if (UNLIKELY(!validate_pixmap_dims(width, height, stride, depth, bpp))) {
		return NULL;
	}

#ifdef USE_GLAMOR
	{
		ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
		AMDGPUInfoPtr info = AMDGPUPTR(scrn);

		/*
		 * Glamor path: This is the modern, accelerated path used by
		 * nearly all current AMDGPU deployments. Mark it as LIKELY.
		 */
		if (LIKELY(info->use_glamor)) {
			pixmap = glamor_pixmap_from_fd(screen, fd, width, height,
						       stride, depth, bpp);
			if (LIKELY(pixmap)) {
				/*
				 * Attach driver private data if not already present.
				 * glamor may or may not allocate this, so check first.
				 */
				if (UNLIKELY(!amdgpu_get_pixmap_private(pixmap))) {
					struct amdgpu_pixmap *priv;
					priv = calloc(1, sizeof(*priv));
					if (UNLIKELY(!priv)) {
						screen->DestroyPixmap(pixmap);
						return NULL;
					}
					amdgpu_set_pixmap_private(pixmap, priv);
				}
				/*
				 * Mark the pixmap as DRI2-originated for bookkeeping.
				 * This flag is used by other parts of the driver.
				 */
				pixmap->usage_hint |= AMDGPU_CREATE_PIXMAP_DRI2;
				return pixmap;
			}
			/* Glamor failed; return NULL immediately */
			return NULL;
		}
	}
#endif /* USE_GLAMOR */

	/*
	 * Non-glamor fallback path: Classic DDX rendering.
	 * This is rare on modern systems but needed for compatibility.
	 */
	pixmap = screen->CreatePixmap(screen, 0, 0, depth,
				      AMDGPU_CREATE_PIXMAP_DRI2);
	if (UNLIKELY(!pixmap)) {
		return NULL;
	}

	if (UNLIKELY(!screen->ModifyPixmapHeader(pixmap, width, height,
	                                         0, bpp, stride, NULL))) {
		screen->DestroyPixmap(pixmap);
		return NULL;
	}

	/*
	 * SetSharedPixmapBacking takes ownership of the FD in the DDX path.
	 * Pass it as a pointer-sized integer per X11 conventions.
	 */
	if (LIKELY(screen->SetSharedPixmapBacking(pixmap,
	                                          (void *)(intptr_t)fd))) {
		return pixmap;
	}

	/* Cleanup on failure */
	screen->DestroyPixmap(pixmap);
	return NULL;
}

/*
 * amdgpu_dri3_fd_from_pixmap: Export a pixmap as a DMA-BUF FD.
 *
 * This is the second hot path in DRI3, called when the X server or
 * compositor needs to share a pixmap with a client for direct rendering.
 *
 * Optimization strategy:
 * 1. Try glamor path first (common case)
 * 2. Flush GPU to ensure coherency before handing off FD
 * 3. Fall back to BO export for non-glamor path
 * 4. Mark all error checks with UNLIKELY
 *
 * Marked HOT as it's called frequently during rendering.
 */
static int AMDGPU_HOT
amdgpu_dri3_fd_from_pixmap(ScreenPtr screen,
                           PixmapPtr pixmap,
                           CARD16 *stride,
                           CARD32 *size)
{
#ifdef USE_GLAMOR
	{
		ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
		AMDGPUInfoPtr info = AMDGPUPTR(scrn);

		/*
		 * Glamor path: Try to export via glamor's EGL-based mechanism.
		 * This is faster and more integrated with the GL driver.
		 */
		if (LIKELY(info->use_glamor)) {
			int fd = glamor_fd_from_pixmap(screen, pixmap, stride, size);
			if (LIKELY(fd >= 0)) {
				/*
				 * CRITICAL: Flush all pending GPU work before handing
				 * the FD to the client. Otherwise the client may read
				 * incomplete rendering, causing corruption.
				 *
				 * This inserts a glFlush() + fence to ensure coherency.
				 */
				amdgpu_glamor_flush(scrn);
				return fd;
			}
			/* Glamor failed; fall through to BO export */
		}
	}
#endif /* USE_GLAMOR */

	/*
	 * Non-glamor fallback: Export the underlying GEM BO directly.
	 */
	{
		struct amdgpu_buffer *bo = amdgpu_get_pixmap_bo(pixmap);
		if (UNLIKELY(!bo)) {
			return -1;
		}

		/*
		 * The DRI3 protocol uses CARD16 for stride, so verify it fits.
		 * Modern displays can exceed this (e.g., 8K @ 32bpp = 122KB stride),
		 * but DRI3 v1.0 doesn't support larger strides.
		 */
		if (UNLIKELY(pixmap->devKind > UINT16_MAX)) {
			return -1;
		}

		/*
		 * Query BO metadata to get the allocation size.
		 * This is needed by the client to map the buffer correctly.
		 */
		struct amdgpu_bo_info bo_info;
		if (UNLIKELY(amdgpu_bo_query_info(bo->bo.amdgpu, &bo_info) != 0)) {
			return -1;
		}

		/*
		 * Export the BO as a DMA-BUF file descriptor.
		 * This is a kernel operation that creates a new FD referencing
		 * the same GPU memory, with refcounting handled by the kernel.
		 */
		uint32_t fd_out;
		if (UNLIKELY(amdgpu_bo_export(bo->bo.amdgpu,
		                              amdgpu_bo_handle_type_dma_buf_fd,
		                              &fd_out) != 0)) {
			return -1;
		}

		*stride = (CARD16)pixmap->devKind;
		*size = (CARD32)bo_info.alloc_size;
		return (int)fd_out;
	}
}

/* ========================================================================
 * DRI3 screen info structure
 * ======================================================================== */

/*
 * This structure is registered with the DRI3 extension and defines the
 * callbacks for FD passing and pixmap conversion.
 *
 * Version 0 is used for maximum compatibility with older X servers.
 */
static dri3_screen_info_rec amdgpu_dri3_screen_info = {
	.version = 0,
	.open = amdgpu_dri3_open,
	.pixmap_from_fd = amdgpu_dri3_pixmap_from_fd,
	.fd_from_pixmap = amdgpu_dri3_fd_from_pixmap
};

/* ========================================================================
 * Public initialization function
 * ======================================================================== */

/*
 * amdgpu_dri3_screen_init: Initialize DRI3 support for a screen.
 *
 * Called once during server startup or screen reconfiguration.
 * Caches the render node path for fast access during client connections.
 *
 * Marked COLD as it's only called during initialization.
 */
Bool AMDGPU_COLD
amdgpu_dri3_screen_init(ScreenPtr screen)
{
	ScrnInfoPtr scrn = xf86ScreenToScrn(screen);
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(scrn);

	/*
	 * Cache the render node device path. This avoids repeated syscalls
	 * to query the kernel on every client connection.
	 *
	 * drmGetRenderDeviceNameFromFd() allocates memory; we take ownership.
	 */
	pAMDGPUEnt->render_node = drmGetRenderDeviceNameFromFd(pAMDGPUEnt->fd);

	/*
	 * Register our DRI3 callbacks with the X server.
	 * This makes the server call our functions for FD passing.
	 */
	if (UNLIKELY(!dri3_screen_init(screen, &amdgpu_dri3_screen_info))) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
		           "DRI3 initialization failed\n");
		/*
		 * Clean up the cached render node path on failure.
		 * Note: drmGetRenderDeviceNameFromFd() uses malloc(), so use free().
		 */
		free(pAMDGPUEnt->render_node);
		pAMDGPUEnt->render_node = NULL;
		return FALSE;
	}

	return TRUE;
}
