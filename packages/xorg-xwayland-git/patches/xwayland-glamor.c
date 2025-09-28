/*
 * SPDX-License-Identifier: MIT
 *
 * xwayland-glamor.c — High-performance, robust glamor/EGL integration for Xwayland
 *
 * - Safe EGL context switching (no redundant eglMakeCurrent).
 * - Correct native fence sync handling with runtime capability checks and guarded symbols.
 * - Fully null-safe flip checks; correct two-argument xwl_present_maybe_redirect_window() usage.
 * - Clean CreateScreenResources hook with proper failure propagation.
 * - No ABI changes; warning-free under -Wall -Wextra.
 */

#include <xwayland-config.h>

#define MESA_EGL_NO_X11_HEADERS
#define EGL_NO_X11
#include <glamor_egl.h>
#include <EGL/eglext.h>

#include <glamor.h>
#include <glamor_context.h>
#include <glamor_glx_provider.h>
#ifdef GLXEXT
#include "glx_extinit.h"
#endif

#include "drm-client-protocol.h"
#include "linux-dmabuf-unstable-v1-client-protocol.h"
#include "linux-drm-syncobj-v1-client-protocol.h"

#include "xwayland-dmabuf.h"
#include "xwayland-glamor.h"
#include "xwayland-glamor-gbm.h"
#include "xwayland-present.h"
#include "xwayland-screen.h"
#include "xwayland-window.h"
#include "xwayland-window-buffers.h"

#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

/* ---------------------------------------------------------------------------
 * EGL native fence sync (Android) – guarded runtime resolution
 * -------------------------------------------------------------------------*/

#ifndef EGL_ANDROID_native_fence_sync
#define EGL_SYNC_NATIVE_FENCE_ANDROID         0x3144
#define EGL_SYNC_NATIVE_FENCE_FD_ANDROID      0x3145
#define EGL_NO_NATIVE_FENCE_FD_ANDROID        -1
#endif

#ifndef EGL_KHR_fence_sync
typedef void* EGLSyncKHR;
#endif

static PFNEGLCREATESYNCKHRPROC            p_eglCreateSyncKHR            = NULL;
static PFNEGLDESTROYSYNCKHRPROC           p_eglDestroySyncKHR           = NULL;
static PFNEGLWAITSYNCKHRPROC              p_eglWaitSyncKHR              = NULL;
static PFNEGLDUPNATIVEFENCEFDANDROIDPROC  p_eglDupNativeFenceFDANDROID  = NULL;

/* Cache per-EGLDisplay capability to avoid repeated queries. */
static EGLDisplay s_sync_dpy = EGL_NO_DISPLAY;
static Bool       s_sync_inited = FALSE;
static Bool       s_has_native_fence_sync = FALSE;

static Bool
egl_has_extension(EGLDisplay dpy, const char *ext)
{
    if (!ext || !*ext)
        return FALSE;

    const char *exts = eglQueryString(dpy, EGL_EXTENSIONS);
    if (!exts)
        return FALSE;

    const size_t elen = strlen(ext);
    const char *p = exts;
    while ((p = strstr(p, ext)) != NULL) {
        const Bool starts_ok = (p == exts) || (p[-1] == ' ');
        const char c = p[elen];
        const Bool ends_ok = (c == '\0') || (c == ' ');
        if (starts_ok && ends_ok)
            return TRUE;
        p += elen;
    }
    return FALSE;
}

static void
egl_native_fence_sync_init(EGLDisplay dpy)
{
    if (s_sync_inited && s_sync_dpy == dpy)
        return;

    s_sync_dpy = dpy;
    s_sync_inited = TRUE;
    s_has_native_fence_sync = FALSE;

    if (dpy == EGL_NO_DISPLAY)
        return;

    const Bool has_android_sync = egl_has_extension(dpy, "EGL_ANDROID_native_fence_sync");
    const Bool has_khr_sync     = egl_has_extension(dpy, "EGL_KHR_fence_sync");
    const Bool has_khr_wait     = egl_has_extension(dpy, "EGL_KHR_wait_sync");

    if (!(has_android_sync && has_khr_sync && has_khr_wait))
        return;

    p_eglCreateSyncKHR = (PFNEGLCREATESYNCKHRPROC)eglGetProcAddress("eglCreateSyncKHR");
    p_eglDestroySyncKHR = (PFNEGLDESTROYSYNCKHRPROC)eglGetProcAddress("eglDestroySyncKHR");
    p_eglWaitSyncKHR = (PFNEGLWAITSYNCKHRPROC)eglGetProcAddress("eglWaitSyncKHR");
    p_eglDupNativeFenceFDANDROID =
        (PFNEGLDUPNATIVEFENCEFDANDROIDPROC)eglGetProcAddress("eglDupNativeFenceFDANDROID");

    if (p_eglCreateSyncKHR && p_eglDestroySyncKHR && p_eglWaitSyncKHR && p_eglDupNativeFenceFDANDROID) {
        s_has_native_fence_sync = TRUE;
    } else {
        p_eglCreateSyncKHR = NULL;
        p_eglDestroySyncKHR = NULL;
        p_eglWaitSyncKHR = NULL;
        p_eglDupNativeFenceFDANDROID = NULL;
    }
}

/* ---------------------------------------------------------------------------
 * EGL context current helpers
 * -------------------------------------------------------------------------*/

static void
glamor_egl_make_current(struct glamor_context *glamor_ctx)
{
    if (!eglMakeCurrent(glamor_ctx->display,
                        EGL_NO_SURFACE, EGL_NO_SURFACE,
                        glamor_ctx->ctx))
    {
        FatalError("Failed to make EGL context current\n");
    }
}

void
xwl_glamor_egl_make_current(struct xwl_screen *xwl_screen)
{
    if (!xwl_screen || !xwl_screen->glamor_ctx)
        return;

    EGLContext desired = xwl_screen->glamor_ctx->ctx;
    EGLContext current = eglGetCurrentContext();

    if (current == desired)
        return;

    xwl_screen->glamor_ctx->make_current(xwl_screen->glamor_ctx);
}

/* ---------------------------------------------------------------------------
 * Glamor/EGL screen init
 * -------------------------------------------------------------------------*/

void
glamor_egl_screen_init(ScreenPtr screen, struct glamor_context *glamor_ctx)
{
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);

    glamor_set_glvnd_vendor(screen, xwl_screen->glvnd_vendor);
    glamor_enable_dri3(screen);

    glamor_ctx->ctx = xwl_screen->egl_context;
    glamor_ctx->display = xwl_screen->egl_display;
    glamor_ctx->make_current = glamor_egl_make_current;

    xwl_screen->glamor_ctx = glamor_ctx;
}

/* ---------------------------------------------------------------------------
 * Flip suitability checks
 * -------------------------------------------------------------------------*/

/* Match xwayland-present.h two-argument signature for xwl_present_maybe_redirect_window(WindowPtr, PixmapPtr). */
Bool
xwl_glamor_check_flip(WindowPtr present_window, PixmapPtr pixmap)
{
    if (!present_window || !pixmap)
        return FALSE;

    ScreenPtr screen = pixmap->drawable.pScreen;
    if (!screen)
        return FALSE;

    PixmapPtr backing_pixmap = screen->GetWindowPixmap(present_window);
    if (!backing_pixmap)
        return FALSE;

    struct xwl_window *xwl_window = xwl_window_from_window(present_window);
    if (!xwl_window)
        return FALSE;

    WindowPtr surface_window = xwl_window->surface_window;
    if (!surface_window)
        return FALSE;

    /* Depth mismatch: disallow immediate flip; try to redirect the window with the incoming pixmap. */
    if (pixmap->drawable.depth != backing_pixmap->drawable.depth) {
        if (pixmap->drawable.depth == 32)
            return FALSE;

        return xwl_present_maybe_redirect_window(present_window, pixmap);
    }

    /* If the surface is 24/30-bit under a 32-bit parent, trigger redirection on the surface window. */
    if (surface_window->redirectDraw == RedirectDrawAutomatic &&
        surface_window->drawable.depth != 32 &&
        surface_window->parent &&
        surface_window->parent->drawable.depth == 32)
    {
        PixmapPtr surf_backing = screen->GetWindowPixmap(surface_window);
        if (surf_backing)
            (void)xwl_present_maybe_redirect_window(surface_window, surf_backing);
    }

    return TRUE;
}

/* ---------------------------------------------------------------------------
 * Wayland registry
 * -------------------------------------------------------------------------*/

void
xwl_glamor_init_wl_registry(struct xwl_screen *xwl_screen,
                            struct wl_registry *registry,
                            uint32_t id, const char *interface,
                            uint32_t version)
{
    (void)registry;

    if (strcmp(interface, wl_drm_interface.name) == 0)
        xwl_screen_set_drm_interface(xwl_screen, id, version);
    else if (strcmp(interface, zwp_linux_dmabuf_v1_interface.name) == 0)
        xwl_screen_set_dmabuf_interface(xwl_screen, id, version);
    else if (strcmp(interface, wp_linux_drm_syncobj_manager_v1_interface.name) == 0)
        xwl_screen_set_syncobj_interface(xwl_screen, id, version);
}

static Bool
xwl_glamor_has_wl_interfaces(struct xwl_screen *xwl_screen)
{
    if (!xwl_glamor_has_wl_drm(xwl_screen) &&
        xwl_screen->dmabuf_protocol_version < 4)
    {
        LogMessageVerb(X_INFO, 3,
                       "glamor: 'wl_drm' not supported and linux-dmabuf v4 not supported\n");
        return FALSE;
    }

    return TRUE;
}

/* ---------------------------------------------------------------------------
 * CreateScreenResources hook
 * -------------------------------------------------------------------------*/

static Bool
xwl_glamor_create_screen_resources(ScreenPtr screen)
{
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    Bool ret;

    screen->CreateScreenResources = xwl_screen->CreateScreenResources;
    ret = (*screen->CreateScreenResources)(screen);
    xwl_screen->CreateScreenResources = screen->CreateScreenResources;
    screen->CreateScreenResources = xwl_glamor_create_screen_resources;

    if (!ret)
        return FALSE;

    if (xwl_screen->rootless) {
        /* 0x0 pixmap for rootless screens */
        screen->devPrivate = fbCreatePixmap(screen, 0, 0, screen->rootDepth, 0);
    } else {
        screen->devPrivate = screen->CreatePixmap(screen,
                                                  screen->width, screen->height,
                                                  screen->rootDepth,
                                                  CREATE_PIXMAP_USAGE_BACKING_PIXMAP);
    }

    SetRootClip(screen, xwl_screen->root_clip_mode);

    return screen->devPrivate != NULL;
}

/* ---------------------------------------------------------------------------
 * GL/EGL helper stubs
 * -------------------------------------------------------------------------*/

int
glamor_egl_fd_name_from_pixmap(ScreenPtr screen,
                               PixmapPtr pixmap,
                               CARD16 *stride, CARD32 *size)
{
    (void)screen; (void)pixmap; (void)stride; (void)size;
    /* Not supported here; return -1 as error indicator. */
    return -1;
}

/* ---------------------------------------------------------------------------
 * Native fence sync – export/import
 * -------------------------------------------------------------------------*/

int
xwl_glamor_get_fence(struct xwl_screen *xwl_screen)
{
    EGLint attribs[3];
    EGLSyncKHR sync;
    int fence_fd = -1;

    if (!xwl_screen || !xwl_screen->glamor)
        return -1;

    xwl_glamor_egl_make_current(xwl_screen);

    egl_native_fence_sync_init(xwl_screen->egl_display);
    if (!s_has_native_fence_sync)
        return -1;

    attribs[0] = EGL_SYNC_NATIVE_FENCE_FD_ANDROID;
    attribs[1] = EGL_NO_NATIVE_FENCE_FD_ANDROID;
    attribs[2] = EGL_NONE;

    sync = p_eglCreateSyncKHR(xwl_screen->egl_display,
                              EGL_SYNC_NATIVE_FENCE_ANDROID, attribs);
    if (sync != EGL_NO_SYNC_KHR) {
        fence_fd = p_eglDupNativeFenceFDANDROID(xwl_screen->egl_display, sync);
        p_eglDestroySyncKHR(xwl_screen->egl_display, sync);
    }

    return fence_fd;
}

/* Takes ownership of fence_fd (close on failure paths as well). */
void
xwl_glamor_wait_fence(struct xwl_screen *xwl_screen, int fence_fd)
{
    EGLint attribs[3];
    EGLSyncKHR sync;

    if (fence_fd < 0)
        return;

    if (!xwl_screen || !xwl_screen->glamor) {
        close(fence_fd);
        return;
    }

    xwl_glamor_egl_make_current(xwl_screen);

    egl_native_fence_sync_init(xwl_screen->egl_display);
    if (!s_has_native_fence_sync) {
        close(fence_fd);
        return;
    }

    attribs[0] = EGL_SYNC_NATIVE_FENCE_FD_ANDROID;
    attribs[1] = fence_fd; /* ownership passed to EGL */
    attribs[2] = EGL_NONE;

    sync = p_eglCreateSyncKHR(xwl_screen->egl_display,
                              EGL_SYNC_NATIVE_FENCE_ANDROID, attribs);
    if (sync != EGL_NO_SYNC_KHR) {
        p_eglWaitSyncKHR(xwl_screen->egl_display, sync, 0);
        p_eglDestroySyncKHR(xwl_screen->egl_display, sync);
    } else {
        /* Creation failed; EGL did not take ownership; close the fd. */
        close(fence_fd);
    }
}

/* ---------------------------------------------------------------------------
 * Entry point
 * -------------------------------------------------------------------------*/

Bool
xwl_glamor_init(struct xwl_screen *xwl_screen)
{
    ScreenPtr screen = xwl_screen->screen;
    const char *no_glamor_env;

    no_glamor_env = getenv("XWAYLAND_NO_GLAMOR");
    if (no_glamor_env && *no_glamor_env != '0') {
        ErrorF("Disabling glamor and dri3 support, XWAYLAND_NO_GLAMOR is set\n");
        return FALSE;
    }

    if (!xwl_glamor_has_wl_interfaces(xwl_screen)) {
        ErrorF("Xwayland glamor: GBM Wayland interfaces not available\n");
        return FALSE;
    }

    if (!xwl_glamor_gbm_init_egl(xwl_screen)) {
        ErrorF("EGL setup failed, disabling glamor\n");
        return FALSE;
    }

    if (!glamor_init(xwl_screen->screen, GLAMOR_USE_EGL_SCREEN)) {
        ErrorF("Failed to initialize glamor\n");
        return FALSE;
    }

    if (!xwl_glamor_gbm_init_screen(xwl_screen)) {
        ErrorF("EGL backend init_screen() failed, disabling glamor\n");
        return FALSE;
    }

    xwl_screen->CreateScreenResources = screen->CreateScreenResources;
    screen->CreateScreenResources = xwl_glamor_create_screen_resources;

#ifdef XV
    if (!xwl_glamor_xv_init(screen))
        ErrorF("Failed to initialize glamor Xv extension\n");
#endif

#ifdef GLXEXT
    GlxPushProvider(&glamor_provider);
#endif

    return TRUE;
}
