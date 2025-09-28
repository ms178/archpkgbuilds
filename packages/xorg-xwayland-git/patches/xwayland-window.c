/*
 * Copyright © 2011-2014 Intel Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of the
 * copyright holders not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#ifdef HAVE_DIX_CONFIG_H
#include <dix-config.h>
#endif

#include <float.h>
#include <math.h>
#include <sys/mman.h>

#include <X11/X.h>
#include <X11/Xatom.h>

#include "compositeext.h"
#include "compint.h"
#include "inputstr.h"
#include "propertyst.h"

#include "xwayland-types.h"
#include "xwayland-input.h"
#include "xwayland-pixmap.h"
#include "xwayland-present.h"
#include "xwayland-screen.h"
#include "xwayland-window.h"
#include "xwayland-window-buffers.h"
#include "xwayland-shm.h"
#include "xwayland-dmabuf.h"

#include "linux-dmabuf-unstable-v1-client-protocol.h"
#include "tearing-control-v1-client-protocol.h"
#include "viewporter-client-protocol.h"
#include "xdg-shell-client-protocol.h"
#include "xwayland-shell-v1-client-protocol.h"
#include "fractional-scale-v1-client-protocol.h"
#include "linux-drm-syncobj-v1-client-protocol.h"

#define DELAYED_WL_SURFACE_DESTROY 1000 /* ms */

#define MAX_ROOTFUL_WIDTH 32767
#define MAX_ROOTFUL_HEIGHT 32767
#define MIN_ROOTFUL_WIDTH 320
#define MIN_ROOTFUL_HEIGHT 200

#define FRACTIONAL_SCALE_DENOMINATOR 120

static DevPrivateKeyRec xwl_window_private_key;
static DevPrivateKeyRec xwl_wm_window_private_key;
static DevPrivateKeyRec xwl_damage_private_key;
static const char *xwl_surface_tag = "xwl-surface";

static Bool xwl_window_attach_buffer(struct xwl_window *);

struct xwl_window *
xwl_window_get(WindowPtr window)
{
    return dixLookupPrivate(&window->devPrivates, &xwl_window_private_key);
}

static DamagePtr
window_get_damage(WindowPtr window)
{
    return dixLookupPrivate(&window->devPrivates, &xwl_damage_private_key);
}

RegionPtr
xwl_window_get_damage_region(struct xwl_window *xwl_window)
{
    return DamageRegion(window_get_damage(xwl_window->surface_window));
}

struct xwl_window *
xwl_window_from_window(WindowPtr window)
{
    struct xwl_window *xwl_window;

    while (window) {
        xwl_window = xwl_window_get(window);
        if (xwl_window) {
            return xwl_window;
        }
        window = window->parent;
    }

    return NULL;
}

static void
xwl_window_set_xwayland_tag(struct xwl_window *xwl_window)
{
    wl_proxy_set_tag((struct wl_proxy *)xwl_window->surface, &xwl_surface_tag);
}

static void
xwl_window_clear_xwayland_tag(struct xwl_window *xwl_window)
{
    wl_proxy_set_tag((struct wl_proxy *)xwl_window->surface, NULL);
}

Bool
is_surface_from_xwl_window(struct wl_surface *surface)
{
    return wl_proxy_get_tag((struct wl_proxy *) surface) == &xwl_surface_tag;
}

static void
xwl_window_set_allow_commits(struct xwl_window *xwl_window, Bool allow,
                             const char *debug_msg)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    DamagePtr damage;

    /* Early exit if no change is needed */
    if (xwl_window->allow_commits == allow) {
        return;
    }

    xwl_window->allow_commits = allow;

    damage = window_get_damage(xwl_window->surface_window);
    if (allow &&
        xorg_list_is_empty(&xwl_window->link_damage) &&
        damage &&
        RegionNotEmpty(DamageRegion(damage))) {
        xorg_list_add(&xwl_window->link_damage,
                      &xwl_screen->damage_window_list);
    }
}

static void
xwl_window_set_allow_commits_from_property(struct xwl_window *xwl_window,
                                           PropertyPtr prop)
{
    static Bool warned = FALSE;
    CARD32 *propdata;

    if (prop->propertyName != xwl_window->xwl_screen->allow_commits_prop) {
        FatalError("Xwayland internal error: prop mismatch in %s.\n", __func__);
    }

    if (prop->type != XA_CARDINAL || prop->format != 32 || prop->size != 1) {
        /* Not properly set, so fall back to safe and glitchy */
        xwl_window_set_allow_commits(xwl_window, TRUE, "WM fault");

        if (!warned) {
            LogMessageVerb(X_WARNING, 0, "Window manager is misusing property %s.\n",
                           NameForAtom(prop->propertyName));
            warned = TRUE;
        }
        return;
    }

    propdata = prop->data;
    xwl_window_set_allow_commits(xwl_window, !!propdata[0], "from property");
}

void
xwl_window_update_property(struct xwl_window *xwl_window,
                           PropertyStateRec *propstate)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    static Atom allow_commits_prop_cache = None;

    if (allow_commits_prop_cache == None) {
        allow_commits_prop_cache = xwl_screen->allow_commits_prop;
    }

    /* Only process property changes we care about */
    if (propstate->prop->propertyName != allow_commits_prop_cache) {
        return;
    }

    switch (propstate->state) {
    case PropertyNewValue:
        xwl_window_set_allow_commits_from_property(xwl_window, propstate->prop);
        break;

    case PropertyDelete:
        xwl_window_set_allow_commits(xwl_window, TRUE, "property deleted");
        break;

    default:
        break;
    }
}

static void
need_source_validate_dec(struct xwl_screen *xwl_screen)
{
    xwl_screen->need_source_validate--;

    if (!xwl_screen->need_source_validate) {
        xwl_screen->screen->SourceValidate = xwl_screen->SourceValidate;
    }
}

static void
xwl_source_validate(DrawablePtr drawable, int x, int y, int width, int height,
                    unsigned int sub_window_mode)
{
    struct xwl_window *xwl_window;
    WindowPtr window, iterator;
    RegionRec region;
    BoxRec box;

    if (sub_window_mode != IncludeInferiors ||
        drawable->type != DRAWABLE_WINDOW) {
        return;
    }

    window = (WindowPtr)drawable;
    xwl_window = xwl_window_from_window(window);
    if (!xwl_window || !xwl_window->surface_window_damage ||
        !RegionNotEmpty(xwl_window->surface_window_damage)) {
        return;
    }

    for (iterator = xwl_window->toplevel;
         ;
         iterator = iterator->firstChild) {
        if (iterator == xwl_window->surface_window) {
            return;
        }
        if (iterator == window) {
            break;
        }
    }

    box.x1 = x;
    box.y1 = y;
    box.x2 = x + width;
    box.y2 = y + height;
    RegionInit(&region, &box, 1);
    RegionIntersect(&region, &region, xwl_window->surface_window_damage);

    if (RegionNotEmpty(&region)) {
        ScreenPtr screen = drawable->pScreen;
        PixmapPtr dst_pix, src_pix;
        BoxPtr pbox;
        GCPtr pGC;
        int nbox;

        dst_pix = screen->GetWindowPixmap(window);
        pGC = GetScratchGC(dst_pix->drawable.depth, screen);
        if (!pGC) {
            FatalError("GetScratchGC failed for depth %d", dst_pix->drawable.depth);
        }
        ValidateGC(&dst_pix->drawable, pGC);

        src_pix = screen->GetWindowPixmap(xwl_window->surface_window);

        RegionSubtract(xwl_window->surface_window_damage,
                       xwl_window->surface_window_damage,
                       &region);

        if (!RegionNotEmpty(xwl_window->surface_window_damage)) {
            need_source_validate_dec(xwl_window->xwl_screen);
        }

#if defined(COMPOSITE)
        if (dst_pix->screen_x || dst_pix->screen_y) {
            RegionTranslate(&region, -dst_pix->screen_x, -dst_pix->screen_y);
        }
#endif

        pbox = RegionRects(&region);
        nbox = RegionNumRects(&region);
        while (nbox--) {
            (void) (*pGC->ops->CopyArea) (&src_pix->drawable,
                                          &dst_pix->drawable,
                                          pGC,
                                          pbox->x1, pbox->y1,
                                          pbox->x2 - pbox->x1, pbox->y2 - pbox->y1,
                                          pbox->x1, pbox->y1);
            pbox++;
        }
        FreeScratchGC(pGC);
    }

    RegionUninit(&region);
}

static void
need_source_validate_inc(struct xwl_screen *xwl_screen)
{
    if (!xwl_screen->need_source_validate) {
        ScreenPtr screen = xwl_screen->screen;

        xwl_screen->SourceValidate = screen->SourceValidate;
        screen->SourceValidate = xwl_source_validate;
    }

    xwl_screen->need_source_validate++;
}

static void
damage_report(DamagePtr pDamage, RegionPtr pRegion, void *data)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    PixmapPtr window_pixmap;

    if (xwl_window->surface_window_damage &&
        RegionNotEmpty(pRegion)) {
        if (!RegionNotEmpty(xwl_window->surface_window_damage)) {
            need_source_validate_inc(xwl_screen);
        }

        RegionUnion(xwl_window->surface_window_damage,
                    xwl_window->surface_window_damage,
                    DamageRegion(pDamage));
    }

    if (__builtin_expect(xwl_screen->ignore_damage, 0)) {
        return;
    }

    if (xorg_list_is_empty(&xwl_window->link_damage)) {
        xorg_list_add(&xwl_window->link_damage, &xwl_screen->damage_window_list);
    }

    window_pixmap = xwl_screen->screen->GetWindowPixmap(xwl_window->surface_window);
    if (xwl_is_client_pixmap(window_pixmap)) {
        xwl_screen->screen->DestroyPixmap(xwl_window_swap_pixmap(xwl_window, FALSE));
    }
}

static void
damage_destroy(DamagePtr pDamage, void *data)
{
}

static Bool
register_damage(struct xwl_window *xwl_window)
{
    WindowPtr surface_window = xwl_window->surface_window;
    DamagePtr damage;

    damage = DamageCreate(damage_report, damage_destroy, DamageReportNonEmpty,
                          FALSE, surface_window->drawable.pScreen, xwl_window);
    if (damage == NULL) {
        ErrorF("Failed creating damage\n");
        return FALSE;
    }

    DamageRegister(&surface_window->drawable, damage);
    dixSetPrivate(&surface_window->devPrivates, &xwl_damage_private_key, damage);

    return TRUE;
}

static void
unregister_damage(struct xwl_window *xwl_window)
{
    WindowPtr surface_window = xwl_window->surface_window;
    DamagePtr damage;

    damage = dixLookupPrivate(&surface_window->devPrivates, &xwl_damage_private_key);
    if (!damage) {
        return;
    }

    DamageUnregister(damage);
    DamageDestroy(damage);

    dixSetPrivate(&surface_window->devPrivates, &xwl_damage_private_key, NULL);
}

static Bool
xwl_window_update_fractional_scale(struct xwl_window *xwl_window,
                                   int fractional_scale_numerator)
{
    int old_scale_numerator = xwl_window->fractional_scale_numerator;

    xwl_window->fractional_scale_numerator = fractional_scale_numerator;

    return (old_scale_numerator != fractional_scale_numerator);
}

static double
xwl_window_get_fractional_scale_factor(struct xwl_window *xwl_window)
{
    const int num = xwl_window->fractional_scale_numerator;

    /* Fast path for common scale factors */
    switch(num) {
        case 120: return 1.0;   /* 1.0x */
        case 150: return 1.25;  /* 1.25x */
        case 180: return 1.5;   /* 1.5x */
        case 210: return 1.75;  /* 1.75x */
        case 240: return 2.0;   /* 2.0x */
        default:
            return (double)num / (double)FRACTIONAL_SCALE_DENOMINATOR;
    }
}

static Bool
xwl_window_has_viewport_enabled(struct xwl_window *xwl_window)
{
    return (xwl_window->viewport != NULL);
}

static void
xwl_window_disable_viewport(struct xwl_window *xwl_window)
{
    assert (xwl_window->viewport);

    wp_viewport_destroy(xwl_window->viewport);
    xwl_window->viewport = NULL;
    xwl_window->viewport_scale_x = 1.0f;
    xwl_window->viewport_scale_y = 1.0f;
    xwl_window_set_input_region(xwl_window, wInputShape(xwl_window->toplevel));
}

/* Enable the viewport for fractional scale support with Xwayland rootful.
 * Fractional scale support is not used with Xwayland rootful fullscreen (which
 * sets its own XRandR resolution) so we can use the viewport for either
 * fullscreen mode or fractional scale.
 */
static void
xwl_window_enable_viewport_for_fractional_scale(struct xwl_window *xwl_window,
                                                int width, int height)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    int buffer_width, buffer_height;
    double scale = xwl_window_get_fractional_scale_factor(xwl_window);

    /* Optimize common cases with integer arithmetic instead of floating point */
    if (scale == 1.0) {
        buffer_width = width;
        buffer_height = height;
    } else if (scale == 2.0) {
        buffer_width = width / 2;
        buffer_height = height / 2;
    } else if (scale == 1.5) {
        buffer_width = (width * 2) / 3;
        buffer_height = (height * 2) / 3;
    } else if (scale == 1.25) {
        buffer_width = (width * 4) / 5;
        buffer_height = (height * 4) / 5;
    } else {
        /* General case, use fast lrintf for rounding */
        buffer_width = lrintf((double)width / scale);
        buffer_height = lrintf((double)height / scale);
    }

    if (!xwl_window_has_viewport_enabled(xwl_window)) {
        xwl_window->viewport = wp_viewporter_get_viewport(xwl_screen->viewporter,
                                                          xwl_window->surface);
    }

    wp_viewport_set_source(xwl_window->viewport,
                           wl_fixed_from_int(0),
                           wl_fixed_from_int(0),
                           wl_fixed_from_int(width),
                           wl_fixed_from_int(height));
    wp_viewport_set_destination(xwl_window->viewport,
                                buffer_width,
                                buffer_height);

    xwl_window->viewport_scale_x = (float)scale;
    xwl_window->viewport_scale_y = (float)scale;
    xwl_window_set_input_region(xwl_window, wInputShape(xwl_window->toplevel));
}

/* Enable the viewport for Xwayland rootful fullscreen, to match the XRandR
 * resolution with the actual output size.
 */
static void
xwl_window_enable_viewport_for_output(struct xwl_window *xwl_window,
                                      struct xwl_output *xwl_output,
                                      struct xwl_emulated_mode *emulated_mode)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    int width, height;

    if (!xwl_window_has_viewport_enabled(xwl_window)) {
        xwl_window->viewport = wp_viewporter_get_viewport(xwl_window->xwl_screen->viewporter,
                                                          xwl_window->surface);
    }

    width = emulated_mode->width / xwl_screen->global_surface_scale;
    height = emulated_mode->height / xwl_screen->global_surface_scale;

    wp_viewport_set_source(xwl_window->viewport,
                           wl_fixed_from_int(0),
                           wl_fixed_from_int(0),
                           wl_fixed_from_int(width),
                           wl_fixed_from_int(height));
    wp_viewport_set_destination(xwl_window->viewport,
                                xwl_output->width,
                                xwl_output->height);

    xwl_window->viewport_scale_x = (float) width / xwl_output->width;
    xwl_window->viewport_scale_y = (float) height / xwl_output->height;
    xwl_window_set_input_region(xwl_window, wInputShape(xwl_window->toplevel));
}

static Bool
window_is_wm_window(WindowPtr window)
{
    struct xwl_screen *xwl_screen = xwl_screen_get(window->drawable.pScreen);
    Bool *is_wm_window;

    if (CLIENT_ID(window->drawable.id) == xwl_screen->wm_client_id) {
        return TRUE;
    }

    is_wm_window = dixLookupPrivate(&window->devPrivates, &xwl_wm_window_private_key);
    return *is_wm_window;
}

static WindowPtr
get_single_output_child(WindowPtr window)
{
    WindowPtr iter, output_child = NULL;

    for (iter = window->firstChild; iter; iter = iter->nextSib) {
        if (iter->drawable.class != InputOutput) {
            continue;
        }

        if (output_child) {
            return NULL;
        }

        output_child = iter;
    }

    return output_child;
}

static WindowPtr
window_get_client_toplevel(WindowPtr window)
{
    assert(window);

    /* If the toplevel window is owned by the window-manager, then the
     * actual client toplevel window has been reparented to some window-manager
     * decoration/wrapper windows. In that case recurse by checking the client
     * of the first *and only* output child of the decoration/wrapper window.
     */
    while (window && window_is_wm_window(window)) {
        window = get_single_output_child(window);
    }

    return window;
}

static Bool
is_output_suitable_for_fullscreen(struct xwl_output *xwl_output)
{
    if (xwl_output == NULL) {
        return FALSE;
    }

    if (xwl_output->width == 0 || xwl_output->height == 0) {
        return FALSE;
    }

    return TRUE;
}

static struct xwl_output *
xwl_window_get_output(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_output *xwl_output;

    xwl_output = xwl_output_get_output_from_name(xwl_screen, xwl_screen->output_name);
    if (is_output_suitable_for_fullscreen(xwl_output)) {
        return xwl_output;
    }

    xwl_output = xwl_output_from_wl_output(xwl_screen, xwl_window->wl_output);
    if (is_output_suitable_for_fullscreen(xwl_output)) {
        return xwl_output;
    }

    return xwl_screen_get_first_output(xwl_screen);
}

static Bool
xwl_window_should_enable_viewport_fullscreen(struct xwl_window *xwl_window,
                                             struct xwl_output **xwl_output_ret,
                                             struct xwl_emulated_mode *emulated_mode_ret)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_output *xwl_output;

    xwl_output = xwl_window_get_output(xwl_window);
    if (!xwl_output) {
        return FALSE;
    }

    *xwl_output_ret = xwl_output;
    emulated_mode_ret->server_output_id = 0;
    emulated_mode_ret->width = xwl_screen_get_width(xwl_screen);
    emulated_mode_ret->height = xwl_screen_get_height(xwl_screen);
    emulated_mode_ret->from_vidmode = FALSE;

    return TRUE;
}

static Bool
xwl_window_should_enable_viewport(struct xwl_window *xwl_window,
                                  struct xwl_output **xwl_output_ret,
                                  struct xwl_emulated_mode *emulated_mode_ret)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_emulated_mode *emulated_mode;
    struct xwl_output *xwl_output;
    ClientPtr owner;
    WindowPtr window;
    DrawablePtr drawable;

    if (!xwl_screen_has_viewport_support(xwl_screen)) {
        return FALSE;
    }

    if (xwl_screen->fullscreen) {
        return xwl_window_should_enable_viewport_fullscreen(xwl_window,
                                                            xwl_output_ret,
                                                            emulated_mode_ret);
    }

    if (!xwl_screen->rootless) {
        return FALSE;
    }

    window = window_get_client_toplevel(xwl_window->toplevel);
    if (!window) {
        return FALSE;
    }

    owner = wClient(window);
    drawable = &window->drawable;

    /* 1. Test if the window matches the emulated mode on one of the outputs
     * This path gets hit by most games / libs (e.g. SDL, SFML, OGRE)
     */
    xorg_list_for_each_entry(xwl_output, &xwl_screen->output_list, link) {
        emulated_mode = xwl_output_get_emulated_mode_for_client(xwl_output, owner);
        if (!emulated_mode) {
            continue;
        }

        if (drawable->x == xwl_output->x &&
            drawable->y == xwl_output->y &&
            drawable->width  == emulated_mode->width &&
            drawable->height == emulated_mode->height) {

            memcpy(emulated_mode_ret, emulated_mode, sizeof(struct xwl_emulated_mode));
            *xwl_output_ret = xwl_output;
            return TRUE;
        }
    }

    /* 2. Test if the window uses override-redirect + vidmode
     * and matches (fully covers) the entire screen.
     * This path gets hit by: allegro4, ClanLib-1.0.
     */
    xwl_output = xwl_screen_get_first_output(xwl_screen);
    emulated_mode = xwl_output_get_emulated_mode_for_client(xwl_output, owner);
    if (xwl_output && xwl_window->toplevel->overrideRedirect &&
        emulated_mode && emulated_mode->from_vidmode &&
        drawable->x == 0 && drawable->y == 0 &&
        drawable->width  == xwl_screen_get_width(xwl_screen) &&
        drawable->height == xwl_screen_get_height(xwl_screen)) {

        memcpy(emulated_mode_ret, emulated_mode, sizeof(struct xwl_emulated_mode));
        *xwl_output_ret = xwl_output;
        return TRUE;
    }

    return FALSE;
}

static Bool
xwl_window_should_enable_fractional_scale_viewport(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    double scale;

    if (!xwl_screen_should_use_fractional_scale(xwl_screen)) {
        return FALSE;
    }

    scale = xwl_window_get_fractional_scale_factor(xwl_window);

    return fabs(scale - 1.00) > DBL_EPSILON;
}

static void
xwl_window_check_fractional_scale_viewport(struct xwl_window *xwl_window,
                                           int width, int height)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;

    if (!xwl_screen_should_use_fractional_scale(xwl_screen)) {
        return;
    }

    if (xwl_window_should_enable_fractional_scale_viewport(xwl_window)) {
        xwl_window_enable_viewport_for_fractional_scale(xwl_window, width, height);
    } else if (xwl_window_has_viewport_enabled(xwl_window)) {
        xwl_window_disable_viewport(xwl_window);
    }
}

void
xwl_window_check_resolution_change_emulation(struct xwl_window *xwl_window)
{
    struct xwl_emulated_mode emulated_mode;
    struct xwl_output *xwl_output;

    if (xwl_window_should_enable_viewport(xwl_window, &xwl_output, &emulated_mode)) {
        xwl_window_enable_viewport_for_output(xwl_window, xwl_output, &emulated_mode);
    } else if (xwl_window_should_enable_fractional_scale_viewport(xwl_window)) {
        return;
    } else if (xwl_window_has_viewport_enabled(xwl_window)) {
        xwl_window_disable_viewport(xwl_window);
    }
}

/* This checks if the passed in Window is a toplevel client window, note this
 * returns false for window-manager decoration windows and returns true for
 * the actual client top-level window even if it has been reparented to
 * a window-manager decoration window.
 */
Bool
xwl_window_is_toplevel(WindowPtr window)
{
    if (!window->parent || window_is_wm_window(window)) {
        return FALSE;
    }

    /* CSD and override-redirect toplevel windows */
    if (!window->parent->parent) {
        return TRUE;
    }

    /* Normal toplevel client windows, reparented to a window-manager window */
    return window_is_wm_window(window->parent);
}

static void
xwl_window_init_allow_commits(struct xwl_window *xwl_window)
{
    PropertyPtr prop = NULL;
    int ret;

    ret = dixLookupProperty(&prop, xwl_window->toplevel,
                            xwl_window->xwl_screen->allow_commits_prop,
                            serverClient, DixReadAccess);
    if (ret == Success && prop) {
        xwl_window_set_allow_commits_from_property(xwl_window, prop);
    } else {
        xwl_window_set_allow_commits(xwl_window, TRUE, "no property");
    }
}

static uint32_t
serial_lo(uint64_t value)
{
    return value & 0xFFFFFFFFu;
}

static uint32_t
serial_hi(uint64_t value)
{
    return value >> 32u;
}

static void
send_window_client_message(struct xwl_window *xwl_window, Atom type_atom, uint64_t value)
{
    DeviceIntPtr dev;
    xEvent e;

    e.u.u.type = ClientMessage;
    e.u.u.detail = 32;
    e.u.clientMessage.window = xwl_window->toplevel->drawable.id;
    e.u.clientMessage.u.l.type = type_atom;
    e.u.clientMessage.u.l.longs0 = serial_lo(value);
    e.u.clientMessage.u.l.longs1 = serial_hi(value);
    e.u.clientMessage.u.l.longs2 = 0;
    e.u.clientMessage.u.l.longs3 = 0;
    e.u.clientMessage.u.l.longs4 = 0;

    dev = PickPointer(serverClient);
    DeliverEventsToWindow(dev, xwl_window->xwl_screen->screen->root,
                          &e, 1, SubstructureRedirectMask, NullGrab);
}

static void
send_surface_id_event_serial(struct xwl_window *xwl_window)
{
    static const char atom_name[] = "WL_SURFACE_SERIAL";
    static Atom type_atom;
    uint64_t serial;

    if (type_atom == None) {
        type_atom = MakeAtom(atom_name, strlen(atom_name), TRUE);
    }

    serial = ++xwl_window->xwl_screen->surface_association_serial;

    send_window_client_message(xwl_window, type_atom, serial);
    xwayland_surface_v1_set_serial(xwl_window->xwayland_surface,
        serial_lo(serial), serial_hi(serial));
    wl_surface_commit(xwl_window->surface);

    /* Flush wayland display *after* commit in the new path. */
    wl_display_flush(xwl_window->xwl_screen->display);
}

static void
send_surface_id_event_legacy(struct xwl_window *xwl_window)
{
    static const char atom_name[] = "WL_SURFACE_ID";
    static Atom type_atom;
    uint32_t surface_id;

    if (type_atom == None) {
        type_atom = MakeAtom(atom_name, strlen(atom_name), TRUE);
    }

    surface_id = wl_proxy_get_id((struct wl_proxy *) xwl_window->surface);

    /* Flush wayland display *before* setting the atom in the legacy path */
    wl_display_flush(xwl_window->xwl_screen->display);

    send_window_client_message(xwl_window, type_atom, (uint64_t)surface_id);
}

static void
send_surface_id_event(struct xwl_window *xwl_window)
{
    if (__builtin_expect(xwl_window->xwayland_surface != NULL, 1)) {
        send_surface_id_event_serial(xwl_window);
    } else {
        send_surface_id_event_legacy(xwl_window);
    }
}

static Bool
xwl_window_set_fullscreen(struct xwl_window *xwl_window)
{
    struct xwl_output *xwl_output;
    struct wl_output *wl_output = NULL;

    if (!xwl_window->xdg_toplevel) {
        return FALSE;
    }

    xwl_output = xwl_window_get_output(xwl_window);
    if (xwl_output) {
        wl_output = xwl_output->output;
    }

    if (wl_output && xwl_window->wl_output_fullscreen == wl_output) {
        return FALSE;
    }

    xdg_toplevel_set_fullscreen(xwl_window->xdg_toplevel, wl_output);
    xwl_window_check_resolution_change_emulation(xwl_window);
    wl_surface_commit(xwl_window->surface);

    xwl_window->wl_output_fullscreen = wl_output;

    return TRUE;
}

void
xwl_window_rootful_update_fullscreen(struct xwl_window *xwl_window,
                                     struct xwl_output *xwl_output)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;

    if (!xwl_screen->fullscreen) {
        return;
    }

    if (xwl_window->toplevel != xwl_screen->screen->root) {
        return;
    }

    if (xwl_window->wl_output_fullscreen != xwl_output->output) {
        return;
    }

    /* The size and position of the output may have changed, clear our
     * output to make sure the next call to xwl_window_set_fullscreen()
     * recomputes the size and updates the viewport as needed.
     */
    xwl_window->wl_output_fullscreen = NULL;
    xwl_window_set_fullscreen(xwl_window);
}

void
xwl_window_rootful_update_title(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    char title[128];
    const char *grab_message = "";

    if (xwl_screen->host_grab) {
        if (xwl_screen->has_grab) {
            grab_message = " - ([ctrl]+[shift] releases mouse and keyboard)";
        } else {
            grab_message = " - ([ctrl]+[shift] grabs mouse and keyboard)";
        }
    }

    snprintf(title, sizeof(title), "Xwayland on :%s%s", display, grab_message);

#ifdef XWL_HAS_LIBDECOR
    if (xwl_window->libdecor_frame) {
        libdecor_frame_set_title(xwl_window->libdecor_frame, title);
    } else
#endif
    if (xwl_window->xdg_toplevel) {
        xdg_toplevel_set_title(xwl_window->xdg_toplevel, title);
    }
}

static void
xwl_window_rootful_set_app_id(struct xwl_window *xwl_window)
{
    const char *app_id = "org.freedesktop.Xwayland";

#ifdef XWL_HAS_LIBDECOR
    if (xwl_window->libdecor_frame) {
        libdecor_frame_set_app_id(xwl_window->libdecor_frame, app_id);
    } else
#endif
    if (xwl_window->xdg_toplevel) {
        xdg_toplevel_set_app_id(xwl_window->xdg_toplevel, app_id);
    }
}

static void
xwl_window_maybe_resize(struct xwl_window *xwl_window, double width, double height)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_output *xwl_output;
    float scale;
    RRModePtr mode;

    /* Clamp the size */
    width = min(max(width, MIN_ROOTFUL_WIDTH), MAX_ROOTFUL_WIDTH);
    height = min(max(height, MIN_ROOTFUL_HEIGHT), MAX_ROOTFUL_HEIGHT);

    /* Make sure the size is a multiple of the scale, it's a protocol error otherwise. */
    scale = (float)xwl_screen->global_surface_scale;
    if (scale > 1.0f) {
        width = lrintf(width / scale) * scale;
        height = lrintf(height / scale) * scale;
    }

    if (width == xwl_screen->width && height == xwl_screen->height) {
        return;
    }

    xwl_screen->width = width;
    xwl_screen->height = height;

    /* When fractional scale is used, the global surface scale is 1, and vice
     * versa, so we can multiply the two here, and have the resulting scale
     * apply for both cases, the legacy wl_surface buffer scale and fractional
     * scaling.
     */
    scale *= (float)xwl_window_get_fractional_scale_factor(xwl_window);

    xwl_output = xwl_screen_get_fixed_or_first_output(xwl_screen);
    if (!xwl_randr_add_modes_fixed(xwl_output, lrintf(width / scale), lrintf(height / scale))) {
        return;
    }

    mode = xwl_output_find_mode(xwl_output, lrintf(width / scale), lrintf(height / scale));
    xwl_output_set_mode_fixed(xwl_output, mode);

    xwl_window_attach_buffer(xwl_window);
}

#ifdef XWL_HAS_LIBDECOR
static void
xwl_window_libdecor_set_size_limits(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;

    libdecor_frame_set_min_content_size(xwl_window->libdecor_frame,
                                        MIN_ROOTFUL_WIDTH /
                                        xwl_screen->global_surface_scale,
                                        MIN_ROOTFUL_HEIGHT /
                                        xwl_screen->global_surface_scale);
    libdecor_frame_set_max_content_size(xwl_window->libdecor_frame,
                                        MAX_ROOTFUL_WIDTH /
                                        xwl_screen->global_surface_scale,
                                        MAX_ROOTFUL_HEIGHT /
                                        xwl_screen->global_surface_scale);
}

static void
xwl_window_update_libdecor_size(struct xwl_window *xwl_window,
                                struct libdecor_configuration *configuration /* nullable */,
                                int width, int height)
{
    struct libdecor_state *state;
    float scale;

    if (xwl_window->libdecor_frame) {
        scale = (float)xwl_window_get_fractional_scale_factor(xwl_window);
        state = libdecor_state_new(lrintf((float) width / scale),
                                   lrintf((float) height / scale));
        libdecor_frame_commit(xwl_window->libdecor_frame, state, configuration);
        libdecor_state_free(state);
    }
}

static void
handle_libdecor_configure(struct libdecor_frame *frame,
                          struct libdecor_configuration *configuration,
                          void *data)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    int width, height;
    double new_width, new_height;
    float scale;

    if (libdecor_configuration_get_content_size(configuration, frame, &width, &height)) {
        new_width = (double) width;
        new_height = (double) height;
    } else {
        new_width = xwl_screen->width / xwl_screen->global_surface_scale;
        new_height = xwl_screen->height / xwl_screen->global_surface_scale;
    }

    new_width *= xwl_screen->global_surface_scale;
    new_height *= xwl_screen->global_surface_scale;

    scale = (float)xwl_window_get_fractional_scale_factor(xwl_window);
    new_width *= scale;
    new_height *= scale;

    xwl_window_maybe_resize(xwl_window, new_width, new_height);

    new_width = xwl_screen->width / xwl_screen->global_surface_scale;
    new_height = xwl_screen->height / xwl_screen->global_surface_scale;

    xwl_window_update_libdecor_size(xwl_window, configuration,
                                    lrintf(new_width), lrintf(new_height));
    wl_surface_commit(xwl_window->surface);
}

static void
handle_libdecor_close(struct libdecor_frame *frame,
                      void *data)
{
    GiveUp(0);
}

static void
handle_libdecor_commit(struct libdecor_frame *frame,
                       void *data)
{
    struct xwl_window *xwl_window = data;
    wl_surface_commit(xwl_window->surface);
}

static void
handle_libdecor_dismiss_popup(struct libdecor_frame *frame,
                              const char *seat_name,
                              void *data)
{
}

static struct libdecor_frame_interface libdecor_frame_iface = {
    .configure = handle_libdecor_configure,
    .close = handle_libdecor_close,
    .commit = handle_libdecor_commit,
    .dismiss_popup = handle_libdecor_dismiss_popup,
};
#endif

static void
xdg_surface_handle_configure(void *data,
                             struct xdg_surface *xdg_surface,
                             uint32_t serial)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;

    if (xwl_screen->fullscreen) {
        xwl_window_set_fullscreen(xwl_window);
    }

    xdg_surface_ack_configure(xdg_surface, serial);
    wl_surface_commit(xwl_window->surface);
}

static const struct xdg_surface_listener xdg_surface_listener = {
    .configure = xdg_surface_handle_configure,
};

static void
xwl_window_update_surface_scale(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    int previous_scale, new_scale;
    double new_width, new_height;

    previous_scale = xwl_screen->global_surface_scale;
    assert(previous_scale != 0);
    xwl_window->surface_scale = xwl_window_get_max_output_scale(xwl_window);

    if (xwl_screen_update_global_surface_scale(xwl_screen)) {
        new_scale = xwl_screen->global_surface_scale;

        new_width = xwl_screen->width / previous_scale * new_scale;
        new_height = xwl_screen->height / previous_scale * new_scale;

        wl_surface_set_buffer_scale(xwl_window->surface, xwl_screen->global_surface_scale);
        /* Reflect the scale factor using XRandR transform */
        xwl_output_set_xscale(xwl_screen->fixed_output, new_scale);
        xwl_window_maybe_resize(xwl_window, new_width, new_height);
#ifdef XWL_HAS_LIBDECOR
        if (xwl_window->libdecor_frame) {
            xwl_window_libdecor_set_size_limits(xwl_window);
            xwl_window_update_libdecor_size(xwl_window,
                                            NULL,
                                            lrintf(new_width / new_scale),
                                            lrintf(new_height / new_scale));
        } else
#endif
        {
            wl_surface_commit(xwl_window->surface);
        }
    }
}

static void
xwl_window_enter_output(struct xwl_window *xwl_window, struct xwl_output *xwl_output)
{
    struct xwl_window_output *window_output;

    window_output = xnfcalloc(1, sizeof(struct xwl_window_output));
    window_output->xwl_output = xwl_output;
    xorg_list_add(&window_output->link, &xwl_window->xwl_output_list);
}

void
xwl_window_leave_output(struct xwl_window *xwl_window, struct xwl_output *xwl_output)
{
    struct xwl_window_output *window_output, *tmp;

    xorg_list_for_each_entry_safe(window_output, tmp, &xwl_window->xwl_output_list, link) {
        if (window_output->xwl_output == xwl_output) {
            xorg_list_del(&window_output->link);
            free(window_output);
        }
    }
}

static void
xwl_window_free_outputs(struct xwl_window *xwl_window)
{
    struct xwl_window_output *window_output, *tmp;

    xorg_list_for_each_entry_safe(window_output, tmp, &xwl_window->xwl_output_list, link) {
        xorg_list_del(&window_output->link);
        free(window_output);
    }
}

int
xwl_window_get_max_output_scale(struct xwl_window *xwl_window)
{
    struct xwl_window_output *window_output;
    struct xwl_output *xwl_output;
    int scale = 1;

    xorg_list_for_each_entry(window_output, &xwl_window->xwl_output_list, link) {
        xwl_output = window_output->xwl_output;
        if (xwl_output->scale > scale) {
            scale = xwl_output->scale;
        }
    }

    return scale;
}

static void
xwl_window_surface_enter(void *data,
                         struct wl_surface *wl_surface,
                         struct wl_output *wl_output)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_output *xwl_output = xwl_output_from_wl_output(xwl_screen, wl_output);

    if (xwl_output) {
        xwl_window_enter_output(xwl_window, xwl_output);
        xwl_window_update_surface_scale(xwl_window);
    }

    if (xwl_window->wl_output != wl_output) {
        xwl_window->wl_output = wl_output;

        if (xwl_screen->fullscreen) {
            xwl_window_set_fullscreen(xwl_window);
        }
    }
}

static void
xwl_window_surface_leave(void *data,
                         struct wl_surface *wl_surface,
                         struct wl_output *wl_output)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_output *xwl_output = xwl_output_from_wl_output(xwl_screen, wl_output);

    if (xwl_output) {
        xwl_window_leave_output(xwl_window, xwl_output);
        xwl_window_update_surface_scale(xwl_window);
    }

    if (xwl_window->wl_output == wl_output) {
        xwl_window->wl_output = NULL;
    }
}

static const struct wl_surface_listener surface_listener = {
    .enter = xwl_window_surface_enter,
    .leave = xwl_window_surface_leave
};

static void
xdg_toplevel_handle_configure(void *data,
                              struct xdg_toplevel *xdg_toplevel,
                              int32_t width,
                              int32_t height,
                              struct wl_array *states)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    uint32_t *p;
    Bool old_active = xwl_screen->active;
    float scale;
    double new_width, new_height;

    /* Maintain our current size if no dimensions are requested */
    if (width == 0 && height == 0) {
        return;
    }

    if (!xwl_screen->fullscreen) {
        new_width = (double) (width * xwl_screen->global_surface_scale);
        new_height = (double) (height * xwl_screen->global_surface_scale);

        /* Cache the scale factor to avoid a second division */
        scale = (float)xwl_window_get_fractional_scale_factor(xwl_window);
        new_width *= scale;
        new_height *= scale;

        /* This will be committed by the xdg_surface.configure handler */
        xwl_window_maybe_resize(xwl_window, new_width, new_height);
    }

    xwl_screen->active = FALSE;
    wl_array_for_each (p, states) {
        uint32_t state = *p;
        if (state == XDG_TOPLEVEL_STATE_ACTIVATED) {
            xwl_screen->active = TRUE;
            break;
        }
    }

    if (old_active != xwl_screen->active) {
        if (!xwl_screen->active) {
            xwl_screen_lost_focus(xwl_screen);
        }
    }
}

static void
xdg_toplevel_handle_close(void *data,
                          struct xdg_toplevel *xdg_toplevel)
{
    GiveUp(0);
}

static const struct xdg_toplevel_listener xdg_toplevel_listener = {
    .configure = xdg_toplevel_handle_configure,
    .close = xdg_toplevel_handle_close,
};

static void
xwl_window_update_rootful_scale(struct xwl_window *xwl_window, double previous_scale)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    double new_scale, new_width, new_height;

    new_scale = xwl_window_get_fractional_scale_factor(xwl_window);
    new_width = xwl_screen->width / previous_scale * new_scale;
    new_height = xwl_screen->height / previous_scale * new_scale;

    xwl_output_set_xscale(xwl_screen->fixed_output, new_scale);
    xwl_window_maybe_resize(xwl_window, new_width, new_height);
    xwl_window_check_fractional_scale_viewport(xwl_window,
                                               xwl_screen_get_width(xwl_screen),
                                               xwl_screen_get_height(xwl_screen));

#ifdef XWL_HAS_LIBDECOR
    if (xwl_window->libdecor_frame) {
        xwl_window_libdecor_set_size_limits(xwl_window);
        xwl_window_update_libdecor_size(xwl_window,
                                        NULL,
                                        xwl_screen_get_width(xwl_screen),
                                        xwl_screen_get_height(xwl_screen));
    } else
#endif
    {
        wl_surface_commit(xwl_window->surface);
    }
}

static void
wp_fractional_scale_preferred_scale(void *data,
                                    struct wp_fractional_scale_v1 *fractional_scale,
                                    uint32_t scale_numerator)
{
    struct xwl_window *xwl_window = data;
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    double previous_scale = xwl_window_get_fractional_scale_factor(xwl_window);

    if (xwl_window_update_fractional_scale(xwl_window, scale_numerator)) {
        if (xwl_screen->fixed_output) { /* We're running rootful */
            xwl_window_update_rootful_scale(xwl_window, previous_scale);
        }
    }
}

static const struct wp_fractional_scale_v1_listener fractional_scale_listener = {
   .preferred_scale = wp_fractional_scale_preferred_scale,
};

static Bool
xwl_create_root_surface(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    WindowPtr window = xwl_window->toplevel;
    struct wl_region *region;


#ifdef XWL_HAS_LIBDECOR
    if (xwl_screen->decorate) {
        xwl_window->libdecor_frame =
            libdecor_decorate(xwl_screen->libdecor_context,
                              xwl_window->surface,
                              &libdecor_frame_iface,
                              xwl_window);
        xwl_window_libdecor_set_size_limits(xwl_window);
        libdecor_frame_map(xwl_window->libdecor_frame);
    } else
#endif
    {
        xwl_window->xdg_surface =
            xdg_wm_base_get_xdg_surface(xwl_screen->xdg_wm_base, xwl_window->surface);
        if (xwl_window->xdg_surface == NULL) {
            ErrorF("Failed creating xdg_wm_base xdg_surface\n");
            goto err_surf;
        }

        xwl_window->xdg_toplevel =
            xdg_surface_get_toplevel(xwl_window->xdg_surface);
        if (xwl_window->xdg_toplevel == NULL) {
            ErrorF("Failed creating xdg_toplevel\n");
            goto err_surf;
        }

        xdg_surface_add_listener(xwl_window->xdg_surface,
                                 &xdg_surface_listener, xwl_window);

        xdg_toplevel_add_listener(xwl_window->xdg_toplevel,
                                  &xdg_toplevel_listener,
                                  xwl_window);
    }

    wl_surface_add_listener(xwl_window->surface,
                            &surface_listener, xwl_window);

    if (xwl_screen_should_use_fractional_scale(xwl_screen)) {
        xwl_window->fractional_scale =
            wp_fractional_scale_manager_v1_get_fractional_scale(xwl_screen->fractional_scale_manager,
                                                                xwl_window->surface);
        wp_fractional_scale_v1_add_listener(xwl_window->fractional_scale,
                                            &fractional_scale_listener, xwl_window);
    }

    xwl_window_rootful_update_title(xwl_window);
    xwl_window_rootful_set_app_id(xwl_window);
    wl_surface_commit(xwl_window->surface);

    region = wl_compositor_create_region(xwl_screen->compositor);
    if (region == NULL) {
        ErrorF("Failed creating region\n");
        goto err_surf;
    }

    wl_region_add(region, 0, 0,
                  window->drawable.width, window->drawable.height);
    wl_surface_set_opaque_region(xwl_window->surface, region);
    wl_region_destroy(region);

    return TRUE;

err_surf:
    if (xwl_window->xdg_toplevel) {
        xdg_toplevel_destroy(xwl_window->xdg_toplevel);
    }
    if (xwl_window->xdg_surface) {
        xdg_surface_destroy(xwl_window->xdg_surface);
    }
    wl_surface_destroy(xwl_window->surface);

    return FALSE;
}

void
xwl_window_update_surface_window(struct xwl_window *xwl_window)
{
    WindowPtr surface_window = xwl_window->toplevel;
    ScreenPtr screen = surface_window->drawable.pScreen;
    PixmapPtr surface_pixmap;
    DamagePtr window_damage;
    RegionRec damage_region;
    WindowPtr window;

    surface_pixmap = screen->GetWindowPixmap(surface_window);

    for (window = surface_window->firstChild; window; window = window->firstChild) {
        PixmapPtr window_pixmap;

        if (!RegionEqual(&window->winSize, &surface_window->winSize)) {
            break;
        }

        if (!window->mapped) {
            break;
        }

        /* The surface window must be top-level for its window pixmap */
        window_pixmap = screen->GetWindowPixmap(window);
        if (window_pixmap == surface_pixmap) {
            continue;
        }

        surface_pixmap = window_pixmap;

        /* A descendant with alpha channel cannot be the surface window, since
         * any non-opaque areas need to take the contents of ancestors into
         * account.
         */
        if (window->drawable.depth == 32) {
            continue;
        }

        if (window->redirectDraw == RedirectDrawManual) {
            break;
        }

        surface_window = window;
    }

    if (xwl_window->surface_window == surface_window) {
        return;
    }

    if (xwl_window->surface_window_damage) {
        if (xwl_present_maybe_unredirect_window(xwl_window->surface_window) &&
            screen->SourceValidate == xwl_source_validate) {
            WindowPtr toplevel = xwl_window->toplevel;

            xwl_source_validate(&toplevel->drawable,
                                toplevel->drawable.x, toplevel->drawable.y,
                                toplevel->drawable.width,
                                toplevel->drawable.height,
                                IncludeInferiors);
        }

        if (RegionNotEmpty(xwl_window->surface_window_damage)) {
            need_source_validate_dec(xwl_window->xwl_screen);
        }

        RegionDestroy(xwl_window->surface_window_damage);
        xwl_window->surface_window_damage = NULL;
    }

    window_damage = window_get_damage(xwl_window->surface_window);
    if (window_damage) {
        RegionInit(&damage_region, NullBox, 1);
        RegionCopy(&damage_region, DamageRegion(window_damage));
        unregister_damage(xwl_window);
    }

    if (surface_window->drawable.depth != xwl_window->surface_window->drawable.depth) {
        xwl_window_buffers_dispose(xwl_window, FALSE);
    }

    xwl_window->surface_window = surface_window;
    register_damage(xwl_window);

    if (window_damage) {
        RegionPtr new_region = DamageRegion(window_get_damage(surface_window));

        RegionUnion(new_region, new_region, &damage_region);
        RegionUninit(&damage_region);
    }
}

static struct xwl_window *
ensure_surface_for_window(WindowPtr window)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;
    WindowPtr toplevel;

    xwl_window = xwl_window_from_window(window);
    if (xwl_window) {
        return xwl_window;
    }

    xwl_screen = xwl_screen_get(screen);

    if (xwl_screen->rootless) {
        if (window->redirectDraw != RedirectDrawManual) {
            return NULL;
        }
    } else {
        if (window->parent) {
            return NULL;
        }
    }

    xwl_window = calloc(1, sizeof *xwl_window);
    if (!xwl_window) [[unlikely]] {
        return NULL;
    }

    xwl_window->xwl_screen = xwl_screen;
    xwl_window->toplevel = window;
    xwl_window->surface_window = window;
    xwl_window->fractional_scale_numerator = FRACTIONAL_SCALE_DENOMINATOR;
    xwl_window->viewport_scale_x = 1.0f;
    xwl_window->viewport_scale_y = 1.0f;
    xwl_window->surface_scale = 1;
    xorg_list_init(&xwl_window->xwl_output_list);
    xwl_window->surface = wl_compositor_create_surface(xwl_screen->compositor);
    if (!xwl_window->surface) [[unlikely]] {
        ErrorF("wl_compositor_create_surface failed\n");
        goto err;
    }

    if (xwl_screen->xwayland_shell) {
        xwl_window->xwayland_surface = xwayland_shell_v1_get_xwayland_surface(
            xwl_screen->xwayland_shell, xwl_window->surface);
    }

    if (!xwl_screen->rootless && !xwl_create_root_surface(xwl_window)) [[unlikely]] {
        goto err;
    }

#ifdef XWL_HAS_GLAMOR
    if (xwl_screen->dmabuf_protocol_version >= 4) {
        xwl_dmabuf_setup_feedback_for_window(xwl_window);
    }
#endif

    wl_display_flush(xwl_screen->display);

    send_surface_id_event(xwl_window);

    wl_surface_set_user_data(xwl_window->surface, xwl_window);
    xwl_window_set_xwayland_tag(xwl_window);

    compRedirectWindow(serverClient, window, CompositeRedirectManual);

    dixSetPrivate(&window->devPrivates, &xwl_window_private_key, xwl_window);
    xorg_list_init(&xwl_window->link_damage);
    xorg_list_add(&xwl_window->link_window, &xwl_screen->window_list);
    xorg_list_init(&xwl_window->frame_callback_list);

    xwl_window_buffers_init(xwl_window);

    xwl_window_update_surface_window(xwl_window);

    xwl_window_init_allow_commits(xwl_window);

    /* When a new window-manager window is realized, then the randr emulation
     * props may have not been set on the managed client window yet.
     */
    if (!xwl_screen->fullscreen && window_is_wm_window(window)) {
        toplevel = window_get_client_toplevel(window);
        if (toplevel) {
            xwl_output_set_window_randr_emu_props(xwl_screen, toplevel);
        }
    } else {
        /* CSD or O-R toplevel window, check viewport on creation */
        xwl_window_check_resolution_change_emulation(xwl_window);
    }

    if (xwl_screen->tearing_control_manager) {
        xwl_window->tearing_control = wp_tearing_control_manager_v1_get_tearing_control(
            xwl_screen->tearing_control_manager, xwl_window->surface);
    }

    xwl_window_set_input_region(xwl_window, wInputShape(window));

    return xwl_window;

err:
    free(xwl_window);
    return NULL;
}

Bool
xwl_realize_window(WindowPtr window)
{
    ScreenPtr screen = window->drawable.pScreen;
    CompScreenPtr comp_screen = GetCompScreen(screen);
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;
    Bool ret;

    xwl_screen = xwl_screen_get(screen);

    screen->RealizeWindow = xwl_screen->RealizeWindow;
    ret = (*screen->RealizeWindow) (window);
    xwl_screen->RealizeWindow = screen->RealizeWindow;
    screen->RealizeWindow = xwl_realize_window;

    if (!ret) {
        return FALSE;
    }

    if (xwl_screen->rootless) {
        /* We do not want the COW to be mapped when rootless in Xwayland */
        if (window == comp_screen->pOverlayWin) {
            window->mapped = FALSE;
            return TRUE;
        }

        if (!window->parent) {
            BoxRec box = {
                0,
                0,
                xwl_screen_get_width(xwl_screen),
                xwl_screen_get_height(xwl_screen)
            };

            RegionReset(&window->winSize, &box);
            RegionNull(&window->clipList);
            RegionNull(&window->borderClip);
        }
    }

    xwl_window = ensure_surface_for_window(window);
    if (!xwl_window) {
        return FALSE;
    }

    if (window == xwl_window->surface_window &&
        !window_get_damage(window)) {
        return register_damage(xwl_window);
    }

    return TRUE;
}

static void
xwl_surface_destroy_free_timer(struct xwl_wl_surface *xwl_wl_surface)
{
    if (xwl_wl_surface->wl_surface_destroy_timer) {
        TimerFree(xwl_wl_surface->wl_surface_destroy_timer);
        xwl_wl_surface->wl_surface_destroy_timer = NULL;
    }
}

void
xwl_window_surface_do_destroy(struct xwl_wl_surface *xwl_wl_surface)
{
    wl_surface_destroy(xwl_wl_surface->wl_surface);
    xorg_list_del(&xwl_wl_surface->link);
    xwl_surface_destroy_free_timer(xwl_wl_surface);
    free(xwl_wl_surface);
}

static CARD32
xwl_surface_destroy_callback(OsTimerPtr timer, CARD32 now, void *arg)
{
    struct xwl_wl_surface *xwl_wl_surface = arg;

    xwl_window_surface_do_destroy(xwl_wl_surface);

    return 0;
}

static void
release_wl_surface_for_window_legacy_delay(struct xwl_window *xwl_window)
{
    struct xwl_wl_surface *xwl_wl_surface;

    /* If the Xserver is terminating, destroy the surface immediately */
    if ((dispatchException & DE_TERMINATE) == DE_TERMINATE) {
        wl_surface_destroy(xwl_window->surface);
        return;
    }

    /* Break the wl_surface / xwl_window relationship */
    wl_surface_set_user_data(xwl_window->surface, NULL);
    xwl_window_clear_xwayland_tag(xwl_window);

    /* Schedule the destruction later, to mitigate the race between X11
     * and Wayland processing so that the compositor has the time to
     * establish the association before the wl_surface is destroyed.
     */
    xwl_wl_surface = xnfcalloc(1, sizeof *xwl_wl_surface);
    xwl_wl_surface->wl_surface = xwl_window->surface;
    xorg_list_add(&xwl_wl_surface->link,
                  &xwl_window->xwl_screen->pending_wl_surface_destroy);
    xwl_wl_surface->wl_surface_destroy_timer =
        TimerSet(NULL, 0, DELAYED_WL_SURFACE_DESTROY,
                 xwl_surface_destroy_callback, xwl_wl_surface);
}

static void
release_wl_surface_for_window_shell(struct xwl_window *xwl_window)
{
    xwayland_surface_v1_destroy(xwl_window->xwayland_surface);
    wl_surface_destroy(xwl_window->surface);
}

static void
release_wl_surface_for_window(struct xwl_window *xwl_window)
{
    if (xwl_window->xwayland_surface) {
        release_wl_surface_for_window_shell(xwl_window);
    } else {
        release_wl_surface_for_window_legacy_delay(xwl_window);
    }
}

static void
xwl_window_dispose(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    struct xwl_seat *xwl_seat;
    WindowPtr window = xwl_window->toplevel;
    ScreenPtr screen = xwl_screen->screen;

    compUnredirectWindow(serverClient, window, CompositeRedirectManual);

    xorg_list_for_each_entry(xwl_seat, &xwl_screen->seat_list, link) {
        if (xwl_seat->focus_window == xwl_window) {
            xwl_seat->focus_window = NULL;
        }
        if (xwl_seat->tablet_focus_window == xwl_window) {
            xwl_seat->tablet_focus_window = NULL;
        }
        if (xwl_seat->last_focus_window == xwl_window) {
            xwl_seat->last_focus_window = NULL;
        }
        if (xwl_seat->cursor_confinement_window == xwl_window) {
            xwl_seat_unconfine_pointer(xwl_seat);
        }
        if (xwl_seat->pointer_warp_emulator &&
            xwl_seat->pointer_warp_emulator->locked_window == xwl_window) {
            xwl_seat_destroy_pointer_warp_emulator(xwl_seat);
        }
        xwl_seat_clear_touch(xwl_seat, xwl_window);
    }

    if (xwl_window_has_viewport_enabled(xwl_window)) {
        xwl_window_disable_viewport(xwl_window);
    }
#ifdef XWL_HAS_GLAMOR
    xwl_dmabuf_feedback_destroy(&xwl_window->feedback);

#ifdef GLAMOR_HAS_GBM
    if (xwl_window->xwl_screen->present) {
        xwl_present_for_each_frame_callback(xwl_window, xwl_present_unrealize_window);
    }
#endif /* GLAMOR_HAS_GBM */
#endif /* XWL_HAS_GLAMOR */

    if (xwl_window->tearing_control) {
        wp_tearing_control_v1_destroy(xwl_window->tearing_control);
    }

    if (xwl_window->fractional_scale) {
        wp_fractional_scale_v1_destroy(xwl_window->fractional_scale);
    }

    if (xwl_window->surface_sync) {
        wp_linux_drm_syncobj_surface_v1_destroy(xwl_window->surface_sync);
    }

    release_wl_surface_for_window(xwl_window);
    xorg_list_del(&xwl_window->link_damage);
    xorg_list_del(&xwl_window->link_window);

    /* Special case for the root window in rootful mode */
    xwl_window_buffers_dispose(xwl_window,
                               (!xwl_screen->rootless && window == screen->root));

    if (xwl_window->window_buffers_timer) {
        TimerFree(xwl_window->window_buffers_timer);
    }

    if (xwl_window->frame_callback) {
        wl_callback_destroy(xwl_window->frame_callback);
    }

    xwl_window_free_outputs(xwl_window);

    free(xwl_window);
    dixSetPrivate(&window->devPrivates, &xwl_window_private_key, NULL);
}

Bool
xwl_unrealize_window(WindowPtr window)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    struct xwl_window *xwl_window = xwl_window_get(window);
    Bool ret;

    if (xwl_window) {
        unregister_damage(xwl_window);
        xwl_window_dispose(xwl_window);
    }

    screen->UnrealizeWindow = xwl_screen->UnrealizeWindow;
    ret = (*screen->UnrealizeWindow) (window);
    xwl_screen->UnrealizeWindow = screen->UnrealizeWindow;
    screen->UnrealizeWindow = xwl_unrealize_window;

    return ret;
}

void
xwl_window_set_window_pixmap(WindowPtr window,
                             PixmapPtr pixmap)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;
    PixmapPtr old_pixmap;

    old_pixmap = (*screen->GetWindowPixmap) (window);
    xwl_screen = xwl_screen_get(screen);

    screen->SetWindowPixmap = xwl_screen->SetWindowPixmap;
    (*screen->SetWindowPixmap) (window, pixmap);
    xwl_screen->SetWindowPixmap = screen->SetWindowPixmap;
    screen->SetWindowPixmap = xwl_window_set_window_pixmap;

    if (!RegionNotEmpty(&window->winSize)) {
        return;
    }

    xwl_window = ensure_surface_for_window(window);

    if (!xwl_window ||
        (old_pixmap->drawable.width == pixmap->drawable.width &&
         old_pixmap->drawable.height == pixmap->drawable.height)) {
       return;
    }

    xwl_window_buffers_dispose(xwl_window, FALSE);
}

Bool
xwl_change_window_attributes(WindowPtr window, unsigned long mask)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    OtherClients *others;
    Bool ret;

    screen->ChangeWindowAttributes = xwl_screen->ChangeWindowAttributes;
    ret = (*screen->ChangeWindowAttributes) (window, mask);
    xwl_screen->ChangeWindowAttributes = screen->ChangeWindowAttributes;
    screen->ChangeWindowAttributes = xwl_change_window_attributes;

    if (window != screen->root || !(mask & CWEventMask)) {
        return ret;
    }

    for (others = wOtherClients(window); others; others = others->next) {
        if (others->mask & (SubstructureRedirectMask | ResizeRedirectMask)) {
            xwl_screen->wm_client_id = CLIENT_ID(others->resource);
        }
    }

    return ret;
}

void
xwl_clip_notify(WindowPtr window, int dx, int dy)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    struct xwl_window *xwl_window = xwl_window_from_window(window);

    screen->ClipNotify = xwl_screen->ClipNotify;
    (*screen->ClipNotify) (window, dx, dy);
    xwl_screen->ClipNotify = screen->ClipNotify;
    screen->ClipNotify = xwl_clip_notify;

    if (xwl_window) {
        xwl_window_update_surface_window(xwl_window);
    }
}

int
xwl_config_notify(WindowPtr window,
                  int x, int y,
                  int width, int height, int bw,
                  WindowPtr sib)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;
    Bool size_changed;
    int ret;

    xwl_screen = xwl_screen_get(screen);
    xwl_window = xwl_window_from_window(window);

    size_changed = width != window->drawable.width || height != window->drawable.height;
    if (size_changed && xwl_window && xwl_window->toplevel == window &&
        screen->SourceValidate == xwl_source_validate) {
        xwl_source_validate(&window->drawable, window->drawable.x, window->drawable.y,
                            window->drawable.width, window->drawable.height,
                            IncludeInferiors);
    }

    screen->ConfigNotify = xwl_screen->ConfigNotify;
    ret = screen->ConfigNotify(window, x, y, width, height, bw, sib);
    xwl_screen->ConfigNotify = screen->ConfigNotify;
    screen->ConfigNotify = xwl_config_notify;

    return ret;
}

void
xwl_reparent_window(WindowPtr window, WindowPtr prior_parent)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    WindowPtr parent = window->parent;
    Bool *is_wm_window;

    screen->ReparentWindow = xwl_screen->ReparentWindow;
    if (screen->ReparentWindow) {
        screen->ReparentWindow(window, prior_parent);
    }
    xwl_screen->ReparentWindow = screen->ReparentWindow;
    screen->ReparentWindow = xwl_reparent_window;

    if (!parent->parent ||
        GetCurrentClient()->index != xwl_screen->wm_client_id) {
        return;
    }

    /* If the WM client reparents a window, mark the new parent as a WM window */
    is_wm_window = dixLookupPrivate(&parent->devPrivates,
                                    &xwl_wm_window_private_key);
    *is_wm_window = TRUE;
}

void
xwl_resize_window(WindowPtr window,
                  int x, int y,
                  unsigned int width, unsigned int height,
                  WindowPtr sib)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;

    xwl_screen = xwl_screen_get(screen);
    xwl_window = xwl_window_from_window(window);

    screen->ResizeWindow = xwl_screen->ResizeWindow;
    screen->ResizeWindow(window, x, y, width, height, sib);
    xwl_screen->ResizeWindow = screen->ResizeWindow;
    screen->ResizeWindow = xwl_resize_window;

    if (xwl_window) {
        if (xwl_window_get(window) || xwl_window_is_toplevel(window)) {
            xwl_window_check_resolution_change_emulation(xwl_window);
        }
        if (window == screen->root) {
#ifdef XWL_HAS_LIBDECOR
            unsigned int decor_width, decor_height;

            decor_width = width / xwl_screen->global_surface_scale;
            decor_height = height / xwl_screen->global_surface_scale;
            xwl_window_update_libdecor_size(xwl_window, NULL,
                                            decor_width, decor_height);
#endif
            xwl_window_check_fractional_scale_viewport(xwl_window, width, height);
        }
    }
}

void
xwl_move_window(WindowPtr window,
                int x, int y,
                WindowPtr next_sib,
                VTKind kind)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen;
    struct xwl_window *xwl_window;

    xwl_screen = xwl_screen_get(screen);
    xwl_window = xwl_window_from_window(window);

    screen->MoveWindow = xwl_screen->MoveWindow;
    (*screen->MoveWindow) (window, x, y, next_sib, kind);
    xwl_screen->MoveWindow = screen->MoveWindow;
    screen->MoveWindow = xwl_move_window;

    if (xwl_window && (xwl_window_get(window) || xwl_window_is_toplevel(window))) {
        xwl_window_check_resolution_change_emulation(xwl_window);
    }
}

static void
frame_callback(void *data,
               struct wl_callback *callback,
               uint32_t time)
{
    struct xwl_window *xwl_window = data;

    wl_callback_destroy (xwl_window->frame_callback);
    xwl_window->frame_callback = NULL;

    if (xwl_window->xwl_screen->present) {
        xwl_present_for_each_frame_callback(xwl_window, xwl_present_frame_callback);

        /* If xwl_window_create_frame_callback was called from
         * xwl_present_frame_callback, need to make sure all fallback timers
         * are adjusted correspondingly.
         */
        if (xwl_window->frame_callback) {
            xwl_present_for_each_frame_callback(xwl_window, xwl_present_reset_timer);
        }
    }
}

static const struct wl_callback_listener frame_listener = {
    .done = frame_callback
};

void
xwl_window_create_frame_callback(struct xwl_window *xwl_window)
{
    xwl_window->frame_callback = wl_surface_frame(xwl_window->surface);
    wl_callback_add_listener(xwl_window->frame_callback, &frame_listener,
                             xwl_window);

    /* If we get called from frame_callback, it will take care of calling
     * xwl_present_reset_timer.
     */
    if (xwl_window->xwl_screen->present &&
        !xwl_present_entered_for_each_frame_callback()) {
        xwl_present_for_each_frame_callback(xwl_window, xwl_present_reset_timer);
    }
}

Bool
xwl_destroy_window(WindowPtr window)
{
    ScreenPtr screen = window->drawable.pScreen;
    struct xwl_screen *xwl_screen = xwl_screen_get(screen);
    struct xwl_window *xwl_window = xwl_window_get(window);
    Bool ret;

    if (xwl_screen->present) {
        xwl_present_cleanup(window);
    }

    if (xwl_window) {
        xwl_window_dispose(xwl_window);
    }

    screen->DestroyWindow = xwl_screen->DestroyWindow;

    if (screen->DestroyWindow) {
        ret = screen->DestroyWindow (window);
    } else {
        ret = TRUE;
    }

    xwl_screen->DestroyWindow = screen->DestroyWindow;
    screen->DestroyWindow = xwl_destroy_window;

    return ret;
}

static Bool
xwl_window_attach_buffer(struct xwl_window *xwl_window)
{
    struct xwl_screen *xwl_screen = xwl_window->xwl_screen;
    WindowPtr surface_window = xwl_window->surface_window;
    RegionPtr region;
    BoxPtr pbox;
    struct wl_buffer *buffer;
    PixmapPtr pixmap;
    int i, num_rects;
    Bool merge_damage = FALSE;

    pixmap = xwl_window_swap_pixmap(xwl_window, TRUE);
    buffer = xwl_pixmap_get_wl_buffer(pixmap);

    if (!buffer) [[unlikely]] {
        ErrorF("Error getting buffer\n");
        return FALSE;
    }

    wl_surface_attach(xwl_window->surface, buffer, 0, 0);

    region = xwl_window_get_damage_region(xwl_window);
    num_rects = RegionNumRects(region);

    if (num_rects == 0) {
        return TRUE;
    }

    /* Use the more modern and efficient wl_surface_damage_buffer if available (wl_surface v4+) */
    if (wl_surface_get_version(xwl_window->surface) >= WL_SURFACE_DAMAGE_BUFFER_SINCE_VERSION) {
        /*
         * Heuristic for damage reporting:
         * 1. If damage is simple (<= 16 rects), send rects individually.
         * 2. If complex (> 16 rects), check if damage is dense (>90% of bounding box).
         *    If so, merge into one damage rect.
         * 3. If complex and sparse, but extremely fragmented (> 256 rects), merge
         *    as a last resort to prevent IPC flooding.
         */
        if (num_rects > 16) {
            if (num_rects > 256) {
                merge_damage = TRUE;
            } else {
                long long total_area = 0;
                BoxPtr extents = RegionExtents(region);
                long long bbox_area = (long long)(extents->x2 - extents->x1) *
                                      (long long)(extents->y2 - extents->y1);
                pbox = RegionRects(region);
                for (i = 0; i < num_rects; i++) {
                    total_area += (long long)(pbox[i].x2 - pbox[i].x1) *
                                  (long long)(pbox[i].y2 - pbox[i].y1);
                }

                /* If total damage area is >= 90% of the bounding box area, merge it */
                if (total_area * 10 >= bbox_area * 9) {
                    merge_damage = TRUE;
                }
            }
        }

        if (merge_damage || (num_rects == 1 && RegionNotEmpty(region))) {
            pbox = RegionExtents(region);
            wl_surface_damage_buffer(xwl_window->surface,
                                     pbox->x1 + surface_window->borderWidth,
                                     pbox->y1 + surface_window->borderWidth,
                                     pbox->x2 - pbox->x1,
                                     pbox->y2 - pbox->y1);
        } else if (num_rects > 1) {
            pbox = RegionRects(region);
            for (i = 0; i < num_rects; i++, pbox++) {
                wl_surface_damage_buffer(xwl_window->surface,
                                         pbox->x1 + surface_window->borderWidth,
                                         pbox->y1 + surface_window->borderWidth,
                                         pbox->x2 - pbox->x1,
                                         pbox->y2 - pbox->y1);
            }
        }
    } else {
        /* Fallback to old wl_surface_damage for older compositors */
        if (num_rects > 256) {
            pbox = RegionExtents(region);
            xwl_surface_damage(xwl_screen, xwl_window->surface,
                               pbox->x1 + surface_window->borderWidth,
                               pbox->y1 + surface_window->borderWidth,
                               pbox->x2 - pbox->x1, pbox->y2 - pbox->y1);
        } else {
            pbox = RegionRects(region);
            for (i = 0; i < num_rects; i++, pbox++) {
                xwl_surface_damage(xwl_screen, xwl_window->surface,
                                   pbox->x1 + surface_window->borderWidth,
                                   pbox->y1 + surface_window->borderWidth,
                                   pbox->x2 - pbox->x1, pbox->y2 - pbox->y1);
            }
        }
    }

    return TRUE;
}

void
xwl_window_post_damage(struct xwl_window *xwl_window)
{
    assert(!xwl_window->frame_callback);

    if (!xwl_window_attach_buffer(xwl_window)) {
        return;
    }

    xwl_window_create_frame_callback(xwl_window);
    DamageEmpty(window_get_damage(xwl_window->surface_window));
}

void
xwl_window_set_input_region(struct xwl_window *xwl_window,
                            RegionPtr input_shape)
{
    struct wl_region *region;
    BoxPtr box;
    int i;

    if (!input_shape) {
        wl_surface_set_input_region(xwl_window->surface, NULL);
        return;
    }

    region = wl_compositor_create_region(xwl_window->xwl_screen->compositor);
    if (!region) {
        ErrorF("Failed creating input region\n");
        return;
    }

    /* Fast path for the most common case: identity scale */
    if (xwl_window->viewport_scale_x == 1.0f && xwl_window->viewport_scale_y == 1.0f) {
        box = RegionRects(input_shape);
        for (i = 0; i < RegionNumRects(input_shape); ++i, ++box) {
            wl_region_add(region,
                          box->x1,
                          box->y1,
                          box->x2 - box->x1,
                          box->y2 - box->y1);
        }
    } else {
        /* Slower path for scaling, optimized with multiply-by-reciprocal */
        box = RegionRects(input_shape);

        const float inv_scale_x = 1.0f / xwl_window->viewport_scale_x;
        const float inv_scale_y = 1.0f / xwl_window->viewport_scale_y;

        for (i = 0; i < RegionNumRects(input_shape); ++i) {
            BoxRec b = box[i];

            b.x1 = floorf((float)b.x1 * inv_scale_x);
            b.x2 = ceilf((float)b.x2 * inv_scale_x);
            b.y1 = floorf((float)b.y1 * inv_scale_y);
            b.y2 = ceilf((float)b.y2 * inv_scale_y);

            wl_region_add(region, b.x1, b.y1, b.x2 - b.x1, b.y2 - b.y1);
        }
    }

    wl_surface_set_input_region(xwl_window->surface, region);
    wl_region_destroy(region);
}

Bool
xwl_window_init(void)
{
    if (!dixRegisterPrivateKey(&xwl_window_private_key, PRIVATE_WINDOW, 0)) {
        return FALSE;
    }

    if (!dixRegisterPrivateKey(&xwl_wm_window_private_key, PRIVATE_WINDOW,
                               sizeof(Bool))) {
        return FALSE;
    }

    if (!dixRegisterPrivateKey(&xwl_damage_private_key, PRIVATE_WINDOW, 0)) {
        return FALSE;
    }

    return TRUE;
}
