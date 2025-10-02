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

#ifndef XWAYLAND_SCREEN_H
#define XWAYLAND_SCREEN_H

#include <xwayland-config.h>

#include <stdio.h>
#include <unistd.h>
#include <X11/X.h>
#include <dix.h>

#include "xwayland-types.h"
#include "xwayland-window.h"
#include "xwayland-output.h"
#include "xwayland-glamor.h"
#include "xwayland-drm-lease.h"
#include "xwayland-dmabuf.h"

#ifdef XWL_HAS_LIBDECOR
#include <libdecor.h>
#endif

/* Forward declarations to reduce header coupling */
struct xwl_format;
struct xwl_emulated_mode;

struct xwl_screen {
    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Core Screen Properties
     * ═════════════════════════════════════════════════════════════════════
     */
    ScreenPtr screen;
    double width;
    double height;
    int depth;
    int global_surface_scale;
    enum RootClipMode root_clip_mode;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Configuration Flags
     * ═════════════════════════════════════════════════════════════════════
     */
    Bool rootless;
    Bool fullscreen;
    Bool host_grab;
    Bool has_grab;
    Bool decorate;
    Bool enable_ei_portal;
    Bool nokeymap;
    Bool hidpi;
    xwl_glamor_mode_flags glamor;
    Bool present;
    Bool force_xrandr_emulation;
    Bool active;
    Bool use_damage_buffer;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Wrapped X Server Function Pointers (for hooking screen operations)
     * ═════════════════════════════════════════════════════════════════════
     */
    CreateScreenResourcesProcPtr CreateScreenResources;
    CloseScreenProcPtr CloseScreen;
    RealizeWindowProcPtr RealizeWindow;
    UnrealizeWindowProcPtr UnrealizeWindow;
    DestroyWindowProcPtr DestroyWindow;
    ConfigNotifyProcPtr ConfigNotify;
    ChangeWindowAttributesProcPtr ChangeWindowAttributes;
    ReparentWindowProcPtr ReparentWindow;
    ResizeWindowProcPtr ResizeWindow;
    MoveWindowProcPtr MoveWindow;
    SetWindowPixmapProcPtr SetWindowPixmap;
    XYToWindowProcPtr XYToWindow;
    SourceValidateProcPtr SourceValidate;
    SetShapeProcPtr SetShape;
    ClipNotifyProcPtr ClipNotify;
    StoreColorsProcPtr StoreColors;
    InstallColormapProcPtr InstallColormap;
    UninstallColormapProcPtr UninstallColormap;
    QueryBestSizeProcPtr QueryBestSize;
    int (*GrabServer) (ClientPtr client);
    int (*UngrabServer) (ClientPtr client);

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Object Lists & State Management
     * ═════════════════════════════════════════════════════════════════════
     */
    struct xorg_list output_list;
    struct xorg_list seat_list;
    struct xorg_list damage_window_list;
    struct xorg_list window_list;
    struct xorg_list drm_lease_devices;
    struct xorg_list queued_drm_lease_devices;
    struct xorg_list drm_leases;
    struct xorg_list pending_wl_surface_destroy;

    int wm_client_id;
    int expecting_event;
    int need_source_validate;
    Bool ignore_damage;
    uint32_t serial;
    uint64_t surface_association_serial;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Wayland Event Loop State
     *
     *  These flags manage the core event loop synchronization with the
     *  Wayland display server. This is a performance-critical section.
     *
     *  - wait_flush: If true, indicates that a previous wl_display_flush()
     *    failed (likely due to a full buffer), and we must successfully
     *    flush before waiting for new events.
     *
     *  - prepare_read: A flag to indicate that we are ready to block in
     *    poll(). It is used to break out of a potential busy-wait loop in
     *    xwl_sync_events() if wl_display_prepare_read() fails because other
     *    code is concurrently flushing to the display.
     * ═════════════════════════════════════════════════════════════════════
     */
    int prepare_read;
    int wait_flush;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Wayland Protocol Objects (must be destroyed in xwl_close_screen)
     * ═════════════════════════════════════════════════════════════════════
     */
    int wayland_fd;
    struct wl_display *display;
    struct wl_registry *registry;
    struct wl_registry *input_registry;
    struct wl_compositor *compositor;
    struct wl_shm *shm;
    struct xdg_wm_base *xdg_wm_base;
    struct zwp_tablet_manager_v2 *tablet_manager;
    struct zwp_relative_pointer_manager_v1 *relative_pointer_manager;
    struct zwp_pointer_constraints_v1 *pointer_constraints;
    struct zwp_pointer_gestures_v1 *pointer_gestures;
    struct zwp_xwayland_keyboard_grab_manager_v1 *wp_grab;
    struct zwp_keyboard_shortcuts_inhibit_manager_v1 *shortcuts_inhibit_manager;
    struct zwp_keyboard_shortcuts_inhibitor_v1 *shortcuts_inhibit;
    struct zwp_linux_dmabuf_v1 *dmabuf;
    struct zxdg_output_manager_v1 *xdg_output_manager;
    struct wp_viewporter *viewporter;
    struct xwayland_shell_v1 *xwayland_shell;
    struct wp_tearing_control_manager_v1 *tearing_control_manager;
    struct wp_fractional_scale_manager_v1 *fractional_scale_manager;
    struct wp_linux_drm_syncobj_manager_v1 *explicit_sync;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  EGL & Glamor Integration
     * ═════════════════════════════════════════════════════════════════════
     */
    void *egl_display;
    void *egl_context;
    struct glamor_context *glamor_ctx;
    const char *glvnd_vendor;
    uint32_t present_capabilities;

    int dmabuf_protocol_version;
    struct xwl_dmabuf_feedback default_feedback;

    uint32_t num_formats;
    struct xwl_format *formats;

    /*
     * ═════════════════════════════════════════════════════════════════════
     *  Rootful / Rootless Specifics
     * ═════════════════════════════════════════════════════════════════════
     */
    struct xwl_output *fixed_output;
    const char *output_name;
    int output_name_serial;
#ifdef XWL_HAS_LIBDECOR
    int libdecor_fd;
    struct libdecor *libdecor_context;
#endif

    Atom allow_commits_prop;
};

/* Apps which use randr/vidmode to change the mode when going fullscreen,
 * usually change the mode of only a single monitor, so this should be plenty.
 */
#define XWL_CLIENT_MAX_EMULATED_MODES 16

struct xwl_client {
    struct xwl_emulated_mode emulated_modes[XWL_CLIENT_MAX_EMULATED_MODES];
};

struct xwl_client *xwl_client_get(ClientPtr client);
struct xwl_screen *xwl_screen_get(ScreenPtr screen);
Bool xwl_screen_has_viewport_support(struct xwl_screen *xwl_screen);
Bool xwl_screen_has_resolution_change_emulation(struct xwl_screen *xwl_screen);
void xwl_screen_check_resolution_change_emulation(struct xwl_screen *xwl_screen);
struct xwl_output *xwl_screen_get_first_output(struct xwl_screen *xwl_screen);
struct xwl_output *xwl_screen_get_fixed_or_first_output(struct xwl_screen *xwl_screen);
int xwl_screen_get_width(struct xwl_screen *xwl_screen);
int xwl_screen_get_height(struct xwl_screen *xwl_screen);

Bool xwl_close_screen(ScreenPtr screen);
Bool xwl_screen_init(ScreenPtr pScreen, int argc, char **argv);
void xwl_sync_events(struct xwl_screen *xwl_screen);
void xwl_screen_roundtrip(struct xwl_screen *xwl_screen);
void xwl_surface_damage(struct xwl_screen *xwl_screen,
                        struct wl_surface *surface,
                        int32_t x, int32_t y, int32_t width, int32_t height);
int xwl_screen_get_next_output_serial(struct xwl_screen * xwl_screen);
void xwl_screen_lost_focus(struct xwl_screen *xwl_screen);
Bool xwl_screen_update_global_surface_scale(struct xwl_screen *xwl_screen);
Bool xwl_screen_should_use_fractional_scale(struct xwl_screen *xwl_screen);

#endif /* XWAYLAND_SCREEN_H */
