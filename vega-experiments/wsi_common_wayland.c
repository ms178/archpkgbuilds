/*
 * Copyright © 2015 Intel Corporation
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
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <wayland-client.h>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <poll.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <math.h>

#include "drm-uapi/drm_fourcc.h"

#include "vk_instance.h"
#include "vk_device.h"
#include "vk_physical_device.h"
#include "vk_util.h"
#include "wsi_common_entrypoints.h"
#include "wsi_common_private.h"
#include "fifo-v1-client-protocol.h"
#include "commit-timing-v1-client-protocol.h"
#include "linux-dmabuf-unstable-v1-client-protocol.h"
#include "presentation-time-client-protocol.h"
#include "linux-drm-syncobj-v1-client-protocol.h"
#include "tearing-control-v1-client-protocol.h"
#include "color-management-v1-client-protocol.h"

#include <util/cnd_monotonic.h>
#include <util/compiler.h>
#include <util/hash_table.h>
#include <util/timespec.h>
#include <util/u_endian.h>
#include <util/u_vector.h>
#include <util/u_dynarray.h>
#include <util/anon_file.h>
#include <util/os_time.h>

#include <loader/loader_wayland_helper.h>

#ifdef MAJOR_IN_MKDEV
#include <sys/mkdev.h>
#endif
#ifdef MAJOR_IN_SYSMACROS
#include <sys/sysmacros.h>
#endif

#if defined(__has_include)
#  if __has_include("fixes-client-protocol.h")
#    include "fixes-client-protocol.h"
#    define WSI_WL_HAS_FIXES_PROTOCOL 1
#  else
#    define WSI_WL_HAS_FIXES_PROTOCOL 0
#  endif
#else
#  define WSI_WL_HAS_FIXES_PROTOCOL 0
#endif

struct wsi_wayland;

struct wsi_wl_format {
   VkFormat vk_format;
   uint32_t flags;
   struct u_vector modifiers;
};

struct dmabuf_feedback_format_table {
   unsigned int size;
   struct {
      uint32_t format;
      uint32_t padding; /* unused */
      uint64_t modifier;
   } *data;
};

struct dmabuf_feedback_tranche {
   dev_t target_device;
   uint32_t flags;
   struct u_vector formats;
};

struct dmabuf_feedback {
   dev_t main_device;
   struct dmabuf_feedback_format_table format_table;
   struct util_dynarray tranches;
   struct dmabuf_feedback_tranche pending_tranche;
};

struct wsi_wl_display {
   /* The real wl_display */
   struct wl_display *wl_display;
   /* Actually a proxy wrapper around the event queue */
   struct wl_display *wl_display_wrapper;
   struct wl_event_queue *queue;
#if WSI_WL_HAS_FIXES_PROTOCOL
   struct wl_fixes *wl_fixes;
#endif

   struct wl_shm *wl_shm;
   struct zwp_linux_dmabuf_v1 *wl_dmabuf;
   struct zwp_linux_dmabuf_feedback_v1 *wl_dmabuf_feedback;
   struct wp_tearing_control_manager_v1 *tearing_control_manager;
   struct wp_linux_drm_syncobj_manager_v1 *wl_syncobj;

   struct wp_color_manager_v1 *color_manager;

   struct dmabuf_feedback_format_table format_table;

   struct u_vector color_primaries;
   struct u_vector color_transfer_funcs;

   /* users want per-chain wsi_wl_swapchain->present_ids.wp_presentation */
   struct wp_presentation *wp_presentation_notwrapped;
   uint32_t wp_presentation_version;

   struct wp_fifo_manager_v1 *fifo_manager;
   struct wp_commit_timing_manager_v1 *commit_timing_manager;
   bool no_timestamps;

   struct wsi_wayland *wsi_wl;

   /* Formats populated by zwp_linux_dmabuf_v1 or wl_shm interfaces */
   struct u_vector formats;

   /* Additional colorspaces returned by wp_color_management_v1. */
   struct u_vector colorspaces;

   bool sw;

   dev_t main_device;
   bool same_gpu;
   bool has_tearing;

   clockid_t presentation_clock_id;

   struct {
      bool mastering_display_primaries;
      bool extended_target_volume;
   } color_features;
};

struct wsi_wayland {
   struct wsi_interface base;

   struct wsi_device *wsi;

   const VkAllocationCallbacks *alloc;
   VkPhysicalDevice physical_device;
};

struct wsi_wl_image {
   struct wsi_image base;
   struct loader_wayland_buffer wayland_buffer;
   bool busy;
   int shm_fd;
   void *shm_ptr;
   unsigned shm_size;

   struct wp_linux_drm_syncobj_timeline_v1 *wl_syncobj_timeline[WSI_ES_COUNT];
};

enum wsi_wl_buffer_type {
   WSI_WL_BUFFER_NATIVE,
   WSI_WL_BUFFER_GPU_SHM,
   WSI_WL_BUFFER_SHM_MEMCPY,
};

struct wsi_wl_surface {
   VkIcdSurfaceWayland base;

   unsigned int chain_count;

   struct wsi_wl_swapchain *chain;
   struct loader_wayland_surface wayland_surface;
   struct wsi_wl_display *display;

   struct zwp_linux_dmabuf_feedback_v1 *wl_dmabuf_feedback;
   struct dmabuf_feedback dmabuf_feedback, pending_dmabuf_feedback;

   struct wp_linux_drm_syncobj_surface_v1 *wl_syncobj_surface;

   struct vk_instance *instance;

   struct {
      struct wp_color_management_surface_v1 *color_surface;
      int color_surface_refcount;
      VkColorSpaceKHR colorspace;
      VkHdrMetadataEXT hdr_metadata;
      bool has_hdr_metadata;
   } color;

   struct {
      /* Cached structures, for eg. GetPhysicalDeviceSurfaceFormatsKHR to avoid
       * round-trips every single time they are called, which is slow
       * and leaks wl_registries on the server side[1].
       *
       * [1]: https://gitlab.freedesktop.org/wayland/wayland/-/merge_requests/388
       */
      mtx_t lock;
      struct wsi_device *last_device;
      struct wsi_wl_display cached_display;
   } query_cache;
};

struct wsi_wl_swapchain {
   struct wsi_swapchain base;

   struct wsi_wl_surface *wsi_wl_surface;
   struct wp_tearing_control_v1 *tearing_control;
   struct wp_fifo_v1 *fifo;
   struct wp_commit_timer_v1 *commit_timer;

   struct wl_callback *frame;

   VkExtent2D extent;
   VkFormat vk_format;
   enum wsi_wl_buffer_type buffer_type;
   uint32_t drm_format;
   enum wl_shm_format shm_format;

   bool suboptimal;
   bool retired;

   uint32_t num_drm_modifiers;
   const uint64_t *drm_modifiers;

   bool legacy_fifo_ready;
   bool next_present_force_wait_barrier;

   struct {
      mtx_t lock; /* protects all members */
      uint64_t max_completed;
      uint64_t max_forward_progress_present_id;
      uint64_t max_present_id;
      uint64_t prev_max_present_id;
      uint64_t outstanding_count;

      struct wl_list fallback_frame_list;
      struct u_cnd_monotonic list_advanced;
      struct wl_event_queue *queue;
      struct loader_wayland_presentation wayland_presentation;
      /* Fallback when wp_presentation is not supported */
      struct wl_surface *surface;
      bool dispatch_in_progress;

      uint64_t display_time_error;
      uint64_t display_time_correction;
      uint64_t last_target_time;
      uint64_t displayed_time;
      bool valid_refresh_nsec;
      unsigned int refresh_nsec;
      bool frame_fallback;
   } present_ids;

   struct {
      VkColorSpaceKHR colorspace;
      VkHdrMetadataEXT hdr_metadata;
      bool has_hdr_metadata;
   } color;

   struct wsi_image_timing_request timing_request;

   struct wsi_wl_image images[0];
};
VK_DEFINE_NONDISP_HANDLE_CASTS(wsi_wl_swapchain, base.base, VkSwapchainKHR,
                               VK_OBJECT_TYPE_SWAPCHAIN_KHR)

static bool
wsi_wl_use_explicit_sync(struct wsi_wl_display *display, struct wsi_device *device)
{
   return wsi_device_supports_explicit_sync(device) &&
          display->wl_syncobj != NULL;
}

enum wsi_wl_fmt_flag {
   WSI_WL_FMT_ALPHA = 1 << 0,
   WSI_WL_FMT_OPAQUE = 1 << 1,
};

static struct wsi_wl_format *
find_format(struct u_vector *formats, VkFormat format)
{
   struct wsi_wl_format *f;

   u_vector_foreach(f, formats)
      if (f->vk_format == format)
         return f;

   return NULL;
}

/* Given a time base and a refresh period, find the next
 * time past 'from' that is an even multiple of the period
 * past the base.
 */
static uint64_t
next_phase_locked_time(uint64_t base, uint64_t period, uint64_t from)
{
   uint64_t target, cycles;

   assert(from != 0);

   if (base == 0)
      return from;

   /* If our time base is in the future (which can happen when using
    * presentation feedback events), target the next possible
    * presentation time.
    */
   if (base >= from)
      return base + period;

   /* The presentation time extension recommends that the compositor
    * use a clock with "precision of one millisecond or better",
    * so we shouldn't rely on these times being perfectly precise.
    *
    * Additionally, some compositors round off feedback times
    * internally, (eg: to microsecond precision), so our times can
    * have some jitter in either direction.
    *
    * We need to be especially careful not to miss an opportunity
    * to display by calculating a cycle too far into the future.
    * This will cause delays in frame presentation.
    *
    * If we choose a cycle too soon, fifo barrier will still keep
    * the pace properly, except in the case of occluded surfaces -
    * but occluded surfaces don't move their base time in response
    * to presentation events, so there is no jitter and the math
    * is more forgiving. That case just needs to monotonically
    * increase.
    *
    * We fairly arbitrarily use period / 4 here to try to stay
    * well away from rounding up too far, but to also avoid
    * scheduling too soon if the time values are imprecise.
    */
   cycles = (from - base + (period >> 2)) / period;
   target = base + (cycles + 1) * period;
   return target;
}

static struct wsi_wl_format *
wsi_wl_display_add_vk_format(struct wsi_wl_display *display,
                             struct u_vector *formats,
                             VkFormat format, uint32_t flags)
{
   assert(flags & (WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE));

   /* Don't add a format that's already in the list */
   struct wsi_wl_format *f = find_format(formats, format);
   if (f) {
      f->flags |= flags;
      return f;
   }

   /* Don't add formats that aren't renderable. */
   VkFormatProperties props;

   display->wsi_wl->wsi->GetPhysicalDeviceFormatProperties(display->wsi_wl->physical_device,
                                                           format, &props);
   if (!(props.optimalTilingFeatures & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT))
      return NULL;

   struct u_vector modifiers;
   if (!u_vector_init_pow2(&modifiers, 4, sizeof(uint64_t)))
      return NULL;

   f = u_vector_add(formats);
   if (!f) {
      u_vector_finish(&modifiers);
      return NULL;
   }

   f->vk_format = format;
   f->flags = flags;
   f->modifiers = modifiers;

   return f;
}

static void
wsi_wl_format_add_modifier(struct wsi_wl_format *format, uint64_t modifier)
{
   uint64_t *mod;

   if (modifier == DRM_FORMAT_MOD_INVALID)
      return;

   u_vector_foreach(mod, &format->modifiers)
      if (*mod == modifier)
         return;

   mod = u_vector_add(&format->modifiers);
   if (mod)
      *mod = modifier;
}

static void
wsi_wl_display_add_vk_format_modifier(struct wsi_wl_display *display,
                                      struct u_vector *formats,
                                      VkFormat vk_format, uint32_t flags,
                                      uint64_t modifier)
{
   struct wsi_wl_format *format;

   format = wsi_wl_display_add_vk_format(display, formats, vk_format, flags);
   if (format)
      wsi_wl_format_add_modifier(format, modifier);
}

static void
wsi_wl_display_add_drm_format_modifier(struct wsi_wl_display *display,
                                       struct u_vector *formats,
                                       uint32_t drm_format, uint64_t modifier)
{
   VK_FROM_HANDLE(vk_physical_device, pdevice, display->wsi_wl->physical_device);
   struct wsi_device *wsi_device = pdevice->wsi_device;

   /* From Vulkan 1.3 onwards, we can always try adding the 4444 formats.
    * If the format isn't supported or isn't renderable,
    * wsi_wl_display_add_vk_format() will reject it via
    * vkGetPhysicalDeviceFormatProperties().
    */
   if (pdevice->supported_features.formatA4R4G4B4 ||
       pdevice->properties.apiVersion >= VK_MAKE_VERSION(1, 3, 0)) {
      switch (drm_format) {
      case DRM_FORMAT_ARGB4444:
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_A4R4G4B4_UNORM_PACK16,
                                               WSI_WL_FMT_ALPHA, modifier);
         break;
      case DRM_FORMAT_XRGB4444:
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_A4R4G4B4_UNORM_PACK16,
                                               WSI_WL_FMT_OPAQUE, modifier);
         break;
      }
   }
   if (pdevice->supported_features.formatA4B4G4R4 ||
       pdevice->properties.apiVersion >= VK_MAKE_VERSION(1, 3, 0)) {
      switch (drm_format) {
      case DRM_FORMAT_ABGR4444:
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_A4B4G4R4_UNORM_PACK16,
                                               WSI_WL_FMT_ALPHA, modifier);
         break;
      case DRM_FORMAT_XBGR4444:
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_A4B4G4R4_UNORM_PACK16,
                                               WSI_WL_FMT_OPAQUE, modifier);
         break;
      }
   }

   switch (drm_format) {
   /* Vulkan _PACKN formats have the same component order as DRM formats
    * on little endian systems, on big endian there exists no analog. */
#if UTIL_ARCH_LITTLE_ENDIAN
   case DRM_FORMAT_RGBA4444:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R4G4B4A4_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_RGBX4444:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R4G4B4A4_UNORM_PACK16,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_BGRA4444:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B4G4R4A4_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_BGRX4444:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B4G4R4A4_UNORM_PACK16,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_RGB565:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R5G6B5_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                            modifier);
      break;
   case DRM_FORMAT_BGR565:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B5G6R5_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                            modifier);
      break;
   case DRM_FORMAT_ARGB1555:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A1R5G5B5_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_XRGB1555:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A1R5G5B5_UNORM_PACK16,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_RGBA5551:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R5G5B5A1_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_RGBX5551:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R5G5B5A1_UNORM_PACK16,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_BGRA5551:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B5G5R5A1_UNORM_PACK16,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_BGRX5551:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B5G5R5A1_UNORM_PACK16,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_ARGB2101010:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A2R10G10B10_UNORM_PACK32,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_XRGB2101010:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A2R10G10B10_UNORM_PACK32,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_ABGR2101010:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_XBGR2101010:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;

   /* Vulkan 16-bits-per-channel formats have an inverted channel order
    * compared to DRM formats, just like the 8-bits-per-channel ones.
    * On little endian systems the memory representation of each channel
    * matches the DRM formats'. */
   case DRM_FORMAT_ABGR16161616:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R16G16B16A16_UNORM,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_XBGR16161616:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R16G16B16A16_UNORM,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_ABGR16161616F:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R16G16B16A16_SFLOAT,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_XBGR16161616F:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R16G16B16A16_SFLOAT,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
#endif

   /* Non-packed 8-bit formats have an inverted channel order compared to the
    * little endian DRM formats, because the DRM channel ordering is high->low
    * but the vulkan channel ordering is in memory byte order
    *
    * For all UNORM formats which have a SRGB variant, we must support both if
    * we can. SRGB in this context means that rendering to it will result in a
    * linear -> nonlinear SRGB colorspace conversion before the data is stored.
    * The inverse function is applied when sampling from SRGB images.
    * From Wayland's perspective nothing changes, the difference is just how
    * Vulkan interprets the pixel data.
    *
    * For bonus points, 24bpp VkFormats may appear as 32bpp, depending on the
    * driver.
    */
   case DRM_FORMAT_BGR888:
      if (!wsi_device->emulate_24as32) {
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_R8G8B8_SRGB,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_R8G8B8_UNORM,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
      }
      break;
   case DRM_FORMAT_XBGR8888:
      if (wsi_device->emulate_24as32) {
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_R8G8B8_SRGB,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_R8G8B8_UNORM,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
      }
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R8G8B8A8_SRGB,
                                            WSI_WL_FMT_OPAQUE, modifier);
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R8G8B8A8_UNORM,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_ABGR8888:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R8G8B8A8_SRGB,
                                            WSI_WL_FMT_ALPHA, modifier);
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_R8G8B8A8_UNORM,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   case DRM_FORMAT_RGB888:
      if (!wsi_device->emulate_24as32) {
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_B8G8R8_SRGB,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_B8G8R8_UNORM,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
      }
      break;
   case DRM_FORMAT_XRGB8888:
      if (wsi_device->emulate_24as32) {
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_B8G8R8_SRGB,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
         wsi_wl_display_add_vk_format_modifier(display, formats,
                                               VK_FORMAT_B8G8R8_UNORM,
                                               WSI_WL_FMT_ALPHA | WSI_WL_FMT_OPAQUE,
                                               modifier);
      }
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B8G8R8A8_SRGB,
                                            WSI_WL_FMT_OPAQUE, modifier);
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B8G8R8A8_UNORM,
                                            WSI_WL_FMT_OPAQUE, modifier);
      break;
   case DRM_FORMAT_ARGB8888:
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B8G8R8A8_SRGB,
                                            WSI_WL_FMT_ALPHA, modifier);
      wsi_wl_display_add_vk_format_modifier(display, formats,
                                            VK_FORMAT_B8G8R8A8_UNORM,
                                            WSI_WL_FMT_ALPHA, modifier);
      break;
   }
}

static uint32_t
drm_format_for_wl_shm_format(enum wl_shm_format shm_format)
{
   /* wl_shm formats are identical to DRM, except ARGB8888 and XRGB8888 */
   switch (shm_format) {
   case WL_SHM_FORMAT_ARGB8888:
      return DRM_FORMAT_ARGB8888;
   case WL_SHM_FORMAT_XRGB8888:
      return DRM_FORMAT_XRGB8888;
   default:
      return shm_format;
   }
}

static void
wsi_wl_display_add_wl_shm_format(struct wsi_wl_display *display,
                                 struct u_vector *formats,
                                 enum wl_shm_format shm_format)
{
   uint32_t drm_format = drm_format_for_wl_shm_format(shm_format);

   wsi_wl_display_add_drm_format_modifier(display, formats, drm_format,
                                          DRM_FORMAT_MOD_INVALID);
}

static uint32_t
wl_drm_format_for_vk_format(struct wsi_device *wsi_device,
                            VkFormat vk_format, bool alpha)
{
   switch (vk_format) {
   case VK_FORMAT_A4R4G4B4_UNORM_PACK16:
      return alpha ? DRM_FORMAT_ARGB4444 : DRM_FORMAT_XRGB4444;
   case VK_FORMAT_A4B4G4R4_UNORM_PACK16:
      return alpha ? DRM_FORMAT_ABGR4444 : DRM_FORMAT_XBGR4444;
#if UTIL_ARCH_LITTLE_ENDIAN
   case VK_FORMAT_R4G4B4A4_UNORM_PACK16:
      return alpha ? DRM_FORMAT_RGBA4444 : DRM_FORMAT_RGBX4444;
   case VK_FORMAT_B4G4R4A4_UNORM_PACK16:
      return alpha ? DRM_FORMAT_BGRA4444 : DRM_FORMAT_BGRX4444;
   case VK_FORMAT_R5G6B5_UNORM_PACK16:
      return DRM_FORMAT_RGB565;
   case VK_FORMAT_B5G6R5_UNORM_PACK16:
      return DRM_FORMAT_BGR565;
   case VK_FORMAT_A1R5G5B5_UNORM_PACK16:
      return alpha ? DRM_FORMAT_ARGB1555 : DRM_FORMAT_XRGB1555;
   case VK_FORMAT_R5G5B5A1_UNORM_PACK16:
      return alpha ? DRM_FORMAT_RGBA5551 : DRM_FORMAT_RGBX5551;
   case VK_FORMAT_B5G5R5A1_UNORM_PACK16:
      return alpha ? DRM_FORMAT_BGRA5551 : DRM_FORMAT_BGRX5551;
   case VK_FORMAT_A2R10G10B10_UNORM_PACK32:
      return alpha ? DRM_FORMAT_ARGB2101010 : DRM_FORMAT_XRGB2101010;
   case VK_FORMAT_A2B10G10R10_UNORM_PACK32:
      return alpha ? DRM_FORMAT_ABGR2101010 : DRM_FORMAT_XBGR2101010;
   case VK_FORMAT_R16G16B16A16_UNORM:
      return alpha ? DRM_FORMAT_ABGR16161616 : DRM_FORMAT_XBGR16161616;
   case VK_FORMAT_R16G16B16A16_SFLOAT:
      return alpha ? DRM_FORMAT_ABGR16161616F : DRM_FORMAT_XBGR16161616F;
#endif
   case VK_FORMAT_R8G8B8_UNORM:
   case VK_FORMAT_R8G8B8_SRGB:
      return wsi_device->emulate_24as32 ? DRM_FORMAT_XBGR8888 : DRM_FORMAT_BGR888;
   case VK_FORMAT_R8G8B8A8_UNORM:
   case VK_FORMAT_R8G8B8A8_SRGB:
      return alpha ? DRM_FORMAT_ABGR8888 : DRM_FORMAT_XBGR8888;
   case VK_FORMAT_B8G8R8_UNORM:
   case VK_FORMAT_B8G8R8_SRGB:
      return wsi_device->emulate_24as32 ? DRM_FORMAT_XRGB8888 : DRM_FORMAT_RGB888;
   case VK_FORMAT_B8G8R8A8_UNORM:
   case VK_FORMAT_B8G8R8A8_SRGB:
      return alpha ? DRM_FORMAT_ARGB8888 : DRM_FORMAT_XRGB8888;

   default:
      assert(!"Unsupported Vulkan format");
      return DRM_FORMAT_INVALID;
   }
}

static enum wl_shm_format
wl_shm_format_for_vk_format(struct wsi_device *wsi_device,
                            VkFormat vk_format, bool alpha)
{
   uint32_t drm_format =
      wl_drm_format_for_vk_format(wsi_device, vk_format, alpha);

   if (drm_format == DRM_FORMAT_INVALID) {
      return 0;
   }

   /* wl_shm formats are identical to DRM, except ARGB8888 and XRGB8888 */
   switch (drm_format) {
   case DRM_FORMAT_ARGB8888:
      return WL_SHM_FORMAT_ARGB8888;
   case DRM_FORMAT_XRGB8888:
      return WL_SHM_FORMAT_XRGB8888;
   default:
      return drm_format;
   }
}

static void
dmabuf_handle_format(void *data, struct zwp_linux_dmabuf_v1 *dmabuf,
                     uint32_t format)
{
   /* Formats are implicitly advertised by the modifier event, so we ignore
    * them here. */
}

static void
dmabuf_handle_modifier(void *data, struct zwp_linux_dmabuf_v1 *dmabuf,
                       uint32_t format, uint32_t modifier_hi,
                       uint32_t modifier_lo)
{
   struct wsi_wl_display *display = data;
   uint64_t modifier;

   /* Ignore this if the compositor advertised dma-buf feedback. From version 4
    * onwards (when dma-buf feedback was introduced), the compositor should not
    * advertise this event anymore, but let's keep this for safety. */
   if (display->wl_dmabuf_feedback)
      return;

   modifier = ((uint64_t) modifier_hi << 32) | modifier_lo;
   wsi_wl_display_add_drm_format_modifier(display, &display->formats,
                                          format, modifier);
}

static const struct zwp_linux_dmabuf_v1_listener dmabuf_listener = {
   dmabuf_handle_format,
   dmabuf_handle_modifier,
};

static void
dmabuf_feedback_format_table_fini(struct dmabuf_feedback_format_table *format_table)
{
   if (format_table->data && format_table->data != MAP_FAILED)
      munmap(format_table->data, format_table->size);
   /* Prevent double-munmap if fini is called twice before reinit. */
   format_table->data = NULL;
   format_table->size = 0;
}

static void
dmabuf_feedback_format_table_init(struct dmabuf_feedback_format_table *format_table)
{
   memset(format_table, 0, sizeof(*format_table));
}

static void
dmabuf_feedback_tranche_fini(struct dmabuf_feedback_tranche *tranche)
{
   struct wsi_wl_format *format;

   u_vector_foreach(format, &tranche->formats)
      u_vector_finish(&format->modifiers);

   u_vector_finish(&tranche->formats);
}

static int
dmabuf_feedback_tranche_init(struct dmabuf_feedback_tranche *tranche)
{
   memset(tranche, 0, sizeof(*tranche));

   if (!u_vector_init(&tranche->formats, 8, sizeof(struct wsi_wl_format)))
      return -1;

   return 0;
}

static void
dmabuf_feedback_fini(struct dmabuf_feedback *dmabuf_feedback)
{
   dmabuf_feedback_tranche_fini(&dmabuf_feedback->pending_tranche);

   util_dynarray_foreach(&dmabuf_feedback->tranches,
                         struct dmabuf_feedback_tranche, tranche)
      dmabuf_feedback_tranche_fini(tranche);
   util_dynarray_fini(&dmabuf_feedback->tranches);

   dmabuf_feedback_format_table_fini(&dmabuf_feedback->format_table);
}

static int
dmabuf_feedback_init(struct dmabuf_feedback *dmabuf_feedback)
{
   memset(dmabuf_feedback, 0, sizeof(*dmabuf_feedback));

   if (dmabuf_feedback_tranche_init(&dmabuf_feedback->pending_tranche) < 0)
      return -1;

   dmabuf_feedback->tranches = UTIL_DYNARRAY_INIT;

   dmabuf_feedback_format_table_init(&dmabuf_feedback->format_table);

   return 0;
}

static size_t
dmabuf_feedback_format_table_entry_count(const struct dmabuf_feedback_format_table *format_table)
{
      if (format_table == NULL || format_table->data == NULL || format_table->data == MAP_FAILED)
            return 0;

      return format_table->size / sizeof(format_table->data[0]);
}

static void
wsi_wl_display_finish_feedback(struct wsi_wl_display *display)
{
      if (display->wl_dmabuf_feedback) {
            zwp_linux_dmabuf_feedback_v1_destroy(display->wl_dmabuf_feedback);
            display->wl_dmabuf_feedback = NULL;
      }

      dmabuf_feedback_format_table_fini(&display->format_table);
      dmabuf_feedback_format_table_init(&display->format_table);
}

static void
wsi_wl_registry_destroy(struct wsi_wl_display *display, struct wl_registry **registry)
{
      if (registry == NULL || *registry == NULL)
            return;

      #if WSI_WL_HAS_FIXES_PROTOCOL
      if (display->wl_fixes)
            wl_fixes_destroy_registry(display->wl_fixes, *registry);
      #endif

      wl_registry_destroy(*registry);
      *registry = NULL;
}

static VkResult
wsi_wl_roundtrip_queue(struct wsi_wl_display *display)
{
      return wl_display_roundtrip_queue(display->wl_display, display->queue) < 0 ?
      VK_ERROR_SURFACE_LOST_KHR :
      VK_SUCCESS;
}

static uint32_t
wsi_wl_round_to_u32_sat(double value)
{
      if (!isfinite(value) || value <= 0.0)
            return 0;

      if (value >= (double)UINT32_MAX)
            return UINT32_MAX;

      return (uint32_t)llround(value);
}

static void
default_dmabuf_feedback_format_table(void *data,
                                     struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                     int32_t fd, uint32_t size)
{
   struct wsi_wl_display *display = data;
   void *map;

   (void)dmabuf_feedback;

   dmabuf_feedback_format_table_fini(&display->format_table);
   dmabuf_feedback_format_table_init(&display->format_table);

   if (fd < 0)
      return;

   if (size == 0 || (size % sizeof(display->format_table.data[0])) != 0u) {
      close(fd);
      return;
   }

   map = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
   close(fd);

   if (map == MAP_FAILED) {
      display->format_table.data = MAP_FAILED;
      display->format_table.size = 0;
      return;
   }

   display->format_table.data = map;
   display->format_table.size = size;
}

static void
default_dmabuf_feedback_main_device(void *data,
                                    struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                    struct wl_array *device)
{
   struct wsi_wl_display *display = data;

   (void)dmabuf_feedback;

   if (device == NULL || device->data == NULL ||
       device->size != sizeof(display->main_device)) {
      display->main_device = 0;
      return;
   }

   memcpy(&display->main_device, device->data, sizeof(display->main_device));
}

static void
default_dmabuf_feedback_tranche_target_device(void *data,
                                              struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                              struct wl_array *device)
{
   /* ignore this event */
}

static void
default_dmabuf_feedback_tranche_flags(void *data,
                                      struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                      uint32_t flags)
{
   /* ignore this event */
}

static void
default_dmabuf_feedback_tranche_formats(void *data,
                                        struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                        struct wl_array *indices)
{
   struct wsi_wl_display *display = data;
   const size_t entry_count =
      dmabuf_feedback_format_table_entry_count(&display->format_table);
   uint16_t *index;

   (void)dmabuf_feedback;

   if (indices == NULL || indices->data == NULL)
      return;

   if (indices->size % sizeof(uint16_t) != 0)
      return;

   if (display->format_table.data == NULL ||
       display->format_table.data == MAP_FAILED)
      return;

   if (entry_count == 0)
      return;

   wl_array_for_each(index, indices) {
      if ((size_t)*index >= entry_count)
         continue;

      const uint32_t format = display->format_table.data[*index].format;
      const uint64_t modifier = display->format_table.data[*index].modifier;

      wsi_wl_display_add_drm_format_modifier(display, &display->formats,
                                             format, modifier);
   }
}

static void
default_dmabuf_feedback_tranche_done(void *data,
                                     struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback)
{
   /* ignore this event */
}

static void
default_dmabuf_feedback_done(void *data,
                             struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback)
{
   /* ignore this event */
}

static const struct zwp_linux_dmabuf_feedback_v1_listener
dmabuf_feedback_listener = {
   .format_table = default_dmabuf_feedback_format_table,
   .main_device = default_dmabuf_feedback_main_device,
   .tranche_target_device = default_dmabuf_feedback_tranche_target_device,
   .tranche_flags = default_dmabuf_feedback_tranche_flags,
   .tranche_formats = default_dmabuf_feedback_tranche_formats,
   .tranche_done = default_dmabuf_feedback_tranche_done,
   .done = default_dmabuf_feedback_done,
};

static void
shm_handle_format(void *data, struct wl_shm *shm, uint32_t format)
{
   struct wsi_wl_display *display = data;

   wsi_wl_display_add_wl_shm_format(display, &display->formats, format);
}

static const struct wl_shm_listener shm_listener = {
   .format = shm_handle_format
};

static bool
vector_contains(const struct u_vector *vec, unsigned int val)
{
   unsigned int *ptr;

   if (vec == NULL)
      return false;

   u_vector_foreach(ptr, (struct u_vector *)(void *)vec) {
      if (*ptr == val)
         return true;
   }

   return false;
}

struct Colorspace {
   VkColorSpaceKHR colorspace;
   enum wp_color_manager_v1_primaries primaries;
   enum wp_color_manager_v1_transfer_function tf;
   bool should_use_hdr_metadata;
   bool needs_extended_range;
};

static const struct Colorspace colorspace_mapping[] = {
   {
      .colorspace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_SRGB,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_SRGB,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_DISPLAY_P3,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_SRGB,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_SRGB,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_EXT_LINEAR,
      .should_use_hdr_metadata = true,
      .needs_extended_range = true,
   },
   {
      .colorspace = VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_DISPLAY_P3,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_EXT_LINEAR,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_BT709_LINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_SRGB,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_EXT_LINEAR,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_BT709_NONLINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_SRGB,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_BT1886,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_BT2020_LINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_BT2020,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_EXT_LINEAR,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_HDR10_ST2084_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_BT2020,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_ST2084_PQ,
      .should_use_hdr_metadata = true,
      .needs_extended_range = false,
   },
   /* VK_COLOR_SPACE_DOLBYVISION_EXT is left out because it's deprecated */
   {
      .colorspace = VK_COLOR_SPACE_HDR10_HLG_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_BT2020,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_HLG,
      .should_use_hdr_metadata = true,
      .needs_extended_range = false,
   },
   {
      .colorspace = VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT,
      .primaries = WP_COLOR_MANAGER_V1_PRIMARIES_ADOBE_RGB,
      .tf = WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_EXT_LINEAR,
      .should_use_hdr_metadata = false,
      .needs_extended_range = false,
   },
   /* VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT is left out because there's no
    * exactly matching transfer function in the Wayland protocol */
   /* VK_COLOR_SPACE_PASS_THROUGH_EXT is handled elsewhere */
   /* VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT is intentionally not added
    * as it's a bit unclear how exactly it should be used
    * and whether or not the transfer function should be gamma 2.2 or piece-wise */
   /* VK_COLOR_SPACE_DISPLAY_NATIVE_AMD isn't supported */
   /* VK_COLORSPACE_SRGB_NONLINEAR_KHR is just an alias */
   /* VK_COLOR_SPACE_DCI_P3_LINEAR_EXT is just an alias */
};

static int
wsi_wl_display_determine_colorspaces(struct wsi_wl_display *display)
{
   u_vector_finish(&display->colorspaces);
   if (!u_vector_init(&display->colorspaces, 8, sizeof(VkColorSpaceKHR)))
      return -1;

   /* SRGB_NONLINEAR is always supported. */
   VkColorSpaceKHR *new_cs = u_vector_add(&display->colorspaces);
   if (!new_cs)
      return -1;
   *new_cs = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;

   /* PASS_THROUGH is always supported. */
   new_cs = u_vector_add(&display->colorspaces);
   if (!new_cs)
      return -1;
   *new_cs = VK_COLOR_SPACE_PASS_THROUGH_EXT;

   if (!display->color_manager)
      return 0;

   const struct u_vector *tfs = &display->color_transfer_funcs;
   const struct u_vector *primaries = &display->color_primaries;

   /* Skip SRGB_NONLINEAR (i = 0), which has already been added above. */
   assert(colorspace_mapping[0].colorspace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR);

   for (size_t i = 1; i < ARRAY_SIZE(colorspace_mapping); i++) {
      if (!vector_contains(primaries, (unsigned int)colorspace_mapping[i].primaries))
         continue;
      if (!vector_contains(tfs, (unsigned int)colorspace_mapping[i].tf))
         continue;
      if (!display->color_features.extended_target_volume && colorspace_mapping[i].needs_extended_range)
         continue;

      new_cs = u_vector_add(&display->colorspaces);
      if (!new_cs)
         return -1;

      *new_cs = colorspace_mapping[i].colorspace;
   }

   return 0;
}

static void
color_management_handle_supported_intent(void *data,
                                         struct wp_color_manager_v1 *color_manager,
                                         unsigned int intent)
{
   /* We only use the perceptual rendering intent, which is always supported. */
}

static void
color_management_handle_supported_features(void *data,
                                           struct wp_color_manager_v1 *color_manager,
                                           unsigned int feature)
{
   struct wsi_wl_display *display = data;
   switch (feature) {
   case WP_COLOR_MANAGER_V1_FEATURE_SET_MASTERING_DISPLAY_PRIMARIES:
      display->color_features.mastering_display_primaries = true;
      break;
   case WP_COLOR_MANAGER_V1_FEATURE_EXTENDED_TARGET_VOLUME:
      display->color_features.extended_target_volume = true;
      break;
   default:
      break;
   }
}

static void
color_management_handle_supported_tf_named(void *data,
                                           struct wp_color_manager_v1 *color_manager,
                                           unsigned int tf)
{
   struct wsi_wl_display *display = data;
   unsigned int *new_tf = u_vector_add(&display->color_transfer_funcs);
   if (new_tf)
      *new_tf = tf;
}

static void
color_management_handle_supported_primaries_named(void *data,
                                                  struct wp_color_manager_v1 *color_manager,
                                                  unsigned int primaries)
{
   struct wsi_wl_display *display = data;
   unsigned int *new_primaries = u_vector_add(&display->color_primaries);
   if (new_primaries)
      *new_primaries = primaries;
}

static void
color_management_handle_done(void *data, struct wp_color_manager_v1 *color_manager)
{
   /* Intentionally left blank */
}

static const struct wp_color_manager_v1_listener color_manager_listener = {
   .supported_intent = color_management_handle_supported_intent,
   .supported_feature = color_management_handle_supported_features,
   .supported_tf_named = color_management_handle_supported_tf_named,
   .supported_primaries_named = color_management_handle_supported_primaries_named,
   .done = color_management_handle_done,
};

enum image_description_status {
   undefined,
   ready,
   failed,
};

static void
color_management_handle_image_desc_failed(void *data,
                                          struct wp_image_description_v1 *desc,
                                          unsigned int cause,
                                          const char *msg)
{
   enum image_description_status *status = data;
   *status = failed;
}

static void
color_management_handle_image_desc_ready(void *data,
                                        struct wp_image_description_v1 *desc,
                                        unsigned int id)
{
   enum image_description_status *status = data;
   *status = ready;
}

static const struct wp_image_description_v1_listener image_description_listener = {
   .failed = color_management_handle_image_desc_failed,
   .ready = color_management_handle_image_desc_ready,
};

static bool
needs_color_surface(struct wsi_wl_display *display, VkColorSpaceKHR colorspace)
{
   if (colorspace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
      /* we want to use a color surface to set sRGB if possible, but
       * only if the compositor actually supports sRGB */
      return vector_contains(&display->color_primaries, WP_COLOR_MANAGER_V1_PRIMARIES_SRGB)
          && vector_contains(&display->color_transfer_funcs, WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_SRGB);
   }
   return colorspace != VK_COLOR_SPACE_PASS_THROUGH_EXT;
}

static bool
wsi_wl_surface_add_color_refcount(struct wsi_wl_surface *wsi_surface)
{
   assert(wsi_surface != NULL);

   wsi_surface->color.color_surface_refcount++;
   if (wsi_surface->color.color_surface_refcount == 1) {
      wsi_surface->color.color_surface =
         wp_color_manager_v1_get_surface(wsi_surface->display->color_manager, wsi_surface->wayland_surface.wrapper);
      if (wsi_surface->color.color_surface == NULL) {
         wsi_surface->color.color_surface_refcount--;
         return false;
      }
   }

   return true;
}

static void
wsi_wl_surface_remove_color_refcount(struct wsi_wl_surface *wsi_surface)
{
   assert(wsi_surface != NULL);
   assert(wsi_surface->color.color_surface_refcount > 0);

   if (wsi_surface->color.color_surface_refcount <= 0)
      return;

   wsi_surface->color.color_surface_refcount--;
   if (wsi_surface->color.color_surface_refcount == 0) {
      wp_color_management_surface_v1_destroy(wsi_surface->color.color_surface);
      wsi_surface->color.color_surface = NULL;
   }
}

struct wayland_hdr_metadata {
   uint32_t min_luminance;
   uint32_t max_luminance;
   uint32_t max_fall;
   uint32_t max_cll;
};

#define MIN_LUM_FACTOR 10000

static bool
is_hdr_metadata_legal(const struct wayland_hdr_metadata *l)
{
   if (l == NULL)
      return false;

   if (l->max_cll != 0) {
      if ((uint64_t)l->max_cll * (uint64_t)MIN_LUM_FACTOR < (uint64_t)l->min_luminance)
         return false;
      if (l->max_luminance != 0 && l->max_cll > l->max_luminance)
         return false;
   }

   if (l->max_fall != 0) {
      if ((uint64_t)l->max_fall * (uint64_t)MIN_LUM_FACTOR < (uint64_t)l->min_luminance)
         return false;
      if (l->max_luminance != 0 && l->max_fall > l->max_luminance)
         return false;
      if (l->max_cll != 0 && l->max_fall > l->max_cll)
         return false;
   }

   /* Be lenient here for a zero (=undefined) max_luminance and handle
    * this in the calling code instead, by not sending min/max mastering
    * luminance data to Wayland, thereby avoiding protocol errors.
    */
   if (l->max_luminance == 0)
      return true;

   return (uint64_t)l->max_luminance * (uint64_t)MIN_LUM_FACTOR > (uint64_t)l->min_luminance;
}

static bool
compare_hdr_metadata(const VkHdrMetadataEXT *l, const VkHdrMetadataEXT *r)
{
   assert(l != NULL && r != NULL);

   return l->displayPrimaryRed.x == r->displayPrimaryRed.x &&
          l->displayPrimaryRed.y == r->displayPrimaryRed.y &&
          l->displayPrimaryGreen.x == r->displayPrimaryGreen.x &&
          l->displayPrimaryGreen.y == r->displayPrimaryGreen.y &&
          l->displayPrimaryBlue.x == r->displayPrimaryBlue.x &&
          l->displayPrimaryBlue.y == r->displayPrimaryBlue.y &&
          l->whitePoint.x == r->whitePoint.x &&
          l->whitePoint.y == r->whitePoint.y &&
          l->maxLuminance == r->maxLuminance &&
          l->minLuminance == r->minLuminance &&
          l->maxContentLightLevel == r->maxContentLightLevel &&
          l->maxFrameAverageLightLevel == r->maxFrameAverageLightLevel;
}

static const struct Colorspace *
wsi_wl_find_colorspace_mapping(VkColorSpaceKHR colorspace)
{
   for (size_t i = 0; i < ARRAY_SIZE(colorspace_mapping); i++) {
      if (colorspace_mapping[i].colorspace == colorspace)
         return &colorspace_mapping[i];
   }

   return NULL;
}

static struct wayland_hdr_metadata
wsi_wl_convert_hdr_metadata(const VkHdrMetadataEXT *hdr)
{
   struct wayland_hdr_metadata out = {
      .min_luminance = wsi_wl_round_to_u32_sat((double)MIN_LUM_FACTOR * (double)hdr->minLuminance),
      .max_luminance = wsi_wl_round_to_u32_sat((double)hdr->maxLuminance),
      .max_fall = wsi_wl_round_to_u32_sat((double)hdr->maxFrameAverageLightLevel),
      .max_cll = wsi_wl_round_to_u32_sat((double)hdr->maxContentLightLevel),
   };

   return out;
}

static VkResult
wsi_wl_swapchain_update_colorspace(struct wsi_wl_swapchain *chain)
{
   struct wsi_wl_surface *surface = chain->wsi_wl_surface;
   struct wsi_wl_display *display = surface->display;
   const VkColorSpaceKHR new_colorspace = chain->color.colorspace;
   const bool had_color_surface = surface->color.color_surface != NULL;
   const bool requested_hdr_metadata = chain->color.has_hdr_metadata;
   const bool needs_color_surface_new = needs_color_surface(display, new_colorspace);

   const struct Colorspace *mapping = wsi_wl_find_colorspace_mapping(new_colorspace);
   bool desired_use_hdr_metadata = requested_hdr_metadata;
   bool added_color_surface = false;
   bool retried_without_hdr = false;
   VkResult result = VK_SUCCESS;

   struct wayland_hdr_metadata wayland_hdr_metadata = {0};

   if (likely(surface->color.colorspace == new_colorspace &&
              !requested_hdr_metadata &&
              !surface->color.has_hdr_metadata)) {
      if (had_color_surface == needs_color_surface_new)
         return VK_SUCCESS;
   }

   if (requested_hdr_metadata) {
      wayland_hdr_metadata = wsi_wl_convert_hdr_metadata(&chain->color.hdr_metadata);

      desired_use_hdr_metadata = is_hdr_metadata_legal(&wayland_hdr_metadata);
      if (!desired_use_hdr_metadata)
         mesa_log_once(MESA_LOG_WARN, "Not using HDR metadata to avoid protocol errors");
   }

   if (mapping)
      desired_use_hdr_metadata &= mapping->should_use_hdr_metadata;

   if (!display->color_manager) {
      if (had_color_surface)
         wsi_wl_surface_remove_color_refcount(surface);

      if (new_colorspace != VK_COLOR_SPACE_SRGB_NONLINEAR_KHR &&
          new_colorspace != VK_COLOR_SPACE_PASS_THROUGH_EXT)
         return VK_ERROR_SURFACE_LOST_KHR;

      surface->color.colorspace = new_colorspace;
      surface->color.hdr_metadata = chain->color.hdr_metadata;
      surface->color.has_hdr_metadata = false;
      return VK_SUCCESS;
   }

   if (had_color_surface == needs_color_surface_new &&
       surface->color.colorspace == new_colorspace &&
       surface->color.has_hdr_metadata == desired_use_hdr_metadata &&
       (!desired_use_hdr_metadata ||
        compare_hdr_metadata(&surface->color.hdr_metadata, &chain->color.hdr_metadata))) {
      return VK_SUCCESS;
   }

   if (!needs_color_surface_new) {
      if (had_color_surface)
         wsi_wl_surface_remove_color_refcount(surface);

      surface->color.colorspace = new_colorspace;
      surface->color.hdr_metadata = chain->color.hdr_metadata;
      surface->color.has_hdr_metadata = false;
      return VK_SUCCESS;
   }

   if (!had_color_surface) {
      if (!wsi_wl_surface_add_color_refcount(surface))
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      added_color_surface = true;
   }

   bool try_hdr_metadata = desired_use_hdr_metadata;

   for (;;) {
      if (!mapping) {
         result = VK_ERROR_SURFACE_LOST_KHR;
         break;
      }

      struct wp_image_description_creator_params_v1 *creator =
         wp_color_manager_v1_create_parametric_creator(display->color_manager);
      if (unlikely(creator == NULL)) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         break;
      }

      wp_image_description_creator_params_v1_set_primaries_named(creator, (unsigned int)mapping->primaries);
      wp_image_description_creator_params_v1_set_tf_named(creator, (unsigned int)mapping->tf);

      if (try_hdr_metadata) {
         wp_image_description_creator_params_v1_set_max_cll(creator, wayland_hdr_metadata.max_cll);
         wp_image_description_creator_params_v1_set_max_fall(creator, wayland_hdr_metadata.max_fall);

         if (display->color_features.mastering_display_primaries) {
            const uint32_t red_x =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryRed.x * 1000000.0);
            const uint32_t red_y =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryRed.y * 1000000.0);
            const uint32_t green_x =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryGreen.x * 1000000.0);
            const uint32_t green_y =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryGreen.y * 1000000.0);
            const uint32_t blue_x =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryBlue.x * 1000000.0);
            const uint32_t blue_y =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.displayPrimaryBlue.y * 1000000.0);
            const uint32_t white_x =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.whitePoint.x * 1000000.0);
            const uint32_t white_y =
               wsi_wl_round_to_u32_sat((double)chain->color.hdr_metadata.whitePoint.y * 1000000.0);

            wp_image_description_creator_params_v1_set_mastering_display_primaries(
               creator, red_x, red_y, green_x, green_y, blue_x, blue_y, white_x, white_y);

            if (wayland_hdr_metadata.max_luminance != 0) {
               wp_image_description_creator_params_v1_set_mastering_luminance(
                  creator, wayland_hdr_metadata.min_luminance, wayland_hdr_metadata.max_luminance);
            }
         }
      }

      wl_proxy_set_queue((struct wl_proxy *)creator, display->queue);

      struct wp_image_description_v1 *image_desc =
         wp_image_description_creator_params_v1_create(creator);
      wp_image_description_creator_params_v1_destroy(creator);

      if (unlikely(image_desc == NULL)) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         break;
      }

      wl_proxy_set_queue((struct wl_proxy *)image_desc, display->queue);

      enum image_description_status status = undefined;
      wp_image_description_v1_add_listener(image_desc, &image_description_listener, &status);

      while (status == undefined) {
         const int ret = wl_display_dispatch_queue(display->wl_display, display->queue);
         if (ret < 0) {
            wp_image_description_v1_destroy(image_desc);
            result = VK_ERROR_OUT_OF_DATE_KHR;
            goto fail;
         }
      }

      if (status == failed) {
         wp_image_description_v1_destroy(image_desc);

         if (!display->color_features.extended_target_volume && try_hdr_metadata) {
            try_hdr_metadata = false;
            retried_without_hdr = true;
            continue;
         }

         result = VK_ERROR_SURFACE_LOST_KHR;
         break;
      }

      wp_color_management_surface_v1_set_image_description(
         surface->color.color_surface, image_desc,
         WP_COLOR_MANAGER_V1_RENDER_INTENT_PERCEPTUAL);
      wp_image_description_v1_destroy(image_desc);

      surface->color.colorspace = new_colorspace;
      surface->color.hdr_metadata = chain->color.hdr_metadata;
      surface->color.has_hdr_metadata = try_hdr_metadata;

      if (retried_without_hdr)
         chain->color.has_hdr_metadata = false;

      return VK_SUCCESS;
   }

fail:
   if (added_color_surface)
      wsi_wl_surface_remove_color_refcount(surface);

   return result;
}

static void
wsi_wl_swapchain_set_hdr_metadata(struct wsi_swapchain *wsi_chain, const VkHdrMetadataEXT *pMetadata)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;

   if (pMetadata == NULL)
      return;

   chain->color.hdr_metadata = *pMetadata;
   chain->color.has_hdr_metadata = true;
}

static void
presentation_handle_clock_id(void *data, struct wp_presentation *wp_presentation, uint32_t clk_id)
{
   struct wsi_wl_display *display = data;
   display->presentation_clock_id = (clockid_t)clk_id;
}

static const struct wp_presentation_listener presentation_listener = {
   presentation_handle_clock_id,
};

static void
registry_handle_global(void *data, struct wl_registry *registry, uint32_t name, const char *interface,
                       uint32_t version)
{
   struct wsi_wl_display *display = data;

   if (display->sw) {
      if (strcmp(interface, wl_shm_interface.name) == 0) {
         display->wl_shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
         if (display->wl_shm)
            wl_shm_add_listener(display->wl_shm, &shm_listener, display);
      }
   } else {
      if (strcmp(interface, zwp_linux_dmabuf_v1_interface.name) == 0 && version >= 3) {
         display->wl_dmabuf =
            wl_registry_bind(registry, name, &zwp_linux_dmabuf_v1_interface,
                             MIN2(version, ZWP_LINUX_DMABUF_V1_GET_DEFAULT_FEEDBACK_SINCE_VERSION));
         if (display->wl_dmabuf)
            zwp_linux_dmabuf_v1_add_listener(display->wl_dmabuf, &dmabuf_listener, display);
      } else if (strcmp(interface, wp_linux_drm_syncobj_manager_v1_interface.name) == 0) {
         display->wl_syncobj = wl_registry_bind(registry, name, &wp_linux_drm_syncobj_manager_v1_interface, 1);
#if WSI_WL_HAS_FIXES_PROTOCOL
      } else if (strcmp(interface, wl_fixes_interface.name) == 0) {
         display->wl_fixes = wl_registry_bind(registry, name, &wl_fixes_interface, 1);
#endif
      }
   }

   if (strcmp(interface, wp_presentation_interface.name) == 0) {
      display->wp_presentation_version = version > 1 ? 2u : 1u;

      display->wp_presentation_notwrapped =
         wl_registry_bind(registry, name, &wp_presentation_interface, display->wp_presentation_version);
      if (display->wp_presentation_notwrapped)
         wp_presentation_add_listener(display->wp_presentation_notwrapped, &presentation_listener, display);
   } else if (strcmp(interface, wp_tearing_control_manager_v1_interface.name) == 0) {
      display->tearing_control_manager =
         wl_registry_bind(registry, name, &wp_tearing_control_manager_v1_interface, 1);
      display->has_tearing = display->tearing_control_manager != NULL;
   } else if (strcmp(interface, wp_fifo_manager_v1_interface.name) == 0) {
      display->fifo_manager = wl_registry_bind(registry, name, &wp_fifo_manager_v1_interface, 1);
   } else if (!display->no_timestamps &&
              strcmp(interface, wp_commit_timing_manager_v1_interface.name) == 0) {
      display->commit_timing_manager =
         wl_registry_bind(registry, name, &wp_commit_timing_manager_v1_interface, 1);
   }

   if (strcmp(interface, wp_color_manager_v1_interface.name) == 0 && display->color_manager == NULL) {
      struct wp_color_manager_v1 *color_manager =
         wl_registry_bind(registry, name, &wp_color_manager_v1_interface, 1);
      if (!color_manager)
         return;

      struct u_vector primaries = {0};
      struct u_vector transfer_funcs = {0};

      if (!u_vector_init(&primaries, 8, sizeof(uint32_t)) ||
          !u_vector_init(&transfer_funcs, 8, sizeof(uint32_t))) {
         u_vector_finish(&primaries);
         u_vector_finish(&transfer_funcs);
         wp_color_manager_v1_destroy(color_manager);
         return;
      }

      display->color_manager = color_manager;
      display->color_primaries = primaries;
      display->color_transfer_funcs = transfer_funcs;

      wp_color_manager_v1_add_listener(display->color_manager, &color_manager_listener, display);
   }
}

static void
registry_handle_global_remove(void *data, struct wl_registry *registry,
                              uint32_t name)
{ /* No-op */ }

static const struct wl_registry_listener registry_listener = {
   registry_handle_global,
   registry_handle_global_remove
};

static void
wsi_wl_display_finish_objects(struct wsi_wl_display *display)
{
   if (display->wl_shm) {
      wl_shm_destroy(display->wl_shm);
      display->wl_shm = NULL;
   }

   if (display->wl_syncobj) {
      wp_linux_drm_syncobj_manager_v1_destroy(display->wl_syncobj);
      display->wl_syncobj = NULL;
   }

   if (display->wl_dmabuf) {
      zwp_linux_dmabuf_v1_destroy(display->wl_dmabuf);
      display->wl_dmabuf = NULL;
   }

   if (display->wp_presentation_notwrapped) {
      wp_presentation_destroy(display->wp_presentation_notwrapped);
      display->wp_presentation_notwrapped = NULL;
   }

   if (display->fifo_manager) {
      wp_fifo_manager_v1_destroy(display->fifo_manager);
      display->fifo_manager = NULL;
   }

   if (display->commit_timing_manager) {
      wp_commit_timing_manager_v1_destroy(display->commit_timing_manager);
      display->commit_timing_manager = NULL;
   }

   if (display->tearing_control_manager) {
      wp_tearing_control_manager_v1_destroy(display->tearing_control_manager);
      display->tearing_control_manager = NULL;
   }

   if (display->color_manager) {
      wp_color_manager_v1_destroy(display->color_manager);
      display->color_manager = NULL;
   }

#if WSI_WL_HAS_FIXES_PROTOCOL
   if (display->wl_fixes) {
      wl_fixes_destroy(display->wl_fixes);
      display->wl_fixes = NULL;
   }
#endif

   if (display->wl_display_wrapper) {
      wl_proxy_wrapper_destroy(display->wl_display_wrapper);
      display->wl_display_wrapper = NULL;
   }

   if (display->queue) {
      wl_event_queue_destroy(display->queue);
      display->queue = NULL;
   }
}

static void
wsi_wl_display_finish(struct wsi_wl_display *display)
{
   struct wsi_wl_format *f;

   u_vector_foreach(f, &display->formats)
      u_vector_finish(&f->modifiers);

   u_vector_finish(&display->formats);
   u_vector_finish(&display->colorspaces);
   u_vector_finish(&display->color_primaries);
   u_vector_finish(&display->color_transfer_funcs);

   wsi_wl_display_finish_feedback(display);
   wsi_wl_display_finish_objects(display);
}

static VkResult
wsi_wl_display_init(struct wsi_wayland *wsi_wl, struct wsi_wl_display *display, struct wl_display *wl_display,
                    bool get_format_list, bool sw, const char *queue_name)
{
   VkResult result = VK_SUCCESS;
   struct wl_registry *registry = NULL;

   memset(display, 0, sizeof(*display));

   if (!u_vector_init(&display->formats, 8, sizeof(struct wsi_wl_format)))
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   display->presentation_clock_id = (clockid_t)-1; /* 0 is a valid clock ID */
   display->wsi_wl = wsi_wl;
   display->wl_display = wl_display;
   display->sw = sw;

   display->queue = wl_display_create_queue_with_name(wl_display, queue_name);
   if (!display->queue) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   display->wl_display_wrapper = wl_proxy_create_wrapper(wl_display);
   if (!display->wl_display_wrapper) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   display->no_timestamps = wsi_wl->wsi->wayland.disable_timestamps;

   wl_proxy_set_queue((struct wl_proxy *)display->wl_display_wrapper, display->queue);

   registry = wl_display_get_registry(display->wl_display_wrapper);
   if (!registry) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   wl_registry_add_listener(registry, &registry_listener, display);

   /* Round-trip to get wl_shm and zwp_linux_dmabuf_v1 globals */
   result = wsi_wl_roundtrip_queue(display);
   if (result != VK_SUCCESS)
      goto fail_registry;

   if (!display->wl_dmabuf && !display->wl_shm) {
      result = VK_ERROR_SURFACE_LOST_KHR;
      goto fail_registry;
   }

   /* Caller doesn't expect us to query formats/modifiers, so return */
   if (!get_format_list)
      goto out;

   /* Default assumption */
   display->same_gpu = true;

   /* Get the default dma-buf feedback */
   if (display->wl_dmabuf &&
       zwp_linux_dmabuf_v1_get_version(display->wl_dmabuf) >=
          ZWP_LINUX_DMABUF_V1_GET_DEFAULT_FEEDBACK_SINCE_VERSION) {
      dmabuf_feedback_format_table_init(&display->format_table);

      display->wl_dmabuf_feedback = zwp_linux_dmabuf_v1_get_default_feedback(display->wl_dmabuf);
      if (!display->wl_dmabuf_feedback) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_registry;
      }

      zwp_linux_dmabuf_feedback_v1_add_listener(display->wl_dmabuf_feedback, &dmabuf_feedback_listener, display);

      /* Round-trip again to fetch dma-buf feedback */
      result = wsi_wl_roundtrip_queue(display);
      if (result != VK_SUCCESS)
         goto fail_registry;

      if (wsi_wl->wsi->drm_info.hasRender || wsi_wl->wsi->drm_info.hasPrimary) {
         /* Apparently some wayland compositors do not send the render
          * device node but the primary, so test against both.
          */
         display->same_gpu =
            (wsi_wl->wsi->drm_info.hasRender &&
             major(display->main_device) == wsi_wl->wsi->drm_info.renderMajor &&
             minor(display->main_device) == wsi_wl->wsi->drm_info.renderMinor) ||
            (wsi_wl->wsi->drm_info.hasPrimary &&
             major(display->main_device) == wsi_wl->wsi->drm_info.primaryMajor &&
             minor(display->main_device) == wsi_wl->wsi->drm_info.primaryMinor);
      }
   }

   /* Round-trip again to get formats, modifiers and color properties */
   result = wsi_wl_roundtrip_queue(display);
   if (result != VK_SUCCESS)
      goto fail_registry;

   if (wsi_wl_display_determine_colorspaces(display) < 0) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail_registry;
   }

   if (wsi_wl->wsi->force_bgra8_unorm_first && u_vector_length(&display->formats) > 0) {
      /* Find BGRA8_UNORM in the list and swap it to the first position if we
       * can find it. Some apps get confused if SRGB is first in the list.
       */
      struct wsi_wl_format *first_fmt = u_vector_tail(&display->formats);
      struct wsi_wl_format *f = find_format(&display->formats, VK_FORMAT_B8G8R8A8_UNORM);

      if (f) {
         const struct wsi_wl_format tmp_fmt = *f;
         *f = *first_fmt;
         *first_fmt = tmp_fmt;
      }
   }

out:
   /* We don't need this anymore */
   wsi_wl_registry_destroy(display, &registry);

   /* Destroy default dma-buf feedback object and format table */
   wsi_wl_display_finish_feedback(display);

   return VK_SUCCESS;

fail_registry:
   wsi_wl_registry_destroy(display, &registry);

fail:
   wsi_wl_display_finish(display);
   return result;
}

static VkResult
wsi_wl_display_create(struct wsi_wayland *wsi, struct wl_display *wl_display,
                      bool sw,
                      struct wsi_wl_display **display_out)
{
   struct wsi_wl_display *display =
      vk_alloc(wsi->alloc, sizeof(*display), 8,
               VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   if (!display)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   VkResult result = wsi_wl_display_init(wsi, display, wl_display, true,
                                         sw, "mesa vk display queue");
   if (result != VK_SUCCESS) {
      vk_free(wsi->alloc, display);
      return result;
   }

   *display_out = display;

   return result;
}

static void
wsi_wl_display_destroy(struct wsi_wl_display *display)
{
   struct wsi_wayland *wsi = display->wsi_wl;
   wsi_wl_display_finish(display);
   vk_free(wsi->alloc, display);
}

VKAPI_ATTR VkBool32 VKAPI_CALL
wsi_GetPhysicalDeviceWaylandPresentationSupportKHR(VkPhysicalDevice physicalDevice,
                                                   uint32_t queueFamilyIndex,
                                                   struct wl_display *wl_display)
{
   VK_FROM_HANDLE(vk_physical_device, pdevice, physicalDevice);
   struct wsi_device *wsi_device = pdevice->wsi_device;
   struct wsi_wayland *wsi =
      (struct wsi_wayland *)wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];

   /* These should overlap. */
   uint64_t effective_queues = wsi_device->queue_supports_blit & wsi_device->queue_supports_timestamps;

   /* If there are no queues that support both blits and timestamps,
    * don't report support for queue timestamps. */
   if (!effective_queues)
      effective_queues = wsi_device->queue_supports_blit;

   if (!(effective_queues & BITFIELD64_BIT(queueFamilyIndex)))
      return false;

   struct wsi_wl_display display;
   VkResult ret = wsi_wl_display_init(wsi, &display, wl_display, false,
                                      wsi_device->sw, "mesa presentation support query");
   if (ret == VK_SUCCESS)
      wsi_wl_display_finish(&display);

   return ret == VK_SUCCESS;
}

static VkResult
wsi_wl_surface_get_support(VkIcdSurfaceBase *surface,
                           struct wsi_device *wsi_device,
                           uint32_t queueFamilyIndex,
                           VkBool32* pSupported)
{
   *pSupported = true;

   return VK_SUCCESS;
}

/* For true mailbox mode, we need at least 4 images:
 *  1) One to scan out from
 *  2) One to have queued for scan-out
 *  3) One to be currently held by the Wayland compositor
 *  4) One to render to
 */
#define WSI_WL_BUMPED_NUM_IMAGES 4

/* Catch-all. 3 images is a sound default for everything except MAILBOX. */
#define WSI_WL_DEFAULT_NUM_IMAGES 3

static uint32_t
wsi_wl_surface_get_min_image_count(struct wsi_wl_display *display,
                                   const VkSurfacePresentModeKHR *present_mode)
{
   if (present_mode) {
      return present_mode->presentMode == VK_PRESENT_MODE_MAILBOX_KHR ?
             WSI_WL_BUMPED_NUM_IMAGES : WSI_WL_DEFAULT_NUM_IMAGES;
   }

   /* If explicit present_mode is not being queried, we need to provide a safe "catch-all"
    * which can work for any presentation mode. Implementations are allowed to bump the minImageCount
    * on swapchain creation, so this limit should be the lowest value which can guarantee forward progress. */

   /* When FIFO protocol is not supported, we always returned 4 here,
    * despite it going against the spirit of minImageCount in the specification.
    * To avoid any unforeseen breakage, just keep using the same values we always have.
    * In this path, we also never consider bumping the image count in minImageCount in swapchain creation time. */

   /* When FIFO protocol is supported, applications will no longer block
    * in QueuePresentKHR due to frame callback, so returning 4 images
    * for a FIFO swapchain is deeply problematic due to excessive latency.
    * This latency can only be limited through means of presentWait which few applications use, and we cannot
    * mandate that shipping applications are rewritten to avoid a regression.
    * 2 images are enough for forward progress in FIFO, but 3 is used here as a pragmatic decision
    * because 2 could result in waiting for the compositor to remove an
    * old image from scanout when we'd like to be rendering,
    * and we don't want naively written applications to head into poor performance territory by default.
    * X11 backend has very similar logic and rationale here.
    */
   return display->fifo_manager ? WSI_WL_DEFAULT_NUM_IMAGES : WSI_WL_BUMPED_NUM_IMAGES;
}

static VkResult
wsi_wl_surface_get_capabilities(VkIcdSurfaceBase *icd_surface,
                                struct wsi_device *wsi_device,
                                const VkSurfacePresentModeKHR *present_mode,
                                VkSurfaceCapabilitiesKHR *caps)
{
   VkIcdSurfaceWayland *surface = (VkIcdSurfaceWayland *)icd_surface;
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   struct wsi_wayland *wsi =
      (struct wsi_wayland *)wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];
   struct wsi_wl_display temp_display;
   struct wsi_wl_display *display = wsi_wl_surface->display;
   VkResult result = VK_SUCCESS;

   if (!wsi_wl_surface->display) {
      /* Capabilities only require protocol globals (e.g. fifo_manager), not full format probing. */
      result = wsi_wl_display_init(wsi, &temp_display, surface->display, false,
                                   wsi_device->sw, "mesa image count query");
      if (result != VK_SUCCESS)
         return result;
      display = &temp_display;
   }

   caps->minImageCount = wsi_wl_surface_get_min_image_count(display, present_mode);

   if (!wsi_wl_surface->display)
      wsi_wl_display_finish(&temp_display);

   caps->maxImageCount = 0;
   caps->currentExtent = (VkExtent2D){UINT32_MAX, UINT32_MAX};
   caps->minImageExtent = (VkExtent2D){1, 1};
   caps->maxImageExtent = (VkExtent2D){
      wsi_device->maxImageDimension2D,
      wsi_device->maxImageDimension2D,
   };

   caps->supportedTransforms = VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
   caps->currentTransform = VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
   caps->maxImageArrayLayers = 1;

   caps->supportedCompositeAlpha =
      VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR |
      VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR;

   caps->supportedUsageFlags = wsi_caps_get_image_usage();

   VK_FROM_HANDLE(vk_physical_device, pdevice, wsi_device->pdevice);
   if (pdevice->supported_extensions.EXT_attachment_feedback_loop_layout)
      caps->supportedUsageFlags |= VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT;

   return VK_SUCCESS;
}

static VkResult
wsi_wl_surface_check_presentation(VkIcdSurfaceBase *icd_surface,
                                  struct wsi_device *wsi_device,
                                  bool *has_wp_presentation, clockid_t *clock_id,
                                  bool *has_commit_timing, bool *has_fifo)
{
   VkIcdSurfaceWayland *surface = (VkIcdSurfaceWayland *)icd_surface;
   struct wsi_wayland *wsi =
      (struct wsi_wayland *)wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];
   struct wsi_wl_display display;
   VkResult result;

   /* We only need global objects/capabilities here, not format/modifier lists. */
   result = wsi_wl_display_init(wsi, &display, surface->display, false,
                                wsi_device->sw, "mesa check wp_presentation");
   if (result != VK_SUCCESS)
      return result;

   if (has_wp_presentation)
      *has_wp_presentation = !!display.wp_presentation_notwrapped;

   if (clock_id)
      *clock_id = display.presentation_clock_id;

   if (has_commit_timing)
      *has_commit_timing = !!display.commit_timing_manager;

   if (has_fifo)
      *has_fifo = !!display.fifo_manager;

   wsi_wl_display_finish(&display);
   return VK_SUCCESS;
}

static void
wsi_wl_surface_invalidate_query_cache(struct wsi_wl_surface *wsi_wl_surface)
{
   const int err = mtx_lock(&wsi_wl_surface->query_cache.lock);
   if (err != thrd_success)
      return;

   if (wsi_wl_surface->query_cache.last_device != NULL) {
      wsi_wl_display_finish(&wsi_wl_surface->query_cache.cached_display);
      wsi_wl_surface->query_cache.last_device = NULL;
   }

   mtx_unlock(&wsi_wl_surface->query_cache.lock);
}

static VkResult
wsi_wl_surface_get_query_display(struct wsi_wl_surface *wsi_wl_surface,
                                 struct wsi_device *wsi_device,
                                 struct wsi_wl_display **out_display)
{
   struct wsi_wayland *wsi;
   VkResult result;
   int err;

   assert(wsi_wl_surface != NULL);
   assert(wsi_device != NULL);
   assert(out_display != NULL);

   *out_display = NULL;

   wsi = (struct wsi_wayland *)
      wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];
   if (wsi == NULL)
      return VK_ERROR_SURFACE_LOST_KHR;

   err = mtx_lock(&wsi_wl_surface->query_cache.lock);
   if (err != thrd_success)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   if (wsi_wl_surface->query_cache.last_device == wsi_device) {
      *out_display = &wsi_wl_surface->query_cache.cached_display;
      return VK_SUCCESS;
   }

   if (wsi_wl_surface->query_cache.last_device != NULL) {
      wsi_wl_display_finish(&wsi_wl_surface->query_cache.cached_display);
      wsi_wl_surface->query_cache.last_device = NULL;
   }

   result = wsi_wl_display_init(wsi,
                                &wsi_wl_surface->query_cache.cached_display,
                                wsi_wl_surface->base.display,
                                true,
                                wsi_device->sw,
                                "mesa display query cache");
   if (result != VK_SUCCESS) {
      mtx_unlock(&wsi_wl_surface->query_cache.lock);
      return result;
   }

   /* Keep only cached query results/state. Transient protocol objects are
    * dropped so repeated format/present-mode queries avoid registry churn.
    */
   wsi_wl_display_finish_objects(&wsi_wl_surface->query_cache.cached_display);

   wsi_wl_surface->query_cache.last_device = wsi_device;
   *out_display = &wsi_wl_surface->query_cache.cached_display;
   return VK_SUCCESS;
}

static void
wsi_wl_surface_release_query_display(struct wsi_wl_surface *wsi_wl_surface)
{
   assert(wsi_wl_surface != NULL);
   (void)mtx_unlock(&wsi_wl_surface->query_cache.lock);
}

static VkResult
wsi_wl_surface_get_capabilities2(VkIcdSurfaceBase *surface,
                                 struct wsi_device *wsi_device,
                                 const void *info_next,
                                 VkSurfaceCapabilities2KHR *caps)
{
   assert(caps->sType == VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR);

   const VkSurfacePresentModeKHR *present_mode =
      vk_find_struct_const(info_next, SURFACE_PRESENT_MODE_KHR);

   VkResult result =
      wsi_wl_surface_get_capabilities(surface, wsi_device, present_mode,
                                      &caps->surfaceCapabilities);
   if (result != VK_SUCCESS)
      return result;

   bool presentation_queried = false;
   bool has_feedback = false;
   bool has_commit_timing = false;
   bool has_fifo = false;
   clockid_t clock_id = (clockid_t)-1;

   vk_foreach_struct(ext, caps->pNext) {
      switch (ext->sType) {
      case VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR: {
         VkSurfaceProtectedCapabilitiesKHR *protected = (void *)ext;
         protected->supportsProtected =
            wsi_device->supports_protected[VK_ICD_WSI_PLATFORM_WAYLAND];
         break;
      }

      case VK_STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR: {
         VkSurfacePresentScalingCapabilitiesKHR *scaling = (void *)ext;
         scaling->supportedPresentScaling = 0;
         scaling->supportedPresentGravityX = 0;
         scaling->supportedPresentGravityY = 0;
         scaling->minScaledImageExtent = caps->surfaceCapabilities.minImageExtent;
         scaling->maxScaledImageExtent = caps->surfaceCapabilities.maxImageExtent;
         break;
      }

      case VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR: {
         VkSurfacePresentModeCompatibilityKHR *compat = (void *)ext;
         if (compat->pPresentModes) {
            assert(present_mode);
            VK_OUTARRAY_MAKE_TYPED(VkPresentModeKHR, modes, compat->pPresentModes, &compat->presentModeCount);

            vk_outarray_append_typed(VkPresentModeKHR, &modes, mode) {
               *mode = present_mode->presentMode;
            }

            switch (present_mode->presentMode) {
            case VK_PRESENT_MODE_MAILBOX_KHR:
               vk_outarray_append_typed(VkPresentModeKHR, &modes, mode) {
                  *mode = VK_PRESENT_MODE_FIFO_KHR;
               }
               break;
            case VK_PRESENT_MODE_FIFO_KHR:
               vk_outarray_append_typed(VkPresentModeKHR, &modes, mode) {
                  *mode = VK_PRESENT_MODE_MAILBOX_KHR;
               }
               break;
            default:
               break;
            }
         } else {
            if (!present_mode) {
               wsi_common_vk_warn_once("Use of VkSurfacePresentModeCompatibilityKHR "
                                       "without a VkSurfacePresentModeKHR set. This is an "
                                       "application bug.\n");
               compat->presentModeCount = 1;
            } else {
               switch (present_mode->presentMode) {
               case VK_PRESENT_MODE_MAILBOX_KHR:
               case VK_PRESENT_MODE_FIFO_KHR:
                  compat->presentModeCount = 2;
                  break;
               default:
                  compat->presentModeCount = 1;
                  break;
               }
            }
         }
         break;
      }

      case VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR: {
         VkSurfaceCapabilitiesPresentId2KHR *pid2 = (void *)ext;

         if (!presentation_queried) {
            result = wsi_wl_surface_check_presentation(surface, wsi_device,
                                                       &has_feedback, &clock_id,
                                                       &has_commit_timing, &has_fifo);
            if (result != VK_SUCCESS)
               return result;
            presentation_queried = true;
         }

         pid2->presentId2Supported = has_feedback;
         break;
      }

      case VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR: {
         VkSurfaceCapabilitiesPresentWait2KHR *pwait2 = (void *)ext;

         if (!presentation_queried) {
            result = wsi_wl_surface_check_presentation(surface, wsi_device,
                                                       &has_feedback, &clock_id,
                                                       &has_commit_timing, &has_fifo);
            if (result != VK_SUCCESS)
               return result;
            presentation_queried = true;
         }

         pwait2->presentWait2Supported = has_feedback;
         break;
      }

      case VK_STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT: {
         VkPresentTimingSurfaceCapabilitiesEXT *wait = (void *)ext;

         wait->presentStageQueries = 0;
         wait->presentTimingSupported = VK_FALSE;
         wait->presentAtAbsoluteTimeSupported = VK_FALSE;
         wait->presentAtRelativeTimeSupported = VK_FALSE;

         if (!presentation_queried) {
            result = wsi_wl_surface_check_presentation(surface, wsi_device,
                                                       &has_feedback, &clock_id,
                                                       &has_commit_timing, &has_fifo);
            if (result != VK_SUCCESS)
               return result;
            presentation_queried = true;
         }

         if (!has_feedback)
            break;

         if (clock_id != CLOCK_MONOTONIC && clock_id != CLOCK_MONOTONIC_RAW)
            break;

         wait->presentTimingSupported = VK_TRUE;
         wait->presentStageQueries = VK_PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT;
         wait->presentAtAbsoluteTimeSupported = has_commit_timing && has_fifo;
         break;
      }

      default:
         break;
      }
   }

   return VK_SUCCESS;
}

static VkResult
wsi_wl_surface_get_formats(VkIcdSurfaceBase *icd_surface,
                           struct wsi_device *wsi_device,
                           uint32_t *pSurfaceFormatCount,
                           VkSurfaceFormatKHR *pSurfaceFormats)
{
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   struct wsi_wl_display *display = wsi_wl_surface->display;
   bool release_query = false;
   VkResult result = VK_SUCCESS;

   if (display == NULL) {
      result = wsi_wl_surface_get_query_display(wsi_wl_surface, wsi_device, &display);
      if (result != VK_SUCCESS)
         return result;
      release_query = true;
   }

   VK_OUTARRAY_MAKE_TYPED(VkSurfaceFormatKHR, out,
                          pSurfaceFormats, pSurfaceFormatCount);

   VkColorSpaceKHR *cs;
   u_vector_foreach(cs, &display->colorspaces) {
      struct wsi_wl_format *disp_fmt;
      u_vector_foreach(disp_fmt, &display->formats) {
         if (!(disp_fmt->flags & WSI_WL_FMT_ALPHA) ||
             !(disp_fmt->flags & WSI_WL_FMT_OPAQUE))
            continue;

         vk_outarray_append_typed(VkSurfaceFormatKHR, &out, out_fmt) {
            out_fmt->format = disp_fmt->vk_format;
            out_fmt->colorSpace = *cs;
         }
      }
   }

   if (release_query)
      wsi_wl_surface_release_query_display(wsi_wl_surface);

   return vk_outarray_status(&out);
}

static VkResult
wsi_wl_surface_get_formats2(VkIcdSurfaceBase *icd_surface,
                            struct wsi_device *wsi_device,
                            const void *info_next,
                            uint32_t *pSurfaceFormatCount,
                            VkSurfaceFormat2KHR *pSurfaceFormats)
{
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   struct wsi_wl_display *display = wsi_wl_surface->display;
   bool release_query = false;
   VkResult result = VK_SUCCESS;

   (void)info_next;

   if (display == NULL) {
      result = wsi_wl_surface_get_query_display(wsi_wl_surface, wsi_device, &display);
      if (result != VK_SUCCESS)
         return result;
      release_query = true;
   }

   VK_OUTARRAY_MAKE_TYPED(VkSurfaceFormat2KHR, out,
                          pSurfaceFormats, pSurfaceFormatCount);

   VkColorSpaceKHR *cs;
   u_vector_foreach(cs, &display->colorspaces) {
      struct wsi_wl_format *disp_fmt;
      u_vector_foreach(disp_fmt, &display->formats) {
         if (!(disp_fmt->flags & WSI_WL_FMT_ALPHA) ||
             !(disp_fmt->flags & WSI_WL_FMT_OPAQUE))
            continue;

         vk_outarray_append_typed(VkSurfaceFormat2KHR, &out, out_fmt) {
            out_fmt->sType = VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR;
            out_fmt->pNext = NULL;
            out_fmt->surfaceFormat.format = disp_fmt->vk_format;
            out_fmt->surfaceFormat.colorSpace = *cs;
         }
      }
   }

   if (release_query)
      wsi_wl_surface_release_query_display(wsi_wl_surface);

   return vk_outarray_status(&out);
}

static VkResult
wsi_wl_surface_get_present_modes(VkIcdSurfaceBase *icd_surface,
                                 struct wsi_device *wsi_device,
                                 uint32_t *pPresentModeCount,
                                 VkPresentModeKHR *pPresentModes)
{
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   struct wsi_wl_display *display = wsi_wl_surface->display;
   bool release_query = false;
   VkPresentModeKHR present_modes[3];
   uint32_t present_modes_count = 0;
   VkResult result = VK_SUCCESS;

   if (display == NULL) {
      result = wsi_wl_surface_get_query_display(wsi_wl_surface, wsi_device, &display);
      if (result != VK_SUCCESS)
         return result;
      release_query = true;
   }

   present_modes[present_modes_count++] = VK_PRESENT_MODE_MAILBOX_KHR;
   present_modes[present_modes_count++] = VK_PRESENT_MODE_FIFO_KHR;

   if (display->has_tearing)
      present_modes[present_modes_count++] = VK_PRESENT_MODE_IMMEDIATE_KHR;

   assert(present_modes_count <= ARRAY_SIZE(present_modes));

   if (release_query)
      wsi_wl_surface_release_query_display(wsi_wl_surface);

   if (pPresentModes == NULL) {
      *pPresentModeCount = present_modes_count;
      return VK_SUCCESS;
   }

   *pPresentModeCount = MIN2(*pPresentModeCount, present_modes_count);
   typed_memcpy(pPresentModes, present_modes, *pPresentModeCount);

   return *pPresentModeCount < present_modes_count ? VK_INCOMPLETE : VK_SUCCESS;
}

static VkResult
wsi_wl_surface_get_present_rectangles(VkIcdSurfaceBase *surface,
                                      struct wsi_device *wsi_device,
                                      uint32_t* pRectCount,
                                      VkRect2D* pRects)
{
   VK_OUTARRAY_MAKE_TYPED(VkRect2D, out, pRects, pRectCount);

   vk_outarray_append_typed(VkRect2D, &out, rect) {
      /* We don't know a size so just return the usual "I don't know." */
      *rect = (VkRect2D) {
         .offset = { 0, 0 },
         .extent = { UINT32_MAX, UINT32_MAX },
      };
   }

   return vk_outarray_status(&out);
}

void
wsi_wl_surface_destroy(VkIcdSurfaceBase *icd_surface, VkInstance _instance,
                       const VkAllocationCallbacks *pAllocator)
{
   VK_FROM_HANDLE(vk_instance, instance, _instance);
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   int err;

   if (wsi_wl_surface->wl_syncobj_surface)
      wp_linux_drm_syncobj_surface_v1_destroy(wsi_wl_surface->wl_syncobj_surface);

   if (wsi_wl_surface->wl_dmabuf_feedback) {
      zwp_linux_dmabuf_feedback_v1_destroy(wsi_wl_surface->wl_dmabuf_feedback);
      dmabuf_feedback_fini(&wsi_wl_surface->dmabuf_feedback);
      dmabuf_feedback_fini(&wsi_wl_surface->pending_dmabuf_feedback);
   }

   if (wsi_wl_surface->color.color_surface)
      wp_color_management_surface_v1_destroy(wsi_wl_surface->color.color_surface);

   loader_wayland_surface_destroy(&wsi_wl_surface->wayland_surface);

   if (wsi_wl_surface->display)
      wsi_wl_display_destroy(wsi_wl_surface->display);

   err = mtx_lock(&wsi_wl_surface->query_cache.lock);
   if (err == thrd_success) {
      if (wsi_wl_surface->query_cache.last_device != NULL) {
         wsi_wl_display_finish(&wsi_wl_surface->query_cache.cached_display);
         wsi_wl_surface->query_cache.last_device = NULL;
      }
      mtx_unlock(&wsi_wl_surface->query_cache.lock);
   } else if (wsi_wl_surface->query_cache.last_device != NULL) {
      wsi_wl_display_finish(&wsi_wl_surface->query_cache.cached_display);
      wsi_wl_surface->query_cache.last_device = NULL;
   }

   mtx_destroy(&wsi_wl_surface->query_cache.lock);

   vk_free2(&instance->alloc, pAllocator, wsi_wl_surface);
}

static struct wsi_wl_format *
pick_format_from_surface_dmabuf_feedback(struct wsi_wl_surface *wsi_wl_surface,
                                         VkFormat vk_format)
{
   struct wsi_wl_format *f = NULL;

   /* If the main_device was not advertised, we don't have valid feedback */
   if (wsi_wl_surface->dmabuf_feedback.main_device == 0)
      return NULL;

   util_dynarray_foreach(&wsi_wl_surface->dmabuf_feedback.tranches,
                         struct dmabuf_feedback_tranche, tranche) {
      f = find_format(&tranche->formats, vk_format);
      if (f)
         break;
   }

   return f;
}

static void
surface_dmabuf_feedback_format_table(void *data,
                                     struct zwp_linux_dmabuf_feedback_v1 *zwp_linux_dmabuf_feedback_v1,
                                     int32_t fd, uint32_t size)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;

   /* Clean up any previous table from a repeated format_table event. */
   dmabuf_feedback_format_table_fini(&feedback->format_table);
   dmabuf_feedback_format_table_init(&feedback->format_table);

   if (fd < 0)
      return;

   if (size == 0 || (size % sizeof(feedback->format_table.data[0])) != 0) {
      close(fd);
      return;
   }

   void *map = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
   close(fd);

   if (map == MAP_FAILED) {
      feedback->format_table.data = MAP_FAILED;
      feedback->format_table.size = 0;
      return;
   }

   feedback->format_table.size = size;
   feedback->format_table.data = map;
}

static void
surface_dmabuf_feedback_main_device(void *data,
                                    struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                    struct wl_array *device)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;

   if (device == NULL || device->data == NULL ||
       device->size != sizeof(feedback->main_device)) {
      feedback->main_device = 0;
      return;
   }

   memcpy(&feedback->main_device, device->data, sizeof(feedback->main_device));
}

static void
surface_dmabuf_feedback_tranche_target_device(void *data,
                                              struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                              struct wl_array *device)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;

   if (device == NULL || device->data == NULL ||
       device->size != sizeof(feedback->pending_tranche.target_device)) {
      feedback->pending_tranche.target_device = 0;
      return;
   }

   memcpy(&feedback->pending_tranche.target_device, device->data,
          sizeof(feedback->pending_tranche.target_device));
}

static void
surface_dmabuf_feedback_tranche_flags(void *data,
                                      struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                      uint32_t flags)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;

   feedback->pending_tranche.flags = flags;
}

static void
surface_dmabuf_feedback_tranche_formats(void *data,
                                        struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback,
                                        struct wl_array *indices)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;
   uint32_t format;
   uint64_t modifier;
   uint16_t *index;

   if (indices == NULL || indices->data == NULL)
      return;

   if (indices->size % sizeof(uint16_t) != 0)
      return;

   /* Compositor may advertise or not a format table. If it does, we use it.
    * Otherwise, we steal the most recent advertised format table. If we don't have
    * a most recent advertised format table, compositor did something wrong. */
   if (feedback->format_table.data == NULL) {
      feedback->format_table = wsi_wl_surface->dmabuf_feedback.format_table;
      dmabuf_feedback_format_table_init(&wsi_wl_surface->dmabuf_feedback.format_table);
   }
   if (feedback->format_table.data == MAP_FAILED ||
       feedback->format_table.data == NULL)
      return;

   const size_t entry_count =
      feedback->format_table.size / sizeof(feedback->format_table.data[0]);
   if (entry_count == 0)
      return;

   wl_array_for_each(index, indices) {
      if ((size_t)*index >= entry_count)
         continue;

      format = feedback->format_table.data[*index].format;
      modifier = feedback->format_table.data[*index].modifier;

      wsi_wl_display_add_drm_format_modifier(wsi_wl_surface->display,
                        &wsi_wl_surface->pending_dmabuf_feedback.pending_tranche.formats,
                        format, modifier);
   }
}

static void
surface_dmabuf_feedback_tranche_done(void *data,
                                     struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct dmabuf_feedback *feedback = &wsi_wl_surface->pending_dmabuf_feedback;

   /* Add tranche to array of tranches. */
   util_dynarray_append(&feedback->tranches, feedback->pending_tranche);

   if (dmabuf_feedback_tranche_init(&feedback->pending_tranche) < 0)
      memset(&feedback->pending_tranche, 0, sizeof(feedback->pending_tranche));
}

static bool
sets_of_modifiers_are_the_same(uint32_t num_drm_modifiers_A, const uint64_t *modifiers_A,
                               uint32_t num_drm_modifiers_B, const uint64_t *modifiers_B)
{
   uint32_t i, j;
   bool mod_found;

   if (num_drm_modifiers_A != num_drm_modifiers_B)
      return false;

   for (i = 0; i < num_drm_modifiers_A; i++) {
      mod_found = false;
      for (j = 0; j < num_drm_modifiers_B; j++) {
         if (modifiers_A[i] == modifiers_B[j]) {
            mod_found = true;
            break;
         }
      }
      if (!mod_found)
         return false;
   }

   return true;
}

static void
surface_dmabuf_feedback_done(void *data,
                             struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback)
{
   struct wsi_wl_surface *wsi_wl_surface = data;
   struct wsi_wl_swapchain *chain = wsi_wl_surface->chain;
   struct wsi_wl_format *f;

   dmabuf_feedback_fini(&wsi_wl_surface->dmabuf_feedback);
   wsi_wl_surface->dmabuf_feedback = wsi_wl_surface->pending_dmabuf_feedback;

   if (dmabuf_feedback_init(&wsi_wl_surface->pending_dmabuf_feedback) < 0)
      memset(&wsi_wl_surface->pending_dmabuf_feedback, 0,
             sizeof(wsi_wl_surface->pending_dmabuf_feedback));

   if (chain == NULL)
      return;

   f = pick_format_from_surface_dmabuf_feedback(wsi_wl_surface, chain->vk_format);
   if (f && !sets_of_modifiers_are_the_same(u_vector_length(&f->modifiers),
                                            u_vector_tail(&f->modifiers),
                                            chain->num_drm_modifiers,
                                            chain->drm_modifiers))
      wsi_wl_surface->chain->suboptimal = true;
}

static const struct zwp_linux_dmabuf_feedback_v1_listener
surface_dmabuf_feedback_listener = {
   .format_table = surface_dmabuf_feedback_format_table,
   .main_device = surface_dmabuf_feedback_main_device,
   .tranche_target_device = surface_dmabuf_feedback_tranche_target_device,
   .tranche_flags = surface_dmabuf_feedback_tranche_flags,
   .tranche_formats = surface_dmabuf_feedback_tranche_formats,
   .tranche_done = surface_dmabuf_feedback_tranche_done,
   .done = surface_dmabuf_feedback_done,
};

static VkResult wsi_wl_surface_bind_to_dmabuf_feedback(struct wsi_wl_surface *wsi_wl_surface)
{
   if (dmabuf_feedback_init(&wsi_wl_surface->dmabuf_feedback) < 0)
      goto fail;

   if (dmabuf_feedback_init(&wsi_wl_surface->pending_dmabuf_feedback) < 0)
      goto fail_pending;

   wsi_wl_surface->wl_dmabuf_feedback =
      zwp_linux_dmabuf_v1_get_surface_feedback(wsi_wl_surface->display->wl_dmabuf,
                                               wsi_wl_surface->wayland_surface.wrapper);
   if (!wsi_wl_surface->wl_dmabuf_feedback)
      goto fail_feedback;

   zwp_linux_dmabuf_feedback_v1_add_listener(wsi_wl_surface->wl_dmabuf_feedback,
                                             &surface_dmabuf_feedback_listener,
                                             wsi_wl_surface);

   return VK_SUCCESS;

fail_feedback:
   dmabuf_feedback_fini(&wsi_wl_surface->pending_dmabuf_feedback);
fail_pending:
   dmabuf_feedback_fini(&wsi_wl_surface->dmabuf_feedback);
fail:
   wsi_wl_surface->wl_dmabuf_feedback = NULL;
   return VK_ERROR_OUT_OF_HOST_MEMORY;
}

static VkResult wsi_wl_surface_init(struct wsi_wl_surface *wsi_wl_surface,
                                    struct wsi_device *wsi_device,
                                    const VkAllocationCallbacks *pAllocator)
{
   struct wsi_wayland *wsi =
      (struct wsi_wayland *)wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];
   VkResult result;

   /* wsi_wl_surface has already been initialized. */
   if (wsi_wl_surface->display)
      return VK_SUCCESS;

   result = wsi_wl_display_create(wsi, wsi_wl_surface->base.display,
                                  wsi_device->sw, &wsi_wl_surface->display);
   if (result != VK_SUCCESS)
      goto fail;

   if (!loader_wayland_wrap_surface(&wsi_wl_surface->wayland_surface,
                                    wsi_wl_surface->base.surface,
                                    wsi_wl_surface->display->queue)) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   /* Bind wsi_wl_surface to dma-buf feedback. */
   if (wsi_wl_surface->display->wl_dmabuf &&
       zwp_linux_dmabuf_v1_get_version(wsi_wl_surface->display->wl_dmabuf) >=
       ZWP_LINUX_DMABUF_V1_GET_SURFACE_FEEDBACK_SINCE_VERSION) {
      result = wsi_wl_surface_bind_to_dmabuf_feedback(wsi_wl_surface);
      if (result != VK_SUCCESS)
         goto fail;

      wl_display_roundtrip_queue(wsi_wl_surface->display->wl_display,
                                 wsi_wl_surface->display->queue);
   }

   if (wsi_wl_use_explicit_sync(wsi_wl_surface->display, wsi_device)) {
      wsi_wl_surface->wl_syncobj_surface =
         wp_linux_drm_syncobj_manager_v1_get_surface(wsi_wl_surface->display->wl_syncobj,
                                                     wsi_wl_surface->wayland_surface.wrapper);

      if (!wsi_wl_surface->wl_syncobj_surface) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail;
      }
   }

   return VK_SUCCESS;

fail:
   if (wsi_wl_surface->wl_syncobj_surface) {
      wp_linux_drm_syncobj_surface_v1_destroy(wsi_wl_surface->wl_syncobj_surface);
      wsi_wl_surface->wl_syncobj_surface = NULL;
   }

   if (wsi_wl_surface->wl_dmabuf_feedback) {
      zwp_linux_dmabuf_feedback_v1_destroy(wsi_wl_surface->wl_dmabuf_feedback);
      wsi_wl_surface->wl_dmabuf_feedback = NULL;
      dmabuf_feedback_fini(&wsi_wl_surface->dmabuf_feedback);
      dmabuf_feedback_fini(&wsi_wl_surface->pending_dmabuf_feedback);
   }

   loader_wayland_surface_destroy(&wsi_wl_surface->wayland_surface);

   if (wsi_wl_surface->display) {
      wsi_wl_display_destroy(wsi_wl_surface->display);
      wsi_wl_surface->display = NULL;
   }

   return result;
}

VKAPI_ATTR VkResult VKAPI_CALL
wsi_CreateWaylandSurfaceKHR(VkInstance _instance,
                            const VkWaylandSurfaceCreateInfoKHR *pCreateInfo,
                            const VkAllocationCallbacks *pAllocator,
                            VkSurfaceKHR *pSurface)
{
   VK_FROM_HANDLE(vk_instance, instance, _instance);
   struct wsi_wl_surface *wsi_wl_surface;
   VkIcdSurfaceWayland *surface;

   assert(pCreateInfo->sType == VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR);

   wsi_wl_surface = vk_zalloc2(&instance->alloc, pAllocator, sizeof *wsi_wl_surface,
                               8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (wsi_wl_surface == NULL)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   surface = &wsi_wl_surface->base;

   surface->base.platform = VK_ICD_WSI_PLATFORM_WAYLAND;
   surface->display = pCreateInfo->display;
   surface->surface = pCreateInfo->surface;

   wsi_wl_surface->instance = instance;
   wsi_wl_surface->color.colorspace = VK_COLOR_SPACE_PASS_THROUGH_EXT;

   if (mtx_init(&wsi_wl_surface->query_cache.lock, mtx_plain) != thrd_success) {
      vk_free2(&instance->alloc, pAllocator, wsi_wl_surface);
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   }
   wsi_wl_surface->query_cache.last_device = NULL;
   memset(&wsi_wl_surface->query_cache.cached_display, 0, sizeof(wsi_wl_surface->query_cache.cached_display));

   *pSurface = VkIcdSurfaceBase_to_handle(&surface->base);

   return VK_SUCCESS;
}

struct wsi_wl_present_id {
   /* Fallback when wp_presentation is not supported.
    * Using frame callback is not the intended way to achieve
    * this, but it is the best effort alternative when the proper interface is
    * not available. This approach also matches Xwayland,
    * which uses frame callback to signal DRI3 COMPLETE. */
   struct wl_callback *frame;
   uint64_t present_id;
   uint64_t timing_serial;
   struct mesa_trace_flow flow;
   uint64_t submission_time;
   const VkAllocationCallbacks *alloc;
   struct wsi_wl_swapchain *chain;
   uint64_t target_time;
   uint64_t correction;
   struct wl_list link;
   struct wsi_image *img;
   bool user_target_time;
};

static struct wsi_image *
wsi_wl_swapchain_get_wsi_image(struct wsi_swapchain *wsi_chain,
                               uint32_t image_index)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   return &chain->images[image_index].base;
}

static VkResult
wsi_wl_swapchain_release_images(struct wsi_swapchain *wsi_chain,
                                uint32_t count, const uint32_t *indices)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   for (uint32_t i = 0; i < count; i++) {
      uint32_t index = indices[i];
      chain->images[index].busy = false;
   }
   return VK_SUCCESS;
}

static void
wsi_wl_swapchain_set_present_mode(struct wsi_swapchain *wsi_chain,
                                  VkPresentModeKHR mode)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   chain->base.present_mode = mode;
}

static void
wsi_wl_swapchain_set_timing_request(struct wsi_swapchain *wsi_chain,
                                    const struct wsi_image_timing_request *request)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   chain->timing_request = *request;
}

static VkResult
dispatch_present_id_queue(struct wsi_swapchain *wsi_chain, struct timespec *end_time)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;

   /* We might not own this surface if we're retired, but it is only used here to
    * read events from the present ID queue. This queue is private to a given VkSwapchainKHR,
    * so calling present wait on a retired swapchain cannot interfere with a non-retired swapchain. */
   struct wl_display *wl_display = chain->wsi_wl_surface->display->wl_display;

   VkResult ret;
   int err;

   /* PresentWait can be called concurrently.
    * If there is contention on this mutex, it means there is currently a dispatcher in flight holding the lock.
    * The lock is only held while there is forward progress processing events from Wayland,
    * so there should be no problem locking without timeout.
    * We would like to be able to support timeout = 0 to query the current max_completed count.
    * A timedlock with no timeout can be problematic in that scenario. */
   err = mtx_lock(&chain->present_ids.lock);
   if (err != thrd_success)
      return VK_ERROR_OUT_OF_DATE_KHR;

   /* Someone else is dispatching events; wait for them to update the chain
    * status and wake us up. */
   if (chain->present_ids.dispatch_in_progress) {
      err = u_cnd_monotonic_timedwait(&chain->present_ids.list_advanced,
                                      &chain->present_ids.lock, end_time);
      mtx_unlock(&chain->present_ids.lock);

      if (err == thrd_timedout)
         return VK_TIMEOUT;
      else if (err != thrd_success)
         return VK_ERROR_OUT_OF_DATE_KHR;

      return VK_SUCCESS;
   }

   /* Whether or not we were dispatching the events before, we are now. */
   assert(!chain->present_ids.dispatch_in_progress);
   chain->present_ids.dispatch_in_progress = true;

   /* We drop the lock now - we're still protected by dispatch_in_progress,
    * and holding the lock while dispatch_queue_timeout waits in poll()
    * might delay other threads unnecessarily.
    *
    * We'll pick up the lock again in the dispatched functions.
    */
   mtx_unlock(&chain->present_ids.lock);

   ret = loader_wayland_dispatch(wl_display,
                                 chain->present_ids.queue,
                                 end_time);

   mtx_lock(&chain->present_ids.lock);

   /* Wake up other waiters who may have been unblocked by the events
    * we just read. */
   u_cnd_monotonic_broadcast(&chain->present_ids.list_advanced);

   assert(chain->present_ids.dispatch_in_progress);
   chain->present_ids.dispatch_in_progress = false;

   u_cnd_monotonic_broadcast(&chain->present_ids.list_advanced);
   mtx_unlock(&chain->present_ids.lock);

   if (ret == -1)
      return VK_ERROR_OUT_OF_DATE_KHR;
   if (ret == 0)
      return VK_TIMEOUT;
   return VK_SUCCESS;
}

static void
wsi_wl_swapchain_poll_timing_request(struct wsi_swapchain *wsi_chain)
{
   /* Timing requests must complete in finite time, and if we're not calling present wait
    * or queue present regularly, timing requests will never come back. */
   struct timespec instant = {0};
   dispatch_present_id_queue(wsi_chain, &instant);
}

static bool
wsi_wl_swapchain_present_id_completes_in_finite_time_locked(struct wsi_wl_swapchain *chain,
                                                            uint64_t present_id)
{
   return present_id <= chain->present_ids.max_forward_progress_present_id;
}

static VkResult
wsi_wl_swapchain_wait_for_present(struct wsi_swapchain *wsi_chain,
                                  uint64_t present_id,
                                  uint64_t timeout)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   struct timespec end_time;
   VkResult ret;
   int err;

   MESA_TRACE_FUNC();

   uint64_t atimeout;
   if (timeout == 0 || timeout == UINT64_MAX)
      atimeout = timeout;
   else
      atimeout = os_time_get_absolute_timeout(timeout);

   /* Need to observe that the swapchain semaphore has been unsignalled,
    * as this is guaranteed when a present is complete. */
   VkResult result = wsi_swapchain_wait_for_present_semaphore(
         &chain->base, present_id, timeout);
   if (result != VK_SUCCESS)
      return result;

   /* If using frame callback, guard against lack of forward progress
    * of the frame callback in some situations,
    * e.g. the surface might not be visible.
    * If rendering has completed on GPU,
    * and we still haven't received a callback after 100ms, unblock the application.
    * 100ms is chosen arbitrarily.
    * The queue depth in WL WSI is just one frame due to frame callback in FIFO mode,
    * so from the time a frame has completed render to when it should be considered presented
    * will not exceed 100ms except in contrived edge cases. */

   /* For FIFO without commit-timing we have a similar concern, but only when waiting on the last presented ID that is pending.
    * It is possible the last presentation is held back due to being occluded, but this scenario is very rare
    * in practice. An application blocking on the last presentation implies zero CPU and GPU overlap,
    * and is likely only going to happen at swapchain destruction or similar. */

   uint64_t assumed_success_at = UINT64_MAX;
   if (chain->present_ids.frame_fallback) {
      assumed_success_at = os_time_get_absolute_timeout(100 * 1000 * 1000);
   } else {
      err = mtx_lock(&chain->present_ids.lock);
      if (err != thrd_success)
         return VK_ERROR_OUT_OF_DATE_KHR;

      /* If we're waiting for the very last commit made for whatever reason,
       * we're not necessarily guaranteed forward progress until a subsequent commit is made.
       * Add a timeout post GPU rendering completion to unblock any waiter in reasonable time. */
      if (!wsi_wl_swapchain_present_id_completes_in_finite_time_locked(chain, present_id)) {
         /* The queue depth could be larger, so just make a heuristic decision here to bump the timeout. */
         uint32_t num_pending_cycles = chain->present_ids.outstanding_count + 1;
         assumed_success_at = os_time_get_absolute_timeout(100ull * 1000 * 1000 * num_pending_cycles);
      }
      mtx_unlock(&chain->present_ids.lock);
   }

   /* If app timeout is beyond the deadline we set for reply,
    * always treat the timeout as successful. */
   VkResult timeout_result = assumed_success_at < atimeout ? VK_SUCCESS : VK_TIMEOUT;
   timespec_from_nsec(&end_time, MIN2(atimeout, assumed_success_at));

   while (1) {
      err = mtx_lock(&chain->present_ids.lock);
      if (err != thrd_success)
         return VK_ERROR_OUT_OF_DATE_KHR;

      bool completed = chain->present_ids.max_completed >= present_id;
      mtx_unlock(&chain->present_ids.lock);

      if (completed)
         return VK_SUCCESS;

retry:
      ret = dispatch_present_id_queue(wsi_chain, &end_time);
      if (ret == VK_TIMEOUT) {
         if (timeout_result == VK_SUCCESS && chain->fifo && !chain->present_ids.frame_fallback) {
            /* If there have been subsequent commits since when we made the decision to add a timeout,
             * we can drop that timeout condition and rely on forward progress instead. */
            err = mtx_lock(&chain->present_ids.lock);
            if (err != thrd_success)
               return VK_ERROR_OUT_OF_DATE_KHR;

            if (wsi_wl_swapchain_present_id_completes_in_finite_time_locked(chain, present_id)) {
               timespec_from_nsec(&end_time, atimeout);
               timeout_result = VK_TIMEOUT;
            }
            mtx_unlock(&chain->present_ids.lock);

            /* Retry the wait, but now without any workaround. */
            if (timeout_result == VK_TIMEOUT)
               goto retry;
         }
         return timeout_result;
      }

      if (ret != VK_SUCCESS)
         return ret;
   }
}

static int
wsi_wl_swapchain_ensure_dispatch(struct wsi_wl_swapchain *chain)
{
   struct wsi_wl_surface *wsi_wl_surface = chain->wsi_wl_surface;
   struct wl_display *display = wsi_wl_surface->display->wl_display;
   struct timespec timeout = {0, 0};
   int ret = 0;

   mtx_lock(&chain->present_ids.lock);
   if (chain->present_ids.dispatch_in_progress)
      goto already_dispatching;

   chain->present_ids.dispatch_in_progress = true;
   mtx_unlock(&chain->present_ids.lock);

   /* Use a dispatch with an instant timeout because dispatch_pending
    * won't read any events in the pipe.
    */
   ret = wl_display_dispatch_queue_timeout(display,
                                           chain->present_ids.queue,
                                           &timeout);

   mtx_lock(&chain->present_ids.lock);
   u_cnd_monotonic_broadcast(&chain->present_ids.list_advanced);
   chain->present_ids.dispatch_in_progress = false;

already_dispatching:
   mtx_unlock(&chain->present_ids.lock);
   return ret;
}

static VkResult
wsi_wl_swapchain_wait_for_present2(struct wsi_swapchain *wsi_chain,
                                   uint64_t present_id,
                                   uint64_t timeout)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   struct timespec end_time;
   VkResult ret;
   int err;

   MESA_TRACE_FUNC();

   uint64_t atimeout;
   if (timeout == 0 || timeout == UINT64_MAX)
      atimeout = timeout;
   else
      atimeout = os_time_get_absolute_timeout(timeout);
   timespec_from_nsec(&end_time, atimeout);

   /* Need to observe that the swapchain semaphore has been unsignalled,
    * as this is guaranteed when a present is complete. */
   VkResult result = wsi_swapchain_wait_for_present_semaphore(
         &chain->base, present_id, timeout);
   if (result != VK_SUCCESS)
      return result;

   while (1) {
      err = mtx_lock(&chain->present_ids.lock);
      if (err != thrd_success)
         return VK_ERROR_OUT_OF_DATE_KHR;

      bool completed = chain->present_ids.max_completed >= present_id;
      mtx_unlock(&chain->present_ids.lock);

      if (completed)
         return VK_SUCCESS;

      ret = dispatch_present_id_queue(wsi_chain, &end_time);
      if (ret != VK_SUCCESS)
         return ret;
   }
}

static VkResult
wsi_wl_swapchain_acquire_next_image_explicit(struct wsi_swapchain *wsi_chain,
                                             const VkAcquireNextImageInfoKHR *info,
                                             uint32_t *image_index)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   struct mesa_trace_flow flow = { 0 };

   MESA_TRACE_FUNC_FLOW(&flow);

   /* See comments in queue_present() */
   if (chain->retired)
      return VK_ERROR_OUT_OF_DATE_KHR;

   STACK_ARRAY(struct wsi_image*, images, wsi_chain->image_count);
   for (uint32_t i = 0; i < wsi_chain->image_count; i++)
      images[i] = &chain->images[i].base;

   VkResult result;
#ifdef HAVE_LIBDRM
   result = wsi_drm_wait_for_explicit_sync_release(wsi_chain,
                                                   wsi_chain->image_count,
                                                   images,
                                                   info->timeout,
                                                   image_index);
#else
   result = VK_ERROR_FEATURE_NOT_PRESENT;
#endif
   STACK_ARRAY_FINISH(images);

   if (result == VK_SUCCESS) {
      loader_wayland_buffer_set_flow(&chain->images[*image_index].wayland_buffer, &flow);
      if (chain->suboptimal)
         result = VK_SUBOPTIMAL_KHR;
   }

   return result;
}

static VkResult
wsi_wl_swapchain_acquire_next_image_implicit(struct wsi_swapchain *wsi_chain,
                                             const VkAcquireNextImageInfoKHR *info,
                                             uint32_t *image_index)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   struct timespec start_time, end_time;
   struct timespec rel_timeout;
   struct mesa_trace_flow flow = { 0 };

   MESA_TRACE_FUNC_FLOW(&flow);

   /* See comments in queue_present() */
   if (chain->retired)
      return VK_ERROR_OUT_OF_DATE_KHR;

   struct wsi_wl_surface *wsi_wl_surface = chain->wsi_wl_surface;
   timespec_from_nsec(&rel_timeout, info->timeout);

   clock_gettime(CLOCK_MONOTONIC, &start_time);
   timespec_add(&end_time, &rel_timeout, &start_time);

   while (1) {
      /* If we can use timestamps, we want to make sure the queue feedback
       * events are in is dispatched so we eventually get a refresh rate
       * and a vsync time to phase lock to. We don't need to wait for it
       * now.
        */
      if (chain->commit_timer) {
         if (wsi_wl_swapchain_ensure_dispatch(chain) == -1)
            return VK_ERROR_OUT_OF_DATE_KHR;
      }
      /* Try to find a free image. */
      for (uint32_t i = 0; i < chain->base.image_count; i++) {
         if (!chain->images[i].busy) {
            /* We found a non-busy image */
            *image_index = i;
            chain->images[i].busy = true;
            loader_wayland_buffer_set_flow(&chain->images[i].wayland_buffer, &flow);
            return (chain->suboptimal ? VK_SUBOPTIMAL_KHR : VK_SUCCESS);
         }
      }

      /* Try to dispatch potential events. */
      int ret = loader_wayland_dispatch(wsi_wl_surface->display->wl_display,
                                        wsi_wl_surface->display->queue,
                                        &end_time);
      if (ret == -1)
         return VK_ERROR_OUT_OF_DATE_KHR;

      /* Check for timeout. */
      if (ret == 0)
         return (info->timeout ? VK_TIMEOUT : VK_NOT_READY);
   }
}

static void
wsi_wl_presentation_update_present_id_locked(struct wsi_wl_present_id *id)
{
   id->chain->present_ids.outstanding_count--;
   if (id->present_id > id->chain->present_ids.max_completed)
      id->chain->present_ids.max_completed = id->present_id;

   id->chain->present_ids.display_time_correction -= id->correction;
}

static void
presentation_handle_presented(void *data,
                              uint64_t presentation_time,
                              uint32_t refresh)
{
   struct wsi_wl_present_id *id = data;
   struct wsi_wl_swapchain *chain = id->chain;
   uint64_t target_time = id->target_time;

   /* In v1 of presentation time, we can know if we're likely running VRR, given refresh is 0.
    * However, we cannot know what the base refresh rate is without some kind of external information.
    * We also cannot know if we're actually driving the display in a VRR fashion.
    * In v2, we should always know the "base refresh" rate, but that means we cannot know if we're driving
    * the display VRR or FRR. We could try to deduce it based on timestamps, but that is too brittle.
    * There is a v3 proposal that adds this information more formally so we don't have to guess.
    * Knowing VRR or FRR is not mission critical for most use cases, so just report "Unknown" for now. */
   wsi_swapchain_present_timing_update_refresh_rate(&chain->base, refresh, 0, 0);

   /* Notify this before present wait to reduce latency of presentation timing requests
    * if the application is driving its queries based off present waits. */
   if (id->timing_serial)
      wsi_swapchain_present_timing_notify_completion(&chain->base, id->timing_serial, presentation_time, id->img);

   mtx_lock(&chain->present_ids.lock);
   chain->present_ids.refresh_nsec = refresh;
   if (!chain->present_ids.valid_refresh_nsec) {
      chain->present_ids.valid_refresh_nsec = true;
      chain->present_ids.last_target_time = presentation_time;
      target_time = presentation_time;
   }

   if (presentation_time > chain->present_ids.displayed_time)
      chain->present_ids.displayed_time = presentation_time;

   /* If we have user-defined target time it can be arbitrarily early, and we don't
    * want to start compensating for that error if application stops requesting specific time. */
   if (!id->user_target_time && target_time && presentation_time > target_time)
      chain->present_ids.display_time_error = presentation_time - target_time;
   else
      chain->present_ids.display_time_error = 0;

   wsi_wl_presentation_update_present_id_locked(id);
   mtx_unlock(&chain->present_ids.lock);
   vk_free(id->alloc, id);
}

static void
presentation_handle_discarded(void *data)
{
   struct wsi_wl_present_id *id = data;
   struct wsi_wl_swapchain *chain = id->chain;

   /* From Vulkan spec:
    * "Timing information for some present stages may have a time value of 0,
    * indicating that results for that present stage are not available."
    * Worst case we can simply take a timestamp of clock_id and pretend, but
    * applications may start to latch onto that timestamp as ground truth, which
    * is obviously not correct. */
   if (id->timing_serial)
      wsi_swapchain_present_timing_notify_completion(&chain->base, id->timing_serial, 0, id->img);

   mtx_lock(&chain->present_ids.lock);
   if (!chain->present_ids.valid_refresh_nsec) {
      /* We've started occluded, so make up some safe values to throttle us */
      chain->present_ids.displayed_time = os_time_get_nano();
      chain->present_ids.last_target_time = chain->present_ids.displayed_time;
      chain->present_ids.refresh_nsec = 16666666;
      chain->present_ids.valid_refresh_nsec = true;
   }

   wsi_wl_presentation_update_present_id_locked(id);
   mtx_unlock(&chain->present_ids.lock);
   vk_free(id->alloc, id);
}

static void
presentation_handle_teardown(void *data)
{
   struct wsi_wl_present_id *id = data;

   vk_free(id->alloc, id);
}

static void
presentation_frame_handle_done(void *data, struct wl_callback *callback, uint32_t serial)
{
   struct wsi_wl_present_id *id = data;
   struct wsi_wl_swapchain *chain = id->chain;

   mtx_lock(&chain->present_ids.lock);
   wl_list_remove(&id->link);

   wsi_wl_presentation_update_present_id_locked(id);
   mtx_unlock(&chain->present_ids.lock);
   vk_free(id->alloc, id);
   wl_callback_destroy(callback);
}

static const struct wl_callback_listener pres_frame_listener = {
   presentation_frame_handle_done,
};

static void
frame_handle_done(void *data, struct wl_callback *callback, uint32_t serial)
{
   struct wsi_wl_swapchain *chain = data;

   chain->frame = NULL;
   chain->legacy_fifo_ready = true;

   wl_callback_destroy(callback);
}

static const struct wl_callback_listener frame_listener = {
   frame_handle_done,
};

static bool
set_application_driven_timestamp(struct wsi_wl_swapchain *chain,
                                 uint64_t *timestamp,
                                 uint64_t *correction)
{
   if (chain->timing_request.serial && chain->timing_request.time) {
      /* Absolute time is requested before we have been able to report a reasonable refresh rate
       * to application. This is valid, but we should not try to perform any rounding.
       * NEAREST_REFRESH_CYCLE flag cannot be honored because it's impossible to know at this time. */
      struct timespec target_ts;
      timespec_from_nsec(&target_ts, chain->timing_request.time);
      wp_commit_timer_v1_set_timestamp(chain->commit_timer,
                                       (uint64_t)target_ts.tv_sec >> 32, target_ts.tv_sec,
                                       target_ts.tv_nsec);
      *timestamp = chain->timing_request.time;
      *correction = 0;
      chain->present_ids.last_target_time = chain->timing_request.time;
      return true;
   } else {
      return false;
   }
}

/* The present_ids lock must be held */
static bool
set_timestamp(struct wsi_wl_swapchain *chain,
              uint64_t *timestamp,
              uint64_t *correction)
{
   uint64_t target;
   struct timespec target_ts;
   uint64_t refresh;
   uint64_t displayed_time;
   uint64_t error = 0;

   if (!chain->present_ids.valid_refresh_nsec)
      return set_application_driven_timestamp(chain, timestamp, correction);

   displayed_time = chain->present_ids.displayed_time;
   refresh = chain->present_ids.refresh_nsec;

   /* If refresh is 0, presentation feedback has informed us we have no
    * fixed refresh cycle. In that case we can't generate sensible
    * timestamps at all, so bail out.
    */
   if (!refresh)
      return set_application_driven_timestamp(chain, timestamp, correction);

   /* We assume we're being fed at the display's refresh rate, but
    * if that doesn't happen our timestamps fall into the past.
    *
    * This would result in an offscreen surface being unthrottled until
    * it "catches up" on missed frames. Instead, correct for missed
    * frame opportunities by jumping forward if our display time
    * didn't match our target time.
    *
    * Since we might have a few frames in flight, we need to keep a
    * running tally of how much correction we're applying and remove
    * it as corrected frames are retired.
    */
   if (chain->present_ids.display_time_error > chain->present_ids.display_time_correction)
      error = chain->present_ids.display_time_error -
              chain->present_ids.display_time_correction;

   /* If we're driving timestamps from application, this is somewhat redundant
    * but it will drain out any accumulated display_time_error over time.
    * Accumulated errors are expected since application might not
    * align the target time perfectly against a refresh cycle. */
   target = chain->present_ids.last_target_time;
   if (error > 0)  {
      uint64_t correction_ns = (error / refresh) * refresh;
      target += correction_ns;
      *correction = correction_ns;
   } else {
      *correction = 0;
   }

   chain->present_ids.display_time_correction += *correction;

   if (chain->timing_request.serial && chain->timing_request.time) {
      target = chain->timing_request.time;
      chain->present_ids.last_target_time = target;
      *timestamp = target;

      if (chain->timing_request.flags & VK_PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT)
         target = target > chain->present_ids.refresh_nsec / 2
                ? target - chain->present_ids.refresh_nsec / 2 : 0;

      /* Without the flag, the application is supposed to deal with any safety margins on its own. */
      timespec_from_nsec(&target_ts, target);
   } else {
      target = next_phase_locked_time(displayed_time,
                                      refresh,
                                      target);

      chain->present_ids.last_target_time = target;
      *timestamp = target;

      /* Take back 500 us as a safety margin, to ensure we don't miss our
       * target due to round-off error.
       */
      timespec_from_nsec(&target_ts, target > 500000 ? target - 500000 : 0);
   }

   wp_commit_timer_v1_set_timestamp(chain->commit_timer,
                                    (uint64_t)target_ts.tv_sec >> 32, target_ts.tv_sec,
                                    target_ts.tv_nsec);

   return true;
}

static VkResult
wsi_wl_swapchain_queue_present(struct wsi_swapchain *wsi_chain,
                               uint32_t image_index,
                               uint64_t present_id,
                               const VkPresentRegionKHR *damage)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;
   bool timestamped = false;
   bool queue_dispatched = false;
   bool legacy_frame_created = false;

   MESA_TRACE_FUNC_FLOW(&chain->images[image_index].wayland_buffer.flow);

   struct timespec instant = {0};
   if (dispatch_present_id_queue(wsi_chain, &instant) == VK_ERROR_OUT_OF_DATE_KHR)
      return VK_ERROR_OUT_OF_DATE_KHR;

   if (chain->retired)
      return VK_ERROR_OUT_OF_DATE_KHR;

   struct wsi_wl_surface *wsi_wl_surface = chain->wsi_wl_surface;
   const bool mode_fifo = chain->base.present_mode == VK_PRESENT_MODE_FIFO_KHR;
   const bool use_legacy_fifo = mode_fifo && !chain->fifo;

   if (chain->buffer_type == WSI_WL_BUFFER_SHM_MEMCPY) {
      struct wsi_wl_image *image = &chain->images[image_index];
      memcpy(image->shm_ptr, image->base.cpu_map,
             image->base.row_pitches[0] * chain->extent.height);
   }

   VkResult ret = wsi_wl_swapchain_update_colorspace(chain);
   if (ret != VK_SUCCESS)
      return ret;

   while (!chain->legacy_fifo_ready) {
      int dispatch_ret = wl_display_dispatch_queue(wsi_wl_surface->display->wl_display,
                                                   wsi_wl_surface->display->queue);
      if (dispatch_ret < 0)
         return VK_ERROR_OUT_OF_DATE_KHR;

      queue_dispatched = true;
   }

   if (chain->base.image_info.explicit_sync) {
      struct wsi_wl_image *image = &chain->images[image_index];
      uint64_t acquire_point = image->base.explicit_sync[WSI_ES_ACQUIRE].timeline;
      uint64_t release_point = image->base.explicit_sync[WSI_ES_RELEASE].timeline;

      wp_linux_drm_syncobj_surface_v1_set_acquire_point(wsi_wl_surface->wl_syncobj_surface,
                                                         image->wl_syncobj_timeline[WSI_ES_ACQUIRE],
                                                         (uint32_t)(acquire_point >> 32),
                                                         (uint32_t)(acquire_point & 0xffffffffu));
      wp_linux_drm_syncobj_surface_v1_set_release_point(wsi_wl_surface->wl_syncobj_surface,
                                                         image->wl_syncobj_timeline[WSI_ES_RELEASE],
                                                         (uint32_t)(release_point >> 32),
                                                         (uint32_t)(release_point & 0xffffffffu));
   }

   assert(image_index < chain->base.image_count);

   wl_surface_attach(wsi_wl_surface->wayland_surface.wrapper,
                     chain->images[image_index].wayland_buffer.buffer, 0, 0);

   if (wl_surface_get_version(wsi_wl_surface->wayland_surface.wrapper) >=
       WL_SURFACE_DAMAGE_BUFFER_SINCE_VERSION) {
      if (damage && damage->pRectangles && damage->rectangleCount > 0) {
         for (unsigned i = 0; i < damage->rectangleCount; i++) {
            const VkRectLayerKHR *rect = &damage->pRectangles[i];
            assert(rect->layer == 0);
            wl_surface_damage_buffer(wsi_wl_surface->wayland_surface.wrapper,
                                     rect->offset.x, rect->offset.y,
                                     rect->extent.width, rect->extent.height);
         }
      } else {
         wl_surface_damage_buffer(wsi_wl_surface->wayland_surface.wrapper, 0, 0, INT32_MAX, INT32_MAX);
      }
   } else {
      wl_surface_damage(wsi_wl_surface->wayland_surface.wrapper, 0, 0, INT32_MAX, INT32_MAX);
   }

   if (use_legacy_fifo) {
      chain->frame = wl_surface_frame(wsi_wl_surface->wayland_surface.wrapper);
      if (!chain->frame)
         return VK_ERROR_OUT_OF_HOST_MEMORY;

      wl_callback_add_listener(chain->frame, &frame_listener, chain);
      chain->legacy_fifo_ready = false;
      legacy_frame_created = true;
   } else {
      chain->legacy_fifo_ready = true;
   }

   if (present_id > 0 || (mode_fifo && chain->commit_timer) ||
       util_perfetto_is_tracing_enabled() || chain->timing_request.serial) {
      struct wsi_wl_present_id *id =
         vk_zalloc(chain->wsi_wl_surface->display->wsi_wl->alloc,
                   sizeof(*id), sizeof(uintptr_t),
                   VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (!id)
         goto fail_present_setup;

      id->chain = chain;
      id->present_id = present_id;
      id->alloc = chain->wsi_wl_surface->display->wsi_wl->alloc;
      id->timing_serial = chain->timing_request.serial;
      id->img = &chain->images[image_index].base;
      id->user_target_time = chain->timing_request.time != 0;

      mtx_lock(&chain->present_ids.lock);

      if (mode_fifo && chain->fifo && chain->commit_timer)
         timestamped = set_timestamp(chain, &id->target_time, &id->correction);

      if (!chain->present_ids.frame_fallback) {
         loader_wayland_presentation_feedback(&chain->present_ids.wayland_presentation,
                                              &chain->images[image_index].wayland_buffer,
                                              id);
      } else {
         id->frame = wl_surface_frame(chain->present_ids.surface);
         if (!id->frame) {
            mtx_unlock(&chain->present_ids.lock);
            vk_free(id->alloc, id);
            goto fail_present_setup;
         }

         wl_callback_add_listener(id->frame, &pres_frame_listener, id);
         wl_list_insert(&chain->present_ids.fallback_frame_list, &id->link);
      }

      chain->present_ids.prev_max_present_id = chain->present_ids.max_present_id;
      if (present_id > chain->present_ids.max_present_id)
         chain->present_ids.max_present_id = present_id;

      if (timestamped || !present_id) {
         chain->present_ids.max_forward_progress_present_id = chain->present_ids.max_present_id;
      } else if (chain->present_ids.prev_max_present_id >
                 chain->present_ids.max_forward_progress_present_id) {
         chain->present_ids.max_forward_progress_present_id =
            chain->present_ids.prev_max_present_id;
      }

      chain->present_ids.outstanding_count++;
      mtx_unlock(&chain->present_ids.lock);
   }

   chain->images[image_index].busy = true;

   if (mode_fifo && chain->fifo) {
      wp_fifo_v1_set_barrier(chain->fifo);
      wp_fifo_v1_wait_barrier(chain->fifo);

      if (timestamped) {
         wl_surface_commit(wsi_wl_surface->wayland_surface.wrapper);
         wp_fifo_v1_wait_barrier(chain->fifo);
      }

      chain->next_present_force_wait_barrier = !timestamped;
   } else if (chain->fifo && chain->next_present_force_wait_barrier) {
      if (mode_fifo)
         wp_fifo_v1_wait_barrier(chain->fifo);
      chain->next_present_force_wait_barrier = false;
   }

   wl_surface_commit(wsi_wl_surface->wayland_surface.wrapper);
   wl_display_flush(wsi_wl_surface->display->wl_display);

   if (!queue_dispatched && wsi_chain->image_info.explicit_sync) {
      wl_display_dispatch_queue_pending(wsi_wl_surface->display->wl_display,
                                        wsi_wl_surface->display->queue);
   }

   memset(&chain->timing_request, 0, sizeof(chain->timing_request));
   return VK_SUCCESS;

fail_present_setup:
   if (legacy_frame_created && chain->frame) {
      wl_callback_destroy(chain->frame);
      chain->frame = NULL;
      chain->legacy_fifo_ready = true;
   }
   return VK_ERROR_OUT_OF_HOST_MEMORY;
}

static void
buffer_handle_release(void *data, struct wl_buffer *buffer)
{
   struct wsi_wl_image *image = data;

   assert(image->wayland_buffer.buffer == buffer);

   image->busy = false;
}

static const struct wl_buffer_listener buffer_listener = {
   buffer_handle_release,
};

static uint8_t *
wsi_wl_alloc_image_shm(struct wsi_image *imagew, unsigned size)
{
   struct wsi_wl_image *image = (struct wsi_wl_image *)imagew;

   /* Create a shareable buffer */
   int fd = os_create_anonymous_file(size, NULL);
   if (fd < 0)
      return NULL;

   void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
   if (ptr == MAP_FAILED) {
      close(fd);
      return NULL;
   }

   image->shm_fd = fd;
   image->shm_ptr = ptr;
   image->shm_size = size;

   return ptr;
}

static VkResult
wsi_wl_image_init(struct wsi_wl_swapchain *chain,
                  struct wsi_wl_image *image,
                  const VkSwapchainCreateInfoKHR *pCreateInfo,
                  const VkAllocationCallbacks* pAllocator)
{
   struct wsi_wl_display *display = chain->wsi_wl_surface->display;
   VkResult result;

   result = wsi_create_image(&chain->base, &chain->base.image_info,
                             &image->base);
   if (result != VK_SUCCESS)
      return result;

   switch (chain->buffer_type) {
   case WSI_WL_BUFFER_GPU_SHM:
   case WSI_WL_BUFFER_SHM_MEMCPY: {
      if (chain->buffer_type == WSI_WL_BUFFER_SHM_MEMCPY) {
         wsi_wl_alloc_image_shm(&image->base, image->base.row_pitches[0] *
                                              chain->extent.height);
      }
      if (image->shm_ptr == NULL)
         goto fail_image;

      /* Share it in a wl_buffer */
      struct wl_shm_pool *pool = wl_shm_create_pool(display->wl_shm,
                                                    image->shm_fd,
                                                    image->shm_size);
      if (!pool)
         goto fail_image;
      wl_proxy_set_queue((struct wl_proxy *)pool, display->queue);
      struct wl_buffer *buffer =
         wl_shm_pool_create_buffer(pool, 0, chain->extent.width,
                                   chain->extent.height,
                                   image->base.row_pitches[0],
                                   chain->shm_format);
      wl_shm_pool_destroy(pool);
      if (!buffer)
         goto fail_image;
      loader_wayland_wrap_buffer(&image->wayland_buffer, buffer);
      break;
   }

   case WSI_WL_BUFFER_NATIVE: {
      assert(display->wl_dmabuf);

      struct zwp_linux_buffer_params_v1 *params =
         zwp_linux_dmabuf_v1_create_params(display->wl_dmabuf);
      if (!params)
         goto fail_image;

      for (int i = 0; i < image->base.num_planes; i++) {
         zwp_linux_buffer_params_v1_add(params,
                                        image->base.dma_buf_fd,
                                        i,
                                        image->base.offsets[i],
                                        image->base.row_pitches[i],
                                        image->base.drm_modifier >> 32,
                                        image->base.drm_modifier & 0xffffffff);
      }

      struct wl_buffer *buffer =
         zwp_linux_buffer_params_v1_create_immed(params,
                                                 chain->extent.width,
                                                 chain->extent.height,
                                                 chain->drm_format,
                                                 0);
      zwp_linux_buffer_params_v1_destroy(params);
      if (!buffer)
         goto fail_image;
      loader_wayland_wrap_buffer(&image->wayland_buffer, buffer);

      if (chain->base.image_info.explicit_sync) {
         for (uint32_t i = 0; i < WSI_ES_COUNT; i++) {
            image->wl_syncobj_timeline[i] =
               wp_linux_drm_syncobj_manager_v1_import_timeline(display->wl_syncobj,
                                                               image->base.explicit_sync[i].fd);
            if (!image->wl_syncobj_timeline[i])
               goto fail_image;
         }
      }

      break;
   }

   default:
      UNREACHABLE("Invalid buffer type");
   }

   if (!image->wayland_buffer.buffer)
      goto fail_image;

   /* No need to listen for release if we are explicit sync. */
   if (!chain->base.image_info.explicit_sync)
      wl_buffer_add_listener(image->wayland_buffer.buffer, &buffer_listener, image);

   return VK_SUCCESS;

fail_image:
   for (uint32_t i = 0; i < WSI_ES_COUNT; i++) {
      if (image->wl_syncobj_timeline[i]) {
         wp_linux_drm_syncobj_timeline_v1_destroy(image->wl_syncobj_timeline[i]);
         image->wl_syncobj_timeline[i] = NULL;
      }
   }
   if (image->wayland_buffer.buffer) {
      loader_wayland_buffer_destroy(&image->wayland_buffer);
      image->wayland_buffer.buffer = NULL;
   }
   if (image->shm_size) {
      close(image->shm_fd);
      munmap(image->shm_ptr, image->shm_size);
      image->shm_size = 0;
   }
   wsi_destroy_image(&chain->base, &image->base);

   return VK_ERROR_OUT_OF_HOST_MEMORY;
}

static void
wsi_wl_swapchain_images_free(struct wsi_wl_swapchain *chain)
{
   for (uint32_t i = 0; i < chain->base.image_count; i++) {
      for (uint32_t j = 0; j < WSI_ES_COUNT; j++) {
         if (chain->images[i].wl_syncobj_timeline[j])
            wp_linux_drm_syncobj_timeline_v1_destroy(chain->images[i].wl_syncobj_timeline[j]);
      }
      if (chain->images[i].wayland_buffer.buffer) {
         loader_wayland_buffer_destroy(&chain->images[i].wayland_buffer);
         wsi_destroy_image(&chain->base, &chain->images[i].base);
         if (chain->images[i].shm_size) {
            close(chain->images[i].shm_fd);
            munmap(chain->images[i].shm_ptr, chain->images[i].shm_size);
         }
      }
   }
}

static void
wsi_wl_swapchain_chain_free(struct wsi_wl_swapchain *chain,
                            const VkAllocationCallbacks *pAllocator)
{
   /* Force wayland-client to release fd sent during the swapchain
    * creation (see MAX_FDS_OUT) to avoid filling up VRAM with
    * released buffers.
    */
   struct wsi_wl_surface *wsi_wl_surface = chain->wsi_wl_surface;
   if (!chain->retired)
      wl_display_roundtrip_queue(wsi_wl_surface->display->wl_display,
                                 wsi_wl_surface->display->queue);

   if (chain->frame)
      wl_callback_destroy(chain->frame);
   if (chain->tearing_control)
      wp_tearing_control_v1_destroy(chain->tearing_control);
   if (needs_color_surface(wsi_wl_surface->display, chain->color.colorspace) &&
       wsi_wl_surface->color.color_surface) {
      wsi_wl_surface_remove_color_refcount(wsi_wl_surface);
   }

   /* Only unregister if we are the non-retired swapchain, or
    * we are a retired swapchain and memory allocation failed,
    * in which case there are only retired swapchains. */
   if (wsi_wl_surface->chain == chain)
      wsi_wl_surface->chain = NULL;

   assert(!chain->present_ids.dispatch_in_progress);

   /* In VK_KHR_swapchain_maintenance1 there is no requirement to wait for all present IDs to be complete.
    * Waiting for the swapchain fence is enough.
    * Just clean up anything user did not wait for. */
   struct wsi_wl_present_id *id, *tmp;
   wl_list_for_each_safe(id, tmp, &chain->present_ids.fallback_frame_list, link) {
      wl_callback_destroy(id->frame);
      wl_list_remove(&id->link);
      vk_free(id->alloc, id);
   }

   loader_wayland_presentation_destroy(&chain->present_ids.wayland_presentation);

   if (chain->present_ids.surface)
      wl_proxy_wrapper_destroy(chain->present_ids.surface);
   u_cnd_monotonic_destroy(&chain->present_ids.list_advanced);
   mtx_destroy(&chain->present_ids.lock);

   if (chain->present_ids.queue)
      wl_event_queue_destroy(chain->present_ids.queue);

   vk_free(pAllocator, (void *)chain->drm_modifiers);

   if (chain->fifo)
      wp_fifo_v1_destroy(chain->fifo);

   if (chain->commit_timer)
      wp_commit_timer_v1_destroy(chain->commit_timer);

   wsi_swapchain_finish(&chain->base);
}

static VkResult
wsi_wl_swapchain_destroy(struct wsi_swapchain *wsi_chain,
                         const VkAllocationCallbacks *pAllocator)
{
   struct wsi_wl_swapchain *chain = (struct wsi_wl_swapchain *)wsi_chain;

   wsi_wl_swapchain_images_free(chain);
   wsi_wl_swapchain_chain_free(chain, pAllocator);

   vk_free(pAllocator, chain);

   return VK_SUCCESS;
}

static VkTimeDomainKHR
clock_id_to_vk_time_domain(clockid_t id)
{
   switch (id) {
      case CLOCK_MONOTONIC:
         return VK_TIME_DOMAIN_CLOCK_MONOTONIC_KHR;
      case CLOCK_MONOTONIC_RAW:
         return VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR;
      default:
         /* Default fallback. Will not be used. */
         return VK_TIME_DOMAIN_DEVICE_KHR;
   }
}

static VkResult
wsi_wl_surface_create_swapchain(VkIcdSurfaceBase *icd_surface,
                                VkDevice device,
                                struct wsi_device *wsi_device,
                                const VkSwapchainCreateInfoKHR *pCreateInfo,
                                const VkAllocationCallbacks *pAllocator,
                                struct wsi_swapchain **swapchain_out)
{
   struct wsi_wl_surface *wsi_wl_surface =
      wl_container_of((VkIcdSurfaceWayland *)icd_surface, wsi_wl_surface, base);
   struct wsi_wl_swapchain *chain;
   VkResult result;
   bool present_ids_cnd_inited = false;
   bool present_ids_lock_inited = false;

   assert(pCreateInfo->sType == VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR);

   if (pCreateInfo->oldSwapchain) {
      VK_FROM_HANDLE(wsi_wl_swapchain, old_chain, pCreateInfo->oldSwapchain);
      old_chain->retired = true;
   }

   const size_t size =
      sizeof(*chain) +
      MAX2(WSI_WL_BUMPED_NUM_IMAGES, pCreateInfo->minImageCount) *
         sizeof(chain->images[0]);

   chain = vk_zalloc(pAllocator, size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (chain == NULL)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   wl_list_init(&chain->present_ids.fallback_frame_list);

   if (wsi_wl_surface->chain &&
       wsi_swapchain_to_handle(&wsi_wl_surface->chain->base) !=
          pCreateInfo->oldSwapchain) {
      result = VK_ERROR_NATIVE_WINDOW_IN_USE_KHR;
      goto fail;
   }

   if (pCreateInfo->oldSwapchain) {
      VK_FROM_HANDLE(wsi_wl_swapchain, old_chain, pCreateInfo->oldSwapchain);
      if (old_chain->tearing_control) {
         wp_tearing_control_v1_destroy(old_chain->tearing_control);
         old_chain->tearing_control = NULL;
      }
      if (old_chain->fifo) {
         wp_fifo_v1_destroy(old_chain->fifo);
         old_chain->fifo = NULL;
      }
      if (old_chain->commit_timer) {
         wp_commit_timer_v1_destroy(old_chain->commit_timer);
         old_chain->commit_timer = NULL;
      }
   }

   chain->wsi_wl_surface = wsi_wl_surface;
   wsi_wl_surface->chain = chain;

   result = wsi_wl_surface_init(wsi_wl_surface, wsi_device, pAllocator);
   if (result != VK_SUCCESS)
      goto fail;

   uint32_t num_images = pCreateInfo->minImageCount;
   const bool uses_present_mode_group =
      vk_find_struct_const(pCreateInfo->pNext,
                           SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR) != NULL;

   if (wsi_wl_surface->display->fifo_manager && !uses_present_mode_group) {
      const VkSurfacePresentModeKHR mode = {
         VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR,
         NULL,
         pCreateInfo->presentMode,
      };

      const uint32_t min_images =
         wsi_wl_surface_get_min_image_count(wsi_wl_surface->display, &mode);

      if (min_images == WSI_WL_BUMPED_NUM_IMAGES)
         num_images = MAX2(min_images, num_images);
   }

   const VkPresentModeKHR present_mode =
      wsi_swapchain_get_present_mode(wsi_device, pCreateInfo);

   chain->color.colorspace = pCreateInfo->imageColorSpace;

   enum wsi_wl_buffer_type buffer_type;
   struct wsi_base_image_params *image_params = NULL;
   struct wsi_cpu_image_params cpu_image_params;
   struct wsi_drm_image_params drm_image_params;
   uint32_t num_drm_modifiers = 0;
   const uint64_t *drm_modifiers = NULL;

   if (wsi_device->sw) {
      cpu_image_params = (struct wsi_cpu_image_params) {
         .base.image_type = WSI_IMAGE_TYPE_CPU,
      };

      if (wsi_device->has_import_memory_host &&
          !(WSI_DEBUG & WSI_DEBUG_NOSHM)) {
         buffer_type = WSI_WL_BUFFER_GPU_SHM;
         cpu_image_params.alloc_shm = wsi_wl_alloc_image_shm;
      } else {
         buffer_type = WSI_WL_BUFFER_SHM_MEMCPY;
      }

      image_params = &cpu_image_params.base;
   } else {
      drm_image_params = (struct wsi_drm_image_params) {
         .base.image_type = WSI_IMAGE_TYPE_DRM,
         .same_gpu = wsi_wl_surface->display->same_gpu,
         .explicit_sync =
            wsi_wl_use_explicit_sync(wsi_wl_surface->display, wsi_device),
      };

      if (wsi_wl_surface->display->wl_dmabuf && wsi_device->supports_modifiers) {
         struct wsi_wl_format *f = NULL;

         if (wsi_wl_surface->wl_dmabuf_feedback)
            f = pick_format_from_surface_dmabuf_feedback(
               wsi_wl_surface, pCreateInfo->imageFormat);

         if (f == NULL)
            f = find_format(&chain->wsi_wl_surface->display->formats,
                            pCreateInfo->imageFormat);

         if (f != NULL) {
            num_drm_modifiers = u_vector_length(&f->modifiers);
            drm_modifiers = u_vector_tail(&f->modifiers);

            drm_image_params.num_modifier_lists = num_drm_modifiers > 0 ? 1u : 0u;
            drm_image_params.num_modifiers = &num_drm_modifiers;
            drm_image_params.modifiers = &drm_modifiers;
         }
      }

      buffer_type = WSI_WL_BUFFER_NATIVE;
      image_params = &drm_image_params.base;
   }

   result = wsi_swapchain_init(wsi_device, &chain->base, device,
                               pCreateInfo, image_params, pAllocator);
   if (result != VK_SUCCESS)
      goto fail;

   if (u_cnd_monotonic_init(&chain->present_ids.list_advanced) != thrd_success) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail_after_base_init;
   }
   present_ids_cnd_inited = true;

   if (mtx_init(&chain->present_ids.lock, mtx_plain) != thrd_success) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail_after_cnd_init;
   }
   present_ids_lock_inited = true;

   const bool alpha =
      pCreateInfo->compositeAlpha == VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR;

   chain->base.destroy = wsi_wl_swapchain_destroy;
   chain->base.get_wsi_image = wsi_wl_swapchain_get_wsi_image;
   chain->base.acquire_next_image = chain->base.image_info.explicit_sync
                                  ? wsi_wl_swapchain_acquire_next_image_explicit
                                  : wsi_wl_swapchain_acquire_next_image_implicit;
   chain->base.queue_present = wsi_wl_swapchain_queue_present;
   chain->base.release_images = wsi_wl_swapchain_release_images;
   chain->base.set_present_mode = wsi_wl_swapchain_set_present_mode;
   chain->base.set_timing_request = wsi_wl_swapchain_set_timing_request;
   chain->base.poll_timing_request = wsi_wl_swapchain_poll_timing_request;
   if (pCreateInfo->flags & VK_SWAPCHAIN_CREATE_PRESENT_TIMING_BIT_EXT) {
      chain->base.present_timing.time_domain =
         clock_id_to_vk_time_domain(
            wsi_wl_surface->display->presentation_clock_id);
   }
   chain->base.wait_for_present = wsi_wl_swapchain_wait_for_present;
   chain->base.wait_for_present2 = wsi_wl_swapchain_wait_for_present2;
   chain->base.present_mode = present_mode;
   chain->base.image_count = num_images;
   chain->base.set_hdr_metadata = wsi_wl_swapchain_set_hdr_metadata;
   chain->extent = pCreateInfo->imageExtent;
   chain->vk_format = pCreateInfo->imageFormat;
   chain->buffer_type = buffer_type;

   if (buffer_type == WSI_WL_BUFFER_NATIVE) {
      chain->drm_format =
         wl_drm_format_for_vk_format(wsi_device, chain->vk_format, alpha);
   } else {
      chain->shm_format =
         wl_shm_format_for_vk_format(wsi_device, chain->vk_format, alpha);
   }

   chain->num_drm_modifiers = num_drm_modifiers;
   if (num_drm_modifiers != 0) {
      uint64_t *drm_modifiers_copy =
         vk_alloc(pAllocator,
                  sizeof(*drm_modifiers) * num_drm_modifiers,
                  8,
                  VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (drm_modifiers_copy == NULL) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_free_wl_chain;
      }

      typed_memcpy(drm_modifiers_copy, drm_modifiers, num_drm_modifiers);
      chain->drm_modifiers = drm_modifiers_copy;
   }

   if (present_mode == VK_PRESENT_MODE_IMMEDIATE_KHR) {
      if (wsi_wl_surface->display->tearing_control_manager == NULL) {
         result = VK_ERROR_INITIALIZATION_FAILED;
         goto fail_free_wl_chain;
      }

      chain->tearing_control =
         wp_tearing_control_manager_v1_get_tearing_control(
            wsi_wl_surface->display->tearing_control_manager,
            wsi_wl_surface->wayland_surface.wrapper);
      if (chain->tearing_control == NULL) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_free_wl_chain;
      }

      wp_tearing_control_v1_set_presentation_hint(
         chain->tearing_control,
         WP_TEARING_CONTROL_V1_PRESENTATION_HINT_ASYNC);
   }

   const char *queue_label = "mesa vk surface queue";
   char *queue_name = vk_asprintf(
      pAllocator,
      VK_SYSTEM_ALLOCATION_SCOPE_OBJECT,
      "mesa vk surface %d swapchain %d queue",
      wsi_wl_surface->wayland_surface.id,
      wsi_wl_surface->chain_count++);
   if (queue_name != NULL)
      queue_label = queue_name;

   chain->present_ids.queue =
      wl_display_create_queue_with_name(
         chain->wsi_wl_surface->display->wl_display,
         queue_label);
   vk_free(pAllocator, queue_name);

   if (chain->present_ids.queue == NULL) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail_free_wl_chain;
   }

   if (chain->wsi_wl_surface->display->wp_presentation_notwrapped) {
      chain->present_ids.frame_fallback = false;
      loader_wayland_wrap_presentation(
         &chain->present_ids.wayland_presentation,
         chain->wsi_wl_surface->display->wp_presentation_notwrapped,
         chain->present_ids.queue,
         chain->wsi_wl_surface->display->presentation_clock_id,
         &chain->wsi_wl_surface->wayland_surface,
         presentation_handle_presented,
         presentation_handle_discarded,
         presentation_handle_teardown);
   } else {
      chain->present_ids.frame_fallback = true;
      chain->present_ids.surface =
         wl_proxy_create_wrapper(wsi_wl_surface->base.surface);
      if (chain->present_ids.surface == NULL) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_free_wl_chain;
      }

      wl_proxy_set_queue((struct wl_proxy *)chain->present_ids.surface,
                         chain->present_ids.queue);
   }

   chain->legacy_fifo_ready = true;

   struct wsi_wl_display *dpy = chain->wsi_wl_surface->display;
   if (dpy->fifo_manager) {
      chain->fifo =
         wp_fifo_manager_v1_get_fifo(dpy->fifo_manager,
                                     chain->wsi_wl_surface->wayland_surface.wrapper);
      if (chain->fifo == NULL) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_free_wl_chain;
      }
   }

   if (dpy->commit_timing_manager && !chain->present_ids.frame_fallback) {
      chain->commit_timer =
         wp_commit_timing_manager_v1_get_timer(
            dpy->commit_timing_manager,
            chain->wsi_wl_surface->wayland_surface.wrapper);
      if (chain->commit_timer == NULL) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail_free_wl_chain;
      }
   }

   for (uint32_t i = 0; i < chain->base.image_count; i++) {
      result = wsi_wl_image_init(chain, &chain->images[i],
                                 pCreateInfo, pAllocator);
      if (result != VK_SUCCESS)
         goto fail_free_wl_images;

      chain->images[i].busy = false;
   }

   chain->present_ids.valid_refresh_nsec = false;
   chain->present_ids.refresh_nsec = 0;

   *swapchain_out = &chain->base;
   return VK_SUCCESS;

fail_free_wl_images:
   wsi_wl_swapchain_images_free(chain);

fail_free_wl_chain:
   if (present_ids_lock_inited || present_ids_cnd_inited)
      wsi_wl_swapchain_chain_free(chain, pAllocator);
   goto fail_after_chain_cleanup;

fail_after_cnd_init:
   u_cnd_monotonic_destroy(&chain->present_ids.list_advanced);

fail_after_base_init:
   wsi_swapchain_finish(&chain->base);

fail_after_chain_cleanup:
fail:
   vk_free(pAllocator, chain);
   wsi_wl_surface->chain = NULL;

   assert(result != VK_SUCCESS);
   return result;
}

VkResult
wsi_wl_init_wsi(struct wsi_device *wsi_device,
                const VkAllocationCallbacks *alloc,
                VkPhysicalDevice physical_device)
{
   struct wsi_wayland *wsi;
   VkResult result;

   wsi = vk_alloc(alloc, sizeof(*wsi), 8,
                   VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   if (!wsi) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   wsi->physical_device = physical_device;
   wsi->alloc = alloc;
   wsi->wsi = wsi_device;

   wsi->base.get_support = wsi_wl_surface_get_support;
   wsi->base.get_capabilities2 = wsi_wl_surface_get_capabilities2;
   wsi->base.get_formats = wsi_wl_surface_get_formats;
   wsi->base.get_formats2 = wsi_wl_surface_get_formats2;
   wsi->base.get_present_modes = wsi_wl_surface_get_present_modes;
   wsi->base.get_present_rectangles = wsi_wl_surface_get_present_rectangles;
   wsi->base.create_swapchain = wsi_wl_surface_create_swapchain;

   wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND] = &wsi->base;

   return VK_SUCCESS;

fail:
   wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND] = NULL;

   return result;
}

void
wsi_wl_finish_wsi(struct wsi_device *wsi_device,
                  const VkAllocationCallbacks *alloc)
{
   struct wsi_wayland *wsi =
      (struct wsi_wayland *)wsi_device->wsi[VK_ICD_WSI_PLATFORM_WAYLAND];
   if (!wsi)
      return;

   vk_free(alloc, wsi);
}
