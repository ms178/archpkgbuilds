/*
 * Copyright © 2011 Kristian Høgsberg
 * Copyright © 2011 Benjamin Franzke
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Kristian Høgsberg <krh@bitplanet.net>
 *    Benjamin Franzke <benjaminfranzke@googlemail.com>
 */

#ifndef WAYLAND_DRM_H
#define WAYLAND_DRM_H

#include <stdbool.h>
#include <stdint.h>
#include <wayland-server.h>

#ifdef __cplusplus
extern "C" {
#endif

struct wl_display;
struct wl_resource;
struct wl_drm_buffer;

/**
 * Callback interface for DRM driver integration.
 *
 * All callbacks are invoked from the Wayland event loop thread.
 * Drivers must ensure thread-safety if callbacks access shared state.
 *
 * Performance notes:
 *   - authenticate, is_format_supported: Called once per client (cold path)
 *   - reference_buffer, release_buffer: Called per buffer lifecycle
 *     Gaming at 240 FPS triple-buffered: ~720 calls/sec each (HOT PATH)
 */
struct wayland_drm_callbacks {
   /**
    * Authenticate a DRM client (legacy, rarely used with render nodes).
    *
    * @param user_data Driver-private context
    * @param id DRM magic token
    * @return 0 on success, negative error code on failure
    */
   int (*authenticate)(void *user_data, uint32_t id);

   /**
    * Import a PRIME buffer into the driver.
    *
    * The driver MUST dup() the fd if it needs to retain it (caller closes original).
    * On success, MUST set buffer->driver_buffer to driver-private handle.
    * On failure, MUST set buffer->driver_buffer to NULL.
    *
    * @param user_data Driver-private context
    * @param fd PRIME file descriptor (will be closed by caller)
    * @param buffer Buffer metadata to populate
    *
    * HOT PATH: ~720 calls/sec at 240 FPS triple-buffered gaming.
    */
   void (*reference_buffer)(void *user_data, int fd,
                            struct wl_drm_buffer *buffer);

   /**
    * Release a previously imported buffer.
    *
    * Driver MUST free all resources associated with buffer->driver_buffer.
    *
    * @param user_data Driver-private context
    * @param buffer Buffer to release
    *
    * HOT PATH: ~720 calls/sec at 240 FPS triple-buffered gaming.
    */
   void (*release_buffer)(void *user_data, struct wl_drm_buffer *buffer);

   /**
    * Check if a pixel format is supported by the hardware.
    *
    * @param user_data Driver-private context
    * @param format FourCC pixel format code
    * @return true if supported, false otherwise
    *
    * COLD PATH: Called once per format during client bind (~20 calls/client).
    */
   bool (*is_format_supported)(void *user_data, uint32_t format);
};

/**
 * Server-side wl_drm protocol implementation.
 *
 * This struct is allocated once per compositor and shared by all clients.
 * All accesses occur from the Wayland event loop thread (single-threaded model).
 */
struct wl_drm {
   struct wl_display *display;
   struct wl_global *wl_drm_global;

   void *user_data;
   char *device_name;

   struct wayland_drm_callbacks callbacks;

   struct wl_buffer_interface buffer_interface;
};

/**
 * Client buffer metadata.
 *
 * Memory layout optimized for cache efficiency:
 *   - Size: 64 bytes (exactly 1 cache line on x86-64)
 *   - Alignment: 8 bytes (natural pointer alignment)
 *
 * Lifecycle:
 *   - Allocated in drm_create_prime_buffer (per-buffer, not pooled)
 *   - Freed in destroy_buffer (on client release or disconnect)
 *
 * Access frequency (240 FPS triple-buffered gaming):
 *   - Created/destroyed: ~720 times/sec
 *   - Accessed in driver callbacks: ~1440 times/sec
 *   - Protocol operations: ~720 times/sec
 *
 * CRITICAL: Do not modify field order (ABI compatibility with drivers).
 */
struct wl_drm_buffer {
   struct wl_resource *resource;
   struct wl_drm *drm;
   int32_t width, height;
   uint32_t format;
   uint32_t egl_components;
   int32_t offset[3];
   int32_t stride[3];
   void *driver_buffer;
};

/* Compile-time verification of cache line optimization */
_Static_assert(sizeof(struct wl_drm_buffer) == 64,
               "wl_drm_buffer must be exactly 64 bytes (1 cache line on x86-64)");

/**
 * Retrieve wl_drm_buffer from a wl_resource, with type checking.
 *
 * This function validates that the resource is actually a wl_drm buffer
 * before returning the associated metadata. Used extensively in driver
 * code to safely downcast generic wl_resources.
 *
 * @param drm The wl_drm instance (must not be NULL)
 * @param resource The wl_resource to query (may be NULL)
 * @return The wl_drm_buffer if resource is a valid wl_drm buffer, NULL otherwise
 *
 * HOT PATH: Called ~1440 times/sec at 240 FPS triple-buffered gaming.
 * Marked static inline for zero-overhead abstraction.
 *
 * Example usage:
 *   struct wl_drm_buffer *buffer = wayland_drm_buffer_get(drm, resource);
 *   if (buffer) {
 *      // Safe to access buffer->driver_buffer
 *   }
 */
static inline struct wl_drm_buffer *
wayland_drm_buffer_get(struct wl_drm *drm, struct wl_resource *resource)
{
   /* CRITICAL FIX: Check both drm and resource for NULL
    * Original code only checked resource, causing segfault if drm == NULL
    * (accessing &drm->buffer_interface with NULL drm) */
#if defined(__GNUC__) || defined(__clang__)
   /* Branch prediction: NULL is unlikely in hot path (after initialization) */
   if (__builtin_expect(resource == NULL || drm == NULL, 0)) {
      return NULL;
   }

   /* Type check: verify resource is a wl_buffer managed by this wl_drm instance
    * Branch prediction: Success is likely (callers typically pass valid buffers) */
   if (__builtin_expect(
         wl_resource_instance_of(resource, &wl_buffer_interface,
                                &drm->buffer_interface), 1)) {
      return wl_resource_get_user_data(resource);
   }
#else
   /* Fallback for non-GCC/Clang compilers (no branch hints) */
   if (resource == NULL || drm == NULL) {
      return NULL;
   }

   if (wl_resource_instance_of(resource, &wl_buffer_interface,
                               &drm->buffer_interface)) {
      return wl_resource_get_user_data(resource);
   }
#endif

   return NULL;
}

/**
 * Initialize the wl_drm protocol implementation.
 *
 * Creates a Wayland global that clients can bind to for legacy buffer sharing.
 * Modern clients should use zwp_linux_dmabuf_v1 instead, but wl_drm is still
 * needed for compatibility with older applications.
 *
 * @param display The Wayland display (must not be NULL)
 * @param device_name DRM device node path, e.g., "/dev/dri/renderD128" (must not be NULL)
 *                    The string is copied internally (caller retains ownership).
 * @param callbacks Driver callback table (must not be NULL, all function pointers must be valid)
 * @param user_data Opaque driver-private data (passed to all callbacks, may be NULL)
 * @return Initialized wl_drm instance, or NULL on allocation failure
 *
 * COLD PATH: Called once during compositor initialization.
 *
 * Example usage:
 *   struct wl_drm *drm = wayland_drm_init(display, "/dev/dri/renderD128",
 *                                         &my_callbacks, driver_data);
 *   if (!drm) {
 *      // Handle allocation failure
 *   }
 */
struct wl_drm *
wayland_drm_init(struct wl_display *display, const char *device_name,
                 const struct wayland_drm_callbacks *callbacks, void *user_data);

/**
 * Destroy the wl_drm protocol implementation.
 *
 * Removes the global and frees all associated resources. This does NOT
 * destroy active buffers - clients must release their buffers first
 * (compositor shutdown typically disconnects all clients beforehand).
 *
 * @param drm The wl_drm instance to destroy (may be NULL for idempotent cleanup)
 *
 * COLD PATH: Called once during compositor shutdown.
 *
 * Example usage:
 *   wayland_drm_uninit(drm);  // Safe even if drm == NULL
 */
void
wayland_drm_uninit(struct wl_drm *drm);

#ifdef __cplusplus
}
#endif

#endif /* WAYLAND_DRM_H */
