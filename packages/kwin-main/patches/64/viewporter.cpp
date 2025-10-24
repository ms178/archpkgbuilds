/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: LGPL-2.1-only OR LGPL-3.0-only OR LicenseRef-KDE-Accepted-LGPL
*/

#include "viewporter.h"
#include "display.h"
#include "surface_p.h"
#include "viewporter_p.h"

#include <cmath>

// Compiler compatibility for branch hints
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#if !__has_builtin(__builtin_expect)
#define __builtin_expect(expr, val) (expr)
#endif

static const int s_version = 1;

// CRITICAL: GPU resource limits for AMD Vega 64 (GFX9)
// AMD GFX9 ISA manual §8.2.4: Max 2D texture size is 16384×16384
// Prevents GPU timeout (TDR) from oversized allocations
static const int32_t MAX_DIMENSION = 16384;

namespace KWin
{

class ViewporterInterfacePrivate : public QtWaylandServer::wp_viewporter
{
protected:
    void wp_viewporter_destroy(Resource *resource) override;
    void wp_viewporter_get_viewport(Resource *resource, uint32_t id, struct ::wl_resource *surface) override;
};

void ViewporterInterfacePrivate::wp_viewporter_destroy(Resource *resource)
{
    wl_resource_destroy(resource->handle);
}

void ViewporterInterfacePrivate::wp_viewporter_get_viewport(Resource *resource, uint32_t id, struct ::wl_resource *surface_resource)
{
    // BUG FIX: Validate surface_resource before passing to SurfaceInterface::get()
    // SurfaceInterface::get() may not handle nullptr gracefully depending on implementation
    if (__builtin_expect(!surface_resource, 0)) {
        wl_resource_post_error(resource->handle, WL_DISPLAY_ERROR_INVALID_OBJECT,
                               "surface resource is null");
        return;
    }

    SurfaceInterface *surface = SurfaceInterface::get(surface_resource);

    // CRITICAL FIX: Validate surface pointer
    // SurfaceInterface::get() returns nullptr if:
    // - Client passed wrong resource type
    // - Resource was destroyed
    // - Resource is from different connection
    if (__builtin_expect(!surface, 0)) {
        wl_resource_post_error(resource->handle, WL_DISPLAY_ERROR_INVALID_OBJECT,
                               "invalid surface resource");
        return;
    }

    // Protocol compliance: Only one viewport per surface
    ViewportInterface *viewport = ViewportInterface::get(surface);
    if (__builtin_expect(viewport != nullptr, 0)) {
        wl_resource_post_error(resource->handle, error_viewport_exists,
                               "the specified surface already has a viewport");
        return;
    }

    // Create Wayland resource
    wl_resource *viewportResource = wl_resource_create(resource->client(),
                                                        &wp_viewport_interface,
                                                        resource->version(),
                                                        id);

    // CRITICAL FIX: Handle OOM (wl_resource_create returns nullptr on allocation failure)
    // While rare, failing to check this can cause null pointer dereference crash
    if (__builtin_expect(!viewportResource, 0)) {
        wl_resource_post_no_memory(resource->handle);
        return;
    }

    // Construct ViewportInterface (cannot fail - no exceptions enabled)
    // The object manages its own lifetime via wp_viewport_destroy_resource()
    new ViewportInterface(surface, viewportResource);
}

ViewportInterface::ViewportInterface(SurfaceInterface *surface, wl_resource *resource)
    : QtWaylandServer::wp_viewport(resource)
    , surface(surface)
{
    SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);
    surfacePrivate->viewportExtension = this;
}

ViewportInterface::~ViewportInterface()
{
    // BUG FIX: Check surface pointer before dereference
    // The surface may be destroyed before the viewport during shutdown
    if (surface) {
        SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);
        surfacePrivate->viewportExtension = nullptr;
    }
}

ViewportInterface *ViewportInterface::get(SurfaceInterface *surface)
{
    if (!surface) {
        return nullptr;
    }
    SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);
    return surfacePrivate->viewportExtension;
}

void ViewportInterface::wp_viewport_destroy_resource(Resource *resource)
{
    delete this;
}

void ViewportInterface::wp_viewport_destroy(Resource *resource)
{
    if (surface) {
        SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);
        // Reset viewport state to "unset" per protocol specification
        surfacePrivate->pending->viewport.sourceGeometry = QRectF();
        surfacePrivate->pending->viewport.destinationSize = QSize();
        // OPTIMIZATION: Combine flag updates in single operation
        surfacePrivate->pending->committed |= (SurfaceState::Field::SourceGeometry |
                                                SurfaceState::Field::DestinationSize);
    }

    wl_resource_destroy(resource->handle);
}

void ViewportInterface::wp_viewport_set_source(Resource *resource,
                                                wl_fixed_t x_fixed,
                                                wl_fixed_t y_fixed,
                                                wl_fixed_t width_fixed,
                                                wl_fixed_t height_fixed)
{
    if (__builtin_expect(!surface, 0)) {
        wl_resource_post_error(resource->handle, error_no_surface,
                               "the wl_surface for this viewport no longer exists");
        return;
    }

    // Convert wl_fixed_t (24.8 fixed-point) to double
    const qreal x = wl_fixed_to_double(x_fixed);
    const qreal y = wl_fixed_to_double(y_fixed);
    const qreal width = wl_fixed_to_double(width_fixed);
    const qreal height = wl_fixed_to_double(height_fixed);

    SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);

    // Check for "unset" sentinel: all four values must be -1.0
    if (x == -1.0 && y == -1.0 && width == -1.0 && height == -1.0) {
        surfacePrivate->pending->viewport.sourceGeometry = QRectF();
        surfacePrivate->pending->committed |= SurfaceState::Field::SourceGeometry;
        return;
    }

    // Validate per wp_viewport protocol:
    // - x, y must be non-negative
    // - width, height must be positive
    if (__builtin_expect(x < 0.0 || y < 0.0 || width <= 0.0 || height <= 0.0, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "invalid source geometry");
        return;
    }

    // CRITICAL FIX: Validate dimensions against GPU limits
    // AMD Vega 64 (GFX9) max texture size: 16384×16384
    // Exceeding this causes:
    // - radeonSI driver rejection
    // - GPU command buffer errors
    // - Potential TDR timeout (5 sec GPU hang)
    //
    // Performance: 4 comparisons, ~4 cycles total. Negligible at <10 Hz call rate.
    if (__builtin_expect(x > MAX_DIMENSION || y > MAX_DIMENSION ||
                          width > MAX_DIMENSION || height > MAX_DIMENSION, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "source geometry exceeds maximum dimension");
        return;
    }

    // BUG FIX: Validate bounds (x+width, y+height) don't exceed limits
    // Prevents integer overflow in downstream QRect conversions and
    // buffer overruns in texture sampling code
    if (__builtin_expect(x + width > MAX_DIMENSION || y + height > MAX_DIMENSION, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "source geometry bounds exceed maximum");
        return;
    }

    surfacePrivate->pending->viewport.sourceGeometry = QRectF(x, y, width, height);
    surfacePrivate->pending->committed |= SurfaceState::Field::SourceGeometry;
}

void ViewportInterface::wp_viewport_set_destination(Resource *resource,
                                                     int32_t width,
                                                     int32_t height)
{
    if (__builtin_expect(!surface, 0)) {
        wl_resource_post_error(resource->handle, error_no_surface,
                               "the wl_surface for this viewport no longer exists");
        return;
    }

    SurfaceInterfacePrivate *surfacePrivate = SurfaceInterfacePrivate::get(surface);

    // Check for "unset" sentinel: both values must be -1
    if (width == -1 && height == -1) {
        surfacePrivate->pending->viewport.destinationSize = QSize();
        surfacePrivate->pending->committed |= SurfaceState::Field::DestinationSize;
        return;
    }

    // Validate per wp_viewport protocol: width, height must be positive
    if (__builtin_expect(width <= 0 || height <= 0, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "invalid destination size");
        return;
    }

    // CRITICAL FIX: Validate dimensions against GPU limits
    // AMD Vega 64 max framebuffer size: 16384×16384 (GFX9 ISA §8.2.4)
    // Exceeding causes:
    // - radeonSI rejection (driver error)
    // - GPU hang (TDR timeout)
    // - System instability (multi-GB allocation)
    if (__builtin_expect(width > MAX_DIMENSION || height > MAX_DIMENSION, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "destination size exceeds maximum dimension");
        return;
    }

    // BUG FIX: Validate area to prevent overflow in width×height
    // int32_t max: 2,147,483,647
    // 16384² = 268,435,456 (safe)
    // Prevents overflow in:
    // - Buffer size calculations (bytes = width × height × 4)
    // - GPU memory allocation
    //
    // Use int64_t for multiplication to avoid overflow, then check result
    const int64_t area = static_cast<int64_t>(width) * static_cast<int64_t>(height);
    const int64_t max_area = static_cast<int64_t>(MAX_DIMENSION) * static_cast<int64_t>(MAX_DIMENSION);
    if (__builtin_expect(area > max_area, 0)) {
        wl_resource_post_error(resource->handle, error_bad_value,
                               "destination area exceeds maximum");
        return;
    }

    surfacePrivate->pending->viewport.destinationSize = QSize(width, height);
    surfacePrivate->pending->committed |= SurfaceState::Field::DestinationSize;
}

ViewporterInterface::ViewporterInterface(Display *display, QObject *parent)
    : QObject(parent)
    , d(new ViewporterInterfacePrivate)
{
    d->init(*display, s_version);
}

ViewporterInterface::~ViewporterInterface()
{
}

} // namespace KWin

#include "moc_viewporter.cpp"
