/*
    SPDX-FileCopyrightText: 2025 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: LGPL-2.1-only OR LGPL-3.0-only OR LicenseRef-KDE-Accepted-LGPL
*/
#include "fifo_v1.h"

#include "display.h"
#include "surface_p.h"

namespace KWin
{

static constexpr uint32_t s_version = 1;

FifoManagerV1::FifoManagerV1(Display *display, QObject *parent)
    : QObject(parent)
    , QtWaylandServer::wp_fifo_manager_v1(*display, s_version)
{
}

void FifoManagerV1::wp_fifo_manager_v1_destroy(Resource *resource)
{
    wl_resource_destroy(resource->handle);
}

void FifoManagerV1::wp_fifo_manager_v1_get_fifo(Resource *resource, uint32_t id, wl_resource *wlSurface)
{
    const auto surface = SurfaceInterface::get(wlSurface);
    if (!surface) [[unlikely]] {
        wl_resource_post_error(resource->handle, WL_DISPLAY_ERROR_INVALID_OBJECT, "invalid wl_surface");
        return;
    }

    const auto surfacePrivate = SurfaceInterfacePrivate::get(surface);
    if (surfacePrivate->fifoSurface) [[unlikely]] {
        wl_resource_post_error(resource->handle, error_already_exists, "wp_fifo_v1 already exists for this surface");
        return;
    }

    surfacePrivate->fifoSurface = new FifoV1Surface(resource->client(), id, resource->version(), surface);
}

FifoV1Surface::FifoV1Surface(wl_client *client, uint32_t id, uint32_t version, SurfaceInterface *surface)
    : QtWaylandServer::wp_fifo_v1(client, id, version)
    , m_surface(surface)
{
}

FifoV1Surface::~FifoV1Surface()
{
    if (m_surface) {
        SurfaceInterfacePrivate::get(m_surface)->fifoSurface = nullptr;
    }
}

void FifoV1Surface::wp_fifo_v1_destroy_resource(Resource *resource)
{
    delete this;
}

void FifoV1Surface::wp_fifo_v1_destroy(Resource *resource)
{
    wl_resource_destroy(resource->handle);
}

void FifoV1Surface::wp_fifo_v1_set_barrier(Resource *resource)
{
    if (!m_surface) [[unlikely]] {
        wl_resource_post_error(resource->handle, error_surface_destroyed, "surface already destroyed");
        return;
    }
    SurfaceInterfacePrivate::get(m_surface)->pending->fifoBarrier = true;
}

void FifoV1Surface::wp_fifo_v1_wait_barrier(Resource *resource)
{
    if (!m_surface) [[unlikely]] {
        wl_resource_post_error(resource->handle, error_surface_destroyed, "surface already destroyed");
        return;
    }
    SurfaceInterfacePrivate::get(m_surface)->pending->hasFifoWaitCondition = true;
}

}
