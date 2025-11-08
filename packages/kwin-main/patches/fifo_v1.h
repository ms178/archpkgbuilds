/*
    SPDX-FileCopyrightText: 2025 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: LGPL-2.1-only OR LGPL-3.0-only OR LicenseRef-KDE-Accepted-LGPL
*/
#pragma once

#include <QObject>
#include <QPointer>

#include "wayland/qwayland-server-fifo-v1.h"

namespace KWin
{

class Display;
class SurfaceInterface;

class FifoManagerV1 final : public QObject, private QtWaylandServer::wp_fifo_manager_v1
{
    Q_OBJECT
public:
    explicit FifoManagerV1(Display *display, QObject *parent);

private:
    void wp_fifo_manager_v1_destroy(Resource *resource) override;
    void wp_fifo_manager_v1_get_fifo(Resource *resource, uint32_t id, wl_resource *surface) override;
};

class FifoV1Surface final : private QtWaylandServer::wp_fifo_v1
{
public:
    explicit FifoV1Surface(wl_client *client, uint32_t id, uint32_t version, SurfaceInterface *surface);
    ~FifoV1Surface() override;

private:
    void wp_fifo_v1_destroy_resource(Resource *resource) override;
    void wp_fifo_v1_destroy(Resource *resource) override;
    void wp_fifo_v1_set_barrier(Resource *resource) override;
    void wp_fifo_v1_wait_barrier(Resource *resource) override;

    QPointer<SurfaceInterface> m_surface;
};

}
