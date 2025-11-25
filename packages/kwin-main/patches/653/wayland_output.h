/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2019 Roman Gilg <subdiff@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "core/output.h"

#include <KWayland/Client/xdgshell.h>

#include <QObject>
#include <QSize>
#include <QString>
#include <QTimer>

#include <array>
#include <chrono>
#include <memory>
#include <optional>
#include <vector>

namespace KWayland
{
namespace Client
{
class Surface;
class Pointer;
class LockedPointer;
class XdgDecoration;
}
}

struct wl_buffer;
struct wl_callback;
struct wl_callback_listener;
struct wp_color_management_surface_v1;
struct wp_fractional_scale_v1;
struct wp_fractional_scale_v1_listener;
struct wp_presentation_feedback;
struct wp_tearing_control_v1;
struct wp_viewport;

namespace KWin
{

class OutputFrame;

namespace Wayland
{

class ColorSurfaceFeedback;
class WaylandBackend;

class WaylandCursor
{
public:
    explicit WaylandCursor(WaylandBackend *backend);
    ~WaylandCursor();

    KWayland::Client::Pointer *pointer() const;
    void setPointer(KWayland::Client::Pointer *pointer);

    void setEnabled(bool enable);
    void update(wl_buffer *buffer, const QSize &logicalSize, const QPoint &hotspot);

private:
    void sync();

    KWayland::Client::Pointer *m_pointer = nullptr;
    std::unique_ptr<KWayland::Client::Surface> m_surface;
    wl_buffer *m_buffer = nullptr;
    wp_viewport *m_viewport = nullptr;
    QPoint m_hotspot;
    QSize m_size;
    bool m_enabled = true;
};

class WaylandOutput : public Output
{
    Q_OBJECT

public:
    WaylandOutput(const QString &name, WaylandBackend *backend);
    ~WaylandOutput() override;

    RenderLoop *renderLoop() const override;
    bool presentAsync(OutputLayer *layer, std::optional<std::chrono::nanoseconds> allowedVrrDelay) override;

    void init(const QSize &pixelSize, qreal scale, bool fullscreen);

    bool isReady() const;
    KWayland::Client::Surface *surface() const;
    WaylandCursor *cursor() const;
    WaylandBackend *backend() const;

    void lockPointer(KWayland::Client::Pointer *pointer, bool lock);
    void setDpmsMode(DpmsMode mode) override;
    void updateDpmsMode(DpmsMode dpmsMode);

    bool testPresentation(const std::shared_ptr<OutputFrame> &frame) override;
    bool present(const QList<OutputLayer *> &layersToUpdate,
                 const std::shared_ptr<OutputFrame> &frame) override;

    void frameDiscarded();
    void framePresented(std::chrono::nanoseconds timestamp, uint32_t refreshRate);

    void applyChanges(const OutputConfiguration &config) override;

    void setOutputLayers(std::vector<std::unique_ptr<OutputLayer>> &&layers);
    QList<OutputLayer *> outputLayers() const;

private:
    void handleConfigure(const QSize &size,
                         KWayland::Client::XdgShellSurface::States states,
                         quint32 serial);
    void updateWindowTitle();
    void applyConfigure(const QSize &size, quint32 serial);
    void updateColor();

    static const wp_fractional_scale_v1_listener s_fractionalScaleListener;
    static void handleFractionalScaleChanged(void *data,
                                             struct wp_fractional_scale_v1 *wp_fractional_scale_v1,
                                             uint32_t scale120);
    static const wl_callback_listener s_frameCallbackListener;
    static void handleFrame(void *data, wl_callback *callback, uint32_t time);

    std::unique_ptr<RenderLoop> m_renderLoop;
    std::unique_ptr<KWayland::Client::Surface> m_surface;
    std::unique_ptr<WaylandCursor> m_cursor;

    OutputLayer *m_cursorLayer = nullptr;

    struct FrameData
    {
        FrameData() = default;
        explicit FrameData(const std::shared_ptr<OutputFrame> &frame,
                           struct wp_presentation_feedback *presentationFeedback,
                           struct wl_callback *frameCallback);
        FrameData(FrameData &&rhs) noexcept;
        FrameData &operator=(FrameData &&rhs) noexcept;
        ~FrameData();

        FrameData(const FrameData &) = delete;
        FrameData &operator=(const FrameData &) = delete;

        std::shared_ptr<OutputFrame> outputFrame;
        wp_presentation_feedback *presentationFeedback = nullptr;
        wl_callback *frameCallback = nullptr;
        std::optional<std::chrono::steady_clock::time_point> frameCallbackTime;
    };

    static constexpr size_t FrameQueueCapacity = 4;
    std::array<FrameData, FrameQueueCapacity> m_frames;
    size_t m_frameHead = 0;
    size_t m_frameTail = 0;
    size_t m_frameCount = 0;

    WaylandBackend *const m_backend;
    wp_viewport *m_viewport = nullptr;

    std::vector<std::unique_ptr<OutputLayer>> m_layers;
    std::unique_ptr<KWayland::Client::XdgShellSurface> m_xdgShellSurface;
    std::unique_ptr<KWayland::Client::LockedPointer> m_pointerLock;
    std::unique_ptr<KWayland::Client::XdgDecoration> m_xdgDecoration;

    QTimer m_turnOffTimer;
    QTimer m_configureThrottleTimer;
    std::unique_ptr<ColorSurfaceFeedback> m_colorSurfaceFeedback;
    wp_fractional_scale_v1 *m_fractionalScale = nullptr;

    quint32 m_pendingConfigureSerial = 0;
    QSize m_pendingConfigureSize;
    uint32_t m_refreshRate = 60'000;
    qreal m_pendingScale = 1.0;
    bool m_hasPointerLock = false;
    bool m_ready = false;
    bool m_mapped = false;

    QSize m_cachedPixelSize;
    QString m_cachedTitle;
};

}
}
