/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2019 Roman Gilg <subdiff@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "wayland_output.h"
#include "color_manager.h"
#include "compositor.h"
#include "core/outputconfiguration.h"
#include "core/outputlayer.h"
#include "core/renderbackend.h"
#include "core/renderloop_p.h"
#include "wayland_backend.h"
#include "wayland_display.h"
#include "wayland_layer.h"

#include <KWayland/Client/compositor.h>
#include <KWayland/Client/pointer.h>
#include <KWayland/Client/pointerconstraints.h>
#include <KWayland/Client/surface.h>
#include <KWayland/Client/xdgdecoration.h>

#include "wayland-fractional-scale-v1-client-protocol.h"
#include "wayland-presentation-time-client-protocol.h"
#include "wayland-single-pixel-buffer-v1-client-protocol.h"
#include "wayland-tearing-control-v1-client-protocol.h"
#include "wayland-viewporter-client-protocol.h"
#include "workspace.h"

#include <KLocalizedString>

#include <QPainter>

#include <algorithm>
#include <climits>
#include <cmath>
#include <utility>

namespace KWin
{
namespace Wayland
{

using namespace KWayland::Client;

WaylandCursor::WaylandCursor(WaylandBackend *backend)
    : m_surface(backend->display()->compositor()->createSurface())
{
    if (auto viewporter = backend->display()->viewporter()) {
        m_viewport = wp_viewporter_get_viewport(viewporter, *m_surface);
    }
}

WaylandCursor::~WaylandCursor()
{
    if (m_viewport) {
        wp_viewport_destroy(m_viewport);
    }
}

KWayland::Client::Pointer *WaylandCursor::pointer() const
{
    return m_pointer;
}

void WaylandCursor::setPointer(KWayland::Client::Pointer *pointer)
{
    if (m_pointer == pointer) {
        return;
    }
    m_pointer = pointer;
    if (m_pointer) {
        m_pointer->setCursor(m_surface.get(), m_hotspot);
    }
}

void WaylandCursor::setEnabled(bool enable)
{
    if (m_enabled != enable) {
        m_enabled = enable;
        sync();
    }
}

void WaylandCursor::update(wl_buffer *buffer, const QSize &logicalSize, const QPoint &hotspot)
{
    if (m_buffer != buffer || m_size != logicalSize || m_hotspot != hotspot) {
        m_buffer = buffer;
        m_size = logicalSize;
        m_hotspot = hotspot;
        sync();
    }
}

void WaylandCursor::sync()
{
    if (!m_enabled) [[unlikely]] {
        m_surface->attachBuffer(KWayland::Client::Buffer::Ptr());
        m_surface->commit(KWayland::Client::Surface::CommitFlag::None);
    } else {
        if (m_viewport) [[likely]] {
            wp_viewport_set_destination(m_viewport, m_size.width(), m_size.height());
        }
        m_surface->attachBuffer(m_buffer);
        if (m_size.isEmpty()) [[unlikely]] {
            m_surface->damageBuffer(QRect(0, 0, INT32_MAX, INT32_MAX));
        } else {
            m_surface->damageBuffer(QRect(QPoint(0, 0), m_size));
        }
        m_surface->commit(KWayland::Client::Surface::CommitFlag::None);
    }

    if (m_pointer) [[likely]] {
        m_pointer->setCursor(m_surface.get(), m_hotspot);
    }
}

void WaylandOutput::handleFractionalScaleChanged(void *data,
                                                 struct wp_fractional_scale_v1 *,
                                                 uint32_t scale120)
{
    // 120 == 1.0. 0 is invalid; clamp to sane bounds to avoid pathological sizes.
    const uint32_t clamped120 = std::clamp(scale120, 120u, 120u * 16u);
    static_cast<WaylandOutput *>(data)->m_pendingScale = clamped120 / 120.0;
}

const wp_fractional_scale_v1_listener WaylandOutput::s_fractionalScaleListener{
    .preferred_scale = &WaylandOutput::handleFractionalScaleChanged,
};

WaylandOutput::WaylandOutput(const QString &name, WaylandBackend *backend)
    : Output(backend)
    , m_renderLoop(std::make_unique<RenderLoop>(this))
    , m_surface(backend->display()->compositor()->createSurface())
    , m_cursor(std::make_unique<WaylandCursor>(backend))
    , m_backend(backend)
    , m_xdgShellSurface(backend->display()->xdgShell()->createSurface(m_surface.get()))
{
    m_renderLoop->setMaxPendingFrameCount(2);

    if (auto *manager = m_backend->display()->xdgDecorationManager()) {
        m_xdgDecoration.reset(manager->getToplevelDecoration(m_xdgShellSurface.get()));
        m_xdgDecoration->setMode(KWayland::Client::XdgDecoration::Mode::ServerSide);
    }

    Capabilities caps = Capability::Dpms;
    if (backend->display()->tearingControl()) {
        caps |= Capability::Tearing;
    }

    if (auto manager = backend->display()->colorManager()) {
        const bool supportsMinFeatures = manager->supportsFeature(WP_COLOR_MANAGER_V1_FEATURE_PARAMETRIC)
            && manager->supportsFeature(WP_COLOR_MANAGER_V1_FEATURE_SET_PRIMARIES)
            && manager->supportsFeature(WP_COLOR_MANAGER_V1_FEATURE_SET_LUMINANCES)
            && manager->supportsTransferFunction(WP_COLOR_MANAGER_V1_TRANSFER_FUNCTION_GAMMA22);
        if (supportsMinFeatures) {
            m_colorSurfaceFeedback = std::make_unique<ColorSurfaceFeedback>(
                wp_color_manager_v1_get_surface_feedback(manager->object(), *m_surface));
            connect(m_colorSurfaceFeedback.get(), &ColorSurfaceFeedback::preferredColorChanged,
                    this, &WaylandOutput::updateColor);
        }
    }

    if (auto manager = backend->display()->fractionalScale()) {
        m_fractionalScale = wp_fractional_scale_manager_v1_get_fractional_scale(manager, *m_surface);
        wp_fractional_scale_v1_add_listener(m_fractionalScale, &s_fractionalScaleListener, this);
    }

    m_viewport = wp_viewporter_get_viewport(backend->display()->viewporter(), *m_surface);

    Information info{};
    info.name = name;
    info.model = name;
    info.capabilities = caps;
    setInformation(info);

    m_turnOffTimer.setSingleShot(true);
    m_turnOffTimer.setInterval(dimAnimationTime());
    connect(&m_turnOffTimer, &QTimer::timeout, this, [this] {
        updateDpmsMode(DpmsMode::Off);
    });

    m_configureThrottleTimer.setSingleShot(true);
    connect(&m_configureThrottleTimer, &QTimer::timeout, this, [this]() {
        applyConfigure(m_pendingConfigureSize, m_pendingConfigureSerial);
    });

    updateWindowTitle();

    connect(m_xdgShellSurface.get(), &XdgShellSurface::configureRequested,
            this, &WaylandOutput::handleConfigure);
    connect(m_xdgShellSurface.get(), &XdgShellSurface::closeRequested,
            qApp, &QCoreApplication::quit);
    connect(this, &WaylandOutput::enabledChanged, this, &WaylandOutput::updateWindowTitle);
    connect(this, &WaylandOutput::dpmsModeChanged, this, &WaylandOutput::updateWindowTitle);
}

WaylandOutput::~WaylandOutput()
{
    while (m_frameCount > 0) {
        m_frames[m_frameHead] = FrameData();
        m_frameHead = (m_frameHead + 1) % FrameQueueCapacity;
        --m_frameCount;
    }

    wp_viewport_destroy(m_viewport);
    m_xdgDecoration.reset();
    m_xdgShellSurface.reset();
    m_surface.reset();
}

void WaylandOutput::updateColor()
{
    const auto &preferred = m_colorSurfaceFeedback->preferredColor();
    const auto tf = TransferFunction(TransferFunction::gamma22,
                                     preferred->transferFunction().minLuminance,
                                     preferred->transferFunction().maxLuminance);
    State next = m_state;
    next.colorDescription = std::make_shared<ColorDescription>(ColorDescription{
        preferred->containerColorimetry(),
        tf,
        preferred->referenceLuminance(),
        preferred->minLuminance(),
        preferred->maxAverageLuminance(),
        preferred->maxHdrLuminance(),
    });
    next.originalColorDescription = next.colorDescription;
    next.blendingColor = next.colorDescription;
    next.layerBlendingColor = next.colorDescription;
    setState(next);
}

static void handleDiscarded(void *data, struct wp_presentation_feedback *)
{
    static_cast<WaylandOutput *>(data)->frameDiscarded();
}

static void handlePresented(void *data,
                            struct wp_presentation_feedback *,
                            uint32_t tv_sec_hi,
                            uint32_t tv_sec_lo,
                            uint32_t tv_nsec,
                            uint32_t refresh,
                            uint32_t,
                            uint32_t,
                            uint32_t)
{
    const auto timestamp = std::chrono::seconds((uint64_t(tv_sec_hi) << 32) | tv_sec_lo)
                         + std::chrono::nanoseconds(tv_nsec);
    uint32_t refreshRate = 60'000;
    if (refresh > 0 && refresh < 1'000'000'000) [[likely]] {
        refreshRate = static_cast<uint32_t>(1'000'000'000'000ull / refresh);
    }
    static_cast<WaylandOutput *>(data)->framePresented(timestamp, refreshRate);
}

static void handleSyncOutput(void *, struct wp_presentation_feedback *, struct wl_output *)
{
}

static constexpr struct wp_presentation_feedback_listener s_presentationListener {
    .sync_output = handleSyncOutput,
    .presented = handlePresented,
    .discarded = handleDiscarded,
};

void WaylandOutput::handleFrame(void *data, wl_callback *callback, uint32_t)
{
    auto *output = static_cast<WaylandOutput *>(data);
    for (size_t i = 0; i < output->m_frameCount; ++i) {
        const size_t idx = (output->m_frameHead + i) % FrameQueueCapacity;
        if (output->m_frames[idx].frameCallback == callback) [[likely]] {
            output->m_frames[idx].frameCallbackTime = std::chrono::steady_clock::now();
            return;
        }
    }
}

const wl_callback_listener WaylandOutput::s_frameCallbackListener{
    .done = &WaylandOutput::handleFrame,
};

bool WaylandOutput::testPresentation(const std::shared_ptr<OutputFrame> &frame)
{
    Q_UNUSED(frame)
    if (m_hasPointerLock && m_cursorLayer && m_cursorLayer->isEnabled()) {
        return false;
    }
    return true;
}

WaylandOutput::FrameData::FrameData(const std::shared_ptr<OutputFrame> &frame,
                                    struct wp_presentation_feedback *presentationFeedback,
                                    struct wl_callback *frameCallback)
    : outputFrame(frame)
    , presentationFeedback(presentationFeedback)
    , frameCallback(frameCallback)
{
}

WaylandOutput::FrameData::FrameData(FrameData &&rhs) noexcept
    : outputFrame(std::move(rhs.outputFrame))
    , presentationFeedback(std::exchange(rhs.presentationFeedback, nullptr))
    , frameCallback(std::exchange(rhs.frameCallback, nullptr))
    , frameCallbackTime(std::exchange(rhs.frameCallbackTime, std::nullopt))
{
}

WaylandOutput::FrameData &WaylandOutput::FrameData::operator=(FrameData &&rhs) noexcept
{
    if (this != &rhs) {
        if (presentationFeedback) {
            wp_presentation_feedback_destroy(presentationFeedback);
        }
        if (frameCallback) {
            wl_callback_destroy(frameCallback);
        }

        outputFrame = std::move(rhs.outputFrame);
        presentationFeedback = std::exchange(rhs.presentationFeedback, nullptr);
        frameCallback = std::exchange(rhs.frameCallback, nullptr);
        frameCallbackTime = std::exchange(rhs.frameCallbackTime, std::nullopt);
    }
    return *this;
}

WaylandOutput::FrameData::~FrameData()
{
    if (presentationFeedback) {
        wp_presentation_feedback_destroy(presentationFeedback);
    }
    if (frameCallback) {
        wl_callback_destroy(frameCallback);
    }
}

bool WaylandOutput::present(const QList<OutputLayer *> &layersToUpdate,
                            const std::shared_ptr<OutputFrame> &frame)
{
    if (m_cursorLayer) [[likely]] {
        if (m_hasPointerLock && m_cursorLayer->isEnabled()) {
            return false;
        }
        m_cursor->setEnabled(m_cursorLayer->isEnabled());
    }

    wl_buffer *mappingBuffer = nullptr;
    if (!m_mapped) [[unlikely]] {
        mappingBuffer = wp_single_pixel_buffer_manager_v1_create_u32_rgba_buffer(
            m_backend->display()->singlePixelManager(), 0, 0, 0, 0xFFFFFFFF);
        m_surface->attachBuffer(mappingBuffer);
        m_mapped = true;
    }

    wp_viewport_set_destination(m_viewport, m_cachedPixelSize.width(), m_cachedPixelSize.height());
    m_surface->setScale(1);

    for (OutputLayer *layer : layersToUpdate) {
        if (layer->type() != OutputLayerType::CursorOnly) [[likely]] {
            static_cast<WaylandLayer *>(layer)->commit(frame->presentationMode());
        }
    }

    if (m_backend->display()->tearingControl()) [[unlikely]] {
        m_renderLoop->setPresentationMode(frame->presentationMode());
    }

    FrameData frameData{
        frame,
        wp_presentation_feedback(m_backend->display()->presentationTime(), *m_surface),
        wl_surface_frame(*m_surface),
    };
    wp_presentation_feedback_add_listener(frameData.presentationFeedback, &s_presentationListener, this);
    wl_callback_add_listener(frameData.frameCallback, &s_frameCallbackListener, this);

    m_surface->commit(KWayland::Client::Surface::CommitFlag::None);

    if (mappingBuffer) {
        wl_buffer_destroy(mappingBuffer);
    }

    m_frames[m_frameTail] = std::move(frameData);
    m_frameTail = (m_frameTail + 1) % FrameQueueCapacity;

    if (m_frameCount == FrameQueueCapacity) {
        m_frameHead = (m_frameHead + 1) % FrameQueueCapacity;
    } else {
        ++m_frameCount;
    }

    return true;
}

void WaylandOutput::frameDiscarded()
{
    if (m_frameCount > 0) {
        m_frames[m_frameHead] = FrameData();
        m_frameHead = (m_frameHead + 1) % FrameQueueCapacity;
        --m_frameCount;
    }
}

void WaylandOutput::framePresented(std::chrono::nanoseconds timestamp, uint32_t refreshRate)
{
    if (refreshRate != m_refreshRate) [[unlikely]] {
        m_refreshRate = refreshRate;
        const auto mode = std::make_shared<OutputMode>(m_cachedPixelSize, m_refreshRate);
        State next = m_state;
        next.modes = {mode};
        next.currentMode = mode;
        setState(next);
        m_renderLoop->setRefreshRate(m_refreshRate);
    }

    if (m_frameCount == 0) {
        return;
    }

    const auto &frame = m_frames[m_frameHead];

    if (auto t = frame.frameCallbackTime) [[likely]] {
        using namespace std::chrono;

        const nanoseconds callbackNs = duration_cast<nanoseconds>(t->time_since_epoch());
        nanoseconds diff = timestamp - callbackNs;

        if (diff < 0ns) [[unlikely]] {
            diff = 0ns;
        }

        // Cap to avoid poisoning scheduling if clocks jitter or feedback is delayed.
        nanoseconds cap = 20ms;
        if (m_refreshRate > 0) {
            const uint64_t vblank = 1'000'000'000'000ULL / static_cast<uint64_t>(m_refreshRate);
            cap = nanoseconds{static_cast<int64_t>(vblank * 3)};
        }

        diff = std::min(diff, cap);
        m_renderLoop->setPresentationSafetyMargin(diff + 1ms);
    }

    frame.outputFrame->presented(timestamp, PresentationMode::VSync);

    m_frames[m_frameHead] = FrameData();
    m_frameHead = (m_frameHead + 1) % FrameQueueCapacity;
    --m_frameCount;
}

void WaylandOutput::applyChanges(const OutputConfiguration &config)
{
    const auto props = config.constChangeSet(this);
    if (!props) {
        return;
    }

    State next = m_state;
    next.enabled = props->enabled.value_or(m_state.enabled);
    next.transform = props->transform.value_or(m_state.transform);
    next.position = props->pos.value_or(m_state.position);
    next.desiredModeSize = props->desiredModeSize.value_or(m_state.desiredModeSize);
    next.desiredModeRefreshRate = props->desiredModeRefreshRate.value_or(m_state.desiredModeRefreshRate);
    next.uuid = props->uuid.value_or(m_state.uuid);
    next.replicationSource = props->replicationSource.value_or(m_state.replicationSource);
    setState(next);
}

bool WaylandOutput::isReady() const
{
    return m_ready;
}

KWayland::Client::Surface *WaylandOutput::surface() const
{
    return m_surface.get();
}

WaylandCursor *WaylandOutput::cursor() const
{
    return m_cursor.get();
}

WaylandBackend *WaylandOutput::backend() const
{
    return m_backend;
}

RenderLoop *WaylandOutput::renderLoop() const
{
    return m_renderLoop.get();
}

bool WaylandOutput::presentAsync(OutputLayer *layer,
                                 std::optional<std::chrono::nanoseconds> allowedVrrDelay)
{
    Q_UNUSED(allowedVrrDelay)
    return layer->type() == OutputLayerType::CursorOnly;
}

void WaylandOutput::init(const QSize &pixelSize, qreal scale, bool fullscreen)
{
    m_renderLoop->setRefreshRate(m_refreshRate);

    auto mode = std::make_shared<OutputMode>(pixelSize, m_refreshRate);

    State initialState{};
    initialState.modes = {mode};
    initialState.currentMode = mode;
    initialState.scale = scale;
    setState(initialState);

    m_cachedPixelSize = pixelSize;

    m_xdgShellSurface->setFullscreen(fullscreen);
    m_surface->commit(KWayland::Client::Surface::CommitFlag::None);
}

void WaylandOutput::setDpmsMode(DpmsMode mode)
{
    if (mode == DpmsMode::Off) {
        if (!m_turnOffTimer.isActive()) {
            Q_EMIT aboutToTurnOff(std::chrono::milliseconds(m_turnOffTimer.interval()));
            m_turnOffTimer.start();
        }
    } else {
        m_turnOffTimer.stop();
        if (mode != dpmsMode()) {
            updateDpmsMode(mode);
            Q_EMIT wakeUp();
        }
    }
}

void WaylandOutput::updateDpmsMode(DpmsMode dpmsMode)
{
    State next = m_state;
    next.dpmsMode = dpmsMode;
    setState(next);
}

void WaylandOutput::handleConfigure(const QSize &size,
                                    XdgShellSurface::States states,
                                    quint32 serial)
{
    Q_UNUSED(states)

    if (!m_ready) [[unlikely]] {
        m_ready = true;
        applyConfigure(size, serial);
    } else {
        m_pendingConfigureSerial = serial;
        m_pendingConfigureSize = size;

        if (!m_configureThrottleTimer.isActive()) {
            const auto *mode = m_state.currentMode.get();
            if (mode && mode->refreshRate() > 0) [[likely]] {
                m_configureThrottleTimer.start(1000000 / mode->refreshRate());
            } else {
                m_configureThrottleTimer.start(16);
            }
        }
    }
}

void WaylandOutput::applyConfigure(const QSize &size, quint32 serial)
{
    m_xdgShellSurface->ackConfigure(serial);

    if (!size.isEmpty()) [[likely]] {
        auto mode = std::make_shared<OutputMode>(size * m_pendingScale, m_refreshRate);

        State next = m_state;
        next.modes = {mode};
        next.currentMode = mode;
        next.scale = m_pendingScale;
        setState(next);

        m_cachedPixelSize = mode->size();

        Q_EMIT m_backend->outputsQueried();
    }
}

void WaylandOutput::updateWindowTitle()
{
    static const QString s_grabLocked = i18n("Press right control to ungrab pointer");
    static const QString s_grabAvailable = i18n("Press right control key to grab pointer");
    static const QString s_outputDisabled = i18n("- Output disabled");
    static const QString s_outputDimmed = i18n("- Output dimmed");

    QString grab;
    if (m_hasPointerLock) [[unlikely]] {
        grab = s_grabLocked;
    } else if (m_backend->display()->pointerConstraints()) {
        grab = s_grabAvailable;
    }

    QString title = i18nc("Title of nested KWin Wayland with Wayland socket identifier as argument",
                          "KDE Wayland Compositor %1", name());

    if (!isEnabled()) [[unlikely]] {
        title += QLatin1String(" ");
        title += s_outputDisabled;
    } else if (dpmsMode() != DpmsMode::On) [[unlikely]] {
        title += QLatin1String(" ");
        title += s_outputDimmed;
    } else if (!grab.isEmpty()) {
        title += QStringLiteral(" — ");
        title += grab;
    }

    if (title != m_cachedTitle) [[likely]] {
        m_cachedTitle = title;
        m_xdgShellSurface->setTitle(title);
    }
}

void WaylandOutput::lockPointer(Pointer *pointer, bool lock)
{
    if (!lock) {
        const bool surfaceWasLocked = m_pointerLock && m_hasPointerLock;
        m_pointerLock.reset();
        m_hasPointerLock = false;
        if (surfaceWasLocked) {
            updateWindowTitle();
            Q_EMIT m_backend->pointerLockChanged(false);
        }
        return;
    }

    Q_ASSERT(!m_pointerLock);
    m_pointerLock.reset(m_backend->display()->pointerConstraints()->lockPointer(
        surface(), pointer, nullptr, PointerConstraints::LifeTime::OneShot));

    if (!m_pointerLock->isValid()) [[unlikely]] {
        m_pointerLock.reset();
        return;
    }

    connect(m_pointerLock.get(), &LockedPointer::locked, this, [this]() {
        m_hasPointerLock = true;
        updateWindowTitle();
        Q_EMIT m_backend->pointerLockChanged(true);
    });

    connect(m_pointerLock.get(), &LockedPointer::unlocked, this, [this]() {
        m_pointerLock.reset();
        m_hasPointerLock = false;
        updateWindowTitle();
        Q_EMIT m_backend->pointerLockChanged(false);
    });
}

void WaylandOutput::setOutputLayers(std::vector<std::unique_ptr<OutputLayer>> &&layers)
{
    m_cursorLayer = nullptr;
    for (const auto &layer : layers) {
        if (layer->type() == OutputLayerType::CursorOnly) {
            m_cursorLayer = layer.get();
            break;
        }
    }
    m_layers = std::move(layers);
}

QList<OutputLayer *> WaylandOutput::outputLayers() const
{
    QList<OutputLayer *> result;
    result.reserve(static_cast<int>(m_layers.size()));
    for (const auto &layer : m_layers) {
        result.append(layer.get());
    }
    return result;
}

}
}

#include "moc_wayland_output.cpp"
