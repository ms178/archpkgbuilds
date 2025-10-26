/*
    SPDX-FileCopyrightText: 2021 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "scene/surfaceitem_wayland.h"
#include "core/drmdevice.h"
#include "core/renderbackend.h"
#include "wayland/linuxdmabufv1clientbuffer.h"
#include "wayland/subcompositor.h"
#include "wayland/surface.h"
#include "window.h"

#if KWIN_BUILD_X11
#include "x11window.h"
#endif

#include <algorithm>

namespace KWin
{

SurfaceItemWayland::SurfaceItemWayland(SurfaceInterface *surface, Item *parent)
    : SurfaceItem(parent)
    , m_surface(surface)
{
    // Signal/slot connections (cold path, called once per surface)
    connect(surface, &SurfaceInterface::sizeChanged,
            this, &SurfaceItemWayland::handleSurfaceSizeChanged);
    connect(surface, &SurfaceInterface::bufferChanged,
            this, &SurfaceItemWayland::handleBufferChanged);
    connect(surface, &SurfaceInterface::bufferSourceBoxChanged,
            this, &SurfaceItemWayland::handleBufferSourceBoxChanged);
    connect(surface, &SurfaceInterface::bufferTransformChanged,
            this, &SurfaceItemWayland::handleBufferTransformChanged);

    connect(surface, &SurfaceInterface::childSubSurfacesChanged,
            this, &SurfaceItemWayland::handleChildSubSurfacesChanged);
    connect(surface, &SurfaceInterface::committed,
            this, &SurfaceItemWayland::handleSurfaceCommitted);
    connect(surface, &SurfaceInterface::damaged,
            this, &SurfaceItemWayland::addDamage);
    connect(surface, &SurfaceInterface::childSubSurfaceRemoved,
            this, &SurfaceItemWayland::handleChildSubSurfaceRemoved);
    connect(surface, &SurfaceInterface::colorDescriptionChanged,
            this, &SurfaceItemWayland::handleColorDescriptionChanged);
    connect(surface, &SurfaceInterface::presentationModeHintChanged,
            this, &SurfaceItemWayland::handlePresentationModeHintChanged);
    connect(surface, &SurfaceInterface::bufferReleasePointChanged,
            this, &SurfaceItemWayland::handleReleasePointChanged);
    connect(surface, &SurfaceInterface::alphaMultiplierChanged,
            this, &SurfaceItemWayland::handleAlphaMultiplierChanged);

    connect(surface, &SurfaceInterface::mapped,
            this, &SurfaceItemWayland::handleSurfaceMappedChanged);
    connect(surface, &SurfaceInterface::unmapped,
            this, &SurfaceItemWayland::handleSurfaceMappedChanged);
    setVisible(surface->isMapped());

    SubSurfaceInterface *subsurface = surface->subSurface();
    if (subsurface) {
        connect(subsurface, &SubSurfaceInterface::positionChanged,
                this, &SurfaceItemWayland::handleSubSurfacePositionChanged);
        setPosition(subsurface->position());
    }

    // Initialize state from surface (called once, not performance-critical)
    handleChildSubSurfacesChanged();
    setDestinationSize(surface->size());
    setBufferTransform(surface->bufferTransform());
    setBufferSourceBox(surface->bufferSourceBox());
    setBuffer(surface->buffer());
    m_bufferReleasePoint = m_surface->bufferReleasePoint();
    setColorDescription(surface->colorDescription());
    setRenderingIntent(surface->renderingIntent());
    setPresentationHint(surface->presentationModeHint());
    setOpacity(surface->alphaMultiplier());

    // FIFO fallback timer setup
    // Default interval (20 Hz = 50ms) will be dynamically adjusted based on
    // actual output refresh rate in handleFramePainted()
    m_fifoFallbackTimer.setInterval(50);
    m_fifoFallbackTimer.setSingleShot(true);
    connect(&m_fifoFallbackTimer, &QTimer::timeout, this, &SurfaceItemWayland::handleFifoFallback);
}

QList<QRectF> SurfaceItemWayland::shape() const
{
    return {rect()};
}

QRegion SurfaceItemWayland::opaque() const
{
    if (m_surface) [[likely]] {
        return m_surface->opaque();
    }
    return QRegion();
}

SurfaceInterface *SurfaceItemWayland::surface() const
{
    return m_surface;
}

void SurfaceItemWayland::handleSurfaceSizeChanged()
{
    setDestinationSize(m_surface->size());
}

void SurfaceItemWayland::handleBufferChanged()
{
    setBuffer(m_surface->buffer());
}

void SurfaceItemWayland::handleBufferSourceBoxChanged()
{
    setBufferSourceBox(m_surface->bufferSourceBox());
}

void SurfaceItemWayland::handleBufferTransformChanged()
{
    setBufferTransform(m_surface->bufferTransform());
}

void SurfaceItemWayland::handleSurfaceCommitted()
{
    if (m_surface->hasFifoBarrier()) {
        m_fifoFallbackTimer.start();
    }
    if (m_surface->hasFrameCallbacks() || m_surface->hasFifoBarrier() || m_surface->hasPresentationFeedback()) {
        scheduleFrame();
    }
}

SurfaceItemWayland *SurfaceItemWayland::getOrCreateSubSurfaceItem(SubSurfaceInterface *child)
{
    auto &item = m_subsurfaces[child];
    if (!item) [[unlikely]] {  // Branch hint: subsurfaces usually already exist after first commit
        item = std::make_unique<SurfaceItemWayland>(child->surface(), this);
    }
    return item.get();
}

void SurfaceItemWayland::handleChildSubSurfaceRemoved(SubSurfaceInterface *child)
{
    m_subsurfaces.erase(child);
}

void SurfaceItemWayland::handleChildSubSurfacesChanged()
{
    const auto &below = m_surface->below();
    const auto &above = m_surface->above();

    // Z-order assignment for below subsurfaces (negative Z values)
    // Example: 3 below items → Z values: -3, -2, -1
    const int belowCount = below.count();
    for (int i = 0; i < belowCount; ++i) {
        SurfaceItemWayland *subsurfaceItem = getOrCreateSubSurfaceItem(below[i]);
        subsurfaceItem->setZ(i - belowCount);
    }

    // Z-order assignment for above subsurfaces (non-negative Z values)
    // Example: 2 above items → Z values: 0, 1
    const int aboveCount = above.count();
    for (int i = 0; i < aboveCount; ++i) {
        SurfaceItemWayland *subsurfaceItem = getOrCreateSubSurfaceItem(above[i]);
        subsurfaceItem->setZ(i);
    }
}

void SurfaceItemWayland::handleSubSurfacePositionChanged()
{
    setPosition(m_surface->subSurface()->position());
}

void SurfaceItemWayland::handleSurfaceMappedChanged()
{
    setVisible(m_surface->isMapped());
}

ContentType SurfaceItemWayland::contentType() const
{
    return m_surface ? m_surface->contentType() : ContentType::None;
}

void SurfaceItemWayland::setScanoutHint(DrmDevice *device, const QHash<uint32_t, QList<uint64_t>> &drmFormats)
{
    if (!m_surface || !m_surface->dmabufFeedbackV1()) [[unlikely]] {
        return;
    }

    if (!device && m_scanoutFeedback.has_value()) {
        // Device removed: clear scanout feedback
        m_surface->dmabufFeedbackV1()->setTranches({});
        m_scanoutFeedback.reset();
        return;
    }

    if (!m_scanoutFeedback || m_scanoutFeedback->device != device || m_scanoutFeedback->formats != drmFormats) {
        m_scanoutFeedback = ScanoutFeedback{
            .device = device,
            .formats = drmFormats,
        };
        m_surface->dmabufFeedbackV1()->setScanoutTranches(device, drmFormats);
    }
}

void SurfaceItemWayland::freeze()
{
    if (!m_surface) [[unlikely]] {
        return;
    }

    m_fifoFallbackTimer.stop();

    m_surface->disconnect(this);
    if (auto subsurface = m_surface->subSurface()) {
        subsurface->disconnect(this);
    }

    // Recursively freeze all subsurfaces (structured binding for clarity)
    for (auto &[subsurface, subsurfaceItem] : m_subsurfaces) {
        subsurfaceItem->freeze();
    }

    m_surface = nullptr;
}

void SurfaceItemWayland::handleColorDescriptionChanged()
{
    setColorDescription(m_surface->colorDescription());
    setRenderingIntent(m_surface->renderingIntent());
}

void SurfaceItemWayland::handlePresentationModeHintChanged()
{
    setPresentationHint(m_surface->presentationModeHint());
}

void SurfaceItemWayland::handleReleasePointChanged()
{
    m_bufferReleasePoint = m_surface->bufferReleasePoint();
}

void SurfaceItemWayland::handleAlphaMultiplierChanged()
{
    setOpacity(m_surface->alphaMultiplier());
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((hot))
#endif
void SurfaceItemWayland::handleFramePainted(Output *output, OutputFrame *frame, std::chrono::milliseconds timestamp)
{
    if (!m_surface) [[unlikely]] {
        return;
    }

    m_surface->frameRendered(timestamp.count());

    if (frame) [[likely]] {

        if (auto feedback = m_surface->presentationFeedback(output)) {
            frame->addFeedback(std::move(feedback));
        }
    }

    m_surface->clearFifoBarrier();

    if (m_fifoFallbackTimer.isActive() && output) [[likely]] {

        const int currentRefreshRate = output->refreshRate(); // mHz (e.g., 60000 = 60.000 Hz)

        if (currentRefreshRate != m_lastRefreshRate) [[unlikely]] {
            m_lastRefreshRate = currentRefreshRate;

            if (currentRefreshRate > 0) [[likely]] {
                // Calculate fallback duration: max(1.25× refresh, 30 Hz minimum)
                // Use uint64_t to prevent overflow (max KWin refreshRate: ~1000000 mHz = 1000 Hz)
                // 1 second = 1,000,000,000 ns; KWin uses mHz (1 Hz = 1000 mHz)
                const uint64_t refreshNs = 1'000'000'000'000ULL / static_cast<uint64_t>(currentRefreshRate);

                // Fallback: 1.25× refresh interval (allows some slack for frame time variance)
                // but never slower than 30 Hz (33,333,333 ns) to avoid unplayable framerates
                const uint64_t fallbackMinNs = 33'333'333ULL;  // 30 Hz floor
                const uint64_t fallbackNs = std::max((refreshNs * 5) / 4, fallbackMinNs);

                // Convert to milliseconds (ceiling division to avoid premature timeout)
                m_cachedFifoFallbackMs = static_cast<int>((fallbackNs + 999'999) / 1'000'000);
            } else {
                // Defensive: Invalid refresh rate (0 or negative)
                // Fallback to 20 Hz (50ms) to ensure timer eventually fires
                m_cachedFifoFallbackMs = 50;
            }
        }

        // Fast path: Use cached value (hit 99.9% of the time)
        // QTimer::start() with same interval is internally optimized (no-op if already scheduled)
        m_fifoFallbackTimer.start(m_cachedFifoFallbackMs);
    }
}

void SurfaceItemWayland::handleFifoFallback()
{
    if (m_surface) [[likely]] {
        m_surface->clearFifoBarrier();
    }
}

#if KWIN_BUILD_X11
SurfaceItemXwayland::SurfaceItemXwayland(X11Window *window, Item *parent)
    : SurfaceItemWayland(window->surface(), parent)
    , m_window(window)
{
    connect(window, &X11Window::shapeChanged, this, &SurfaceItemXwayland::handleShapeChange);
}

void SurfaceItemXwayland::handleShapeChange()
{
    if (!m_window) [[unlikely]] {
        return;
    }

    const auto newShape = m_window->shapeRegion();
    QRegion newBufferShape;
    for (const auto &rect : newShape) {
        newBufferShape |= rect.toAlignedRect();
    }

    scheduleRepaint(newBufferShape.xored(m_previousBufferShape));

    // Update cached shape for next delta computation
    m_previousBufferShape = newBufferShape;

    // Discard cached quads (shape changed, so tessellation is invalid)
    discardQuads();
}

QList<QRectF> SurfaceItemXwayland::shape() const
{
    if (!m_window) [[unlikely]] {
        return {};
    }

    auto shape = m_window->shapeRegion();
    const QRectF itemRect = rect();

    for (QRectF &shapePart : shape) {
        shapePart = shapePart.intersected(itemRect);
    }

    return shape;
}

QRegion SurfaceItemXwayland::opaque() const
{
    if (!m_window) [[unlikely]] {
        return QRegion();
    }

    QRegion shapeRegion;
    for (const QRectF &shapePart : shape()) {
        shapeRegion += shapePart.toRect();
    }

    if (!m_window->hasAlpha()) {
        return shapeRegion;
    } else {
        return m_window->opaqueRegion() & shapeRegion;
    }
}
#endif

} // namespace KWin

#include "moc_surfaceitem_wayland.cpp"
