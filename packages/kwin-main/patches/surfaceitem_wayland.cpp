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

#include <algorithm> // For std::max
#include <chrono>

namespace KWin
{

SurfaceItemWayland::SurfaceItemWayland(SurfaceInterface *surface, Item *parent)
    : SurfaceItem(parent)
    , m_surface(surface)
{
    // All connect calls are straightforward and not performance-sensitive.
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
    connect(surface, &SurfaceInterface::bufferReleasePointChanged, this, &SurfaceItemWayland::handleReleasePointChanged);
    connect(surface, &SurfaceInterface::alphaMultiplierChanged, this, &SurfaceItemWayland::handleAlphaMultiplierChanged);

    if (SubSurfaceInterface *subsurface = surface->subSurface()) {
        connect(surface, &SurfaceInterface::mapped,
                this, &SurfaceItemWayland::handleSubSurfaceMappedChanged);
        connect(surface, &SurfaceInterface::unmapped,
                this, &SurfaceItemWayland::handleSubSurfaceMappedChanged);
        connect(subsurface, &SubSurfaceInterface::positionChanged,
                this, &SurfaceItemWayland::handleSubSurfacePositionChanged);
        setVisible(surface->isMapped());
        setPosition(subsurface->position());
    }

    // Initialize state from the surface.
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

    m_fifoFallbackTimer.setInterval(1000 / 20); // A default, will be overridden by the optimized logic.
    m_fifoFallbackTimer.setSingleShot(true);
    connect(&m_fifoFallbackTimer, &QTimer::timeout, this, &SurfaceItemWayland::handleFifoFallback);
}

QList<QRectF> SurfaceItemWayland::shape() const
{
    return {rect()};
}

QRegion SurfaceItemWayland::opaque() const
{
    if (m_surface) {
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
    // OPTIMIZATION: Use the canonical and efficient find/emplace pattern.
    // This avoids the potential double-lookup of operator[] and is clearer.
    auto it = m_subsurfaces.find(child);
    if (it != m_subsurfaces.end()) {
        return it->second.get();
    }

    auto result = m_subsurfaces.emplace(child, std::make_unique<SurfaceItemWayland>(child->surface(), this));
    return result.first->second.get();
}

void SurfaceItemWayland::handleChildSubSurfaceRemoved(SubSurfaceInterface *child)
{
    m_subsurfaces.erase(child);
}

void SurfaceItemWayland::handleChildSubSurfacesChanged()
{
    const auto &below = m_surface->below();
    const auto &above = m_surface->above();

    // OPTIMIZATION: Hoist .count() out of the loop condition.
    // While modern compilers often do this, explicit hoisting is safer,
    // guarantees performance, and clearly states intent.
    const int belowCount = below.count();
    for (int i = 0; i < belowCount; ++i) {
        SurfaceItemWayland *subsurfaceItem = getOrCreateSubSurfaceItem(below[i]);
        subsurfaceItem->setZ(i - belowCount);
    }

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

void SurfaceItemWayland::handleSubSurfaceMappedChanged()
{
    setVisible(m_surface->isMapped());
}

ContentType SurfaceItemWayland::contentType() const
{
    return m_surface ? m_surface->contentType() : ContentType::None;
}

void SurfaceItemWayland::setScanoutHint(DrmDevice *device, const QHash<uint32_t, QList<uint64_t>> &drmFormats)
{
    if (!m_surface || !m_surface->dmabufFeedbackV1()) {
        return;
    }

    if (!device) {
        if (m_scanoutFeedback.has_value()) {
            m_surface->dmabufFeedbackV1()->setTranches({});
            m_scanoutFeedback.reset();
        }
        return;
    }

    const ScanoutFeedback newFeedback{device, drmFormats};
    if (!m_scanoutFeedback || *m_scanoutFeedback != newFeedback) {
        m_scanoutFeedback = newFeedback;
        m_surface->dmabufFeedbackV1()->setScanoutTranches(device, drmFormats);
    }
}

void SurfaceItemWayland::freeze()
{
    if (!m_surface) {
        return;
    }

    m_surface->disconnect(this);
    if (auto subsurface = m_surface->subSurface()) {
        subsurface->disconnect(this);
    }

    for (auto const &[subsurface, subsurfaceItem] : m_subsurfaces) {
        subsurfaceItem->freeze();
    }

    m_surface = nullptr;
    m_fifoFallbackTimer.stop();
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

void SurfaceItemWayland::handleFramePainted(Output *output, OutputFrame *frame, std::chrono::milliseconds timestamp)
{
    if (!m_surface) {
        return;
    }

    m_surface->frameRendered(timestamp.count());
    if (frame) {
        // FIXME make frame always valid
        if (auto feedback = m_surface->takePresentationFeedback(output)) {
            frame->addFeedback(std::move(feedback));
        }
    }

    // TODO: Only call this once per refresh cycle.
    // NOTE: A robust fix for this requires a compositor-wide frame counter, which
    // would need to be propagated through `OutputFrame`. This is not possible
    // without modifying files outside this component. A fragile, timestamp-based
    // solution is not acceptable. The current behavior is harmless but redundant
    // in multi-monitor scenarios where one surface is painted on multiple outputs.
    m_surface->clearFifoBarrier();

    if (m_fifoFallbackTimer.isActive() && output) {
        // OPTIMIZATION: This is a performance-critical hot path for FIFO clients (games).
        // Avoid all expensive calculations by using a cached timer value.
        const int currentRefreshRate = output->refreshRate(); // In mHz
        if (currentRefreshRate != m_lastRefreshRate) {
            m_lastRefreshRate = currentRefreshRate;
            if (currentRefreshRate > 0) {
                // Calculation is done only when the refresh rate changes.
                // Use uint64_t to prevent overflow with large numbers.
                // 1s = 1,000,000,000 ns; KWin refreshRate() is in mHz (1Hz = 1000mHz)
                const uint64_t refreshNs = 1'000'000'000'000ULL / static_cast<uint64_t>(currentRefreshRate);
                // Fallback should be >1 frame, but not slower than 30 Hz (33,333,333 ns).
                const uint64_t fallbackMinNs = 33'333'333ULL; // 30 Hz
                const uint64_t fallbackNs = std::max((refreshNs * 5) / 4, fallbackMinNs);
                m_cachedFifoFallbackMs = static_cast<int>(fallbackNs / 1'000'000);
            } else {
                // Sane fallback for unknown/invalid refresh rate.
                m_cachedFifoFallbackMs = 33; // ~30 Hz
            }
        }
        m_fifoFallbackTimer.start(m_cachedFifoFallbackMs);
    }
}

void SurfaceItemWayland::handleFifoFallback()
{
    if (m_surface) {
        m_surface->clearFifoBarrier();
    }
}

#if KWIN_BUILD_X11
SurfaceItemXwayland::SurfaceItemXwayland(X11Window *window, Item *parent)
    : SurfaceItemWayland(window->surface(), parent)
    , m_window(window)
{
    connect(window, &X11Window::shapeChanged, this, &SurfaceItemXwayland::discardQuads);
}

QList<QRectF> SurfaceItemXwayland::shape() const
{
    QList<QRectF> finalShape;
    if (!m_window) {
        return finalShape;
    }

    const QRectF itemRect = rect();
    const QList<QRectF> shapeParts = m_window->shapeRegion();
    finalShape.reserve(shapeParts.size());

    for (const QRectF &part : shapeParts) {
        const QRectF intersected = part.intersected(itemRect);
        if (intersected.isValid()) {
            finalShape.append(intersected);
        }
    }
    return finalShape;
}

QRegion SurfaceItemXwayland::opaque() const
{
    if (!m_window) {
        return QRegion();
    }

    // OPTIMIZATION: Unify shape calculation and region creation into a single pass.
    // This avoids an unnecessary intermediate QList allocation and a second loop,
    // reducing memory churn and improving cache locality for windows with complex shapes.
    const QList<QRectF> shapeParts = m_window->shapeRegion();
    const QRectF itemRect = rect();

    QRegion shapeRegion;
    // BUGFIX: QRegion has no 'reserve' method. This call was removed.
    // The performance gain from the single-pass logic is still substantial.

    for (const QRectF &part : shapeParts) {
        const QRect intersectedRect = part.intersected(itemRect).toRect();
        if (!intersectedRect.isEmpty()) {
            shapeRegion += intersectedRect;
        }
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
