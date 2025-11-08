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

    m_fifoFallbackTimer.setInterval(m_cachedFifoFallbackMs);
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
    const bool hasBarrier = m_surface->hasFifoBarrier();

    if (hasBarrier) [[unlikely]] {
        m_lastCommitTime = std::chrono::steady_clock::now();
        m_fifoFallbackTimer.start(m_cachedFifoFallbackMs);
    }

    if (m_surface->hasFrameCallbacks() || hasBarrier || m_surface->hasPresentationFeedback()) [[likely]] {
        scheduleFrame();
    }
}

SurfaceItemWayland *SurfaceItemWayland::getOrCreateSubSurfaceItem(SubSurfaceInterface *child)
{
    SurfaceItemWayland *&item = m_subsurfaces[child];
    if (!item) [[unlikely]] {
        item = new SurfaceItemWayland(child->surface(), this);
    }
    return item;
}

void SurfaceItemWayland::handleChildSubSurfaceRemoved(SubSurfaceInterface *child)
{
    delete m_subsurfaces.take(child);
}

void SurfaceItemWayland::handleChildSubSurfacesChanged()
{
    const auto &below = m_surface->below();
    const auto &above = m_surface->above();

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

void SurfaceItemWayland::handleSurfaceMappedChanged()
{
    setVisible(m_surface->isMapped());
}

ContentType SurfaceItemWayland::contentType() const
{
    if (!m_surface) [[unlikely]] {
        return ContentType::None;
    }

    const auto surfaceContentType = m_surface->contentType();
    if (surfaceContentType != ContentType::None) [[unlikely]] {
        return surfaceContentType;
    }

    if (m_consecutiveStableFrames >= 5 && m_estimatedContentRate >= 24 && m_estimatedContentRate <= 120) [[unlikely]] {
        return ContentType::Video;
    }

    return ContentType::None;
}

void SurfaceItemWayland::setScanoutHint(DrmDevice *device, const QHash<uint32_t, QList<uint64_t>> &drmFormats)
{
    if (!m_surface || !m_surface->dmabufFeedbackV1()) [[unlikely]] {
        return;
    }

    if (!device && m_scanoutFeedback.has_value()) {
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

    qDeleteAll(m_subsurfaces);
    m_subsurfaces.clear();

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

    const bool hadBarrier = m_surface->hasFifoBarrier();
    if (hadBarrier) [[unlikely]] {
        m_surface->clearFifoBarrier();
        m_fifoFallbackTimer.stop();
    }

    if (!output) [[unlikely]] {
        return;
    }

    const auto now = std::chrono::steady_clock::now();
    m_lastPaintTime = now;

    const int currentRefreshRate = output->refreshRate();

    if (hadBarrier && m_lastCommitTime.time_since_epoch().count() > 0) [[unlikely]] {
        const auto intervalNs = std::chrono::duration_cast<std::chrono::nanoseconds>(now - m_lastCommitTime).count();

        if (intervalNs > 8'000'000 && intervalNs < 100'000'000) [[likely]] {
            const int instantRate = static_cast<int>(1'000'000'000 / intervalNs);

            if (instantRate >= 24 && instantRate <= currentRefreshRate) [[likely]] {
                if (m_estimatedContentRate == 0) [[unlikely]] {
                    m_estimatedContentRate = instantRate;
                    m_consecutiveStableFrames = 1;
                } else {
                    const int diff = std::abs(instantRate - m_estimatedContentRate);
                    if (diff <= 2) [[likely]] {
                        m_consecutiveStableFrames = std::min<uint16_t>(m_consecutiveStableFrames + 1, 100);
                        m_estimatedContentRate = (m_estimatedContentRate * 5 + instantRate * 3) / 8;
                    } else {
                        m_consecutiveStableFrames = 0;
                        m_estimatedContentRate = 0;
                    }
                }
            } else {
                m_consecutiveStableFrames = 0;
                m_estimatedContentRate = 0;
            }
        }
    }

    int effectiveRate = currentRefreshRate;
    if (m_consecutiveStableFrames >= 3 && m_estimatedContentRate >= 30 && m_estimatedContentRate < currentRefreshRate - 5) [[unlikely]] {
        effectiveRate = m_estimatedContentRate;
    }

    if (effectiveRate != m_lastRefreshRate || m_lastRefreshRate == 0) [[unlikely]] {
        m_lastRefreshRate = effectiveRate;

        const uint64_t refreshNs = 1'000'000'000'000ULL / static_cast<uint64_t>(effectiveRate);
        constexpr uint64_t fallbackMinNs = 16'666'667ULL;
        const uint64_t safetyMarginNs = std::max((refreshNs * 11) / 10, fallbackMinNs);
        m_cachedFifoFallbackMs = static_cast<int>((safetyMarginNs + 999'999) / 1'000'000);

        if (m_fifoFallbackTimer.isActive()) {
            m_fifoFallbackTimer.setInterval(m_cachedFifoFallbackMs);
        }
    }
}

void SurfaceItemWayland::handleFifoFallback()
{
    if (m_surface && m_surface->hasFifoBarrier()) [[likely]] {
        m_surface->clearFifoBarrier();
        m_consecutiveStableFrames = 0;
        m_estimatedContentRate = 0;
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
    if (newShape.isEmpty()) [[unlikely]] {
        if (m_previousBufferShape.isEmpty()) {
            return;
        }
        scheduleRepaint(m_previousBufferShape);
        m_previousBufferShape = QRegion();
        discardQuads();
        return;
    }

    QRegion newBufferShape;
    for (const QRectF &rect : newShape) {
        newBufferShape |= rect.toAlignedRect();
    }

    const QRegion damage = newBufferShape.xored(m_previousBufferShape);
    if (!damage.isEmpty()) [[likely]] {
        scheduleRepaint(damage);
        discardQuads();
    }

    m_previousBufferShape = newBufferShape;
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

}

#include "moc_surfaceitem_wayland.cpp"
