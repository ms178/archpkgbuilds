/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "config-kwin.h"
#include "scene/surfaceitem.h"

#include <QHash>
#include <QTimer>
#include <chrono>
#include <cstdint>
#include <memory>
#include <optional>

namespace KWin
{

class DrmDevice;
class SubSurfaceInterface;
class SurfaceInterface;
class SyncReleasePoint;

#if KWIN_BUILD_X11
class X11Window;
#endif

class KWIN_EXPORT SurfaceItemWayland : public SurfaceItem
{
    Q_OBJECT

public:
    explicit SurfaceItemWayland(SurfaceInterface *surface, Item *parent = nullptr);

    QList<QRectF> shape() const override;
    QRegion opaque() const override;
    ContentType contentType() const override;

    SurfaceInterface *surface() const;

    void setScanoutHint(DrmDevice *device, const QHash<uint32_t, QList<uint64_t>> &drmFormats);
    void freeze();

protected:
    void handleFramePainted(Output *output, OutputFrame *frame, std::chrono::milliseconds timestamp) override;

private:
    SurfaceItemWayland *getOrCreateSubSurfaceItem(SubSurfaceInterface *child);

    void handleSurfaceSizeChanged();
    void handleBufferChanged();
    void handleBufferSourceBoxChanged();
    void handleBufferTransformChanged();
    void handleSurfaceCommitted();
    void handleChildSubSurfaceRemoved(SubSurfaceInterface *child);
    void handleChildSubSurfacesChanged();
    void handleSubSurfacePositionChanged();
    void handleSurfaceMappedChanged();
    void handleColorDescriptionChanged();
    void handlePresentationModeHintChanged();
    void handleReleasePointChanged();
    void handleAlphaMultiplierChanged();
    void handleFifoFallback();

    alignas(64) SurfaceInterface *m_surface;
    QTimer m_fifoFallbackTimer;

    std::chrono::steady_clock::time_point m_lastCommitTime;
    std::chrono::steady_clock::time_point m_lastPaintTime;

    std::shared_ptr<SyncReleasePoint> m_bufferReleasePoint;

    int m_cachedFifoFallbackMs = 17;
    int m_lastRefreshRate = 0;
    int m_estimatedContentRate = 0;
    uint16_t m_consecutiveStableFrames = 0;

    QHash<SubSurfaceInterface *, SurfaceItemWayland *> m_subsurfaces;

    struct ScanoutFeedback
    {
        DrmDevice *device;
        QHash<uint32_t, QList<uint64_t>> formats;
    };
    std::optional<ScanoutFeedback> m_scanoutFeedback;
};

#if KWIN_BUILD_X11
class KWIN_EXPORT SurfaceItemXwayland : public SurfaceItemWayland
{
    Q_OBJECT

public:
    explicit SurfaceItemXwayland(X11Window *window, Item *parent = nullptr);

    QList<QRectF> shape() const override;
    QRegion opaque() const override;

private:
    void handleShapeChange();

    X11Window *m_window;
    QRegion m_previousBufferShape;
};
#endif

}
