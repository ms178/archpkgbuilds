/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "effect/globals.h"

#include <QObject>

namespace KWin
{

class RenderLoopPrivate;
class SurfaceItem;
class Item;
class Output;
class OutputLayer;

class KWIN_EXPORT RenderLoop : public QObject
{
    Q_OBJECT

public:
    explicit RenderLoop(Output *output);
    ~RenderLoop() override;

    void inhibit();
    void uninhibit();
    void prepareNewFrame();
    void newFramePrepared();

    int refreshRate() const;
    void setRefreshRate(int refreshRate);
    void setPresentationSafetyMargin(std::chrono::nanoseconds safetyMargin);
    void scheduleRepaint(Item *item = nullptr, OutputLayer *outputLayer = nullptr);

    std::chrono::nanoseconds lastPresentationTimestamp() const;
    std::chrono::nanoseconds nextPresentationTimestamp() const;

    void setPresentationMode(PresentationMode mode);
    void setMaxPendingFrameCount(uint32_t maxCount);
    std::chrono::nanoseconds predictedRenderTime() const;
    bool activeWindowControlsVrrRefreshRate() const;

    void timerEvent(QTimerEvent *event) override;

Q_SIGNALS:
    void refreshRateChanged();
    void framePresented(RenderLoop *loop, std::chrono::nanoseconds timestamp, PresentationMode mode);
    void frameRequested(RenderLoop *loop);

private:
    std::unique_ptr<RenderLoopPrivate> d;
    friend class RenderLoopPrivate;
};

} // namespace KWin
