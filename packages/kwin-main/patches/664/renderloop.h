/*
SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "effect/globals.h"

#include <QObject>
#include <chrono>
#include <memory>
#include <optional>

namespace KWin
{

class RenderLoopPrivate;
class SurfaceItem;
class Item;
class BackendOutput;
class OutputLayer;

enum class VrrContentHint : uint8_t {
    Unknown = 0,
    Interactive = 1,
    Video = 2,
    Film = 3,
    ForceVsync = 4,
    ForceVrr = 5,
};

enum class VrrDecisionReason : uint8_t {
    None = 0,
    UserPolicy,
    ExplicitHint,
    FilmCadence,
    InteractiveGrace,
    OscillationGuard,
    BelowVrrFloor,
    AmbiguousKeepCurrent,
};

struct KWIN_EXPORT VrrCapabilities {
    int minRefreshRate = 0; // mHz
    int maxRefreshRate = 0; // mHz
    bool supportsLfc = false;
};

struct KWIN_EXPORT PresentFeedback {
    std::chrono::nanoseconds actualPresentationTimestamp{0};
    std::chrono::nanoseconds targetPresentationTimestamp{0};
    std::chrono::nanoseconds refreshDuration{0};
    bool deadlineMissed = false;
    bool directScanout = false;
    bool valid = false;
};

class KWIN_EXPORT RenderLoop : public QObject
{
    Q_OBJECT
public:
    explicit RenderLoop(BackendOutput *output);
    ~RenderLoop() override;

    void inhibit();
    void uninhibit();
    void prepareNewFrame();
    void newFramePrepared();

    [[nodiscard]] int refreshRate() const;
    void setRefreshRate(int refreshRate);
    void setPresentationSafetyMargin(std::chrono::nanoseconds safetyMargin);
    void scheduleRepaint(Item *item = nullptr, OutputLayer *outputLayer = nullptr);

    [[nodiscard]] std::chrono::nanoseconds lastPresentationTimestamp() const;
    [[nodiscard]] std::chrono::nanoseconds nextPresentationTimestamp() const;

    void setPresentationMode(PresentationMode mode);
    void setMaxPendingFrameCount(uint32_t maxCount);
    [[nodiscard]] std::chrono::nanoseconds predictedRenderTime() const;
    [[nodiscard]] bool activeWindowControlsVrrRefreshRate() const;
    void timerEvent(QTimerEvent *event) override;

    void setVrrCapabilities(const VrrCapabilities &caps);
    void setActiveContentHint(VrrContentHint hint,
                              std::optional<std::chrono::nanoseconds> nominalFrameInterval = std::nullopt);
    void notifyPresentFeedback(const PresentFeedback &feedback);

Q_SIGNALS:
    void refreshRateChanged();
    void framePresented(RenderLoop *loop, std::chrono::nanoseconds timestamp, PresentationMode mode);
    void frameRequested(RenderLoop *loop);
    void presentationModeChanged(RenderLoop *loop, PresentationMode mode, VrrDecisionReason reason);

private:
    std::unique_ptr<RenderLoopPrivate> d;
    friend class RenderLoopPrivate;
};

}

Q_DECLARE_METATYPE(KWin::VrrContentHint)
Q_DECLARE_METATYPE(KWin::VrrDecisionReason)
Q_DECLARE_METATYPE(KWin::VrrCapabilities)
Q_DECLARE_METATYPE(KWin::PresentFeedback)
