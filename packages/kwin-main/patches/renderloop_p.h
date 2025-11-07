/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "renderbackend.h"
#include "renderjournal.h"
#include "renderloop.h"

#include <QBasicTimer>

#include <fstream>
#include <optional>
#include <cstdint>

namespace KWin
{

class SurfaceItem;
class OutputFrame;

class alignas(64) KWIN_EXPORT RenderLoopPrivate {
public:
    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    uint64_t cachedVblankIntervalNs;
    uint64_t vblankIntervalReciprocal;
    uint8_t reciprocalShift;
    int pendingFrameCount = 0;
    int inhibitCount = 0;
    PresentationMode presentationMode = PresentationMode::VSync;
    int16_t scheduledTimerMs = -1;
    int16_t doubleBufferingCounter = 0;
    bool pendingReschedule = false;
    bool wasTripleBuffering = false;
    bool preparingNewFrame = false;

    bool vrrCapable = false;
    bool vrrEnabled = false;
    enum class VrrMode : uint8_t {
        Automatic,
        Always,
        Never
    };
    VrrMode vrrMode = VrrMode::Automatic;
    uint8_t vrrStabilityCounter = 0;

    uint8_t padding[3];

    RenderLoop *const q;
    Output *const output;
    std::chrono::nanoseconds safetyMargin{0};
    int maxPendingFrameCount = 1;
    int refreshRate = 60'000;
    QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;

    RenderJournal renderJournal;
    std::optional<std::fstream> m_debugOutput;

    static RenderLoopPrivate *get(RenderLoop *loop);
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);

    void updateReciprocal();
    void initializeVrrCapabilities();
    bool shouldEngageVrr() const;
    PresentationMode selectPresentationMode() const;
    void dispatch();
    void delayScheduleRepaint();
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

}
