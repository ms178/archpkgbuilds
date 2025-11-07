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
class Window;

class alignas(64) KWIN_EXPORT RenderLoopPrivate {
public:
    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    uint64_t cachedVblankIntervalNs;
    uint64_t vblankIntervalReciprocal;
    uint64_t vblankIntervalReciprocal64;
    RenderLoop *const q;
    Output *const output;
    std::chrono::nanoseconds safetyMargin{0};

    PresentationMode presentationMode = PresentationMode::VSync;
    int pendingFrameCount = 0;
    int inhibitCount = 0;
    int maxPendingFrameCount = 1;
    int refreshRate = 60'000;
    int16_t scheduledTimerMs = -1;
    int16_t doubleBufferingCounter = 0;
    uint8_t reciprocalShift;
    uint8_t reciprocalShift64;
    uint8_t vrrStabilityCounter = 0;
    bool pendingReschedule = false;
    bool wasTripleBuffering = false;
    bool preparingNewFrame = false;
    bool vrrCapable = false;
    bool vrrEnabled = false;
    uint8_t padding1[32];

    enum class VrrMode : uint8_t {
        Automatic,
        Always,
        Never
    };
    VrrMode vrrMode = VrrMode::Automatic;
    uint8_t padding2[63];

    struct alignas(64) VrrContext {
        Window *cachedActiveWindow = nullptr;
        SurfaceItem *cachedSurfaceItem = nullptr;
        PresentationModeHint cachedHint = PresentationModeHint::VSync;
        ContentType cachedContentType = ContentType::None;
        uint32_t generation = 0;
        bool cachedIsFullScreen = false;
        bool cachedIsOnOutput = false;
        uint8_t padding[34];
    } vrrCtx;
    uint32_t vrrCtxGeneration = 0;
    uint8_t padding3[60];

    alignas(64) QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;

    RenderJournal renderJournal;
    std::optional<std::fstream> m_debugOutput;

    static RenderLoopPrivate *get(RenderLoop *loop);
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);

    void updateReciprocal();
    void initializeVrrCapabilities();
    void updateVrrContext();
    PresentationMode selectPresentationModeFromContext() const;
    void dispatch();
    void delayScheduleRepaint();
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

static_assert(sizeof(RenderLoopPrivate::VrrContext) == 64, "VrrContext must be exactly one cache line");

}
