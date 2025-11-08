/*
    KWin - the KDE window manager
    This file is part of the KDE project.

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
    uint8_t reciprocalShift;
    uint8_t reciprocalShift64;
    PresentationMode presentationMode = PresentationMode::VSync;
    uint8_t vrrStabilityCounter = 0;
    bool pendingReschedule = false;
    bool wasTripleBuffering = false;
    int16_t scheduledTimerMs = -1;
    int16_t doubleBufferingCounter = 0;
    int pendingFrameCount = 0;
    int maxPendingFrameCount = 1;
    uint8_t padding0[8];

    RenderLoop *const q;
    Output *const output;
    std::chrono::nanoseconds safetyMargin{0};
    int refreshRate = 60'000;
    bool preparingNewFrame = false;
    bool vrrCapable = false;
    bool vrrEnabled = false;
    enum class VrrMode : uint8_t {
        Automatic,
        Always,
        Never
    };
    VrrMode vrrMode = VrrMode::Automatic;
    int inhibitCount = 0;
    uint8_t padding1[20];

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
    uint8_t padding2[60];

    alignas(64) QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;
    RenderJournal renderJournal;
    std::optional<std::fstream> m_debugOutput;

    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop *loop) noexcept;
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);

    void updateReciprocal() noexcept;
    void initializeVrrCapabilities();
    void updateVrrContext();
    [[nodiscard]] PresentationMode selectPresentationModeFromContext() const noexcept;
    void dispatch();
    void delayScheduleRepaint() noexcept;
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

static_assert(sizeof(RenderLoopPrivate::VrrContext) == 64, "VrrContext must be exactly one cache line");

} // namespace KWin
