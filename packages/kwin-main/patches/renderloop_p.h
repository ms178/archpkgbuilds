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
#include <array>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <memory>
#include <optional>
namespace KWin
{
class SurfaceItem;
class OutputFrame;
class Window;

struct VrrHistoryEntry {
    std::chrono::steady_clock::time_point timestamp{};
};

class KWIN_EXPORT RenderLoopPrivate {
public:
    RenderLoop *const q;
    Output *const output;
    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    std::chrono::nanoseconds safetyMargin{0};
    uint64_t cachedVblankIntervalNs;
    uint64_t vblankIntervalReciprocal64;
    uint64_t vblankIntervalReciprocal;
    uint64_t nsToMsReciprocal;
    int64_t tripleBufferEnterThresholdNs;
    int64_t tripleBufferExitThresholdNs;
    int refreshRate = 60'000;
    int pendingFrameCount = 0;
    int maxPendingFrameCount = 1;
    int inhibitCount = 0;
    int16_t scheduledTimerMs = -1;
    int16_t doubleBufferingCounter = 0;
    uint16_t stableModeFrameCount = 0;
    uint8_t reciprocalShift64;
    uint8_t reciprocalShift;
    uint8_t nsToMsShift;
    uint8_t vrrStabilityCounter = 0;
    uint8_t recentSwitchCount = 0;
    uint8_t modeSwitchHistoryIndex = 0;
    PresentationMode presentationMode = PresentationMode::VSync;
    PresentationMode lastStableMode = PresentationMode::VSync;
    bool preparingNewFrame = false;
    bool vrrCapable = false;
    bool vrrEnabled = false;
    bool pendingReschedule = false;
    bool wasTripleBuffering = false;
    bool vrrOscillationLockout = false;
    enum class VrrMode : uint8_t {
        Automatic,
        Always,
        Never
    };
    VrrMode vrrMode = VrrMode::Automatic;
    std::chrono::steady_clock::time_point lastModeSwitch{};
    alignas(64) std::array<VrrHistoryEntry, 8> modeSwitchHistory{};
    QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;
    RenderJournal renderJournal;
    std::optional<std::fstream> m_debugOutput;

    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop *loop) noexcept;
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);
    void updateReciprocal() noexcept;
    void initializeVrrCapabilities();
    [[nodiscard]] PresentationMode selectPresentationMode(PresentationModeHint hint, bool isOnOutput, bool isFullScreen) noexcept;
    [[nodiscard]] bool detectVrrOscillation() noexcept;
    void recordModeSwitch() noexcept;
    void dispatch();
    void delayScheduleRepaint() noexcept;
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};
}
