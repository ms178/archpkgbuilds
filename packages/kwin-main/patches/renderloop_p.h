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
#include <QMetaObject>
#include <array>
#include <chrono>
#include <cstdint>
#include <cstring>
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

class VrrStateCache {
public:
    union State {
        struct {
            uint8_t hint : 2;
            uint8_t isOnOutput : 1;
            uint8_t isFullScreen : 1;
            uint8_t valid : 1;
            uint8_t reserved : 3;
        };
        uint8_t raw;
        constexpr State() noexcept : raw(0) {}
    };
    static_assert(sizeof(State) == 1);

    void setState(State newState) noexcept {
        m_state = newState.raw;
    }

    [[nodiscard]] State getState() const noexcept {
        State s;
        s.raw = m_state;
        return s;
    }

private:
    uint8_t m_state{0};
};

class alignas(64) KWIN_EXPORT RenderLoopPrivate {
public:
    RenderLoop *const q;
    Output *const output;
    QBasicTimer compositeTimer;
    int32_t pendingFrameCount;
    int32_t inhibitCount;
    int32_t refreshRate;
    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    std::chrono::nanoseconds scheduledRenderTimestamp{0};
    uint64_t cachedVblankIntervalNs;

    uint64_t vblankIntervalReciprocal64;
    std::chrono::nanoseconds framePrediction{0};
    std::chrono::nanoseconds safetyMargin{0};
    int32_t maxPendingFrameCount;
    int16_t scheduledTimerMs;
    int16_t doubleBufferingCounter;
    uint8_t reciprocalShift64;

    uint8_t preparingNewFrame : 1;
    uint8_t pendingReschedule : 1;
    uint8_t wasTripleBuffering : 1;
    uint8_t vrrOscillationLockout : 1;
    uint8_t vrrEnabled : 1;
    uint8_t vrrCapable : 1;
    uint8_t : 2;

    uint8_t oscillationCheckCounter_;
    uint8_t consecutiveErrorCount;
    uint8_t vrrStabilityCounter;
    uint8_t recentSwitchCount;
    uint8_t modeSwitchHistoryIndex;
    uint8_t vrrConnectionCount_;

    enum class VrrMode : uint8_t { Automatic, Always, Never };
    VrrMode vrrMode;

    PresentationMode presentationMode;
    PresentationMode lastStableMode;

    int64_t tripleBufferEnterThresholdNs;
    int64_t tripleBufferExitThresholdNs;
    uint64_t vrrTargetIntervalNs;

    uint16_t stableModeFrameCount;
    uint16_t starvationRecoveryCounter;

    std::chrono::steady_clock::time_point lastModeSwitch{};

    VrrStateCache vrrStateCache_{};
    bool vrrStateDirty_;

    QBasicTimer delayedVrrTimer;
    RenderJournal renderJournal;

    std::optional<std::fstream> m_debugOutput;
    Window *trackedWindow_;
    std::array<QMetaObject::Connection, 4> vrrConnections_{};

    alignas(64) std::array<VrrHistoryEntry, 8> modeSwitchHistory{};

    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop *loop) noexcept;
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);
    ~RenderLoopPrivate();

    void updateReciprocal() noexcept;
    void initializeVrrCapabilities();
    void connectVrrSignals(Window *window);
    void disconnectVrrSignals() noexcept;
    void invalidateVrrState() noexcept;
    void updateVrrState() noexcept;

    [[nodiscard]] PresentationMode selectPresentationMode() noexcept;
    [[nodiscard]] bool detectVrrOscillation() noexcept;
    void recordModeSwitch() noexcept;
    void updateFramePrediction(std::chrono::nanoseconds measured) noexcept;

    void dispatch();
    void delayScheduleRepaint() noexcept;
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);

    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

static_assert(offsetof(RenderLoopPrivate, vblankIntervalReciprocal64) == 64, "Cache line 0 boundary must be at 64 bytes");
static_assert(offsetof(RenderLoopPrivate, modeSwitchHistory) % 64 == 0, "History must be cache-line aligned");

} // namespace KWin
