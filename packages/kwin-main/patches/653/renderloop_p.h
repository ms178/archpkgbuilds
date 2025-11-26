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
#include <bit>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <memory>
#include <optional>
#include <type_traits>

namespace KWin
{

class SurfaceItem;
class OutputFrame;
class Window;

class VrrStateCache
{
public:
    struct State {
        uint8_t hint : 2;
        uint8_t isOnOutput : 1;
        uint8_t isFullScreen : 1;
        uint8_t valid : 1;
        uint8_t reserved : 3;

        constexpr State() noexcept
            : hint(0)
            , isOnOutput(0)
            , isFullScreen(0)
            , valid(0)
            , reserved(0)
        {
        }

        [[nodiscard]] constexpr uint8_t toRaw() const noexcept
        {
            return std::bit_cast<uint8_t>(*this);
        }

        [[nodiscard]] static constexpr State fromRaw(uint8_t raw) noexcept
        {
            return std::bit_cast<State>(raw);
        }
    };

    static_assert(sizeof(State) == 1, "State must be exactly 1 byte");
    static_assert(std::is_trivially_copyable_v<State>,
                  "State must be trivially copyable for bit_cast");

    constexpr void setState(State newState) noexcept
    {
        m_state = newState.toRaw();
    }

    [[nodiscard]] constexpr State getState() const noexcept
    {
        return State::fromRaw(m_state);
    }

    [[nodiscard]] constexpr uint8_t raw() const noexcept
    {
        return m_state;
    }

    constexpr void setRaw(uint8_t value) noexcept
    {
        m_state = value;
    }

private:
    uint8_t m_state{0};
};

static_assert(sizeof(std::chrono::nanoseconds) == 8,
              "std::chrono::nanoseconds must be 8 bytes for layout assumptions");

class alignas(64) KWIN_EXPORT RenderLoopPrivate
{
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

    uint8_t consecutiveErrorCount;
    uint8_t vrrConnectionCount_;

    enum class VrrMode : uint8_t {
        Automatic,
        Always,
        Never
    };
    VrrMode vrrMode;

    PresentationMode presentationMode;
    PresentationMode lastStableMode;
    PresentationMode pendingTargetMode_;

    int64_t tripleBufferEnterThresholdNs;
    int64_t tripleBufferExitThresholdNs;
    uint64_t vrrMinIntervalNs;
    uint64_t vrrMaxIntervalNs;

    uint16_t stableModeFrameCount;
    uint16_t starvationRecoveryCounter;
    uint16_t modeDwellCounter_;
    uint16_t pendingModeCounter_;
    uint16_t oscillationCooldownCounter_;

    int64_t frameTimeVariance_;
    int64_t frameTimeMean_;

    std::chrono::steady_clock::time_point lastModeSwitch{};

    static constexpr size_t kModeSwitchHistorySize = 8;
    std::array<std::chrono::steady_clock::time_point, kModeSwitchHistorySize> modeSwitchHistory_{};
    uint8_t modeSwitchHistoryHead_{0};
    uint8_t modeSwitchHistoryCount_{0};

    VrrStateCache vrrStateCache_{};
    bool vrrStateDirty_;

    QBasicTimer delayedVrrTimer;
    RenderJournal renderJournal;

    std::optional<std::fstream> m_debugOutput;
    Window *trackedWindow_;
    std::array<QMetaObject::Connection, 4> vrrConnections_{};

    alignas(64) uint64_t padding_[8];

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
    [[nodiscard]] bool shouldSwitchMode(PresentationMode target) noexcept;
    void recordModeSwitch() noexcept;
    [[nodiscard]] bool detectVrrOscillation() noexcept;
    void updateFramePrediction(std::chrono::nanoseconds measured) noexcept;
    void updateFrameTimeStats(int64_t frameTimeNs) noexcept;
    [[nodiscard]] bool isFrameTimeStable() const noexcept;

    void dispatch();
    void delayScheduleRepaint() noexcept;
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);

    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                              std::optional<RenderTimeSpan> renderTime,
                              PresentationMode mode,
                              OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
#endif

static_assert(offsetof(RenderLoopPrivate, vblankIntervalReciprocal64) == 64,
              "Cache line boundary at 64 bytes");

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

}
