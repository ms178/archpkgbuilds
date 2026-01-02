/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-FileCopyrightText: 2025 Performance Engineering Team

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

class VrrStateCache final
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

    static_assert(sizeof(State) == 1);
    static_assert(std::is_trivially_copyable_v<State>);

    constexpr void setState(State s) noexcept { m_state = s.toRaw(); }
    [[nodiscard]] constexpr State getState() const noexcept { return State::fromRaw(m_state); }
    [[nodiscard]] constexpr uint8_t raw() const noexcept { return m_state; }
    constexpr void setRaw(uint8_t v) noexcept { m_state = v; }

private:
    uint8_t m_state{0};
};

static_assert(sizeof(std::chrono::nanoseconds) == 8);

class KWIN_EXPORT RenderLoopPrivate final
{
public:
    static constexpr size_t kModeSwitchHistorySize = 8;

    enum class VrrMode : uint8_t {
        Automatic = 0,
        Always = 1,
        Never = 2
    };

    RenderLoop *const q;
    Output *const output;

    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    std::chrono::nanoseconds scheduledRenderTimestamp{0};
    std::chrono::nanoseconds framePrediction{0};
    std::chrono::nanoseconds safetyMargin{0};

    uint64_t cachedVblankIntervalNs{16'666'667};
    uint64_t vblankIntervalReciprocal64{0};

    int64_t lastIntervalNs_{0};
    int64_t tripleBufferEnterThresholdNs{0};
    int64_t tripleBufferExitThresholdNs{0};

    QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;

    int32_t pendingFrameCount{0};
    int32_t refreshRate{60'000};
    int32_t inhibitCount{0};
    int32_t maxPendingFrameCount{1};

    int16_t cadenceStability_{128};
    int16_t scheduledTimerMs{-1};
    int16_t doubleBufferingCounter{0};

    uint16_t starvationRecoveryCounter{0};
    uint16_t modeDwellCounter_{0};
    uint16_t pendingModeCounter_{0};
    uint16_t oscillationCooldownCounter_{0};

    uint8_t reciprocalShift64{0};
    uint8_t consecutiveErrorCount{0};
    uint8_t vrrConnectionCount_{0};
    uint8_t modeSwitchHistoryHead_{0};
    uint8_t modeSwitchHistoryCount_{0};
    uint8_t tripleBufferHysteresisCounter{0};

    bool preparingNewFrame{false};
    bool pendingReschedule{false};
    bool wasTripleBuffering{false};
    bool vrrOscillationLockout{false};
    bool vrrEnabled{false};
    bool vrrCapable{false};
    bool vrrStateDirty_{true};

    PresentationMode presentationMode{PresentationMode::VSync};
    PresentationMode lastStableMode{PresentationMode::VSync};
    PresentationMode pendingTargetMode_{PresentationMode::VSync};

    VrrMode vrrMode{VrrMode::Automatic};
    VrrStateCache vrrStateCache_{};

    std::chrono::steady_clock::time_point lastModeSwitch{};
    std::array<std::chrono::steady_clock::time_point, kModeSwitchHistorySize> modeSwitchHistory_{};

    RenderJournal renderJournal;

    Window *trackedWindow_{nullptr};
    std::array<QMetaObject::Connection, 4> vrrConnections_{};

    std::optional<std::fstream> m_debugOutput;

    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop *loop) noexcept;

    explicit RenderLoopPrivate(RenderLoop *q, Output *output);
    ~RenderLoopPrivate();

    RenderLoopPrivate(const RenderLoopPrivate &) = delete;
    RenderLoopPrivate &operator=(const RenderLoopPrivate &) = delete;
    RenderLoopPrivate(RenderLoopPrivate &&) = delete;
    RenderLoopPrivate &operator=(RenderLoopPrivate &&) = delete;

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
    void updatePresentationCadence(int64_t intervalNs) noexcept;
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
    void notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs);
};

}
