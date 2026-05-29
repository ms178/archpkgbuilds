/*
    KWin - the KDE window manager
    SPDX-FileCopyrightText: 2025 Performance Engineering Team
    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "renderbackend.h"
#include "renderjournal.h"
#include "renderloop.h"

#include <QBasicTimer>
#include <QMetaObject>
#include <QPointer>

#include <array>
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
class SurfaceInterface;
class OutputFrame;
class Window;

// ─────────────────────────────────────────────────────────────────────────────
// VrrStateCache
//
// 1-byte cache of the per-frame VRR decision inputs.  The wp_content_type_v1
// content type (video/game/none) is now persisted alongside the presentation
// mode hint and fullscreen state; both fit in the previously-reserved bits.
//
// Layout (still exactly 1 byte):
//   bits 0-1 : hint           (PresentationModeHint VSync=0, Async=1)
//   bit  2   : isOnOutput
//   bit  3   : isFullScreen
//   bit  4   : valid          (hint is valid)
//   bits 5-6 : contentType    (0=None, 1=Photo, 2=Video, 3=Game)
//   bit  7   : contentTypeValid
// ─────────────────────────────────────────────────────────────────────────────
class VrrStateCache final
{
public:
    enum class ContentTypeBits : uint8_t {
        None = 0,
        Photo = 1,
        Video = 2,
        Game = 3,
    };

    struct State {
        uint8_t hint : 2;
        uint8_t isOnOutput : 1;
        uint8_t isFullScreen : 1;
        uint8_t valid : 1;
        uint8_t contentType : 2;
        uint8_t contentTypeValid : 1;

        constexpr State() noexcept
            : hint(0)
            , isOnOutput(0)
            , isFullScreen(0)
            , valid(0)
            , contentType(0)
            , contentTypeValid(0)
        {
        }

        [[nodiscard]] constexpr uint8_t toRaw() const noexcept
        {
            return static_cast<uint8_t>(
                  (hint & 0x3U)
                | ((isOnOutput & 0x1U) << 2)
                | ((isFullScreen & 0x1U) << 3)
                | ((valid & 0x1U) << 4)
                | ((contentType & 0x3U) << 5)
                | ((contentTypeValid & 0x1U) << 7));
        }

        [[nodiscard]] static constexpr State fromRaw(uint8_t raw) noexcept
        {
            State s;
            s.hint = static_cast<uint8_t>(raw & 0x3U);
            s.isOnOutput = static_cast<uint8_t>((raw >> 2) & 0x1U);
            s.isFullScreen = static_cast<uint8_t>((raw >> 3) & 0x1U);
            s.valid = static_cast<uint8_t>((raw >> 4) & 0x1U);
            s.contentType = static_cast<uint8_t>((raw >> 5) & 0x3U);
            s.contentTypeValid = static_cast<uint8_t>((raw >> 7) & 0x1U);
            return s;
        }

        [[nodiscard]] constexpr bool isVideoContent() const noexcept
        {
            return contentTypeValid != 0U
                && static_cast<ContentTypeBits>(contentType) == ContentTypeBits::Video;
        }

        [[nodiscard]] constexpr bool isGameContent() const noexcept
        {
            return contentTypeValid != 0U
                && static_cast<ContentTypeBits>(contentType) == ContentTypeBits::Game;
        }
    };

    static_assert(sizeof(State) == 1, "VrrStateCache::State must remain 1 byte");
    static_assert(std::is_trivially_copyable_v<State>);

    constexpr void setState(State s) noexcept { m_state = s.toRaw(); }
    [[nodiscard]] constexpr State getState() const noexcept { return State::fromRaw(m_state); }
    [[nodiscard]] constexpr uint8_t raw() const noexcept { return m_state; }
    constexpr void setRaw(uint8_t v) noexcept { m_state = v; }

private:
    uint8_t m_state{0};
};

class KWIN_EXPORT RenderLoopPrivate final
{
public:
    static constexpr size_t kModeSwitchHistorySize = 8;
    static constexpr size_t kPresentHistorySize = 16;
    static_assert((kModeSwitchHistorySize & (kModeSwitchHistorySize - 1U)) == 0U, "Must be power of two");
    static_assert((kPresentHistorySize & (kPresentHistorySize - 1U)) == 0U, "Must be power of two");
    static constexpr uint16_t kStarvationRecoveryFrames = 4;

    enum class VrrMode : uint8_t { Automatic = 0, Always = 1, Never = 2 };

    struct PresentSample {
        std::chrono::nanoseconds timestamp{0};
        uint32_t refresh{0};
        bool deadlineMissed{false};
        bool directScanout{false};
        bool valid{false};
        uint8_t pad{0};
    };

    RenderLoop *const q;
    BackendOutput *const output;

    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    std::chrono::nanoseconds scheduledRenderTimestamp{0};
    std::chrono::nanoseconds framePrediction{0};
    std::chrono::nanoseconds safetyMargin{0};
    std::chrono::nanoseconds nominalContentFrameInterval{0};

    uint64_t cachedVblankIntervalNs{16666667ULL};
    uint64_t vblankIntervalReciprocal64{0};

    int64_t lastIntervalNs_{0};
    int64_t tripleBufferEnterThresholdNs{0};
    int64_t tripleBufferExitThresholdNs{0};

    QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;
    QBasicTimer stalledFrameTimer;

    int32_t pendingFrameCount{0};
    int32_t refreshRate{60000};
    int32_t inhibitCount{0};
    int32_t maxPendingFrameCount{1};

    int16_t cadenceStability_{128};
    int16_t scheduledTimerMs{-1};
    int16_t vrrControlDelayMs{-1};
    int16_t stalledFrameTimerMs_{-1};

    uint16_t starvationRecoveryCounter{0};
    uint16_t modeDwellCounter_{0};
    uint16_t pendingModeCounter_{0};
    uint16_t oscillationCooldownCounter_{0};
    uint16_t interactiveGraceFrames_{0};

    // Panel-refresh-rate ground truth (from wp_presentation_feedback).
    uint32_t currentPanelRefreshRateMhz_{0};
    uint32_t filteredPanelRefreshRateMhz_{0};
    uint8_t  panelRefreshStableCount_{0};

    // Direct scanout guard: when true, scheduleRepaint() returns early so we
    // don't waste GPU cycles compositing during a client's direct scanout.
    bool directScanoutActive_{false};

    uint8_t reciprocalShift64{0};
    uint8_t consecutiveErrorCount{0};
    uint8_t vrrConnectionCount_{0};
    uint8_t modeSwitchHistoryHead_{0};
    uint8_t modeSwitchHistoryCount_{0};
    uint8_t tripleBufferHysteresisCounter{0};
    uint8_t presentHistoryHead_{0};
    uint8_t presentHistoryCount_{0};
    uint8_t stalledFrameTimeoutCount_{0};

    bool preparingNewFrame{false};
    bool pendingReschedule{false};
    bool wasTripleBuffering{false};
    bool vrrOscillationLockout{false};
    bool vrrEnabled{false};
    bool vrrCapable{false};
    bool vrrStateDirty_{true};
    bool lfcCapable_{false};

    PresentationMode presentationMode{PresentationMode::VSync};
    PresentationMode lastStableMode{PresentationMode::VSync};
    PresentationMode pendingTargetMode_{PresentationMode::VSync};
    VrrMode vrrMode{VrrMode::Automatic};
    VrrContentHint activeContentHint_{VrrContentHint::Unknown};
    VrrDecisionReason lastDecisionReason_{VrrDecisionReason::None};

    VrrCapabilities vrrCaps_{};
    VrrStateCache vrrStateCache_{};

    std::chrono::steady_clock::time_point lastModeSwitch{};
    std::array<std::chrono::steady_clock::time_point, kModeSwitchHistorySize> modeSwitchHistory_{};
    std::array<PresentSample, kPresentHistorySize> presentHistory_{};

    RenderJournal renderJournal;

    QPointer<Window> trackedWindow_{nullptr};
    QPointer<SurfaceItem> trackedSurfaceItem_{nullptr};
    QPointer<SurfaceInterface> trackedSurface_{nullptr};

    // One extra connection slot for SurfaceInterface::contentTypeChanged.
    // The array grows from 4 to 5 — still tiny, still cache-friendly.
    std::array<QMetaObject::Connection, 5> vrrConnections_{};

    std::optional<std::fstream> m_debugOutput;

    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop* loop) noexcept;
    explicit RenderLoopPrivate(RenderLoop *q, BackendOutput* output);
    ~RenderLoopPrivate();

    RenderLoopPrivate(const RenderLoopPrivate &) = delete;
    RenderLoopPrivate &operator=(const RenderLoopPrivate &) = delete;
    RenderLoopPrivate(RenderLoopPrivate &&) = delete;
    RenderLoopPrivate &operator=(RenderLoopPrivate &&) = delete;

    void updateReciprocal() noexcept;
    void armStalledFrameTimer() noexcept;
    void disarmStalledFrameTimer() noexcept;
    void handleStalledFrameTimeout();
    void initializeVrrCapabilities();
    void connectVrrSignals(Window *window);
    void disconnectVrrSignals() noexcept;
    void invalidateVrrState() noexcept;
    void updateVrrState() noexcept;
    void setVrrCapabilities(const VrrCapabilities &caps) noexcept;
    void setActiveContentHint(VrrContentHint hint,
                              std::optional<std::chrono::nanoseconds> nominalFrameInterval) noexcept;
    void addPresentFeedback(const PresentFeedback &feedback) noexcept;

    [[nodiscard]] bool cadenceMatchesNominal() const noexcept;
    [[nodiscard]] bool isBelowVrrFloor() const noexcept;
    [[nodiscard]] PresentationMode selectPresentationMode() noexcept;
    [[nodiscard]] bool shouldSwitchMode(PresentationMode target) noexcept;
    void recordModeSwitch() noexcept;
    [[nodiscard]] bool detectVrrOscillation() noexcept;

    void updateFramePrediction(std::chrono::nanoseconds measured) noexcept;
    void updatePresentationCadence(int64_t intervalNs) noexcept;
    void dispatch();
    void delayScheduleRepaint() noexcept;
    void scheduleNextRepaint();
    void scheduleRepaint();
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                              std::optional<RenderTimeSpan> renderTime,
                              PresentationMode mode,
                              OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs);
};

} // namespace KWin
