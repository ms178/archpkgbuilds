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
#include <atomic>
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
class KWIN_EXPORT RenderLoopPrivate {
public:
    std::chrono::nanoseconds lastPresentationTimestamp{0};
    std::chrono::nanoseconds nextPresentationTimestamp{0};
    uint64_t cachedVblankIntervalNs;
    uint64_t vblankIntervalReciprocal64;
    uint64_t vblankIntervalReciprocal;
    uint64_t nsToMsReciprocal;
    uint8_t reciprocalShift64;
    uint8_t reciprocalShift;
    uint8_t nsToMsShift;
    PresentationMode presentationMode = PresentationMode::VSync;
    uint8_t vrrStabilityCounter = 0;
    bool pendingReschedule = false;
    bool wasTripleBuffering = false;
    int16_t scheduledTimerMs = -1;
    int16_t doubleBufferingCounter = 0;
    int pendingFrameCount = 0;
    int maxPendingFrameCount = 1;
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
    std::atomic<Window *> cachedActiveWindow{nullptr};
    std::atomic<SurfaceItem *> cachedSurfaceItem{nullptr};
    std::atomic<PresentationModeHint> cachedHint{PresentationModeHint::VSync};
    std::atomic<ContentType> cachedContentType{ContentType::None};
    std::atomic<bool> cachedIsFullScreen{false};
    std::atomic<bool> cachedIsOnOutput{false};
    std::atomic<bool> vrrContextDirty{true};
    PresentationModeHint lastObservedHint = PresentationModeHint::VSync;
    PresentationMode lastStableMode = PresentationMode::VSync;
    std::chrono::steady_clock::time_point lastModeSwitch{};
    std::array<std::chrono::steady_clock::time_point, 8> modeSwitchHistory{};
    uint8_t modeSwitchHistoryIndex = 0;
    bool vrrOscillationLockout = false;
    uint16_t stableModeFrameCount = 0;
    QBasicTimer compositeTimer;
    QBasicTimer delayedVrrTimer;
    RenderJournal renderJournal;
    std::optional<std::fstream> m_debugOutput;
    [[nodiscard]] static RenderLoopPrivate *get(RenderLoop *loop) noexcept;
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);
    void updateReciprocal() noexcept;
    void initializeVrrCapabilities();
    void updateVrrContext() noexcept;
    void invalidateVrrContext() noexcept;
    [[nodiscard]] PresentationMode selectPresentationModeFromContext() noexcept;
    [[nodiscard]] bool checkForPresentationHintChange() noexcept;
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
} // namespace KWin
