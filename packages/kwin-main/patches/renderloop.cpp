/*
    KWin - the KDE window manager
    This file is part of the KDE project.
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "renderloop.h"
#include "options.h"
#include "renderloop_p.h"
#include "scene/surfaceitem.h"
#include "scene/surfaceitem_wayland.h"
#include "wayland/surface.h"
#include "utils/common.h"
#include "window.h"
#include "workspace.h"

#include <KSharedConfig>
#include <KConfigGroup>

#include <QThread>

#include <algorithm>
#include <cctype>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>

using namespace std::chrono_literals;

namespace
{
constexpr int64_t kNanosecondsPerMillisecond = 1'000'000;
constexpr int64_t kPredictionSlackNs = 500'000;
constexpr int64_t kMaxFutureLeadNs = static_cast<int64_t>(1) << 40;
constexpr uint64_t kPageflipDriftThreshold = 100;
constexpr uint64_t kIntervalsClamp = 1'000'000;
constexpr uint8_t kVrrVsyncStabilityFrames = 90;
constexpr uint8_t kVrrAdaptiveStabilityFrames = 90;
constexpr uint16_t kVrrLockoutStableFrames = 180;
constexpr auto kVrrControlThreshold = 33ms;
constexpr auto kVrrOscillationWindow = 2000ms;
constexpr uint8_t kVrrOscillationThreshold = 4;
constexpr int kMaxTimerDelayMs = std::numeric_limits<std::int16_t>::max();
constexpr int kTripleBufferEnterPct = 82;
constexpr int kTripleBufferExitPct = 70;
constexpr int kTripleBufferExitFrames = 4;
constexpr double kPredictionEmaAlpha = 0.12;
constexpr double kPredictionClampMin = 0.5;
constexpr double kPredictionClampMax = 1.8;
constexpr int kMinVrrRefreshMhz = 48'000;
constexpr int kMaxVrrRefreshMhz = 165'000;
constexpr int kVrrTargetPercentile = 95;
constexpr uint8_t kStarvationRecoveryFrames = 60;
constexpr uint8_t kMaxConsecutiveErrors = 5;
constexpr int kMaxErrorBackoffMs = 1000;
}

namespace KWin
{
static const bool s_printDebugInfo = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop) noexcept
{
    return loop ? loop->d.get() : nullptr;
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, Output *output)
    : q(q)
    , output(output)
    , pendingFrameCount(0)
    , inhibitCount(0)
    , refreshRate(60'000)
    , cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)
    , vblankIntervalReciprocal64(0)
    , maxPendingFrameCount(1)
    , scheduledTimerMs(-1)
    , doubleBufferingCounter(0)
    , reciprocalShift64(0)
    , preparingNewFrame(0)
    , pendingReschedule(0)
    , wasTripleBuffering(0)
    , vrrOscillationLockout(0)
    , vrrEnabled(0)
    , vrrCapable(0)
    , oscillationCheckCounter_(0)
    , consecutiveErrorCount(0)
    , vrrStabilityCounter(0)
    , recentSwitchCount(0)
    , modeSwitchHistoryIndex(0)
    , vrrConnectionCount_(0)
    , vrrMode(VrrMode::Automatic)
    , presentationMode(PresentationMode::VSync)
    , lastStableMode(PresentationMode::VSync)
    , tripleBufferEnterThresholdNs(0)
    , tripleBufferExitThresholdNs(0)
    , vrrTargetIntervalNs(0)
    , stableModeFrameCount(0)
    , starvationRecoveryCounter(0)
    , vrrStateDirty_(true)
    , trackedWindow_(nullptr)
{
    updateReciprocal();
    initializeVrrCapabilities();
    lastModeSwitch = std::chrono::steady_clock::now();
    lastPresentationTimestamp = std::chrono::steady_clock::now().time_since_epoch();
}

RenderLoopPrivate::~RenderLoopPrivate()
{
    disconnectVrrSignals();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    if (cachedVblankIntervalNs == 0) [[unlikely]] {
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        tripleBufferEnterThresholdNs = 0;
        tripleBufferExitThresholdNs = 0;
        vrrTargetIntervalNs = 0;
        return;
    }

    if (cachedVblankIntervalNs < (1ull << 32)) [[likely]] {
        constexpr uint8_t shift64 = 63;
        const __uint128_t numerator = static_cast<__uint128_t>(1) << shift64;
        const __uint128_t result = (numerator + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
        vblankIntervalReciprocal64 = static_cast<uint64_t>(result);
        reciprocalShift64 = shift64;
    } else {
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
    }

    tripleBufferEnterThresholdNs = static_cast<int64_t>((cachedVblankIntervalNs * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((cachedVblankIntervalNs * kTripleBufferExitPct) / 100);

    const uint64_t maxRefreshIntervalNs = 1'000'000'000'000ull / kMinVrrRefreshMhz;
    const uint64_t minRefreshIntervalNs = 1'000'000'000'000ull / kMaxVrrRefreshMhz;
    const uint64_t targetIntervalNs = minRefreshIntervalNs +
        ((maxRefreshIntervalNs - minRefreshIntervalNs) * (100 - kVrrTargetPercentile)) / 100;
    vrrTargetIntervalNs = std::max(targetIntervalNs, cachedVblankIntervalNs);
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
        vrrCapable = 0;
        vrrEnabled = 0;
        return;
    }
    const auto capabilities = output->capabilities();
    vrrCapable = capabilities.testFlag(Output::Capability::Vrr) ? 1 : 0;

    const auto config = KSharedConfig::openConfig(QStringLiteral("kwinrc"));
    const auto vrrGroup = config->group(QStringLiteral("VRR"));
    const QString policy = vrrGroup.readEntry(QStringLiteral("Policy"), QStringLiteral("Automatic"));

    if (policy.compare(QStringLiteral("Never"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Never;
        vrrEnabled = 0;
    } else if (policy.compare(QStringLiteral("Always"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Always;
        vrrEnabled = vrrCapable;
    } else {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    }

    if (vrrEnabled) {
        qCInfo(KWIN_CORE) << "VRR enabled for output" << output->name() << "policy:" << policy;
    }
}

void RenderLoopPrivate::connectVrrSignals(Window *window)
{
    if (!window || !vrrEnabled) {
        return;
    }
    if (trackedWindow_ == window) {
        return;
    }
    disconnectVrrSignals();

    vrrConnectionCount_ = 0;
    auto addConnection = [this](QMetaObject::Connection &&conn) {
        if (vrrConnectionCount_ < vrrConnections_.size()) {
            vrrConnections_[vrrConnectionCount_++] = std::move(conn);
        }
    };

    addConnection(QObject::connect(window, &QObject::destroyed, q, [this]() {
        trackedWindow_ = nullptr;
        disconnectVrrSignals();
    }));

    trackedWindow_ = window;

    addConnection(QObject::connect(window, &Window::fullScreenChanged, q, [this]() {
        invalidateVrrState();
    }));
    addConnection(QObject::connect(window, &Window::outputChanged, q, [this]() {
        invalidateVrrState();
    }));

    if (SurfaceItem *surf = window->surfaceItem()) {
        if (auto *waylandSurf = qobject_cast<SurfaceItemWayland *>(surf)) {
            if (auto *surface = waylandSurf->surface()) {
                addConnection(QObject::connect(surface, &SurfaceInterface::presentationModeHintChanged, q, [this]() {
                    invalidateVrrState();
                }));
            }
        }
    }
}

void RenderLoopPrivate::disconnectVrrSignals() noexcept
{
    for (uint8_t i = 0; i < vrrConnectionCount_; ++i) {
        QObject::disconnect(vrrConnections_[i]);
    }
    vrrConnectionCount_ = 0;
    trackedWindow_ = nullptr;
}

void RenderLoopPrivate::invalidateVrrState() noexcept
{
    vrrStateDirty_ = true;
}

void RenderLoopPrivate::updateVrrState() noexcept
{
    if (!vrrStateDirty_) {
        return;
    }

    VrrStateCache::State newState{};

    if (vrrEnabled && vrrCapable) [[unlikely]] {
        Workspace *const ws = workspace();
        if (ws && output) [[likely]] {
            Window *const activeWindow = ws->activeWindow();

            if (activeWindow) [[likely]] {
                connectVrrSignals(activeWindow);
                newState.isOnOutput = activeWindow->isOnOutput(output) ? 1 : 0;

                if (newState.isOnOutput) {
                    newState.isFullScreen = activeWindow->isFullScreen() ? 1 : 0;
                    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();

                    if (surfaceItem) [[likely]] {
                        if (auto *const waylandItem = qobject_cast<SurfaceItemWayland *>(surfaceItem)) {
                            if (auto *const surface = waylandItem->surface()) {
                                newState.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3;
                                newState.valid = 1;
                            }
                        }
                    }
                }
            }
        }
    }

    vrrStateCache_.setState(newState);
    vrrStateDirty_ = false;
}

void RenderLoopPrivate::recordModeSwitch() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;

    modeSwitchHistory[modeSwitchHistoryIndex].timestamp = now;
    modeSwitchHistoryIndex = (modeSwitchHistoryIndex + 1) & 7;
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    const auto windowStart = now - kVrrOscillationWindow;

    uint8_t activeCount = 0;
    for (const auto &entry : modeSwitchHistory) {
        if (entry.timestamp.time_since_epoch().count() > 0 && entry.timestamp > windowStart) {
            ++activeCount;
        }
    }
    recentSwitchCount = activeCount;

    if (activeCount >= kVrrOscillationThreshold) [[unlikely]] {
        if (!vrrOscillationLockout) {
            qCInfo(KWIN_CORE) << "VRR oscillation detected (" << static_cast<int>(activeCount)
                              << " switches in" << kVrrOscillationWindow.count() << "ms), entering lockout mode";
            vrrOscillationLockout = 1;
            stableModeFrameCount = 0;
            lastStableMode = PresentationMode::VSync;
            return true;
        }
    }
    return false;
}

PresentationMode RenderLoopPrivate::selectPresentationMode() noexcept
{
    if (!vrrEnabled || !vrrCapable) [[likely]] {
        return PresentationMode::VSync;
    }

    const VrrStateCache::State state = vrrStateCache_.getState();
    if (!state.valid) [[unlikely]] {
        return PresentationMode::VSync;
    }

    const auto hint = static_cast<PresentationModeHint>(state.hint);

    if (vrrOscillationLockout) [[unlikely]] {
        const PresentationMode targetMode = (vrrMode == VrrMode::Always && hint != PresentationModeHint::VSync)
            ? PresentationMode::AdaptiveSync
            : PresentationMode::VSync;

        if (presentationMode == targetMode) {
            ++stableModeFrameCount;
            if (stableModeFrameCount >= kVrrLockoutStableFrames) {
                vrrOscillationLockout = 0;
                stableModeFrameCount = 0;
                qCInfo(KWIN_CORE) << "VRR lockout released after" << kVrrLockoutStableFrames << "stable frames";
            }
        } else {
            stableModeFrameCount = 0;
        }
        return lastStableMode;
    }

    if (vrrMode == VrrMode::Always) [[unlikely]] {
        if (hint == PresentationModeHint::Async) [[unlikely]] {
            return PresentationMode::AdaptiveAsync;
        }
        if (hint != PresentationModeHint::VSync) [[likely]] {
            return PresentationMode::AdaptiveSync;
        }
        return PresentationMode::VSync;
    }

    if (!state.isOnOutput || !state.isFullScreen) [[unlikely]] {
        return PresentationMode::VSync;
    }
    if (hint == PresentationModeHint::VSync) [[likely]] {
        return PresentationMode::VSync;
    }
    if (hint == PresentationModeHint::Async) [[unlikely]] {
        return PresentationMode::AdaptiveAsync;
    }
    return PresentationMode::AdaptiveSync;
}

void RenderLoopPrivate::updateFramePrediction(std::chrono::nanoseconds measured) noexcept
{
    if (measured.count() <= 0) [[unlikely]] {
        return;
    }
    const double measuredNs = static_cast<double>(measured.count());
    const double currentPredictionNs = static_cast<double>(framePrediction.count());

    const double newPredictionNs = currentPredictionNs * (1.0 - kPredictionEmaAlpha) + measuredNs * kPredictionEmaAlpha;
    const double vblankNs = static_cast<double>(cachedVblankIntervalNs);
    double clampedNs = newPredictionNs;
    clampedNs = std::max(clampedNs, vblankNs * kPredictionClampMin);
    clampedNs = std::min(clampedNs, vblankNs * kPredictionClampMax);

    framePrediction = std::chrono::nanoseconds(static_cast<int64_t>(clampedNs));
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    const bool shouldSchedule = !kwinApp()->isTerminating() &&
                                 !compositeTimer.isActive() &&
                                 !preparingNewFrame;
    if (__builtin_expect(shouldSchedule, 1)) [[likely]] {
        scheduleRepaint(nextPresentationTimestamp);
    }
}

[[gnu::always_inline, gnu::hot]]
static inline uint64_t fastDivideByVblank(uint64_t numerator,
                                          uint64_t reciprocal64,
                                          uint8_t shift64) noexcept
{
    const __uint128_t product = static_cast<__uint128_t>(numerator) * reciprocal64;
    return static_cast<uint64_t>(product >> shift64);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    if (q->thread() != QThread::currentThread()) {
        QMetaObject::invokeMethod(q, [this, lastTargetTimestamp]() {
            scheduleRepaint(lastTargetTimestamp);
        }, Qt::QueuedConnection);
        return;
    }

    pendingReschedule = 0;
    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) [[unlikely]] {
        return;
    }

    const int64_t nowNs = std::chrono::steady_clock::now().time_since_epoch().count();
    int64_t lastPresNs = lastPresentationTimestamp.count();

    if (lastPresNs <= 0 || (nowNs - lastPresNs) > 5'000'000'000ll) [[unlikely]] {
        lastPresNs = nowNs;
        lastPresentationTimestamp = std::chrono::nanoseconds(nowNs);
    }

    const int64_t predictedRenderNs = (framePrediction.count() > 0)
        ? framePrediction.count()
        : renderJournal.result().count();

    const int64_t safetyNs = safetyMargin.count();
    const int64_t vblankSigned = static_cast<int64_t>(vblankIntervalNs);
    const int64_t maxCompositeNs = vblankSigned * 2;
    const int64_t baseCompositeNs = predictedRenderNs + safetyNs + kPredictionSlackNs;
    int64_t expectedCompositeNs = baseCompositeNs;
    expectedCompositeNs = std::max(expectedCompositeNs, int64_t{0});
    expectedCompositeNs = std::min(expectedCompositeNs, maxCompositeNs);

    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();

    const bool shouldCheckOscillation = (++oscillationCheckCounter_ >= 8);
    if (shouldCheckOscillation) {
        oscillationCheckCounter_ = 0;
    }

    if (targetMode != presentationMode) [[unlikely]] {
        const uint8_t threshold = (targetMode == PresentationMode::VSync)
            ? kVrrVsyncStabilityFrames
            : kVrrAdaptiveStabilityFrames;

        if (vrrStabilityCounter >= threshold) {
            presentationMode = targetMode;
            lastStableMode = targetMode;
            vrrStabilityCounter = 0;
            starvationRecoveryCounter = 0;
            recordModeSwitch();
            if (shouldCheckOscillation) {
                detectVrrOscillation();
            }
            qCDebug(KWIN_CORE) << "Presentation mode changed to" << static_cast<int>(targetMode);
        } else {
            ++vrrStabilityCounter;
        }
    } else {
        vrrStabilityCounter = 0;
    }

    int64_t nextPresNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = nowNs - lastPresNs;
        uint64_t flipsSince = 0;

        if (sinceLastNs > 0 && vblankIntervalReciprocal64 != 0) [[likely]] {
            flipsSince = fastDivideByVblank(static_cast<uint64_t>(sinceLastNs),
                                            vblankIntervalReciprocal64,
                                            reciprocalShift64);
        }

        const bool driftDetected = flipsSince > kPageflipDriftThreshold;
        if (driftDetected) [[unlikely]] {
            const int64_t earlyStart = vblankSigned - 1'000;
            if (expectedCompositeNs < earlyStart) {
                expectedCompositeNs = earlyStart;
            }
        }

        uint64_t flipsInAdvance = 1;
        if (expectedCompositeNs > vblankSigned) [[unlikely]] {
            const uint64_t need = (static_cast<uint64_t>(expectedCompositeNs) + vblankIntervalNs - 1) / vblankIntervalNs;
            const uint64_t maxPending = static_cast<uint64_t>(maxPendingFrameCount > 0 ? maxPendingFrameCount : 1);
            flipsInAdvance = std::min(std::max(need, uint64_t{1}), maxPending);
        }

        const bool highLoad = expectedCompositeNs > tripleBufferEnterThresholdNs;
        if (highLoad) [[unlikely]] {
            wasTripleBuffering = 1;
            doubleBufferingCounter = 0;
            if (flipsInAdvance < 2) {
                flipsInAdvance = 2;
            }
        } else if (wasTripleBuffering) [[unlikely]] {
            if (expectedCompositeNs < tripleBufferExitThresholdNs) {
                ++doubleBufferingCounter;
            } else {
                doubleBufferingCounter = 0;
            }

            if (doubleBufferingCounter >= kTripleBufferExitFrames) {
                wasTripleBuffering = 0;
            } else if (flipsInAdvance < 2) {
                flipsInAdvance = 2;
            }
        }

        uint64_t flipsTarget;
        if (__builtin_expect(compositeTimer.isActive(), 0) && nextPresentationTimestamp.count() > 0) [[unlikely]] {
            const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresNs;
            if (deltaNs > 0) {
                const uint64_t num = static_cast<uint64_t>(deltaNs) + vblankIntervalNs / 2;
                const uint64_t intervals = num / vblankIntervalNs;
                flipsTarget = std::max(intervals, uint64_t{1});
                flipsTarget = std::min(flipsTarget, kIntervalsClamp);
            } else {
                flipsTarget = 1;
            }
        } else {
            const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresNs;
            uint64_t flipsToTarget = 0;
            if (toTargetNs > 0 && vblankIntervalReciprocal64 != 0) [[likely]] {
                const uint64_t offset = vblankIntervalNs / 2;
                const uint64_t numeratorSafe = (static_cast<uint64_t>(toTargetNs) < (UINT64_MAX - offset))
                    ? static_cast<uint64_t>(toTargetNs) + offset
                    : UINT64_MAX;
                flipsToTarget = fastDivideByVblank(numeratorSafe, vblankIntervalReciprocal64, reciprocalShift64);
            }
            flipsTarget = std::max(flipsSince + flipsInAdvance, flipsToTarget + 1);
        }

        uint64_t tempResult;
        if (__builtin_mul_overflow(flipsTarget, vblankIntervalNs, &tempResult)) [[unlikely]] {
            nextPresNs = std::numeric_limits<int64_t>::max() - lastPresNs;
        } else {
            nextPresNs = static_cast<int64_t>(tempResult);
        }

        if (__builtin_add_overflow(lastPresNs, nextPresNs, &nextPresNs)) [[unlikely]] {
            nextPresNs = std::numeric_limits<int64_t>::max();
        }

    } else {
        wasTripleBuffering = 0;
        doubleBufferingCounter = 0;

        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) [[unlikely]] {
            if (__builtin_add_overflow(nowNs, expectedCompositeNs, &nextPresNs)) [[unlikely]] {
                nextPresNs = std::numeric_limits<int64_t>::max();
            }
        } else {
            const int64_t targetIntervalNs = static_cast<int64_t>(vrrTargetIntervalNs);
            int64_t tentativeNs;
            if (__builtin_add_overflow(nowNs, expectedCompositeNs, &tentativeNs)) [[unlikely]] {
                tentativeNs = std::numeric_limits<int64_t>::max();
            }

            const int64_t sinceLastNs = nowNs - lastPresNs;
            if (sinceLastNs < targetIntervalNs) [[likely]] {
                int64_t minNext;
                if (__builtin_add_overflow(lastPresNs, targetIntervalNs, &minNext)) [[unlikely]] {
                    minNext = std::numeric_limits<int64_t>::max();
                }
                nextPresNs = std::max(tentativeNs, minNext);
            } else {
                nextPresNs = tentativeNs;
            }
        }
    }

    const int64_t clampLower = nowNs;
    int64_t clampUpper;
    if (__builtin_add_overflow(nowNs, kMaxFutureLeadNs, &clampUpper)) [[unlikely]] {
        clampUpper = std::numeric_limits<int64_t>::max();
    }
    nextPresNs = std::max(nextPresNs, clampLower);
    nextPresNs = std::min(nextPresNs, clampUpper);

    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    int64_t nextRenderNs;
    if (__builtin_sub_overflow(nextPresNs, expectedCompositeNs, &nextRenderNs)) [[unlikely]] {
        nextRenderNs = 0;
    }

    int64_t delayNs = nextRenderNs - nowNs;
    if (delayNs < 0) {
        delayNs = 0;
    }

    if (__builtin_expect(compositeTimer.isActive(), 1)) [[likely]] {
        const int64_t currentTargetNs = scheduledRenderTimestamp.count();
        const int64_t diff = std::abs(currentTargetNs - nextRenderNs);
        if (diff < 100'000) [[likely]] {
            return;
        }
    }

    const uint64_t delayNsU64 = static_cast<uint64_t>(delayNs);
    const uint64_t safeAdd = (delayNsU64 <= UINT64_MAX - kNanosecondsPerMillisecond)
        ? delayNsU64 + kNanosecondsPerMillisecond - 1
        : UINT64_MAX;
    const int64_t delayMs64 = static_cast<int64_t>(safeAdd / kNanosecondsPerMillisecond);
    const int delayMs = static_cast<int>(std::min(delayMs64, static_cast<int64_t>(kMaxTimerDelayMs)));

    scheduledRenderTimestamp = std::chrono::nanoseconds{nextRenderNs};
    scheduledTimerMs = static_cast<int16_t>(delayMs);
    compositeTimer.start(delayMs, Qt::PreciseTimer, q);
}

void RenderLoopPrivate::delayScheduleRepaint() noexcept
{
    pendingReschedule = 1;
}

void RenderLoopPrivate::notifyFrameDropped()
{
    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;

    if (consecutiveErrorCount < 255) {
        ++consecutiveErrorCount;
    }

    if (!inhibitCount && pendingReschedule) [[likely]] {
        if (consecutiveErrorCount > kMaxConsecutiveErrors) {
            const int exponent = std::min<int>(consecutiveErrorCount - kMaxConsecutiveErrors, 10);
            int delay = 2 << exponent;
            delay = std::min(delay, kMaxErrorBackoffMs);

            const int minDelay = static_cast<int>(cachedVblankIntervalNs / kNanosecondsPerMillisecond);
            delay = std::max(delay, minDelay);

            compositeTimer.start(delay, Qt::PreciseTimer, q);
        } else {
            scheduleNextRepaint();
        }
    }
}

namespace
{
static void writeDebugOutput(RenderLoopPrivate *d,
                             std::optional<RenderTimeSpan> renderTime,
                             OutputFrame *frame,
                             std::chrono::nanoseconds timestamp,
                             PresentationMode mode)
{
    if (!d->m_debugOutput) {
        if (!d->output) return;

        std::string sanitizedName = d->output->name().toStdString();
        std::replace_if(sanitizedName.begin(), sanitizedName.end(), [](char c){
            return !std::isalnum(static_cast<unsigned char>(c));
        }, '_');

        d->m_debugOutput = std::fstream("kwin_perf_" + sanitizedName + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *(d->m_debugOutput) << "target_flip,flip,start,end,margin,duration,vrr,pred_render\n";
        }
    }

    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const RenderTimeSpan times = renderTime.value_or(RenderTimeSpan{});
        const bool vrr = (mode == PresentationMode::AdaptiveSync || mode == PresentationMode::AdaptiveAsync);
        *(d->m_debugOutput) << frame->targetPageflipTime().time_since_epoch().count() << ','
                            << timestamp.count() << ','
                            << times.start.time_since_epoch().count() << ','
                            << times.end.time_since_epoch().count() << ','
                            << d->safetyMargin.count() << ','
                            << frame->refreshDuration().count() << ','
                            << vrr << ','
                            << frame->predictedRenderTime().count() << '\n';
    }
}
}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    consecutiveErrorCount = 0;

    if (s_printDebugInfo) [[unlikely]] {
        writeDebugOutput(this, renderTime, frame, timestamp, mode);
    }

    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    notifyVblank(timestamp);

    if (renderTime) [[likely]] {
        const auto duration = renderTime->end - renderTime->start;
        renderJournal.add(duration, timestamp);
        updateFramePrediction(duration);

        if (presentationMode != PresentationMode::VSync) {
            const int64_t frameTimeNs = duration.count();
            const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
            if (frameTimeNs > vblankNs && starvationRecoveryCounter < kStarvationRecoveryFrames) {
                ++starvationRecoveryCounter;
            } else if (frameTimeNs <= vblankNs && starvationRecoveryCounter > 0) {
                --starvationRecoveryCounter;
            }
        }
    }

    if (__builtin_expect(compositeTimer.isActive(), 0)) [[unlikely]] {
        scheduleRepaint(lastPresentationTimestamp);
    }

    if (!inhibitCount && pendingReschedule) [[likely]] {
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp)
{
    if (lastPresentationTimestamp <= timestamp) [[likely]] {
        lastPresentationTimestamp = timestamp;
    } else {
        lastPresentationTimestamp = std::chrono::steady_clock::now().time_since_epoch();
    }
}

void RenderLoop::timerEvent(QTimerEvent *event)
{
    if (event->timerId() == d->compositeTimer.timerId()) [[likely]] {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (event->timerId() == d->delayedVrrTimer.timerId()) [[unlikely]] {
        d->delayedVrrTimer.stop();
        scheduleRepaint(nullptr, nullptr);
    } else {
        QObject::timerEvent(event);
    }
}

void RenderLoopPrivate::dispatch()
{
    Q_EMIT q->frameRequested(q);
}

RenderLoop::RenderLoop(Output *output)
    : d(std::make_unique<RenderLoopPrivate>(this, output))
{
}

RenderLoop::~RenderLoop() = default;

void RenderLoop::inhibit()
{
    ++d->inhibitCount;
    if (d->inhibitCount == 1) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
    }
}

void RenderLoop::uninhibit()
{
    Q_ASSERT(d->inhibitCount > 0);
    --d->inhibitCount;
    if (d->inhibitCount == 0) {
        d->scheduleNextRepaint();
    }
}

void RenderLoop::prepareNewFrame()
{
    Q_ASSERT(!d->preparingNewFrame);
    ++d->pendingFrameCount;
    d->preparingNewFrame = 1;
}

void RenderLoop::newFramePrepared()
{
    d->preparingNewFrame = 0;
}

[[nodiscard]] int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int refreshRate)
{
    constexpr int kMinRefreshRate = 1'000;
    constexpr int kMaxRefreshRate = 1'000'000;
    int rr = refreshRate;
    rr = std::max(rr, kMinRefreshRate);
    rr = std::min(rr, kMaxRefreshRate);

    if (d->refreshRate == rr) [[unlikely]] {
        return;
    }
    d->refreshRate = rr;
    d->cachedVblankIntervalNs = 1'000'000'000'000ull / static_cast<uint64_t>(rr);
    d->updateReciprocal();
    Q_EMIT refreshRateChanged();

    if (!d->inhibitCount) [[likely]] {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->scheduleNextRepaint();
    }
}

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds safetyMargin)
{
    d->safetyMargin = std::max(safetyMargin, 0ns);
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *outputLayer)
{
    if (thread() != QThread::currentThread()) {
        QMetaObject::invokeMethod(this, [this, item, outputLayer]() {
            scheduleRepaint(item, outputLayer);
        }, Qt::QueuedConnection);
        return;
    }

    d->invalidateVrrState();

    const bool vrrOrTearing = (d->presentationMode != PresentationMode::VSync);

    if (vrrOrTearing && (item || outputLayer)) [[unlikely]] {
        if (activeWindowControlsVrrRefreshRate()) {
             Workspace *const ws = workspace();
             if (ws && d->output) {
                 Window *const active = ws->activeWindow();
                 if (active && active->isOnOutput(d->output)) {
                     SurfaceItem *surface = active->surfaceItem();
                     if (item && surface && item != surface && !surface->isAncestorOf(item)) {
                         if (d->pendingFrameCount > 0) {
                             const int delayMs = 1000 / (d->refreshRate / 1000);
                             d->delayedVrrTimer.start(delayMs, Qt::PreciseTimer, this);
                             return;
                         }
                     }
                 }
             }
        }
    }

    d->delayedVrrTimer.stop();

    int effectiveMax = d->maxPendingFrameCount;
    if (vrrOrTearing) {
        effectiveMax = (d->starvationRecoveryCounter >= kStarvationRecoveryFrames) ? 2 : 1;
    }

    if (d->pendingFrameCount < effectiveMax && !d->inhibitCount) [[likely]] {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

[[nodiscard]] bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *const ws = workspace();
    if (!ws) return false;

    Window *const active = ws->activeWindow();
    if (!active || !d->output || !active->isOnOutput(d->output)) {
        return false;
    }

    SurfaceItem *const surface = active->surfaceItem();
    return surface && (surface->recursiveFrameTimeEstimation() <= kVrrControlThreshold);
}

[[nodiscard]] std::chrono::nanoseconds RenderLoop::lastPresentationTimestamp() const
{
    return d->lastPresentationTimestamp;
}

[[nodiscard]] std::chrono::nanoseconds RenderLoop::nextPresentationTimestamp() const
{
    return d->nextPresentationTimestamp;
}

void RenderLoop::setPresentationMode(PresentationMode mode)
{
    if (mode != d->presentationMode) [[unlikely]] {
        qCDebug(KWIN_CORE) << "Changed presentation mode to" << static_cast<int>(mode);
        d->presentationMode = mode;
    }
}

void RenderLoop::setMaxPendingFrameCount(uint32_t maxCount)
{
    const int clamped = static_cast<int>(maxCount);
    d->maxPendingFrameCount = std::max(clamped, 1);
    d->maxPendingFrameCount = std::min(d->maxPendingFrameCount, 3);
}

[[nodiscard]] std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

} // namespace KWin

#include "moc_renderloop.cpp"
