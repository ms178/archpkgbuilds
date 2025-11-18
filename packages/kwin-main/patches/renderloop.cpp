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
#include <bit>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>

using namespace std::chrono_literals;

namespace
{
constexpr int64_t kNanosecondsPerMillisecond = 1'000'000;
constexpr int64_t kPredictionSlackNs = 1'000'000;
constexpr int64_t kEarlyStartSlackNs = 1'000;
constexpr int64_t kMaxFutureLeadNs = static_cast<int64_t>(1) << 40;
constexpr uint64_t kPageflipDriftThreshold = 100;
constexpr uint64_t kIntervalsClamp = 1'000'000;
constexpr uint8_t kVrrVsyncStabilityThreshold = 2;
constexpr uint8_t kVrrAdaptiveStabilityThreshold = 8;
constexpr uint16_t kVrrLockoutStableFrames = 120;
constexpr auto kVrrControlThreshold = 33ms;
constexpr auto kVrrOscillationWindow = 2000ms;
constexpr uint8_t kVrrOscillationThreshold = 4;
constexpr int kMaxTimerDelayMs = std::numeric_limits<std::int16_t>::max();
constexpr int kTripleBufferEnterPct = 82;
constexpr int kTripleBufferExitPct = 70;
constexpr int kTripleBufferExitFrames = 4;
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
    , cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)
{
    updateReciprocal();
    initializeVrrCapabilities();
    lastModeSwitch = std::chrono::steady_clock::now();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    if (cachedVblankIntervalNs == 0) [[unlikely]] {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        nsToMsReciprocal = 0;
        nsToMsShift = 0;
        tripleBufferEnterThresholdNs = 0;
        tripleBufferExitThresholdNs = 0;
        return;
    }
    if (cachedVblankIntervalNs < (1ull << 32)) [[likely]] {
        constexpr uint8_t shift64 = 63;
        vblankIntervalReciprocal64 = ((1ull << shift64) + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
        reciprocalShift64 = shift64;
        const unsigned int clzResult = __builtin_clzll(cachedVblankIntervalNs);
        const uint8_t shift = static_cast<uint8_t>(64 - clzResult + 31);
        if (shift < 64) [[likely]] {
            const uint64_t numerator = 1ull << shift;
            vblankIntervalReciprocal = (numerator + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
            reciprocalShift = shift;
        } else {
            vblankIntervalReciprocal = 0;
            reciprocalShift = 0;
        }
    } else {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
    }
    constexpr uint64_t kNsPerMs = 1'000'000;
    constexpr uint8_t nsToMsShiftVal = 52;
    nsToMsReciprocal = ((1ull << nsToMsShiftVal) + kNsPerMs - 1) / kNsPerMs;
    nsToMsShift = nsToMsShiftVal;

    tripleBufferEnterThresholdNs = static_cast<int64_t>((cachedVblankIntervalNs * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((cachedVblankIntervalNs * kTripleBufferExitPct) / 100);
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
        return;
    }
    const auto capabilities = output->capabilities();
    vrrCapable = capabilities.testFlag(Output::Capability::Vrr);
    const auto config = KSharedConfig::openConfig(QStringLiteral("kwinrc"));
    const auto vrrGroup = config->group(QStringLiteral("VRR"));
    const QString policy = vrrGroup.readEntry(QStringLiteral("Policy"), QStringLiteral("Automatic"));
    const QString trimmedPolicy = policy.trimmed();
    if (trimmedPolicy.compare(QStringLiteral("Never"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Never;
        vrrEnabled = false;
    } else if (trimmedPolicy.compare(QStringLiteral("Always"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Always;
        vrrEnabled = vrrCapable;
    } else {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    }
    if (vrrEnabled) {
        qCInfo(KWIN_CORE) << "VRR enabled for output" << output->name() << "policy:" << trimmedPolicy;
    }
}

void RenderLoopPrivate::recordModeSwitch() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    const auto windowStart = now - kVrrOscillationWindow;
    uint8_t activeCount = 0;
    for (auto &entry : modeSwitchHistory) {
        if (entry.timestamp > windowStart) {
            ++activeCount;
        }
    }
    modeSwitchHistory[modeSwitchHistoryIndex].timestamp = now;
    modeSwitchHistoryIndex = (modeSwitchHistoryIndex + 1) % static_cast<uint8_t>(modeSwitchHistory.size());
    const uint8_t newActiveCount = (activeCount < modeSwitchHistory.size()) ? (activeCount + 1) : static_cast<uint8_t>(modeSwitchHistory.size());
    recentSwitchCount = newActiveCount;
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    const auto windowStart = now - kVrrOscillationWindow;
    uint8_t activeCount = 0;
    for (const auto &entry : modeSwitchHistory) {
        if (entry.timestamp > windowStart) {
            ++activeCount;
        }
    }
    recentSwitchCount = activeCount;
    if (activeCount >= kVrrOscillationThreshold) [[unlikely]] {
        if (!vrrOscillationLockout) {
            qCInfo(KWIN_CORE) << "VRR oscillation detected (" << static_cast<int>(activeCount)
                              << " switches in" << kVrrOscillationWindow.count() << "ms), entering lockout mode";
            vrrOscillationLockout = true;
            stableModeFrameCount = 0;
            lastStableMode = PresentationMode::VSync;
            return true;
        }
    }
    return false;
}

PresentationMode RenderLoopPrivate::selectPresentationMode(PresentationModeHint hint, bool isOnOutput, bool isFullScreen) noexcept
{
    if (!vrrEnabled || !vrrCapable) [[unlikely]] {
        return PresentationMode::VSync;
    }
    if (vrrOscillationLockout) [[unlikely]] {
        const PresentationMode targetMode = (vrrMode == VrrMode::Always && hint != PresentationModeHint::VSync)
            ? PresentationMode::AdaptiveSync
            : PresentationMode::VSync;
        if (presentationMode == targetMode) {
            ++stableModeFrameCount;
            if (stableModeFrameCount >= kVrrLockoutStableFrames) {
                vrrOscillationLockout = false;
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
    if (!isOnOutput || !isFullScreen) [[unlikely]] {
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

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (Q_UNLIKELY(kwinApp()->isTerminating()) || compositeTimer.isActive() || preparingNewFrame) [[unlikely]] {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

#if defined(__SIZEOF_INT128__)
[[gnu::always_inline, gnu::hot]]
static inline uint64_t fastDivideByVblank(uint64_t numerator,
                                          uint64_t reciprocal64,
                                          uint8_t shift64) noexcept
{
    const __uint128_t product = static_cast<__uint128_t>(numerator) * reciprocal64;
    return static_cast<uint64_t>(product >> shift64);
}
#else
[[gnu::always_inline, gnu::hot]]
static inline uint64_t fastDivideByVblank(uint64_t numerator,
                                          uint64_t reciprocal64,
                                          uint8_t shift64) noexcept
{
    if (reciprocal64 == 0) [[unlikely]] {
        return 0;
    }
    const uint64_t high = (numerator >> 32) * reciprocal64;
    const uint64_t low = (numerator & 0xFFFFFFFFu) * reciprocal64;
    const uint64_t result = (high >> (shift64 - 32)) + (low >> shift64);
    return result;
}
#endif

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    if (q->thread() != QThread::currentThread()) {
        QMetaObject::invokeMethod(q, [this, lastTargetTimestamp]() {
            scheduleRepaint(lastTargetTimestamp);
        }, Qt::QueuedConnection);
        return;
    }

    pendingReschedule = false;
    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) [[unlikely]] {
        return;
    }
    const int64_t nowNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresNs = lastPresentationTimestamp.count();
    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyNs = safetyMargin.count();
    const int64_t vblankSigned = static_cast<int64_t>(vblankIntervalNs);
    const int64_t maxCompositeNs = vblankSigned * 2;
    const int64_t baseCompositeNs = predictedRenderNs + safetyNs + kPredictionSlackNs;
    int64_t expectedCompositeNs = std::clamp(baseCompositeNs, int64_t{0}, maxCompositeNs);

    PresentationModeHint currentHint = PresentationModeHint::VSync;
    bool isOnOutput = false;
    bool isFullScreen = false;
    if (vrrEnabled && vrrCapable) [[unlikely]] {
        Workspace *const ws = workspace();
        if (ws && output) [[likely]] {
            Window *const activeWindow = ws->activeWindow();
            if (activeWindow) [[likely]] {
                isOnOutput = activeWindow->isOnOutput(output);
                if (isOnOutput) {
                    isFullScreen = activeWindow->isFullScreen();
                    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
                    if (surfaceItem) [[likely]] {
                        if (auto *const waylandItem = qobject_cast<SurfaceItemWayland *>(surfaceItem)) {
                            if (auto *const surface = waylandItem->surface()) {
                                currentHint = surface->presentationModeHint();
                            }
                        }
                    }
                }
            }
        }
    }

    const PresentationMode targetMode = selectPresentationMode(currentHint, isOnOutput, isFullScreen);
    if (targetMode != presentationMode) [[unlikely]] {
        const uint8_t threshold = (targetMode == PresentationMode::VSync)
            ? kVrrVsyncStabilityThreshold
            : kVrrAdaptiveStabilityThreshold;
        if (vrrStabilityCounter >= threshold) {
            presentationMode = targetMode;
            lastStableMode = targetMode;
            vrrStabilityCounter = 0;
            recordModeSwitch();
            detectVrrOscillation();
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
            const int64_t earlyStart = vblankSigned - kEarlyStartSlackNs;
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
            wasTripleBuffering = true;
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
                wasTripleBuffering = false;
            } else if (flipsInAdvance < 2) {
                flipsInAdvance = 2;
            }
        }
        uint64_t flipsTarget;
        if (compositeTimer.isActive() && nextPresentationTimestamp.count() > 0) [[unlikely]] {
            const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresNs;
            if (deltaNs > 0) {
                const uint64_t num = static_cast<uint64_t>(deltaNs) + vblankIntervalNs / 2;
                const uint64_t intervals = std::clamp(num / vblankIntervalNs, uint64_t{1}, kIntervalsClamp);
                flipsTarget = intervals;
            } else {
                flipsTarget = 1;
            }
        } else {
            const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresNs;
            uint64_t flipsToTarget = 0;
            if (toTargetNs > 0 && vblankIntervalReciprocal64 != 0) [[likely]] {
                flipsToTarget = fastDivideByVblank(static_cast<uint64_t>(toTargetNs),
                                                   vblankIntervalReciprocal64,
                                                   reciprocalShift64);
            }
            flipsTarget = std::max(flipsSince + flipsInAdvance, flipsToTarget + 1);
        }
        if (__builtin_mul_overflow(flipsTarget, vblankIntervalNs, &nextPresNs)) [[unlikely]] {
            nextPresNs = std::numeric_limits<int64_t>::max() - lastPresNs;
        }
        if (__builtin_add_overflow(lastPresNs, nextPresNs, &nextPresNs)) [[unlikely]] {
            nextPresNs = std::numeric_limits<int64_t>::max();
        }
    } else {
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;
        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) [[unlikely]] {
            if (__builtin_add_overflow(nowNs, expectedCompositeNs, &nextPresNs)) [[unlikely]] {
                nextPresNs = std::numeric_limits<int64_t>::max();
            }
        } else {
            const int64_t minIntervalNs = vblankSigned;
            int64_t tentativeNs;
            if (__builtin_add_overflow(nowNs, expectedCompositeNs, &tentativeNs)) [[unlikely]] {
                tentativeNs = std::numeric_limits<int64_t>::max();
            }
            const int64_t sinceLastNs = nowNs - lastPresNs;
            if (sinceLastNs < minIntervalNs) [[likely]] {
                int64_t minNext;
                if (__builtin_add_overflow(lastPresNs, minIntervalNs, &minNext)) [[unlikely]] {
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
    nextPresNs = std::clamp(nextPresNs, clampLower, clampUpper);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};
    int64_t nextRenderNs;
    if (__builtin_sub_overflow(nextPresNs, expectedCompositeNs, &nextRenderNs)) [[unlikely]] {
        nextRenderNs = 0;
    }
    int64_t delayNs = nextRenderNs - nowNs;
    if (delayNs < 0) {
        delayNs = 0;
    }
    const uint64_t delayNsU64 = static_cast<uint64_t>(delayNs);
    int64_t delayMs64;
    if (nsToMsReciprocal != 0) [[likely]] {
        delayMs64 = static_cast<int64_t>((delayNsU64 * nsToMsReciprocal) >> nsToMsShift);
    } else {
        delayMs64 = (delayNs + (kNanosecondsPerMillisecond - 1)) / kNanosecondsPerMillisecond;
    }
    const int delayMs = static_cast<int>(std::clamp(delayMs64, int64_t{0}, static_cast<int64_t>(kMaxTimerDelayMs)));
    const int threshold = (delayMs <= 8) ? 0 : ((delayMs < 16) ? 1 : 2);
    const int delta = (scheduledTimerMs >= 0) ? std::abs(delayMs - scheduledTimerMs) : delayMs;
    if (!compositeTimer.isActive() || delta > threshold) [[likely]] {
        scheduledTimerMs = static_cast<int16_t>(delayMs);
        compositeTimer.start(delayMs, Qt::PreciseTimer, q);
    }
}

void RenderLoopPrivate::delayScheduleRepaint() noexcept
{
    pendingReschedule = true;
}

void RenderLoopPrivate::notifyFrameDropped()
{
    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    if (!inhibitCount && pendingReschedule) [[likely]] {
        scheduleNextRepaint();
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
    if (!d->output || !d->m_debugOutput) {
        if (!d->output) {
            return;
        }
        const QString name = d->output->name();
        std::string sanitizedName;
        sanitizedName.reserve(static_cast<size_t>(name.size()));
        for (const QChar ch : name) {
            const char16_t unicode = ch.unicode();
            if ((unicode >= u'a' && unicode <= u'z') ||
                (unicode >= u'A' && unicode <= u'Z') ||
                (unicode >= u'0' && unicode <= u'9') ||
                unicode == u'_' || unicode == u'-') {
                sanitizedName.push_back(static_cast<char>(unicode & 0xFF));
            } else {
                sanitizedName.push_back('_');
            }
        }
        const std::string filename = "kwin_perf_" + sanitizedName + ".csv";
        d->m_debugOutput = std::fstream(filename, std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *(d->m_debugOutput)
                << "target_pageflip_ns,pageflip_ns,render_start_ns,render_end_ns,"
                << "safety_margin_ns,refresh_duration_ns,vrr,tearing,predicted_render_ns\n"
                << std::flush;
        }
    }
    if (!d->m_debugOutput || !d->m_debugOutput->is_open()) {
        return;
    }
    const RenderTimeSpan times = renderTime.value_or(RenderTimeSpan{});
    const bool vrr = (mode == PresentationMode::AdaptiveSync || mode == PresentationMode::AdaptiveAsync);
    const bool tearing = (mode == PresentationMode::Async || mode == PresentationMode::AdaptiveAsync);
    *(d->m_debugOutput) << frame->targetPageflipTime().time_since_epoch().count() << ','
                        << timestamp.count() << ','
                        << times.start.time_since_epoch().count() << ','
                        << times.end.time_since_epoch().count() << ','
                        << d->safetyMargin.count() << ','
                        << frame->refreshDuration().count() << ','
                        << (vrr ? 1 : 0) << ','
                        << (tearing ? 1 : 0) << ','
                        << frame->predictedRenderTime().count() << '\n';
}
}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    if (s_printDebugInfo) [[unlikely]] {
        writeDebugOutput(this, renderTime, frame, timestamp, mode);
    }
    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    notifyVblank(timestamp);
    if (renderTime) [[likely]] {
        renderJournal.add(renderTime->end - renderTime->start, timestamp);
    }
    if (compositeTimer.isActive()) [[unlikely]] {
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
        qCDebug(KWIN_CORE,
                "Got invalid presentation timestamp: %lld (current %lld)",
                static_cast<long long>(timestamp.count()),
                static_cast<long long>(lastPresentationTimestamp.count()));
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
    d->preparingNewFrame = true;
}

void RenderLoop::newFramePrepared()
{
    d->preparingNewFrame = false;
}

[[nodiscard]] int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int refreshRate)
{
    constexpr int kMinRefreshRate = 1'000;
    constexpr int kMaxRefreshRate = 1'000'000;
    const int rr = std::clamp(refreshRate, kMinRefreshRate, kMaxRefreshRate);
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

    const bool vrr =
        (d->presentationMode == PresentationMode::AdaptiveSync ||
         d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing =
        (d->presentationMode == PresentationMode::Async ||
         d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool checkOwnership = (item || outputLayer) && (vrr || tearing);
    const bool activeControlsVrr = checkOwnership ? activeWindowControlsVrrRefreshRate() : false;
    if ((vrr || tearing) && checkOwnership && activeControlsVrr) [[unlikely]] {
        Workspace *const ws = workspace();
        if (ws && d->output) [[likely]] {
            Window *const activeWindow = ws->activeWindow();
            if (activeWindow && activeWindow->isOnOutput(d->output)) [[likely]] {
                SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
                if (surfaceItem && item && item != surfaceItem && !surfaceItem->isAncestorOf(item)) [[unlikely]] {
                    if (d->pendingFrameCount > 0) {
                        const uint64_t delayNs = d->cachedVblankIntervalNs;
                        const int64_t delayMsInt64 = static_cast<int64_t>(
                            (delayNs + (static_cast<uint64_t>(kNanosecondsPerMillisecond) - 1)) / static_cast<uint64_t>(kNanosecondsPerMillisecond)
                        );
                        if (delayMsInt64 > 0 && delayMsInt64 <= kMaxTimerDelayMs) [[likely]] {
                            d->delayedVrrTimer.start(static_cast<int>(delayMsInt64), Qt::PreciseTimer, this);
                            return;
                        }
                    }
                }
            }
        }
    }
    d->delayedVrrTimer.stop();
    int effectiveMaxPendingFrameCount = d->maxPendingFrameCount;
    if (vrr || tearing) [[unlikely]] {
        effectiveMaxPendingFrameCount = 1;
    }
    effectiveMaxPendingFrameCount = std::max(effectiveMaxPendingFrameCount, 1);
    if (d->pendingFrameCount < effectiveMaxPendingFrameCount && !d->inhibitCount) [[likely]] {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

[[nodiscard]] bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *const ws = workspace();
    if (!ws) [[unlikely]] {
        return false;
    }
    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow || !d->output || !activeWindow->isOnOutput(d->output)) [[unlikely]] {
        return false;
    }
    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
    return surfaceItem && (surfaceItem->recursiveFrameTimeEstimation() <= kVrrControlThreshold);
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
    d->maxPendingFrameCount = std::clamp(static_cast<int>(maxCount), 1, INT_MAX);
}

[[nodiscard]] std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

}

#include "moc_renderloop.cpp"
