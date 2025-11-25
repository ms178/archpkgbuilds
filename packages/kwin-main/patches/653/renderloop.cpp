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
#include "utils/common.h"
#include "wayland/surface.h"
#include "window.h"
#include "workspace.h"

#include <KConfigGroup>
#include <KSharedConfig>

#include <QThread>

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <limits>
#include <string>

using namespace std::chrono_literals;

namespace
{

constexpr int64_t kNsPerMs = 1'000'000;
constexpr int64_t kNsPerSecond = 1'000'000'000;
constexpr int64_t kPredictionSlackNs = 200'000;
constexpr int64_t kMaxFutureLeadNs = int64_t{1} << 34;
constexpr uint64_t kPageflipDriftThreshold = 32;
constexpr uint64_t kIntervalsClamp = 50'000;

constexpr uint8_t kVrrVsyncStabilityFrames = 6;
constexpr uint8_t kVrrAdaptiveStabilityFrames = 3;
constexpr uint16_t kVrrLockoutStableFrames = 20;
constexpr auto kVrrControlThreshold = 25ms;
constexpr auto kVrrOscillationWindow = 250ms;
constexpr uint8_t kVrrOscillationThreshold = 8;

constexpr int kMaxTimerDelayMs = 16000;
constexpr int kTripleBufferEnterPct = 78;
constexpr int kTripleBufferExitPct = 60;
constexpr int kTripleBufferExitFrames = 2;

constexpr int kMinVrrRefreshMhz = 48'000;
constexpr int kMaxVrrRefreshMhz = 165'000;
constexpr int kVrrTargetPercentile = 98;

constexpr uint16_t kStarvationRecoveryFrames = 16;
constexpr uint8_t kMaxConsecutiveErrors = 2;
constexpr int kMaxErrorBackoffMs = 250;

constexpr int64_t kTimerRescheduleThresholdNs = 80'000;

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t branchlessAbs(int64_t v) noexcept
{
    const int64_t mask = v >> 63;
    return (v ^ mask) - mask;
}

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t branchlessMax(int64_t a, int64_t b) noexcept
{
    const int64_t diff = a - b;
    const int64_t mask = diff >> 63;
    return a - (diff & mask);
}

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t branchlessMin(int64_t a, int64_t b) noexcept
{
    const int64_t diff = b - a;
    const int64_t mask = diff >> 63;
    return a + (diff & mask);
}

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t branchlessClamp(int64_t v, int64_t lo, int64_t hi) noexcept
{
    const int64_t tLo = branchlessMax(v, lo);
    return branchlessMin(tLo, hi);
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessMaxU(uint64_t a, uint64_t b) noexcept
{
    return (a > b) ? a : b;
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessMinU(uint64_t a, uint64_t b) noexcept
{
    return (a < b) ? a : b;
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessClampU(uint64_t v, uint64_t lo, uint64_t hi) noexcept
{
    const uint64_t tLo = branchlessMaxU(v, lo);
    return branchlessMinU(tLo, hi);
}

[[gnu::always_inline, gnu::hot, gnu::const]]
inline uint64_t fastDiv64(uint64_t n, uint64_t recip, uint8_t shift) noexcept
{
#if defined(__BMI2__) && defined(__x86_64__)
    uint64_t hi, lo;
    __asm__ __volatile__("mulxq %3, %0, %1"
                         : "=r"(lo), "=r"(hi)
                         : "d"(n), "r"(recip)
                         :);
    if (__builtin_constant_p(shift)) {
        if (shift == 63) {
            return (hi << 1) | (lo >> 63);
        } else if (shift >= 64) {
            return hi >> (shift - 64);
        } else {
            return (hi << (64 - shift)) | (lo >> shift);
        }
    }
    const uint8_t lowShift = shift & 63;
    const uint8_t highShift = 64 - lowShift;
    const bool useHigh = shift >= 64;
    const uint64_t lowResult = (hi << highShift) | (lo >> lowShift);
    const uint64_t highResult = hi >> (shift - 64 * useHigh);
    return useHigh ? highResult : lowResult;
#else
    return static_cast<uint64_t>((static_cast<__uint128_t>(n) * recip) >> shift);
#endif
}

}

namespace KWin
{

static const bool s_debugEnabled = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

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
    , cachedVblankIntervalNs(1'000'000'000'000ULL / 60'000ULL)
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
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    lastPresentationTimestamp = now.time_since_epoch();
}

RenderLoopPrivate::~RenderLoopPrivate()
{
    disconnectVrrSignals();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    const uint64_t interval = cachedVblankIntervalNs;
    if (__builtin_expect(interval == 0, 0)) {
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        tripleBufferEnterThresholdNs = 0;
        tripleBufferExitThresholdNs = 0;
        vrrTargetIntervalNs = 0;
        return;
    }

    constexpr uint8_t shift = 63;
    const __uint128_t num = __uint128_t{1} << shift;
    vblankIntervalReciprocal64 = static_cast<uint64_t>((num + interval - 1) / interval);
    reciprocalShift64 = shift;

    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);

    const uint64_t maxIntervalNs = 1'000'000'000'000ULL / kMinVrrRefreshMhz;
    const uint64_t minIntervalNs = 1'000'000'000'000ULL / kMaxVrrRefreshMhz;
    const uint64_t range = maxIntervalNs - minIntervalNs;
    const uint64_t targetNs = minIntervalNs + (range * (100 - kVrrTargetPercentile)) / 100;
    vrrTargetIntervalNs = branchlessMaxU(targetNs, interval);
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (__builtin_expect(!output, 0)) {
        vrrCapable = 0;
        vrrEnabled = 0;
        return;
    }

    vrrCapable = output->capabilities().testFlag(Output::Capability::Vrr) ? 1 : 0;

    const auto config = KSharedConfig::openConfig(QStringLiteral("kwinrc"));
    const auto group = config->group(QStringLiteral("VRR"));
    const QString policy = group.readEntry(QStringLiteral("Policy"), QStringLiteral("Automatic"));

    if (policy.compare(QLatin1String("Never"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Never;
        vrrEnabled = 0;
    } else if (policy.compare(QLatin1String("Always"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Always;
        vrrEnabled = vrrCapable;
    } else {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    }
}

void RenderLoopPrivate::connectVrrSignals(Window *window)
{
    if (!window || !vrrEnabled || trackedWindow_ == window) {
        return;
    }

    disconnectVrrSignals();
    trackedWindow_ = window;
    vrrConnectionCount_ = 0;

    auto store = [this](QMetaObject::Connection c) {
        if (vrrConnectionCount_ < 4) {
            vrrConnections_[vrrConnectionCount_++] = c;
        }
    };

    store(QObject::connect(window, &QObject::destroyed, q, [this]() {
        trackedWindow_ = nullptr;
        disconnectVrrSignals();
    }));

    store(QObject::connect(window, &Window::fullScreenChanged, q, [this]() {
        vrrStateDirty_ = true;
    }));

    store(QObject::connect(window, &Window::outputChanged, q, [this]() {
        vrrStateDirty_ = true;
    }));

    SurfaceItem *surf = window->surfaceItem();
    if (surf) {
        auto *wayland = qobject_cast<SurfaceItemWayland *>(surf);
        if (wayland) {
            auto *surface = wayland->surface();
            if (surface) {
                store(QObject::connect(surface, &SurfaceInterface::presentationModeHintChanged, q, [this]() {
                    vrrStateDirty_ = true;
                }));
            }
        }
    }
}

void RenderLoopPrivate::disconnectVrrSignals() noexcept
{
    const uint8_t count = vrrConnectionCount_;
    for (uint8_t i = 0; i < count; ++i) {
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
    vrrStateDirty_ = false;

    VrrStateCache::State state{};
    const bool canVrr = static_cast<bool>(vrrEnabled) && static_cast<bool>(vrrCapable);
    if (canVrr) {
        Workspace *ws = workspace();
        Window *active = ws ? ws->activeWindow() : nullptr;
        if (active && output) {
            connectVrrSignals(active);
            const bool onOutput = active->isOnOutput(output);
            state.isOnOutput = onOutput ? 1 : 0;
            if (onOutput) {
                state.isFullScreen = active->isFullScreen() ? 1 : 0;
                SurfaceItem *surf = active->surfaceItem();
                if (surf) {
                    auto *wayland = qobject_cast<SurfaceItemWayland *>(surf);
                    if (wayland) {
                        auto *surface = wayland->surface();
                        if (surface) {
                            state.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3;
                            state.valid = 1;
                        }
                    }
                }
            }
        }
    }

    vrrStateCache_.setState(state);
}

void RenderLoopPrivate::recordModeSwitch() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    modeSwitchBitmap = (modeSwitchBitmap << 1) | 1;

    if (oldestSwitchTime.time_since_epoch().count() == 0) {
        oldestSwitchTime = now;
    }
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    const auto cutoff = now - kVrrOscillationWindow;

    if (oldestSwitchTime < cutoff) {
        const auto elapsed = now - oldestSwitchTime;
        const auto windowCount = elapsed / kVrrOscillationWindow;
        const uint64_t shifts = static_cast<uint64_t>(windowCount);
        if (shifts >= 64) {
            modeSwitchBitmap = 0;
            oldestSwitchTime = {};
        } else {
            modeSwitchBitmap >>= shifts;
            oldestSwitchTime = cutoff;
        }
    }

    const auto count = static_cast<uint8_t>(__builtin_popcountll(modeSwitchBitmap));
    recentSwitchCount = count;

    if (__builtin_expect(count >= kVrrOscillationThreshold && !vrrOscillationLockout, 0)) {
        vrrOscillationLockout = 1;
        stableModeFrameCount = 0;
        lastStableMode = presentationMode;
        return true;
    }
    return false;
}

PresentationMode RenderLoopPrivate::selectPresentationMode() noexcept
{
    const bool canVrr = static_cast<bool>(vrrEnabled) && static_cast<bool>(vrrCapable);
    if (__builtin_expect(!canVrr, 0)) {
        return PresentationMode::VSync;
    }

    const auto state = vrrStateCache_.getState();
    if (__builtin_expect(!state.valid, 0)) {
        return PresentationMode::VSync;
    }

    const auto hint = static_cast<PresentationModeHint>(state.hint);

    if (__builtin_expect(vrrOscillationLockout, 0)) {
        const bool wantsAdaptive = (vrrMode == VrrMode::Always) && (hint != PresentationModeHint::VSync);
        const auto target = wantsAdaptive ? PresentationMode::AdaptiveSync : PresentationMode::VSync;
        stableModeFrameCount = (presentationMode == target) ? stableModeFrameCount + 1 : 0;
        if (stableModeFrameCount >= kVrrLockoutStableFrames) {
            vrrOscillationLockout = 0;
            stableModeFrameCount = 0;
        }
        return lastStableMode;
    }

    if (vrrMode == VrrMode::Always) {
        if (hint == PresentationModeHint::Async) {
            return PresentationMode::AdaptiveAsync;
        }
        return (hint != PresentationModeHint::VSync) ? PresentationMode::AdaptiveSync : PresentationMode::VSync;
    }

    if (__builtin_expect(!state.isOnOutput || !state.isFullScreen, 0)) {
        return PresentationMode::VSync;
    }

    if (hint == PresentationModeHint::Async) {
        return PresentationMode::AdaptiveAsync;
    }
    if (hint == PresentationModeHint::VSync) {
        return PresentationMode::VSync;
    }
    return PresentationMode::AdaptiveSync;
}

void RenderLoopPrivate::updateFramePrediction(std::chrono::nanoseconds measured) noexcept
{
    const int64_t m = measured.count();
    if (__builtin_expect(m <= 0, 0)) {
        return;
    }

    const int64_t cur = framePrediction.count();
    const int64_t updated = (cur * 192 + m * 64) >> 8;
    const int64_t vblank = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t lo = vblank >> 2;
    const int64_t hi = vblank + (vblank >> 1);
    framePrediction = std::chrono::nanoseconds{branchlessClamp(updated, lo, hi)};
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    const bool blocked = kwinApp()->isTerminating() || compositeTimer.isActive() || preparingNewFrame;
    if (__builtin_expect(blocked, 0)) {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTarget)
{
    if (__builtin_expect(q->thread() != QThread::currentThread(), 0)) {
        QMetaObject::invokeMethod(q, [this, lastTarget]() {
            scheduleRepaint(lastTarget);
        }, Qt::QueuedConnection);
        return;
    }

    pendingReschedule = 0;

    const uint64_t vblankNs = cachedVblankIntervalNs;
    if (__builtin_expect(vblankNs == 0, 0)) {
        return;
    }

    const int64_t nowNs = std::chrono::steady_clock::now().time_since_epoch().count();
    int64_t lastPresNs = lastPresentationTimestamp.count();

    const bool staleTime = (lastPresNs <= 0) || ((nowNs - lastPresNs) > 3 * kNsPerSecond);
    if (__builtin_expect(staleTime, 0)) {
        lastPresNs = nowNs;
        lastPresentationTimestamp = std::chrono::nanoseconds{nowNs};
    }

    const int64_t predNs = framePrediction.count() > 0 ? framePrediction.count() : renderJournal.result().count();
    const int64_t vblankSigned = static_cast<int64_t>(vblankNs);
    const int64_t rawComposite = predNs + safetyMargin.count() + kPredictionSlackNs;
    const int64_t compositeNs = branchlessClamp(rawComposite, int64_t{0}, vblankSigned << 1);

    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();

    if (__builtin_expect(targetMode != presentationMode, 0)) {
        const uint8_t thresh = (targetMode == PresentationMode::VSync) ? kVrrVsyncStabilityFrames : kVrrAdaptiveStabilityFrames;
        ++vrrStabilityCounter;
        if (vrrStabilityCounter >= thresh) {
            presentationMode = targetMode;
            lastStableMode = targetMode;
            vrrStabilityCounter = 0;
            starvationRecoveryCounter = 0;
            recordModeSwitch();
            oscillationCheckCounter_ = (oscillationCheckCounter_ + 1) & 15;
            if (oscillationCheckCounter_ == 0) {
                detectVrrOscillation();
            }
        }
    } else {
        vrrStabilityCounter = 0;
    }

    int64_t nextPresNs;
    const bool isVSync = presentationMode == PresentationMode::VSync;

    if (__builtin_expect(isVSync, 0)) {
        const int64_t sinceLastNs = nowNs - lastPresNs;
        uint64_t flipsSince = 0;
        if (sinceLastNs > 0 && vblankIntervalReciprocal64 != 0) {
            flipsSince = fastDiv64(static_cast<uint64_t>(sinceLastNs), vblankIntervalReciprocal64, reciprocalShift64);
        }

        const int64_t earlyStart = vblankSigned - 500;
        const bool drifted = flipsSince > kPageflipDriftThreshold;
        const int64_t adjCompositeNs = drifted ? branchlessMax(compositeNs, earlyStart) : compositeNs;

        uint64_t flipsAhead = 1;
        if (__builtin_expect(adjCompositeNs > vblankSigned, 0)) {
            const uint64_t need = (static_cast<uint64_t>(adjCompositeNs) + vblankNs - 1) / vblankNs;
            const uint64_t maxFlips = static_cast<uint64_t>(branchlessMax(maxPendingFrameCount, 1));
            flipsAhead = branchlessClampU(need, 1, maxFlips);
        }

        const bool highLoad = adjCompositeNs > tripleBufferEnterThresholdNs;
        const bool lowLoad = adjCompositeNs < tripleBufferExitThresholdNs;

        if (__builtin_expect(highLoad, 0)) {
            wasTripleBuffering = 1;
            doubleBufferingCounter = 0;
            flipsAhead = branchlessMaxU(flipsAhead, 2);
        } else if (__builtin_expect(wasTripleBuffering, 0)) {
            doubleBufferingCounter = lowLoad ? doubleBufferingCounter + 1 : 0;
            if (doubleBufferingCounter >= kTripleBufferExitFrames) {
                wasTripleBuffering = 0;
            }
            if (wasTripleBuffering) {
                flipsAhead = branchlessMaxU(flipsAhead, 2);
            }
        }

        uint64_t flipsTarget;
        const bool hasScheduled = compositeTimer.isActive() && (nextPresentationTimestamp.count() > 0);

        if (__builtin_expect(hasScheduled, 0)) {
            const int64_t delta = nextPresentationTimestamp.count() - lastPresNs;
            const uint64_t intervals = (delta > 0) ? (static_cast<uint64_t>(delta) + (vblankNs >> 1)) / vblankNs : 1;
            flipsTarget = branchlessClampU(intervals, 1, kIntervalsClamp);
        } else {
            const int64_t toTargetNs = lastTarget.count() - lastPresNs;
            uint64_t flipsToTarget = 0;
            if (toTargetNs > 0 && vblankIntervalReciprocal64 != 0) {
                const uint64_t adjusted = static_cast<uint64_t>(toTargetNs) + (vblankNs >> 1);
                flipsToTarget = fastDiv64(adjusted, vblankIntervalReciprocal64, reciprocalShift64);
            }
            flipsTarget = branchlessMaxU(flipsSince + flipsAhead, flipsToTarget + 1);
        }

        if (__builtin_expect(flipsTarget > UINT64_MAX / vblankNs, 0)) {
            nextPresNs = std::numeric_limits<int64_t>::max();
        } else {
            nextPresNs = lastPresNs + static_cast<int64_t>(flipsTarget * vblankNs);
        }
    } else {
        wasTripleBuffering = 0;
        doubleBufferingCounter = 0;
        const bool isAsync = (presentationMode == PresentationMode::Async) ||
                             (presentationMode == PresentationMode::AdaptiveAsync);
        if (__builtin_expect(isAsync, 0)) {
            nextPresNs = nowNs + compositeNs;
        } else {
            const int64_t targetInterval = static_cast<int64_t>(vrrTargetIntervalNs);
            const int64_t sinceLast = nowNs - lastPresNs;
            const int64_t tentative = nowNs + compositeNs;
            const int64_t minNext = lastPresNs + targetInterval;
            nextPresNs = (sinceLast < targetInterval) ? branchlessMax(tentative, minNext) : tentative;
        }
    }

    nextPresNs = branchlessMax(nextPresNs, nowNs);
    nextPresNs = branchlessMin(nextPresNs, nowNs + kMaxFutureLeadNs);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    const int64_t nextRenderNs = branchlessMax(nextPresNs - compositeNs, int64_t{0});
    const int64_t delayNs = branchlessMax(nextRenderNs - nowNs, int64_t{0});

    if (compositeTimer.isActive()) {
        const int64_t diff = scheduledRenderTimestamp.count() - nextRenderNs;
        if (branchlessAbs(diff) < kTimerRescheduleThresholdNs) {
            return;
        }
    }

    const int64_t delayMs = (delayNs + kNsPerMs - 1) / kNsPerMs;
    const int clampedMs = static_cast<int>(branchlessMin(delayMs, static_cast<int64_t>(kMaxTimerDelayMs)));

    scheduledRenderTimestamp = std::chrono::nanoseconds{nextRenderNs};
    scheduledTimerMs = static_cast<int16_t>(clampedMs);
    compositeTimer.start(clampedMs, Qt::PreciseTimer, q);
}

void RenderLoopPrivate::delayScheduleRepaint() noexcept
{
    pendingReschedule = 1;
}

void RenderLoopPrivate::notifyFrameDropped()
{
    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    ++consecutiveErrorCount;

    if (inhibitCount == 0 && pendingReschedule) {
        if (consecutiveErrorCount > kMaxConsecutiveErrors) {
            const int exp = consecutiveErrorCount - kMaxConsecutiveErrors;
            const int capped = static_cast<int>(branchlessMin(static_cast<int64_t>(exp), int64_t{8}));
            int delay = 1 << capped;
            delay = static_cast<int>(branchlessMin(static_cast<int64_t>(delay), static_cast<int64_t>(kMaxErrorBackoffMs)));
            const int minDelay = static_cast<int>(cachedVblankIntervalNs / kNsPerMs);
            delay = static_cast<int>(branchlessMax(static_cast<int64_t>(delay), static_cast<int64_t>(minDelay)));
            compositeTimer.start(delay, Qt::PreciseTimer, q);
        } else {
            scheduleNextRepaint();
        }
    }
}

namespace
{

void writeDebug(RenderLoopPrivate *d,
                std::optional<RenderTimeSpan> rt,
                OutputFrame *frame,
                std::chrono::nanoseconds ts,
                PresentationMode mode)
{
    if (!d->m_debugOutput) {
        if (!d->output) {
            return;
        }
        std::string name = d->output->name().toStdString();
        for (char &c : name) {
            c = std::isalnum(static_cast<unsigned char>(c)) ? c : '_';
        }
        d->m_debugOutput = std::fstream("kwin_perf_" + name + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *d->m_debugOutput << "target,flip,start,end,margin,dur,vrr,pred\n";
        }
    }

    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const auto times = rt.value_or(RenderTimeSpan{});
        const bool vrr = (mode == PresentationMode::AdaptiveSync) ||
                         (mode == PresentationMode::AdaptiveAsync);
        *d->m_debugOutput << frame->targetPageflipTime().time_since_epoch().count() << ','
                          << ts.count() << ','
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

    if (__builtin_expect(s_debugEnabled, 0)) {
        writeDebug(this, renderTime, frame, timestamp, mode);
    }

    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    notifyVblank(timestamp);

    if (renderTime) {
        const auto dur = renderTime->end - renderTime->start;
        renderJournal.add(dur, timestamp);
        updateFramePrediction(dur);

        if (presentationMode != PresentationMode::VSync) {
            const int64_t frameNs = dur.count();
            const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
            if (frameNs > vblankNs) {
                const uint16_t next = starvationRecoveryCounter + 1;
                starvationRecoveryCounter = static_cast<uint16_t>(branchlessMinU(next, kStarvationRecoveryFrames));
            } else if (starvationRecoveryCounter > 0) {
                --starvationRecoveryCounter;
            }
        }
    }

    if (__builtin_expect(compositeTimer.isActive(), 0)) {
        scheduleRepaint(lastPresentationTimestamp);
    }

    if (inhibitCount == 0 && pendingReschedule) {
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp)
{
    lastPresentationTimestamp = (timestamp >= lastPresentationTimestamp)
        ? timestamp
        : std::chrono::steady_clock::now().time_since_epoch();
}

void RenderLoop::timerEvent(QTimerEvent *event)
{
    const int id = event->timerId();
    if (id == d->compositeTimer.timerId()) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (__builtin_expect(id == d->delayedVrrTimer.timerId(), 0)) {
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
    if (++d->inhibitCount == 1) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
    }
}

void RenderLoop::uninhibit()
{
    Q_ASSERT(d->inhibitCount > 0);
    if (--d->inhibitCount == 0) {
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

int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int rate)
{
    rate = static_cast<int>(branchlessClamp(rate, 1'000, 1'000'000));
    if (d->refreshRate == rate) {
        return;
    }
    d->refreshRate = rate;
    d->cachedVblankIntervalNs = 1'000'000'000'000ULL / static_cast<uint64_t>(rate);
    d->updateReciprocal();
    Q_EMIT refreshRateChanged();

    if (d->inhibitCount == 0) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->scheduleNextRepaint();
    }
}

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds margin)
{
    d->safetyMargin = margin.count() > 0 ? margin : 0ns;
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *layer)
{
    if (__builtin_expect(thread() != QThread::currentThread(), 0)) {
        QMetaObject::invokeMethod(this, [this, item, layer]() {
            scheduleRepaint(item, layer);
        }, Qt::QueuedConnection);
        return;
    }

    d->vrrStateDirty_ = true;

    const bool vrrActive = d->presentationMode != PresentationMode::VSync;
    const bool hasTarget = item || layer;

    if (vrrActive && hasTarget) {
        if (activeWindowControlsVrrRefreshRate()) {
            Workspace *ws = workspace();
            Window *active = ws ? ws->activeWindow() : nullptr;
            if (active && d->output && active->isOnOutput(d->output)) {
                SurfaceItem *surface = active->surfaceItem();
                const bool isOther = item && surface && (item != surface) && !surface->isAncestorOf(item);
                if (isOther && d->pendingFrameCount > 0) {
                    const int delayMs = 1000 / (d->refreshRate / 1000);
                    d->delayedVrrTimer.start(delayMs, Qt::PreciseTimer, this);
                    return;
                }
            }
        }
    }

    d->delayedVrrTimer.stop();

    int maxPending = d->maxPendingFrameCount;
    if (vrrActive) {
        maxPending = (d->starvationRecoveryCounter >= kStarvationRecoveryFrames) ? 2 : 1;
    }

    if (d->pendingFrameCount < maxPending && d->inhibitCount == 0) {
        d->scheduleNextRepaint();
    } else {
        d->pendingReschedule = 1;
    }
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *ws = workspace();
    if (!ws) {
        return false;
    }
    Window *active = ws->activeWindow();
    if (!active || !d->output || !active->isOnOutput(d->output)) {
        return false;
    }
    SurfaceItem *surface = active->surfaceItem();
    return surface && (surface->recursiveFrameTimeEstimation() <= kVrrControlThreshold);
}

std::chrono::nanoseconds RenderLoop::lastPresentationTimestamp() const
{
    return d->lastPresentationTimestamp;
}

std::chrono::nanoseconds RenderLoop::nextPresentationTimestamp() const
{
    return d->nextPresentationTimestamp;
}

void RenderLoop::setPresentationMode(PresentationMode mode)
{
    d->presentationMode = mode;
}

void RenderLoop::setMaxPendingFrameCount(uint32_t count)
{
    d->maxPendingFrameCount = static_cast<int>(branchlessClampU(count, 1, 3));
}

std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

}

#include "moc_renderloop.cpp"
