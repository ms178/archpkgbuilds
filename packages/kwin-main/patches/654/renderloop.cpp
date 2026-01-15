/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-FileCopyrightText: 2025 Performance Engineering Team

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
#include <fstream>
#include <limits>
#include <string>

using namespace std::chrono_literals;

namespace
{

constexpr int64_t kNsPerMs = 1'000'000;
constexpr int64_t kMinCompositeNs = 200'000;
constexpr int64_t kMaxCompositeNs = 33'000'000;
constexpr int64_t kRenderSlackNs = 300'000;

constexpr int16_t kCadenceStableThreshold = 224;
constexpr int16_t kCadenceUnstableThreshold = 64;
constexpr int64_t kMaxReasonableIntervalNs = 100'000'000;
constexpr int64_t kMinReasonableIntervalNs = 2'000'000;

constexpr uint16_t kVrrToVsyncStabilityFrames = 4;
constexpr uint16_t kVsyncToVrrStabilityFrames = 2;
constexpr uint16_t kMinModeDwellFrames = 2;
constexpr uint16_t kOscillationCooldownFrames = 120;
constexpr auto kVrrControlThreshold = 25ms;
constexpr auto kOscillationWindow = 500ms;
constexpr uint8_t kOscillationThreshold = 10;

constexpr int kMaxTimerDelayMs = 16000;
constexpr int kTripleBufferEnterPct = 90;
constexpr int kTripleBufferExitPct = 70;
constexpr uint8_t kTripleBufferHysteresisLimit = 5;

constexpr uint16_t kStarvationRecoveryFrames = 4;
constexpr uint8_t kMaxConsecutiveErrors = 3;
constexpr int kMaxErrorBackoffMs = 50;

constexpr int64_t kTimerJitterFilterNs = 50'000;

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t safeAbs64(int64_t v) noexcept
{
    if (v == std::numeric_limits<int64_t>::min()) [[unlikely]] {
        return std::numeric_limits<int64_t>::max();
    }
    return v < 0 ? -v : v;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isVrrMode(KWin::PresentationMode mode) noexcept
{
    return mode == KWin::PresentationMode::AdaptiveSync ||
           mode == KWin::PresentationMode::AdaptiveAsync;
}

[[gnu::always_inline]]
inline int64_t steadyNowNs() noexcept
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

[[gnu::always_inline, gnu::const]]
inline uint64_t ceilDivU64Reciprocal(uint64_t n, uint64_t d, uint64_t recip, uint8_t shift) noexcept
{
    if (n == 0 || d == 0) [[unlikely]] {
        return 0;
    }
    if (recip == 0 || shift == 0) [[unlikely]] {
        return (n + d - 1) / d;
    }
    const auto prod = static_cast<__uint128_t>(n) * recip;
    auto q = static_cast<uint64_t>(prod >> shift);
    if (q * d < n) {
        ++q;
    }
    return q;
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
{
    cachedVblankIntervalNs = 1'000'000'000'000ULL / 60'000ULL;
    updateReciprocal();
    initializeVrrCapabilities();
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    lastPresentationTimestamp = now.time_since_epoch();
    lastIntervalNs_ = 0;
    cadenceStability_ = 128;
    tripleBufferHysteresisCounter = 0;
}

RenderLoopPrivate::~RenderLoopPrivate()
{
    disconnectVrrSignals();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    const uint64_t interval = cachedVblankIntervalNs;
    if (interval == 0) [[unlikely]] {
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        tripleBufferEnterThresholdNs = 0;
        tripleBufferExitThresholdNs = 0;
        return;
    }

    constexpr uint8_t shift = 63;
    const auto num = static_cast<__uint128_t>(1) << shift;
    vblankIntervalReciprocal64 = static_cast<uint64_t>((num + interval - 1) / interval);
    reciprocalShift64 = shift;

    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
        vrrCapable = false;
        vrrEnabled = false;
        return;
    }

    vrrCapable = output->capabilities().testFlag(Output::Capability::Vrr);

    const auto config = KSharedConfig::openConfig(QStringLiteral("kwinrc"));
    const auto group = config->group(QStringLiteral("VRR"));
    const QString policy = group.readEntry(QStringLiteral("Policy"), QStringLiteral("Automatic"));

    if (policy.compare(QLatin1String("Never"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Never;
        vrrEnabled = false;
    } else if (policy.compare(QLatin1String("Always"), Qt::CaseInsensitive) == 0) {
        vrrMode = VrrMode::Always;
        vrrEnabled = vrrCapable;
    } else {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    }

    presentationMode = PresentationMode::VSync;
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
        if (c && vrrConnectionCount_ < vrrConnections_.size()) {
            vrrConnections_[vrrConnectionCount_++] = c;
        }
    };

    store(QObject::connect(window, &QObject::destroyed, q, [this]() {
        trackedWindow_ = nullptr;
        vrrStateDirty_ = true;
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
    for (uint8_t i = 0; i < vrrConnectionCount_; ++i) {
        QObject::disconnect(vrrConnections_[i]);
        vrrConnections_[i] = {};
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
    const bool canVrr = vrrEnabled && vrrCapable;
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
                            state.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3u;
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
    modeDwellCounter_ = 0;

    modeSwitchHistory_[modeSwitchHistoryHead_] = now;
    modeSwitchHistoryHead_ = static_cast<uint8_t>((modeSwitchHistoryHead_ + 1) % kModeSwitchHistorySize);
    if (modeSwitchHistoryCount_ < kModeSwitchHistorySize) {
        ++modeSwitchHistoryCount_;
    }
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    if (modeSwitchHistoryCount_ < kOscillationThreshold) {
        return false;
    }

    const auto now = std::chrono::steady_clock::now();
    const auto cutoff = now - kOscillationWindow;

    uint8_t recentCount = 0;
    for (uint8_t i = 0; i < modeSwitchHistoryCount_ && recentCount < kOscillationThreshold; ++i) {
        const uint8_t idx = static_cast<uint8_t>(
            (modeSwitchHistoryHead_ + kModeSwitchHistorySize - 1 - i) % kModeSwitchHistorySize);
        if (modeSwitchHistory_[idx] >= cutoff) {
            ++recentCount;
        }
    }

    if (recentCount >= kOscillationThreshold) [[unlikely]] {
        vrrOscillationLockout = true;
        oscillationCooldownCounter_ = kOscillationCooldownFrames;
        return true;
    }
    return false;
}

void RenderLoopPrivate::updatePresentationCadence(int64_t intervalNs) noexcept
{
    if (intervalNs < kMinReasonableIntervalNs || intervalNs > kMaxReasonableIntervalNs) [[unlikely]] {
        cadenceStability_ = 128;
        lastIntervalNs_ = 0;
        return;
    }

    const int64_t prev = lastIntervalNs_;
    lastIntervalNs_ = intervalNs;

    if (prev < kMinReasonableIntervalNs) [[unlikely]] {
        return;
    }

    const int64_t diff = intervalNs > prev ? intervalNs - prev : prev - intervalNs;
    const int64_t relativeDeviation = (diff << 8) / prev;
    const int64_t sample = std::clamp(256 - relativeDeviation, int64_t{0}, int64_t{256});

    const int16_t current = cadenceStability_;
    int16_t next;

    if (sample > current) {
        next = static_cast<int16_t>((current * 7 + sample) >> 3);
    } else {
        next = static_cast<int16_t>((current + sample) >> 1);
    }

    cadenceStability_ = next;
}

bool RenderLoopPrivate::isFrameTimeStable() const noexcept
{
    return cadenceStability_ >= kCadenceStableThreshold;
}

bool RenderLoopPrivate::shouldSwitchMode(PresentationMode target) noexcept
{
    if (target == presentationMode) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = presentationMode;
        return false;
    }

    const bool exitingToVsync = (target == PresentationMode::VSync);

    if (vrrMode == VrrMode::Always && !exitingToVsync) {
        if (modeDwellCounter_ >= kMinModeDwellFrames) {
            pendingModeCounter_ = 0;
            return true;
        }
    }

    if (exitingToVsync && cadenceStability_ >= kCadenceStableThreshold) {
        pendingModeCounter_ = 0;
        return true;
    }

    if (!exitingToVsync && cadenceStability_ <= kCadenceUnstableThreshold) {
        pendingModeCounter_ = 0;
        return true;
    }

    if (modeDwellCounter_ < kMinModeDwellFrames) {
        return false;
    }

    if (oscillationCooldownCounter_ > 0) {
        --oscillationCooldownCounter_;
        if (oscillationCooldownCounter_ == 0) {
            vrrOscillationLockout = false;
        }
        return false;
    }

    if (vrrOscillationLockout) {
        return false;
    }

    if (target == pendingTargetMode_) {
        if (pendingModeCounter_ < std::numeric_limits<uint16_t>::max()) {
            ++pendingModeCounter_;
        }
    } else {
        pendingTargetMode_ = target;
        pendingModeCounter_ = 1;
    }

    const uint16_t threshold = exitingToVsync ? kVrrToVsyncStabilityFrames : kVsyncToVrrStabilityFrames;

    if (pendingModeCounter_ >= threshold) {
        if (detectVrrOscillation()) {
            return false;
        }
        pendingModeCounter_ = 0;
        return true;
    }

    return false;
}

PresentationMode RenderLoopPrivate::selectPresentationMode() noexcept
{
    if (!vrrEnabled || !vrrCapable) [[unlikely]] {
        return PresentationMode::VSync;
    }

    if (vrrMode == VrrMode::Never) {
        return PresentationMode::VSync;
    }

    const auto state = vrrStateCache_.getState();
    const bool fullscreenOnOutput = state.isOnOutput && state.isFullScreen;

    if (!fullscreenOnOutput) {
        return PresentationMode::VSync;
    }

    if (vrrMode == VrrMode::Always) {
        return PresentationMode::AdaptiveSync;
    }

    if (cadenceStability_ >= kCadenceStableThreshold) {
        return PresentationMode::VSync;
    }

    if (!state.valid) {
        return PresentationMode::VSync;
    }

    const auto hint = static_cast<PresentationModeHint>(state.hint);

    if (hint == PresentationModeHint::VSync) {
        return PresentationMode::VSync;
    }

    if (cadenceStability_ > kCadenceUnstableThreshold) {
        if (presentationMode == PresentationMode::VSync) {
            return PresentationMode::VSync;
        }
    }

    if (hint == PresentationModeHint::Async) {
        return PresentationMode::AdaptiveAsync;
    }

    return PresentationMode::AdaptiveSync;
}

void RenderLoopPrivate::updateFramePrediction(std::chrono::nanoseconds measured) noexcept
{
    const int64_t m = measured.count();
    if (m <= 0) [[unlikely]] {
        return;
    }

    const int64_t cur = framePrediction.count();
    if (cur <= 0) [[unlikely]] {
        framePrediction = measured;
        return;
    }

    const int shift = 1 + ((cadenceStability_ * 3) >> 8);

    const int64_t diff = m - cur;
    int64_t updated;
    if (diff > 0) {
        const int attackShift = shift > 2 ? 2 : 1;
        updated = cur + ((diff + 1) >> attackShift);
    } else {
        updated = cur + (diff >> shift);
    }

    const int64_t vblank = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t lo = std::max(vblank >> 5, int64_t{100'000});
    const int64_t hi = vblank << 2;
    framePrediction = std::chrono::nanoseconds{std::clamp(updated, lo, hi)};
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (kwinApp()->isTerminating()) [[unlikely]] {
        return;
    }

    if (preparingNewFrame) {
        pendingReschedule = true;
        return;
    }

    if (compositeTimer.isActive()) {
        return;
    }

    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTarget)
{
    (void)lastTarget;

    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this, lastTarget]() {
            scheduleRepaint(lastTarget);
        }, Qt::QueuedConnection);
        return;
    }

    pendingReschedule = false;

    const uint64_t vblankNs = cachedVblankIntervalNs;
    if (vblankNs == 0) [[unlikely]] {
        return;
    }

    const int64_t nowNs = steadyNowNs();
    int64_t lastPresNs = lastPresentationTimestamp.count();

    if (lastPresNs <= 0 || lastPresNs > nowNs) [[unlikely]] {
        lastPresNs = nowNs - static_cast<int64_t>(vblankNs);
        lastPresentationTimestamp = std::chrono::nanoseconds{lastPresNs};
    }

    int64_t predNs = framePrediction.count();
    if (predNs <= 0) {
        predNs = renderJournal.result().count();
    }
    if (predNs <= 0) {
        predNs = static_cast<int64_t>(vblankNs >> 1);
    }

    const int64_t stabilityBonus = (static_cast<int64_t>(cadenceStability_) * kRenderSlackNs) >> 9;
    const int64_t effectiveSlack = kRenderSlackNs - stabilityBonus;
    const int64_t totalMargin = safetyMargin.count() + effectiveSlack;
    const int64_t compositeNs = std::clamp(predNs + totalMargin, kMinCompositeNs, kMaxCompositeNs);

    const bool canVrr = vrrEnabled && vrrCapable;
    if (canVrr && !vrrStateDirty_) [[likely]] {
        Workspace *ws = workspace();
        Window *active = ws ? ws->activeWindow() : nullptr;
        if (active != trackedWindow_) [[unlikely]] {
            vrrStateDirty_ = true;
        }
    }

    updateVrrState();

    const PresentationMode targetMode = selectPresentationMode();

    if (targetMode != presentationMode && shouldSwitchMode(targetMode)) {
        presentationMode = targetMode;
        recordModeSwitch();
    }

    if (modeDwellCounter_ < std::numeric_limits<uint16_t>::max()) {
        ++modeDwellCounter_;
    }

    int64_t nextPresNs;
    const int64_t vblankI64 = static_cast<int64_t>(vblankNs);

    if (presentationMode == PresentationMode::VSync) {
        const int64_t earliestReadyNs = nowNs + compositeNs;
        const int64_t nsFromLastPres = std::max(earliestReadyNs - lastPresNs, int64_t{1});

        uint64_t targetVblankU = ceilDivU64Reciprocal(
            static_cast<uint64_t>(nsFromLastPres),
            vblankNs,
            vblankIntervalReciprocal64,
            reciprocalShift64);
        int64_t targetVblank = static_cast<int64_t>(targetVblankU);

        if (wasTripleBuffering && targetVblank <= 1) {
            targetVblank = 2;
        }

        nextPresNs = lastPresNs + targetVblank * vblankI64;

        if (compositeNs > tripleBufferEnterThresholdNs) {
            if (tripleBufferHysteresisCounter < kTripleBufferHysteresisLimit) {
                ++tripleBufferHysteresisCounter;
            } else {
                wasTripleBuffering = true;
            }
        } else if (compositeNs < tripleBufferExitThresholdNs) {
            tripleBufferHysteresisCounter = 0;
            wasTripleBuffering = false;
        }
    } else {
        nextPresNs = nowNs + compositeNs;
        wasTripleBuffering = false;
        tripleBufferHysteresisCounter = 0;
    }

    const int64_t minPresNs = nowNs + kMinCompositeNs;
    nextPresNs = std::max(nextPresNs, minPresNs);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    const int64_t nextRenderNs = nextPresNs - compositeNs;
    const int64_t delayNs = std::max(nextRenderNs - nowNs, int64_t{0});

    const int64_t scheduledNs = scheduledRenderTimestamp.count();
    const int64_t diffNs = nextRenderNs - scheduledNs;

    if (compositeTimer.isActive() && safeAbs64(diffNs) < kTimerJitterFilterNs) {
        return;
    }

    int timerMs;
    if (delayNs < kNsPerMs) {
        timerMs = 0;
    } else {
        timerMs = static_cast<int>(std::clamp(delayNs / kNsPerMs, int64_t{1}, int64_t{kMaxTimerDelayMs}));
    }

    if (compositeTimer.isActive() && scheduledTimerMs >= 0) {
        if (timerMs == scheduledTimerMs && safeAbs64(diffNs) < kNsPerMs) {
            return;
        }
    }

    scheduledRenderTimestamp = std::chrono::nanoseconds{nextRenderNs};
    compositeTimer.start(timerMs, Qt::PreciseTimer, q);
    scheduledTimerMs = static_cast<int16_t>(timerMs);
}

void RenderLoopPrivate::delayScheduleRepaint() noexcept
{
    pendingReschedule = true;
}

void RenderLoopPrivate::notifyFrameDropped()
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this]() {
            notifyFrameDropped();
        }, Qt::QueuedConnection);
        return;
    }

    if (pendingFrameCount > 0) {
        --pendingFrameCount;
    } else {
        pendingFrameCount = 0;
    }

    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) {
        ++consecutiveErrorCount;
    }

    if (inhibitCount == 0 && pendingReschedule) {
        if (consecutiveErrorCount > kMaxConsecutiveErrors) {
            int delay = 1 << std::min<int>(consecutiveErrorCount - kMaxConsecutiveErrors, 6);
            delay = std::min(delay, kMaxErrorBackoffMs);
            compositeTimer.start(delay, Qt::PreciseTimer, q);
            scheduledTimerMs = static_cast<int16_t>(delay);
        } else {
            scheduleNextRepaint();
        }
    }
}

namespace
{

void writeDebug(RenderLoopPrivate *d,
                std::optional<RenderTimeSpan> rt,
                std::chrono::nanoseconds targetFlip,
                std::chrono::nanoseconds refreshDur,
                std::chrono::nanoseconds predRender,
                std::chrono::nanoseconds ts,
                PresentationMode mode)
{
    if (!d->m_debugOutput) {
        if (!d->output) {
            return;
        }
        std::string name = d->output->name().toStdString();
        for (char &c : name) {
            const auto uc = static_cast<unsigned char>(c);
            c = static_cast<char>(std::isalnum(uc) ? c : '_');
        }
        d->m_debugOutput.emplace("kwin_perf_" + name + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *d->m_debugOutput << "target,flip,start,end,margin,dur,vrr,pred,cadence\n";
        }
    }

    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const auto times = rt.value_or(RenderTimeSpan{});
        *d->m_debugOutput << targetFlip.count() << ','
                          << ts.count() << ','
                          << times.start.time_since_epoch().count() << ','
                          << times.end.time_since_epoch().count() << ','
                          << d->safetyMargin.count() << ','
                          << refreshDur.count() << ','
                          << isVrrMode(mode) << ','
                          << predRender.count() << ','
                          << d->cadenceStability_ << '\n';
    }
}

}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    std::chrono::nanoseconds targetFlip{0};
    std::chrono::nanoseconds refreshDur{0};
    std::chrono::nanoseconds predRender{0};

    if (s_debugEnabled && frame) [[unlikely]] {
        targetFlip = frame->targetPageflipTime().time_since_epoch();
        refreshDur = frame->refreshDuration();
        predRender = frame->predictedRenderTime();
    }

    auto processCompletion = [this, timestamp, renderTime, mode, targetFlip, refreshDur, predRender]() {
        consecutiveErrorCount = 0;

        if (s_debugEnabled) [[unlikely]] {
            writeDebug(this, renderTime, targetFlip, refreshDur, predRender, timestamp, mode);
        }

        if (pendingFrameCount > 0) {
            --pendingFrameCount;
        } else {
            pendingFrameCount = 0;
        }

        const int64_t prevPresentationNs = lastPresentationTimestamp.count();
        const int64_t nowNs = steadyNowNs();
        notifyVblank(timestamp, nowNs);
        const int64_t intervalNs = lastPresentationTimestamp.count() - prevPresentationNs;
        updatePresentationCadence(intervalNs);

        if (renderTime) {
            const auto dur = renderTime->end - renderTime->start;
            renderJournal.add(dur, timestamp);
            updateFramePrediction(dur);
        }

        if (isVrrMode(mode)) {
            if (starvationRecoveryCounter < kStarvationRecoveryFrames) {
                ++starvationRecoveryCounter;
            }
        } else {
            starvationRecoveryCounter = 0;
        }

        if (inhibitCount == 0) {
            if (pendingReschedule || pendingFrameCount == 0) {
                pendingReschedule = false;
                scheduleNextRepaint();
            }
        }

        Q_EMIT q->framePresented(q, timestamp, mode);
    };

    if (q->thread() == QThread::currentThread()) [[likely]] {
        processCompletion();
    } else {
        QMetaObject::invokeMethod(q, std::move(processCompletion), Qt::QueuedConnection);
    }
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs)
{
    int64_t ts = timestamp.count();

    if (ts > nowNs) [[unlikely]] {
        ts = nowNs;
    }
    if (ts < lastPresentationTimestamp.count()) [[unlikely]] {
        ts = lastPresentationTimestamp.count();
    }

    lastPresentationTimestamp = std::chrono::nanoseconds{ts};
}

void RenderLoopPrivate::dispatch()
{
    Q_EMIT q->frameRequested(q);
}

void RenderLoop::timerEvent(QTimerEvent *event)
{
    const int id = event->timerId();
    if (id == d->compositeTimer.timerId()) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (id == d->delayedVrrTimer.timerId()) [[unlikely]] {
        d->delayedVrrTimer.stop();
        scheduleRepaint(nullptr, nullptr);
    } else {
        QObject::timerEvent(event);
    }
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
    d->preparingNewFrame = true;
}

void RenderLoop::newFramePrepared()
{
    d->preparingNewFrame = false;
}

int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int rate)
{
    rate = std::clamp(rate, 1'000, 1'000'000);
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
    d->safetyMargin = margin.count() > 0 ? margin : std::chrono::nanoseconds{0};
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *layer)
{
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, item, layer]() {
            scheduleRepaint(item, layer);
        }, Qt::QueuedConnection);
        return;
    }

    const bool vrrActive = isVrrMode(d->presentationMode);

    if (vrrActive && item && d->pendingFrameCount > 0) {
        Workspace *ws = workspace();
        Window *active = ws ? ws->activeWindow() : nullptr;
        if (active && d->output && active->isOnOutput(d->output)) {
            SurfaceItem *surface = active->surfaceItem();
            if (surface && (item != surface) && !surface->isAncestorOf(item)) {
                if (surface->recursiveFrameTimeEstimation() <= kVrrControlThreshold) {
                    const uint64_t vblankNs = d->cachedVblankIntervalNs;
                    const auto delayMsU = std::clamp(
                        vblankNs / static_cast<uint64_t>(kNsPerMs),
                        uint64_t{1},
                        static_cast<uint64_t>(kMaxTimerDelayMs));
                    const int delayMs = static_cast<int>(delayMsU);
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
        d->pendingReschedule = true;
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
    d->maxPendingFrameCount = static_cast<int>(std::clamp(count, 1u, 3u));
}

std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

}

#include "moc_renderloop.cpp"
