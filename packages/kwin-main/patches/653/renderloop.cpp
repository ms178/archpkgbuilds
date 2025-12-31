/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

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
#include <type_traits>

using namespace std::chrono_literals;

namespace
{

constexpr int64_t kNsPerMs = 1'000'000;
constexpr int64_t kNsPerSecond = 1'000'000'000;
constexpr int64_t kPredictionSlackNs = 200'000;
constexpr int64_t kContextSwitchOverheadNs = 1'000'000;
constexpr uint64_t kPageflipDriftThreshold = 32;

constexpr uint16_t kVrrToVsyncStabilityFrames = 24;
constexpr uint16_t kVsyncToVrrStabilityFrames = 12;
constexpr uint16_t kMinModeDwellFrames = 60;
constexpr uint16_t kOscillationCooldownFrames = 300;
constexpr auto kVrrControlThreshold = 25ms;
constexpr auto kOscillationWindow = 2000ms;
constexpr uint8_t kOscillationThreshold = 4;

constexpr int kMaxTimerDelayMs = 16000;
constexpr int kTripleBufferEnterPct = 78;
constexpr int kTripleBufferExitPct = 60;

constexpr int kMinVrrRefreshMhz = 48'000;
constexpr int kMaxVrrRefreshMhz = 165'000;

constexpr uint16_t kStarvationRecoveryFrames = 16;
constexpr uint8_t kMaxConsecutiveErrors = 2;
constexpr int kMaxErrorBackoffMs = 250;

constexpr int64_t kTimerJitterFilterNs = 150'000;
constexpr int64_t kFrameTimeVarianceThreshold = 500'000;
constexpr int64_t kMinAbsoluteLeadNs = 1'500'000;

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
    return branchlessMin(branchlessMax(v, lo), hi);
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessMaxU(uint64_t a, uint64_t b) noexcept
{
    return a ^ ((a ^ b) & -static_cast<uint64_t>(a < b));
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessMinU(uint64_t a, uint64_t b) noexcept
{
    return b ^ ((a ^ b) & -static_cast<uint64_t>(a < b));
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t branchlessClampU(uint64_t v, uint64_t lo, uint64_t hi) noexcept
{
    return branchlessMinU(branchlessMaxU(v, lo), hi);
}

[[gnu::always_inline, gnu::hot, gnu::const]]
inline uint64_t fastDiv64(uint64_t n, uint64_t recip, uint8_t shift) noexcept
{
#if defined(__BMI2__) && defined(__x86_64__)
    uint64_t hi;
    uint64_t lo;
    __asm__ __volatile__("mulxq %3, %0, %1"
                         : "=r"(lo), "=r"(hi)
                         : "d"(n), "r"(recip)
                         :);
    if (__builtin_constant_p(shift)) {
        if (shift == 63) {
            return (hi << 1) | (lo >> 63);
        }
        if (shift == 0) {
            return lo;
        }
        if (shift >= 64) {
            return hi >> (shift - 64);
        }
        return (hi << (64 - shift)) | (lo >> shift);
    }
    const unsigned shiftMod = shift & 63u;
    const uint64_t lowResult = (hi << (64u - shiftMod)) | (lo >> shiftMod);
    const uint64_t highResult = hi >> shiftMod;
    return (shift >= 64) ? highResult : lowResult;
#else
    return static_cast<uint64_t>((static_cast<__uint128_t>(n) * recip) >> shift);
#endif
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isVrrMode(KWin::PresentationMode mode) noexcept
{
    return mode == KWin::PresentationMode::AdaptiveSync ||
           mode == KWin::PresentationMode::AdaptiveAsync;
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
    , consecutiveErrorCount(0)
    , vrrConnectionCount_(0)
    , vrrMode(VrrMode::Automatic)
    , presentationMode(PresentationMode::VSync)
    , lastStableMode(PresentationMode::VSync)
    , pendingTargetMode_(PresentationMode::VSync)
    , tripleBufferEnterThresholdNs(0)
    , tripleBufferExitThresholdNs(0)
    , vrrMinIntervalNs(0)
    , vrrMaxIntervalNs(0)
    , stableModeFrameCount(0)
    , starvationRecoveryCounter(0)
    , modeDwellCounter_(0)
    , pendingModeCounter_(0)
    , oscillationCooldownCounter_(0)
    , frameTimeVariance_(0)
    , frameTimeMean_(0)
    , modeSwitchHistoryHead_(0)
    , modeSwitchHistoryCount_(0)
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
    if (interval == 0) [[unlikely]] {
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        tripleBufferEnterThresholdNs = 0;
        tripleBufferExitThresholdNs = 0;
        vrrMinIntervalNs = 0;
        vrrMaxIntervalNs = 0;
        return;
    }

    constexpr uint8_t shift = 63;
    const auto num = static_cast<__uint128_t>(1) << shift;
    vblankIntervalReciprocal64 = static_cast<uint64_t>((num + interval - 1) / interval);
    reciprocalShift64 = shift;

    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);

    vrrMinIntervalNs = 1'000'000'000'000ULL / static_cast<uint64_t>(kMaxVrrRefreshMhz);
    vrrMaxIntervalNs = 1'000'000'000'000ULL / static_cast<uint64_t>(kMinVrrRefreshMhz);
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
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
    modeSwitchHistoryHead_ = (modeSwitchHistoryHead_ + 1) % kModeSwitchHistorySize;
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
    for (uint8_t i = 0; i < modeSwitchHistoryCount_; ++i) {
        const uint8_t idx = (modeSwitchHistoryHead_ + kModeSwitchHistorySize - 1 - i) % kModeSwitchHistorySize;
        if (modeSwitchHistory_[idx] >= cutoff) {
            ++recentCount;
        }
    }

    if (recentCount >= kOscillationThreshold) [[unlikely]] {
        vrrOscillationLockout = 1;
        oscillationCooldownCounter_ = kOscillationCooldownFrames;
        lastStableMode = PresentationMode::VSync;
        presentationMode = PresentationMode::VSync;
        pendingTargetMode_ = PresentationMode::VSync;
        pendingModeCounter_ = 0;
        return true;
    }
    return false;
}

void RenderLoopPrivate::updateFrameTimeStats(int64_t frameTimeNs) noexcept
{
    constexpr int64_t kAlpha = 32;
    constexpr int64_t kAlphaComplement = 256 - kAlpha;

    const int64_t delta = frameTimeNs - frameTimeMean_;
    frameTimeMean_ = (frameTimeMean_ * kAlphaComplement + frameTimeNs * kAlpha) >> 8;

    const int64_t absDelta = branchlessAbs(delta);
    frameTimeVariance_ = (frameTimeVariance_ * kAlphaComplement + absDelta * kAlpha) >> 8;
}

bool RenderLoopPrivate::isFrameTimeStable() const noexcept
{
    return frameTimeVariance_ < kFrameTimeVarianceThreshold;
}

bool RenderLoopPrivate::shouldSwitchMode(PresentationMode target) noexcept
{
    if (target == presentationMode) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = presentationMode;
        return false;
    }

    if (modeDwellCounter_ < kMinModeDwellFrames) {
        return false;
    }

    if (oscillationCooldownCounter_ > 0) {
        --oscillationCooldownCounter_;
        if (oscillationCooldownCounter_ == 0) {
            vrrOscillationLockout = 0;
        }
        return false;
    }

    if (vrrOscillationLockout) {
        return false;
    }

    if (target == pendingTargetMode_) {
        ++pendingModeCounter_;
    } else {
        pendingTargetMode_ = target;
        pendingModeCounter_ = 1;
    }

    const bool toVsync = (target == PresentationMode::VSync);
    const uint16_t threshold = toVsync ? kVrrToVsyncStabilityFrames : kVsyncToVrrStabilityFrames;

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
    const bool canVrr = static_cast<bool>(vrrEnabled) && static_cast<bool>(vrrCapable);
    if (!canVrr) [[unlikely]] {
        return PresentationMode::VSync;
    }

    const auto state = vrrStateCache_.getState();
    if (!state.valid) [[unlikely]] {
        return PresentationMode::VSync;
    }

    const auto hint = static_cast<PresentationModeHint>(state.hint);

    if (vrrMode == VrrMode::Always) {
        if (hint == PresentationModeHint::Async) {
            return PresentationMode::AdaptiveAsync;
        }
        if (hint != PresentationModeHint::VSync) {
            return PresentationMode::AdaptiveSync;
        }
        return PresentationMode::VSync;
    }

    if (!state.isOnOutput || !state.isFullScreen) [[unlikely]] {
        return PresentationMode::VSync;
    }

    if (hint == PresentationModeHint::VSync) {
        return PresentationMode::VSync;
    }

    if (isFrameTimeStable() && presentationMode == PresentationMode::VSync) {
        return PresentationMode::VSync;
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
    const int64_t updated = (cur * 224 + m * 32) >> 8;
    const int64_t vblank = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t lo = vblank >> 3;
    const int64_t hi = vblank * 3;
    framePrediction = std::chrono::nanoseconds{branchlessClamp(updated, lo, hi)};
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    const bool blocked = kwinApp()->isTerminating() || compositeTimer.isActive() || preparingNewFrame;
    if (blocked) [[unlikely]] {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTarget)
{
    if (Q_UNLIKELY(q->thread() != QThread::currentThread())) {
        QMetaObject::invokeMethod(q, [this, lastTarget]() {
            scheduleRepaint(lastTarget);
        }, Qt::QueuedConnection);
        return;
    }

    pendingReschedule = 0;
    const uint64_t vblankNs = cachedVblankIntervalNs;
    if (Q_UNLIKELY(vblankNs == 0)) {
        return;
    }

    const int64_t nowNs = std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();

    int64_t lastPresNs = lastPresentationTimestamp.count();
    if (Q_UNLIKELY(lastPresNs >= nowNs)) {
        lastPresNs = nowNs - 1000;
        lastPresentationTimestamp = std::chrono::nanoseconds{lastPresNs};
    }

    const int64_t predNs = framePrediction.count() > 0 ? framePrediction.count() : renderJournal.result().count();
    const int64_t compositeNs = branchlessClamp(
        predNs + safetyMargin.count() + kPredictionSlackNs + kContextSwitchOverheadNs,
        0,
        static_cast<int64_t>(vblankNs) * 3);

    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();

    if (shouldSwitchMode(targetMode)) {
        presentationMode = targetMode;
        recordModeSwitch();
    }

    ++modeDwellCounter_;

    int64_t nextPresNs;
    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = nowNs - lastPresNs;
        uint64_t flipsSince = 0;
        if (sinceLastNs > 0) {
            flipsSince = (vblankIntervalReciprocal64 != 0)
                ? fastDiv64(static_cast<uint64_t>(sinceLastNs), vblankIntervalReciprocal64, reciprocalShift64)
                : (static_cast<uint64_t>(sinceLastNs) / vblankNs);
        }

        uint64_t flipsAhead = (static_cast<uint64_t>(compositeNs) / vblankNs) + 1;
        if (wasTripleBuffering) {
            flipsAhead = branchlessMaxU(flipsAhead, 2);
        }

        nextPresNs = lastPresNs + static_cast<int64_t>((flipsSince + flipsAhead) * vblankNs);
    } else {
        nextPresNs = nowNs + compositeNs;
    }

    const int64_t minAbsolutePresNs = nowNs + kMinAbsoluteLeadNs;
    nextPresNs = branchlessMax(nextPresNs, minAbsolutePresNs);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    const int64_t nextRenderNs = nextPresNs - compositeNs;
    const int64_t delayNs = branchlessMax(nextRenderNs - nowNs, 0);

    if (compositeTimer.isActive()) {
        if (branchlessAbs(scheduledRenderTimestamp.count() - nextRenderNs) < kTimerJitterFilterNs) {
            return;
        }
    }

    scheduledRenderTimestamp = std::chrono::nanoseconds{nextRenderNs};

    const int64_t delayMs64 = delayNs / kNsPerMs;
    const int64_t clampedMs64 = branchlessClamp(delayMs64, int64_t{0}, int64_t{kMaxTimerDelayMs});
    const int clampedMs = static_cast<int>(clampedMs64);

    compositeTimer.start(clampedMs, Qt::PreciseTimer, q);
    scheduledTimerMs = clampedMs;
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
            const int minDelay = static_cast<int>(cachedVblankIntervalNs / static_cast<uint64_t>(kNsPerMs));
            delay = static_cast<int>(branchlessMax(static_cast<int64_t>(delay), static_cast<int64_t>(minDelay)));
            compositeTimer.start(delay, Qt::PreciseTimer, q);
            scheduledTimerMs = delay;
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
            const auto uc = static_cast<unsigned char>(c);
            c = std::isalnum(uc) ? c : '_';
        }
        d->m_debugOutput = std::fstream("kwin_perf_" + name + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *d->m_debugOutput << "target,flip,start,end,margin,dur,vrr,pred,var\n";
        }
    }

    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const auto times = rt.value_or(RenderTimeSpan{});
        const bool vrr = isVrrMode(mode);
        *d->m_debugOutput << frame->targetPageflipTime().time_since_epoch().count() << ','
                          << ts.count() << ','
                          << times.start.time_since_epoch().count() << ','
                          << times.end.time_since_epoch().count() << ','
                          << d->safetyMargin.count() << ','
                          << frame->refreshDuration().count() << ','
                          << vrr << ','
                          << frame->predictedRenderTime().count() << ','
                          << d->frameTimeVariance_ << '\n';
    }
}

}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame)
{
    if (Q_UNLIKELY(q->thread() != QThread::currentThread())) {
        QMetaObject::invokeMethod(q, [this, timestamp, renderTime, mode, frame]() {
            notifyFrameCompleted(timestamp, renderTime, mode, frame);
        }, Qt::QueuedConnection);
        return;
    }

    consecutiveErrorCount = 0;
    if (s_debugEnabled) [[unlikely]] {
        writeDebug(this, renderTime, frame, timestamp, mode);
    }

    Q_ASSERT(pendingFrameCount > 0);
    --pendingFrameCount;
    notifyVblank(timestamp);

    if (renderTime) {
        const auto dur = renderTime->end - renderTime->start;
        renderJournal.add(dur, timestamp);
        updateFramePrediction(dur);
        updateFrameTimeStats(dur.count());
    }

    if (isVrrMode(mode)) {
        if (starvationRecoveryCounter < kStarvationRecoveryFrames) {
            ++starvationRecoveryCounter;
        }
    } else {
        starvationRecoveryCounter = 0;
    }

    if (compositeTimer.isActive()) {
        scheduleRepaint(lastPresentationTimestamp);
    }
    if (inhibitCount == 0 && pendingReschedule) {
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp)
{
    const auto now = std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::steady_clock::now().time_since_epoch());

    auto ts = timestamp;

    if (ts > now) [[unlikely]] {
        ts = now;
    }
    if (ts < lastPresentationTimestamp) [[unlikely]] {
        ts = lastPresentationTimestamp;
    }

    lastPresentationTimestamp = ts;
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
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, item, layer]() {
            scheduleRepaint(item, layer);
        }, Qt::QueuedConnection);
        return;
    }

    d->vrrStateDirty_ = true;

    const bool vrrActive = isVrrMode(d->presentationMode);
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
