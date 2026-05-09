/*
KWin - the KDE window manager
SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
SPDX-FileCopyrightText: 2025 Performance Engineering Team
SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "renderloop.h"
#include "backendoutput.h"
#include "options.h"
#include "renderloop_p.h"
#include "scene/surfaceitem.h"
#include "scene/surfaceitem_wayland.h"
#include "utils/common.h"
#include "wayland/surface.h"
#include "window.h"
#include "workspace.h"
#include "output.h"

#include <KConfigGroup>
#include <KSharedConfig>

#include <QThread>
#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <limits>
#include <optional>

class LogicalOutput;

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
constexpr uint16_t kInteractiveGraceFrames = 90;
constexpr uint16_t kInteractiveTailFrames = 45;

constexpr auto kVrrControlThreshold = 25ms;
constexpr auto kOscillationWindow = 500ms;
constexpr uint8_t kOscillationThreshold = 10;
constexpr int kMaxTimerDelayMs = 16000;

constexpr int kTripleBufferEnterPct = 90;
constexpr int kTripleBufferExitPct = 70;
constexpr uint8_t kTripleBufferHysteresisLimit = 5;

constexpr uint8_t kMaxConsecutiveErrors = 3;
constexpr int kMaxErrorBackoffMs = 50;
constexpr int64_t kTimerJitterFilterNs = 250'000;
constexpr int64_t kCadenceNoiseFloorNs = 125'000;
constexpr int64_t kFilm24Ns = 41'666'666;
constexpr int64_t kFilm25Ns = 40'000'000;

constexpr int64_t kStalledFrameMinTimeoutNs = 12'000'000;
constexpr int64_t kStalledFrameMaxTimeoutNs = 120'000'000;
constexpr int64_t kStalledFrameExtraTimeoutNs = 2'000'000;
constexpr uint8_t kStalledFrameRecoveryThreshold = 2;
constexpr int64_t kStalledFrameVblankMultiplier = 3;

[[nodiscard]]
inline KWin::SurfaceInterface *resolvePresentationSurface(KWin::Window* window) noexcept
{
    if (window == nullptr) {
        return nullptr;
    }
    KWin::SurfaceItem *const surf = window->surfaceItem();
    if (surf == nullptr) {
        return nullptr;
    }
    auto *const wayland = qobject_cast<KWin::SurfaceItemWayland* >(surf);
    return wayland ? wayland->surface() : nullptr;
}

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t fastAbs64(int64_t v) noexcept
{
    const int64_t mask = v >> 63;
    return (v ^ mask) - mask;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isVrrMode(KWin::PresentationMode mode) noexcept
{
    return mode == KWin::PresentationMode::AdaptiveSync || mode == KWin::PresentationMode::AdaptiveAsync;
}

[[gnu::always_inline]]
inline int64_t steadyNowNs() noexcept
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t ceilDivU64Reciprocal(uint64_t n, uint64_t d, uint64_t recip, uint8_t shift) noexcept
{
    if (recip == 0 || shift == 0) [[unlikely]] {
        return d == 0 ? 0 : (n + d - 1) / d;
    }
    const auto prod = static_cast<__uint128_t>(n) * recip;
    const auto q = static_cast<uint64_t>(prod >> shift);
    return q + static_cast<uint64_t>(q * d < n);
}

[[nodiscard]] consteval uint64_t reciprocalForInterval(uint64_t interval) noexcept
{
    constexpr uint8_t shift = 63;
    return ((1ULL << shift) + interval - 1) / interval;
}

constexpr uint64_t kRecip60Hz = reciprocalForInterval(16'666'666ULL);
constexpr uint64_t kRecip120Hz = reciprocalForInterval(8'333'333ULL);
constexpr uint64_t kRecip144Hz = reciprocalForInterval(6'944'444ULL);
constexpr uint64_t kRecip165Hz = reciprocalForInterval(6'060'606ULL);
constexpr uint64_t kRecip240Hz = reciprocalForInterval(4'166'666ULL);

template <typename Estimate>
[[gnu::always_inline]]
inline bool durationEstimateAtMost(const Estimate &estimate, std::chrono::nanoseconds limit) noexcept
{
    return static_cast<bool>(estimate) && (*estimate <= limit);
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool nearIntervalNs(int64_t value, int64_t target, int64_t minToleranceNs) noexcept
{
    const int64_t tolerance = std::max<int64_t>(target / 64, minToleranceNs);
    return fastAbs64(value - target) <= tolerance;
}

template <int64_t Target, int64_t MinToleranceNs>
[[gnu::always_inline, gnu::const]]
inline constexpr bool nearIntervalNs(int64_t value) noexcept
{
    constexpr int64_t tolerance = (Target / 64) > MinToleranceNs ? (Target / 64) : MinToleranceNs;
    return fastAbs64(value - Target) <= tolerance;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isExplicitVsyncHint(uint8_t hintRaw, bool valid) noexcept
{
    return valid && static_cast<KWin::PresentationModeHint>(hintRaw) == KWin::PresentationModeHint::VSync;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isExplicitAsyncHint(uint8_t hintRaw, bool valid) noexcept
{
    return valid && static_cast<KWin::PresentationModeHint>(hintRaw) == KWin::PresentationModeHint::Async;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isUnhintedFilmCadenceNs(int64_t intervalNs, int16_t stability) noexcept
{
    if (intervalNs <= 0 || stability < kCadenceStableThreshold) {
        return false;
    }
    return nearIntervalNs<kFilm24Ns, 350'000>(intervalNs) || nearIntervalNs<kFilm25Ns, 300'000>(intervalNs);
}

[[gnu::always_inline, gnu::const]]
inline KWin::PresentationMode adaptiveModeForState(const KWin::VrrStateCache::State &state) noexcept
{
    const bool validHint = state.valid != 0U;
    return isExplicitAsyncHint(state.hint, validHint)
        ? KWin::PresentationMode::AdaptiveAsync
        : KWin::PresentationMode::AdaptiveSync;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool contentIsInteractive(KWin::VrrContentHint hint) noexcept
{
    return hint == KWin::VrrContentHint::Interactive || hint == KWin::VrrContentHint::ForceVrr;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool contentIsVideoLike(KWin::VrrContentHint hint) noexcept
{
    return hint == KWin::VrrContentHint::Video || hint == KWin::VrrContentHint::Film || hint == KWin::VrrContentHint::ForceVsync;
}

[[gnu::always_inline]]
inline int effectiveMaxPendingFrames(const KWin::RenderLoopPrivate *d, bool vrrActive) noexcept
{
    const int configuredMax = d->maxPendingFrameCount;
    if (!vrrActive) [[likely]] {
        return configuredMax;
    }

    if (configuredMax <= 1) {
        return configuredMax;
    }

    switch (d->activeContentHint_) {
    case KWin::VrrContentHint::ForceVsync:
    case KWin::VrrContentHint::Video:
    case KWin::VrrContentHint::Film:
        return 1;
    case KWin::VrrContentHint::ForceVrr:
    case KWin::VrrContentHint::Interactive:
        return std::min(configuredMax, 2);
    case KWin::VrrContentHint::Unknown:
        break;
    }

    const int maxPending = std::min(configuredMax, 2);
    if (d->interactiveGraceFrames_ > 0U ||
        d->pendingReschedule ||
        d->starvationRecoveryCounter >= KWin::RenderLoopPrivate::kStarvationRecoveryFrames ||
        d->consecutiveErrorCount != 0U ||
        d->cadenceStability_ < kCadenceStableThreshold) {
        return maxPending;
    }

    const int64_t vblankNs = static_cast<int64_t>(d->cachedVblankIntervalNs);
    int64_t predictedNs = d->framePrediction.count();
    if (predictedNs <= 0) {
        predictedNs = d->renderJournal.result().count();
        if (predictedNs <= 0) {
            predictedNs = vblankNs >> 1;
        }
    }

    return (predictedNs * 5) >= (vblankNs * 2) ? maxPending : 1;
}

} // namespace

namespace KWin
{

static const bool s_debugEnabled = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop* loop) noexcept
{
    return loop ? loop->d.get() : nullptr;
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, BackendOutput* output)
    : q(q)
    , output(output)
{
    cachedVblankIntervalNs = 1'000'000'000'000ULL / 60'000ULL;
    vrrCaps_.maxRefreshRate = refreshRate;
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
        vrrControlDelayMs = 0;
        return;
    }

    constexpr uint8_t shift = 63;
    reciprocalShift64 = shift;

    switch (interval) {
    case 16'666'666ULL:
        vblankIntervalReciprocal64 = kRecip60Hz;
        break;
    case 8'333'333ULL:
        vblankIntervalReciprocal64 = kRecip120Hz;
        break;
    case 6'944'444ULL:
        vblankIntervalReciprocal64 = kRecip144Hz;
        break;
    case 6'060'606ULL:
        vblankIntervalReciprocal64 = kRecip165Hz;
        break;
    case 4'166'666ULL:
        vblankIntervalReciprocal64 = kRecip240Hz;
        break;
    default: {
        const uint64_t num = 1ULL << shift;
        vblankIntervalReciprocal64 = (num + interval - 1) / interval;
        break;
    }
    }

    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);
    vrrControlDelayMs = static_cast<int16_t>(std::clamp(
        interval / static_cast<uint64_t>(kNsPerMs),
        uint64_t{1},
        static_cast<uint64_t>(kMaxTimerDelayMs)));
}

void RenderLoopPrivate::armStalledFrameTimer() noexcept
{
    if (kwinApp()->isTerminating() || inhibitCount != 0) {
        return;
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    if (!preparingNewFrame && pendingFrameCount < maxPending) {
        return;
    }

    int64_t predictedNs = framePrediction.count();
    if (predictedNs <= 0) {
        predictedNs = renderJournal.result().count();
        if (predictedNs <= 0) {
            predictedNs = std::max<int64_t>(static_cast<int64_t>(cachedVblankIntervalNs >> 1), kMinCompositeNs);
        }
    }

    const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t timeoutNs = std::clamp(
        std::max<int64_t>(vblankNs * kStalledFrameVblankMultiplier,
                          predictedNs + safetyMargin.count() + kStalledFrameExtraTimeoutNs),
        kStalledFrameMinTimeoutNs,
        kStalledFrameMaxTimeoutNs);

    const int timerMs = static_cast<int>(std::clamp(
        (timeoutNs + kNsPerMs - 1) / kNsPerMs,
        int64_t{1},
        int64_t{kMaxTimerDelayMs}));

    if (stalledFrameTimer.isActive() &&
        stalledFrameTimerMs_ >= 0 &&
        stalledFrameTimerMs_ <= timerMs) {
        return;
    }

    stalledFrameTimer.start(timerMs, Qt::PreciseTimer, q);
    stalledFrameTimerMs_ = static_cast<int16_t>(timerMs);
}

void RenderLoopPrivate::disarmStalledFrameTimer() noexcept
{
    stalledFrameTimer.stop();
    stalledFrameTimerMs_ = -1;
    stalledFrameTimeoutCount_ = 0;
}

void RenderLoopPrivate::handleStalledFrameTimeout()
{
    stalledFrameTimer.stop();
    stalledFrameTimerMs_ = -1;

    if (kwinApp()->isTerminating() || inhibitCount != 0) {
        stalledFrameTimeoutCount_ = 0;
        return;
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    const bool blocked = preparingNewFrame || pendingFrameCount >= maxPending;

    if (!blocked || (!pendingReschedule && !preparingNewFrame)) {
        stalledFrameTimeoutCount_ = 0;
        return;
    }

    if ((stalledFrameTimeoutCount_ + 1U) < kStalledFrameRecoveryThreshold) {
        ++stalledFrameTimeoutCount_;
        armStalledFrameTimer();
        return;
    }

    stalledFrameTimeoutCount_ = 0;
    delayedVrrTimer.stop();

    if (preparingNewFrame) {
        preparingNewFrame = false;
    }
    if (pendingFrameCount > 0) {
        --pendingFrameCount;
    }
    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) {
        ++consecutiveErrorCount;
    }

    pendingReschedule = true;
    scheduleNextRepaint();
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
        vrrCapable = false;
        vrrEnabled = false;
        return;
    }

    vrrCapable = output->capabilities().testFlag(BackendOutput::Capability::Vrr);
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
    if (!window || !vrrEnabled) {
        return;
    }

    SurfaceInterface *const surface = resolvePresentationSurface(window);
    if (trackedWindow_ == window && trackedSurface_ == surface) {
        return;
    }

    disconnectVrrSignals();
    trackedWindow_ = window;
    trackedSurfaceItem_ = window->surfaceItem();
    trackedSurface_ = surface;
    vrrConnectionCount_ = 0;

    const uint8_t capacity = static_cast<uint8_t>(vrrConnections_.size());
    auto store = [this, capacity](QMetaObject::Connection c) {
        if (c && vrrConnectionCount_ < capacity) {
            vrrConnections_[vrrConnectionCount_++] = c;
        }
    };

    store(QObject::connect(window, &QObject::destroyed, q, [this]() {
        vrrStateDirty_ = true;
    }));
    store(QObject::connect(window, &Window::fullScreenChanged, q, [this]() {
        vrrStateDirty_ = true;
    }));
    store(QObject::connect(window, &Window::outputChanged, q, [this]() {
        vrrStateDirty_ = true;
    }));

    if (surface != nullptr) {
        store(QObject::connect(surface,
                               &SurfaceInterface::presentationModeHintChanged,
                               q,
                               [this]() {
                                   vrrStateDirty_ = true;
                               }));
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
    trackedSurfaceItem_ = nullptr;
    trackedSurface_ = nullptr;
}

void RenderLoopPrivate::invalidateVrrState() noexcept
{
    vrrStateDirty_ = true;
}

void RenderLoopPrivate::updateVrrState() noexcept
{
    if (!vrrEnabled || !vrrCapable || output == nullptr) {
        if (trackedWindow_ != nullptr) {
            disconnectVrrSignals();
        }
        if (vrrStateCache_.raw() != 0U) {
            vrrStateCache_.setRaw(0U);
        }
        vrrStateDirty_ = false;
        return;
    }

    if (!vrrStateDirty_) {
        if (trackedWindow_ != nullptr) {
            if (trackedWindow_->surfaceItem() != trackedSurfaceItem_.data()) {
                vrrStateDirty_ = true;
            }
        }
        if (!vrrStateDirty_) {
            return;
        }
    }

    vrrStateDirty_ = false;
    VrrStateCache::State state{};
    const uint8_t oldRaw = vrrStateCache_.raw();

    Window *active = nullptr;
    bool eligible = false;

    if (Workspace *const ws = workspace()) {
        active = ws->activeWindow();
        if (active != nullptr && active->isFullScreen()) {
            if (LogicalOutput *const logical = ws->findOutput(output)) {
                eligible = active->isOnOutput(logical);
            }
        }
    }

    if (!eligible) {
        if (trackedWindow_ != nullptr) {
            disconnectVrrSignals();
        }
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    connectVrrSignals(active);
    state.isOnOutput = 1U;
    state.isFullScreen = 1U;

    if (SurfaceInterface *const surface = trackedSurface_.data()) {
        state.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3U;
        state.valid = 1U;
    }

    if (oldRaw != state.toRaw()) {
        vrrStateCache_.setState(state);
    }
}

void RenderLoopPrivate::setVrrCapabilities(const VrrCapabilities &caps) noexcept
{
    vrrCaps_ = caps;
    if (vrrCaps_.minRefreshRate < 0) {
        vrrCaps_.minRefreshRate = 0;
    }
    if (vrrCaps_.maxRefreshRate < 0) {
        vrrCaps_.maxRefreshRate = 0;
    }
    if (vrrCaps_.maxRefreshRate > 0 && vrrCaps_.minRefreshRate > vrrCaps_.maxRefreshRate) {
        std::swap(vrrCaps_.minRefreshRate, vrrCaps_.maxRefreshRate);
    }
    lfcCapable_ = vrrCaps_.supportsLfc;
}

void RenderLoopPrivate::setActiveContentHint(VrrContentHint hint,
                                             std::optional<std::chrono::nanoseconds> nominalFrameInterval) noexcept
{
    const VrrContentHint oldHint = activeContentHint_;
    activeContentHint_ = hint;

    if (nominalFrameInterval && nominalFrameInterval->count() > 0) {
        nominalContentFrameInterval = std::chrono::nanoseconds{
            std::clamp(nominalFrameInterval->count(), kMinReasonableIntervalNs, kMaxReasonableIntervalNs)};
    } else {
        nominalContentFrameInterval = std::chrono::nanoseconds{0};
    }

    if (hint == VrrContentHint::Interactive || hint == VrrContentHint::ForceVrr) {
        interactiveGraceFrames_ = std::max<uint16_t>(interactiveGraceFrames_, kInteractiveGraceFrames);
    } else if (hint == VrrContentHint::Unknown && contentIsInteractive(oldHint)) {
        interactiveGraceFrames_ = std::max<uint16_t>(interactiveGraceFrames_, kInteractiveTailFrames);
    } else if (contentIsVideoLike(hint)) {
        interactiveGraceFrames_ = 0;
        delayedVrrTimer.stop();
    }

    if (inhibitCount == 0) {
        scheduleNextRepaint();
    }
}

void RenderLoopPrivate::addPresentFeedback(const PresentFeedback &feedback) noexcept
{
    if (!feedback.valid) {
        return;
    }

    std::chrono::nanoseconds ts = feedback.actualPresentationTimestamp;
    if (ts.count() <= 0) {
        ts = feedback.targetPresentationTimestamp;
    }
    if (ts.count() <= 0) {
        return;
    }

    PresentSample sample{};
    sample.timestamp = ts;
    sample.refresh = static_cast<uint32_t>(std::clamp<int64_t>(
        feedback.refreshDuration.count() > 0 ? feedback.refreshDuration.count() : static_cast<int64_t>(cachedVblankIntervalNs),
        0LL, 4'000'000'000LL));
    sample.deadlineMissed = feedback.deadlineMissed;
    sample.directScanout = feedback.directScanout;
    sample.valid = true;

    presentHistory_[presentHistoryHead_] = sample;
    presentHistoryHead_ = static_cast<uint8_t>((presentHistoryHead_ + 1U) & (kPresentHistorySize - 1U));
    presentHistoryCount_ += static_cast<uint8_t>(presentHistoryCount_ < kPresentHistorySize);
}

bool RenderLoopPrivate::cadenceMatchesNominal() const noexcept
{
    const int64_t nominalNs = nominalContentFrameInterval.count();
    if (nominalNs <= 0) {
        return false;
    }

    constexpr size_t kMaxIntervals = 6;
    size_t found = 0;
    size_t matches = 0;
    int64_t prevTs = 0;
    int64_t sum = 0;
    int64_t minValue = std::numeric_limits<int64_t>::max();
    int64_t maxValue = 0;

    const int64_t toleranceNs = std::max<int64_t>(nominalNs >> 6, 300'000LL);
    const int64_t doubleTolerance = toleranceNs << 1;

    for (uint8_t offset = 0; offset < presentHistoryCount_ && found < kMaxIntervals; ++offset) {
        const uint8_t idx = static_cast<uint8_t>(
            (presentHistoryHead_ + kPresentHistorySize - 1U - offset) & (kPresentHistorySize - 1U));
        const PresentSample &sample = presentHistory_[idx];

        if (!sample.valid) {
            continue;
        }

        const int64_t ts = sample.timestamp.count();
        if (ts <= 0) {
            continue;
        }

        if (prevTs > 0) {
            const int64_t delta = prevTs - ts;
            if (delta >= kMinReasonableIntervalNs && delta <= kMaxReasonableIntervalNs) {
                ++found;
                sum += delta;
                minValue = std::min(minValue, delta);
                maxValue = std::max(maxValue, delta);

                const bool match =
                    sample.directScanout || fastAbs64(delta - nominalNs) <= doubleTolerance;
                matches += static_cast<size_t>(match);
            }
        }
        prevTs = ts;
    }

    if (found == 0) {
        return nearIntervalNs(lastIntervalNs_, nominalNs, toleranceNs);
    }

    const int64_t divisor = static_cast<int64_t>(found > 2 ? found - 2 : found);
    const int64_t dividend = found > 2 ? sum - minValue - maxValue : sum;
    int64_t avgNs = 0;

    switch (divisor) {
    case 1:
        avgNs = dividend;
        break;
    case 2:
        avgNs = dividend >> 1;
        break;
    case 3:
        avgNs = dividend / 3;
        break;
    case 4:
        avgNs = dividend >> 2;
        break;
    default:
        avgNs = dividend / divisor;
        break;
    }

    if (!nearIntervalNs(avgNs, nominalNs, toleranceNs)) {
        return false;
    }

    return (matches << 1U) >= (found + 1U);
}

bool RenderLoopPrivate::isBelowVrrFloor() const noexcept
{
    if (lfcCapable_ || vrrCaps_.minRefreshRate <= 0) {
        return false;
    }

    const int64_t intervalNs = nominalContentFrameInterval.count() > 0
        ? nominalContentFrameInterval.count()
        : lastIntervalNs_;

    if (intervalNs <= 0) {
        return false;
    }

    return static_cast<__uint128_t>(static_cast<uint64_t>(intervalNs)) *
            static_cast<uint64_t>(vrrCaps_.minRefreshRate) >
        static_cast<__uint128_t>(1'000'000'000'000ULL);
}

void RenderLoopPrivate::recordModeSwitch() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    modeDwellCounter_ = 0;

    modeSwitchHistory_[modeSwitchHistoryHead_] = now;
    modeSwitchHistoryHead_ = static_cast<uint8_t>((modeSwitchHistoryHead_ + 1U) & (kModeSwitchHistorySize - 1U));
    modeSwitchHistoryCount_ += static_cast<uint8_t>(modeSwitchHistoryCount_ < kModeSwitchHistorySize);
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    const uint8_t required = std::min<uint8_t>(kOscillationThreshold, static_cast<uint8_t>(kModeSwitchHistorySize));
    if (modeSwitchHistoryCount_ < required) {
        return false;
    }

    const auto cutoff = std::chrono::steady_clock::now() - kOscillationWindow;
    const uint8_t idx = static_cast<uint8_t>(
        (modeSwitchHistoryHead_ + kModeSwitchHistorySize - required) & (kModeSwitchHistorySize - 1U));

    if (modeSwitchHistory_[idx] >= cutoff) [[unlikely]] {
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

    const int64_t maxStepNs = std::max<int64_t>(prev >> 1, 250'000LL);
    const int64_t lo = std::max<int64_t>(prev - maxStepNs, kMinReasonableIntervalNs);
    const int64_t hi = std::min<int64_t>(prev + maxStepNs, kMaxReasonableIntervalNs);
    const int64_t filtered = std::clamp(intervalNs, lo, hi);

    const int64_t rawDiff = fastAbs64(filtered - prev);
    const int64_t diff = rawDiff > kCadenceNoiseFloorNs ? (rawDiff - kCadenceNoiseFloorNs) : 0LL;
    const int64_t relativeDeviation = std::min<int64_t>(
        (diff << 8) / std::max<int64_t>(prev, 1LL),
        256LL);

    const int32_t sample = static_cast<int32_t>(256LL - relativeDeviation);
    const int32_t current = static_cast<int32_t>(cadenceStability_);
    const int32_t isGreater = static_cast<int32_t>(sample > current);

    const int32_t shift = 1 + (isGreater << 1);
    const int32_t weight = 1 + (isGreater * 6);

    cadenceStability_ = static_cast<int16_t>((current * weight + sample) >> shift);
}

PresentationMode RenderLoopPrivate::selectPresentationMode() noexcept
{
    lastDecisionReason_ = VrrDecisionReason::None;

    if (!vrrEnabled || !vrrCapable) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::UserPolicy;
        return PresentationMode::VSync;
    }

    if (vrrMode == VrrMode::Never) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::UserPolicy;
        return PresentationMode::VSync;
    }

    const VrrStateCache::State state = vrrStateCache_.getState();
    if (state.isOnOutput == 0U || state.isFullScreen == 0U) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::UserPolicy;
        return PresentationMode::VSync;
    }

    const PresentationMode adaptive = adaptiveModeForState(state);
    const bool validHint = state.valid != 0U;

    if (vrrMode == VrrMode::Always) {
        lastDecisionReason_ = VrrDecisionReason::UserPolicy;
        return adaptive;
    }

    if (activeContentHint_ == VrrContentHint::ForceVsync) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }

    if (activeContentHint_ == VrrContentHint::ForceVrr) [[likely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return adaptive;
    }

    if (isExplicitVsyncHint(state.hint, validHint)) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }

    if (isExplicitAsyncHint(state.hint, validHint)) {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::AdaptiveAsync;
    }

    if (activeContentHint_ == VrrContentHint::Video || activeContentHint_ == VrrContentHint::Film) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }

    if (activeContentHint_ == VrrContentHint::Interactive || interactiveGraceFrames_ > 0U) [[likely]] {
        lastDecisionReason_ = VrrDecisionReason::InteractiveGrace;
        return adaptive;
    }

    if (vrrOscillationLockout && oscillationCooldownCounter_ > 0U) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::OscillationGuard;
        return PresentationMode::VSync;
    }

    const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
    if (cadenceStability_ <= kCadenceUnstableThreshold && lastIntervalNs_ >= (vblankNs * 4)) {
        lastDecisionReason_ = VrrDecisionReason::AmbiguousKeepCurrent;
        return presentationMode;
    }

    if (!validHint && isUnhintedFilmCadenceNs(lastIntervalNs_, cadenceStability_) && isBelowVrrFloor()) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::FilmCadence;
        return PresentationMode::VSync;
    }

    lastDecisionReason_ = VrrDecisionReason::AmbiguousKeepCurrent;
    return adaptive;
}

bool RenderLoopPrivate::shouldSwitchMode(PresentationMode target) noexcept
{
    if (oscillationCooldownCounter_ > 0U) {
        --oscillationCooldownCounter_;
        if (oscillationCooldownCounter_ == 0U) {
            vrrOscillationLockout = false;
        }
    }

    if (target == presentationMode) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = presentationMode;
        lastStableMode = presentationMode;
        return false;
    }

    const bool exitingToVsync = (target == PresentationMode::VSync);
    const VrrStateCache::State state = vrrStateCache_.getState();
    const bool fullscreenOnOutput = (state.isOnOutput != 0U) && (state.isFullScreen != 0U);
    const bool validHint = state.valid != 0U;
    const bool explicitVsync = isExplicitVsyncHint(state.hint, validHint);
    const bool explicitAsync = isExplicitAsyncHint(state.hint, validHint);
    const bool filmFallback = !validHint && isUnhintedFilmCadenceNs(lastIntervalNs_, cadenceStability_) && isBelowVrrFloor();

    if (exitingToVsync && (!vrrEnabled || !vrrCapable || vrrMode == VrrMode::Never || !fullscreenOnOutput)) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = target;
        lastStableMode = target;
        return true;
    }

    if (modeDwellCounter_ < kMinModeDwellFrames) {
        return false;
    }

    if (vrrMode == VrrMode::Always && !exitingToVsync) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = target;
        lastStableMode = target;
        return true;
    }

    if (vrrOscillationLockout
        && !exitingToVsync
        && activeContentHint_ == VrrContentHint::Unknown
        && !explicitAsync
        && interactiveGraceFrames_ == 0U) {
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

    uint16_t threshold = exitingToVsync ? kVrrToVsyncStabilityFrames : kVsyncToVrrStabilityFrames;

    if (exitingToVsync) {
        if (activeContentHint_ == VrrContentHint::ForceVsync) {
            threshold = 1U;
        } else if (activeContentHint_ == VrrContentHint::Video || activeContentHint_ == VrrContentHint::Film) {
            threshold = cadenceMatchesNominal() ? 2U : 3U;
        } else if (explicitVsync) {
            threshold = 2U;
        } else if (filmFallback) {
            threshold = 12U;
        } else {
            return false;
        }
    } else {
        if (activeContentHint_ == VrrContentHint::ForceVrr) {
            threshold = 1U;
        } else if (activeContentHint_ == VrrContentHint::Interactive || interactiveGraceFrames_ > 0U) {
            threshold = 1U;
        } else if (explicitAsync) {
            threshold = 1U;
        } else if (q->activeWindowControlsVrrRefreshRate()) {
            threshold = 1U;
        } else {
            threshold = 2U;
        }
    }

    if (pendingModeCounter_ < threshold) {
        return false;
    }

    if (!exitingToVsync
        && activeContentHint_ == VrrContentHint::Unknown
        && !explicitAsync
        && detectVrrOscillation()) {
        return false;
    }

    pendingModeCounter_ = 0;
    lastStableMode = target;
    return true;
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
    const int64_t isPos = diff > 0 ? 1 : 0;
    const int actualShift = isPos ? std::min(shift, 2) : shift;

    int64_t updated = cur + ((diff + isPos) >> actualShift);

    const int64_t vblank = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t lo = std::max(vblank >> 5, int64_t{100'000});
    const int64_t hi = vblank << 2;

    framePrediction = std::chrono::nanoseconds{std::clamp(updated, lo, hi)};
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (kwinApp()->isTerminating() || compositeTimer.isActive()) {
        return;
    }

    if (preparingNewFrame) {
        pendingReschedule = true;
        armStalledFrameTimer();
        return;
    }

    scheduleRepaint();
}

void RenderLoopPrivate::scheduleRepaint()
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this]() {
            scheduleRepaint();
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
        if (predNs <= 0) {
            predNs = static_cast<int64_t>(vblankNs >> 1);
        }
    }

    const int64_t stabilityBonus = (static_cast<int64_t>(cadenceStability_) * kRenderSlackNs) >> 9;
    const int64_t effectiveSlack = kRenderSlackNs - stabilityBonus;
    const int64_t compositeNs = std::clamp(
        predNs + safetyMargin.count() + effectiveSlack,
        kMinCompositeNs,
        kMaxCompositeNs);

    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();
    const VrrDecisionReason decisionReason = lastDecisionReason_;

    if (targetMode != presentationMode && shouldSwitchMode(targetMode)) {
        presentationMode = targetMode;
        recordModeSwitch();
        Q_EMIT q->presentationModeChanged(q, presentationMode, decisionReason);
    }

    if (modeDwellCounter_ < std::numeric_limits<uint16_t>::max()) {
        ++modeDwellCounter_;
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);

    if (pendingFrameCount >= maxPending) {
        if (!vrrActive && delayedVrrTimer.isActive()) {
            delayedVrrTimer.stop();
        }
        pendingReschedule = true;
        armStalledFrameTimer();
        return;
    }

    disarmStalledFrameTimer();

    const int64_t vblankI64 = static_cast<int64_t>(vblankNs);
    int64_t nextPresNs = nowNs + compositeNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t earliestReadyNs = nowNs + compositeNs;
        const int64_t nsFromLastPres = std::max<int64_t>(earliestReadyNs - lastPresNs, 1LL);

        int64_t targetVblank = static_cast<int64_t>(
            ceilDivU64Reciprocal(static_cast<uint64_t>(nsFromLastPres),
                                 vblankNs,
                                 vblankIntervalReciprocal64,
                                 reciprocalShift64));

        if (wasTripleBuffering && targetVblank <= 1) {
            targetVblank = 2;
        }

        nextPresNs = lastPresNs + (targetVblank * vblankI64);

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
        wasTripleBuffering = false;
        tripleBufferHysteresisCounter = 0;
    }

    const int64_t minPresNs = nowNs + kMinCompositeNs;
    if (nextPresNs < minPresNs) {
        nextPresNs = minPresNs;
    }
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    const int64_t nextRenderNs = nextPresNs - compositeNs;
    const int64_t delayNs = std::max<int64_t>(nextRenderNs - nowNs, 0LL);
    const int timerMs = delayNs < kNsPerMs
        ? 0
        : static_cast<int>(std::clamp<int64_t>(delayNs / kNsPerMs, 1LL, static_cast<int64_t>(kMaxTimerDelayMs)));

    if (compositeTimer.isActive()) {
        const int64_t scheduledNs = scheduledRenderTimestamp.count();
        const int64_t absDiffNs = fastAbs64(nextRenderNs - scheduledNs);

        if (absDiffNs < kTimerJitterFilterNs) {
            return;
        }

        if (scheduledTimerMs >= 0 && timerMs == scheduledTimerMs && absDiffNs < kNsPerMs) {
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

    preparingNewFrame = false;
    if (pendingFrameCount > 0) {
        --pendingFrameCount;
    }

    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) {
        ++consecutiveErrorCount;
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);

    if (!preparingNewFrame && pendingFrameCount < maxPending) {
        disarmStalledFrameTimer();
    } else if (pendingReschedule) {
        armStalledFrameTimer();
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
    if (!s_debugEnabled) [[likely]] {
        return;
    }

    if (!d->m_debugOutput) {
        if (!d->output) {
            return;
        }
        std::string name = d->output->name().toStdString();
        for (char &c : name) {
            c = std::isalnum(static_cast<unsigned char>(c)) ? c : '_';
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
} // namespace

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this, timestamp, renderTime, mode]() {
            notifyFrameCompleted(timestamp, renderTime, mode, nullptr);
        }, Qt::QueuedConnection);
        return;
    }

    preparingNewFrame = false;
    consecutiveErrorCount = 0;

    if (s_debugEnabled && frame) [[unlikely]] {
        writeDebug(this,
                   renderTime,
                   std::chrono::duration_cast<std::chrono::nanoseconds>(frame->targetPageflipTime().time_since_epoch()),
                   frame->refreshDuration(),
                   frame->predictedRenderTime(),
                   timestamp,
                   mode);
    }

    if (pendingFrameCount > 0) {
        --pendingFrameCount;
    }

    const int64_t prevPresentationNs = lastPresentationTimestamp.count();
    const int64_t nowNs = steadyNowNs();
    int64_t tsNs = timestamp.count();

    if (tsNs > nowNs) [[unlikely]] {
        tsNs = nowNs;
    }
    if (tsNs < prevPresentationNs) [[unlikely]] {
        tsNs = prevPresentationNs;
    }

    lastPresentationTimestamp = std::chrono::nanoseconds{tsNs};
    updatePresentationCadence(tsNs - prevPresentationNs);

    if (renderTime) {
        const auto dur = renderTime->end - renderTime->start;
        renderJournal.add(dur, timestamp);
        updateFramePrediction(dur);
    }

    if (interactiveGraceFrames_ > 0U) {
        --interactiveGraceFrames_;
    }

    if (isVrrMode(mode)) {
        if (starvationRecoveryCounter < kStarvationRecoveryFrames) {
            ++starvationRecoveryCounter;
        }
    } else {
        starvationRecoveryCounter = 0;
    }

    if (frame != nullptr) {
        PresentFeedback feedback{};
        feedback.actualPresentationTimestamp = timestamp;
        feedback.targetPresentationTimestamp =
            std::chrono::duration_cast<std::chrono::nanoseconds>(frame->targetPageflipTime().time_since_epoch());
        feedback.refreshDuration = frame->refreshDuration();

        const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
        int64_t graceNs = vblankNs / 8;

        if (graceNs < 700'000) {
            graceNs = 700'000;
        } else if (graceNs > 2'500'000) {
            graceNs = 2'500'000;
        }

        if (!isVrrMode(mode) && feedback.targetPresentationTimestamp.count() > 0) {
            feedback.deadlineMissed =
                timestamp > (feedback.targetPresentationTimestamp + std::chrono::nanoseconds{graceNs});
        } else {
            feedback.deadlineMissed = false;
        }

        feedback.directScanout = false;
        feedback.valid = true;
        addPresentFeedback(feedback);
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);

    if (!preparingNewFrame && pendingFrameCount < maxPending) {
        disarmStalledFrameTimer();
    } else if (pendingReschedule) {
        armStalledFrameTimer();
    }

    if (inhibitCount == 0 && (pendingReschedule || pendingFrameCount == 0)) {
        pendingReschedule = false;
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs)
{
    int64_t ts = timestamp.count();
    if (ts > nowNs) [[unlikely]] {
        ts = nowNs;
    }

    const int64_t prev = lastPresentationTimestamp.count();
    if (ts < prev) [[unlikely]] {
        ts = prev;
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
    } else if (id == d->stalledFrameTimer.timerId()) [[unlikely]] {
        d->handleStalledFrameTimeout();
    } else {
        QObject::timerEvent(event);
    }
}

RenderLoop::RenderLoop(BackendOutput *output)
    : d(std::make_unique<RenderLoopPrivate>(this, output))
{
}

RenderLoop::~RenderLoop() = default;

void RenderLoop::inhibit()
{
    if (++d->inhibitCount == 1) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->delayedVrrTimer.stop();
        d->disarmStalledFrameTimer();
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
    const bool vrrActive = isVrrMode(d->presentationMode);
    const int maxPending = effectiveMaxPendingFrames(d.get(), vrrActive);

    if (!d->preparingNewFrame && d->pendingFrameCount < maxPending) {
        d->disarmStalledFrameTimer();
    } else if (d->pendingReschedule) {
        d->armStalledFrameTimer();
    }

    if (d->inhibitCount == 0 && d->pendingReschedule) {
        d->scheduleNextRepaint();
    }
}

int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int rate)
{
    rate = std::clamp(rate, 1000, 1000000);
    if (d->refreshRate == rate) {
        return;
    }

    d->refreshRate = rate;
    d->cachedVblankIntervalNs = 1'000'000'000'000ULL / static_cast<uint64_t>(rate);

    if (d->vrrCaps_.maxRefreshRate <= 0) {
        d->vrrCaps_.maxRefreshRate = rate;
    }

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

void RenderLoop::setVrrCapabilities(const VrrCapabilities &caps)
{
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, caps]() {
            setVrrCapabilities(caps);
        }, Qt::QueuedConnection);
        return;
    }

    d->setVrrCapabilities(caps);
    if (d->inhibitCount == 0) {
        d->scheduleNextRepaint();
    }
}

void RenderLoop::setActiveContentHint(VrrContentHint hint,
                                      std::optional<std::chrono::nanoseconds> nominalFrameInterval)
{
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, hint, nominalFrameInterval]() {
            setActiveContentHint(hint, nominalFrameInterval);
        }, Qt::QueuedConnection);
        return;
    }

    d->setActiveContentHint(hint, nominalFrameInterval);
}

void RenderLoop::notifyPresentFeedback(const PresentFeedback &feedback)
{
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, feedback]() {
            notifyPresentFeedback(feedback);
        }, Qt::QueuedConnection);
        return;
    }

    d->addPresentFeedback(feedback);
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer* layer)
{
    (void)layer;
    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this]() {
            scheduleRepaint(nullptr, nullptr);
        }, Qt::QueuedConnection);
        return;
    }

    if (d->inhibitCount != 0) {
        if (d->delayedVrrTimer.isActive()) {
            d->delayedVrrTimer.stop();
        }
        d->pendingReschedule = true;
        return;
    }

    if (d->preparingNewFrame) {
        d->pendingReschedule = true;
        d->armStalledFrameTimer();
        return;
    }

    if (d->vrrStateDirty_) {
        d->updateVrrState();
    }

    const bool vrrActive = isVrrMode(d->presentationMode);
    const int maxPending = effectiveMaxPendingFrames(d.get(), vrrActive);
    const bool blockedByQueue = d->pendingFrameCount >= maxPending;

    if (blockedByQueue) {
        if (!vrrActive && d->delayedVrrTimer.isActive()) {
            d->delayedVrrTimer.stop();
        }
        d->pendingReschedule = true;
        d->armStalledFrameTimer();
        return;
    }

    const bool maybeDelayControl = vrrActive &&
        item != nullptr &&
        d->activeContentHint_ == VrrContentHint::Unknown &&
        d->interactiveGraceFrames_ == 0U &&
        d->cadenceStability_ < kCadenceStableThreshold &&
        d->pendingFrameCount > 0;

    if (maybeDelayControl) {
        const VrrStateCache::State state = d->vrrStateCache_.getState();
        if (state.isOnOutput != 0U) {
            Window *const tracked = d->trackedWindow_.data();
            if (tracked != nullptr) {
                SurfaceItem *const surface = tracked->surfaceItem();
                if (surface != nullptr && item != surface && !surface->isAncestorOf(item)) {
                    const auto estimate = surface->recursiveFrameTimeEstimation();
                    if (durationEstimateAtMost(estimate, kVrrControlThreshold)) {
                        if (!d->delayedVrrTimer.isActive()) {
                            d->delayedVrrTimer.start(d->vrrControlDelayMs, Qt::PreciseTimer, this);
                        }
                        return;
                    }
                }
            }
        }
    }

    if (d->delayedVrrTimer.isActive()) {
        d->delayedVrrTimer.stop();
    }

    d->disarmStalledFrameTimer();
    d->scheduleNextRepaint();
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    if (d->output == nullptr) {
        return false;
    }

    switch (d->activeContentHint_) {
    case VrrContentHint::ForceVsync:
    case VrrContentHint::Video:
    case VrrContentHint::Film:
        return false;
    case VrrContentHint::ForceVrr:
    case VrrContentHint::Interactive:
        return true;
    case VrrContentHint::Unknown:
        break;
    }

    Workspace *const ws = workspace();
    if (ws == nullptr) {
        return false;
    }

    Window *const active = ws->activeWindow();
    if (active == nullptr) {
        return false;
    }

    LogicalOutput *const logical = ws->findOutput(d->output);
    if (logical == nullptr || !active->frameGeometry().intersects(logical->geometryF())) {
        return false;
    }

    SurfaceItem *const surface = active->surfaceItem();
    if (surface == nullptr) {
        return false;
    }

    constexpr auto k30HzFrame = std::chrono::nanoseconds{1'000'000'000LL / 30LL};
    const auto estimate = surface->recursiveFrameTimeEstimation();
    return durationEstimateAtMost(estimate, k30HzFrame);
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
    if (d->presentationMode == mode) {
        return;
    }

    d->presentationMode = mode;
    d->lastStableMode = mode;
    Q_EMIT presentationModeChanged(this, mode, VrrDecisionReason::UserPolicy);
}

void RenderLoop::setMaxPendingFrameCount(uint32_t count)
{
    d->maxPendingFrameCount = static_cast<int>(std::clamp(count, 1U, 3U));
}

std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

} // namespace KWin

#include "moc_renderloop.cpp"
