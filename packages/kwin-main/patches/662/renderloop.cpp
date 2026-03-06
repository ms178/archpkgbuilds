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
#include <chrono>
#include <cstdint>
#include <fstream>
#include <limits>

class LogicalOutput;               // forward declaration – fixes include-order issues in 6.6.1

using namespace std::chrono_literals;

namespace {

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
inline constexpr int64_t safeAbs64(int64_t v) noexcept {
    if (v == std::numeric_limits<int64_t>::min()) [[unlikely]] return std::numeric_limits<int64_t>::max();
    return v < 0 ? -v : v;
}

[[gnu::always_inline, gnu::const]]
inline constexpr bool isVrrMode(KWin::PresentationMode mode) noexcept {
    return mode == KWin::PresentationMode::AdaptiveSync || mode == KWin::PresentationMode::AdaptiveAsync;
}

[[gnu::always_inline]]
inline int64_t steadyNowNs() noexcept {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

[[gnu::always_inline, gnu::const]]
inline uint64_t ceilDivU64Reciprocal(uint64_t n, uint64_t d, uint64_t recip, uint8_t shift) noexcept {
    if (n == 0 || d == 0) [[unlikely]] return 0;
    if (recip == 0 || shift == 0) [[unlikely]] return (n + d - 1) / d;
    const auto prod = static_cast<__uint128_t>(n) * recip;
    auto q = static_cast<uint64_t>(prod >> shift);
    if (q * d < n) ++q;
    return q;
}

[[nodiscard]] consteval uint64_t reciprocalForInterval(uint64_t interval) noexcept
{
    constexpr uint8_t shift = 63;
    return static_cast<uint64_t>(((static_cast<__uint128_t>(1) << shift) + interval - 1) / interval);
}

constexpr uint64_t kRecip60Hz  = reciprocalForInterval(16'666'666ULL);
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

enum class CadenceClass : uint8_t {
    High = 0,
    Video = 1,
    Variable = 2,
    Unstable = 3,
};

[[gnu::always_inline, gnu::const]]
inline bool nearNs(int64_t value, int64_t target, int64_t tolerance) noexcept
{
    return safeAbs64(value - target) <= tolerance;
}

[[gnu::always_inline, gnu::const]]
inline bool isLikelyVideoCadenceNs(int64_t intervalNs, int16_t stability) noexcept
{
    if (intervalNs <= 0 || stability < kCadenceStableThreshold) {
        return false;
    }

    constexpr int64_t kVideoCadencesNs[] = {
        16'666'666LL, // 60 / 59.94 fps
        20'000'000LL, // 50 fps
        33'333'333LL, // 30 / 29.97 fps
        40'000'000LL, // 25 fps
        41'666'666LL, // 24 / 23.976 fps
    };

    for (const int64_t cadenceNs : kVideoCadencesNs) {
        const int64_t toleranceNs = std::max<int64_t>(cadenceNs / 64, 300'000LL);
        if (nearNs(intervalNs, cadenceNs, toleranceNs)) {
            return true;
        }
    }
    return false;
}

[[gnu::always_inline, gnu::const]]
inline CadenceClass classifyCadence(int64_t intervalNs, int64_t vblankNs, int16_t stability) noexcept
{
    if (intervalNs <= 0 || vblankNs <= 0) {
        return CadenceClass::Variable;
    }
    if (isLikelyVideoCadenceNs(intervalNs, stability)) {
        return CadenceClass::Video;
    }
    if (intervalNs <= ((vblankNs * 3) / 2)) {
        return CadenceClass::High;
    }
    if (stability <= kCadenceUnstableThreshold && intervalNs >= (vblankNs * 4)) {
        return CadenceClass::Unstable;
    }
    return CadenceClass::Variable;
}

[[gnu::always_inline, gnu::const]]
inline KWin::PresentationMode adaptiveModeForHint(uint8_t hintRaw) noexcept
{
    return static_cast<KWin::PresentationModeHint>(hintRaw) == KWin::PresentationModeHint::Async
        ? KWin::PresentationMode::AdaptiveAsync
        : KWin::PresentationMode::AdaptiveSync;
}

} // namespace

namespace KWin {

static const bool s_debugEnabled = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop) noexcept {
    return loop ? loop->d.get() : nullptr;
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, BackendOutput *output) : q(q), output(output) {
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

RenderLoopPrivate::~RenderLoopPrivate() {
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
        const auto num = static_cast<__uint128_t>(1) << shift;
        vblankIntervalReciprocal64 = static_cast<uint64_t>((num + interval - 1) / interval);
        break;
    }
    }

    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);
    vrrControlDelayMs = static_cast<int16_t>(std::clamp(
        interval / static_cast<uint64_t>(kNsPerMs),
        uint64_t{1},
        static_cast<uint64_t>(kMaxTimerDelayMs)
    ));
}

void RenderLoopPrivate::initializeVrrCapabilities() {
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

void RenderLoopPrivate::connectVrrSignals(Window *window) {
    if (!window || !vrrEnabled || trackedWindow_ == window) return;
    disconnectVrrSignals();
    trackedWindow_ = window;
    vrrConnectionCount_ = 0;
    auto store = [this](QMetaObject::Connection c) {
        if (c && vrrConnectionCount_ < vrrConnections_.size()) vrrConnections_[vrrConnectionCount_++] = c;
    };
    store(QObject::connect(window, &QObject::destroyed, q, [this]() { vrrStateDirty_ = true; }));
    store(QObject::connect(window, &Window::fullScreenChanged, q, [this]() { vrrStateDirty_ = true; }));
    store(QObject::connect(window, &Window::outputChanged, q, [this]() { vrrStateDirty_ = true; }));
    SurfaceItem *surf = window->surfaceItem();
    if (surf) {
        auto *wayland = qobject_cast<SurfaceItemWayland *>(surf);
        if (wayland) {
            auto *surface = wayland->surface();
            if (surface) {
                store(QObject::connect(surface, &SurfaceInterface::presentationModeHintChanged, q, [this]() { vrrStateDirty_ = true; }));
            }
        }
    }
}

void RenderLoopPrivate::disconnectVrrSignals() noexcept {
    for (uint8_t i = 0; i < vrrConnectionCount_; ++i) {
        QObject::disconnect(vrrConnections_[i]);
        vrrConnections_[i] = {};
    }
    vrrConnectionCount_ = 0;
    trackedWindow_ = nullptr;
}

void RenderLoopPrivate::invalidateVrrState() noexcept { vrrStateDirty_ = true; }

void RenderLoopPrivate::updateVrrState() noexcept
{
    if (!vrrStateDirty_) {
        return;
    }
    vrrStateDirty_ = false;

    VrrStateCache::State state{};
    const uint8_t oldRaw = vrrStateCache_.raw();

    if (!vrrEnabled || !vrrCapable || output == nullptr) {
        if (trackedWindow_ != nullptr) {
            disconnectVrrSignals();
        }
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    Workspace *const ws = workspace();
    if (ws == nullptr) {
        if (trackedWindow_ != nullptr) {
            disconnectVrrSignals();
        }
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    Window *const active = ws->activeWindow();
    if (active == nullptr) {
        if (trackedWindow_ != nullptr) {
            disconnectVrrSignals();
        }
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    connectVrrSignals(active);

    if (!active->isFullScreen()) {
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    LogicalOutput *const logical = ws->findOutput(output);
    if (logical == nullptr || !active->isOnOutput(logical)) {
        if (oldRaw != state.toRaw()) {
            vrrStateCache_.setState(state);
        }
        return;
    }

    state.isOnOutput = 1U;
    state.isFullScreen = 1U;

    SurfaceItem *const surf = active->surfaceItem();
    if (surf != nullptr) {
        if (auto *const wayland = qobject_cast<SurfaceItemWayland *>(surf)) {
            if (auto *const surface = wayland->surface()) {
                state.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3U;
                state.valid = 1U;
            }
        }
    }

    if (oldRaw != state.toRaw()) {
        vrrStateCache_.setState(state);
    }
}

void RenderLoopPrivate::recordModeSwitch() noexcept {
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    modeDwellCounter_ = 0;
    modeSwitchHistory_[modeSwitchHistoryHead_] = now;
    modeSwitchHistoryHead_ = static_cast<uint8_t>((modeSwitchHistoryHead_ + 1) % kModeSwitchHistorySize);
    if (modeSwitchHistoryCount_ < kModeSwitchHistorySize) ++modeSwitchHistoryCount_;
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept
{
    const uint8_t required = std::min<uint8_t>(kOscillationThreshold, static_cast<uint8_t>(kModeSwitchHistorySize));
    if (modeSwitchHistoryCount_ < required) {
        return false;
    }

    const auto cutoff = std::chrono::steady_clock::now() - kOscillationWindow;
    uint8_t recentCount = 0;

    for (uint8_t i = 0; i < modeSwitchHistoryCount_; ++i) {
        const uint8_t idx = static_cast<uint8_t>((modeSwitchHistoryHead_ - 1U - i) & (kModeSwitchHistorySize - 1U));
        if (modeSwitchHistory_[idx] < cutoff) {
            break;
        }
        ++recentCount;
        if (recentCount >= required) [[unlikely]] {
            vrrOscillationLockout = true;
            oscillationCooldownCounter_ = kOscillationCooldownFrames;
            return true;
        }
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

    const int64_t diff = safeAbs64(filtered - prev);
    const int64_t relativeDeviation = std::min<int64_t>(
        (diff << 8) / std::max<int64_t>(prev, int64_t{1}),
        int64_t{256});
    const int64_t sample = std::max<int64_t>(int64_t{0}, int64_t{256} - relativeDeviation);

    const int16_t current = cadenceStability_;
    const int16_t next = sample > current
        ? static_cast<int16_t>((static_cast<int32_t>(current) * 7 + static_cast<int32_t>(sample)) >> 3)
        : static_cast<int16_t>((static_cast<int32_t>(current) + static_cast<int32_t>(sample)) >> 1);
    cadenceStability_ = next;
}

bool RenderLoopPrivate::isFrameTimeStable() const noexcept {
    return cadenceStability_ >= kCadenceStableThreshold;
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

    if (exitingToVsync && (!vrrEnabled || !vrrCapable || vrrMode == VrrMode::Never || !fullscreenOnOutput)) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = target;
        lastStableMode = target;
        return true;
    }

    if (modeDwellCounter_ < kMinModeDwellFrames) {
        return false;
    }

    const bool likelyVideo = isLikelyVideoCadenceNs(lastIntervalNs_, cadenceStability_);

    if (vrrMode == VrrMode::Always && !exitingToVsync) {
        pendingModeCounter_ = 0;
        pendingTargetMode_ = target;
        lastStableMode = target;
        return true;
    }

    if (vrrOscillationLockout && !exitingToVsync) {
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
    if (likelyVideo && exitingToVsync) {
        threshold = 2U;
    } else if (lastStableMode == PresentationMode::VSync && !exitingToVsync) {
        threshold = std::max<uint16_t>(kVsyncToVrrStabilityFrames, static_cast<uint16_t>(3));
    }

    if (pendingModeCounter_ < threshold) {
        return false;
    }

    if (!exitingToVsync && detectVrrOscillation()) {
        return false;
    }

    pendingModeCounter_ = 0;
    lastStableMode = target;

    if (likelyVideo && exitingToVsync) {
        vrrOscillationLockout = true;
        oscillationCooldownCounter_ = std::max<uint16_t>(oscillationCooldownCounter_, kOscillationCooldownFrames);
    }

    return true;
}

PresentationMode RenderLoopPrivate::selectPresentationMode() noexcept
{
    if (!vrrEnabled || !vrrCapable) [[likely]] {
        return PresentationMode::VSync;
    }
    if (vrrMode == VrrMode::Never) {
        return PresentationMode::VSync;
    }

    const VrrStateCache::State state = vrrStateCache_.getState();
    if ((state.isOnOutput == 0U) || (state.isFullScreen == 0U)) {
        return PresentationMode::VSync;
    }

    if (vrrMode == VrrMode::Always) {
        return PresentationMode::AdaptiveSync;
    }

    const bool validHint = (state.valid != 0U);
    const auto hint = validHint ? static_cast<PresentationModeHint>(state.hint)
                                : PresentationModeHint::VSync;

    if (validHint && hint == PresentationModeHint::VSync) {
        return PresentationMode::VSync;
    }

    if (vrrOscillationLockout && oscillationCooldownCounter_ > 0U) {
        return PresentationMode::VSync;
    }

    const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
    const CadenceClass cadence = classifyCadence(lastIntervalNs_, vblankNs, cadenceStability_);
    const PresentationMode adaptive = validHint ? adaptiveModeForHint(state.hint)
                                                : PresentationMode::AdaptiveSync;

    switch (cadence) {
    case CadenceClass::Video:
        return (validHint && hint == PresentationModeHint::Async)
            ? PresentationMode::AdaptiveAsync
            : PresentationMode::VSync;
    case CadenceClass::High:
        return adaptive;
    case CadenceClass::Unstable:
        return presentationMode;
    case CadenceClass::Variable:
        return adaptive;
    }

    return adaptive;
}

void RenderLoopPrivate::updateFramePrediction(std::chrono::nanoseconds measured) noexcept {
    const int64_t m = measured.count();
    if (m <= 0) [[unlikely]] return;
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

void RenderLoopPrivate::scheduleNextRepaint() {
    if (kwinApp()->isTerminating() || compositeTimer.isActive() || preparingNewFrame) return;
    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTarget)
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this, lastTarget]() { scheduleRepaint(lastTarget); }, Qt::QueuedConnection);
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
    const int64_t compositeNs = std::clamp(predNs + safetyMargin.count() + effectiveSlack,
                                           kMinCompositeNs,
                                           kMaxCompositeNs);

    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();
    if (targetMode != presentationMode && shouldSwitchMode(targetMode)) {
        presentationMode = targetMode;
        recordModeSwitch();
    }

    if (modeDwellCounter_ < std::numeric_limits<uint16_t>::max()) {
        ++modeDwellCounter_;
    }

    const int64_t vblankI64 = static_cast<int64_t>(vblankNs);
    int64_t nextPresNs = nowNs + compositeNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t earliestReadyNs = nowNs + compositeNs;
        const int64_t nsFromLastPres = std::max(earliestReadyNs - lastPresNs, int64_t{1});
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

    const int64_t delayNs = std::max(nextRenderNs - nowNs, int64_t{0});
    const int timerMs = delayNs < kNsPerMs
        ? 0
        : static_cast<int>(std::clamp(delayNs / kNsPerMs, int64_t{1}, int64_t{kMaxTimerDelayMs}));

    if (compositeTimer.isActive()) {
        const int64_t scheduledNs = scheduledRenderTimestamp.count();
        const int64_t absDiffNs = safeAbs64(nextRenderNs - scheduledNs);
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

void RenderLoopPrivate::delayScheduleRepaint() noexcept { pendingReschedule = true; }

void RenderLoopPrivate::notifyFrameDropped() {
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this]() { notifyFrameDropped(); }, Qt::QueuedConnection);
        return;
    }
    if (pendingFrameCount > 0) --pendingFrameCount;
    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) ++consecutiveErrorCount;
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

namespace {
void writeDebug(RenderLoopPrivate *d, std::optional<RenderTimeSpan> rt, std::chrono::nanoseconds targetFlip, std::chrono::nanoseconds refreshDur, std::chrono::nanoseconds predRender, std::chrono::nanoseconds ts, PresentationMode mode) {
    if (!s_debugEnabled) [[likely]] return;
    if (!d->m_debugOutput) {
        if (!d->output) return;
        std::string name = d->output->name().toStdString();
        for (char &c : name) c = std::isalnum(static_cast<unsigned char>(c)) ? c : '_';
        d->m_debugOutput.emplace("kwin_perf_" + name + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) *d->m_debugOutput << "target,flip,start,end,margin,dur,vrr,pred,cadence\n";
    }
    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const auto times = rt.value_or(RenderTimeSpan{});
        *d->m_debugOutput << targetFlip.count() << ',' << ts.count() << ',' << times.start.time_since_epoch().count() << ',' << times.end.time_since_epoch().count() << ',' << d->safetyMargin.count() << ',' << refreshDur.count() << ',' << isVrrMode(mode) << ',' << predRender.count() << ',' << d->cadenceStability_ << '\n';
    }
}
}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q,
                                  [this, timestamp, renderTime, mode, frame]() {
                                      notifyFrameCompleted(timestamp, renderTime, mode, frame);
                                  },
                                  Qt::QueuedConnection);
        return;
    }

    consecutiveErrorCount = 0;

    if (s_debugEnabled && frame) [[unlikely]] {
        writeDebug(this,
                   renderTime,
                   frame->targetPageflipTime().time_since_epoch(),
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

    if (isVrrMode(mode)) {
        if (starvationRecoveryCounter < kStarvationRecoveryFrames) {
            ++starvationRecoveryCounter;
        }
    } else {
        starvationRecoveryCounter = 0;
    }

    if (inhibitCount == 0 && (pendingReschedule || pendingFrameCount == 0)) {
        pendingReschedule = false;
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs) {
    int64_t ts = timestamp.count();
    if (ts > nowNs) [[unlikely]] ts = nowNs;
    if (ts < lastPresentationTimestamp.count()) [[unlikely]] ts = lastPresentationTimestamp.count();
    lastPresentationTimestamp = std::chrono::nanoseconds{ts};
}

void RenderLoopPrivate::dispatch() { Q_EMIT q->frameRequested(q); }

void RenderLoop::timerEvent(QTimerEvent *event) {
    const int id = event->timerId();
    if (id == d->compositeTimer.timerId()) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (id == d->delayedVrrTimer.timerId()) [[unlikely]] {
        d->delayedVrrTimer.stop();
        scheduleRepaint(nullptr, nullptr);
    } else QObject::timerEvent(event);
}

RenderLoop::RenderLoop(BackendOutput *output) : d(std::make_unique<RenderLoopPrivate>(this, output)) {}
RenderLoop::~RenderLoop() = default;

void RenderLoop::inhibit() {
    if (++d->inhibitCount == 1) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
    }
}

void RenderLoop::uninhibit() {
    Q_ASSERT(d->inhibitCount > 0);
    if (--d->inhibitCount == 0) d->scheduleNextRepaint();
}

void RenderLoop::prepareNewFrame() {
    Q_ASSERT(!d->preparingNewFrame);
    ++d->pendingFrameCount;
    d->preparingNewFrame = true;
}

void RenderLoop::newFramePrepared() { d->preparingNewFrame = false; }

int RenderLoop::refreshRate() const { return d->refreshRate; }

void RenderLoop::setRefreshRate(int rate) {
    rate = std::clamp(rate, 1000, 1000000);
    if (d->refreshRate == rate) return;
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

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds margin) {
    d->safetyMargin = margin.count() > 0 ? margin : std::chrono::nanoseconds{0};
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *layer)
{
    (void)layer;

    if (thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(this, [this, item, layer]() { scheduleRepaint(item, layer); }, Qt::QueuedConnection);
        return;
    }

    const bool vrrActive = isVrrMode(d->presentationMode);
    int maxPending = d->maxPendingFrameCount;
    if (vrrActive) {
        maxPending = (d->starvationRecoveryCounter >= kStarvationRecoveryFrames) ? 2 : 1;
    }

    if (d->inhibitCount != 0 || d->pendingFrameCount >= maxPending) {
        if (!vrrActive && d->delayedVrrTimer.isActive()) {
            d->delayedVrrTimer.stop();
        }
        d->pendingReschedule = true;
        return;
    }

    if (vrrActive && item != nullptr && d->pendingFrameCount > 0) {
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

    d->scheduleNextRepaint();
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    if (d->output == nullptr) {
        return false;
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

std::chrono::nanoseconds RenderLoop::lastPresentationTimestamp() const { return d->lastPresentationTimestamp; }
std::chrono::nanoseconds RenderLoop::nextPresentationTimestamp() const { return d->nextPresentationTimestamp; }
void RenderLoop::setPresentationMode(PresentationMode mode) { d->presentationMode = mode; }
void RenderLoop::setMaxPendingFrameCount(uint32_t count) { d->maxPendingFrameCount = static_cast<int>(std::clamp(count, 1u, 3u)); }
std::chrono::nanoseconds RenderLoop::predictedRenderTime() const { return d->renderJournal.result(); }

} // namespace KWin

#include "moc_renderloop.cpp"
