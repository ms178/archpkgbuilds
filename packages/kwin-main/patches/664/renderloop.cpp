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

constexpr uint8_t kPanelRefreshFloorStableFrames = 3;

constexpr int64_t kStalledFrameMinTimeoutNs = 12'000'000;
constexpr int64_t kStalledFrameMaxTimeoutNs = 120'000'000;
constexpr int64_t kStalledFrameExtraTimeoutNs = 2'000'000;
constexpr uint8_t kStalledFrameRecoveryThreshold = 2;
constexpr int64_t kStalledFrameVblankMultiplier = 3;

constexpr int64_t kMinDynamicJitterNs = 50'000;
constexpr int64_t kMinSameMsGuardNs = 100'000;
constexpr int64_t kMaxSameMsGuardNs = 900'000;

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t dynamicRescheduleJitterNs(int64_t vblankNs) noexcept
{
    const int64_t scaled = vblankNs / 24;
    return std::clamp(scaled, kMinDynamicJitterNs, kTimerJitterFilterNs);
}

[[gnu::always_inline, gnu::const]]
inline constexpr int64_t dynamicSameMsGuardNs(int64_t vblankNs) noexcept
{
    const int64_t scaled = vblankNs / 8;
    return std::clamp(scaled, kMinSameMsGuardNs, kMaxSameMsGuardNs);
}

[[gnu::always_inline]]
inline int adaptiveErrorBackoffMaxMs(int64_t vblankNs) noexcept
{
    const int64_t vblankMs = std::clamp<int64_t>((vblankNs + kNsPerMs - 1) / kNsPerMs, 1LL, 16LL);
    const int64_t adaptive = std::clamp<int64_t>(vblankMs * 2, 4LL, static_cast<int64_t>(kMaxErrorBackoffMs));
    return static_cast<int>(adaptive);
}

[[nodiscard]]
inline KWin::SurfaceInterface *resolvePresentationSurface(KWin::Window *window) noexcept
{
    if (window == nullptr) { return nullptr; }
    KWin::SurfaceItem *const surf = window->surfaceItem();
    if (surf == nullptr) { return nullptr; }
    auto *const wayland = qobject_cast<KWin::SurfaceItemWayland *>(surf);
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
    if (recip == 0 || shift == 0) [[unlikely]] { return d == 0 ? 0 : (n + d - 1) / d; }
    const auto prod = static_cast<__uint128_t>(n) * recip;
    const auto q = static_cast<uint64_t>(prod >> shift);
    return q + static_cast<uint64_t>(static_cast<__uint128_t>(q) * d < n);
}

[[nodiscard]] consteval uint64_t reciprocalForInterval(uint64_t interval) noexcept
{
    constexpr uint8_t shift = 63;
    return ((static_cast<__uint128_t>(1) << shift) + interval - 1) / interval;
}

constexpr uint64_t kRecip60Hz  = reciprocalForInterval(16'666'666ULL);
constexpr uint64_t kRecip120Hz = reciprocalForInterval( 8'333'333ULL);
constexpr uint64_t kRecip144Hz = reciprocalForInterval( 6'944'444ULL);
constexpr uint64_t kRecip165Hz = reciprocalForInterval( 6'060'606ULL);
constexpr uint64_t kRecip240Hz = reciprocalForInterval( 4'166'666ULL);

template<typename Estimate>
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

// Map the Wayland contenttype-v1 enum onto the 2-bit ContentTypeBits stored
// in the VrrStateCache.  Returns std::nullopt if no value is reported, so the
// cache can correctly mark contentTypeValid=0.
[[gnu::always_inline]]
inline std::optional<KWin::VrrStateCache::ContentTypeBits>
mapContentType(KWin::ContentType ct) noexcept
{
    switch (ct) {
    case KWin::ContentType::None:  return KWin::VrrStateCache::ContentTypeBits::None;
    case KWin::ContentType::Photo: return KWin::VrrStateCache::ContentTypeBits::Photo;
    case KWin::ContentType::Video: return KWin::VrrStateCache::ContentTypeBits::Video;
    case KWin::ContentType::Game:  return KWin::VrrStateCache::ContentTypeBits::Game;
    }
    return std::nullopt;
}

[[gnu::always_inline]]
inline int effectiveMaxPendingFrames(const KWin::RenderLoopPrivate *d, bool vrrActive) noexcept
{
    const int configuredMax = d->maxPendingFrameCount;
    if (!vrrActive) [[likely]] { return configuredMax; }
    if (configuredMax <= 1) { return configuredMax; }
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
    if (d->interactiveGraceFrames_ > 0U
        || d->pendingReschedule
        || d->starvationRecoveryCounter >= KWin::RenderLoopPrivate::kStarvationRecoveryFrames
        || d->consecutiveErrorCount != 0U
        || d->cadenceStability_ < kCadenceStableThreshold) {
        return maxPending;
    }
    const int64_t vblankNs = static_cast<int64_t>(d->cachedVblankIntervalNs);
    int64_t predictedNs = d->framePrediction.count();
    if (predictedNs <= 0) { predictedNs = d->renderJournal.result().count(); if (predictedNs <= 0) { predictedNs = vblankNs >> 1; } }
    return (predictedNs * 5) >= (vblankNs * 2) ? maxPending : 1;
}

} // namespace

namespace KWin
{

static const bool s_debugEnabled = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop) noexcept { return loop ? loop->d.get() : nullptr; }

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, BackendOutput *output)
    : q(q)
    , output(output)
{
    cachedVblankIntervalNs = 1'000'000'000'000ULL / 60'000ULL;
    vrrCaps_.maxRefreshRate = refreshRate;
    updateReciprocal();
    initializeVrrCapabilities();
    filteredPanelRefreshRateMhz_ = static_cast<uint32_t>(refreshRate);
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now;
    lastPresentationTimestamp = now.time_since_epoch();
    lastIntervalNs_ = 0;
    cadenceStability_ = 128;
    tripleBufferHysteresisCounter = 0;
}

RenderLoopPrivate::~RenderLoopPrivate() { disconnectVrrSignals(); }

void RenderLoopPrivate::updateReciprocal() noexcept
{
    const uint64_t interval = cachedVblankIntervalNs;
    if (interval == 0) [[unlikely]] {
        vblankIntervalReciprocal64 = 0; reciprocalShift64 = 0;
        tripleBufferEnterThresholdNs = 0; tripleBufferExitThresholdNs = 0; vrrControlDelayMs = 0;
        return;
    }
    constexpr uint8_t shift = 63;
    reciprocalShift64 = shift;
    switch (interval) {
    case 16'666'666ULL: vblankIntervalReciprocal64 = kRecip60Hz;  break;
    case  8'333'333ULL: vblankIntervalReciprocal64 = kRecip120Hz; break;
    case  6'944'444ULL: vblankIntervalReciprocal64 = kRecip144Hz; break;
    case  6'060'606ULL: vblankIntervalReciprocal64 = kRecip165Hz; break;
    case  4'166'666ULL: vblankIntervalReciprocal64 = kRecip240Hz; break;
    default: {
        const __uint128_t num = static_cast<__uint128_t>(1) << shift;
        vblankIntervalReciprocal64 = static_cast<uint64_t>((num + interval - 1) / interval);
        break;
    }
    }
    tripleBufferEnterThresholdNs = static_cast<int64_t>((interval * kTripleBufferEnterPct) / 100);
    tripleBufferExitThresholdNs = static_cast<int64_t>((interval * kTripleBufferExitPct) / 100);
    vrrControlDelayMs = static_cast<int16_t>(std::clamp(
        interval / static_cast<uint64_t>(kNsPerMs), uint64_t{1}, static_cast<uint64_t>(kMaxTimerDelayMs)));
}

void RenderLoopPrivate::armStalledFrameTimer() noexcept
{
    if (kwinApp()->isTerminating() || inhibitCount != 0) { return; }
    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    if (!preparingNewFrame && pendingFrameCount < maxPending) { return; }
    int64_t predictedNs = framePrediction.count();
    if (predictedNs <= 0) {
        predictedNs = renderJournal.result().count();
        if (predictedNs <= 0) { predictedNs = std::max<int64_t>(static_cast<int64_t>(cachedVblankIntervalNs >> 1), kMinCompositeNs); }
    }
    const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
    const int64_t timeoutNs = std::clamp(
        std::max<int64_t>(vblankNs * kStalledFrameVblankMultiplier, predictedNs + safetyMargin.count() + kStalledFrameExtraTimeoutNs),
        kStalledFrameMinTimeoutNs, kStalledFrameMaxTimeoutNs);
    const int timerMs = static_cast<int>(std::clamp((timeoutNs + kNsPerMs - 1) / kNsPerMs, int64_t{1}, int64_t{kMaxTimerDelayMs}));
    if (stalledFrameTimer.isActive() && stalledFrameTimerMs_ >= 0 && stalledFrameTimerMs_ <= timerMs) { return; }
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
    stalledFrameTimer.stop(); stalledFrameTimerMs_ = -1;
    if (kwinApp()->isTerminating() || inhibitCount != 0) { stalledFrameTimeoutCount_ = 0; return; }
    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    const bool blocked = preparingNewFrame || pendingFrameCount >= maxPending;
    if (!blocked || (!pendingReschedule && !preparingNewFrame)) { stalledFrameTimeoutCount_ = 0; return; }
    if ((stalledFrameTimeoutCount_ + 1U) < kStalledFrameRecoveryThreshold) { ++stalledFrameTimeoutCount_; armStalledFrameTimer(); return; }
    stalledFrameTimeoutCount_ = 0; delayedVrrTimer.stop();
    if (preparingNewFrame) { preparingNewFrame = false; }
    if (pendingFrameCount > 0) { --pendingFrameCount; }
    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) { ++consecutiveErrorCount; }
    pendingReschedule = true;
    scheduleNextRepaint();
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] { vrrCapable = false; vrrEnabled = false; return; }
    vrrCapable = output->capabilities().testFlag(BackendOutput::Capability::Vrr);
    const auto config = KSharedConfig::openConfig(QStringLiteral("kwinrc"));
    const auto group = config->group(QStringLiteral("VRR"));
    const QString policy = group.readEntry(QStringLiteral("Policy"), QStringLiteral("Automatic"));
    if (policy.compare(QLatin1String("Never"), Qt::CaseInsensitive) == 0) { vrrMode = VrrMode::Never; vrrEnabled = false; }
    else if (policy.compare(QLatin1String("Always"), Qt::CaseInsensitive) == 0) { vrrMode = VrrMode::Always; vrrEnabled = vrrCapable; }
    else { vrrMode = VrrMode::Automatic; vrrEnabled = vrrCapable; }
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

    store(QObject::connect(window, &QObject::destroyed, q, [this]() { vrrStateDirty_ = true; }));
    store(QObject::connect(window, &Window::fullScreenChanged, q, [this]() { vrrStateDirty_ = true; }));
    store(QObject::connect(window, &Window::outputChanged, q, [this]() { vrrStateDirty_ = true; }));

    if (surface != nullptr) {
        store(QObject::connect(surface, &SurfaceInterface::presentationModeHintChanged, q,
                               [this]() { vrrStateDirty_ = true; }));

        store(QObject::connect(surface, &SurfaceInterface::committed, q,
                               [this]() { vrrStateDirty_ = true; }));
    }
}

void RenderLoopPrivate::disconnectVrrSignals() noexcept
{
    for (uint8_t i = 0; i < vrrConnectionCount_; ++i) { QObject::disconnect(vrrConnections_[i]); vrrConnections_[i] = {}; }
    vrrConnectionCount_ = 0;
    trackedWindow_ = nullptr; trackedSurfaceItem_ = nullptr; trackedSurface_ = nullptr;
}

void RenderLoopPrivate::invalidateVrrState() noexcept { vrrStateDirty_ = true; }

void RenderLoopPrivate::updateVrrState() noexcept
{
    if (!vrrEnabled || !vrrCapable || output == nullptr) {
        if (trackedWindow_ != nullptr) { disconnectVrrSignals(); }
        if (vrrStateCache_.raw() != 0U) { vrrStateCache_.setRaw(0U); }
        vrrStateDirty_ = false;
        return;
    }
    if (!vrrStateDirty_) {
        if (trackedWindow_ != nullptr && trackedWindow_->surfaceItem() != trackedSurfaceItem_.data()) { vrrStateDirty_ = true; }
        if (!vrrStateDirty_) { return; }
    }
    vrrStateDirty_ = false;
    VrrStateCache::State state{};
    const uint8_t oldRaw = vrrStateCache_.raw();
    Window *active = nullptr;
    bool eligible = false;
    if (Workspace *const ws = workspace()) {
        active = ws->activeWindow();
        if (active != nullptr && active->isFullScreen()) {
            if (LogicalOutput *const logical = ws->findOutput(output)) { eligible = active->isOnOutput(logical); }
        }
    }
    if (!eligible) {
        if (trackedWindow_ != nullptr) { disconnectVrrSignals(); }
        if (oldRaw != state.toRaw()) { vrrStateCache_.setState(state); }
        return;
    }
    connectVrrSignals(active);
    state.isOnOutput = 1U;
    state.isFullScreen = 1U;
    if (SurfaceInterface *const surface = trackedSurface_.data()) {
        state.hint = static_cast<uint8_t>(surface->presentationModeHint()) & 0x3U;
        state.valid = 1U;

        // wp_content_type_v1 readout. The mapping helper returns nullopt only
        // if the surface enum ever grows new variants we don't recognise.
        if (auto ct = mapContentType(surface->contentType()); ct.has_value()) {
            state.contentType = static_cast<uint8_t>(*ct) & 0x3U;
            state.contentTypeValid = 1U;
        }
    }
    if (oldRaw != state.toRaw()) { vrrStateCache_.setState(state); }
}

void RenderLoopPrivate::setVrrCapabilities(const VrrCapabilities &caps) noexcept
{
    vrrCaps_ = caps;
    if (vrrCaps_.minRefreshRate < 0) { vrrCaps_.minRefreshRate = 0; }
    if (vrrCaps_.maxRefreshRate < 0) { vrrCaps_.maxRefreshRate = 0; }
    if (vrrCaps_.maxRefreshRate > 0 && vrrCaps_.minRefreshRate > vrrCaps_.maxRefreshRate) { std::swap(vrrCaps_.minRefreshRate, vrrCaps_.maxRefreshRate); }
    lfcCapable_ = vrrCaps_.supportsLfc;
}

void RenderLoopPrivate::setActiveContentHint(VrrContentHint hint, std::optional<std::chrono::nanoseconds> nominalFrameInterval) noexcept
{
    const VrrContentHint oldHint = activeContentHint_;
    activeContentHint_ = hint;
    if (nominalFrameInterval && nominalFrameInterval->count() > 0) { nominalContentFrameInterval = std::chrono::nanoseconds{std::clamp(nominalFrameInterval->count(), kMinReasonableIntervalNs, kMaxReasonableIntervalNs)}; }
    else { nominalContentFrameInterval = std::chrono::nanoseconds{0}; }
    if (hint == VrrContentHint::Interactive || hint == VrrContentHint::ForceVrr) { interactiveGraceFrames_ = std::max<uint16_t>(interactiveGraceFrames_, kInteractiveGraceFrames); }
    else if (hint == VrrContentHint::Unknown && contentIsInteractive(oldHint)) { interactiveGraceFrames_ = std::max<uint16_t>(interactiveGraceFrames_, kInteractiveTailFrames); }
    else if (contentIsVideoLike(hint)) { interactiveGraceFrames_ = 0; delayedVrrTimer.stop(); }
    if (inhibitCount == 0) { scheduleNextRepaint(); }
}

void RenderLoopPrivate::addPresentFeedback(const PresentFeedback &feedback) noexcept
{
    if (!feedback.valid) { return; }
    std::chrono::nanoseconds ts = feedback.actualPresentationTimestamp;
    if (ts.count() <= 0) { ts = std::chrono::nanoseconds{steadyNowNs()}; }
    PresentSample sample{};
    sample.timestamp = ts;
    sample.refresh = static_cast<uint32_t>(std::clamp<int64_t>(feedback.refreshDuration.count() > 0 ? feedback.refreshDuration.count() : static_cast<int64_t>(cachedVblankIntervalNs), 0LL, 4'000'000'000LL));
    sample.deadlineMissed = feedback.deadlineMissed;
    sample.directScanout = feedback.directScanout;
    sample.valid = true;
    presentHistory_[presentHistoryHead_] = sample;
    presentHistoryHead_ = static_cast<uint8_t>((presentHistoryHead_ + 1U) & (kPresentHistorySize - 1U));
    presentHistoryCount_ += static_cast<uint8_t>(presentHistoryCount_ < kPresentHistorySize ? 1U : 0U);
}

bool RenderLoopPrivate::cadenceMatchesNominal() const noexcept
{
    const int64_t nominalNs = nominalContentFrameInterval.count();
    if (nominalNs <= 0 || cadenceStability_ < kCadenceStableThreshold) { return false; }
    int64_t sum = 0; uint8_t found = 0; int64_t prevTs = 0;
    int64_t minDelta = std::numeric_limits<int64_t>::max(); int64_t maxDelta = 0;
    const int64_t toleranceNs = std::max<int64_t>(nominalNs >> 6, 300'000LL);
    const int64_t doubleTolerance = toleranceNs << 1;
    for (uint8_t offset = 0; offset < kPresentHistorySize; ++offset) {
        const uint8_t idx = static_cast<uint8_t>((presentHistoryHead_ + kPresentHistorySize - 1U - offset) & (kPresentHistorySize - 1U));
        const PresentSample &sample = presentHistory_[idx];
        if (!sample.valid) { continue; }
        const int64_t ts = sample.timestamp.count();
        if (ts > 0) {
            if (prevTs > 0) {
                const int64_t delta = prevTs - ts;
                if (delta >= kMinReasonableIntervalNs && delta <= kMaxReasonableIntervalNs && fastAbs64(delta - nominalNs) <= doubleTolerance) {
                    sum += delta; ++found;
                    if (delta < minDelta) { minDelta = delta; }
                    if (delta > maxDelta) { maxDelta = delta; }
                }
            }
            prevTs = ts;
        }
    }
    if (found == 0) { return nearIntervalNs(lastIntervalNs_, nominalNs, toleranceNs); }
    const bool doOutlierRemoval = (found >= 3);
    const int64_t divisor = doOutlierRemoval ? static_cast<int64_t>(found) - 2 : static_cast<int64_t>(found);
    const int64_t dividend = doOutlierRemoval ? sum - minDelta - maxDelta : sum;
    int64_t avgNs = 0;
    switch (divisor) { case 1: avgNs = dividend; break; case 2: avgNs = dividend >> 1; break; case 3: avgNs = dividend / 3; break; case 4: avgNs = dividend >> 2; break; default: avgNs = dividend / divisor; break; }
    return nearIntervalNs(avgNs, nominalNs, toleranceNs) && found >= (kPresentHistorySize >> 2);
}

bool RenderLoopPrivate::isBelowVrrFloor() const noexcept
{
    uint32_t effectiveRateMhz = filteredPanelRefreshRateMhz_;
    if (effectiveRateMhz == 0) {
        effectiveRateMhz = static_cast<uint32_t>(refreshRate);
    }
    int64_t floormHz = vrrCaps_.minRefreshRate;
    if (floormHz <= 0) {
        floormHz = 48'000;
    }
    return effectiveRateMhz > 0
        && static_cast<uint64_t>(effectiveRateMhz) <= static_cast<uint64_t>(floormHz);
}

void RenderLoopPrivate::recordModeSwitch() noexcept
{
    const auto now = std::chrono::steady_clock::now();
    lastModeSwitch = now; modeDwellCounter_ = 0;
    modeSwitchHistory_[modeSwitchHistoryHead_] = now;
    modeSwitchHistoryHead_ = static_cast<uint8_t>((modeSwitchHistoryHead_ + 1U) & (kModeSwitchHistorySize - 1U));
    modeSwitchHistoryCount_ += static_cast<uint8_t>(modeSwitchHistoryCount_ < kModeSwitchHistorySize ? 1U : 0U);
    const uint8_t required = static_cast<uint8_t>(std::min<size_t>(kOscillationThreshold, kModeSwitchHistorySize));
    if (modeSwitchHistoryCount_ < required) { return; }
    const auto cutoff = now - kOscillationWindow;
    const uint8_t idx = static_cast<uint8_t>((modeSwitchHistoryHead_ + kModeSwitchHistorySize - required) & (kModeSwitchHistorySize - 1U));
    if (modeSwitchHistory_[idx] >= cutoff) [[unlikely]] { vrrOscillationLockout = true; oscillationCooldownCounter_ = kOscillationCooldownFrames; }
}

void RenderLoopPrivate::updatePresentationCadence(int64_t intervalNs) noexcept
{
    if (intervalNs < kMinReasonableIntervalNs || intervalNs > kMaxReasonableIntervalNs) [[unlikely]] { cadenceStability_ = 128; lastIntervalNs_ = 0; return; }
    const int64_t prev = lastIntervalNs_;
    if (prev <= 0) [[unlikely]] { lastIntervalNs_ = intervalNs; return; }
    const int64_t maxStepNs = std::max<int64_t>(prev >> 1, 250'000LL);
    const int64_t filtered = std::clamp(intervalNs, std::max<int64_t>(prev - maxStepNs, kMinReasonableIntervalNs), std::min<int64_t>(prev + maxStepNs, kMaxReasonableIntervalNs));
    lastIntervalNs_ = filtered;
    const int64_t rawDiff = fastAbs64(filtered - prev);
    const int64_t diff = rawDiff > kCadenceNoiseFloorNs ? (rawDiff - kCadenceNoiseFloorNs) : 0LL;
    const int64_t relativeDeviation = std::min<int64_t>((diff << 8) / std::max<int64_t>(prev, 1LL), 256LL);
    const int32_t sample = static_cast<int32_t>(256LL - relativeDeviation);
    const int32_t current = static_cast<int32_t>(cadenceStability_);
    const int32_t isGreater = static_cast<int32_t>(sample > current);
    const int32_t shift = 1 + (isGreater << 2);
    const int32_t weight = (1 << shift) - 1;
    cadenceStability_ = static_cast<int16_t>((current * weight + sample) >> shift);
}

// ─────────────────────────────────────────────────────────────────────────────
// selectPresentationMode — wp_content_type_v1 first, hardware truth second
//
// THE GODLIKE INSIGHT (finally correct):
// `wp_content_type_v1` is a stable Wayland protocol that lets the app tell the
// compositor exactly what kind of content it's showing.  Chromium/Firefox set
// `video` on fullscreen video surfaces; games typically set `game`.  This is
// the authoritative signal we've been trying to reverse-engineer all along.
//
// Decision priority:
//   1. capability / policy / fullscreen gates
//   2. explicit content-hint overrides           (ForceVsync / ForceVrr)
//   3. explicit presentation-mode hint           (VSync / Async)
//   4. *** wp_content_type_v1 == Video ***       → VSync   (NEW, top-priority)
//   5. *** wp_content_type_v1 == Game  ***       → VRR     (NEW, top-priority)
//   6. active content hint                       (Video/Film/Interactive)
//   7. interactive grace
//   8. oscillation guard
//   9. fallback: panel refresh ≤ floor (stable) → VSync   (un-hinted apps)
//  10. default                                   → VRR
// ─────────────────────────────────────────────────────────────────────────────

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

    // Tier 2: explicit content-hint overrides (set by external APIs)
    if (activeContentHint_ == VrrContentHint::ForceVsync) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }
    if (activeContentHint_ == VrrContentHint::ForceVrr) [[likely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return adaptive;
    }

    // Tier 3: explicit surface presentation-mode hint (VSync/Async)
    if (isExplicitVsyncHint(state.hint, validHint)) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }
    if (isExplicitAsyncHint(state.hint, validHint)) {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::AdaptiveAsync;
    }

    // Tier 4: wp_content_type_v1 == Video  → VSync
    // The browser/video-player has authoritatively told us this is video.
    // No inference. No jitter sensitivity. Mouse movement is irrelevant.
    if (state.isVideoContent()) {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }

    // Tier 5: wp_content_type_v1 == Game → VRR
    // The game has authoritatively asked for VRR/tearing.
    if (state.isGameContent()) {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return adaptive;
    }

    // Tier 6: active content hint (Video/Film/Interactive)
    if (activeContentHint_ == VrrContentHint::Video || activeContentHint_ == VrrContentHint::Film) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::ExplicitHint;
        return PresentationMode::VSync;
    }

    // Tier 7: interactive / grace
    if (activeContentHint_ == VrrContentHint::Interactive || interactiveGraceFrames_ > 0U) [[likely]] {
        lastDecisionReason_ = VrrDecisionReason::InteractiveGrace;
        return adaptive;
    }

    // Tier 8: oscillation guard
    if (vrrOscillationLockout && oscillationCooldownCounter_ > 0U) [[unlikely]] {
        lastDecisionReason_ = VrrDecisionReason::OscillationGuard;
        return PresentationMode::VSync;
    }

    // Tier 9: fallback for unhinted apps — panel-refresh-rate floor protection.
    // This catches old apps that don't set wp_content_type_v1.  The panel rate
    // is hardware ground truth from wp_presentation_feedback, so it's immune
    // to mouse jitter.  Stability counter prevents false triggers.
    if (isBelowVrrFloor() && panelRefreshStableCount_ >= kPanelRefreshFloorStableFrames) {
        lastDecisionReason_ = VrrDecisionReason::BelowVrrFloor;
        return PresentationMode::VSync;
    }

    // Tier 10: ambiguous, keep current
    const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
    if (cadenceStability_ < kCadenceUnstableThreshold && lastIntervalNs_ <= (vblankNs * 4)) {
        lastDecisionReason_ = VrrDecisionReason::AmbiguousKeepCurrent;
        return presentationMode;
    }

    // Tier 11: default adaptive
    lastDecisionReason_ = VrrDecisionReason::AmbiguousKeepCurrent;
    return adaptive;
}

bool RenderLoopPrivate::shouldSwitchMode(PresentationMode target) noexcept
{
    if (oscillationCooldownCounter_ > 0U) { --oscillationCooldownCounter_; if (oscillationCooldownCounter_ == 0U) { vrrOscillationLockout = false; } }
    if (target == presentationMode) { pendingModeCounter_ = 0; pendingTargetMode_ = presentationMode; lastStableMode = presentationMode; return false; }

    const bool exitingToVsync = (target == PresentationMode::VSync);
    const VrrStateCache::State state = vrrStateCache_.getState();
    const bool fullscreenOnOutput = (state.isOnOutput != 0U) && (state.isFullScreen != 0U);
    const bool validHint = state.valid != 0U;
    const bool explicitVsync = isExplicitVsyncHint(state.hint, validHint);
    const bool explicitAsync = isExplicitAsyncHint(state.hint, validHint);
    const bool explicitVideoCT = state.isVideoContent();
    const bool explicitGameCT  = state.isGameContent();
    const bool floorTriggered  = (lastDecisionReason_ == VrrDecisionReason::BelowVrrFloor);

    if (exitingToVsync && (!vrrEnabled || !vrrCapable || vrrMode == VrrMode::Never || !fullscreenOnOutput)) { pendingModeCounter_ = 0; pendingTargetMode_ = target; lastStableMode = target; return true; }
    if (modeDwellCounter_ < kMinModeDwellFrames) { return false; }

    if (pendingTargetMode_ == target) { if (pendingModeCounter_ < std::numeric_limits<uint16_t>::max()) { ++pendingModeCounter_; } }
    else { pendingTargetMode_ = target; pendingModeCounter_ = 1; }

    uint16_t threshold = exitingToVsync ? kVrrToVsyncStabilityFrames : kVsyncToVrrStabilityFrames;

    if (exitingToVsync) {
        if (activeContentHint_ == VrrContentHint::ForceVsync) { threshold = 1U; }
        else if (explicitVideoCT) { threshold = 1U; }                                 // wp_content_type=Video → instant
        else if (activeContentHint_ == VrrContentHint::Video || activeContentHint_ == VrrContentHint::Film) { threshold = cadenceMatchesNominal() ? 2U : 3U; }
        else if (explicitVsync) { threshold = 2U; }
        else if (floorTriggered) { threshold = 1U; }                                  // panel-rate fallback already stable
        else { return false; }
    } else {
        if (activeContentHint_ == VrrContentHint::ForceVrr) { threshold = 1U; }
        else if (explicitGameCT) { threshold = 1U; }                                  // wp_content_type=Game → instant
        else if (activeContentHint_ == VrrContentHint::Interactive || interactiveGraceFrames_ > 0U) { threshold = 1U; }
        else if (explicitAsync) { threshold = 1U; }
        else if (q->activeWindowControlsVrrRefreshRate()) { threshold = 1U; }
        else { threshold = 2U; }
    }

    if (pendingModeCounter_ >= threshold) { pendingModeCounter_ = 0; lastStableMode = target; return true; }
    return false;
}

bool RenderLoopPrivate::detectVrrOscillation() noexcept { return vrrOscillationLockout; }

void RenderLoopPrivate::updateFramePrediction(std::chrono::nanoseconds measured) noexcept
{
    const int64_t m = measured.count();
    const int64_t cur = framePrediction.count();
    if (cur <= 0) { framePrediction = measured; return; }
    const int64_t vblank = static_cast<int64_t>(cachedVblankIntervalNs);
    const int shift = (vblank > 0 && cur > (vblank >> 1)) ? 3 : 4;
    const int64_t diff = m - cur;
    const int64_t isPos = diff > 0 ? 1 : 0;
    const int actualShift = isPos ? std::min(shift, 2) : shift;
    const int64_t updated = cur + ((diff + isPos) >> actualShift);
    const int64_t lo = std::max<int64_t>(vblank >> 5, int64_t{100'000});
    const int64_t hi = vblank > 0 ? (vblank * 2) : int64_t{33'000'000};
    framePrediction = std::chrono::nanoseconds{std::clamp(updated, lo, hi)};
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(q, [this]() { scheduleNextRepaint(); }, Qt::QueuedConnection); return; }
    if (kwinApp()->isTerminating() || compositeTimer.isActive()) { return; }
    if (preparingNewFrame) { pendingReschedule = true; armStalledFrameTimer(); return; }
    scheduleRepaint();
}

void RenderLoopPrivate::scheduleRepaint()
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(q, [this]() { scheduleRepaint(); }, Qt::QueuedConnection); return; }
    pendingReschedule = false;

    if (directScanoutActive_) { return; }

    const uint64_t vblankNs = cachedVblankIntervalNs;
    if (vblankNs == 0) [[unlikely]] { return; }
    const int64_t nowNs = steadyNowNs();
    int64_t lastPresNs = lastPresentationTimestamp.count();
    if (lastPresNs <= 0 || lastPresNs > nowNs) [[unlikely]] { lastPresNs = nowNs - static_cast<int64_t>(vblankNs); lastPresentationTimestamp = std::chrono::nanoseconds{lastPresNs}; }
    int64_t predNs = framePrediction.count();
    if (predNs <= 0) { predNs = renderJournal.result().count(); if (predNs <= 0) { predNs = static_cast<int64_t>(vblankNs >> 1); } }
    const int64_t stabilityBonus = (static_cast<int64_t>(cadenceStability_) * kRenderSlackNs) >> 9;
    const int64_t effectiveSlack = std::max<int64_t>(kRenderSlackNs - stabilityBonus, 0LL);
    const int64_t compositeNs = std::clamp(predNs + safetyMargin.count() + effectiveSlack, kMinCompositeNs, kMaxCompositeNs);
    updateVrrState();
    const PresentationMode targetMode = selectPresentationMode();
    const VrrDecisionReason decisionReason = lastDecisionReason_;
    if (targetMode != presentationMode && shouldSwitchMode(targetMode)) {
        presentationMode = targetMode;
        recordModeSwitch();
        Q_EMIT q->presentationModeChanged(q, presentationMode, decisionReason);
    }
    if (modeDwellCounter_ < std::numeric_limits<uint16_t>::max()) { ++modeDwellCounter_; }
    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    if (pendingFrameCount >= maxPending) {
        if (!vrrActive && delayedVrrTimer.isActive()) { delayedVrrTimer.stop(); }
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
        int64_t targetVblank = static_cast<int64_t>(ceilDivU64Reciprocal(static_cast<uint64_t>(nsFromLastPres), vblankNs, vblankIntervalReciprocal64, reciprocalShift64));
        if (wasTripleBuffering && targetVblank <= 1) { targetVblank = 2; }
        nextPresNs = lastPresNs + (targetVblank * vblankI64);
        if (compositeNs > tripleBufferEnterThresholdNs) { if (tripleBufferHysteresisCounter < kTripleBufferHysteresisLimit) { ++tripleBufferHysteresisCounter; } else { wasTripleBuffering = true; } }
        else if (compositeNs < tripleBufferExitThresholdNs) { tripleBufferHysteresisCounter = 0; wasTripleBuffering = false; }
    } else { wasTripleBuffering = false; tripleBufferHysteresisCounter = 0; }
    const int64_t minPresNs = nowNs + kMinCompositeNs;
    if (nextPresNs < minPresNs) { nextPresNs = minPresNs; }
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};
    const int64_t nextRenderNs = nextPresNs - compositeNs;
    const int64_t delayNs = std::max<int64_t>(nextRenderNs - nowNs, 0LL);
    const int timerMs = delayNs < kNsPerMs ? 0 : static_cast<int>(std::clamp<int64_t>(delayNs / kNsPerMs, 1LL, static_cast<int64_t>(kMaxTimerDelayMs)));
    if (compositeTimer.isActive()) {
        const int64_t scheduledNs = scheduledRenderTimestamp.count();
        const int64_t absDiffNs = fastAbs64(nextRenderNs - scheduledNs);
        const bool earlier = nextRenderNs < scheduledNs;
        const int64_t jitterFilterNs = dynamicRescheduleJitterNs(vblankI64);
        if (!earlier && absDiffNs < jitterFilterNs) { return; }
        if (scheduledTimerMs >= 0 && timerMs == scheduledTimerMs) { const int64_t sameMsGuardNs = dynamicSameMsGuardNs(vblankI64); if (!earlier && absDiffNs < sameMsGuardNs) { return; } }
    }
    scheduledRenderTimestamp = std::chrono::nanoseconds{nextRenderNs};
    compositeTimer.start(timerMs, Qt::PreciseTimer, q);
    scheduledTimerMs = static_cast<int16_t>(timerMs);
}

void RenderLoopPrivate::delayScheduleRepaint() noexcept { pendingReschedule = true; }

void RenderLoopPrivate::notifyFrameDropped()
{
    if (q->thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(q, [this]() { notifyFrameDropped(); }, Qt::QueuedConnection); return; }
    preparingNewFrame = false;
    if (pendingFrameCount > 0) { --pendingFrameCount; }
    if (consecutiveErrorCount < std::numeric_limits<uint8_t>::max()) { ++consecutiveErrorCount; }
    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    if (!preparingNewFrame && pendingFrameCount < maxPending) { disarmStalledFrameTimer(); }
    else if (pendingReschedule) { armStalledFrameTimer(); }
    if (inhibitCount == 0 && pendingReschedule) {
        if (consecutiveErrorCount > kMaxConsecutiveErrors) {
            int delay = 1 << std::min<int>(consecutiveErrorCount - kMaxConsecutiveErrors, 6);
            delay = std::min(delay, adaptiveErrorBackoffMaxMs(static_cast<int64_t>(cachedVblankIntervalNs)));
            compositeTimer.start(delay, Qt::PreciseTimer, q);
            scheduledTimerMs = static_cast<int16_t>(delay);
            scheduledRenderTimestamp = std::chrono::nanoseconds{steadyNowNs() + static_cast<int64_t>(delay) * kNsPerMs};
        } else { scheduleNextRepaint(); }
    }
}

namespace {
void writeDebug(RenderLoopPrivate *d, std::optional<RenderTimeSpan> rt, std::chrono::nanoseconds targetFlip,
    std::chrono::nanoseconds refreshDur, std::chrono::nanoseconds predRender, std::chrono::nanoseconds ts, PresentationMode mode)
{
    if (!s_debugEnabled) [[likely]] { return; }
    if (!d->m_debugOutput) {
        if (!d->output) { return; }
        std::string name = d->output->name().toStdString();
        for (char &c : name) { c = std::isalnum(static_cast<unsigned char>(c)) ? c : '_'; }
        d->m_debugOutput.emplace("kwin_perf_" + name + ".csv", std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) { *d->m_debugOutput << "target,flip,start,end,margin,dur,vrr,pred,cadence\n"; }
    }
    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        const auto times = rt.value_or(RenderTimeSpan{});
        *d->m_debugOutput << targetFlip.count() << ',' << ts.count() << ',' << times.start.time_since_epoch().count() << ','
                          << times.end.time_since_epoch().count() << ',' << d->safetyMargin.count() << ',' << refreshDur.count() << ','
                          << isVrrMode(mode) << ',' << predRender.count() << ',' << d->cadenceStability_ << '\n';
    }
}
} // namespace

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
{
    const bool directScanout = frame != nullptr && !renderTime.has_value();

    auto buildFeedback = [this, timestamp, frame, mode, directScanout]() -> std::optional<PresentFeedback> {
        if (!frame) { return std::nullopt; }
        PresentFeedback feedback{};
        feedback.actualPresentationTimestamp = timestamp;
        feedback.targetPresentationTimestamp = std::chrono::duration_cast<std::chrono::nanoseconds>(frame->targetPageflipTime().time_since_epoch());
        feedback.refreshDuration = frame->refreshDuration();
        const int64_t vblankNs = static_cast<int64_t>(cachedVblankIntervalNs);
        int64_t graceNs = vblankNs / 8;
        if (graceNs < 700'000) { graceNs = 700'000; } else if (graceNs > 2'500'000) { graceNs = 2'500'000; }
        if (!isVrrMode(mode) && feedback.targetPresentationTimestamp.count() > 0) { feedback.deadlineMissed = timestamp > (feedback.targetPresentationTimestamp + std::chrono::nanoseconds{graceNs}); }
        else { feedback.deadlineMissed = false; }
        feedback.directScanout = directScanout;
        feedback.valid = true;
        return feedback;
    };

    const std::optional<PresentFeedback> initialFeedback = buildFeedback();

    if (q->thread() != QThread::currentThread()) [[unlikely]] {
        QMetaObject::invokeMethod(q, [this, timestamp, renderTime, mode, initialFeedback]() {
            notifyFrameCompleted(timestamp, renderTime, mode, nullptr);
            if (initialFeedback.has_value()) { addPresentFeedback(*initialFeedback); }
        }, Qt::QueuedConnection);
        return;
    }

    preparingNewFrame = false;
    consecutiveErrorCount = 0;

    if (s_debugEnabled && frame) [[unlikely]] {
        writeDebug(this, renderTime, std::chrono::duration_cast<std::chrono::nanoseconds>(frame->targetPageflipTime().time_since_epoch()),
            frame->refreshDuration(), frame->predictedRenderTime(), timestamp, mode);
    }

    if (pendingFrameCount > 0) { --pendingFrameCount; }

    const int64_t prevPresentationNs = lastPresentationTimestamp.count();
    const int64_t nowNs = steadyNowNs();
    int64_t tsNs = timestamp.count();
    if (tsNs > nowNs) [[unlikely]] { tsNs = nowNs; }
    if (tsNs < prevPresentationNs) [[unlikely]] { tsNs = prevPresentationNs; }

    lastPresentationTimestamp = std::chrono::nanoseconds{tsNs};
    updatePresentationCadence(tsNs - prevPresentationNs);

    // Panel-rate ground truth from wp_presentation_feedback.
    // IIR-filter (weight = 7/8) absorbs single-frame hiccups but tracks real
    // mode changes within ~6 frames (100 ms at 60 Hz).
    if (frame && frame->refreshDuration().count() > 0) {
        const int64_t refreshDurNs = frame->refreshDuration().count();
        currentPanelRefreshRateMhz_ = static_cast<uint32_t>(1'000'000'000'000ULL / static_cast<uint64_t>(refreshDurNs));
        if (filteredPanelRefreshRateMhz_ == 0) { filteredPanelRefreshRateMhz_ = currentPanelRefreshRateMhz_; }
        else {
            filteredPanelRefreshRateMhz_ = (7U * filteredPanelRefreshRateMhz_ + currentPanelRefreshRateMhz_ + 4U) / 8U;
        }
    }

    directScanoutActive_ = directScanout;

    if (renderTime && !directScanout) {
        const auto dur = renderTime->end - renderTime->start;
        renderJournal.add(dur, timestamp);
        updateFramePrediction(dur);
    }

    if (interactiveGraceFrames_ > 0U) { --interactiveGraceFrames_; }

    if (isVrrMode(mode)) { if (starvationRecoveryCounter < kStarvationRecoveryFrames) { ++starvationRecoveryCounter; } }
    else { starvationRecoveryCounter = 0; }

    if (initialFeedback.has_value()) { addPresentFeedback(*initialFeedback); }

    // Stability counter for the unhinted-fallback floor check.
    {
        uint32_t effectiveRateMhz = filteredPanelRefreshRateMhz_;
        if (effectiveRateMhz == 0) { effectiveRateMhz = static_cast<uint32_t>(refreshRate); }
        int64_t floormHz = vrrCaps_.minRefreshRate;
        if (floormHz <= 0) { floormHz = 48'000; }
        const bool atOrBelowFloor = effectiveRateMhz > 0 && effectiveRateMhz <= static_cast<uint32_t>(floormHz);
        if (atOrBelowFloor) {
            if (panelRefreshStableCount_ < 255U) { ++panelRefreshStableCount_; }
        } else {
            panelRefreshStableCount_ = 0;
        }
    }

    const bool vrrActive = isVrrMode(presentationMode);
    const int maxPending = effectiveMaxPendingFrames(this, vrrActive);
    if (!preparingNewFrame && pendingFrameCount < maxPending) { disarmStalledFrameTimer(); }
    else if (pendingReschedule) { armStalledFrameTimer(); }

    if (compositeTimer.isActive()) {
        compositeTimer.stop();
        scheduledTimerMs = -1;
        scheduleRepaint();
    } else if (inhibitCount == 0 && (pendingReschedule || pendingFrameCount == 0)) {
        pendingReschedule = false;
        scheduleNextRepaint();
    }

    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp, int64_t nowNs)
{
    int64_t ts = timestamp.count();
    if (ts > nowNs) [[unlikely]] { ts = nowNs; }
    const int64_t prev = lastPresentationTimestamp.count();
    if (ts < prev) [[unlikely]] { ts = prev; }
    lastPresentationTimestamp = std::chrono::nanoseconds{ts};
}

void RenderLoopPrivate::dispatch() { Q_EMIT q->frameRequested(q); }

void RenderLoop::timerEvent(QTimerEvent *event)
{
    const int id = event->timerId();
    if (id == d->compositeTimer.timerId()) { d->compositeTimer.stop(); d->scheduledTimerMs = -1; d->dispatch(); }
    else if (id == d->delayedVrrTimer.timerId()) [[unlikely]] { d->delayedVrrTimer.stop(); scheduleRepaint(nullptr, nullptr); }
    else if (id == d->stalledFrameTimer.timerId()) [[unlikely]] { d->handleStalledFrameTimeout(); }
    else { QObject::timerEvent(event); }
}

RenderLoop::RenderLoop(BackendOutput *output) : d(std::make_unique<RenderLoopPrivate>(this, output)) {}
RenderLoop::~RenderLoop() = default;

void RenderLoop::inhibit() { if (++d->inhibitCount == 1) { d->compositeTimer.stop(); d->scheduledTimerMs = -1; d->delayedVrrTimer.stop(); d->disarmStalledFrameTimer(); } }
void RenderLoop::uninhibit() { Q_ASSERT(d->inhibitCount > 0); if (--d->inhibitCount == 0) { d->scheduleNextRepaint(); } }

void RenderLoop::prepareNewFrame() { if (d->preparingNewFrame) { return; } ++d->pendingFrameCount; d->preparingNewFrame = true; d->armStalledFrameTimer(); }

void RenderLoop::newFramePrepared() {
    d->preparingNewFrame = false;
    const bool vrrActive = isVrrMode(d->presentationMode);
    const int maxPending = effectiveMaxPendingFrames(d.get(), vrrActive);
    if (!d->preparingNewFrame && d->pendingFrameCount < maxPending) { d->disarmStalledFrameTimer(); }
    else if (d->pendingReschedule) { d->armStalledFrameTimer(); }
    if (d->inhibitCount == 0 && d->pendingReschedule) { d->scheduleNextRepaint(); }
}

int RenderLoop::refreshRate() const { return d->refreshRate; }

void RenderLoop::setRefreshRate(int rate) {
    rate = std::clamp(rate, 1000, 1000000);
    if (d->refreshRate == rate) { return; }
    d->refreshRate = rate;
    d->cachedVblankIntervalNs = 1'000'000'000'000ULL / static_cast<uint64_t>(rate);
    if (d->vrrCaps_.maxRefreshRate <= 0) { d->vrrCaps_.maxRefreshRate = rate; }
    d->updateReciprocal();
    Q_EMIT refreshRateChanged();
    if (d->inhibitCount == 0) { d->compositeTimer.stop(); d->scheduledTimerMs = -1; d->scheduleNextRepaint(); }
}

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds margin) { d->safetyMargin = margin.count() > 0 ? margin : std::chrono::nanoseconds{0}; }

void RenderLoop::setVrrCapabilities(const VrrCapabilities &caps) {
    if (thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(this, [this, caps]() { setVrrCapabilities(caps); }, Qt::QueuedConnection); return; }
    d->setVrrCapabilities(caps);
    if (d->inhibitCount == 0) { d->scheduleNextRepaint(); }
}

void RenderLoop::setActiveContentHint(VrrContentHint hint, std::optional<std::chrono::nanoseconds> nominalFrameInterval) {
    if (thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(this, [this, hint, nominalFrameInterval]() { setActiveContentHint(hint, nominalFrameInterval); }, Qt::QueuedConnection); return; }
    d->setActiveContentHint(hint, nominalFrameInterval);
}

void RenderLoop::notifyPresentFeedback(const PresentFeedback &feedback) {
    if (thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(this, [this, feedback]() { notifyPresentFeedback(feedback); }, Qt::QueuedConnection); return; }
    d->addPresentFeedback(feedback);
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *layer) {
    (void)layer;
    if (thread() != QThread::currentThread()) [[unlikely]] { QMetaObject::invokeMethod(this, [this, item, layer]() { scheduleRepaint(item, layer); }, Qt::QueuedConnection); return; }
    if (d->inhibitCount != 0) { if (d->delayedVrrTimer.isActive()) { d->delayedVrrTimer.stop(); } d->pendingReschedule = true; return; }
    if (d->preparingNewFrame) { d->pendingReschedule = true; d->armStalledFrameTimer(); return; }
    if (d->vrrStateDirty_) { d->updateVrrState(); }
    const bool vrrActive = isVrrMode(d->presentationMode);
    const int maxPending = effectiveMaxPendingFrames(d.get(), vrrActive);
    if (d->pendingFrameCount >= maxPending) { if (!vrrActive && d->delayedVrrTimer.isActive()) { d->delayedVrrTimer.stop(); } d->pendingReschedule = true; d->armStalledFrameTimer(); return; }
    const bool maybeDelayControl = vrrActive && item != nullptr && d->activeContentHint_ == VrrContentHint::Unknown && d->interactiveGraceFrames_ == 0U && d->cadenceStability_ < kCadenceStableThreshold && d->pendingFrameCount > 0;
    if (maybeDelayControl) {
        const VrrStateCache::State state = d->vrrStateCache_.getState();
        if (state.isOnOutput != 0U) {
            Window *const tracked = d->trackedWindow_.data();
            if (tracked != nullptr) { SurfaceItem *const surface = tracked->surfaceItem();
                if (surface != nullptr && item != surface && !surface->isAncestorOf(item)) { const auto estimate = surface->recursiveFrameTimeEstimation();
                    if (durationEstimateAtMost(estimate, kVrrControlThreshold)) { if (!d->delayedVrrTimer.isActive()) { d->delayedVrrTimer.start(d->vrrControlDelayMs, Qt::PreciseTimer, this); } return; } } } } }
    if (d->delayedVrrTimer.isActive()) { d->delayedVrrTimer.stop(); }
    d->disarmStalledFrameTimer();
    d->scheduleNextRepaint();
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const {
    if (d->output == nullptr) { return false; }
    switch (d->activeContentHint_) { case VrrContentHint::ForceVsync: case VrrContentHint::Video: case VrrContentHint::Film: return false; case VrrContentHint::ForceVrr: case VrrContentHint::Interactive: return true; case VrrContentHint::Unknown: break; }
    Workspace *const ws = workspace(); if (!ws) { return false; }
    Window *const active = ws->activeWindow(); if (!active) { return false; }
    LogicalOutput *const logical = ws->findOutput(d->output); if (!logical || !active->frameGeometry().intersects(logical->geometryF())) { return false; }
    SurfaceItem *const surface = active->surfaceItem(); if (!surface) { return false; }
    constexpr auto k30HzFrame = std::chrono::nanoseconds{1'000'000'000LL / 30LL};
    return durationEstimateAtMost(surface->recursiveFrameTimeEstimation(), k30HzFrame);
}

std::chrono::nanoseconds RenderLoop::lastPresentationTimestamp() const { return d->lastPresentationTimestamp; }
std::chrono::nanoseconds RenderLoop::nextPresentationTimestamp() const { return d->nextPresentationTimestamp; }

void RenderLoop::setPresentationMode(PresentationMode mode) {
    if (d->presentationMode == mode) { return; }
    d->presentationMode = mode;
    d->lastStableMode = mode;
    Q_EMIT presentationModeChanged(this, mode, VrrDecisionReason::UserPolicy);
}

void RenderLoop::setMaxPendingFrameCount(uint32_t count) { d->maxPendingFrameCount = static_cast<int>(std::clamp(count, 1U, 3U)); }
std::chrono::nanoseconds RenderLoop::predictedRenderTime() const { return d->renderJournal.result(); }

} // namespace KWin

#include "moc_renderloop.cpp"
