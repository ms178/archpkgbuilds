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

#include <algorithm>
#include <chrono>
#include <climits>
#include <cmath>
#include <filesystem>
#include <limits>
#include <string>
#include <bit>
#include <type_traits>
#include <cstdlib>

#if defined(__x86_64__) || defined(_M_X64)
#include <immintrin.h>
#endif

using namespace std::chrono_literals;

namespace
{
constexpr qint64 kNanosecondsPerMillisecond = 1'000'000;
constexpr qint64 kPredictionSlackNs = 1'000'000;
constexpr qint64 kEarlyStartSlackNs = 1'000;
constexpr qint64 kMaxFutureLeadNs = static_cast<qint64>(1) << 40;
constexpr uint64_t kPageflipDriftThreshold = 100;
constexpr uint64_t kIntervalsClamp = 1'000'000;
constexpr uint8_t kVrrVsyncStabilityThreshold = 2;
constexpr uint8_t kVrrAdaptiveStabilityThreshold = 3;
constexpr auto kVrrControlThreshold = 33ms;
constexpr int kMaxTimerDelayMs = std::numeric_limits<std::int16_t>::max();
} // namespace

namespace KWin
{

static const bool s_printDebugInfo = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop) noexcept
{
    Q_ASSERT(loop);
    return loop ? loop->d.get() : nullptr;
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, Output *output)
    : cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)
    , vblankIntervalReciprocal(0)
    , vblankIntervalReciprocal64(0)
    , reciprocalShift(0)
    , reciprocalShift64(0)
    , q(q)
    , output(output)
{
    updateReciprocal();
    initializeVrrCapabilities();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    if (cachedVblankIntervalNs == 0) {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        return;
    }

    if (cachedVblankIntervalNs < (1ull << 32)) {
        const uint64_t shift = std::bit_width(cachedVblankIntervalNs) + 31;
        const uint64_t numerator = (shift < 64) ? (1ull << shift) : 0;
        vblankIntervalReciprocal = numerator ? ((numerator + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs) : 0;
        reciprocalShift = static_cast<uint8_t>(shift);

        constexpr uint64_t shift64 = 63;
        vblankIntervalReciprocal64 = ((1ull << shift64) + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
        reciprocalShift64 = static_cast<uint8_t>(shift64);
    } else {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
    }
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) {
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

void RenderLoopPrivate::updateVrrContext()
{
    vrrCtxGeneration++;
    vrrCtx.generation = vrrCtxGeneration;
    vrrCtx.cachedActiveWindow = nullptr;
    vrrCtx.cachedSurfaceItem = nullptr;
    vrrCtx.cachedHint = PresentationModeHint::VSync;
    vrrCtx.cachedContentType = ContentType::None;
    vrrCtx.cachedIsFullScreen = false;
    vrrCtx.cachedIsOnOutput = false;

    if (!vrrEnabled || !vrrCapable) {
        return;
    }

    Workspace *const ws = workspace();
    if (!ws || !output) {
        return;
    }

    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow) {
        return;
    }

    vrrCtx.cachedActiveWindow = activeWindow;
    vrrCtx.cachedIsOnOutput = activeWindow->isOnOutput(output);

    if (!vrrCtx.cachedIsOnOutput) {
        return;
    }

    vrrCtx.cachedIsFullScreen = activeWindow->isFullScreen();

    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
    if (!surfaceItem) {
        return;
    }

    vrrCtx.cachedSurfaceItem = surfaceItem;
    vrrCtx.cachedContentType = surfaceItem->contentType();

    if (auto *const waylandItem = qobject_cast<SurfaceItemWayland *>(surfaceItem)) {
        if (auto *const surface = waylandItem->surface()) {
            vrrCtx.cachedHint = surface->presentationModeHint();
        }
    }
}

PresentationMode RenderLoopPrivate::selectPresentationModeFromContext() const noexcept
{
    if (!vrrEnabled || !vrrCapable) {
        return PresentationMode::VSync;
    }

    if (vrrMode != VrrMode::Always) {
        if (!vrrCtx.cachedActiveWindow || !vrrCtx.cachedIsOnOutput || !vrrCtx.cachedIsFullScreen) {
            return PresentationMode::VSync;
        }

        if (vrrCtx.cachedHint == PresentationModeHint::VSync) {
            return PresentationMode::VSync;
        }
    }

    if (vrrCtx.cachedHint == PresentationModeHint::Async) {
        return PresentationMode::AdaptiveAsync;
    }

    return PresentationMode::AdaptiveSync;
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (Q_UNLIKELY(kwinApp()->isTerminating()) || compositeTimer.isActive() || preparingNewFrame) {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((always_inline, hot))
#endif
static inline uint64_t fastDivideByVblank(uint64_t numerator, uint64_t vblankNs,
                                          uint64_t reciprocal64, uint8_t shift64,
                                          uint64_t reciprocalFallback, uint8_t shiftFallback)
{
    if (reciprocal64 > 0 && numerator < (1ull << 32)) [[likely]] {
        return (numerator * reciprocal64) >> shift64;
    }

#if defined(__SIZEOF_INT128__)
    if (reciprocalFallback > 0) [[likely]] {
        return static_cast<uint64_t>((static_cast<__uint128_t>(numerator) * reciprocalFallback) >> shiftFallback);
    }
#endif

    return (vblankNs > 0) ? (numerator / vblankNs) : 0;
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((hot, target("avx2")))
#endif
void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    pendingReschedule = false;

    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) {
        return;
    }

    const int64_t currentTimeNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresentationNs = lastPresentationTimestamp.count();
    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyMarginNs = safetyMargin.count();

    const int64_t vblankIntervalSigned = static_cast<int64_t>(vblankIntervalNs);
    const int64_t maxCompositingNs = vblankIntervalSigned * 2;
    const int64_t baseCompositeNs = predictedRenderNs + safetyMarginNs + kPredictionSlackNs;
    int64_t expectedCompositingNs = std::clamp<int64_t>(baseCompositeNs, 0, maxCompositingNs);

    const PresentationMode targetMode = selectPresentationModeFromContext();
    if (targetMode != presentationMode) {
        const uint8_t threshold = (targetMode == PresentationMode::VSync) ? kVrrVsyncStabilityThreshold : kVrrAdaptiveStabilityThreshold;
        if (vrrStabilityCounter >= threshold) {
            presentationMode = targetMode;
            vrrStabilityCounter = 0;
            qCDebug(KWIN_CORE) << "Presentation mode changed to" << static_cast<int>(targetMode);
        } else {
            ++vrrStabilityCounter;
        }
    } else {
        vrrStabilityCounter = 0;
    }

    int64_t nextPresentationNs = lastPresentationNs + static_cast<int64_t>(vblankIntervalNs);

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = currentTimeNs - lastPresentationNs;
        uint64_t pageflipsSince = 0;

        if (sinceLastNs > 0) [[likely]] {
            const uint64_t sinceLastNsU = static_cast<uint64_t>(sinceLastNs);
            pageflipsSince = fastDivideByVblank(sinceLastNsU, vblankIntervalNs,
                                                vblankIntervalReciprocal64, reciprocalShift64,
                                                vblankIntervalReciprocal, reciprocalShift);
        }

        if (pageflipsSince > kPageflipDriftThreshold) [[unlikely]] {
            const int64_t earlyStart = vblankIntervalSigned - kEarlyStartSlackNs;
            expectedCompositingNs = std::max(expectedCompositingNs, earlyStart);
        }

        const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresentationNs;
        uint64_t pageflipsToTarget = 0;
        if (toTargetNs > 0) [[likely]] {
            const uint64_t toTargetNsU = static_cast<uint64_t>(toTargetNs);
            pageflipsToTarget = fastDivideByVblank(toTargetNsU, vblankIntervalNs,
                                                   vblankIntervalReciprocal64, reciprocalShift64,
                                                   vblankIntervalReciprocal, reciprocalShift);
        }

        uint64_t pageflipsInAdvance = 1;
        if (expectedCompositingNs > static_cast<int64_t>(vblankIntervalNs)) {
            const uint64_t required = (static_cast<uint64_t>(expectedCompositingNs) + vblankIntervalNs - 1) / vblankIntervalNs;
            pageflipsInAdvance = std::clamp<uint64_t>(required, 1, static_cast<uint64_t>(maxPendingFrameCount));
        }

        const bool highLoad = (expectedCompositingNs > vblankIntervalSigned * 85 / 100);
        if (highLoad) {
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
            pageflipsInAdvance = std::max<uint64_t>(pageflipsInAdvance, 2);
        } else if (wasTripleBuffering) {
            if (expectedCompositingNs < vblankIntervalSigned * 65 / 100) {
                ++doubleBufferingCounter;
            } else {
                doubleBufferingCounter = 0;
            }

            if (doubleBufferingCounter >= 4) {
                wasTripleBuffering = false;
            } else {
                pageflipsInAdvance = std::max<uint64_t>(pageflipsInAdvance, 2);
            }
        }

        if (compositeTimer.isActive()) {
            if (nextPresentationTimestamp.count() > 0) {
                const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresentationNs;
                const uint64_t intervals = (deltaNs > 0)
                    ? std::min<uint64_t>((static_cast<uint64_t>(deltaNs) + vblankIntervalNs / 2) / vblankIntervalNs, kIntervalsClamp)
                    : 1;
                nextPresentationNs = lastPresentationNs + static_cast<int64_t>(intervals * vblankIntervalNs);
            } else {
                nextPresentationNs = lastPresentationNs + static_cast<int64_t>(vblankIntervalNs);
            }
        } else {
            const uint64_t targetVblanks = std::max<uint64_t>(pageflipsSince + pageflipsInAdvance, pageflipsToTarget + 1);
            nextPresentationNs = lastPresentationNs + static_cast<int64_t>(targetVblanks * vblankIntervalNs);
        }
    } else {
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;

        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) {
            nextPresentationNs = currentTimeNs + expectedCompositingNs;
        } else {
            const int64_t minIntervalNs = vblankIntervalSigned;
            nextPresentationNs = currentTimeNs + expectedCompositingNs;

            const int64_t sinceLastNs = currentTimeNs - lastPresentationNs;
            if (sinceLastNs < minIntervalNs) {
                nextPresentationNs = std::max(nextPresentationNs, lastPresentationNs + minIntervalNs);
            }
        }
    }

    const qint64 clampLower = static_cast<qint64>(currentTimeNs);
    const qint64 clampUpper = clampLower + kMaxFutureLeadNs;
    const qint64 clampedNext = std::clamp(static_cast<qint64>(nextPresentationNs), clampLower, clampUpper);
    nextPresentationNs = static_cast<int64_t>(clampedNext);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresentationNs};

    const int64_t nextRenderNs = nextPresentationNs - expectedCompositingNs;
    const int64_t rawDelayNs = std::max<int64_t>(nextRenderNs - currentTimeNs, 0);
    int delayMs = static_cast<int>((rawDelayNs + (kNanosecondsPerMillisecond - 1)) / kNanosecondsPerMillisecond);
    delayMs = std::clamp(delayMs, 0, kMaxTimerDelayMs);

    const int threshold = (delayMs <= 8) ? 0 : ((delayMs < 16) ? 1 : 2);
    const int delta = std::abs(delayMs - scheduledTimerMs);
    if (!compositeTimer.isActive() || delta > threshold) {
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

    if (!inhibitCount && pendingReschedule) {
        scheduleNextRepaint();
    }
}

namespace
{

#if defined(__GNUC__) || defined(__clang__)
#define KWIN_COLD __attribute__((cold))
#define KWIN_NOINLINE __attribute__((noinline))
#else
#define KWIN_COLD
#define KWIN_NOINLINE
#endif

static KWIN_COLD KWIN_NOINLINE void sanitizeName(const QString &in, std::string &out)
{
    out.clear();
    const int size = in.size();
    if (size == 0) {
        return;
    }
    out.reserve(static_cast<size_t>(size));

    const char16_t *const raw = reinterpret_cast<const char16_t *>(in.utf16());
    int i = 0;

#if defined(__AVX2__) && (defined(__GNUC__) || defined(__clang__))
    if (size >= 32) {
        const __m256i lower_a = _mm256_set1_epi16('a' - 1);
        const __m256i lower_z = _mm256_set1_epi16('z' + 1);
        const __m256i upper_A = _mm256_set1_epi16('A' - 1);
        const __m256i upper_Z = _mm256_set1_epi16('Z' + 1);
        const __m256i digits_0 = _mm256_set1_epi16('0' - 1);
        const __m256i digits_9 = _mm256_set1_epi16('9' + 1);
        const __m256i underscore = _mm256_set1_epi16('_');
        const __m256i dash = _mm256_set1_epi16('-');
        const __m256i replacement = _mm256_set1_epi16('_');

        for (; i + 15 < size; i += 16) {
            __m256i chars = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(raw + i));

            __m256i is_lower = _mm256_and_si256(
                _mm256_cmpgt_epi16(chars, lower_a),
                _mm256_cmpgt_epi16(lower_z, chars)
            );
            __m256i is_upper = _mm256_and_si256(
                _mm256_cmpgt_epi16(chars, upper_A),
                _mm256_cmpgt_epi16(upper_Z, chars)
            );
            __m256i is_digit = _mm256_and_si256(
                _mm256_cmpgt_epi16(chars, digits_0),
                _mm256_cmpgt_epi16(digits_9, chars)
            );
            __m256i is_underscore = _mm256_cmpeq_epi16(chars, underscore);
            __m256i is_dash = _mm256_cmpeq_epi16(chars, dash);

            __m256i is_valid = _mm256_or_si256(
                _mm256_or_si256(is_lower, is_upper),
                _mm256_or_si256(
                    _mm256_or_si256(is_digit, is_underscore),
                    is_dash
                )
            );

            __m256i result = _mm256_blendv_epi8(replacement, chars, is_valid);

            alignas(32) char16_t temp[16];
            _mm256_store_si256(reinterpret_cast<__m256i *>(temp), result);
            for (int j = 0; j < 16; ++j) {
                out.push_back(static_cast<char>(temp[j] & 0xFF));
            }
        }
    }
#endif

    for (; i < size; ++i) {
        const char16_t unicode = raw[i];
        if ((unicode >= u'a' && unicode <= u'z') ||
            (unicode >= u'A' && unicode <= u'Z') ||
            (unicode >= u'0' && unicode <= u'9') ||
            unicode == u'_' || unicode == u'-') {
            out.push_back(static_cast<char>(unicode & 0xFF));
        } else {
            out.push_back('_');
        }
    }
}

static KWIN_COLD KWIN_NOINLINE void writeDebugOutput(RenderLoopPrivate *d,
                                                     std::optional<RenderTimeSpan> renderTime,
                                                     OutputFrame *frame,
                                                     std::chrono::nanoseconds timestamp,
                                                     PresentationMode mode)
{
    if (d->output && !d->m_debugOutput) {
        std::string sanitizedName;
        sanitizeName(d->output->name(), sanitizedName);
        const std::string filename = "kwin_perf_" + sanitizedName + ".csv";
        d->m_debugOutput = std::fstream(filename, std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *(d->m_debugOutput)
                << "target_pageflip_ns,pageflip_ns,render_start_ns,render_end_ns,"
                << "safety_margin_ns,refresh_duration_ns,vrr,tearing,predicted_render_ns";
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
                        << frame->predictedRenderTime().count() << ',';
}

#undef KWIN_COLD
#undef KWIN_NOINLINE

} // namespace

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame)
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

    if (compositeTimer.isActive()) {
        scheduleRepaint(lastPresentationTimestamp);
    }

    if (!inhibitCount && pendingReschedule) {
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
    if (event->timerId() == d->compositeTimer.timerId()) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (event->timerId() == d->delayedVrrTimer.timerId()) {
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
    if (d->refreshRate == rr) {
        return;
    }
    d->refreshRate = rr;
    d->cachedVblankIntervalNs = 1'000'000'000'000ull / static_cast<uint64_t>(rr);
    d->updateReciprocal();
    Q_EMIT refreshRateChanged();

    if (!d->inhibitCount) {
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
    d->updateVrrContext();

    const bool vrr = (d->presentationMode == PresentationMode::AdaptiveSync || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing = (d->presentationMode == PresentationMode::Async || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool checkOwnership = (item || outputLayer) && (vrr || tearing);
    const bool activeControlsVrr = checkOwnership ? activeWindowControlsVrrRefreshRate() : false;

    if ((vrr || tearing) && d->vrrCtx.cachedSurfaceItem) {
        SurfaceItem *const cachedSurface = d->vrrCtx.cachedSurfaceItem;
        if (checkOwnership && activeControlsVrr &&
            item && item != cachedSurface && !cachedSurface->isAncestorOf(item)) {
            const uint64_t delayNs = d->cachedVblankIntervalNs * 2;
            const int delayMsInt = static_cast<int>((delayNs + (kNanosecondsPerMillisecond - 1)) / kNanosecondsPerMillisecond);
            d->delayedVrrTimer.start(delayMsInt, Qt::PreciseTimer, this);
            return;
        }
    }

    d->delayedVrrTimer.stop();

    int effectiveMaxPendingFrameCount = d->maxPendingFrameCount;
    if (vrr || tearing) {
        if (d->vrrCtx.cachedSurfaceItem) {
            effectiveMaxPendingFrameCount = (d->vrrCtx.cachedContentType == ContentType::Video) ? 2 : 1;
        } else {
            effectiveMaxPendingFrameCount = 1;
        }
    }

    const int clampedPending = std::max(1, effectiveMaxPendingFrameCount);
    if (d->pendingFrameCount < clampedPending && !d->inhibitCount) {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

[[nodiscard]] bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *const ws = workspace();
    if (Q_UNLIKELY(!ws)) {
        return false;
    }
    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow || Q_UNLIKELY(!d->output) || !activeWindow->isOnOutput(d->output)) {
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
    if (mode != d->presentationMode) {
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

} // namespace KWin

#include "moc_renderloop.cpp"
