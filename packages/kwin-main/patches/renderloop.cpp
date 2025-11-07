/*
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
#include <filesystem>
#include <string>
#include <bit>
#include <type_traits>

#if defined(__x86_64__) || defined(_M_X64)
#include <immintrin.h>
#endif

using namespace std::chrono_literals;

namespace KWin
{

static const bool s_printDebugInfo = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop)
{
    return loop->d.get();
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, Output *output)
    : q(q)
    , output(output)
    , cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)
    , vblankIntervalReciprocal(0)
    , vblankIntervalReciprocal64(0)
    , reciprocalShift(0)
    , reciprocalShift64(0)
{
    updateReciprocal();
    initializeVrrCapabilities();
}

void RenderLoopPrivate::updateReciprocal()
{
    if (cachedVblankIntervalNs > 0 && cachedVblankIntervalNs < (1ull << 32)) {
        const uint64_t shift = std::bit_width(cachedVblankIntervalNs) + 31;
        vblankIntervalReciprocal = ((1ull << shift) + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
        reciprocalShift = static_cast<uint8_t>(shift);

        if (cachedVblankIntervalNs < (1ull << 31)) {
            constexpr uint64_t shift64 = 63;
            vblankIntervalReciprocal64 = ((1ull << shift64) + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
            reciprocalShift64 = static_cast<uint8_t>(shift64);
        } else {
            vblankIntervalReciprocal64 = vblankIntervalReciprocal;
            reciprocalShift64 = reciprocalShift;
        }
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

    const QString policy = vrrGroup.readEntry("Policy", "Automatic");
    if (policy == "Never") {
        vrrEnabled = false;
    } else if (policy == "Always") {
        vrrMode = VrrMode::Always;
        vrrEnabled = vrrCapable;
    } else if (policy == "Automatic") {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    } else {
        vrrMode = VrrMode::Automatic;
        vrrEnabled = vrrCapable;
    }

    if (vrrEnabled) {
        qCInfo(KWIN_CORE) << "VRR enabled for output" << output->name() << "mode:" << policy;
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

    auto *waylandItem = qobject_cast<SurfaceItemWayland *>(surfaceItem);
    if (waylandItem) {
        if (auto *surface = waylandItem->surface()) {
            vrrCtx.cachedHint = surface->presentationModeHint();
        }
    }
}

PresentationMode RenderLoopPrivate::selectPresentationModeFromContext() const
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
    if (kwinApp()->isTerminating() || compositeTimer.isActive() || preparingNewFrame) {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((always_inline, hot))
#endif
static inline uint64_t fastDivideByVblank(uint64_t numerator, uint64_t vblankNs,
                                          uint64_t reciprocal64, uint8_t shift64,
                                          uint64_t reciprocal128, uint8_t shift128)
{
    if (numerator < (1ull << 32) && reciprocal64 > 0) [[likely]] {
        return (numerator * reciprocal64) >> shift64;
    }

    if (reciprocal128 > 0) [[likely]] {
        return static_cast<uint64_t>((static_cast<__uint128_t>(numerator) * reciprocal128) >> shift128);
    }

    return (vblankNs > 0) ? (numerator / vblankNs) : 0;
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((hot, target("avx2")))
#endif
void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    pendingReschedule = false;

    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) [[unlikely]] {
        return;
    }

    const int64_t currentTimeNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresentationNs = lastPresentationTimestamp.count();
    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyMarginNs = safetyMargin.count();

    const int64_t vblankIntervalSigned = static_cast<int64_t>(vblankIntervalNs);
    const int64_t maxCompositingNs = vblankIntervalSigned * 2;
    int64_t expectedCompositingNs = predictedRenderNs + safetyMarginNs + 1'000'000;
    if (expectedCompositingNs > maxCompositingNs) {
        expectedCompositingNs = maxCompositingNs;
    }

    const PresentationMode targetMode = selectPresentationModeFromContext();
    if (targetMode != presentationMode) {
        const uint8_t threshold = (targetMode == PresentationMode::VSync) ? 2 : 3;
        if (vrrStabilityCounter >= threshold) {
            presentationMode = targetMode;
            vrrStabilityCounter = 0;
            qCDebug(KWIN_CORE) << "Presentation mode changed to" << static_cast<int>(targetMode);
        } else {
            vrrStabilityCounter++;
        }
    } else {
        vrrStabilityCounter = 0;
    }

    int64_t nextPresentationNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = currentTimeNs - lastPresentationNs;
        uint64_t pageflipsSince = 0;

        if (sinceLastNs > 0) [[likely]] {
            const uint64_t sinceLastNsU = static_cast<uint64_t>(sinceLastNs);
            pageflipsSince = fastDivideByVblank(sinceLastNsU, vblankIntervalNs,
                                                vblankIntervalReciprocal64, reciprocalShift64,
                                                vblankIntervalReciprocal, reciprocalShift);
        }

        if (pageflipsSince > 100) [[unlikely]] {
            const int64_t earlyStart = vblankIntervalSigned - 1'000;
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
            uint64_t required = (static_cast<uint64_t>(expectedCompositingNs) + vblankIntervalNs - 1) / vblankIntervalNs;
            pageflipsInAdvance = required < static_cast<uint64_t>(maxPendingFrameCount) ? required : static_cast<uint64_t>(maxPendingFrameCount);
        }

        const bool highLoad = (expectedCompositingNs > vblankIntervalSigned * 85 / 100);
        if (highLoad) {
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
            if (pageflipsInAdvance < 2) {
                pageflipsInAdvance = 2;
            }
        } else if (wasTripleBuffering) {
            doubleBufferingCounter = (expectedCompositingNs < vblankIntervalSigned * 65 / 100) ?
                                    (doubleBufferingCounter + 1) : 0;
            if (doubleBufferingCounter >= 4) {
                wasTripleBuffering = false;
            } else if (pageflipsInAdvance < 2) {
                pageflipsInAdvance = 2;
            }
        }

        if (compositeTimer.isActive()) {
            const uint64_t intervals = static_cast<uint64_t>((nextPresentationTimestamp.count() - lastPresentationNs + static_cast<int64_t>(vblankIntervalNs)/2) / static_cast<int64_t>(vblankIntervalNs));
            const uint64_t clampedIntervals = intervals < 1000000 ? intervals : 1000000;
            nextPresentationNs = lastPresentationNs + static_cast<int64_t>(clampedIntervals * vblankIntervalNs);
        } else {
            const uint64_t targetVblanks = (pageflipsSince + pageflipsInAdvance) > (pageflipsToTarget + 1) ?
                                          (pageflipsSince + pageflipsInAdvance) : (pageflipsToTarget + 1);
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

    const int64_t maxFutureNs = currentTimeNs + (static_cast<int64_t>(1) << 40);
    nextPresentationNs = std::clamp(nextPresentationNs, currentTimeNs, maxFutureNs);
    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresentationNs};

    const int64_t nextRenderNs = nextPresentationNs - expectedCompositingNs;
    int64_t delayNs = (nextRenderNs > currentTimeNs) ? (nextRenderNs - currentTimeNs) : 0;
    int delayMs = std::clamp(static_cast<int>((delayNs + 999'999) / 1'000'000), 0, 32767);

    const int threshold = (delayMs <= 8) ? 0 : ((delayMs < 16) ? 1 : 2);
    if (!compositeTimer.isActive() || std::abs(delayMs - scheduledTimerMs) > threshold) {
        scheduledTimerMs = static_cast<int16_t>(delayMs);
        compositeTimer.start(delayMs, Qt::PreciseTimer, q);
    }
}

void RenderLoopPrivate::delayScheduleRepaint()
{
    pendingReschedule = true;
}

void RenderLoopPrivate::notifyFrameDropped()
{
    Q_ASSERT(pendingFrameCount > 0);
    pendingFrameCount--;

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
            __m256i chars = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(in.utf16() + i));

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
            _mm256_store_si256(reinterpret_cast<__m256i*>(temp), result);
            for (int j = 0; j < 16; ++j) {
                out.push_back(static_cast<char>(temp[j] & 0xFF));
            }
        }
    }
#endif

    for (; i < size; ++i) {
        const QChar ch = in.at(i);
        const char16_t unicode = ch.unicode();
        if ((unicode >= 'a' && unicode <= 'z') ||
            (unicode >= 'A' && unicode <= 'Z') ||
            (unicode >= '0' && unicode <= '9') ||
            unicode == '_' || unicode == '-') {
            out.push_back(static_cast<char>(unicode));
        } else {
            out.push_back('_');
        }
    }
}

static KWIN_COLD KWIN_NOINLINE void writeDebugOutput(RenderLoopPrivate *d, std::optional<RenderTimeSpan> renderTime, OutputFrame *frame, std::chrono::nanoseconds timestamp, PresentationMode mode)
{
    if (d->output && !d->m_debugOutput) {
        std::string sanitizedName;
        sanitizeName(d->output->name(), sanitizedName);
        const std::string filename = "kwin_perf_" + sanitizedName + ".csv";
        d->m_debugOutput = std::fstream(filename, std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *(d->m_debugOutput) << "target_pageflip_ns,pageflip_ns,render_start_ns,render_end_ns,"
                                << "safety_margin_ns,refresh_duration_ns,vrr,tearing,predicted_render_ns\n";
        }
    }

    if (d->m_debugOutput && d->m_debugOutput->is_open()) {
        auto times = renderTime.value_or(RenderTimeSpan{});
        const bool vrr = (mode == PresentationMode::AdaptiveSync || mode == PresentationMode::AdaptiveAsync);
        const bool tearing = (mode == PresentationMode::Async || mode == PresentationMode::AdaptiveAsync);

        *(d->m_debugOutput) << frame->targetPageflipTime().time_since_epoch().count() << ","
                            << timestamp.count() << ","
                            << times.start.time_since_epoch().count() << ","
                            << times.end.time_since_epoch().count() << ","
                            << d->safetyMargin.count() << ","
                            << frame->refreshDuration().count() << ","
                            << (vrr ? 1 : 0) << ","
                            << (tearing ? 1 : 0) << ","
                            << frame->predictedRenderTime().count() << "\n";
    }
}

#undef KWIN_COLD
#undef KWIN_NOINLINE

}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame)
{
    if (s_printDebugInfo) [[unlikely]] {
        writeDebugOutput(this, renderTime, frame, timestamp, mode);
    }

    Q_ASSERT(pendingFrameCount > 0);
    pendingFrameCount--;

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
    d->inhibitCount++;
    if (d->inhibitCount == 1) {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
    }
}

void RenderLoop::uninhibit()
{
    Q_ASSERT(d->inhibitCount > 0);
    d->inhibitCount--;
    if (d->inhibitCount == 0) {
        d->scheduleNextRepaint();
    }
}

void RenderLoop::prepareNewFrame()
{
    d->pendingFrameCount++;
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

void RenderLoop::setRefreshRate(int refreshRate)
{
    const int rr = std::clamp(refreshRate, 1000, 1000000);
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
    d->safetyMargin = safetyMargin;
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *outputLayer)
{
    d->updateVrrContext();

    const bool vrr = (d->presentationMode == PresentationMode::AdaptiveSync || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing = (d->presentationMode == PresentationMode::Async || d->presentationMode == PresentationMode::AdaptiveAsync);

    if ((vrr || tearing) && d->vrrCtx.cachedSurfaceItem) [[likely]] {
        if ((item || outputLayer) && activeWindowControlsVrrRefreshRate() &&
            item != d->vrrCtx.cachedSurfaceItem && !d->vrrCtx.cachedSurfaceItem->isAncestorOf(item)) {
            const uint64_t delayNs = d->cachedVblankIntervalNs * 2;
            const int delayMsInt = static_cast<int>((delayNs + 999'999) / 1'000'000);
            d->delayedVrrTimer.start(delayMsInt, Qt::PreciseTimer, this);
            return;
        }
    }

    d->delayedVrrTimer.stop();

    int effectiveMaxPendingFrameCount = d->maxPendingFrameCount;

    if (vrr || tearing) {
        if (d->vrrCtx.cachedSurfaceItem) [[likely]] {
            effectiveMaxPendingFrameCount = (d->vrrCtx.cachedContentType == ContentType::Video) ? 2 : 1;
        } else {
            effectiveMaxPendingFrameCount = 1;
        }
    }

    if (d->pendingFrameCount < effectiveMaxPendingFrameCount && !d->inhibitCount) {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *const ws = workspace();
    if (!ws) [[unlikely]] {
        return false;
    }
    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow || !activeWindow->isOnOutput(d->output)) {
        return false;
    }
    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
    return surfaceItem && (surfaceItem->recursiveFrameTimeEstimation() <= 33ms);
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
    if (mode != d->presentationMode) {
        qCDebug(KWIN_CORE) << "Changed presentation mode to" << static_cast<int>(mode);
        d->presentationMode = mode;
    }
}

void RenderLoop::setMaxPendingFrameCount(uint32_t maxCount)
{
    d->maxPendingFrameCount = std::clamp(static_cast<int>(maxCount), 1, INT_MAX);
}

std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

}

#include "moc_renderloop.cpp"
