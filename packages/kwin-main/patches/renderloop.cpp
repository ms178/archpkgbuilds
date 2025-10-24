/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "renderloop.h"
#include "options.h"
#include "renderloop_p.h"
#include "scene/surfaceitem.h"
#include "utils/common.h"
#include "window.h"
#include "workspace.h"

#include <algorithm>
#include <chrono>
#include <climits>
#include <cstdlib>
#include <filesystem>
#include <string>

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
{
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (kwinApp()->isTerminating() || compositeTimer.isActive() || preparingNewFrame) {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    pendingReschedule = false;

    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) [[unlikely]] {
        return;
    }

    const int64_t vblankInterval = static_cast<int64_t>(vblankIntervalNs);
    const int64_t currentTimeNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresentationNs = lastPresentationTimestamp.count();

    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyMarginNs = safetyMargin.count();

    const int64_t maxCompositingNs = vblankInterval * 2;
    int64_t expectedCompositingNs = predictedRenderNs + safetyMarginNs + 1'000'000;
    if (expectedCompositingNs > maxCompositingNs) {
        expectedCompositingNs = maxCompositingNs;
    }

    int64_t nextPresentationNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = currentTimeNs - lastPresentationNs;
        const uint64_t pageflipsSince = (sinceLastNs > 0) ? (static_cast<uint64_t>(sinceLastNs) / vblankIntervalNs) : 0;

        if (pageflipsSince > 100) [[unlikely]] {
            const int64_t earlyStart = vblankInterval - 1'000;
            if (expectedCompositingNs < earlyStart) {
                expectedCompositingNs = earlyStart;
            }
        }

        const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresentationNs;
        const uint64_t pageflipsSinceLastToTarget = (toTargetNs > 0) ? ((static_cast<uint64_t>(toTargetNs) + vblankIntervalNs / 2) / vblankIntervalNs) : 0;

        uint64_t pageflipsInAdvance = (static_cast<uint64_t>(expectedCompositingNs) + vblankIntervalNs - 1) / vblankIntervalNs;
        pageflipsInAdvance = std::clamp(pageflipsInAdvance, uint64_t(1), static_cast<uint64_t>(maxPendingFrameCount));

        if (pageflipsInAdvance > 1) {
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
        } else if (wasTripleBuffering) {
            const int64_t threshold = vblankInterval * 95 / 100;
            if (expectedCompositingNs >= threshold) {
                doubleBufferingCounter = 0;
            } else {
                doubleBufferingCounter++;
            }

            if (doubleBufferingCounter >= 10) {
                wasTripleBuffering = false;
            } else {
                pageflipsInAdvance = 2;
            }
        }

        if (compositeTimer.isActive()) {
            const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresentationNs;
            uint32_t intervalsSinceLastTimestamp = 1;
            if (deltaNs > 0) [[likely]] {
                const uint64_t intervals = (static_cast<uint64_t>(deltaNs) + vblankIntervalNs / 2) / vblankIntervalNs;
                if (intervals > 0 && intervals <= UINT32_MAX) {
                    intervalsSinceLastTimestamp = static_cast<uint32_t>(intervals);
                }
            }
            nextPresentationNs = lastPresentationNs + static_cast<int64_t>(intervalsSinceLastTimestamp) * vblankInterval;
        } else {
            const uint64_t targetVblanks = std::max(pageflipsSince + pageflipsInAdvance, pageflipsSinceLastToTarget + 1);
            nextPresentationNs = lastPresentationNs + static_cast<int64_t>(targetVblanks) * vblankInterval;
        }
    } else {
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;
        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) {
            nextPresentationNs = currentTimeNs;
        } else {
            const int64_t candidate = lastPresentationNs + vblankInterval;
            nextPresentationNs = std::max(currentTimeNs, candidate);
        }
    }

    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresentationNs};

    const int64_t nextRenderNs = nextPresentationNs - expectedCompositingNs;
    int64_t delayNs = nextRenderNs - currentTimeNs;
    if (delayNs < 0) {
        delayNs = 0;
    }

    const int delayMs = static_cast<int>((delayNs + 999'999) / 1'000'000);

    if (!compositeTimer.isActive() || std::abs(delayMs - scheduledTimerMs) > 1) {
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

#if (defined(__x86_64__) || defined(_M_X64)) && (defined(__GNUC__) || defined(__clang__))
    if (__builtin_cpu_supports("avx2")) {
        const __m256i lower_a_m1 = _mm256_set1_epi16('a' - 1);
        const __m256i upper_z_p1 = _mm256_set1_epi16('z' + 1);
        const __m256i upper_A_m1 = _mm256_set1_epi16('A' - 1);
        const __m256i upper_Z_p1 = _mm256_set1_epi16('Z' + 1);
        const __m256i digit_0_m1 = _mm256_set1_epi16('0' - 1);
        const __m256i digit_9_p1 = _mm256_set1_epi16('9' + 1);
        const __m256i underscore = _mm256_set1_epi16('_');
        const __m256i dash = _mm256_set1_epi16('-');
        const __m256i replacement = _mm256_set1_epi16('_');

        for (; i + 15 < size; i += 16) {
            __m256i chars = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(in.data() + i));

            __m256i is_lower = _mm256_and_si256(_mm256_cmpgt_epi16(chars, lower_a_m1), _mm256_cmpgt_epi16(upper_z_p1, chars));
            __m256i is_upper = _mm256_and_si256(_mm256_cmpgt_epi16(chars, upper_A_m1), _mm256_cmpgt_epi16(upper_Z_p1, chars));
            __m256i is_digit = _mm256_and_si256(_mm256_cmpgt_epi16(chars, digit_0_m1), _mm256_cmpgt_epi16(digit_9_p1, chars));
            __m256i is_underscore = _mm256_cmpeq_epi16(chars, underscore);
            __m256i is_dash = _mm256_cmpeq_epi16(chars, dash);

            __m256i is_allowed = _mm256_or_si256(is_underscore, _mm256_or_si256(is_dash, _mm256_or_si256(is_digit, _mm256_or_si256(is_lower, is_upper))));
            __m256i result = _mm256_blendv_epi8(replacement, chars, is_allowed);

            __m128i lo = _mm256_castsi256_si128(result);
            __m128i hi = _mm256_extracti128_si256(result, 1);
            __m128i packed = _mm_packus_epi16(lo, hi);

            char temp[16];
            _mm_storeu_si128(reinterpret_cast<__m128i *>(temp), packed);
            out.append(temp, 16);
        }
    }
#endif

    for (; i < size; ++i) {
        const QChar ch = in.at(i);
        if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-') {
            out.push_back(static_cast<char>(ch.toLatin1()));
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
        d->m_debugOutput->flush();
    }
}

#undef KWIN_COLD
#undef KWIN_NOINLINE

} // anonymous namespace

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

RenderLoop::~RenderLoop()
{
}

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
    const int rr = std::clamp(refreshRate, 1'000, 1'000'000);
    if (d->refreshRate == rr) {
        return;
    }
    d->refreshRate = rr;
    d->cachedVblankIntervalNs = 1'000'000'000'000ull / static_cast<uint64_t>(rr);
    Q_EMIT refreshRateChanged();
}

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds safetyMargin)
{
    d->safetyMargin = safetyMargin;
}

void RenderLoop::scheduleRepaint(Item *item, OutputLayer *outputLayer)
{
    const bool vrr = (d->presentationMode == PresentationMode::AdaptiveSync || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing = (d->presentationMode == PresentationMode::Async || d->presentationMode == PresentationMode::AdaptiveAsync);

    if ((vrr || tearing) && workspace() && d->output) [[unlikely]] {
        Window *const activeWin = workspace()->activeWindow();
        if (activeWin) {
            SurfaceItem *const surfaceItem = activeWin->surfaceItem();
            if ((item || outputLayer) && activeWindowControlsVrrRefreshRate() && surfaceItem && item != surfaceItem && !surfaceItem->isAncestorOf(item)) {
                constexpr std::chrono::milliseconds s_delayVrrTimer = 1'000ms / 30;
                d->delayedVrrTimer.start(s_delayVrrTimer, Qt::PreciseTimer, this);
                return;
            }
        }
    }

    d->delayedVrrTimer.stop();
    const int effectiveMaxPendingFrameCount = (vrr || tearing) ? 1 : d->maxPendingFrameCount;
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
    if (!surfaceItem) {
        return false;
    }
    return surfaceItem->recursiveFrameTimeEstimation() <= std::chrono::nanoseconds(1'000'000'000) / 30;
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
        qCDebug(KWIN_CORE) << "Changed presentation mode to" << mode;
    }
    d->presentationMode = mode;
}

void RenderLoop::setMaxPendingFrameCount(uint32_t maxCount)
{
    if (maxCount == 0) {
        d->maxPendingFrameCount = 1;
    } else if (maxCount > static_cast<uint32_t>(INT_MAX)) {
        d->maxPendingFrameCount = INT_MAX;
    } else {
        d->maxPendingFrameCount = static_cast<int>(maxCount);
    }
}

std::chrono::nanoseconds RenderLoop::predictedRenderTime() const
{
    return d->renderJournal.result();
}

} // namespace KWin

#include "moc_renderloop.cpp"
