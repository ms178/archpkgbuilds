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

    const int64_t currentTimeNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresentationNs = lastPresentationTimestamp.count();

    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyMarginNs = safetyMargin.count();

    // Compute expected compositing duration with upper bound
    const int64_t vblankIntervalSigned = static_cast<int64_t>(vblankIntervalNs);
    const int64_t maxCompositingNs = vblankIntervalSigned * 2;
    int64_t expectedCompositingNs = predictedRenderNs + safetyMarginNs + 1'000'000;
    if (expectedCompositingNs > maxCompositingNs) {
        expectedCompositingNs = maxCompositingNs;
    }

    int64_t nextPresentationNs;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = currentTimeNs - lastPresentationNs;

        // Compute pageflips since last presentation (avoid division if recent)
        uint64_t pageflipsSince = 0;
        if (sinceLastNs > 0) [[likely]] {
            // Common case: 1-3 vblanks since last frame
            if (sinceLastNs < static_cast<int64_t>(vblankIntervalNs * 4)) [[likely]] {
                // Use linear search for small counts (faster than division)
                uint64_t accumNs = vblankIntervalNs;
                pageflipsSince = 1;
                while (accumNs <= static_cast<uint64_t>(sinceLastNs) && pageflipsSince < 4) {
                    accumNs += vblankIntervalNs;
                    pageflipsSince++;
                }
                if (accumNs > static_cast<uint64_t>(sinceLastNs)) {
                    pageflipsSince--;
                }
            } else {
                // Uncommon case: many vblanks (e.g., suspend/resume)
                pageflipsSince = static_cast<uint64_t>(sinceLastNs) / vblankIntervalNs;
            }
        }

        // Detect suspend/resume: if >100 vblanks elapsed, we likely suspended
        if (pageflipsSince > 100) [[unlikely]] {
            const int64_t earlyStart = vblankIntervalSigned - 1'000;
            if (expectedCompositingNs < earlyStart) {
                expectedCompositingNs = earlyStart;
            }
        }

        // Compute pageflips since last target timestamp (rounding division)
        const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresentationNs;
        uint64_t pageflipsSinceLastToTarget = 0;
        if (toTargetNs > 0) [[likely]] {
            const uint64_t toTargetUns = static_cast<uint64_t>(toTargetNs);
            // Rounding division: (x + d/2) / d
            pageflipsSinceLastToTarget = (toTargetUns + (vblankIntervalNs >> 1)) / vblankIntervalNs;
        }

        // Compute pageflips in advance (ceiling division)
        // Ceiling: (x + d - 1) / d
        const uint64_t expectedCompositingUns = static_cast<uint64_t>(expectedCompositingNs);
        uint64_t pageflipsInAdvance = (expectedCompositingUns + vblankIntervalNs - 1) / vblankIntervalNs;
        pageflipsInAdvance = std::clamp(pageflipsInAdvance, uint64_t(1), static_cast<uint64_t>(maxPendingFrameCount));

        // Triple-buffering heuristic
        if (pageflipsInAdvance > 1) {
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
        } else if (wasTripleBuffering) {
            const int64_t threshold = vblankIntervalSigned * 95 / 100;
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

        // Compute next presentation timestamp
        if (compositeTimer.isActive()) {
            const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresentationNs;
            uint32_t intervalsSinceLastTimestamp = 1;
            if (deltaNs > 0) [[likely]] {
                // Rounding division with overflow check
                const uint64_t deltaUns = static_cast<uint64_t>(deltaNs);
                const uint64_t intervals = (deltaUns + (vblankIntervalNs >> 1)) / vblankIntervalNs;
                if (intervals > 0 && intervals <= UINT32_MAX) {
                    intervalsSinceLastTimestamp = static_cast<uint32_t>(intervals);
                }
            }
            // Overflow-safe multiplication
            if (intervalsSinceLastTimestamp <= INT64_MAX / vblankIntervalSigned) [[likely]] {
                nextPresentationNs = lastPresentationNs + static_cast<int64_t>(intervalsSinceLastTimestamp) * vblankIntervalSigned;
            } else {
                // Overflow: clamp to far future
                nextPresentationNs = INT64_MAX;
            }
        } else {
            const uint64_t targetVblanks = std::max(pageflipsSince + pageflipsInAdvance, pageflipsSinceLastToTarget + 1);
            // Overflow-safe multiplication
            if (targetVblanks <= static_cast<uint64_t>(INT64_MAX) / vblankIntervalNs) [[likely]] {
                nextPresentationNs = lastPresentationNs + static_cast<int64_t>(targetVblanks * vblankIntervalNs);
            } else {
                nextPresentationNs = INT64_MAX;
            }
        }
    } else {
        // Non-VSync modes
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;
        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) {
            nextPresentationNs = currentTimeNs;
        } else {
            const int64_t candidate = lastPresentationNs + vblankIntervalSigned;
            nextPresentationNs = std::max(currentTimeNs, candidate);
        }
    }

    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresentationNs};

    // Compute delay until next render start
    const int64_t nextRenderNs = nextPresentationNs - expectedCompositingNs;
    int64_t delayNs = nextRenderNs - currentTimeNs;
    if (delayNs < 0) {
        delayNs = 0;
    }

    // Convert to milliseconds with overflow protection
    int delayMs;
    if (delayNs <= INT64_MAX - 999'999) [[likely]] {
        delayMs = static_cast<int>((delayNs + 999'999) / 1'000'000);
    } else {
        delayMs = INT_MAX; // Far future
    }

    // Only restart timer if delay changed significantly (avoid timer thrashing)
    if (!compositeTimer.isActive() || std::abs(delayMs - scheduledTimerMs) > 1) {
        scheduledTimerMs = static_cast<int16_t>(std::min(delayMs, 32767)); // Clamp to int16_t range
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

// Fixed and optimized sanitizeName with correct AVX2 logic
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
    // AVX2 fast path: process 16 characters at a time
    if (__builtin_cpu_supports("avx2") && size >= 16) {
        const __m256i lower_a = _mm256_set1_epi16('a');
        const __m256i upper_z = _mm256_set1_epi16('z');
        const __m256i upper_A = _mm256_set1_epi16('A');
        const __m256i upper_Z = _mm256_set1_epi16('Z');
        const __m256i digit_0 = _mm256_set1_epi16('0');
        const __m256i digit_9 = _mm256_set1_epi16('9');
        const __m256i underscore = _mm256_set1_epi16('_');
        const __m256i dash = _mm256_set1_epi16('-');
        const __m256i replacement = _mm256_set1_epi16('_');

        for (; i + 15 < size; i += 16) {
            // Load 16 QChars (16-bit each, UTF-16)
            __m256i chars = _mm256_loadu_si256(reinterpret_cast<const __m256i *>(in.data() + i));

            // Check if characters are in allowed ranges (16-bit comparisons)
            __m256i is_lower = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(lower_a, _mm256_set1_epi16(1))),
                                                _mm256_cmpgt_epi16(_mm256_add_epi16(upper_z, _mm256_set1_epi16(1)), chars));
            __m256i is_upper = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(upper_A, _mm256_set1_epi16(1))),
                                                _mm256_cmpgt_epi16(_mm256_add_epi16(upper_Z, _mm256_set1_epi16(1)), chars));
            __m256i is_digit = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(digit_0, _mm256_set1_epi16(1))),
                                                _mm256_cmpgt_epi16(_mm256_add_epi16(digit_9, _mm256_set1_epi16(1)), chars));
            __m256i is_underscore = _mm256_cmpeq_epi16(chars, underscore);
            __m256i is_dash = _mm256_cmpeq_epi16(chars, dash);

            __m256i is_allowed = _mm256_or_si256(_mm256_or_si256(is_lower, is_upper),
                                                  _mm256_or_si256(_mm256_or_si256(is_digit, is_underscore), is_dash));

            // Blend: keep allowed chars, replace others with '_'
            __m256i result = _mm256_blendv_epi8(replacement, chars, is_allowed);

            // Pack 16-bit to 8-bit (assumes ASCII range, discards high bytes)
            __m128i lo = _mm256_castsi256_si128(result);
            __m128i hi = _mm256_extracti128_si256(result, 1);
            __m128i packed = _mm_packus_epi16(lo, hi);

            // packus_epi16 produces: [lo0, lo1, ..., lo7, hi0, hi1, ..., hi7]
            // We need to shuffle to correct order
            const __m128i shuffle_mask = _mm_setr_epi8(0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15);
            packed = _mm_shuffle_epi8(packed, shuffle_mask);

            // Store 16 bytes
            alignas(16) char temp[16];
            _mm_storeu_si128(reinterpret_cast<__m128i *>(temp), packed);
            out.append(temp, 16);
        }
    }
#endif

    // Scalar fallback for remaining characters
    for (; i < size; ++i) {
        const QChar ch = in.at(i);
        const char16_t unicode = ch.unicode();
        if ((unicode >= 'a' && unicode <= 'z') || (unicode >= 'A' && unicode <= 'Z') ||
            (unicode >= '0' && unicode <= '9') || unicode == '_' || unicode == '-') {
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
        // Remove flush(): let OS buffer writes to avoid blocking hot path
        // File will be flushed on close or periodic kernel writeback
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

    // When refresh rate changes, reschedule to recalculate vblank intervals.
    // Stop active timer first to prevent stale scheduling with old interval.
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
    const bool vrr = (d->presentationMode == PresentationMode::AdaptiveSync || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing = (d->presentationMode == PresentationMode::Async || d->presentationMode == PresentationMode::AdaptiveAsync);

    if ((vrr || tearing) && workspace() && d->output) [[likely]] {
        Window *const activeWin = workspace()->activeWindow();
        if (activeWin) {
            SurfaceItem *const surfaceItem = activeWin->surfaceItem();
            if ((item || outputLayer) && activeWindowControlsVrrRefreshRate() && surfaceItem && item != surfaceItem && !surfaceItem->isAncestorOf(item)) {
                // Adaptive delay: use 2 vblanks instead of fixed 30Hz to minimize latency while avoiding spurious VRR triggers
                const uint64_t delayNs = d->cachedVblankIntervalNs * 2;
                const int delayMsInt = static_cast<int>((delayNs + 999'999) / 1'000'000);
                d->delayedVrrTimer.start(delayMsInt, Qt::PreciseTimer, this);
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
