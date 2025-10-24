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
#include <cmath>
#include <fstream>
#include <limits>
#include <string>

#include <QString>

#if defined(__x86_64__) || defined(_M_X64)
#include <immintrin.h>
#endif

using namespace std::chrono_literals;

namespace KWin
{

// Static configuration: enable performance CSV logging via environment variable
static const bool s_printDebugInfo = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop)
{
    return loop->d.get();
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, Output *output)
    : q(q)
    , output(output)
    , refreshRate(60'000)                                               // 60 Hz in millihertz
    , pendingFrameCount(0)
    , inhibitCount(0)
    , maxPendingFrameCount(1)
    , presentationMode(PresentationMode::VSync)
    , pendingReschedule(false)
    , wasTripleBuffering(false)
    , doubleBufferingCounter(0)
    , cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)         // ~16.666ms for 60Hz
    , lastPresentationTimestamp(std::chrono::nanoseconds::zero())
    , nextPresentationTimestamp(std::chrono::nanoseconds::zero())
    , safetyMargin(std::chrono::nanoseconds::zero())
{
    compositeTimer.setSingleShot(true);
    compositeTimer.setTimerType(Qt::PreciseTimer);

    QObject::connect(&compositeTimer, &QTimer::timeout, q, [this]() {
        dispatch();
    });

    delayedVrrTimer.setSingleShot(true);
    delayedVrrTimer.setInterval(1'000 / 30); // ~33ms delay for VRR gating
    delayedVrrTimer.setTimerType(Qt::PreciseTimer);

    QObject::connect(&delayedVrrTimer, &QTimer::timeout, q, [q]() {
        q->scheduleRepaint(nullptr, nullptr, nullptr);
    });
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (kwinApp()->isTerminating() || compositeTimer.isActive()) [[unlikely]] {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    pendingReschedule = false;

    // Early exit if refresh rate is invalid (defensive; setRefreshRate clamps to [1000, 1000000])
    if (cachedVblankIntervalNs == 0) [[unlikely]] {
        qCWarning(KWIN_CORE) << "Invalid vblank interval; skipping scheduleRepaint";
        return;
    }

    // Hoist invariant to enable compiler optimization (division by constant → multiply+shift)
    const std::chrono::nanoseconds vblankInterval{static_cast<int64_t>(cachedVblankIntervalNs)};
    const int64_t vblankNs = vblankInterval.count(); // Cache for repeated use

    const std::chrono::nanoseconds currentTime(std::chrono::steady_clock::now().time_since_epoch());

    // Estimate expected compositing time with safety margin and scheduler slop
    // Cap at 2× vblank to prevent pathological cases
    std::chrono::nanoseconds expectedCompositingTime = std::min(
        renderJournal.result() + safetyMargin + 1ms,
        2 * vblankInterval
    );

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        // === VSync Mode: Pageflips occur at vblank boundaries ===

        // Calculate vblanks since last presentation (integer division optimized by compiler)
        const auto sinceLast = currentTime - lastPresentationTimestamp;
        uint64_t pageflipsSince = 0;
        if (sinceLast.count() > 0) [[likely]] {
            pageflipsSince = static_cast<uint64_t>(sinceLast.count()) / static_cast<uint64_t>(vblankNs);
        }

        if (pageflipsSince > 100) [[unlikely]] {
            // GPU wake from low-power: expect high latency
            expectedCompositingTime = std::max(vblankInterval - 1us, expectedCompositingTime);
        }

        // Vblanks from last presentation to target (with rounding)
        const auto toTarget = lastTargetTimestamp - lastPresentationTimestamp;
        uint64_t pageflipsSinceLastToTarget = 0;
        if (toTarget.count() > 0) [[likely]] {
            const int64_t toTargetPlusHalf = toTarget.count() + (vblankNs / 2);
            if (toTargetPlusHalf > 0) {
                pageflipsSinceLastToTarget = static_cast<uint64_t>(toTargetPlusHalf) / static_cast<uint64_t>(vblankNs);
            }
        }

        // Determine pageflips in advance: ceil(expectedCompositingTime / vblankInterval)
        uint64_t pageflipsInAdvance = (static_cast<uint64_t>(expectedCompositingTime.count()) + static_cast<uint64_t>(vblankNs) - 1)
                                       / static_cast<uint64_t>(vblankNs);
        if (pageflipsInAdvance < 1) {
            pageflipsInAdvance = 1;
        }
        if (pageflipsInAdvance > static_cast<uint64_t>(maxPendingFrameCount)) {
            pageflipsInAdvance = static_cast<uint64_t>(maxPendingFrameCount);
        }

        // === Triple Buffering Hysteresis (optimized branching) ===
        if (pageflipsInAdvance > 1) {
            // Render time requires triple buffering
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
        } else if (wasTripleBuffering) {
            // Currently triple buffering; check if we can switch back
            const auto vblank95 = vblankInterval - (vblankInterval / 20);

            // Branchless counter update (reduces mispredicts)
            const bool renderTooHigh = (expectedCompositingTime >= vblank95);
            doubleBufferingCounter = renderTooHigh ? static_cast<int16_t>(0) : static_cast<int16_t>(doubleBufferingCounter + 1);

            if (doubleBufferingCounter >= 10) {
                // Stable: switch to double buffering
                wasTripleBuffering = false;
                pageflipsInAdvance = 1;
                doubleBufferingCounter = 0;
            } else {
                // Stay in triple buffering
                pageflipsInAdvance = 2;
                if (renderTooHigh) {
                    expectedCompositingTime = vblankInterval;
                }
            }
        }

        // === Schedule Next Presentation ===
        if (compositeTimer.isActive()) {
            // Timer already running: recompute target with updated render time
            const auto delta = nextPresentationTimestamp - lastPresentationTimestamp;
            uint32_t intervalsSinceLastTimestamp = 1;
            if (delta.count() > 0) [[likely]] {
                const int64_t deltaRounded = delta.count() + (vblankNs / 2);
                if (deltaRounded > 0) {
                    intervalsSinceLastTimestamp = static_cast<uint32_t>(
                        static_cast<uint64_t>(deltaRounded) / static_cast<uint64_t>(vblankNs)
                    );
                    if (intervalsSinceLastTimestamp < 1) {
                        intervalsSinceLastTimestamp = 1;
                    }
                }
            }
            nextPresentationTimestamp = lastPresentationTimestamp + intervalsSinceLastTimestamp * vblankInterval;
        } else {
            // Schedule for max(pageflipsSince + pageflipsInAdvance, pageflipsSinceLastToTarget + 1)
            const uint64_t targetVblanks = std::max(pageflipsSince + pageflipsInAdvance, pageflipsSinceLastToTarget + 1);
            nextPresentationTimestamp = lastPresentationTimestamp + targetVblanks * vblankInterval;
        }

    } else {
        // === Adaptive Sync / Tearing Mode ===
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;

        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) {
            nextPresentationTimestamp = currentTime;
        } else {
            nextPresentationTimestamp = std::max(currentTime, lastPresentationTimestamp + vblankInterval);
        }
    }

    // === Start QTimer ===
    const std::chrono::nanoseconds nextRenderTimestamp = nextPresentationTimestamp - expectedCompositingTime;
    const std::chrono::nanoseconds timeUntilRender = nextRenderTimestamp - currentTime;
    const auto msUntilRender = std::chrono::duration_cast<std::chrono::milliseconds>(std::max(0ns, timeUntilRender));
    compositeTimer.start(msUntilRender);
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

namespace {

// === Cold-Path Debug Helpers ===
// Mark as cold and noinline to keep them out of the instruction cache hot path

#if defined(__GNUC__) || defined(__clang__)
#  define KWIN_COLD __attribute__((cold))
#  define KWIN_NOINLINE __attribute__((noinline))
#else
#  define KWIN_COLD
#  define KWIN_NOINLINE
#endif

/**
 * Sanitize output name for use in filesystem paths.
 * Replaces non-ASCII and special characters with underscores.
 */
static KWIN_COLD KWIN_NOINLINE void sanitizeName(const QString &in, std::string &out)
{
    out.clear();
    const int size = in.size();
    if (size > 0) {
        out.reserve(static_cast<size_t>(size));
    }

    int i = 0;

#if defined(__x86_64__) || defined(_M_X64)
    // AVX2 path: process 16 UTF-16 chars (32 bytes) per iteration
    if (size >= 16 && __builtin_cpu_supports("avx2")) {
        const __m256i lowerA = _mm256_set1_epi16('a');
        const __m256i upperZ = _mm256_set1_epi16('z');
        const __m256i lowerAlpha = _mm256_set1_epi16('A');
        const __m256i upperAlpha = _mm256_set1_epi16('Z');
        const __m256i lower0 = _mm256_set1_epi16('0');
        const __m256i upper9 = _mm256_set1_epi16('9');
        const __m256i underscore = _mm256_set1_epi16('_');
        const __m256i dash = _mm256_set1_epi16('-');
        const __m256i replaceChar = _mm256_set1_epi16('_');

        for (; i + 15 < size; i += 16) {
            // Load 16 UTF-16 chars (unaligned OK for AVX2)
            __m256i chars = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(in.data() + i));

            // Check ranges: a-z, A-Z, 0-9
            __m256i isLowerAlpha = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(lowerA, _mm256_set1_epi16(1))),
                                                     _mm256_cmpgt_epi16(_mm256_add_epi16(upperZ, _mm256_set1_epi16(1)), chars));
            __m256i isUpperAlpha = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(lowerAlpha, _mm256_set1_epi16(1))),
                                                     _mm256_cmpgt_epi16(_mm256_add_epi16(upperAlpha, _mm256_set1_epi16(1)), chars));
            __m256i isDigit = _mm256_and_si256(_mm256_cmpgt_epi16(chars, _mm256_sub_epi16(lower0, _mm256_set1_epi16(1))),
                                                _mm256_cmpgt_epi16(_mm256_add_epi16(upper9, _mm256_set1_epi16(1)), chars));
            __m256i isUnderscore = _mm256_cmpeq_epi16(chars, underscore);
            __m256i isDash = _mm256_cmpeq_epi16(chars, dash);

            __m256i isAllowed = _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(isLowerAlpha, isUpperAlpha), _mm256_or_si256(isDigit, isUnderscore)), isDash);

            // Replace disallowed chars with '_'
            __m256i result = _mm256_blendv_epi8(replaceChar, chars, isAllowed);

            // Narrow UTF-16 → UTF-8 (for ASCII subset, just take low byte)
            // Pack 16-bit → 8-bit using packus (saturate to 0-255)
            __m128i lo = _mm256_castsi256_si128(result);
            __m128i hi = _mm256_extracti128_si256(result, 1);
            __m128i packed = _mm_packus_epi16(lo, hi);

            // Store 16 bytes
            char temp[16];
            _mm_storeu_si128(reinterpret_cast<__m128i*>(temp), packed);
            out.append(temp, 16);
        }
    }
#endif

    // Scalar fallback for remainder
    for (; i < size; ++i) {
        const char16_t ch = in.at(i).unicode();
        const bool isAlphaNum = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9');
        const bool isAllowed = isAlphaNum || ch == '_' || ch == '-';
        out.push_back(isAllowed ? static_cast<char>(ch) : '_');
    }
}

/**
 * Write a single frame's performance data to the debug CSV.
 * Called only when KWIN_LOG_PERFORMANCE_DATA=1.
 */
static KWIN_COLD KWIN_NOINLINE void writeDebugOutput(
    std::optional<std::fstream> &debugOutput,
    Output *output,
    std::optional<RenderTimeSpan> renderTime,
    OutputFrame *frame,
    std::chrono::nanoseconds timestamp,
    std::chrono::nanoseconds safetyMargin,
    PresentationMode mode)
{
    // Open CSV file on first write
    if (!debugOutput && output) {
        std::string sanitized;
        sanitizeName(output->name(), sanitized);
        const std::string filename = "kwin_perf_" + sanitized + ".csv";
        debugOutput = std::fstream(filename, std::ios::out | std::ios::trunc);

        if (debugOutput && debugOutput->is_open()) {
            // Write CSV header
            *debugOutput << "target_pageflip_ns,pageflip_ns,render_start_ns,render_end_ns,"
                         << "safety_margin_ns,refresh_duration_ns,vrr,tearing,predicted_render_ns\n";
        }
    }

    if (debugOutput && debugOutput->is_open()) {
        const auto times = renderTime.value_or(RenderTimeSpan{});
        const bool vrr = (mode == PresentationMode::AdaptiveSync || mode == PresentationMode::AdaptiveAsync);
        const bool tearing = (mode == PresentationMode::Async || mode == PresentationMode::AdaptiveAsync);

        // Write CSV row
        *debugOutput << frame->targetPageflipTime().time_since_epoch().count() << ","
                     << timestamp.count() << ","
                     << times.start.time_since_epoch().count() << ","
                     << times.end.time_since_epoch().count() << ","
                     << safetyMargin.count() << ","
                     << frame->refreshDuration().count() << ","
                     << (vrr ? 1 : 0) << ","
                     << (tearing ? 1 : 0) << ","
                     << frame->predictedRenderTime().count() << "\n";

        // Flush to ensure data is written even if KWin crashes
        debugOutput->flush();
    }
}

#undef KWIN_COLD
#undef KWIN_NOINLINE

} // anonymous namespace

void RenderLoopPrivate::notifyFrameCompleted(
    std::chrono::nanoseconds timestamp,
    std::optional<RenderTimeSpan> renderTime,
    PresentationMode mode,
    OutputFrame *frame)
{
    // Write debug CSV if enabled (cold path, out-of-line)
    if (s_printDebugInfo) [[unlikely]] {
        writeDebugOutput(m_debugOutput, output, renderTime, frame, timestamp, safetyMargin, mode);
    }

    Q_ASSERT(pendingFrameCount > 0);
    pendingFrameCount--;

    // Update last presentation timestamp (with validation)
    notifyVblank(timestamp);

    // Add render time to the journal for future predictions
    if (renderTime) [[likely]] {
        renderJournal.add(renderTime->end - renderTime->start, timestamp);
    }

    // Reschedule if timer is already running (refines estimate with actual render time)
    if (compositeTimer.isActive()) [[likely]] {
        scheduleRepaint(lastPresentationTimestamp);
    }

    // If inhibited or no pending reschedule, skip scheduling
    if (!inhibitCount && pendingReschedule) [[likely]] {
        scheduleNextRepaint();
    }

    // Emit signal for frame statistics and effect timing
    Q_EMIT q->framePresented(q, timestamp, mode);
}

void RenderLoopPrivate::notifyVblank(std::chrono::nanoseconds timestamp)
{
    // Validate timestamp: must not go backwards (system clock may be adjusted)
    if (lastPresentationTimestamp <= timestamp) [[likely]] {
        lastPresentationTimestamp = timestamp;
    } else {
        // Clock went backwards (NTP adjustment, suspend/resume, etc.)
        qCDebug(KWIN_CORE,
                "Got invalid presentation timestamp: %lld ns (current %lld ns). "
                "Clock may have been adjusted. Using steady_clock fallback.",
                static_cast<long long>(timestamp.count()),
                static_cast<long long>(lastPresentationTimestamp.count()));

        // Fallback: use current steady_clock time
        lastPresentationTimestamp = std::chrono::steady_clock::now().time_since_epoch();
    }
}

void RenderLoopPrivate::dispatch()
{
    Q_EMIT q->frameRequested(q);
}

// === RenderLoop Public Interface ===

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
        // First inhibit: stop the timer
        d->compositeTimer.stop();
    }
}

void RenderLoop::uninhibit()
{
    Q_ASSERT(d->inhibitCount > 0);
    d->inhibitCount--;

    if (d->inhibitCount == 0) {
        // Last uninhibit: resume scheduling
        d->scheduleNextRepaint();
    }
}

void RenderLoop::prepareNewFrame()
{
    d->pendingFrameCount++;
}

int RenderLoop::refreshRate() const
{
    return d->refreshRate;
}

void RenderLoop::setRefreshRate(int refreshRate)
{
    // Validate and clamp refresh rate to sane range
    // Range: 1 Hz to 1000 Hz (in millihertz: 1'000 to 1'000'000)
    const int rr = std::clamp(refreshRate, 1'000, 1'000'000);

    if (d->refreshRate == rr) {
        return;
    }

    d->refreshRate = rr;

    // Update cached vblank interval (nanoseconds per refresh)
    // cachedVblankIntervalNs = 1e12 / refreshRate_mHz
    d->cachedVblankIntervalNs = 1'000'000'000'000ull / static_cast<uint64_t>(rr);

    Q_EMIT refreshRateChanged();
}

void RenderLoop::setPresentationSafetyMargin(std::chrono::nanoseconds safetyMargin)
{
    d->safetyMargin = safetyMargin;
}

void RenderLoop::scheduleRepaint(Item *item, RenderLayer *layer, OutputLayer *outputLayer)
{
    const bool vrr = (d->presentationMode == PresentationMode::AdaptiveSync
                      || d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing = (d->presentationMode == PresentationMode::Async
                          || d->presentationMode == PresentationMode::AdaptiveAsync);

    // VRR/Tearing Mode: Gate repaints to the active window's frame timing
    if ((vrr || tearing) && d->output) [[unlikely]] {
        Workspace *const ws = workspace();
        if (ws) [[likely]] {
            Window *const activeWin = ws->activeWindow();
            if (activeWin && (item || layer || outputLayer)) {
                // Check if active window controls refresh rate (frame time ≤ 33ms → ≥30 FPS)
                const bool controlsRefresh = activeWin->isOnOutput(d->output)
                    && activeWin->surfaceItem()
                    && activeWin->surfaceItem()->recursiveFrameTimeEstimation() <= std::chrono::nanoseconds(1'000'000'000 / 30);

                if (controlsRefresh) {
                    SurfaceItem *const surfaceItem = activeWin->surfaceItem();
                    // If repaint is triggered by a different surface, delay it
                    if (surfaceItem && item != surfaceItem && !surfaceItem->isAncestorOf(item)) {
                        d->delayedVrrTimer.start();
                        return;
                    }
                }
            }
        }
    }

    d->delayedVrrTimer.stop();

    // In VRR/tearing mode, limit to 1 pending frame; otherwise use configured limit
    const int effectiveMaxPendingFrameCount = (vrr || tearing) ? 1 : d->maxPendingFrameCount;

    if (d->pendingFrameCount < effectiveMaxPendingFrameCount && !d->inhibitCount) [[likely]] {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    // Cache for 1 vblank to avoid repeated lookups
    const auto now = std::chrono::steady_clock::now().time_since_epoch();
    const auto vblankInterval = std::chrono::nanoseconds{static_cast<int64_t>(d->cachedVblankIntervalNs)};

    if (now - d->vrrCacheTimestamp < vblankInterval) [[likely]] {
        return d->cachedVrrControl;
    }

    Workspace *const ws = workspace();
    bool result = false;
    if (ws) [[likely]] {
        Window *const activeWindow = ws->activeWindow();
        result = activeWindow
            && activeWindow->isOnOutput(d->output)
            && activeWindow->surfaceItem()
            && activeWindow->surfaceItem()->recursiveFrameTimeEstimation() <= std::chrono::nanoseconds(1'000'000'000 / 30);
    }

    d->cachedVrrControl = result;
    d->vrrCacheTimestamp = now;
    return result;
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
    // Clamp to valid range: [1, INT_MAX]
    if (maxCount == 0) {
        d->maxPendingFrameCount = 1;
    } else if (maxCount > static_cast<uint32_t>(std::numeric_limits<int>::max())) {
        d->maxPendingFrameCount = std::numeric_limits<int>::max();
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
