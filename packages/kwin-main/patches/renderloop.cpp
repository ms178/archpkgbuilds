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

    // CRITICAL OPTIMIZATION: Use cached vblank interval as integer nanoseconds.
    // Compiler (Clang-21 -O3) will optimize divisions by constants into multiply+shift.
    // Do NOT use floating-point reciprocal—it loses precision and causes frame count errors.
    const std::chrono::nanoseconds vblankInterval{static_cast<int64_t>(cachedVblankIntervalNs)};
    const std::chrono::nanoseconds currentTime(std::chrono::steady_clock::now().time_since_epoch());

    // Estimate expected compositing time with safety margin and scheduler slop
    // Cap at 2× vblank to prevent pathological cases from stalling indefinitely
    std::chrono::nanoseconds expectedCompositingTime = std::min(renderJournal.result() + safetyMargin + 1ms, 2 * vblankInterval);

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        // === VSync Mode: Pageflips occur at vblank boundaries ===

        // Calculate how many vblanks have passed since last presentation
        // CRITICAL: Use integer division for exact frame counting
        const auto sinceLast = currentTime - lastPresentationTimestamp;
        uint64_t pageflipsSince = 0;
        if (sinceLast.count() > 0 && vblankInterval.count() > 0) [[likely]] {
            // Integer division—compiler optimizes with reciprocal multiply when vblankInterval is constant
            pageflipsSince = static_cast<uint64_t>(sinceLast.count()) / static_cast<uint64_t>(vblankInterval.count());
        }

        if (pageflipsSince > 100) [[unlikely]] {
            // GPU likely entered low-power state → render time will spike
            // Start compositing very early to absorb the wake-up latency
            expectedCompositingTime = std::max(vblankInterval - 1us, expectedCompositingTime);
        }

        // Calculate vblanks from last presentation to the previously targeted timestamp
        // Use rounding (add half-interval before division) for temporal coherence
        const auto toTarget = lastTargetTimestamp - lastPresentationTimestamp;
        uint64_t pageflipsSinceLastToTarget = 0;
        if (toTarget.count() > 0 && vblankInterval.count() > 0) [[likely]] {
            const int64_t toTargetPlusHalf = toTarget.count() + (vblankInterval.count() / 2);
            if (toTargetPlusHalf > 0) {
                pageflipsSinceLastToTarget = static_cast<uint64_t>(toTargetPlusHalf) / static_cast<uint64_t>(vblankInterval.count());
            }
        }

        // Determine how many vblanks ahead to schedule compositing
        // Formula: ceil(expectedCompositingTime / vblankInterval)
        uint64_t pageflipsInAdvance = 1; // Default: double buffering
        if (vblankInterval.count() > 0) [[likely]] {
            pageflipsInAdvance = (static_cast<uint64_t>(expectedCompositingTime.count()) + static_cast<uint64_t>(vblankInterval.count()) - 1)
                                 / static_cast<uint64_t>(vblankInterval.count());
            if (pageflipsInAdvance < 1) {
                pageflipsInAdvance = 1;
            }
            if (pageflipsInAdvance > static_cast<uint64_t>(maxPendingFrameCount)) {
                pageflipsInAdvance = static_cast<uint64_t>(maxPendingFrameCount);
            }
        }

        // === Triple Buffering Hysteresis ===
        // Switching from double→triple causes a frame drop, so apply hysteresis
        // to avoid thrashing when render times oscillate near the threshold.
        if (pageflipsInAdvance > 1) {
            // Render time requires triple buffering → switch immediately
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
        } else if (wasTripleBuffering) {
            // Currently triple buffering but render time suggests double buffering is viable
            // Wait for 10 consecutive stable frames before switching back
            const auto vblank95 = vblankInterval - (vblankInterval / 20); // 0.95 × vblank

            if (doubleBufferingCounter >= 10) {
                // Stable enough → switch to double buffering
                wasTripleBuffering = false;
                pageflipsInAdvance = 1;
                doubleBufferingCounter = 0;
            } else if (expectedCompositingTime >= vblank95) {
                // Render time too high → stay in triple buffering, reset counter
                pageflipsInAdvance = 2;
                doubleBufferingCounter = 0;
                expectedCompositingTime = vblankInterval; // Pessimistic estimate
            } else {
                // Render time acceptable → increment stability counter
                doubleBufferingCounter++;
                pageflipsInAdvance = 2;
                expectedCompositingTime = vblankInterval; // Pessimistic estimate
            }
        }

        // === Schedule Next Presentation ===
        if (compositeTimer.isActive()) {
            // Timer already running: recompute target timestamp with updated render time
            // Keep the same vblank interval to avoid unnecessary frame drops
            const auto delta = nextPresentationTimestamp - lastPresentationTimestamp;
            uint32_t intervalsSinceLastTimestamp = 1;
            if (delta.count() > 0 && vblankInterval.count() > 0) [[likely]] {
                const int64_t deltaRounded = delta.count() + (vblankInterval.count() / 2);
                if (deltaRounded > 0) {
                    intervalsSinceLastTimestamp = static_cast<uint32_t>(
                        static_cast<uint64_t>(deltaRounded) / static_cast<uint64_t>(vblankInterval.count())
                    );
                    if (intervalsSinceLastTimestamp < 1) {
                        intervalsSinceLastTimestamp = 1;
                    }
                }
            }
            nextPresentationTimestamp = lastPresentationTimestamp + intervalsSinceLastTimestamp * vblankInterval;
        } else {
            // Schedule for the furthest of:
            // 1. pageflipsSince + pageflipsInAdvance (accounting for current time)
            // 2. pageflipsSinceLastToTarget + 1 (maintaining temporal coherence)
            const uint64_t targetVblanks = std::max(pageflipsSince + pageflipsInAdvance, pageflipsSinceLastToTarget + 1);
            nextPresentationTimestamp = lastPresentationTimestamp + targetVblanks * vblankInterval;
        }

    } else {
        // === Adaptive Sync / Tearing Mode ===
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;

        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) {
            // Tearing enabled: present ASAP, no vblank alignment
            nextPresentationTimestamp = currentTime;
        } else {
            // Adaptive sync (VRR): wait at least one vblank interval
            // TODO: Query EDID minimum refresh rate and use as lower bound
            const std::chrono::nanoseconds vblankInterval{static_cast<int64_t>(cachedVblankIntervalNs)};
            nextPresentationTimestamp = std::max(currentTime, lastPresentationTimestamp + vblankInterval);
        }
    }

    // === Start QTimer ===
    // Calculate when to start compositing (presentation time minus render time)
    const std::chrono::nanoseconds nextRenderTimestamp = nextPresentationTimestamp - expectedCompositingTime;
    const std::chrono::nanoseconds timeUntilRender = nextRenderTimestamp - currentTime;

    // Qt timers use millisecond precision; round to nearest ms (Qt::PreciseTimer provides best effort)
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

    for (int i = 0; i < size; ++i) {
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
    Workspace *const ws = workspace();
    if (!ws) [[unlikely]] {
        return false;
    }

    Window *const activeWindow = ws->activeWindow();
    return activeWindow
        && activeWindow->isOnOutput(d->output)
        && activeWindow->surfaceItem()
        && activeWindow->surfaceItem()->recursiveFrameTimeEstimation() <= std::chrono::nanoseconds(1'000'000'000 / 30);
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
