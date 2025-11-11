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
#include <bit>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>

using namespace std::chrono_literals;

namespace
{
constexpr qint64 kNanosecondsPerMillisecond = 1'000'000;
constexpr qint64 kPredictionSlackNs = 800'000;
constexpr qint64 kEarlyStartSlackNs = 1'000;
constexpr qint64 kMaxFutureLeadNs = static_cast<qint64>(1) << 40;
constexpr uint64_t kPageflipDriftThreshold = 100;
constexpr uint64_t kIntervalsClamp = 1'000'000;
constexpr uint8_t kVrrVsyncStabilityThreshold = 3;
constexpr uint8_t kVrrAdaptiveStabilityThreshold = 3;
constexpr auto kVrrControlThreshold = 33ms;
constexpr int kMaxTimerDelayMs = std::numeric_limits<std::int16_t>::max();
constexpr int kTripleBufferEnterPct = 78;
constexpr int kTripleBufferExitPct = 68;
constexpr int kTripleBufferExitFrames = 2;
}

namespace KWin
{

static const bool s_printDebugInfo = qEnvironmentVariableIntValue("KWIN_LOG_PERFORMANCE_DATA") != 0;

RenderLoopPrivate *RenderLoopPrivate::get(RenderLoop *loop) noexcept
{
    return loop ? loop->d.get() : nullptr;
}

RenderLoopPrivate::RenderLoopPrivate(RenderLoop *q, Output *output)
    : cachedVblankIntervalNs(1'000'000'000'000ull / 60'000ull)
    , vblankIntervalReciprocal64(0)
    , vblankIntervalReciprocal(0)
    , q(q)
    , output(output)
{
    updateReciprocal();
    initializeVrrCapabilities();
}

void RenderLoopPrivate::updateReciprocal() noexcept
{
    if (cachedVblankIntervalNs == 0) [[unlikely]] {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
        return;
    }

    if (cachedVblankIntervalNs < (1ull << 32)) [[likely]] {
        constexpr uint64_t shift64 = 63;
        vblankIntervalReciprocal64 = ((1ull << shift64) + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs;
        reciprocalShift64 = static_cast<uint8_t>(shift64);

        const uint64_t shift = std::bit_width(cachedVblankIntervalNs) + 31;
        const uint64_t numerator = (shift < 64) ? (1ull << shift) : 0;
        vblankIntervalReciprocal = numerator ? ((numerator + cachedVblankIntervalNs - 1) / cachedVblankIntervalNs) : 0;
        reciprocalShift = static_cast<uint8_t>(shift);
    } else {
        vblankIntervalReciprocal = 0;
        reciprocalShift = 0;
        vblankIntervalReciprocal64 = 0;
        reciprocalShift64 = 0;
    }
}

void RenderLoopPrivate::initializeVrrCapabilities()
{
    if (!output) [[unlikely]] {
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

void RenderLoopPrivate::updateVrrContext() noexcept
{
    cachedActiveWindow = nullptr;
    cachedSurfaceItem = nullptr;
    cachedHint = PresentationModeHint::VSync;
    cachedContentType = ContentType::None;
    cachedIsFullScreen = false;
    cachedIsOnOutput = false;

    if (!vrrEnabled || !vrrCapable) [[unlikely]] {
        return;
    }

    Workspace *const ws = workspace();
    if (!ws || !output) [[unlikely]] {
        return;
    }

    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow) [[unlikely]] {
        return;
    }

    cachedActiveWindow = activeWindow;
    cachedIsOnOutput = activeWindow->isOnOutput(output);

    if (!cachedIsOnOutput) [[unlikely]] {
        return;
    }

    cachedIsFullScreen = activeWindow->isFullScreen();

    SurfaceItem *const surfaceItem = activeWindow->surfaceItem();
    if (!surfaceItem) [[unlikely]] {
        return;
    }

    cachedSurfaceItem = surfaceItem;
    cachedContentType = surfaceItem->contentType();

    if (auto *const waylandItem = qobject_cast<SurfaceItemWayland *>(surfaceItem)) {
        if (auto *const surface = waylandItem->surface()) {
            cachedHint = surface->presentationModeHint();
        }
    }
}

PresentationMode RenderLoopPrivate::selectPresentationModeFromContext() const noexcept
{
    if (!vrrEnabled || !vrrCapable) [[unlikely]] {
        return PresentationMode::VSync;
    }

    if (vrrMode != VrrMode::Always) [[likely]] {
        if (!cachedActiveWindow || !cachedIsOnOutput || !cachedIsFullScreen) [[unlikely]] {
            return PresentationMode::VSync;
        }

        if (cachedHint == PresentationModeHint::VSync) [[likely]] {
            return PresentationMode::VSync;
        }
    }

    if (cachedHint == PresentationModeHint::Async) [[unlikely]] {
        return PresentationMode::AdaptiveAsync;
    }

    return PresentationMode::AdaptiveSync;
}

void RenderLoopPrivate::scheduleNextRepaint()
{
    if (Q_UNLIKELY(kwinApp()->isTerminating()) || compositeTimer.isActive() || preparingNewFrame) [[unlikely]] {
        return;
    }
    scheduleRepaint(nextPresentationTimestamp);
}

#if defined(__GNUC__) || defined(__clang__)
__attribute__((always_inline, hot))
#endif
static inline uint64_t fastDivideByVblank(uint64_t numerator,
                                          uint64_t vblankNs,
                                          uint64_t reciprocal64,
                                          uint8_t shift64,
                                          uint64_t reciprocalFallback,
                                          uint8_t shiftFallback) noexcept
{
    if (numerator == 0 || vblankNs == 0) [[unlikely]] {
        return 0;
    }

    if (reciprocal64 != 0 && numerator < (1ull << 32)) [[likely]] {
        return (numerator * reciprocal64) >> shift64;
    }

#if defined(__SIZEOF_INT128__)
    if (reciprocalFallback != 0) [[likely]] {
        return static_cast<uint64_t>(
            (static_cast<__uint128_t>(numerator) * reciprocalFallback) >> shiftFallback
        );
    }
#else
    (void)reciprocalFallback;
    (void)shiftFallback;
#endif

    return numerator / vblankNs;
}

void RenderLoopPrivate::scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp)
{
    pendingReschedule = false;

    const uint64_t vblankIntervalNs = cachedVblankIntervalNs;
    if (vblankIntervalNs == 0) [[unlikely]] {
        return;
    }

    const int64_t nowNs = std::chrono::steady_clock::now().time_since_epoch().count();
    const int64_t lastPresNs = lastPresentationTimestamp.count();
    const int64_t predictedRenderNs = renderJournal.result().count();
    const int64_t safetyNs = safetyMargin.count();
    const int64_t vblankSigned = static_cast<int64_t>(vblankIntervalNs);

    const int64_t maxCompositeNs = vblankSigned * 2;
    const int64_t baseCompositeNs = predictedRenderNs + safetyNs + kPredictionSlackNs;
    int64_t expectedCompositeNs = std::clamp(baseCompositeNs, static_cast<int64_t>(0), maxCompositeNs);

    const PresentationMode targetMode = selectPresentationModeFromContext();
    if (targetMode != presentationMode) [[unlikely]] {
        const uint8_t threshold = (targetMode == PresentationMode::VSync)
            ? kVrrVsyncStabilityThreshold
            : kVrrAdaptiveStabilityThreshold;
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

    int64_t nextPresNs = lastPresNs + vblankSigned;

    if (presentationMode == PresentationMode::VSync) [[likely]] {
        const int64_t sinceLastNs = nowNs - lastPresNs;
        uint64_t flipsSince = 0;
        if (sinceLastNs > 0) [[likely]] {
            flipsSince = fastDivideByVblank(static_cast<uint64_t>(sinceLastNs),
                                            vblankIntervalNs,
                                            vblankIntervalReciprocal64,
                                            reciprocalShift64,
                                            vblankIntervalReciprocal,
                                            reciprocalShift);
        }

        if (flipsSince > kPageflipDriftThreshold) [[unlikely]] {
            const int64_t earlyStart = vblankSigned - kEarlyStartSlackNs;
            if (expectedCompositeNs < earlyStart) {
                expectedCompositeNs = earlyStart;
            }
        }

        const int64_t toTargetNs = lastTargetTimestamp.count() - lastPresNs;
        uint64_t flipsToTarget = 0;
        if (toTargetNs > 0) [[likely]] {
            flipsToTarget = fastDivideByVblank(static_cast<uint64_t>(toTargetNs),
                                               vblankIntervalNs,
                                               vblankIntervalReciprocal64,
                                               reciprocalShift64,
                                               vblankIntervalReciprocal,
                                               reciprocalShift);
        }

        uint64_t flipsInAdvance = 1;
        if (expectedCompositeNs > vblankSigned) [[unlikely]] {
            const uint64_t need = (static_cast<uint64_t>(expectedCompositeNs) + vblankIntervalNs - 1) / vblankIntervalNs;
            const uint64_t maxPending = static_cast<uint64_t>(maxPendingFrameCount > 0 ? maxPendingFrameCount : 1);
            flipsInAdvance = std::min(std::max(need, uint64_t{1}), maxPending);
        }

        const bool highLoad = (expectedCompositeNs * 100) > (vblankSigned * kTripleBufferEnterPct);
        if (highLoad) [[unlikely]] {
            wasTripleBuffering = true;
            doubleBufferingCounter = 0;
            if (flipsInAdvance < 2) {
                flipsInAdvance = 2;
            }
        } else if (wasTripleBuffering) [[unlikely]] {
            if ((expectedCompositeNs * 100) < (vblankSigned * kTripleBufferExitPct)) {
                ++doubleBufferingCounter;
            } else {
                doubleBufferingCounter = 0;
            }

            if (doubleBufferingCounter >= kTripleBufferExitFrames) {
                wasTripleBuffering = false;
            } else if (flipsInAdvance < 2) {
                flipsInAdvance = 2;
            }
        }

        if (compositeTimer.isActive()) [[unlikely]] {
            if (nextPresentationTimestamp.count() > 0) {
                const int64_t deltaNs = nextPresentationTimestamp.count() - lastPresNs;
                uint64_t intervals = 1;
                if (deltaNs > 0) {
                    const uint64_t num = static_cast<uint64_t>(deltaNs) + vblankIntervalNs / 2;
                    intervals = std::clamp(num / vblankIntervalNs, uint64_t{1}, kIntervalsClamp);
                }
                nextPresNs = lastPresNs + static_cast<int64_t>(intervals * vblankIntervalNs);
            } else {
                nextPresNs = lastPresNs + vblankSigned;
            }
        } else {
            const uint64_t flipsTarget = std::max<uint64_t>(flipsSince + flipsInAdvance, flipsToTarget + 1);
            nextPresNs = lastPresNs + static_cast<int64_t>(flipsTarget * vblankIntervalNs);
        }
    } else {
        wasTripleBuffering = false;
        doubleBufferingCounter = 0;

        if (presentationMode == PresentationMode::Async || presentationMode == PresentationMode::AdaptiveAsync) [[unlikely]] {
            nextPresNs = nowNs + expectedCompositeNs;
        } else {
            const int64_t minIntervalNs = vblankSigned;
            nextPresNs = nowNs + expectedCompositeNs;
            const int64_t sinceLastNs = nowNs - lastPresNs;
            if (sinceLastNs < minIntervalNs) [[likely]] {
                const int64_t minNext = lastPresNs + minIntervalNs;
                if (nextPresNs < minNext) {
                    nextPresNs = minNext;
                }
            }
        }
    }

    const int64_t clampLower = nowNs;
    const int64_t clampUpper = nowNs + kMaxFutureLeadNs;
    if (clampUpper < clampLower) [[unlikely]] {
        nextPresNs = std::numeric_limits<int64_t>::max();
    } else {
        nextPresNs = std::clamp(nextPresNs, clampLower, clampUpper);
    }

    nextPresentationTimestamp = std::chrono::nanoseconds{nextPresNs};

    const int64_t nextRenderNs = nextPresNs - expectedCompositeNs;
    int64_t delayNs = nextRenderNs - nowNs;
    if (delayNs < 0) {
        delayNs = 0;
    }

    int64_t delayMs64 = (delayNs + (kNanosecondsPerMillisecond - 1)) / kNanosecondsPerMillisecond;
    int delayMs = static_cast<int>(std::clamp(delayMs64, static_cast<int64_t>(0), static_cast<int64_t>(kMaxTimerDelayMs)));

    const int threshold = (delayMs <= 8) ? 0 : ((delayMs < 16) ? 1 : 2);
    const int delta = (scheduledTimerMs >= 0) ? std::abs(delayMs - scheduledTimerMs) : delayMs;

    if (!compositeTimer.isActive() || delta > threshold) [[likely]] {
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

    if (!inhibitCount && pendingReschedule) [[likely]] {
        scheduleNextRepaint();
    }
}

namespace
{
static void writeDebugOutput(RenderLoopPrivate *d,
                             std::optional<RenderTimeSpan> renderTime,
                             OutputFrame *frame,
                             std::chrono::nanoseconds timestamp,
                             PresentationMode mode)
{
    if (!d->output || !d->m_debugOutput) {
        if (!d->output) {
            return;
        }
        const QString name = d->output->name();
        std::string sanitizedName;
        sanitizedName.reserve(static_cast<size_t>(name.size()));
        for (const QChar ch : name) {
            const char16_t unicode = ch.unicode();
            if ((unicode >= u'a' && unicode <= u'z') ||
                (unicode >= u'A' && unicode <= u'Z') ||
                (unicode >= u'0' && unicode <= u'9') ||
                unicode == u'_' || unicode == u'-') {
                sanitizedName.push_back(static_cast<char>(unicode & 0xFF));
            } else {
                sanitizedName.push_back('_');
            }
        }
        const std::string filename = "kwin_perf_" + sanitizedName + ".csv";
        d->m_debugOutput = std::fstream(filename, std::ios::out | std::ios::trunc);
        if (d->m_debugOutput && d->m_debugOutput->is_open()) {
            *(d->m_debugOutput)
                << "target_pageflip_ns,pageflip_ns,render_start_ns,render_end_ns,"
                << "safety_margin_ns,refresh_duration_ns,vrr,tearing,predicted_render_ns\n"
                << std::flush;
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
                        << frame->predictedRenderTime().count() << '\n';
}
}

void RenderLoopPrivate::notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                                             std::optional<RenderTimeSpan> renderTime,
                                             PresentationMode mode,
                                             OutputFrame *frame)
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

    if (compositeTimer.isActive()) [[unlikely]] {
        scheduleRepaint(lastPresentationTimestamp);
    }

    if (!inhibitCount && pendingReschedule) [[likely]] {
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
    if (event->timerId() == d->compositeTimer.timerId()) [[likely]] {
        d->compositeTimer.stop();
        d->scheduledTimerMs = -1;
        d->dispatch();
    } else if (event->timerId() == d->delayedVrrTimer.timerId()) [[unlikely]] {
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
    if (d->refreshRate == rr) [[unlikely]] {
        return;
    }
    d->refreshRate = rr;
    d->cachedVblankIntervalNs = 1'000'000'000'000ull / static_cast<uint64_t>(rr);
    d->updateReciprocal();
    Q_EMIT refreshRateChanged();

    if (!d->inhibitCount) [[likely]] {
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

    const bool vrr =
        (d->presentationMode == PresentationMode::AdaptiveSync ||
         d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool tearing =
        (d->presentationMode == PresentationMode::Async ||
         d->presentationMode == PresentationMode::AdaptiveAsync);
    const bool checkOwnership = (item || outputLayer) && (vrr || tearing);
    const bool activeControlsVrr = checkOwnership ? activeWindowControlsVrrRefreshRate() : false;

    if ((vrr || tearing) && d->cachedSurfaceItem) [[unlikely]] {
        SurfaceItem *const cachedSurface = d->cachedSurfaceItem;

        if (checkOwnership && activeControlsVrr &&
            item && item != cachedSurface && !cachedSurface->isAncestorOf(item)) [[unlikely]] {

            if (d->pendingFrameCount > 0) {
                const uint64_t delayNs = d->cachedVblankIntervalNs;
                const int delayMsInt = static_cast<int>(
                    (delayNs + (kNanosecondsPerMillisecond - 1)) / kNanosecondsPerMillisecond
                );
                if (delayMsInt > 0 && delayMsInt <= kMaxTimerDelayMs) [[likely]] {
                    d->delayedVrrTimer.start(delayMsInt, Qt::PreciseTimer, this);
                    return;
                }
            }
        }
    }

    d->delayedVrrTimer.stop();

    int effectiveMaxPendingFrameCount = d->maxPendingFrameCount;
    if (vrr || tearing) [[unlikely]] {
        if (d->cachedSurfaceItem) {
            effectiveMaxPendingFrameCount =
                (d->cachedContentType == ContentType::Video) ? 2 : 1;
        } else {
            effectiveMaxPendingFrameCount = 1;
        }
    }

    effectiveMaxPendingFrameCount = std::max(effectiveMaxPendingFrameCount, 1);

    if (d->pendingFrameCount < effectiveMaxPendingFrameCount && !d->inhibitCount) [[likely]] {
        d->scheduleNextRepaint();
    } else {
        d->delayScheduleRepaint();
    }
}

[[nodiscard]] bool RenderLoop::activeWindowControlsVrrRefreshRate() const
{
    Workspace *const ws = workspace();
    if (!ws) [[unlikely]] {
        return false;
    }
    Window *const activeWindow = ws->activeWindow();
    if (!activeWindow || !d->output || !activeWindow->isOnOutput(d->output)) [[unlikely]] {
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
    if (mode != d->presentationMode) [[unlikely]] {
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
