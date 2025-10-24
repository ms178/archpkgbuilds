/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "renderbackend.h"
#include "renderjournal.h"
#include "renderloop.h"

#include <QTimer>
#include <chrono>
#include <fstream>
#include <optional>
#include <cstdint>

namespace KWin
{

class SurfaceItem;
class OutputFrame;

// Cache-aligned for false sharing prevention.
// Field order optimized for Raptor Lake 64-byte cache lines.
class alignas(64) KWIN_EXPORT RenderLoopPrivate
{
public:
    static RenderLoopPrivate *get(RenderLoop *loop);
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);

    void dispatch();
    void delayScheduleRepaint();
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp,
                              std::optional<RenderTimeSpan> renderTime,
                              PresentationMode mode,
                              OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);

    // === CACHE LINE 0 (0-63 bytes): HOT READ/WRITE FIELDS ===
    RenderLoop *const q;                            // 0-7: const after construction
    Output *const output;                           // 8-15: const after construction
    int refreshRate;                                // 16-19: read every scheduleRepaint
    int pendingFrameCount;                          // 20-23: read/write every frame
    int inhibitCount;                               // 24-27: checked every scheduleRepaint
    int maxPendingFrameCount;                       // 28-31: compared every scheduleRepaint
    PresentationMode presentationMode;              // 32-35: checked every scheduleRepaint
    bool pendingReschedule;                         // 36: toggled frequently
    bool wasTripleBuffering;                        // 37: hysteresis state
    int16_t doubleBufferingCounter;                 // 38-39: hysteresis counter (narrowed from int)
    uint32_t padding0_;                             // 40-43: explicit padding for alignment
    uint64_t cachedVblankIntervalNs;                // 44-51: read every scheduleRepaint
    std::chrono::nanoseconds lastPresentationTimestamp; // 52-59: read/write every frame
    // 60-63: natural struct padding

    // === CACHE LINE 1 (64-127 bytes): WARM FIELDS ===
    mutable bool cachedVrrControl;
    mutable std::chrono::nanoseconds vrrCacheTimestamp;
    std::chrono::nanoseconds nextPresentationTimestamp; // 64-71: written every scheduleRepaint
    std::chrono::nanoseconds safetyMargin;              // 72-79: read every scheduleRepaint
    RenderJournal renderJournal;                        // 80-...: updated every notifyFrameCompleted

    // === CACHE LINE 2+ (128+ bytes): COLD FIELDS ===
    QTimer compositeTimer;                          // Accessed during schedule/timeout only
    QTimer delayedVrrTimer;                         // Accessed only in VRR gating (rare)
    std::optional<std::fstream> m_debugOutput;      // Debug only (cold)
};

static_assert(alignof(RenderLoopPrivate) == 64, "RenderLoopPrivate must be 64-byte aligned");
static_assert(sizeof(void*) == 8, "Assumes 64-bit pointers");
static_assert(sizeof(std::chrono::nanoseconds) == 8, "Assumes nanoseconds is int64_t");

} // namespace KWin
