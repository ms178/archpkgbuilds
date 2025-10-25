/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "renderbackend.h"
#include "renderjournal.h"
#include "renderloop.h"

#include <QBasicTimer>

#include <fstream>
#include <optional>
#include <cstdint>

namespace KWin
{

class SurfaceItem;
class OutputFrame;

class alignas(64) KWIN_EXPORT RenderLoopPrivate {
public:
    // Cache Line 0 (0-63 bytes): Hottest read-write fields accessed every scheduleRepaint() call
    std::chrono::nanoseconds lastPresentationTimestamp{0};    // 8 bytes (read/write every frame)
    std::chrono::nanoseconds nextPresentationTimestamp{0};    // 8 bytes (read/write every frame)
    uint64_t cachedVblankIntervalNs;                          // 8 bytes (read every frame)
    int pendingFrameCount = 0;                                // 4 bytes (read/write every frame)
    int inhibitCount = 0;                                     // 4 bytes (read every frame)
    PresentationMode presentationMode = PresentationMode::VSync; // 4 bytes (read every frame)
    int16_t scheduledTimerMs = -1;                            // 2 bytes (read/write per schedule)
    int16_t doubleBufferingCounter = 0;                       // 2 bytes (read/write per VSync schedule)
    bool pendingReschedule = false;                           // 1 byte (read/write per frame)
    bool wasTripleBuffering = false;                          // 1 byte (read/write per VSync schedule)
    bool preparingNewFrame = false;                           // 1 byte (read/write per frame)
    // 43 bytes used, 21 bytes padding to next cache line

    // Cache Line 1 (64-127 bytes): Warm fields and const pointers
    RenderLoop *const q;                                      // 8 bytes (read rarely, const)
    Output *const output;                                     // 8 bytes (read rarely, const)
    std::chrono::nanoseconds safetyMargin{0};                 // 8 bytes (read per frame, written rarely)
    int maxPendingFrameCount = 1;                             // 4 bytes (read per frame, written rarely)
    int refreshRate = 60'000;                                 // 4 bytes (read rarely)
    QBasicTimer compositeTimer;                               // 4 bytes (read/write per frame)
    QBasicTimer delayedVrrTimer;                              // 4 bytes (read/write occasionally)
    // 40 bytes used, 24 bytes padding

    // Cache Line 2+ (128+ bytes): Cold fields
    RenderJournal renderJournal;                              // ~64 bytes (read/write per frame, but self-contained)
    std::optional<std::fstream> m_debugOutput;                // ~32 bytes (debug only, almost never accessed)

    static RenderLoopPrivate *get(RenderLoop *loop);
    explicit RenderLoopPrivate(RenderLoop *q, Output *output);

    void dispatch();
    void delayScheduleRepaint();
    void scheduleNextRepaint();
    void scheduleRepaint(std::chrono::nanoseconds lastTargetTimestamp);
    void notifyFrameDropped();
    void notifyFrameCompleted(std::chrono::nanoseconds timestamp, std::optional<RenderTimeSpan> renderTime, PresentationMode mode, OutputFrame *frame);
    void notifyVblank(std::chrono::nanoseconds timestamp);
};

} // namespace KWin
