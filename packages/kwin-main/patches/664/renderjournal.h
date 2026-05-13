/*
 * SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#pragma once

#include "kwin_export.h"

#include <chrono>
#include <optional>

namespace KWin
{

/**
 * The RenderJournal class tracks render time history and estimates a safe
 * render budget for the next frame.
 */
class KWIN_EXPORT RenderJournal
{
public:
    explicit RenderJournal();

    /**
     * Adds a new render-time sample.
     *
     * @param renderTime             Time spent rendering a frame.
     * @param presentationTimestamp  Monotonic presentation timestamp of that frame.
     */
    void add(std::chrono::nanoseconds renderTime, std::chrono::nanoseconds presentationTimestamp);

    /**
     * Returns the predicted render budget for the next frame.
     */
    [[nodiscard]] std::chrono::nanoseconds result() const noexcept;

private:
    // Predicted render budget consumed by the scheduler.
    std::chrono::nanoseconds m_result{0};

    // Internal model state (nanoseconds domain).
    double m_meanNs{0.0};
    double m_varianceNs2{0.0};

    // Last monotonic presentation timestamp used for time-aware adaptation.
    std::optional<std::chrono::nanoseconds> m_lastPresentationTimestamp;
};

} // namespace KWin
