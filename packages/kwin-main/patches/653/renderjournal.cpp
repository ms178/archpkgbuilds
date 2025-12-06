/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-FileCopyrightText: 2025 Xaver Hugl <xaver.hugl@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "renderjournal.h"

#include <algorithm>
#include <cmath>

namespace KWin
{

RenderJournal::RenderJournal() = default;

void RenderJournal::add(std::chrono::nanoseconds renderTime,
                        [[maybe_unused]] std::chrono::nanoseconds presentationTimestamp)
{
    const std::int64_t clampedNs = std::max(renderTime.count(), std::int64_t{0});
    const double renderTimeNs = static_cast<double>(clampedNs);

    m_history[m_writeIndex] = renderTimeNs;
    m_writeIndex = (m_writeIndex + 1) & kHistoryMask;

    if (m_count < kHistorySize) {
        ++m_count;
    }

    const std::size_t n = m_count;
    double sum = 0.0;
    double sumSq = 0.0;

    for (std::size_t i = 0; i < n; ++i) {
        const double val = m_history[i];
        sum += val;
        sumSq += val * val;
    }

    const double invN = 1.0 / static_cast<double>(n);
    const double mean = sum * invN;
    const double variance = std::max(sumSq * invN - mean * mean, 0.0);
    const double stddev = std::sqrt(variance);

    m_result = std::chrono::nanoseconds{static_cast<std::int64_t>(std::round(mean + 3.0 * stddev))};
}

std::chrono::nanoseconds RenderJournal::result() const noexcept
{
    return m_result;
}

}
