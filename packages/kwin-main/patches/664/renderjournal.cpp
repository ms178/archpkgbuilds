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
                        std::chrono::nanoseconds /*presentationTimestamp*/)
{
    const double val = std::clamp(static_cast<double>(renderTime.count()), 0.0, 1e12);

    m_history[m_writeIndex] = val;
    m_writeIndex = (m_writeIndex + 1) & kHistoryMask;

    if (m_count < kHistorySize) [[unlikely]] {
        ++m_count;
    }

    const std::size_t n = m_count;
    const double invN = 1.0 / static_cast<double>(n);

    double sum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        sum += m_history[i];
    }
    const double mean = sum * invN;

    double varAccum = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double d = m_history[i] - mean;
        varAccum += d * d;
    }

    const double stddev = std::sqrt(varAccum * invN);
    m_result = std::chrono::nanoseconds{
        static_cast<std::int64_t>(std::round(mean + 3.0 * stddev))};
}

std::chrono::nanoseconds RenderJournal::result() const noexcept
{
    return m_result;
}

}
