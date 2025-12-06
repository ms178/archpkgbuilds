/*
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>
    SPDX-FileCopyrightText: 2025 Xaver Hugl <xaver.hugl@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "kwin_export.h"

#include <array>
#include <chrono>
#include <cstddef>

namespace KWin
{

class KWIN_EXPORT RenderJournal
{
public:
    explicit RenderJournal();

    void add(std::chrono::nanoseconds renderTime, std::chrono::nanoseconds presentationTimestamp);

    [[nodiscard]] std::chrono::nanoseconds result() const noexcept;

private:
    static constexpr std::size_t kHistorySize = 100;

    std::array<double, kHistorySize> m_history{};
    std::size_t m_count{0};
    std::size_t m_writeIndex{0};
    std::chrono::nanoseconds m_result{0};
};

}
