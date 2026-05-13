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

void RenderJournal::add(std::chrono::nanoseconds renderTime, std::chrono::nanoseconds presentationTimestamp)
{
    constexpr int64_t kNominalFrameNs = 16'666'667;      // 60 Hz fallback
    constexpr int64_t kMinDtNs = 1'000'000;             // 1 ms
    constexpr int64_t kMaxDtNs = 500'000'000;           // 500 ms

    constexpr int64_t kMinSampleNs = 50'000;            // 0.05 ms
    constexpr int64_t kMaxSampleNs = 150'000'000;       // 150 ms
    constexpr int64_t kMaxPredictionNs = 200'000'000;   // 200 ms hard cap

    constexpr double kMeanTimeConstantNs = 180'000'000.0; // 180 ms
    constexpr double kVarTimeConstantNs = 700'000'000.0;  // 700 ms

    constexpr double kMeanAlphaMin = 0.02;
    constexpr double kMeanAlphaMax = 0.30;
    constexpr double kVarAlphaMin = 0.01;
    constexpr double kVarAlphaMax = 0.20;

    constexpr double kRiseBoost = 1.8;    // react faster to worsening render cost
    constexpr double kRiseAlphaCap = 0.55;

    constexpr double kSigma = 2.5;        // conservative but not over-pessimistic
    constexpr double kHeadroomMinNs = 100'000.0; // 0.1 ms
    constexpr double kHeadroomRatio = 0.02;      // 2% of mean
    constexpr double kStdFloorNs = 120'000.0;    // 0.12 ms base jitter floor
    constexpr double kStdFloorRatio = 0.015;     // 1.5% of mean

    const int64_t sampleNs = std::clamp<int64_t>(renderTime.count(), kMinSampleNs, kMaxSampleNs);

    int64_t dtNs = kNominalFrameNs;
    if (m_lastPresentationTimestamp && presentationTimestamp > *m_lastPresentationTimestamp) {
        dtNs = std::clamp<int64_t>((presentationTimestamp - *m_lastPresentationTimestamp).count(), kMinDtNs, kMaxDtNs);
    }
    m_lastPresentationTimestamp = presentationTimestamp;

    if (m_result.count() == 0) {
        m_meanNs = static_cast<double>(sampleNs);
        const double seedStd = std::max(kStdFloorNs, m_meanNs * 0.10);
        m_varianceNs2 = seedStd * seedStd;

        const double headroom = std::max(kHeadroomMinNs, m_meanNs * kHeadroomRatio);
        const double prediction = std::clamp(m_meanNs + kSigma * seedStd + headroom,
                                             static_cast<double>(kMinSampleNs),
                                             static_cast<double>(kMaxPredictionNs));
        m_result = std::chrono::nanoseconds{static_cast<int64_t>(std::llround(prediction))};
        return;
    }

    const double baseMeanAlpha = std::clamp(static_cast<double>(dtNs) / kMeanTimeConstantNs, kMeanAlphaMin, kMeanAlphaMax);
    const double diffBefore = static_cast<double>(sampleNs) - m_meanNs;

    // Faster on regression, normal on improvement.
    const double meanAlpha = diffBefore > 0.0
        ? std::min(baseMeanAlpha * kRiseBoost, kRiseAlphaCap)
        : baseMeanAlpha;

    m_meanNs += meanAlpha * diffBefore;

    const double err = static_cast<double>(sampleNs) - m_meanNs;
    const double varAlpha = std::clamp(static_cast<double>(dtNs) / kVarTimeConstantNs, kVarAlphaMin, kVarAlphaMax);
    m_varianceNs2 += varAlpha * ((err * err) - m_varianceNs2);

    const double stddev = std::sqrt(std::max(m_varianceNs2, 0.0));
    const double robustStd = std::max(stddev, std::max(kStdFloorNs, m_meanNs * kStdFloorRatio));
    const double headroom = std::max(kHeadroomMinNs, m_meanNs * kHeadroomRatio);

    const double prediction = std::clamp(m_meanNs + kSigma * robustStd + headroom,
                                         static_cast<double>(kMinSampleNs),
                                         static_cast<double>(kMaxPredictionNs));
    m_result = std::chrono::nanoseconds{static_cast<int64_t>(std::llround(prediction))};
}

std::chrono::nanoseconds RenderJournal::result() const noexcept
{
    return m_result;
}

} // namespace KWin
