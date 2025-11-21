/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include <QObject>
#include <QThread>
#include <condition_variable>
#include <mutex>
#include <vector>
#include <cstdint>

namespace KWin
{

class DrmGpu;
class DrmCommit;
class DrmAtomicCommit;
class DrmLegacyCommit;

using TimePoint = std::chrono::steady_clock::time_point;

class DrmCommitThread : public QObject
{
    Q_OBJECT
public:
    explicit DrmCommitThread(DrmGpu *gpu, const QString &name);
    ~DrmCommitThread() override;

    void addCommit(std::unique_ptr<DrmAtomicCommit> &&commit);
    void setPendingCommit(std::unique_ptr<DrmLegacyCommit> &&commit);

    void setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime);
    void pageFlipped(std::chrono::nanoseconds timestamp);
    [[nodiscard]] bool pageflipsPending();
    [[nodiscard]] std::chrono::nanoseconds safetyMargin() const;

private:
    void clearDroppedCommits();
    [[nodiscard]] TimePoint estimateNextVblank(TimePoint now) const;
    void optimizeCommits(TimePoint pageflipTarget);
    void submit();
    void handlePing();

    DrmGpu *const m_gpu;
    std::unique_ptr<DrmCommit> m_committed;
    std::vector<std::unique_ptr<DrmAtomicCommit>> m_commits;
    std::unique_ptr<QThread> m_thread;

    TimePoint m_lastPageflip;
    TimePoint m_targetPageflipTime;
    TimePoint m_lastCommitTime;

    std::chrono::nanoseconds m_minVblankInterval{0};
    std::chrono::nanoseconds m_safetyMargin{0};
    std::chrono::nanoseconds m_baseSafetyMargin{0};
    std::chrono::nanoseconds m_additionalSafetyMargin{std::chrono::milliseconds(1)};

    uint64_t m_vblankReciprocal = 0;
    uint8_t m_vblankReciprocalShift = 0;

    std::vector<std::unique_ptr<DrmAtomicCommit>> m_commitsToDelete;

    bool m_vrr = false;
    bool m_tearing = false;
    bool m_ping = false;
    bool m_pageflipTimeoutDetected = false;

    alignas(64) mutable std::mutex m_mutex;
    alignas(64) std::condition_variable m_commitPending;
    std::condition_variable m_pong;
};

} // namespace KWin
