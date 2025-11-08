/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_commit_thread.h"
#include "drm_commit.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "utils/envvar.h"
#include "utils/realtime.h"

#include <algorithm>
#include <bit>
#include <span>
#include <thread>

using namespace std::chrono_literals;

namespace KWin
{

DrmCommitThread::DrmCommitThread(DrmGpu *gpu, const QString &name)
    : m_gpu(gpu)
    , m_targetPageflipTime(std::chrono::steady_clock::now())
{
    if (!gpu->atomicModeSetting()) {
        return;
    }

    m_commits.reserve(32);
    m_commitsToDelete.reserve(128);

    m_thread.reset(QThread::create([this]() {
        const auto thread = QThread::currentThread();
        gainRealTime();
        while (true) {
            if (thread->isInterruptionRequested()) {
                return;
            }
            std::unique_lock lock(m_mutex);
            bool timeout = false;
            if (m_committed) {
                timeout = m_commitPending.wait_for(lock, DrmGpu::s_pageflipTimeout) == std::cv_status::timeout;
            } else if (m_commits.empty()) {
                m_commitPending.wait(lock);
            }
            if (m_committed) {
                if (timeout) {
                    m_ping = false;
                    lock.unlock();
                    QMetaObject::invokeMethod(this, &DrmCommitThread::handlePing, Qt::ConnectionType::QueuedConnection);
                    lock.lock();
                    while (!m_ping) {
                        m_pong.wait(lock);
                    }
                    if (m_committed) {
                        qCCritical(KWIN_DRM, "Pageflip timed out! This is a bug in the %s kernel driver", qPrintable(m_gpu->driverName()));
                        if (m_gpu->isAmdgpu()) {
                            qCCritical(KWIN_DRM, "Please report this at https://gitlab.freedesktop.org/drm/amd/-/issues");
                        } else if (m_gpu->isNVidia()) {
                            qCCritical(KWIN_DRM, "Please report this at https://forums.developer.nvidia.com/c/gpu-graphics/linux");
                        } else if (m_gpu->isI915()) {
                            qCCritical(KWIN_DRM, "Please report this at https://gitlab.freedesktop.org/drm/i915/kernel/-/issues");
                        }
                        qCCritical(KWIN_DRM, "With the output of 'sudo dmesg' and 'journalctl --user-unit plasma-kwin_wayland --boot 0'");
                        m_pageflipTimeoutDetected = true;
                    } else {
                        qCWarning(KWIN_DRM, "The main thread was hanging temporarily!");
                    }
                }
                continue;
            }
            if (m_commits.empty()) {
                continue;
            }
            const auto now = std::chrono::steady_clock::now();
            if (m_targetPageflipTime > now + m_safetyMargin) {
                lock.unlock();
                std::this_thread::sleep_until(m_targetPageflipTime - m_safetyMargin);
                lock.lock();
                if (m_commits.empty()) {
                    continue;
                }
            }
            optimizeCommits(m_targetPageflipTime);
            if (m_commits.empty()) [[unlikely]] {
                continue;
            }
            if (!m_commits.front()->isReadyFor(m_targetPageflipTime)) {
                if (m_vrr || m_tearing) {
                    m_targetPageflipTime += 50us;
                } else {
                    m_targetPageflipTime += m_minVblankInterval;
                }
                continue;
            }
            if (m_commits.front()->allowedVrrDelay() && m_vrr) {
                bool allDelay = true;
                std::chrono::nanoseconds lowestDelay = std::chrono::nanoseconds::max();

                for (const auto &c : m_commits) {
                    if (auto delay = c->allowedVrrDelay()) {
                        lowestDelay = std::min(lowestDelay, *delay);
                    } else {
                        allDelay = false;
                    }
                }

                const auto delayedTarget = m_lastPageflip + lowestDelay;
                if (allDelay) {
                    if (m_commitPending.wait_until(lock, delayedTarget) == std::cv_status::no_timeout) {
                        continue;
                    }
                } else {
                    bool waitTimeout = true;
                    while (std::chrono::steady_clock::now() < delayedTarget && waitTimeout
                           && !m_commits.empty() && m_commits.front()->allowedVrrDelay().has_value()) {
                        waitTimeout = m_commitPending.wait_for(lock, 50us) == std::cv_status::timeout;
                        if (m_commits.empty()) {
                            break;
                        }
                        optimizeCommits(delayedTarget);
                    }
                    if (!waitTimeout) {
                        continue;
                    }
                }
                if (m_commits.empty()) {
                    continue;
                }
            }
            submit();
        }
    }));
    m_thread->setObjectName(name);
    m_thread->start();
}

void DrmCommitThread::submit()
{
    if (m_commits.empty()) [[unlikely]] {
        return;
    }

    DrmAtomicCommit *commit = m_commits.front().get();
    const auto vrr = commit->isVrr();
    const bool success = commit->commit();

    if (success) {
        m_vrr = vrr.value_or(m_vrr);
        m_tearing = commit->isTearing();
        m_committed = std::move(m_commits.front());
        m_commits.erase(m_commits.begin());

        m_lastCommitTime = std::chrono::steady_clock::now();
        const auto targetTimestamp = m_targetPageflipTime - m_baseSafetyMargin;
        const auto safetyDifference = targetTimestamp - m_lastCommitTime;

        if (safetyDifference < std::chrono::nanoseconds::zero()) {
            m_additionalSafetyMargin -= safetyDifference;
        } else {
            m_additionalSafetyMargin -= safetyDifference / 10;
        }

        const auto maximumReasonableMargin = std::min(std::chrono::nanoseconds(3ms), m_minVblankInterval / 2);
        m_additionalSafetyMargin = std::clamp(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        if (m_commits.size() > 1) {
            while (m_commits.size() > 1) {
                auto toMerge = std::move(m_commits[1]);
                m_commits.erase(m_commits.begin() + 1);
                commit->merge(toMerge.get());
                m_commitsToDelete.push_back(std::move(toMerge));
            }
            if (commit->test()) {
                submit();
                return;
            }
        }

        m_commitsToDelete.reserve(m_commitsToDelete.size() + m_commits.size());
        for (auto &c : m_commits) {
            m_commitsToDelete.push_back(std::move(c));
        }
        m_commits.clear();
        qCWarning(KWIN_DRM) << "atomic commit failed:" << strerror(errno);
    }

    m_mutex.unlock();
    QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::ConnectionType::QueuedConnection);
    m_mutex.lock();
}

static std::unique_ptr<DrmAtomicCommit> mergeCommits(std::span<const std::unique_ptr<DrmAtomicCommit>> commits)
{
    auto ret = std::make_unique<DrmAtomicCommit>(*commits.front());
    for (const auto &onTop : commits.subspan(1)) {
        ret->merge(onTop.get());
    }
    return ret;
}

void DrmCommitThread::optimizeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() <= 1) {
        return;
    }

    if (m_commits.front()->areBuffersReadable()) {
        auto firstNotReady = m_commits.begin() + 1;
        while (firstNotReady != m_commits.end() && (*firstNotReady)->isReadyFor(pageflipTarget)) {
            ++firstNotReady;
        }

        if (firstNotReady != m_commits.begin() + 1) {
            auto merged = mergeCommits(std::span(m_commits.begin(), firstNotReady));
            const size_t mergeCount = static_cast<size_t>(std::distance(m_commits.begin(), firstNotReady));
            m_commitsToDelete.reserve(m_commitsToDelete.size() + mergeCount);
            for (auto it = m_commits.begin(); it != firstNotReady; ++it) {
                m_commitsToDelete.push_back(std::move(*it));
            }
            m_commits.erase(m_commits.begin() + 1, firstNotReady);
            m_commits.front() = std::move(merged);
        }
    }

    for (auto it = m_commits.begin(); it != m_commits.end();) {
        const auto startIt = it;
        auto &startCommit = *startIt;

        auto nextIt = startIt + 1;
        while (nextIt != m_commits.end() &&
               (*nextIt)->isReadyFor(pageflipTarget) &&
               startCommit->modifiedPlanes() == (*nextIt)->modifiedPlanes()) {
            ++nextIt;
        }

        if (nextIt == startIt + 1) {
            ++it;
            continue;
        }

        auto merged = mergeCommits(std::span(startIt, nextIt));
        const size_t mergeCount = static_cast<size_t>(std::distance(startIt, nextIt));
        m_commitsToDelete.reserve(m_commitsToDelete.size() + mergeCount);
        for (auto mergeIt = startIt; mergeIt != nextIt; ++mergeIt) {
            m_commitsToDelete.push_back(std::move(*mergeIt));
        }
        startCommit = std::move(merged);
        it = m_commits.erase(startIt + 1, nextIt);
    }
}

DrmCommitThread::~DrmCommitThread()
{
    if (m_thread) {
        {
            std::unique_lock lock(m_mutex);
            m_thread->requestInterruption();
            m_commitPending.notify_all();
            m_ping = true;
            m_pong.notify_all();
        }
        m_thread->wait();
    }
    if (m_committed) {
        m_committed->setDefunct();
        m_gpu->addDefunctCommit(std::move(m_committed));
    }
}

void DrmCommitThread::addCommit(std::unique_ptr<DrmAtomicCommit> &&commit)
{
    std::unique_lock lock(m_mutex);

    const auto now = std::chrono::steady_clock::now();

    TimePoint newTarget;
    if (m_tearing) {
        newTarget = now;
    } else if (m_vrr && now >= m_lastPageflip + m_minVblankInterval) {
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    m_targetPageflipTime = std::max(m_targetPageflipTime, newTarget);
    commit->setDeadline(m_targetPageflipTime - m_safetyMargin);

    m_commits.push_back(std::move(commit));
    m_commitPending.notify_all();
}

void DrmCommitThread::setPendingCommit(std::unique_ptr<DrmLegacyCommit> &&commit)
{
    std::unique_lock lock(m_mutex);
    m_committed = std::move(commit);
}

void DrmCommitThread::clearDroppedCommits()
{
    std::unique_lock lock(m_mutex);
    m_commitsToDelete.clear();
}

static const std::chrono::microseconds s_safetyMarginMinimum{environmentVariableIntValue("KWIN_DRM_OVERRIDE_SAFETY_MARGIN").value_or(1500)};

void DrmCommitThread::setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime)
{
    std::unique_lock lock(m_mutex);

    if (maximum == 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid maximum refresh rate: 0, using 60Hz fallback";
        maximum = 60;
    }

    m_minVblankInterval = std::chrono::nanoseconds(1'000'000'000'000ull / maximum);

    const uint64_t vblankNs = static_cast<uint64_t>(m_minVblankInterval.count());
    if (vblankNs > 0 && vblankNs < (1ull << 31)) {
        constexpr uint64_t shift = 63;
        m_vblankReciprocal = ((1ull << shift) + vblankNs - 1) / vblankNs;
        m_vblankReciprocalShift = static_cast<uint8_t>(shift);
    } else {
        m_vblankReciprocal = 0;
        m_vblankReciprocalShift = 0;
    }

    m_baseSafetyMargin = vblankTime + s_safetyMarginMinimum;
    m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
}

void DrmCommitThread::pageFlipped(std::chrono::nanoseconds timestamp)
{
    std::unique_lock lock(m_mutex);

    if (m_pageflipTimeoutDetected) [[unlikely]] {
        qCCritical(KWIN_DRM, "Pageflip arrived after all, %lums after the commit",
                   std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - m_lastCommitTime).count());
        m_pageflipTimeoutDetected = false;
    }

    m_lastPageflip = TimePoint(timestamp);
    m_committed.reset();

    if (!m_commits.empty()) {
        m_targetPageflipTime = estimateNextVblank(std::chrono::steady_clock::now());
        m_commitPending.notify_all();
    }
}

bool DrmCommitThread::pageflipsPending()
{
    std::unique_lock lock(m_mutex);
    return !m_commits.empty() || m_committed;
}

TimePoint DrmCommitThread::estimateNextVblank(TimePoint now) const
{
    if (m_minVblankInterval <= std::chrono::nanoseconds::zero()) [[unlikely]] {
        return now + std::chrono::milliseconds(16);
    }

    const auto elapsed = (now >= m_lastPageflip)
        ? (now - m_lastPageflip)
        : std::chrono::nanoseconds::zero();

    const uint64_t elapsedNs = static_cast<uint64_t>(elapsed.count());
    const uint64_t vblankNs = static_cast<uint64_t>(m_minVblankInterval.count());

    uint64_t pageflipsSince = 0;
    if (m_vblankReciprocal > 0 && elapsedNs < (1ull << 32)) [[likely]] {
        pageflipsSince = (elapsedNs * m_vblankReciprocal) >> m_vblankReciprocalShift;
    } else if (vblankNs > 0) {
        pageflipsSince = elapsedNs / vblankNs;
    }

    constexpr uint64_t maxReasonablePageflips = 10000;
    const uint64_t clampedPageflips = std::min(pageflipsSince, maxReasonablePageflips);

    return m_lastPageflip + m_minVblankInterval * (clampedPageflips + 1);
}

std::chrono::nanoseconds DrmCommitThread::safetyMargin() const
{
    std::unique_lock lock(m_mutex);
    return m_safetyMargin;
}

void DrmCommitThread::handlePing()
{
    m_gpu->dispatchEvents();
    std::unique_lock lock(m_mutex);
    m_ping = true;
    m_pong.notify_one();
}

}
