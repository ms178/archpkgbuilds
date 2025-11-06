/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Performance Optimization

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_commit_thread.h"
#include "drm_commit.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "utils/envvar.h"
#include "utils/realtime.h"

#include <algorithm>
#include <cmath>
#include <ranges>
#include <span>
#include <thread>

#if defined(__linux__)
#include <pthread.h>
#include <sched.h>
#endif

#if defined(__x86_64__) || defined(_M_X64)
#include <emmintrin.h>
#endif

using namespace std::chrono_literals;

namespace KWin
{

static bool setPCoreAffinity()
{
#if defined(__linux__)
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);

    const long numCpus = sysconf(_SC_NPROCESSORS_ONLN);
    if (numCpus <= 0) {
        return false;
    }

    const int pCoreThreads = std::min(16L, numCpus);
    for (int i = 0; i < pCoreThreads; ++i) {
        CPU_SET(i, &cpuset);
    }

    if (pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset) == 0) {
        qCDebug(KWIN_DRM) << "Commit thread pinned to P-cores (CPUs 0-" << (pCoreThreads - 1) << ")";
        return true;
    } else {
        qCWarning(KWIN_DRM) << "Failed to set CPU affinity";
        return false;
    }
#else
    return false;
#endif
}

static void preciseSleep(std::chrono::steady_clock::time_point deadline)
{
    constexpr auto spinThreshold = 150us;

    const auto now = std::chrono::steady_clock::now();
    if (deadline <= now) [[unlikely]] {
        return;
    }

    const auto sleepUntil = deadline - spinThreshold;
    if (sleepUntil > now) [[likely]] {
        std::this_thread::sleep_until(sleepUntil);
    }

#if defined(__x86_64__) || defined(_M_X64)
    while (std::chrono::steady_clock::now() < deadline) [[likely]] {
        _mm_pause();
    }
#else
    while (std::chrono::steady_clock::now() < deadline) [[likely]] {
        std::this_thread::yield();
    }
#endif
}

DrmCommitThread::DrmCommitThread(DrmGpu *gpu, const QString &name)
    : m_gpu(gpu)
    , m_targetPageflipTime(std::chrono::steady_clock::now())
{
    if (!gpu->atomicModeSetting()) {
        return;
    }

    m_commitsToDelete.reserve(64);

    m_thread.reset(QThread::create([this]() {
        const auto thread = QThread::currentThread();
        gainRealTime();
        setPCoreAffinity();

        while (true) [[likely]] {
            if (thread->isInterruptionRequested()) [[unlikely]] {
                return;
            }

            std::unique_lock lock(m_mutex);
            bool timeout = false;

            if (m_committed) [[unlikely]] {
                timeout = m_commitPending.wait_for(lock, DrmGpu::s_pageflipTimeout) == std::cv_status::timeout;
            } else if (m_commits.empty()) [[likely]] {
                m_commitPending.wait(lock);
            }

            if (m_committed) [[unlikely]] {
                if (timeout) [[unlikely]] {
                    m_ping = false;
                    lock.unlock();
                    QMetaObject::invokeMethod(this, &DrmCommitThread::handlePing, Qt::ConnectionType::QueuedConnection);
                    lock.lock();

                    while (!m_ping) {
                        m_pong.wait(lock);
                    }

                    if (m_committed) [[unlikely]] {
                        const auto driverName = m_gpu->driverName();
                        qCCritical(KWIN_DRM, "Pageflip timed out! This is a bug in the %s kernel driver", qPrintable(driverName));
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

            if (m_commits.empty()) [[likely]] {
                continue;
            }

            const auto now = std::chrono::steady_clock::now();
            if (m_targetPageflipTime > now + m_safetyMargin) [[unlikely]] {
                lock.unlock();
                preciseSleep(m_targetPageflipTime - m_safetyMargin);
                lock.lock();
                if (m_commits.empty()) [[likely]] {
                    continue;
                }
            }

            optimizeCommits(m_targetPageflipTime);

            if (m_commits.empty()) [[unlikely]] {
                continue;
            }

            if (!m_commits.front()->isReadyFor(m_targetPageflipTime)) [[unlikely]] {
                if (m_vrr || m_tearing) [[unlikely]] {
                    m_targetPageflipTime += 50us;
                } else {
                    m_targetPageflipTime += m_minVblankInterval;
                }
                continue;
            }

            if (m_commits.front()->allowedVrrDelay() && m_vrr) [[unlikely]] {
                const bool allDelay = std::ranges::all_of(m_commits, [](const auto &commit) {
                    return commit->allowedVrrDelay().has_value();
                });

                auto delays = m_commits | std::views::filter([](const auto &commit) {
                    return commit->allowedVrrDelay().has_value();
                }) | std::views::transform([](const auto &commit) {
                    return *commit->allowedVrrDelay();
                });

                const std::chrono::nanoseconds lowestDelay = *std::ranges::min_element(delays);
                const auto delayedTarget = m_lastPageflip + lowestDelay;

                if (allDelay) {
                    if (m_commitPending.wait_until(lock, delayedTarget) == std::cv_status::no_timeout) {
                        continue;
                    }
                } else {
                    bool waitTimeout = true;
                    while (std::chrono::steady_clock::now() < delayedTarget && waitTimeout &&
                           !m_commits.empty() && m_commits.front()->allowedVrrDelay().has_value()) {
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

                if (m_commits.empty()) [[likely]] {
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

#if defined(__x86_64__) || defined(_M_X64)
    if (m_commits.size() > 1 && m_commits[1]) [[unlikely]] {
        __builtin_prefetch(m_commits[1].get(), 0, 3);
    }
#endif

    DrmAtomicCommit *commit = m_commits.front().get();
    const auto vrr = commit->isVrr();
    const bool success = commit->commit();

    if (success) [[likely]] {
        m_vrr = vrr.value_or(m_vrr);
        m_tearing = commit->isTearing();
        m_committed = std::move(m_commits.front());
        m_commits.erase(m_commits.begin());

        m_lastCommitTime = std::chrono::steady_clock::now();
        const auto targetTimestamp = m_targetPageflipTime - m_baseSafetyMargin;
        const auto safetyDifference = targetTimestamp - m_lastCommitTime;

        if (safetyDifference < std::chrono::nanoseconds::zero()) [[unlikely]] {
            m_additionalSafetyMargin -= safetyDifference;
        } else {
            m_additionalSafetyMargin -= safetyDifference / 10;
        }

        const auto maximumReasonableMargin = std::min<std::chrono::nanoseconds>(3ms, m_minVblankInterval / 2);
        m_additionalSafetyMargin = std::clamp(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        if (m_commits.size() > 1) [[unlikely]] {
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

    QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::ConnectionType::QueuedConnection);
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
    if (m_commits.size() <= 1) [[likely]] {
        return;
    }

#if defined(__x86_64__) || defined(_M_X64)
    if (!m_commits.empty() && m_commits[0]) {
        __builtin_prefetch(m_commits[0].get(), 0, 3);
    }
#endif

    if (m_commits.front()->areBuffersReadable()) [[likely]] {
        const auto firstNotReady = std::find_if(m_commits.begin() + 1, m_commits.end(),
            [pageflipTarget](const auto &commit) {
                return !commit->isReadyFor(pageflipTarget);
            });

        if (firstNotReady != m_commits.begin() + 1) [[unlikely]] {
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

#if defined(__x86_64__) || defined(_M_X64)
        if (startCommit) {
            __builtin_prefetch(startCommit.get(), 0, 3);
        }
#endif

        const auto firstNotSamePlaneNotReady = std::find_if(startIt + 1, m_commits.end(),
            [&startCommit, pageflipTarget](const auto &commit) {
                return startCommit->modifiedPlanes() != commit->modifiedPlanes() ||
                       !commit->isReadyFor(pageflipTarget);
            });

        if (firstNotSamePlaneNotReady == startIt + 1) [[likely]] {
            ++it;
            continue;
        }

        auto merged = mergeCommits(std::span(startIt, firstNotSamePlaneNotReady));
        const size_t mergeCount = static_cast<size_t>(std::distance(startIt, firstNotSamePlaneNotReady));

        m_commitsToDelete.reserve(m_commitsToDelete.size() + mergeCount);
        for (auto mergeIt = startIt; mergeIt != firstNotSamePlaneNotReady; ++mergeIt) {
            m_commitsToDelete.push_back(std::move(*mergeIt));
        }

        startCommit = std::move(merged);
        it = m_commits.erase(startIt + 1, firstNotSamePlaneNotReady);
    }

    if (m_commits.size() == 1) [[likely]] {
        return;
    }

    std::unique_ptr<DrmAtomicCommit> front;
    if (m_commits.front() && m_commits.front()->isReadyFor(pageflipTarget)) [[likely]] {
        front = std::make_unique<DrmAtomicCommit>(*m_commits.front());
        m_commitsToDelete.push_back(std::move(m_commits.front()));
        m_commits.erase(m_commits.begin());
    }

    if (m_commits.empty()) {
        if (front) {
            m_commits.push_back(std::move(front));
        }
        return;
    }

    struct CommitInfo {
        size_t index;
        uint64_t planeMask;
        bool isReady;
    };

    std::vector<CommitInfo> readyCommits;
    readyCommits.reserve(m_commits.size());

    uint64_t planeIdMap[256] = {};
    uint32_t nextBit = 0;

    for (size_t i = 0; i < m_commits.size(); ++i) {
        auto &commit = m_commits[i];
        if (!commit || !commit->isReadyFor(pageflipTarget)) {
            continue;
        }

        CommitInfo info{};
        info.index = i;
        info.isReady = true;
        info.planeMask = 0;

        const auto &planes = commit->modifiedPlanes();
        for (const auto *plane : planes) {
            const uint32_t planeId = plane->id();
            if (planeId < 256) [[likely]] {
                if (planeIdMap[planeId] == 0 && nextBit < 64) {
                    planeIdMap[planeId] = uint64_t(1) << nextBit;
                    nextBit++;
                }
                info.planeMask |= planeIdMap[planeId];
            }
        }

        readyCommits.push_back(info);
    }

    std::vector<size_t> toRemove;
    toRemove.reserve(m_commits.size());

    for (const auto &readyInfo : readyCommits) {
        bool hasConflict = false;
        for (size_t j = 0; j < readyInfo.index; ++j) {
            if (!m_commits[j]) {
                continue;
            }

            const auto &otherPlanes = m_commits[j]->modifiedPlanes();
            uint64_t otherMask = 0;
            for (const auto *plane : otherPlanes) {
                const uint32_t planeId = plane->id();
                if (planeId < 256) {
                    otherMask |= planeIdMap[planeId];
                }
            }

            if ((readyInfo.planeMask & otherMask) != 0) {
                hasConflict = true;
                break;
            }
        }

        if (hasConflict) {
            continue;
        }

        std::unique_ptr<DrmAtomicCommit> testMerge;
        if (front) {
            testMerge = std::make_unique<DrmAtomicCommit>(*front);
            testMerge->merge(m_commits[readyInfo.index].get());
            if (!testMerge->test()) {
                m_commitsToDelete.push_back(std::move(testMerge));
                continue;
            }
        } else {
            if (!m_commits[readyInfo.index]->test()) {
                continue;
            }
            testMerge = std::make_unique<DrmAtomicCommit>(*m_commits[readyInfo.index]);
        }

        if (front) {
            front->merge(m_commits[readyInfo.index].get());
        } else {
            front = std::make_unique<DrmAtomicCommit>(*m_commits[readyInfo.index]);
        }

        m_commitsToDelete.push_back(std::move(testMerge));
        m_commitsToDelete.push_back(std::move(m_commits[readyInfo.index]));
        toRemove.push_back(readyInfo.index);
    }

    if (!toRemove.empty()) {
        std::vector<std::unique_ptr<DrmAtomicCommit>> remaining;
        remaining.reserve(m_commits.size() - toRemove.size());

        size_t removeIdx = 0;
        for (size_t i = 0; i < m_commits.size(); ++i) {
            if (removeIdx < toRemove.size() && i == toRemove[removeIdx]) {
                ++removeIdx;
            } else if (m_commits[i]) {
                remaining.push_back(std::move(m_commits[i]));
            }
        }
        m_commits = std::move(remaining);
    }

    if (front) {
        m_commits.insert(m_commits.begin(), std::move(front));
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
    const auto now = std::chrono::steady_clock::now();

    std::unique_lock lock(m_mutex);

    TimePoint newTarget;
    if (m_tearing) [[unlikely]] {
        newTarget = now;
    } else if (m_vrr && now >= m_lastPageflip + m_minVblankInterval) [[unlikely]] {
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    m_targetPageflipTime = std::max(m_targetPageflipTime, newTarget);
    const auto deadline = m_targetPageflipTime - m_safetyMargin;
    commit->setDeadline(deadline);

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

static const std::chrono::microseconds s_safetyMarginMinimum{
    environmentVariableIntValue("KWIN_DRM_OVERRIDE_SAFETY_MARGIN").value_or(1500)
};

void DrmCommitThread::setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime)
{
    std::unique_lock lock(m_mutex);

    if (maximum == 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid maximum refresh rate: 0, using 60Hz fallback";
        maximum = 60;
    }

    m_minVblankInterval = std::chrono::nanoseconds(1'000'000'000ull / maximum);
    m_vblankIntervalInv = static_cast<double>(maximum) / 1e9;
    m_baseSafetyMargin = vblankTime + s_safetyMarginMinimum;
    m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
}

void DrmCommitThread::pageFlipped(std::chrono::nanoseconds timestamp)
{
    std::unique_lock lock(m_mutex);

    if (m_pageflipTimeoutDetected) [[unlikely]] {
        const auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - m_lastCommitTime).count();
        qCCritical(KWIN_DRM, "Pageflip arrived after all, %lums after the commit", elapsedMs);
        m_pageflipTimeoutDetected = false;
    }

    m_lastPageflip = TimePoint(timestamp);
    m_committed.reset();

    if (!m_commits.empty()) [[likely]] {
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

    const auto elapsed = now >= m_lastPageflip ?
        now - m_lastPageflip : std::chrono::nanoseconds::zero();

    constexpr std::chrono::nanoseconds maxReasonableElapsed{std::chrono::seconds(10)};
    const auto clampedElapsed = std::min(elapsed, maxReasonableElapsed);

    const double elapsedSeconds = static_cast<double>(clampedElapsed.count()) * 1e-9;
    const double pageflipsFloat = elapsedSeconds * m_vblankIntervalInv;

    constexpr double maxSafePageflips = 100000.0;
    const uint64_t pageflipsSince = (pageflipsFloat >= 0.0 && pageflipsFloat < maxSafePageflips) ?
        static_cast<uint64_t>(pageflipsFloat) : 0;

    return m_lastPageflip + m_minVblankInterval * (pageflipsSince + 1);
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
