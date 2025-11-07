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
#include <cmath>
#include <span>
#include <thread>

using namespace std::chrono_literals;

namespace KWin
{

static constexpr int kMaxRetries = 3;
static constexpr std::chrono::microseconds kRetryDelays[] = {10us, 50us, 200us};

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
                    QMetaObject::invokeMethod(this, &DrmCommitThread::handlePing, Qt::QueuedConnection);
                    lock.lock();
                    while (!m_ping) {
                        m_pong.wait(lock);
                    }
                    if (m_committed) {
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
            if (m_commits.empty()) {
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
                    if (c->allowedVrrDelay()) {
                        lowestDelay = std::min(lowestDelay, *c->allowedVrrDelay());
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
    if (m_commits.empty()) {
        return;
    }

    DrmAtomicCommit *commit = m_commits.front().get();
    const auto vrr = commit->isVrr();

    bool success = false;
    int retryCount = 0;

    while (!success && retryCount <= kMaxRetries) {
        success = commit->commit();

        if (!success) {
            const int err = errno;
            if (err == EBUSY && retryCount < kMaxRetries) {
                std::this_thread::sleep_for(kRetryDelays[retryCount]);
                ++retryCount;
                continue;
            }
            break;
        }
    }

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
        m_additionalSafetyMargin = (std::clamp)(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        const int finalErrno = errno;
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
        if (finalErrno == EBUSY) {
            qCWarning(KWIN_DRM) << "Atomic commit failed after" << kMaxRetries << "retries: EBUSY (kernel display pipeline busy)";
        } else if (finalErrno == EINVAL) {
            qCWarning(KWIN_DRM) << "Atomic commit failed: EINVAL (invalid property combination - likely CRTC state conflict)";
        } else {
            qCWarning(KWIN_DRM) << "atomic commit failed:" << strerror(finalErrno);
        }
    }
    QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
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

    if (m_commits.size() == 1) {
        return;
    }

    std::unique_ptr<DrmAtomicCommit> front;
    if (m_commits.front() && m_commits.front()->isReadyFor(pageflipTarget)) {
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

    std::vector<size_t> toErase;
    toErase.reserve(m_commits.size());

    for (size_t i = 0; i < m_commits.size(); ++i) {
        auto &commit = m_commits[i];
        if (!commit || !commit->isReadyFor(pageflipTarget)) {
            continue;
        }

        const auto &planes = commit->modifiedPlanes();
        bool skipping = false;
        for (size_t j = 0; j < i; ++j) {
            if (m_commits[j]) {
                const auto &otherPlanes = m_commits[j]->modifiedPlanes();
                for (DrmPlane *plane : planes) {
                    if (otherPlanes.contains(plane)) {
                        skipping = true;
                        break;
                    }
                }
                if (skipping) {
                    break;
                }
            }
        }

        if (skipping) {
            continue;
        }

        std::unique_ptr<DrmAtomicCommit> duplicate;
        if (front) {
            duplicate = std::make_unique<DrmAtomicCommit>(*front);
            duplicate->merge(commit.get());
            if (!duplicate->test()) {
                m_commitsToDelete.push_back(std::move(duplicate));
                continue;
            }
        } else {
            if (!commit->test()) {
                continue;
            }
            duplicate = std::make_unique<DrmAtomicCommit>(*commit);
        }

        bool success = true;
        for (size_t j = 0; j < m_commits.size(); ++j) {
            if (j != i && m_commits[j]) {
                duplicate->merge(m_commits[j].get());
                if (!duplicate->test()) {
                    success = false;
                    break;
                }
            }
        }

        m_commitsToDelete.push_back(std::move(duplicate));

        if (success) {
            if (front) {
                front->merge(commit.get());
            } else {
                front = std::make_unique<DrmAtomicCommit>(*commit);
            }
            m_commitsToDelete.push_back(std::move(commit));
            toErase.push_back(i);
        }
    }

    if (!toErase.empty()) {
        std::vector<std::unique_ptr<DrmAtomicCommit>> remaining;
        remaining.reserve(m_commits.size() - toErase.size());
        size_t eraseIdx = 0;
        for (size_t i = 0; i < m_commits.size(); ++i) {
            if (eraseIdx < toErase.size() && i == toErase[eraseIdx]) {
                ++eraseIdx;
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
    if (m_tearing) {
        newTarget = now;
    } else if (m_vrr && now >= m_lastPageflip + m_minVblankInterval) {
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    m_targetPageflipTime = (std::max)(m_targetPageflipTime, newTarget);
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

static const std::chrono::microseconds s_safetyMarginMinimum{environmentVariableIntValue("KWIN_DRM_OVERRIDE_SAFETY_MARGIN").value_or(1500)};

void DrmCommitThread::setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime)
{
    std::unique_lock lock(m_mutex);
    if (maximum == 0) {
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
    if (m_pageflipTimeoutDetected) {
        const auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - m_lastCommitTime).count();
        qCCritical(KWIN_DRM, "Pageflip arrived after all, %lums after the commit", elapsedMs);
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
    if (m_minVblankInterval <= std::chrono::nanoseconds::zero()) {
        return now + std::chrono::milliseconds(16);
    }

    const auto elapsed = now >= m_lastPageflip ? now - m_lastPageflip : std::chrono::nanoseconds::zero();

    const double elapsedSeconds = static_cast<double>(elapsed.count()) * 1e-9;
    const double pageflipsFloat = elapsedSeconds * m_vblankIntervalInv;
    const uint64_t pageflipsSince = pageflipsFloat >= 0.0 ? static_cast<uint64_t>(pageflipsFloat) : 0;

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
