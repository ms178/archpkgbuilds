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
#include <cerrno>
#include <chrono>
#include <cstring>
#include <thread>
#include <time.h>

using namespace std::chrono_literals;

namespace KWin
{

namespace
{

constexpr std::chrono::microseconds kVrrPollInterval{50};
constexpr uint64_t kMaxPageflipSpan = 10000;
constexpr std::chrono::milliseconds kDefaultFrameInterval{16};
constexpr size_t kInitialCommitCapacity = 16;
constexpr size_t kInitialDeleteCapacity = 64;
constexpr int kMaxSubmitRetries = 3;
constexpr std::chrono::nanoseconds kMaxSafetyMargin{3'000'000};

[[nodiscard]]
inline std::chrono::nanoseconds clampNs(std::chrono::nanoseconds val,
                                        std::chrono::nanoseconds lo,
                                        std::chrono::nanoseconds hi) noexcept
{
    return val < lo ? lo : (val > hi ? hi : val);
}

} // namespace

static const std::chrono::microseconds s_safetyMarginMinimum{
    environmentVariableIntValue("KWIN_DRM_OVERRIDE_SAFETY_MARGIN").value_or(1500)};

DrmCommitThread::DrmCommitThread(DrmGpu *gpu, const QString &name)
    : m_targetPageflipTime(std::chrono::steady_clock::now())
    , m_gpu(gpu)
{
    if (!gpu->atomicModeSetting()) {
        return;
    }

    m_commits.reserve(kInitialCommitCapacity);
    m_commitsToDelete.reserve(kInitialDeleteCapacity);

    m_thread.reset(QThread::create([this]() {
        const auto thread = QThread::currentThread();
        gainRealTime();

        for (;;) {
            if (thread->isInterruptionRequested()) [[unlikely]] {
                return;
            }

            std::unique_lock<std::mutex> lock(m_mutex);
            bool timeout = false;

            if (m_committed) {
                timeout = m_commitPending.wait_for(lock, DrmGpu::s_pageflipTimeout) == std::cv_status::timeout;
            } else if (m_commits.empty()) {
                m_commitPending.wait(lock, [this, thread]() noexcept {
                    return thread->isInterruptionRequested() || m_committed || !m_commits.empty();
                });
                if (thread->isInterruptionRequested()) [[unlikely]] {
                    return;
                }
            }

            if (m_committed) {
                if (timeout) [[unlikely]] {
                    m_ping.store(false, std::memory_order_release);
                    lock.unlock();
                    m_gpu->dispatchEvents();
                    lock.lock();

                    m_ping.store(true, std::memory_order_release);
                    m_pong.notify_one();

                    m_pong.wait(lock, [this]() noexcept {
                        return m_ping.load(std::memory_order_acquire);
                    });

                    if (m_committed) {
                        qCCritical(KWIN_DRM, "Pageflip timed out! This is a bug in the %s kernel driver",
                                   qPrintable(m_gpu->driverName()));
                        if (m_gpu->isAmdgpu()) {
                            qCCritical(KWIN_DRM, "Please report this at https://gitlab.freedesktop.org/drm/amd/-/issues");
                        } else if (m_gpu->isNVidia()) {
                            qCCritical(KWIN_DRM, "Please report this at https://forums.developer.nvidia.com/c/gpu-graphics/linux");
                        } else if (m_gpu->isI915()) {
                            qCCritical(KWIN_DRM, "Please report this at https://gitlab.freedesktop.org/drm/i915/kernel/-/issues");
                        }
                        qCCritical(KWIN_DRM, "Include 'sudo dmesg' and 'journalctl --user-unit plasma-kwin_wayland --boot 0'");
                        m_pageflipTimeoutDetected.store(true, std::memory_order_release);
                    }
                }
                continue;
            }

            if (m_commits.empty()) {
                continue;
            }

            const auto now = std::chrono::steady_clock::now();
            const auto scheduledTarget = m_targetPageflipTime;
            const auto wakeTarget = scheduledTarget - m_safetyMargin;

            if (wakeTarget > now) {
                const auto waitResult = m_commitPending.wait_until(lock, wakeTarget, [this, thread, scheduledTarget]() noexcept {
                    return thread->isInterruptionRequested() || m_committed || m_commits.empty() || m_targetPageflipTime != scheduledTarget;
                });

                if (thread->isInterruptionRequested()) [[unlikely]] {
                    return;
                }
                if (!waitResult) {
                    continue;
                }
                if (m_commits.empty()) {
                    continue;
                }
            }

            optimizeCommits(m_targetPageflipTime);
            if (m_commits.empty()) [[unlikely]] {
                continue;
            }

            DrmAtomicCommit *frontCommit = m_commits.front().get();
            if (!frontCommit) [[unlikely]] {
                m_commits.erase(m_commits.begin());
                continue;
            }

            if (!frontCommit->isReadyFor(m_targetPageflipTime)) {
                const bool vrrOrTearing = m_vrr.load(std::memory_order_acquire) ||
                                          m_tearing.load(std::memory_order_acquire);
                m_targetPageflipTime += vrrOrTearing ? kVrrPollInterval : m_minVblankInterval;
                continue;
            }

            const auto frontDelay = frontCommit->allowedVrrDelay();
            if (frontDelay && m_vrr.load(std::memory_order_acquire)) {
                std::chrono::nanoseconds lowestDelay = *frontDelay;
                bool allDelay = true;

                const size_t commitCount = m_commits.size();
                for (size_t i = 1; i < commitCount; ++i) {
                    const auto delay = m_commits[i]->allowedVrrDelay();
                    if (delay) {
                        if (*delay < lowestDelay) {
                            lowestDelay = *delay;
                        }
                    } else {
                        allDelay = false;
                    }
                }

                lowestDelay = clampNs(lowestDelay, 0ns, 10'000'000ns);
                const auto delayedTarget = m_lastPageflip + lowestDelay;

                if (allDelay) {
                    if (m_commitPending.wait_until(lock, delayedTarget) != std::cv_status::timeout) {
                        continue;
                    }
                } else {
                    auto loopNow = std::chrono::steady_clock::now();
                    bool interrupted = false;
                    while (loopNow < delayedTarget) {
                        const auto waitResultInner = m_commitPending.wait_for(lock, kVrrPollInterval);
                        if (waitResultInner == std::cv_status::no_timeout) {
                            interrupted = true;
                            break;
                        }
                        if (m_commits.empty()) {
                            break;
                        }
                        optimizeCommits(delayedTarget);
                        if (m_commits.empty()) {
                            break;
                        }
                        if (!m_commits.front()->allowedVrrDelay().has_value()) {
                            break;
                        }
                        loopNow = std::chrono::steady_clock::now();
                    }
                    if (interrupted || m_commits.empty() || loopNow < delayedTarget) {
                        continue;
                    }
                }

                if (m_commits.empty() || !m_commits.front()->allowedVrrDelay().has_value()) {
                    continue;
                }
            }

            submit();
        }
    }));

    if (!m_thread) [[unlikely]] {
        qCCritical(KWIN_DRM) << "Failed to create DRM commit thread";
        return;
    }

    m_thread->setObjectName(name);
    m_thread->start(QThread::TimeCriticalPriority);
}

void DrmCommitThread::submit()
{
    if (m_commits.empty()) [[unlikely]] {
        return;
    }

    DrmAtomicCommit *commit = m_commits.front().get();
    if (!commit) [[unlikely]] {
        m_commits.erase(m_commits.begin());
        return;
    }

    bool testedOk = commit->test();
    int savedErrno = testedOk ? 0 : errno;

    if (!testedOk) [[unlikely]] {
        thread_local std::chrono::steady_clock::time_point lastWarnTime{};
        thread_local uint32_t suppressed = 0;

        const auto now = std::chrono::steady_clock::now();
        const bool shouldLog = (now - lastWarnTime) > 500ms;
        if (shouldLog) {
            lastWarnTime = now;
            if (suppressed > 0) {
                qCWarning(KWIN_DRM) << "Commit test failed before submission, errno:" << savedErrno
                                   << strerror(savedErrno) << "(+" << suppressed << " similar suppressed)";
                suppressed = 0;
            } else {
                qCWarning(KWIN_DRM) << "Commit test failed before submission, errno:" << savedErrno << strerror(savedErrno);
            }
        } else {
            ++suppressed;
        }

        for (int retry = 0; retry < kMaxSubmitRetries && m_commits.size() > 1; ++retry) {
            if (!m_commits[1]) [[unlikely]] {
                m_commits.erase(m_commits.begin() + 1);
                continue;
            }

            auto toMerge = std::move(m_commits[1]);
            m_commits.erase(m_commits.begin() + 1);
            commit->merge(toMerge.get());
            m_commitsToDelete.push_back(std::move(toMerge));

            testedOk = commit->test();
            if (testedOk) {
                break;
            }
            savedErrno = errno;
        }

        if (!testedOk) {
            m_commitsToDelete.push_back(std::move(m_commits.front()));
            m_commits.erase(m_commits.begin());
            QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
            return;
        }
    }

    const auto vrr = commit->isVrr();
    const bool success = commit->commit();

    if (success) [[likely]] {
        if (vrr.has_value()) {
            m_vrr.store(*vrr, std::memory_order_release);
        }
        m_tearing.store(commit->isTearing(), std::memory_order_release);
        m_committed = std::move(m_commits.front());
        m_commits.erase(m_commits.begin());

        m_lastCommitTime = std::chrono::steady_clock::now();
        const auto targetTimestamp = m_targetPageflipTime - m_baseSafetyMargin;
        const auto safetyDifference = targetTimestamp - m_lastCommitTime;

        if (safetyDifference < 0ns) {
            const auto penalty = -safetyDifference;
            const auto delta = penalty - m_additionalSafetyMargin;
            m_additionalSafetyMargin += delta / 8;
        } else {
            m_additionalSafetyMargin -= m_additionalSafetyMargin / 8;
        }

        const auto halfVblank = m_minVblankInterval / 2;
        const auto maximumReasonableMargin = std::min(kMaxSafetyMargin, halfVblank);
        m_additionalSafetyMargin = clampNs(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        const int commitErrno = errno;
        qCWarning(KWIN_DRM) << "Atomic commit failed: errno =" << commitErrno
                           << "(" << strerror(commitErrno) << ")"
                           << "; tearing =" << commit->isTearing();

        const size_t totalToDelete = m_commitsToDelete.size() + m_commits.size();
        m_commitsToDelete.reserve(totalToDelete);
        for (auto &c : m_commits) {
            if (c) {
                m_commitsToDelete.push_back(std::move(c));
            }
        }
        m_commits.clear();
    }

    QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
}

bool DrmCommitThread::tryMergeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() < 2) {
        return false;
    }

    size_t firstNotReady = 1;
    while (firstNotReady < m_commits.size()) {
        if (!m_commits[firstNotReady]) {
            break;
        }
        if (!m_commits[firstNotReady]->isReadyFor(pageflipTarget)) {
            break;
        }
        if (m_commits[firstNotReady]->isTearing()) {
            break;
        }
        ++firstNotReady;
    }

    if (firstNotReady <= 1) {
        return false;
    }

    auto baseCommit = std::move(m_commits.front());
    const size_t mergeCount = firstNotReady - 1;

    if (m_commitsToDelete.capacity() < m_commitsToDelete.size() + mergeCount) {
        m_commitsToDelete.reserve(m_commitsToDelete.size() + mergeCount);
    }

    for (size_t i = 1; i < firstNotReady; ++i) {
        if (m_commits[i]) {
            baseCommit->merge(m_commits[i].get());
            m_commitsToDelete.push_back(std::move(m_commits[i]));
        }
    }

    if (!baseCommit->test()) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Merged commit test failed, reverting optimization";
        m_commitsToDelete.push_back(std::move(baseCommit));
        m_commits.erase(m_commits.begin(), m_commits.begin() + static_cast<ptrdiff_t>(firstNotReady));
        return false;
    }

    m_commits.erase(m_commits.begin(), m_commits.begin() + static_cast<ptrdiff_t>(firstNotReady));
    m_commits.insert(m_commits.begin(), std::move(baseCommit));
    return true;
}

void DrmCommitThread::optimizeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() <= 1) {
        return;
    }

    if (m_commits.front()->isTearing()) {
        return;
    }

    bool optimized = m_commits.front()->areBuffersReadable() && tryMergeCommits(pageflipTarget);

    size_t write = 1;
    for (size_t read = 1; read < m_commits.size(); ++read) {
        if (!m_commits[read]) {
            continue;
        }

        auto &base = m_commits[read];
        const bool baseTearing = base->isTearing();
        const auto &basePlanes = base->modifiedPlanes();

        if (!basePlanes.empty()) {
            size_t next = read + 1;
            while (next < m_commits.size() && m_commits[next]) {
                if (!m_commits[next]->isReadyFor(pageflipTarget) ||
                    m_commits[next]->isTearing() != baseTearing ||
                    basePlanes != m_commits[next]->modifiedPlanes()) {
                    break;
                }
                ++next;
            }

            if (next > read + 1) {
                const size_t mergeCount = next - (read + 1);
                if (m_commitsToDelete.capacity() < m_commitsToDelete.size() + mergeCount) {
                    m_commitsToDelete.reserve(m_commitsToDelete.size() + mergeCount);
                }

                for (size_t m = read + 1; m < next; ++m) {
                    if (m_commits[m]) {
                        base->merge(m_commits[m].get());
                        m_commitsToDelete.push_back(std::move(m_commits[m]));
                    }
                }
                optimized = true;
            }
        }

        if (write != read) {
            m_commits[write] = std::move(m_commits[read]);
        }
        ++write;
    }

    if (write < m_commits.size()) {
        m_commits.erase(m_commits.begin() + write, m_commits.end());
    }

    if (optimized) {
        QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
    }
}

DrmCommitThread::~DrmCommitThread()
{
    if (m_thread) {
        {
            std::unique_lock<std::mutex> lock(m_mutex);
            m_thread->requestInterruption();
            m_commitPending.notify_all();
            m_ping.store(true, std::memory_order_release);
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
    if (!commit) [[unlikely]] {
        return;
    }

    std::unique_lock<std::mutex> lock(m_mutex);

    const auto now = std::chrono::steady_clock::now();
    TimePoint newTarget;

    const bool isTearing = m_tearing.load(std::memory_order_acquire);
    const bool isVrr = m_vrr.load(std::memory_order_acquire);

    if (isTearing) {
        newTarget = now;
    } else if (isVrr && now >= m_lastPageflip + m_minVblankInterval) {
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    if (newTarget < now) {
        newTarget = now;
    }
    if (m_targetPageflipTime < newTarget) {
        m_targetPageflipTime = newTarget;
    }

    commit->setDeadline(m_targetPageflipTime - m_safetyMargin);

    if (m_commits.size() >= m_commits.capacity()) {
        const size_t newCap = std::max(kInitialCommitCapacity, m_commits.capacity() * 2);
        m_commits.reserve(newCap);
    }
    m_commits.push_back(std::move(commit));
    m_commitPending.notify_one();
}

void DrmCommitThread::setPendingCommit(std::unique_ptr<DrmLegacyCommit> &&commit)
{
    std::unique_lock<std::mutex> lock(m_mutex);
    m_committed = std::move(commit);
}

void DrmCommitThread::clearDroppedCommits()
{
    std::vector<std::unique_ptr<DrmAtomicCommit>> toDelete;
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        if (m_commitsToDelete.empty()) {
            return;
        }
        toDelete = std::move(m_commitsToDelete);
        m_commitsToDelete.clear();
        if (m_commitsToDelete.capacity() > kInitialDeleteCapacity * 2) {
            m_commitsToDelete.shrink_to_fit();
            m_commitsToDelete.reserve(kInitialDeleteCapacity);
        }
    }
}

void DrmCommitThread::setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime)
{
    std::unique_lock<std::mutex> lock(m_mutex);

    if (maximum == 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid maximum refresh rate: 0, using 60Hz fallback";
        maximum = 60000;
    }

    m_minVblankInterval = std::chrono::nanoseconds(1'000'000'000'000ULL / maximum);

    const uint64_t vblankNs = static_cast<uint64_t>(m_minVblankInterval.count());
    if (vblankNs > 0 && vblankNs < (1ULL << 31)) [[likely]] {
        constexpr uint8_t shift = 63;
        m_vblankReciprocal = ((1ULL << shift) + vblankNs - 1) / vblankNs;
        m_vblankReciprocalShift = shift;
    } else {
        m_vblankReciprocal = 0;
        m_vblankReciprocalShift = 0;
    }

    m_baseSafetyMargin = vblankTime + s_safetyMarginMinimum;
    const auto halfVblank = m_minVblankInterval / 2;
    const auto maximumReasonableMargin = std::min(kMaxSafetyMargin, halfVblank);
    m_additionalSafetyMargin = clampNs(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
    m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
}

void DrmCommitThread::pageFlipped(std::chrono::nanoseconds timestamp)
{
    std::unique_lock<std::mutex> lock(m_mutex);

    if (m_pageflipTimeoutDetected.load(std::memory_order_acquire)) [[unlikely]] {
        const auto elapsed = std::chrono::steady_clock::now() - m_lastCommitTime;
        const auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
        qCCritical(KWIN_DRM, "Pageflip arrived %lldms after the commit", static_cast<long long>(elapsedMs));
        m_pageflipTimeoutDetected.store(false, std::memory_order_release);
    }

    const auto steadyNow = std::chrono::steady_clock::now();
    const int64_t steadyNowNs = static_cast<int64_t>(steadyNow.time_since_epoch().count());

    timespec monoTs{};
    clock_gettime(CLOCK_MONOTONIC, &monoTs);
    const int64_t monoNowNs = static_cast<int64_t>(monoTs.tv_sec) * 1'000'000'000LL + monoTs.tv_nsec;
    const int64_t offset = steadyNowNs - monoNowNs;

    int64_t mappedNs = 0;
    if (__builtin_add_overflow(timestamp.count(), offset, &mappedNs)) {
        mappedNs = steadyNowNs;
    }

    mappedNs = std::clamp<int64_t>(mappedNs, steadyNowNs - 1'000'000, steadyNowNs + 100'000'000);
    m_lastPageflip = TimePoint(std::chrono::nanoseconds(mappedNs));
    m_committed.reset();

    if (!m_commits.empty()) {
        m_targetPageflipTime = estimateNextVblank(steadyNow);
        m_commitPending.notify_one();
    }
}

bool DrmCommitThread::pageflipsPending()
{
    std::unique_lock<std::mutex> lock(m_mutex);
    return !m_commits.empty() || static_cast<bool>(m_committed);
}

TimePoint DrmCommitThread::estimateNextVblank(TimePoint now) const
{
    if (m_minVblankInterval <= 0ns) [[unlikely]] {
        return now + kDefaultFrameInterval;
    }

    const auto elapsed = std::max(0ns, now - m_lastPageflip);
    const uint64_t elapsedNs = static_cast<uint64_t>(elapsed.count());
    const uint64_t vblankNs = static_cast<uint64_t>(m_minVblankInterval.count());

    uint64_t pageflipsSince = 0;
    if (m_vblankReciprocal > 0 && elapsedNs < (1ULL << 32)) [[likely]] {
        const __uint128_t product = static_cast<__uint128_t>(elapsedNs) * m_vblankReciprocal;
        pageflipsSince = static_cast<uint64_t>(product >> m_vblankReciprocalShift);
    } else {
        pageflipsSince = elapsedNs / vblankNs;
    }

    pageflipsSince = std::min(pageflipsSince, kMaxPageflipSpan);
    return m_lastPageflip + m_minVblankInterval * (pageflipsSince + 1);
}

std::chrono::nanoseconds DrmCommitThread::safetyMargin() const
{
    std::unique_lock<std::mutex> lock(m_mutex);
    return m_safetyMargin;
}

void DrmCommitThread::handlePing()
{
    m_gpu->dispatchEvents();
    std::unique_lock<std::mutex> lock(m_mutex);
    m_ping.store(true, std::memory_order_release);
    m_pong.notify_one();
}

} // namespace KWin
