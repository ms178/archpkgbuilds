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

constexpr auto kVrrPollIntervalFine = 50us;
constexpr auto kVrrPollIntervalCoarse = 250us;
constexpr auto kVrrPollFineWindow = 300us;
constexpr uint64_t kMaxPageflipSpan = 10000;
constexpr std::chrono::milliseconds kDefaultFrameInterval{16};
constexpr size_t kInitialCommitCapacity = 16;
constexpr size_t kInitialDeleteCapacity = 64;
constexpr int kMaxSubmitRetries = 3;

inline void preciseSleepUntil(const std::chrono::steady_clock::time_point &target) noexcept
{
    const auto dur = target.time_since_epoch();
    const auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(dur).count();
    if (ns <= 0) [[unlikely]] {
        return;
    }

    struct timespec ts;
    ts.tv_sec = static_cast<time_t>(ns / 1'000'000'000LL);
    ts.tv_nsec = static_cast<long>(ns % 1'000'000'000LL);

    int ret = 0;
    do {
        ret = clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &ts, nullptr);
    } while (ret == EINTR);
}

[[nodiscard]]
inline std::chrono::nanoseconds clampNs(std::chrono::nanoseconds val,
                                        std::chrono::nanoseconds lo,
                                        std::chrono::nanoseconds hi) noexcept
{
    if (val < lo) return lo;
    if (val > hi) return hi;
    return val;
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

        constexpr std::chrono::microseconds kFinalSleepWindow{200};

        for (;;) {
            if (thread->isInterruptionRequested()) [[unlikely]] {
                return;
            }

            std::unique_lock<std::mutex> lock(m_mutex);
            bool timeout = false;

            if (m_committed) {
                timeout = m_commitPending.wait_for(lock, DrmGpu::s_pageflipTimeout) == std::cv_status::timeout;
            } else if (m_commits.empty()) {
                m_commitPending.wait(lock, [this, thread] {
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
                    const bool invoked = QMetaObject::invokeMethod(this, &DrmCommitThread::handlePing, Qt::QueuedConnection);
                    lock.lock();

                    if (!invoked) [[unlikely]] {
                        qCWarning(KWIN_DRM) << "Failed to queue DRM event ping";
                        m_ping.store(true, std::memory_order_release);
                    } else {
                        m_pong.wait(lock, [this] {
                            return m_ping.load(std::memory_order_acquire);
                        });
                    }

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
                const auto remaining = wakeTarget - now;

                if (remaining > kFinalSleepWindow) {
                    const auto coarseTarget = wakeTarget - kFinalSleepWindow;

                    const bool wokeForReason = m_commitPending.wait_until(lock, coarseTarget, [this, thread, scheduledTarget] {
                        return thread->isInterruptionRequested() || m_committed || m_commits.empty() || m_targetPageflipTime != scheduledTarget;
                    });

                    if (thread->isInterruptionRequested()) [[unlikely]] {
                        return;
                    }
                    if (wokeForReason) {
                        continue;
                    }
                }

                lock.unlock();
                preciseSleepUntil(wakeTarget);
                lock.lock();
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

            const bool vrrCached = m_vrr.load(std::memory_order_acquire);
            const bool tearingCached = m_tearing.load(std::memory_order_acquire);

            if (!frontCommit->isReadyFor(m_targetPageflipTime)) {
                const bool vrrOrTearing = vrrCached || tearingCached;
                m_targetPageflipTime += vrrOrTearing ? kVrrPollIntervalFine : m_minVblankInterval;
                continue;
            }

            const auto frontDelay = frontCommit->allowedVrrDelay();
            if (frontDelay && vrrCached) {
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

                if (lowestDelay < 0ns) {
                    lowestDelay = 0ns;
                }
                const auto delayedTarget = m_lastPageflip + lowestDelay;

                if (allDelay) {
                    if (m_commitPending.wait_until(lock, delayedTarget) == std::cv_status::no_timeout) {
                        continue;
                    }
                } else {
                    auto waitDeadline = delayedTarget;
                    while (true) {
                        const auto loopNow = std::chrono::steady_clock::now();
                        if (loopNow >= waitDeadline) break;
                        const auto remaining = waitDeadline - loopNow;
                        const auto waitTime = (remaining > kVrrPollFineWindow)
                            ? std::min(remaining, std::chrono::duration_cast<std::chrono::nanoseconds>(kVrrPollIntervalCoarse))
                            : std::min(remaining, std::chrono::duration_cast<std::chrono::nanoseconds>(kVrrPollIntervalFine));

                        if (m_commitPending.wait_for(lock, waitTime) == std::cv_status::no_timeout) break;
                        if (m_commits.empty()) break;
                        optimizeCommits(waitDeadline);
                        if (m_commits.empty() || !m_commits.front()->allowedVrrDelay().has_value()) break;
                    }
                    if (m_commits.empty() || std::chrono::steady_clock::now() < delayedTarget) {
                        continue;
                    }
                }

                if (m_commits.empty()) {
                    continue;
                }
                if (!m_commits.front()->allowedVrrDelay().has_value()) {
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
    if (m_commits.empty()) [[unlikely]] return;

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

        size_t merges = 0;
        while (merges < static_cast<size_t>(kMaxSubmitRetries) && m_commits.size() > 1) {
            if (!m_commits[1]) [[unlikely]] {
                m_commits.erase(m_commits.begin() + 1);
                continue;
            }
            auto toMerge = std::move(m_commits[1]);
            m_commits.erase(m_commits.begin() + 1);
            commit->merge(toMerge.get());
            m_commitsToDelete.push_back(std::move(toMerge));
            ++merges;
        }
        testedOk = commit->test();
        savedErrno = testedOk ? 0 : errno;

        if (!testedOk) {
            m_commitsToDelete.push_back(std::move(m_commits.front()));
            m_commits.erase(m_commits.begin());
            if (!m_commitsToDelete.empty()) {
                QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
            }
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
            const auto penalty = -safetyDifference + 500us;
            if (penalty > m_additionalSafetyMargin) {
                m_additionalSafetyMargin = penalty;
            }
        } else {
            const int64_t count = m_additionalSafetyMargin.count();
            const int64_t decay = (count * 41) >> 13;
            m_additionalSafetyMargin = std::chrono::nanoseconds(count - decay);
        }

        const auto halfVblank = m_minVblankInterval / 2;
        const auto maximumReasonableMargin = (3ms < halfVblank) ? 3ms : halfVblank;
        m_additionalSafetyMargin = std::clamp(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        const int commitErrno = errno;
        qCWarning(KWIN_DRM) << "Atomic commit failed: errno =" << commitErrno
                            << "(" << strerror(commitErrno) << ")"
                            << "; tearing =" << commit->isTearing();

        const size_t totalToDelete = m_commitsToDelete.size() + m_commits.size();
        if (m_commitsToDelete.capacity() < totalToDelete) {
            m_commitsToDelete.reserve(totalToDelete);
        }
        for (auto &c : m_commits) {
            if (c) {
                m_commitsToDelete.push_back(std::move(c));
            }
        }
        m_commits.clear();
    }

    if (!m_commitsToDelete.empty()) {
        QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
    }
}

bool DrmCommitThread::tryMergeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() < 2) return false;
    DrmAtomicCommit* const front = m_commits.front().get();
    if (!front) [[unlikely]] return false;

    size_t firstNotReady = 1;
    const size_t commitCount = m_commits.size();
    while (firstNotReady < commitCount) {
        if (!m_commits[firstNotReady]) [[unlikely]] break;
        if (!m_commits[firstNotReady]->isReadyFor(pageflipTarget)) break;
        if (m_commits[firstNotReady]->isTearing()) break;
        ++firstNotReady;
    }

    if (firstNotReady == 1) return false;

    auto baseCommit = std::move(m_commits.front());
    const size_t mergeCount = firstNotReady - 1;
    const size_t required = m_commitsToDelete.size() + mergeCount;
    if (m_commitsToDelete.capacity() < required) {
        m_commitsToDelete.reserve(required);
    }

    for (size_t i = 1; i < firstNotReady; ++i) {
        baseCommit->merge(m_commits[i].get());
        m_commitsToDelete.push_back(std::move(m_commits[i]));
    }

    if (!baseCommit->test()) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Merged commit test failed, reverting optimization";
        m_commitsToDelete.push_back(std::move(baseCommit));
        m_commits.erase(m_commits.begin(), m_commits.begin() + static_cast<std::ptrdiff_t>(firstNotReady));
        return false;
    }

    m_commits.front() = std::move(baseCommit);
    m_commits.erase(m_commits.begin() + 1, m_commits.begin() + static_cast<std::ptrdiff_t>(firstNotReady));
    return true;
}

void DrmCommitThread::optimizeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() <= 1) return;
    DrmAtomicCommit* const front = m_commits.front().get();
    if (!front || front->isTearing()) return;

    bool optimized = false;
    if (front->areBuffersReadable() && tryMergeCommits(pageflipTarget)) {
        optimized = true;
    }

    m_commitsToDelete.reserve(m_commitsToDelete.size() + m_commits.size());

    const size_t count = m_commits.size();
    size_t write = 1;
    for (size_t read = 1; read < count; ) {
        if (!m_commits[read]) [[unlikely]] { ++read; continue; }
        auto& startCommit = m_commits[read];
        const bool startTearing = startCommit->isTearing();
        const auto& startPlanes = startCommit->modifiedPlanes();

        size_t next = read + 1;
        if (!startPlanes.empty()) {
            while (next < count && m_commits[next] &&
                   m_commits[next]->isReadyFor(pageflipTarget) &&
                   m_commits[next]->isTearing() == startTearing &&
                   startPlanes == m_commits[next]->modifiedPlanes()) {
                ++next;
            }
            if (next > read + 1) {
                for (size_t i = read + 1; i < next; ++i) {
                    startCommit->merge(m_commits[i].get());
                    m_commitsToDelete.push_back(std::move(m_commits[i]));
                }
                optimized = true;
            }
        }
        if (write != read) {
            m_commits[write] = std::move(m_commits[read]);
        }
        ++write;
        read = next;
    }

    if (write < m_commits.size()) {
        m_commits.erase(m_commits.begin() + static_cast<std::ptrdiff_t>(write), m_commits.end());
    }

    if (optimized && !m_commitsToDelete.empty()) {
        QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
    }
}

DrmCommitThread::~DrmCommitThread()
{
    if (m_thread) {
        {
            std::unique_lock<std::mutex> lock(m_mutex);
            m_thread->requestInterruption();
            m_ping.store(true, std::memory_order_release);
        }
        m_commitPending.notify_all();
        m_pong.notify_all();
        m_thread->wait();
    }
    if (m_committed) {
        m_committed->setDefunct();
        m_gpu->addDefunctCommit(std::move(m_committed));
    }
}

void DrmCommitThread::addCommit(std::unique_ptr<DrmAtomicCommit> &&commit)
{
    if (!commit) [[unlikely]] return;

    std::unique_lock<std::mutex> lock(m_mutex);
    const bool isTearing = m_tearing.load(std::memory_order_acquire);
    const bool isVrr = m_vrr.load(std::memory_order_acquire);
    const auto now = std::chrono::steady_clock::now();
    TimePoint newTarget;

    if (isTearing) {
        newTarget = now;
    } else if (isVrr && now >= m_lastPageflip + m_minVblankInterval) {
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    if (newTarget < now) newTarget = now;
    if (m_targetPageflipTime < newTarget) {
        m_targetPageflipTime = newTarget;
    }
    commit->setDeadline(m_targetPageflipTime - m_safetyMargin);

    if (m_commits.size() == m_commits.capacity()) {
        m_commits.reserve(m_commits.capacity() == 0 ? kInitialCommitCapacity : m_commits.capacity() * 2);
    }
    m_commits.push_back(std::move(commit));
    lock.unlock();
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
        if (m_commitsToDelete.empty()) return;
        toDelete = std::move(m_commitsToDelete);
        m_commitsToDelete.clear();
        if (m_commitsToDelete.capacity() < kInitialDeleteCapacity) {
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
    const auto maximumReasonableMargin = (3ms < halfVblank) ? 3ms : halfVblank;
    m_additionalSafetyMargin = clampNs(m_additionalSafetyMargin, 0ns, maximumReasonableMargin);
    m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
}

void DrmCommitThread::pageFlipped(std::chrono::nanoseconds timestamp)
{
    const int64_t steadyNowNs = std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();

    struct alignas(64) OffsetCache {
        int64_t offsetNs = 0;
        uint32_t countdown = 0;
        bool valid = false;
    };
    thread_local OffsetCache cache;

    auto resyncOffset = [&]() {
        timespec monoNowTs{};
        if (clock_gettime(CLOCK_MONOTONIC, &monoNowTs) != 0) return;
        const int64_t monoNowNs = static_cast<int64_t>(monoNowTs.tv_sec) * 1'000'000'000LL + static_cast<int64_t>(monoNowTs.tv_nsec);
        const int64_t newOffset = steadyNowNs - monoNowNs;
        if (!cache.valid) {
            cache.offsetNs = newOffset;
            cache.valid = true;
        } else {
            cache.offsetNs += (newOffset - cache.offsetNs) >> 3;
        }
    };

    if (!cache.valid || cache.countdown == 0) [[unlikely]] {
        resyncOffset();
        cache.countdown = 4096;
    } else {
        --cache.countdown;
    }

    int64_t mappedNs = 0;
    if (__builtin_add_overflow(timestamp.count(), cache.offsetNs, &mappedNs)) [[unlikely]] {
        mappedNs = steadyNowNs;
    }
    mappedNs = std::min(mappedNs, steadyNowNs);
    mappedNs = std::max(mappedNs, int64_t{0});

    TimePoint mappedTime{std::chrono::duration_cast<TimePoint::duration>(std::chrono::nanoseconds{mappedNs})};
    const auto steadyNow = std::chrono::steady_clock::now();

    bool needsNotify = false;
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        if (m_pageflipTimeoutDetected.load(std::memory_order_acquire)) [[unlikely]] {
            const auto elapsed = steadyNow - m_lastCommitTime;
            const auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
            qCCritical(KWIN_DRM, "Pageflip arrived %lldms after the commit", static_cast<long long>(elapsedMs));
            m_pageflipTimeoutDetected.store(false, std::memory_order_release);
        }
        if (mappedTime < m_lastPageflip) mappedTime = m_lastPageflip;
        m_lastPageflip = mappedTime;
        m_committed.reset();

        needsNotify = !m_commits.empty();
        if (needsNotify) {
            m_targetPageflipTime = estimateNextVblank(steadyNow);
        }
    }
    if (needsNotify) {
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
    if (m_minVblankInterval <= std::chrono::nanoseconds::zero()) [[unlikely]] {
        return now + kDefaultFrameInterval;
    }

    const auto elapsed = (now >= m_lastPageflip) ? (now - m_lastPageflip) : std::chrono::nanoseconds::zero();
    const uint64_t elapsedNs = static_cast<uint64_t>(elapsed.count());
    const uint64_t vblankNs = static_cast<uint64_t>(m_minVblankInterval.count());

    if (vblankNs == 0) [[unlikely]] return now + kDefaultFrameInterval;

    uint64_t pageflipsSince = 0;
    if (m_vblankReciprocal > 0 && elapsedNs < (1ULL << 32)) [[likely]] {
        const __uint128_t product = static_cast<__uint128_t>(elapsedNs) * m_vblankReciprocal;
        pageflipsSince = static_cast<uint64_t>(product >> m_vblankReciprocalShift);
    } else {
        pageflipsSince = elapsedNs / vblankNs;
    }

    pageflipsSince = std::min(pageflipsSince, kMaxPageflipSpan);
    const auto flips = static_cast<TimePoint::duration::rep>(pageflipsSince + 1);
    return m_lastPageflip + m_minVblankInterval * flips;
}

std::chrono::nanoseconds DrmCommitThread::safetyMargin() const
{
    std::unique_lock<std::mutex> lock(m_mutex);
    return m_safetyMargin;
}

void DrmCommitThread::handlePing()
{
    m_gpu->dispatchEvents();
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_ping.store(true, std::memory_order_release);
    }
    m_pong.notify_one();
}

} // namespace KWin
