/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Performance Engineering Team

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_commit_thread.h"
#include "drm_commit.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "utils/envvar.h"
#include "utils/realtime.h"

#include <cerrno>
#include <cstring>
#include <ctime>
#include <algorithm>

#include <QCoreApplication>
#include <QThread>

using namespace std::chrono_literals;

namespace KWin
{

namespace
{
// VRR polling cadence: when we're inside the "fine" window before the deadline,
// poll aggressively for buffer readiness; outside of it, poll coarsely to save power.
constexpr auto kVrrPollIntervalFine = 50us;
constexpr auto kVrrPollIntervalCoarse = 250us;
constexpr auto kVrrPollFineWindow = 300us;

// Sanity clamp for the estimateNextVblank fast path, so a stale m_lastPageflip
// can never make us project an absurd number of vblanks into the future.
constexpr uint64_t kMaxPageflipSpan = 10000;

constexpr std::chrono::milliseconds kDefaultFrameInterval{16};

constexpr size_t kInitialCommitCapacity = 16;
constexpr size_t kInitialDeleteCapacity = 64;

// How many times we attempt to recover a failing front commit by merging the
// next queued commit into it before we give up and drop the front commit.
constexpr int kMaxSubmitRetries = 3;

// Absolute-time sleep on CLOCK_MONOTONIC with EINTR handling. This is the
// lowest-jitter way to wake up just before the target pageflip time.
inline void preciseSleepUntil(const std::chrono::steady_clock::time_point &target) noexcept
{
    const auto dur = target.time_since_epoch();
    const auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(dur).count();
    if (ns <= 0) {
        return;
    }

    timespec ts;
    ts.tv_sec = static_cast<time_t>(ns / 1'000'000'000LL);
    ts.tv_nsec = static_cast<long>(ns % 1'000'000'000LL);

    int ret = 0;
    do {
        ret = clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &ts, nullptr);
    } while (ret == EINTR);
}

[[nodiscard]] inline std::chrono::nanoseconds clampNs(std::chrono::nanoseconds val,
                                                      std::chrono::nanoseconds lo,
                                                      std::chrono::nanoseconds hi) noexcept
{
    if (val < lo) {
        return lo;
    }
    if (val > hi) {
        return hi;
    }
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

    m_lastPageflip = std::chrono::steady_clock::now();
    m_commits.reserve(kInitialCommitCapacity);
    m_commitsToDelete.reserve(kInitialDeleteCapacity);

    m_thread.reset(QThread::create([this]() {
        const auto thread = QThread::currentThread();
        gainRealTime();

        // Window in which we busy-ish sleep with the precise monotonic timer,
        // instead of waiting on the condition variable. Keeps wake jitter low.
        constexpr std::chrono::microseconds kFinalSleepWindow{200};

        for (;;) {
            if (thread->isInterruptionRequested()) [[unlikely]] {
                return;
            }

            std::unique_lock lock(m_mutex);

            bool timeout = false;
            if (m_committed) {
                // We're waiting for a pageflip event for the commit in flight.
                timeout = m_commitPending.wait_for(lock, DrmGpu::s_pageflipTimeout) == std::cv_status::timeout;
            } else if (m_commits.empty()) {
                // Nothing queued and nothing in flight: sleep until work arrives.
                m_commitPending.wait(lock, [this, thread] {
                    return thread->isInterruptionRequested() || m_committed || !m_commits.empty();
                });
                if (thread->isInterruptionRequested()) [[unlikely]] {
                    return;
                }
            }

            if (m_committed) {
                if (timeout) [[unlikely]] {
                    // A pageflip didn't arrive in time. Ping the main thread to
                    // dispatch DRM events (the event may simply be stuck in the
                    // socket queue while the main loop is busy).
                    m_ping.store(false, std::memory_order_release);
                    lock.unlock();
                    const bool invoked = QMetaObject::invokeMethod(this, &DrmCommitThread::handlePing, Qt::QueuedConnection);
                    lock.lock();
                    if (!invoked) [[unlikely]] {
                        qCWarning(KWIN_DRM, "Failed to invoke handlePing on the main thread");
                    } else {
                        // Wait for the main thread to confirm it dispatched events.
                        const bool ponged = m_pong.wait_for(lock, DrmGpu::s_pageflipTimeout, [this] {
                            return m_ping.load(std::memory_order_acquire);
                        });
                        if (!ponged && !m_committed) {
                            // Event was dispatched in the meantime; nothing to do.
                        } else if (!ponged) [[unlikely]] {
                            qCCritical(KWIN_DRM, "Pageflip timed out on GPU %s! This is a kernel/driver bug.",
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
                }
                // Either way, while a commit is in flight we cannot submit a new one.
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
                    // Coarse phase: wait on the cv so we react instantly to new
                    // commits, retarget requests, or interruption.
                    const auto coarseTarget = wakeTarget - kFinalSleepWindow;
                    const bool wokeForReason = m_commitPending.wait_until(lock, coarseTarget, [this, thread, scheduledTarget] {
                        return thread->isInterruptionRequested()
                            || m_committed
                            || m_commits.empty()
                            || m_targetPageflipTime != scheduledTarget;
                    });
                    if (thread->isInterruptionRequested()) [[unlikely]] {
                        return;
                    }
                    if (wokeForReason) {
                        continue;
                    }
                }
                // Fine phase: precise sleep without holding the lock.
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
                // Buffers not yet readable, or the commit targets a later vblank.
                const bool vrrOrTearing = vrrCached || tearingCached;
                m_targetPageflipTime += vrrOrTearing ? kVrrPollIntervalFine : m_minVblankInterval;
                continue;
            }

            // VRR delay coalescing: if the front commit allows a delay and we're
            // in VRR, hold it a little to coalesce with subsequent commits that
            // share the delay window. This avoids waking the panel unnecessarily.
            const auto frontDelay = frontCommit->allowedVrrDelay();
            if (frontDelay && vrrCached) {
                std::chrono::nanoseconds lowestDelay = *frontDelay;
                bool allDelay = true;
                const size_t commitCount = m_commits.size();
                for (size_t i = 1; i < commitCount; ++i) {
                    if (!m_commits[i]) {
                        continue;
                    }
                    const auto delay = m_commits[i]->allowedVrrDelay();
                    if (delay) {
                        if (*delay < lowestDelay) {
                            lowestDelay = *delay;
                        }
                    } else {
                        allDelay = false;
                        break;
                    }
                }

                if (allDelay) {
                    const auto waitDeadline = m_lastPageflip + lowestDelay;
                    for (;;) {
                        const auto loopNow = std::chrono::steady_clock::now();
                        if (loopNow >= waitDeadline) {
                            break;
                        }
                        const auto remaining = waitDeadline - loopNow;
                        const auto waitTime = (remaining > kVrrPollFineWindow)
                            ? std::min(remaining, std::chrono::duration_cast<std::chrono::nanoseconds>(kVrrPollIntervalCoarse))
                            : std::min(remaining, std::chrono::duration_cast<std::chrono::nanoseconds>(kVrrPollIntervalFine));
                        if (m_commitPending.wait_for(lock, waitTime) == std::cv_status::no_timeout) {
                            break;
                        }
                        if (m_commits.empty()) {
                            break;
                        }
                        optimizeCommits(waitDeadline);
                        if (m_commits.empty() || !m_commits.front()->allowedVrrDelay().has_value()) {
                            break;
                        }
                    }
                    if (m_commits.empty()
                        || (std::chrono::steady_clock::now() < (m_lastPageflip + lowestDelay)
                            && m_commits.front()->allowedVrrDelay().has_value())) {
                        continue;
                    }
                }
            }

            submit();
        }
    }));

    if (!m_thread) [[unlikely]] {
        qCCritical(KWIN_DRM, "Failed to create the DRM commit thread");
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
        // Rate-limit warnings; a transient atomic-test failure during heavy
        // plane reconfiguration is normal and shouldn't spam the journal.
        thread_local std::chrono::steady_clock::time_point lastWarnTime{};
        thread_local uint32_t suppressed = 0;
        const auto now = std::chrono::steady_clock::now();
        const bool shouldLog = (now - lastWarnTime) > 500ms;
        if (shouldLog) {
            lastWarnTime = now;
            if (suppressed > 0) {
                qCWarning(KWIN_DRM, "Atomic commit test failed: %s (%u similar messages suppressed)",
                          strerror(savedErrno), suppressed);
                suppressed = 0;
            } else {
                qCWarning(KWIN_DRM, "Atomic commit test failed: %s", strerror(savedErrno));
            }
        } else {
            ++suppressed;
        }

        // Recovery strategy: try to merge subsequent queued commits into the
        // front commit. A later commit often supersedes the plane state that
        // made the current one fail (e.g. a cursor plane being re-enabled with
        // a valid buffer), so the merged result can pass the atomic test.
        int merges = 0;
        while (merges < kMaxSubmitRetries && m_commits.size() > 1) {
            if (!m_commits[1]) [[unlikely]] {
                m_commits.erase(m_commits.begin() + 1);
                continue;
            }
            auto toMerge = std::move(m_commits[1]);
            m_commits.erase(m_commits.begin() + 1);
            commit->merge(toMerge.get());
            m_commitsToDelete.push_back(std::move(toMerge));
            ++merges;

            testedOk = commit->test();
            savedErrno = testedOk ? 0 : errno;
            if (testedOk) {
                break;
            }
        }

        if (!testedOk) {
            // Give up on this commit. Dropping it lets the kernel keep the
            // previous good state on the planes; the next frame will rebuild
            // a fresh, valid commit. This is what makes the compositor fall
            // back to a software cursor cleanly instead of wedging the planes.
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

        // Adaptive safety margin: how far ahead of the deadline did we actually
        // manage to submit? If we cut it close (or missed), grow the margin so
        // the next frame is scheduled earlier; if we had slack, decay it back
        // down so we don't add unnecessary latency.
        const auto targetTimestamp = m_targetPageflipTime - m_baseSafetyMargin;
        const auto safetyDifference = targetTimestamp - m_lastCommitTime;
        if (safetyDifference < std::chrono::nanoseconds::zero()) {
            const auto penalty = -safetyDifference;
            if (penalty > m_additionalSafetyMargin) {
                m_additionalSafetyMargin = penalty;
            }
        } else {
            // Exponential decay: subtract ~0.5% (41/8192) of the current value.
            const int64_t count = m_additionalSafetyMargin.count();
            const int64_t decay = (count * 41) >> 13;
            m_additionalSafetyMargin = std::chrono::nanoseconds(count - decay);
        }

        const auto halfVblank = m_minVblankInterval / 2;
        const auto maximumReasonableMargin = (3ms < halfVblank) ? std::chrono::nanoseconds(3ms) : halfVblank;
        m_additionalSafetyMargin = clampNs(m_additionalSafetyMargin,
                                           std::chrono::nanoseconds::zero(),
                                           maximumReasonableMargin);

        m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
    } else {
        savedErrno = errno;
        qCWarning(KWIN_DRM, "Atomic commit failed: %s", strerror(savedErrno));
        m_commitsToDelete.push_back(std::move(m_commits.front()));
        m_commits.erase(m_commits.begin());
    }

    const size_t totalToDelete = m_commitsToDelete.size();
    if (totalToDelete > 0) {
        QMetaObject::invokeMethod(this, &DrmCommitThread::clearDroppedCommits, Qt::QueuedConnection);
    }
}

bool DrmCommitThread::tryMergeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() < 2) {
        return false;
    }

    // Find the run of leading commits that are all ready for this target and
    // non-tearing. Tearing commits must never be merged across, as each tearing
    // commit must be flipped on its own as soon as possible.
    size_t firstNotReady = 1;
    const size_t count = m_commits.size();
    while (firstNotReady < count) {
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

    if (firstNotReady == 1) {
        return false;
    }

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
        // The merged result doesn't pass; abort the merge by restoring the base
        // commit at the front. The already-moved-out slots are tombstones that
        // will be erased below, but the merged-in state is harmless to retest
        // later since merge() is idempotent w.r.t. property overwrite.
        qCDebug(KWIN_DRM, "Merging %zu commits failed the atomic test, reverting", mergeCount);
        m_commits.front() = std::move(baseCommit);
        // Drop the now-empty tombstones we merged in.
        m_commits.erase(m_commits.begin() + 1, m_commits.begin() + static_cast<std::ptrdiff_t>(firstNotReady));
        return false;
    }

    m_commits.front() = std::move(baseCommit);
    m_commits.erase(m_commits.begin() + 1, m_commits.begin() + static_cast<std::ptrdiff_t>(firstNotReady));
    return true;
}

void DrmCommitThread::optimizeCommits(TimePoint pageflipTarget)
{
    if (m_commits.size() < 2) {
        return;
    }

    DrmAtomicCommit *front = m_commits.front().get();
    if (!front) [[unlikely]] {
        return;
    }
    if (front->isTearing()) {
        // Never coalesce tearing commits; flip each immediately.
        return;
    }

    bool optimized = false;

    // First, try to merge the leading run of ready non-tearing commits into one.
    if (front->areBuffersReadable() && tryMergeCommits(pageflipTarget)) {
        optimized = true;
    }

    m_commitsToDelete.reserve(m_commitsToDelete.size() + m_commits.size());

    // Second pass: compact runs of commits that touch the exact same set of
    // planes (and share tearing state and readiness) into a single commit.
    const size_t count = m_commits.size();
    size_t write = 1;
    for (size_t read = 1; read < count;) {
        DrmAtomicCommit *startCommit = m_commits[read].get();
        if (!startCommit) [[unlikely]] {
            ++read;
            continue;
        }

        const bool startTearing = startCommit->isTearing();
        const auto &startPlanes = startCommit->modifiedPlanes();

        size_t next = read + 1;
        if (!startPlanes.empty()) {
            while (next < count
                   && m_commits[next]
                   && m_commits[next]->isReadyFor(pageflipTarget)
                   && m_commits[next]->isTearing() == startTearing
                   && startPlanes == m_commits[next]->modifiedPlanes()) {
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

    if (write < count) {
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
            std::unique_lock lock(m_mutex);
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
    if (!commit) [[unlikely]] {
        return;
    }

    std::unique_lock lock(m_mutex);

    const bool isTearing = m_tearing.load(std::memory_order_acquire);
    const bool isVrr = m_vrr.load(std::memory_order_acquire);
    const auto now = std::chrono::steady_clock::now();

    TimePoint newTarget;
    if (isTearing) {
        // Tearing: present as soon as possible.
        newTarget = now;
    } else if (isVrr && now >= m_lastPageflip + m_minVblankInterval) {
        // VRR and at least one minimum vblank interval has elapsed: present now.
        newTarget = now;
    } else {
        newTarget = estimateNextVblank(now);
    }

    if (newTarget < m_targetPageflipTime || m_commits.empty()) {
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
    // Legacy entry point: a DrmLegacyCommit just flipped via the legacy ioctl
    // path and we now own it until its pageflip event arrives. Assignment to
    // m_committed (std::unique_ptr<DrmCommit>) upcasts cleanly through the
    // unique_ptr converting move-assignment operator.
    std::unique_lock lock(m_mutex);
    m_committed = std::move(commit);
}

void DrmCommitThread::clearDroppedCommits()
{
    std::vector<std::unique_ptr<DrmAtomicCommit>> toDelete;
    {
        std::unique_lock lock(m_mutex);
        if (m_commitsToDelete.empty()) {
            return;
        }
        toDelete = std::move(m_commitsToDelete);
        m_commitsToDelete.clear();
        if (m_commitsToDelete.capacity() > kInitialDeleteCapacity * 4) {
            m_commitsToDelete.shrink_to_fit();
            m_commitsToDelete.reserve(kInitialDeleteCapacity);
        }
    }
    // Destruction happens here, on the main thread, with the lock released.
    toDelete.clear();
}

void DrmCommitThread::setModeInfo(uint32_t maximum, std::chrono::nanoseconds vblankTime)
{
    std::unique_lock lock(m_mutex);

    if (maximum == 0) [[unlikely]] {
        qCWarning(KWIN_DRM, "setModeInfo called with refresh rate 0; ignoring");
        return;
    }

    // m_minVblankInterval is the time for one vblank at the maximum refresh rate.
    m_minVblankInterval = std::chrono::nanoseconds(1'000'000'000'000ull / maximum);

    // Precompute a fixed-point reciprocal of the vblank interval so that
    // estimateNextVblank can divide via multiply+shift on the hot path.
    const int64_t vblankNs = m_minVblankInterval.count();
    if (vblankNs > 0 && vblankNs < (int64_t{1} << 40)) {
        // Choose a shift that maximizes precision without overflowing a 64-bit
        // product for elapsed times up to ~kMaxPageflipSpan vblanks.
        m_vblankReciprocalShift = 40;
        m_vblankReciprocal = (uint64_t{1} << m_vblankReciprocalShift) / static_cast<uint64_t>(vblankNs);
    } else {
        m_vblankReciprocal = 0;
        m_vblankReciprocalShift = 0;
    }

    // The base safety margin accounts for the kernel-reported time it takes the
    // hardware to latch a commit before vblank, plus our configured minimum.
    m_baseSafetyMargin = std::max<std::chrono::nanoseconds>(vblankTime, s_safetyMarginMinimum);
    m_safetyMargin = m_baseSafetyMargin + m_additionalSafetyMargin;
}

void DrmCommitThread::pageFlipped(std::chrono::nanoseconds timestamp)
{
    Q_ASSERT(!QCoreApplication::instance() || QThread::currentThread() == QCoreApplication::instance()->thread());

    // ---- Timestamp domain conversion (CLOCK_MONOTONIC → steady_clock) ----
    // Thread-local IIR-tracked offset; one resync every 4096 flips, single
    // add on the hot path.
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
        if (clock_gettime(CLOCK_MONOTONIC, &monoNowTs) != 0) {
            return;
        }
        const int64_t monoNowNs = static_cast<int64_t>(monoNowTs.tv_sec) * 1'000'000'000LL
            + static_cast<int64_t>(monoNowTs.tv_nsec);
        const int64_t newOffset = steadyNowNs - monoNowNs;
        if (!cache.valid) {
            cache.offsetNs = newOffset;
            cache.valid = true;
        } else {
            // IIR: offset += (new - offset) / 8
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

    const TimePoint mappedTime{
        std::chrono::duration_cast<TimePoint::duration>(std::chrono::nanoseconds{mappedNs})};

    const auto steadyNow = std::chrono::steady_clock::now();

    // =====================================================================
    // CORRECTNESS-CRITICAL: defer destruction of the in-flight commit.
    // =====================================================================
    //
    // We are called from inside the DRM event-dispatch chain on the very
    // object held by m_committed:
    //
    //     DrmGpu::pageFlipHandler(user_data = commit raw ptr)
    //       └─> commit->pageFlipped()              [virtual dispatch]
    //             ├─ updates plane "current" buffers
    //             ├─ delivers wp_presentation feedback
    //             └─ for each pipeline in this->m_pipelines:
    //                  └─> DrmPipeline::pageFlipped()
    //                        └─> THIS FUNCTION
    //
    // Destroying the commit here (e.g. m_committed.reset() or letting a
    // local unique_ptr fall out of scope) leaves the outer pageFlipped()
    // iterating freed memory the instant it touches `this` again → GPF.
    // Invoking the commit's pageFlipped() a SECOND time is equally wrong:
    // wayland clients see duplicate wp_presentation events (protocol
    // violation), and the duplicated maybeModeset() races the realtime
    // commit thread, producing the "EBUSY" storm you saw before the crash.
    //
    // m_committed is typed std::unique_ptr<DrmCommit> (base, so it can
    // hold either a DrmAtomicCommit or a DrmLegacyCommit), whereas
    // m_commitsToDelete is std::vector<std::unique_ptr<DrmAtomicCommit>>
    // and cannot accept a base-typed unique_ptr. Solution: wrap in a
    // shared_ptr and capture it into a Qt queued invocation; the lambda
    // runs on this same (main) thread on the next event-loop tick — after
    // the entire pageflip-event call stack has unwound — and the
    // capture's destructor disposes of the commit safely.
    std::shared_ptr<DrmCommit> deferred;
    {
        std::unique_lock lock(m_mutex);

        if (m_pageflipTimeoutDetected.load(std::memory_order_acquire)) [[unlikely]] {
            const auto elapsed = steadyNow - m_lastCommitTime;
            const auto elapsedMs =
                std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
            qCCritical(KWIN_DRM, "Pageflip arrived %lldms after the commit",
                       static_cast<long long>(elapsedMs));
            m_pageflipTimeoutDetected.store(false, std::memory_order_release);
        }

        if (mappedTime > m_lastPageflip) {
            m_lastPageflip = mappedTime;
        }

        if (m_committed) {
            // unique_ptr<DrmCommit> → shared_ptr<DrmCommit>: ownership is
            // transferred, m_committed becomes null. One tiny control-block
            // allocation; negligible at any realistic refresh rate.
            deferred = std::shared_ptr<DrmCommit>(std::move(m_committed));
        }
    }

    // The realtime commit thread may be parked in cv::wait_for while
    // m_committed was set. Now that the slot is free it can submit the
    // next queued frame; wake it. Notifying without holding the lock is
    // permitted by std::condition_variable and skips one futex round-trip.
    m_commitPending.notify_one();

    if (deferred) {
        // Queued connection: dispatched on the main thread after the
        // current stack fully unwinds. By that point the outer
        // commit->pageFlipped() has returned and no one else holds a
        // pointer to the object, so destruction is race-free.
        //
        // The lambda is intentionally empty: its capture is the only owner
        // of the shared_ptr after the std::move below, and the capture's
        // destructor (run when the queued event finishes) is what frees
        // the commit. `noexcept` is safe — neither shared_ptr destruction
        // nor any DrmCommit subclass destructor is expected to throw, and
        // throwing from a queued slot would terminate() the process.
        QMetaObject::invokeMethod(
            this,
            [held = std::move(deferred)]() noexcept {
                // Empty by design — destruction happens via the capture.
            },
            Qt::QueuedConnection);
    }
}

bool DrmCommitThread::pageflipsPending()
{
    std::unique_lock lock(m_mutex);
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
    if (vblankNs == 0) [[unlikely]] {
        return now + kDefaultFrameInterval;
    }

    uint64_t pageflipsSince = 0;
    if (m_vblankReciprocal > 0 && elapsedNs < (uint64_t{1} << 40)) {
        const unsigned __int128 product = static_cast<unsigned __int128>(elapsedNs) * m_vblankReciprocal;
        pageflipsSince = static_cast<uint64_t>(product >> m_vblankReciprocalShift);
    } else {
        pageflipsSince = elapsedNs / vblankNs;
    }
    pageflipsSince = std::min(pageflipsSince, kMaxPageflipSpan);

    const auto flips = static_cast<int64_t>(pageflipsSince + 1);
    return m_lastPageflip + m_minVblankInterval * flips;
}

std::chrono::nanoseconds DrmCommitThread::safetyMargin() const
{
    std::unique_lock lock(m_mutex);
    return m_safetyMargin;
}

void DrmCommitThread::handlePing()
{
    m_gpu->dispatchEvents();
    {
        std::unique_lock lock(m_mutex);
        m_ping.store(true, std::memory_order_release);
    }
    m_pong.notify_one();
}

} // namespace KWin
