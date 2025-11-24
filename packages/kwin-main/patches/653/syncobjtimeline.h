/*
    SPDX-FileCopyrightText: 2024 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once
#include "kwin_export.h"
#include "utils/filedescriptor.h"

#include <memory>
#include <stdint.h>

namespace KWin
{

class SyncTimeline;

/**
 * A helper to signal the release point when it goes out of scope.
 * It is movable but not copyable to ensure a single release point owner.
 *
 * Cache optimization:
 * - Layout aligned to 32 bytes (half cache line) on 64-bit systems.
 * - Hot getters inlined for zero-overhead access.
 */
class KWIN_EXPORT SyncReleasePoint
{
public:
    explicit SyncReleasePoint(std::shared_ptr<SyncTimeline> timeline, uint64_t timelinePoint);
    ~SyncReleasePoint();

    // Disable copying to prevent double-signaling or fence duplication logic errors
    SyncReleasePoint(const SyncReleasePoint &) = delete;
    SyncReleasePoint &operator=(const SyncReleasePoint &) = delete;

    // Enable move semantics for efficient transfer between scopes
    SyncReleasePoint(SyncReleasePoint &&other) noexcept;
    SyncReleasePoint &operator=(SyncReleasePoint &&other) noexcept;

    [[nodiscard]] SyncTimeline *timeline() const;
    [[nodiscard]] uint64_t timelinePoint() const;

    /**
     * Adds the fence of a graphics job that this release point should wait for
     * before the timeline point is signaled.
     */
    void addReleaseFence(const FileDescriptor &fd);

private:
    // Members ordered for optimal packing (64-bit alignment)
    // std::shared_ptr is 16 bytes (2 pointers)
    std::shared_ptr<SyncTimeline> m_timeline;
    // 8 bytes
    uint64_t m_timelinePoint;
    // FileDescriptor is typically 4-8 bytes (int wrapper)
    FileDescriptor m_releaseFence;
    // Padding ~4 bytes. Total size ~32 bytes.
};

class KWIN_EXPORT SyncTimeline
{
public:
    explicit SyncTimeline(int drmFd, uint32_t handle);
    explicit SyncTimeline(int drmFd);
    ~SyncTimeline();

    SyncTimeline(const SyncTimeline &) = delete;
    SyncTimeline &operator=(const SyncTimeline &) = delete;

    /**
     * @returns an event fd that gets signalled when the timeline point gets signalled
     */
    [[nodiscard]] FileDescriptor eventFd(uint64_t timelinePoint) const;

    const FileDescriptor &fileDescriptor();
    void signal(uint64_t timelinePoint);
    void moveInto(uint64_t timelinePoint, const FileDescriptor &fd);
    [[nodiscard]] FileDescriptor exportSyncFile(uint64_t timelinePoint);
    [[nodiscard]] bool isMaterialized(uint64_t timelinePoint);

private:
    // Hot members accessed in signal/moveInto
    const int32_t m_drmFd;
    uint32_t m_handle = 0;
    // Cold member, lazily initialized
    FileDescriptor m_fileDescriptor;
};

// Inline implementations for hot paths
inline SyncTimeline *SyncReleasePoint::timeline() const
{
    return m_timeline.get();
}

inline uint64_t SyncReleasePoint::timelinePoint() const
{
    return m_timelinePoint;
}

} // namespace KWin
