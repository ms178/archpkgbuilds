/*
    SPDX-FileCopyrightText: 2024 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once
#include "kwin_export.h"
#include "utils/filedescriptor.h"

#include <cstdint>
#include <memory>

namespace KWin
{

class SyncTimeline;

class KWIN_EXPORT SyncReleasePoint
{
public:
    explicit SyncReleasePoint(std::shared_ptr<SyncTimeline> timeline, uint64_t timelinePoint);
    ~SyncReleasePoint();

    SyncReleasePoint(const SyncReleasePoint &) = delete;
    SyncReleasePoint &operator=(const SyncReleasePoint &) = delete;

    SyncReleasePoint(SyncReleasePoint &&other) noexcept;
    SyncReleasePoint &operator=(SyncReleasePoint &&other) noexcept;

    [[nodiscard]] SyncTimeline *timeline() const noexcept;
    [[nodiscard]] uint64_t timelinePoint() const noexcept;

    void addReleaseFence(const FileDescriptor &fd);

private:
    std::shared_ptr<SyncTimeline> m_timeline;
    uint64_t m_timelinePoint;
    FileDescriptor m_releaseFence;
};

class KWIN_EXPORT SyncTimeline
{
public:
    explicit SyncTimeline(int drmFd, uint32_t handle);
    explicit SyncTimeline(int drmFd);
    ~SyncTimeline();

    SyncTimeline(const SyncTimeline &) = delete;
    SyncTimeline &operator=(const SyncTimeline &) = delete;

    [[nodiscard]] FileDescriptor eventFd(uint64_t timelinePoint) const;

    const FileDescriptor &fileDescriptor();
    void signal(uint64_t timelinePoint);
    void moveInto(uint64_t timelinePoint, const FileDescriptor &fd);
    [[nodiscard]] FileDescriptor exportSyncFile(uint64_t timelinePoint);
    [[nodiscard]] bool isMaterialized(uint64_t timelinePoint);

private:
    const int m_drmFd;
    uint32_t m_handle = 0;
    FileDescriptor m_fileDescriptor;
};

inline SyncTimeline *SyncReleasePoint::timeline() const noexcept
{
    return m_timeline.get();
}

inline uint64_t SyncReleasePoint::timelinePoint() const noexcept
{
    return m_timelinePoint;
}

}
