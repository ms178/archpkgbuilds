/*
    SPDX-FileCopyrightText: 2024 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "syncobjtimeline.h"

#include <cerrno>
#include <cstring>
#include <sys/eventfd.h>
#include <sys/ioctl.h>
#include <xf86drm.h>

#if defined(Q_OS_LINUX)
#include <linux/sync_file.h>
#else
struct sync_merge_data
{
    char name[32];
    __s32 fd2;
    __s32 fence;
    __u32 flags;
    __u32 pad;
};
#define SYNC_IOC_MAGIC '>'
#define SYNC_IOC_MERGE _IOWR(SYNC_IOC_MAGIC, 3, struct sync_merge_data)
#endif

namespace KWin
{

namespace
{

struct alignas(8) ThreadLocalScratch
{
    uint32_t handle = 0;
    int fd = -1;

    ~ThreadLocalScratch()
    {
        if (handle != 0 && fd >= 0) {
            drmSyncobjDestroy(fd, handle);
        }
    }

    uint32_t get(int drmFd) noexcept
    {
        if (handle != 0 && fd == drmFd) [[likely]] {
            return handle;
        }
        if (handle != 0 && fd >= 0) {
            drmSyncobjDestroy(fd, handle);
            handle = 0;
        }
        fd = drmFd;
        if (drmSyncobjCreate(drmFd, 0, &handle) != 0) {
            handle = 0;
        }
        return handle;
    }
};

static_assert(sizeof(ThreadLocalScratch) == 8, "ThreadLocalScratch size mismatch");

thread_local ThreadLocalScratch s_scratch;

constexpr char s_mergeName[] = "KWin merge";
constexpr int s_maxIoctlRetries = 8;

FileDescriptor mergeSyncFds(const FileDescriptor &fd1, const FileDescriptor &fd2)
{
    struct sync_merge_data data = {};
    static_assert(sizeof(s_mergeName) <= sizeof(data.name), "merge name too long");
    std::memcpy(data.name, s_mergeName, sizeof(s_mergeName));
    data.fd2 = fd2.get();
    data.fence = -1;

    int ret;
    int retries = 0;
    do {
        ret = ioctl(fd1.get(), SYNC_IOC_MERGE, &data);
    } while (ret == -1 && (errno == EINTR || errno == EAGAIN) && ++retries < s_maxIoctlRetries);

    if (ret < 0) [[unlikely]] {
        return FileDescriptor{};
    }
    return FileDescriptor(data.fence);
}

}

SyncReleasePoint::SyncReleasePoint(std::shared_ptr<SyncTimeline> timeline, uint64_t timelinePoint)
    : m_timeline(std::move(timeline))
    , m_timelinePoint(timelinePoint)
{
}

SyncReleasePoint::~SyncReleasePoint()
{
    if (m_timeline) [[likely]] {
        if (m_releaseFence.isValid()) {
            m_timeline->moveInto(m_timelinePoint, m_releaseFence);
        } else {
            m_timeline->signal(m_timelinePoint);
        }
    }
}

SyncReleasePoint::SyncReleasePoint(SyncReleasePoint &&other) noexcept
    : m_timeline(std::move(other.m_timeline))
    , m_timelinePoint(other.m_timelinePoint)
    , m_releaseFence(std::move(other.m_releaseFence))
{
}

SyncReleasePoint &SyncReleasePoint::operator=(SyncReleasePoint &&other) noexcept
{
    if (this != &other) [[likely]] {
        if (m_timeline) {
            if (m_releaseFence.isValid()) {
                m_timeline->moveInto(m_timelinePoint, m_releaseFence);
            } else {
                m_timeline->signal(m_timelinePoint);
            }
        }
        m_timeline = std::move(other.m_timeline);
        m_timelinePoint = other.m_timelinePoint;
        m_releaseFence = std::move(other.m_releaseFence);
    }
    return *this;
}

void SyncReleasePoint::addReleaseFence(const FileDescriptor &fd)
{
    if (!fd.isValid()) [[unlikely]] {
        return;
    }
    if (m_releaseFence.isValid()) {
        m_releaseFence = mergeSyncFds(m_releaseFence, fd);
    } else {
        m_releaseFence = fd.duplicate();
    }
}

SyncTimeline::SyncTimeline(int drmFd, uint32_t handle)
    : m_drmFd(drmFd)
    , m_handle(handle)
{
}

SyncTimeline::SyncTimeline(int drmFd)
    : m_drmFd(drmFd)
{
    if (drmSyncobjCreate(m_drmFd, 0, &m_handle) != 0) {
        m_handle = 0;
    }
}

SyncTimeline::~SyncTimeline()
{
    if (m_handle != 0) {
        drmSyncobjDestroy(m_drmFd, m_handle);
    }
}

const FileDescriptor &SyncTimeline::fileDescriptor()
{
    if (!m_fileDescriptor.isValid() && m_handle != 0) {
        int fd = -1;
        if (drmSyncobjHandleToFD(m_drmFd, m_handle, &fd) == 0) {
            m_fileDescriptor = FileDescriptor(fd);
        }
    }
    return m_fileDescriptor;
}

FileDescriptor SyncTimeline::eventFd(uint64_t timelinePoint) const
{
    if (m_handle == 0) [[unlikely]] {
        return {};
    }
    FileDescriptor ret{eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK)};
    if (!ret.isValid()) [[unlikely]] {
        return {};
    }
    if (drmSyncobjEventfd(m_drmFd, m_handle, timelinePoint, ret.get(), 0) != 0) [[unlikely]] {
        return {};
    }
    return ret;
}

void SyncTimeline::signal(uint64_t timelinePoint)
{
    if (m_handle != 0) [[likely]] {
        drmSyncobjTimelineSignal(m_drmFd, &m_handle, &timelinePoint, 1);
    }
}

void SyncTimeline::moveInto(uint64_t timelinePoint, const FileDescriptor &fd)
{
    if (m_handle == 0) [[unlikely]] {
        return;
    }
    if (!fd.isValid()) [[unlikely]] {
        signal(timelinePoint);
        return;
    }

    uint32_t tempHandle = s_scratch.get(m_drmFd);
    bool success = false;

    if (tempHandle != 0) [[likely]] {
        if (drmSyncobjImportSyncFile(m_drmFd, tempHandle, fd.get()) == 0) [[likely]] {
            if (drmSyncobjTransfer(m_drmFd, m_handle, timelinePoint, tempHandle, 0, 0) == 0) [[likely]] {
                success = true;
            }
        }
    }

    if (!success) [[unlikely]] {
        signal(timelinePoint);
    }
}

FileDescriptor SyncTimeline::exportSyncFile(uint64_t timelinePoint)
{
    if (m_handle == 0) [[unlikely]] {
        return {};
    }

    uint32_t tempHandle = s_scratch.get(m_drmFd);
    if (tempHandle == 0) [[unlikely]] {
        return {};
    }

    int syncFileFd = -1;
    if (drmSyncobjTransfer(m_drmFd, tempHandle, 0, m_handle, timelinePoint, 0) == 0) [[likely]] {
        drmSyncobjExportSyncFile(m_drmFd, tempHandle, &syncFileFd);
    }

    return FileDescriptor(syncFileFd);
}

bool SyncTimeline::isMaterialized(uint64_t timelinePoint)
{
    if (m_handle == 0) [[unlikely]] {
        return false;
    }
    return drmSyncobjTimelineWait(m_drmFd,
                                  &m_handle,
                                  &timelinePoint,
                                  1,
                                  0,
                                  DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE,
                                  nullptr) == 0;
}

}
