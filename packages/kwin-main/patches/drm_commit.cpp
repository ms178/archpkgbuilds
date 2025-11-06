/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_commit.h"
#include "core/renderbackend.h"
#include "drm_blob.h"
#include "drm_buffer.h"
#include "drm_connector.h"
#include "drm_crtc.h"
#include "drm_gpu.h"
#include "drm_object.h"
#include "drm_property.h"
#include "drm_logging.h"

#include <QCoreApplication>
#include <QThread>
#include <QVarLengthArray>
#include <algorithm>
#include <cerrno>
#include <cstring>

using namespace std::chrono_literals;

namespace KWin
{

DrmCommit::DrmCommit(DrmGpu *gpu)
    : m_gpu(gpu)
{
}

DrmCommit::~DrmCommit()
{
    Q_ASSERT(QThread::currentThread() == QCoreApplication::instance()->thread());
}

DrmGpu *DrmCommit::gpu() const
{
    return m_gpu;
}

void DrmCommit::setDefunct()
{
    m_defunct = true;
}

DrmAtomicCommit::DrmAtomicCommit(DrmGpu *gpu)
    : DrmCommit(gpu)
{
}

DrmAtomicCommit::DrmAtomicCommit(const QList<DrmPipeline *> &pipelines)
    : DrmCommit(pipelines.front()->gpu())
    , m_pipelines(pipelines)
{
}

void DrmAtomicCommit::addProperty(const DrmProperty &prop, uint64_t value)
{
    if (Q_UNLIKELY(!prop.isValid())) {
        qCWarning(KWIN_DRM) << "Trying to add an invalid property" << prop.name() << "to object" << prop.drmObject()->id();
        return;
    }

#ifndef NDEBUG
    prop.checkValueInRange(value);
#endif

    m_properties[prop.drmObject()->id()][prop.propId()] = value;
}

void DrmAtomicCommit::addBlob(const DrmProperty &prop, const std::shared_ptr<DrmBlob> &blob)
{
    addProperty(prop, blob ? blob->blobId() : 0);
    m_blobs[&prop] = blob;
}

void DrmAtomicCommit::addBuffer(DrmPlane *plane, const std::shared_ptr<DrmFramebuffer> &buffer, const std::shared_ptr<OutputFrame> &frame)
{
    addProperty(plane->fbId, buffer ? buffer->framebufferId() : 0);
    m_buffers[plane] = buffer;
    m_frames[plane] = frame;

    if (plane->inFenceFd.isValid() && buffer) [[likely]] {
        if (plane->gpu()->isNVidia()) [[unlikely]] {
        } else {
            const int fenceFd = buffer->syncFd().get();
            if (fenceFd >= 0) [[likely]] {
                addProperty(plane->inFenceFd, static_cast<uint64_t>(fenceFd));
            }
        }
    }

    m_planes.emplace(plane);

    if (frame) {
        if (m_targetPageflipTime) {
            m_targetPageflipTime = std::min(*m_targetPageflipTime, frame->targetPageflipTime());
        } else {
            m_targetPageflipTime = frame->targetPageflipTime();
        }
    }
}

void DrmAtomicCommit::setVrr(DrmCrtc *crtc, bool vrr)
{
    addProperty(crtc->vrrEnabled, vrr ? 1 : 0);
    m_vrr = vrr;
}

void DrmAtomicCommit::setPresentationMode(PresentationMode mode)
{
    m_mode = mode;
}

bool DrmAtomicCommit::test()
{
    uint32_t flags = DRM_MODE_ATOMIC_TEST_ONLY | DRM_MODE_ATOMIC_NONBLOCK;
    if (isTearing()) {
        flags |= DRM_MODE_PAGE_FLIP_ASYNC;
    }
    return doCommit(flags);
}

bool DrmAtomicCommit::testAllowModeset()
{
    return doCommit(DRM_MODE_ATOMIC_TEST_ONLY | DRM_MODE_ATOMIC_ALLOW_MODESET);
}

bool DrmAtomicCommit::commit()
{
    uint32_t flags = DRM_MODE_ATOMIC_NONBLOCK | DRM_MODE_PAGE_FLIP_EVENT;
    if (isTearing()) {
        flags |= DRM_MODE_PAGE_FLIP_ASYNC;
    }
    return doCommit(flags);
}

bool DrmAtomicCommit::commitModeset()
{
    m_modeset = true;
    return doCommit(DRM_MODE_ATOMIC_ALLOW_MODESET);
}

bool DrmAtomicCommit::doCommit(uint32_t flags)
{
    constexpr size_t inlineObjectCapacity = 16;
    constexpr size_t inlinePropertyCapacity = 256;

    QVarLengthArray<uint32_t, inlineObjectCapacity> objects;
    QVarLengthArray<uint32_t, inlineObjectCapacity> propertyCounts;
    QVarLengthArray<uint32_t, inlinePropertyCapacity> propertyIds;
    QVarLengthArray<uint64_t, inlinePropertyCapacity> values;

    const auto objectCount = std::min(m_properties.size(), static_cast<size_t>(INT_MAX));
    objects.reserve(static_cast<int>(objectCount));
    propertyCounts.reserve(static_cast<int>(objectCount));

#if defined(__x86_64__) || defined(_M_X64)
    if (!m_properties.empty()) {
        __builtin_prefetch(&(*m_properties.begin()), 0, 3);
    }
#endif

    for (const auto &[object, properties] : m_properties) {
        objects.push_back(object);
        propertyCounts.push_back(static_cast<uint32_t>(properties.size()));
        for (const auto &[property, value] : properties) {
            propertyIds.push_back(property);
            values.push_back(value);
        }
    }

    drm_mode_atomic commitData{
        .flags = flags,
        .count_objs = static_cast<uint32_t>(objects.size()),
        .objs_ptr = reinterpret_cast<uint64_t>(objects.data()),
        .count_props_ptr = reinterpret_cast<uint64_t>(propertyCounts.data()),
        .props_ptr = reinterpret_cast<uint64_t>(propertyIds.data()),
        .prop_values_ptr = reinterpret_cast<uint64_t>(values.data()),
        .reserved = 0,
        .user_data = reinterpret_cast<uint64_t>(this),
    };

    if (drmIoctl(m_gpu->fd(), DRM_IOCTL_MODE_ATOMIC, &commitData) == 0) [[likely]] {
        return true;
    }

    qCWarning(KWIN_DRM) << "Atomic commit failed: errno =" << errno << "(" << strerror(errno) << ");"
                        << "objects =" << objects.size() << ", properties =" << propertyIds.size();
    return false;
}

void DrmAtomicCommit::pageFlipped(std::chrono::nanoseconds timestamp)
{
    Q_ASSERT(QThread::currentThread() == QCoreApplication::instance()->thread());

    for (const auto &[plane, buffer] : m_buffers) {
        plane->setCurrentBuffer(buffer);
    }

    if (m_defunct) {
        return;
    }

    const size_t frameCount = m_frames.size();

    if (frameCount == 1) [[likely]] {
        const auto &frame = m_frames.begin()->second;
        if (frame) [[likely]] {
            frame->presented(timestamp, m_mode);
        }
    } else if (frameCount > 1) {
        QVarLengthArray<OutputFrame *, 8> frames;
        frames.reserve(static_cast<int>(std::min(frameCount, static_cast<size_t>(8))));
        for (const auto &[plane, frame] : m_frames) {
            if (frame) {
                frames.append(frame.get());
            }
        }

        if (!frames.isEmpty()) {
            std::sort(frames.begin(), frames.end());
            const auto last = std::unique(frames.begin(), frames.end());
            for (auto it = frames.begin(); it != last; ++it) {
                (*it)->presented(timestamp, m_mode);
            }
        }
    }

    m_frames.clear();

    for (const auto pipeline : std::as_const(m_pipelines)) {
        pipeline->pageFlipped(timestamp);
    }
}

bool DrmAtomicCommit::areBuffersReadable() const
{
    return std::ranges::all_of(m_buffers, [](const auto &pair) {
        const auto &[plane, buffer] = pair;
        return !buffer || buffer->isReadable();
    });
}

void DrmAtomicCommit::setDeadline(std::chrono::steady_clock::time_point deadline)
{
    for (const auto &[plane, buffer] : m_buffers) {
        if (buffer) {
            buffer->setDeadline(deadline);
        }
    }
}

std::optional<bool> DrmAtomicCommit::isVrr() const
{
    return m_vrr;
}

const std::unordered_set<DrmPlane *> &DrmAtomicCommit::modifiedPlanes() const
{
    return m_planes;
}

void DrmAtomicCommit::merge(DrmAtomicCommit *onTop)
{
    if (!onTop) [[unlikely]] {
        return;
    }

    for (const auto &[obj, properties] : onTop->m_properties) {
        auto [it, inserted] = m_properties.try_emplace(obj);
        auto &ownProperties = it->second;
        for (const auto &[prop, value] : properties) {
            ownProperties.insert_or_assign(prop, value);
        }
    }

    for (const auto &[plane, buffer] : onTop->m_buffers) {
        m_buffers[plane] = buffer;
        m_frames[plane] = onTop->m_frames[plane];
        m_planes.emplace(plane);
    }

    for (const auto &[prop, blob] : onTop->m_blobs) {
        m_blobs[prop] = blob;
    }

    if (onTop->m_vrr) {
        m_vrr = onTop->m_vrr;
    }

    if (!m_targetPageflipTime) {
        m_targetPageflipTime = onTop->m_targetPageflipTime;
    } else if (onTop->m_targetPageflipTime) {
        *m_targetPageflipTime = std::min(*m_targetPageflipTime, *onTop->m_targetPageflipTime);
    }

    if (m_allowedVrrDelay && onTop->m_allowedVrrDelay) {
        *m_allowedVrrDelay = std::min(*m_allowedVrrDelay, *onTop->m_allowedVrrDelay);
    } else {
        m_allowedVrrDelay.reset();
    }
}

void DrmAtomicCommit::setAllowedVrrDelay(std::optional<std::chrono::nanoseconds> allowedDelay)
{
    m_allowedVrrDelay = allowedDelay;
}

std::optional<std::chrono::nanoseconds> DrmAtomicCommit::allowedVrrDelay() const
{
    return m_allowedVrrDelay;
}

std::optional<std::chrono::steady_clock::time_point> DrmAtomicCommit::targetPageflipTime() const
{
    return m_targetPageflipTime;
}

bool DrmAtomicCommit::isReadyFor(std::chrono::steady_clock::time_point pageflipTarget) const
{
    static constexpr auto s_pageflipSlop = 500us;
    return (!m_targetPageflipTime || pageflipTarget + s_pageflipSlop >= *m_targetPageflipTime) && areBuffersReadable();
}

bool DrmAtomicCommit::isTearing() const
{
    return m_mode == PresentationMode::Async || m_mode == PresentationMode::AdaptiveAsync;
}

DrmLegacyCommit::DrmLegacyCommit(DrmPipeline *pipeline, const std::shared_ptr<DrmFramebuffer> &buffer, const std::shared_ptr<OutputFrame> &frame)
    : DrmCommit(pipeline->gpu())
    , m_pipeline(pipeline)
    , m_crtc(m_pipeline->crtc())
    , m_buffer(buffer)
    , m_frame(frame)
{
}

bool DrmLegacyCommit::doModeset(DrmConnector *connector, DrmConnectorMode *mode)
{
    uint32_t connectorId = connector->id();
    if (drmModeSetCrtc(gpu()->fd(), m_crtc->id(), m_buffer->framebufferId(), 0, 0, &connectorId, 1, mode->nativeMode()) == 0) {
        m_crtc->setCurrent(m_buffer);
        return true;
    } else {
        return false;
    }
}

bool DrmLegacyCommit::doPageflip(PresentationMode mode)
{
    m_mode = mode;
    uint32_t flags = DRM_MODE_PAGE_FLIP_EVENT;
    if (mode == PresentationMode::Async || mode == PresentationMode::AdaptiveAsync) {
        flags |= DRM_MODE_PAGE_FLIP_ASYNC;
    }
    return drmModePageFlip(gpu()->fd(), m_crtc->id(), m_buffer->framebufferId(), flags, this) == 0;
}

void DrmLegacyCommit::pageFlipped(std::chrono::nanoseconds timestamp)
{
    Q_ASSERT(QThread::currentThread() == QCoreApplication::instance()->thread());
    m_crtc->setCurrent(m_buffer);
    if (m_defunct) {
        return;
    }
    if (m_frame) {
        m_frame->presented(timestamp, m_mode);
        m_frame.reset();
    }
    m_pipeline->pageFlipped(timestamp);
}

}

#include "moc_drm_commit.cpp"
