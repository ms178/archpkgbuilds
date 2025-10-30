/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2016 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_plane.h"

#include "config-kwin.h"

#include "drm_buffer.h"
#include "drm_commit.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "drm_pointer.h"
#include "utils/drm_format_helper.h"

#include <drm_fourcc.h>
#include <algorithm>
#include <cstdint>
#include <limits>
#include <span>

namespace KWin
{

DrmPlane::DrmPlane(DrmGpu *gpu, uint32_t planeId)
    : DrmObject(gpu, planeId, DRM_MODE_OBJECT_PLANE)
    , type(this, QByteArrayLiteral("type"),
           {QByteArrayLiteral("Overlay"), QByteArrayLiteral("Primary"), QByteArrayLiteral("Cursor")})
    , srcX(this, QByteArrayLiteral("SRC_X"))
    , srcY(this, QByteArrayLiteral("SRC_Y"))
    , srcW(this, QByteArrayLiteral("SRC_W"))
    , srcH(this, QByteArrayLiteral("SRC_H"))
    , crtcX(this, QByteArrayLiteral("CRTC_X"))
    , crtcY(this, QByteArrayLiteral("CRTC_Y"))
    , crtcW(this, QByteArrayLiteral("CRTC_W"))
    , crtcH(this, QByteArrayLiteral("CRTC_H"))
    , fbId(this, QByteArrayLiteral("FB_ID"))
    , crtcId(this, QByteArrayLiteral("CRTC_ID"))
    , rotation(this, QByteArrayLiteral("rotation"),
               {QByteArrayLiteral("rotate-0"), QByteArrayLiteral("rotate-90"), QByteArrayLiteral("rotate-180"),
                QByteArrayLiteral("rotate-270"), QByteArrayLiteral("reflect-x"), QByteArrayLiteral("reflect-y")})
    , inFormats(this, QByteArrayLiteral("IN_FORMATS"))
    , alpha(this, QByteArrayLiteral("alpha"))
    , pixelBlendMode(this, QByteArrayLiteral("pixel blend mode"),
                     {QByteArrayLiteral("None"), QByteArrayLiteral("Pre-multiplied"), QByteArrayLiteral("Coverage")})
    , colorEncoding(this, QByteArrayLiteral("COLOR_ENCODING"),
                    {QByteArrayLiteral("ITU-R BT.601 YCbCr"), QByteArrayLiteral("ITU-R BT.709 YCbCr"),
                     QByteArrayLiteral("ITU-R BT.2020 YCbCr")})
    , colorRange(this, QByteArrayLiteral("COLOR_RANGE"),
                 {QByteArrayLiteral("YCbCr limited range"), QByteArrayLiteral("YCbCr full range")})
    , vmHotspotX(this, QByteArrayLiteral("HOTSPOT_X"))
    , vmHotspotY(this, QByteArrayLiteral("HOTSPOT_Y"))
    , inFenceFd(this, QByteArrayLiteral("IN_FENCE_FD"))
    , sizeHints(this, QByteArrayLiteral("SIZE_HINTS"))
    , inFormatsForTearing(this, QByteArrayLiteral("IN_FORMATS_ASYNC"))
    , zpos(this, QByteArrayLiteral("zpos"))
{
}

bool DrmPlane::init()
{
    return updateProperties();
}

bool DrmPlane::updateProperties()
{
    DrmUniquePtr<drmModePlane> p(drmModeGetPlane(gpu()->fd(), id()));
    if (!p) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Failed to get kernel plane" << id();
        return false;
    }

    DrmPropertyList props = queryProperties();

    type.update(props);
    srcX.update(props);
    srcY.update(props);
    srcW.update(props);
    srcH.update(props);
    crtcX.update(props);
    crtcY.update(props);
    crtcW.update(props);
    crtcH.update(props);
    fbId.update(props);
    crtcId.update(props);
    rotation.update(props);
    inFormats.update(props);
    alpha.update(props);
    pixelBlendMode.update(props);
    colorEncoding.update(props);
    colorRange.update(props);
    vmHotspotX.update(props);
    vmHotspotY.update(props);
    inFenceFd.update(props);
    sizeHints.update(props);
    inFormatsForTearing.update(props);
    zpos.update(props);

    if (!type.isValid() || !srcX.isValid() || !srcY.isValid() || !srcW.isValid() || !srcH.isValid()
        || !crtcX.isValid() || !crtcY.isValid() || !crtcW.isValid() || !crtcH.isValid() || !fbId.isValid()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Failed to update the basic plane properties for plane" << id();
        return false;
    }

    m_possibleCrtcs = p->possible_crtcs;

    m_supportedFormats.clear();
    m_supportedFormats.reserve(128);

    if (inFormats.isValid() && inFormats.immutableBlob() && gpu()->addFB2ModifiersSupported()) [[likely]] {
        drmModePropertyBlobPtr blob = inFormats.immutableBlob();

        if (blob && blob->data && blob->length > 0) [[likely]] {
            __builtin_prefetch(blob->data, 0, 3);
            drmModeFormatModifierIterator iterator{};
            while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
                m_supportedFormats[iterator.fmt].push_back(iterator.mod);
            }
        } else [[unlikely]] {
            qCWarning(KWIN_DRM) << "IN_FORMATS blob is empty for plane" << id();
        }
    } else {
        const bool isCursor = (type.enumValue() == TypeIndex::Cursor);
        const uint64_t modifier = isCursor ? DRM_FORMAT_MOD_LINEAR : DRM_FORMAT_MOD_INVALID;
        for (uint32_t i = 0; i < p->count_formats; i++) {
            m_supportedFormats[p->formats[i]] = {modifier};
        }
    }

    if (m_supportedFormats.isEmpty()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Driver advertises no formats for plane" << id() << "- falling back to XRGB8888";
        const bool isCursor = (type.enumValue() == TypeIndex::Cursor);
        const uint64_t modifier = isCursor ? DRM_FORMAT_MOD_LINEAR : DRM_FORMAT_MOD_INVALID;
        m_supportedFormats.insert(DRM_FORMAT_XRGB8888, {modifier});
    }

    // Upstream change: Implement low bandwidth format selection with FormatInfo
    m_lowBandwidthFormats.clear();
    m_lowBandwidthFormats.reserve(m_supportedFormats.size());
    for (auto it = m_supportedFormats.constBegin(); it != m_supportedFormats.constEnd(); ++it) {
        const auto info = FormatInfo::get(it.key());
        if (info && info->bitsPerPixel <= 32) {
            if (it.value().contains(DRM_FORMAT_MOD_INVALID)) {
                // Mesa usually picks the modifier with lowest bandwidth requirements,
                // so prefer implicit modifiers for low bandwidth if supported
                m_lowBandwidthFormats.insert(it.key(), {DRM_FORMAT_MOD_INVALID});
            } else {
                m_lowBandwidthFormats.insert(it.key(), it.value());
            }
        }
    }

    m_sizeHints.clear();
    if (sizeHints.isValid() && sizeHints.immutableBlob()) {
        drmModePropertyBlobPtr blob = sizeHints.immutableBlob();
        constexpr size_t hintSize = sizeof(uint16_t) * 2;

        if (blob && blob->data && blob->length >= hintSize && (blob->length % hintSize) == 0) [[likely]] {
            const size_t hintCount = blob->length / hintSize;
            if (hintCount <= static_cast<size_t>(std::numeric_limits<int>::max())) {
                m_sizeHints.reserve(static_cast<int>(hintCount));
                auto hints = std::span(reinterpret_cast<const uint16_t *>(blob->data), hintCount * 2);
                for (size_t i = 0; i < hintCount; i++) {
                    m_sizeHints.append(QSize(hints[i * 2], hints[i * 2 + 1]));
                }
            }
        } else if (blob && blob->data && blob->length > 0) [[unlikely]] {
            qCWarning(KWIN_DRM) << "SIZE_HINTS blob has invalid length" << blob->length << "for plane" << id();
        }
    }

    if (m_sizeHints.isEmpty() && type.enumValue() == TypeIndex::Cursor) [[unlikely]] {
        m_sizeHints.append(gpu()->cursorSize());
    }

    if (inFormatsForTearing.isValid() && inFormatsForTearing.immutableBlob() && gpu()->addFB2ModifiersSupported()) {
        drmModePropertyBlobPtr blob = inFormatsForTearing.immutableBlob();
        m_supportedTearingFormats.clear();
        m_supportedTearingFormats.reserve(128);
        if (blob && blob->data && blob->length > 0) [[likely]] {
            __builtin_prefetch(blob->data, 0, 3);
            drmModeFormatModifierIterator iterator{};
            while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
                m_supportedTearingFormats[iterator.fmt].push_back(iterator.mod);
            }
        }
    } else {
        m_supportedTearingFormats = m_supportedFormats;
    }

    return true;
}

void DrmPlane::set(DrmAtomicCommit *commit, const QRect &src, const QRect &dst)
{
    if (dst.width() <= 0 || dst.height() <= 0) [[unlikely]] {
        disable(commit);
        return;
    }

    constexpr int maxCoord = 32767;
    constexpr int minCoord = -32768;

    const int clampedSrcX = std::clamp(src.x(), minCoord, maxCoord);
    const int clampedSrcY = std::clamp(src.y(), minCoord, maxCoord);
    const int clampedSrcW = std::clamp(src.width(), 0, maxCoord);
    const int clampedSrcH = std::clamp(src.height(), 0, maxCoord);

    const auto toFixed16 = [](int val) -> uint64_t {
        // UB FIX: Safe cast to preserve two's complement for negative numbers.
        return static_cast<uint64_t>(static_cast<uint32_t>(val)) << 16;
    };

    commit->addProperty(srcX, toFixed16(clampedSrcX));
    commit->addProperty(srcY, toFixed16(clampedSrcY));
    commit->addProperty(srcW, toFixed16(clampedSrcW));
    commit->addProperty(srcH, toFixed16(clampedSrcH));

    commit->addProperty(crtcX, dst.x());
    commit->addProperty(crtcY, dst.y());
    commit->addProperty(crtcW, dst.width());
    commit->addProperty(crtcH, dst.height());
}

bool DrmPlane::isCrtcSupported(int pipeIndex) const
{
    return (m_possibleCrtcs & (1 << pipeIndex));
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::lowBandwidthFormats() const
{
    return m_lowBandwidthFormats;
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::formats() const
{
    return m_supportedFormats;
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::tearingFormats() const
{
    return m_supportedTearingFormats;
}

std::shared_ptr<DrmFramebuffer> DrmPlane::currentBuffer() const
{
    return m_current;
}

void DrmPlane::setCurrentBuffer(const std::shared_ptr<DrmFramebuffer> &b)
{
    if (m_current == b) [[likely]] {
        return;
    }

    m_current = b;

    if (!b) [[unlikely]] {
        return;
    }

    const auto newData = b->data();

    // PERFORMANCE: This branchless version is faster than a loop for a fixed small size.
    const bool found = (m_lastBuffers[0] == newData) | (m_lastBuffers[1] == newData)
        | (m_lastBuffers[2] == newData) | (m_lastBuffers[3] == newData);

    if (!found) {
        m_lastBuffers[m_lastBufferWriteIndex] = newData;
        m_lastBufferWriteIndex = (m_lastBufferWriteIndex + 1) & 3; // Bitwise AND for fast modulo 4
    }
}

void DrmPlane::disable(DrmAtomicCommit *commit)
{
    commit->addProperty(crtcId, 0);
    commit->addBuffer(this, nullptr, nullptr);
}

void DrmPlane::releaseCurrentBuffer()
{
    if (m_current) [[likely]] {
        m_current->releaseBuffer();
    }
}

DrmPlane::Transformations DrmPlane::outputTransformToPlaneTransform(OutputTransform transform)
{
    switch (transform.kind()) {
    case OutputTransform::Kind::Normal:
        return Transformation::Rotate0;
    case OutputTransform::Kind::Rotate90:
        return Transformation::Rotate270;
    case OutputTransform::Kind::Rotate180:
        return Transformation::Rotate180;
    case OutputTransform::Kind::Rotate270:
        return Transformation::Rotate90;
    case OutputTransform::Kind::FlipY:
        return Transformation::Rotate0 | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY90:
        return Transformation::Rotate270 | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY180:
        return Transformation::Rotate180 | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY270:
        return Transformation::Rotate90 | Transformation::ReflectY;
    }
    Q_UNREACHABLE();
}

bool DrmPlane::supportsTransformation(OutputTransform transform) const
{
    return rotation.isValid() && rotation.hasEnum(outputTransformToPlaneTransform(transform));
}

const QList<QSize> &DrmPlane::recommendedSizes() const
{
    return m_sizeHints;
}

} // namespace KWin

#include "moc_drm_plane.cpp"
