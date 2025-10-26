/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2016 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_plane.h"

#include "config-kwin.h"

#include "drm_buffer.h"
#include "drm_commit.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "drm_pointer.h"

#include <drm_fourcc.h>
#include <algorithm>
#include <cstdint>
#include <limits>

namespace KWin
{

DrmPlane::DrmPlane(DrmGpu *gpu, uint32_t planeId)
    : DrmObject(gpu, planeId, DRM_MODE_OBJECT_PLANE)
    , type(this, QByteArrayLiteral("type"), {
                                                QByteArrayLiteral("Overlay"),
                                                QByteArrayLiteral("Primary"),
                                                QByteArrayLiteral("Cursor"),
                                            })
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
    , rotation(this, QByteArrayLiteral("rotation"), {
                                                        QByteArrayLiteral("rotate-0"),
                                                        QByteArrayLiteral("rotate-90"),
                                                        QByteArrayLiteral("rotate-180"),
                                                        QByteArrayLiteral("rotate-270"),
                                                        QByteArrayLiteral("reflect-x"),
                                                        QByteArrayLiteral("reflect-y"),
                                                    })
    , inFormats(this, QByteArrayLiteral("IN_FORMATS"))
    , alpha(this, QByteArrayLiteral("alpha"))
    , pixelBlendMode(this, QByteArrayLiteral("pixel blend mode"), {
                                                                      QByteArrayLiteral("None"),
                                                                      QByteArrayLiteral("Pre-multiplied"),
                                                                      QByteArrayLiteral("Coverage"),
                                                                  })
    , colorEncoding(this, QByteArrayLiteral("COLOR_ENCODING"), {
                                                                   QByteArrayLiteral("ITU-R BT.601 YCbCr"),
                                                                   QByteArrayLiteral("ITU-R BT.709 YCbCr"),
                                                                   QByteArrayLiteral("ITU-R BT.2020 YCbCr"),
                                                               })
    , colorRange(this, QByteArrayLiteral("COLOR_RANGE"), {
                                                             QByteArrayLiteral("YCbCr limited range"),
                                                             QByteArrayLiteral("YCbCr full range"),
                                                         })
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

    // Update all properties - batch to improve cache locality
    // All property objects share the same underlying props list
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

    // Validate critical properties required for plane operation
    if (!type.isValid() || !srcX.isValid() || !srcY.isValid() || !srcW.isValid() || !srcH.isValid()
        || !crtcX.isValid() || !crtcY.isValid() || !crtcW.isValid() || !crtcH.isValid() || !fbId.isValid()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Failed to update the basic plane properties for plane" << id();
        return false;
    }

    m_possibleCrtcs = p->possible_crtcs;

    // Parse supported formats with modifiers
    // Reserve capacity upfront to avoid rehashing (typical: 10-30 formats)
    m_supportedFormats.clear();
    m_supportedFormats.reserve(32);

    if (inFormats.isValid() && inFormats.immutableBlob() && gpu()->addFB2ModifiersSupported()) [[likely]] {
        drmModePropertyBlobPtr blob = inFormats.immutableBlob();

        // SAFETY: Validate blob before iteration
        // Malformed blobs can crash the compositor if not validated
        if (!blob->data || blob->length == 0) [[unlikely]] {
            qCWarning(KWIN_DRM) << "IN_FORMATS blob is empty for plane" << id();
        } else {
            // Parse format/modifier pairs from blob
            drmModeFormatModifierIterator iterator{};
            while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
                m_supportedFormats[iterator.fmt].push_back(iterator.mod);
            }
        }
    } else {
        // Legacy path: read formats from drmModePlane without explicit modifiers
        // Cursor planes require linear layout for CPU access (DRM_FORMAT_MOD_LINEAR)
        // Other planes use implicit modifier (DRM_FORMAT_MOD_INVALID)
        const bool isCursor = (type.enumValue() == TypeIndex::Cursor);
        const uint64_t modifier = isCursor ? DRM_FORMAT_MOD_LINEAR : DRM_FORMAT_MOD_INVALID;

        for (uint32_t i = 0; i < p->count_formats; i++) {
            m_supportedFormats[p->formats[i]] = QList<uint64_t>{modifier};
        }

        // Defensive fallback: if driver advertises no formats, assume XRGB8888
        if (m_supportedFormats.isEmpty()) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Driver advertises no formats for plane" << id()
                                << "- falling back to XRGB8888";
            m_supportedFormats.insert(DRM_FORMAT_XRGB8888, QList<uint64_t>{modifier});
        }
    }

    // Build implicit-modifier-only formats map
    // This is used for legacy code paths that don't support explicit modifiers
    m_implicitModifierOnlyFormats.clear();
    m_implicitModifierOnlyFormats.reserve(m_supportedFormats.size());

    const QList<uint64_t> implicitOnly = {DRM_FORMAT_MOD_INVALID};
    for (auto it = m_supportedFormats.constBegin(); it != m_supportedFormats.constEnd(); ++it) {
        m_implicitModifierOnlyFormats.insert(it.key(), implicitOnly);
    }

    // Parse size hints blob (preferred plane buffer sizes)
    m_sizeHints.clear();
    if (sizeHints.isValid() && sizeHints.immutableBlob()) {
        drmModePropertyBlobPtr blob = sizeHints.immutableBlob();

        // SAFETY: Validate blob pointer, length, and alignment
        // Struct must be: { uint16_t width; uint16_t height; }
        constexpr size_t hintSize = sizeof(uint16_t) * 2;

        if (blob->data && blob->length >= hintSize && (blob->length % hintSize) == 0) [[likely]] {
            const size_t hintCount = blob->length / hintSize;

            // Avoid truncation on 64-bit systems: validate count fits in int
            if (hintCount <= static_cast<size_t>(std::numeric_limits<int>::max())) [[likely]] {
                m_sizeHints.reserve(static_cast<int>(hintCount));

                // Manual parsing with alignment-safe access
                // Cannot use std::span in C++20 mode without proper checks
                const uint16_t *data = reinterpret_cast<const uint16_t *>(blob->data);
                for (size_t i = 0; i < hintCount; i++) {
                    const uint16_t width = data[i * 2];
                    const uint16_t height = data[i * 2 + 1];
                    m_sizeHints.append(QSize(width, height));
                }
            } else [[unlikely]] {
                qCWarning(KWIN_DRM) << "SIZE_HINTS blob too large for plane" << id()
                                    << "- ignoring";
            }
        } else if (blob->data && blob->length > 0) [[unlikely]] {
            qCWarning(KWIN_DRM) << "SIZE_HINTS blob has invalid length" << blob->length
                                << "for plane" << id();
        }
    }

    // Cursor planes always have at least the default cursor size hint
    if (m_sizeHints.isEmpty() && type.enumValue() == TypeIndex::Cursor) [[unlikely]] {
        m_sizeHints.append(gpu()->cursorSize());
    }

    // Parse tearing-capable formats (VRR async page-flip support)
    if (inFormatsForTearing.isValid() && inFormatsForTearing.immutableBlob() && gpu()->addFB2ModifiersSupported()) {
        drmModePropertyBlobPtr blob = inFormatsForTearing.immutableBlob();

        m_supportedTearingFormats.clear();
        m_supportedTearingFormats.reserve(32);

        if (blob->data && blob->length > 0) [[likely]] {
            drmModeFormatModifierIterator iterator{};
            while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
                m_supportedTearingFormats[iterator.fmt].push_back(iterator.mod);
            }
        }
    } else {
        // If no explicit tearing formats, assume all regular formats support tearing
        m_supportedTearingFormats = m_supportedFormats;
    }

    return true;
}

void DrmPlane::set(DrmAtomicCommit *commit, const QRect &src, const QRect &dst)
{
    // SRC_* properties use 16.16 fixed-point format per DRM KMS API spec
    // This allows sub-pixel precision for scaling operations
    //
    // CRITICAL: Must handle negative coordinates (off-screen positioning)
    // and prevent overflow when shifting by 16 bits

    // Clamp source rectangle to valid range for 16.16 fixed-point
    // Max safe value: (2^31 - 1) >> 16 = 32767 (fits in int16_t range)
    constexpr int maxCoord = 32767;
    constexpr int minCoord = -32768;

    const int clampedSrcX = std::clamp(src.x(), minCoord, maxCoord);
    const int clampedSrcY = std::clamp(src.y(), minCoord, maxCoord);
    const int clampedSrcW = std::clamp(src.width(), 0, maxCoord);
    const int clampedSrcH = std::clamp(src.height(), 0, maxCoord);

    // Cast to unsigned BEFORE shifting to prevent signed overflow (UB)
    // For negative values, two's complement representation is preserved
    commit->addProperty(srcX, static_cast<uint64_t>(static_cast<uint32_t>(clampedSrcX)) << 16);
    commit->addProperty(srcY, static_cast<uint64_t>(static_cast<uint32_t>(clampedSrcY)) << 16);
    commit->addProperty(srcW, static_cast<uint64_t>(static_cast<uint32_t>(clampedSrcW)) << 16);
    commit->addProperty(srcH, static_cast<uint64_t>(static_cast<uint32_t>(clampedSrcH)) << 16);

    // CRTC_* properties use integer pixel coordinates (no fixed-point)
    // These can be negative for off-screen positioning
    commit->addProperty(crtcX, dst.x());
    commit->addProperty(crtcY, dst.y());
    commit->addProperty(crtcW, dst.width());
    commit->addProperty(crtcH, dst.height());
}

bool DrmPlane::isCrtcSupported(int pipeIndex) const
{
    return (m_possibleCrtcs & (1 << pipeIndex));
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::implicitModifierOnlyFormats() const
{
    return m_implicitModifierOnlyFormats;
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

    const bool found = (m_lastBuffers[0] == newData) |
                       (m_lastBuffers[1] == newData) |
                       (m_lastBuffers[2] == newData) |
                       (m_lastBuffers[3] == newData);

    if (!found) {
        m_lastBuffers[m_lastBufferWriteIndex] = newData;
        m_lastBufferWriteIndex = (m_lastBufferWriteIndex + 1) & 3;
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
    // DRM plane transformations are specified counter-clockwise
    // OutputTransform rotations are specified clockwise
    // Therefore, we must invert the rotation direction in this mapping
    //
    // Example: OutputTransform::Rotate90 (90° clockwise) maps to
    //          DRM Transformation::Rotate270 (270° counter-clockwise = 90° clockwise)

    switch (transform.kind()) {
    case OutputTransform::Kind::Normal:
        return Transformation::Rotate0;
    case OutputTransform::Kind::Rotate90:
        return Transformation::Rotate270;  // Inverted
    case OutputTransform::Kind::Rotate180:
        return Transformation::Rotate180;  // Same in both directions
    case OutputTransform::Kind::Rotate270:
        return Transformation::Rotate90;   // Inverted
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

}

#include "moc_drm_plane.cpp"
