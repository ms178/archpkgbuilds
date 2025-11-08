/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

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

#include <algorithm>
#include <cstdint>
#include <limits>
#include <span>

#if defined(__SSE4_1__)
#include <smmintrin.h>
#endif

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
    DrmUniquePtr<drmModePlane> plane(drmModeGetPlane(gpu()->fd(), id()));
    if (!plane) [[unlikely]] {
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

    m_possibleCrtcs = plane->possible_crtcs;

    m_supportedFormats.clear();
    m_lowBandwidthFormats.clear();
    m_supportedTearingFormats.clear();
    m_sizeHints.clear();

    if (inFormats.isValid() && gpu()->addFB2ModifiersSupported()) {
        refreshSupportedFormats(inFormats.immutableBlob());
    } else {
        const uint32_t formatCount = plane->count_formats;
        m_supportedFormats.reserve(static_cast<int>(formatCount));
        const bool isCursor = (type.enumValue() == TypeIndex::Cursor);
        const uint64_t modifier = isCursor ? DRM_FORMAT_MOD_LINEAR : DRM_FORMAT_MOD_INVALID;

        for (uint32_t i = 0; i < formatCount; ++i) {
            m_supportedFormats.insert(plane->formats[i], QList<uint64_t>{modifier});
        }
    }

    if (m_supportedFormats.isEmpty()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Driver advertises no formats for plane" << id() << "- falling back to XRGB8888";
        const bool isCursor = (type.enumValue() == TypeIndex::Cursor);
        const uint64_t modifier = isCursor ? DRM_FORMAT_MOD_LINEAR : DRM_FORMAT_MOD_INVALID;
        m_supportedFormats.insert(DRM_FORMAT_XRGB8888, {modifier});
    }

    m_lowBandwidthFormats.reserve(m_supportedFormats.size());
    for (auto it = m_supportedFormats.cbegin(); it != m_supportedFormats.cend(); ++it) {
        const auto formatInfo = FormatInfo::get(it.key());
        if (formatInfo && formatInfo->bitsPerPixel <= 32) {
            QList<uint64_t> modifiers = it.value();
            if (!modifiers.contains(DRM_FORMAT_MOD_INVALID)) {
                modifiers.append(DRM_FORMAT_MOD_INVALID);
            }
            std::sort(modifiers.begin(), modifiers.end());
            const auto newEnd = std::unique(modifiers.begin(), modifiers.end());
            modifiers.erase(newEnd, modifiers.end());
            m_lowBandwidthFormats.insert(it.key(), modifiers);
        }
    }

    if (sizeHints.isValid()) {
        refreshSizeHints(sizeHints.immutableBlob());
    }

    if (m_sizeHints.isEmpty() && type.enumValue() == TypeIndex::Cursor) {
        m_sizeHints.append(gpu()->cursorSize());
    }

    if (inFormatsForTearing.isValid() && gpu()->addFB2ModifiersSupported()) {
        refreshSupportedTearingFormats(inFormatsForTearing.immutableBlob());
    }

    if (m_supportedTearingFormats.isEmpty()) {
        m_supportedTearingFormats = m_supportedFormats;
    }

    return true;
}

void DrmPlane::refreshSupportedFormats(drmModePropertyBlobPtr blob)
{
    if (!blob || !blob->data || blob->length == 0) {
        qCWarning(KWIN_DRM) << "IN_FORMATS blob missing for plane" << id();
        return;
    }

    m_supportedFormats.reserve(256);
    drmModeFormatModifierIterator iterator{};
    while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
        auto &mods = m_supportedFormats[iterator.fmt];
        if (!mods.contains(iterator.mod)) {
            mods.append(iterator.mod);
        }
    }
}

void DrmPlane::refreshSupportedTearingFormats(drmModePropertyBlobPtr blob)
{
    if (!blob || !blob->data || blob->length == 0) {
        return;
    }

    m_supportedTearingFormats.reserve(256);
    drmModeFormatModifierIterator iterator{};
    while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
        auto &mods = m_supportedTearingFormats[iterator.fmt];
        if (!mods.contains(iterator.mod)) {
            mods.append(iterator.mod);
        }
    }
}

void DrmPlane::refreshSizeHints(drmModePropertyBlobPtr blob)
{
    if (!blob || !blob->data || blob->length < sizeof(uint16_t) * 2 || (blob->length % (sizeof(uint16_t) * 2)) != 0) {
        if (blob && blob->data && blob->length > 0) {
            qCWarning(KWIN_DRM) << "SIZE_HINTS blob has invalid length" << blob->length << "for plane" << id();
        }
        return;
    }

    const size_t hintCount = blob->length / (sizeof(uint16_t) * 2);
    if (hintCount > static_cast<size_t>(std::numeric_limits<int>::max())) {
        return;
    }

    auto hints = std::span(reinterpret_cast<const uint16_t *>(blob->data), hintCount * 2);
    for (size_t i = 0; i < hintCount; ++i) {
        const QSize size(hints[i * 2], hints[i * 2 + 1]);
        if (!m_sizeHints.contains(size)) {
            m_sizeHints.append(size);
        }
    }
}

void DrmPlane::set(DrmAtomicCommit *commit, const QRect &src, const QRect &dst)
{
    const bool needsDisable = (dst.width() <= 0) || (dst.height() <= 0);
    if (needsDisable) {
        disable(commit);
        return;
    }

    int clampedSrcX = src.x();
    int clampedSrcY = src.y();
    int clampedSrcW = src.width();
    int clampedSrcH = src.height();

#if defined(__SSE4_1__)
    alignas(16) const int32_t srcValues[4] = {src.x(), src.y(), src.width(), src.height()};
    const __m128i vals = _mm_load_si128(reinterpret_cast<const __m128i *>(srcValues));
    const __m128i minVals = _mm_setr_epi32(-32768, -32768, 0, 0);
    const __m128i maxVals = _mm_set1_epi32(32767);
    const __m128i clamped = _mm_max_epi32(_mm_min_epi32(vals, maxVals), minVals);

    alignas(16) int32_t clampedValues[4];
    _mm_store_si128(reinterpret_cast<__m128i *>(clampedValues), clamped);

    clampedSrcX = clampedValues[0];
    clampedSrcY = clampedValues[1];
    clampedSrcW = clampedValues[2];
    clampedSrcH = clampedValues[3];
#else
    constexpr int maxCoord = 32767;
    constexpr int minCoord = -32768;
    clampedSrcX = std::clamp(clampedSrcX, minCoord, maxCoord);
    clampedSrcY = std::clamp(clampedSrcY, minCoord, maxCoord);
    clampedSrcW = std::clamp(clampedSrcW, 0, maxCoord);
    clampedSrcH = std::clamp(clampedSrcH, 0, maxCoord);
#endif

    auto toFixed16 = [](int value) -> uint64_t {
        return static_cast<uint64_t>(static_cast<uint32_t>(value) << 16);
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

bool DrmPlane::isCrtcSupported(int pipeIndex) const noexcept
{
    if (pipeIndex < 0 || pipeIndex >= static_cast<int>(sizeof(uint32_t) * 8)) {
        return false;
    }
    return (m_possibleCrtcs & (1u << pipeIndex)) != 0u;
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::lowBandwidthFormats() const noexcept
{
    return m_lowBandwidthFormats;
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::formats() const noexcept
{
    return m_supportedFormats;
}

const QHash<uint32_t, QList<uint64_t>> &DrmPlane::tearingFormats() const noexcept
{
    return m_supportedTearingFormats;
}

std::shared_ptr<DrmFramebuffer> DrmPlane::currentBuffer() const noexcept
{
    return m_current;
}

void DrmPlane::setCurrentBuffer(const std::shared_ptr<DrmFramebuffer> &buffer)
{
    if (m_current == buffer) {
        return;
    }

    m_current = buffer;
    if (!buffer) {
        return;
    }

    const auto data = buffer->data();
    if (!data) {
        return;
    }

    bool found = false;
    for (const auto &cached : m_lastBuffers) {
        if (cached == data) {
            found = true;
            break;
        }
    }

    if (!found) {
        m_lastBuffers[m_lastBufferWriteIndex] = data;
        m_lastBufferWriteIndex = (m_lastBufferWriteIndex + 1) % BufferHistoryDepth;
    }
}

void DrmPlane::disable(DrmAtomicCommit *commit)
{
    commit->addProperty(crtcId, 0);
    commit->addBuffer(this, nullptr, nullptr);
    m_current.reset();
}

void DrmPlane::releaseCurrentBuffer()
{
    if (m_current) {
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

bool DrmPlane::supportsTransformation(OutputTransform transform) const noexcept
{
    return rotation.isValid() && rotation.hasEnum(outputTransformToPlaneTransform(transform));
}

const QList<QSize> &DrmPlane::recommendedSizes() const noexcept
{
    return m_sizeHints;
}

} // namespace KWin

#include "moc_drm_plane.cpp"
