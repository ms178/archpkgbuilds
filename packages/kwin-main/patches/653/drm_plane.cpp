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

namespace
{

[[gnu::always_inline, gnu::const]]
inline constexpr uint64_t toFixed16(int value) noexcept
{
    return static_cast<uint64_t>(static_cast<uint32_t>(value)) << 16;
}

[[gnu::always_inline, gnu::const]]
inline constexpr int clampNonNegative(int v, int hi) noexcept
{
    const int clamped = (v < 0) ? 0 : v;
    return (clamped > hi) ? hi : clamped;
}

[[gnu::always_inline, gnu::const]]
inline constexpr int clampPositive(int v, int hi) noexcept
{
    const int clamped = (v < 1) ? 1 : v;
    return (clamped > hi) ? hi : clamped;
}

}

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

    const bool basicPropsValid = type.isValid() && srcX.isValid() && srcY.isValid() &&
                                  srcW.isValid() && srcH.isValid() && crtcX.isValid() &&
                                  crtcY.isValid() && crtcW.isValid() && crtcH.isValid() &&
                                  fbId.isValid();
    if (!basicPropsValid) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Failed to update the basic plane properties for plane" << id();
        return false;
    }

    m_possibleCrtcs = plane->possible_crtcs;

    m_supportedFormats.clear();
    m_lowBandwidthFormats.clear();
    m_supportedTearingFormats.clear();
    m_sizeHints.clear();

    if (inFormats.isValid() && gpu()->addFB2ModifiersSupported()) [[likely]] {
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
        if (formatInfo && formatInfo->bitsPerPixel <= 32) [[likely]] {
            QList<uint64_t> modifiers = it.value();
            bool hasInvalid = false;
            for (const uint64_t mod : modifiers) {
                if (mod == DRM_FORMAT_MOD_INVALID) {
                    hasInvalid = true;
                    break;
                }
            }
            if (!hasInvalid) {
                modifiers.append(DRM_FORMAT_MOD_INVALID);
            }
            std::sort(modifiers.begin(), modifiers.end());
            const auto newEnd = std::unique(modifiers.begin(), modifiers.end());
            modifiers.erase(newEnd, modifiers.end());
            m_lowBandwidthFormats.insert(it.key(), std::move(modifiers));
        }
    }

    if (sizeHints.isValid()) [[unlikely]] {
        refreshSizeHints(sizeHints.immutableBlob());
    }

    if (m_sizeHints.isEmpty() && type.enumValue() == TypeIndex::Cursor) [[likely]] {
        m_sizeHints.append(gpu()->cursorSize());
    }

    if (inFormatsForTearing.isValid() && gpu()->addFB2ModifiersSupported()) [[unlikely]] {
        refreshSupportedTearingFormats(inFormatsForTearing.immutableBlob());
    }

    if (m_supportedTearingFormats.isEmpty()) [[likely]] {
        m_supportedTearingFormats = m_supportedFormats;
    }

    return true;
}

void DrmPlane::refreshSupportedFormats(drmModePropertyBlobPtr blob)
{
    if (!blob || !blob->data || blob->length == 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "IN_FORMATS blob missing for plane" << id();
        return;
    }

    m_supportedFormats.reserve(256);
    drmModeFormatModifierIterator iterator{};
    while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
        auto &mods = m_supportedFormats[iterator.fmt];
        bool found = false;
        for (const uint64_t m : mods) {
            if (m == iterator.mod) {
                found = true;
                break;
            }
        }
        if (!found) [[likely]] {
            mods.append(iterator.mod);
        }
    }
}

void DrmPlane::refreshSupportedTearingFormats(drmModePropertyBlobPtr blob)
{
    if (!blob || !blob->data || blob->length == 0) [[unlikely]] {
        return;
    }

    m_supportedTearingFormats.reserve(256);
    drmModeFormatModifierIterator iterator{};
    while (drmModeFormatModifierBlobIterNext(blob, &iterator)) {
        auto &mods = m_supportedTearingFormats[iterator.fmt];
        bool found = false;
        for (const uint64_t m : mods) {
            if (m == iterator.mod) {
                found = true;
                break;
            }
        }
        if (!found) [[likely]] {
            mods.append(iterator.mod);
        }
    }
}

void DrmPlane::refreshSizeHints(drmModePropertyBlobPtr blob)
{
    const bool invalidBlob = !blob || !blob->data || blob->length < sizeof(uint16_t) * 2 ||
                             (blob->length % (sizeof(uint16_t) * 2)) != 0;
    if (invalidBlob) [[unlikely]] {
        if (blob && blob->data && blob->length > 0) {
            qCWarning(KWIN_DRM) << "SIZE_HINTS blob has invalid length" << blob->length << "for plane" << id();
        }
        return;
    }

    const size_t hintCount = blob->length / (sizeof(uint16_t) * 2);
    if (hintCount > static_cast<size_t>(std::numeric_limits<int>::max())) [[unlikely]] {
        return;
    }

    auto hints = std::span(reinterpret_cast<const uint16_t *>(blob->data), hintCount * 2);
    m_sizeHints.reserve(static_cast<int>(hintCount));
    for (size_t i = 0; i < hintCount; ++i) {
        const QSize size(hints[i * 2], hints[i * 2 + 1]);
        bool found = false;
        for (const QSize &existing : m_sizeHints) {
            if (existing == size) {
                found = true;
                break;
            }
        }
        if (!found) [[likely]] {
            m_sizeHints.append(size);
        }
    }
}

void DrmPlane::set(DrmAtomicCommit *commit, const QRect &src, const QRect &dst)
{
    const bool needsDisable = (dst.width() <= 0) || (dst.height() <= 0);
    if (needsDisable) [[unlikely]] {
        disable(commit);
        return;
    }

    constexpr int kMaxSrcCoord = 32767;

    int clampedSrcX, clampedSrcY, clampedSrcW, clampedSrcH;

#if defined(__SSE4_1__)
    alignas(16) const int32_t srcValues[4] = {src.x(), src.y(), src.width(), src.height()};
    const __m128i vals = _mm_load_si128(reinterpret_cast<const __m128i *>(srcValues));
    const __m128i zeros = _mm_setzero_si128();
    const __m128i maxVals = _mm_set1_epi32(kMaxSrcCoord);
    __m128i clamped = _mm_max_epi32(vals, zeros);
    clamped = _mm_min_epi32(clamped, maxVals);
    alignas(16) int32_t clampedValues[4];
    _mm_store_si128(reinterpret_cast<__m128i *>(clampedValues), clamped);
    clampedSrcX = clampedValues[0];
    clampedSrcY = clampedValues[1];
    clampedSrcW = (clampedValues[2] < 1) ? 1 : clampedValues[2];
    clampedSrcH = (clampedValues[3] < 1) ? 1 : clampedValues[3];
#else
    clampedSrcX = clampNonNegative(src.x(), kMaxSrcCoord);
    clampedSrcY = clampNonNegative(src.y(), kMaxSrcCoord);
    clampedSrcW = clampPositive(src.width(), kMaxSrcCoord);
    clampedSrcH = clampPositive(src.height(), kMaxSrcCoord);
#endif

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
    if (pipeIndex < 0 || pipeIndex >= 32) [[unlikely]] {
        return false;
    }
    return (m_possibleCrtcs & (1u << static_cast<unsigned>(pipeIndex))) != 0u;
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
    if (m_current.get() == buffer.get()) [[likely]] {
        return;
    }

    m_current = buffer;
    if (!buffer) [[unlikely]] {
        return;
    }

    const auto data = buffer->data();
    if (!data) [[unlikely]] {
        return;
    }

    DrmFramebufferData *rawPtr = data.get();
    bool found = false;
    for (std::size_t i = 0; i < BufferHistoryDepth; ++i) {
        if (m_lastBuffers[i] == rawPtr) {
            found = true;
            break;
        }
    }

    if (!found) [[likely]] {
        m_lastBuffers[m_lastBufferWriteIndex] = rawPtr;
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
    if (m_current) [[likely]] {
        m_current->releaseBuffer();
    }
}

DrmPlane::Transformations DrmPlane::outputTransformToPlaneTransform(OutputTransform transform) noexcept
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
        return Transformations{Transformation::Rotate0} | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY90:
        return Transformations{Transformation::Rotate270} | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY180:
        return Transformations{Transformation::Rotate180} | Transformation::ReflectY;
    case OutputTransform::Kind::FlipY270:
        return Transformations{Transformation::Rotate90} | Transformation::ReflectY;
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

}

#include "moc_drm_plane.cpp"
