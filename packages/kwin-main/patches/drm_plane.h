/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2016 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "core/output.h"
#include "drm_object.h"

#include <QHash>
#include <QList>
#include <QPoint>
#include <QSize>

#include <array>
#include <cstdint>
#include <memory>

namespace KWin
{

class DrmFramebuffer;
class DrmFramebufferData;
class DrmCrtc;
class DrmAtomicCommit;

class DrmPlane : public DrmObject
{
    Q_GADGET
public:
    static constexpr std::size_t BufferHistoryDepth = 4;

    DrmPlane(DrmGpu *gpu, uint32_t planeId);

    bool init();

    bool updateProperties() override;
    void disable(DrmAtomicCommit *commit) override;

    [[nodiscard]] bool isCrtcSupported(int pipeIndex) const noexcept;
    [[nodiscard]] const QHash<uint32_t, QList<uint64_t>> &lowBandwidthFormats() const noexcept;
    [[nodiscard]] const QHash<uint32_t, QList<uint64_t>> &formats() const noexcept;
    [[nodiscard]] const QHash<uint32_t, QList<uint64_t>> &tearingFormats() const noexcept;
    [[nodiscard]] bool supportsTransformation(OutputTransform transform) const noexcept;

    [[nodiscard]] std::shared_ptr<DrmFramebuffer> currentBuffer() const noexcept;
    void setCurrentBuffer(const std::shared_ptr<DrmFramebuffer> &buffer);
    void releaseCurrentBuffer();

    void set(DrmAtomicCommit *commit, const QRect &src, const QRect &dst);

    [[nodiscard]] const QList<QSize> &recommendedSizes() const noexcept;

    enum class TypeIndex : uint64_t {
        Overlay = 0,
        Primary = 1,
        Cursor = 2
    };

    enum class Transformation : uint32_t {
        Rotate0 = 1 << 0,
        Rotate90 = 1 << 1,
        Rotate180 = 1 << 2,
        Rotate270 = 1 << 3,
        ReflectX = 1 << 4,
        ReflectY = 1 << 5
    };
    Q_ENUM(Transformation)
    Q_DECLARE_FLAGS(Transformations, Transformation)

    static Transformations outputTransformToPlaneTransform(OutputTransform transform);

    enum class PixelBlendMode : uint64_t {
        None,
        PreMultiplied,
        Coverage
    };
    enum class ColorEncoding : uint64_t {
        BT601_YCbCr,
        BT709_YCbCr,
        BT2020_YCbCr
    };
    enum class ColorRange : uint64_t {
        Limited_YCbCr,
        Full_YCbCr
    };

    DrmEnumProperty<TypeIndex> type;
    DrmProperty srcX;
    DrmProperty srcY;
    DrmProperty srcW;
    DrmProperty srcH;
    DrmProperty crtcX;
    DrmProperty crtcY;
    DrmProperty crtcW;
    DrmProperty crtcH;
    DrmProperty fbId;
    DrmProperty crtcId;
    DrmEnumProperty<Transformations> rotation;
    DrmProperty inFormats;
    DrmProperty alpha;
    DrmEnumProperty<PixelBlendMode> pixelBlendMode;
    DrmEnumProperty<ColorEncoding> colorEncoding;
    DrmEnumProperty<ColorRange> colorRange;
    DrmProperty vmHotspotX;
    DrmProperty vmHotspotY;
    DrmProperty inFenceFd;
    DrmProperty sizeHints;
    DrmProperty inFormatsForTearing;
    DrmProperty zpos;

private:
    void refreshSupportedFormats(drmModePropertyBlobPtr blob);
    void refreshSupportedTearingFormats(drmModePropertyBlobPtr blob);
    void refreshSizeHints(drmModePropertyBlobPtr blob);

    std::shared_ptr<DrmFramebuffer> m_current;
    std::array<std::shared_ptr<DrmFramebufferData>, BufferHistoryDepth> m_lastBuffers{};
    std::size_t m_lastBufferWriteIndex = 0;

    QHash<uint32_t, QList<uint64_t>> m_supportedFormats;
    QHash<uint32_t, QList<uint64_t>> m_lowBandwidthFormats;
    QHash<uint32_t, QList<uint64_t>> m_supportedTearingFormats;
    uint32_t m_possibleCrtcs = 0;
    QList<QSize> m_sizeHints;
};

} // namespace KWin

Q_DECLARE_OPERATORS_FOR_FLAGS(KWin::DrmPlane::Transformations)
