/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2016 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include <QPoint>
#include <QSize>

#include "core/output.h"
#include "drm_blob.h"
#include "drm_object.h"
#include "drm_pointer.h"
#include "utils/edid.h"

namespace KWin
{

class DrmConnector;
class DrmCrtc;

/**
 * The DrmConnectorMode class represents a native mode and the associated blob.
 */
class DrmConnectorMode : public OutputMode
{
public:
    DrmConnectorMode(DrmConnector *connector, const drmModeModeInfo &nativeMode, Flags additionalFlags);

    [[nodiscard]] drmModeModeInfo *nativeMode();
    [[nodiscard]] const drmModeModeInfo *nativeMode() const;

    std::shared_ptr<DrmBlob> blob();
    [[nodiscard]] std::chrono::nanoseconds vblankTime() const;

    bool operator==(const DrmConnectorMode &otherMode) const;
    bool operator==(const drmModeModeInfo &otherMode) const;

    // Helper for mode comparison exposed for efficient inlining
    static bool isEqual(const drmModeModeInfo &one, const drmModeModeInfo &two);

private:
    DrmConnector *const m_connector;
    drmModeModeInfo m_nativeMode;
    std::shared_ptr<DrmBlob> m_blob;
};

class DrmConnector : public DrmObject
{
public:
    enum class UnderscanOptions : uint64_t {
        Off = 0,
        On = 1,
        Auto = 2,
    };
    enum class BroadcastRgbOptions : uint64_t {
        Automatic = 0,
        Full = 1,
        Limited = 2
    };
    enum class LinkStatus : uint64_t {
        Good = 0,
        Bad = 1
    };
    enum class DrmContentType : uint64_t {
        None = 0,
        Graphics = 1,
        Photo = 2,
        Cinema = 3,
        Game = 4
    };
    enum class PanelOrientation : uint64_t {
        Normal = 0,
        UpsideDown = 1,
        LeftUp = 2,
        RightUp = 3
    };
    enum class ScalingMode : uint64_t {
        None = 0,
        Full = 1,
        Center = 2,
        Full_Aspect = 3
    };
    enum class Colorspace : uint64_t {
        Default,
        BT709_YCC,
        opRGB,
        BT2020_RGB,
        BT2020_YCC,
    };

    DrmConnector(DrmGpu *gpu, uint32_t connectorId);

    bool init();

    bool updateProperties() override;
    void disable(DrmAtomicCommit *commit) override;

    [[nodiscard]] bool isCrtcSupported(DrmCrtc *crtc) const;
    [[nodiscard]] bool isConnected() const;
    [[nodiscard]] bool isNonDesktop() const;
    [[nodiscard]] bool isInternal() const;

    [[nodiscard]] const Edid *edid() const;
    [[nodiscard]] QString connectorName() const;
    [[nodiscard]] QString modelName() const;
    [[nodiscard]] QSize physicalSize() const;
    /**
     * @returns the mst path of the connector. Is empty if invalid
     */
    [[nodiscard]] QByteArray mstPath() const;

    [[nodiscard]] QList<std::shared_ptr<DrmConnectorMode>> modes() const;
    [[nodiscard]] std::shared_ptr<DrmConnectorMode> findMode(const drmModeModeInfo &modeInfo) const;

    [[nodiscard]] Output::SubPixel subpixel() const;

    // Properties exposed publicly for atomic commits
    DrmProperty crtcId;
    DrmProperty nonDesktop;
    DrmProperty dpms;
    DrmProperty edidProp;
    DrmProperty overscan;
    DrmProperty vrrCapable;
    DrmEnumProperty<UnderscanOptions> underscan;
    DrmProperty underscanVBorder;
    DrmProperty underscanHBorder;
    DrmEnumProperty<BroadcastRgbOptions> broadcastRGB;
    DrmProperty maxBpc;
    DrmEnumProperty<LinkStatus> linkStatus;
    DrmEnumProperty<DrmContentType> contentType;
    DrmEnumProperty<PanelOrientation> panelOrientation;
    DrmProperty hdrMetadata;
    DrmEnumProperty<ScalingMode> scalingMode;
    DrmEnumProperty<Colorspace> colorspace;
    DrmProperty path;

    static DrmContentType kwinToDrmContentType(ContentType type);
    static OutputTransform toKWinTransform(PanelOrientation orientation);
    static BroadcastRgbOptions rgbRangeToBroadcastRgb(Output::RgbRange rgbRange);
    static Output::RgbRange broadcastRgbToRgbRange(BroadcastRgbOptions rgbRange);

private:
    QList<std::shared_ptr<DrmConnectorMode>> generateCommonModes();
    std::shared_ptr<DrmConnectorMode> generateMode(const QSize &size, float refreshRate);

    // Reordered for better cache locality (hot members first)
    DrmUniquePtr<drmModeConnector> m_conn;
    QList<std::shared_ptr<DrmConnectorMode>> m_modes;
    QList<std::shared_ptr<DrmConnectorMode>> m_driverModes;
    Edid m_edid;
    QSize m_physicalSize = QSize(-1, -1);
    QByteArray m_mstPath;
    uint32_t m_possibleCrtcs = 0;

    friend QDebug &operator<<(QDebug &s, const KWin::DrmConnector *obj);
};

QDebug &operator<<(QDebug &s, const KWin::DrmConnector *obj);

}
