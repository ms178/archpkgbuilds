/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2019 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include <kwin_export.h>

#include "renderloop.h"
#include "utils/edid.h"

#include <QDebug>
#include <QList>
#include <QMatrix4x4>
#include <QObject>
#include <QRect>
#include <QSize>
#include <QUuid>

#include <chrono>
#include <optional>

namespace KWin
{

class RenderLoop;
class OutputConfiguration;
class ColorTransformation;
class IccProfile;
class OutputChangeSet;
class BrightnessDevice;
class OutputFrame;
class OutputLayer;

/**
 * The OutputTransform type is used to describe the transform applied to the output content.
 */
class KWIN_EXPORT OutputTransform
{
public:
    enum Kind {
        Normal = 0, // no rotation
        Rotate90 = 1, // rotate 90 degrees counterclockwise
        Rotate180 = 2, // rotate 180 degrees counterclockwise
        Rotate270 = 3, // rotate 270 degrees counterclockwise
        FlipX = 4, // mirror horizontally
        FlipX90 = 5, // mirror horizontally, then rotate 90 degrees counterclockwise
        FlipX180 = 6, // mirror horizontally, then rotate 180 degrees counterclockwise
        FlipX270 = 7, // mirror horizontally, then rotate 270 degrees counterclockwise
        FlipY = FlipX180, // mirror vertically
        FlipY90 = FlipX270, // mirror vertically, then rotate 90 degrees counterclockwise
        FlipY180 = FlipX, // mirror vertically, then rotate 180 degrees counterclockwise
        FlipY270 = FlipX90, // mirror vertically, then rotate 270 degrees counterclockwise
    };

    constexpr OutputTransform() = default;
    constexpr OutputTransform(Kind kind)
        : m_kind(kind)
    {
    }

    constexpr bool operator<=>(const OutputTransform &other) const = default;

    /**
     * Returns the transform kind.
     */
    [[nodiscard]] constexpr Kind kind() const { return m_kind; }

    /**
     * Returns the inverse transform. The inverse transform can be used for mapping between
     * surface and buffer coordinate systems.
     */
    [[nodiscard]] OutputTransform inverted() const;

    /**
     * Applies the output transform to the given @a size.
     */
    [[nodiscard]] QSizeF map(const QSizeF &size) const;
    [[nodiscard]] QSize map(const QSize &size) const;

    /**
     * Applies the output transform to the given @a rect within a buffer with dimensions @a bounds.
     */
    [[nodiscard]] QRectF map(const QRectF &rect, const QSizeF &bounds) const;
    [[nodiscard]] QRect map(const QRect &rect, const QSize &bounds) const;

    /**
     * Applies the output transform to the given @a point.
     */
    [[nodiscard]] QPointF map(const QPointF &point, const QSizeF &bounds) const;
    [[nodiscard]] QPoint map(const QPoint &point, const QSize &bounds) const;

    /**
     * Returns an output transform that is equivalent to applying this transform and @a other
     * transform sequentially.
     */
    [[nodiscard]] OutputTransform combine(OutputTransform other) const;

    /**
     * Returns the matrix corresponding to this output transform.
     */
    [[nodiscard]] QMatrix4x4 toMatrix() const;

private:
    Kind m_kind = Kind::Normal;
};

class KWIN_EXPORT OutputMode
{
public:
    enum class Flag : uint {
        Preferred = 0x1,
        Generated = 0x2,
        Removed = 0x4,
    };
    Q_DECLARE_FLAGS(Flags, Flag)

    OutputMode(const QSize &size, uint32_t refreshRate, Flags flags = {});
    virtual ~OutputMode() = default;

    [[nodiscard]] QSize size() const;
    [[nodiscard]] uint32_t refreshRate() const;
    [[nodiscard]] Flags flags() const;

    void setRemoved();

private:
    const QSize m_size;
    const uint32_t m_refreshRate;
    Flags m_flags;
};

/**
 * Generic output representation.
 */
class KWIN_EXPORT Output : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QRect geometry READ geometry NOTIFY geometryChanged)
    Q_PROPERTY(qreal devicePixelRatio READ scale NOTIFY scaleChanged)
    Q_PROPERTY(QString name READ name CONSTANT)
    Q_PROPERTY(QString manufacturer READ manufacturer CONSTANT)
    Q_PROPERTY(QString model READ model CONSTANT)
    Q_PROPERTY(QString serialNumber READ serialNumber CONSTANT)

public:
    enum class DpmsMode {
        On,
        Standby,
        Suspend,
        Off,
        AboutToTurnOff,
    };
    Q_ENUM(DpmsMode)

    enum class Capability : uint {
        Dpms = 1,
        Overscan = 1 << 1,
        Vrr = 1 << 2,
        RgbRange = 1 << 3,
        HighDynamicRange = 1 << 4,
        WideColorGamut = 1 << 5,
        AutoRotation = 1 << 6,
        IccProfile = 1 << 7,
        Tearing = 1 << 8,
        BrightnessControl = 1 << 9,
        BuiltInColorProfile = 1 << 10,
        DdcCi = 1 << 11,
        MaxBitsPerColor = 1 << 12,
        Edr = 1 << 13,
    };
    Q_DECLARE_FLAGS(Capabilities, Capability)

    enum class SubPixel {
        Unknown,
        None,
        Horizontal_RGB,
        Horizontal_BGR,
        Vertical_RGB,
        Vertical_BGR,
    };
    Q_ENUM(SubPixel)

    enum class RgbRange {
        Automatic = 0,
        Full = 1,
        Limited = 2,
    };
    Q_ENUM(RgbRange)

    enum class AutoRotationPolicy {
        Never = 0,
        InTabletMode,
        Always
    };
    Q_ENUM(AutoRotationPolicy);
    enum class ColorProfileSource {
        sRGB = 0,
        ICC,
        EDID,
    };
    enum class ColorPowerTradeoff {
        PreferEfficiency = 0,
        PreferAccuracy,
    };
    Q_ENUM(ColorPowerTradeoff);

    enum class EdrPolicy {
        Never = 0,
        Always,
    };
    Q_ENUM(EdrPolicy);

    explicit Output(QObject *parent = nullptr);
    ~Output() override;

    void ref();
    void unref();

    /**
     * Maps the specified @a rect from the global coordinate system to the output-local coords.
     */
    [[nodiscard]] QRect mapFromGlobal(const QRect &rect) const;

    /**
     * Maps the specified @a rect from the global coordinate system to the output-local coords.
     */
    [[nodiscard]] QRectF mapFromGlobal(const QRectF &rect) const;

    /**
     * Maps a @a rect in this output coordinates to the global coordinate system.
     */
    [[nodiscard]] QRectF mapToGlobal(const QRectF &rect) const;

    /**
     * Maps a @a region in this output coordinates to the global coordinate system.
     */
    [[nodiscard]] QRegion mapToGlobal(const QRegion &region) const;

    Q_INVOKABLE [[nodiscard]] QPointF mapToGlobal(const QPointF &pos) const;
    Q_INVOKABLE [[nodiscard]] QPointF mapFromGlobal(const QPointF &pos) const;

    /**
     * Returns a short identifiable name of this output.
     */
    [[nodiscard]] QString name() const;

    /**
     * Returns the identifying uuid of this output.
     * NOTE that this is set by the output configuration store, and
     * can potentially change on hotplug events, because displays are terrible
     */
    [[nodiscard]] QString uuid() const;

    /**
     * Returns @c true if the output is enabled; otherwise returns @c false.
     */
    [[nodiscard]] bool isEnabled() const;

    /**
     * Returns geometry of this output in device independent pixels.
     * Optimized: Returns a cached value to avoid repeated calculations in hot paths.
     */
    [[nodiscard]] QRect geometry() const;

    /**
     * Returns geometry of this output in device independent pixels, without rounding.
     * Optimized: Returns a cached value to avoid repeated calculations in hot paths.
     */
    [[nodiscard]] QRectF geometryF() const;

    /**
     * Equivalent to `QRect(QPoint(0, 0), geometry().size())`
     */
    [[nodiscard]] QRect rect() const;

    /**
     * Equivalent to `QRectF(QPointF(0, 0), geometryF().size())`
     */
    [[nodiscard]] QRectF rectF() const;

    /**
     * Returns the approximate vertical refresh rate of this output, in mHz.
     */
    [[nodiscard]] uint32_t refreshRate() const;

    /**
     * Returns whether this output is connected through an internal connector,
     * e.g. LVDS, or eDP.
     */
    [[nodiscard]] bool isInternal() const;

    /**
     * Returns the ratio between physical pixels and logical pixels.
     */
    [[nodiscard]] qreal scale() const;

    /**
     * Returns the non-rotated physical size of this output, in millimeters.
     */
    [[nodiscard]] QSize physicalSize() const;

    /** Returns the resolution of the output.  */
    [[nodiscard]] QSize pixelSize() const;
    [[nodiscard]] QSize modeSize() const;

    [[nodiscard]] QString eisaId() const;

    /**
     * Returns the manufacturer of the screen.
     */
    [[nodiscard]] QString manufacturer() const;
    /**
     * Returns the model of the screen.
     */
    [[nodiscard]] QString model() const;
    /**
     * Returns the serial number of the screen.
     */
    [[nodiscard]] QString serialNumber() const;

    /**
     * Returns the RenderLoop for this output. If the platform does not support per screen
     * rendering, all outputs will share the same render loop.
     */
    virtual RenderLoop *renderLoop() const = 0;

    /**
     * @returns the configured time for an output to dim
     *
     * This allows the backends to coordinate with the front-end the time they
     * allow to decorate the dimming until the display is turned off
     *
     * @see aboutToTurnOff
     */
    static std::chrono::milliseconds dimAnimationTime();

    [[nodiscard]] OutputTransform transform() const;
    /**
     * The transform that the user has configured, and which doesn't get changed
     * by automatic rotation
     */
    [[nodiscard]] OutputTransform manualTransform() const;
    [[nodiscard]] QSize orientateSize(const QSize &size) const;

    virtual void applyChanges(const OutputConfiguration &config);

    [[nodiscard]] SubPixel subPixel() const;
    [[nodiscard]] QString description() const;
    [[nodiscard]] Capabilities capabilities() const;
    [[nodiscard]] const Edid &edid() const;
    [[nodiscard]] QList<std::shared_ptr<OutputMode>> modes() const;
    [[nodiscard]] std::shared_ptr<OutputMode> currentMode() const;
    [[nodiscard]] QSize desiredModeSize() const;
    [[nodiscard]] uint32_t desiredModeRefreshRate() const;
    [[nodiscard]] DpmsMode dpmsMode() const;
    virtual void setDpmsMode(DpmsMode mode);

    [[nodiscard]] uint32_t overscan() const;

    [[nodiscard]] VrrPolicy vrrPolicy() const;
    [[nodiscard]] RgbRange rgbRange() const;

    [[nodiscard]] bool isPlaceholder() const;
    [[nodiscard]] bool isNonDesktop() const;
    [[nodiscard]] OutputTransform panelOrientation() const;
    [[nodiscard]] bool wideColorGamut() const;
    [[nodiscard]] bool highDynamicRange() const;
    [[nodiscard]] uint32_t referenceLuminance() const;
    [[nodiscard]] AutoRotationPolicy autoRotationPolicy() const;
    [[nodiscard]] std::shared_ptr<IccProfile> iccProfile() const;
    [[nodiscard]] QString iccProfilePath() const;
    /**
     * @returns the mst path of this output. Is empty if invalid
     */
    [[nodiscard]] QByteArray mstPath() const;

    virtual bool setChannelFactors(const QVector3D &rgb);

    [[nodiscard]] std::optional<double> maxPeakBrightness() const;
    [[nodiscard]] std::optional<double> maxAverageBrightness() const;
    [[nodiscard]] double minBrightness() const;
    [[nodiscard]] std::optional<double> maxPeakBrightnessOverride() const;
    [[nodiscard]] std::optional<double> maxAverageBrightnessOverride() const;
    [[nodiscard]] std::optional<double> minBrightnessOverride() const;

    [[nodiscard]] double sdrGamutWideness() const;
    [[nodiscard]] ColorProfileSource colorProfileSource() const;

    [[nodiscard]] double brightnessSetting() const;
    [[nodiscard]] std::optional<double> currentBrightness() const;
    [[nodiscard]] double artificialHdrHeadroom() const;
    [[nodiscard]] double dimming() const;

    [[nodiscard]] bool detectedDdcCi() const;
    [[nodiscard]] bool allowDdcCi() const;
    [[nodiscard]] bool isDdcCiKnownBroken() const;

    [[nodiscard]] BrightnessDevice *brightnessDevice() const;
    virtual void unsetBrightnessDevice();
    [[nodiscard]] bool allowSdrSoftwareBrightness() const;

    [[nodiscard]] ColorPowerTradeoff colorPowerTradeoff() const;
    [[nodiscard]] QString replicationSource() const;
    [[nodiscard]] uint32_t maxBitsPerColor() const;
    struct BpcRange
    {
        uint32_t min = 0;
        uint32_t max = 0;
        auto operator<=>(const BpcRange &) const = default;
    };
    [[nodiscard]] BpcRange bitsPerColorRange() const;
    [[nodiscard]] std::optional<uint32_t> automaticMaxBitsPerColorLimit() const;
    [[nodiscard]] EdrPolicy edrPolicy() const;
    [[nodiscard]] std::optional<uint32_t> minVrrRefreshRateHz() const;

    virtual void setAutoRotateAvailable(bool isAvailable);

    virtual bool presentAsync(OutputLayer *layer, std::optional<std::chrono::nanoseconds> allowedVrrDelay);
    virtual bool testPresentation(const std::shared_ptr<OutputFrame> &frame) = 0;
    virtual bool present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame) = 0;
    virtual void repairPresentation();

    /**
     * Can be used by the backend to suggest the compositor not to
     * use overlay planes, to avoid driver issues
     */
    [[nodiscard]] virtual bool overlayLayersLikelyBroken() const;

    /**
     * The color space in which the scene is blended
     */
    [[nodiscard]] const std::shared_ptr<ColorDescription> &blendingColor() const;
    /**
     * The color space in which output layers are blended.
     * Note that this may be different from blendingColor.
     */
    [[nodiscard]] const std::shared_ptr<ColorDescription> &layerBlendingColor() const;
    /**
     * The color space that is sent to the output, after blending
     * has happened. May be different from layerBlendingColor.
     */
    [[nodiscard]] const std::shared_ptr<ColorDescription> &colorDescription() const;

Q_SIGNALS:
    /**
     * This signal is emitted when the geometry of this output has changed.
     */
    void geometryChanged();
    /**
     * This signal is emitted when the output has been enabled or disabled.
     */
    void enabledChanged();
    /**
     * This signal is emitted when the device pixel ratio of the output has changed.
     */
    void scaleChanged();

    /**
     * Notifies that the display will be dimmed in @p time ms. This allows
     * effects to plan for it and hopefully animate it
     */
    void aboutToTurnOff(std::chrono::milliseconds time);

    /**
     * Notifies that the output has been turned on and the wake can be decorated.
     */
    void wakeUp();

    /**
     * Notifies that the output is about to change configuration based on a
     * user interaction.
     *
     * Be it because it gets a transformation or moved around.
     *
     * Only to be used for effects
     */
    void aboutToChange(OutputChangeSet *changeSet);

    /**
     * Notifies that the output changed based on a user interaction.
     *
     * Be it because it gets a transformation or moved around.
     *
     * Only to be used for effects
     */
    void changed();

    void outputLayersChanged();

    void currentModeChanged();
    void modesChanged();
    void transformChanged();
    void dpmsModeChanged();
    void capabilitiesChanged();
    void overscanChanged();
    void vrrPolicyChanged();
    void rgbRangeChanged();
    void wideColorGamutChanged();
    void referenceLuminanceChanged();
    void highDynamicRangeChanged();
    void autoRotationPolicyChanged();
    void iccProfileChanged();
    void iccProfilePathChanged();
    void brightnessMetadataChanged();
    void sdrGamutWidenessChanged();
    void colorDescriptionChanged();
    void blendingColorChanged();
    void colorProfileSourceChanged();
    void brightnessChanged();
    void colorPowerTradeoffChanged();
    void dimmingChanged();
    void uuidChanged();
    void replicationSourceChanged();
    void allowDdcCiChanged();
    void maxBitsPerColorChanged();
    void edrPolicyChanged();

protected:
    struct Information
    {
        QString name;
        QString manufacturer;
        QString model;
        QString serialNumber;
        QString eisaId;
        QSize physicalSize;
        Edid edid;
        SubPixel subPixel = SubPixel::Unknown;
        Capabilities capabilities;
        OutputTransform panelOrientation = OutputTransform::Normal;
        bool internal = false;
        bool placeholder = false;
        bool nonDesktop = false;
        QByteArray mstPath;
        std::optional<double> maxPeakBrightness;
        std::optional<double> maxAverageBrightness;
        double minBrightness = 0;
        BpcRange bitsPerColorRange;
        std::optional<uint32_t> minVrrRefreshRateHz;
    };

    struct State
    {
        // 1. Hot Path Members (Grouped for Cache Locality)
        // ----------------------------------------------------
        // Geometry fields are accessed frequently in rendering and input handling.
        // QRectF is 4 doubles (32 bytes), QRect is 4 ints (16 bytes).
        // Total aligned block: ~64 bytes (1 Cache Line)
        QRectF cachedGeometryF;         // 32 bytes
        QRect cachedGeometry;           // 16 bytes
        qreal scale = 1;                // 8 bytes
        QPoint position;                // 8 bytes
        // ----------------------------------------------------

        // 2. Transform & Configuration (Next Access Frequency)
        OutputTransform transform = OutputTransform::Normal;        // 4 bytes
        OutputTransform manualTransform = OutputTransform::Normal;  // 4 bytes
        QSize desiredModeSize;                                      // 8 bytes
        uint32_t desiredModeRefreshRate = 0;                        // 4 bytes
        uint32_t overscan = 0;                                      // 4 bytes
        uint32_t referenceLuminance = 200;                          // 4 bytes
        uint32_t maxBitsPerColor = 0;                               // 4 bytes

        // 3. Floating Point Settings
        double sdrGamutWideness = 0;                                // 8 bytes
        double brightnessSetting = 1.0;                             // 8 bytes
        double artificialHdrHeadroom = 1.0;                         // 8 bytes
        double dimming = 1.0;                                       // 8 bytes

        // 4. Enums (Packed)
        DpmsMode dpmsMode = DpmsMode::On;                           // 4 bytes
        SubPixel subPixel = SubPixel::Unknown;                      // 4 bytes
        RgbRange rgbRange = RgbRange::Automatic;                    // 4 bytes
        AutoRotationPolicy autoRotatePolicy = AutoRotationPolicy::InTabletMode; // 4 bytes
        ColorProfileSource colorProfileSource = ColorProfileSource::sRGB; // 4 bytes
        VrrPolicy vrrPolicy = VrrPolicy::Automatic;                 // 4 bytes
        ColorPowerTradeoff colorPowerTradeoff = ColorPowerTradeoff::PreferEfficiency; // 4 bytes
        EdrPolicy edrPolicy = EdrPolicy::Always;                    // 4 bytes

        // 5. Flags (Bool)
        bool enabled = false;
        bool wideColorGamut = false;
        bool highDynamicRange = false;
        bool allowSdrSoftwareBrightness = true;
        bool detectedDdcCi = false;
        bool allowDdcCi = true;

        // 6. Complex Types & Pointers (Cold Path)
        std::optional<double> currentBrightness;
        std::optional<double> maxPeakBrightnessOverride;
        std::optional<double> maxAverageBrightnessOverride;
        std::optional<double> minBrightnessOverride;
        std::optional<uint32_t> automaticMaxBitsPerColorLimit;

        QList<std::shared_ptr<OutputMode>> modes;
        std::shared_ptr<OutputMode> currentMode;

        QString iccProfilePath;
        std::shared_ptr<IccProfile> iccProfile;
        std::shared_ptr<ColorDescription> originalColorDescription = ColorDescription::sRGB;
        std::shared_ptr<ColorDescription> colorDescription = ColorDescription::sRGB;
        std::shared_ptr<ColorDescription> blendingColor = ColorDescription::sRGB;
        std::shared_ptr<ColorDescription> layerBlendingColor = ColorDescription::sRGB;

        BrightnessDevice *brightnessDevice = nullptr;
        QString uuid;
        QString replicationSource;
    };

    void setInformation(const Information &information);
    void setState(const State &state);

    State m_state;
    Information m_information;
    int m_refCount = 1;
};

// Inline optimizations for hot paths using cached values
inline QRect Output::geometry() const
{
    return m_state.cachedGeometry;
}

inline QRectF Output::geometryF() const
{
    return m_state.cachedGeometryF;
}

inline QRect Output::rect() const
{
    // Reconstruct QRect from cached size to avoid calling geometry() again
    return QRect(QPoint(0, 0), m_state.cachedGeometry.size());
}

inline QRectF Output::rectF() const
{
    // Reconstruct QRectF from cached size
    return QRectF(QPointF(0, 0), m_state.cachedGeometryF.size());
}

KWIN_EXPORT QDebug operator<<(QDebug debug, const Output *output);

} // namespace KWin

Q_DECLARE_OPERATORS_FOR_FLAGS(KWin::Output::Capabilities)
