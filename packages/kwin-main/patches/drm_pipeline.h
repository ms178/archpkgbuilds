/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include <QList>
#include <QPoint>
#include <QSize>

#include <chrono>
#include <xf86drmMode.h>

#include "core/colorpipeline.h"
#include "core/colorspace.h"
#include "core/output.h"
#include "core/renderloop_p.h"
#include "drm_blob.h"
#include "drm_connector.h"
#include "drm_plane.h"

namespace KWin
{

class DrmGpu;
class DrmConnector;
class DrmCrtc;
class DrmConnectorMode;
class DrmPipelineLayer;
class DrmCommitThread;
class OutputFrame;

class DrmPipeline
{
public:
    DrmPipeline(DrmConnector *conn);
    ~DrmPipeline();

    enum class Error {
        None,
        OutofMemory,
        InvalidArguments,
        NoPermission,
        FramePending,
        TestBufferFailed,
        NotEnoughCrtcs,
        Unknown,
    };
    Q_ENUM(Error)

    /**
     * tests the pending commit first and commits it if the test passes
     * if the test fails, there is a guarantee for no lasting changes
     */
    Error present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame);
    Error testPresent(const std::shared_ptr<OutputFrame> &frame);
    void maybeModeset(const std::shared_ptr<OutputFrame> &frame);
    void forceLegacyModeset();

    bool needsModeset() const;
    void applyPendingChanges();
    void revertPendingChanges();

    bool presentAsync(OutputLayer *layer, std::optional<std::chrono::nanoseconds> allowedVrrDelay);

    DrmConnector *connector() const;
    DrmGpu *gpu() const;

    void pageFlipped(std::chrono::nanoseconds timestamp);
    bool modesetPresentPending() const;
    void resetModesetPresentPending();
    DrmCommitThread *commitThread() const;

    void setOutput(DrmOutput *output);
    DrmOutput *output() const;

    QList<DrmPipelineLayer *> layers() const;
    void setLayers(const QList<DrmPipelineLayer *> &layers);
    std::chrono::nanoseconds presentationDeadline() const;

    DrmCrtc *crtc() const;
    std::shared_ptr<DrmConnectorMode> mode() const;
    bool active() const;
    bool activePending() const;
    bool enabled() const;
    PresentationMode presentationMode() const;
    uint32_t overscan() const;
    Output::RgbRange rgbRange() const;
    DrmConnector::DrmContentType contentType() const;
    const std::shared_ptr<IccProfile> &iccProfile() const;

    void setCrtc(DrmCrtc *crtc);
    void setMode(const std::shared_ptr<DrmConnectorMode> &mode);
    void setActive(bool active);
    void setEnable(bool enable);
    void setPresentationMode(PresentationMode mode);
    void setOverscan(uint32_t overscan);
    void setRgbRange(Output::RgbRange range);
    void setCrtcColorPipeline(const ColorPipeline &pipeline);
    void setContentType(DrmConnector::DrmContentType type);
    void setIccProfile(const std::shared_ptr<IccProfile> &profile);
    void setHighDynamicRange(bool hdr);
    void setWideColorGamut(bool wcg);
    void setMaxBpc(uint32_t max);

    enum class CommitMode {
        Test,
        TestAllowModeset,
        CommitModeset
    };
    Q_ENUM(CommitMode)
    static Error commitPipelines(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects = {});

private:
    bool isBufferForDirectScanout() const;
    uint32_t calculateUnderscan();
    static Error errnoToError();
    std::shared_ptr<DrmBlob> createHdrMetadata(TransferFunction::Type transferFunction) const;

    // legacy only
    Error presentLegacy(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame);
    Error legacyModeset();
    Error setLegacyGamma();
    Error applyPendingChangesLegacy();
    bool setCursorLegacy(DrmPipelineLayer *layer);
    static Error commitPipelinesLegacy(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects);

    // atomic modesetting only
    Error prepareAtomicCommit(DrmAtomicCommit *commit, CommitMode mode, const std::shared_ptr<OutputFrame> &frame);
    bool prepareAtomicModeset(DrmAtomicCommit *commit);
    Error prepareAtomicPresentation(DrmAtomicCommit *commit, const std::shared_ptr<OutputFrame> &frame);
    Error prepareAtomicPlane(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<OutputFrame> &frame);
    void prepareAtomicDisable(DrmAtomicCommit *commit);
    static Error commitPipelinesAtomic(const QList<DrmPipeline *> &pipelines, CommitMode mode, const std::shared_ptr<OutputFrame> &frame, const QList<DrmObject *> &unusedObjects);

    DrmOutput *m_output = nullptr;
    DrmConnector *m_connector = nullptr;

    bool m_modesetPresentPending = false;
    ColorPipeline m_currentLegacyGamma;

    struct State
    {
        DrmCrtc *crtc = nullptr;
        bool active = true; // whether or not the pipeline should be currently used
        bool enabled = true; // whether or not the pipeline needs a crtc
        bool needsModeset = false;
        bool needsModesetProperties = false;
        std::shared_ptr<DrmConnectorMode> mode;
        uint32_t overscan = 0;
        Output::RgbRange rgbRange = Output::RgbRange::Automatic;
        PresentationMode presentationMode = PresentationMode::VSync;
        ColorPipeline crtcColorPipeline;
        DrmConnector::DrmContentType contentType = DrmConnector::DrmContentType::Graphics;

        std::shared_ptr<IccProfile> iccProfile;
        bool hdr = false;
        bool wcg = false;
        uint32_t maxBpc = 10;

        QList<DrmPipelineLayer *> layers;

        struct alignas(8) PlaneCache {
            //
            // 8-byte aligned fields (48 bytes total)
            //
            // These are placed first to ensure natural alignment and avoid padding.
            // All uint64_t fields use UINT64_MAX as sentinel except crtcId and rotation.
            //
            uint64_t crtcId = 0;              // Sentinel: 0 (CRTC IDs start at 1)
            uint64_t alpha = UINT64_MAX;      // Sentinel: UINT64_MAX (valid range: 0-65535)
            uint64_t zpos = UINT64_MAX;       // Sentinel: UINT64_MAX (valid range: 0-255)
            uint64_t pixelBlendMode = UINT64_MAX;   // Enum stored as uint64_t
            uint64_t colorEncoding = UINT64_MAX;    // Enum stored as uint64_t
            uint64_t colorRange = UINT64_MAX;       // Enum stored as uint64_t

            //
            // 4-byte aligned field (4 bytes)
            //
            // Placed after uint64_t fields to maintain alignment without padding.
            //
            uint32_t rotation = 0;            // Sentinel: 0 (valid rotations are bitmask 1-63)

            //
            // 2-byte aligned fields (20 bytes total)
            //
            // Placed last. Total struct size will be 48 + 4 + 20 = 72 bytes.
            // Since 72 is divisible by 8 (alignas requirement), no tail padding needed.
            //
            // Using int16_t supports coordinates from -32768 to +32767, sufficient for:
            // - 8K displays (7680×4320)
            // - Negative coordinates (off-screen positioning)
            // - Sub-pixel precision after scaling
            //
            int16_t srcX = -1, srcY = -1, srcW = -1, srcH = -1;  // Source rectangle (4 × 2 = 8 bytes)
            int16_t dstX = -1, dstY = -1, dstW = -1, dstH = -1;  // Dest rectangle (4 × 2 = 8 bytes)
            int16_t hotspotX = -1, hotspotY = -1;                // Cursor hotspot (2 × 2 = 4 bytes)

            /**
             * Reset cache to initial (unset) state.
             *
             * Called on modeset when property capabilities may have changed.
             * Uses memset for optimal performance (single REP STOSQ instruction on x86-64).
             *
             * SAFETY: All sentinel values can be set via memset(0xFF):
             * - UINT64_MAX = 0xFFFFFFFFFFFFFFFF
             * - int16_t(-1) = 0xFFFF (two's complement)
             *
             * Then override fields that need different sentinels (crtcId, rotation).
             */
            void clear() {
                // Fast clear: set all bytes to 0xFF (one instruction on x86-64)
                std::memset(this, 0xFF, sizeof(*this));

                // Override fields with non-0xFF sentinels
                crtcId = 0;
                rotation = 0;
                // All int16_t fields already -1 from memset (0xFFFF = -1 in two's complement)
                // All UINT64_MAX fields already set from memset
            }
        } planeCache;

        // Compile-time size verification
        static_assert(sizeof(PlaneCache) == 72,
                      "PlaneCache must be exactly 72 bytes (6×8 + 1×4 + 10×2)");
        static_assert(alignof(PlaneCache) == 8,
                      "PlaneCache must be 8-byte aligned for optimal cache access");
        static_assert(sizeof(PlaneCache) % 8 == 0,
                      "PlaneCache size must be multiple of 8 (no tail padding)");

        // Verify sentinel assumptions
        static_assert(static_cast<uint64_t>(-1) == UINT64_MAX,
                      "UINT64_MAX must equal all-bits-set for memset optimization");
        static_assert(static_cast<int16_t>(0xFFFF) == -1,
                      "int16_t must use two's complement (required for memset clear)");
    };

    // the state that is to be tested next
    State m_pending;
    // the state that will be applied at the next real atomic commit
    State m_next;

    std::unique_ptr<DrmCommitThread> m_commitThread;
};

}
