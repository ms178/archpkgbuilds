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
#include <cstring>
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
class DrmFramebuffer;

class DrmPipeline
{
    Q_GADGET

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

    Error presentLegacy(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame);
    Error legacyModeset();
    Error setLegacyGamma();
    Error applyPendingChangesLegacy();
    bool setCursorLegacy(DrmPipelineLayer *layer);
    static Error commitPipelinesLegacy(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects);

    Error prepareAtomicCommit(DrmAtomicCommit *commit, CommitMode mode, const std::shared_ptr<OutputFrame> &frame);
    bool prepareAtomicModeset(DrmAtomicCommit *commit);
    Error prepareAtomicPresentation(DrmAtomicCommit *commit, const std::shared_ptr<OutputFrame> &frame);
    Error prepareAtomicPlane(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<OutputFrame> &frame);
    void prepareAtomicDisable(DrmAtomicCommit *commit);
    static Error commitPipelinesAtomic(const QList<DrmPipeline *> &pipelines, CommitMode mode, const std::shared_ptr<OutputFrame> &frame, const QList<DrmObject *> &unusedObjects);

    Error prepareAtomicPlaneCursor(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<DrmFramebuffer> &fb, const std::shared_ptr<OutputFrame> &frame);
    Error prepareAtomicPlanePrimary(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<DrmFramebuffer> &fb, const std::shared_ptr<OutputFrame> &frame);

    DrmOutput *m_output = nullptr;
    DrmConnector *m_connector = nullptr;

    bool m_modesetPresentPending = false;
    ColorPipeline m_currentLegacyGamma;

    struct State
    {
        DrmCrtc *crtc = nullptr;
        bool active = true;
        bool enabled = true;
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

        struct alignas(64) PrimaryPlaneCache {
            uint64_t crtcId = 0;
            uint64_t alpha = UINT64_MAX;
            uint64_t zpos = UINT64_MAX;
            uint64_t pixelBlendMode = UINT64_MAX;
            uint64_t colorEncoding = UINT64_MAX;
            uint64_t colorRange = UINT64_MAX;

            uint32_t rotation = 0;
            uint32_t pixelFormat = 0;

            alignas(16) int16_t geometry[8] = {-1, -1, -1, -1, -1, -1, -1, -1};

            uint32_t _pad[9] = {};

            void clear() noexcept
            {
                crtcId = 0;
                alpha = UINT64_MAX;
                zpos = UINT64_MAX;
                pixelBlendMode = UINT64_MAX;
                colorEncoding = UINT64_MAX;
                colorRange = UINT64_MAX;
                rotation = 0;
                pixelFormat = 0;
                for (int i = 0; i < 8; ++i) {
                    geometry[i] = -1;
                }
                for (auto &p : _pad) {
                    p = 0;
                }
            }
        } primaryCache;

        struct alignas(64) CursorPlaneCache {
            uint64_t crtcId = 0;
            int16_t hotspotX = -1;
            int16_t hotspotY = -1;
            uint32_t _pad[29] = {};

            void clear() noexcept
            {
                crtcId = 0;
                hotspotX = -1;
                hotspotY = -1;
                for (auto &p : _pad) {
                    p = 0;
                }
            }
        } cursorCache;

#ifndef Q_MOC_RUN
        static_assert(sizeof(PrimaryPlaneCache) == 128,
                      "PrimaryPlaneCache must be 128 bytes");
        static_assert(sizeof(CursorPlaneCache) == 128,
                      "CursorPlaneCache must be 128 bytes");
        static_assert(alignof(PrimaryPlaneCache) == 64,
                      "PrimaryPlaneCache must be 64-byte aligned");
        static_assert(alignof(CursorPlaneCache) == 64,
                      "CursorPlaneCache must be 64-byte aligned");
#endif
    };

    State m_pending;
    State m_next;

    std::unique_ptr<DrmCommitThread> m_commitThread;
};

}
