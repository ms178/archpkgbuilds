/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "core/colorpipeline.h"
#include "core/colorspace.h"
#include "core/output.h"
#include "core/renderloop_p.h"
#include "drm_blob.h"
#include "drm_connector.h"
#include "drm_plane.h"

#include <QList>
#include <QPoint>
#include <QSize>

#include <chrono>
#include <memory>
#include <optional>

struct hdr_output_metadata;

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
class OutputLayer;
class DrmAtomicCommit;

class DrmPipeline
{
    Q_GADGET
public:
    Q_DISABLE_COPY_MOVE(DrmPipeline)

    explicit DrmPipeline(DrmConnector *conn);
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

    [[nodiscard]] Error present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame);
    [[nodiscard]] Error testPresent(const std::shared_ptr<OutputFrame> &frame);
    void maybeModeset(const std::shared_ptr<OutputFrame> &frame);
    void forceLegacyModeset();

    [[nodiscard]] bool needsModeset() const noexcept;
    void applyPendingChanges();
    void revertPendingChanges();

    [[nodiscard]] bool presentAsync(OutputLayer *layer, std::optional<std::chrono::nanoseconds> allowedVrrDelay);

    [[nodiscard]] DrmConnector *connector() const noexcept;
    [[nodiscard]] DrmGpu *gpu() const noexcept;

    void pageFlipped(std::chrono::nanoseconds timestamp);
    [[nodiscard]] bool modesetPresentPending() const noexcept;
    void resetModesetPresentPending() noexcept;
    [[nodiscard]] DrmCommitThread *commitThread() const noexcept;

    void setOutput(DrmOutput *output);
    [[nodiscard]] DrmOutput *output() const noexcept;

    [[nodiscard]] QList<DrmPipelineLayer *> layers() const;
    void setLayers(const QList<DrmPipelineLayer *> &layers);
    [[nodiscard]] std::chrono::nanoseconds presentationDeadline() const noexcept;

    [[nodiscard]] DrmCrtc *crtc() const noexcept;
    [[nodiscard]] std::shared_ptr<DrmConnectorMode> mode() const noexcept;
    [[nodiscard]] bool active() const noexcept;
    [[nodiscard]] bool activePending() const noexcept;
    [[nodiscard]] bool enabled() const noexcept;
    [[nodiscard]] PresentationMode presentationMode() const noexcept;
    [[nodiscard]] uint32_t overscan() const noexcept;
    [[nodiscard]] Output::RgbRange rgbRange() const noexcept;
    [[nodiscard]] DrmConnector::DrmContentType contentType() const noexcept;
    [[nodiscard]] const std::shared_ptr<IccProfile> &iccProfile() const noexcept;

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
    [[nodiscard]] bool isBufferForDirectScanout() const;
    [[nodiscard]] uint32_t calculateUnderscan() noexcept;
    [[nodiscard]] static Error errnoToError() noexcept;
    [[nodiscard]] std::shared_ptr<DrmBlob> createHdrMetadata(TransferFunction::Type transferFunction) const;

    Error presentLegacy(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame);
    Error legacyModeset();
    Error setLegacyGamma();
    Error applyPendingChangesLegacy();
    bool setCursorLegacy(DrmPipelineLayer *layer);
    static Error commitPipelinesLegacy(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects);

    [[nodiscard]] Error prepareAtomicCommit(DrmAtomicCommit *commit, CommitMode mode, const std::shared_ptr<OutputFrame> &frame);
    [[nodiscard]] bool prepareAtomicModeset(DrmAtomicCommit *commit);
    [[nodiscard]] Error prepareAtomicPresentation(DrmAtomicCommit *commit, const std::shared_ptr<OutputFrame> &frame);
    [[nodiscard]] Error prepareAtomicPlane(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<OutputFrame> &frame);
    void prepareAtomicDisable(DrmAtomicCommit *commit);
    static Error commitPipelinesAtomic(const QList<DrmPipeline *> &pipelines, CommitMode mode, const std::shared_ptr<OutputFrame> &frame, const QList<DrmObject *> &unusedObjects);

    [[nodiscard]] Error prepareAtomicPlaneCursor(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<DrmFramebuffer> &fb, const std::shared_ptr<OutputFrame> &frame);
    [[nodiscard]] Error prepareAtomicPlanePrimary(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<DrmFramebuffer> &fb, const std::shared_ptr<OutputFrame> &frame);

    DrmOutput *m_output = nullptr;
    DrmConnector *m_connector = nullptr;

    bool m_modesetPresentPending = false;
    ColorPipeline m_currentLegacyGamma;

    struct State
    {
        DrmCrtc *crtc = nullptr;
        std::shared_ptr<DrmConnectorMode> mode;
        QList<DrmPipelineLayer *> layers;
        PresentationMode presentationMode = PresentationMode::VSync;

        bool active = true;
        bool enabled = true;
        bool needsModeset = false;
        bool needsModesetProperties = false;
        bool hdr = false;
        bool wcg = false;

        uint32_t overscan = 0;
        uint32_t maxBpc = 10;

        Output::RgbRange rgbRange = Output::RgbRange::Automatic;
        DrmConnector::DrmContentType contentType = DrmConnector::DrmContentType::Graphics;

        ColorPipeline crtcColorPipeline;
        std::shared_ptr<IccProfile> iccProfile;
    };

    State m_pending;
    State m_next;

    std::unique_ptr<DrmCommitThread> m_commitThread;
};

}
