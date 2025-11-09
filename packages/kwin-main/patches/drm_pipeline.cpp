/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "drm_pipeline.h"

#include "core/iccprofile.h"
#include "core/session.h"
#include "drm_backend.h"
#include "drm_buffer.h"
#include "drm_commit.h"
#include "drm_commit_thread.h"
#include "drm_connector.h"
#include "drm_crtc.h"
#include "drm_egl_backend.h"
#include "drm_gpu.h"
#include "drm_layer.h"
#include "drm_logging.h"
#include "drm_output.h"
#include "utils/drm_format_helper.h"
#include "utils/envvar.h"
#include "utils/kernel.h"

#include <cerrno>
#include <chrono>
#include <cmath>
#include <cstring>
#include <drm_fourcc.h>
#include <gbm.h>

using namespace std::chrono_literals;

namespace KWin
{

namespace
{
[[nodiscard]] constexpr bool isYuvFormat(uint32_t format) noexcept
{
    switch (format) {
    case DRM_FORMAT_YUV410:
    case DRM_FORMAT_YVU410:
    case DRM_FORMAT_YUV411:
    case DRM_FORMAT_YVU411:
    case DRM_FORMAT_YUV420:
    case DRM_FORMAT_YVU420:
    case DRM_FORMAT_YUV422:
    case DRM_FORMAT_YVU422:
    case DRM_FORMAT_YUV444:
    case DRM_FORMAT_YVU444:
    case DRM_FORMAT_NV12:
    case DRM_FORMAT_NV21:
    case DRM_FORMAT_NV16:
    case DRM_FORMAT_NV61:
    case DRM_FORMAT_NV24:
    case DRM_FORMAT_NV42:
    case DRM_FORMAT_YUYV:
    case DRM_FORMAT_YVYU:
    case DRM_FORMAT_UYVY:
    case DRM_FORMAT_VYUY:
    case DRM_FORMAT_P010:
    case DRM_FORMAT_P012:
    case DRM_FORMAT_P016:
        return true;
    default:
        return false;
    }
}

const auto s_forceScalingMode = []() -> std::optional<DrmConnector::ScalingMode> {
    const auto env = qEnvironmentVariable("KWIN_DRM_FORCE_SCALING_MODE");
    if (env == "NONE") {
        return DrmConnector::ScalingMode::None;
    } else if (env == "FULL") {
        return DrmConnector::ScalingMode::Full;
    } else if (env == "CENTER") {
        return DrmConnector::ScalingMode::Center;
    } else if (env == "FULL_ASPECT") {
        return DrmConnector::ScalingMode::Full_Aspect;
    }
    return std::nullopt;
}();
}

DrmPipeline::DrmPipeline(DrmConnector *conn)
    : m_connector(conn)
    , m_commitThread(std::make_unique<DrmCommitThread>(conn->gpu(), conn->connectorName()))
{
}

DrmPipeline::~DrmPipeline() = default;

DrmPipeline::Error DrmPipeline::testPresent(const std::shared_ptr<OutputFrame> &frame)
{
    if (!gpu()->atomicModeSetting()) [[unlikely]] {
        return Error::None;
    }
    return DrmPipeline::commitPipelinesAtomic({this}, CommitMode::Test, frame, {});
}

DrmPipeline::Error DrmPipeline::present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame)
{
    Q_ASSERT(m_pending.crtc);

    if (!gpu()->atomicModeSetting()) [[unlikely]] {
        return presentLegacy(layersToUpdate, frame);
    }

    if (m_pending.needsModeset) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Modeset required, delegating to maybeModeset";
        return Error::InvalidArguments;
    }

    if (Error err = testPresent(frame); err != Error::None) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Test present failed, error:" << static_cast<int>(err);
        return err;
    }

    auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
    if (Error err = prepareAtomicPresentation(partialUpdate.get(), frame); err != Error::None) [[unlikely]] {
        qCWarning(KWIN_DRM) << "prepareAtomicPresentation failed:" << static_cast<int>(err);
        return err;
    }

    bool updatedAnyLayer = false;
    for (const auto layer : layersToUpdate) {
        if (!layer) [[unlikely]] {
            continue;
        }
        auto *pipelineLayer = static_cast<DrmPipelineLayer *>(layer);
        if (Error err = prepareAtomicPlane(partialUpdate.get(), pipelineLayer->plane(), pipelineLayer, frame); err != Error::None) [[unlikely]] {
            qCWarning(KWIN_DRM) << "prepareAtomicPlane failed for layer:" << static_cast<int>(err);
            return err;
        }
        updatedAnyLayer = true;
    }

    if (!updatedAnyLayer) [[unlikely]] {
        if (m_pending.layers.isEmpty()) {
            qCWarning(KWIN_DRM) << "No layers to present";
            return Error::InvalidArguments;
        }
        auto *fallbackLayer = m_pending.layers.front();
        if (!fallbackLayer) {
            qCWarning(KWIN_DRM) << "Fallback layer is null";
            return Error::InvalidArguments;
        }
        if (Error err = prepareAtomicPlane(partialUpdate.get(), fallbackLayer->plane(), fallbackLayer, frame); err != Error::None) {
            qCWarning(KWIN_DRM) << "Fallback layer preparation failed:" << static_cast<int>(err);
            return err;
        }
    }

    if (m_pending.needsModesetProperties) [[unlikely]] {
        if (!prepareAtomicModeset(partialUpdate.get())) {
            qCWarning(KWIN_DRM) << "prepareAtomicModeset failed";
            return Error::InvalidArguments;
        }
    }

    m_next.needsModesetProperties = m_pending.needsModesetProperties = false;
    m_commitThread->addCommit(std::move(partialUpdate));
    return Error::None;
}

void DrmPipeline::maybeModeset(const std::shared_ptr<OutputFrame> &frame)
{
    m_modesetPresentPending = true;
    gpu()->maybeModeset(this, frame);
}

DrmPipeline::Error DrmPipeline::commitPipelines(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects)
{
    Q_ASSERT(!pipelines.isEmpty());
    if (pipelines.front()->gpu()->atomicModeSetting()) [[likely]] {
        return commitPipelinesAtomic(pipelines, mode, nullptr, unusedObjects);
    }
    return commitPipelinesLegacy(pipelines, mode, unusedObjects);
}

DrmPipeline::Error DrmPipeline::commitPipelinesAtomic(const QList<DrmPipeline *> &pipelines,
                                                      CommitMode mode,
                                                      const std::shared_ptr<OutputFrame> &frame,
                                                      const QList<DrmObject *> &unusedObjects)
{
    auto commit = std::make_unique<DrmAtomicCommit>(pipelines);

    if (mode == CommitMode::Test) [[likely]] {
        const bool anyModeset = std::any_of(pipelines.cbegin(), pipelines.cend(),
                                            [](const DrmPipeline *pipeline) {
                                                return pipeline->needsModeset();
                                            });
        if (anyModeset) [[unlikely]] {
            mode = CommitMode::TestAllowModeset;
        }
    }

    for (DrmPipeline *pipeline : pipelines) {
        if (Error err = pipeline->prepareAtomicCommit(commit.get(), mode, frame); err != Error::None) [[unlikely]] {
            qCWarning(KWIN_DRM) << "prepareAtomicCommit failed for pipeline:" << static_cast<int>(err);
            return err;
        }
    }

    for (DrmObject *unused : unusedObjects) {
        if (unused) [[likely]] {
            unused->disable(commit.get());
        }
    }

    switch (mode) {
    case CommitMode::TestAllowModeset: {
        if (!commit->testAllowModeset()) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Atomic modeset test failed! errno:" << errno << strerror(errno);
            return errnoToError();
        }

        bool withoutModeset = true;
        for (DrmPipeline *pipeline : pipelines) {
            auto testCommit = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{pipeline});
            if (pipeline->prepareAtomicCommit(testCommit.get(), CommitMode::TestAllowModeset, frame) != Error::None || !testCommit->test()) {
                withoutModeset = false;
                break;
            }
        }

        for (DrmPipeline *pipeline : pipelines) {
            pipeline->m_pending.needsModeset = !withoutModeset;
            pipeline->m_pending.needsModesetProperties = true;
        }
        return Error::None;
    }
    case CommitMode::CommitModeset: {
        if (!commit->commitModeset()) [[unlikely]] {
            qCCritical(KWIN_DRM) << "Atomic modeset commit failed! errno:" << errno << strerror(errno);
            return errnoToError();
        }
        for (DrmPipeline *pipeline : pipelines) {
            pipeline->m_next.needsModeset = pipeline->m_pending.needsModeset = false;
        }
        commit->pageFlipped(std::chrono::steady_clock::now().time_since_epoch());
        return Error::None;
    }
    case CommitMode::Test: {
        if (!commit->test()) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Atomic test failed! errno:" << errno << strerror(errno);
            return errnoToError();
        }
        return Error::None;
    }
    default:
        Q_UNREACHABLE();
    }
}

DrmPipeline::Error DrmPipeline::prepareAtomicCommit(DrmAtomicCommit *commit, CommitMode mode, const std::shared_ptr<OutputFrame> &frame)
{
    if (!commit) [[unlikely]] {
        return Error::InvalidArguments;
    }

    if (activePending()) [[likely]] {
        if (Error err = prepareAtomicPresentation(commit, frame); err != Error::None) [[unlikely]] {
            return err;
        }
        for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
            if (!layer) [[unlikely]] {
                return Error::InvalidArguments;
            }
            if (Error err = prepareAtomicPlane(commit, layer->plane(), layer, frame); err != Error::None) [[unlikely]] {
                return err;
            }
        }
        if (mode != CommitMode::Test && (mode == CommitMode::CommitModeset || m_pending.needsModesetProperties)) [[unlikely]] {
            if (!prepareAtomicModeset(commit)) {
                return Error::InvalidArguments;
            }
        }
    } else {
        prepareAtomicDisable(commit);
    }
    return Error::None;
}

DrmPipeline::Error DrmPipeline::prepareAtomicPresentation(DrmAtomicCommit *commit, const std::shared_ptr<OutputFrame> &)
{
    if (!commit || !m_pending.crtc) [[unlikely]] {
        return Error::InvalidArguments;
    }

    commit->setPresentationMode(m_pending.presentationMode);

    if (m_connector->contentType.isValid()) [[likely]] {
        commit->addEnum(m_connector->contentType, m_pending.contentType);
    }

    if (m_pending.crtc->vrrEnabled.isValid()) [[likely]] {
        bool shouldEnableVrr = (m_pending.presentationMode == PresentationMode::AdaptiveSync)
            || (m_pending.presentationMode == PresentationMode::AdaptiveAsync);

        if (m_pending.needsModeset) [[unlikely]] {
            shouldEnableVrr = false;
        }
        commit->setVrr(m_pending.crtc, shouldEnableVrr);
    }

    if (m_pending.layers.isEmpty()) [[unlikely]] {
        return Error::InvalidArguments;
    }

    DrmPipelineLayer *referenceLayer = nullptr;
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (!layer) [[unlikely]] {
            return Error::InvalidArguments;
        }
        if (!referenceLayer && layer->isEnabled()) [[likely]] {
            referenceLayer = layer;
        }
    }

    if (!referenceLayer) [[unlikely]] {
        return Error::InvalidArguments;
    }

    const ColorPipeline &referencePipeline = referenceLayer->colorPipeline();
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (!layer || !layer->isEnabled()) [[unlikely]] {
            continue;
        }
        if (layer->colorPipeline() != referencePipeline) [[unlikely]] {
            return Error::InvalidArguments;
        }
    }

    const ColorPipeline mergedPipeline = referencePipeline.merged(m_pending.crtcColorPipeline);
    if (!m_pending.crtc->postBlendingPipeline) [[unlikely]] {
        if (!mergedPipeline.isIdentity()) {
            return Error::InvalidArguments;
        }
    } else {
        if (!m_pending.crtc->postBlendingPipeline->matchPipeline(commit, mergedPipeline)) [[unlikely]] {
            return Error::InvalidArguments;
        }
    }

    return Error::None;
}

DrmPipeline::Error DrmPipeline::prepareAtomicPlane(DrmAtomicCommit *commit,
                                                   DrmPlane *plane,
                                                   DrmPipelineLayer *layer,
                                                   const std::shared_ptr<OutputFrame> &frame)
{
    if (!commit || !plane || !layer) [[unlikely]] {
        return Error::InvalidArguments;
    }

    if (!layer->isEnabled()) [[unlikely]] {
        plane->disable(commit);
        return Error::None;
    }

    const auto fb = layer->currentBuffer();
    if (!fb) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Enabled plane has no buffer";
        return Error::TestBufferFailed;
    }

    const QRect sourceRect = layer->sourceRect().toRect();
    const QRect targetRect = layer->targetRect();

    if (sourceRect.width() <= 0 || sourceRect.height() <= 0 || targetRect.width() <= 0 || targetRect.height() <= 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid plane geometry: src=" << sourceRect << "dst=" << targetRect;
        return Error::InvalidArguments;
    }

    plane->set(commit, sourceRect, targetRect);
    commit->addBuffer(plane, fb, frame);
    commit->addProperty(plane->crtcId, m_pending.crtc->id());

    const bool isCursor = (plane->type.enumValue() == DrmPlane::TypeIndex::Cursor);
    if (isCursor) [[unlikely]] {
        if (plane->vmHotspotX.isValid() && plane->vmHotspotY.isValid()) [[likely]] {
            const QPointF hotspot = layer->hotspot();
            const int64_t hotX = std::lround(hotspot.x());
            const int64_t hotY = std::lround(hotspot.y());

            if (hotX < 0 || hotY < 0 || hotX > 32767 || hotY > 32767) [[unlikely]] {
                qCWarning(KWIN_DRM) << "Cursor hotspot out of range:" << hotspot;
                return Error::InvalidArguments;
            }

            commit->addProperty(plane->vmHotspotX, static_cast<uint64_t>(hotX));
            commit->addProperty(plane->vmHotspotY, static_cast<uint64_t>(hotY));
        }
        return Error::None;
    }

    if (plane->rotation.isValid()) [[likely]] {
        const OutputTransform transform = layer->offloadTransform();
        if (!plane->supportsTransformation(transform)) [[unlikely]] {
            return Error::InvalidArguments;
        }
        if (transform.kind() != OutputTransform::Kind::Normal) [[unlikely]] {
            commit->addEnum(plane->rotation, DrmPlane::outputTransformToPlaneTransform(transform));
        }
    }

    if (plane->alpha.isValid()) [[likely]] {
        commit->addProperty(plane->alpha, plane->alpha.maxValue());
    }
    if (plane->pixelBlendMode.isValid()) [[likely]] {
        if (plane->pixelBlendMode.hasEnum(DrmPlane::PixelBlendMode::PreMultiplied)) [[likely]] {
            commit->addEnum(plane->pixelBlendMode, DrmPlane::PixelBlendMode::PreMultiplied);
        }
    }
    if (plane->zpos.isValid() && !plane->zpos.isImmutable()) [[likely]] {
        commit->addProperty(plane->zpos, layer->zpos());
    }

    const auto buffer = fb->buffer();
    const auto *attrs = buffer ? buffer->dmabufAttributes() : nullptr;
    if (!attrs) [[unlikely]] {
        return Error::None;
    }

    if (!isYuvFormat(attrs->format)) [[likely]] {
        return Error::None;
    }

    const auto colorDesc = layer->colorDescription();
    if (!colorDesc) [[unlikely]] {
        return Error::None;
    }

    if (plane->colorEncoding.isValid()) [[likely]] {
        const auto matrix = colorDesc->yuvCoefficients();
        if (matrix == YUVMatrixCoefficients::BT709 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) [[likely]] {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
        } else if (matrix == YUVMatrixCoefficients::BT601 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT601_YCbCr)) {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT601_YCbCr);
        } else if (matrix == YUVMatrixCoefficients::BT2020 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT2020_YCbCr)) [[unlikely]] {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT2020_YCbCr);
        }
    }

    if (plane->colorRange.isValid()) [[likely]] {
        const auto range = (colorDesc->range() == EncodingRange::Full)
            ? DrmPlane::ColorRange::Full_YCbCr
            : DrmPlane::ColorRange::Limited_YCbCr;
        if (plane->colorRange.hasEnum(range)) [[likely]] {
            commit->addEnum(plane->colorRange, range);
        }
    }

    return Error::None;
}

void DrmPipeline::prepareAtomicDisable(DrmAtomicCommit *commit)
{
    if (!commit) [[unlikely]] {
        return;
    }

    m_connector->disable(commit);
    if (!m_pending.crtc) [[unlikely]] {
        return;
    }

    m_pending.crtc->disable(commit);
    for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
        if (layer && layer->plane()) [[likely]] {
            layer->plane()->disable(commit);
        }
    }
}

bool DrmPipeline::prepareAtomicModeset(DrmAtomicCommit *commit)
{
    if (!commit || !m_pending.crtc || !m_pending.mode) [[unlikely]] {
        return false;
    }

    commit->addProperty(m_connector->crtcId, m_pending.crtc->id());

    if (m_connector->broadcastRGB.isValid()) [[likely]] {
        commit->addEnum(m_connector->broadcastRGB, DrmConnector::rgbRangeToBroadcastRgb(m_pending.rgbRange));
    }
    if (m_connector->linkStatus.isValid()) [[likely]] {
        commit->addEnum(m_connector->linkStatus, DrmConnector::LinkStatus::Good);
    }

    if (m_connector->overscan.isValid()) [[likely]] {
        commit->addProperty(m_connector->overscan, m_pending.overscan);
    } else if (m_connector->underscan.isValid()) [[likely]] {
        const uint32_t hborder = calculateUnderscan();
        const auto underscanMode = m_pending.overscan != 0
            ? DrmConnector::UnderscanOptions::On
            : DrmConnector::UnderscanOptions::Off;
        commit->addEnum(m_connector->underscan, underscanMode);
        commit->addProperty(m_connector->underscanVBorder, m_pending.overscan);
        commit->addProperty(m_connector->underscanHBorder, hborder);
    }

    if (m_connector->maxBpc.isValid()) [[likely]] {
        const uint32_t clampedBpc = std::clamp<uint32_t>(m_pending.maxBpc,
                                                         m_connector->maxBpc.minValue(),
                                                         m_connector->maxBpc.maxValue());
        commit->addProperty(m_connector->maxBpc, clampedBpc);
    }

    if (m_connector->hdrMetadata.isValid()) [[unlikely]] {
        const auto tf = m_pending.hdr ? TransferFunction::PerceptualQuantizer : TransferFunction::gamma22;
        commit->addBlob(m_connector->hdrMetadata, createHdrMetadata(tf));
    } else if (m_pending.hdr) [[unlikely]] {
        return false;
    }

    if (m_pending.wcg) [[unlikely]] {
        if (!m_connector->colorspace.isValid() || !m_connector->colorspace.hasEnum(DrmConnector::Colorspace::BT2020_RGB)) {
            return false;
        }
        commit->addEnum(m_connector->colorspace, DrmConnector::Colorspace::BT2020_RGB);
    } else if (m_connector->colorspace.isValid()) [[likely]] {
        commit->addEnum(m_connector->colorspace, DrmConnector::Colorspace::Default);
    }

    if (m_connector->scalingMode.isValid()) [[likely]] {
        if (s_forceScalingMode.has_value()) [[unlikely]] {
            if (m_connector->scalingMode.hasEnum(*s_forceScalingMode)) {
                commit->addEnum(m_connector->scalingMode, *s_forceScalingMode);
            } else if (m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) {
                commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::None);
            }
        } else if (m_connector->isInternal()
                   && m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::Full_Aspect)
                   && (m_pending.mode->flags() & OutputMode::Flag::Generated)) [[unlikely]] {
            commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::Full_Aspect);
        } else if (m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) {
            commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::None);
        }
    }

    commit->addProperty(m_pending.crtc->active, 1);
    commit->addBlob(m_pending.crtc->modeId, m_pending.mode->blob());

    if (m_pending.crtc->degammaLut.isValid()) [[likely]] {
        commit->addProperty(m_pending.crtc->degammaLut, 0);
    }

    return true;
}

uint32_t DrmPipeline::calculateUnderscan()
{
    if (!m_pending.mode) [[unlikely]] {
        return 0;
    }
    const QSize size = m_pending.mode->size();
    if (size.height() <= 0) [[unlikely]] {
        return 0;
    }
    const float aspectRatio = size.width() / static_cast<float>(size.height());
    uint32_t hborder = static_cast<uint32_t>(std::lround(m_pending.overscan * aspectRatio));
    if (hborder > 128u) [[unlikely]] {
        hborder = 128u;
        m_pending.overscan = static_cast<uint32_t>(128 / aspectRatio);
    }
    return hborder;
}

DrmPipeline::Error DrmPipeline::errnoToError()
{
    switch (errno) {
    case EINVAL:
        return Error::InvalidArguments;
    case EBUSY:
        return Error::FramePending;
    case ENOMEM:
        return Error::OutofMemory;
    case EACCES:
        return Error::NoPermission;
    default:
        return Error::Unknown;
    }
}

bool DrmPipeline::presentAsync(OutputLayer *layer, std::optional<std::chrono::nanoseconds> allowedVrrDelay)
{
    if (!layer || needsModeset() || !m_pending.crtc || !m_pending.active) [[unlikely]] {
        return false;
    }

    if (gpu()->isVmwgfx()) [[unlikely]] {
        return false;
    }

    auto *drmLayer = static_cast<DrmPipelineLayer *>(layer);
    DrmPlane *plane = drmLayer ? drmLayer->plane() : nullptr;

    if (!plane) [[unlikely]] {
        return setCursorLegacy(drmLayer);
    }

    if (DrmPipeline::commitPipelinesAtomic({this}, CommitMode::Test, nullptr, {}) != Error::None) [[unlikely]] {
        qCDebug(KWIN_DRM) << "presentAsync: atomic test failed, aborting";
        return false;
    }

    auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
    if (prepareAtomicPlane(partialUpdate.get(), plane, drmLayer, nullptr) != Error::None) [[unlikely]] {
        qCDebug(KWIN_DRM) << "presentAsync: plane preparation failed";
        return false;
    }
    partialUpdate->setAllowedVrrDelay(allowedVrrDelay);
    m_commitThread->addCommit(std::move(partialUpdate));
    return true;
}

void DrmPipeline::applyPendingChanges()
{
    const bool layersChanged = m_next.layers != m_pending.layers;
    m_next = m_pending;

    if (m_pending.mode) [[likely]] {
        m_commitThread->setModeInfo(m_pending.mode->refreshRate(), m_pending.mode->vblankTime());
    }

    if (m_output) [[likely]] {
        if (auto *loop = m_output->renderLoop()) {
            loop->setPresentationSafetyMargin(m_commitThread->safetyMargin());
            if (m_pending.mode) {
                loop->setRefreshRate(m_pending.mode->refreshRate());
            }
        }
        if (layersChanged) [[unlikely]] {
            Q_EMIT m_output->outputLayersChanged();
        }
    }
}

DrmConnector *DrmPipeline::connector() const noexcept
{
    return m_connector;
}

DrmGpu *DrmPipeline::gpu() const noexcept
{
    return m_connector->gpu();
}

void DrmPipeline::pageFlipped(std::chrono::nanoseconds timestamp)
{
    if (m_output && m_output->renderLoop()) [[likely]] {
        RenderLoopPrivate::get(m_output->renderLoop())->notifyVblank(timestamp);
        m_output->renderLoop()->setPresentationSafetyMargin(m_commitThread->safetyMargin());
    }
    m_commitThread->pageFlipped(timestamp);
    if (gpu()->needsModeset()) [[unlikely]] {
        gpu()->maybeModeset(nullptr, nullptr);
    }
}

void DrmPipeline::setOutput(DrmOutput *output)
{
    m_output = output;
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (layer) [[likely]] {
            layer->setOutput(output);
        }
    }
}

DrmOutput *DrmPipeline::output() const noexcept
{
    return m_output;
}

bool DrmPipeline::needsModeset() const noexcept
{
    return m_pending.needsModeset;
}

bool DrmPipeline::activePending() const noexcept
{
    return m_pending.crtc && m_pending.mode && m_pending.active;
}

void DrmPipeline::revertPendingChanges()
{
    m_pending = m_next;
}

DrmCommitThread *DrmPipeline::commitThread() const noexcept
{
    return m_commitThread.get();
}

bool DrmPipeline::modesetPresentPending() const noexcept
{
    return m_modesetPresentPending;
}

void DrmPipeline::resetModesetPresentPending()
{
    m_modesetPresentPending = false;
}

DrmCrtc *DrmPipeline::crtc() const noexcept
{
    return m_pending.crtc;
}

std::shared_ptr<DrmConnectorMode> DrmPipeline::mode() const noexcept
{
    return m_pending.mode;
}

bool DrmPipeline::active() const noexcept
{
    return m_pending.active;
}

bool DrmPipeline::enabled() const noexcept
{
    return m_pending.enabled;
}

QList<DrmPipelineLayer *> DrmPipeline::layers() const
{
    return m_pending.layers;
}

PresentationMode DrmPipeline::presentationMode() const noexcept
{
    return m_pending.presentationMode;
}

uint32_t DrmPipeline::overscan() const noexcept
{
    return m_pending.overscan;
}

Output::RgbRange DrmPipeline::rgbRange() const noexcept
{
    return m_pending.rgbRange;
}

DrmConnector::DrmContentType DrmPipeline::contentType() const noexcept
{
    return m_pending.contentType;
}

const std::shared_ptr<IccProfile> &DrmPipeline::iccProfile() const noexcept
{
    return m_pending.iccProfile;
}

void DrmPipeline::setCrtc(DrmCrtc *crtc)
{
    m_pending.crtc = crtc;
}

void DrmPipeline::setMode(const std::shared_ptr<DrmConnectorMode> &mode)
{
    m_pending.mode = mode;
    if (mode) [[likely]] {
        m_pending.needsModeset = true;
    }
}

void DrmPipeline::setActive(bool active)
{
    m_pending.active = active;
}

void DrmPipeline::setEnable(bool enable)
{
    m_pending.enabled = enable;
}

void DrmPipeline::setLayers(const QList<DrmPipelineLayer *> &layers)
{
    m_pending.layers = layers;
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (layer) [[likely]] {
            layer->setOutput(m_output);
        }
    }
}

void DrmPipeline::setPresentationMode(PresentationMode mode)
{
    m_pending.presentationMode = mode;
}

void DrmPipeline::setOverscan(uint32_t overscan)
{
    m_pending.overscan = overscan;
}

void DrmPipeline::setRgbRange(Output::RgbRange range)
{
    m_pending.rgbRange = range;
}

void DrmPipeline::setCrtcColorPipeline(const ColorPipeline &pipeline)
{
    m_pending.crtcColorPipeline = pipeline;
}

void DrmPipeline::setHighDynamicRange(bool hdr)
{
    m_pending.hdr = hdr;
}

void DrmPipeline::setWideColorGamut(bool wcg)
{
    m_pending.wcg = wcg;
}

void DrmPipeline::setMaxBpc(uint32_t max)
{
    m_pending.maxBpc = max;
}

void DrmPipeline::setContentType(DrmConnector::DrmContentType type)
{
    m_pending.contentType = type;
}

void DrmPipeline::setIccProfile(const std::shared_ptr<IccProfile> &profile)
{
    m_pending.iccProfile = profile;
}

std::shared_ptr<DrmBlob> DrmPipeline::createHdrMetadata(TransferFunction::Type transferFunction) const
{
    if (transferFunction != TransferFunction::PerceptualQuantizer) [[likely]] {
        return nullptr;
    }
    const auto edid = m_connector->edid();
    if (!edid || !edid->supportsPQ()) [[unlikely]] {
        return nullptr;
    }

    const auto colorimetry = edid->nativeColorimetry().value_or(Colorimetry::BT709);
    const xyY red = colorimetry.red().toxyY();
    const xyY green = colorimetry.green().toxyY();
    const xyY blue = colorimetry.blue().toxyY();
    const xyY white = colorimetry.white().toxyY();

    auto to16Bit = [](float value) -> uint16_t {
        return static_cast<uint16_t>(std::lround(value / 0.00002f));
    };

    const auto desiredMax = edid->desiredMaxFrameAverageLuminance().value_or(0.0f);
    const auto desiredMin = edid->desiredMinLuminance();
    hdr_output_metadata data{
        .metadata_type = 0,
        .hdmi_metadata_type1 = hdr_metadata_infoframe{
            .eotf = static_cast<uint8_t>(2),
            .metadata_type = 0,
            .display_primaries = {
                {to16Bit(red.x), to16Bit(red.y)},
                {to16Bit(green.x), to16Bit(green.y)},
                {to16Bit(blue.x), to16Bit(blue.y)},
            },
            .white_point = {to16Bit(white.x), to16Bit(white.y)},
            .max_display_mastering_luminance = static_cast<uint16_t>(std::lround(desiredMax)),
            .min_display_mastering_luminance = static_cast<uint16_t>(std::lround(desiredMin * 10000.0f)),
            .max_cll = static_cast<uint16_t>(std::lround(desiredMax)),
            .max_fall = static_cast<uint16_t>(std::lround(desiredMax)),
        },
    };

    return DrmBlob::create(gpu(), &data, sizeof(data));
}

std::chrono::nanoseconds DrmPipeline::presentationDeadline() const noexcept
{
    return m_commitThread->safetyMargin();
}

}

#include "moc_drm_pipeline.cpp"
