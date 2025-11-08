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
} // namespace

DrmPipeline::DrmPipeline(DrmConnector *conn)
    : m_connector(conn)
    , m_commitThread(std::make_unique<DrmCommitThread>(conn->gpu(), conn->connectorName()))
{
}

DrmPipeline::~DrmPipeline() = default;

DrmPipeline::Error DrmPipeline::testPresent(const std::shared_ptr<OutputFrame> &frame)
{
    if (!gpu()->atomicModeSetting()) {
        return Error::None;
    }
    return DrmPipeline::commitPipelinesAtomic({this}, CommitMode::Test, frame, {});
}

DrmPipeline::Error DrmPipeline::present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame)
{
    Q_ASSERT(m_pending.crtc);

    if (!gpu()->atomicModeSetting()) {
        return presentLegacy(layersToUpdate, frame);
    }

    if (Error err = testPresent(frame); err != Error::None) {
        return err;
    }

    auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
    if (Error err = prepareAtomicPresentation(partialUpdate.get(), frame); err != Error::None) {
        return err;
    }

    bool updatedAnyLayer = false;
    for (const auto layer : layersToUpdate) {
        if (!layer) {
            continue;
        }
        auto *pipelineLayer = static_cast<DrmPipelineLayer *>(layer);
        if (Error err = prepareAtomicPlane(partialUpdate.get(), pipelineLayer->plane(), pipelineLayer, frame); err != Error::None) {
            return err;
        }
        updatedAnyLayer = true;
    }

    if (!updatedAnyLayer) {
        if (m_pending.layers.isEmpty()) {
            return Error::InvalidArguments;
        }
        auto *fallbackLayer = m_pending.layers.front();
        if (!fallbackLayer) {
            return Error::InvalidArguments;
        }
        if (Error err = prepareAtomicPlane(partialUpdate.get(), fallbackLayer->plane(), fallbackLayer, frame); err != Error::None) {
            return err;
        }
    }

    if (m_pending.needsModesetProperties && !prepareAtomicModeset(partialUpdate.get())) {
        return Error::InvalidArguments;
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
    if (pipelines.front()->gpu()->atomicModeSetting()) {
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

    if (mode == CommitMode::Test) {
        const bool anyModeset = std::any_of(pipelines.cbegin(), pipelines.cend(),
                                            [](const DrmPipeline *pipeline) {
                                                return pipeline->needsModeset();
                                            });
        if (anyModeset) {
            mode = CommitMode::TestAllowModeset;
        }
    }

    for (DrmPipeline *pipeline : pipelines) {
        if (Error err = pipeline->prepareAtomicCommit(commit.get(), mode, frame); err != Error::None) {
            return err;
        }
    }

    for (DrmObject *unused : unusedObjects) {
        if (unused) {
            unused->disable(commit.get());
        }
    }

    switch (mode) {
    case CommitMode::TestAllowModeset: {
        if (!commit->testAllowModeset()) {
            qCWarning(KWIN_DRM) << "Atomic modeset test failed!" << strerror(errno);
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
        if (!commit->commitModeset()) {
            qCCritical(KWIN_DRM) << "Atomic modeset commit failed!" << strerror(errno);
            return errnoToError();
        }
        for (DrmPipeline *pipeline : pipelines) {
            pipeline->m_next.needsModeset = pipeline->m_pending.needsModeset = false;
        }
        commit->pageFlipped(std::chrono::steady_clock::now().time_since_epoch());
        return Error::None;
    }
    case CommitMode::Test: {
        if (!commit->test()) {
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
    if (!commit) {
        return Error::InvalidArguments;
    }

    if (activePending()) {
        if (Error err = prepareAtomicPresentation(commit, frame); err != Error::None) {
            return err;
        }
        for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
            if (!layer) {
                return Error::InvalidArguments;
            }
            if (Error err = prepareAtomicPlane(commit, layer->plane(), layer, frame); err != Error::None) {
                return err;
            }
        }
        if (mode != CommitMode::Test && (mode == CommitMode::CommitModeset || m_pending.needsModesetProperties)) {
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
    if (!commit || !m_pending.crtc) {
        return Error::InvalidArguments;
    }

    commit->setPresentationMode(m_pending.presentationMode);

    if (m_connector->contentType.isValid()) {
        commit->addEnum(m_connector->contentType, m_pending.contentType);
    }

    if (m_pending.crtc->vrrEnabled.isValid()) {
        bool shouldEnableVrr = (m_pending.presentationMode == PresentationMode::AdaptiveSync)
            || (m_pending.presentationMode == PresentationMode::AdaptiveAsync);

        if (m_pending.needsModeset) {
            shouldEnableVrr = false;
        }
        commit->setVrr(m_pending.crtc, shouldEnableVrr);
    }

    if (m_pending.layers.isEmpty()) {
        return Error::InvalidArguments;
    }

    DrmPipelineLayer *referenceLayer = nullptr;
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (!layer) {
            return Error::InvalidArguments;
        }
        if (!referenceLayer && layer->isEnabled()) {
            referenceLayer = layer;
        }
    }

    if (!referenceLayer) {
        return Error::InvalidArguments;
    }

    const ColorPipeline &referencePipeline = referenceLayer->colorPipeline();
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (!layer || !layer->isEnabled()) {
            continue;
        }
        if (layer->colorPipeline() != referencePipeline) {
            return Error::InvalidArguments;
        }
    }

    const ColorPipeline mergedPipeline = referencePipeline.merged(m_pending.crtcColorPipeline);
    if (!m_pending.crtc->postBlendingPipeline) {
        if (!mergedPipeline.isIdentity()) {
            return Error::InvalidArguments;
        }
    } else {
        if (!m_pending.crtc->postBlendingPipeline->matchPipeline(commit, mergedPipeline)) {
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
    if (!commit || !plane || !layer) {
        return Error::InvalidArguments;
    }

    if (!layer->isEnabled()) {
        plane->disable(commit);
        return Error::None;
    }

    const auto fb = layer->currentBuffer();
    if (!fb) {
        qCWarning(KWIN_DRM) << "Enabled plane has no buffer";
        return Error::TestBufferFailed;
    }

    plane->set(commit, layer->sourceRect().toRect(), layer->targetRect());
    commit->addBuffer(plane, fb, frame);
    commit->addProperty(plane->crtcId, m_pending.crtc->id());

    const bool isCursor = (plane->type.enumValue() == DrmPlane::TypeIndex::Cursor);
    if (isCursor) {
        if (plane->vmHotspotX.isValid() && plane->vmHotspotY.isValid()) {
            const QPointF hotspot = layer->hotspot();
            commit->addProperty(plane->vmHotspotX, std::lround(hotspot.x()));
            commit->addProperty(plane->vmHotspotY, std::lround(hotspot.y()));
        }
        return Error::None;
    }

    if (plane->rotation.isValid()) {
        const OutputTransform transform = layer->offloadTransform();
        if (!plane->supportsTransformation(transform)) {
            return Error::InvalidArguments;
        }
        if (transform.kind() != OutputTransform::Kind::Normal) {
            commit->addEnum(plane->rotation, DrmPlane::outputTransformToPlaneTransform(transform));
        }
    }

    if (plane->alpha.isValid()) {
        commit->addProperty(plane->alpha, plane->alpha.maxValue());
    }
    if (plane->pixelBlendMode.isValid()) {
        if (plane->pixelBlendMode.hasEnum(DrmPlane::PixelBlendMode::PreMultiplied)) {
            commit->addEnum(plane->pixelBlendMode, DrmPlane::PixelBlendMode::PreMultiplied);
        }
    }
    if (plane->zpos.isValid() && !plane->zpos.isImmutable()) {
        commit->addProperty(plane->zpos, layer->zpos());
    }

    const auto buffer = fb->buffer();
    const auto *attrs = buffer ? buffer->dmabufAttributes() : nullptr;
    if (!attrs) {
        return Error::None;
    }

    if (!isYuvFormat(attrs->format)) {
        return Error::None;
    }

    const auto colorDesc = layer->colorDescription();
    if (!colorDesc) {
        return Error::None;
    }

    if (plane->colorEncoding.isValid()) {
        const auto matrix = colorDesc->yuvCoefficients();
        if (matrix == YUVMatrixCoefficients::BT709 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
        } else if (matrix == YUVMatrixCoefficients::BT601 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT601_YCbCr)) {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT601_YCbCr);
        } else if (matrix == YUVMatrixCoefficients::BT2020 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT2020_YCbCr)) {
            commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT2020_YCbCr);
        }
    }

    if (plane->colorRange.isValid()) {
        const auto range = (colorDesc->range() == EncodingRange::Full)
            ? DrmPlane::ColorRange::Full_YCbCr
            : DrmPlane::ColorRange::Limited_YCbCr;
        if (plane->colorRange.hasEnum(range)) {
            commit->addEnum(plane->colorRange, range);
        }
    }

    return Error::None;
}

void DrmPipeline::prepareAtomicDisable(DrmAtomicCommit *commit)
{
    if (!commit) {
        return;
    }

    m_connector->disable(commit);
    if (!m_pending.crtc) {
        return;
    }

    m_pending.crtc->disable(commit);
    for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
        if (layer && layer->plane()) {
            layer->plane()->disable(commit);
        }
    }
}

bool DrmPipeline::prepareAtomicModeset(DrmAtomicCommit *commit)
{
    if (!commit || !m_pending.crtc || !m_pending.mode) {
        return false;
    }

    commit->addProperty(m_connector->crtcId, m_pending.crtc->id());

    if (m_connector->broadcastRGB.isValid()) {
        commit->addEnum(m_connector->broadcastRGB, DrmConnector::rgbRangeToBroadcastRgb(m_pending.rgbRange));
    }
    if (m_connector->linkStatus.isValid()) {
        commit->addEnum(m_connector->linkStatus, DrmConnector::LinkStatus::Good);
    }

    if (m_connector->overscan.isValid()) {
        commit->addProperty(m_connector->overscan, m_pending.overscan);
    } else if (m_connector->underscan.isValid()) {
        const uint32_t hborder = calculateUnderscan();
        const auto underscanMode = m_pending.overscan != 0
            ? DrmConnector::UnderscanOptions::On
            : DrmConnector::UnderscanOptions::Off;
        commit->addEnum(m_connector->underscan, underscanMode);
        commit->addProperty(m_connector->underscanVBorder, m_pending.overscan);
        commit->addProperty(m_connector->underscanHBorder, hborder);
    }

    if (m_connector->maxBpc.isValid()) {
        const uint32_t clampedBpc = std::clamp<uint32_t>(m_pending.maxBpc,
                                                         m_connector->maxBpc.minValue(),
                                                         m_connector->maxBpc.maxValue());
        commit->addProperty(m_connector->maxBpc, clampedBpc);
    }

    if (m_connector->hdrMetadata.isValid()) {
        const auto tf = m_pending.hdr ? TransferFunction::PerceptualQuantizer : TransferFunction::gamma22;
        commit->addBlob(m_connector->hdrMetadata, createHdrMetadata(tf));
    } else if (m_pending.hdr) {
        return false;
    }

    if (m_pending.wcg) {
        if (!m_connector->colorspace.isValid() || !m_connector->colorspace.hasEnum(DrmConnector::Colorspace::BT2020_RGB)) {
            return false;
        }
        commit->addEnum(m_connector->colorspace, DrmConnector::Colorspace::BT2020_RGB);
    } else if (m_connector->colorspace.isValid()) {
        commit->addEnum(m_connector->colorspace, DrmConnector::Colorspace::Default);
    }

    if (m_connector->scalingMode.isValid()) {
        if (s_forceScalingMode.has_value()) {
            if (m_connector->scalingMode.hasEnum(*s_forceScalingMode)) {
                commit->addEnum(m_connector->scalingMode, *s_forceScalingMode);
            } else if (m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) {
                commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::None);
            }
        } else if (m_connector->isInternal()
                   && m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::Full_Aspect)
                   && (m_pending.mode->flags() & OutputMode::Flag::Generated)) {
            commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::Full_Aspect);
        } else if (m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) {
            commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::None);
        }
    }

    commit->addProperty(m_pending.crtc->active, 1);
    commit->addBlob(m_pending.crtc->modeId, m_pending.mode->blob());

    if (m_pending.crtc->degammaLut.isValid()) {
        commit->addProperty(m_pending.crtc->degammaLut, 0);
    }

    return true;
}

uint32_t DrmPipeline::calculateUnderscan()
{
    if (!m_pending.mode) {
        return 0;
    }
    const QSize size = m_pending.mode->size();
    if (size.height() <= 0) {
        return 0;
    }
    const float aspectRatio = size.width() / static_cast<float>(size.height());
    uint32_t hborder = static_cast<uint32_t>(std::lround(m_pending.overscan * aspectRatio));
    if (hborder > 128u) {
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
    if (!layer || needsModeset() || !m_pending.crtc || !m_pending.active) {
        return false;
    }

    if (gpu()->isVmwgfx()) {
        return false;
    }

    auto *drmLayer = static_cast<DrmPipelineLayer *>(layer);
    DrmPlane *plane = drmLayer ? drmLayer->plane() : nullptr;

    if (!plane) {
        return setCursorLegacy(drmLayer);
    }

    const bool isCursorPlane = plane->type.enumValue() == DrmPlane::TypeIndex::Cursor;
    const bool canSkipTest = isCursorPlane && !needsModeset() && plane->crtcId.value() == m_pending.crtc->id();

    if (!canSkipTest) {
        if (DrmPipeline::commitPipelinesAtomic({this}, CommitMode::Test, nullptr, {}) != Error::None) {
            return false;
        }
    }

    auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
    if (prepareAtomicPlane(partialUpdate.get(), plane, drmLayer, nullptr) != Error::None) {
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

    if (m_pending.mode) {
        m_commitThread->setModeInfo(m_pending.mode->refreshRate(), m_pending.mode->vblankTime());
    }

    if (m_output) {
        if (auto *loop = m_output->renderLoop()) {
            loop->setPresentationSafetyMargin(m_commitThread->safetyMargin());
            if (m_pending.mode) {
                loop->setRefreshRate(m_pending.mode->refreshRate());
            }
        }
        if (layersChanged) {
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
    if (m_output && m_output->renderLoop()) {
        RenderLoopPrivate::get(m_output->renderLoop())->notifyVblank(timestamp);
        m_output->renderLoop()->setPresentationSafetyMargin(m_commitThread->safetyMargin());
    }
    m_commitThread->pageFlipped(timestamp);
    if (gpu()->needsModeset()) {
        gpu()->maybeModeset(nullptr, nullptr);
    }
}

void DrmPipeline::setOutput(DrmOutput *output)
{
    m_output = output;
    for (DrmPipelineLayer *layer : m_pending.layers) {
        if (layer) {
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
    if (mode) {
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
        if (layer) {
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
    if (transferFunction != TransferFunction::PerceptualQuantizer) {
        return nullptr;
    }
    const auto edid = m_connector->edid();
    if (!edid || !edid->supportsPQ()) {
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

} // namespace KWin

#include "moc_drm_pipeline.cpp"
