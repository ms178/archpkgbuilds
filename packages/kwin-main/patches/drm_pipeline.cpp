/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_pipeline.h"

#include <errno.h>

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
#include "drm_plane.h"
#include "utils/drm_format_helper.h"
#include "utils/envvar.h"
#include "utils/kernel.h"

#include <drm_fourcc.h>
#include <gbm.h>

using namespace std::literals;

namespace KWin
{

static constexpr bool isYuvFormat(uint32_t format) noexcept
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

DrmPipeline::DrmPipeline(DrmConnector *conn)
    : m_connector(conn)
    , m_commitThread(std::make_unique<DrmCommitThread>(conn->gpu(), conn->connectorName()))
{
}

DrmPipeline::~DrmPipeline()
{
    m_commitThread.reset();
}

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
    if (gpu()->atomicModeSetting()) [[likely]] {

        if (auto err = testPresent(frame); err != Error::None) [[unlikely]] {
            return err;
        }

        auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});

        if (Error err = prepareAtomicPresentation(partialUpdate.get(), frame); err != Error::None) [[unlikely]] {
            return err;
        }

        for (const auto layer : layersToUpdate) {
            const auto pipelineLayer = static_cast<DrmPipelineLayer *>(layer);
            if (Error err = prepareAtomicPlane(partialUpdate.get(), pipelineLayer->plane(), pipelineLayer, frame); err != Error::None) [[unlikely]] {
                return err;
            }
        }

        if (layersToUpdate.isEmpty()) [[unlikely]] {
            if (m_pending.layers.isEmpty()) {
                return Error::InvalidArguments;
            }
            if (Error err = prepareAtomicPlane(partialUpdate.get(), m_pending.layers.front()->plane(), m_pending.layers.front(), frame); err != Error::None) {
                return err;
            }
        }

        if (m_pending.needsModesetProperties && !prepareAtomicModeset(partialUpdate.get())) [[unlikely]] {
            return Error::InvalidArguments;
        }

        m_next.needsModesetProperties = m_pending.needsModesetProperties = false;
        m_commitThread->addCommit(std::move(partialUpdate));
        return Error::None;
    } else {
        return presentLegacy(layersToUpdate, frame);
    }
}

void DrmPipeline::maybeModeset(const std::shared_ptr<OutputFrame> &frame)
{
    m_modesetPresentPending = true;
    gpu()->maybeModeset(this, frame);
}

DrmPipeline::Error DrmPipeline::commitPipelines(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects)
{
    Q_ASSERT(!pipelines.isEmpty());
    if (pipelines[0]->gpu()->atomicModeSetting()) [[likely]] {
        return commitPipelinesAtomic(pipelines, mode, nullptr, unusedObjects);
    } else {
        return commitPipelinesLegacy(pipelines, mode, unusedObjects);
    }
}

DrmPipeline::Error DrmPipeline::commitPipelinesAtomic(const QList<DrmPipeline *> &pipelines, CommitMode mode, const std::shared_ptr<OutputFrame> &frame, const QList<DrmObject *> &unusedObjects)
{
    auto commit = std::make_unique<DrmAtomicCommit>(pipelines);

    if (mode == CommitMode::Test) {
        if (std::ranges::any_of(pipelines, &DrmPipeline::needsModeset)) [[unlikely]] {
            mode = CommitMode::TestAllowModeset;
        }
    }

    for (DrmPipeline *pipeline : pipelines) {
        if (Error err = pipeline->prepareAtomicCommit(commit.get(), mode, frame); err != Error::None) {
            return err;
        }
    }

    for (DrmObject *unused : unusedObjects) {
        unused->disable(commit.get());
    }

    switch (mode) {
    case CommitMode::TestAllowModeset: {
        if (!commit->testAllowModeset()) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Atomic modeset test failed!" << strerror(errno);
            return errnoToError();
        }

        const bool withoutModeset = std::ranges::all_of(pipelines, [&frame](DrmPipeline *pipeline) {
            auto testCommit = std::make_unique<DrmAtomicCommit>(QVector<DrmPipeline *>{pipeline});
            return pipeline->prepareAtomicCommit(testCommit.get(), CommitMode::TestAllowModeset, frame) == Error::None && testCommit->test();
        });

        for (DrmPipeline *pipeline : pipelines) {
            pipeline->m_pending.needsModeset = !withoutModeset;
            pipeline->m_pending.needsModesetProperties = true;
        }
        return Error::None;
    }
    case CommitMode::CommitModeset: {
        if (!commit->commitModeset()) [[unlikely]] {
            qCCritical(KWIN_DRM) << "Atomic modeset commit failed!" << strerror(errno);
            return errnoToError();
        }
        for (const auto pipeline : pipelines) {
            pipeline->m_next.needsModeset = pipeline->m_pending.needsModeset = false;
        }
        commit->pageFlipped(std::chrono::steady_clock::now().time_since_epoch());
        return Error::None;
    }
    case CommitMode::Test: {
        if (!commit->test()) [[unlikely]] {
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
    if (activePending()) [[likely]] {
        if (Error err = prepareAtomicPresentation(commit, frame); err != Error::None) {
            return err;
        }
        for (const auto &layer : m_pending.layers) {
            if (Error err = prepareAtomicPlane(commit, layer->plane(), layer, frame); err != Error::None) {
                return err;
            }
        }
        if (mode == CommitMode::TestAllowModeset || mode == CommitMode::CommitModeset || m_pending.needsModesetProperties) [[unlikely]] {
            if (!prepareAtomicModeset(commit)) {
                return Error::InvalidArguments;
            }
        }
    } else {
        prepareAtomicDisable(commit);
    }
    return Error::None;
}

DrmPipeline::Error DrmPipeline::prepareAtomicPresentation(DrmAtomicCommit *commit, const std::shared_ptr<OutputFrame> &frame)
{
    commit->setPresentationMode(m_pending.presentationMode);

    if (m_connector->contentType.isValid()) [[likely]] {
        commit->addEnum(m_connector->contentType, m_pending.contentType);
    }

    if (m_pending.crtc->vrrEnabled.isValid()) [[likely]] {
        const bool shouldEnableVrr = (m_pending.presentationMode == PresentationMode::AdaptiveSync) |
                                     (m_pending.presentationMode == PresentationMode::AdaptiveAsync);
        commit->setVrr(m_pending.crtc, shouldEnableVrr);
    }

    if (m_pending.layers.empty()) [[unlikely]] {
        return Error::InvalidArguments;
    }

    const auto *firstLayer = m_pending.layers.front();
    const auto &firstColorPipeline = firstLayer->colorPipeline();
    const size_t layerCount = m_pending.layers.size();

    for (size_t i = 1; i < layerCount; ++i) {
        const auto *layer = m_pending.layers[i];

        if (i + 1 < layerCount) [[likely]] {
            __builtin_prefetch(m_pending.layers[i + 1], 0, 3);
        }

        if (!layer->isEnabled()) [[unlikely]] {
            continue;
        }

        if (layer->colorPipeline() != firstColorPipeline) [[unlikely]] {
            return Error::InvalidArguments;
        }
    }

    const ColorPipeline colorPipeline = firstColorPipeline.merged(m_pending.crtcColorPipeline);

    if (!m_pending.crtc->postBlendingPipeline) [[unlikely]] {
        if (!colorPipeline.isIdentity()) {
            return Error::InvalidArguments;
        }
    } else {
        if (!m_pending.crtc->postBlendingPipeline->matchPipeline(commit, colorPipeline)) [[unlikely]] {
            return Error::InvalidArguments;
        }
    }

    return Error::None;
}

DrmPipeline::Error DrmPipeline::prepareAtomicPlane(DrmAtomicCommit *commit, DrmPlane *plane, DrmPipelineLayer *layer, const std::shared_ptr<OutputFrame> &frame)
{
    if (!plane || !layer) [[unlikely]] {
        return Error::InvalidArguments;
    }

    if (!layer->isEnabled()) [[unlikely]] {
        plane->disable(commit);
        return Error::None;
    }

    const auto fb = layer->currentBuffer();
    if (!fb) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Enabled plane has no buffer!";
        return Error::TestBufferFailed;
    }

    const bool isCursor = (plane->type.enumValue() == DrmPlane::TypeIndex::Cursor);
    const bool hasVmHotspotX = plane->vmHotspotX.isValid();
    const bool hasVmHotspotY = plane->vmHotspotY.isValid();
    const bool hasAlpha = plane->alpha.isValid();
    const bool hasBlendMode = plane->pixelBlendMode.isValid();
    const bool hasZpos = plane->zpos.isValid() && !plane->zpos.isImmutable();
    const bool hasColorEncoding = plane->colorEncoding.isValid();
    const bool hasColorRange = plane->colorRange.isValid();

    const QRect srcRect = layer->sourceRect().toRect();
    const QRect dstRect = layer->targetRect();

    plane->set(commit, srcRect, dstRect);
    commit->addBuffer(plane, fb, frame);

    const auto transform = layer->offloadTransform();
    const bool isNormalTransform = (transform.kind() == OutputTransform::Kind::Normal);

    if (!isNormalTransform) [[unlikely]] {
        if (!plane->rotation.isValid() || !plane->supportsTransformation(transform)) [[unlikely]] {
            return Error::InvalidArguments;
        }
        commit->addEnum(plane->rotation, DrmPlane::outputTransformToPlaneTransform(transform));
    }

    commit->addProperty(plane->crtcId, m_pending.crtc->id());

    if (isCursor) [[unlikely]] {
        if (hasVmHotspotX & hasVmHotspotY) [[likely]] {
            const auto hotspot = layer->hotspot();
            commit->addProperty(plane->vmHotspotX, std::round(hotspot.x()));
            commit->addProperty(plane->vmHotspotY, std::round(hotspot.y()));
        }
        return Error::None;
    }

    if (hasAlpha) [[likely]] {
        commit->addProperty(plane->alpha, plane->alpha.maxValue());
    }
    if (hasBlendMode) [[likely]] {
        commit->addEnum(plane->pixelBlendMode, DrmPlane::PixelBlendMode::PreMultiplied);
    }
    if (hasZpos) [[likely]] {
        commit->addProperty(plane->zpos, layer->zpos());
    }

    const auto buffer = fb->buffer();
    const auto attrs = buffer->dmabufAttributes();

    if (!attrs) [[unlikely]] {
        return Error::None;
    }

    const uint32_t format = attrs->format;
    if (!isYuvFormat(format)) [[likely]] {
        return Error::None;
    }

    const auto &colorDesc = layer->colorDescription();
    if (!colorDesc) [[unlikely]] {
        return Error::None;
    }

    if (hasColorEncoding) [[likely]] {
        const auto coeffs = colorDesc->yuvCoefficients();

        if (coeffs == YUVMatrixCoefficients::BT709) [[likely]] {
            if (plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) {
                commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
            }
        } else if (coeffs == YUVMatrixCoefficients::BT601) {
            if (plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT601_YCbCr)) {
                commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT601_YCbCr);
            }
        } else if (coeffs == YUVMatrixCoefficients::BT2020) {
            if (plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT2020_YCbCr)) {
                commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT2020_YCbCr);
            }
        }
    }

    if (hasColorRange) [[likely]] {
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
    m_connector->disable(commit);
    if (m_pending.crtc) {
        m_pending.crtc->disable(commit);
        for (const auto layer : m_pending.layers) {
            if (DrmPlane *plane = layer->plane()) {
                plane->disable(commit);
            }
        }
    }
}

static const auto s_forceScalingMode = []() -> std::optional<DrmConnector::ScalingMode> {
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

bool DrmPipeline::prepareAtomicModeset(DrmAtomicCommit *commit)
{
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
        const auto underscanMode = m_pending.overscan != 0 ?
            DrmConnector::UnderscanOptions::On : DrmConnector::UnderscanOptions::Off;
        commit->addEnum(m_connector->underscan, underscanMode);
        commit->addProperty(m_connector->underscanVBorder, m_pending.overscan);
        commit->addProperty(m_connector->underscanHBorder, hborder);
    }

    if (m_connector->maxBpc.isValid()) [[likely]] {
        const uint32_t clampedBpc = std::clamp<uint32_t>(
            m_pending.maxBpc,
            m_connector->maxBpc.minValue(),
            m_connector->maxBpc.maxValue()
        );
        commit->addProperty(m_connector->maxBpc, clampedBpc);
    }

    if (m_connector->hdrMetadata.isValid()) [[likely]] {
        const auto transferFunc = m_pending.hdr ?
            TransferFunction::PerceptualQuantizer : TransferFunction::gamma22;
        commit->addBlob(m_connector->hdrMetadata, createHdrMetadata(transferFunc));
    } else if (m_pending.hdr) [[unlikely]] {
        return false;
    }

    if (m_pending.wcg) [[unlikely]] {
        if (!m_connector->colorspace.isValid() ||
            !m_connector->colorspace.hasEnum(DrmConnector::Colorspace::BT2020_RGB)) {
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
        } else if (m_connector->isInternal() &&
                   m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::Full_Aspect) &&
                   (m_pending.mode->flags() & OutputMode::Flag::Generated)) [[unlikely]] {
            commit->addEnum(m_connector->scalingMode, DrmConnector::ScalingMode::Full_Aspect);
        } else if (m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) [[likely]] {
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
    const auto size = m_pending.mode->size();
    const float aspectRatio = size.width() / static_cast<float>(size.height());
    uint32_t hborder = static_cast<uint32_t>(m_pending.overscan * aspectRatio);
    if (hborder > 128) {
        hborder = 128;
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
    if (needsModeset() || !m_pending.crtc || !m_pending.active) [[unlikely]] {
        return false;
    }

    if (gpu()->isVmwgfx()) [[unlikely]] {
        return false;
    }

    const auto drmLayer = static_cast<DrmPipelineLayer *>(layer);
    if (drmLayer->plane()) [[likely]] {
        const bool isCursorPlane = (drmLayer->plane()->type.enumValue() == DrmPlane::TypeIndex::Cursor);
        const bool canSkipTest = isCursorPlane &&
                                 !needsModeset() &&
                                 drmLayer->plane()->crtcId.value() == m_pending.crtc->id();

        if (!canSkipTest) [[unlikely]] {
            if (DrmPipeline::commitPipelinesAtomic({this}, CommitMode::Test, nullptr, {}) != Error::None) {
                return false;
            }
        }

        auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
        if (Error err = prepareAtomicPlane(partialUpdate.get(), drmLayer->plane(), drmLayer, nullptr); err != Error::None) [[unlikely]] {
            return false;
        }
        partialUpdate->setAllowedVrrDelay(allowedVrrDelay);
        m_commitThread->addCommit(std::move(partialUpdate));
        return true;
    } else {
        return setCursorLegacy(drmLayer);
    }
}

void DrmPipeline::applyPendingChanges()
{
    const bool layersChanged = m_next.layers != m_pending.layers;
    m_next = m_pending;
    m_commitThread->setModeInfo(m_pending.mode->refreshRate(), m_pending.mode->vblankTime());
    m_output->renderLoop()->setPresentationSafetyMargin(m_commitThread->safetyMargin());
    m_output->renderLoop()->setRefreshRate(m_pending.mode->refreshRate());
    if (layersChanged) [[unlikely]] {
        Q_EMIT m_output->outputLayersChanged();
    }
}

DrmConnector *DrmPipeline::connector() const
{
    return m_connector;
}

DrmGpu *DrmPipeline::gpu() const
{
    return m_connector->gpu();
}

void DrmPipeline::pageFlipped(std::chrono::nanoseconds timestamp)
{
    RenderLoopPrivate::get(m_output->renderLoop())->notifyVblank(timestamp);
    m_commitThread->pageFlipped(timestamp);
    m_output->renderLoop()->setPresentationSafetyMargin(m_commitThread->safetyMargin());
    if (gpu()->needsModeset()) [[unlikely]] {
        gpu()->maybeModeset(nullptr, nullptr);
    }
}

void DrmPipeline::setOutput(DrmOutput *output)
{
    m_output = output;
    for (const auto layer : m_pending.layers) {
        layer->setOutput(output);
    }
}

DrmOutput *DrmPipeline::output() const
{
    return m_output;
}

bool DrmPipeline::needsModeset() const
{
    return m_pending.needsModeset;
}

bool DrmPipeline::activePending() const
{
    return m_pending.crtc && m_pending.mode && m_pending.active;
}

void DrmPipeline::revertPendingChanges()
{
    m_pending = m_next;
}

DrmCommitThread *DrmPipeline::commitThread() const
{
    return m_commitThread.get();
}

bool DrmPipeline::modesetPresentPending() const
{
    return m_modesetPresentPending;
}

void DrmPipeline::resetModesetPresentPending()
{
    m_modesetPresentPending = false;
}

DrmCrtc *DrmPipeline::crtc() const
{
    return m_pending.crtc;
}

std::shared_ptr<DrmConnectorMode> DrmPipeline::mode() const
{
    return m_pending.mode;
}

bool DrmPipeline::active() const
{
    return m_pending.active;
}

bool DrmPipeline::enabled() const
{
    return m_pending.enabled;
}

QList<DrmPipelineLayer *> DrmPipeline::layers() const
{
    return m_pending.layers;
}

PresentationMode DrmPipeline::presentationMode() const
{
    return m_pending.presentationMode;
}

uint32_t DrmPipeline::overscan() const
{
    return m_pending.overscan;
}

Output::RgbRange DrmPipeline::rgbRange() const
{
    return m_pending.rgbRange;
}

DrmConnector::DrmContentType DrmPipeline::contentType() const
{
    return m_pending.contentType;
}

const std::shared_ptr<IccProfile> &DrmPipeline::iccProfile() const
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
    for (const auto layer : layers) {
        layer->setOutput(m_output);
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
    if (!m_connector->edid()->supportsPQ()) {
        return nullptr;
    }

    const auto colorimetry = m_connector->edid()->nativeColorimetry().value_or(Colorimetry::BT709);
    const xyY red = colorimetry.red().toxyY();
    const xyY green = colorimetry.green().toxyY();
    const xyY blue = colorimetry.blue().toxyY();
    const xyY white = colorimetry.white().toxyY();

    const auto to16Bit = [](float value) {
        return uint16_t(std::round(value / 0.00002));
    };

    hdr_output_metadata data{
        .metadata_type = 0,
        .hdmi_metadata_type1 = hdr_metadata_infoframe{
            .eotf = uint8_t(2),
            .metadata_type = 0,
            .display_primaries = {
                {to16Bit(red.x), to16Bit(red.y)},
                {to16Bit(green.x), to16Bit(green.y)},
                {to16Bit(blue.x), to16Bit(blue.y)},
            },
            .white_point = {to16Bit(white.x), to16Bit(white.y)},
            .max_display_mastering_luminance = uint16_t(std::round(m_connector->edid()->desiredMaxFrameAverageLuminance().value_or(0))),
            .min_display_mastering_luminance = uint16_t(std::round(m_connector->edid()->desiredMinLuminance() * 10000)),
            .max_cll = uint16_t(std::round(m_connector->edid()->desiredMaxFrameAverageLuminance().value_or(0))),
            .max_fall = uint16_t(std::round(m_connector->edid()->desiredMaxFrameAverageLuminance().value_or(0))),
        },
    };

    return DrmBlob::create(gpu(), &data, sizeof(data));
}

std::chrono::nanoseconds DrmPipeline::presentationDeadline() const
{
    return m_commitThread->safetyMargin();
}

}

#include "moc_drm_pipeline.cpp"
