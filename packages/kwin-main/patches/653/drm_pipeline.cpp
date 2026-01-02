/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>

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

#include <QCoreApplication>
#include <QMetaObject>
#include <QThread>

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

[[nodiscard]] inline bool isMainThread() noexcept
{
    if (auto *app = QCoreApplication::instance()) [[likely]] {
        return QThread::currentThread() == app->thread();
    }
    return true;
}

template<typename Func>
inline void invokeOnMainThreadBlocking(Func &&func)
{
    auto *app = QCoreApplication::instance();
    if (!app || isMainThread()) {
        std::forward<Func>(func)();
        return;
    }
    QMetaObject::invokeMethod(app, std::forward<Func>(func), Qt::BlockingQueuedConnection);
}

[[nodiscard, gnu::const]]
constexpr bool isYuvFormat(uint32_t format) noexcept
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

[[nodiscard, gnu::const]]
constexpr bool isAsyncMode(PresentationMode mode) noexcept
{
    return mode == PresentationMode::Async || mode == PresentationMode::AdaptiveAsync;
}

[[nodiscard, gnu::const]]
constexpr bool isAdaptiveMode(PresentationMode mode) noexcept
{
    return mode == PresentationMode::AdaptiveSync || mode == PresentationMode::AdaptiveAsync;
}

[[nodiscard, gnu::always_inline]]
inline bool validateSourceRect(const QRect &rect) noexcept
{
    return rect.x() >= 0 && rect.y() >= 0 && rect.width() > 0 && rect.height() > 0;
}

[[nodiscard, gnu::always_inline]]
inline bool validateTargetRect(const QRect &rect) noexcept
{
    return rect.width() > 0 && rect.height() > 0;
}

[[nodiscard, gnu::always_inline]]
inline QRect sanitizeSourceRect(const QRectF &srcF) noexcept
{
    const int x = (srcF.x() < 0.0) ? 0 : static_cast<int>(srcF.x());
    const int y = (srcF.y() < 0.0) ? 0 : static_cast<int>(srcF.y());
    const int w = (srcF.width() < 1.0) ? 1 : static_cast<int>(std::ceil(srcF.width()));
    const int h = (srcF.height() < 1.0) ? 1 : static_cast<int>(std::ceil(srcF.height()));
    return QRect(x, y, w, h);
}

const auto s_forceScalingMode = []() -> std::optional<DrmConnector::ScalingMode> {
    const auto env = qEnvironmentVariable("KWIN_DRM_FORCE_SCALING_MODE");
    if (env == QLatin1String("NONE")) {
        return DrmConnector::ScalingMode::None;
    } else if (env == QLatin1String("FULL")) {
        return DrmConnector::ScalingMode::Full;
    } else if (env == QLatin1String("CENTER")) {
        return DrmConnector::ScalingMode::Center;
    } else if (env == QLatin1String("FULL_ASPECT")) {
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
    return commitPipelinesAtomic({this}, CommitMode::Test, frame, {});
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

    if (const Error err = testPresent(frame); err != Error::None) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Test present failed, error:" << static_cast<int>(err);
        return err;
    }

    auto partialUpdate = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{this});
    if (const Error err = prepareAtomicPresentation(partialUpdate.get(), frame); err != Error::None) [[unlikely]] {
        qCWarning(KWIN_DRM) << "prepareAtomicPresentation failed:" << static_cast<int>(err);
        return err;
    }

    int layerCount = 0;
    for (OutputLayer *layer : layersToUpdate) {
        if (!layer) [[unlikely]] {
            continue;
        }
        auto *pipelineLayer = static_cast<DrmPipelineLayer *>(layer);
        DrmPlane *plane = pipelineLayer->plane();
        if (!plane) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Layer has no plane";
            return Error::InvalidArguments;
        }
        if (const Error err = prepareAtomicPlane(partialUpdate.get(), plane, pipelineLayer, frame); err != Error::None) [[unlikely]] {
            qCWarning(KWIN_DRM) << "prepareAtomicPlane failed for layer:" << static_cast<int>(err);
            return err;
        }
        ++layerCount;
    }

    if (partialUpdate->isTearing() && layerCount > 1) {
        partialUpdate->setPresentationMode(PresentationMode::VSync);
    }

    if (layerCount == 0) [[unlikely]] {
        if (m_pending.layers.isEmpty()) {
            qCWarning(KWIN_DRM) << "No layers to present";
            return Error::InvalidArguments;
        }
        DrmPipelineLayer *fallbackLayer = m_pending.layers.constFirst();
        if (!fallbackLayer) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Fallback layer is null";
            return Error::InvalidArguments;
        }
        DrmPlane *fallbackPlane = fallbackLayer->plane();
        if (!fallbackPlane) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Fallback plane is null";
            return Error::InvalidArguments;
        }
        if (const Error err = prepareAtomicPlane(partialUpdate.get(), fallbackPlane, fallbackLayer, frame); err != Error::None) [[unlikely]] {
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
    if (pipelines.constFirst()->gpu()->atomicModeSetting()) [[likely]] {
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
        bool anyModeset = false;
        for (const DrmPipeline *pipeline : pipelines) {
            if (pipeline->needsModeset()) {
                anyModeset = true;
                break;
            }
        }
        if (anyModeset) [[unlikely]] {
            mode = CommitMode::TestAllowModeset;
        }
    }

    for (DrmPipeline *pipeline : pipelines) {
        if (const Error err = pipeline->prepareAtomicCommit(commit.get(), mode, frame); err != Error::None) [[unlikely]] {
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
            const int savedErrno = errno;
            qCWarning(KWIN_DRM) << "Atomic modeset test failed! errno:" << savedErrno << strerror(savedErrno);
            return errnoToError();
        }

        bool withoutModeset = true;
        for (DrmPipeline *pipeline : pipelines) {
            auto testCommit = std::make_unique<DrmAtomicCommit>(QList<DrmPipeline *>{pipeline});
            const bool prepareOk = pipeline->prepareAtomicCommit(testCommit.get(), CommitMode::TestAllowModeset, frame) == Error::None;
            if (!prepareOk || !testCommit->test()) {
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
            const int savedErrno = errno;
            qCCritical(KWIN_DRM) << "Atomic modeset commit failed! errno:" << savedErrno << strerror(savedErrno);
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
            const int savedErrno = errno;
            qCDebug(KWIN_DRM) << "Atomic test failed! errno:" << savedErrno << strerror(savedErrno);
            return errnoToError();
        }
        return Error::None;
    }
    }
    Q_UNREACHABLE();
}

DrmPipeline::Error DrmPipeline::prepareAtomicCommit(DrmAtomicCommit *commit, CommitMode mode, const std::shared_ptr<OutputFrame> &frame)
{
    if (!commit) [[unlikely]] {
        return Error::InvalidArguments;
    }

    if (activePending()) [[likely]] {
        if (const Error err = prepareAtomicPresentation(commit, frame); err != Error::None) [[unlikely]] {
            return err;
        }
        for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
            if (!layer) [[unlikely]] {
                return Error::InvalidArguments;
            }
            DrmPlane *plane = layer->plane();
            if (!plane) [[unlikely]] {
                return Error::InvalidArguments;
            }
            if (const Error err = prepareAtomicPlane(commit, plane, layer, frame); err != Error::None) [[unlikely]] {
                return err;
            }
        }
        const bool needsModesetCommit = (mode != CommitMode::Test) &&
                                        (mode == CommitMode::CommitModeset || m_pending.needsModesetProperties);
        if (needsModesetCommit) [[unlikely]] {
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

    const bool isAsync = isAsyncMode(m_pending.presentationMode);
    commit->setPresentationMode(m_pending.presentationMode);

    if (m_connector->contentType.isValid()) {
        commit->addEnum(m_connector->contentType, m_pending.contentType);
    }

    if (m_pending.crtc->vrrEnabled.isValid() && !isAsync) {
        const bool shouldEnableVrr = isAdaptiveMode(m_pending.presentationMode) && !m_pending.needsModeset;
        commit->setVrr(m_pending.crtc, shouldEnableVrr);
    }

    if (m_pending.layers.isEmpty()) [[unlikely]] {
        return Error::InvalidArguments;
    }

    DrmPipelineLayer *referenceLayer = nullptr;
    int enabledCount = 0;
    bool pipelinesMatch = true;
    const ColorPipeline *firstPipeline = nullptr;

    for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
        if (!layer) [[unlikely]] {
            return Error::InvalidArguments;
        }
        if (!layer->isEnabled()) {
            continue;
        }
        if (!referenceLayer) {
            referenceLayer = layer;
            firstPipeline = &layer->colorPipeline();
        } else if (pipelinesMatch && firstPipeline) {
            if (layer->colorPipeline() != *firstPipeline) {
                pipelinesMatch = false;
            }
        }
        ++enabledCount;
    }

    if (!referenceLayer) [[unlikely]] {
        return Error::InvalidArguments;
    }

    if (enabledCount > 1 && !pipelinesMatch) [[unlikely]] {
        return Error::InvalidArguments;
    }

    const ColorPipeline mergedPipeline = referenceLayer->colorPipeline().merged(m_pending.crtcColorPipeline);
    if (!m_pending.crtc->postBlendingPipeline) {
        if (!mergedPipeline.isIdentity()) [[unlikely]] {
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

    if (!layer->isEnabled()) {
        plane->disable(commit);
        return Error::None;
    }

    const auto fb = layer->currentBuffer();
    if (!fb) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Enabled plane has no buffer";
        return Error::TestBufferFailed;
    }

    const bool isCursor = plane->type.isValid() && plane->type.enumValue() == DrmPlane::TypeIndex::Cursor;
    if (isCursor) {
        return prepareAtomicPlaneCursor(commit, plane, layer, fb, frame);
    }
    return prepareAtomicPlanePrimary(commit, plane, layer, fb, frame);
}

DrmPipeline::Error DrmPipeline::prepareAtomicPlaneCursor(DrmAtomicCommit *commit,
                                                         DrmPlane *plane,
                                                         DrmPipelineLayer *layer,
                                                         const std::shared_ptr<DrmFramebuffer> &fb,
                                                         const std::shared_ptr<OutputFrame> &frame)
{
    if (!commit || !plane || !layer || !fb || !m_pending.crtc) [[unlikely]] {
        return Error::InvalidArguments;
    }

    const QRect sourceRect = sanitizeSourceRect(layer->sourceRect());
    const QRect targetRect = layer->targetRect();

    if (!validateSourceRect(sourceRect)) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid cursor source geometry:" << sourceRect
                           << "from" << layer->sourceRect();
        return Error::InvalidArguments;
    }

    if (!validateTargetRect(targetRect)) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid cursor target geometry:" << targetRect;
        return Error::InvalidArguments;
    }

    plane->set(commit, sourceRect, targetRect);
    commit->addBuffer(plane, fb, frame);
    commit->addProperty(plane->crtcId, m_pending.crtc->id());

    if (plane->vmHotspotX.isValid() && plane->vmHotspotY.isValid()) {
        const QPointF hotspot = layer->hotspot();
        const double hx = hotspot.x();
        const double hy = hotspot.y();
        const uint64_t hxClamped = (hx < 0.0) ? 0 : ((hx > 32767.0) ? 32767 : static_cast<uint64_t>(std::lround(hx)));
        const uint64_t hyClamped = (hy < 0.0) ? 0 : ((hy > 32767.0) ? 32767 : static_cast<uint64_t>(std::lround(hy)));
        commit->addProperty(plane->vmHotspotX, hxClamped);
        commit->addProperty(plane->vmHotspotY, hyClamped);
    }

    if (plane->rotation.isValid()) {
        commit->addEnum(plane->rotation, DrmPlane::Transformations{DrmPlane::Transformation::Rotate0});
    }

    return Error::None;
}

DrmPipeline::Error DrmPipeline::prepareAtomicPlanePrimary(DrmAtomicCommit *commit,
                                                          DrmPlane *plane,
                                                          DrmPipelineLayer *layer,
                                                          const std::shared_ptr<DrmFramebuffer> &fb,
                                                          const std::shared_ptr<OutputFrame> &frame)
{
    if (!commit || !plane || !layer || !fb || !m_pending.crtc) [[unlikely]] {
        return Error::InvalidArguments;
    }

    const QRect sourceRect = sanitizeSourceRect(layer->sourceRect());
    const QRect targetRect = layer->targetRect();

    if (!validateSourceRect(sourceRect)) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid primary source geometry:" << sourceRect
                           << "from" << layer->sourceRect();
        return Error::InvalidArguments;
    }

    if (!validateTargetRect(targetRect)) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Invalid primary target geometry:" << targetRect;
        return Error::InvalidArguments;
    }

    plane->set(commit, sourceRect, targetRect);
    commit->addBuffer(plane, fb, frame);
    commit->addProperty(plane->crtcId, m_pending.crtc->id());

    if (plane->rotation.isValid()) {
        const OutputTransform transform = layer->offloadTransform();
        const DrmPlane::Transformations planeTransform = DrmPlane::outputTransformToPlaneTransform(transform);
        if (!plane->supportsTransformation(transform)) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Unsupported transform:" << static_cast<int>(transform.kind());
            return Error::InvalidArguments;
        }
        commit->addEnum(plane->rotation, planeTransform);
    }

    if (plane->alpha.isValid()) {
        commit->addProperty(plane->alpha, plane->alpha.maxValue());
    }

    if (plane->pixelBlendMode.isValid()) {
        if (plane->pixelBlendMode.hasEnum(DrmPlane::PixelBlendMode::PreMultiplied)) {
            commit->addEnum(plane->pixelBlendMode, DrmPlane::PixelBlendMode::PreMultiplied);
        } else if (plane->pixelBlendMode.hasEnum(DrmPlane::PixelBlendMode::None)) {
            commit->addEnum(plane->pixelBlendMode, DrmPlane::PixelBlendMode::None);
        }
    }

    if (plane->zpos.isValid() && !plane->zpos.isImmutable()) {
        commit->addProperty(plane->zpos, layer->zpos());
    }

    const auto buffer = fb->buffer();
    if (!buffer) {
        return Error::None;
    }

    const auto *attrs = buffer->dmabufAttributes();
    if (!attrs) {
        return Error::None;
    }

    const bool isYuv = isYuvFormat(attrs->format);

    if (plane->colorEncoding.isValid()) {
        if (isYuv) {
            const auto colorDesc = layer->colorDescription();
            if (colorDesc) {
                const auto matrix = colorDesc->yuvCoefficients();
                if (matrix == YUVMatrixCoefficients::BT709 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) {
                    commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
                } else if (matrix == YUVMatrixCoefficients::BT601 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT601_YCbCr)) {
                    commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT601_YCbCr);
                } else if (matrix == YUVMatrixCoefficients::BT2020 && plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT2020_YCbCr)) {
                    commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT2020_YCbCr);
                } else if (plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) {
                    commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
                }
            } else if (plane->colorEncoding.hasEnum(DrmPlane::ColorEncoding::BT709_YCbCr)) {
                commit->addEnum(plane->colorEncoding, DrmPlane::ColorEncoding::BT709_YCbCr);
            }
        }
    }

    if (plane->colorRange.isValid()) {
        if (isYuv) {
            const auto colorDesc = layer->colorDescription();
            DrmPlane::ColorRange range = DrmPlane::ColorRange::Limited_YCbCr;
            if (colorDesc && colorDesc->range() == EncodingRange::Full) {
                range = DrmPlane::ColorRange::Full_YCbCr;
            }
            if (plane->colorRange.hasEnum(range)) {
                commit->addEnum(plane->colorRange, range);
            } else if (plane->colorRange.hasEnum(DrmPlane::ColorRange::Limited_YCbCr)) {
                commit->addEnum(plane->colorRange, DrmPlane::ColorRange::Limited_YCbCr);
            }
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

    if (m_pending.crtc) [[likely]] {
        m_pending.crtc->disable(commit);

        for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
            if (layer) [[likely]] {
                DrmPlane *plane = layer->plane();
                if (plane) [[likely]] {
                    plane->disable(commit);
                }
            }
        }
    }
}

bool DrmPipeline::prepareAtomicModeset(DrmAtomicCommit *commit)
{
    if (!commit || !m_pending.crtc || !m_pending.mode) [[unlikely]] {
        return false;
    }

    commit->addProperty(m_connector->crtcId, m_pending.crtc->id());

    if (m_pending.crtc->modeId.isValid()) {
        auto modeBlob = DrmBlob::create(gpu(), m_pending.mode->nativeMode(), sizeof(drmModeModeInfo));
        if (!modeBlob) [[unlikely]] {
            return false;
        }
        commit->addBlob(m_pending.crtc->modeId, modeBlob);
    }

    if (m_pending.crtc->active.isValid()) {
        commit->addProperty(m_pending.crtc->active, m_pending.active ? 1 : 0);
    }

    if (m_connector->underscan.isValid()) {
        const bool useUnderscan = m_connector->underscan.hasEnum(DrmConnector::UnderscanOptions::On) && m_pending.overscan > 0;
        commit->addEnum(m_connector->underscan, useUnderscan ? DrmConnector::UnderscanOptions::On : DrmConnector::UnderscanOptions::Off);

        if (useUnderscan) {
            if (m_connector->underscanVBorder.isValid()) {
                commit->addProperty(m_connector->underscanVBorder, m_pending.overscan);
            }
            if (m_connector->underscanHBorder.isValid()) {
                commit->addProperty(m_connector->underscanHBorder, calculateUnderscan());
            }
        }
    }

    if (m_connector->broadcastRGB.isValid()) {
        DrmConnector::BroadcastRgbOptions broadcastOption;
        switch (m_pending.rgbRange) {
        case Output::RgbRange::Automatic:
            broadcastOption = DrmConnector::BroadcastRgbOptions::Automatic;
            break;
        case Output::RgbRange::Full:
            broadcastOption = DrmConnector::BroadcastRgbOptions::Full;
            break;
        case Output::RgbRange::Limited:
            broadcastOption = DrmConnector::BroadcastRgbOptions::Limited;
            break;
        }
        commit->addEnum(m_connector->broadcastRGB, broadcastOption);
    }

    if (m_connector->scalingMode.isValid()) {
        const auto scalingMode = s_forceScalingMode.value_or(DrmConnector::ScalingMode::None);
        if (m_connector->scalingMode.hasEnum(scalingMode)) {
            commit->addEnum(m_connector->scalingMode, scalingMode);
        }
    }

    if (m_connector->maxBpc.isValid()) {
        commit->addProperty(m_connector->maxBpc, m_pending.maxBpc);
    }

    if (m_connector->hdrMetadata.isValid() && m_pending.hdr) {
        TransferFunction::Type tf = TransferFunction::PerceptualQuantizer;
        commit->addBlob(m_connector->hdrMetadata, createHdrMetadata(tf));
    } else if (m_connector->hdrMetadata.isValid()) {
        commit->addBlob(m_connector->hdrMetadata, nullptr);
    }

    return true;
}

uint32_t DrmPipeline::calculateUnderscan() noexcept
{
    if (!m_pending.mode) [[unlikely]] {
        return 0;
    }

    const QSize size = m_pending.mode->size();
    const int height = size.height();
    if (height <= 0) [[unlikely]] {
        return 0;
    }

    const int width = size.width();
    const float aspectRatio = static_cast<float>(width) / static_cast<float>(height);
    float hborderF = static_cast<float>(m_pending.overscan) * aspectRatio;

    if (hborderF > 128.0f) [[unlikely]] {
        hborderF = 128.0f;
        m_pending.overscan = static_cast<uint32_t>(std::lround(128.0f / aspectRatio));
    }

    return static_cast<uint32_t>(std::lround(hborderF));
}

DrmPipeline::Error DrmPipeline::errnoToError() noexcept
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
    if (!drmLayer) [[unlikely]] {
        return false;
    }

    DrmPlane *plane = drmLayer->plane();
    if (!plane) [[unlikely]] {
        return setCursorLegacy(drmLayer);
    }

    const bool isCursor = plane->type.isValid() && plane->type.enumValue() == DrmPlane::TypeIndex::Cursor;
    if (!isCursor) {
        if (commitPipelinesAtomic({this}, CommitMode::Test, nullptr, {}) != Error::None) [[unlikely]] {
            qCDebug(KWIN_DRM) << "presentAsync: atomic test failed, aborting";
            return false;
        }
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
        RenderLoop *loop = m_output->renderLoop();
        if (loop) [[likely]] {
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
    invokeOnMainThreadBlocking([this, timestamp] {
        if (m_output) [[likely]] {
            RenderLoop *loop = m_output->renderLoop();
            if (loop) [[likely]] {
                const int64_t nowNs = std::chrono::duration_cast<std::chrono::nanoseconds>(
                    std::chrono::steady_clock::now().time_since_epoch()).count();
                RenderLoopPrivate::get(loop)->notifyVblank(timestamp, nowNs);
                loop->setPresentationSafetyMargin(m_commitThread->safetyMargin());
            }
        }
        m_commitThread->pageFlipped(timestamp);
        if (gpu()->needsModeset()) [[unlikely]] {
            gpu()->maybeModeset(nullptr, nullptr);
        }
    });
}

void DrmPipeline::setOutput(DrmOutput *output)
{
    m_output = output;
    for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
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

void DrmPipeline::resetModesetPresentPending() noexcept
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
    for (DrmPipelineLayer *layer : std::as_const(m_pending.layers)) {
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

    auto to16Bit = [](float value) noexcept -> uint16_t {
        const float scaled = value / 0.00002f;
        const float clamped = (scaled < 0.0f) ? 0.0f : ((scaled > 65535.0f) ? 65535.0f : scaled);
        return static_cast<uint16_t>(std::lround(clamped));
    };

    const float desiredMax = edid->desiredMaxFrameAverageLuminance().value_or(0.0f);
    const float desiredMin = edid->desiredMinLuminance();

    const uint16_t maxLum = static_cast<uint16_t>(std::lround((desiredMax < 0.0f) ? 0.0f : ((desiredMax > 65535.0f) ? 65535.0f : desiredMax)));
    const float minScaled = desiredMin * 10000.0f;
    const uint16_t minLum = static_cast<uint16_t>(std::lround((minScaled < 0.0f) ? 0.0f : ((minScaled > 65535.0f) ? 65535.0f : minScaled)));

    hdr_output_metadata data{
        .metadata_type = 0,
        .hdmi_metadata_type1 = hdr_metadata_infoframe{
            .eotf = 2,
            .metadata_type = 0,
            .display_primaries = {
                {to16Bit(red.x), to16Bit(red.y)},
                {to16Bit(green.x), to16Bit(green.y)},
                {to16Bit(blue.x), to16Bit(blue.y)},
            },
            .white_point = {to16Bit(white.x), to16Bit(white.y)},
            .max_display_mastering_luminance = maxLum,
            .min_display_mastering_luminance = minLum,
            .max_cll = maxLum,
            .max_fall = maxLum,
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
