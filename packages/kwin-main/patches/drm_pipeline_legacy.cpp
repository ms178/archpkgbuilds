#include "core/graphicsbuffer.h"
#include "drm_buffer.h"
#include "drm_commit.h"
#include "drm_commit_thread.h"
#include "drm_connector.h"
#include "drm_crtc.h"
#include "drm_gpu.h"
#include "drm_layer.h"
#include "drm_logging.h"
#include "drm_pipeline.h"

#include <errno.h>
#include <gbm.h>

namespace KWin
{

static DrmPipelineLayer *findLayer(const auto &layers, OutputLayerType type)
{
    const auto it = std::ranges::find_if(layers, [type](OutputLayer *layer) {
        return layer->type() == type;
    });
    return it == layers.end() ? nullptr : static_cast<DrmPipelineLayer *>(*it);
}

DrmPipeline::Error DrmPipeline::presentLegacy(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame)
{
    if (Error err = applyPendingChangesLegacy(); err != Error::None) [[unlikely]] {
        return err;
    }

    if (auto cursor = findLayer(layersToUpdate, OutputLayerType::CursorOnly)) [[likely]] {
        if (!setCursorLegacy(cursor)) [[unlikely]] {
            return Error::InvalidArguments;
        }
    }

    const auto primary = findLayer(m_pending.layers, OutputLayerType::Primary);
    const auto buffer = primary->currentBuffer();
    const QSize bufferSize = buffer->buffer()->size();
    const QRect bufferRect(QPoint(0, 0), bufferSize);

    if (primary->sourceRect() != primary->targetRect() || primary->targetRect() != bufferRect) [[unlikely]] {
        return Error::InvalidArguments;
    }

    auto commit = std::make_unique<DrmLegacyCommit>(this, buffer, frame);
    if (!commit->doPageflip(m_pending.presentationMode)) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Page flip failed:" << strerror(errno);
        return errnoToError();
    }

    m_commitThread->setPendingCommit(std::move(commit));
    return Error::None;
}

void DrmPipeline::forceLegacyModeset()
{
    if (activePending()) {
        if (const Error err = legacyModeset(); err != Error::None) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Force modeset failed:" << static_cast<int>(err);
        }
        if (const Error err = setLegacyGamma(); err != Error::None) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Force gamma update failed:" << static_cast<int>(err);
        }
    }
}

DrmPipeline::Error DrmPipeline::legacyModeset()
{
    const auto primary = findLayer(m_pending.layers, OutputLayerType::Primary);
    const auto buffer = primary->currentBuffer();
    if (!buffer) {
        return Error::InvalidArguments;
    }
    if (primary->sourceRect() != QRect(QPoint(0, 0), buffer->buffer()->size())) {
        return Error::InvalidArguments;
    }
    auto commit = std::make_unique<DrmLegacyCommit>(this, buffer, nullptr);
    if (!commit->doModeset(m_connector, m_pending.mode.get())) {
        qCWarning(KWIN_DRM) << "Modeset failed!" << strerror(errno);
        return errnoToError();
    }
    return Error::None;
}

DrmPipeline::Error DrmPipeline::commitPipelinesLegacy(const QList<DrmPipeline *> &pipelines, CommitMode mode, const QList<DrmObject *> &unusedObjects)
{
    for (DrmPipeline *pipeline : pipelines) {
        if (Error err = pipeline->applyPendingChangesLegacy(); err != Error::None) [[unlikely]] {
            for (DrmPipeline *revert : pipelines) {
                revert->revertPendingChanges();
                [[maybe_unused]] const Error revertErr = revert->applyPendingChangesLegacy();
            }
            return err;
        }
    }

    for (DrmPipeline *pipeline : pipelines) {
        pipeline->applyPendingChanges();
        if (mode == CommitMode::CommitModeset && pipeline->activePending()) [[unlikely]] {
            pipeline->pageFlipped(std::chrono::steady_clock::now().time_since_epoch());
        }
    }

    for (DrmObject *obj : unusedObjects) {
        if (auto crtc = dynamic_cast<DrmCrtc *>(obj)) [[likely]] {
            drmModeSetCrtc(pipelines.front()->gpu()->fd(), crtc->id(), 0, 0, 0, nullptr, 0, nullptr);
        }
    }

    return Error::None;
}

DrmPipeline::Error DrmPipeline::applyPendingChangesLegacy()
{
    if (!m_pending.active && m_pending.crtc) [[unlikely]] {
        drmModeSetCursor(gpu()->fd(), m_pending.crtc->id(), 0, 0, 0);
    }

    if (!activePending()) [[unlikely]] {
        if (!m_connector->dpms.setPropertyLegacy(DRM_MODE_DPMS_OFF)) {
            qCWarning(KWIN_DRM) << "Setting legacy dpms failed!" << strerror(errno);
            return errnoToError();
        }
        return Error::None;
    }

    const bool colorTransforms = std::ranges::any_of(m_pending.layers, [](DrmPipelineLayer *layer) {
        return !layer->colorPipeline().isIdentity();
    });
    if (colorTransforms) [[unlikely]] {
        return DrmPipeline::Error::InvalidArguments;
    }

    const bool shouldEnableVrr = m_pending.presentationMode == PresentationMode::AdaptiveSync || m_pending.presentationMode == PresentationMode::AdaptiveAsync;
    if (m_pending.crtc->vrrEnabled.isValid()) [[likely]] {
        if (!m_pending.crtc->vrrEnabled.setPropertyLegacy(shouldEnableVrr)) [[unlikely]] {
            qCWarning(KWIN_DRM) << "Setting vrr failed!" << strerror(errno);
            return errnoToError();
        }
    }

    if (m_connector->broadcastRGB.isValid()) [[likely]] {
        m_connector->broadcastRGB.setEnumLegacy(DrmConnector::rgbRangeToBroadcastRgb(m_pending.rgbRange));
    }

    if (m_connector->overscan.isValid()) [[likely]] {
        m_connector->overscan.setPropertyLegacy(m_pending.overscan);
    } else if (m_connector->underscan.isValid()) [[likely]] {
        const uint32_t hborder = calculateUnderscan();
        m_connector->underscan.setEnumLegacy(m_pending.overscan != 0 ? DrmConnector::UnderscanOptions::On : DrmConnector::UnderscanOptions::Off);
        m_connector->underscanVBorder.setPropertyLegacy(m_pending.overscan);
        m_connector->underscanHBorder.setPropertyLegacy(hborder);
    }

    if (m_connector->scalingMode.isValid() && m_connector->scalingMode.hasEnum(DrmConnector::ScalingMode::None)) [[likely]] {
        m_connector->scalingMode.setEnumLegacy(DrmConnector::ScalingMode::None);
    }

    if (m_connector->hdrMetadata.isValid()) [[unlikely]] {
        const auto blob = createHdrMetadata(m_pending.hdr ? TransferFunction::PerceptualQuantizer : TransferFunction::gamma22);
        m_connector->hdrMetadata.setPropertyLegacy(blob ? blob->blobId() : 0);
    } else if (m_pending.hdr) [[unlikely]] {
        return DrmPipeline::Error::InvalidArguments;
    }

    if (m_connector->colorspace.isValid()) [[unlikely]] {
        m_connector->colorspace.setEnumLegacy(m_pending.wcg ? DrmConnector::Colorspace::BT2020_RGB : DrmConnector::Colorspace::Default);
    } else if (m_pending.wcg) [[unlikely]] {
        return DrmPipeline::Error::InvalidArguments;
    }

    const auto currentModeContent = m_pending.crtc->queryCurrentMode();
    if (m_pending.crtc != m_next.crtc || *m_pending.mode != currentModeContent) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Using legacy path to set mode" << m_pending.mode->nativeMode()->name;
        Error err = legacyModeset();
        if (err != Error::None) {
            return err;
        }
    }

    if (m_pending.crtcColorPipeline != m_currentLegacyGamma) [[unlikely]] {
        if (Error err = setLegacyGamma(); err != Error::None) {
            return err;
        }
    }

    if (m_connector->contentType.isValid()) [[likely]] {
        m_connector->contentType.setEnumLegacy(m_pending.contentType);
    }

    if (m_connector->maxBpc.isValid()) [[likely]] {
        m_connector->maxBpc.setPropertyLegacy(8);
    }

    setCursorLegacy(findLayer(m_pending.layers, OutputLayerType::CursorOnly));

    if (!m_connector->dpms.setPropertyLegacy(DRM_MODE_DPMS_ON)) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Setting legacy dpms failed!" << strerror(errno);
        return errnoToError();
    }

    return Error::None;
}

DrmPipeline::Error DrmPipeline::setLegacyGamma()
{
    const int gammaSize = m_pending.crtc->gammaRampSize();

    constexpr int kStackThreshold = 1024;
    uint16_t stackBuffer[kStackThreshold * 3];
    std::unique_ptr<uint16_t[]> heapBuffer;

    uint16_t *red;
    uint16_t *green;
    uint16_t *blue;

    if (gammaSize <= kStackThreshold) [[likely]] {
        red = stackBuffer;
        green = stackBuffer + gammaSize;
        blue = stackBuffer + (gammaSize * 2);
    } else {
        const size_t totalSize = static_cast<size_t>(gammaSize) * 3;
        heapBuffer = std::make_unique<uint16_t[]>(totalSize);
        red = heapBuffer.get();
        green = red + gammaSize;
        blue = green + gammaSize;
    }

    const float scale = 1.0f / static_cast<float>(gammaSize - 1);
    constexpr float maxValue = 65535.0f;

    if (m_pending.crtcColorPipeline.ops.empty()) [[likely]] {
        for (int i = 0; i < gammaSize; ++i) {
            const uint16_t value = static_cast<uint16_t>(std::min(static_cast<float>(i) * scale * maxValue, maxValue));
            red[i] = value;
            green[i] = value;
            blue[i] = value;
        }
    } else {
        for (int i = 0; i < gammaSize; ++i) {
            const float input = static_cast<float>(i) * scale;
            QVector3D output(input, input, input);

            for (const auto &op : m_pending.crtcColorPipeline.ops) {
                if (const auto *tf = std::get_if<ColorTransferFunction>(&op.operation)) {
                    output = tf->tf.encodedToNits(output);
                } else if (const auto *invTf = std::get_if<InverseColorTransferFunction>(&op.operation)) {
                    output = invTf->tf.nitsToEncoded(output);
                } else if (const auto *mult = std::get_if<ColorMultiplier>(&op.operation)) {
                    output *= mult->factors;
                } else [[unlikely]] {
                    return Error::InvalidArguments;
                }
            }

            red[i] = static_cast<uint16_t>(std::clamp(output.x(), 0.0f, 1.0f) * maxValue);
            green[i] = static_cast<uint16_t>(std::clamp(output.y(), 0.0f, 1.0f) * maxValue);
            blue[i] = static_cast<uint16_t>(std::clamp(output.z(), 0.0f, 1.0f) * maxValue);
        }
    }

    if (drmModeCrtcSetGamma(gpu()->fd(), m_pending.crtc->id(), gammaSize, red, green, blue) != 0) [[unlikely]] {
        qCWarning(KWIN_DRM) << "Setting gamma failed!" << strerror(errno);
        return errnoToError();
    }

    m_currentLegacyGamma = m_pending.crtcColorPipeline;
    return Error::None;
}

bool DrmPipeline::setCursorLegacy(DrmPipelineLayer *layer)
{
    const auto bo = layer->currentBuffer();
    uint32_t handle = 0;

    if (bo && bo->buffer() && layer->isEnabled()) [[likely]] {
        const DmaBufAttributes *attributes = bo->buffer()->dmabufAttributes();
        if (drmPrimeFDToHandle(gpu()->fd(), attributes->fd[0].get(), &handle) != 0) [[unlikely]] {
            qCWarning(KWIN_DRM) << "drmPrimeFDToHandle() failed";
            return false;
        }
    }

    const auto cursorSize = gpu()->cursorSize();
    const QRect targetRect = layer->targetRect();
    const QPointF hotspotF = layer->hotspot();

    drm_mode_cursor2 arg = {
        .flags = DRM_MODE_CURSOR_BO | DRM_MODE_CURSOR_MOVE,
        .crtc_id = m_pending.crtc->id(),
        .x = static_cast<int32_t>(targetRect.x()),
        .y = static_cast<int32_t>(targetRect.y()),
        .width = static_cast<uint32_t>(cursorSize.width()),
        .height = static_cast<uint32_t>(cursorSize.height()),
        .handle = handle,
        .hot_x = static_cast<int32_t>(std::lround(hotspotF.x())),
        .hot_y = static_cast<int32_t>(std::lround(hotspotF.y())),
    };

    const int ret = drmIoctl(gpu()->fd(), DRM_IOCTL_MODE_CURSOR2, &arg);

    if (handle != 0) [[likely]] {
        drmCloseBufferHandle(gpu()->fd(), handle);
    }

    return ret == 0;
}

}
