/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_egl_layer.h"
#include "core/colorpipeline.h"
#include "core/iccprofile.h"
#include "drm_backend.h"
#include "drm_buffer.h"
#include "drm_crtc.h"
#include "drm_egl_backend.h"
#include "drm_gpu.h"
#include "drm_output.h"
#include "drm_pipeline.h"
#include "scene/surfaceitem_wayland.h"
#include "utils/envvar.h"
#include "wayland/surface.h"

#include <QRegion>
#include <drm_fourcc.h>
#include <errno.h>
#include <gbm.h>
#include <unistd.h>

namespace KWin
{

static constexpr EglGbmLayerSurface::BufferTarget targetFor(DrmGpu *gpu, DrmPlane::TypeIndex planeType) noexcept
{
    return ((!gpu->atomicModeSetting() || gpu->isVirtualMachine()) && planeType == DrmPlane::TypeIndex::Cursor)
        ? EglGbmLayerSurface::BufferTarget::Dumb
        : EglGbmLayerSurface::BufferTarget::Normal;
}

EglGbmLayer::EglGbmLayer(EglGbmBackend *eglBackend, DrmPlane *plane)
    : DrmPipelineLayer(plane)
    , m_surface(plane->gpu(), eglBackend, targetFor(plane->gpu(), plane->type.enumValue()))
{
}

EglGbmLayer::EglGbmLayer(EglGbmBackend *eglBackend, DrmGpu *gpu, DrmPlane::TypeIndex type)
    : DrmPipelineLayer(type)
    , m_surface(gpu, eglBackend, targetFor(gpu, type))
{
}

std::optional<OutputLayerBeginFrameInfo> EglGbmLayer::doBeginFrame()
{
    m_scanoutBuffer.reset();
    return m_surface.startRendering(targetRect().size(),
                                    drmOutput()->transform().combine(OutputTransform::FlipY),
                                    supportedDrmFormats(),
                                    drmOutput()->blendingColor(),
                                    drmOutput()->layerBlendingColor(),
                                    drmOutput()->needsShadowBuffer() ? pipeline()->iccProfile() : nullptr,
                                    drmOutput()->scale(),
                                    drmOutput()->colorPowerTradeoff(),
                                    drmOutput()->needsShadowBuffer(),
                                    m_requiredAlphaBits);
}

bool EglGbmLayer::doEndFrame(const QRegion &renderedRegion, const QRegion &damagedRegion, OutputFrame *frame)
{
    return m_surface.endRendering(damagedRegion, frame);
}

bool EglGbmLayer::preparePresentationTest()
{
    if (m_type != OutputLayerType::Primary && drmOutput()->shouldDisableNonPrimaryPlanes()) [[unlikely]] {
        return false;
    }
    m_scanoutBuffer.reset();
    return m_surface.renderTestBuffer(targetRect().size(), supportedDrmFormats(), drmOutput()->colorPowerTradeoff(), m_requiredAlphaBits) != nullptr;
}

bool EglGbmLayer::importScanoutBuffer(GraphicsBuffer *buffer, const std::shared_ptr<OutputFrame> &frame)
{
    static const bool directScanoutDisabled = environmentVariableBoolValue("KWIN_DRM_NO_DIRECT_SCANOUT").value_or(false);

    if (directScanoutDisabled) [[unlikely]] {
        return false;
    }

    const bool isPrimaryWithDisabledPlanes = (m_type == OutputLayerType::Primary) && drmOutput()->shouldDisableNonPrimaryPlanes();
    if (isPrimaryWithDisabledPlanes) [[unlikely]] {
        return false;
    }

    DrmGpu *gpuPtr = gpu();
    if (gpuPtr->needsModeset() || drmOutput()->needsShadowBuffer() || gpuPtr != gpuPtr->platform()->primaryGpu()) [[unlikely]] {
        return false;
    }

    if (!m_colorPipeline.isIdentity() && drmOutput()->colorPowerTradeoff() == Output::ColorPowerTradeoff::PreferAccuracy) [[unlikely]] {
        return false;
    }

    const QRectF src = sourceRect();
    if (src != src.toRect()) [[unlikely]] {
        return false;
    }

    const auto transform = offloadTransform();
    if (transform != OutputTransform::Kind::Normal && (!m_plane || !m_plane->supportsTransformation(transform))) [[unlikely]] {
        return false;
    }

    m_scanoutBuffer = gpuPtr->importBuffer(buffer, FileDescriptor{});
    if (m_scanoutBuffer) [[likely]] {
        m_surface.forgetDamage();
    }
    return m_scanoutBuffer != nullptr;
}

std::shared_ptr<DrmFramebuffer> EglGbmLayer::currentBuffer() const
{
    return m_scanoutBuffer ? m_scanoutBuffer : m_surface.currentBuffer();
}

void EglGbmLayer::releaseBuffers()
{
    m_scanoutBuffer.reset();
    m_surface.destroyResources();
}

}
