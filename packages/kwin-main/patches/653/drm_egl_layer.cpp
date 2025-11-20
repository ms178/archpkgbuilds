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
#include <cmath>
#include <drm_fourcc.h>
#include <errno.h>
#include <gbm.h>
#include <unistd.h>

namespace KWin
{

// Helper to determine buffer target. Static to avoid class overhead.
// Note: gpu->atomicModeSetting() is a runtime check, so this cannot be constexpr.
static EglGbmLayerSurface::BufferTarget targetFor(DrmGpu *gpu, DrmPlane::TypeIndex planeType)
{
    if ((!gpu->atomicModeSetting() || gpu->isVirtualMachine()) && planeType == DrmPlane::TypeIndex::Cursor) {
        return EglGbmLayerSurface::BufferTarget::Dumb;
    }
    return EglGbmLayerSurface::BufferTarget::Normal;
}

EglGbmLayer::EglGbmLayer(EglGbmBackend *eglBackend, DrmPlane *plane)
    : DrmPipelineLayer(plane)
    , m_surface(plane->gpu(), eglBackend, targetFor(plane->gpu(), plane->type.enumValue()))
    , m_isPrimaryGpu(plane->gpu() == plane->gpu()->platform()->primaryGpu())
{
}

EglGbmLayer::EglGbmLayer(EglGbmBackend *eglBackend, DrmGpu *gpu, DrmPlane::TypeIndex type)
    : DrmPipelineLayer(type)
    , m_surface(gpu, eglBackend, targetFor(gpu, type))
    , m_isPrimaryGpu(gpu == gpu->platform()->primaryGpu())
{
}

std::optional<OutputLayerBeginFrameInfo> EglGbmLayer::doBeginFrame()
{
    m_scanoutBuffer.reset();
    // Cache output pointer and status to reduce virtual calls and pointer chasing
    DrmOutput *const output = drmOutput();
    const bool needsShadow = output->needsShadowBuffer();

    return m_surface.startRendering(targetRect().size(),
                                    output->transform().combine(OutputTransform::FlipY),
                                    supportedDrmFormats(),
                                    output->blendingColor(),
                                    output->layerBlendingColor(),
                                    needsShadow ? pipeline()->iccProfile() : nullptr,
                                    output->scale(),
                                    output->colorPowerTradeoff(),
                                    needsShadow,
                                    m_requiredAlphaBits);
}

bool EglGbmLayer::doEndFrame(const QRegion &renderedRegion, const QRegion &damagedRegion, OutputFrame *frame)
{
    return m_surface.endRendering(damagedRegion, frame);
}

bool EglGbmLayer::preparePresentationTest()
{
    DrmOutput *const output = drmOutput();
    // Fail fast if non-primary planes are disabled and we aren't primary
    if (m_type != OutputLayerType::Primary && output->shouldDisableNonPrimaryPlanes()) [[unlikely]] {
        return false;
    }
    m_scanoutBuffer.reset();
    return m_surface.renderTestBuffer(targetRect().size(), supportedDrmFormats(), output->colorPowerTradeoff(), m_requiredAlphaBits) != nullptr;
}

// Fast check for integer coordinates without constructing QRect
static inline bool isInteger(qreal v)
{
    return v == std::floor(v);
}

bool EglGbmLayer::importScanoutBuffer(GraphicsBuffer *buffer, const std::shared_ptr<OutputFrame> &frame)
{
    // Static check is thread-safe and extremely cheap (cached)
    static const bool directScanoutDisabled = environmentVariableBoolValue("KWIN_DRM_NO_DIRECT_SCANOUT").value_or(false);
    if (directScanoutDisabled) [[unlikely]] {
        return false;
    }

    // 1. Cheap Boolean/Integer Checks
    // If we are not the primary layer and the output restricts overlays, we must bail.
    DrmOutput *const output = drmOutput();
    if (m_type != OutputLayerType::Primary && output->shouldDisableNonPrimaryPlanes()) [[unlikely]] {
        return false;
    }

    if (output->needsShadowBuffer()) [[unlikely]] {
        // Client buffer won't match shadow requirements usually
        return false;
    }

    // 2. GPU Capability Checks (Cached)
    // Direct scanout across GPUs is risky (driver bugs, implicit modifiers)
    if (!m_isPrimaryGpu) [[unlikely]] {
        return false;
    }

    DrmGpu *const gpuPtr = gpu();
    if (gpuPtr->needsModeset()) [[unlikely]] {
        // Avoid locking hardware to a specific format during modeset
        return false;
    }

    // 3. Geometry Checks (Floating Point)
    // Kernel requires integer coordinates. Fast-fail before more complex logic.
    const QRectF src = sourceRect();
    if (!isInteger(src.x()) || !isInteger(src.y()) || !isInteger(src.width()) || !isInteger(src.height())) [[unlikely]] {
        return false;
    }

    // 4. Pipeline & Transform Checks (Virtual Calls / Complex Objects)
    if (!m_colorPipeline.isIdentity() && output->colorPowerTradeoff() == Output::ColorPowerTradeoff::PreferAccuracy) [[unlikely]] {
        return false;
    }

    const auto transform = offloadTransform();
    if (transform != OutputTransform::Kind::Normal && (!m_plane || !m_plane->supportsTransformation(transform))) [[unlikely]] {
        return false;
    }

    // 5. Import (System Call / Expensive)
    m_scanoutBuffer = gpuPtr->importBuffer(buffer, FileDescriptor{});
    if (m_scanoutBuffer) {
        m_surface.forgetDamage();
        return true;
    }

    return false;
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
