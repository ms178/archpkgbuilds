/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include <QHash>
#include <QMap>
#include <QPointer>
#include <QRegion>
#include <chrono>
#include <optional>

#include "core/outputlayer.h"
#include "drm_plane.h"
#include "opengl/gltexture.h"
#include "utils/damagejournal.h"
#include "utils/filedescriptor.h"

namespace KWin
{

class DrmFramebuffer;
class EglSwapchain;
class EglSwapchainSlot;
class QPainterSwapchain;
class ShadowBuffer;
class EglContext;
class EglGbmBackend;
class GraphicsBuffer;
class SurfaceItem;
class GLTexture;
class GLRenderTimeQuery;
class ColorTransformation;
class GlLookUpTable;
class IccProfile;
class IccShader;

class EglGbmLayerSurface final : public QObject
{
    Q_OBJECT
public:
    enum class BufferTarget {
        Normal,
        Dumb
    };
    explicit EglGbmLayerSurface(DrmGpu *gpu, EglGbmBackend *eglBackend, BufferTarget target = BufferTarget::Normal);
    ~EglGbmLayerSurface() override;

    std::optional<OutputLayerBeginFrameInfo> startRendering(const QSize &bufferSize, OutputTransform transformation, const QHash<uint32_t, QList<uint64_t>> &formats, const std::shared_ptr<ColorDescription> &blendingColor, const std::shared_ptr<ColorDescription> &layerBlendingColor, const std::shared_ptr<IccProfile> &iccProfile, double scale, Output::ColorPowerTradeoff tradeoff, bool useShadowBuffer, uint32_t requiredAlphaBits);
    bool endRendering(const QRegion &damagedRegion, OutputFrame *frame);

    void destroyResources();
    EglGbmBackend *eglBackend() const;
    std::shared_ptr<DrmFramebuffer> renderTestBuffer(const QSize &bufferSize, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits);
    void forgetDamage();

    std::shared_ptr<DrmFramebuffer> currentBuffer() const;
    const std::shared_ptr<ColorDescription> &colorDescription() const;

private:
    enum class MultiGpuImportMode : uint8_t {
        None,
        Dmabuf,
        LinearDmabuf,
        Egl,
        DumbBuffer
    };

    // Optimized layout for Raptor Lake (64-byte cache line)
    struct alignas(64) Surface
    {
        // --- Cache Line 0 (Hot Path: checkSurface / startRendering) ---
        std::shared_ptr<EglSwapchain> gbmSwapchain;             // 0-16
        std::shared_ptr<EglSwapchainSlot> currentSlot;          // 16-32

        double scale = 1.0;                                     // 32-40
        float invWidth = 1.0f;                                  // 40-44 (Precomputed inverse width)
        float invHeight = 1.0f;                                 // 44-48 (Precomputed inverse height)
        Output::ColorPowerTradeoff tradeoff = Output::ColorPowerTradeoff::PreferEfficiency; // 48-52
        uint32_t requiredAlphaBits = 0;                         // 52-56
        BufferTarget bufferTarget;                              // 56-60
        MultiGpuImportMode importMode;                          // 60-61
        bool needsRecreation = false;                           // 61-62
        bool needsShadowBuffer = false;                         // 62-63
        // 1 byte padding

        // --- Cache Line 1 (Color Management & Context) ---
        std::unique_ptr<GLRenderTimeQuery> compositingTimeQuery;// 64-72
        std::shared_ptr<EglContext> context;                    // 72-88
        std::shared_ptr<ColorDescription> blendingColor = ColorDescription::sRGB;      // 88-104
        std::shared_ptr<ColorDescription> layerBlendingColor = ColorDescription::sRGB; // 104-120
        // 8 bytes padding

        // --- Cache Line 2 (Render State / Buffers) ---
        std::shared_ptr<IccProfile> iccProfile;                 // 128-144
        std::shared_ptr<DrmFramebuffer> currentFramebuffer;     // 144-160
        std::unique_ptr<IccShader> iccShader;                   // 160-168
        double brightness = 1.0;                                // 168-176

        // --- Remaining (Cold / Large Objects) ---
        DamageJournal damageJournal;
        std::shared_ptr<EglSwapchain> shadowSwapchain;
        std::shared_ptr<EglSwapchainSlot> currentShadowSlot;
        DamageJournal shadowDamageJournal;

        std::unique_ptr<QPainterSwapchain> importDumbSwapchain;
        std::shared_ptr<EglContext> importContext;
        std::shared_ptr<EglSwapchain> importGbmSwapchain;
        QHash<GraphicsBuffer *, std::shared_ptr<GLTexture>> importedTextureCache;
        DamageJournal importDamageJournal;
        QImage cpuCopyCache;

        ~Surface();
    };

    bool checkSurface(const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits);
    bool doesSurfaceFit(const Surface *surface, const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const;
    std::unique_ptr<Surface> createSurface(const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const;
    std::unique_ptr<Surface> createSurface(const QSize &size, uint32_t format, const QList<uint64_t> &modifiers, MultiGpuImportMode importMode, BufferTarget bufferTarget, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const;
    std::shared_ptr<EglSwapchain> createGbmSwapchain(DrmGpu *gpu, EglContext *context, const QSize &size, uint32_t format, const QList<uint64_t> &modifiers, MultiGpuImportMode importMode, BufferTarget bufferTarget) const;

    std::shared_ptr<DrmFramebuffer> doRenderTestBuffer(Surface *surface) const;
    std::shared_ptr<DrmFramebuffer> importBuffer(Surface *surface, EglSwapchainSlot *source, FileDescriptor &&readFence, OutputFrame *frame, const QRegion &damagedRegion) const;
    std::shared_ptr<DrmFramebuffer> importWithEgl(Surface *surface, GraphicsBuffer *sourceBuffer, FileDescriptor &&readFence, OutputFrame *frame, const QRegion &damagedRegion) const;
    std::shared_ptr<DrmFramebuffer> importWithCpu(Surface *surface, EglSwapchainSlot *source, OutputFrame *frame) const;

    std::unique_ptr<Surface> m_surface;
    std::unique_ptr<Surface> m_oldSurface;

    DrmGpu *const m_gpu;
    EglGbmBackend *const m_eglBackend;
    const BufferTarget m_requestedBufferTarget;
};

}
