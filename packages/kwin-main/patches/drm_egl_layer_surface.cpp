/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2022 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_egl_layer_surface.h"

#include "config-kwin.h"

#include "core/colortransformation.h"
#include "core/graphicsbufferview.h"
#include "core/iccprofile.h"
#include "drm_egl_backend.h"
#include "drm_gpu.h"
#include "drm_logging.h"
#include "opengl/eglnativefence.h"
#include "opengl/eglswapchain.h"
#include "opengl/gllut.h"
#include "opengl/glrendertimequery.h"
#include "opengl/icc_shader.h"
#include "qpainter/qpainterswapchain.h"
#include "utils/drm_format_helper.h"

#include <drm_fourcc.h>
#include <errno.h>
#include <gbm.h>
#include <unistd.h>
#include <cstring>

namespace KWin
{

static const QList<uint64_t> linearModifier = {DRM_FORMAT_MOD_LINEAR};
static const QList<uint64_t> implicitModifier = {DRM_FORMAT_MOD_INVALID};
static const QList<uint32_t> cpuCopyFormats = {DRM_FORMAT_ARGB8888, DRM_FORMAT_XRGB8888};

static const bool bufferAgeEnabled = qEnvironmentVariable("KWIN_USE_BUFFER_AGE") != QStringLiteral("0");

static inline gbm_format_name_desc formatName(uint32_t format)
{
    gbm_format_name_desc ret;
    gbm_format_get_name(format, &ret);
    return ret;
}

EglGbmLayerSurface::EglGbmLayerSurface(DrmGpu *gpu, EglGbmBackend *eglBackend, BufferTarget target)
    : m_gpu(gpu)
    , m_eglBackend(eglBackend)
    , m_requestedBufferTarget(target)
{
}

EglGbmLayerSurface::~EglGbmLayerSurface() = default;

EglGbmLayerSurface::Surface::~Surface()
{
    if (importContext) {
        importContext->makeCurrent();
        importGbmSwapchain.reset();
        importedTextureCache.clear();
        importContext.reset();
    }
    if (context) {
        context->makeCurrent();
    }
}

void EglGbmLayerSurface::destroyResources()
{
    m_surface.reset();
    m_oldSurface.reset();
}

std::optional<OutputLayerBeginFrameInfo> EglGbmLayerSurface::startRendering(const QSize &bufferSize, OutputTransform transformation,
                                                                            const QHash<uint32_t, QList<uint64_t>> &formats,
                                                                            const std::shared_ptr<ColorDescription> &blendingColor,
                                                                            const std::shared_ptr<ColorDescription> &layerBlendingColor,
                                                                            const std::shared_ptr<IccProfile> &iccProfile, double scale,
                                                                            Output::ColorPowerTradeoff tradeoff, bool useShadowBuffer,
                                                                            uint32_t requiredAlphaBits)
{
    if (!checkSurface(bufferSize, formats, tradeoff, requiredAlphaBits)) [[unlikely]] {
        return std::nullopt;
    }
    m_oldSurface.reset();

    if (!m_eglBackend->openglContext()->makeCurrent()) [[unlikely]] {
        return std::nullopt;
    }

    auto slot = m_surface->gbmSwapchain->acquire();
    if (!slot) [[unlikely]] {
        return std::nullopt;
    }

    auto *colorAttachment = slot->framebuffer()->colorAttachment();
    if (colorAttachment->contentTransform() != transformation) {
        colorAttachment->setContentTransform(transformation);
        m_surface->damageJournal.clear();
    }

    m_surface->currentSlot = std::move(slot);
    m_surface->scale = scale;

    const bool colorManagementChanged = (m_surface->blendingColor != blendingColor)
                                     || (m_surface->layerBlendingColor != layerBlendingColor)
                                     || (m_surface->iccProfile != iccProfile);

    if (colorManagementChanged) {
        m_surface->damageJournal.clear();
        m_surface->shadowDamageJournal.clear();
        m_surface->needsShadowBuffer = useShadowBuffer;
        m_surface->blendingColor = blendingColor;
        m_surface->layerBlendingColor = layerBlendingColor;
        m_surface->iccProfile = iccProfile;

        if (iccProfile) {
            if (!m_surface->iccShader) {
                m_surface->iccShader = std::make_unique<IccShader>();
            }
        } else {
            m_surface->iccShader.reset();
        }
    }

    m_surface->compositingTimeQuery = std::make_unique<GLRenderTimeQuery>(m_surface->context);
    m_surface->compositingTimeQuery->begin();

    if (m_surface->needsShadowBuffer) [[unlikely]] {
        const QSize swapchainSize = m_surface->gbmSwapchain->size();
        if (!m_surface->shadowSwapchain || m_surface->shadowSwapchain->size() != swapchainSize) {
            const auto availableFormats = m_eglBackend->eglDisplayObject()->nonExternalOnlySupportedDrmFormats();
            const QList<FormatInfo> sortedFormats = OutputLayer::filterAndSortFormats(availableFormats, requiredAlphaBits, tradeoff);

            for (const auto &formatInfo : sortedFormats) {
                auto modifiers = availableFormats[formatInfo.drmFormat];

                if (formatInfo.floatingPoint && m_eglBackend->gpu()->isAmdgpu() && qEnvironmentVariableIntValue("KWIN_DRM_NO_DCC_WORKAROUND") == 0) [[unlikely]] {
                    if (!modifiers.contains(DRM_FORMAT_MOD_LINEAR)) {
                        continue;
                    }
                    modifiers = {DRM_FORMAT_MOD_LINEAR};
                }

                m_surface->shadowSwapchain = EglSwapchain::create(m_eglBackend->drmDevice()->allocator(),
                                                                  m_eglBackend->openglContext(),
                                                                  swapchainSize,
                                                                  formatInfo.drmFormat,
                                                                  modifiers);
                if (m_surface->shadowSwapchain) [[likely]] {
                    break;
                }
            }
        }

        if (!m_surface->shadowSwapchain) [[unlikely]] {
            qCCritical(KWIN_DRM) << "Failed to create shadow swapchain!";
            return std::nullopt;
        }

        m_surface->currentShadowSlot = m_surface->shadowSwapchain->acquire();
        if (!m_surface->currentShadowSlot) [[unlikely]] {
            return std::nullopt;
        }

        m_surface->currentShadowSlot->texture()->setContentTransform(colorAttachment->contentTransform());

        return OutputLayerBeginFrameInfo{
            .renderTarget = RenderTarget(m_surface->currentShadowSlot->framebuffer(), m_surface->blendingColor),
            .repaint = bufferAgeEnabled ? m_surface->shadowDamageJournal.accumulate(m_surface->currentShadowSlot->age(), infiniteRegion()) : infiniteRegion(),
        };
    }

    m_surface->shadowSwapchain.reset();
    m_surface->currentShadowSlot.reset();

    return OutputLayerBeginFrameInfo{
        .renderTarget = RenderTarget(m_surface->currentSlot->framebuffer(), m_surface->blendingColor),
        .repaint = bufferAgeEnabled ? m_surface->damageJournal.accumulate(m_surface->currentSlot->age(), infiniteRegion()) : infiniteRegion(),
    };
}

[[gnu::hot]] static inline GLVertexBuffer *uploadGeometrySingleRect(const QRect &rect, const QSize &fboSize)
{
    GLVertexBuffer *vbo = GLVertexBuffer::streamingBuffer();
    vbo->reset();
    vbo->setAttribLayout(std::span(GLVertexBuffer::GLVertex2DLayout), sizeof(GLVertex2D));

    const auto optMap = vbo->map<GLVertex2D>(6);
    if (!optMap) [[unlikely]] {
        return nullptr;
    }

    const auto map = *optMap;
    const float invWidth = 1.0f / static_cast<float>(fboSize.width());
    const float invHeight = 1.0f / static_cast<float>(fboSize.height());

    const float x0 = static_cast<float>(rect.left());
    const float y0 = static_cast<float>(rect.top());
    const float x1 = static_cast<float>(rect.right());
    const float y1 = static_cast<float>(rect.bottom());

    const float u0 = x0 * invWidth;
    const float v0 = y0 * invHeight;
    const float u1 = x1 * invWidth;
    const float v1 = y1 * invHeight;

    map[0] = GLVertex2D{QVector2D(x0, y0), QVector2D(u0, v0)};
    map[1] = GLVertex2D{QVector2D(x1, y1), QVector2D(u1, v1)};
    map[2] = GLVertex2D{QVector2D(x0, y1), QVector2D(u0, v1)};
    map[3] = GLVertex2D{QVector2D(x0, y0), QVector2D(u0, v0)};
    map[4] = GLVertex2D{QVector2D(x1, y0), QVector2D(u1, v0)};
    map[5] = GLVertex2D{QVector2D(x1, y1), QVector2D(u1, v1)};

    vbo->unmap();
    vbo->setVertexCount(6);
    return vbo;
}

[[gnu::hot]] static GLVertexBuffer *uploadGeometry(const QRegion &devicePixels, const QSize &fboSize)
{
    const int rectCount = devicePixels.rectCount();
    if (rectCount == 1) [[likely]] {
        return uploadGeometrySingleRect(*devicePixels.begin(), fboSize);
    }

    GLVertexBuffer *vbo = GLVertexBuffer::streamingBuffer();
    vbo->reset();
    vbo->setAttribLayout(std::span(GLVertexBuffer::GLVertex2DLayout), sizeof(GLVertex2D));

    const size_t vertexCount = rectCount * 6;
    const auto optMap = vbo->map<GLVertex2D>(vertexCount);
    if (!optMap) [[unlikely]] {
        return nullptr;
    }

    const auto map = *optMap;
    const float invWidth = 1.0f / static_cast<float>(fboSize.width());
    const float invHeight = 1.0f / static_cast<float>(fboSize.height());

    size_t vboIndex = 0;
    for (const QRect &rect : devicePixels) {
        const float x0 = static_cast<float>(rect.left());
        const float y0 = static_cast<float>(rect.top());
        const float x1 = static_cast<float>(rect.right());
        const float y1 = static_cast<float>(rect.bottom());

        const float u0 = x0 * invWidth;
        const float v0 = y0 * invHeight;
        const float u1 = x1 * invWidth;
        const float v1 = y1 * invHeight;

        map[vboIndex++] = GLVertex2D{QVector2D(x0, y0), QVector2D(u0, v0)};
        map[vboIndex++] = GLVertex2D{QVector2D(x1, y1), QVector2D(u1, v1)};
        map[vboIndex++] = GLVertex2D{QVector2D(x0, y1), QVector2D(u0, v1)};
        map[vboIndex++] = GLVertex2D{QVector2D(x0, y0), QVector2D(u0, v0)};
        map[vboIndex++] = GLVertex2D{QVector2D(x1, y0), QVector2D(u1, v0)};
        map[vboIndex++] = GLVertex2D{QVector2D(x1, y1), QVector2D(u1, v1)};
    }

    vbo->unmap();
    vbo->setVertexCount(vboIndex);
    return vbo;
}

bool EglGbmLayerSurface::endRendering(const QRegion &damagedRegion, OutputFrame *frame)
{
    if (m_surface->needsShadowBuffer) [[unlikely]] {
        const QRegion logicalRepaint = damagedRegion | m_surface->damageJournal.accumulate(m_surface->currentSlot->age(), infiniteRegion());
        m_surface->damageJournal.add(damagedRegion);
        m_surface->shadowDamageJournal.add(damagedRegion);

        const QSize swapchainSize = m_surface->gbmSwapchain->size();
        QRegion repaint;

        if (logicalRepaint == infiniteRegion()) {
            repaint = QRect(QPoint(), swapchainSize);
        } else {
            const auto mapping = m_surface->currentShadowSlot->framebuffer()->colorAttachment()->contentTransform().combine(OutputTransform::FlipY);
            const QSize rotatedSize = mapping.map(swapchainSize);
            const QRect bounds(QPoint(), swapchainSize);

            for (const QRect &rect : logicalRepaint) {
                repaint |= mapping.map(scaledRect(rect, m_surface->scale), rotatedSize).toAlignedRect() & bounds;
            }
        }

        GLFramebuffer *fbo = m_surface->currentSlot->framebuffer();
        GLFramebuffer::pushFramebuffer(fbo);

        ShaderBinder binder = m_surface->iccShader
            ? ShaderBinder(m_surface->iccShader->shader())
            : ShaderBinder(ShaderTrait::MapTexture | ShaderTrait::TransformColorspace);

        if (m_surface->iccShader) {
            m_surface->iccShader->setUniforms(m_surface->iccProfile, m_surface->blendingColor, RenderingIntent::AbsoluteColorimetricNoAdaptation);
        } else {
            binder.shader()->setColorspaceUniforms(m_surface->blendingColor, m_surface->layerBlendingColor, RenderingIntent::AbsoluteColorimetricNoAdaptation);
        }

        QMatrix4x4 mat;
        mat.scale(1.0f, -1.0f);
        mat.ortho(QRectF(QPointF(), fbo->size()));
        binder.shader()->setUniform(GLShader::Mat4Uniform::ModelViewProjectionMatrix, mat);
        glDisable(GL_BLEND);

        if (const auto vbo = uploadGeometry(repaint, swapchainSize)) [[likely]] {
            m_surface->currentShadowSlot->texture()->bind();
            vbo->render(GL_TRIANGLES);
            m_surface->currentShadowSlot->texture()->unbind();
        }

        EGLNativeFence fence(m_surface->context->displayObject());
        m_surface->shadowSwapchain->release(m_surface->currentShadowSlot, fence.takeFileDescriptor());
        GLFramebuffer::popFramebuffer();
    } else {
        m_surface->damageJournal.add(damagedRegion);
    }

    m_surface->compositingTimeQuery->end();
    if (frame) [[likely]] {
        frame->addRenderTimeQuery(std::move(m_surface->compositingTimeQuery));
    }

    glFlush();

    EGLNativeFence sourceFence(m_eglBackend->eglDisplayObject());
    if (!sourceFence.isValid()) [[unlikely]] {
        glFinish();
    }

    m_surface->gbmSwapchain->release(m_surface->currentSlot, sourceFence.fileDescriptor().duplicate());

    const auto buffer = importBuffer(m_surface.get(), m_surface->currentSlot.get(), sourceFence.takeFileDescriptor(), frame, damagedRegion);
    if (buffer) [[likely]] {
        m_surface->currentFramebuffer = buffer;
        return true;
    }
    return false;
}

EglGbmBackend *EglGbmLayerSurface::eglBackend() const
{
    return m_eglBackend;
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::currentBuffer() const
{
    return m_surface ? m_surface->currentFramebuffer : nullptr;
}

const std::shared_ptr<ColorDescription> &EglGbmLayerSurface::colorDescription() const
{
    return m_surface ? m_surface->blendingColor : ColorDescription::sRGB;
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::renderTestBuffer(const QSize &bufferSize, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits)
{
    EglContext *context = m_eglBackend->openglContext();
    if (!context->makeCurrent()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "EglGbmLayerSurface::renderTestBuffer: failed to make opengl context current";
        return nullptr;
    }

    return checkSurface(bufferSize, formats, tradeoff, requiredAlphaBits) ? m_surface->currentFramebuffer : nullptr;
}

void EglGbmLayerSurface::forgetDamage()
{
    if (m_surface) {
        m_surface->damageJournal.clear();
        m_surface->importDamageJournal.clear();
        m_surface->shadowDamageJournal.clear();
    }
}

bool EglGbmLayerSurface::checkSurface(const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits)
{
    if (doesSurfaceFit(m_surface.get(), size, formats, tradeoff, requiredAlphaBits)) [[likely]] {
        return true;
    }

    if (doesSurfaceFit(m_oldSurface.get(), size, formats, tradeoff, requiredAlphaBits)) [[unlikely]] {
        m_surface = std::move(m_oldSurface);
        return true;
    }

    auto newSurface = createSurface(size, formats, tradeoff, requiredAlphaBits);
    if (!newSurface) [[unlikely]] {
        return false;
    }

    m_oldSurface = std::move(m_surface);
    if (m_oldSurface) {
        m_oldSurface->damageJournal.clear();
        m_oldSurface->shadowDamageJournal.clear();
        m_oldSurface->gbmSwapchain->resetBufferAge();

        if (m_oldSurface->shadowSwapchain) {
            m_oldSurface->shadowSwapchain->resetBufferAge();
        }
        if (m_oldSurface->importGbmSwapchain) {
            m_oldSurface->importGbmSwapchain->resetBufferAge();
            m_oldSurface->importDamageJournal.clear();
        }
    }

    m_surface = std::move(newSurface);
    return true;
}

bool EglGbmLayerSurface::doesSurfaceFit(const Surface *surface, const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const
{
    if (!surface || surface->needsRecreation || !surface->gbmSwapchain) [[unlikely]] {
        return false;
    }

    if (surface->gbmSwapchain->size() != size) {
        return false;
    }

    if (surface->tradeoff != tradeoff || surface->requiredAlphaBits != requiredAlphaBits) {
        return false;
    }

    if (surface->bufferTarget == BufferTarget::Dumb) {
        return formats.contains(surface->importDumbSwapchain->format());
    }

    switch (surface->importMode) {
    case MultiGpuImportMode::None:
    case MultiGpuImportMode::Dmabuf:
    case MultiGpuImportMode::LinearDmabuf: {
        const uint32_t format = surface->gbmSwapchain->format();
        const auto it = formats.constFind(format);
        if (it == formats.cend()) [[unlikely]] {
            return false;
        }
        const uint64_t modifier = surface->gbmSwapchain->modifier();
        return modifier == DRM_FORMAT_MOD_INVALID || it->contains(modifier);
    }
    case MultiGpuImportMode::DumbBuffer:
        return formats.contains(surface->importDumbSwapchain->format());
    case MultiGpuImportMode::Egl: {
        const uint32_t format = surface->importGbmSwapchain->format();
        const auto it = formats.constFind(format);
        if (it == formats.cend()) [[unlikely]] {
            return false;
        }
        const uint64_t modifier = surface->importGbmSwapchain->modifier();
        return modifier == DRM_FORMAT_MOD_INVALID || it->contains(modifier);
    }
    }
    Q_UNREACHABLE();
}

std::unique_ptr<EglGbmLayerSurface::Surface> EglGbmLayerSurface::createSurface(const QSize &size, const QHash<uint32_t, QList<uint64_t>> &formats, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const
{
    const QList<FormatInfo> sortedFormats = OutputLayer::filterAndSortFormats(formats, requiredAlphaBits, tradeoff);

    auto bufferTarget = m_requestedBufferTarget;
    if (m_gpu == m_eglBackend->gpu()) [[likely]] {
        const bool needsLinear = std::ranges::all_of(sortedFormats, [&formats](const FormatInfo &fmt) {
            const auto &mods = formats[fmt.drmFormat];
            return std::ranges::all_of(mods, [](uint64_t mod) {
                return mod == DRM_FORMAT_MOD_LINEAR;
            });
        });

        if (needsLinear) [[unlikely]] {
            const auto renderFormats = m_eglBackend->eglDisplayObject()->allSupportedDrmFormats();
            const bool noLinearSupport = std::ranges::none_of(sortedFormats, [&renderFormats](const FormatInfo &formatInfo) {
                const auto it = renderFormats.constFind(formatInfo.drmFormat);
                return it != renderFormats.cend() && it->nonExternalOnlyModifiers.contains(DRM_FORMAT_MOD_LINEAR);
            });
            if (noLinearSupport) {
                bufferTarget = BufferTarget::Dumb;
            }
        }
    }

    const auto doTestFormats = [&](const QList<FormatInfo> &gbmFormats, MultiGpuImportMode importMode) -> std::unique_ptr<Surface> {
        for (const auto &formatInfo : gbmFormats) {
            const auto formatIt = formats.constFind(formatInfo.drmFormat);
            if (formatIt == formats.cend()) [[unlikely]] {
                continue;
            }

            if (auto surface = createSurface(size, formatInfo.drmFormat, *formatIt, importMode, bufferTarget, tradeoff, requiredAlphaBits)) [[likely]] {
                return surface;
            }
        }
        return nullptr;
    };

    if (m_gpu == m_eglBackend->gpu()) [[likely]] {
        return doTestFormats(sortedFormats, MultiGpuImportMode::None);
    }

    const auto display = m_eglBackend->displayForGpu(m_gpu);
    if (display && !display->renderNode().isEmpty() && display->renderNode() == m_eglBackend->eglDisplayObject()->renderNode()) [[unlikely]] {
        if (auto surface = doTestFormats(sortedFormats, MultiGpuImportMode::None)) {
            return surface;
        }
    }

    if (auto surface = doTestFormats(sortedFormats, MultiGpuImportMode::Egl)) {
        qCDebug(KWIN_DRM) << "chose egl import with format" << formatName(surface->gbmSwapchain->format()).name << "and modifier" << surface->gbmSwapchain->modifier();
        return surface;
    }
    if (auto surface = doTestFormats(sortedFormats, MultiGpuImportMode::Dmabuf)) {
        qCDebug(KWIN_DRM) << "chose dmabuf import with format" << formatName(surface->gbmSwapchain->format()).name << "and modifier" << surface->gbmSwapchain->modifier();
        return surface;
    }
    if (auto surface = doTestFormats(sortedFormats, MultiGpuImportMode::LinearDmabuf)) {
        qCDebug(KWIN_DRM) << "chose linear dmabuf import with format" << formatName(surface->gbmSwapchain->format()).name << "and modifier" << surface->gbmSwapchain->modifier();
        return surface;
    }
    if (auto surface = doTestFormats(sortedFormats, MultiGpuImportMode::DumbBuffer)) {
        qCDebug(KWIN_DRM) << "chose cpu import with format" << formatName(surface->gbmSwapchain->format()).name << "and modifier" << surface->gbmSwapchain->modifier();
        return surface;
    }

    return nullptr;
}

static QList<uint64_t> filterModifiers(const QList<uint64_t> &one, const QList<uint64_t> &two)
{
    QList<uint64_t> ret;
    ret.reserve(std::min(one.size(), two.size()));

    for (uint64_t mod : one) {
        if (two.contains(mod)) {
            ret.append(mod);
        }
    }

    return ret;
}

std::unique_ptr<EglGbmLayerSurface::Surface> EglGbmLayerSurface::createSurface(const QSize &size, uint32_t format, const QList<uint64_t> &modifiers, MultiGpuImportMode importMode, BufferTarget bufferTarget, Output::ColorPowerTradeoff tradeoff, uint32_t requiredAlphaBits) const
{
    const bool cpuCopy = (importMode == MultiGpuImportMode::DumbBuffer) || (bufferTarget == BufferTarget::Dumb);

    auto ret = std::make_unique<Surface>();
    ret->bufferTarget = bufferTarget;
    ret->importMode = importMode;
    ret->tradeoff = tradeoff;
    ret->requiredAlphaBits = requiredAlphaBits;

    const auto allFormats = m_eglBackend->eglDisplayObject()->allSupportedDrmFormats();
    const auto drmFormatIt = allFormats.constFind(format);
    if (drmFormatIt == allFormats.cend()) [[unlikely]] {
        return nullptr;
    }
    const auto &drmFormat = *drmFormatIt;

    QList<uint64_t> renderModifiers;

    if (importMode == MultiGpuImportMode::Egl) [[unlikely]] {
        ret->importContext = m_eglBackend->contextForGpu(m_gpu);
        if (!ret->importContext || ret->importContext->isSoftwareRenderer()) {
            return nullptr;
        }

        const auto importFormats = ret->importContext->displayObject()->allSupportedDrmFormats();
        const auto importFormatIt = importFormats.constFind(format);
        if (importFormatIt == importFormats.cend()) [[unlikely]] {
            return nullptr;
        }

        renderModifiers = filterModifiers(importFormatIt->allModifiers, drmFormat.nonExternalOnlyModifiers);
        renderModifiers.removeAll(DRM_FORMAT_MOD_INVALID);
    } else if (cpuCopy) [[unlikely]] {
        if (!cpuCopyFormats.contains(format)) {
            return nullptr;
        }
        renderModifiers = drmFormat.nonExternalOnlyModifiers;
    } else {
        renderModifiers = filterModifiers(modifiers, drmFormat.nonExternalOnlyModifiers);
    }

    if (renderModifiers.empty()) {
        return nullptr;
    }

    ret->context = m_eglBackend->contextForGpu(m_eglBackend->gpu());
    ret->gbmSwapchain = createGbmSwapchain(m_eglBackend->gpu(), m_eglBackend->openglContext(), size, format, renderModifiers, importMode, bufferTarget);

    if (!ret->gbmSwapchain) {
        return nullptr;
    }

    if (cpuCopy) [[unlikely]] {
        ret->importDumbSwapchain = std::make_unique<QPainterSwapchain>(m_gpu->drmDevice()->allocator(), size, format);
    } else if (importMode == MultiGpuImportMode::Egl) [[unlikely]] {
        ret->importGbmSwapchain = createGbmSwapchain(m_gpu, ret->importContext.get(), size, format, modifiers, MultiGpuImportMode::None, BufferTarget::Normal);
        if (!ret->importGbmSwapchain) {
            return nullptr;
        }
    }

    if (!doRenderTestBuffer(ret.get())) {
        return nullptr;
    }

    return ret;
}

std::shared_ptr<EglSwapchain> EglGbmLayerSurface::createGbmSwapchain(DrmGpu *gpu, EglContext *context, const QSize &size, uint32_t format, const QList<uint64_t> &modifiers, MultiGpuImportMode importMode, BufferTarget bufferTarget) const
{
    static bool modifiersEnvSet = false;
    static const bool modifiersEnv = qEnvironmentVariableIntValue("KWIN_DRM_USE_MODIFIERS", &modifiersEnvSet) != 0;

    const bool allowModifiers = (m_gpu->addFB2ModifiersSupported()
                               || importMode == MultiGpuImportMode::Egl
                               || importMode == MultiGpuImportMode::DumbBuffer)
                              && (!modifiersEnvSet || modifiersEnv)
                              && (modifiers != implicitModifier);

#if !HAVE_GBM_BO_GET_FD_FOR_PLANE
    const bool modifiersAllowed = allowModifiers && (m_gpu == gpu);
#else
    const bool modifiersAllowed = allowModifiers;
#endif

    const bool linearSupported = modifiers.contains(DRM_FORMAT_MOD_LINEAR);
    const bool preferLinear = (importMode == MultiGpuImportMode::DumbBuffer);
    const bool forceLinear = (importMode == MultiGpuImportMode::LinearDmabuf)
                          || ((importMode != MultiGpuImportMode::None)
                           && (importMode != MultiGpuImportMode::DumbBuffer)
                           && !modifiersAllowed);

    if (forceLinear && !linearSupported) {
        return nullptr;
    }

    if (linearSupported && (preferLinear || forceLinear)) [[unlikely]] {
        if (auto swapchain = EglSwapchain::create(gpu->drmDevice()->allocator(), context, size, format, linearModifier)) {
            return swapchain;
        }
        if (forceLinear) {
            return nullptr;
        }
    }

    if (modifiersAllowed) [[likely]] {
        if (auto swapchain = EglSwapchain::create(gpu->drmDevice()->allocator(), context, size, format, modifiers)) {
            return swapchain;
        }
    }

    return EglSwapchain::create(gpu->drmDevice()->allocator(), context, size, format, implicitModifier);
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::doRenderTestBuffer(Surface *surface) const
{
    auto slot = surface->gbmSwapchain->acquire();
    if (!slot) [[unlikely]] {
        return nullptr;
    }

    if (!m_gpu->atomicModeSetting()) [[unlikely]] {
        EglContext::currentContext()->pushFramebuffer(slot->framebuffer());
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
        glClear(GL_COLOR_BUFFER_BIT);
        EglContext::currentContext()->popFramebuffer();
    }

    const auto ret = importBuffer(surface, slot.get(), FileDescriptor{}, nullptr, infiniteRegion());
    if (!ret) [[unlikely]] {
        return nullptr;
    }

    surface->importDamageJournal.clear();
    surface->currentSlot = std::move(slot);
    surface->currentFramebuffer = ret;
    return ret;
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::importBuffer(Surface *surface, EglSwapchainSlot *slot, FileDescriptor &&readFence, OutputFrame *frame, const QRegion &damagedRegion) const
{
    const BufferTarget target = surface->bufferTarget;
    const MultiGpuImportMode mode = surface->importMode;

    if (target == BufferTarget::Dumb || mode == MultiGpuImportMode::DumbBuffer) [[unlikely]] {
        return importWithCpu(surface, slot, frame);
    }

    if (mode == MultiGpuImportMode::Egl) [[unlikely]] {
        return importWithEgl(surface, slot->buffer(), std::move(readFence), frame, damagedRegion);
    }

    const auto ret = m_gpu->importBuffer(slot->buffer(), std::move(readFence));
    if (!ret) [[unlikely]] {
        qCWarning(KWIN_DRM, "Failed to create framebuffer: %s", strerror(errno));
    }
    return ret;
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::importWithEgl(Surface *surface, GraphicsBuffer *sourceBuffer, FileDescriptor &&readFence, OutputFrame *frame, const QRegion &damagedRegion) const
{
    Q_ASSERT(surface->importGbmSwapchain);

    const auto display = m_eglBackend->displayForGpu(m_gpu);
    const bool hasNativeFence = display->supportsNativeFence();

    if (!readFence.isValid() || !hasNativeFence) [[unlikely]] {
        glFinish();
    }

    if (!surface->importContext->makeCurrent()) [[unlikely]] {
        qCWarning(KWIN_DRM, "Failed to make import context current");
        surface->needsRecreation = true;
        m_eglBackend->resetContextForGpu(m_gpu);
        return nullptr;
    }

    const auto restoreContext = qScopeGuard([this]() {
        m_eglBackend->openglContext()->makeCurrent();
    });

    if (surface->importContext->checkGraphicsResetStatus() != GL_NO_ERROR) [[unlikely]] {
        qCWarning(KWIN_DRM, "Detected GPU reset on secondary GPU %s", qPrintable(m_gpu->drmDevice()->path()));
        surface->needsRecreation = true;
        m_eglBackend->resetContextForGpu(m_gpu);
        return nullptr;
    }

    std::unique_ptr<GLRenderTimeQuery> renderTime;
    if (frame) [[likely]] {
        renderTime = std::make_unique<GLRenderTimeQuery>(surface->importContext);
        renderTime->begin();
    }

    if (readFence.isValid()) [[likely]] {
        const auto destinationFence = EGLNativeFence::importFence(surface->importContext->displayObject(), std::move(readFence));
        destinationFence.waitSync();
    }

    auto &sourceTexture = surface->importedTextureCache[sourceBuffer];
    if (!sourceTexture) {
        sourceTexture = surface->importContext->importDmaBufAsTexture(*sourceBuffer->dmabufAttributes());
    }
    if (!sourceTexture) [[unlikely]] {
        qCWarning(KWIN_DRM, "failed to import the source texture!");
        return nullptr;
    }

    auto slot = surface->importGbmSwapchain->acquire();
    if (!slot) [[unlikely]] {
        qCWarning(KWIN_DRM, "failed to import the local texture!");
        return nullptr;
    }

    const QSize swapchainSize = surface->gbmSwapchain->size();
    QRegion deviceDamage;

    if (damagedRegion == infiniteRegion()) {
        deviceDamage = QRect(QPoint(), swapchainSize);
    } else {
        const auto mapping = surface->currentSlot->framebuffer()->colorAttachment()->contentTransform().combine(OutputTransform::FlipY);
        const QSize rotatedSize = mapping.map(swapchainSize);
        const QRect bounds(QPoint(), swapchainSize);

        for (const QRect &rect : damagedRegion) {
            deviceDamage |= mapping.map(scaledRect(rect, surface->scale), rotatedSize).toAlignedRect() & bounds;
        }
    }

    const QRect swapchainBounds(QPoint(), swapchainSize);
    const QRegion repaint = (deviceDamage | surface->importDamageJournal.accumulate(slot->age(), infiniteRegion())) & swapchainBounds;
    surface->importDamageJournal.add(deviceDamage);

    GLFramebuffer *fbo = slot->framebuffer();
    surface->importContext->pushFramebuffer(fbo);

    const GLenum textureTarget = sourceTexture->target();
    const auto shader = surface->importContext->shaderManager()->pushShader(
        textureTarget == GL_TEXTURE_EXTERNAL_OES ? ShaderTrait::MapExternalTexture : ShaderTrait::MapTexture);

    QMatrix4x4 mat;
    mat.scale(1.0f, -1.0f);
    mat.ortho(QRect(QPoint(), fbo->size()));
    shader->setUniform(GLShader::Mat4Uniform::ModelViewProjectionMatrix, mat);

    if (const auto vbo = uploadGeometry(repaint, fbo->size())) [[likely]] {
        sourceTexture->bind();
        vbo->render(GL_TRIANGLES);
        sourceTexture->unbind();
    }

    surface->importContext->popFramebuffer();
    surface->importContext->shaderManager()->popShader();
    glFlush();

    EGLNativeFence endFence(display);
    if (!endFence.isValid()) [[unlikely]] {
        glFinish();
    }

    surface->importGbmSwapchain->release(slot, endFence.fileDescriptor().duplicate());

    if (frame) [[likely]] {
        renderTime->end();
        frame->addRenderTimeQuery(std::move(renderTime));
    }

    return m_gpu->importBuffer(slot->buffer(), endFence.takeFileDescriptor());
}

std::shared_ptr<DrmFramebuffer> EglGbmLayerSurface::importWithCpu(Surface *surface, EglSwapchainSlot *source, OutputFrame *frame) const
{
    std::unique_ptr<CpuRenderTimeQuery> copyTime;
    if (frame) [[likely]] {
        copyTime = std::make_unique<CpuRenderTimeQuery>();
    }

    Q_ASSERT(surface->importDumbSwapchain);
    const auto slot = surface->importDumbSwapchain->acquire();
    if (!slot) [[unlikely]] {
        qCWarning(KWIN_DRM) << "EglGbmLayerSurface::importWithCpu: failed to get a target dumb buffer";
        return nullptr;
    }

    const QSize bufferSize = source->buffer()->size();
    const int width = bufferSize.width();
    const int height = bufferSize.height();
    const qsizetype srcStride = 4 * width;

    EglContext *context = m_eglBackend->openglContext();
    GLFramebuffer::pushFramebuffer(source->framebuffer());

    QImage * const dst = slot->view()->image();
    const qsizetype dstStride = dst->bytesPerLine();

    if (dstStride == srcStride) [[likely]] {
        context->glReadnPixels(0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, dst->sizeInBytes(), dst->bits());
    } else {
        if (surface->cpuCopyCache.size() != bufferSize) {
            surface->cpuCopyCache = QImage(bufferSize, QImage::Format_RGBA8888);
        }

        context->glReadnPixels(0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV,
                             surface->cpuCopyCache.sizeInBytes(), surface->cpuCopyCache.bits());

        const uchar *srcBits = surface->cpuCopyCache.constBits();
        uchar *dstBits = dst->bits();

        for (int y = 0; y < height; ++y) {
            std::memcpy(dstBits, srcBits, srcStride);
            srcBits += srcStride;
            dstBits += dstStride;
        }
    }

    GLFramebuffer::popFramebuffer();

    const auto ret = m_gpu->importBuffer(slot->buffer(), FileDescriptor{});
    if (!ret) [[unlikely]] {
        qCWarning(KWIN_DRM, "Failed to create a framebuffer: %s", strerror(errno));
    }

    surface->importDumbSwapchain->release(slot);

    if (frame) [[likely]] {
        copyTime->end();
        frame->addRenderTimeQuery(std::move(copyTime));
    }

    return ret;
}

}

#include "moc_drm_egl_layer_surface.cpp"
