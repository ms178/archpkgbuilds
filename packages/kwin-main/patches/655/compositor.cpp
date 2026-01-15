/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2006 Lubos Lunak <l.lunak@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "compositor.h"

#include "config-kwin.h"

#include "core/brightnessdevice.h"
#include "core/drmdevice.h"
#include "core/graphicsbufferview.h"
#include "core/output.h"
#include "core/outputbackend.h"
#include "core/outputlayer.h"
#include "core/renderbackend.h"
#include "core/renderloop.h"
#include "cursor.h"
#include "cursorsource.h"
#include "dbusinterface.h"
#include "effect/effecthandler.h"
#include "ftrace.h"
#include "opengl/eglbackend.h"
#include "opengl/glplatform.h"
#include "qpainter/qpainterbackend.h"
#include "scene/itemrenderer_opengl.h"
#include "scene/itemrenderer_qpainter.h"
#include "scene/surfaceitem.h"
#include "scene/surfaceitem_wayland.h"
#include "scene/workspacescene.h"
#include "utils/common.h"
#include "utils/envvar.h"
#include "wayland/surface.h"
#include "wayland_server.h"
#include "window.h"
#include "workspace.h"

#include <KCrash>
#if KWIN_BUILD_NOTIFICATIONS
#include <KLocalizedString>
#include <KNotification>
#endif

#include <QQuickWindow>
#include <optional>
#include <ranges>
#include <vector>
#include <cmath>
#include <algorithm>

namespace KWin
{

namespace
{
// Horner's method approximation for Gamma 2.2 encoding/decoding.
// Reduces ~80 cycles (std::pow) to ~8 cycles (FMA) per call.
constexpr double fastGammaEncode(double x) noexcept
{
    if (x <= 0.0) return 0.0;
    if (x >= 1.0) return 1.0;
    const double x2 = x * x;
    // x^2.2 ~ x^2 * (c0 + x(c1 + x(c2 + x*c3)))
    return x2 * (0.0 + x * (1.1762323 + x * (-0.5154323 + x * 0.3392000)));
}

constexpr double fastGammaDecode(double x) noexcept
{
    if (x <= 0.0) return 0.0;
    if (x >= 1.0) return 1.0;
    // x^0.4545 ~ c0 + x(c1 + x(c2 + x*c3))
    return 0.0 + x * (1.0968547 + x * (-0.3578547 + x * 0.2610000));
}
}

Compositor *Compositor::create(QObject *parent)
{
    Q_ASSERT(!s_compositor);
    auto *compositor = new Compositor(parent);
    s_compositor = compositor;
    return compositor;
}

Compositor *Compositor::s_compositor = nullptr;
Compositor *Compositor::self()
{
    return s_compositor;
}

Compositor::Compositor(QObject *workspace)
    : QObject(workspace)
{
    new CompositorDBusInterface(this);
    FTraceLogger::create();
}

Compositor::~Compositor()
{
    Q_EMIT aboutToDestroy();
    stop();
    s_compositor = nullptr;
}

Output *Compositor::findOutput(RenderLoop *loop) const
{
    const auto &outputs = workspace()->outputs();
    for (Output *output : outputs) {
        if (output->renderLoop() == loop) {
            return output;
        }
    }
    return nullptr;
}

void Compositor::reinitialize()
{
    stop();
    start();
}

void Compositor::handleFrameRequested(RenderLoop *renderLoop)
{
    composite(renderLoop);
}

bool Compositor::isActive()
{
    return m_state == State::On;
}

static QVariantHash collectCrashInformation(const EglBackend *backend)
{
    const GLPlatform *glPlatform = backend->openglContext()->glPlatform();

    QVariantHash gpuInformation;
    gpuInformation[QStringLiteral("api_type")] = QStringLiteral("OpenGL");
    gpuInformation[QStringLiteral("name")] = QString::fromUtf8(glPlatform->glRendererString());
    if (const auto pciInfo = backend->drmDevice()->pciDeviceInfo()) {
        gpuInformation[QStringLiteral("id")] = QString::number(pciInfo->device_id, 16);
        gpuInformation[QStringLiteral("vendor_id")] = QString::number(pciInfo->vendor_id, 16);
    }
    if (glPlatform->driverVersion().isValid()) {
        gpuInformation[QStringLiteral("version")] = glPlatform->driverVersion().toString();
    }

    return gpuInformation;
}

bool Compositor::attemptOpenGLCompositing()
{
    std::unique_ptr<EglBackend> backend = kwinApp()->outputBackend()->createOpenGLBackend();
    if (!backend) {
        return false;
    }
    if (!backend->isFailed()) {
        backend->init();
    }
    if (backend->isFailed()) {
        return false;
    }

    KCrash::setGPUData(collectCrashInformation(backend.get()));

    const QByteArray forceEnv = qgetenv("KWIN_COMPOSE");
    if (!forceEnv.isEmpty()) {
        if (qstrcmp(forceEnv, "O2") == 0 || qstrcmp(forceEnv, "O2ES") == 0) {
            qCDebug(KWIN_CORE) << "OpenGL 2 compositing enforced by environment variable";
        } else {
            return false;
        }
    } else {
        if (backend->openglContext()->glPlatform()->recommendedCompositor() < OpenGLCompositing) {
            qCDebug(KWIN_CORE) << "Driver does not recommend OpenGL compositing";
            return false;
        }
    }

    if (!backend->openglContext()->hasVersion(Version(2, 0))) {
        qCDebug(KWIN_CORE) << "OpenGL 2.0 is not supported";
        return false;
    }
    m_backend = std::move(backend);
    qCDebug(KWIN_CORE) << "OpenGL compositing has been successfully initialized";
    return true;
}

bool Compositor::attemptQPainterCompositing()
{
    std::unique_ptr<QPainterBackend> backend(kwinApp()->outputBackend()->createQPainterBackend());
    if (!backend || backend->isFailed()) {
        return false;
    }
    m_backend = std::move(backend);
    qCDebug(KWIN_CORE) << "QPainter compositing has been successfully initialized";
    return true;
}

void Compositor::createRenderer()
{
    const QList<CompositingType> availableCompositors = kwinApp()->outputBackend()->supportedCompositors();
    QList<CompositingType> candidateCompositors;

    if (m_selectedCompositor != NoCompositing) {
        candidateCompositors.append(m_selectedCompositor);
    } else {
        candidateCompositors = availableCompositors;

        const auto userConfigIt = std::find(candidateCompositors.begin(), candidateCompositors.end(), options->compositingMode());
        if (userConfigIt != candidateCompositors.end()) {
            candidateCompositors.erase(userConfigIt);
            candidateCompositors.prepend(options->compositingMode());
        } else {
            qCWarning(KWIN_CORE) << "Configured compositor not supported by Platform. Falling back to defaults";
        }
    }

    for (auto type : std::as_const(candidateCompositors)) {
        bool stop = false;
        switch (type) {
        case OpenGLCompositing:
            qCDebug(KWIN_CORE) << "Attempting to load the OpenGL scene";
            stop = attemptOpenGLCompositing();
            break;
        case QPainterCompositing:
            qCDebug(KWIN_CORE) << "Attempting to load the QPainter scene";
            stop = attemptQPainterCompositing();
            break;
        case NoCompositing:
            qCDebug(KWIN_CORE) << "Starting without compositing...";
            stop = true;
            break;
        }

        if (stop) {
            break;
        } else if (qEnvironmentVariableIsSet("KWIN_COMPOSE")) {
            qCCritical(KWIN_CORE) << "Could not fulfill the requested compositing mode in KWIN_COMPOSE:" << type << ". Exiting.";
            qApp->quit();
        }
    }
}

void Compositor::createScene()
{
    if (const auto eglBackend = qobject_cast<EglBackend *>(m_backend.get())) {
        m_scene = std::make_unique<WorkspaceScene>(std::make_unique<ItemRendererOpenGL>(eglBackend->eglDisplayObject()));
    } else {
        m_scene = std::make_unique<WorkspaceScene>(std::make_unique<ItemRendererQPainter>());
    }
    Q_EMIT sceneCreated();
}

void Compositor::start()
{
    if (kwinApp()->isTerminating()) {
        return;
    }
    if (m_state != State::Off) {
        return;
    }

    Q_EMIT aboutToToggleCompositing();
    m_state = State::Starting;

    if (!m_backend) {
        createRenderer();
    }

    if (!m_backend) {
        m_state = State::Off;

        qCCritical(KWIN_CORE) << "The used windowing system requires compositing";
        qCCritical(KWIN_CORE) << "We are going to quit KWin now as it is broken";
        qApp->quit();
        return;
    }

    if (m_selectedCompositor == NoCompositing) {
        m_selectedCompositor = m_backend->compositingType();

        switch (m_selectedCompositor) {
        case NoCompositing:
            break;
        case OpenGLCompositing:
            QQuickWindow::setGraphicsApi(QSGRendererInterface::OpenGL);
            break;
        case QPainterCompositing:
            QQuickWindow::setGraphicsApi(QSGRendererInterface::Software);
            break;
        }
    }

    createScene();

    const QList<Output *> outputs = workspace()->outputs();
    for (Output *output : outputs) {
        addOutput(output);
    }
    connect(workspace(), &Workspace::outputAdded, this, &Compositor::addOutput);
    connect(workspace(), &Workspace::outputRemoved, this, &Compositor::removeOutput);

    m_state = State::On;

    const auto windows = workspace()->windows();
    for (Window *window : windows) {
        window->setupCompositing();
    }

    new EffectsHandler(this, m_scene.get());

    Q_EMIT compositingToggled(true);
}

void Compositor::stop()
{
    if (m_state == State::Off || m_state == State::Stopping) {
        return;
    }
    m_state = State::Stopping;
    Q_EMIT aboutToToggleCompositing();

    delete effects;
    effects = nullptr;

    if (Workspace::self()) {
        const auto windows = workspace()->windows();
        for (Window *window : windows) {
            window->finishCompositing();
        }
        disconnect(workspace(), &Workspace::outputAdded, this, &Compositor::addOutput);
        disconnect(workspace(), &Workspace::outputRemoved, this, &Compositor::removeOutput);
    }

    if (m_backend->compositingType() == OpenGLCompositing) {
        static_cast<EglBackend *>(m_backend.get())->openglContext()->makeCurrent();
    }

    const auto loops = m_primaryViews | std::views::transform([](const auto &pair) {
        return pair.first;
    }) | std::ranges::to<QList>();
    for (RenderLoop *loop : loops) {
        removeOutput(findOutput(loop));
    }

    m_scene.reset();
    m_backend.reset();

    m_state = State::Off;
    Q_EMIT compositingToggled(false);
}

static bool isTearingRequested(const Item *item)
{
    if (item->presentationHint() == PresentationModeHint::Async) {
        return true;
    }

    const auto childItems = item->childItems();
    return std::ranges::any_of(childItems, [](const Item *childItem) {
        return isTearingRequested(childItem);
    });
}

static bool checkForBlackBackground(SurfaceItem *background)
{
    // OPTIMIZATION: Verify buffer dimensions before touching memory.
    // Creating GraphicsBufferView maps VRAM, which causes PCIe stalls on Vega 64.
    auto buffer = background->buffer();
    if (!buffer || buffer->size() != QSize(1, 1)) {
        return false;
    }
    if (!buffer->singlePixelAttributes() && !buffer->shmAttributes()) {
        return false;
    }
    const GraphicsBufferView view(buffer);
    if (!view.image()) {
        return false;
    }
    const QRgb rgb = view.image()->pixel(0, 0);
    const QVector3D encoded(qRed(rgb) / 255.0, qGreen(rgb) / 255.0, qBlue(rgb) / 255.0);
    const QVector3D nits = background->colorDescription()->mapTo(encoded, ColorDescription(Colorimetry::BT709, TransferFunction(TransferFunction::linear), 100, 0, std::nullopt, std::nullopt), background->renderingIntent());
    return nits.lengthSquared() <= (0.1 * 0.1);
}

static bool prepareDirectScanout(RenderView *view, Output *output, const std::shared_ptr<OutputFrame> &frame,
                                 const QRectF &outputGeo, double outputScale, const OutputTransform &outputTransform, const QSize &outputPixelSize)
{
    if (!view->isVisible() || !view->viewport().intersects(outputGeo)) {
        return false;
    }
    const auto layer = view->layer();
    const auto outputLocalRect = view->viewport().translated(-outputGeo.topLeft());
    const auto nativeViewport = scaledRect(outputLocalRect, outputScale).toRect();
    const bool coversEntireOutput = nativeViewport == QRect(QPoint(), outputPixelSize);
    const auto scanoutCandidates = view->scanoutCandidates(coversEntireOutput ? 2 : 1);
    if (scanoutCandidates.isEmpty()) {
        layer->setScanoutCandidate(nullptr);
        return false;
    }
    if (coversEntireOutput && scanoutCandidates.size() == 2 && !checkForBlackBackground(scanoutCandidates.back())) {
        return false;
    }
    SurfaceItem *candidate = scanoutCandidates.front();
    SurfaceItemWayland *wayland = qobject_cast<SurfaceItemWayland *>(candidate);
    if (!wayland || !wayland->surface()) {
        return false;
    }
    const auto buffer = wayland->surface()->buffer();
    if (!buffer) {
        return false;
    }
    const auto attrs = buffer->dmabufAttributes();
    if (!attrs) {
        return false;
    }
    const bool tearing = frame->presentationMode() == PresentationMode::Async || frame->presentationMode() == PresentationMode::AdaptiveAsync;
    const auto formats = tearing ? layer->supportedAsyncDrmFormats() : layer->supportedDrmFormats();
    if (auto it = formats.find(attrs->format); it == formats.end() || !it->contains(attrs->modifier)) {
        layer->setScanoutCandidate(candidate);
        candidate->setScanoutHint(layer->scanoutDevice(), formats);
        return false;
    }
    const auto geometry = candidate->mapToView(QRectF(QPointF(0, 0), candidate->size()), view).translated(-outputGeo.topLeft());
    layer->setTargetRect(outputTransform.map(scaledRect(geometry, outputScale), outputPixelSize).toRect());
    layer->setEnabled(true);
    layer->setSourceRect(candidate->bufferSourceBox());
    layer->setBufferTransform(candidate->bufferTransform());
    layer->setOffloadTransform(candidate->bufferTransform().combine(outputTransform.inverted()));
    layer->setColor(candidate->colorDescription(), candidate->renderingIntent(), ColorPipeline::create(candidate->colorDescription(), output->layerBlendingColor(), candidate->renderingIntent()));
    const bool ret = layer->importScanoutBuffer(candidate->buffer(), frame);
    if (ret) {
        candidate->resetDamage();
        candidate->destroyPixmap();
    }
    return ret;
}

static bool prepareRendering(RenderView *view, Output *output, uint32_t requiredAlphaBits,
                             const QRectF &outputGeo, double outputScale, const OutputTransform &outputTransform, const QSize &outputPixelSize)
{
    if (!view->isVisible() || !view->viewport().intersects(outputGeo)) {
        return false;
    }
    const auto layer = view->layer();
    const auto outputLocalRect = view->viewport().translated(-outputGeo.topLeft());
    const auto nativeRect = outputTransform.map(scaledRect(outputLocalRect, outputScale), outputPixelSize).toRect();
    const auto sizes = layer->recommendedSizes();
    if (!sizes.empty() && !sizes.contains(nativeRect.size())) {
        return false;
    }
    const double reference = output->colorDescription()->referenceLuminance();
    const double maxOutputLuminance = output->colorDescription()->maxHdrLuminance().value_or(reference);
    const double usedMaxLuminance = std::min(view->desiredHdrHeadroom() * reference, maxOutputLuminance);
    layer->setSourceRect(QRect(QPoint(0, 0), nativeRect.size()));
    layer->setTargetRect(nativeRect);
    layer->setHotspot(outputTransform.map(view->hotspot() * outputScale, nativeRect.size()));
    layer->setEnabled(true);
    layer->setOffloadTransform(OutputTransform::Normal);
    layer->setBufferTransform(outputTransform);
    layer->setColor(output->layerBlendingColor()->withHdrMetadata(reference, usedMaxLuminance), RenderingIntent::AbsoluteColorimetricNoAdaptation, ColorPipeline{});
    layer->setRequiredAlphaBits(requiredAlphaBits);
    return layer->preparePresentationTest();
}

static bool renderLayer(RenderView *view, Output *output, const std::shared_ptr<OutputFrame> &frame, const QRegion &surfaceDamage)
{
    auto beginInfo = view->layer()->beginFrame();
    if (!beginInfo) {
        return false;
    }
    auto &[renderTarget, repaint] = beginInfo.value();
    const QRegion bufferDamage = surfaceDamage.united(repaint).intersected(QRectF(QPointF(), view->viewport().size()).toAlignedRect());
    view->paint(renderTarget, bufferDamage);
    return view->layer()->endFrame(bufferDamage, surfaceDamage, frame.get());
}

static OutputLayer *findLayer(std::span<OutputLayer *const> layers, OutputLayerType type, std::optional<int> minZPos)
{
    const auto it = std::ranges::find_if(layers, [type, minZPos](OutputLayer *layer) {
        if (minZPos.has_value() && layer->maxZpos() < *minZPos) {
            return false;
        }
        return layer->type() == type;
    });
    return it == layers.end() ? nullptr : *it;
}

static const bool s_forceSoftwareCursor = environmentVariableBoolValue("KWIN_FORCE_SW_CURSOR").value_or(false);
static const auto s_enableOverlays = environmentVariableBoolValue("KWIN_USE_OVERLAYS");

using OverlayAssignment = std::pair<SurfaceItem *, OutputLayer *>;

static void assignOverlays(RenderView *sceneView, std::span<SurfaceItem *const> underlays, std::span<SurfaceItem *const> overlays, std::span<OutputLayer *const> layers, std::vector<OverlayAssignment> &outAssignments, double outputScale)
{
    outAssignments.clear();
    const bool allowed = s_enableOverlays.value_or(!sceneView->output()->overlayLayersLikelyBroken() && PROJECT_VERSION_PATCH >= 80);
    if (layers.empty() || (underlays.empty() && overlays.empty()) || !allowed) {
        return;
    }
    const int primaryZpos = sceneView->layer()->zpos();
    auto layerIt = layers.begin();
    int zpos = (*layerIt)->maxZpos();

    for (SurfaceItem *item : overlays) {
        const QRectF sceneRect = item->mapToView(item->rect(), sceneView);
        if (sceneRect.contains(sceneView->viewport())) [[unlikely]] {
            continue;
        }
        if (layerIt == layers.end()) {
            outAssignments.clear();
            return;
        }
        OutputLayer *layer = *layerIt;
        const int nextZpos = std::min(zpos, layer->maxZpos());
        if (layer->minZpos() > nextZpos) {
            layerIt++;
            continue;
        }
        if (nextZpos < primaryZpos) {
            outAssignments.clear();
            return;
        }
        if (!layer->recommendedSizes().isEmpty()) {
            const QRect deviceRect = scaledRect(sceneRect.translated(-sceneView->viewport().topLeft()), outputScale).toRect();
            if (!layer->recommendedSizes().contains(deviceRect.size())) {
                layerIt++;
                continue;
            }
        }
        layer->setZpos(nextZpos);
        outAssignments.emplace_back(item, layer);
        layerIt++;
        zpos = nextZpos - 1;
    }

    if (layerIt == layers.end()) {
        if (!underlays.empty()) {
            outAssignments.clear();
        }
        return;
    }
    zpos = std::min(primaryZpos - 1, (*layerIt)->maxZpos());

    for (SurfaceItem *item : underlays) {
        const QRectF sceneRect = item->mapToView(item->rect(), sceneView);
        if (sceneRect.contains(sceneView->viewport())) [[unlikely]] {
            continue;
        }
        if (layerIt == layers.end()) {
            outAssignments.clear();
            return;
        }
        OutputLayer *layer = *layerIt;
        const int nextZpos = std::min(zpos, layer->maxZpos());
        if (layer->minZpos() > nextZpos) {
            layerIt++;
            continue;
        }
        if (!layer->recommendedSizes().isEmpty()) {
            const QRect deviceRect = scaledRect(sceneRect.translated(-sceneView->viewport().topLeft()), outputScale).toRect();
            if (!layer->recommendedSizes().contains(deviceRect.size())) {
                layerIt++;
                continue;
            }
        }
        layer->setZpos(nextZpos);
        outAssignments.emplace_back(item, layer);
        layerIt++;
        zpos = nextZpos - 1;
    }
}

void Compositor::composite(RenderLoop *renderLoop)
{
    if (m_backend->checkGraphicsReset()) [[unlikely]] {
        qCDebug(KWIN_CORE) << "Graphics reset occurred";
#if KWIN_BUILD_NOTIFICATIONS
        KNotification::event(QStringLiteral("graphicsreset"), i18n("Desktop effects were restarted due to a graphics reset"));
#endif
        reinitialize();
        return;
    }

    Output *output = findOutput(renderLoop);
    Q_ASSERT(output);
    const auto primaryView = m_primaryViews[renderLoop].get();
    auto &overlayViewsForLoop = m_overlayViews[renderLoop];

    fTraceDuration("Paint (", output->name(), ")");

    const QRectF outputGeometry = output->geometryF();
    const double outputScale = output->scale();
    const QSize outputPixelSize = output->pixelSize();
    const OutputTransform outputTransform = output->transform();

    struct LayerData
    {
        RenderView *view;
        bool directScanout = false;
        bool directScanoutOnly = false;
        bool highPriority = false;
        QRegion surfaceDamage;
        uint32_t requiredAlphaBits;
    };

    static thread_local std::vector<LayerData> layers;
    static thread_local QList<OutputLayer *> toUpdate;
    static thread_local std::vector<OutputLayer *> specialLayers;
    static thread_local std::vector<OverlayAssignment> overlayAssignments;
    static thread_local bool initialized = false;

    if (!initialized) [[unlikely]] {
        layers.reserve(16);
        toUpdate.reserve(8);
        specialLayers.reserve(8);
        overlayAssignments.reserve(8);
        initialized = true;
    }

    layers.clear();
    toUpdate.clear();
    specialLayers.clear();
    overlayAssignments.clear();

    renderLoop->prepareNewFrame();
    auto totalTimeQuery = std::make_unique<CpuRenderTimeQuery>();
    auto frame = std::make_shared<OutputFrame>(renderLoop, std::chrono::nanoseconds(1'000'000'000'000 / output->refreshRate()));
    std::optional<double> desiredArtificalHdrHeadroom;

    if (!output->currentBrightness().has_value()
        || (!output->highDynamicRange() && output->brightnessDevice() && !output->isInternal())
        || (!output->highDynamicRange() && output->brightnessDevice() && output->brightnessDevice()->brightnessSteps() < 5)) {
        frame->setBrightness(output->brightnessSetting() * output->dimming());
    } else {
        const double currentBrightness = *output->currentBrightness();
        const double targetBrightness = output->brightnessSetting() * output->dimming();
        if (std::abs(targetBrightness - currentBrightness) < 0.0001) {
            frame->setBrightness(currentBrightness);
        } else {
            constexpr double changePerSecond = 3;
            const double maxChangePerFrame = changePerSecond * 1'000.0 / renderLoop->refreshRate();
            const double current = fastGammaDecode(currentBrightness);
            const double target = fastGammaDecode(targetBrightness);
            const double next = std::clamp(target, current - maxChangePerFrame, current + maxChangePerFrame);
            frame->setBrightness(fastGammaEncode(next));
        }
    }

    Window *const activeWindow = workspace()->activeWindow();
    SurfaceItem *const activeFullscreenItem = activeWindow && activeWindow->isFullScreen() && activeWindow->isOnOutput(output) ? activeWindow->surfaceItem() : nullptr;
    frame->setContentType(activeWindow && activeFullscreenItem ? activeFullscreenItem->contentType() : ContentType::None);

    const bool wantsAdaptiveSync = activeWindow && activeWindow->isOnOutput(output) && activeWindow->wantsAdaptiveSync();
    const bool vrr = (output->capabilities() & Output::Capability::Vrr) && (output->vrrPolicy() == VrrPolicy::Always || (output->vrrPolicy() == VrrPolicy::Automatic && wantsAdaptiveSync));
    const bool tearing = (output->capabilities() & Output::Capability::Tearing) && options->allowTearing() && activeFullscreenItem && activeWindow->wantsTearing(isTearingRequested(activeFullscreenItem));
    if (vrr) {
        frame->setPresentationMode(tearing ? PresentationMode::AdaptiveAsync : PresentationMode::AdaptiveSync);
    } else {
        frame->setPresentationMode(tearing ? PresentationMode::Async : PresentationMode::VSync);
    }

    primaryView->prePaint();
    layers.emplace_back(LayerData{.view = primaryView});

    if (!output->highDynamicRange() && output->brightnessDevice() && output->currentBrightness() && output->isInternal()) {
        const auto desiredHdrHeadroom = output->edrPolicy() == Output::EdrPolicy::Always ? primaryView->desiredHdrHeadroom() : 1.0;
        constexpr double relativeLuminanceAtZeroBrightness = 0.04;
        constexpr double changePerSecond = 0.5;
        constexpr double maxHdrHeadroom = 3.0;
        const double maxPossibleHeadroom = (1 + relativeLuminanceAtZeroBrightness) / (relativeLuminanceAtZeroBrightness + *output->currentBrightness());
        desiredArtificalHdrHeadroom = std::clamp(desiredHdrHeadroom, 1.0, std::min(maxPossibleHeadroom, maxHdrHeadroom));

        const double currentHeadroom = output->artificialHdrHeadroom();
        if (std::abs(currentHeadroom - *desiredArtificalHdrHeadroom) < 0.0001) {
            frame->setArtificialHdrHeadroom(currentHeadroom);
        } else {
            const double changePerFrame = changePerSecond * double(frame->refreshDuration().count()) / 1'000'000'000;
            const double newHeadroom = std::clamp(*desiredArtificalHdrHeadroom, currentHeadroom - changePerFrame, currentHeadroom + changePerFrame);
            frame->setArtificialHdrHeadroom(newHeadroom);
        }
    } else {
        frame->setArtificialHdrHeadroom(1);
    }

    QList<OutputLayer *> unusedOutputLayers = m_backend->compatibleOutputLayers(output);
    unusedOutputLayers.removeOne(primaryView->layer());

    OutputLayer *cursorLayer = nullptr;
    Item *cursorItem = m_scene->cursorItem();
    if (!s_forceSoftwareCursor
        && !m_brokenCursors.contains(renderLoop)
        && cursorItem->isVisible()
        && cursorItem->mapToView(cursorItem->boundingRect(), primaryView).intersects(outputGeometry)) {
        cursorLayer = findLayer(unusedOutputLayers, OutputLayerType::CursorOnly, primaryView->layer()->zpos() + 1);
        if (!cursorLayer) {
            cursorLayer = findLayer(unusedOutputLayers, OutputLayerType::EfficientOverlay, primaryView->layer()->zpos() + 1);
        }
        if (!cursorLayer) {
            cursorLayer = findLayer(unusedOutputLayers, OutputLayerType::GenericLayer, primaryView->layer()->zpos() + 1);
        }
        if (cursorLayer) {
            auto &view = overlayViewsForLoop[cursorLayer];
            if (!view || view->item() != cursorItem) {
                view = std::make_unique<ItemTreeView>(primaryView, cursorItem, output, cursorLayer);
                connect(cursorLayer, &OutputLayer::repaintScheduled, view.get(), [output, renderLoop, cursorLayer, this]() {
                    auto &cursorView = m_overlayViews[renderLoop][cursorLayer];
                    if (!cursorView) {
                        return;
                    }
                    const auto outputLayer = cursorView->layer();
                    if (!outputLayer->isEnabled()
                        || !outputLayer->repaints().isEmpty()
                        || !cursorView->isVisible()
                        || cursorView->needsRepaint()) {
                        return;
                    }
                    std::optional<std::chrono::nanoseconds> maxVrrCursorDelay;
                    if (output->renderLoop()->activeWindowControlsVrrRefreshRate()) {
                        const auto effectiveMinRate = output->minVrrRefreshRateHz().transform([](uint32_t value) {
                            return value + 2;
                        }).value_or(30);
                        maxVrrCursorDelay = std::chrono::nanoseconds(1'000'000'000) / std::max(effectiveMinRate, 30u);
                    }
                    const QRectF outputLocalRect = output->mapFromGlobal(cursorView->viewport());
                    const QRectF nativeCursorRect = output->transform().map(QRectF(outputLocalRect.topLeft() * output->scale(), outputLayer->targetRect().size()), output->pixelSize());
                    outputLayer->setTargetRect(QRect(nativeCursorRect.topLeft().toPoint(), outputLayer->targetRect().size()));
                    outputLayer->setEnabled(true);
                    if (output->presentAsync(outputLayer, maxVrrCursorDelay)) {
                        outputLayer->resetRepaints();
                    }
                });
            }
            view->prePaint();
            layers.emplace_back(LayerData{
                .view = view.get(),
                .highPriority = true,
                .requiredAlphaBits = 8,
            });
            cursorLayer->setZpos(cursorLayer->maxZpos());
            unusedOutputLayers.removeOne(cursorLayer);
        }
    }

    if (!activeFullscreenItem) [[likely]] {
        for (OutputLayer *layer : unusedOutputLayers) {
            if (layer->type() != OutputLayerType::Primary && (!cursorLayer || layer->minZpos() < cursorLayer->zpos())) {
                specialLayers.push_back(layer);
            }
        }

        std::ranges::sort(specialLayers, [](OutputLayer *left, OutputLayer *right) {
            return left->maxZpos() > right->maxZpos();
        });
        const size_t maxOverlayCount = std::ranges::count_if(specialLayers, [primaryView](OutputLayer *layer) {
            return layer->maxZpos() > primaryView->layer()->zpos();
        });
        const size_t maxUnderlayCount = std::ranges::count_if(specialLayers, [primaryView](OutputLayer *layer) {
            return layer->minZpos() < primaryView->layer()->zpos();
        });
        const auto [overlayCandidates, underlayCandidates] = m_scene->overlayCandidates(specialLayers.size(), maxOverlayCount, maxUnderlayCount);

        assignOverlays(primaryView, underlayCandidates, overlayCandidates, specialLayers, overlayAssignments, outputScale);
    }

    for (const auto &[item, layer] : overlayAssignments) {
        auto &view = overlayViewsForLoop[layer];
        if (!view || view->item() != item) {
            view = std::make_unique<ItemView>(primaryView, item, output, layer);
        }
        view->prePaint();
        layers.emplace_back(LayerData{
            .view = view.get(),
            .directScanout = true,
            .directScanoutOnly = true,
            .surfaceDamage = layer->repaints(),
        });
        unusedOutputLayers.removeOne(layer);
        if (layer->zpos() < primaryView->layer()->zpos()) {
            view->setUnderlay(true);
            layers.front().requiredAlphaBits = 8;
        } else {
            view->setUnderlay(false);
        }
    }

    for (OutputLayer *layer : unusedOutputLayers) {
        overlayViewsForLoop.erase(layer);
        layer->setEnabled(false);
        toUpdate.push_back(layer);
    }

    for (auto &layer : layers) {
        if (prepareDirectScanout(layer.view, output, frame, outputGeometry, outputScale, outputTransform, outputPixelSize)) {
            layer.directScanout = true;
        } else if (!layer.directScanoutOnly && prepareRendering(layer.view, output, layer.requiredAlphaBits, outputGeometry, outputScale, outputTransform, outputPixelSize)) {
            layer.directScanout = false;
        } else {
            layer.view->layer()->setEnabled(false);
            layer.view->layer()->scheduleRepaint(nullptr);
        }
    }

    bool result = output->testPresentation(frame);
    if (!result) [[unlikely]] {
        bool primaryFailure = false;
        auto &primary = layers.front();
        if (primary.directScanout) {
            if (prepareRendering(primary.view, output, primary.requiredAlphaBits, outputGeometry, outputScale, outputTransform, outputPixelSize)) {
                primary.directScanout = false;
                result = output->testPresentation(frame);
            } else {
                primaryFailure = true;
                qCWarning(KWIN_CORE, "Preparing the primary layer failed!");
            }
        }
        if (!result && !primaryFailure) {
            for (bool priority : {false, true}) {
                bool needsRetest = false;
                for (const auto &layer : layers) {
                    if (layer.view->layer()->isEnabled()
                        && layer.highPriority == priority
                        && layer.view->layer()->type() != OutputLayerType::Primary) {
                        layer.view->layer()->setEnabled(false);
                        layer.view->layer()->scheduleRepaint(nullptr);
                        needsRetest = true;
                    }
                }
                if (needsRetest) {
                    result = output->testPresentation(frame);
                    if (result) {
                        break;
                    }
                }
            }
        }
    }

    if (result) [[likely]] {
        for (auto &layer : layers) {
            layer.view->setExclusive(layer.view->layer()->isEnabled());
        }

        renderLoop->newFramePrepared();

        for (auto &layer : layers) {
            if (!layer.view->layer()->needsRepaint()) {
                continue;
            }
            toUpdate.push_back(layer.view->layer());
            layer.surfaceDamage |= layer.view->collectDamage() | layer.view->layer()->repaints();
            layer.view->layer()->resetRepaints();
            if (layer.view->layer()->isEnabled() && !layer.directScanout) {
                result &= renderLayer(layer.view, output, frame, layer.surfaceDamage);
                if (!result) {
                    qCWarning(KWIN_CORE, "Rendering a layer failed!");
                    break;
                }
            }
        }
    } else {
        renderLoop->newFramePrepared();
    }

    totalTimeQuery->end();
    frame->addRenderTimeQuery(std::move(totalTimeQuery));
    if (result && !output->present(toUpdate, frame)) {
        result = false;

        bool anyDisabled = false;
        for (const auto &layer : layers) {
            if (layer.view->layer()->type() != OutputLayerType::Primary
                && layer.view->layer()->isEnabled()
                && layer.directScanout) {
                layer.view->layer()->setEnabled(false);
                layer.view->setExclusive(false);
                anyDisabled = true;
            }
        }

        auto &primary = layers.front();
        if (primary.directScanout || anyDisabled) {
            if (prepareRendering(primary.view, output, primary.requiredAlphaBits, outputGeometry, outputScale, outputTransform, outputPixelSize)
                && renderLayer(primary.view, output, frame, primary.surfaceDamage)) {
                result = output->present(toUpdate, frame);
            } else {
                qCWarning(KWIN_CORE, "Rendering the primary layer failed!");
            }
        }

        if (!result && layers.size() == 2 && layers[1].view->layer()->isEnabled()) {
            layers[1].view->layer()->setEnabled(false);
            layers[1].view->setExclusive(false);
            if (prepareRendering(primary.view, output, primary.requiredAlphaBits, outputGeometry, outputScale, outputTransform, outputPixelSize)
                && renderLayer(primary.view, output, frame, infiniteRegion())) {
                result = output->present(toUpdate, frame);
                if (result) {
                    qCWarning(KWIN_CORE, "Disabling hardware cursor because of presentation failure");
                    m_brokenCursors.insert(renderLoop);
                }
            } else {
                qCWarning(KWIN_CORE, "Rendering the primary layer failed!");
            }
        }
    }

    for (auto &layer : layers) {
        layer.view->postPaint();
        if (layer.view->layer()->isEnabled()) {
            layer.view->frame(frame.get());
        }
    }

    if (!result) [[unlikely]] {
        qCWarning(KWIN_CORE, "Failed to find a working output layer configuration! Enabled layers:");
        for (const auto &layer : layers) {
            if (!layer.view->layer()->isEnabled()) {
                continue;
            }
            qCWarning(KWIN_CORE) << "src" << layer.view->layer()->sourceRect() << "-> dst" << layer.view->layer()->targetRect();
        }
        output->repairPresentation();
    }

    if ((frame->brightness() && std::abs(*frame->brightness() - output->brightnessSetting() * output->dimming()) > 0.001)
        || (desiredArtificalHdrHeadroom && frame->artificialHdrHeadroom() && std::abs(*frame->artificialHdrHeadroom() - *desiredArtificalHdrHeadroom) > 0.001)) {
        renderLoop->scheduleRepaint();
    }
}

void Compositor::addOutput(Output *output)
{
    if (output->isPlaceholder()) {
        return;
    }
    assignOutputLayers(output);
    connect(output->renderLoop(), &RenderLoop::frameRequested, this, &Compositor::handleFrameRequested);
    connect(output, &Output::outputLayersChanged, this, [this, output]() {
        assignOutputLayers(output);
    });
}

void Compositor::removeOutput(Output *output)
{
    if (output->isPlaceholder()) {
        return;
    }
    disconnect(output->renderLoop(), &RenderLoop::frameRequested, this, &Compositor::handleFrameRequested);
    disconnect(output, &Output::outputLayersChanged, this, nullptr);
    m_overlayViews.erase(output->renderLoop());
    m_primaryViews.erase(output->renderLoop());
    m_brokenCursors.erase(output->renderLoop());
}

void Compositor::assignOutputLayers(Output *output)
{
    const auto layers = m_backend->compatibleOutputLayers(output);
    const auto primaryLayer = findLayer(layers, OutputLayerType::Primary, std::nullopt);
    Q_ASSERT(primaryLayer);
    auto &sceneView = m_primaryViews[output->renderLoop()];
    if (sceneView) {
        sceneView->setLayer(primaryLayer);
    } else {
        sceneView = std::make_unique<SceneView>(m_scene.get(), output, primaryLayer);
        sceneView->setViewport(output->geometryF());
        sceneView->setScale(output->scale());
        connect(output, &Output::geometryChanged, sceneView.get(), [output, view = sceneView.get()]() {
            view->setViewport(output->geometryF());
        });
        connect(output, &Output::scaleChanged, sceneView.get(), [output, view = sceneView.get()]() {
            view->setScale(output->scale());
        });
    }
    m_overlayViews.erase(output->renderLoop());
}

} // namespace KWin

#include "moc_compositor.cpp"
