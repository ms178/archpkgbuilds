/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2006 Lubos Lunak <l.lunak@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

/*
 Design:

 When compositing is turned on, XComposite extension is used to redirect
 drawing of windows to pixmaps and XDamage extension is used to get informed
 about damage (changes) to window contents. This code is mostly in composite.cpp .

 Compositor::performCompositing() starts one painting pass. Painting is done
 by painting the screen, which in turn paints every window. Painting can be affected
 using effects, which are chained. E.g. painting a screen means that actually
 paintScreen() of the first effect is called, which possibly does modifications
 and calls next effect's paintScreen() and so on, until Scene::finalPaintScreen()
 is called.

 There are 3 phases of every paint (not necessarily done together):
 The pre-paint phase, the paint phase and the post-paint phase.

 The pre-paint phase is used to find out about how the painting will be actually
 done (i.e. what the effects will do). For example when only a part of the screen
 needs to be updated and no effect will do any transformation it is possible to use
 an optimized paint function. How the painting will be done is controlled
 by the mask argument, see PAINT_WINDOW_* and PAINT_SCREEN_* flags in scene.h .
 For example an effect that decides to paint a normal windows as translucent
 will need to modify the mask in its prePaintWindow() to include
 the PAINT_WINDOW_TRANSLUCENT flag. The paintWindow() function will then get
 the mask with this flag turned on and will also paint using transparency.

 The paint pass does the actual painting, based on the information collected
 using the pre-paint pass. After running through the effects' paintScreen()
 either paintGenericScreen() or optimized paintSimpleScreen() are called.
 Those call paintWindow() on windows (not necessarily all), possibly using
 clipping to optimize performance and calling paintWindow() first with only
 PAINT_WINDOW_OPAQUE to paint the opaque parts and then later
 with PAINT_WINDOW_TRANSLUCENT to paint the transparent parts. Function
 paintWindow() again goes through effects' paintWindow() until
 finalPaintWindow() is called, which calls the window's performPaint() to
 do the actual painting.

 The post-paint can be used for cleanups and is also used for scheduling
 repaints during the next painting pass for animations. Effects wanting to
 repaint certain parts can manually damage them during post-paint and repaint
 of these parts will be done during the next paint pass.
*/

#include "scene/workspacescene.h"
#include "compositor.h"
#include "core/backendoutput.h"
#include "core/graphicsbufferview.h"
#include "core/output.h"
#include "core/pixelgrid.h"
#include "core/renderbackend.h"
#include "core/renderloop.h"
#include "core/renderviewport.h"
#include "cursoritem.h"
#include "effect/effecthandler.h"
#include "opengl/eglbackend.h"
#include "opengl/eglcontext.h"
#include "scene/backgroundeffectitem.h"
#include "scene/decorationitem.h"
#include "scene/dndiconitem.h"
#include "scene/itemrenderer.h"
#include "scene/rootitem.h"
#include "scene/surfaceitem.h"
#include "scene/windowitem.h"
#include "utils/envvar.h"
#include "wayland/seat.h"
#include "wayland_server.h"
#include "window.h"
#include "workspace.h"

#include <QScopeGuard>
#include <QtMath>

#include <algorithm>
#include <chrono>
#include <limits>
#include <optional>
#include <ranges>

namespace KWin
{

//****************************************
// Scene
//****************************************

WorkspaceScene::WorkspaceScene(std::unique_ptr<ItemRenderer> renderer)
    : Scene(std::move(renderer))
    , m_containerItem(std::make_unique<RootItem>(this))
    , m_overlayItem(std::make_unique<RootItem>(this))
    , m_cursorItem(std::make_unique<CursorItem>(m_overlayItem.get()))
{
    setGeometry(workspace()->geometry());
    connect(workspace(), &Workspace::geometryChanged, this, [this]() {
        setGeometry(workspace()->geometry());
    });

    connect(waylandServer()->seat(), &SeatInterface::dragStarted, this, &WorkspaceScene::createDndIconItem);
    connect(waylandServer()->seat(), &SeatInterface::dragEnded, this, &WorkspaceScene::destroyDndIconItem);

    m_cursorItem->setZ(1);
    connect(Cursors::self(), &Cursors::hiddenChanged, this, &WorkspaceScene::updateCursor);
    connect(Cursors::self(), &Cursors::positionChanged, this, &WorkspaceScene::updateCursor);
    updateCursor();
}

WorkspaceScene::~WorkspaceScene() = default;

void WorkspaceScene::createDndIconItem()
{
    DragAndDropIcon *dragIcon = waylandServer()->seat()->dragIcon();
    if (!dragIcon) {
        return;
    }

    m_dndIcon = std::make_unique<DragAndDropIconItem>(dragIcon, m_overlayItem.get());

    const auto updatePosition = [this]() {
        const auto position = waylandServer()->seat()->dragPosition();
        m_dndIcon->setPosition(position);
        m_dndIcon->setOutput(workspace()->outputAt(position));
    };

    updatePosition();
    connect(waylandServer()->seat(), &SeatInterface::dragMoved, m_dndIcon.get(), updatePosition);
}

void WorkspaceScene::destroyDndIconItem()
{
    m_dndIcon.reset();
}

void WorkspaceScene::updateCursor()
{
    if (Cursors::self()->isCursorHidden()) {
        m_cursorItem->setVisible(false);
    } else {
        m_cursorItem->setVisible(true);
        m_cursorItem->setPosition(Cursors::self()->currentCursor()->pos());
    }
}

Item *WorkspaceScene::containerItem() const
{
    return m_containerItem.get();
}

Item *WorkspaceScene::overlayItem() const
{
    return m_overlayItem.get();
}

Item *WorkspaceScene::cursorItem() const
{
    return m_cursorItem.get();
}

struct ClipCorner
{
    RectF box;
    BorderRadius radius;
};

static std::optional<ClipCorner> calculateClipCorner(Item *item, const std::optional<ClipCorner> &parentClip)
{
    if (!item->borderRadius().isNull()) {
        return ClipCorner{
            .box = item->rect(),
            .radius = item->borderRadius(),
        };
    }

    if (parentClip.has_value()) {
        return ClipCorner{
            .box = item->transform().inverted().mapRect(parentClip->box.translated(-item->position())),
            .radius = parentClip->radius,
        };
    }

    return std::nullopt;
}

static bool pushClipCorner(Item *item, QStack<ClipCorner> &corners)
{
    std::optional<ClipCorner> parentClip;
    if (!corners.isEmpty()) {
        parentClip = corners.top();
    }

    if (const std::optional<ClipCorner> clip = calculateClipCorner(item, parentClip)) {
        corners.push(*clip);
        return true;
    }

    return false;
}

static bool addCandidates(SceneView *delegate,
                          Item *item,
                          QList<SurfaceItem *> &candidates,
                          ssize_t maxCount,
                          Region &occluded,
                          QStack<ClipCorner> &corners)
{
    if (item->opacity() == 0.0) {
        return true;
    }

    if (item->opacity() != 1.0 || item->hasEffects()) {
        return false;
    }

    const QList<Item *> children = item->sortedChildItems();
    auto it = children.rbegin();

    for (; it != children.rend(); ++it) {
        Item *const child = *it;
        if (child->z() < 0) {
            break;
        }
        if (!delegate->shouldRenderItem(child) || !child->isVisible()) {
            continue;
        }

        const Rect childBounds = child->mapToView(child->boundingRect(), delegate).rounded();
        if (!occluded.contains(childBounds)) {
            if (!addCandidates(delegate, child, candidates, maxCount, occluded, corners)) {
                return false;
            }
        }
    }

    const Rect itemBounds = item->mapToView(item->boundingRect(), delegate).rounded();
    if (occluded.contains(itemBounds)) {
        return true;
    }

    if (delegate->shouldRenderItem(item)) {
        if (SurfaceItem *const surfaceItem = qobject_cast<SurfaceItem *>(item)) {
            candidates.push_back(surfaceItem);
            if (candidates.size() > maxCount) {
                return false;
            }
        } else {
            return false;
        }
    }

    const bool cornerPushed = pushClipCorner(item, corners);
    const auto cleanupCorners = qScopeGuard([&corners, cornerPushed]() {
        if (cornerPushed) {
            corners.pop();
        }
    });

    Region opaque = item->opaque();
    if (!opaque.isEmpty() && !corners.isEmpty()) {
        const ClipCorner &top = corners.top();
        opaque = top.radius.clip(opaque, top.box);
    }

    if (!opaque.isEmpty()) {
        occluded += item->mapToView(opaque, delegate);
    }

    for (; it != children.rend(); ++it) {
        Item *const child = *it;
        if (!delegate->shouldRenderItem(child) || !child->isVisible()) {
            continue;
        }

        const Rect childBounds = child->mapToView(child->boundingRect(), delegate).rounded();
        if (!occluded.contains(childBounds)) {
            if (!addCandidates(delegate, child, candidates, maxCount, occluded, corners)) {
                return false;
            }
        }
    }

    return true;
}

static bool checkForBlackBackground(SurfaceItem *background)
{
    const auto buffer = background->buffer();
    if (!buffer
        || (!buffer->singlePixelAttributes() && !buffer->shmAttributes())
        || buffer->size() != QSize(1, 1)) {
        return false;
    }

    const GraphicsBufferView view(buffer);
    if (!view.image()) {
        return false;
    }

    const QRgb rgb = view.image()->pixel(0, 0);
    constexpr float inv255 = 1.0f / 255.0f;
    const QVector3D encoded(static_cast<float>(qRed(rgb)) * inv255,
                            static_cast<float>(qGreen(rgb)) * inv255,
                            static_cast<float>(qBlue(rgb)) * inv255);

    const QVector3D nits = background->colorDescription()->mapTo(encoded,
                                                                  ColorDescription(Colorimetry::BT709,
                                                                                   TransferFunction(TransferFunction::linear),
                                                                                   100,
                                                                                   0,
                                                                                   std::nullopt,
                                                                                   std::nullopt),
                                                                  background->renderingIntent());

    constexpr float blackThresholdNits = 0.1f;
    return nits.lengthSquared() <= blackThresholdNits * blackThresholdNits;
}

QList<SurfaceItem *> WorkspaceScene::scanoutCandidates(ssize_t maxCount) const
{
    if (maxCount <= 0) {
        return {};
    }

    const QList<Item *> overlayItems = m_overlayItem->childItems();
    const bool needsRendering = std::ranges::any_of(overlayItems, [this](Item *child) {
        if (!child->isVisible() || child->opacity() == 0.0) {
            return false;
        }

        const RectF boundingRect = child->boundingRect();
        return !boundingRect.isEmpty()
            && painted_delegate->shouldRenderItem(child)
            && painted_delegate->viewport().intersects(child->mapToView(boundingRect, painted_delegate));
    });

    if (needsRendering) {
        return {};
    }

    const ssize_t candidateLimit = maxCount == std::numeric_limits<ssize_t>::max() ? maxCount : maxCount + 1;

    QList<SurfaceItem *> ret;
    ret.reserve(static_cast<qsizetype>(candidateLimit));

    if (!effects->blocksDirectScanout()) {
        Region occlusion;
        QStack<ClipCorner> corners;
        const QList<Item *> items = m_containerItem->sortedChildItems();

        for (Item *item : items | std::views::reverse) {
            if (!item->isVisible() || !painted_delegate->shouldRenderItem(item)) {
                continue;
            }

            const RectF boundingRect = item->boundingRect();
            if (boundingRect.isEmpty()
                || !painted_delegate->viewport().intersects(item->mapToView(boundingRect, painted_delegate))) {
                continue;
            }

            if (!addCandidates(painted_delegate, item, ret, candidateLimit, occlusion, corners)) {
                return {};
            }

            if (ret.size() == candidateLimit && !checkForBlackBackground(ret.back())) {
                return {};
            }

            if (occlusion.contains(painted_screen->geometry())) {
                break;
            }
        }
    }

    if (!ret.empty() && checkForBlackBackground(ret.back())) {
        ret.pop_back();
    }

    return ret;
}

static Rect mapToDevice(SceneView *view, Item *item, const RectF &itemLocal)
{
    const RectF localLogical = item->mapToView(itemLocal, view).translated(-view->viewport().topLeft());
    return localLogical.scaled(view->scale()).rounded();
}

static Region mapToDevice(SceneView *view, Item *item, const Region &itemLocal)
{
    Region ret;
    if (itemLocal.isEmpty()) {
        return ret;
    }

    for (const RectF local : itemLocal.rects()) {
        ret |= mapToDevice(view, item, local);
    }

    return ret;
}

/**
 * Returns:
 * - true if the search is complete and visible scene parts are fully represented.
 * - std::nullopt if search can continue.
 * - false if search failed and caller should fallback.
 */
static std::optional<bool> findOverlayCandidates(SceneView *view,
                                                 const Rect &viewDeviceRect,
                                                 Item *item,
                                                 ssize_t maxTotalCount,
                                                 ssize_t maxOverlayCount,
                                                 ssize_t maxUnderlayCount,
                                                 Region &occupied,
                                                 Region &opaque,
                                                 Region &effected,
                                                 QList<Item *> &overlays,
                                                 QList<Item *> &underlays,
                                                 QStack<ClipCorner> &corners)
{
    if (opaque.contains(viewDeviceRect)) {
        return true;
    }

    if (!item || !item->isVisible() || item->opacity() == 0.0) {
        return std::nullopt;
    }

    const RectF boundingRect = item->boundingRect();
    if (boundingRect.isEmpty()) {
        return std::nullopt;
    }

    if (!view->viewport().intersects(item->mapToView(boundingRect, view))) {
        return std::nullopt;
    }

    if (item->hasEffects()) {
        const Rect effectRect = mapToDevice(view, item, boundingRect) & viewDeviceRect;
        if (!effectRect.isEmpty()) {
            effected += effectRect;
        }
        return std::nullopt;
    }

    const bool cornerPushed = pushClipCorner(item, corners);
    const auto cleanupCorners = qScopeGuard([&corners, cornerPushed]() {
        if (cornerPushed) {
            corners.pop();
        }
    });

    const QList<Item *> children = item->sortedChildItems();
    auto it = children.rbegin();

    for (; it != children.rend(); ++it) {
        Item *const child = *it;
        if (child->z() < 0) {
            break;
        }

        if (std::optional<bool> ret = findOverlayCandidates(view,
                                                            viewDeviceRect,
                                                            child,
                                                            maxTotalCount,
                                                            maxOverlayCount,
                                                            maxUnderlayCount,
                                                            occupied,
                                                            opaque,
                                                            effected,
                                                            overlays,
                                                            underlays,
                                                            corners)) {
            return ret;
        }
    }

    SurfaceItem *const surfaceItem = qobject_cast<SurfaceItem *>(item);
    const Rect deviceRect = mapToDevice(view, item, item->rect()) & viewDeviceRect;

    bool isCandidate = false;
    if (surfaceItem
        && !deviceRect.isEmpty()
        && !surfaceItem->rect().isEmpty()
        && surfaceItem->opacity() == 1.0
        && !opaque.contains(deviceRect)
        && !effected.intersects(deviceRect)) {
        const auto estimatedFrameTime = surfaceItem->frameTimeEstimation();
        const auto buffer = surfaceItem->buffer();

        isCandidate = estimatedFrameTime.has_value()
            && *estimatedFrameTime < std::chrono::nanoseconds(1'000'000'000) / 20
            && buffer
            && buffer->dmabufAttributes();
    }

    if (isCandidate) {
        const bool clippedByCorner = !corners.isEmpty()
            && corners.top().radius.clips(item->rect(), corners.top().box);

        if (occupied.intersects(deviceRect) || clippedByCorner) {
            const bool isOpaque = surfaceItem->opaque().contains(surfaceItem->rect().roundedOut());
            if (!isOpaque) {
                return false;
            }
            underlays.push_back(surfaceItem);
        } else {
            overlays.push_back(surfaceItem);
        }

        if (overlays.size() + underlays.size() > maxTotalCount
            || overlays.size() > maxOverlayCount
            || underlays.size() > maxUnderlayCount) {
            return false;
        }
    } else if (!deviceRect.isEmpty()) {
        occupied += deviceRect;
    }

    const Region itemOpaque = item->opaque();
    if (!itemOpaque.isEmpty()) {
        opaque += mapToDevice(view, item, itemOpaque);
        if (opaque.contains(viewDeviceRect)) {
            return true;
        }
    }

    for (; it != children.rend(); ++it) {
        Item *const child = *it;

        if (std::optional<bool> ret = findOverlayCandidates(view,
                                                            viewDeviceRect,
                                                            child,
                                                            maxTotalCount,
                                                            maxOverlayCount,
                                                            maxUnderlayCount,
                                                            occupied,
                                                            opaque,
                                                            effected,
                                                            overlays,
                                                            underlays,
                                                            corners)) {
            return ret;
        }
    }

    return std::nullopt;
}

static const bool s_forceSoftwareCursor = environmentVariableBoolValue("KWIN_FORCE_SW_CURSOR").value_or(false);

Scene::OverlayCandidates WorkspaceScene::overlayCandidates(ssize_t maxTotalCount, ssize_t maxOverlayCount, ssize_t maxUnderlayCount) const
{
    const auto fallback = [&]() {
        if (s_forceSoftwareCursor
            || maxTotalCount <= 0
            || maxOverlayCount <= 0
            || !cursorItem()->isVisible()
            || !painted_delegate->viewport().intersects(cursorItem()->mapToView(cursorItem()->boundingRect(), painted_delegate))) {
            return OverlayCandidates{};
        }

        return OverlayCandidates{
            .overlays = {cursorItem()},
            .underlays = {},
        };
    };

    if (maxTotalCount <= 0) {
        return fallback();
    }

    Region occupied;
    Region opaque;
    Region effected;
    QList<Item *> overlays;
    QList<Item *> underlays;
    overlays.reserve(static_cast<qsizetype>(std::max<ssize_t>(0, maxOverlayCount)));
    underlays.reserve(static_cast<qsizetype>(std::max<ssize_t>(0, maxUnderlayCount)));

    QStack<ClipCorner> cornerStack;
    std::optional<bool> result;
    const Rect viewDeviceRect = painted_delegate->deviceRect();

    const QList<Item *> overlayItems = m_overlayItem->sortedChildItems();
    for (Item *item : overlayItems | std::views::reverse) {
        if (!item->isVisible()) {
            continue;
        }

        const RectF boundingRect = item->boundingRect();
        if (boundingRect.isEmpty()
            || !painted_delegate->viewport().intersects(item->mapToView(boundingRect, painted_delegate))) {
            continue;
        }

        if (item == cursorItem()) {
            if (s_forceSoftwareCursor) {
                continue;
            }

            overlays.push_back(item);
            if (overlays.size() > maxOverlayCount || underlays.size() + overlays.size() > maxTotalCount) {
                return fallback();
            }
            continue;
        }

        result = findOverlayCandidates(painted_delegate,
                                       viewDeviceRect,
                                       item,
                                       maxTotalCount,
                                       maxOverlayCount,
                                       maxUnderlayCount,
                                       occupied,
                                       opaque,
                                       effected,
                                       overlays,
                                       underlays,
                                       cornerStack);

        if (result.has_value()) {
            if (*result) {
                break;
            }
            return fallback();
        }
    }

    if (effects->blocksDirectScanout()) {
        return fallback();
    }

    if (!result) {
        const QList<Item *> items = m_containerItem->sortedChildItems();
        for (Item *item : items | std::views::reverse) {
            result = findOverlayCandidates(painted_delegate,
                                           viewDeviceRect,
                                           item,
                                           maxTotalCount,
                                           maxOverlayCount,
                                           maxUnderlayCount,
                                           occupied,
                                           opaque,
                                           effected,
                                           overlays,
                                           underlays,
                                           cornerStack);

            if (result.has_value()) {
                if (*result) {
                    break;
                }
                return fallback();
            }
        }
    }

    return OverlayCandidates{
        .overlays = overlays,
        .underlays = underlays,
    };
}

static double getDesiredHdrHeadroom(Item *item)
{
    if (!item->isVisible()) {
        return 1.0;
    }

    double ret = 1.0;
    const QList<Item *> children = item->childItems();
    for (Item *child : children) {
        ret = std::max(ret, getDesiredHdrHeadroom(child));
    }

    const auto &color = item->colorDescription();
    const std::optional<double> maxHdrLuminance = color->maxHdrLuminance();
    if (maxHdrLuminance && *maxHdrLuminance > color->referenceLuminance()) {
        return std::max(ret, *maxHdrLuminance / color->referenceLuminance());
    }

    return ret;
}

double WorkspaceScene::desiredHdrHeadroom() const
{
    double maxHeadroom = 1.0;

    for (WindowItem *item : std::as_const(stacking_order)) {
        if (!item->window()->frameGeometry().intersects(painted_delegate->viewport())) {
            continue;
        }

        maxHeadroom = std::max(maxHeadroom, getDesiredHdrHeadroom(item));
    }

    return maxHeadroom;
}

void WorkspaceScene::frame(SceneView *delegate, OutputFrame *frame)
{
    LogicalOutput *logicalOutput = delegate->logicalOutput();
    const auto frameTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        logicalOutput->backendOutput()->renderLoop()->lastPresentationTimestamp());

    m_containerItem->framePainted(delegate, logicalOutput, frame, frameTime);
    if (m_overlayItem) {
        m_overlayItem->framePainted(delegate, logicalOutput, frame, frameTime);
    }
}

void WorkspaceScene::prePaint(SceneView *delegate)
{
    painted_delegate = delegate;
    painted_screen = painted_delegate->logicalOutput();

    createStackingOrder();

    const RenderLoop *renderLoop = painted_screen->backendOutput()->renderLoop();
    const std::chrono::milliseconds presentTime =
        std::chrono::duration_cast<std::chrono::milliseconds>(renderLoop->nextPresentationTimestamp());

    if (presentTime > m_expectedPresentTimestamp) {
        m_expectedPresentTimestamp = presentTime;
    }

    effects->startPaint();

    ScreenPrePaintData prePaintData;
    prePaintData.mask = 0;
    prePaintData.screen = painted_screen;
    prePaintData.view = delegate;

    effects->makeOpenGLContextCurrent();
    Q_EMIT preFrameRender();

    effects->prePaintScreen(prePaintData, m_expectedPresentTimestamp);

    m_paintContext.deviceDamage = painted_delegate->mapToDeviceCoordinatesAligned(prePaintData.paint)
        & painted_delegate->deviceRect();
    m_paintContext.mask = prePaintData.mask;
    m_paintContext.phase2Data.clear();
    m_paintContext.phase2Data.reserve(stacking_order.size());

    if (m_paintContext.mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) {
        preparePaintGenericScreen();
    } else {
        preparePaintSimpleScreen();
    }
}

static void resetRepaintsHelper(Item *item, SceneView *delegate)
{
    if (delegate->shouldRenderItem(item)) {
        item->resetRepaints(delegate);
    }

    const QList<Item *> childItems = item->childItems();
    for (Item *childItem : childItems) {
        resetRepaintsHelper(childItem, delegate);
    }
}

static void accumulateRepaints(Item *item,
                               SceneView *view,
                               Region *windowRepaints,
                               Region *accumulatedRepaints,
                               Region *forceTranslucent)
{
    const QList<Item *> childItems = item->sortedChildItems();
    auto childIt = childItems.begin();

    for (; childIt != childItems.end() && (*childIt)->z() < 0; ++childIt) {
        accumulateRepaints(*childIt, view, windowRepaints, accumulatedRepaints, forceTranslucent);
    }

    if (BackgroundEffectItem *background = qobject_cast<BackgroundEffectItem *>(item)) {
        const Rect viewRect = view->mapToDeviceCoordinates(item->mapToView(item->rect(), view)).rounded();

        if (accumulatedRepaints->intersects(viewRect)) {
            *windowRepaints |= viewRect;
            *accumulatedRepaints |= viewRect;

            if (const uint32_t pixels = background->pixelsToExpandRepaintsBelowOpaqueRegions()) {
                for (const Rect &rect : accumulatedRepaints->rects()) {
                    *forceTranslucent |= rect.adjusted(-pixels, -pixels, pixels, pixels) & viewRect;
                }
            }
        }
    } else if (view->shouldRenderItem(item)) {
        const Region repaints = item->takeDeviceRepaints(view);
        *windowRepaints |= repaints;
        *accumulatedRepaints |= repaints;
    }

    for (; childIt != childItems.end(); ++childIt) {
        accumulateRepaints(*childIt, view, windowRepaints, accumulatedRepaints, forceTranslucent);
    }
}

void WorkspaceScene::preparePaintGenericScreen()
{
    for (WindowItem *windowItem : std::as_const(stacking_order)) {
        resetRepaintsHelper(windowItem, painted_delegate);

        WindowPrePaintData data;
        data.mask = m_paintContext.mask;

        effects->prePaintWindow(painted_delegate, windowItem->effectWindow(), data, m_expectedPresentTimestamp);
        m_paintContext.phase2Data.append(Phase2Data{
            .item = windowItem,
            .deviceRegion = Region::infinite(),
            .deviceOpaque = data.deviceOpaque,
            .mask = data.mask,
        });
    }
}

static void addOpaqueRegionRecursive(SceneView *view,
                                     Item *item,
                                     const std::optional<ClipCorner> &parentCorner,
                                     Region &ret)
{
    const std::optional<ClipCorner> corner = calculateClipCorner(item, parentCorner);

    Region opaque = item->opaque();
    if (!opaque.isEmpty()) {
        if (corner.has_value()) {
            opaque = corner->radius.clip(opaque, corner->box);
        }

        if (!opaque.isEmpty()) {
            const Rect deviceRect = snapToPixelGrid(view->mapToDeviceCoordinates(item->mapToView(item->rect(), view)));

            for (const RectF rect : opaque.rects()) {
                ret |= snapToPixelGrid(view->mapToDeviceCoordinates(item->mapToView(rect, view))) & deviceRect;
            }
        }
    }

    const QList<Item *> children = item->childItems();
    for (Item *child : children) {
        addOpaqueRegionRecursive(view, child, corner, ret);
    }
}

void WorkspaceScene::preparePaintSimpleScreen()
{
    for (WindowItem *windowItem : std::as_const(stacking_order)) {
        Window *window = windowItem->window();

        WindowPrePaintData data;
        data.mask = m_paintContext.mask;

        if (window->opacity() == 1.0) {
            addOpaqueRegionRecursive(painted_delegate, windowItem, std::nullopt, data.deviceOpaque);
        }

        effects->prePaintWindow(painted_delegate, windowItem->effectWindow(), data, m_expectedPresentTimestamp);
        m_paintContext.phase2Data.append(Phase2Data{
            .item = windowItem,
            .deviceRegion = Region{},
            .deviceOpaque = data.deviceOpaque,
            .mask = data.mask,
        });
    }
}

Region WorkspaceScene::collectDamage()
{
    if (m_paintContext.mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) {
        resetRepaintsHelper(m_overlayItem.get(), painted_delegate);
        m_paintContext.deviceDamage = painted_delegate->deviceRect();
        return m_paintContext.deviceDamage;
    }

    Region accumulatedRepaints;
    Region forceTranslucent;

    for (Phase2Data &data : m_paintContext.phase2Data) {
        data.deviceOpaque -= forceTranslucent;
        accumulateRepaints(data.item, painted_delegate, &data.deviceRegion, &accumulatedRepaints, &forceTranslucent);
    }

    accumulateRepaints(m_overlayItem.get(), painted_delegate, &m_paintContext.deviceDamage, &accumulatedRepaints, &forceTranslucent);

    Region opaque;
    for (Phase2Data &paintData : m_paintContext.phase2Data | std::views::reverse) {
        m_paintContext.deviceDamage |= paintData.deviceRegion - opaque;

        SurfaceItem *const surfaceItem = paintData.item->surfaceItem();
        const bool canCover = painted_delegate->shouldRenderItem(surfaceItem)
            || painted_delegate->shouldRenderHole(surfaceItem);

        if (!(paintData.mask & (PAINT_WINDOW_TRANSLUCENT | PAINT_WINDOW_TRANSFORMED)) && canCover) {
            opaque += paintData.deviceOpaque;
        }
    }

    return m_paintContext.deviceDamage & painted_delegate->deviceRect();
}

void WorkspaceScene::postPaint()
{
    effects->postPaintScreen();

    painted_delegate = nullptr;
    painted_screen = nullptr;
    clearStackingOrder();
}

void WorkspaceScene::paint(const RenderTarget &renderTarget, const QPoint &deviceOffset, const Region &deviceRegion)
{
    RenderViewport viewport(painted_delegate->viewport(), painted_delegate->scale(), renderTarget, deviceOffset);

    m_renderer->beginFrame(renderTarget, viewport);

    effects->paintScreen(renderTarget, viewport, m_paintContext.mask, deviceRegion, painted_screen);
    m_paintScreenCount = 0;

    if (m_overlayItem) {
        const Rect bounds = viewport.mapToDeviceCoordinates(m_overlayItem->mapToScene(m_overlayItem->boundingRect())).toRect();
        const Region deviceRepaint = deviceRegion & bounds;

        if (!deviceRepaint.isEmpty()) {
            m_renderer->renderItem(renderTarget,
                                   viewport,
                                   m_overlayItem.get(),
                                   PAINT_SCREEN_TRANSFORMED,
                                   deviceRepaint,
                                   WindowPaintData{},
                                   [this](Item *item) {
                                       return !painted_delegate->shouldRenderItem(item);
                                   },
                                   [this](Item *item) {
                                       return painted_delegate->shouldRenderHole(item);
                                   });
        }
    }

    Q_EMIT frameRendered();
    m_renderer->endFrame();
}

void WorkspaceScene::finalPaintScreen(const RenderTarget &renderTarget,
                                      const RenderViewport &viewport,
                                      int mask,
                                      const Region &deviceRegion,
                                      LogicalOutput *screen)
{
    m_paintScreenCount++;

    if (mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) {
        paintGenericScreen(renderTarget, viewport, mask, screen);
    } else {
        paintSimpleScreen(renderTarget, viewport, mask, deviceRegion);
    }
}

void WorkspaceScene::paintGenericScreen(const RenderTarget &renderTarget,
                                        const RenderViewport &viewport,
                                        int,
                                        LogicalOutput *)
{
    if (m_paintContext.mask & PAINT_SCREEN_BACKGROUND_FIRST) {
        if (m_paintScreenCount == 1) {
            m_renderer->renderBackground(renderTarget, viewport, Region::infinite());
        }
    } else {
        m_renderer->renderBackground(renderTarget, viewport, Region::infinite());
    }

    for (const Phase2Data &paintData : std::as_const(m_paintContext.phase2Data)) {
        paintWindow(renderTarget, viewport, paintData.item, paintData.mask, paintData.deviceRegion);
    }
}

void WorkspaceScene::paintSimpleScreen(const RenderTarget &renderTarget,
                                       const RenderViewport &viewport,
                                       int,
                                       const Region &deviceRegion)
{
    Region visible = deviceRegion;
    const Rect deviceRect = viewport.deviceRect();

    for (int i = m_paintContext.phase2Data.size() - 1; i >= 0; --i) {
        Phase2Data *data = &m_paintContext.phase2Data[i];
        data->deviceRegion = visible & deviceRect;

        if (!(data->mask & PAINT_WINDOW_TRANSFORMED)) {
            data->deviceRegion &= viewport.mapToDeviceCoordinatesAligned(data->item->mapToScene(data->item->boundingRect()));

            SurfaceItem *const surfaceItem = data->item->surfaceItem();
            const bool canCover = painted_delegate->shouldRenderItem(surfaceItem)
                || painted_delegate->shouldRenderHole(surfaceItem);

            if (!(data->mask & PAINT_WINDOW_TRANSLUCENT) && canCover) {
                visible -= data->deviceOpaque;
            }
        }
    }

    m_renderer->renderBackground(renderTarget, viewport, visible);

    for (const Phase2Data &paintData : std::as_const(m_paintContext.phase2Data)) {
        paintWindow(renderTarget, viewport, paintData.item, paintData.mask, paintData.deviceRegion);
    }
}

void WorkspaceScene::createStackingOrder()
{
    const QList<Item *> items = m_containerItem->sortedChildItems();

    stacking_order.clear();
    stacking_order.reserve(items.size());

    for (Item *item : std::as_const(items)) {
        WindowItem *windowItem = static_cast<WindowItem *>(item);

        if (painted_delegate && painted_delegate->shouldHideWindow(windowItem->window())) {
            continue;
        }

        if (windowItem->isVisible()) {
            stacking_order.append(windowItem);
        }
    }
}

void WorkspaceScene::clearStackingOrder()
{
    stacking_order.clear();
}

void WorkspaceScene::paintWindow(const RenderTarget &renderTarget,
                                 const RenderViewport &viewport,
                                 WindowItem *item,
                                 int mask,
                                 const Region &deviceRegion)
{
    if (deviceRegion.isEmpty()) {
        return;
    }

    WindowPaintData data;
    effects->paintWindow(renderTarget, viewport, item->effectWindow(), mask, deviceRegion, data);
}

void WorkspaceScene::finalPaintWindow(const RenderTarget &renderTarget,
                                      const RenderViewport &viewport,
                                      EffectWindow *w,
                                      int mask,
                                      const Region &deviceRegion,
                                      WindowPaintData &data)
{
    effects->drawWindow(renderTarget, viewport, w, mask, deviceRegion, data);
}

void WorkspaceScene::finalDrawWindow(const RenderTarget &renderTarget,
                                     const RenderViewport &viewport,
                                     EffectWindow *w,
                                     int mask,
                                     const Region &deviceRegion,
                                     WindowPaintData &data)
{
    m_renderer->renderItem(renderTarget,
                           viewport,
                           w->windowItem(),
                           mask,
                           deviceRegion,
                           data,
                           [this](Item *item) {
                               return painted_delegate && !painted_delegate->shouldRenderItem(item);
                           },
                           [this](Item *item) {
                               return painted_delegate && painted_delegate->shouldRenderHole(item);
                           });
}

EglContext *WorkspaceScene::openglContext() const
{
    if (EglBackend *eglBackend = qobject_cast<EglBackend *>(Compositor::self()->backend())) {
        return eglBackend->openglContext();
    }

    return nullptr;
}

bool WorkspaceScene::animationsSupported() const
{
    const auto context = openglContext();
    return context && !context->isSoftwareRenderer();
}

} // namespace KWin

#include "moc_workspacescene.cpp"
