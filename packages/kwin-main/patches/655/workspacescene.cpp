/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2006 Lubos Lunak <l.lunak@kde.org>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "scene/workspacescene.h"
#include "compositor.h"
#include "core/output.h"
#include "core/pixelgrid.h"
#include "core/renderbackend.h"
#include "core/renderloop.h"
#include "core/renderviewport.h"
#include "cursoritem.h"
#include "effect/effecthandler.h"
#include "opengl/eglbackend.h"
#include "opengl/eglcontext.h"
#include "scene/decorationitem.h"
#include "scene/dndiconitem.h"
#include "scene/itemrenderer.h"
#include "scene/rootitem.h"
#include "scene/surfaceitem.h"
#include "scene/windowitem.h"
#include "wayland/seat.h"
#include "wayland_server.h"
#include "window.h"
#include "workspace.h"

#include <QtMath>
#include <vector>

namespace KWin
{

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

    stacking_order.reserve(64);
    m_paintContext.phase2Data.reserve(64);

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
    if (!dragIcon) [[unlikely]] {
        return;
    }
    m_dndIcon = std::make_unique<DragAndDropIconItem>(dragIcon, m_overlayItem.get());

    auto updatePosition = [this]() {
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
    if (Cursors::self()->isCursorHidden()) [[unlikely]] {
        m_cursorItem->setVisible(false);
    } else [[likely]] {
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

static inline bool regionActuallyContains(const QRegion &region, const QRect &rect) noexcept
    __attribute__((always_inline, pure));

static inline bool regionActuallyContains(const QRegion &region, const QRect &rect) noexcept
{
    if (rect.isEmpty()) [[unlikely]] {
        return true;
    }
    const QRect regionBounds = region.boundingRect();
    if (!regionBounds.contains(rect)) [[likely]] {
        return false;
    }
    return (region & rect) == rect;
}

struct ClipCorner
{
    QRectF box;
    BorderRadius radius;
};

static inline bool maybePushCorners(Item *item, std::vector<ClipCorner> &corners) noexcept
    __attribute__((always_inline));

static inline bool maybePushCorners(Item *item, std::vector<ClipCorner> &corners) noexcept
{
    if (!item) [[unlikely]] {
        return false;
    }
    if (!item->borderRadius().isNull()) {
        corners.push_back({
            .box = item->rect(),
            .radius = item->borderRadius(),
        });
        return true;
    }
    if (!corners.empty()) {
        const auto &top = corners.back();
        corners.push_back({
            .box = item->transform().inverted().mapRect(top.box.translated(-item->position())),
            .radius = top.radius,
        });
        return true;
    }
    return false;
}

static bool addCandidates(SceneView *delegate, Item *item, QList<SurfaceItem *> &candidates,
                          ssize_t maxCount, QRegion &occluded, std::vector<ClipCorner> &corners)
    __attribute__((hot));

static bool addCandidates(SceneView *delegate, Item *item, QList<SurfaceItem *> &candidates,
                          ssize_t maxCount, QRegion &occluded, std::vector<ClipCorner> &corners)
{
    if (!item) [[unlikely]] {
        return false;
    }
    if (item->opacity() != 1.0 || item->hasEffects()) [[unlikely]] {
        return false;
    }

    const QList<Item *> children = item->sortedChildItems();

    auto it = children.rbegin();
    const auto rend = children.rend();
    for (; it != rend; ++it) {
        Item *const child = *it;
        if (child->z() < 0) {
            break;
        }

        if (!delegate->shouldRenderItem(child)) [[unlikely]] {
            continue;
        }
        if (!child->isVisible()) [[unlikely]] {
            continue;
        }

        const QRect childBounds = child->mapToView(child->boundingRect(), delegate).toAlignedRect();
        if (regionActuallyContains(occluded, childBounds)) [[unlikely]] {
            continue;
        }

        {
            auto nextIt = it + 1;
            if (nextIt != rend) [[likely]] {
                __builtin_prefetch(*nextIt, 0, 2);
            }
            auto nextIt2 = it + 2;
            if (nextIt2 != rend) [[likely]] {
                __builtin_prefetch(*nextIt2, 0, 1);
            }
        }

        if (!addCandidates(delegate, child, candidates, maxCount, occluded, corners)) {
            return false;
        }
    }

    const QRect itemBounds = item->mapToView(item->boundingRect(), delegate).toAlignedRect();
    if (regionActuallyContains(occluded, itemBounds)) [[unlikely]] {
        return true;
    }

    if (delegate->shouldRenderItem(item)) [[likely]] {
        auto *surfaceItem = qobject_cast<SurfaceItem *>(item);
        if (surfaceItem) [[likely]] {
            candidates.push_back(surfaceItem);
            if (candidates.size() > maxCount) [[unlikely]] {
                return false;
            }
        } else {
            return false;
        }
    }

    const bool pushed = maybePushCorners(item, corners);

    QRegion opaque = item->opaque();
    if (!corners.empty()) [[unlikely]] {
        const auto &top = corners.back();
        opaque = top.radius.clip(opaque, top.box);
    }
    occluded += item->mapToView(opaque, delegate);

    for (; it != rend; ++it) {
        Item *const child = *it;

        if (!delegate->shouldRenderItem(child)) [[unlikely]] {
            continue;
        }
        if (!child->isVisible()) [[unlikely]] {
            continue;
        }

        const QRect childBounds = child->mapToView(child->boundingRect(), delegate).toAlignedRect();
        if (regionActuallyContains(occluded, childBounds)) [[unlikely]] {
            continue;
        }

        {
            auto nextIt = it + 1;
            if (nextIt != rend) [[likely]] {
                __builtin_prefetch(*nextIt, 0, 2);
            }
        }

        if (!addCandidates(delegate, child, candidates, maxCount, occluded, corners)) {
            if (pushed) {
                corners.pop_back();
            }
            return false;
        }
    }

    if (pushed) {
        corners.pop_back();
    }
    return true;
}

QList<SurfaceItem *> WorkspaceScene::scanoutCandidates(ssize_t maxCount) const
{
    const auto overlayItems = m_overlayItem->childItems();
    const bool needsRendering = std::ranges::any_of(overlayItems, [this](Item *child) {
        return child->isVisible()
            && !child->boundingRect().isEmpty()
            && painted_delegate->shouldRenderItem(child);
    });
    if (needsRendering) [[unlikely]] {
        return {};
    }

    QList<SurfaceItem *> ret;
    if (!effects->blocksDirectScanout()) [[likely]] {
        QRegion occlusion;
        std::vector<ClipCorner> corners;
        corners.reserve(16);

        const auto items = m_containerItem->sortedChildItems();
        const auto numItems = items.size();

        for (auto i = numItems; i-- > 0;) {
            Item *item = items[i];

            if (!item->isVisible()) [[unlikely]] {
                continue;
            }
            if (!painted_delegate->shouldRenderItem(item)) [[unlikely]] {
                continue;
            }

            const QRect itemBounds = item->mapToView(item->boundingRect(), painted_delegate).toAlignedRect();
            if (!painted_delegate->viewport().intersects(itemBounds)) [[unlikely]] {
                continue;
            }

            if (i > 0) [[likely]] {
                Item *nextItem = items[i - 1];
                __builtin_prefetch(nextItem, 0, 2);
                const auto &nextChildren = nextItem->childItems();
                if (!nextChildren.isEmpty()) [[likely]] {
                    __builtin_prefetch(nextChildren.data(), 0, 2);
                }
            }
            if (i > 1) [[likely]] {
                __builtin_prefetch(items[i - 2], 0, 1);
            }

            if (!addCandidates(painted_delegate, item, ret, maxCount, occlusion, corners)) {
                return {};
            }

            if (regionActuallyContains(occlusion, painted_screen->geometry())) [[unlikely]] {
                return ret;
            }
        }
    }
    return ret;
}

static QRect mapToDevice(SceneView *view, Item *item, const QRectF &itemLocal) noexcept
{
    const QRectF localLogical = item->mapToView(itemLocal, view).translated(-view->viewport().topLeft());
    return snapToPixelGridF(scaledRect(localLogical, view->scale())).toRect();
}

static QRegion mapToDevice(SceneView *view, Item *item, const QRegion &itemLocal)
{
    QRegion ret;
    for (const QRectF &local : itemLocal) {
        ret |= mapToDevice(view, item, local);
    }
    return ret;
}

static bool findOverlayCandidates(SceneView *view, Item *item, ssize_t maxTotalCount,
                                   ssize_t maxOverlayCount, ssize_t maxUnderlayCount,
                                   QRegion &occupied, QRegion &opaque, QRegion &effected,
                                   QList<SurfaceItem *> &overlays, QList<SurfaceItem *> &underlays,
                                   std::vector<ClipCorner> &corners)
    __attribute__((hot));

static bool findOverlayCandidates(SceneView *view, Item *item, ssize_t maxTotalCount,
                                   ssize_t maxOverlayCount, ssize_t maxUnderlayCount,
                                   QRegion &occupied, QRegion &opaque, QRegion &effected,
                                   QList<SurfaceItem *> &overlays, QList<SurfaceItem *> &underlays,
                                   std::vector<ClipCorner> &corners)
{
    if (!item) [[unlikely]] {
        return true;
    }
    if (!item->isVisible() || item->boundingRect().isEmpty()) [[unlikely]] {
        return true;
    }

    const QRect itemBounds = item->mapToView(item->boundingRect(), view).toAlignedRect();
    if (!view->viewport().intersects(itemBounds)) [[unlikely]] {
        return true;
    }

    if (item->hasEffects()) [[unlikely]] {
        effected += mapToDevice(view, item, item->boundingRect());
        return true;
    }

    const bool pushed = maybePushCorners(item, corners);

    const QList<Item *> children = item->sortedChildItems();
    const auto rend = children.rend();

    auto it = children.rbegin();
    for (; it != rend; ++it) {
        Item *const child = *it;
        if (child->z() < 0) {
            break;
        }

        {
            auto nextIt = it + 1;
            if (nextIt != rend) [[likely]] {
                __builtin_prefetch(*nextIt, 0, 2);
            }
        }

        if (!findOverlayCandidates(view, child, maxTotalCount, maxOverlayCount, maxUnderlayCount,
                                    occupied, opaque, effected, overlays, underlays, corners)) {
            if (pushed) {
                corners.pop_back();
            }
            return false;
        }
    }

    SurfaceItem *surfaceItem = qobject_cast<SurfaceItem *>(item);
    const QRect deviceRect = mapToDevice(view, item, item->rect());

    if (surfaceItem
        && !surfaceItem->rect().isEmpty()
        && surfaceItem->frameTimeEstimation() <= std::chrono::nanoseconds(1'000'000'000 / 20)
        && surfaceItem->buffer()
        && surfaceItem->buffer()->dmabufAttributes()
        && surfaceItem->opacity() == 1.0
        && !regionActuallyContains(opaque, deviceRect)
        && !effected.intersects(deviceRect)) [[likely]] {

        if (occupied.intersects(deviceRect)
            || (!corners.empty() && corners.back().radius.clips(item->rect(), corners.back().box))) [[unlikely]] {
            underlays.push_back(surfaceItem);
        } else [[likely]] {
            overlays.push_back(surfaceItem);
        }

        const auto totalCandidates = overlays.size() + underlays.size();
        if (totalCandidates > maxTotalCount
            || overlays.size() > maxOverlayCount
            || underlays.size() > maxUnderlayCount) [[unlikely]] {
            if (pushed) {
                corners.pop_back();
            }
            return false;
        }
    } else {
        occupied += deviceRect;
    }

    opaque += mapToDevice(view, item, item->opaque());

    for (; it != rend; ++it) {
        Item *const child = *it;

        {
            auto nextIt = it + 1;
            if (nextIt != rend) [[likely]] {
                __builtin_prefetch(*nextIt, 0, 2);
            }
        }

        if (!findOverlayCandidates(view, child, maxTotalCount, maxOverlayCount, maxUnderlayCount,
                                    occupied, opaque, effected, overlays, underlays, corners)) {
            if (pushed) {
                corners.pop_back();
            }
            return false;
        }
    }

    if (pushed) {
        corners.pop_back();
    }
    return true;
}

Scene::OverlayCandidates WorkspaceScene::overlayCandidates(ssize_t maxTotalCount, ssize_t maxOverlayCount, ssize_t maxUnderlayCount) const
{
    if (effects->blocksDirectScanout()) [[unlikely]] {
        return {};
    }

    QRegion occupied;
    QRegion opaque;
    QRegion effected;
    QList<SurfaceItem *> overlays;
    QList<SurfaceItem *> underlays;
    std::vector<ClipCorner> cornerStack;
    cornerStack.reserve(16);

    const auto overlayItems = m_overlayItem->sortedChildItems();
    const auto numOverlayItems = overlayItems.size();
    for (auto i = numOverlayItems; i-- > 0;) {
        Item *item = overlayItems[i];

        if (item == cursorItem() && !painted_delegate->shouldRenderItem(item)) [[unlikely]] {
            continue;
        }

        if (i > 0) [[likely]] {
            __builtin_prefetch(overlayItems[i - 1], 0, 2);
        }

        if (!findOverlayCandidates(painted_delegate, item, maxTotalCount, maxOverlayCount, maxUnderlayCount,
                                    occupied, opaque, effected, overlays, underlays, cornerStack)) {
            return {};
        }
    }

    const auto items = m_containerItem->sortedChildItems();
    const auto numItems = items.size();
    for (auto i = numItems; i-- > 0;) {
        Item *item = items[i];

        if (i > 0) [[likely]] {
            __builtin_prefetch(items[i - 1], 0, 2);
        }

        if (!findOverlayCandidates(painted_delegate, item, maxTotalCount, maxOverlayCount, maxUnderlayCount,
                                    occupied, opaque, effected, overlays, underlays, cornerStack)) {
            return {};
        }
    }

    return OverlayCandidates{
        .overlays = overlays,
        .underlays = underlays,
    };
}

static double getDesiredHdrHeadroom(Item *root) noexcept
{
    if (!root || !root->isVisible()) [[unlikely]] {
        return 1.0;
    }

    double maxHeadroom = 1.0;

    std::vector<Item *> stack;
    stack.reserve(32);
    stack.push_back(root);

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        if (!item || !item->isVisible()) [[unlikely]] {
            continue;
        }

        const auto &color = item->colorDescription();
        if (color->maxHdrLuminance() && *color->maxHdrLuminance() > color->referenceLuminance()) [[unlikely]] {
            const double headroom = *color->maxHdrLuminance() / color->referenceLuminance();
            maxHeadroom = std::max(maxHeadroom, headroom);
        }

        const auto &children = item->childItems();
        const qsizetype numChildren = children.size();
        for (qsizetype i = 0; i < numChildren; ++i) {
            stack.push_back(children[i]);

            if (i + 2 < numChildren) [[likely]] {
                __builtin_prefetch(children[i + 2], 0, 1);
            }
        }
    }

    return maxHeadroom;
}

double WorkspaceScene::desiredHdrHeadroom() const
{
    double maxHeadroom = 1.0;

    const auto numWindows = stacking_order.size();
    for (std::size_t i = 0; i < numWindows; ++i) {
        WindowItem *item = stacking_order[i];

        if (!item->window()->frameGeometry().intersects(painted_delegate->viewport())) [[unlikely]] {
            continue;
        }

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(stacking_order[i + 1], 0, 2);
        }
        if (i + 2 < numWindows) [[likely]] {
            __builtin_prefetch(stacking_order[i + 2], 0, 1);
        }

        maxHeadroom = std::max(maxHeadroom, getDesiredHdrHeadroom(item));
    }

    return maxHeadroom;
}

void WorkspaceScene::frame(SceneView *delegate, OutputFrame *frame)
{
    if (waylandServer()) [[likely]] {
        Output *output = delegate->output();
        const auto frameTime = std::chrono::duration_cast<std::chrono::milliseconds>(output->renderLoop()->lastPresentationTimestamp());
        m_containerItem->framePainted(delegate, output, frame, frameTime);
        if (m_overlayItem) [[likely]] {
            m_overlayItem->framePainted(delegate, output, frame, frameTime);
        }
    }
}

void WorkspaceScene::prePaint(SceneView *delegate)
{
    createStackingOrder();

    painted_delegate = delegate;
    painted_screen = painted_delegate->output();

    const RenderLoop *renderLoop = painted_screen->renderLoop();
    const std::chrono::milliseconds presentTime =
        std::chrono::duration_cast<std::chrono::milliseconds>(renderLoop->nextPresentationTimestamp());

    if (presentTime > m_expectedPresentTimestamp) [[likely]] {
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
    m_paintContext.damage = prePaintData.paint;
    m_paintContext.mask = prePaintData.mask;

    if (m_paintContext.mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) [[unlikely]] {
        preparePaintGenericScreen();
    } else [[likely]] {
        preparePaintSimpleScreen();
    }
}

static void resetRepaintsHelper(Item *root, SceneView *delegate) noexcept
{
    if (!root) [[unlikely]] {
        return;
    }

    std::vector<Item *> stack;
    stack.reserve(32);
    stack.push_back(root);

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        if (delegate->shouldRenderItem(item)) [[likely]] {
            item->resetRepaints(delegate);
        }

        const auto &childItems = item->childItems();
        const qsizetype numChildren = childItems.size();
        for (qsizetype i = 0; i < numChildren; ++i) {
            stack.push_back(childItems[i]);

            if (i + 2 < numChildren) [[likely]] {
                __builtin_prefetch(childItems[i + 2], 0, 1);
            }
        }
    }
}

static void accumulateRepaints(Item *root, SceneView *delegate, QRegion *repaints) noexcept
{
    if (!root || !repaints) [[unlikely]] {
        return;
    }

    std::vector<Item *> stack;
    stack.reserve(32);
    stack.push_back(root);

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        if (delegate->shouldRenderItem(item)) [[likely]] {
            *repaints += item->takeRepaints(delegate);
        }

        const auto &childItems = item->childItems();
        const qsizetype numChildren = childItems.size();
        for (qsizetype i = 0; i < numChildren; ++i) {
            stack.push_back(childItems[i]);

            if (i + 2 < numChildren) [[likely]] {
                __builtin_prefetch(childItems[i + 2], 0, 1);
            }
        }
    }
}

void WorkspaceScene::preparePaintGenericScreen()
{
    const auto numWindows = stacking_order.size();

    if (m_paintContext.phase2Data.size() != numWindows) {
        m_paintContext.phase2Data.resize(numWindows);
    }

    for (std::size_t i = 0; i < numWindows; ++i) {
        WindowItem *windowItem = stacking_order[i];
        resetRepaintsHelper(windowItem, painted_delegate);

        WindowPrePaintData data;
        data.mask = m_paintContext.mask;
        data.paint = infiniteRegion();

        effects->prePaintWindow(windowItem->effectWindow(), data, m_expectedPresentTimestamp);

        m_paintContext.phase2Data[i] = Phase2Data{
            .item = windowItem,
            .region = infiniteRegion(),
            .opaque = data.opaque,
            .mask = data.mask,
        };

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(stacking_order[i + 1], 0, 2);
            __builtin_prefetch(&m_paintContext.phase2Data[i + 1], 1, 3);
        }
    }
}

void WorkspaceScene::preparePaintSimpleScreen()
{
    const auto numWindows = stacking_order.size();

    if (m_paintContext.phase2Data.size() != numWindows) {
        m_paintContext.phase2Data.resize(numWindows);
    }

    for (std::size_t i = 0; i < numWindows; ++i) {
        WindowItem *windowItem = stacking_order[i];
        Window *window = windowItem->window();
        WindowPrePaintData data;
        data.mask = m_paintContext.mask;

        if (window->opacity() == 1.0) [[likely]] {
            const SurfaceItem *surfaceItem = windowItem->surfaceItem();
            if (Q_LIKELY(surfaceItem)) {
                data.opaque = surfaceItem->mapToScene(surfaceItem->borderRadius().clip(surfaceItem->opaque(), surfaceItem->rect()));
            }

            const DecorationItem *decorationItem = windowItem->decorationItem();
            if (decorationItem) [[unlikely]] {
                data.opaque += decorationItem->mapToScene(decorationItem->borderRadius().clip(decorationItem->opaque(), decorationItem->rect()));
            }
        }

        effects->prePaintWindow(windowItem->effectWindow(), data, m_expectedPresentTimestamp);

        m_paintContext.phase2Data[i] = Phase2Data{
            .item = windowItem,
            .region = data.paint,
            .opaque = data.opaque,
            .mask = data.mask,
        };

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(stacking_order[i + 1], 0, 2);
            __builtin_prefetch(&m_paintContext.phase2Data[i + 1], 1, 3);
        }
    }
}

QRegion WorkspaceScene::collectDamage()
{
    if (m_paintContext.mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) [[unlikely]] {
        resetRepaintsHelper(m_overlayItem.get(), painted_delegate);
        m_paintContext.damage = infiniteRegion();
        return infiniteRegion();
    }

    QRegion opaque;
    const auto numWindows = m_paintContext.phase2Data.size();

    for (std::size_t idx = numWindows; idx-- > 0;) {
        auto &paintData = m_paintContext.phase2Data[idx];
        accumulateRepaints(paintData.item, painted_delegate, &paintData.region);

        const QRegion windowDamage = paintData.region.subtracted(opaque);
        if (!windowDamage.isEmpty()) [[likely]] {
            m_paintContext.damage += windowDamage;
        }

        if (!(paintData.mask & (PAINT_WINDOW_TRANSLUCENT | PAINT_WINDOW_TRANSFORMED))) [[likely]] {
            opaque += paintData.opaque;
        }

        if (idx > 0) [[likely]] {
            __builtin_prefetch(&m_paintContext.phase2Data[idx - 1], 0, 3);
        }
    }

    accumulateRepaints(m_overlayItem.get(), painted_delegate, &m_paintContext.damage);
    return m_paintContext.damage.translated(-painted_delegate->viewport().topLeft().toPoint());
}

void WorkspaceScene::postPaint()
{
    const auto numWindows = stacking_order.size();
    for (std::size_t i = 0; i < numWindows; ++i) {
        WindowItem *w = stacking_order[i];
        effects->postPaintWindow(w->effectWindow());

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(stacking_order[i + 1], 0, 2);
        }
    }

    effects->postPaintScreen();

    painted_delegate = nullptr;
    painted_screen = nullptr;
    clearStackingOrder();
}

void WorkspaceScene::paint(const RenderTarget &renderTarget, const QRegion &region)
{
    RenderViewport viewport(painted_delegate->viewport(), painted_delegate->scale(), renderTarget);

    m_renderer->beginFrame(renderTarget, viewport);

    effects->paintScreen(renderTarget, viewport, m_paintContext.mask, region, painted_screen);
    m_paintScreenCount = 0;

    if (m_overlayItem) [[likely]] {
        const QRegion repaint = region & m_overlayItem->mapToScene(m_overlayItem->boundingRect()).toRect();
        if (!repaint.isEmpty()) [[unlikely]] {
            m_renderer->renderItem(renderTarget, viewport, m_overlayItem.get(), PAINT_SCREEN_TRANSFORMED, repaint, WindowPaintData{}, [this](Item *item) {
                return !painted_delegate->shouldRenderItem(item);
            }, [this](Item *item) {
                return painted_delegate->shouldRenderHole(item);
            });
        }
    }

    Q_EMIT frameRendered();
    m_renderer->endFrame();
}

void WorkspaceScene::finalPaintScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int mask, const QRegion &region, Output *screen)
{
    m_paintScreenCount++;
    if (mask & (PAINT_SCREEN_TRANSFORMED | PAINT_SCREEN_WITH_TRANSFORMED_WINDOWS)) [[unlikely]] {
        paintGenericScreen(renderTarget, viewport, mask, screen);
    } else [[likely]] {
        paintSimpleScreen(renderTarget, viewport, mask, region);
    }
}

void WorkspaceScene::paintGenericScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int, Output *)
{
    if (m_paintContext.mask & PAINT_SCREEN_BACKGROUND_FIRST) [[unlikely]] {
        if (m_paintScreenCount == 1) [[likely]] {
            m_renderer->renderBackground(renderTarget, viewport, infiniteRegion());
        }
    } else [[likely]] {
        m_renderer->renderBackground(renderTarget, viewport, infiniteRegion());
    }

    const auto numWindows = m_paintContext.phase2Data.size();
    for (std::size_t i = 0; i < numWindows; ++i) {
        const Phase2Data &paintData = m_paintContext.phase2Data[i];
        paintWindow(renderTarget, viewport, paintData.item, paintData.mask, paintData.region);

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(&m_paintContext.phase2Data[i + 1], 0, 3);
        }
    }
}

void WorkspaceScene::paintSimpleScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int, const QRegion &region)
{
    QRegion visible = region;

    const auto numWindows = m_paintContext.phase2Data.size();
    for (std::size_t idx = numWindows; idx-- > 0;) {
        Phase2Data &data = m_paintContext.phase2Data[idx];

        data.region = visible;

        if (!(data.mask & PAINT_WINDOW_TRANSFORMED)) [[likely]] {
            data.region &= data.item->mapToScene(data.item->boundingRect()).toAlignedRect();

            if (!(data.mask & PAINT_WINDOW_TRANSLUCENT)) [[likely]] {
                visible -= data.opaque;
            }
        }

        if (idx > 0) [[likely]] {
            __builtin_prefetch(&m_paintContext.phase2Data[idx - 1], 0, 3);
        }
    }

    m_renderer->renderBackground(renderTarget, viewport, visible);

    for (std::size_t i = 0; i < numWindows; ++i) {
        const Phase2Data &paintData = m_paintContext.phase2Data[i];
        paintWindow(renderTarget, viewport, paintData.item, paintData.mask, paintData.region);

        if (i + 1 < numWindows) [[likely]] {
            __builtin_prefetch(&m_paintContext.phase2Data[i + 1], 0, 3);
        }
    }
}

void WorkspaceScene::createStackingOrder()
{
    stacking_order.clear();
    const QList<Item *> items = m_containerItem->sortedChildItems();
    const qsizetype numItems = items.size();

    if (stacking_order.capacity() < static_cast<std::size_t>(numItems)) [[unlikely]] {
        stacking_order.reserve(static_cast<std::size_t>(numItems));
    }

    for (qsizetype i = 0; i < numItems; ++i) {
        Item *item = items[i];
        WindowItem *windowItem = static_cast<WindowItem *>(item);

        if (windowItem->isVisible()) [[likely]] {
            stacking_order.push_back(windowItem);
        }

        if (i + 2 < numItems) [[likely]] {
            __builtin_prefetch(items[i + 2], 0, 1);
        }
    }
}

void WorkspaceScene::clearStackingOrder()
{
    stacking_order.clear();
}

void WorkspaceScene::paintWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, WindowItem *item, int mask, const QRegion &region)
{
    if (region.isEmpty()) [[unlikely]] {
        return;
    }

    if (!item) [[unlikely]] {
        return;
    }

    WindowPaintData data;
    effects->paintWindow(renderTarget, viewport, item->effectWindow(), mask, region, data);
}

void WorkspaceScene::finalPaintWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, EffectWindow *w, int mask, const QRegion &region, WindowPaintData &data)
{
    if (!w) [[unlikely]] {
        return;
    }
    effects->drawWindow(renderTarget, viewport, w, mask, region, data);
}

void WorkspaceScene::finalDrawWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, EffectWindow *w, int mask, const QRegion &region, WindowPaintData &data)
{
    if (!w || !w->windowItem()) [[unlikely]] {
        return;
    }

    m_renderer->renderItem(renderTarget, viewport, w->windowItem(), mask, region, data, [this](Item *item) {
        return painted_delegate && !painted_delegate->shouldRenderItem(item);
    }, [this](Item *item) {
        return painted_delegate && painted_delegate->shouldRenderHole(item);
    });
}

EglContext *WorkspaceScene::openglContext() const
{
    if (auto *eglBackend = qobject_cast<EglBackend *>(Compositor::self()->backend())) [[likely]] {
        return eglBackend->openglContext();
    }
    return nullptr;
}

bool WorkspaceScene::animationsSupported() const
{
    const auto *context = openglContext();
    return context && !context->isSoftwareRenderer();
}

} // namespace KWin

#include "moc_workspacescene.cpp"
