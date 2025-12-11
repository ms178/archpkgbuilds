/*
    SPDX-FileCopyrightText: 2022 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "scene/scene.h"
#include "core/output.h"
#include "core/outputlayer.h"
#include "core/pixelgrid.h"
#include "core/renderviewport.h"
#include "effect/effect.h"
#include "scene/cursoritem.h"
#include "scene/item.h"
#include "scene/itemrenderer.h"
#include "scene/surfaceitem.h"

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <limits>
#include <vector>

namespace KWin
{

namespace
{

constexpr std::size_t kTraversalStackInitialCapacity = 256;

thread_local std::vector<Item *> s_traversalStack = []() {
    std::vector<Item *> stack;
    stack.reserve(kTraversalStackInitialCapacity);
    return stack;
}();

[[nodiscard]] inline std::vector<Item *> &getTraversalStack() noexcept
{
    s_traversalStack.clear();
    return s_traversalStack;
}

} // namespace

RenderView::RenderView(Output *output, OutputLayer *layer)
    : m_output(output)
    , m_layer(layer)
{
}

Output *RenderView::output() const
{
    return m_output;
}

qreal RenderView::scale() const
{
    return m_output ? m_output->scale() : 1.0;
}

OutputLayer *RenderView::layer() const
{
    return m_layer;
}

void RenderView::setLayer(OutputLayer *layer)
{
    m_layer = layer;
}

void RenderView::addRepaint(const QRegion &region)
{
    if (m_layer) [[likely]] {
        m_layer->addRepaint(region);
    }
}

void RenderView::scheduleRepaint(Item *item)
{
    if (m_layer) [[likely]] {
        m_layer->scheduleRepaint(item);
    }
}

bool RenderView::canSkipMoveRepaint(Item *item)
{
    return false;
}

bool RenderView::shouldRenderItem(Item *item) const
{
    return true;
}

void RenderView::setExclusive(bool enable)
{
}

QPointF RenderView::hotspot() const
{
    return QPointF{};
}

bool RenderView::isVisible() const
{
    return true;
}

bool RenderView::shouldRenderHole(Item *item) const
{
    return false;
}

SceneView::SceneView(Scene *scene, Output *output, OutputLayer *layer)
    : RenderView(output, layer)
    , m_scene(scene)
{
    m_scene->addView(this);
}

SceneView::~SceneView()
{
    m_scene->removeView(this);
}

QList<SurfaceItem *> SceneView::scanoutCandidates(ssize_t maxCount) const
{
    return m_scene->scanoutCandidates(maxCount);
}

void SceneView::prePaint()
{
    m_scene->prePaint(this);
}

QRegion SceneView::collectDamage()
{
    return m_scene->collectDamage();
}

void SceneView::postPaint()
{
    m_scene->postPaint();
}

void SceneView::paint(const RenderTarget &renderTarget, const QRegion &region)
{
    if (region == infiniteRegion()) [[unlikely]] {
        m_scene->paint(renderTarget, infiniteRegion());
        return;
    }

    QRegion globalRegion = region;
    globalRegion.translate(m_viewport.topLeft().toPoint());
    m_scene->paint(renderTarget, globalRegion);
}

double SceneView::desiredHdrHeadroom() const
{
    return m_scene->desiredHdrHeadroom();
}

void SceneView::frame(OutputFrame *frame)
{
    m_scene->frame(this, frame);
}

void SceneView::setViewport(const QRectF &viewport)
{
    if (viewport == m_viewport) {
        return;
    }
    m_viewport = viewport;
    addRepaint(QRect(QPoint(), m_viewport.size().toSize()));
}

void SceneView::setScale(qreal scale)
{
    if (scale == m_scale) {
        return;
    }
    m_scale = scale;
    addRepaint(QRect(QPoint(), m_viewport.size().toSize()));
}

QRectF SceneView::viewport() const
{
    return m_viewport;
}

qreal SceneView::scale() const
{
    return m_scale;
}

void SceneView::addExclusiveView(RenderView *view)
{
    m_exclusiveViews.push_back(view);
}

void SceneView::removeExclusiveView(RenderView *view)
{
    m_exclusiveViews.removeOne(view);
    m_underlayViews.removeOne(view);
}

void SceneView::addUnderlay(RenderView *view)
{
    m_underlayViews.push_back(view);
}

void SceneView::removeUnderlay(RenderView *view)
{
    m_underlayViews.removeOne(view);
}

bool SceneView::shouldRenderItem(Item *item) const
{
    for (RenderView *view : m_exclusiveViews) {
        if (view->shouldRenderItem(item)) [[unlikely]] {
            return false;
        }
    }
    return true;
}

bool SceneView::shouldRenderHole(Item *item) const
{
    for (RenderView *view : m_underlayViews) {
        if (view->shouldRenderItem(item)) [[likely]] {
            return true;
        }
    }
    return false;
}

Scene *SceneView::scene() const
{
    return m_scene;
}

ItemView::ItemView(SceneView *parentView, Item *item, Output *output, OutputLayer *layer)
    : RenderView(output, layer)
    , m_parentView(parentView)
    , m_item(item)
{
    if (parentView && parentView->scene()) [[likely]] {
        parentView->scene()->addView(this);
    }
}

ItemView::~ItemView()
{
    if (m_parentView && m_parentView->scene()) [[likely]] {
        m_parentView->scene()->removeView(this);
        if (m_exclusive) {
            m_parentView->removeExclusiveView(this);
            if (m_item) [[likely]] {
                m_item->scheduleSceneRepaint(m_item->rect());
            }
        }
    }
}

QPointF ItemView::hotspot() const
{
    if (auto cursor = qobject_cast<CursorItem *>(m_item.get())) {
        return cursor->hotspot();
    }
    return QPointF{};
}

QRectF ItemView::viewport() const
{
    if (!m_item) [[unlikely]] {
        return QRectF();
    }
    return calculateViewport(m_item->rect());
}

QRectF ItemView::calculateViewport(const QRectF &itemRect) const
{
    const qreal viewScale = scale();
    const QRectF snapped = snapToPixels(itemRect, viewScale);

    if (!m_layer) [[unlikely]] {
        return m_item->mapToView(snapped, this);
    }

    const auto recommendedSizes = m_layer->recommendedSizes();
    if (recommendedSizes.isEmpty()) {
        return m_item->mapToView(snapped, this);
    }

    const QSizeF bufferSize = scaledRect(snapped, viewScale).size();
    const QSize *bestSize = nullptr;
    std::int64_t bestArea = std::numeric_limits<std::int64_t>::max();

    for (const QSize &size : recommendedSizes) {
        if (size.width() >= bufferSize.width() && size.height() >= bufferSize.height()) {
            const std::int64_t area = static_cast<std::int64_t>(size.width()) * size.height();
            if (area < bestArea) {
                bestArea = area;
                bestSize = &size;
            }
        }
    }

    if (bestSize) {
        const QSizeF logicalSize = QSizeF(*bestSize) / viewScale;
        return m_item->mapToView(QRectF(snapped.topLeft(), logicalSize), this);
    }

    return m_item->mapToView(snapped, this);
}

bool ItemView::isVisible() const
{
    return m_item && m_item->isVisible();
}

QList<SurfaceItem *> ItemView::scanoutCandidates(ssize_t maxCount) const
{
    if (auto item = dynamic_cast<SurfaceItem *>(m_item.get())) {
        return {item};
    }
    return {};
}

void ItemView::frame(OutputFrame *frame)
{
    if (!m_output || !m_item) [[unlikely]] {
        return;
    }
    if (auto *renderLoop = m_output->renderLoop()) [[likely]] {
        const auto frameTime = std::chrono::duration_cast<std::chrono::milliseconds>(
            renderLoop->lastPresentationTimestamp());
        m_item->framePainted(this, m_output, frame, frameTime);
    }
}

void ItemView::prePaint()
{
}

QRegion ItemView::collectDamage()
{
    if (!m_item) [[unlikely]] {
        return QRegion();
    }
    QRegion damage = m_item->takeRepaints(this);
    damage.translate(-viewport().topLeft().toPoint());
    return damage;
}

void ItemView::postPaint()
{
}

void ItemView::paint(const RenderTarget &renderTarget, const QRegion &region)
{
    if (!m_item) [[unlikely]] {
        return;
    }

    QRegion globalRegion;
    if (region == infiniteRegion()) {
        globalRegion = infiniteRegion();
    } else {
        globalRegion = region;
        globalRegion.translate(viewport().topLeft().toPoint());
    }

    RenderViewport renderViewport(viewport(), m_output->scale(), renderTarget);
    auto renderer = m_item->scene()->renderer();
    renderer->beginFrame(renderTarget, renderViewport);
    renderer->renderBackground(renderTarget, renderViewport, globalRegion);
    WindowPaintData data;
    renderer->renderItem(renderTarget, renderViewport, m_item, 0, globalRegion, data,
                         [this](Item *toRender) { return toRender != m_item; }, {});
    renderer->endFrame();
}

bool ItemView::shouldRenderItem(Item *item) const
{
    return m_item && item == m_item;
}

void ItemView::setExclusive(bool enable)
{
    if (m_exclusive == enable) {
        return;
    }
    m_exclusive = enable;

    if (!m_item || !m_parentView) [[unlikely]] {
        return;
    }

    if (enable) {
        m_item->scheduleSceneRepaint(m_item->rect());
        m_parentView->addRepaint(m_item->takeRepaints(m_parentView));
        m_parentView->addExclusiveView(this);
        if (m_underlay) {
            m_parentView->addUnderlay(this);
        }
    } else {
        m_parentView->removeExclusiveView(this);
        m_item->scheduleRepaint(m_item->rect());
    }
}

void ItemView::setUnderlay(bool underlay)
{
    if (m_underlay == underlay) {
        return;
    }
    m_underlay = underlay;
    if (!m_exclusive || !m_parentView) {
        return;
    }
    if (m_underlay) {
        m_parentView->addUnderlay(this);
    } else {
        m_parentView->removeUnderlay(this);
    }
    if (m_item) [[likely]] {
        m_item->scheduleSceneRepaint(m_item->rect());
    }
}

bool ItemView::needsRepaint()
{
    return m_item && m_item->hasRepaints(this);
}

bool ItemView::canSkipMoveRepaint(Item *item)
{
    return m_layer && item == m_item;
}

Item *ItemView::item() const
{
    return m_item;
}

double ItemView::desiredHdrHeadroom() const
{
    if (!m_item) [[unlikely]] {
        return 1.0;
    }
    const auto &color = m_item->colorDescription();
    const double refLuminance = color->referenceLuminance();

    if (refLuminance <= 0.0) [[unlikely]] {
        return 1.0;
    }

    const double max = color->maxHdrLuminance().value_or(refLuminance);
    return max / refLuminance;
}

ItemTreeView::ItemTreeView(SceneView *parentView, Item *item, Output *output, OutputLayer *layer)
    : ItemView(parentView, item, output, layer)
{
}

ItemTreeView::~ItemTreeView()
{
    if (m_exclusive && m_item) {
        m_item->scheduleRepaint(m_item->boundingRect());
    }
}

QRectF ItemTreeView::viewport() const
{
    if (!m_item) [[unlikely]] {
        return QRectF();
    }
    return calculateViewport(m_item->boundingRect());
}

QList<SurfaceItem *> ItemTreeView::scanoutCandidates(ssize_t maxCount) const
{
    auto *surfaceItem = dynamic_cast<SurfaceItem *>(m_item.get());
    if (!surfaceItem) [[likely]] {
        return {};
    }
    const auto &childItems = m_item->childItems();
    for (const Item *child : childItems) {
        if (child->isVisible()) {
            return {};
        }
    }
    return {surfaceItem};
}

namespace
{

void accumulateRepaints(Item *rootItem, RenderView *view, QRegion &repaints)
{
    std::vector<Item *> &stack = getTraversalStack();
    stack.push_back(rootItem);

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        repaints += item->takeRepaints(view);

        const auto &children = item->childItems();
        const auto count = children.size();
        for (qsizetype i = count - 1; i >= 0; --i) {
            stack.push_back(children[i]);
        }
    }
}

[[nodiscard]] bool checkNeedsRepaint(Item *rootItem, RenderView *view)
{
    std::vector<Item *> &stack = getTraversalStack();
    stack.push_back(rootItem);

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        if (item->hasRepaints(view)) {
            return true;
        }

        const auto &children = item->childItems();
        const auto count = children.size();
        for (qsizetype i = count - 1; i >= 0; --i) {
            stack.push_back(children[i]);
        }
    }
    return false;
}

[[nodiscard]] double computeMaxHdrHeadroom(Item *rootItem)
{
    std::vector<Item *> &stack = getTraversalStack();
    stack.push_back(rootItem);

    double maxHeadroom = 1.0;

    while (!stack.empty()) {
        Item *item = stack.back();
        stack.pop_back();

        const auto &color = item->colorDescription();
        const double refLuminance = color->referenceLuminance();

        if (refLuminance > 0.0) [[likely]] {
            const double maxLum = color->maxHdrLuminance().value_or(refLuminance);
            maxHeadroom = std::max(maxHeadroom, maxLum / refLuminance);
        }

        const auto &children = item->childItems();
        const auto count = children.size();
        for (qsizetype i = count - 1; i >= 0; --i) {
            stack.push_back(children[i]);
        }
    }
    return maxHeadroom;
}

void transferPendingRepaints(RenderView *view, Item *item)
{
    view->addRepaint(item->takeRepaints(view));
    const auto &children = item->childItems();
    for (Item *child : children) {
        transferPendingRepaints(view, child);
    }
}

} // namespace

QRegion ItemTreeView::collectDamage()
{
    if (!m_item) [[unlikely]] {
        return QRegion();
    }
    QRegion damage;
    accumulateRepaints(m_item, this, damage);
    damage.translate(-viewport().topLeft().toPoint());
    return damage;
}

void ItemTreeView::paint(const RenderTarget &renderTarget, const QRegion &region)
{
    if (!m_item) [[unlikely]] {
        return;
    }

    QRegion globalRegion;
    if (region == infiniteRegion()) {
        globalRegion = infiniteRegion();
    } else {
        globalRegion = region;
        globalRegion.translate(viewport().topLeft().toPoint());
    }

    RenderViewport renderViewport(viewport(), m_output->scale(), renderTarget);
    auto renderer = m_item->scene()->renderer();
    renderer->beginFrame(renderTarget, renderViewport);
    renderer->renderBackground(renderTarget, renderViewport, globalRegion);
    WindowPaintData data;
    renderer->renderItem(renderTarget, renderViewport, m_item, 0, globalRegion, data, {}, {});
    renderer->endFrame();
}

bool ItemTreeView::shouldRenderItem(Item *item) const
{
    return m_item && (item == m_item || m_item->isAncestorOf(item));
}

void ItemTreeView::setExclusive(bool enable)
{
    if (m_exclusive == enable) {
        return;
    }
    m_exclusive = enable;

    if (!m_item || !m_parentView) [[unlikely]] {
        return;
    }

    if (enable) {
        m_item->scheduleSceneRepaint(m_item->boundingRect());
        transferPendingRepaints(m_parentView, m_item);
        m_parentView->addExclusiveView(this);
        if (m_underlay) {
            m_parentView->addUnderlay(this);
        }
    } else {
        m_parentView->removeExclusiveView(this);
        m_item->scheduleRepaint(m_item->boundingRect());
    }
}

bool ItemTreeView::needsRepaint()
{
    return m_item && checkNeedsRepaint(m_item, this);
}

bool ItemTreeView::isVisible() const
{
    return m_item && m_item->hasVisibleContents();
}

bool ItemTreeView::canSkipMoveRepaint(Item *item)
{
    return m_layer && item == m_item;
}

double ItemTreeView::desiredHdrHeadroom() const
{
    if (!m_item) [[unlikely]] {
        return 1.0;
    }
    return computeMaxHdrHeadroom(m_item);
}

Scene::Scene(std::unique_ptr<ItemRenderer> &&renderer)
    : m_renderer(std::move(renderer))
{
}

Scene::~Scene()
{
}

ItemRenderer *Scene::renderer() const
{
    return m_renderer.get();
}

void Scene::addRepaintFull()
{
    addRepaint(m_geometry);
}

void Scene::addRepaint(int x, int y, int width, int height)
{
    addRepaint(QRegion(x, y, width, height));
}

void Scene::addRepaint(const QRegion &region)
{
    if (region.isEmpty()) [[unlikely]] {
        return;
    }

    for (RenderView *view : m_views) {
        const QRectF viewportF = view->viewport();
        const QRect viewportRect = viewportF.toAlignedRect();

        QRegion localDamage = region.intersected(viewportRect);
        if (!localDamage.isEmpty()) [[likely]] {
            localDamage.translate(-viewportF.topLeft().toPoint());
            view->addRepaint(localDamage);
        }
    }
}

void Scene::addRepaint(RenderView *view, const QRegion &region)
{
    QRegion localRegion = region;
    localRegion.translate(-view->viewport().topLeft().toPoint());
    view->addRepaint(localRegion);
}

QRegion Scene::damage() const
{
    return QRegion();
}

QRect Scene::geometry() const
{
    return m_geometry;
}

void Scene::setGeometry(const QRect &rect)
{
    if (m_geometry != rect) {
        m_geometry = rect;
        addRepaintFull();
    }
}

QList<RenderView *> Scene::views() const
{
    return m_views;
}

void Scene::addView(RenderView *view)
{
    m_views.append(view);
}

void Scene::removeView(RenderView *view)
{
    m_views.removeOne(view);
    Q_EMIT viewRemoved(view);
}

QList<SurfaceItem *> Scene::scanoutCandidates(ssize_t maxCount) const
{
    return {};
}

void Scene::frame(SceneView *view, OutputFrame *frame)
{
}

double Scene::desiredHdrHeadroom() const
{
    return 1.0;
}

} // namespace KWin

#include "moc_scene.cpp"
