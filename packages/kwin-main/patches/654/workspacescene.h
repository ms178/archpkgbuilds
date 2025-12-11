/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2006 Lubos Lunak <l.lunak@kde.org>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "core/renderviewport.h"
#include "scene/scene.h"

#include <vector>

namespace KWin
{

class DragAndDropIconItem;
class EffectWindow;
class EglContext;
class Item;
class SurfaceItem;
class WindowItem;
class WindowPaintData;
class CursorItem;

class KWIN_EXPORT WorkspaceScene : public Scene
{
    Q_OBJECT

public:
    explicit WorkspaceScene(std::unique_ptr<ItemRenderer> renderer);
    ~WorkspaceScene() override;

    WorkspaceScene(const WorkspaceScene &) = delete;
    WorkspaceScene &operator=(const WorkspaceScene &) = delete;
    WorkspaceScene(WorkspaceScene &&) = delete;
    WorkspaceScene &operator=(WorkspaceScene &&) = delete;

    void initialize();

    Item *containerItem() const;
    Item *overlayItem() const;
    Item *cursorItem() const;

    QList<SurfaceItem *> scanoutCandidates(ssize_t maxCount) const override;
    OverlayCandidates overlayCandidates(ssize_t maxTotalCount, ssize_t maxOverlayCount, ssize_t maxUnderlayCount) const override;
    void prePaint(SceneView *delegate) override;
    QRegion collectDamage() override;
    void postPaint() override;
    void paint(const RenderTarget &renderTarget, const QRegion &region) override;
    void frame(SceneView *delegate, OutputFrame *frame) override;
    double desiredHdrHeadroom() const override;

    EglContext *openglContext() const;
    bool animationsSupported() const;

Q_SIGNALS:
    void preFrameRender();
    void frameRendered();

protected:
    void createStackingOrder();
    void clearStackingOrder();
    friend class EffectsHandler;
    void finalPaintScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int mask, const QRegion &region, Output *screen);
    void preparePaintGenericScreen();
    void paintGenericScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int mask, Output *screen);
    void preparePaintSimpleScreen();
    void paintSimpleScreen(const RenderTarget &renderTarget, const RenderViewport &viewport, int mask, const QRegion &region);
    void finalPaintWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, EffectWindow *w, int mask, const QRegion &region, WindowPaintData &data);
    void paintWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, WindowItem *w, int mask, const QRegion &region);
    void finalDrawWindow(const RenderTarget &renderTarget, const RenderViewport &viewport, EffectWindow *w, int mask, const QRegion &region, WindowPaintData &data);

    struct Phase2Data
    {
        WindowItem *item = nullptr;
        QRegion region;
        QRegion opaque;
        int mask = 0;
    };

    struct PaintContext
    {
        QRegion damage;
        int mask = 0;
        std::vector<Phase2Data> phase2Data;
    };

    Output *painted_screen = nullptr;
    SceneView *painted_delegate = nullptr;
    std::vector<WindowItem *> stacking_order;

private:
    void createDndIconItem();
    void destroyDndIconItem();
    void updateCursor();

    std::chrono::milliseconds m_expectedPresentTimestamp = std::chrono::milliseconds::zero();
    int m_paintScreenCount = 0;
    PaintContext m_paintContext;
    std::unique_ptr<Item> m_containerItem;
    std::unique_ptr<Item> m_overlayItem;
    std::unique_ptr<DragAndDropIconItem> m_dndIcon;
    std::unique_ptr<CursorItem> m_cursorItem;
};

} // namespace KWin
