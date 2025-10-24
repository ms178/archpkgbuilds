/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2006 Lubos Lunak <l.lunak@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "scene/scene.h"

#include <vector>
#include <cstddef>

namespace KWin
{

class DragAndDropIconItem;
class EffectWindow;
class EglContext;
class Item;
class SurfaceItem;
class WindowItem;
class WindowPaintData;

/**
 * Main workspace scene coordinator.
 * Manages window stacking, damage tracking, and render dispatch.
 *
 * CRITICAL HOT PATHS (called every frame @ 60-360 Hz):
 * - prePaint() - Damage calculation and effect coordination
 * - paint() - Main render dispatch
 * - createStackingOrder() / clearStackingOrder() - Window list management (O(n))
 * - scanoutCandidates() - Direct scanout eligibility (O(n×m))
 *
 * PERFORMANCE OPTIMIZATIONS:
 * - stacking_order: std::vector with pre-reserved capacity (avoids realloc)
 * - phase2Data: std::vector pre-allocated to typical scene size
 * - Tree traversals: Iterative DFS to avoid stack overhead and cache misses
 * - Branch hints: [[likely]]/[[unlikely]] guide CPU predictor
 * - const correctness: Enables compiler optimizations
 *
 * SAFETY GUARANTEES:
 * - Depth-limited recursion (max 64 levels) prevents stack overflow
 * - Null pointer checks on all dereferences
 * - Type-safe casts with validation
 * - No raw pointer ownership (RAII via smart pointers)
 */
class KWIN_EXPORT WorkspaceScene : public Scene
{
    Q_OBJECT

public:
    explicit WorkspaceScene(std::unique_ptr<ItemRenderer> renderer);
    ~WorkspaceScene() override;

    // Non-copyable, non-movable (contains Qt signal/slot machinery)
    WorkspaceScene(const WorkspaceScene &) = delete;
    WorkspaceScene &operator=(const WorkspaceScene &) = delete;
    WorkspaceScene(WorkspaceScene &&) = delete;
    WorkspaceScene &operator=(WorkspaceScene &&) = delete;

    void initialize();

    Item *containerItem() const;
    Item *overlayItem() const;

    QList<SurfaceItem *> scanoutCandidates(ssize_t maxCount) const override;
    QRegion prePaint(SceneDelegate *delegate) override;
    void postPaint() override;
    void paint(const RenderTarget &renderTarget, const QRegion &region) override;
    void frame(SceneDelegate *delegate, OutputFrame *frame) override;
    double desiredHdrHeadroom() const override;

    EglContext *openglContext() const;

    /**
     * Whether the Scene is able to drive animations.
     * Returns false for software rendering, true for hardware acceleration.
     */
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

    /**
     * Phase2 paint data (pre-allocated to avoid per-frame heap churn).
     * Stores per-window state for the optimized paint pass.
     */
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

    // Current output being painted
    Output *painted_screen = nullptr;
    SceneDelegate *painted_delegate = nullptr;

    // Windows in Z-order (bottom to top)
    // OPTIMIZATION: std::vector provides:
    // - Contiguous memory (better cache locality)
    // - O(1) random access
    // - Reserve capacity to avoid reallocation
    // Typical size: 5-20 windows, reserved: 48 (next power-of-2 above 32)
    std::vector<WindowItem *> stacking_order;

private:
    void createDndIconItem();
    void destroyDndIconItem();

    std::chrono::milliseconds m_expectedPresentTimestamp = std::chrono::milliseconds::zero();
    int m_paintScreenCount = 0;
    PaintContext m_paintContext;
    std::unique_ptr<Item> m_containerItem;
    std::unique_ptr<Item> m_overlayItem;
    std::unique_ptr<DragAndDropIconItem> m_dndIcon;
};

} // namespace KWin
