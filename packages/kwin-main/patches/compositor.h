/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2011 Arthur Arlt <a.arlt@stud.uni-heidelberg.de>
    SPDX-FileCopyrightText: 2012 Martin Gräßlin <mgraesslin@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "effect/globals.h"
#include "kwin_export.h"

#include <QHash>
#include <QObject>
#include <QRegion>

#include <chrono>
#include <memory>
#include <optional>

namespace KWin
{

class ColorDescription;
class GLTexture;
class Output;
class CursorScene;
class RenderBackend;
class RenderLayer;
class RenderLoop;
class RenderTarget;
class WorkspaceScene;
class Window;
class OutputFrame;

/**
 * Main compositor coordinator. Orchestrates scene rendering, backend presentation,
 * and frame scheduling across all outputs.
 *
 * CRITICAL HOT PATHS (called every frame @ 60-360 Hz):
 * - composite() - main render loop, VRR timing-critical
 * - prePaintPass() / paintPass() / postPaintPass() - scene traversal
 * - framePass() - frame metadata propagation
 *
 * VRR OPTIMIZATION:
 * - Early cursor updates to avoid tearing in adaptive sync
 * - Frame pacing respects min/max refresh rate windows
 * - Per-output VRR state tracking
 */
class KWIN_EXPORT Compositor : public QObject
{
    Q_OBJECT
public:
    static Compositor *create(QObject *parent = nullptr);

    enum class State {
        On = 0,
        Off,
        Starting,
        Stopping
    };

    ~Compositor() override;
    static Compositor *self();

    void start();
    void stop();

    void reinitialize();

    bool isActive();

    WorkspaceScene *scene() const
    {
        return m_scene.get();
    }

    CursorScene *cursorScene() const
    {
        return m_cursorScene.get();
    }

    RenderBackend *backend() const
    {
        return m_backend.get();
    }

    void createRenderer();

    std::pair<std::shared_ptr<GLTexture>, ColorDescription> textureForOutput(Output *output) const;

Q_SIGNALS:
    void compositingToggled(bool active);
    void aboutToDestroy();
    void aboutToToggleCompositing();
    void sceneCreated();

protected:
    explicit Compositor(QObject *parent = nullptr);

    static Compositor *s_compositor;

protected Q_SLOTS:
    void composite(RenderLoop *renderLoop);

private Q_SLOTS:
    void handleFrameRequested(RenderLoop *renderLoop);

private:
    Output *findOutput(RenderLoop *loop) const;

    void addSuperLayer(RenderLayer *layer);
    void removeSuperLayer(RenderLayer *layer);

    void prePaintPass(RenderLayer *layer, QRegion *damage);
    void postPaintPass(RenderLayer *layer);
    void paintPass(RenderLayer *layer, const RenderTarget &renderTarget, const QRegion &region);
    void framePass(RenderLayer *layer, OutputFrame *frame);

    void createScene();
    bool attemptOpenGLCompositing();
    bool attemptQPainterCompositing();
    void addOutput(Output *output);
    void removeOutput(Output *output);

    /**
     * Per-output VRR state for frame pacing optimization.
     * POD struct with explicit initialization to avoid undefined behavior.
     */
    struct VrrState {
        bool enabled = false;
        bool tearing = false;
        std::optional<std::chrono::nanoseconds> maxCursorDelay;

        // Cached gamma-encoded brightness to avoid std::pow in hot path
        double cachedBrightnessCurrent = 1.0;
        double cachedBrightnessTarget = 1.0;
    };

    CompositingType m_selectedCompositor = NoCompositing;

    State m_state = State::Off;
    std::unique_ptr<WorkspaceScene> m_scene;
    std::unique_ptr<CursorScene> m_cursorScene;
    std::unique_ptr<RenderBackend> m_backend;

    QHash<RenderLoop *, Output *> m_outputMap;
    QHash<RenderLoop *, RenderLayer *> m_superlayers;
    QHash<Output *, VrrState> m_vrrStates;
};

} // namespace KWin
