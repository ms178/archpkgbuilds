/*
    SPDX-FileCopyrightText: 2015 Martin Flöser <mgraesslin@kde.org>
    SPDX-FileCopyrightText: 2018 David Edmundson <davidedmundson@kde.org>
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#include "waylandwindow.h"
#include "core/pixelgrid.h"
#include "scene/windowitem.h"
#include "wayland/clientconnection.h"
#include "wayland/display.h"
#include "wayland/surface.h"
#include "wayland_server.h"
#include "workspace.h"

#include <QFileInfo>
#include <QTimer>

#include <csignal>

#include <sys/types.h>
#include <unistd.h>

namespace KWin
{

enum WaylandGeometryType {
    WaylandGeometryClient = 0x1,
    WaylandGeometryFrame = 0x2,
    WaylandGeometryBuffer = 0x4,
};
Q_DECLARE_FLAGS(WaylandGeometryTypes, WaylandGeometryType)

WaylandWindow::WaylandWindow(SurfaceInterface *surface)
    : m_isScreenLocker(surface->client() == waylandServer()->screenLockerClientConnection())
{
    setSurface(surface);

    connect(surface, &SurfaceInterface::shadowChanged,
            this, &WaylandWindow::updateShadow);
    connect(this, &WaylandWindow::frameGeometryChanged,
            this, &WaylandWindow::updateClientOutputs);
    connect(workspace(), &Workspace::outputsChanged, this, &WaylandWindow::updateClientOutputs);

    updateResourceName();
    updateShadow();
}

std::unique_ptr<WindowItem> WaylandWindow::createItem(Item *parentItem)
{
    return std::make_unique<WindowItemWayland>(this, parentItem);
}

QString WaylandWindow::captionNormal() const
{
    return m_captionNormal;
}

QString WaylandWindow::captionSuffix() const
{
    return m_captionSuffix;
}

pid_t WaylandWindow::pid() const
{
    if (const SurfaceInterface *const s = surface()) {
        if (ClientConnection *const c = s->client()) {
            return c->processId();
        }
    }
    return pid_t(-1);
}

bool WaylandWindow::isClient() const
{
    return true;
}

bool WaylandWindow::isLockScreen() const
{
    return m_isScreenLocker;
}

bool WaylandWindow::isLocalhost() const
{
    return true;
}

QRectF WaylandWindow::resizeWithChecks(const QRectF &geometry, const QSizeF &size) const
{
    const QRectF area = workspace()->clientArea(WorkArea, this, geometry.center());
    const qreal w = qMin(size.width(), area.width());
    const qreal h = qMin(size.height(), area.height());
    return QRectF(geometry.topLeft(), QSizeF(w, h));
}

void WaylandWindow::killWindow()
{
    SurfaceInterface *const s = surface();
    if (Q_UNLIKELY(!s)) {
        return;
    }

    ClientConnection *const c = s->client();
    if (Q_UNLIKELY(!c)) {
        return;
    }

    const pid_t targetPid = c->processId();
    if (targetPid == 0) {
        c->destroy();
        return;
    }

    static const pid_t selfPid = ::getpid();
    if (targetPid == selfPid) {
        c->destroy();
        return;
    }

    ::kill(targetPid, SIGTERM);
    QTimer::singleShot(5000, c, &ClientConnection::destroy);
}

QString WaylandWindow::windowRole() const
{
    return QString();
}

bool WaylandWindow::belongsToSameApplication(const Window *other, SameApplicationChecks checks) const
{
    if (!other) {
        return false;
    }
    if (other == this) {
        return true;
    }

    const SurfaceInterface *const thisSurface = surface();
    if (thisSurface) {
        if (const SurfaceInterface *const otherSurface = other->surface()) {
            if (otherSurface->client() == thisSurface->client()) {
                return true;
            }
        }
    }

    if (checks.testFlag(SameApplicationCheck::AllowCrossProcesses)) {
        return other->desktopFileName() == desktopFileName();
    }

    return false;
}

bool WaylandWindow::belongsToDesktop() const
{
    const auto clients = waylandServer()->windows();
    const SameApplicationChecks checks;

    for (const Window *client : clients) {
        if (!client || !client->isDesktop()) {
            continue;
        }
        if (belongsToSameApplication(client, checks)) {
            return true;
        }
    }

    return false;
}

void WaylandWindow::updateClientOutputs()
{
    if (isDeleted()) {
        return;
    }

    SurfaceInterface *const s = surface();
    if (Q_UNLIKELY(!s)) {
        return;
    }

    const QRect rect = frameGeometry().toAlignedRect();
    if (rect.isEmpty()) {
        return;
    }

    static constexpr const char kLastRectProp[] = "_kwin_lastClientOutputsRect";

    if (sender() == this) {
        const QVariant cached = property(kLastRectProp);
        if (cached.isValid() && cached.toRect() == rect) {
            return;
        }
    }

    Display *const dpy = waylandServer()->display();
    s->setOutputs(dpy->outputsIntersecting(rect),
                  dpy->largestIntersectingOutput(rect));

    setProperty(kLastRectProp, rect);
}

void WaylandWindow::updateResourceName()
{
    const QFileInfo fileInfo(surface()->client()->executablePath());
    if (fileInfo.exists()) {
        const QByteArray executableFileName = fileInfo.fileName().toUtf8();
        setResourceClass(executableFileName, executableFileName);
    }
}

void WaylandWindow::updateCaption()
{
    const QString suffix = shortcutCaptionSuffix();
    if (m_captionSuffix != suffix) {
        m_captionSuffix = suffix;
        Q_EMIT captionChanged();
    }
}

void WaylandWindow::setCaption(const QString &caption)
{
    const QString simplified = caption.simplified();
    if (m_captionNormal != simplified) {
        m_captionNormal = simplified;
        Q_EMIT captionNormalChanged();
        Q_EMIT captionChanged();
    }
}

void WaylandWindow::doSetActive()
{
    if (isActive()) { // TODO: Xwayland clients must be unfocused somewhere else.
        StackingUpdatesBlocker blocker(workspace());
        workspace()->focusToNull();
    }
}

void WaylandWindow::cleanGrouping()
{
    if (transientFor()) {
        transientFor()->removeTransientFromList(this);
        setTransientFor(nullptr);
    }

    const auto children = transients();
    for (Window *transient : children) {
        removeTransientFromList(transient);
        transient->setTransientFor(nullptr);
    }
}

QRectF WaylandWindow::frameRectToBufferRect(const QRectF &rect) const
{
    const SurfaceInterface *const s = surface();
    if (Q_UNLIKELY(!s)) {
        return QRectF(rect.topLeft(), QSizeF());
    }
    return QRectF(rect.topLeft(), snapToPixels(s->size(), targetScale()));
}

void WaylandWindow::updateGeometry(const QRectF &rect)
{
    const QRectF oldClientGeometry = m_clientGeometry;
    const QRectF oldFrameGeometry = m_frameGeometry;
    const QRectF oldBufferGeometry = m_bufferGeometry;
    const Output *const oldOutput = m_output;

    const QRectF newClientGeometry = frameRectToClientRect(rect);
    const QRectF newFrameGeometry = rect;
    const QRectF newBufferGeometry = frameRectToBufferRect(rect);

    const bool clientChanged = (newClientGeometry != oldClientGeometry);
    const bool frameChanged = (newFrameGeometry != oldFrameGeometry);
    const bool bufferChanged = (newBufferGeometry != oldBufferGeometry);

    if (!clientChanged && !frameChanged && !bufferChanged) {
        return;
    }

    m_clientGeometry = newClientGeometry;
    m_frameGeometry = newFrameGeometry;
    m_bufferGeometry = newBufferGeometry;

    m_output = workspace()->outputAt(rect.center());

    if (clientChanged || frameChanged) {
        updateWindowRules(Rules::Position | Rules::Size);
    }

    if (bufferChanged) {
        Q_EMIT bufferGeometryChanged(oldBufferGeometry);
    }
    if (clientChanged) {
        Q_EMIT clientGeometryChanged(oldClientGeometry);
    }
    if (frameChanged) {
        Q_EMIT frameGeometryChanged(oldFrameGeometry);
    }
    if (oldOutput != m_output) {
        Q_EMIT outputChanged();
    }
}

void WaylandWindow::markAsMapped()
{
    if (Q_UNLIKELY(!ready_for_painting)) {
        setupCompositing();
        setReadyForPainting();
    }
}

} // namespace KWin

#include "moc_waylandwindow.cpp"
