/*
    SPDX-FileCopyrightText: 2014 Martin Gräßlin <mgraesslin@kde.org>
    SPDX-FileCopyrightText: 2018 David Edmundson <davidedmundson@kde.org>

    SPDX-License-Identifier: LGPL-2.1-only OR LGPL-3.0-only OR LicenseRef-KDE-Accepted-LGPL
*/

#pragma once

#include <wayland-server-core.h>

#include "utils/filedescriptor.h"
#include <QList>
#include <QSocketNotifier>
#include <QString>

struct wl_resource;

namespace KWin
{
class ClientConnection;
class Display;
class OutputInterface;
class OutputDeviceV2Interface;
class SeatInterface;

class DisplayPrivate
{
public:
    static DisplayPrivate *get(Display *display);
    explicit DisplayPrivate(Display *q);

    void registerSocketName(const QString &socketName);

    static void clientCreatedCallback(wl_listener *listener, void *data);

    // --- Cache Line 0 (0-64 bytes) ---
    // Hot path members grouped to minimize cache line fetches.
    Display *q;                         // 0-8
    wl_display *display = nullptr;      // 8-16
    wl_event_loop *loop = nullptr;      // 16-24
    QSocketNotifier *socketNotifier = nullptr; // 24-32

    // 'running' fits into the padding space before the 8-byte aligned wl_listener
    bool running = false;               // 32-33
    // 7 bytes padding implicit         // 33-40

    // wl_listener is 24 bytes (3 pointers), requires 8-byte alignment.
    wl_listener clientCreatedListener;  // 40-64
    // --- End of Cache Line 0 ---

    // --- Cache Line 1 (64-128 bytes) ---
    QList<OutputInterface *> outputs;                 // 64-80 (16 bytes in Qt6)
    QList<OutputDeviceV2Interface *> outputdevicesV2; // 80-96
    QList<SeatInterface *> seats;                     // 96-112
    QStringList socketNames;                          // 112-128
    // --- End of Cache Line 1 ---
};

/**
 * @brief The SecurityContext is a helper for the SecurityContextProtocol
 * It stays alive whilst closeFd remains open, listening for new connections on listenFd
 * Any new clients created via listenFd are tagged with the appId
 * It is parented to the display
 */
class SecurityContext : public QObject
{
    Q_OBJECT
public:
    SecurityContext(Display *display, FileDescriptor &&listenFd, FileDescriptor &&closeFd, const QString &appId);
    ~SecurityContext() override;

private:
    void onCloseFdActivated();
    void onListenFdActivated(QSocketDescriptor descriptor);
    Display *m_display;
    FileDescriptor m_listenFd;
    FileDescriptor m_closeFd;
    QString m_appId;
};

} // namespace KWin
