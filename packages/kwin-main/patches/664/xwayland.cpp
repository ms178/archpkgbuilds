/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2014 Martin Gräßlin <mgraesslin@kde.org>
    SPDX-FileCopyrightText: 2019 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2020 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "xwayland.h"

#include "config-kwin.h"

#include "databridge.h"
#include "dnd.h"
#include "window.h"
#include "xwaylandlauncher.h"
#include "xwldrophandler.h"

#include "core/output.h"
#include "keyboard_input.h"
#include "main_wayland.h"
#include "utils/common.h"
#include "utils/xcbutils.h"
#include "wayland/display.h"
#include "wayland_server.h"
#include "waylandwindow.h"
#include "workspace.h"
#include "x11eventfilter.h"
#include "xkb.h"
#include "xwayland_logging.h"

#include <KSelectionOwner>
#include <wayland/keyboard.h>
#include <wayland/pointer.h>
#include <wayland/seat.h>
#include <wayland/surface.h>

#include <QAbstractEventDispatcher>
#include <QDataStream>
#include <QDir>
#include <QFile>
#include <QRandomGenerator>
#include <QScopeGuard>
#include <QSocketNotifier>
#include <QTimer>
#include <QtConcurrentRun>

#include <algorithm>
#include <array>
#include <cerrno>
#include <cstring>
#include <input_event.h>
#include <sys/socket.h>
#include <unistd.h>
#include <xkbcommon/xkbcommon-keysyms.h>

namespace KWin
{
namespace Xwl
{

class XrandrEventFilter : public X11EventFilter
{
public:
    explicit XrandrEventFilter(Xwayland *backend);

    bool event(xcb_generic_event_t *event) override;

private:
    Xwayland *const m_backend;
};

XrandrEventFilter::XrandrEventFilter(Xwayland *backend)
    : X11EventFilter(Xcb::Extensions::self()->randrNotifyEvent())
    , m_backend(backend)
{
}

bool XrandrEventFilter::event(xcb_generic_event_t *event)
{
    Q_ASSERT((event->response_type & ~0x80) == Xcb::Extensions::self()->randrNotifyEvent());
    m_backend->updatePrimary();
    return false;
}

class XwaylandInputFilter : public QObject, public KWin::InputEventFilter
{
public:
    XwaylandInputFilter()
        : KWin::InputEventFilter(InputFilterOrder::XWayland)
    {
        connect(waylandServer()->seat(), &SeatInterface::focusedKeyboardSurfaceAboutToChange,
                this, [this](SurfaceInterface *newSurface) {
            auto keyboard = waylandServer()->seat()->keyboard();
            if (!newSurface || !keyboard) {
                m_cachedFocusedClient = nullptr;
                return;
            }

            m_cachedFocusedClient = newSurface->client();

            if (waylandServer()->xWaylandConnection() == newSurface->client()) {
                for (quint32 scancode = 0; scancode < m_pressed.size(); ++scancode) {
                    if (m_pressed[scancode]) {
                        keyboard->sendKey(scancode, KeyboardKeyState::Released,
                                          waylandServer()->xWaylandConnection(),
                                          waylandServer()->display()->nextSerial());
                    }
                }
                for (const quint32 scancode : std::as_const(m_pressedOverflow)) {
                    keyboard->sendKey(scancode, KeyboardKeyState::Released,
                                      waylandServer()->xWaylandConnection(),
                                      waylandServer()->display()->nextSerial());
                }
                m_modifiers = {};
                std::fill(m_pressed.begin(), m_pressed.end(), 0);
                m_pressedOverflow.clear();
            }
        });
    }

    void setMode(XwaylandEavesdropsMode mode, bool eavesdropsMouse)
    {
        static const Qt::KeyboardModifiers modifierKeys = {
            Qt::ControlModifier,
            Qt::AltModifier,
            Qt::MetaModifier,
        };

        static constexpr std::array<quint32, 83> excludedKeys = {
            Qt::Key_Multi_key,
            Qt::Key_Codeinput,
            Qt::Key_SingleCandidate,
            Qt::Key_MultipleCandidate,
            Qt::Key_PreviousCandidate,
            Qt::Key_Mode_switch,
            Qt::Key_Kanji,
            Qt::Key_Muhenkan,
            Qt::Key_Henkan,
            Qt::Key_Romaji,
            Qt::Key_Hiragana,
            Qt::Key_Katakana,
            Qt::Key_Hiragana_Katakana,
            Qt::Key_Zenkaku,
            Qt::Key_Hankaku,
            Qt::Key_Zenkaku_Hankaku,
            Qt::Key_Touroku,
            Qt::Key_Massyo,
            Qt::Key_Kana_Lock,
            Qt::Key_Kana_Shift,
            Qt::Key_Eisu_Shift,
            Qt::Key_Eisu_toggle,
            Qt::Key_Hangul,
            Qt::Key_Hangul_Start,
            Qt::Key_Hangul_End,
            Qt::Key_Hangul_Hanja,
            Qt::Key_Hangul_Jamo,
            Qt::Key_Hangul_Romaja,
            Qt::Key_Hangul_Jeonja,
            Qt::Key_Hangul_Banja,
            Qt::Key_Hangul_PreHanja,
            Qt::Key_Hangul_PostHanja,
            Qt::Key_Hangul_Special,
            Qt::Key_Dead_Grave,
            Qt::Key_Dead_Acute,
            Qt::Key_Dead_Circumflex,
            Qt::Key_Dead_Tilde,
            Qt::Key_Dead_Macron,
            Qt::Key_Dead_Breve,
            Qt::Key_Dead_Abovedot,
            Qt::Key_Dead_Diaeresis,
            Qt::Key_Dead_Abovering,
            Qt::Key_Dead_Doubleacute,
            Qt::Key_Dead_Caron,
            Qt::Key_Dead_Cedilla,
            Qt::Key_Dead_Ogonek,
            Qt::Key_Dead_Iota,
            Qt::Key_Dead_Voiced_Sound,
            Qt::Key_Dead_Semivoiced_Sound,
            Qt::Key_Dead_Belowdot,
            Qt::Key_Dead_Hook,
            Qt::Key_Dead_Horn,
            Qt::Key_Dead_Stroke,
            Qt::Key_Dead_Abovecomma,
            Qt::Key_Dead_Abovereversedcomma,
            Qt::Key_Dead_Doublegrave,
            Qt::Key_Dead_Belowring,
            Qt::Key_Dead_Belowmacron,
            Qt::Key_Dead_Belowcircumflex,
            Qt::Key_Dead_Belowtilde,
            Qt::Key_Dead_Belowbreve,
            Qt::Key_Dead_Belowdiaeresis,
            Qt::Key_Dead_Invertedbreve,
            Qt::Key_Dead_Belowcomma,
            Qt::Key_Dead_Currency,
            Qt::Key_Dead_a,
            Qt::Key_Dead_A,
            Qt::Key_Dead_e,
            Qt::Key_Dead_E,
            Qt::Key_Dead_i,
            Qt::Key_Dead_I,
            Qt::Key_Dead_o,
            Qt::Key_Dead_O,
            Qt::Key_Dead_u,
            Qt::Key_Dead_U,
            Qt::Key_Dead_Small_Schwa,
            Qt::Key_Dead_Capital_Schwa,
            Qt::Key_Dead_Greek,
            Qt::Key_Dead_Lowline,
            Qt::Key_Dead_Aboveverticalline,
            Qt::Key_Dead_Belowverticalline,
        };

        static const auto isSpecialKey = [](quint32 key) noexcept -> bool {
            if (key < 0x01000000) [[likely]] {
                return false;
            }
            return std::find(excludedKeys.begin(), excludedKeys.end(), key) == excludedKeys.end();
        };

        switch (mode) {
        case None:
            m_filterKey = {};
            m_filterMouse = false;
            break;
        case NonCharacterKeys:
            m_filterKey = [](int key, Qt::KeyboardModifiers) {
                return isSpecialKey(static_cast<quint32>(key));
            };
            m_filterMouse = eavesdropsMouse;
            break;
        case AllKeysWithModifier:
            m_filterKey = [](int key, Qt::KeyboardModifiers m) {
                return m.testAnyFlags(modifierKeys) || isSpecialKey(static_cast<quint32>(key));
            };
            m_filterMouse = eavesdropsMouse;
            break;
        case All:
            m_filterKey = [](int, Qt::KeyboardModifiers) {
                return true;
            };
            m_filterMouse = eavesdropsMouse;
            break;
        }
    }

    bool keyboardKey(KWin::KeyboardKeyEvent *event) override
    {
        ClientConnection *xwaylandClient = waylandServer()->xWaylandConnection();
        if (!xwaylandClient) [[unlikely]] {
            return false;
        }

        if (m_cachedFocusedClient == xwaylandClient) [[likely]] {
            return false;
        }

        if (event->state == KeyboardKeyState::Repeated) {
            return false;
        }

        if (!isPressed(event->nativeScanCode) && (!m_filterKey || !m_filterKey(event->key, event->modifiers))) {
            return false;
        }

        if (event->state == KeyboardKeyState::Released) {
            if (!setPressed(event->nativeScanCode, false)) {
                return false;
            }
        }

        auto keyboard = waylandServer()->seat()->keyboard();
        if (!keyboard) [[unlikely]] {
            return false;
        }

        if (!m_cachedFocusedClient) {
            auto surface = keyboard->focusedSurface();
            if (surface && surface->client() == xwaylandClient) {
                return false;
            }
        }

        if (event->state == KeyboardKeyState::Pressed) {
            if (!setPressed(event->nativeScanCode, true)) {
                return false;
            }
        }

        auto xkb = input()->keyboard()->xkb();
        if (!xkb) [[unlikely]] {
            return false;
        }

        keyboard->sendKey(event->nativeScanCode, event->state, xwaylandClient, event->serial);

        bool changed = false;
        const auto &xkbState = xkb->modifierState();
        if (m_modifiers.depressed != xkbState.depressed) {
            m_modifiers.depressed = xkbState.depressed;
            changed = true;
        }
        if (m_modifiers.latched != xkbState.latched) {
            m_modifiers.latched = xkbState.latched;
            changed = true;
        }
        if (m_modifiers.locked != xkbState.locked) {
            m_modifiers.locked = xkbState.locked;
            changed = true;
        }
        if (m_modifiers.group != xkb->currentLayout()) {
            m_modifiers.group = xkb->currentLayout();
            changed = true;
        }

        if (!changed) {
            return false;
        }

        keyboard->sendModifiers(xkbState.depressed,
                                xkbState.latched,
                                xkbState.locked,
                                xkb->currentLayout(),
                                xwaylandClient);
        return false;
    }

    bool pointerButton(KWin::PointerButtonEvent *event) override
    {
        ClientConnection *xwaylandClient = waylandServer()->xWaylandConnection();
        if (!xwaylandClient) [[unlikely]] {
            return false;
        }

        if (!m_filterMouse) {
            return false;
        }

        if (event->button == Qt::MouseButton::LeftButton
            || event->button == Qt::MouseButton::RightButton
            || event->button == Qt::MouseButton::MiddleButton) {
            return false;
        }

        auto pointer = waylandServer()->seat()->pointer();
        if (!pointer) [[unlikely]] {
            return false;
        }

        auto surface = pointer->focusedSurface();
        if (surface && surface->client() == xwaylandClient) {
            return false;
        }

        pointer->sendButton(event->nativeButton, event->state, xwaylandClient);
        return false;
    }

private:
    static constexpr size_t MaxScanCode = 0x300;

    bool isPressed(quint32 scancode) const noexcept
    {
        if (scancode < MaxScanCode) [[likely]] {
            return m_pressed[scancode] != 0;
        }
        return m_pressedOverflow.contains(scancode);
    }

    bool setPressed(quint32 scancode, bool pressed) noexcept
    {
        if (scancode < MaxScanCode) [[likely]] {
            const uint8_t old = m_pressed[scancode];
            m_pressed[scancode] = pressed ? 1 : 0;
            return old != m_pressed[scancode];
        }

        if (pressed) {
            if (m_pressedOverflow.contains(scancode)) {
                return false;
            }
            m_pressedOverflow.insert(scancode);
            return true;
        } else {
            return m_pressedOverflow.remove(scancode) > 0;
        }
    }

    alignas(64) std::array<uint8_t, MaxScanCode> m_pressed{};
    alignas(64) QSet<quint32> m_pressedOverflow;

    struct alignas(64) Modifiers
    {
        quint32 depressed = 0;
        quint32 latched = 0;
        quint32 locked = 0;
        quint32 group = 0;
    } m_modifiers;

    alignas(64) std::function<bool(int key, Qt::KeyboardModifiers)> m_filterKey;
    bool m_filterMouse = false;
    ClientConnection *m_cachedFocusedClient = nullptr;
};

Xwayland::Xwayland(Application *app)
    : m_app(app)
    , m_launcher(new XwaylandLauncher(this))
{
    connect(m_launcher, &XwaylandLauncher::ready, this, &Xwayland::handleXwaylandReady);
    connect(m_launcher, &XwaylandLauncher::finished, this, &Xwayland::handleXwaylandFinished);
    connect(m_launcher, &XwaylandLauncher::errorOccurred, this, &Xwayland::errorOccurred);
}

Xwayland::~Xwayland()
{
    m_launcher->stop();
}

void Xwayland::init()
{
    m_launcher->enable();

    auto env = m_app->processStartupEnvironment();
    env.insert(QStringLiteral("DISPLAY"), m_launcher->displayName());
    env.insert(QStringLiteral("XAUTHORITY"), m_launcher->xauthority());
    qputenv("DISPLAY", m_launcher->displayName().toLatin1());
    qputenv("XAUTHORITY", m_launcher->xauthority().toLatin1());
    m_app->setProcessStartupEnvironment(env);
}

XwaylandLauncher *Xwayland::xwaylandLauncher() const
{
    return m_launcher;
}

void Xwayland::dispatchEvents(DispatchEventsMode mode)
{
    xcb_connection_t *connection = kwinApp()->x11Connection();
    if (!connection) {
        qCWarning(KWIN_XWL, "Attempting to dispatch X11 events with no connection");
        return;
    }

    const int connectionError = xcb_connection_has_error(connection);
    if (connectionError) {
        qCWarning(KWIN_XWL, "The X11 connection broke (error %d)", connectionError);
        m_launcher->stop();
        return;
    }

    auto pollEventFunc = mode == DispatchEventsMode::Poll ? xcb_poll_for_event : xcb_poll_for_queued_event;

    QAbstractEventDispatcher *dispatcher = QCoreApplication::eventDispatcher();
    const QByteArray eventType = QByteArrayLiteral("xcb_generic_event_t");
    qintptr result = 0;

    xcb_generic_event_t *events[32];
    size_t eventCount = 0;

    while (xcb_generic_event_t *event = pollEventFunc(connection)) {
        events[eventCount++] = event;
        if (eventCount == 32) {
            for (size_t i = 0; i < eventCount; ++i) {
                dispatcher->filterNativeEvent(eventType, events[i], &result);
                free(events[i]);
            }
            eventCount = 0;
        }
    }

    for (size_t i = 0; i < eventCount; ++i) {
        dispatcher->filterNativeEvent(eventType, events[i], &result);
        free(events[i]);
    }

    xcb_flush(connection);
}

void Xwayland::installSocketNotifier()
{
    const int fileDescriptor = xcb_get_file_descriptor(kwinApp()->x11Connection());

    m_socketNotifier = new QSocketNotifier(fileDescriptor, QSocketNotifier::Read, this);
    connect(m_socketNotifier, &QSocketNotifier::activated, this, [this]() {
        dispatchEvents(DispatchEventsMode::Poll);
    });

    QAbstractEventDispatcher *dispatcher = QCoreApplication::eventDispatcher();
    connect(dispatcher, &QAbstractEventDispatcher::aboutToBlock, this, [this]() {
        dispatchEvents(DispatchEventsMode::EventQueue);
    });
    connect(dispatcher, &QAbstractEventDispatcher::awake, this, [this]() {
        dispatchEvents(DispatchEventsMode::EventQueue);
    });
}

void Xwayland::uninstallSocketNotifier()
{
    QAbstractEventDispatcher *dispatcher = QCoreApplication::eventDispatcher();
    disconnect(dispatcher, nullptr, this, nullptr);

    delete m_socketNotifier;
    m_socketNotifier = nullptr;
}

void Xwayland::handleXwaylandFinished()
{
    disconnect(workspace(), &Workspace::outputOrderChanged, this, &Xwayland::updatePrimary);

    delete m_xrandrEventsFilter;
    m_xrandrEventsFilter = nullptr;

    // If Xwayland has crashed, we must deactivate the socket notifier and ensure that no X11
    // events will be dispatched before blocking; otherwise we will simply hang...
    uninstallSocketNotifier();

    m_dataBridge.reset();
    m_compositingManagerSelectionOwner.reset();
    m_windowManagerSelectionOwner.reset();

    m_inputFilter.reset();
    disconnect(options, &Options::xwaylandEavesdropsChanged, this, &Xwayland::refreshEavesdropping);
    disconnect(options, &Options::xwaylandEavesdropsMouseChanged, this, &Xwayland::refreshEavesdropping);

    destroyX11Connection();
}

void Xwayland::handleXwaylandReady()
{
    if (!createX11Connection()) {
        Q_EMIT errorOccurred();
        return;
    }

    qCInfo(KWIN_XWL) << "Xwayland server started on display" << m_launcher->displayName();

    m_compositingManagerSelectionOwner = std::make_unique<KSelectionOwner>("_NET_WM_CM_S0", kwinApp()->x11Connection(), kwinApp()->x11RootWindow());
    m_compositingManagerSelectionOwner->claim(true);

    xcb_composite_redirect_subwindows(kwinApp()->x11Connection(),
                                      kwinApp()->x11RootWindow(),
                                      XCB_COMPOSITE_REDIRECT_MANUAL);

    // create selection owner for WM_S0 - magic X display number expected by XWayland
    m_windowManagerSelectionOwner = std::make_unique<KSelectionOwner>("WM_S0", kwinApp()->x11Connection(), kwinApp()->x11RootWindow());
    m_windowManagerSelectionOwner->claim(true);

    m_dataBridge = std::make_unique<DataBridge>();

    connect(workspace(), &Workspace::outputOrderChanged, this, &Xwayland::updatePrimary);
    updatePrimary();

    delete m_xrandrEventsFilter;
    m_xrandrEventsFilter = new XrandrEventFilter(this);

    refreshEavesdropping();
    connect(options, &Options::xwaylandEavesdropsChanged, this, &Xwayland::refreshEavesdropping);
    connect(options, &Options::xwaylandEavesdropsMouseChanged, this, &Xwayland::refreshEavesdropping);

    runXWaylandStartupScripts();

    Q_EMIT started();
}

void Xwayland::refreshEavesdropping()
{
    if (!waylandServer()->seat()->keyboard()) {
        return;
    }

    const bool enabled = options->xwaylandEavesdrops() != None;
    if (enabled == bool(m_inputFilter)) {
        if (m_inputFilter) {
            m_inputFilter->setMode(options->xwaylandEavesdrops(), options->xwaylandEavesdropsMouse());
        }
        return;
    }

    if (enabled) {
        m_inputFilter = std::make_unique<XwaylandInputFilter>();
        m_inputFilter->setMode(options->xwaylandEavesdrops(), options->xwaylandEavesdropsMouse());
        input()->installInputEventFilter(m_inputFilter.get());
    } else {
        m_inputFilter.reset();
    }
}

void Xwayland::updatePrimary()
{
    if (workspace()->outputOrder().empty()) {
        return;
    }
    Xcb::RandR::ScreenResources resources(kwinApp()->x11RootWindow());
    auto outputs = resources.outputs();
    if (!outputs) {
        qCWarning(KWIN_XWL) << "Failed to get RandR screen resources or CRTCs for updating primary output.";
        return;
    }

    Output *const primaryOutput = workspace()->outputOrder().front();
    const QString primaryOutputName = primaryOutput->name();
    for (int i = 0; i < resources->num_outputs; i++) {
        Xcb::RandR::OutputInfo outputInfo(outputs[i], resources->config_timestamp);
        if (outputInfo.name() == primaryOutputName) {
            qCDebug(KWIN_XWL) << "Setting primary" << primaryOutput << outputs[i];
            xcb_randr_set_output_primary(kwinApp()->x11Connection(), kwinApp()->x11RootWindow(), outputs[i]);
            return;
        }
    }
}

bool Xwayland::createX11Connection()
{
    xcb_connection_t *connection = xcb_connect_to_fd(m_launcher->takeXcbConnectionFd().take(), nullptr);

    const int errorCode = xcb_connection_has_error(connection);
    if (errorCode) {
        qCDebug(KWIN_XWL, "Failed to establish the XCB connection (error %d)", errorCode);
        return false;
    }

    xcb_screen_t *screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
    Q_ASSERT(screen);

    m_app->setX11Connection(connection);
    m_app->setX11RootWindow(screen->root);

    m_app->createAtoms();
    m_app->installNativeX11EventFilter();

    installSocketNotifier();

    // Note that it's very important to have valid x11RootWindow(), and atoms when the
    // rest of kwin is notified about the new X11 connection.
    Q_EMIT m_app->x11ConnectionChanged();

    return true;
}

void Xwayland::destroyX11Connection()
{
    if (!m_app->x11Connection()) {
        return;
    }

    Q_EMIT m_app->x11ConnectionAboutToBeDestroyed();

    Xcb::setInputFocus(XCB_INPUT_FOCUS_POINTER_ROOT);
    m_app->destroyAtoms();
    m_app->removeNativeX11EventFilter();

    xcb_disconnect(m_app->x11Connection());

    m_app->setX11Connection(nullptr);
    m_app->setX11RootWindow(XCB_WINDOW_NONE);

    Q_EMIT m_app->x11ConnectionChanged();
}

void Xwayland::runXWaylandStartupScripts()
{
    QDir scriptDir(XWAYLAND_SESSION_SCRIPTS);
    const QStringList scripts = scriptDir.entryList(QStringList(), QDir::Files, QDir::Name);

    for (const QString &script : scripts) {
        const QString path = scriptDir.filePath(script);
        qCDebug(KWIN_XWL) << "Running Xwayland startup script" << path;
        QProcessEnvironment environment = kwinApp()->processStartupEnvironment();

        auto *process = new QProcess;
        process->setProcessEnvironment(environment);
        process->start(path);
        connect(process, &QProcess::finished, process, &QProcess::deleteLater);
    }
}

bool Xwayland::dragMoveFilter(Window *target, const QPointF &position)
{
    if (m_dataBridge) {
        return m_dataBridge->dragMoveFilter(target, position);
    } else {
        return false;
    }
}

AbstractDropHandler *Xwayland::xwlDropHandler()
{
    if (m_dataBridge) {
        return m_dataBridge->dnd()->dropHandler();
    } else {
        return nullptr;
    }
}

} // namespace Xwl
} // namespace KWin

#include "moc_xwayland.cpp"
