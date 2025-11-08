/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2015 Martin Gräßlin <mgraesslin@kde.org>
    SPDX-FileCopyrightText: 2019 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include "core/output.h"
#include "cursor.h"
#include "effect/globals.h"
#include "options.h"
#include "rules.h"
#include "scene/borderradius.h"
#include "utils/common.h"
#include "utils/gravity.h"

#include <functional>
#include <memory>
#include <optional>

#include <QElapsedTimer>
#include <QIcon>
#include <QKeySequence>
#include <QMatrix4x4>
#include <QObject>
#include <QPointer>
#include <QRectF>
#include <QTimer>
#include <QUuid>

#if KWIN_BUILD_X11
#include <xcb/xcb.h>
#endif

class QMouseEvent;

namespace KDecoration3
{
class Decoration;
}

namespace KWin
{
class PlasmaWindowInterface;
class SurfaceInterface;
class Group;
class Output;
class ClientMachine;
class EffectWindow;
class Tile;
class Shadow;
class SurfaceItem;
class VirtualDesktop;
class WindowItem;

namespace Decoration
{
class DecoratedWindowImpl;
class DecorationPalette;
}

using ElectricBorderMode = std::variant<QuickTileMode, MaximizeMode>;
using PlacementCommand = std::variant<QPointF, QRectF, MaximizeMode>;

class KWIN_EXPORT Window : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QRectF bufferGeometry READ bufferGeometry NOTIFY bufferGeometryChanged)
    Q_PROPERTY(QRectF clientGeometry READ clientGeometry NOTIFY clientGeometryChanged)
    Q_PROPERTY(QPointF pos READ pos)
    Q_PROPERTY(QSizeF size READ size)
    Q_PROPERTY(qreal x READ x NOTIFY frameGeometryChanged)
    Q_PROPERTY(qreal y READ y NOTIFY frameGeometryChanged)
    Q_PROPERTY(qreal width READ width NOTIFY frameGeometryChanged)
    Q_PROPERTY(qreal height READ height NOTIFY frameGeometryChanged)
    Q_PROPERTY(qreal opacity READ opacity WRITE setOpacity NOTIFY opacityChanged)
    Q_PROPERTY(KWin::Output *output READ output NOTIFY outputChanged)
    Q_PROPERTY(QRectF rect READ rect)
    Q_PROPERTY(QString resourceName READ resourceName NOTIFY windowClassChanged)
    Q_PROPERTY(QString resourceClass READ resourceClass NOTIFY windowClassChanged)
    Q_PROPERTY(QString windowRole READ windowRole NOTIFY windowRoleChanged)
    Q_PROPERTY(bool desktopWindow READ isDesktop CONSTANT)
    Q_PROPERTY(bool dock READ isDock CONSTANT)
    Q_PROPERTY(bool toolbar READ isToolbar CONSTANT)
    Q_PROPERTY(bool menu READ isMenu CONSTANT)
    Q_PROPERTY(bool normalWindow READ isNormalWindow CONSTANT)
    Q_PROPERTY(bool dialog READ isDialog CONSTANT)
    Q_PROPERTY(bool splash READ isSplash CONSTANT)
    Q_PROPERTY(bool utility READ isUtility CONSTANT)
    Q_PROPERTY(bool dropdownMenu READ isDropdownMenu CONSTANT)
    Q_PROPERTY(bool popupMenu READ isPopupMenu CONSTANT)
    Q_PROPERTY(bool tooltip READ isTooltip CONSTANT)
    Q_PROPERTY(bool notification READ isNotification CONSTANT)
    Q_PROPERTY(bool criticalNotification READ isCriticalNotification CONSTANT)
    Q_PROPERTY(bool appletPopup READ isAppletPopup CONSTANT)
    Q_PROPERTY(bool onScreenDisplay READ isOnScreenDisplay CONSTANT)
    Q_PROPERTY(bool comboBox READ isComboBox CONSTANT)
    Q_PROPERTY(bool dndIcon READ isDNDIcon CONSTANT)
    Q_PROPERTY(WindowType windowType READ windowType CONSTANT)
    Q_PROPERTY(bool managed READ isClient CONSTANT)
    Q_PROPERTY(bool deleted READ isDeleted CONSTANT)
    Q_PROPERTY(bool skipsCloseAnimation READ skipsCloseAnimation WRITE setSkipCloseAnimation NOTIFY skipCloseAnimationChanged)
    Q_PROPERTY(bool popupWindow READ isPopupWindow)
    Q_PROPERTY(bool outline READ isOutline)
    Q_PROPERTY(QUuid internalId READ internalId CONSTANT)
    Q_PROPERTY(int pid READ pid CONSTANT)
    Q_PROPERTY(int stackingOrder READ stackingOrder NOTIFY stackingOrderChanged)
    Q_PROPERTY(bool fullScreen READ isFullScreen WRITE setFullScreen NOTIFY fullScreenChanged)
    Q_PROPERTY(bool fullScreenable READ isFullScreenable)
    Q_PROPERTY(bool active READ isActive NOTIFY activeChanged)
    Q_PROPERTY(QList<KWin::VirtualDesktop *> desktops READ desktops WRITE setDesktops NOTIFY desktopsChanged)
    Q_PROPERTY(bool onAllDesktops READ isOnAllDesktops WRITE setOnAllDesktops NOTIFY desktopsChanged)
    Q_PROPERTY(QStringList activities READ activities WRITE setOnActivities NOTIFY activitiesChanged)
    Q_PROPERTY(bool skipTaskbar READ skipTaskbar WRITE setSkipTaskbar NOTIFY skipTaskbarChanged)
    Q_PROPERTY(bool skipPager READ skipPager WRITE setSkipPager NOTIFY skipPagerChanged)
    Q_PROPERTY(bool skipSwitcher READ skipSwitcher WRITE setSkipSwitcher NOTIFY skipSwitcherChanged)
    Q_PROPERTY(bool closeable READ isCloseable NOTIFY closeableChanged)
    Q_PROPERTY(QIcon icon READ icon NOTIFY iconChanged)
    Q_PROPERTY(bool keepAbove READ keepAbove WRITE setKeepAbove NOTIFY keepAboveChanged)
    Q_PROPERTY(bool keepBelow READ keepBelow WRITE setKeepBelow NOTIFY keepBelowChanged)
    Q_PROPERTY(bool minimizable READ isMinimizable)
    Q_PROPERTY(bool minimized READ isMinimized WRITE setMinimized NOTIFY minimizedChanged)
    Q_PROPERTY(QRectF iconGeometry READ iconGeometry)
    Q_PROPERTY(bool specialWindow READ isSpecialWindow)
    Q_PROPERTY(bool demandsAttention READ isDemandingAttention WRITE demandAttention NOTIFY demandsAttentionChanged)
    Q_PROPERTY(QString caption READ caption NOTIFY captionChanged)
    Q_PROPERTY(QString captionNormal READ captionNormal NOTIFY captionNormalChanged)
    Q_PROPERTY(QSizeF minSize READ minSize)
    Q_PROPERTY(QSizeF maxSize READ maxSize)
    Q_PROPERTY(bool wantsInput READ wantsInput)
    Q_PROPERTY(bool transient READ isTransient NOTIFY transientChanged)
    Q_PROPERTY(KWin::Window *transientFor READ transientFor NOTIFY transientChanged)
    Q_PROPERTY(bool modal READ isModal NOTIFY modalChanged)
    Q_PROPERTY(QRectF frameGeometry READ frameGeometry WRITE moveResize NOTIFY frameGeometryChanged)
    Q_PROPERTY(bool move READ isInteractiveMove NOTIFY moveResizedChanged)
    Q_PROPERTY(bool resize READ isInteractiveResize NOTIFY moveResizedChanged)
    Q_PROPERTY(bool decorationHasAlpha READ decorationHasAlpha)
    Q_PROPERTY(bool noBorder READ noBorder WRITE setNoBorder NOTIFY noBorderChanged)
    Q_PROPERTY(bool providesContextHelp READ providesContextHelp CONSTANT)
    Q_PROPERTY(bool maximizable READ isMaximizable)
    Q_PROPERTY(KWin::MaximizeMode maximizeMode READ maximizeMode NOTIFY maximizedChanged)
    Q_PROPERTY(bool moveable READ isMovable)
    Q_PROPERTY(bool moveableAcrossScreens READ isMovableAcrossScreens)
    Q_PROPERTY(bool resizeable READ isResizable)
    Q_PROPERTY(QString desktopFileName READ desktopFileName NOTIFY desktopFileNameChanged)
    Q_PROPERTY(bool hasApplicationMenu READ hasApplicationMenu NOTIFY hasApplicationMenuChanged)
    Q_PROPERTY(bool applicationMenuActive READ applicationMenuActive NOTIFY applicationMenuActiveChanged)
    Q_PROPERTY(bool unresponsive READ unresponsive NOTIFY unresponsiveChanged)
    Q_PROPERTY(QString colorScheme READ colorScheme NOTIFY colorSchemeChanged)
    Q_PROPERTY(KWin::Layer layer READ layer)
    Q_PROPERTY(bool hidden READ isHidden NOTIFY hiddenChanged)
    Q_PROPERTY(KWin::Tile *tile READ requestedTile WRITE setTileCompatibility NOTIFY tileChanged)
    Q_PROPERTY(bool inputMethod READ isInputMethod)
    Q_PROPERTY(QString tag READ tag NOTIFY tagChanged)
    Q_PROPERTY(QString description READ description NOTIFY descriptionChanged)

public:
    ~Window() override;

    void ref();
    void unref();

    QRectF moveResizeGeometry() const;
    Output *moveResizeOutput() const;
    void setMoveResizeOutput(Output *output);

    QRectF bufferGeometry() const;
    QRectF frameGeometry() const;
    QRectF clientGeometry() const;
    QMargins frameMargins() const;

    BorderRadius borderRadius() const;
    void setBorderRadius(const BorderRadius &radius);

    virtual QSizeF minSize() const;
    virtual QSizeF maxSize() const;
    QSizeF size() const;
    QPointF pos() const;
    QRectF rect() const;
    qreal x() const;
    qreal y() const;
    qreal width() const;
    qreal height() const;
    bool isOnOutput(Output *output) const;
    bool isOnActiveOutput() const;
    Output *output() const;
    void setOutput(Output *output);
    QSizeF clientSize() const;
    QRectF visibleGeometry() const;

    QPointF mapToFrame(const QPointF &point) const;
    QPointF mapToLocal(const QPointF &point) const;
    QPointF mapFromLocal(const QPointF &point) const;

    virtual QPointF framePosToClientPos(const QPointF &point) const;
    virtual QPointF nextFramePosToClientPos(const QPointF &point) const;
    virtual QPointF clientPosToFramePos(const QPointF &point) const;
    virtual QPointF nextClientPosToFramePos(const QPointF &point) const;
    virtual QSizeF frameSizeToClientSize(const QSizeF &size) const;
    virtual QSizeF nextFrameSizeToClientSize(const QSizeF &size) const;
    virtual QSizeF clientSizeToFrameSize(const QSizeF &size) const;
    virtual QSizeF nextClientSizeToFrameSize(const QSizeF &size) const;
    QRectF frameRectToClientRect(const QRectF &rect) const;
    QRectF nextFrameRectToClientRect(const QRectF &rect) const;
    QRectF clientRectToFrameRect(const QRectF &rect) const;
    QRectF nextClientRectToFrameRect(const QRectF &rect) const;

    enum SizeMode {
        SizeModeAny,
        SizeModeFixedW,
        SizeModeFixedH,
        SizeModeMax
    };

    virtual QSizeF constrainClientSize(const QSizeF &size, SizeMode mode = SizeModeAny) const;
    QSizeF constrainFrameSize(const QSizeF &size, SizeMode mode = SizeModeAny) const;

    void move(const QPointF &topLeft);
    void resize(const QSizeF &size);
    void moveResize(const QRectF &geometry);

    bool isPlaced() const;
    void place(const PlacementCommand &placement);

    void growHorizontal();
    void shrinkHorizontal();
    void growVertical();
    void shrinkVertical();

    virtual QRectF resizeWithChecks(const QRectF &geometry, const QSizeF &s) const = 0;
    QRectF keepInArea(QRectF geometry, QRectF area, bool partial = false) const;

    virtual WindowType windowType() const = 0;
    bool hasNETSupport() const;
    bool isDesktop() const;
    bool isDock() const;
    bool isToolbar() const;
    bool isMenu() const;
    bool isNormalWindow() const;
    bool isDialog() const;
    bool isSplash() const;
    bool isUtility() const;
    bool isDropdownMenu() const;
    bool isPopupMenu() const;
    bool isTooltip() const;
    bool isNotification() const;
    bool isCriticalNotification() const;
    bool isAppletPopup() const;
    bool isOnScreenDisplay() const;
    bool isComboBox() const;
    bool isDNDIcon() const;

    virtual bool isLockScreen() const;
    virtual bool isInputMethod() const;
    virtual bool isOutline() const;
    virtual bool isInternal() const;
    virtual bool isPopupWindow() const;

    virtual bool isClient() const;
    bool isDeleted() const;
    virtual bool isUnmanaged() const;
    virtual bool isPictureInPicture() const;

    bool isLockScreenOverlay() const;
    void setLockScreenOverlay(bool allowed);

    QStringList desktopIds() const;
    QList<VirtualDesktop *> desktops() const;
    void setDesktops(QList<VirtualDesktop *> desktops);
    void enterDesktop(VirtualDesktop *desktop);
    void leaveDesktop(VirtualDesktop *desktop);
    bool isOnDesktop(VirtualDesktop *desktop) const;
    bool isOnCurrentDesktop() const;
    bool isOnAllDesktops() const;
    void setOnAllDesktops(bool set);

    virtual QStringList activities() const;
    bool isOnActivity(const QString &activity) const;
    bool isOnCurrentActivity() const;
    bool isOnAllActivities() const;
    void setOnActivity(const QString &activity, bool enable);
    void setOnActivities(const QStringList &newActivitiesList);
    void setOnAllActivities(bool all);
    virtual void updateActivities(bool includeTransients);
    void blockActivityUpdates(bool b = true);
    virtual void checkActivities(){};

    virtual QString windowRole() const;
    QString resourceName() const;
    QString resourceClass() const;
    QString wmClientMachine(bool use_localhost) const;
    ClientMachine *clientMachine() const;
    virtual bool isLocalhost() const;
    virtual pid_t pid() const;

    bool readyForPainting() const;
    void setOpacity(qreal opacity);
    qreal opacity() const;
    bool setupCompositing();
    void finishCompositing();
    EffectWindow *effectWindow();
    const EffectWindow *effectWindow() const;
    SurfaceItem *surfaceItem() const;
    WindowItem *windowItem() const;

    Shadow *shadow() const;
    void updateShadow();
    bool wantsShadowToBeRendered() const;

    bool skipsCloseAnimation() const;
    void setSkipCloseAnimation(bool set);

    SurfaceInterface *surface() const;
    void setSurface(SurfaceInterface *surface);

    QMatrix4x4 inputTransformation() const;

    virtual bool hitTest(const QPointF &point) const;

    virtual bool hasPopupGrab() const
    {
        return false;
    }
    virtual void popupDone(){};

    template<class T, class U>
    static T *findInList(const QList<T *> &list, std::function<bool(const U *)> func);

    QUuid internalId() const
    {
        return m_internalId;
    }

    int stackingOrder() const;
    void setStackingOrder(int order);

    bool skipSwitcher() const
    {
        return m_skipSwitcher;
    }
    void setSkipSwitcher(bool set);

    bool skipTaskbar() const
    {
        return m_skipTaskbar;
    }
    void setSkipTaskbar(bool set);
    void setOriginalSkipTaskbar(bool set);
    bool originalSkipTaskbar() const
    {
        return m_originalSkipTaskbar;
    }

    bool skipPager() const
    {
        return m_skipPager;
    }
    void setSkipPager(bool set);

    const QIcon &icon() const
    {
        return m_icon;
    }

    bool isActive() const
    {
        return m_active;
    }
    void setActive(bool);

    bool keepAbove() const
    {
        return m_keepAbove;
    }
    void setKeepAbove(bool);
    bool keepBelow() const
    {
        return m_keepBelow;
    }
    void setKeepBelow(bool);

    void demandAttention(bool set = true);
    bool isDemandingAttention() const
    {
        return m_demandsAttention;
    }

    void cancelAutoRaise();

    QString caption() const;
    virtual QString captionNormal() const = 0;
    virtual QString captionSuffix() const = 0;
    virtual bool isPlaceable() const;
    virtual bool isCloseable() const = 0;
    bool isShown() const;
    bool isHidden() const;
    void setHidden(bool hidden);
    bool isHiddenByShowDesktop() const;
    void setHiddenByShowDesktop(bool hidden);
    Window *findModal() const;
    virtual bool isTransient() const;
    const Window *transientFor() const;
    Window *transientFor();
    void setTransientFor(Window *transientFor);
    virtual bool hasTransient(const Window *transient, bool indirect) const;
    const QList<Window *> &transients() const;
    virtual void addTransient(Window *transient);
    virtual void removeTransient(Window *transient);
    void removeTransientFromList(Window *cl);
    virtual QList<Window *> mainWindows() const;
    QList<Window *> allMainWindows() const;
    bool isSpecialWindow() const;
    void sendToOutput(Output *output);
    const QKeySequence &shortcut() const
    {
        return _shortcut;
    }
    void setShortcut(const QString &cut);

    QRectF iconGeometry() const;

    void setMinimized(bool set);
    bool isMinimized() const
    {
        return m_minimized;
    }
    virtual bool isMinimizable() const;

    bool isSuspended() const;
    void setSuspended(bool suspended);

    QRectF fullscreenGeometryRestore() const;
    void setFullscreenGeometryRestore(const QRectF &geom);
    virtual bool isFullScreenable() const;
    virtual bool isFullScreen() const;
    virtual bool isRequestedFullScreen() const;
    virtual void setFullScreen(bool set);

    bool wantsAdaptiveSync() const;
    bool wantsTearing(bool tearingRequested) const;

    QRectF geometryRestore() const;
    void setGeometryRestore(const QRectF &rect);
    virtual bool isMaximizable() const;
    virtual MaximizeMode maximizeMode() const;
    virtual MaximizeMode requestedMaximizeMode() const;
    virtual void maximize(MaximizeMode mode, const QRectF &restore = QRectF());
    Q_INVOKABLE void setMaximize(bool vertically, bool horizontally, const QRectF &restore = QRectF());

    QPalette palette();
    const Decoration::DecorationPalette *decorationPalette();

    virtual bool isResizable() const = 0;
    virtual bool isMovable() const = 0;
    virtual bool isMovableAcrossScreens() const = 0;

    const WindowRules *rules() const
    {
        return &m_rules;
    }
    void removeRule(Rules *r);
    void setupWindowRules();
    void finishWindowRules();
    void evaluateWindowRules();
    virtual void updateWindowRules(Rules::Types selection);
    virtual void applyWindowRules();
    virtual bool supportsWindowRules() const;

    bool wantsTabFocus() const;
    virtual bool takeFocus() = 0;
    virtual bool wantsInput() const = 0;
    void checkWorkspacePosition(QRectF oldGeometry = QRectF(), const VirtualDesktop *oldDesktop = nullptr);
#if KWIN_BUILD_X11
    virtual xcb_timestamp_t userTime() const;
#endif

    void keyPressEvent(QKeyCombination key_code);

    virtual void pointerEnterEvent(const QPointF &globalPos);
    virtual void pointerLeaveEvent();

    void packTo(qreal left, qreal top);

    Tile *tile() const;
    void commitTile(Tile *tile);
    Tile *requestedTile() const;
    void requestTile(Tile *tile);
    void forgetTile(Tile *tile);
    void setTileCompatibility(Tile *tile);

    void handleQuickTileShortcut(QuickTileMode mode);
    void setQuickTileModeAtCurrentPosition(QuickTileMode mode);
    void setQuickTileMode(QuickTileMode mode, const QPointF &tileAtPoint);
    QuickTileMode quickTileMode() const;
    QuickTileMode requestedQuickTileMode() const;

    void handleCustomQuickTileShortcut(QuickTileMode mode);

    Layer layer() const;
    void updateLayer();

    bool isInteractiveMove() const
    {
        return isInteractiveMoveResize() && interactiveMoveResizeGravity() == Gravity::None;
    }
    bool isInteractiveResize() const
    {
        return isInteractiveMoveResize() && interactiveMoveResizeGravity() != Gravity::None;
    }
    Gravity interactiveMoveResizeGravity() const
    {
        return m_interactiveMoveResize.gravity;
    }
    QPointF interactiveMoveResizeAnchor() const
    {
        return m_interactiveMoveResize.anchor;
    }
    CursorShape cursor() const
    {
        return m_interactiveMoveResize.cursor;
    }
    uint32_t interactiveMoveResizeCount() const;

    void updateInteractiveMoveResize(const QPointF &global, Qt::KeyboardModifiers modifiers);
    void endInteractiveMoveResize();
    void cancelInteractiveMoveResize();

    virtual StrutRect strutRect(StrutArea area) const;
    StrutRects strutRects() const;
    virtual bool hasStrut() const;

    void setModal(bool modal);
    bool isModal() const;

    std::optional<Options::MouseCommand> getMousePressCommand(Qt::MouseButton button) const;
    std::optional<Options::MouseCommand> getMouseReleaseCommand(Qt::MouseButton button) const;
    std::optional<Options::MouseCommand> getWheelCommand(Qt::Orientation orientation) const;
    bool mousePressCommandConsumesEvent(Options::MouseCommand command) const;
    bool performMousePressCommand(Options::MouseCommand, const QPointF &globalPos);
    void performMouseReleaseCommand(Options::MouseCommand, const QPointF &globalPos);

    Qt::Edge titlebarPosition() const;
    bool titlebarPositionUnderMouse() const;
    KDecoration3::Decoration *decoration() const
    {
        return m_decoration.decoration.get();
    }
    virtual KDecoration3::Decoration *nextDecoration() const;
    bool isDecorated() const
    {
        return m_decoration.decoration != nullptr;
    }
    Decoration::DecoratedWindowImpl *decoratedWindow() const;
    void setDecoratedWindow(Decoration::DecoratedWindowImpl *client);
    bool decorationHasAlpha() const;
    void triggerDecorationRepaint();
    void layoutDecorationRects(QRectF &left, QRectF &top, QRectF &right, QRectF &bottom) const;
    void processDecorationMove(const QPointF &localPos, const QPointF &globalPos);
    bool processDecorationButtonPress(const QPointF &localPos, const QPointF &globalPos, Qt::MouseButton button, bool ignoreMenu = false);
    void processDecorationButtonRelease(Qt::MouseButton button);

    virtual void invalidateDecoration();

    virtual bool noBorder() const;
    virtual void setNoBorder(bool set);
    virtual bool userCanSetNoBorder() const;
    virtual void checkNoBorder();

    virtual bool providesContextHelp() const;
    virtual void showContextHelp();

    QRectF virtualKeyboardGeometry() const;
    virtual void setVirtualKeyboardGeometry(const QRectF &geo);
    virtual void showOnScreenEdge();

    QString desktopFileName() const
    {
        return m_desktopFileName;
    }
    static QString iconFromDesktopFile(const QString &fileName);
    static QString findDesktopFile(const QString &fileName);

    virtual void killWindow() = 0;
    virtual void destroyWindow() = 0;

    enum class SameApplicationCheck {
        RelaxedForActive = 1 << 0,
        AllowCrossProcesses = 1 << 1
    };
    Q_DECLARE_FLAGS(SameApplicationChecks, SameApplicationCheck)
    static bool belongToSameApplication(const Window *c1, const Window *c2, SameApplicationChecks checks = SameApplicationChecks());
    virtual bool belongsToDesktop() const;

    bool hasApplicationMenu() const;
    bool applicationMenuActive() const
    {
        return m_applicationMenuActive;
    }
    void setApplicationMenuActive(bool applicationMenuActive);

    QString applicationMenuServiceName() const
    {
        return m_applicationMenuServiceName;
    }
    QString applicationMenuObjectPath() const
    {
        return m_applicationMenuObjectPath;
    }

    void showApplicationMenu(int actionId);

    virtual QString preferredColorScheme() const;
    QString colorScheme() const;
    void setColorScheme(const QString &colorScheme);

    bool unresponsive() const;

    virtual bool groupTransient() const;
    virtual const Group *group() const;
    virtual Group *group();

    PlasmaWindowInterface *windowManagementInterface() const
    {
        return m_windowManagementInterface;
    }

    void setLastUsageSerial(quint32 serial);
    quint32 lastUsageSerial() const;

    void refOffscreenRendering();
    void unrefOffscreenRendering();
    bool isOffscreenRendering() const;

    qreal targetScale() const;
    qreal nextTargetScale() const;
    void setNextTargetScale(qreal scale);

    OutputTransform preferredBufferTransform() const;
    void setPreferredBufferTransform(OutputTransform transform);

    const std::shared_ptr<ColorDescription> &preferredColorDescription() const;
    void setPreferredColorDescription(const std::shared_ptr<ColorDescription> &description);

    QString tag() const;
    QString description() const;

    void setActivationToken(const QString &token);
    QString activationToken() const;

public Q_SLOTS:
    virtual void closeWindow() = 0;

protected Q_SLOTS:
    void setReadyForPainting();

Q_SIGNALS:
    void stackingOrderChanged();
    void opacityChanged(KWin::Window *window, qreal oldOpacity);
    void damaged(KWin::Window *window);
    void inputTransformationChanged();
    void closed();
    void outputChanged();
    void skipCloseAnimationChanged();
    void windowRoleChanged();
    void windowClassChanged();
    void surfaceChanged();
    void shadowChanged();
    void bufferGeometryChanged(const QRectF &oldGeometry);
    void frameGeometryChanged(const QRectF &oldGeometry);
    void clientGeometryChanged(const QRectF &oldGeometry);
    void frameGeometryAboutToChange();
    void visibleGeometryChanged();
    void tileChanged(KWin::Tile *tile);
    void requestedTileChanged();
    void fullScreenChanged();
    void skipTaskbarChanged();
    void skipPagerChanged();
    void skipSwitcherChanged();
    void iconChanged();
    void activeChanged();
    void keepAboveChanged(bool);
    void keepBelowChanged(bool);
    void demandsAttentionChanged();
    void desktopsChanged();
    void activitiesChanged();
    void minimizedChanged();
    void paletteChanged(const QPalette &p);
    void colorSchemeChanged();
    void captionChanged();
    void captionNormalChanged();
    void maximizedAboutToChange(MaximizeMode mode);
    void maximizedChanged();
    void transientChanged();
    void modalChanged();
    void quickTileModeChanged();
    void moveResizedChanged();
    void moveResizeCursorChanged(CursorShape);
    void interactiveMoveResizeStarted();
    void interactiveMoveResizeStepped(const QRectF &geometry);
    void interactiveMoveResizeFinished();
    void closeableChanged(bool);
    void minimizeableChanged(bool);
    void maximizeableChanged(bool);
    void desktopFileNameChanged();
    void applicationMenuChanged();
    void hasApplicationMenuChanged(bool);
    void applicationMenuActiveChanged(bool);
    void unresponsiveChanged(bool);
    void decorationChanged();
    void hiddenChanged();
    void hiddenByShowDesktopChanged();
    void lockScreenOverlayChanged();
    void readyForPaintingChanged();
    void maximizeGeometryRestoreChanged();
    void fullscreenGeometryRestoreChanged();
    void offscreenRenderingChanged();
    void targetScaleChanged();
    void nextTargetScaleChanged();
    void noBorderChanged();
    void tagChanged();
    void descriptionChanged();
    void borderRadiusChanged();

protected:
    Window();

    virtual std::unique_ptr<WindowItem> createItem(Item *parentItem) = 0;

    void setResourceClass(const QString &name, const QString &className = QString());
    void setIcon(const QIcon &icon);
    void startAutoRaise();
    void autoRaise();
    bool isMostRecentlyRaised() const;
    void markAsDeleted();
    void markAsPlaced();
    virtual bool acceptsFocus() const = 0;
    virtual void doSetActive();
    virtual void doSetKeepAbove();
    virtual void doSetKeepBelow();
    virtual void doSetDesktop();
    virtual void doSetOnActivities(const QStringList &activityList);
    virtual void doMinimize();
    virtual bool belongsToSameApplication(const Window *other, SameApplicationChecks checks) const = 0;

    virtual void doSetSkipTaskbar();
    virtual void doSetSkipPager();
    virtual void doSetSkipSwitcher();
    virtual void doSetDemandsAttention();
    virtual void doSetQuickTileMode();
    virtual void doSetHidden();
    virtual void doSetHiddenByShowDesktop();
    virtual void doSetSuspended();
    virtual void doSetModal();
    virtual void doSetNextTargetScale();
    virtual void doSetPreferredBufferTransform();
    virtual void doSetPreferredColorDescription();

    void setupWindowManagementInterface();
    void destroyWindowManagementInterface();
    void updateColorScheme();
    void ensurePalette();
    void handlePaletteChange();

    virtual Layer belongsToLayer() const;
    bool isActiveFullScreen() const;

    void setElectricBorderMode(std::optional<ElectricBorderMode> mode);
    std::optional<ElectricBorderMode> electricBorderMode() const
    {
        return m_electricMode;
    }
    void setElectricBorderMaximizing(bool maximizing);
    bool isElectricBorderMaximizing() const
    {
        return m_electricMaximizing;
    }
    void updateElectricGeometryRestore();
    QRectF quickTileGeometryRestore() const;
    QRectF quickTileGeometry(QuickTileMode mode, const QPointF &pos) const;
    void exitQuickTileMode();

    void checkOffscreenPosition(QRectF *geom, const QRectF &screenArea);
    qreal borderLeft() const;
    qreal borderRight() const;
    qreal borderTop() const;
    qreal borderBottom() const;

    enum class MoveResizeMode : uint {
        None,
        Move = 0x1,
        Resize = 0x2,
        MoveResize = Move | Resize,
    };
    virtual void moveResizeInternal(const QRectF &rect, MoveResizeMode mode) = 0;

    bool isInteractiveMoveResize() const
    {
        return m_interactiveMoveResize.enabled;
    }
    void setInteractiveMoveResize(bool enabled)
    {
        m_interactiveMoveResize.enabled = enabled;
    }
    void setInteractiveMoveResizeAnchor(const QPointF &anchor)
    {
        m_interactiveMoveResize.anchor = anchor;
    }
    void setInteractiveMoveResizeModifiers(Qt::KeyboardModifiers modifiers)
    {
        m_interactiveMoveResize.modifiers = modifiers;
    }
    bool isUnrestrictedInteractiveMoveResize() const
    {
        return m_interactiveMoveResize.unrestricted;
    }
    void setUnrestrictedInteractiveMoveResize(bool set)
    {
        m_interactiveMoveResize.unrestricted = set;
    }
    QPointF interactiveMoveOffset() const
    {
        return m_interactiveMoveResize.offset;
    }
    void setInteractiveMoveOffset(const QPointF &offset)
    {
        m_interactiveMoveResize.offset = offset;
    }
    QRectF initialInteractiveMoveResizeGeometry() const
    {
        return m_interactiveMoveResize.initialGeometry;
    }
    void setMoveResizeGeometry(const QRectF &geo);
    void setInteractiveMoveResizeGravity(Gravity gravity)
    {
        m_interactiveMoveResize.gravity = gravity;
    }
    bool isInteractiveMoveResizePointerButtonDown() const
    {
        return m_interactiveMoveResize.buttonDown;
    }
    void setInteractiveMoveResizePointerButtonDown(bool down)
    {
        m_interactiveMoveResize.buttonDown = down;
    }
    Output *interactiveMoveResizeStartOutput() const
    {
        return m_interactiveMoveResize.startOutput;
    }
    void checkUnrestrictedInteractiveMoveResize();
    void updateCursor();
    void startDelayedInteractiveMoveResize();
    void stopDelayedInteractiveMoveResize();
    bool startInteractiveMoveResize();
    virtual bool doStartInteractiveMoveResize();
    virtual void doFinishInteractiveMoveResize();
    virtual void leaveInteractiveMoveResize();
    void checkQuickTilingMaximizationZones(int xroot, int yroot);
    void resetQuickTilingMaximizationZones();
    virtual bool isWaitingForInteractiveResizeSync() const;
    virtual void doInteractiveResizeSync(const QRectF &rect);
    qreal titlebarThickness() const;
    QRectF nextInteractiveMoveGeometry(const QPointF &global) const;
    QRectF nextInteractiveResizeGeometry(const QPointF &global) const;
    void dontInteractiveMoveResize();

    virtual QSizeF resizeIncrements() const;

    Gravity mouseGravity() const;

    void setDecoration(std::shared_ptr<KDecoration3::Decoration> decoration);
    void startDecorationDoubleClickTimer();
    void invalidateDecorationDoubleClickTimer();
    void updateDecorationInputShape();
    void updateDecorationBorderRadius();

    void setDesktopFileName(const QString &name);
    QString iconFromDesktopFile() const;

    void updateApplicationMenuServiceName(const QString &serviceName);
    void updateApplicationMenuObjectPath(const QString &objectPath);

    void setUnresponsive(bool unresponsive);

    virtual void setShortcutInternal();
    QString shortcutCaptionSuffix() const;
    virtual void updateCaption() = 0;

    QRectF keyboardGeometryRestore() const;
    void setKeyboardGeometryRestore(const QRectF &geom);

    QRectF moveToArea(const QRectF &geometry, const QRectF &oldArea, const QRectF &newArea);
    QRectF ensureSpecialStateGeometry(const QRectF &geometry);

    void cleanTabBox();
    void maybeSendFrameCallback();

    void updateNextTargetScale();
    void updatePreferredBufferTransform();
    void updatePreferredColorDescription();
    void setTargetScale(qreal scale);

    void setDescription(const QString &description);

    Output *m_output = nullptr;
    QRectF m_frameGeometry;
    QRectF m_clientGeometry;
    QRectF m_bufferGeometry;
    bool ready_for_painting;
    bool m_hidden = false;
    bool m_hiddenByShowDesktop = false;

    BorderRadius m_borderRadius;

    qreal m_nextTargetScale = 1;
    qreal m_targetScale = 1;
    OutputTransform m_preferredBufferTransform = OutputTransform::Normal;
    std::shared_ptr<ColorDescription> m_preferredColorDescription = ColorDescription::sRGB;

    int m_refCount = 1;
    QUuid m_internalId;
    std::unique_ptr<WindowItem> m_windowItem;
    std::unique_ptr<Shadow> m_shadow;
    QString resource_name;
    QString resource_class;
    ClientMachine *m_clientMachine;
    bool m_skipCloseAnimation;
    QPointer<SurfaceInterface> m_surface;
    qreal m_opacity = 1.0;
    int m_stackingOrder = 0;

    bool m_skipTaskbar = false;
    bool m_originalSkipTaskbar = false;
    bool m_skipPager = false;
    bool m_skipSwitcher = false;
    QIcon m_icon;
    bool m_active = false;
    bool m_deleted = false;
    bool m_placed = false;
    bool m_keepAbove = false;
    bool m_keepBelow = false;
    bool m_demandsAttention = false;
    bool m_minimized = false;
    bool m_suspended = false;
    QTimer *m_autoRaiseTimer = nullptr;
    QList<VirtualDesktop *> m_desktops;

    QStringList m_activityList;
    int m_activityUpdatesBlocked = 0;
    bool m_blockedActivityUpdatesRequireTransients = false;

    QString m_colorScheme;
    std::shared_ptr<Decoration::DecorationPalette> m_palette;
    static QHash<QString, std::weak_ptr<Decoration::DecorationPalette>> s_palettes;
    static std::shared_ptr<Decoration::DecorationPalette> s_defaultPalette;

    PlasmaWindowInterface *m_windowManagementInterface = nullptr;

    Window *m_transientFor = nullptr;
    QList<Window *> m_transients;
    bool m_modal = false;
    Layer m_layer = UnknownLayer;
    QPointer<Tile> m_requestedTile;
    QPointer<Tile> m_tile;

    std::optional<ElectricBorderMode> m_electricMode = std::nullopt;
    QRectF m_electricGeometryRestore;
    bool m_electricMaximizing = false;
    QTimer *m_electricMaximizingDelay = nullptr;

    Output *m_moveResizeOutput = nullptr;
    QRectF m_moveResizeGeometry;
    QRectF m_keyboardGeometryRestore;
    QRectF m_maximizeGeometryRestore;
    QRectF m_fullscreenGeometryRestore;
    QRectF m_virtualKeyboardGeometry;

    struct
    {
        bool enabled = false;
        bool unrestricted = false;
        QPointF anchor;
        Qt::KeyboardModifiers modifiers;
        QPointF offset;
        QRectF initialGeometry;
        QRectF initialGeometryRestore;
        Gravity gravity = Gravity::None;
        bool buttonDown = false;
        CursorShape cursor = Qt::ArrowCursor;
        Output *startOutput = nullptr;
        QTimer *delayedTimer = nullptr;
        uint32_t counter = 0;
        MaximizeMode initialMaximizeMode;
        QuickTileMode initialQuickTileMode;
    } m_interactiveMoveResize;

    struct
    {
        std::shared_ptr<KDecoration3::Decoration> decoration;
        QPointer<Decoration::DecoratedWindowImpl> client;
        QElapsedTimer doubleClickTimer;
        QRegion inputRegion;
    } m_decoration;
    QString m_desktopFileName;

    bool m_applicationMenuActive = false;
    QString m_applicationMenuServiceName;
    QString m_applicationMenuObjectPath;

    bool m_unresponsive = false;

    QKeySequence _shortcut;

    WindowRules m_rules;
    quint32 m_lastUsageSerial = 0;
    bool m_lockScreenOverlay = false;
    uint32_t m_offscreenRenderCount = 0;
    QTimer m_offscreenFramecallbackTimer;

    QString m_tag;
    QString m_description;

    QString m_activationToken;
};

inline QRectF Window::bufferGeometry() const
{
    return m_bufferGeometry;
}

inline QRectF Window::clientGeometry() const
{
    return m_clientGeometry;
}

inline QSizeF Window::clientSize() const
{
    return m_clientGeometry.size();
}

inline QRectF Window::frameGeometry() const
{
    return m_frameGeometry;
}

inline QSizeF Window::size() const
{
    return m_frameGeometry.size();
}

inline QPointF Window::pos() const
{
    return m_frameGeometry.topLeft();
}

inline qreal Window::x() const
{
    return m_frameGeometry.x();
}

inline qreal Window::y() const
{
    return m_frameGeometry.y();
}

inline qreal Window::width() const
{
    return m_frameGeometry.width();
}

inline qreal Window::height() const
{
    return m_frameGeometry.height();
}

inline QRectF Window::rect() const
{
    return QRectF(0, 0, width(), height());
}

inline bool Window::readyForPainting() const
{
    return ready_for_painting;
}

inline bool Window::isDesktop() const
{
    return windowType() == WindowType::Desktop;
}

inline bool Window::isDock() const
{
    return windowType() == WindowType::Dock;
}

inline bool Window::isMenu() const
{
    return windowType() == WindowType::Menu;
}

inline bool Window::isToolbar() const
{
    return windowType() == WindowType::Toolbar;
}

inline bool Window::isSplash() const
{
    return windowType() == WindowType::Splash;
}

inline bool Window::isUtility() const
{
    return windowType() == WindowType::Utility;
}

inline bool Window::isDialog() const
{
    return windowType() == WindowType::Dialog;
}

inline bool Window::isNormalWindow() const
{
    return windowType() == WindowType::Normal;
}

inline bool Window::isDropdownMenu() const
{
    return windowType() == WindowType::DropdownMenu;
}

inline bool Window::isPopupMenu() const
{
    return windowType() == WindowType::PopupMenu;
}

inline bool Window::isTooltip() const
{
    return windowType() == WindowType::Tooltip;
}

inline bool Window::isNotification() const
{
    return windowType() == WindowType::Notification;
}

inline bool Window::isCriticalNotification() const
{
    return windowType() == WindowType::CriticalNotification;
}

inline bool Window::isAppletPopup() const
{
    return windowType() == WindowType::AppletPopup;
}

inline bool Window::isOnScreenDisplay() const
{
    return windowType() == WindowType::OnScreenDisplay;
}

inline bool Window::isComboBox() const
{
    return windowType() == WindowType::ComboBox;
}

inline bool Window::isDNDIcon() const
{
    return windowType() == WindowType::DNDIcon;
}

inline bool Window::isLockScreen() const
{
    return false;
}

inline bool Window::isInputMethod() const
{
    return false;
}

inline bool Window::isOutline() const
{
    return false;
}

inline bool Window::isInternal() const
{
    return false;
}

inline bool Window::isPictureInPicture() const
{
    return false;
}

inline WindowItem *Window::windowItem() const
{
    return m_windowItem.get();
}

inline bool Window::isOnAllDesktops() const
{
    return desktops().isEmpty();
}

inline bool Window::isOnAllActivities() const
{
    return activities().isEmpty();
}

inline bool Window::isOnActivity(const QString &activity) const
{
    return activities().isEmpty() || activities().contains(activity);
}

inline QString Window::resourceName() const
{
    return resource_name;
}

inline QString Window::resourceClass() const
{
    return resource_class;
}

inline ClientMachine *Window::clientMachine() const
{
    return m_clientMachine;
}

template<class T, class U>
inline T *Window::findInList(const QList<T *> &list, std::function<bool(const U *)> func)
{
    static_assert(std::is_base_of<U, T>::value,
                  "U must be derived from T");
    const auto it = std::find_if(list.begin(), list.end(), func);
    if (it == list.end()) {
        return nullptr;
    }
    return *it;
}

inline bool Window::isPopupWindow() const
{
    switch (windowType()) {
    case WindowType::ComboBox:
    case WindowType::DropdownMenu:
    case WindowType::PopupMenu:
    case WindowType::Tooltip:
        return true;

    default:
        return false;
    }
}

inline const QList<Window *> &Window::transients() const
{
    return m_transients;
}

KWIN_EXPORT QDebug operator<<(QDebug debug, const Window *window);

} // namespace KWin

Q_DECLARE_METATYPE(KWin::Window *)
Q_DECLARE_METATYPE(QList<KWin::Window *>)
Q_DECLARE_OPERATORS_FOR_FLAGS(KWin::Window::SameApplicationChecks)
