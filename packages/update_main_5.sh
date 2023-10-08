#!/bin/sh
# Maintainer: Marcus Seyfarth <marcus85@gmx.de>
# You need to adjust the path in the first line to the folder where you placed all of these packages and make this script executable with "chmod +x". I haven't found a way to say "Yes" automatically to pacman, hence you need to install all of the following packages manually in the correct order when installing them first, but you can use this script for updates. Execute it with "./update.sh". Beware of the dangers, the build order is important and this is now tested to be the correct order. As I personally use Kwinft-main and compile that seperatly, you need that package and its dependancies (wrapland-main, disman-main, kdisplay-main) in addition to this list to be installed. You can integrate these packages above plasma-workspace-main in this script if you like.
cd /home/marcus/Downloads/KDE
for pkg in extra-cmake-modules-main \
kdnssd5-main \
ki18n5-main \
solid5-main \
sonnet5-main \
kcodecs5-main \
syndication5-main \
syntax-highlighting5-main \
karchive5-main \
kdoctools5-main \
milou-main \
prison5-main \
kconfig5-main \
kcoreaddons5-main \
kdecoration5-main \
kguiaddons5-main \
kquickcharts5-main \
attica5-main \
kdegraphics-mobipocket-main \
kfilemetadata5-main \
kwindowsystem5-main \
kcrash5-main \
kidletime5-main \
kdbusaddons5-main \
kglobalaccel5-main \
polkit-qt5-main \
kauth5-main \
kwidgetsaddons5-main \
kconfigwidgets5-main \
kitemviews5-main \
kiconthemes5-main \
kxmlgui5-main \
kbookmarks5-main \
kjobwidgets5-main \
knotifications5-main \
kservice5-main \
kwallet5-main \
kcompletion5-main \
ktextwidgets5-main \
kded5-main \
kio5-main \
baloo5-main \
breeze-icons-main \
kwayland-integration-main \
plasma-wayland-protocols-main \
kwayland5-main \
kpackage5-main \
knewstuff5-main \
frameworkintegration5-main \
kdeclarative5-main \
kcmutils5-main \
breeze-main \
kactivities5-main \
kactivities-stats5-main \
kactivitymanagerd-main \
kdesignerplugin-main \
kunitconversion5-main \
kitemmodels5-main \
kemoticons-main \
kparts5-main \
kdelibs4support-main \
kpty5-main \
kdesu5-main \
kholidays5-main \
kinit-main \
kirigami2-main \
purpose5-main \
kmenuedit-main \
knotifyconfig5-main \
kpeople5-main \
plasma-framework-main \
threadweaver5-main \
krunner5-main \
layer-shell-qt-main \
libkscreen-main \
kscreenlocker-main \
ktexteditor5-main \
kuserfeedback-main \
libksysguard-main \
polkit-kde-agent-main \
qqc2-desktop-style5-main \
libqaccessibilityclient-main \
networkmanager-qt-main \
phonon-main \
plasma-integration-main \
kpipewire-main \
kscreen-main \
kwin-main \
plasma-workspace-main \
kde-cli-tools-main \
systemsettings-main \
kross-main \
kdeplasma-addons-main \
plasma-desktop-main \
qca-main \
modemmanager-qt-main \
plasma-nm-main \
plasma-pa-main \
dolphin-main \
kate-main \
konsole-main \
gwenview-main \
bluez-qt5-main \
kde-gtk-config-main \
khotkeys-main \
kinfocenter-main \
ksystemstats-main \
kwallet-pam-main \
powerdevil-main \
sddm-kcm-main \
bluedevil-main \
kio-fuse-main \
ksysguard-main \
plasma-systemmonitor-main \
xdg-desktop-portal-kde-main \
; do
  cd $pkg
  rm -f *.zst
  makepkg -si --cleanbuild --skippgpcheck --skipchecksums --noconfirm
  cd ..
done