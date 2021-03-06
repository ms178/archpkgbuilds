#!/bin/sh
# You need to adjust the path in the first line to the folder where you placed all of these packages and make this script executable with "chmod +x". I haven't found a way to say "Yes" automatically to pacman, hence you need to install all of the following packages manually in the correct order at first and use this script for updates. Execute it with "./install.sh". Beware of the dangers, the build order is important and this is now tested to be the correct order. As my plasma-workspace-git depends on Kwinft-git, you need that package and its dependancies (wrapland-git, disman-git, kdisplay-git) in addition to be installed, too. You can integrate these packages above plasma-workspace-git in this script if you like.
cd /home/marcus/Downloads/KDE
for pkg in extra-cmake-modules-git \
ki18n-git \
solid-git \
sonnet-git \
kcodecs-git \
syndication-git \
syntax-highlighting-git \
milou-git \
prison-git \
karchive-git \
kconfig-git \
kcoreaddons-git \
kdecoration-git \
kguiaddons-git \
kquickcharts-git \
attica-git \
kfilemetadata-git \
kwindowsystem-git \
kcrash-git \
kidletime-git \
kdbusaddons-git \
kdoctools-git \
kglobalaccel-git \
polkit-qt5-git \
kauth-git \
kwidgetsaddons-git \
kconfigwidgets-git \
kitemviews-git \
kiconthemes-git \
kxmlgui-git \
kbookmarks-git \
kjobwidgets-git \
knotifications-git \
kservice-git \
kwallet-git \
kcompletion-git \
ktextwidgets-git \
kded-git \
kio-git \
baloo-git \
breeze-icons-git \
kwayland-server-git \
plasma-wayland-protocols-git \
kwayland-git \
kpackage-git \
knewstuff-git \
frameworkintegration-git \
kdeclarative-git \
kcmutils-git \
breeze-git \
kactivities-git \
kactivities-stats-git \
kactivitymanagerd-git \
kdesignerplugin-git \
kunitconversion-git \
kitemmodels-git \
kemoticons-git \
kparts-git \
kdelibs4support-git \
kpty-git \
kdesu-git \
kholidays-git \
kinit-git \
kirigami2-git \
kmenuedit-git \
knotifyconfig-git \
kpeople-git \
plasma-framework-git \
threadweaver-git \
krunner-git \
layer-shell-qt-git \
kscreenlocker-git \
ktexteditor-git \
kuserfeedback-git \
libkscreen-git \
libksysguard-git \
polkit-kde-agent-git \
qqc2-desktop-style-git \
libqaccessibilityclient-git \
networkmanager-qt-git \
phonon-git \
plasma-integration-git \
kpipewire-git \
plasma-workspace-git \
kde-cli-tools-git \
systemsettings-git \
plasma-desktop-git \
qca-qt5-git \
plasma-disks-git \
modemmanager-qt-git \
plasma-nm-git \
plasma-pa-git \
dolphin-git \
kate-git \
konsole-git \
gwenview \
bluez-qt-git \
kde-gtk-config-git \
khotkeys-git \
kinfocenter-git \
ksystemstats-git \
kwallet-pam-git \
powerdevil-git \
sddm-kcm-git \
; do
  cd $pkg
  rm -f *.zst
  makepkg -si --cleanbuild --skippgpcheck --skipchecksums --noconfirm
  cd ..
done
