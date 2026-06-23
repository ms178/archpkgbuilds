#!/bin/sh
set -e
basedir=/usr/share/dxvk
: "${WINEPREFIX:=$HOME/.wine}"
install_file() {
  src="$1"; dst="$2"; mkdir -p "$(dirname "$dst")"; ln -sf "$src" "$dst"
}
case "$1" in
  install|--install|"")
    install_file "$basedir/x64/d3d9.dll" "$WINEPREFIX/drive_c/windows/system32/d3d9.dll"
    install_file "$basedir/x64/d3d10core.dll" "$WINEPREFIX/drive_c/windows/system32/d3d10core.dll"
    install_file "$basedir/x64/d3d11.dll" "$WINEPREFIX/drive_c/windows/system32/d3d11.dll"
    install_file "$basedir/x64/dxgi.dll" "$WINEPREFIX/drive_c/windows/system32/dxgi.dll"
    install_file "$basedir/x32/d3d9.dll" "$WINEPREFIX/drive_c/windows/syswow64/d3d9.dll"
    install_file "$basedir/x32/d3d10core.dll" "$WINEPREFIX/drive_c/windows/syswow64/d3d10core.dll"
    install_file "$basedir/x32/d3d11.dll" "$WINEPREFIX/drive_c/windows/syswow64/d3d11.dll"
    install_file "$basedir/x32/dxgi.dll" "$WINEPREFIX/drive_c/windows/syswow64/dxgi.dll"
    ;;
  *) echo "Usage: setup_dxvk [install]" >&2; exit 2 ;;
esac
