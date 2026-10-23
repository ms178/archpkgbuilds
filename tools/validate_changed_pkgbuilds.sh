#!/usr/bin/env bash
# Validate changed package directories in an Arch/makepkg environment.
#
# This script never runs against the checkout directly: each package is copied
# to a temporary directory first, which prevents source extraction, generated
# PGO workloads, or prepare() from changing tracked files.
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: tools/validate_changed_pkgbuilds.sh [--base <git-revision>] [--path <package>]... [--mode <metadata|source|prepare>]

  --base <revision>  Diff base used to discover changed packages (default: origin/main, then HEAD~1).
  --path <path>      Validate one package directory or PKGBUILD; repeatable. Overrides discovery.
  --mode <mode>      metadata: makepkg --printsrcinfo
                     source:   metadata plus makepkg --verifysource (default)
                     prepare:  source plus prepare() in an isolated copy; build/check/package are replaced by no-ops
EOF
}

base=""
mode="source"
declare -a requested=()
while (($#)); do
  case "$1" in
    --base)
      base=${2:?--base requires a revision}
      shift 2
      ;;
    --path)
      requested+=("${2:?--path requires a path}")
      shift 2
      ;;
    --mode)
      mode=${2:?--mode requires a value}
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$mode" in
  metadata|source|prepare) ;;
  *) echo "error: invalid mode: $mode" >&2; exit 2 ;;
esac

command -v makepkg >/dev/null || {
  echo "error: makepkg is required; run this in an Arch clean-chroot/container" >&2
  exit 2
}

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$root"

if ((${#requested[@]})); then
  package_dirs=()
  for requested_path in "${requested[@]}"; do
    path=$requested_path
    [[ -d $path ]] || path=$(dirname -- "$path")
    if [[ ! -f "$path/PKGBUILD" ]]; then
      echo "error: not a package directory: $requested_path" >&2
      exit 2
    fi
    package_dirs+=("${path#./}")
  done
else
  if [[ -z $base ]]; then
    if git rev-parse --verify --quiet origin/main >/dev/null; then
      base=origin/main
    elif git rev-parse --verify --quiet HEAD~1 >/dev/null; then
      base=HEAD~1
    else
      echo "error: cannot infer a diff base; pass --base or --path" >&2
      exit 2
    fi
  fi

  mapfile -t changed_files < <(git diff --name-only "$base"...HEAD -- packages)
  declare -A selected=()
  for changed in "${changed_files[@]}"; do
    [[ $changed == packages/*/* ]] || continue
    remainder=${changed#packages/}
    package_dir="packages/${remainder%%/*}"
    [[ -f "$package_dir/PKGBUILD" ]] && selected["$package_dir"]=1
  done
  if ((${#selected[@]})); then
    mapfile -t package_dirs < <(printf '%s\n' "${!selected[@]}" | sort)
  else
    package_dirs=()
  fi
fi

if ((${#package_dirs[@]} == 0)); then
  echo "No changed package directories to validate."
  exit 0
fi

# Ensure PGO/BOLT content is unchanged before any makepkg evaluation.  The
# copied package below makes a second guard check unnecessary for this command.
python3 tools/pkgbuild_guard.py check

work_root=$(mktemp -d "${TMPDIR:-/tmp}/archpkgbuild-validation.XXXXXX")
trap 'rm -rf "$work_root"' EXIT

for package_dir in "${package_dirs[@]}"; do
  echo "==> Validating $package_dir ($mode)"
  work_package="$work_root/${package_dir##*/}"
  mkdir -p "$work_package"
  cp -a "$package_dir/." "$work_package/"
  (
    cd "$work_package"
    makepkg --printsrcinfo >/dev/null

    if [[ $mode == source || $mode == prepare ]]; then
      makepkg --verifysource --holdver
    fi

    if [[ $mode == prepare ]]; then
      cat >> PKGBUILD <<'EOF'

# CI-only prepare-phase harness.  This is appended only to the temporary copy
# made by tools/validate_changed_pkgbuilds.sh.
build() { :; }
check() { :; }
package() { :; }
for _maintenance_pkg in "${pkgname[@]}"; do
  _maintenance_package_function="package_${_maintenance_pkg//-/_}"
  eval "${_maintenance_package_function}() { :; }"
done
unset _maintenance_pkg _maintenance_package_function
EOF
      # --noarchive still invokes prepare(), but the injected functions avoid
      # package-specific PGO/BOLT compilation, tests, and installation work.
      makepkg --nodeps --nocheck --noarchive --holdver
    fi
  )
done

echo "Changed package validation completed successfully."
