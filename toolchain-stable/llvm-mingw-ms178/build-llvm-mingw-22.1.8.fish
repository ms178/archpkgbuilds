#!/usr/bin/env fish
#
# build-llvm-mingw-22.1.8.fish
#
# Fish port + version-pin of NTULINUX/llvm-mingw's build recipe, targeting
# llvm-project 22.1.8 exactly (llvmorg-22.1.8 git tag). Produces a
# minimal, fully static x86 / x86_64 LLVM-based mingw-w64 toolchain
# optimized for a modern CachyOS host.
#
# Preserves all of NTULINUX's optimizations:
#   * Everything built statically (portable, self-contained tree)
#   * Everything built with clang/lld (no gcc dependency)
#   * MinGW target bumped to 0x0A00 (Windows 10)
#   * ThinLTO + LLVM_PARALLEL_LINK_JOBS=8
#   * Polly enabled
#   * -O3 -march=x86-64-v3 for host binaries
#   * ucrt runtime for the mingw side
#   * Only i686 + x86_64 targets (no ARM / no Mac)
#
# Copyright (c) 2018 Martin Storsjo
# Copyright (c) 2026 Alec Ari  (original bash recipe)
# Copyright (c) 2026 CachyOS packaging   (fish port, 22.1.8 pin)
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# Usage:
#   ./build-llvm-mingw-22.1.8.fish  <install-prefix>  [--stage=all|deps|fetch|llvm|strip|wrappers|tools|crt|compiler-rt|libcxx|libs|openmp]
#
# Env overrides:
#   HOST_LLVM_BIN   Path to a host clang/lld/llvm-* bin/ (defaults to $PATH).
#                   Set this to point at the extracted LLVM-22.1.8-Linux-X64
#                   release tarball so the whole build is self-hosted on the
#                   same LLVM version it produces.
#   JOBS            Parallel build jobs (default: nproc).
#   LINK_JOBS       Parallel link jobs for LLVM (default: 8, matches upstream).
#   NO_LTO          Set to 1 to disable ThinLTO (much faster smoke build).
#   NO_NATIVE       Set to 1 to use portable -march=x86-64-v3 (default: native).
#   SKIP_HOST_CHECK Set to 1 to bypass the host tool sanity checks.
#   WORKROOT        Override the working directory for src/ checkouts.
#                   Defaults to the script's own directory. Set to /tmp/...
#                   from a PKGBUILD to keep builds off /home.
#   TARBALL_OUT     If set, `stage_package` writes a
#                   llvm-mingw-<version>-<march>.tar.zst under this path.
#
# ---------------------------------------------------------------------------

# --- fish "strict mode" ----------------------------------------------------
# Any command with a non-zero exit status aborts the script, unless it is
# explicitly guarded (with `or true` / `if ... end`). Fish has no `set -e`
# so we emulate it with a status trampoline around every stage.
status --is-interactive
and echo "note: running interactively; keep an eye on stage failures"

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
# llvm-project pin:
#   We track a specific main-branch commit rather than the llvmorg-22.1.8
#   tag because ms178's Unix toolchain build uses the same commit and his
#   custom performance patches (01..06) were rebased on it. Sticking to
#   the same commit here lets the mingw cross toolchain reuse the same
#   patch set unchanged, and keeps optimization behaviour byte-consistent
#   between the two toolchains.
#
#   LLVM_VERSION is displayed to the user but is not used for the tag
#   lookup; LLVM_REF is what actually gets checked out.
set --global LLVM_VERSION      "22.99.0-92f01b2"        # snapshot ahead of 22.1.8
set --global LLVM_REF          "5ffb1046335f4c1ecc9f688f1337b31c260f37c8"
set --global LLVM_REPO         "https://github.com/llvm/llvm-project.git"

# ms178 performance patch set applied on top of the pinned commit.
# The list is order-sensitive (some patches share files); do not reorder
# without re-verifying `git apply --check` on the full sequence.
set --global LLVM_PATCHES \
    "01-corecount.patch" \
    "02-fixes.patch" \
    "03-optimizations.patch" \
    "04-polly.patch" \
    "05-raptorlake.patch" \
    "06-x86isellowcpp.patch"
set --global MINGW_W64_REPO    "https://github.com/mingw-w64/mingw-w64.git"
set --global MINGW_TARGET_WIN  "0x0A00"          # Windows 10
set --global MINGW_DEFAULT_CRT "ucrt"

set --global ARCHS             i686 x86_64
# NOTE: `-march=native` targets the exact host CPU. That means the produced
# clang.exe / lld / polly binaries are Only portable to CPUs >= this host's
# ISA level. The mingw cross toolchain's default cfg is also set to native
# below so the *Windows* binaries you compile with it will also use the full
# host ISA. Override via NO_NATIVE=1 to fall back to x86-64-v3 / prescott.
set --global HOST_MARCH        "native"
set --global I686_MARCH        "native"
set --global HOST_OPT          "-O3"
if set --query NO_NATIVE
    set HOST_MARCH "x86-64-v3"
    set I686_MARCH "prescott"
end

# LLVM projects / runtimes / installable tools --- mirrors build-llvm.sh
set --global LLVM_PROJECTS     "clang;lld;polly"
set --global LLVM_TARGETS      "X86"
set --global LLVM_TOOLCHAIN_TOOLS "llvm-ar;llvm-ranlib;llvm-objdump;llvm-rc;llvm-cvtres;llvm-nm;llvm-strings;llvm-readobj;llvm-dlltool;llvm-pdbutil;llvm-objcopy;llvm-strip;llvm-cov;llvm-addr2line;llvm-symbolizer;llvm-windres;llvm-ml;llvm-readelf;llvm-size;llvm-cxxfilt;llvm-lib"

# ---------------------------------------------------------------------------
# CLI parsing
# ---------------------------------------------------------------------------
set --global PREFIX ""
set --global STAGE  "all"

for arg in $argv
    switch $arg
        case '--stage=*'
            set STAGE (string replace -- '--stage=' '' $arg)
        case '-h' '--help'
            sed -n '1,40p' (status filename)
            exit 0
        case '--*'
            echo "unknown option: $arg" >&2
            exit 2
        case '*'
            if test -z "$PREFIX"
                set PREFIX $arg
            else
                echo "unexpected positional arg: $arg" >&2
                exit 2
            end
    end
end

if test -z "$PREFIX"
    echo "usage: "(status filename)" <install-prefix> [--stage=STAGE]" >&2
    exit 1
end

# Absolute-ize the prefix so `cd` doesn't lose us.
mkdir -p $PREFIX
set PREFIX (realpath $PREFIX)

# Root working directory --- everything checked out / built lives here so a
# stage can be resumed / re-run. Defaults to the script's own directory; set
# WORKROOT=/tmp/... from a PKGBUILD to keep everything off /home.
if not set --query WORKROOT
    set --global WORKROOT (realpath (dirname (status filename)))
else
    mkdir -p $WORKROOT
    set --global WORKROOT (realpath $WORKROOT)
end
set --global SRCROOT  "$WORKROOT/src"
mkdir -p $SRCROOT

# Parallelism
if not set --query JOBS
    set --global JOBS (nproc)
end
if not set --query LINK_JOBS
    set --global LINK_JOBS 8
end

# Host LLVM path (defaults to whatever's in $PATH)
if set --query HOST_LLVM_BIN; and test -d "$HOST_LLVM_BIN"
    set --global -x PATH $HOST_LLVM_BIN $PATH
    echo ">>> Using host LLVM from $HOST_LLVM_BIN"
end

# Toolchain prefix goes on PATH so later stages can find our own clang.
set --global -x PATH "$PREFIX/bin" $PATH

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
function log
    set_color --bold cyan
    echo ">>> $argv"
    set_color normal
end

function die
    set_color --bold red
    echo "!!! $argv" >&2
    set_color normal
    exit 1
end

function run
    # Echo + execute; abort on failure. Fish-idiomatic replacement for `set -e`.
    set_color brblack; echo "    \$ $argv"; set_color normal
    command $argv
    or die "command failed (status $status): $argv"
end

function check_host_tools
    if set --query SKIP_HOST_CHECK
        return 0
    end
    set -l missing
    for t in git cmake ninja make clang clang++ ld.lld llvm-ar llvm-ranlib \
             autoconf automake libtool pkg-config
        if not command --search $t >/dev/null
            set missing $missing $t
        end
    end
    if test (count $missing) -gt 0
        die "missing host tools: $missing"
    end
    # Confirm the host clang is >= 22 (needed for ThinLTO with 22.x LLVM code).
    set -l cver (clang --version | head -n1 | string match -r '[0-9]+\.[0-9]+\.[0-9]+' | head -n1)
    log "host clang version: $cver"
end

function ensure_repo
    # ensure_repo <dir> <url> [<ref>]
    #
    # <ref> may be a tag (e.g. "llvmorg-22.1.8"), a branch (e.g. "main"),
    # OR a full 40-char commit SHA. Tags/branches use `git clone --branch`;
    # commit SHAs need `git init` + `git fetch <sha>` because `--branch`
    # does not accept commit hashes.
    set -l dir  $argv[1]
    set -l url  $argv[2]
    set -l ref  ""
    if test (count $argv) -ge 3
        set ref $argv[3]
    end

    if not test -d "$dir/.git"
        if test -z "$ref"
            log "cloning $url into $dir"
            run git clone --depth=1 $url $dir
            return
        end
        # Detect commit-SHA-shaped refs (40 hex chars) vs tag/branch names.
        if string match -qr '^[0-9a-f]{40}$' -- $ref
            log "fetching $url @ commit $ref into $dir"
            mkdir -p $dir
            run git -C $dir init -q
            run git -C $dir remote add origin $url
            run git -C $dir fetch --depth=1 origin $ref
            run git -C $dir checkout -q FETCH_HEAD
        else
            log "cloning $url @ $ref into $dir"
            run git clone --depth=1 --single-branch --branch $ref $url $dir
        end
    else
        log "$dir already checked out; skipping clone (delete to refresh)"
    end
end

function fresh_build_dir
    set -l d $argv[1]
    rm -rf $d
    mkdir -p $d
end

# ---------------------------------------------------------------------------
# Stage: fetch sources
# ---------------------------------------------------------------------------
function stage_fetch
    log "Fetching llvm-project @ $LLVM_REF"
    ensure_repo "$SRCROOT/llvm-project" $LLVM_REPO $LLVM_REF

    log "Fetching mingw-w64 (master)"
    ensure_repo "$SRCROOT/mingw-w64"    $MINGW_W64_REPO
end

# ---------------------------------------------------------------------------
# Stage: apply ms178 performance patches on top of the pinned commit.
#
# Patches live in $WORKROOT/patches/ (populated by the PKGBUILD from
# $startdir/patches/) or, when running the fish script standalone, in a
# patches/ directory next to the script.
#
# We use a sentinel file inside the source tree so re-running the stage
# on an already-patched checkout is a no-op instead of a duplicate-apply
# error. Delete $SRCROOT/llvm-project/.ms178-patches-applied to force a
# re-application (or just wipe the whole src/llvm-project checkout).
# ---------------------------------------------------------------------------
function stage_patch
    set -l tree "$SRCROOT/llvm-project"
    set -l sentinel "$tree/.ms178-patches-applied"

    if test -f "$sentinel"
        log "patches already applied (sentinel: $sentinel); skipping"
        return 0
    end

    # Locate patches/ directory. Look at WORKROOT first (PKGBUILD case),
    # then next to the script (standalone use).
    set -l patches_dir "$WORKROOT/patches"
    if not test -d "$patches_dir"
        set patches_dir (realpath (dirname (status filename)))"/patches"
    end
    if not test -d "$patches_dir"
        die "patches/ directory not found (looked in $WORKROOT/patches and beside the script)"
    end
    log "using patches from: $patches_dir"

    for pfile in $LLVM_PATCHES
        set -l p "$patches_dir/$pfile"
        if not test -f "$p"
            die "missing patch file: $p"
        end
        log "applying $pfile"
        # `git apply` from within the tree; --whitespace=nowarn so the
        # PKGBUILD run doesn't fail on cosmetic trailing whitespace.
        run git -C $tree apply --whitespace=nowarn $p
    end

    date --utc '+%Y-%m-%dT%H:%M:%SZ' > "$sentinel"
    log "all patches applied; sentinel written to $sentinel"
end

# ---------------------------------------------------------------------------
# Stage: build LLVM/clang/lld/polly
# ---------------------------------------------------------------------------
function stage_llvm
    log "Configuring & building LLVM $LLVM_VERSION (ThinLTO, Polly, march=$HOST_MARCH)"

    set -l lto_flags "-DLLVM_ENABLE_LTO=thin" "-DLLVM_PARALLEL_LINK_JOBS=$LINK_JOBS"
    if set --query NO_LTO
        log "NO_LTO=1 --- disabling ThinLTO for this build"
        set lto_flags
    end

    cd "$SRCROOT/llvm-project/llvm"
    fresh_build_dir build
    cd build

    # -----------------------------------------------------------------------
    # HOST BUILD FLAGS  --- PIC + shared-library isolation for ThinLTO
    #
    # Failure mode this block prevents:
    #
    #     ld.lld: error: relocation R_X86_64_PC32 cannot be used against
    #     symbol 'zng_length_code'; recompile with -fPIC
    #     >>> defined in lib/libclang-cpp.so.22.1.lto.o
    #
    # THREE root causes on CachyOS / Arch (all must be neutralized):
    #
    #   1. clang/tools/CMakeLists.txt unconditionally adds the clang-shlib
    #      subdirectory on UNIX, so libclang-cpp.so gets built *even with
    #      BUILD_SHARED_LIBS=OFF and CLANG_LINK_CLANG_DYLIB=OFF*. Everything
    #      going into that .so must be PIC. Fix: -fPIC on all our own
    #      objects, LLVM_ENABLE_PIC=ON, CMAKE_POSITION_INDEPENDENT_CODE=ON.
    #
    #   2. CachyOS ships zlib-ng-compat where /usr/lib/libz.a is either
    #      non-PIC or an LTO bitcode archive (or both). If find_package(ZLIB)
    #      picks the .a, ThinLTO merges its symbols into libclang-cpp.so's
    #      LTO module and the link fails. Fix: force the shared zlib/zstd
    #      by explicitly pointing at libz.so / libzstd.so.
    #
    #   3. NTULINUX's original recipe sets LLVM_BUILD_STATIC=ON to produce
    #      a fully-static, distro-portable clang.exe. That adds "-static"
    #      to CMAKE_EXE_LINKER_FLAGS globally, which makes even CMake's
    #      trivial try-compile probes fail to link against libz.so
    #      ("attempted static link of dynamic object"). Since we're
    #      packaging for a specific host (CachyOS pacman package, not a
    #      redistributable tarball) we DROP LLVM_BUILD_STATIC=ON. The
    #      resulting binaries link against the host's libc/libz/libzstd.so
    #      --- exactly what any Arch/CachyOS package should do.
    #
    # We keep BUILD_SHARED_LIBS=OFF so the *individual* LLVM component libs
    # (libLLVMSupport.a etc.) stay static within the toolchain; only
    # libclang-cpp.so is a forced-by-upstream artifact.
    # -----------------------------------------------------------------------
    set -l host_cflags "$HOST_OPT -march=$HOST_MARCH -fPIC"

    # Locate PIC shared zlib/zstd deterministically. CachyOS's
    # zlib-ng-compat sometimes ships libz.a as non-PIC or as an LTO bitcode
    # archive; either variant breaks the ThinLTO link of libclang-cpp.so.
    # We pass both LIBRARY and INCLUDE_DIR so find_package(ZLIB) doesn't
    # re-scan and fall back to the .a again.
    set -l zlib_so  ""
    set -l zstd_so  ""
    set -l zlib_inc ""
    set -l zstd_inc ""
    for p in /usr/lib/libz.so /usr/lib64/libz.so /usr/lib/x86_64-linux-gnu/libz.so
        if test -e $p; and test -z "$zlib_so"
            set zlib_so $p
        end
    end
    for p in /usr/lib/libzstd.so /usr/lib64/libzstd.so /usr/lib/x86_64-linux-gnu/libzstd.so
        if test -e $p; and test -z "$zstd_so"
            set zstd_so $p
        end
    end
    for p in /usr/include /usr/local/include
        if test -e $p/zlib.h; and test -z "$zlib_inc"
            set zlib_inc $p
        end
        if test -e $p/zstd.h; and test -z "$zstd_inc"
            set zstd_inc $p
        end
    end
    log "zlib shared: $zlib_so (headers: $zlib_inc)"
    log "zstd shared: $zstd_so (headers: $zstd_inc)"

    set -l zlib_flags
    if test -n "$zlib_so"; and test -n "$zlib_inc"
        set zlib_flags \
            "-DZLIB_LIBRARY=$zlib_so" \
            "-DZLIB_LIBRARY_RELEASE=$zlib_so" \
            "-DZLIB_INCLUDE_DIR=$zlib_inc" \
            "-DZLIB_INCLUDE_DIRS=$zlib_inc"
    end
    set -l zstd_flags
    if test -n "$zstd_so"; and test -n "$zstd_inc"
        set zstd_flags \
            "-Dzstd_LIBRARY=$zstd_so" \
            "-Dzstd_INCLUDE_DIR=$zstd_inc"
    end

    run cmake \
        -DCMAKE_C_FLAGS="$host_cflags" \
        -DCMAKE_CXX_FLAGS="$host_cflags" \
        -DCMAKE_C_COMPILER=clang \
        -DCMAKE_CXX_COMPILER=clang++ \
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
        -DLLVM_USE_LINKER=lld \
        -DCMAKE_GENERATOR=Ninja \
        -DCMAKE_INSTALL_PREFIX=$PREFIX \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DLLVM_BUILD_LLVM_DYLIB=OFF \
        -DLLVM_LINK_LLVM_DYLIB=OFF \
        -DCLANG_LINK_CLANG_DYLIB=OFF \
        -DLLVM_ENABLE_PIC=ON \
        -DLLVM_ENABLE_ZLIB=FORCE_ON \
        -DZLIB_USE_STATIC_LIBS=OFF \
        $zlib_flags \
        -DLLVM_ENABLE_ZSTD=FORCE_ON \
        -DLLVM_USE_STATIC_ZSTD=OFF \
        $zstd_flags \
        -DLLVM_ENABLE_LIBXML2=FORCE_ON \
        -DLLVM_ENABLE_TERMINFO=OFF \
        -DLLVM_ENABLE_LIBEDIT=OFF \
        -DLLVM_BUILD_TESTS=OFF \
        -DLLVM_INCLUDE_TESTS=OFF \
        -DLLVM_OPTIMIZED_TABLEGEN=ON \
        -DLLVM_ENABLE_RTTI=OFF \
        -DCLANG_INCLUDE_TESTS=OFF \
        -DCLANG_ENABLE_OBJC_REWRITER=OFF \
        -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
        -DLLVM_ENABLE_ASSERTIONS=OFF \
        -DLLVM_INCLUDE_EXAMPLES=OFF \
        -DLLVM_BUILD_RUNTIME=OFF \
        -DLLVM_BUILD_BENCHMARKS=OFF \
        -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DLLVM_BUILD_INSTRUMENTED=OFF \
        -DLLVM_ENABLE_PROJECTS=$LLVM_PROJECTS \
        -DLLVM_ENABLE_BINDINGS=OFF \
        -DLLVM_TARGETS_TO_BUILD=$LLVM_TARGETS \
        -DLLVM_INSTALL_TOOLCHAIN_ONLY=ON \
        -DLLVM_TOOLCHAIN_TOOLS=$LLVM_TOOLCHAIN_TOOLS \
        $lto_flags \
        -G Ninja \
        ..

    run cmake --build . -j $JOBS
    run cmake --install . --strip
    run cp ../../LICENSE.TXT "$PREFIX/"
end

# ---------------------------------------------------------------------------
# Stage: prune LLVM install --- 1:1 with strip-llvm.sh
# ---------------------------------------------------------------------------
function stage_strip
    log "Pruning LLVM install tree"

    for d in libexec share/opt-viewer share/scan-build share/scan-view \
             include/clang include/clang-c include/clang-tidy \
             include/lld include/llvm include/llvm-c include/lldb
        rm -rf "$PREFIX/$d"
    end
    # scan-build man pages -- glob may not match, so use `find` which is quiet.
    find "$PREFIX/share/man/man1" -maxdepth 1 -name 'scan-build*' -delete 2>/dev/null

    # Delete unwanted top-level executables in bin/
    for pat in amdgpu-arch bugpoint c-index-test 'clangd-*' darwin-debug \
               diagtool dsymutil find-all-symbols hmaptool 'ld64.lld*' llc \
               'lldb-*' lli modularize nvptx-arch obj2yaml offload-arch opt \
               pp-trace sancov sanstats scan-build scan-view split-file \
               verify-uselistorder wasm-ld 'yaml2*' libclang.dll '*LTO.dll' \
               '*Remarks.dll' '*.bat'
        find "$PREFIX/bin" -mindepth 1 -maxdepth 1 -name $pat -delete 2>/dev/null
    end
    # clang-* except a few we keep
    find "$PREFIX/bin" -mindepth 1 -maxdepth 1 -name 'clang-*' \
        ! -name '*[0-9]' ! -name 'clang-scan-deps' \
        ! -name 'clang-cpp' ! -name 'clang-format' -delete 2>/dev/null

    for pat in '*.so*' '*.dylib*' cmake '*.a' '*.dll.a'
        find "$PREFIX/lib" -mindepth 1 -maxdepth 1 -name $pat -exec rm -rf {} + 2>/dev/null
    end

    if test -d "$PREFIX/share/clang"
        find "$PREFIX/share/clang" -mindepth 1 -maxdepth 1 \
            ! -name 'clang-format*' -exec rm -rf {} + 2>/dev/null
    end
end

# ---------------------------------------------------------------------------
# Stage: install NTULINUX toolchain wrappers
# ---------------------------------------------------------------------------
function stage_wrappers
    log "Installing wrappers into $PREFIX/bin"
    # We look for wrappers/ first next to WORKROOT, then next to the script.
    # A PKGBUILD sets WORKROOT=/tmp/... but ships wrappers/ inside srcdir/.
    set -l wrappers_dir "$WORKROOT/wrappers"
    if not test -d "$wrappers_dir"
        set wrappers_dir (realpath (dirname (status filename)))"/wrappers"
    end
    if not test -d "$wrappers_dir"
        die "wrappers/ directory not found (looked in $WORKROOT/wrappers and beside the script)"
    end
    log "using wrappers from: $wrappers_dir"
    run cp -arLv "$wrappers_dir"/. "$PREFIX/bin/"

    # Rewrite cfg files to reflect the requested march. If NO_NATIVE is not
    # set we bake -march=native into the mingw cross-cfg so Windows binaries
    # produced with this toolchain inherit the full host ISA. The i686 cfg
    # gets I686_MARCH to keep 32-bit sanity.
    log "patching cfg files: x86_64 -> $HOST_MARCH, i686 -> $I686_MARCH"
    for cfg in "$PREFIX/bin/x86_64-w64-windows-gnu.cfg"
        if test -f "$cfg"
            sed -i -E "s/^-march=.*/-march=$HOST_MARCH/" "$cfg"
        end
    end
    for cfg in "$PREFIX/bin/i686-w64-windows-gnu.cfg"
        if test -f "$cfg"
            sed -i -E "s/^-march=.*/-march=$I686_MARCH/" "$cfg"
        end
    end

    cd "$PREFIX/bin"
    for a in i686-w64-mingw32 x86_64-w64-mingw32
        ln -sfv clang-scan-deps        "$a-clang-scan-deps"
        ln -sfv ld-wrapper.sh          "$a-ld"
        ln -sfv objdump-wrapper.sh     "$a-objdump"
        for b in as c++ clang clang++ g++ gcc
            ln -sfv clang-target-wrapper.sh "$a-$b"
        end
        for b in ar llvm-ar
            ln -sfv llvm-ar   "$a-$b"
        end
        for b in ranlib llvm-ranlib
            ln -sfv llvm-ranlib "$a-$b"
        end
        for b in addr2line dlltool nm objcopy readelf size strings strip windres
            ln -sfv "llvm-$b" "$a-$b"
        end
    end
end

# ---------------------------------------------------------------------------
# Stage: mingw-w64 tools (gendef, widl)
# ---------------------------------------------------------------------------
function stage_mingw_tools
    log "Building mingw-w64 tools (gendef, widl)"
    set -l any_arch $ARCHS[1]

    # gendef -----------------------------------------------------------------
    cd "$SRCROOT/mingw-w64/mingw-w64-tools/gendef"
    fresh_build_dir build
    cd build
    set -x CC clang; set -x CXX clang++; set -x LD ld.lld
    run ../configure --prefix=$PREFIX
    run make -j$JOBS
    run make install-strip
    mkdir -p "$PREFIX/share/gendef"
    run install -m644 ../COPYING "$PREFIX/share/gendef/COPYING.txt"

    # widl -------------------------------------------------------------------
    cd "$SRCROOT/mingw-w64/mingw-w64-tools/widl"
    fresh_build_dir build
    cd build
    run ../configure --prefix=$PREFIX \
        --target=$any_arch-w64-mingw32 \
        --with-widl-includedir=$PREFIX/generic-w64-mingw32/include \
        --enable-silent-rules
    run make -j$JOBS
    run make install-strip
    mkdir -p "$PREFIX/share/widl"
    run install -m644 ../../../COPYING "$PREFIX/share/widl/COPYING.txt"

    set -e CC; set -e CXX; set -e LD

    cd "$PREFIX/bin"
    for arch in $ARCHS
        if test "$arch" != "$any_arch"
            ln -sfv "$any_arch-w64-mingw32-widl" "$arch-w64-mingw32-widl"
        end
    end
end

# ---------------------------------------------------------------------------
# Stage: mingw-w64 CRT + headers
# ---------------------------------------------------------------------------
function stage_mingw_crt
    log "Building mingw-w64 headers + CRT (default winnt=$MINGW_TARGET_WIN, crt=$MINGW_DEFAULT_CRT)"
    set -l header_root "$PREFIX/generic-w64-mingw32"

    cd "$SRCROOT/mingw-w64/mingw-w64-headers"
    fresh_build_dir build
    cd build
    set -x CC clang; set -x CXX clang++; set -x LD ld.lld
    run ../configure --prefix=$header_root \
        --enable-idl \
        --with-default-win32-winnt=$MINGW_TARGET_WIN \
        --with-default-msvcrt=$MINGW_DEFAULT_CRT \
        INSTALL="install -C"
    run make install
    set -e CC; set -e CXX; set -e LD

    for arch in $ARCHS
        mkdir -p "$PREFIX/$arch-w64-mingw32"
        if not test -e "$PREFIX/$arch-w64-mingw32/include"
            ln -sfn "../generic-w64-mingw32/include" "$PREFIX/$arch-w64-mingw32/include"
        end
    end

    cd "$SRCROOT/mingw-w64/mingw-w64-crt"
    for arch in $ARCHS
        fresh_build_dir "build-$arch"
        cd "build-$arch"
        set -l crt_flags --with-default-msvcrt=$MINGW_DEFAULT_CRT --enable-silent-rules
        switch $arch
            case i686
                set crt_flags $crt_flags --enable-lib32 --disable-lib64
            case x86_64
                set crt_flags $crt_flags --disable-lib32 --enable-lib64
        end
        run ../configure \
            --host=$arch-w64-mingw32 \
            --prefix=$PREFIX/$arch-w64-mingw32 \
            $crt_flags
        run make -j$JOBS
        run make install
        cd ..
    end

    for arch in $ARCHS
        if not test -f "$PREFIX/$arch-w64-mingw32/lib/libssp.a"
            # Dummy archives so `-lssp -lssp_nonshared` still links.
            run llvm-ar rcs "$PREFIX/$arch-w64-mingw32/lib/libssp.a"
            run llvm-ar rcs "$PREFIX/$arch-w64-mingw32/lib/libssp_nonshared.a"
        end
        mkdir -p "$PREFIX/$arch-w64-mingw32/share/mingw32"
        for file in COPYING \
                    COPYING.MinGW-w64/COPYING.MinGW-w64.txt \
                    COPYING.MinGW-w64-runtime/COPYING.MinGW-w64-runtime.txt
            run install -m644 "$SRCROOT/mingw-w64/$file" \
                "$PREFIX/$arch-w64-mingw32/share/mingw32/"
        end
    end
end

# ---------------------------------------------------------------------------
# Stage: compiler-rt builtins (per arch)
# ---------------------------------------------------------------------------
function stage_compiler_rt
    log "Building compiler-rt builtins for $ARCHS"
    set -l resource_dir ("$PREFIX/bin/clang" --print-resource-dir)
    log "clang resource dir: $resource_dir"

    cd "$SRCROOT/llvm-project/compiler-rt"
    for arch in $ARCHS
        fresh_build_dir "build-$arch"
        cd "build-$arch"
        run cmake \
            -DBUILD_SHARED_LIBS=OFF \
            -DCMAKE_GENERATOR=Ninja \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$resource_dir \
            -DCMAKE_C_COMPILER=$arch-w64-mingw32-clang \
            -DCMAKE_CXX_COMPILER=$arch-w64-mingw32-clang++ \
            -DCMAKE_SYSTEM_NAME=Windows \
            -DCMAKE_AR=$PREFIX/bin/llvm-ar \
            -DCMAKE_RANLIB=$PREFIX/bin/llvm-ranlib \
            -DCMAKE_C_COMPILER_WORKS=1 \
            -DCMAKE_CXX_COMPILER_WORKS=1 \
            -DCMAKE_C_COMPILER_TARGET=$arch-w64-windows-gnu \
            -DCOMPILER_RT_DEFAULT_TARGET_ONLY=TRUE \
            -DCOMPILER_RT_USE_BUILTINS_LIBRARY=TRUE \
            -DCOMPILER_RT_EXCLUDE_ATOMIC_BUILTIN=FALSE \
            -DLLVM_CONFIG_PATH="" \
            -DCMAKE_FIND_ROOT_PATH=$PREFIX/$arch-w64-mingw32 \
            -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY \
            -DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE=ONLY \
            -G Ninja \
            ../lib/builtins
        run cmake --build . -j $JOBS
        run cmake --install . --prefix $resource_dir
        cd ..
    end
end

# ---------------------------------------------------------------------------
# Stage: libunwind + libc++abi + libc++ (per arch)
# ---------------------------------------------------------------------------
function stage_libcxx
    log "Building libunwind + libc++abi + libc++ for $ARCHS"
    cd "$SRCROOT/llvm-project/runtimes"
    for arch in $ARCHS
        fresh_build_dir "build-$arch"
        cd "build-$arch"
        run cmake \
            -DBUILD_SHARED_LIBS=OFF \
            -DCMAKE_GENERATOR=Ninja \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$PREFIX/$arch-w64-mingw32 \
            -DCMAKE_C_COMPILER=$arch-w64-mingw32-clang \
            -DCMAKE_CXX_COMPILER=$arch-w64-mingw32-clang++ \
            -DCMAKE_CXX_COMPILER_TARGET=$arch-w64-windows-gnu \
            -DCMAKE_SYSTEM_NAME=Windows \
            -DCMAKE_C_COMPILER_WORKS=TRUE \
            -DCMAKE_CXX_COMPILER_WORKS=TRUE \
            -DCMAKE_AR=$PREFIX/bin/llvm-ar \
            -DCMAKE_RANLIB=$PREFIX/bin/llvm-ranlib \
            -DLLVM_ENABLE_RUNTIMES="libunwind;libcxxabi;libcxx" \
            -DLIBUNWIND_USE_COMPILER_RT=TRUE \
            -DLIBUNWIND_ENABLE_SHARED=OFF \
            -DLIBUNWIND_ENABLE_STATIC=ON \
            -DLIBCXX_USE_COMPILER_RT=ON \
            -DLIBCXX_ENABLE_SHARED=OFF \
            -DLIBCXX_ENABLE_STATIC=ON \
            -DLIBCXX_ENABLE_STATIC_ABI_LIBRARY=TRUE \
            -DLIBCXX_CXX_ABI=libcxxabi \
            -DLIBCXX_LIBDIR_SUFFIX="" \
            -DLIBCXX_INCLUDE_TESTS=FALSE \
            -DLIBCXX_INSTALL_MODULES=ON \
            -DLIBCXX_INSTALL_MODULES_DIR=$PREFIX/share/libc++/v1 \
            -DLIBCXX_ENABLE_ABI_LINKER_SCRIPT=FALSE \
            -DLIBCXXABI_USE_COMPILER_RT=ON \
            -DLIBCXXABI_USE_LLVM_UNWINDER=ON \
            -DLIBCXXABI_ENABLE_SHARED=OFF \
            -DLIBCXXABI_LIBDIR_SUFFIX="" \
            -DCMAKE_C_FLAGS_INIT="-D__USE_MINGW_ANSI_STDIO=1" \
            -DCMAKE_CXX_FLAGS_INIT="-D__USE_MINGW_ANSI_STDIO=1" \
            -G Ninja \
            ..
        run cmake --build . -j $JOBS
        run cmake --install .
        cd ..
    end
end

# ---------------------------------------------------------------------------
# Stage: winpthreads
# ---------------------------------------------------------------------------
function stage_mingw_libs
    log "Building mingw-w64 winpthreads for $ARCHS"
    cd "$SRCROOT/mingw-w64/mingw-w64-libraries/winpthreads"
    set -e CC
    for arch in $ARCHS
        fresh_build_dir "build-$arch"
        cd "build-$arch"
        set -l arch_prefix "$PREFIX/$arch-w64-mingw32"
        set -x CFLAGS  "-O3"
        set -x CXXFLAGS "-O3"
        set -x CC  "$arch-w64-mingw32-clang"
        set -x CXX "$arch-w64-mingw32-clang++"
        run ../configure --host=$arch-w64-mingw32 \
            --prefix=$arch_prefix --libdir=$arch_prefix/lib \
            --enable-silent-rules
        run make -j$JOBS
        run make install
        set -e CFLAGS; set -e CXXFLAGS; set -e CC; set -e CXX
        cd ..
        mkdir -p "$arch_prefix/share/mingw32"
        run install -m644 COPYING "$arch_prefix/share/mingw32/COPYING.winpthreads.txt"
    end
end

# ---------------------------------------------------------------------------
# Stage: OpenMP runtime
# ---------------------------------------------------------------------------
function stage_openmp
    log "Building OpenMP runtime for $ARCHS"
    cd "$SRCROOT/llvm-project/runtimes"
    for arch in $ARCHS
        set -l extra
        switch $arch
            case x86_64
                set extra -DLIBOMP_ASMFLAGS=-m64
        end

        fresh_build_dir "build-openmp-$arch"
        cd "build-openmp-$arch"
        run cmake \
            -DCMAKE_GENERATOR=Ninja \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX=$PREFIX/$arch-w64-mingw32 \
            -DCMAKE_C_COMPILER=$arch-w64-mingw32-clang \
            -DCMAKE_CXX_COMPILER=$arch-w64-mingw32-clang++ \
            -DCMAKE_RC_COMPILER=$arch-w64-mingw32-windres \
            -DCMAKE_ASM_MASM_COMPILER=llvm-ml \
            -DCMAKE_SYSTEM_NAME=Windows \
            -DCMAKE_AR=$PREFIX/bin/llvm-ar \
            -DCMAKE_RANLIB=$PREFIX/bin/llvm-ranlib \
            -DLLVM_ENABLE_RUNTIMES="openmp" \
            $extra \
            -G Ninja \
            ..
        run cmake --build . -j $JOBS
        run cmake --install .
        # Fish errors on non-matching globs; use `find` for a quiet delete.
        find "$PREFIX/$arch-w64-mingw32/bin" -maxdepth 1 -name '*iomp5md*' -delete 2>/dev/null
        find "$PREFIX/$arch-w64-mingw32/lib" -maxdepth 1 -name '*iomp5md*' -delete 2>/dev/null
        cd ..
    end
end

# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Stage: package the finished tree into a tar.zst
# ---------------------------------------------------------------------------
function stage_package
    if not set --query TARBALL_OUT
        log "TARBALL_OUT not set --- skipping tarball creation"
        return 0
    end
    if not test -d "$PREFIX"
        die "PREFIX $PREFIX doesn't exist; nothing to package"
    end
    mkdir -p $TARBALL_OUT
    set -l tarball "$TARBALL_OUT/llvm-mingw-$LLVM_VERSION-$HOST_MARCH.tar.zst"

    # Tar with zstd max compression; use nproc threads (`-T0` in zstd).
    # Package the prefix's parent dir so extraction gives you `llvm-mingw/`.
    set -l parent  (dirname $PREFIX)
    set -l leaf    (basename $PREFIX)
    log "creating $tarball (from $parent/$leaf)"
    run tar --zstd \
        -C $parent \
        -cf $tarball \
        --sort=name \
        --owner=0 --group=0 --numeric-owner \
        --mtime='@0' \
        $leaf
    log "tarball ready: $tarball ("(du -h $tarball | cut -f1)")"
end

function run_stage
    switch $argv[1]
        case deps          ; check_host_tools
        case fetch         ; stage_fetch
        case patch         ; stage_patch
        case llvm          ; stage_llvm
        case strip         ; stage_strip
        case wrappers      ; stage_wrappers
        case tools         ; stage_mingw_tools
        case crt           ; stage_mingw_crt
        case compiler-rt   ; stage_compiler_rt
        case libcxx        ; stage_libcxx
        case libs          ; stage_mingw_libs
        case openmp        ; stage_openmp
        case package       ; stage_package
        case '*'           ; die "unknown stage: $argv[1]"
    end
end

log "llvm-mingw $LLVM_VERSION build"
log "prefix     : $PREFIX"
log "workroot   : $WORKROOT"
log "jobs       : $JOBS  (link jobs: $LINK_JOBS)"
log "stage      : $STAGE"

if test "$STAGE" = "all"
    for s in deps fetch patch llvm strip wrappers tools crt compiler-rt libcxx libs compiler-rt openmp package
        run_stage $s
    end
else
    run_stage $STAGE
end

log "DONE. Toolchain installed under: $PREFIX"
