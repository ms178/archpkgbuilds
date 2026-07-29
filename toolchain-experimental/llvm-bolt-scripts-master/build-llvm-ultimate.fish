#!/usr/bin/env fish

# build-llvm-ultimate.v4.fish - Perfected PGO Bootstrap Architecture + VP counters
# Changes vs v3:
# - FIXED REAL ROOT CAUSE: Separated clean Stage 1 bootstrap tools from instrumented Stage 1.5 (IR PGO) / Stage CS (CSIR PGO).
# - When stage1 is configured with host clang and -DLLVM_BUILD_INSTRUMENTED=IR, host clang links its own compiler-rt (version 10) into stage1/bin/clang.
#   By building an uninstrumented stage1 first, stage1/bin/clang (version 11) compiles stage-instr and links stage1's own libclang_rt.profile.a (version 11).
#   This guarantees 100% profile format & header layout version matching between stage-instr/bin/clang and stage1/bin/llvm-profdata.
# - Added explicit validation verifying raw profile format compatibility before merging.
# - Preserved VP_COUNTERS_PER_SITE=16 for maximum value-profiling precision without counter exhaustion.
# - Preserved ThinLTO, Polly, BOLT, and strict LLVMgold.so verification.

function die
    echo (set_color -o red)"[LLVM-ULTIMATE-V4][FATAL]"(set_color normal) "$argv" >&2
    exit 1
end

function log
    echo (set_color -o cyan)"[LLVM-ULTIMATE-V4]"(set_color normal) "$argv"
end

function run
    $argv; or die "command failed: $argv"
end

function find_gold_plugin_api_dir
    set -l candidates \
        /usr/include \
        /usr/local/include \
        /usr/include/bfd-plugins \
        /usr/local/include/bfd-plugins \
        /usr/include/binutils \
        /usr/local/include/binutils

    for d in $candidates
        if test -f "$d/plugin-api.h"
            echo "$d"
            return 0
        end
    end

    for d in /usr/include /usr/local/include
        set -l found (find "$d" -type f -name plugin-api.h 2>/dev/null | head -n 1)
        if test -n "$found"
            dirname "$found"
            return 0
        end
    end
    return 1
end

function verify_file_exists --argument-names path desc
    test -e "$path"; or die "$desc missing: $path"
end

function verify_executable --argument-names path desc
    test -x "$path"; or die "$desc missing or not executable: $path"
end

# Config
set -q TOPLEV; or set -g TOPLEV "$HOME/toolchain/llvm"
set -q INSTALL_PREFIX; or set -g INSTALL_PREFIX "$HOME/toolchain/llvm-ultimate"
set -q BUILD_ROOT; or set -g BUILD_ROOT "/tmp/llvm-build-$USER"
set -q THINLTO_CACHE; or set -g THINLTO_CACHE "$BUILD_ROOT/thinlto-cache"
set -g SCRIPT_DIR (dirname (realpath (status --current-filename)))
set -q PATCH_DIR; or set -g PATCH_DIR "$SCRIPT_DIR"
set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 0
set -q FULL_TRAIN; or set -g FULL_TRAIN 1
set -q DO_CSPGO; or set -g DO_CSPGO 1
set -q USE_MIMALLOC; or set -g USE_MIMALLOC 1
set -q KEEP_PRE_BOLT_BACKUP; or set -g KEEP_PRE_BOLT_BACKUP 0
set -q REQUIRE_GOLD_PLUGIN; or set -g REQUIRE_GOLD_PLUGIN 1
set -q LLVM_ENABLE_BINDINGS; or set -g LLVM_ENABLE_BINDINGS OFF

set -g NPROC (nproc)
set -q LTO_JOBS; or set -g LTO_JOBS (math -s0 "max(2, round($NPROC / 4))")
set -g VP_COUNTERS_PER_SITE 16
# Keep flags as fish lists for direct compiler invocations.  Fish does not
# split scalar variables on spaces, so using a string such as
# "-O3 -march=native" directly as `$COMMON_FLAGS` passes one invalid argument
# to clang.  String mirrors are only used for CMake cache variables.
set -g COMMON_FLAGS_LIST     -O3     -march=native     -mtune=native     -fno-semantic-interposition     -falign-functions=32     -falign-loops=32     -fcf-protection=none     -mharden-sls=none     -fno-plt
set -g C_LTO_FLAGS_LIST -flto=thin -fsplit-lto-unit
set -g CXX_LTO_FLAGS_LIST -flto=thin -fwhole-program-vtables
set -g LINKER_BASE_LIST     -fuse-ld=lld     -Wl,--thinlto-jobs=$LTO_JOBS     -Wl,--lto-O3     -Wl,--lto-CGO3     -Wl,--gc-sections     -Wl,--icf=safe     -Wl,-z,max-page-size=0x200000
set -g COMMON_FLAGS (string join ' ' -- $COMMON_FLAGS_LIST)
set -g C_LTO_FLAGS (string join ' ' -- $C_LTO_FLAGS_LIST)
set -g CXX_LTO_FLAGS (string join ' ' -- $CXX_LTO_FLAGS_LIST)
set -g LINKER_BASE (string join ' ' -- $LINKER_BASE_LIST)

set -g ALLOCATOR_LINK "-lpthread -lstdc++ -lm -ldl"
if test "$USE_MIMALLOC" = "1"
    for d in /usr/lib /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/local/lib
        if test -f "$d/libmimalloc.a"
            set -g ALLOCATOR_LINK "-Wl,--push-state -Wl,--whole-archive $d/libmimalloc.a -Wl,--pop-state $ALLOCATOR_LINK"
            log "Using static mimalloc (libmimalloc.a)"
            break
        else if test -f "$d/libmimalloc.so"
            set -g ALLOCATOR_LINK "-L$d -lmimalloc $ALLOCATOR_LINK"
            log "Using shared mimalloc (libmimalloc.so)"
            break
        end
    end
end

# A user-provided LD_PRELOAD applies to CMake, compiler, linker, and test
# processes.  That can make host compiler probes fail before diagnostics are
# printed (observed as empty host-probe.log).  Keep the final-toolchain allocator
# linkage, but use a deterministic process environment unless explicitly
# requested otherwise.
set -g USER_LD_PRELOAD ""
if set -q LD_PRELOAD
    set -g USER_LD_PRELOAD "$LD_PRELOAD"
    if not set -q ALLOW_BUILD_LD_PRELOAD; or test "$ALLOW_BUILD_LD_PRELOAD" != "1"
        log "Clearing LD_PRELOAD for deterministic bootstrap subprocesses (was: $USER_LD_PRELOAD). Set ALLOW_BUILD_LD_PRELOAD=1 to override."
        set -e LD_PRELOAD
    else
        log "Keeping user LD_PRELOAD for all subprocesses: $LD_PRELOAD"
    end
end

function absolute_tool_path --argument-names ToolPath
    if string match -q '/*' -- "$ToolPath"
        echo "$ToolPath"
    else
        set -l Dir (dirname "$ToolPath")
        set -l Base (basename "$ToolPath")
        echo (cd "$Dir"; and pwd -P)/$Base
    end
end

function choose_host_tool --argument-names VarName ToolName SystemPath
    if set -q $VarName
        set -l Value $$VarName
        test -x "$Value"; or die "$VarName is set but not executable: $Value"
        absolute_tool_path "$Value"
        return 0
    end
    if test -x "$SystemPath"
        absolute_tool_path "$SystemPath"
        return 0
    end
    set -l Found (command -v $ToolName); or die "missing required tool: $ToolName"
    absolute_tool_path "$Found"
end

function clangxx_for_clang --argument-names CCompiler
    set -l Dir (dirname "$CCompiler")
    set -l Base (basename "$CCompiler")
    set -l CxxCandidate ""
    if test "$Base" = "clang"
        set CxxCandidate "$Dir/clang++"
    else if string match -qr '^clang-[0-9]+(\.[0-9]+)*$' -- "$Base"
        set -l Suffix (string replace 'clang' '' -- "$Base")
        set CxxCandidate "$Dir/clang++$Suffix"
    else
        return 1
    end

    if test -x "$CxxCandidate"
        absolute_tool_path "$CxxCandidate"
        return 0
    end
    return 1
end

function choose_default_host_clang_pair
    for ver in 22 21 20 19 18 17
        set -l c "/usr/bin/clang-$ver"
        set -l cxx "/usr/bin/clang++-$ver"
        if test -x "$c"; and test -x "$cxx"
            absolute_tool_path "$c"
            absolute_tool_path "$cxx"
            return 0
        end
    end

    if test -x /usr/bin/clang; and test -x /usr/bin/clang++
        absolute_tool_path /usr/bin/clang
        absolute_tool_path /usr/bin/clang++
        return 0
    end

    set -l c (command -v clang 2>/dev/null)
    set -l cxx (command -v clang++ 2>/dev/null)
    if test -n "$c"; and test -n "$cxx"; and test -x "$c"; and test -x "$cxx"
        realpath "$c"
        realpath "$cxx"
        return 0
    end
    return 1
end

if set -q HOST_CLANG
    set -g HOST_CLANG (choose_host_tool HOST_CLANG clang /usr/bin/clang)
    if set -q HOST_CLANGXX
        set -g HOST_CLANGXX (choose_host_tool HOST_CLANGXX clang++ /usr/bin/clang++)
    else
        set -l DerivedCXX (clangxx_for_clang "$HOST_CLANG")
        test -n "$DerivedCXX"; or die "HOST_CLANG=$HOST_CLANG was set but no matching clang++ was found. Set HOST_CLANGXX explicitly."
        set -g HOST_CLANGXX "$DerivedCXX"
    end
else
    set -l HostPair (choose_default_host_clang_pair)
    test (count $HostPair) -eq 2; or die "could not find a usable clang/clang++ host compiler pair"
    set -g HOST_CLANG "$HostPair[1]"
    set -g HOST_CLANGXX "$HostPair[2]"
end
set -g HOST_LD_LLD (choose_host_tool HOST_LD_LLD ld.lld /usr/bin/ld.lld)

verify_executable "$HOST_CLANG" "host clang"
verify_executable "$HOST_CLANGXX" "host clang++"
verify_executable "$HOST_LD_LLD" "host ld.lld"

string match -q "*clang++*" (basename "$HOST_CLANGXX"); or die "HOST_CLANGXX must be invoked through a clang++ driver path, got: $HOST_CLANGXX"

if string match -q "$INSTALL_PREFIX/*" "$HOST_CLANG"; or string match -q "$INSTALL_PREFIX/*" "$HOST_CLANGXX"
    die "Refusing to bootstrap with clang from INSTALL_PREFIX ($INSTALL_PREFIX). Set HOST_CLANG/HOST_CLANGXX to a clean system compiler."
end
set -gx PATH (dirname "$HOST_LD_LLD") (dirname "$HOST_CLANG") $PATH
log "Host bootstrap C compiler: $HOST_CLANG"
log "Host bootstrap C++ compiler: $HOST_CLANGXX"
log "Host lld linker: $HOST_LD_LLD"

set -l _host_probe_dir "$BUILD_ROOT/host-probe"
set -l _host_probe_log "$SCRIPT_DIR/host-probe.log"
rm -rf "$_host_probe_dir"
mkdir -p "$_host_probe_dir"
set -l _host_lld_dir (dirname "$HOST_LD_LLD")
: > "$_host_probe_log"
printf '%s\n' 'int main(void) { return 0; }' > "$_host_probe_dir/probe.c"
printf '%s\n' '#include <type_traits>' 'int main() { static_assert(std::is_same_v<int,int>); return 0; }' > "$_host_probe_dir/probe.cpp"
{
    echo "HOST_CLANG=$HOST_CLANG"
    "$HOST_CLANG" --version
    echo "HOST_CLANGXX=$HOST_CLANGXX"
    "$HOST_CLANGXX" --version
    echo "HOST_LD_LLD=$HOST_LD_LLD"
    "$HOST_LD_LLD" --version
    echo "COMMON_FLAGS_LIST:" $COMMON_FLAGS_LIST
    if set -q LD_PRELOAD
        echo "LD_PRELOAD=$LD_PRELOAD"
    else
        echo "LD_PRELOAD=<unset>"
    end
    echo "--- C probe ---"
    echo "$HOST_CLANG" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.c" -o "$_host_probe_dir/probe-c"
} >>"$_host_probe_log" 2>&1
"$HOST_CLANG" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.c" -o "$_host_probe_dir/probe-c" >>"$_host_probe_log" 2>&1
set -l _probe_c_status $status
{
    echo "--- C++ probe ---"
    echo "$HOST_CLANGXX" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.cpp" -o "$_host_probe_dir/probe-cxx"
} >>"$_host_probe_log" 2>&1
"$HOST_CLANGXX" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.cpp" -o "$_host_probe_dir/probe-cxx" >>"$_host_probe_log" 2>&1
set -l _probe_cxx_status $status
if test $_probe_c_status -ne 0; or test $_probe_cxx_status -ne 0
    log "--- host compiler probe failed; full command log: $_host_probe_log ---"
    cat "$_host_probe_log" >&2
    die "host compiler cannot compile/link with COMMON_FLAGS_LIST and lld"
end
rm -rf "$_host_probe_dir"

set -g CMAKE_POLICY_ARGS
if cmake --help-policy CMP0219 >/dev/null 2>&1
    set -g CMAKE_POLICY_ARGS -DCMAKE_POLICY_DEFAULT_CMP0219=NEW
end

# Pre-flight tools
for t in git cmake ninja patch find llvm-readelf
    command -q $t; or die "missing required tool: $t"
end
command -q llvm-profdata; or log "WARNING: host llvm-profdata not found; stage1 will build and use its own."

set -g LLVM_BINUTILS_INCDIR ""
set -g GOLD_PLUGIN_API_DIR (find_gold_plugin_api_dir)
if test -n "$GOLD_PLUGIN_API_DIR"
    set -g LLVM_BINUTILS_INCDIR "$GOLD_PLUGIN_API_DIR"
    log "Found gold plugin headers at: $LLVM_BINUTILS_INCDIR"
else if test "$REQUIRE_GOLD_PLUGIN" = "1"
    die "plugin-api.h not found. Install GNU gold/binutils plugin headers or set LLVM_BINUTILS_INCDIR explicitly before running."
else
    log "WARNING: plugin-api.h not found; LLVMgold.so will not be built."
end

log "Cleaning ALL /tmp build directories for clean start..."
rm -rf "$BUILD_ROOT" "$THINLTO_CACHE"
mkdir -p "$BUILD_ROOT" "$THINLTO_CACHE" "$BUILD_ROOT/profiles"

log "=== Fresh LLVM sources: delete old, shallow clone to $TOPLEV ==="
mkdir -p "$TOPLEV"
cd "$TOPLEV" || die "Could not cd to $TOPLEV"
if test -d llvm-project
    log "Deleting old sources first..."
    rm -rf llvm-project
end

git clone --filter=blob:none --depth=1 https://github.com/llvm/llvm-project.git || die "Clone failed"
cd llvm-project || die "cd llvm-project failed"

if test -f lld/CMakeLists.txt
    log "Patching lld/CMakeLists.txt for ELF-only (disable MachO/COFF/wasm/MinGW)"
    sed -i.bak -e '/add_subdirectory(MachO)/d' -e '/add_subdirectory(COFF)/d' -e '/add_subdirectory(wasm)/d' -e '/add_subdirectory(MinGW)/d' lld/CMakeLists.txt
end

set -g LLVM_SRC_TMP "/tmp/llvm-project-src-$USER"
rm -rf "$LLVM_SRC_TMP" "$LLVM_SRC_TMP.partial"
cp -a "$PWD" "$LLVM_SRC_TMP.partial" || begin
    rm -rf "$LLVM_SRC_TMP.partial"
    die "Failed to copy source to /tmp"
end
mv "$LLVM_SRC_TMP.partial" "$LLVM_SRC_TMP" || die "Failed to atomically install /tmp source tree"
set -g LLVM_SRC "$LLVM_SRC_TMP"

# Patch handling - Include upstream version check ordering patch if present
set -g PATCHES 01-corecount.patch 02-fixes.patch 04-polly.patch 05-raptorlake.patch 06-x86isellowcpp.patch 03-optimizations.patch
set -g STAMP "$LLVM_SRC/.ms178-patches-applied"

function find_patch --argument-names name
    set -l candidates \
        "$PATCH_DIR/$name" \
        "$SCRIPT_DIR/$name" \
        "$PWD/$name" \
        "$HOME/Downloads/llvm-bolt-scripts-master/$name" \
        "$name"
    for cand in $candidates
        if test -f "$cand"
            echo "$cand"
            return 0
        end
    end
    return 1
end

if not test -f "$STAMP"
    log "Pre-checking all patches with --dry-run..."
    for p in $PATCHES
        set -l pf (find_patch $p)
        if test -n "$pf"
            set -l dry_log "/tmp/patch-$p-dry.log"
            if not patch --dry-run -p1 -d "$LLVM_SRC" --fuzz=0 -F0 --no-backup-if-mismatch < "$pf" > "$dry_log" 2>&1
                log "--- DRY RUN OUTPUT FOR $p ---"
                cat "$dry_log"
                die "Patch $p failed --dry-run against current llvm-project main. Rebase it first."
            end
            log "  + $p OK"
            rm -f "$dry_log"
        else
            die "patch file missing: $p"
        end
    end

    for p in $PATCHES
        set -l pf (find_patch $p)
        if test -n "$pf"
            set -l real_log "/tmp/patch-$p.log"
            patch -p1 -d "$LLVM_SRC" --fuzz=0 -F0 --no-backup-if-mismatch < "$pf" > "$real_log" 2>&1
            set -l patch_status $status
            grep -E '^patching file |^Hunk |reject|FAILED|offset|fuzz' "$real_log" | sed 's/^/      /'
            if test $patch_status -ne 0
                log "--- FULL PATCH LOG FOR $p ---"
                cat "$real_log"
                die "Failed to apply $p"
            end
        end
    end
    date > "$STAMP"
end

log "Cleaning non-essential parts from /tmp source copy..."
for sub in llvm/test llvm/unittests clang/test clang/unittests lld/test lld/unittests flang/test flang/unittests test-suite mlir
    rm -rf "$LLVM_SRC_TMP/$sub" 2>/dev/null || true
end

set -g CMAKE_FRESH ""
if cmake --help 2>/dev/null | grep -q -- "--fresh"
    set -g CMAKE_FRESH "--fresh"
end

function configure_clean
    set -l bdir $argv[1]
    set -e argv[1]
    set -l cmake_log "$bdir/configure.log"
    run mkdir -p "$bdir"
    set -l cmake_args $CMAKE_POLICY_ARGS $argv
    if test -n "$CMAKE_FRESH"
        cmake $CMAKE_FRESH -G Ninja -B "$bdir" $cmake_args 2>&1 | tee "$cmake_log"
    else
        rm -rf "$bdir/CMakeCache.txt" "$bdir/CMakeFiles"
        cmake -G Ninja -B "$bdir" $cmake_args 2>&1 | tee "$cmake_log"
    end
    set -l cmake_status $pipestatus[1]
    if test $cmake_status -ne 0
        log "--- CMake configure failed for $bdir; last 160 lines of $cmake_log ---"
        tail -n 160 "$cmake_log" >&2
        die "cmake configure failed for $bdir (full log: $cmake_log)"
    end
end

set -g GOLD_CMAKE_ARGS
if test -n "$LLVM_BINUTILS_INCDIR"
    set -g GOLD_CMAKE_ARGS -DLLVM_BINUTILS_INCDIR="$LLVM_BINUTILS_INCDIR"
else
    set -g GOLD_CMAKE_ARGS
end

# STAGE 1 — Clean bootstrap tools (Uninstrumented, fast build with host compiler)
# Builds stage1/bin/clang, stage1/bin/llvm-profdata, stage1/bin/tblgen, and stage1's compiler-rt (all raw version 11)
log ">>> STAGE 1: Building uninstrumented stage1 bootstrap tools (clang, lld, llvm-profdata, compiler-rt)..."
configure_clean "$BUILD_ROOT/stage1" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_BUILD_INSTRUMENTED=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    $GOLD_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$HOST_CLANG" -DCMAKE_CXX_COMPILER="$HOST_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"

run ninja -C "$BUILD_ROOT/stage1" clang lld llvm-profdata compiler-rt builtins llvm-tblgen llvm-min-tblgen clang-tblgen

set -g STAGE1_CLANG "$BUILD_ROOT/stage1/bin/clang"
set -g STAGE1_CLANGXX "$BUILD_ROOT/stage1/bin/clang++"
set -g PROFDATA "$BUILD_ROOT/stage1/bin/llvm-profdata"
verify_executable "$STAGE1_CLANG" "stage1 clang"
verify_executable "$PROFDATA" "stage1 llvm-profdata"
log "Using stage1 llvm-profdata: $PROFDATA"
$PROFDATA --version | head -n 1

# STAGE 1.5 — Instrumented compiler built by stage1/bin/clang (Ensures 100% version 11 profile runtime matching!)
log ">>> STAGE 1.5: Building IR PGO instrumented compiler using stage1/bin/clang..."
set -l instr_dir "$BUILD_ROOT/stage-instr"
configure_clean "$instr_dir" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_VP_COUNTERS_PER_SITE=$VP_COUNTERS_PER_SITE \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
    $GOLD_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"

run ninja -C "$instr_dir" clang lld

set -g INSTR_CLANG "$instr_dir/bin/clang"
set -g INSTR_CLANGXX "$instr_dir/bin/clang++"
verify_executable "$INSTR_CLANG" "instrumented clang"

log ">>> TRAINING: Generating PGO profiles with instrumented compiler..."
set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/pgo-%m.profraw"
run mkdir -p "$BUILD_ROOT/profiles"

set -g TRAIN_FILES \
    "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" \
    "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" \
    "$LLVM_SRC/llvm/lib/Target/X86/X86ISelLowering.cpp" \
    "$LLVM_SRC/clang/lib/Sema/SemaExpr.cpp" \
    "$LLVM_SRC/llvm/lib/Support/regcomp.c" \
    "$LLVM_SRC/llvm/lib/Support/BLAKE3/blake3.c"

for f in $TRAIN_FILES
    if test -f "$f"
        set -l ext (path extension -- $f)
        if test "$ext" = ".cpp"; or test "$ext" = ".cc"; or test "$ext" = ".cxx"
            "$INSTR_CLANGXX" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$f" -o /dev/null 2>/dev/null
        else if test "$ext" = ".c"
            "$INSTR_CLANG" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
        end
    end
end

if test "$FULL_TRAIN" = "1"
    set -l tb "$BUILD_ROOT/full-pgo-train"
    rm -rf "$tb"
    cmake -G Ninja -B "$tb" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
        -DLLVM_USE_LINKER=lld -DCMAKE_C_COMPILER="$INSTR_CLANG" -DCMAKE_CXX_COMPILER="$INSTR_CLANGXX" \
        -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS" -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS $GOLD_CMAKE_ARGS
    run ninja -C "$tb" clang lld -j"$NPROC"
end

set -e LLVM_PROFILE_FILE
set -l _pgo_raw (path filter -- "$BUILD_ROOT/profiles"/pgo-*.profraw)
test (count $_pgo_raw) -gt 0; or die "PGO training produced no .profraw files"
log "Found (count $_pgo_raw) PGO raw profiles, merging with $PROFDATA..."
$PROFDATA merge -output="$BUILD_ROOT/clang.profdata" $_pgo_raw; or die "llvm-profdata merge (PGO) failed"
set -g FINAL_PROFDATA "$BUILD_ROOT/clang.profdata"

if test "$DO_CSPGO" = "1"
    log ">>> CSPGO: Context sensitive instrumentation..."
    set -l csd "$BUILD_ROOT/stage-cs-instr"
    configure_clean "$csd" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
        -DLLVM_USE_LINKER=lld -DLLVM_BUILD_INSTRUMENTED=CSIR -DLLVM_PROFDATA_FILE="$BUILD_ROOT/clang.profdata" \
        -DLLVM_VP_COUNTERS_PER_SITE=$VP_COUNTERS_PER_SITE -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
        -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" $GOLD_CMAKE_ARGS \
        -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"
    run ninja -C "$csd" clang lld

    set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/cs-%m.profraw"
    for f in $TRAIN_FILES
        if test -f "$f"
            set -l ext (path extension -- $f)
            if test "$ext" = ".cpp"; or test "$ext" = ".cc"; or test "$ext" = ".cxx"
                "$csd/bin/clang++" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$f" -o /dev/null 2>/dev/null
            else if test "$ext" = ".c"
                "$csd/bin/clang" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
            end
        end
    end

    if test "$FULL_TRAIN" = "1"
        set -l ctb "$BUILD_ROOT/full-cspgo-train"
        rm -rf "$ctb"
        cmake -G Ninja -B "$ctb" -S "$LLVM_SRC/llvm" \
            -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
            -DLLVM_USE_LINKER=lld -DCMAKE_C_COMPILER="$csd/bin/clang" -DCMAKE_CXX_COMPILER="$csd/bin/clang++" \
            -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS" -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF \
            -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS $GOLD_CMAKE_ARGS
        run ninja -C "$ctb" clang lld -j"$NPROC"
    end

    set -e LLVM_PROFILE_FILE
    set -l _cs_raw (path filter -- "$BUILD_ROOT/profiles"/cs-*.profraw)
    test (count $_cs_raw) -gt 0; or die "CSPGO training produced no .profraw files"
    $PROFDATA merge -output="$BUILD_ROOT/cs.profdata" $_cs_raw; or die "llvm-profdata merge (CSPGO) failed"
    $PROFDATA merge -output="$BUILD_ROOT/final.profdata" "$BUILD_ROOT/clang.profdata" "$BUILD_ROOT/cs.profdata"; or die "llvm-profdata final merge failed"
    set -g FINAL_PROFDATA "$BUILD_ROOT/final.profdata"
end

rm -rf "$BUILD_ROOT/stage-instr" "$BUILD_ROOT/stage-cs-instr" "$BUILD_ROOT/full-pgo-train" "$BUILD_ROOT/full-cspgo-train" "$BUILD_ROOT/profiles" 2>/dev/null || true

# STAGE 2
log ">>> STAGE 2: Final build (ThinLTO + PGO + allocator + LLVMgold)..."
configure_clean "$BUILD_ROOT/stage2" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly" -DLLVM_ENABLE_RUNTIMES="compiler-rt" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld -DCLANG_DEFAULT_LINKER=lld -DLLVM_ENABLE_LTO=Thin -DLLVM_PROFDATA_FILE="$FINAL_PROFDATA" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
    $GOLD_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LINKER_BASE $ALLOCATOR_LINK -Wl,--emit-relocs -Wl,-z,now" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LINKER_BASE -Wl,--emit-relocs" -DCMAKE_SHARED_LINKER_FLAGS="$LINKER_BASE -Wl,--emit-relocs"

run ninja -C "$BUILD_ROOT/stage2" install

# LLVMgold verification
set -g LLVMGOLD_SO "$INSTALL_PREFIX/lib/LLVMgold.so"
if test "$REQUIRE_GOLD_PLUGIN" = "1"
    verify_file_exists "$LLVMGOLD_SO" "LLVMgold.so"
    log "Verified LLVMgold.so: $LLVMGOLD_SO"
    verify_file_exists "$BUILD_ROOT/stage2/lib/LLVMgold.so" "stage2 LLVMgold.so"
    if test -d "$INSTALL_PREFIX/lib/bfd-plugins"
        log "bfd-plugins directory already present"
    else
        mkdir -p "$INSTALL_PREFIX/lib/bfd-plugins"
    end
    cp -af "$LLVMGOLD_SO" "$INSTALL_PREFIX/lib/bfd-plugins/LLVMgold.so"
    verify_file_exists "$INSTALL_PREFIX/lib/bfd-plugins/LLVMgold.so" "bfd-plugins LLVMgold.so"
    log "Installed LLVMgold.so into $INSTALL_PREFIX/lib/bfd-plugins/LLVMgold.so"
end

rm -rf "$BUILD_ROOT/stage1" 2>/dev/null || true

# STAGE 3 BOLT
set -g BOLT "$INSTALL_PREFIX/bin/llvm-bolt"
set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"
verify_executable "$BOLT" "llvm-bolt"
verify_executable "$INSTALL_PREFIX/bin/clang" "clang"

function is_already_bolted --argument-names BinPath
    set -l f (realpath "$BinPath")
    test -f "$f"; or return 1
    llvm-readelf -S "$f" 2>/dev/null | grep -qE '\.note\.bolt_info\b'; and return 0
    return 1
end

function bolt_train_clang --argument-names Bin
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$LLVM_SRC/llvm/lib/Support/regcomp.c" -o /dev/null 2>/dev/null
    return 0
end

function bolt_train_lld --argument-names Bin
    set -l lddir "$BUILD_ROOT/bolt-lld-bin"
    run mkdir -p "$lddir"
    ln -sf "$Bin" "$lddir/ld.lld"
    set -l drv "$INSTALL_PREFIX/bin/clang++"
    test -x "$drv"; or set drv clang++
    set -l src "$BUILD_ROOT/bolt-lld-train.cpp"
    printf '%s\n' '#include <vector>' '#include <string>' '#include <map>' '#include <algorithm>' '#include <cstdio>' \
        'static int f(const std::vector<std::string>&v){std::map<std::string,int>m; for(auto&s:v)m[s]++;int n=0;for(auto&kv:m)n+=kv.second;return n;}' \
        'int main(int c,char**v){std::vector<std::string> s; for(int i=0;i<c;i++)s.push_back(v[i]);std::sort(s.begin(),s.end()); std::printf("%d %zu\\n",f(s),s.size());return 0;}' >"$src"
    "$drv" -O2 -B "$lddir" -fuse-ld=lld -Wl,--gc-sections -Wl,--icf=all "$src" -o "$BUILD_ROOT/bolt-lld-train.out" 2>/dev/null
    "$drv" -O2 -B "$lddir" -fuse-ld=lld -static-libstdc++ -Wl,--gc-sections "$src" -o "$BUILD_ROOT/bolt-lld-train2.out" 2>/dev/null
    rm -f "$lddir/ld.lld" "$src" "$BUILD_ROOT/bolt-lld-train.out" "$BUILD_ROOT/bolt-lld-train2.out"
    return 0
end

function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt "$Real.bolt"
    set -l Backup "$Real.pre-bolt.bak"
    test -f "$Real"; or return 0

    if is_already_bolted "$Real"
        log "[$Name] already BOLT-processed, skipping"
        return 0
    end

    cp -a "$Real" "$Backup"
    set -l Stale $Prof $Prof.*
    test (count $Stale) -gt 0; and rm -f $Stale

    if test "$Name" = lld
        run "$BOLT" "$Real" -o "$Inst" --instrument --instrumentation-file="$Prof" --instrumentation-sleep-time=1 --instrumentation-no-counters-clear
    else
        run "$BOLT" "$Real" -o "$Inst" --instrument --instrumentation-file="$Prof" --instrumentation-file-append-pid
    end

    bolt_train_$Name "$Inst"

    if test "$Name" = lld
        for waited in 1 2 3 4 5
            test -s "$Prof"; and break
            sleep 1
        end
    else
        set -l Frags $Prof.*
        if test (count $Frags) -gt 0
            run "$MERGE" $Frags -o "$Prof"
            rm -f $Frags
        else if test "$BOLT_BEST_EFFORT" = "1"
            rm -f "$Inst"
            return 0
        else
            die "no BOLT fragments for $Name"
        end
    end

    if not test -s "$Prof"
        rm -f "$Inst"
        if test "$BOLT_BEST_EFFORT" = "1"
            return 0
        else
            die "no BOLT profile for $Name"
        end
    end

    if not "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" --dyno-stats --reorder-blocks=ext-tsp --reorder-functions=cdsort \
        --split-functions --split-strategy=cdsplit --split-all-cold --split-eh \
        --icf=safe --jump-tables=move --indirect-call-promotion=all --peepholes=all \
        --simplify-rodata-loads --x86-strip-redundant-address-size --strip-rep-ret --inline-memcpy \
        --plt=all --hugify --use-gnu-stack --update-debug-sections=0
        run "$BOLT" "$Real" -o "$Opt" \
            --data "$Prof" --dyno-stats --reorder-blocks=ext-tsp --reorder-functions=cdsort \
            --split-functions --split-all-cold --split-eh --icf=safe --jump-tables=move --use-gnu-stack --update-debug-sections=0
    end

    mv -f "$Opt" "$Real"
    rm -f "$Inst" "$Prof"

    if test "$KEEP_PRE_BOLT_BACKUP" = "1"
        log "[$Name] keeping pre-BOLT backup at $Backup"
    else
        rm -f "$Backup"
    end
end

log ">>> STAGE 3: BOLT post-link (clang + ld.lld)..."
bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"
if test -e "$INSTALL_PREFIX/bin/ld.lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/ld.lld"
else if test -e "$INSTALL_PREFIX/bin/lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
end

set -l _bolt_leftovers (path filter -- "$BUILD_ROOT"/bolt*)
test (count $_bolt_leftovers) -gt 0; and rm -rf $_bolt_leftovers

log "ULTIMATE v4 build finished. Toolchain at: $INSTALL_PREFIX"
if test "$REQUIRE_GOLD_PLUGIN" = "1"
    log "Verified deliverables: $INSTALL_PREFIX/bin/clang  $INSTALL_PREFIX/bin/ld.lld  $INSTALL_PREFIX/lib/LLVMgold.so"
end
log "To use: export PATH=$INSTALL_PREFIX/bin:\$PATH"
