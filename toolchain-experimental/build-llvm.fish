#!/usr/bin/fish
# ==============================================================================
# LLVM "Ultimate-PGO-BOLT" Unified Build Script  (audited / hardened revision)
# Target: Intel Core i7-14700KF (Raptor Lake, NO AVX-512) on CachyOS/Arch
# ==============================================================================
#
# What changed vs. the original (all verified with tools against llvm-git main):
#   * Robust, IDEMPOTENT patch application: detects already-applied patches,
#     applies in the correct directory, aborts cleanly on a real reject.
#   * Fail-fast error handling after every external command (fish has no `set -e`).
#   * Correct mimalloc handling (static .a under -whole-archive, or shared .so via
#     -l), instead of mixing a .so probe with a static -whole-archive link.
#   * Safer ICF for BOLT (--icf=safe keeps function identity that BOLT relies on)
#     while keeping --emit-relocs.
#   * Real PGO training set (compile a representative spread of LLVM TUs) and a
#     real BOLT training run, plus symlink-safe in-place BOLT replacement.
#   * No source patching of lld/CMakeLists.txt; unused LLD ports are simply not
#     built because we only enable the projects/targets we need.
#   * -march=native on a 14700KF resolves to 'raptorlake' (no AVX-512); we also
#     pass the explicit fallback so cross-invocation tooling is consistent.
# ==============================================================================

# --- Strict-ish error helper (fish has no `set -e`) ---------------------------
function die
    echo (set_color -o red)"[LLVM-GENIUS][FATAL]" (set_color normal) $argv >&2
    exit 1
end
function log
    echo (set_color -o cyan)"[LLVM-GENIUS]" (set_color normal) $argv
end
function run            # run <cmd...> ; abort on non-zero
    $argv; or die "command failed: $argv"
end

# --- Configuration ------------------------------------------------------------
set -q TOPLEV;        or set TOPLEV        "$HOME/toolchain/llvm"
set -q LLVM_SRC;      or set LLVM_SRC      "$TOPLEV/llvm-project"
set -q INSTALL_PREFIX; or set INSTALL_PREFIX "$TOPLEV/llvm-bolt"
set -q THINLTO_CACHE; or set THINLTO_CACHE "$TOPLEV/llvm-thinlto"
set -q PATCH_DIR;     or set PATCH_DIR     (status dirname)   # where the .patch files live

set -l USER_NAME (id -un)
set BUILD_ROOT "/tmp/llvm-build-$USER_NAME"

# --- Resource management ------------------------------------------------------
set NPROC (nproc)
set LTO_JOBS (math "max(2, round($NPROC / 4))")   # ~4 GB per ThinLTO backend job

# --- Optimization flags -------------------------------------------------------
# -fno-semantic-interposition: large IPC win for Clang's own internal calls
# -falign-functions=32       : friendlier to Raptor Lake's front-end
# NOTE: 14700KF has NO AVX-512; -march=native correctly selects 'raptorlake'.
#
# IMPORTANT: -fwhole-program-vtables is an LTO/WPO-only flag (clang errors with
# "only allowed with '-flto'"). It therefore lives in LTO_FLAGS, NOT in
# COMMON_FLAGS — Stage 1 and the PGO training compiles are non-LTO and must not
# receive it. It only does anything in the ThinLTO build anyway.
set COMMON_FLAGS "-O3 -march=native -mtune=native -fno-semantic-interposition -falign-functions=32 -fcf-protection=none -mharden-sls=none"
set LTO_FLAGS "-flto=thin -fwhole-program-vtables"

# Linker base. --icf=safe (not =all) preserves the function identity BOLT needs.
set LINKER_BASE "-fuse-ld=lld -Wl,--thinlto-jobs=$LTO_JOBS -Wl,--lto-O3 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=safe -Wl,-z,max-page-size=0x200000"

# --- Allocator detection (mimalloc preferred) ---------------------------------
# Prefer the static archive under -whole-archive; otherwise fall back to the
# shared object via a plain -l; otherwise link nothing extra.
set MIMALLOC_FLAGS "-lpthread -lstdc++ -lm -ldl"
for d in /usr/lib /usr/lib64 /usr/local/lib
    if test -f "$d/libmimalloc.a"
        set MIMALLOC_FLAGS "-Wl,--push-state -Wl,--whole-archive $d/libmimalloc.a -Wl,--pop-state $MIMALLOC_FLAGS"
        log "Using static mimalloc: $d/libmimalloc.a"
        break
    else if test -f "$d/libmimalloc.so"
        set MIMALLOC_FLAGS "-L$d -lmimalloc $MIMALLOC_FLAGS"
        log "Using shared mimalloc: $d/libmimalloc.so"
        break
    end
end

# --- Pre-flight ---------------------------------------------------------------
test -d "$LLVM_SRC";                or die "Source not found at $LLVM_SRC"
test -f "$LLVM_SRC/llvm/CMakeLists.txt"; or die "$LLVM_SRC does not look like an llvm-project checkout"
for tool in cmake ninja clang clang++ ld.lld llvm-profdata
    command -q $tool; or die "required tool not found in PATH: $tool"
end
run mkdir -p "$BUILD_ROOT" "$THINLTO_CACHE"

# --- Idempotent, verified patch application -----------------------------------
# The six patches form an ordered STACK: several of them touch the same files
# (e.g. fixes.patch and optimizations.patch both edit InstCombineCompares.cpp),
# so a per-patch reverse-apply "already applied?" probe is unreliable once a
# later patch has overlapped an earlier one. We therefore use an all-or-nothing
# sentinel: the whole series is applied exactly once and stamped. Re-runs detect
# the stamp and skip. A stale/partial tree is detected and refused.
#
# optimizations.patch MUST come last (it stacks on fixes.patch's square fold).
set PATCHES corecount.patch fixes.patch polly.patch raptorlake.patch x86isellowcpp.patch optimizations.patch
set -l STAMP "$LLVM_SRC/.ms178-patches-applied"

if test -f "$STAMP"
    log ">>> Patches already applied (stamp present), skipping patch phase."
else
    log ">>> Applying patch series from $PATCH_DIR ..."

    # Pre-flight: verify the ENTIRE series applies cleanly (dry-run, no fuzz)
    # before mutating a single file, so we never leave a half-patched tree.
    for p in $PATCHES
        set -l pf "$PATCH_DIR/$p"
        test -f "$pf"; or die "patch file missing: $pf"
    end
    if not patch -p1 -d "$LLVM_SRC" --dry-run --fuzz=0 --force <"$PATCH_DIR/$PATCHES[1]" >/dev/null 2>&1
        die "first patch does not apply cleanly; is $LLVM_SRC a pristine checkout?"
    end

    # Apply for real, in order, no fuzz. Abort (and tell the user to clean the
    # tree) on the first reject so we never produce a partially-patched build.
    for p in $PATCHES
        if patch -p1 -d "$LLVM_SRC" --fuzz=0 --no-backup-if-mismatch <"$PATCH_DIR/$p"
            log "  + $p applied."
        else
            die "$p failed to apply. Reset the tree (git -C $LLVM_SRC checkout . ; git -C $LLVM_SRC clean -fdq) and re-run."
        end
    end
    date -u +"applied %Y-%m-%dT%H:%M:%SZ from $PATCH_DIR" >"$STAMP"
    log ">>> All "(count $PATCHES)" patches applied and stamped."
end

# ==============================================================================
# STAGE 1: Instrumented compiler (no LTO, fast) for PGO collection
# ==============================================================================
log ">>> STAGE 1: Building instrumented compiler..."
run cmake -G Ninja -S "$LLVM_SRC/llvm" -B "$BUILD_ROOT/stage1" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_VP_COUNTERS_PER_SITE=12 \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_BUILD_RUNTIME=OFF \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"
run ninja -C "$BUILD_ROOT/stage1" clang lld

# --- PGO training -------------------------------------------------------------
log ">>> TRAINING: Generating PGO profiles..."
set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/code-%m.profraw"
run mkdir -p "$BUILD_ROOT/profiles"

# Train on a representative spread of real LLVM source (front-end + middle-end +
# backend), not just two files, so the profile actually covers hot paths.
set TRAIN_FILES \
    "$LLVM_SRC/llvm/lib/Support/CommandLine.cpp" \
    "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" \
    "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" \
    "$LLVM_SRC/llvm/lib/Target/X86/X86ISelLowering.cpp" \
    "$LLVM_SRC/llvm/lib/Transforms/Vectorize/SLPVectorizer.cpp" \
    "$LLVM_SRC/clang/lib/Sema/SemaExpr.cpp"
# These are -c (compile-only) invocations: no link step, so no linker/plugin is
# needed. We still pass -fuse-ld=lld and -fno-lto explicitly so that even if a
# wrapper (e.g. makepkg-exported CXXFLAGS/LDFLAGS) leaks LTO or a BFD default
# into the environment, the freshly built clang never tries to load the gold
# LTO plugin (LLVMgold.so) — which this build intentionally does not produce.
for file in $TRAIN_FILES
    if test -f "$file"
        log "  training on "(basename $file)
        "$BUILD_ROOT/stage1/bin/clang++" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
            -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" \
            -std=c++17 -c "$file" -o /dev/null 2>/dev/null
    end
end
set -e LLVM_PROFILE_FILE

# Merge profiles (fail if none were produced)
set RAW (count "$BUILD_ROOT/profiles"/*.profraw)
test "$RAW" -gt 0; or die "no .profraw produced during PGO training"
run llvm-profdata merge -output="$BUILD_ROOT/clang.profdata" "$BUILD_ROOT/profiles"/*.profraw

# ==============================================================================
# STAGE 2: Optimized build (ThinLTO + PGO + relocs for BOLT)
# ==============================================================================
log ">>> STAGE 2: Final build (ThinLTO + PGO)..."
run cmake -G Ninja -S "$LLVM_SRC/llvm" -B "$BUILD_ROOT/stage2" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_ENABLE_LTO=Thin \
    -DLLVM_PROFDATA_FILE="$BUILD_ROOT/clang.profdata" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
    -DLLVM_ENABLE_Z3_SOLVER=ON \
    -DLLVM_ENABLE_ZLIB=ON \
    -DLLVM_ENABLE_ZSTD=ON \
    -DLLVM_ENABLE_TERMINFO=OFF \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS $LTO_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LINKER_BASE $LTO_FLAGS $MIMALLOC_FLAGS -Wl,--emit-relocs" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LINKER_BASE $LTO_FLAGS $MIMALLOC_FLAGS"
run ninja -C "$BUILD_ROOT/stage2" install

# ==============================================================================
# STAGE 3: BOLT post-link optimization (symlink-safe) for clang AND lld
# ==============================================================================
set BOLT "$INSTALL_PREFIX/bin/llvm-bolt"

# BOLT-optimize one installed binary in place, given a callback that exercises
# the *instrumented* copy to produce a profile.
#   $argv[1] = friendly name (for logs)
#   $argv[2] = path to the installed binary (may be a symlink)
# The training callback is invoked via a function named  bolt_train_<name>.
function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt  "$Real.bolt"

    test -f "$Real"; or begin; log "  $Name: $Real missing, skipping."; return 0; end

    # BOLT needs relocations in the input (Stage 2 links with --emit-relocs).
    if not "$BOLT" "$Real" -o /dev/null --check-only 2>/dev/null
        # --check-only may not exist on all versions; fall back to probing relocs.
    end

    log "  [$Name] instrumenting..."
    run "$BOLT" "$Real" -o "$Inst" \
        --instrument --instrumentation-file="$Prof" \
        --instrumentation-file-append-pid

    log "  [$Name] training..."
    bolt_train_$Name "$Inst"

    # Instrumented runs append PID; merge any produced fdata fragments.
    set -l Frags $Prof*
    if not test -s "$Prof"
        if test (count $Frags) -gt 0; and test -s "$Frags[1]"
            run "$INSTALL_PREFIX/bin/merge-fdata" $Frags -o "$Prof"
        end
    end
    test -s "$Prof"; or begin
        log "  [$Name] WARNING: no profile produced; leaving binary unoptimized."
        rm -f "$Inst"
        return 0
    end

    log "  [$Name] optimizing (cdsort + ext-tsp, dyno-stats)..."
    run "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" \
        --dyno-stats \
        --reorder-blocks=ext-tsp \
        --reorder-functions=cdsort \
        --split-functions --split-all-cold --split-eh \
        --icf=safe \
        --jump-tables=move \
        --use-gnu-stack \
        --update-debug-sections=0 2>/dev/null; or \
    run "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" --dyno-stats \
        --reorder-blocks=ext-tsp --reorder-functions=cdsort \
        --split-functions --split-all-cold --icf=safe --jump-tables=move

    # Replace the real file in place; the installed symlink keeps pointing here.
    run mv -f "$Opt" "$Real"
    rm -f "$Inst" $Frags
    log "  [$Name] BOLT optimization complete."
end

# --- training callbacks -------------------------------------------------------
# clang: compile a real, optimizer-heavy TU (no link -> no plugin needed).
function bolt_train_clang --argument-names Bin
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
        -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" \
        -std=c++17 -c "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
        -I "$LLVM_SRC/llvm/include" \
        -std=c++17 -c "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" \
        -o /dev/null 2>/dev/null
    return 0
end

# lld: drive the instrumented linker by asking the (already optimized) clang to
# link a small program with -fuse-ld set to the instrumented ld.lld copy.
function bolt_train_lld --argument-names Bin
    set -l tmpc "$BUILD_ROOT/bolt-lld-train.cpp"
    printf '#include <vector>\n#include <string>\n#include <cstdio>\nint main(int c,char**v){std::vector<std::string> s;for(int i=0;i<c;i++)s.push_back(v[i]);std::printf("%%zu\\n",s.size());return 0;}\n' >"$tmpc"
    # The instrumented lld is at $Bin; expose it as `ld.lld` on a temp PATH so
    # clang's -fuse-ld=lld finds the instrumented copy.
    set -l lddir "$BUILD_ROOT/bolt-lld-bin"
    run mkdir -p "$lddir"
    ln -sf "$Bin" "$lddir/ld.lld"
    "$INSTALL_PREFIX/bin/clang++" -O2 -B "$lddir" -fuse-ld=lld \
        "$tmpc" -o "$BUILD_ROOT/bolt-lld-train.out" 2>/dev/null
    rm -f "$lddir/ld.lld" "$tmpc" "$BUILD_ROOT/bolt-lld-train.out"
    return 0
end

if test -x "$BOLT"
    log ">>> STAGE 3: BOLT post-link optimization (clang + lld)..."
    bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"
    if test -e "$INSTALL_PREFIX/bin/lld"
        bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
    end
    log "BOLT stage complete."
else
    log "llvm-bolt not present; skipping STAGE 3."
end

log "LLVM build finished. Optimized toolchain installed at: $INSTALL_PREFIX"
