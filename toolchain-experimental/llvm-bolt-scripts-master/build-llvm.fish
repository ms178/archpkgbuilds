#!/usr/bin/env fish
# LLVM PGO+BOLT toolchain build for i7-14700KF (Raptor Lake, no AVX-512).
# Source: $HOME/toolchain/llvm/llvm-project   Build: /tmp/llvm-build-$USER

function die
    echo (set_color -o red)"[LLVM-GENIUS][FATAL]"(set_color normal) "$argv" >&2
    exit 1
end
function log
    echo (set_color -o cyan)"[LLVM-GENIUS]"(set_color normal) "$argv"
end
function run
    $argv; or die "command failed: $argv"
end

# --- Configuration (-g so STAGE 3 functions can read these) -------------------
set -q TOPLEV;          or set -g TOPLEV          "$HOME/toolchain/llvm"
set -q LLVM_SRC;        or set -g LLVM_SRC        "$TOPLEV/llvm-project"
set -q INSTALL_PREFIX;  or set -g INSTALL_PREFIX  "$TOPLEV/llvm-bolt"
set -q PATCH_DIR;       or set -g PATCH_DIR       (status dirname)
set -q LLVM_PIN;        or set -g LLVM_PIN        ""
set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 0

set -g USER_NAME (id -un)
set -q BUILD_ROOT;      or set -g BUILD_ROOT      "/tmp/llvm-build-$USER_NAME"
# ThinLTO cache is regenerable scratch -> keep it in /tmp with the rest.
set -q THINLTO_CACHE;   or set -g THINLTO_CACHE   "$BUILD_ROOT/llvm-thinlto"

set -g NPROC (nproc)
set -g LTO_JOBS (math -s0 "max(2, round($NPROC / 4))")   # ~4 GB per ThinLTO job

# --- Flags --------------------------------------------------------------------
set -g COMMON_FLAGS  "-O3 -march=native -mtune=native -fno-semantic-interposition -falign-functions=32 -fcf-protection=none -mharden-sls=none"
# C and C++ must agree on LTO-unit splitting or lld errors ("inconsistent LTO
# Unit splitting"). -fwhole-program-vtables (C++) implies -fsplit-lto-unit; the C
# side sets it explicitly. -flto=thin is kept in-flags (not only via
# LLVM_ENABLE_LTO) so CMake's TryCompile, which sees -fwhole-program-vtables,
# also has -flto and passes.
set -g C_LTO_FLAGS   "-flto=thin -fsplit-lto-unit"
set -g CXX_LTO_FLAGS "-flto=thin -fwhole-program-vtables"
set -g LINKER_BASE "-fuse-ld=lld -Wl,--thinlto-jobs=$LTO_JOBS -Wl,--lto-O3 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=safe -Wl,-z,max-page-size=0x200000"

# --- Allocator (mimalloc, EXE link only) --------------------------------------
set -g MIMALLOC_EXE "-lpthread -lstdc++ -lm -ldl"
for d in /usr/lib /usr/lib64 /usr/local/lib
    if test -f "$d/libmimalloc.a"
        set -g MIMALLOC_EXE "-Wl,--push-state -Wl,--whole-archive $d/libmimalloc.a -Wl,--pop-state $MIMALLOC_EXE"
        log "Using static mimalloc: $d/libmimalloc.a"
        break
    else if test -f "$d/libmimalloc.so"
        set -g MIMALLOC_EXE "-L$d -lmimalloc $MIMALLOC_EXE"
        log "Using shared mimalloc: $d/libmimalloc.so"
        break
    end
end

# --- Pre-flight ---------------------------------------------------------------
test -d "$LLVM_SRC";                     or die "Source not found at $LLVM_SRC"
test -f "$LLVM_SRC/llvm/CMakeLists.txt"; or die "$LLVM_SRC is not an llvm-project checkout"
for tool in cmake ninja clang clang++ ld.lld llvm-profdata
    command -q $tool; or die "required tool not found in PATH: $tool"
end
run mkdir -p "$BUILD_ROOT" "$THINLTO_CACHE"

# --- Always reconfigure from a clean cache (stale cache silently drops -D) -----
set -g CMAKE_FRESH ""
if cmake --help 2>/dev/null | grep -q -- "--fresh"
    set -g CMAKE_FRESH "--fresh"
end

function configure_clean
    set -l bdir $argv[1]
    set -e argv[1]
    run mkdir -p "$bdir"
    if test -n "$CMAKE_FRESH"
        run cmake $CMAKE_FRESH -G Ninja -B "$bdir" $argv
    else
        rm -rf "$bdir/CMakeCache.txt" "$bdir/CMakeFiles"
        run cmake -G Ninja -B "$bdir" $argv
    end
end

# --- Optional reproducibility pin ---------------------------------------------
if test -n "$LLVM_PIN"
    command -q git; or die "LLVM_PIN set but git not found"
    test -d "$LLVM_SRC/.git"; or die "LLVM_PIN set but $LLVM_SRC is not a git checkout"
    log "Pinning llvm-project to $LLVM_PIN"
    run git -C "$LLVM_SRC" checkout -q "$LLVM_PIN"
end

# --- Patch series (applied once, stamped; optimizations.patch must be last) ----
set -g PATCHES corecount.patch fixes.patch polly.patch raptorlake.patch x86isellowcpp.patch optimizations.patch
set -g STAMP "$LLVM_SRC/.ms178-patches-applied"
if test -f "$STAMP"
    log "Patches already applied, skipping."
else if not test -d "$PATCH_DIR"; or test (count $PATCH_DIR/*.patch) -eq 0
    log "No patches found at $PATCH_DIR; skipping patch phase."
else
    log "Applying patch series from $PATCH_DIR ..."
    for p in $PATCHES
        set -l pf "$PATCH_DIR/$p"
        test -f "$pf"; or die "patch file missing: $pf"
    end
    if not patch -p1 -d "$LLVM_SRC" --dry-run --fuzz=0 --force <"$PATCH_DIR/$PATCHES[1]" >/dev/null 2>&1
        die "first patch does not apply cleanly; is $LLVM_SRC pristine?"
    end
    for p in $PATCHES
        if patch -p1 -d "$LLVM_SRC" --fuzz=0 --no-backup-if-mismatch <"$PATCH_DIR/$p"
            log "  + $p applied."
        else
            die "$p failed. Reset the tree (git -C $LLVM_SRC checkout . ; git -C $LLVM_SRC clean -fdq) and re-run."
        end
    end
    date -u +"applied %Y-%m-%dT%H:%M:%SZ from $PATCH_DIR" >"$STAMP"
    log "All "(count $PATCHES)" patches applied and stamped."
end

# ==============================================================================
# STAGE 1: Instrumented compiler (no LTO) for PGO collection
# ==============================================================================
log ">>> STAGE 1: Building instrumented compiler..."
configure_clean "$BUILD_ROOT/stage1" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_VP_COUNTERS_PER_SITE=12 \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_BUILD_RUNTIME=OFF \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"
run ninja -C "$BUILD_ROOT/stage1" clang lld llvm-profdata

# Use stage-1 llvm-profdata to avoid IR-profile format skew vs a system tool.
set -g PROFDATA "$BUILD_ROOT/stage1/bin/llvm-profdata"
test -x "$PROFDATA"; or set -g PROFDATA (command -v llvm-profdata)

# --- PGO training -------------------------------------------------------------
log ">>> TRAINING: Generating PGO profiles (C + C++ TUs)..."
set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/code-%m.profraw"
run mkdir -p "$BUILD_ROOT/profiles"

set -g TRAIN_CXX \
    "$LLVM_SRC/llvm/lib/Support/CommandLine.cpp" \
    "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" \
    "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" \
    "$LLVM_SRC/llvm/lib/Target/X86/X86ISelLowering.cpp" \
    "$LLVM_SRC/llvm/lib/Transforms/Vectorize/SLPVectorizer.cpp" \
    "$LLVM_SRC/clang/lib/Sema/SemaExpr.cpp"

set -g TRAIN_C \
    "$LLVM_SRC/llvm/lib/Support/regcomp.c" \
    "$LLVM_SRC/llvm/lib/Support/regexec.c" \
    "$LLVM_SRC/llvm/lib/Support/regerror.c" \
    "$LLVM_SRC/llvm/lib/Support/regfree.c" \
    "$LLVM_SRC/llvm/lib/Support/BLAKE3/blake3.c" \
    "$LLVM_SRC/llvm/lib/Support/BLAKE3/blake3_dispatch.c" \
    "$LLVM_SRC/llvm/lib/Support/BLAKE3/blake3_portable.c"

for file in $TRAIN_CXX
    if test -f "$file"
        log "  [c++] training on "(basename $file)
        "$BUILD_ROOT/stage1/bin/clang++" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
            -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" \
            -std=c++17 -c "$file" -o /dev/null 2>/dev/null
    end
end
for file in $TRAIN_C
    if test -f "$file"
        log "  [c]   training on "(basename $file)
        "$BUILD_ROOT/stage1/bin/clang" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
            -I "$LLVM_SRC/llvm/include" \
            -std=gnu17 -c "$file" -o /dev/null 2>/dev/null
    end
end
set -e LLVM_PROFILE_FILE

set -g RAW "$BUILD_ROOT/profiles"/*.profraw
test (count $RAW) -gt 0; or die "no .profraw produced during PGO training"
run $PROFDATA merge -output="$BUILD_ROOT/clang.profdata" $RAW
log "Merged "(count $RAW)" raw profiles into clang.profdata"

# ==============================================================================
# STAGE 2: Optimized build (ThinLTO + PGO + relocs for BOLT)
# ==============================================================================
log ">>> STAGE 2: Final build (ThinLTO + PGO)..."
configure_clean "$BUILD_ROOT/stage2" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_ENABLE_LTO=Thin \
    -DLLVM_PROFDATA_FILE="$BUILD_ROOT/clang.profdata" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
    -DLLVM_ENABLE_Z3_SOLVER=ON \
    -DLLVM_ENABLE_ZLIB=ON \
    -DLLVM_ENABLE_ZSTD=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LINKER_BASE $MIMALLOC_EXE -Wl,--emit-relocs -Wl,-z,now" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LINKER_BASE" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LINKER_BASE"
run ninja -C "$BUILD_ROOT/stage2" install

# ==============================================================================
# STAGE 3: BOLT post-link optimization (symlink-safe) for clang AND lld
# ==============================================================================
set -g BOLT  "$INSTALL_PREFIX/bin/llvm-bolt"
set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"

# clang exits normally (DT_FINI flush works) -> --instrumentation-file-append-pid
# so every process/fork writes its OWN file; merge fragments with merge-fdata.
# A single fixed file shared by multiple instrumented runs gets torn writes
# ("expected 0, 1 or 2" parse error). lld exits via _exit() (no DT_FINI flush,
# #59059), so it needs the sleep-time watcher and trains serially (one watcher
# at a time -> one file, no sharing).
function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt  "$Real.bolt"
    test -f "$Real"; or begin; log "  $Name: $Real missing, skipping."; return 0; end

    # Glob into a var first: a bare glob in command position aborts fish on no-match.
    set -l Stale $Prof $Prof.*
    test (count $Stale) -gt 0; and rm -f $Stale

    log "  [$Name] instrumenting..."
    if test "$Name" = lld
        run "$BOLT" "$Real" -o "$Inst" \
            --instrument \
            --instrumentation-file="$Prof" \
            --instrumentation-sleep-time=1 \
            --instrumentation-no-counters-clear
    else
        run "$BOLT" "$Real" -o "$Inst" \
            --instrument \
            --instrumentation-file="$Prof" \
            --instrumentation-file-append-pid
    end

    log "  [$Name] training..."
    bolt_train_$Name "$Inst"

    if test "$Name" = lld
        # Wait for the watcher's final on-death dump to land.
        for waited in 1 2 3 4 5
            test -s "$Prof"; and break
            sleep 1
        end
    else
        # Merge the per-pid fragments into $Prof.
        set -l Frags $Prof.*
        test (count $Frags) -gt 0; or begin
            if test "$BOLT_BEST_EFFORT" = 1
                log "  [$Name] WARNING: no profile fragments; leaving binary unoptimized."
                rm -f "$Inst"; return 0
            end
            die "[$Name] no BOLT profile fragments produced. Set BOLT_BEST_EFFORT=1 to skip."
        end
        run "$MERGE" $Frags -o "$Prof"
        rm -f $Frags
    end

    if not test -s "$Prof"
        rm -f "$Inst"
        if test "$BOLT_BEST_EFFORT" = 1
            log "  [$Name] WARNING: no profile produced; leaving binary unoptimized."
            return 0
        end
        die "[$Name] no BOLT profile produced. Set BOLT_BEST_EFFORT=1 to ship unoptimized."
    end

    log "  [$Name] optimizing..."
    # Validated against llvm-git BOLT 23.0.0git (rev 570e532d): all options below
    # are accepted and produce a correct (output-identical) binary.
    # Layout (reorder-functions=cdsort + reorder-blocks=ext-tsp) + hot/cold
    # splitting (cdsplit warm fragment) are the dominant wins (~6% on I-cache/iTLB
    # -bound code locally; clang/lld are exactly that). ICP/peepholes/
    # simplify-rodata-loads help real clang (skewed vtable calls, complex blocks).
    # x86-strip-redundant-address-size + strip-rep-ret remove obsolete-instruction
    # bytes; inline-memcpy uses rep movsb (all harmless, verified correct).
    # hugify maps hot text onto huge pages (iTLB). --plt does NOT need -z now on
    # x86 (llvm-git note), but we keep it linked -z now anyway (harmless).
    # NOTE: --reg-reassign / --use-aggr-reg-reassign are deliberately EXCLUDED —
    # they miscompile BOLTed clang/lld (segfaults building the kernel; llvm
    # issue #123809, never fixed).
    if not "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" \
        --dyno-stats \
        --reorder-blocks=ext-tsp \
        --reorder-functions=cdsort \
        --split-functions --split-strategy=cdsplit --split-all-cold --split-eh \
        --icf=safe \
        --jump-tables=move \
        --indirect-call-promotion=all \
        --peepholes=all \
        --simplify-rodata-loads \
        --x86-strip-redundant-address-size \
        --strip-rep-ret \
        --inline-memcpy \
        --plt=all \
        --hugify \
        --use-gnu-stack \
        --update-debug-sections=0
        # Fallback: a future BOLT may drop/rename an aggressive option. Retry with
        # the minimal, always-supported, measured-safe core so the build never
        # aborts here.
        log "  [$Name] aggressive BOLT failed; retrying with safe core set..."
        run "$BOLT" "$Real" -o "$Opt" \
            --data "$Prof" \
            --dyno-stats \
            --reorder-blocks=ext-tsp \
            --reorder-functions=cdsort \
            --split-functions --split-all-cold --split-eh \
            --icf=safe \
            --jump-tables=move \
            --use-gnu-stack \
            --update-debug-sections=0
    end

    run mv -f "$Opt" "$Real"
    rm -f "$Inst" "$Prof"
    log "  [$Name] BOLT optimization complete."
end

function bolt_train_clang --argument-names Bin
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
        -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" \
        -std=c++17 -c "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
        -I "$LLVM_SRC/llvm/include" \
        -std=c++17 -c "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" \
        -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
        -I "$LLVM_SRC/llvm/include" \
        -std=gnu17 -c "$LLVM_SRC/llvm/lib/Support/regcomp.c" -o /dev/null 2>/dev/null
    return 0
end

# Drive the instrumented ld.lld via clang -B so -fuse-ld=lld finds it.
function bolt_train_lld --argument-names Bin
    set -l lddir "$BUILD_ROOT/bolt-lld-bin"
    run mkdir -p "$lddir"
    ln -sf "$Bin" "$lddir/ld.lld"

    set -l drv "$INSTALL_PREFIX/bin/clang++"
    test -x "$drv"; or set drv clang++

    set -l src "$BUILD_ROOT/bolt-lld-train.cpp"
    printf '%s\n' \
        '#include <vector>' '#include <string>' '#include <map>' '#include <algorithm>' \
        '#include <cstdio>' \
        'static int f(const std::vector<std::string>&v){std::map<std::string,int>m;' \
        'for(auto&s:v)m[s]++;int n=0;for(auto&kv:m)n+=kv.second;return n;}' \
        'int main(int c,char**v){std::vector<std::string> s;' \
        'for(int i=0;i<c;i++)s.push_back(v[i]);std::sort(s.begin(),s.end());' \
        'std::printf("%d %zu\n",f(s),s.size());return 0;}' >"$src"
    "$drv" -O2 -B "$lddir" -fuse-ld=lld \
        -Wl,--gc-sections -Wl,--icf=all \
        "$src" -o "$BUILD_ROOT/bolt-lld-train.out" 2>/dev/null
    "$drv" -O2 -B "$lddir" -fuse-ld=lld -static-libstdc++ \
        -Wl,--gc-sections "$src" -o "$BUILD_ROOT/bolt-lld-train2.out" 2>/dev/null

    rm -f "$lddir/ld.lld" "$src" \
        "$BUILD_ROOT/bolt-lld-train.out" "$BUILD_ROOT/bolt-lld-train2.out"
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

log "LLVM build finished. Toolchain at: $INSTALL_PREFIX"
