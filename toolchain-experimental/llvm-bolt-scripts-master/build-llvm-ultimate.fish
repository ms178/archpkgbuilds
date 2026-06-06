#!/usr/bin/env fish

# ULTIMATE build-llvm-ultimate.fish - MAX PERF for 14700KF Raptor Lake
# Full multi-stage: fresh shallow clone + lld ELF patch + /tmp src copy (docs preserved) + cleanup
# + robust patch lookup (same dir or Downloads) + PGO (VP=8) + CSPGO (VP=8) + FULL_TRAIN + ThinLTO + mimalloc
# + stage1 tblgen provision in stage2 (avoids ProfileSummary conflict on llvm-min-tblgen under ThinLTO)
# + BOLT with hybrid cdsplit+hugify etc.  (clang + ld.lld *explicitly*, with already-BOLTed skip + lld wait for safe resumption)
# The final LTO+PGO stage2 build dir is *deliberately preserved* (the perfected ThinLTO + PGO + mimalloc build before BOLT)
# All other /tmp stages cleaned as soon as no longer needed (stage1 kept only until after stage2 needs its tblgen)
# Defaults: FULL_TRAIN=1 DO_CSPGO=1 USE_MIMALLOC=1

function die
    echo (set_color -o red)"[LLVM-ULTIMATE][FATAL]"(set_color normal) "$argv" >&2
    exit 1
end

function log
    echo (set_color -o cyan)"[LLVM-ULTIMATE]"(set_color normal) "$argv"
end

function run
    $argv; or die "command failed: $argv"
end

# Config
set -q TOPLEV; or set -g TOPLEV "$HOME/toolchain/llvm"
set -q INSTALL_PREFIX; or set -g INSTALL_PREFIX "$HOME/toolchain/llvm-ultimate"
set -q BUILD_ROOT; or set -g BUILD_ROOT "/tmp/llvm-build-$USER"
set -q THINLTO_CACHE; or set -g THINLTO_CACHE "$BUILD_ROOT/thinlto-cache"

set -q PATCH_DIR; or set -g PATCH_DIR (dirname (status filename))

set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 0
set -q FULL_TRAIN; or set -g FULL_TRAIN 1
set -q DO_CSPGO; or set -g DO_CSPGO 1
set -q USE_MIMALLOC; or set -g USE_MIMALLOC 1

set -g NPROC (nproc)
set -g LTO_JOBS (math -s0 "max(2, round($NPROC / 4))")

# Identical VP for PGO and CSPGO (critical to avoid ProfileSummary ID conflicts)
set -g VP_COUNTERS_PER_SITE 8

set -g COMMON_FLAGS "-O3 -march=native -mtune=native -fno-semantic-interposition -falign-functions=32 -falign-loops=32 -fcf-protection=none -mharden-sls=none -fno-plt"
set -g C_LTO_FLAGS "-flto=thin -fsplit-lto-unit"
set -g CXX_LTO_FLAGS "-flto=thin -fwhole-program-vtables"
set -g LINKER_BASE "-fuse-ld=lld -Wl,--thinlto-jobs=$LTO_JOBS -Wl,--lto-O3 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=safe -Wl,-z,max-page-size=0x200000"

# Allocator (mimalloc for the final ThinLTO binaries)
# Prefers static libmimalloc.a with whole-archive for zero-overhead integration.
# Falls back to shared -lmimalloc if only .so is present.
set -g ALLOCATOR_LINK "-lpthread -lstdc++ -lm -ldl"
if test "$USE_MIMALLOC" = "1"
    for d in /usr/lib /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/local/lib
        if test -f "$d/libmimalloc.a"
            set -g ALLOCATOR_LINK "-Wl,--push-state -Wl,--whole-archive $d/libmimalloc.a -Wl,--pop-state $ALLOCATOR_LINK"
            log "ULTIMATE: Using static mimalloc (libmimalloc.a)"
            break
        else if test -f "$d/libmimalloc.so"
            set -g ALLOCATOR_LINK "-L$d -lmimalloc $ALLOCATOR_LINK"
            log "ULTIMATE: Using shared mimalloc (libmimalloc.so)"
            break
        end
    end
end

# === /tmp clean at start - no leftovers ===
log "Cleaning ALL /tmp build directories for clean start..."
rm -rf "$BUILD_ROOT"
rm -rf "$THINLTO_CACHE"
mkdir -p "$BUILD_ROOT" "$THINLTO_CACHE" "$BUILD_ROOT/profiles"
log "/tmp fully cleaned: $BUILD_ROOT (no previous build leftovers)"

# === Fresh LLVM sources: delete old first, shallow clone to $TOPLEV, lld ELF patch ===
log "=== Fresh LLVM sources: delete old, shallow clone to $TOPLEV ==="
mkdir -p "$TOPLEV"
cd "$TOPLEV" || die "Could not cd to $TOPLEV"

if test -d llvm-project
    log "Deleting old sources first..."
    rm -rf llvm-project
end

log "Cloning ( --filter=blob:none --depth=1 )..."
git clone --filter=blob:none --depth=1 https://github.com/llvm/llvm-project.git || die "Clone failed"

cd llvm-project || die "cd llvm-project failed"

# lld ELF-only patch (applied to home tree before copy so copy gets it)
if test -f lld/CMakeLists.txt
    log "Patching lld/CMakeLists.txt for ELF-only (disable MachO/COFF/wasm/MinGW)"
    sed -i.bak -e '/add_subdirectory(MachO)/d' -e '/add_subdirectory(COFF)/d' -e '/add_subdirectory(wasm)/d' -e '/add_subdirectory(MinGW)/d' lld/CMakeLists.txt
    log "lld ELF-only patch applied"
end

# === Copy source to /tmp + clean non-essential parts (EXPLICITLY PRESERVE docs + bolt/polly) ===
set -g LLVM_SRC_TMP "/tmp/llvm-project-src-$USER"
log "Copying source to /tmp for build: $LLVM_SRC_TMP (deleting old tmp source first)"
rm -rf "$LLVM_SRC_TMP"
cp -a "$PWD" "$LLVM_SRC_TMP" || die "Failed to copy source to /tmp"

log "Cleaning non-essential parts from /tmp source copy to save space (tests, unittests, mlir, flang tests, test-suite etc. -- ALL docs dirs + bolt/polly trees EXPLICITLY PRESERVED)"
for sub in llvm/test llvm/unittests clang/test clang/unittests lld/test lld/unittests flang/test flang/unittests test-suite mlir
    rm -rf "$LLVM_SRC_TMP/$sub" 2>/dev/null || true
end
# Note: do NOT rm docs, llvm/docs, clang/docs, lld/docs, bolt/, polly/ etc. -- CMake requires them

set -g LLVM_SRC "$LLVM_SRC_TMP"
log "Source for build now at $LLVM_SRC on /tmp (docs + bolt/polly preserved)"

# Pre-flight
test -d "$LLVM_SRC"; or die "no source"
for t in cmake ninja clang clang++ ld.lld llvm-profdata
    command -q $t; or die "missing $t"
end

# === Robust patch handling for ms178 patches (works from script dir or /home/marcus/Downloads/... etc) ===
set -g PATCHES corecount.patch fixes.patch polly.patch raptorlake.patch x86isellowcpp.patch optimizations.patch
set -g STAMP "$LLVM_SRC/.ms178-patches-applied"

function find_patch --argument-names name
    set -l candidates \
        "$PATCH_DIR/$name" \
        "$PWD/$name" \
        "$HOME/Downloads/llvm-bolt-scripts-master/$name" \
        "/home/marcus/Downloads/llvm-bolt-scripts-master/$name" \
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
    log "Applying raptorlake etc patches (robust search)..."
    for p in $PATCHES
        set -l pf (find_patch $p)
        if test -z "$pf"
            set -l searched "$PATCH_DIR, $PWD, ~/Downloads/llvm-bolt-scripts-master, /home/marcus/Downloads/llvm-bolt-scripts-master, current dir"
            die "patch file missing: $p (searched: $searched). Put patches next to the script or in Downloads/llvm-bolt-scripts-master"
        end
        log "  + $p from $pf"
        patch -p1 -d "$LLVM_SRC" --fuzz=3 --no-backup-if-mismatch < "$pf" 2>&1 | tail -1
    end
    date > "$STAMP"
    log "All patches applied to $LLVM_SRC"
end

# configure_clean helper
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

# STAGE 1
log ">>> STAGE 1: Building instrumented compiler (IR PGO)..."
configure_clean "$BUILD_ROOT/stage1" -S "$LLVM_SRC/llvm" \
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
    -DLLVM_BUILD_RUNTIME=OFF \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"

# Build host TableGen tools here so we can point stage2 at them (prevents ProfileSummary conflicts in ThinLTO host-tool links)
run ninja -C "$BUILD_ROOT/stage1" clang lld llvm-profdata llvm-tblgen llvm-min-tblgen clang-tblgen

test -x "$BUILD_ROOT/stage1/bin/llvm-tblgen" || die "stage1 llvm-tblgen missing"
test -x "$BUILD_ROOT/stage1/bin/clang-tblgen" || die "stage1 clang-tblgen missing"

set -g PROFDATA "$BUILD_ROOT/stage1/bin/llvm-profdata"
test -x "$PROFDATA"; or set -g PROFDATA (command -v llvm-profdata)

# Training (small + optional full)
log ">>> TRAINING: Generating PGO profiles..."
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
        set -l ext (string split . $f)[-1]
        if test "$ext" = "cpp" -o "$ext" = "cc"
            "$BUILD_ROOT/stage1/bin/clang++" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
                -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" \
                -std=c++17 -c "$f" -o /dev/null 2>/dev/null
        else
            "$BUILD_ROOT/stage1/bin/clang" $COMMON_FLAGS -fno-lto -fuse-ld=lld \
                -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
        end
    end
end

if test "$FULL_TRAIN" = "1"
    log "FULL_TRAIN=1: full LLVM training build in /tmp..."
    set -l tb "$BUILD_ROOT/full-pgo-train"
    rm -rf "$tb"
    cmake -G Ninja -B "$tb" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
        -DLLVM_USE_LINKER=lld -DCMAKE_C_COMPILER="$BUILD_ROOT/stage1/bin/clang" -DCMAKE_CXX_COMPILER="$BUILD_ROOT/stage1/bin/clang++" \
        -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS" -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF
    run ninja -C "$tb" clang lld -j"$NPROC"
end

set -e LLVM_PROFILE_FILE
$PROFDATA merge -output="$BUILD_ROOT/clang.profdata" "$BUILD_ROOT/profiles"/pgo-*.profraw
log "PGO profdata ready"
set -g FINAL_PROFDATA "$BUILD_ROOT/clang.profdata"

# CSPGO (identical VP=8)
if test "$DO_CSPGO" = "1"
    log ">>> CSPGO: Context sensitive..."
    set -g csd "$BUILD_ROOT/stage-cs-instr"
    configure_clean "$csd" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
        -DLLVM_USE_LINKER=lld -DLLVM_BUILD_INSTRUMENTED=CSIR -DLLVM_PROFDATA_FILE="$BUILD_ROOT/clang.profdata" \
        -DLLVM_VP_COUNTERS_PER_SITE=$VP_COUNTERS_PER_SITE -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"
    run ninja -C "$csd" clang lld llvm-profdata llvm-tblgen llvm-min-tblgen

    set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/cs-%m.profraw"
    for f in $TRAIN_FILES
        if test -f "$f"
            set -l ext (string split . $f)[-1]
            if test "$ext" = "cpp" -o "$ext" = "cc"
                "$csd/bin/clang++" $COMMON_FLAGS -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$f" -o /dev/null 2>/dev/null
            else
                "$csd/bin/clang" $COMMON_FLAGS -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
            end
        end
    end

    if test "$FULL_TRAIN" = "1"
        log "CSPGO full training..."
        set -l ctb "$BUILD_ROOT/full-cspgo-train"
        rm -rf "$ctb"
        cmake -G Ninja -B "$ctb" -S "$LLVM_SRC/llvm" \
            -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
            -DLLVM_USE_LINKER=lld -DCMAKE_C_COMPILER="$csd/bin/clang" -DCMAKE_CXX_COMPILER="$csd/bin/clang++" \
            -DCMAKE_C_FLAGS="$COMMON_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS" -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF
        run ninja -C "$ctb" clang lld -j"$NPROC"
    end

    set -e LLVM_PROFILE_FILE
    $PROFDATA merge -output="$BUILD_ROOT/cs.profdata" "$BUILD_ROOT/profiles"/cs-*.profraw
    $PROFDATA merge -output="$BUILD_ROOT/final.profdata" "$BUILD_ROOT/clang.profdata" "$BUILD_ROOT/cs.profdata"
    set -g FINAL_PROFDATA "$BUILD_ROOT/final.profdata"
end

# Clean previous stage dirs that are no longer needed.
# IMPORTANT: keep stage1 until AFTER stage2 (its tblgen is referenced in stage2 configure/build to avoid host-tool ProfileSummary conflicts)
# The final LTO+PGO stage2 build is deliberately *not* deleted (see comment after install).
log "Cleaning previous stage dirs in /tmp to save space (stage1 kept for tblgen during stage2)..."
rm -rf "$BUILD_ROOT/stage-cs-instr" "$BUILD_ROOT/full-pgo-train" "$BUILD_ROOT/full-cspgo-train" "$BUILD_ROOT/profiles" 2>/dev/null || true

# STAGE 2 - key: -DLLVM_TABLEGEN + -DCLANG_TABLEGEN from stage1 so that host TableGen tools (llvm-min-tblgen etc)
# are NEVER compiled/linked under the final.profdata + ThinLTO (this eliminates the exact ProfileSummary ID conflict)
log ">>> STAGE 2: Final build (ThinLTO + PGO + allocator)..."
configure_clean "$BUILD_ROOT/stage2" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly" -DLLVM_ENABLE_RUNTIMES="compiler-rt" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld -DCLANG_DEFAULT_LINKER=lld -DLLVM_ENABLE_LTO=Thin -DLLVM_PROFDATA_FILE="$FINAL_PROFDATA" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" -DLLVM_OPTIMIZED_TABLEGEN=ON -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
    -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LINKER_BASE $ALLOCATOR_LINK -Wl,--emit-relocs -Wl,-z,now" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LINKER_BASE" -DCMAKE_SHARED_LINKER_FLAGS="$LINKER_BASE"

run ninja -C "$BUILD_ROOT/stage2" install

# IMPORTANT: The perfected LTO+PGO final stage (stage2) is *deliberately not deleted*.
# This preserves the complete ThinLTO + final.profdata + mimalloc build (pre-BOLT) for inspection,
# comparison against the final BOLTed binaries, or as a fallback "perfected non-BOLT" toolchain.
# Only stage1 (no longer needed after tblgen use) and earlier temp stages are cleaned.
log "Cleaning stage1 (tblgen no longer needed) ... (stage2 / final LTO+PGO build is deliberately preserved)"
rm -rf "$BUILD_ROOT/stage1" 2>/dev/null || true

# STAGE 3 BOLT
set -g BOLT "$INSTALL_PREFIX/bin/llvm-bolt"
set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"

# Detect if a binary has already been processed by BOLT (prevents "input file was processed by BOLT. Cannot re-optimize")
function is_already_bolted --argument-names BinPath
    set -l f (realpath "$BinPath")
    test -f "$f"; or return 1

    # BOLT leaves these markers after successful processing
    nm -D "$f" 2>/dev/null | grep -q __bolt_runtime_start; and return 0
    strings "$f" 2>/dev/null | grep -q __bolt_runtime_start; and return 0
    strings "$f" 2>/dev/null | grep -q 'BOLT'; and return 0
    return 1
end

function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt "$Real.bolt"
    test -f "$Real"; or return 0

    # NEW: Skip gracefully if already BOLTed (allows re-running the script or recovering from partial BOLT runs)
    if is_already_bolted "$Real"
        log "  [$Name] already BOLT-processed (detected BOLT markers like __bolt_runtime_start), skipping."
        return 0
    end

    set -l Stale $Prof $Prof.*
    test (count $Stale) -gt 0; and rm -f $Stale

    log "  [$Name] instrumenting..."
    if test "$Name" = lld
        run "$BOLT" "$Real" -o "$Inst" --instrument --instrumentation-file="$Prof" --instrumentation-sleep-time=1 --instrumentation-no-counters-clear
    else
        run "$BOLT" "$Real" -o "$Inst" --instrument --instrumentation-file="$Prof" --instrumentation-file-append-pid
    end

    log "  [$Name] training..."
    bolt_train_$Name "$Inst"

    if test "$Name" = lld
        # Wait for the watcher's final on-death dump to land (lld uses _exit() and the
        # --instrumentation-sleep-time watcher; the profile may not be flushed immediately).
        for waited in 1 2 3 4 5
            test -s "$Prof"; and break
            sleep 1
        end
    else
        # For clang etc. that use append-pid, merge the per-pid fragments.
        set -l Frags $Prof.*
        if test (count $Frags) -gt 0
            run "$MERGE" $Frags -o "$Prof"
            rm -f $Frags
        else if test "$BOLT_BEST_EFFORT" = "1"
            rm -f "$Inst"; return 0
        else
            die "no BOLT fragments for $Name"
        end
    end

    if not test -s "$Prof"
        rm -f "$Inst"
        if test "$BOLT_BEST_EFFORT" = "1"; return 0; else; die "no BOLT profile for $Name"; end
    end

    log "  [$Name] optimizing with ultimate flags..."
    if not "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" --dyno-stats --reorder-blocks=ext-tsp --reorder-functions=cdsort \
        --split-functions --split-strategy=cdsplit --split-all-cold --split-eh \
        --icf=safe --jump-tables=move --indirect-call-promotion=all --peepholes=all \
        --simplify-rodata-loads --x86-strip-redundant-address-size --strip-rep-ret --inline-memcpy \
        --plt=all --hugify --use-gnu-stack --update-debug-sections=0
        log "  [$Name] aggressive BOLT failed; retrying with safe core set..."
        run "$BOLT" "$Real" -o "$Opt" \
            --data "$Prof" --dyno-stats --reorder-blocks=ext-tsp --reorder-functions=cdsort \
            --split-functions --split-all-cold --split-eh --icf=safe --jump-tables=move --use-gnu-stack --update-debug-sections=0
    end

    mv -f "$Opt" "$Real"
    rm -f "$Inst" "$Prof"
end

function bolt_train_clang --argument-names Bin
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS -fno-lto -fuse-ld=lld -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$LLVM_SRC/llvm/lib/Support/regcomp.c" -o /dev/null 2>/dev/null
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
        'int main(int c,char**v){std::vector<std::string> s; for(int i=0;i<c;i++)s.push_back(v[i]);std::sort(s.begin(),s.end()); std::printf("%d %zu\n",f(s),s.size());return 0;}' >"$src"

    "$drv" -O2 -B "$lddir" -fuse-ld=lld -Wl,--gc-sections -Wl,--icf=all "$src" -o "$BUILD_ROOT/bolt-lld-train.out" 2>/dev/null
    "$drv" -O2 -B "$lddir" -fuse-ld=lld -static-libstdc++ -Wl,--gc-sections "$src" -o "$BUILD_ROOT/bolt-lld-train2.out" 2>/dev/null

    rm -f "$lddir/ld.lld" "$src" "$BUILD_ROOT/bolt-lld-train.out" "$BUILD_ROOT/bolt-lld-train2.out"
    return 0
end

if test -x "$BOLT"
    log ">>> STAGE 3: BOLT post-link (clang + ld.lld)..."
    bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"

    # Explicitly BOLT the actual ELF linker (ld.lld is what `clang -fuse-ld=lld` invokes at runtime).
    # We pass Name="lld" so the function uses the correct instrumentation mode for lld
    # (the sleep-time watcher + no append-pid, because lld exits via _exit() without flushing).
    # This also drives training via the bolt_train_lld logic that sets up ld.lld in -B.
    if test -e "$INSTALL_PREFIX/bin/ld.lld"
        bolt_optimize_binary lld "$INSTALL_PREFIX/bin/ld.lld"
    else if test -e "$INSTALL_PREFIX/bin/lld"
        bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
    end
end

# Final clean of bolt work dirs (fdata cleaned inside bolt_optimize)
rm -rf "$BUILD_ROOT/bolt"* 2>/dev/null || true

log "ULTIMATE build finished. Toolchain at: $INSTALL_PREFIX"
log "Source was fresh cloned, copied to /tmp ($LLVM_SRC), non-essential parts cleaned while preserving docs/bolt/polly."
log "stage2 (final LTO+PGO ThinLTO build) is deliberately preserved; only earlier stages cleaned."
log "VP_COUNTERS_PER_SITE=8 for both PGO and CSPGO; -DLLVM_TABLEGEN/-DCLANG_TABLEGEN from stage1 in stage2 (ProfileSummary fix)."
log "clang + ld.lld explicitly BOLTed (with lld-specific instrumentation + training + already-BOLTed skip for safe re-runs)."
log "BOLT stage is now robust: skips binaries already processed by BOLT (e.g. on partial runs or re-execution)."
log "To use: export PATH=$INSTALL_PREFIX/bin:\$PATH"
log "Verify: ls -l $INSTALL_PREFIX/bin/clang $INSTALL_PREFIX/bin/ld.lld ; perf stat -e L1-icache-misses -- $INSTALL_PREFIX/bin/clang++ -O3 -march=native -c test.cpp"
