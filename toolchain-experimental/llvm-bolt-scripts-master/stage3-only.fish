#!/usr/bin/env fish
# Standalone STAGE 3 (BOLT) runner — reuses the just-installed Stage 2 toolchain.
# Requires: $INSTALL_PREFIX/bin/{llvm-bolt,clang,clang++,lld} built by Stage 2
# with -Wl,--emit-relocs. Does NOT touch the Stage 1/2 build trees.

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

# --- Same config defaults as the main script (override via env if needed) -----
set -q TOPLEV;          or set -g TOPLEV          "$HOME/toolchain/llvm"
set -q LLVM_SRC;        or set -g LLVM_SRC        "$TOPLEV/llvm-project"
set -q INSTALL_PREFIX;  or set -g INSTALL_PREFIX  "$TOPLEV/llvm-bolt"
set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 0
set -g USER_NAME (id -un)
set -q BUILD_ROOT;      or set -g BUILD_ROOT      "/tmp/llvm-build-$USER_NAME"
set -g COMMON_FLAGS  "-O3 -march=native -mtune=native -fno-semantic-interposition -falign-functions=32 -fcf-protection=none -mharden-sls=none"

set -g BOLT "$INSTALL_PREFIX/bin/llvm-bolt"

# --- Pre-flight ---------------------------------------------------------------
test -x "$BOLT";                          or die "llvm-bolt not found at $BOLT (run Stage 2 first)"
test -e "$INSTALL_PREFIX/bin/clang";      or die "clang not found at $INSTALL_PREFIX/bin/clang"
test -d "$LLVM_SRC/llvm/lib/Support";     or die "LLVM source not found at $LLVM_SRC"
run mkdir -p "$BUILD_ROOT"

# Warn early if Stage 2 forgot --emit-relocs (BOLT needs relocations).
if not llvm-readelf -S (realpath "$INSTALL_PREFIX/bin/clang") 2>/dev/null | grep -q '\.rela\.text'
    log "WARNING: clang seems to lack .rela.text; Stage 2 must link with -Wl,--emit-relocs."
end

set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"

# clang exits normally (DT_FINI flush works) -> --instrumentation-file-append-pid
# so every process/fork writes its OWN file; merge fragments with merge-fdata.
# A single fixed file shared by multiple instrumented runs gets torn writes
# ("expected 0, 1 or 2" parse error). lld exits via _exit() (no DT_FINI flush),
# so it needs the sleep-time watcher instead and must train serially (one watcher
# at a time -> one file, no sharing).
function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt  "$Real.bolt"
    test -f "$Real"; or begin; log "  $Name: $Real missing, skipping."; return 0; end

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

log ">>> STAGE 3 (standalone): BOLT post-link optimization (clang + lld)..."
bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"
if test -e "$INSTALL_PREFIX/bin/lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
end
log "BOLT stage complete. Toolchain at: $INSTALL_PREFIX"
