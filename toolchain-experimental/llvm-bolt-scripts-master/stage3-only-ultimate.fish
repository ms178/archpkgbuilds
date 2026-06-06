#!/usr/bin/env fish

# Standalone STAGE 3 (BOLT) runner — resume/finish BOLT after a partial main build.
# Reuses the toolchain already installed by build-llvm-ultimate.fish (Stage 2).
# Safe to run multiple times: it will SKIP binaries that have already been BOLT-processed.
#
# Requirements:
# - Stage 2 must have succeeded and installed (with -Wl,--emit-relocs).
# - LLVM source tree for training (the main script leaves a cleaned copy in /tmp).

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

# --- Config (matches the main ultimate script) --------------------------------
set -q INSTALL_PREFIX;  or set -g INSTALL_PREFIX  "$HOME/toolchain/llvm-ultimate"
set -q BUILD_ROOT;      or set -g BUILD_ROOT      "/tmp/llvm-build-$USER"
set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 0

# Prefer the /tmp source copy left by the main script
set -q LLVM_SRC; or set -g LLVM_SRC "/tmp/llvm-project-src-$USER"
if not test -d "$LLVM_SRC/llvm/lib/Support"
    set -g LLVM_SRC "$HOME/toolchain/llvm/llvm-project"
end

set -g COMMON_FLAGS "-O3 -march=native -mtune=native -fno-semantic-interposition -falign-functions=32 -fcf-protection=none -mharden-sls=none -fno-plt"

set -g BOLT  "$INSTALL_PREFIX/bin/llvm-bolt"
set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"

# --- Pre-flight ---------------------------------------------------------------
test -x "$BOLT"; or die "llvm-bolt not found at $BOLT (run the main build through Stage 2 first)"

test -x "$INSTALL_PREFIX/bin/clang"; or die "clang not found at $INSTALL_PREFIX/bin/clang"

run mkdir -p "$BUILD_ROOT"

# Warn if relocations are missing (BOLT works much better with them)
set -l clang_real (realpath "$INSTALL_PREFIX/bin/clang")
if not llvm-readelf -S "$clang_real" 2>/dev/null | grep -q '\.rela\.text'
    log "WARNING: $clang_real appears to lack .rela.text sections."
    log "         Stage 2 must have been linked with -Wl,--emit-relocs."
end

# --- Helper: detect already-BOLTed binary (prevents "Cannot re-optimize") -----
function is_already_bolted --argument-names BinPath
    set -l f (realpath "$BinPath")
    test -f "$f"; or return 1

    # BOLT leaves these markers after processing
    nm -D "$f" 2>/dev/null | grep -q __bolt_runtime_start; and return 0
    strings "$f" 2>/dev/null | grep -q __bolt_runtime_start; and return 0
    strings "$f" 2>/dev/null | grep -q 'BOLT'; and return 0
    return 1
end

# --- BOLT functions (exact same logic as the main script) ---------------------

function bolt_optimize_binary --argument-names Name BinPath
    set -l Real (realpath "$BinPath")
    set -l Prof "$BUILD_ROOT/$Name.bolt.fdata"
    set -l Inst "$Real.inst"
    set -l Opt  "$Real.bolt"

    test -f "$Real"; or begin; log "  $Name: $Real missing, skipping."; return 0; end

    # Skip if already BOLTed (this is the key fix for your situation)
    if is_already_bolted "$Real"
        log "  [$Name] already BOLT-processed, skipping."
        return 0
    end

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
        # Wait for the watcher's final on-death dump (lld uses _exit)
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
            log "  [$Name] WARNING: no profile fragments; leaving binary unoptimized."
            rm -f "$Inst"; return 0
        else
            die "[$Name] no BOLT profile fragments produced."
        end
    end

    if not test -s "$Prof"
        rm -f "$Inst"
        if test "$BOLT_BEST_EFFORT" = "1"
            log "  [$Name] WARNING: no profile produced; leaving binary unoptimized."
            return 0
        end
        die "[$Name] no BOLT profile produced."
    end

    log "  [$Name] optimizing with ultimate flags..."
    if not "$BOLT" "$Real" -o "$Opt" \
        --data "$Prof" \
        --dyno-stats \
        --reorder-blocks=ext-tsp \
        --reorder-functions=cdsort \
        --split-functions --split-strategy=cdsplit --split-all-cold --split-eh \
        --icf=safe --jump-tables=move --indirect-call-promotion=all --peepholes=all \
        --simplify-rodata-loads --x86-strip-redundant-address-size --strip-rep-ret --inline-memcpy \
        --plt=all --hugify --use-gnu-stack --update-debug-sections=0
        log "  [$Name] aggressive BOLT failed; retrying with safe core set..."
        run "$BOLT" "$Real" -o "$Opt" \
            --data "$Prof" \
            --dyno-stats \
            --reorder-blocks=ext-tsp \
            --reorder-functions=cdsort \
            --split-functions --split-all-cold --split-eh \
            --icf=safe --jump-tables=move --use-gnu-stack --update-debug-sections=0
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

# --- Main ---------------------------------------------------------------------
log ">>> STAGE 3 (standalone): BOLT post-link (clang + ld.lld)..."

bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"

if test -e "$INSTALL_PREFIX/bin/ld.lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/ld.lld"
else if test -e "$INSTALL_PREFIX/bin/lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
end

log "STAGE 3 BOLT complete (skipped already-BOLTed binaries)."
log "Toolchain at: $INSTALL_PREFIX"
log "export PATH=$INSTALL_PREFIX/bin:\$PATH"
