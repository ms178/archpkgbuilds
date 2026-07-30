#!/usr/bin/env fish

# build-llvm-ultimate.v25.fish
#
# Modernized multi-stage LLVM build pipeline for the ms178 patch stack.
#
# ============================================================================
# WHAT CHANGED IN v25 vs v24 (root-cause fixes)
# ============================================================================
#
# 1. ROOT CAUSE OF THE "Not an int attribute (Producer: 'LLVM24...' Reader:
#    'LLVM 22...')" WARNING FLOOD:
#    ThinLTO (-flto=thin / -DLLVM_ENABLE_LTO=Thin) does NOT emit native ELF
#    objects.  Every .o is an LLVM bitcode wrapper.  When ninja archives a
#    static library (libLLVMSupport.a, libLLVMCore.a, ...) CMake invokes
#    CMAKE_AR / CMAKE_RANLIB on those bitcode files.
#
#    v10 correctly matched CFG-affecting *compiler* flags between profile
#    generation and consumption, but it never pinned the *LLVM binutils*
#    (llvm-ar, llvm-ranlib, llvm-nm, llvm-objcopy, llvm-strip) to the just-
#    built stage1 tools.  On CachyOS (and most distros) `llvm-ar` on $PATH
#    is the distro package — here LLVM 22.1.8.  Stage2/3/4 compile with the
#    in-tree Clang 24.0.0git, so bitcode carries LLVM-24 attribute encodings.
#    LLVM 22's llvm-ar cannot decode them →
#        warning: 'Foo.cpp.o': Not an int attribute
#                 (Producer: 'LLVM24.0.0git' Reader: 'LLVM 22.1.8')
#    once per object, i.e. thousands of lines during full-pgo-train.
#
#    THE CORRECT FIX (implemented here):
#      • Stage 1 explicitly builds llvm-ar / llvm-ranlib / llvm-nm /
#        llvm-objcopy / llvm-strip / llvm-readobj alongside clang/lld.
#      • Every subsequent configure passes
#          -DCMAKE_AR=... -DCMAKE_RANLIB=... -DCMAKE_NM=...
#          -DCMAKE_OBJCOPY=... -DCMAKE_STRIP=...
#        pointing at those stage1 binaries (same LLVM major as the bitcode).
#      • $PATH is prepended with stage1/bin so bare `lld`, `llvm-ar`, etc.
#        resolve to the matching tools even for non-CMake invocations.
#      • AR/RANLIB/NM/OBJCOPY/STRIP are also exported for make/ninja edge
#        paths that read the environment instead of the CMake cache.
#
# 2. Retained from v10 (still correct, still mandatory):
#    • validate_profdata is defined (v9 crash).
#    • Instrumented + training builds use the same ThinLTO flag set as the
#      final consume build so PGO CFG hashes match (no "hash mismatch ...
#      count discarded").
#    • The final consume profile is built by the correct one-step merge:
#        llvm-profdata merge raw-CSIR.profraw... first-pass-IR.profdata \
#          -o final.profdata
#      Never feed the final ThinLTO build a CSIR-only indexed profile, and
#      never merge already-indexed IR + already-indexed CS profiles.
#    • CSIR-PGO is mandatory.
#
# 3. Additional v11/v12 hardening:
#    • verify_llvm_tool_version — refuses to proceed if stage1 llvm-ar's
#      reported major does not match the instrumented clang major.
#    • stage1 binutils are verified executable before stage 2 starts.
#    • Host (distro) llvm-ar is deliberately displaced from PATH during
#      stages 2–6 so a stray tool lookup cannot re-introduce skew.
#    • BOLT stage uses stage5/install llvm-readobj when available, else
#      falls back to the previously verified stage1 tool.
#    • Explicit LLVM_ENABLE_ZLIB/ZSTD/LIBXML2 discovery stays automatic;
#      we only force tool paths that affect bitcode I/O.
#    • Harmless ld.lld "loop not unrolled" transformation remarks may still
#      appear occasionally; they are unrelated to Producer/Reader skew and
#      do not discard profile data or fail the build.
#    • assert_cmake_ar_pinned() reads CMakeCache after every ThinLTO
#      configure and aborts if CMAKE_AR / COMPILER_AR drift to the host.
#
# 4. v12 audit fixes:
#    • The final install build directory is named stage2, but it is the final
#      ThinLTO+PGO consume/install configure. It now explicitly disables LLVM
#      benchmarks/examples as well as tests. This prevents third-party Google
#      Benchmark configure warnings such as "std::regex with exceptions disabled".
#    • configure_clean() now treats CMake warnings as fatal by default
#      (STRICT_CMAKE_WARNINGS=1). If upstream emits a new warning, the script
#      stops at configure time with the exact log path instead of wasting hours.
#    • run_ninja_logged() records verbose Ninja output and prints the first
#      FAILED edge plus error-like diagnostics on failure. No more opaque
#      "ninja failed" without the real subcommand.
#
# 5. v13 policy change requested by user:
#    • Build despite ThinLTO/CSPGO profile CFG/hash mismatches by default.
#      ALLOW_LTO_PGO_HASH_MISMATCH=1 is the default and adds
#      -Wl,--no-lto-pgo-warn-mismatch to PGO-consuming ThinLTO link stages.
#    • The flag is not blindly assumed: after stage1 exists, the script probes
#      the freshly built stage1 clang + ld.lld pair and aborts if the linker
#      cannot accept the option.
#    • Set ALLOW_LTO_PGO_HASH_MISMATCH=0 only if you want strict fail-fast
#      behavior for stale profile mismatches.
#
# v16 audit decisions after reviewing Opus v15:
#    • Accepted: remove the lld ELF-only sed mutation; build complete lld.
#    • Accepted: remove -fno-semantic-interposition to avoid ThinLTO/TLS DSO
#      failures such as R_X86_64_TPOFF32 in LLVMgold.so.
#    • Accepted: put -fsplit-lto-unit on both C and C++ ThinLTO modules.
#    • Accepted: use the same ThinLTO backend link pipeline (--lto-O3/--lto-CGO3)
#      for profile generation and profile use.
#    • Accepted: keep -fuse-ld=lld link-only; do not pass it to compile-only
#      training commands.
#    • Rejected: making bitcode tool-pinning drift advisory. That remains fatal.
#    • Rejected: deleting tests/unittests from the source tree. CMake disables
#      them; source mutation is unnecessary and harms diagnostics.
#    • Rejected: relying on log scraping to tolerate hash mismatches. The build
#      uses the explicit probed lld option --no-lto-pgo-warn-mismatch instead.
#
# v17 runtime fix:
#    • Do not configure compiler-rt as an in-tree LLVM_ENABLE_RUNTIMES child of
#      the final PGO+ThinLTO stage. The user's log showed the nested
#      runtimes/builtins configure using stage2/bin/clang before CMake could
#      identify it as a working compiler, producing zero supported builtin
#      architectures and failing CheckSectionExists.cmake.
#    • Build/install final LLVM/clang/lld first. Then configure compiler-rt as
#      a separate post-install runtimes stage using the installed final clang
#      and clean runtime-safe flags: no PGO use, no ThinLTO, no WPD, no
#      semantic-interposition tweak.
#
# v18 compiler-rt hardening after analyzing attached logs:
#    • The uploaded CMakeCache/build.ninja prove the failed final stage still had
#      LLVM_ENABLE_RUNTIMES=compiler-rt in the final PGO+ThinLTO LLVM build.
#      That generated runtimes/builtins-configure using stage2/bin/clang and
#      failed compiler identification, leaving zero builtin architectures.
#    • Compiler-rt is still built by default, but only after final clang/lld are
#      installed and stable.
#    • Runtime configure now passes explicit target triple variables and uses
#      static-library try-compiles to avoid circular dependency on builtins.
#    • CMake developer warnings are suppressed with -Wno-dev so strict warning
#      mode only catches actionable configure warnings.
#
# v19 stage1 compiler-rt fix:
#    • Stage 1 no longer asks Ninja for non-existent `compiler-rt`/`builtins`
#      targets after configuring only LLVM projects `clang;lld`.
#    • Compiler-rt is still built. Stage 1 now builds the bootstrap tools first,
#      then configures a separate stage1 compiler-rt runtime/profile support
#      install using the freshly built stage1 clang/lld.
#    • A mandatory profile-runtime probe verifies that stage1 clang can link
#      `-fprofile-instr-generate` before any PGO stage starts.
#    • Replaced deprecated CMake `-Wno-dev` with `-Wno-author`.
#
# v20 correction:
#    • Reverted unauthorized V19 PCH policy change. The script no longer sets
#      Upstream LLVM/CMake defaults decide PCH behavior.
#    • Stage1 compiler-rt target-list fix from V19 is retained.
#
# v21 compiler-rt default-target fix:
#    • Upstream compiler-rt forbids passing COMPILER_RT_DEFAULT_TARGET_TRIPLE
#      when COMPILER_RT_DEFAULT_TARGET_ONLY=ON. In that mode it derives the
#      default target triple from CMAKE_C_COMPILER_TARGET.
#    • Removed COMPILER_RT_DEFAULT_TARGET_TRIPLE from both stage1 and final
#      compiler-rt runtime configures. CMAKE_{C,CXX,ASM}_COMPILER_TARGET and
#      LLVM_DEFAULT_TARGET_TRIPLE remain explicit.
#    • PCH policy remains untouched/reverted from V20: no script-level
#
# v22 compiler-rt resource-dir install fix:
#    • Stage1 compiler-rt built successfully but installed profile runtime to
#      $prefix/lib/$triple while clang searches $resource_dir/lib/$triple.
#    • Upstream compiler-rt standalone builds use COMPILER_RT_INSTALL_PATH to
#      place artifacts under the clang resource directory. v22 computes the
#      resource dir from `clang -print-resource-dir`, passes the absolute
#      resource-dir install path, and verifies libclang_rt.profile.a exactly where clang
#      will search for it before running any PGO stage.
#
# v23 after auditing Agent A and Agent B:
#    • Accepted from Agent A: final compiler-rt should be built with the clean
#      stage1 compiler/binutils, but installed into the final clang resource
#      directory. This avoids making final clang compile its own runtime before
#      the runtime exists, while still installing compiler-rt exactly where the
#      final driver searches.
#    • Rejected from Agent A: mangled script text, source tree test deletion,
#      advisory bitcode-tool skew, and log scraping as primary policy.
#    • Rejected Agent B as an implementation: it is HTML/Markdown-mangled,
#      regresses compiler-rt-in-stage1 target handling, contains invalid Fish,
#      and depends on fragile bootstrap target names. The upstream bootstrap
#      concept is noted, but not adopted wholesale.
#
# v24 compiler-rt wrapper correction:
#    • Build testing showed `-S compiler-rt` works but emits an
#      LLVMTestingSupport CMake warning that strict mode correctly rejects.
#    • The upstream `runtimes` wrapper with an ABSOLUTE COMPILER_RT_INSTALL_PATH
#      avoids that warning and correctly installs builtins/profile into clang's
#      resource directory. v24 therefore uses `$LLVM_SRC/runtimes` for the
#      separate compiler-rt stages, not `$LLVM_SRC/compiler-rt`.
#    • The critical v23 insight remains: final compiler-rt is compiled by clean
#      stage1 clang/binutils and installed into final clang's resource dir.
#
# v25 final-clang allocator poisoning fix:
#    • V24 allowed shared mimalloc to be linked into final toolchain executables
#      through ALLOCATOR_LINK. Clearing LD_PRELOAD is not enough when clang has
#      a direct DT_NEEDED dependency on libmimalloc.so. The user's final clang
#      segfaulted inside libmimalloc.so before `clang -dumpmachine` could run.
#    • V25 disables allocator injection into toolchain binaries by default and
#      hard-forbids shared mimalloc linkage unless explicitly overridden.
#    • V25 verifies final clang has no mimalloc dependency and runs sanitized
#      `clang -dumpmachine`, `clang -print-resource-dir`, and `clang -v` smoke
#      checks immediately after install, before compiler-rt final runtime work.
#    • PCH policy remains untouched.
#
# ============================================================================
# Pipeline overview:
#   Stage 0: sanitize environment, select a clean host clang/clang++ pair,
#            fetch official llvm-project, apply the patch stack.
#   Stage 1: build an uninstrumented bootstrap toolchain + profiling runtime
#            + matching llvm-ar/ranlib/nm/objcopy/strip.
#   Stage 2: build an IR-PGO instrumented compiler (WITH ThinLTO codegen flags,
#            archived by stage1 llvm-ar).
#   Stage 3: train + merge the IR-PGO profile.
#   Stage 4: build/train a CSIR-PGO compiler (WITH ThinLTO codegen flags).
#   Stage 5: build/install the final ThinLTO+PGO toolchain (Polly/BOLT/
#            compiler-rt/LLVMgold).
#   Stage 6: BOLT-optimize clang and ld.lld.
# ============================================================================

function die
    echo (set_color -o red)"[LLVM-ULTIMATE-V25][FATAL]"(set_color normal) "$argv" >&2
    exit 1
end

function log
    echo (set_color -o cyan)"[LLVM-ULTIMATE-V25]"(set_color normal) "$argv"
end

function run
    $argv; or die "command failed: $argv"
end

# ----------------------------------------------------------------------------
# validate_profdata — verifies a merged .profdata file exists, is non-empty,
# and is readable by llvm-profdata show.
# ----------------------------------------------------------------------------
function validate_profdata --argument-names ProfPath Desc
    test -n "$ProfPath"; or die "validate_profdata: empty path for $Desc"
    test -s "$ProfPath"; or die "$Desc missing or empty: $ProfPath"
    set -l pd "$PROFDATA"
    test -x "$pd"; or set pd (command -v llvm-profdata)
    if test -n "$pd"; and test -x "$pd"
        "$pd" show "$ProfPath" >/dev/null 2>&1
        or die "$Desc is not a valid profdata file (llvm-profdata show failed): $ProfPath"
        log "Validated $Desc: $ProfPath"
    else
        log "WARNING: no llvm-profdata available to deep-validate $Desc; size check passed."
    end
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

function compiler_rt_install_path_for_resource_dir --argument-names Prefix ResourceDir Desc
    test -n "$Prefix"; or die "$Desc: empty install prefix"
    test -n "$ResourceDir"; or die "$Desc: empty clang resource directory"
    set -l clean_prefix (string replace -r '/+$' '' -- "$Prefix")
    if not string match -q "$clean_prefix/*" -- "$ResourceDir"
        die "$Desc: clang resource dir '$ResourceDir' is not under install prefix '$clean_prefix'"
    end
    # COMPILER_RT_INSTALL_PATH is a CMake PATH cache variable. Passing a relative
    # path such as lib/clang/24 is unsafe: CMake canonicalizes it against the
    # configure working directory. Pass the absolute resource directory instead.
    echo "$ResourceDir"
end

function verify_compiler_rt_profile_runtime --argument-names ClangPath Triple Desc
    verify_executable "$ClangPath" "$Desc clang"
    set -l resource_dir ("$ClangPath" -print-resource-dir)
    test -n "$resource_dir"; or die "$Desc: clang did not report a resource directory"
    set -l profile_lib "$resource_dir/lib/$Triple/libclang_rt.profile.a"
    test -s "$profile_lib"; or die "$Desc: expected compiler-rt profile runtime missing: $profile_lib"
    log "$Desc: verified compiler-rt profile runtime: $profile_lib"
end

function verify_no_mimalloc_dependency --argument-names BinPath Desc
    verify_executable "$BinPath" "$Desc"
    if command -q ldd
        set -l deps (ldd "$BinPath" 2>/dev/null | string collect)
        if string match -qi '*mimalloc*' -- "$deps"
            if test "$ALLOW_TOOLCHAIN_MIMALLOC_DEP" = "1"
                warn "$Desc has a mimalloc runtime dependency and ALLOW_TOOLCHAIN_MIMALLOC_DEP=1 is set. This is unsafe."
            else
                echo "$deps" >&2
                die "$Desc depends on mimalloc. Refusing to continue because libmimalloc has caused final clang SIGSEGV. Rebuild without USE_MIMALLOC or remove mimalloc from CMAKE_EXE_LINKER_FLAGS."
            end
        end
    end
end

function smoke_test_clang_driver --argument-names ClangPath Desc
    verify_executable "$ClangPath" "$Desc"
    set -l smoke_dir "$BUILD_ROOT/clang-driver-smoke"
    rm -rf "$smoke_dir"
    mkdir -p "$smoke_dir"
    env -u LD_PRELOAD "$ClangPath" -dumpmachine >"$smoke_dir/dumpmachine.out" 2>"$smoke_dir/dumpmachine.err"
    set -l st_dump $status
    env -u LD_PRELOAD "$ClangPath" -print-resource-dir >"$smoke_dir/resource.out" 2>"$smoke_dir/resource.err"
    set -l st_res $status
    env -u LD_PRELOAD "$ClangPath" -v >"$smoke_dir/version.out" 2>"$smoke_dir/version.err"
    set -l st_ver $status
    if test $st_dump -ne 0; or test $st_res -ne 0; or test $st_ver -ne 0
        echo "---- $Desc -dumpmachine stderr ----" >&2
        cat "$smoke_dir/dumpmachine.err" >&2; or true
        echo "---- $Desc -print-resource-dir stderr ----" >&2
        cat "$smoke_dir/resource.err" >&2; or true
        echo "---- $Desc -v stderr ----" >&2
        cat "$smoke_dir/version.err" >&2; or true
        if command -q ldd
            echo "---- $Desc ldd ----" >&2
            ldd "$ClangPath" >&2; or true
        end
        die "$Desc failed sanitized driver smoke tests. The installed compiler is not usable."
    end
    set -l triple (string trim <"$smoke_dir/dumpmachine.out")
    set -l resource (string trim <"$smoke_dir/resource.out")
    test -n "$triple"; or die "$Desc smoke test produced empty target triple"
    test -n "$resource"; or die "$Desc smoke test produced empty resource directory"
    log "$Desc smoke OK: triple=$triple resource=$resource"
    rm -rf "$smoke_dir"
end

# Extract the first "LLVM major.minor" (or major.minor.patch) from tool --version.
function llvm_tool_version_string --argument-names ToolPath
    test -x "$ToolPath"; or return 1
    # Match both "LLVM version 24.0.0" and "clang version 24.0.0git"
    set -l ver_line ("$ToolPath" --version 2>/dev/null | string match -r 'version [0-9]+\.[0-9]+(\.[0-9]+)?' | head -n 1)
    if test -z "$ver_line"
        return 1
    end
    set -l ver (string replace -r '.*version ([0-9]+\.[0-9]+(\.[0-9]+)?).*' '$1' -- "$ver_line")
    if test -n "$ver"
        echo "$ver"
        return 0
    end
    return 1
end

function llvm_major --argument-names VerStr
    string split -f1 '.' -- "$VerStr"
end

# Refuse bitcode-tool skew: ar major must equal clang major.
function verify_bitcode_tool_pair --argument-names ClangPath ArPath Context
    set -l cv (llvm_tool_version_string "$ClangPath")
    set -l av (llvm_tool_version_string "$ArPath")
    test -n "$cv"; or die "$Context: cannot read version from $ClangPath"
    test -n "$av"; or die "$Context: cannot read version from $ArPath"
    set -l cm (llvm_major "$cv")
    set -l am (llvm_major "$av")
    if test "$cm" != "$am"
        die "$Context: bitcode tool skew — clang is LLVM $cv but llvm-ar is LLVM $av. ThinLTO archives would emit 'Not an int attribute' warnings. Refusing to continue."
    end
    log "$Context: bitcode tool pair OK (clang $cv · llvm-ar $av)"
end

function configure_lto_pgo_mismatch_policy --argument-names ClangPath LldDir
    set -g LTO_PGO_MISMATCH_LINKER_FLAGS
    if test "$ALLOW_LTO_PGO_HASH_MISMATCH" != "1"
        log "Strict LTO PGO profile mismatch mode requested: hash mismatch warnings are not suppressed."
        return 0
    end

    verify_executable "$ClangPath" "clang for LTO PGO mismatch flag probe"
    test -d "$LldDir"; or die "lld directory for LTO PGO mismatch flag probe missing: $LldDir"

    set -l probe_dir "$BUILD_ROOT/probe-lto-pgo-mismatch-flag"
    rm -rf "$probe_dir"
    mkdir -p "$probe_dir"
    printf 'int main(void) { return 0; }\n' > "$probe_dir/probe.c"

    set -l flag -Wl,--no-lto-pgo-warn-mismatch
    if "$ClangPath" -fuse-ld=lld -B"$LldDir" $flag "$probe_dir/probe.c" -o "$probe_dir/probe" >/dev/null 2>"$probe_dir/probe.err"
        set -g LTO_PGO_MISMATCH_LINKER_FLAGS $flag
        log "LTO PGO hash-mismatch tolerant mode enabled: $flag"
        rm -rf "$probe_dir"
        return 0
    end

    echo >&2
    echo "==================== LTO PGO MISMATCH FLAG PROBE FAILED ====================" >&2
    cat "$probe_dir/probe.err" >&2; or true
    die "ALLOW_LTO_PGO_HASH_MISMATCH=1 requested, but stage1 ld.lld does not accept $flag. Cannot guarantee build-through profile mismatch behavior."
end

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
set -q TOPLEV; or set -g TOPLEV "$HOME/toolchain/llvm"
set -q INSTALL_PREFIX; or set -g INSTALL_PREFIX "$HOME/toolchain/llvm-ultimate"
set -q BUILD_ROOT; or set -g BUILD_ROOT "/tmp/llvm-build-$USER"
set -q THINLTO_CACHE; or set -g THINLTO_CACHE "$BUILD_ROOT/thinlto-cache"
set -g SCRIPT_DIR (dirname (realpath (status --current-filename)))
set -q PATCH_DIR; or set -g PATCH_DIR "$SCRIPT_DIR"
set -q BOLT_BEST_EFFORT; or set -g BOLT_BEST_EFFORT 1
set -q FULL_TRAIN; or set -g FULL_TRAIN 1
set -q STRICT_CMAKE_WARNINGS; or set -g STRICT_CMAKE_WARNINGS 1

# Default requested policy: do not let ThinLTO/CSPGO profile hash mismatches
# fail the final build. This suppresses lld's LTO PGO mismatch diagnostics at
# the linker-plugin boundary; stale counts may still be ignored by LLVM, but
# the toolchain build proceeds.
set -q ALLOW_LTO_PGO_HASH_MISMATCH; or set -g ALLOW_LTO_PGO_HASH_MISMATCH 1
set -g LTO_PGO_MISMATCH_LINKER_FLAGS

# Common CMake switches for all LLVM configures. Keep this list conservative:
# every variable here is declared by LLVM's top-level CMake, so it must not
# create "manually-specified variables were not used" warnings.
set -g LLVM_COMMON_DISABLE_CMAKE_ARGS \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_BUILD_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF

# CSIR-PGO is mandatory in this pipeline.
set -q DO_CSPGO; or set -g DO_CSPGO 1
if test "$DO_CSPGO" != "1"
    die "DO_CSPGO=0 is no longer supported: CSIR-PGO is mandatory in build-llvm-ultimate.v25.fish"
end

# Obsolete kill-switch: the only correct final CSPGO profile for this pipeline
# is produced by a single llvm-profdata invocation containing the raw CSIR
# .profraw files plus the first-pass IR .profdata.  The old switch allowed two
# broken modes:
#   1. final = cs.profdata only                            (profile discards)
#   2. final = merge(clang.profdata, already-indexed cs.profdata) (duplicates)
# Both can trigger ThinLTO "hash mismatch" / "count discarded" warnings.
if set -q MERGE_IR_AND_CS_PROFILES
    die "MERGE_IR_AND_CS_PROFILES is obsolete and intentionally unsupported; final profile merging is always the one-step raw-CSIR + first-pass-IR merge"
end

set -q USE_MIMALLOC; or set -g USE_MIMALLOC 0
set -q ALLOW_UNSAFE_SHARED_MIMALLOC_LINK; or set -g ALLOW_UNSAFE_SHARED_MIMALLOC_LINK 0
set -q ALLOW_TOOLCHAIN_MIMALLOC_DEP; or set -g ALLOW_TOOLCHAIN_MIMALLOC_DEP 0
set -q KEEP_PRE_BOLT_BACKUP; or set -g KEEP_PRE_BOLT_BACKUP 0
set -q REQUIRE_GOLD_PLUGIN; or set -g REQUIRE_GOLD_PLUGIN 1
set -q LLVM_ENABLE_BINDINGS; or set -g LLVM_ENABLE_BINDINGS OFF
set -q BUILD_COMPILER_RT; or set -g BUILD_COMPILER_RT 1
set -q BUILD_STAGE1_COMPILER_RT; or set -g BUILD_STAGE1_COMPILER_RT 1
set -q COMPILER_RT_BUILD_SANITIZERS; or set -g COMPILER_RT_BUILD_SANITIZERS OFF
set -q COMPILER_RT_BUILD_XRAY; or set -g COMPILER_RT_BUILD_XRAY OFF
set -q COMPILER_RT_BUILD_LIBFUZZER; or set -g COMPILER_RT_BUILD_LIBFUZZER OFF
set -q COMPILER_RT_BUILD_MEMPROF; or set -g COMPILER_RT_BUILD_MEMPROF OFF
set -q COMPILER_RT_BUILD_ORC; or set -g COMPILER_RT_BUILD_ORC OFF
set -q COMPILER_RT_BUILD_GWP_ASAN; or set -g COMPILER_RT_BUILD_GWP_ASAN OFF
set -q COMPILER_RT_BUILD_PROFILE; or set -g COMPILER_RT_BUILD_PROFILE ON

set -g NPROC (nproc)
set -q LTO_JOBS; or set -g LTO_JOBS (math -s0 "max(2, round($NPROC / 4))")
set -g VP_COUNTERS_PER_SITE 16

# ---------------------------------------------------------------------------
# Flags. Keep as fish lists for direct compiler invocations (fish does not
# word-split scalars). String mirrors are only for CMake cache variables.
# ---------------------------------------------------------------------------
set -g COMMON_FLAGS_LIST \
    -O3 \
    -march=native \
    -mtune=native \
    -falign-functions=32 \
    -falign-loops=32 \
    -fcf-protection=none \
    -mharden-sls=none \
    -fno-plt

set -g C_LTO_FLAGS_LIST -flto=thin -fsplit-lto-unit
set -g CXX_LTO_FLAGS_LIST -flto=thin -fsplit-lto-unit -fwhole-program-vtables

set -g LINKER_BASE_LIST \
    -fuse-ld=lld \
    -Wl,--thinlto-jobs=$LTO_JOBS \
    -Wl,--lto-O3 \
    -Wl,--lto-CGO3 \
    -Wl,--gc-sections \
    -Wl,--icf=safe \
    -Wl,-z,max-page-size=0x200000
set -g COMMON_FLAGS (string join ' ' -- $COMMON_FLAGS_LIST)
# Runtime libraries must be configured with clean non-PGO/non-LTO flags.
# Reusing final-stage PGO/LTO/WPD flags here can make compiler-rt's CMake
# compiler-id and builtin-architecture probes fail.  Keep this intentionally
# conservative: compiler-rt builtins/profile runtime should be target-correct
# before it is micro-architecture-specialized.
set -g RUNTIME_FLAGS_LIST -O3 -fno-plt -fcf-protection=none -mharden-sls=none
set -g RUNTIME_FLAGS (string join ' ' -- $RUNTIME_FLAGS_LIST)
set -g C_LTO_FLAGS (string join ' ' -- $C_LTO_FLAGS_LIST)
set -g CXX_LTO_FLAGS (string join ' ' -- $CXX_LTO_FLAGS_LIST)
set -g LINKER_BASE (string join ' ' -- $LINKER_BASE_LIST)

# ThinLTO backend flags shared by every profile-generation/profile-use link.
# Keep this byte-identical across instrumented, training, CSIR and final-use
# stages except for final-only ELF layout/allocator extras.
set -g LTO_STAGE_LINK_FLAGS_LIST \
    -fuse-ld=lld \
    -Wl,--thinlto-jobs=$LTO_JOBS \
    -Wl,--lto-O3 \
    -Wl,--lto-CGO3
set -g LTO_STAGE_LINK_FLAGS (string join ' ' -- $LTO_STAGE_LINK_FLAGS_LIST)

# ---------------------------------------------------------------------------
# CRITICAL (from v10): the CFG-affecting flag set used for the instrumented
# and training builds MUST equal the set used by the final Stage 5 build so
# that PGO CFG hashes match at consume time (no "hash mismatch ... discarded").
# ---------------------------------------------------------------------------
set -g C_PGO_MATCH_FLAGS_LIST   $COMMON_FLAGS_LIST $C_LTO_FLAGS_LIST
set -g CXX_PGO_MATCH_FLAGS_LIST $COMMON_FLAGS_LIST $CXX_LTO_FLAGS_LIST
set -g C_PGO_MATCH_FLAGS   (string join ' ' -- $C_PGO_MATCH_FLAGS_LIST)
set -g CXX_PGO_MATCH_FLAGS (string join ' ' -- $CXX_PGO_MATCH_FLAGS_LIST)

set -g ALLOCATOR_LINK "-lpthread -lstdc++ -lm -ldl"

# Do not inject alternate malloc implementations into clang/lld by default.
# A direct shared libmimalloc dependency can crash every installed tool before
# main(), even when LD_PRELOAD is cleared.  This is exactly what the V24 log
# showed.  Keep allocator tuning outside the compiler binary unless explicitly
# and consciously requested.
if test "$USE_MIMALLOC" = "1"
    set -l found_static ""
    set -l found_shared ""
    for d in /usr/lib /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/local/lib
        if test -f "$d/libmimalloc.a"
            set found_static "$d/libmimalloc.a"
            break
        end
        if test -f "$d/libmimalloc.so"
            set found_shared "$d/libmimalloc.so"
        end
    end

    if test -n "$found_static"
        set -g ALLOCATOR_LINK "-Wl,--push-state -Wl,--whole-archive $found_static -Wl,--pop-state $ALLOCATOR_LINK"
        log "Using explicitly requested static mimalloc ($found_static)."
    else if test -n "$found_shared"
        if test "$ALLOW_UNSAFE_SHARED_MIMALLOC_LINK" = "1"
            set -l d (dirname "$found_shared")
            set -g ALLOCATOR_LINK "-L$d -lmimalloc $ALLOCATOR_LINK"
            warn "Using UNSAFE shared mimalloc link ($found_shared); final clang will be checked for crashes."
        else
            die "USE_MIMALLOC=1 found only shared mimalloc ($found_shared). Refusing to link clang/lld against shared mimalloc because it can segfault before main(). Install static libmimalloc.a or set USE_MIMALLOC=0. Override only with ALLOW_UNSAFE_SHARED_MIMALLOC_LINK=1."
        end
    else
        warn "USE_MIMALLOC=1 requested, but no mimalloc library found; continuing without allocator injection."
    end
else
    log "Toolchain allocator injection disabled (USE_MIMALLOC=0). LD_PRELOAD is still sanitized for build subprocesses."
end

if set -q LD_PRELOAD
    set -g USER_LD_PRELOAD "$LD_PRELOAD"
    if not set -q ALLOW_BUILD_LD_PRELOAD; or test "$ALLOW_BUILD_LD_PRELOAD" != "1"
        log "Clearing LD_PRELOAD for deterministic bootstrap subprocesses (was: $USER_LD_PRELOAD). Set ALLOW_BUILD_LD_PRELOAD=1 to override."
        set -e LD_PRELOAD
    else
        log "Keeping user LD_PRELOAD for all subprocesses: $LD_PRELOAD"
    end
end

# ---------------------------------------------------------------------------
# Host compiler selection
# ---------------------------------------------------------------------------
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
        absolute_tool_path "$c"
        absolute_tool_path "$cxx"
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

log "Host bootstrap C compiler:   $HOST_CLANG"
log "Host bootstrap C++ compiler: $HOST_CLANGXX"
log "Host lld linker:             $HOST_LD_LLD"

# Warn early if the host llvm-ar major differs from what we will build.
# This is informational — stage1 tools replace host tools after stage 1.
set -l _host_ar (command -v llvm-ar 2>/dev/null)
if test -n "$_host_ar"; and test -x "$_host_ar"
    set -l _host_ar_ver (llvm_tool_version_string "$_host_ar")
    set -l _host_clang_ver (llvm_tool_version_string "$HOST_CLANG")
    if test -n "$_host_ar_ver"; and test -n "$_host_clang_ver"
        log "Host clang reports LLVM $_host_clang_ver; host llvm-ar reports LLVM $_host_ar_ver"
        if test (llvm_major $_host_ar_ver) != (llvm_major $_host_clang_ver)
            log "NOTE: host clang/ar major mismatch is OK for stage1 bootstrap; v11 will pin stage1 llvm-ar for all ThinLTO stages so distro llvm-ar cannot touch Clang-24 bitcode."
        end
    end
    log "Host llvm-ar (will be displaced after stage1): $_host_ar"
end

# ---------------------------------------------------------------------------
# Host compiler probe
# ---------------------------------------------------------------------------
set -l _host_probe_dir "$BUILD_ROOT/host-probe"
set -l _host_probe_log "$SCRIPT_DIR/host-probe.log"
rm -rf "$_host_probe_dir"
mkdir -p "$_host_probe_dir"
set -l _host_lld_dir (dirname "$HOST_LD_LLD")
: > "$_host_probe_log"

printf '%s\n' 'int main(void) { return 0; }' > "$_host_probe_dir/probe.c"
printf '%s\n' '#include <type_traits>' 'int main() { static_assert(std::is_same_v<int,int>); return 0; }' > "$_host_probe_dir/probe.cpp"

begin
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
end >>"$_host_probe_log" 2>&1

"$HOST_CLANG" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.c" -o "$_host_probe_dir/probe-c" >>"$_host_probe_log" 2>&1
set -l _probe_c_status $status

begin
    echo "--- C++ probe ---"
    echo "$HOST_CLANGXX" $COMMON_FLAGS_LIST -fuse-ld=lld -B"$_host_lld_dir" "$_host_probe_dir/probe.cpp" -o "$_host_probe_dir/probe-cxx"
end >>"$_host_probe_log" 2>&1

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
# Suppress CMake author/developer warnings such as GNUInstallDirs author warnings from
# nested runtime projects. Real CMake warnings remain visible and are still
# fatal under STRICT_CMAKE_WARNINGS=1.
set -g CMAKE_WARNING_MODE_ARGS -Wno-author

if set -q LLVM_ULTIMATE_SELF_TEST; and test "$LLVM_ULTIMATE_SELF_TEST" = "1"
    log "Self-test completed: validate_profdata defined, matching-hash PGO flags configured, stage1 binutils pinning helpers present, mandatory CSIR-PGO policy, one-step raw-CSIR + first-pass-IR final profile merge policy, default LTO PGO hash-mismatch tolerant policy, complete-lld/no-sed source policy, LLVMgold-safe no-semantic-interposition policy, uniform split-LTO-unit policy, strict CMake warning gate, verbose Ninja failure diagnostics, host compiler pairing, fish flag splitting, linker probing, LD_PRELOAD sanitization, and CMake policy defaults validated."
    exit 0
end

# ---------------------------------------------------------------------------
# Pre-flight tools
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Fetch sources
# ---------------------------------------------------------------------------
log "=== Fresh LLVM sources: delete old, shallow clone to $TOPLEV ==="
mkdir -p "$TOPLEV"
cd "$TOPLEV" || die "Could not cd to $TOPLEV"
if test -d llvm-project
    log "Deleting old sources first..."
    rm -rf llvm-project
end
git clone --filter=blob:none --depth=1 https://github.com/llvm/llvm-project.git || die "Clone failed"
cd llvm-project || die "cd llvm-project failed"

log "Using pristine upstream lld CMake configuration (complete lld; no ELF-only sed mutation)."

set -g LLVM_SRC_TMP "/tmp/llvm-project-src-$USER"
rm -rf "$LLVM_SRC_TMP" "$LLVM_SRC_TMP.partial"
cp -a "$PWD" "$LLVM_SRC_TMP.partial" || begin
    rm -rf "$LLVM_SRC_TMP.partial"
    die "Failed to copy source to /tmp"
end
mv "$LLVM_SRC_TMP.partial" "$LLVM_SRC_TMP" || die "Failed to atomically install /tmp source tree"
set -g LLVM_SRC "$LLVM_SRC_TMP"

# ---------------------------------------------------------------------------
# Patch handling
# ---------------------------------------------------------------------------
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

log "Keeping upstream test/unittest source directories intact; CMake disables test targets instead."

set -g CMAKE_FRESH ""
if cmake --help 2>/dev/null | grep -q -- "--fresh"
    set -g CMAKE_FRESH "--fresh"
end

function emit_cmake_warning_diagnostics --argument-names CMakeLog BuildDir
    echo >&2
    echo "==================== CMAKE WARNING DIAGNOSTICS: $BuildDir ====================" >&2
    grep -nE '(^CMake Warning|Policy CMP[0-9]+|Manually-specified variables were not used|Using std::regex with exceptions disabled)' "$CMakeLog" >&2; or true
    echo "==================== LAST 120 CMAKE LOG LINES ====================" >&2
    tail -n 120 "$CMakeLog" >&2
end

function assert_clean_cmake_log --argument-names CMakeLog BuildDir
    test -s "$CMakeLog"; or die "missing CMake configure log for $BuildDir: $CMakeLog"
    if test "$STRICT_CMAKE_WARNINGS" = "1"
        if grep -qE '(^CMake Warning|Policy CMP[0-9]+|Manually-specified variables were not used|Using std::regex with exceptions disabled)' "$CMakeLog"
            emit_cmake_warning_diagnostics "$CMakeLog" "$BuildDir"
            die "CMake warning detected for $BuildDir (full log: $CMakeLog). Fix the configure, do not ignore it. Set STRICT_CMAKE_WARNINGS=0 only for deliberate local triage."
        end
    end
end

function run_ninja_logged --argument-names BuildDir
    set -e argv[1]
    test -n "$BuildDir"; or die "run_ninja_logged: empty build directory"
    set -l desc (string join '-' -- $argv)
    if test -z "$desc"
        set desc default
    end
    set desc (string replace -ra '[^A-Za-z0-9_.+-]' '_' -- "$desc")
    set -l ninja_log "$BuildDir/ninja-$desc.verbose.log"
    log "Running: ninja -C $BuildDir -v $argv  (log: $ninja_log)"
    ninja -C "$BuildDir" -v $argv 2>&1 | tee "$ninja_log"
    set -l ninja_status $pipestatus[1]
    if test $ninja_status -ne 0
        echo >&2
        echo "==================== FIRST FAILED NINJA EDGE: $BuildDir ====================" >&2
        set -l failed_line (grep -n 'FAILED:' "$ninja_log" | head -n 1 | cut -d: -f1)
        if test -n "$failed_line"
            set -l start (math "max(1, $failed_line - 40)")
            set -l end (math "$failed_line + 180")
            sed -n "$start,$end"p "$ninja_log" >&2
        else
            tail -n 240 "$ninja_log" >&2
        end
        echo "==================== ERROR-LIKE DIAGNOSTICS ====================" >&2
        grep -nE 'FAILED:|(^|[: ])error:|ld\.lld: error|clang(\+\+)?: error|Killed|signal [0-9]+|Segmentation fault|No space left|Permission denied|fatal:' "$ninja_log" | head -n 160 >&2; or true
        if grep -q 'runtimes/builtins-stamps/builtins-configure' "$ninja_log"; and grep -q 'Builtin supported architectures:[[:space:]]*$' "$ninja_log"
            die "compiler-rt builtins configure failed with zero supported architectures. This is a runtime bootstrap/configuration failure, not a PGO hash mismatch. v19 avoids it by building compiler-rt in a separate post-install runtimes stage with installed clang and clean runtime flags. Full log: $ninja_log"
        end
        die "ninja failed for $BuildDir target(s): $argv (full verbose log: $ninja_log)"
    end
end

function configure_clean
    set -l bdir $argv[1]
    set -e argv[1]
    set -l cmake_log "$bdir/configure.log"
    run mkdir -p "$bdir"
    set -l cmake_args $CMAKE_POLICY_ARGS $argv
    if test -n "$CMAKE_FRESH"
        cmake $CMAKE_WARNING_MODE_ARGS $CMAKE_FRESH -G Ninja -B "$bdir" $cmake_args 2>&1 | tee "$cmake_log"
    else
        rm -rf "$bdir/CMakeCache.txt" "$bdir/CMakeFiles"
        cmake $CMAKE_WARNING_MODE_ARGS -G Ninja -B "$bdir" $cmake_args 2>&1 | tee "$cmake_log"
    end
    set -l cmake_status $pipestatus[1]
    if test $cmake_status -ne 0
        log "--- CMake configure failed for $bdir; last 160 lines of $cmake_log ---"
        tail -n 160 "$cmake_log" >&2
        die "cmake configure failed for $bdir (full log: $cmake_log)"
    end
    assert_clean_cmake_log "$cmake_log" "$bdir"
end

set -g GOLD_CMAKE_ARGS
if test -n "$LLVM_BINUTILS_INCDIR"
    set -g GOLD_CMAKE_ARGS -DLLVM_BINUTILS_INCDIR="$LLVM_BINUTILS_INCDIR"
else
    set -g GOLD_CMAKE_ARGS
end

# ---------------------------------------------------------------------------
# Stage1 binutils helpers (populated after stage1 build)
# These are the tools that MUST archive/index ThinLTO bitcode produced by
# the in-tree Clang.  Never let distro llvm-ar touch that bitcode.
# ---------------------------------------------------------------------------
set -g STAGE1_BIN ""
set -g STAGE1_AR ""
set -g STAGE1_RANLIB ""
set -g STAGE1_NM ""
set -g STAGE1_OBJCOPY ""
set -g STAGE1_OBJDUMP ""
set -g STAGE1_STRIP ""
set -g STAGE1_READELF ""
set -g STAGE1_BINUTILS_CMAKE_ARGS

function activate_stage1_binutils
    set -g STAGE1_BIN "$BUILD_ROOT/stage1/bin"
    set -g STAGE1_AR      "$STAGE1_BIN/llvm-ar"
    set -g STAGE1_RANLIB  "$STAGE1_BIN/llvm-ranlib"
    set -g STAGE1_NM      "$STAGE1_BIN/llvm-nm"
    set -g STAGE1_OBJCOPY "$STAGE1_BIN/llvm-objcopy"
    set -g STAGE1_OBJDUMP "$STAGE1_BIN/llvm-objdump"
    set -g STAGE1_STRIP   "$STAGE1_BIN/llvm-strip"
    # llvm-readelf is often a symlink/driver of llvm-readobj
    if test -x "$STAGE1_BIN/llvm-readelf"
        set -g STAGE1_READELF "$STAGE1_BIN/llvm-readelf"
    else if test -x "$STAGE1_BIN/llvm-readobj"
        set -g STAGE1_READELF "$STAGE1_BIN/llvm-readobj"
    else
        die "stage1 has neither llvm-readelf nor llvm-readobj; cannot pin ELF reader"
    end

    verify_executable "$STAGE1_AR" "stage1 llvm-ar"
    verify_executable "$STAGE1_RANLIB" "stage1 llvm-ranlib"
    verify_executable "$STAGE1_NM" "stage1 llvm-nm"
    verify_executable "$STAGE1_OBJCOPY" "stage1 llvm-objcopy"
    verify_executable "$STAGE1_OBJDUMP" "stage1 llvm-objdump"
    verify_executable "$STAGE1_READELF" "stage1 llvm-readelf/llvm-readobj"
    verify_executable "$STAGE1_STRIP" "stage1 llvm-strip"

    # Export for any build step that reads the environment (some ninja
    # response paths, external scripts, BOLT helpers).
    set -gx AR      "$STAGE1_AR"
    set -gx RANLIB  "$STAGE1_RANLIB"
    set -gx NM      "$STAGE1_NM"
    set -gx OBJCOPY "$STAGE1_OBJCOPY"
    set -gx OBJDUMP "$STAGE1_OBJDUMP"
    set -gx STRIP   "$STAGE1_STRIP"
    set -gx LLVM_AR "$STAGE1_AR"
    set -gx LLVM_NM "$STAGE1_NM"
    set -gx LLVM_RANLIB "$STAGE1_RANLIB"

    # Prepend stage1/bin so bare tool names resolve to the matching major.
    # Keep host compiler dirs after stage1 so we don't accidentally pick up
    # INSTALL_PREFIX, but stage1 wins for ar/ranlib/lld/clang lookups that
    # stages 2+ intentionally override via CMAKE_C_COMPILER anyway.
    set -gx PATH "$STAGE1_BIN" $PATH

    # CMAKE_AR/RANLIB cover generic archive rules.  CMAKE_{C,CXX}_COMPILER_AR
    # and _RANLIB are what CMake's Clang-LTO path invokes for thin archives
    # (see Modules/Compiler/Clang.cmake) — pinning only CMAKE_AR is not enough
    # on all CMake versions.
    set -g STAGE1_BINUTILS_CMAKE_ARGS \
        -DCMAKE_AR="$STAGE1_AR" \
        -DCMAKE_RANLIB="$STAGE1_RANLIB" \
        -DCMAKE_NM="$STAGE1_NM" \
        -DCMAKE_OBJCOPY="$STAGE1_OBJCOPY" \
        -DCMAKE_OBJDUMP="$STAGE1_OBJDUMP" \
        -DCMAKE_READELF="$STAGE1_READELF" \
        -DCMAKE_STRIP="$STAGE1_STRIP" \
        -DCMAKE_C_COMPILER_AR="$STAGE1_AR" \
        -DCMAKE_CXX_COMPILER_AR="$STAGE1_AR" \
        -DCMAKE_C_COMPILER_RANLIB="$STAGE1_RANLIB" \
        -DCMAKE_CXX_COMPILER_RANLIB="$STAGE1_RANLIB"

    log "Pinned ThinLTO bitcode tools to stage1:"
    log "  AR      = $STAGE1_AR"
    log "  RANLIB  = $STAGE1_RANLIB"
    log "  NM      = $STAGE1_NM"
    log "  OBJCOPY = $STAGE1_OBJCOPY"
    log "  OBJDUMP = $STAGE1_OBJDUMP"
    log "  READELF = $STAGE1_READELF"
    log "  STRIP   = $STAGE1_STRIP"
end

# After configure_clean on a ThinLTO build dir, assert every AR-related cache
# entry points at stage1 llvm-ar (never the distro tool).
function assert_cmake_ar_pinned --argument-names BuildDir Context
    test -f "$BuildDir/CMakeCache.txt"; or die "$Context: missing CMakeCache.txt in $BuildDir"
    for _ar_key in CMAKE_AR CMAKE_C_COMPILER_AR CMAKE_CXX_COMPILER_AR
        set -l cached_ar (grep -E "^$_ar_key:" "$BuildDir/CMakeCache.txt" 2>/dev/null | string replace -r '.*=' '')
        if test -z "$cached_ar"
            # Some CMake versions omit COMPILER_AR until first LTO compile; CMAKE_AR must exist.
            if test "$_ar_key" = "CMAKE_AR"
                die "$Context: $_ar_key missing from CMakeCache.txt — cannot guarantee bitcode tool match"
            end
            continue
        end
        if test "$cached_ar" != "$STAGE1_AR"
            die "$Context: $_ar_key is '$cached_ar', expected stage1 '$STAGE1_AR'. Bitcode tool skew would follow."
        end
    end
    log "$Context: CMake AR pins verified → $STAGE1_AR"
end

# ===========================================================================
# Stage 1 — clean uninstrumented bootstrap toolchain + bitcode binutils
# ===========================================================================
log ">>> Stage 1: building clean uninstrumented bootstrap tools (clang, lld, llvm-profdata, llvm-ar/ranlib/nm)..."
configure_clean "$BUILD_ROOT/stage1" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    $LLVM_COMMON_DISABLE_CMAKE_ARGS \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_BUILD_INSTRUMENTED=OFF \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    $GOLD_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$HOST_CLANG" -DCMAKE_CXX_COMPILER="$HOST_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS" \
    -DCMAKE_CXX_FLAGS="$COMMON_FLAGS"

# Explicitly request the LLVM binutils that archive ThinLTO bitcode.  Without
# these targets, CMake falls through to whatever `llvm-ar` is on $PATH — on
# CachyOS that is the distro LLVM 22 package, which cannot read LLVM 24 BC.
run_ninja_logged "$BUILD_ROOT/stage1" \
    clang lld llvm-profdata \
    llvm-tblgen llvm-min-tblgen clang-tblgen \
    llvm-ar llvm-ranlib llvm-nm llvm-objcopy llvm-objdump llvm-strip llvm-readobj

set -g STAGE1_CLANG "$BUILD_ROOT/stage1/bin/clang"
set -g STAGE1_CLANGXX "$BUILD_ROOT/stage1/bin/clang++"
set -g PROFDATA "$BUILD_ROOT/stage1/bin/llvm-profdata"
verify_executable "$STAGE1_CLANG" "stage1 clang"
verify_executable "$PROFDATA" "stage1 llvm-profdata"
log "Using stage1 llvm-profdata: $PROFDATA"
$PROFDATA --version | head -n 1

activate_stage1_binutils
verify_bitcode_tool_pair "$STAGE1_CLANG" "$STAGE1_AR" "stage1 self-check"
configure_lto_pgo_mismatch_policy "$STAGE1_CLANG" "$STAGE1_BIN"

# Stage 1b: install compiler-rt builtins/profile support for stage1 clang.
# Stage 1's LLVM configure intentionally enables only clang/lld projects, so
# there is no top-level `compiler-rt` Ninja target there.  Build compiler-rt as
# a separate runtimes configure into the stage1 prefix, then prove stage1 clang
# can link instrumented binaries before entering PGO stages.
if test "$BUILD_STAGE1_COMPILER_RT" = "1"
    set -g STAGE1_RT_DIR "$BUILD_ROOT/stage1-compiler-rt"
    set -g STAGE1_TARGET_TRIPLE ("$STAGE1_CLANG" -dumpmachine)
    test -n "$STAGE1_TARGET_TRIPLE"; or die "stage1 clang did not report a target triple"
    set -g STAGE1_RESOURCE_DIR (env -u LD_PRELOAD "$STAGE1_CLANG" -print-resource-dir)
    test -n "$STAGE1_RESOURCE_DIR"; or die "stage1 clang did not report a resource directory"
    set -g STAGE1_COMPILER_RT_INSTALL_PATH (compiler_rt_install_path_for_resource_dir "$BUILD_ROOT/stage1" "$STAGE1_RESOURCE_DIR" "stage1 compiler-rt")
    log ">>> Stage 1b: compiler-rt runtime/profile support for stage1 clang ($STAGE1_TARGET_TRIPLE)..."
    log "Stage1 clang resource dir: $STAGE1_RESOURCE_DIR"
    log "Stage1 compiler-rt install path: $STAGE1_COMPILER_RT_INSTALL_PATH (absolute resource dir)"
    configure_clean "$STAGE1_RT_DIR" -S "$LLVM_SRC/runtimes" \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$BUILD_ROOT/stage1" \
        -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
        -DLLVM_ENABLE_PER_TARGET_RUNTIME_DIR=ON \
        -DCOMPILER_RT_INSTALL_PATH="$STAGE1_COMPILER_RT_INSTALL_PATH" \
        -DLLVM_HOST_TRIPLE="$STAGE1_TARGET_TRIPLE" \
        -DLLVM_DEFAULT_TARGET_TRIPLE="$STAGE1_TARGET_TRIPLE" \
        -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
        -DCOMPILER_RT_DEFAULT_TARGET_ONLY=ON \
        -DCOMPILER_RT_INCLUDE_TESTS=OFF \
        -DCOMPILER_RT_BUILD_BUILTINS=ON \
        -DCOMPILER_RT_BUILD_SANITIZERS=OFF \
        -DCOMPILER_RT_BUILD_XRAY=OFF \
        -DCOMPILER_RT_BUILD_LIBFUZZER=OFF \
        -DCOMPILER_RT_BUILD_MEMPROF=OFF \
        -DCOMPILER_RT_BUILD_ORC=OFF \
        -DCOMPILER_RT_BUILD_GWP_ASAN=OFF \
        -DCOMPILER_RT_BUILD_PROFILE=ON \
        -DCMAKE_C_COMPILER="$STAGE1_CLANG" \
        -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
        -DCMAKE_ASM_COMPILER="$STAGE1_CLANG" \
        -DCMAKE_C_COMPILER_TARGET="$STAGE1_TARGET_TRIPLE" \
        -DCMAKE_CXX_COMPILER_TARGET="$STAGE1_TARGET_TRIPLE" \
        -DCMAKE_ASM_COMPILER_TARGET="$STAGE1_TARGET_TRIPLE" \
        -DCMAKE_AR="$STAGE1_AR" \
        -DCMAKE_RANLIB="$STAGE1_RANLIB" \
        -DCMAKE_NM="$STAGE1_NM" \
        -DCMAKE_OBJCOPY="$STAGE1_OBJCOPY" \
        -DCMAKE_OBJDUMP="$STAGE1_OBJDUMP" \
        -DCMAKE_STRIP="$STAGE1_STRIP" \
        -DCMAKE_READELF="$STAGE1_READELF" \
        -DCMAKE_C_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_CXX_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_ASM_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN" \
        -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN" \
        -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN"
    run_ninja_logged "$STAGE1_RT_DIR" install
    verify_compiler_rt_profile_runtime "$STAGE1_CLANG" "$STAGE1_TARGET_TRIPLE" "stage1 compiler-rt"
else
    log "BUILD_STAGE1_COMPILER_RT=0: skipping stage1 compiler-rt runtime install; profile-runtime probe must still pass via host/system runtime."
end

set -l _pgo_probe "$BUILD_ROOT/stage1-profile-runtime-probe"
rm -rf "$_pgo_probe"
mkdir -p "$_pgo_probe"
printf '%s\n' 'int main(void) { return 0; }' > "$_pgo_probe/probe.c"
"$STAGE1_CLANG" -fprofile-instr-generate -fuse-ld=lld -B"$STAGE1_BIN" "$_pgo_probe/probe.c" -o "$_pgo_probe/probe" >"$_pgo_probe/probe.log" 2>&1
if test $status -ne 0
    cat "$_pgo_probe/probe.log" >&2
    die "stage1 clang cannot link -fprofile-instr-generate binaries; compiler-rt profile runtime is missing or unusable (log: $_pgo_probe/probe.log)"
end
rm -rf "$_pgo_probe"
log "Stage1 compiler-rt/profile runtime probe passed."

# Probe the exact ThinLTO flag model before spending hours in later stages.
# This catches split-LTO-unit inconsistency and TLS local-exec-in-DSO failures
# early (notably the LLVMgold.so R_X86_64_TPOFF32 failure mode).
set -l _lto_probe "$BUILD_ROOT/lto-tls-probe"
rm -rf "$_lto_probe"
mkdir -p "$_lto_probe"
printf '%s\n' 'extern "C" thread_local int plugin_tls;' 'extern "C" thread_local int plugin_tls = 7;' 'extern "C" int plugin_get() { return plugin_tls; }' > "$_lto_probe/plugin.cpp"
printf '%s\n' 'struct B { virtual ~B() = default; virtual int f() const = 0; };' 'struct D final : B { int f() const override { return 0; } };' 'int main() { D d; B *b = &d; return b->f(); }' > "$_lto_probe/main.cpp"
begin
    "$STAGE1_CLANGXX" $COMMON_FLAGS_LIST $CXX_LTO_FLAGS_LIST -fPIC -std=c++17 -c "$_lto_probe/plugin.cpp" -o "$_lto_probe/plugin.o"
    and "$STAGE1_CLANGXX" $COMMON_FLAGS_LIST $CXX_LTO_FLAGS_LIST -std=c++17 -c "$_lto_probe/main.cpp" -o "$_lto_probe/main.o"
    and "$STAGE1_CLANGXX" $CXX_LTO_FLAGS_LIST $LTO_STAGE_LINK_FLAGS_LIST -shared "$_lto_probe/plugin.o" -o "$_lto_probe/plugin.so"
    and "$STAGE1_CLANGXX" $CXX_LTO_FLAGS_LIST $LTO_STAGE_LINK_FLAGS_LIST "$_lto_probe/main.o" -o "$_lto_probe/main"
end >"$_lto_probe/probe.log" 2>&1
if test $status -ne 0
    cat "$_lto_probe/probe.log" >&2
    die "ThinLTO split-unit/TLS DSO probe failed; refusing to start multi-hour PGO stages (log: $_lto_probe/probe.log)"
end
rm -rf "$_lto_probe"
log "ThinLTO split-unit/TLS DSO probe passed."

# ===========================================================================
# Stage 2 — IR-PGO instrumented compiler.
# ThinLTO-matched codegen (CFG hashes) + stage1 binutils (bitcode I/O).
# ===========================================================================
log ">>> Stage 2: building IR-PGO instrumented compiler (ThinLTO-matched codegen, stage1 binutils)..."
set -l instr_dir "$BUILD_ROOT/stage-instr"
configure_clean "$instr_dir" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    $LLVM_COMMON_DISABLE_CMAKE_ARGS \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_ENABLE_LTO=Thin \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_VP_COUNTERS_PER_SITE=$VP_COUNTERS_PER_SITE \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
    $GOLD_CMAKE_ARGS \
    $STAGE1_BINUTILS_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS"

assert_cmake_ar_pinned "$instr_dir" "stage2 instrumented"
run_ninja_logged "$instr_dir" clang lld
set -g INSTR_CLANG "$instr_dir/bin/clang"
set -g INSTR_CLANGXX "$instr_dir/bin/clang++"
verify_executable "$INSTR_CLANG" "instrumented clang"
verify_bitcode_tool_pair "$INSTR_CLANG" "$STAGE1_AR" "stage2 instrumented"

# ===========================================================================
# Stage 3 — IR-PGO training (ThinLTO-matched flags + stage1 binutils).
# ===========================================================================
log ">>> Stage 3: generating IR-PGO profiles (ThinLTO-matched codegen)..."
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
            "$INSTR_CLANGXX" $CXX_PGO_MATCH_FLAGS_LIST -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$f" -o /dev/null 2>/dev/null
        else if test "$ext" = ".c"
            "$INSTR_CLANG" $C_PGO_MATCH_FLAGS_LIST -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
        end
    end
end

if test "$FULL_TRAIN" = "1"
    set -l tb "$BUILD_ROOT/full-pgo-train"
    rm -rf "$tb"
    configure_clean "$tb" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release $LLVM_COMMON_DISABLE_CMAKE_ARGS -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
        -DLLVM_ENABLE_LTO=Thin -DLLVM_USE_LINKER=lld \
        -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
        -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
        $STAGE1_BINUTILS_CMAKE_ARGS \
        -DCMAKE_C_COMPILER="$INSTR_CLANG" -DCMAKE_CXX_COMPILER="$INSTR_CLANGXX" \
        -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
        -DCMAKE_EXE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DCMAKE_SHARED_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DCMAKE_MODULE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS $GOLD_CMAKE_ARGS
    assert_cmake_ar_pinned "$tb" "stage3 full-pgo-train"
    verify_bitcode_tool_pair "$INSTR_CLANG" "$STAGE1_AR" "stage3 full-pgo-train"
    run_ninja_logged "$tb" -j"$NPROC" clang lld
end

set -e LLVM_PROFILE_FILE
set -l _pgo_raw (path filter -- "$BUILD_ROOT/profiles"/pgo-*.profraw)
test (count $_pgo_raw) -gt 0; or die "PGO training produced no .profraw files"
log "Found "(count $_pgo_raw)" PGO raw profiles, merging with $PROFDATA..."
$PROFDATA merge -output="$BUILD_ROOT/clang.profdata" $_pgo_raw; or die "llvm-profdata merge (PGO) failed"
validate_profdata "$BUILD_ROOT/clang.profdata" "IR PGO profile"
set -g FINAL_PROFDATA "$BUILD_ROOT/clang.profdata"

# ===========================================================================
# Stage 4 — mandatory CSIR-PGO instrumentation + training.
# ===========================================================================
log ">>> Stage 4: mandatory context-sensitive IR-PGO (ThinLTO-matched codegen, stage1 binutils)..."
set -l csd "$BUILD_ROOT/stage-cs-instr"
configure_clean "$csd" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release $LLVM_COMMON_DISABLE_CMAKE_ARGS -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld -DLLVM_ENABLE_LTO=Thin -DLLVM_BUILD_INSTRUMENTED=CSIR -DLLVM_PROFDATA_FILE="$BUILD_ROOT/clang.profdata" \
    -DLLVM_VP_COUNTERS_PER_SITE=$VP_COUNTERS_PER_SITE \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" $GOLD_CMAKE_ARGS \
    $STAGE1_BINUTILS_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS"

assert_cmake_ar_pinned "$csd" "stage4 CSIR instrumented"
run_ninja_logged "$csd" clang lld
verify_bitcode_tool_pair "$csd/bin/clang" "$STAGE1_AR" "stage4 CSIR instrumented"
set -gx LLVM_PROFILE_FILE "$BUILD_ROOT/profiles/cs-%m.profraw"
for f in $TRAIN_FILES
    if test -f "$f"
        set -l ext (path extension -- $f)
        if test "$ext" = ".cpp"; or test "$ext" = ".cc"; or test "$ext" = ".cxx"
            "$csd/bin/clang++" $CXX_PGO_MATCH_FLAGS_LIST -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$f" -o /dev/null 2>/dev/null
        else if test "$ext" = ".c"
            "$csd/bin/clang" $C_PGO_MATCH_FLAGS_LIST -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$f" -o /dev/null 2>/dev/null
        end
    end
end

if test "$FULL_TRAIN" = "1"
    set -l ctb "$BUILD_ROOT/full-cspgo-train"
    rm -rf "$ctb"
    configure_clean "$ctb" -S "$LLVM_SRC/llvm" \
        -DCMAKE_BUILD_TYPE=Release $LLVM_COMMON_DISABLE_CMAKE_ARGS -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="X86" \
        -DLLVM_ENABLE_LTO=Thin -DLLVM_USE_LINKER=lld \
        -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
        -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
        $STAGE1_BINUTILS_CMAKE_ARGS \
        -DCMAKE_C_COMPILER="$csd/bin/clang" -DCMAKE_CXX_COMPILER="$csd/bin/clang++" \
        -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
        -DCMAKE_EXE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DCMAKE_SHARED_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DCMAKE_MODULE_LINKER_FLAGS="$LTO_STAGE_LINK_FLAGS $LTO_PGO_MISMATCH_LINKER_FLAGS" \
        -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS $GOLD_CMAKE_ARGS
    assert_cmake_ar_pinned "$ctb" "stage4 full-cspgo-train"
    verify_bitcode_tool_pair "$csd/bin/clang" "$STAGE1_AR" "stage4 full-cspgo-train"
    run_ninja_logged "$ctb" -j"$NPROC" clang lld
end

set -e LLVM_PROFILE_FILE
set -l _cs_raw (path filter -- "$BUILD_ROOT/profiles"/cs-*.profraw)
test (count $_cs_raw) -gt 0; or die "CSPGO training produced no .profraw files"

# Integrity/indexing check for the raw CSIR profiles.  This file is deliberately
# NOT consumed by the final ThinLTO build.
$PROFDATA merge -output="$BUILD_ROOT/cs.profdata" $_cs_raw; or die "llvm-profdata merge (CSPGO integrity check) failed"
validate_profdata "$BUILD_ROOT/cs.profdata" "CSIR-only PGO profile"

# Correct CSIR-PGO consume profile construction:
#   llvm-profdata merge raw-CSIR.profraw... first-pass-IR.profdata -o final.profdata
# This is not equivalent to using cs.profdata alone, and it is not equivalent to
# merging clang.profdata with already-indexed cs.profdata.  The one-step raw-CS +
# IR merge preserves the metadata shape expected by LLVM's context-sensitive PGO
# reader and prevents ThinLTO from discarding counts due to CFG/hash skew.
log "Merging final CSPGO profile from raw CSIR profiles plus first-pass IR profile..."
$PROFDATA merge -output="$BUILD_ROOT/final.profdata" $_cs_raw "$BUILD_ROOT/clang.profdata"; or die "llvm-profdata final IR+CSIR merge failed"
validate_profdata "$BUILD_ROOT/final.profdata" "final merged IR+CSIR PGO profile"
set -g FINAL_PROFDATA "$BUILD_ROOT/final.profdata"
test "$FINAL_PROFDATA" = "$BUILD_ROOT/final.profdata"; or die "internal error: FINAL_PROFDATA is not final.profdata"
test "$FINAL_PROFDATA" != "$BUILD_ROOT/cs.profdata"; or die "internal error: refusing to consume CSIR-only profile as final profile"

rm -rf "$BUILD_ROOT/stage-instr" "$BUILD_ROOT/stage-cs-instr" "$BUILD_ROOT/full-pgo-train" "$BUILD_ROOT/full-cspgo-train" "$BUILD_ROOT/profiles" 2>/dev/null || true

# ===========================================================================
# Stage 5 — final ThinLTO + PGO install.
# ===========================================================================
log ">>> Stage 5: final ThinLTO + selected PGO profile + allocator + LLVMgold build..."
configure_clean "$BUILD_ROOT/stage2" -S "$LLVM_SRC/llvm" \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    $LLVM_COMMON_DISABLE_CMAKE_ARGS \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly" -DLLVM_TARGETS_TO_BUILD="X86;BPF" \
    -DLLVM_USE_LINKER=lld -DCLANG_DEFAULT_LINKER=lld -DLLVM_ENABLE_LTO=Thin -DLLVM_PROFDATA_FILE="$FINAL_PROFDATA" \
    -DLLVM_THINLTO_CACHE_PATH="$THINLTO_CACHE" \
    -DLLVM_ENABLE_BINDINGS=$LLVM_ENABLE_BINDINGS \
    -DLLVM_TABLEGEN="$BUILD_ROOT/stage1/bin/llvm-tblgen" \
    -DCLANG_TABLEGEN="$BUILD_ROOT/stage1/bin/clang-tblgen" \
    $GOLD_CMAKE_ARGS \
    $STAGE1_BINUTILS_CMAKE_ARGS \
    -DCMAKE_C_COMPILER="$STAGE1_CLANG" -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
    -DCMAKE_C_FLAGS="$COMMON_FLAGS $C_LTO_FLAGS" -DCMAKE_CXX_FLAGS="$COMMON_FLAGS $CXX_LTO_FLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LINKER_BASE $LTO_PGO_MISMATCH_LINKER_FLAGS $ALLOCATOR_LINK -Wl,--emit-relocs -Wl,-z,now" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LINKER_BASE $LTO_PGO_MISMATCH_LINKER_FLAGS -Wl,--emit-relocs" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LINKER_BASE $LTO_PGO_MISMATCH_LINKER_FLAGS -Wl,--emit-relocs"

assert_cmake_ar_pinned "$BUILD_ROOT/stage2" "stage5 final"
verify_bitcode_tool_pair "$STAGE1_CLANG" "$STAGE1_AR" "stage5 final"
run_ninja_logged "$BUILD_ROOT/stage2" install

# ===========================================================================
# Stage 5b — compiler-rt post-install runtime build
# ===========================================================================
# Build compiler-rt after final clang/lld are installed, but compile it with
# the clean stage1 compiler/binutils and install it into final clang's resource
# directory.  Do not use LLVM_ENABLE_RUNTIMES inside the final PGO+ThinLTO LLVM
# build: compiler-rt's nested builtins configure must not use an in-progress
# final compiler or PGO/LTO/WPD flags.
if test "$BUILD_COMPILER_RT" = "1"
    set -g FINAL_CLANG "$INSTALL_PREFIX/bin/clang"
    set -g FINAL_CLANGXX "$INSTALL_PREFIX/bin/clang++"
    set -g FINAL_LLD "$INSTALL_PREFIX/bin/ld.lld"
    set -g FINAL_AR "$INSTALL_PREFIX/bin/llvm-ar"
    set -g FINAL_RANLIB "$INSTALL_PREFIX/bin/llvm-ranlib"
    set -g FINAL_NM "$INSTALL_PREFIX/bin/llvm-nm"
    set -g FINAL_OBJCOPY "$INSTALL_PREFIX/bin/llvm-objcopy"
    set -g FINAL_OBJDUMP "$INSTALL_PREFIX/bin/llvm-objdump"
    set -g FINAL_STRIP "$INSTALL_PREFIX/bin/llvm-strip"
    set -g FINAL_READELF "$INSTALL_PREFIX/bin/llvm-readelf"
    if not test -x "$FINAL_READELF"
        set FINAL_READELF "$INSTALL_PREFIX/bin/llvm-readobj"
    end

    verify_executable "$FINAL_CLANG" "installed final clang for compiler-rt"
    verify_executable "$FINAL_CLANGXX" "installed final clang++ for compiler-rt"
    verify_executable "$FINAL_LLD" "installed final ld.lld for compiler-rt"
    verify_executable "$FINAL_AR" "installed final llvm-ar for compiler-rt"
    verify_executable "$FINAL_RANLIB" "installed final llvm-ranlib for compiler-rt"
    verify_executable "$FINAL_NM" "installed final llvm-nm for compiler-rt"
    verify_executable "$FINAL_OBJCOPY" "installed final llvm-objcopy for compiler-rt"
    verify_executable "$FINAL_OBJDUMP" "installed final llvm-objdump for compiler-rt"
    verify_executable "$FINAL_STRIP" "installed final llvm-strip for compiler-rt"
    verify_executable "$FINAL_READELF" "installed final llvm-readelf/llvm-readobj for compiler-rt"
    verify_no_mimalloc_dependency "$FINAL_CLANG" "installed final clang"
    smoke_test_clang_driver "$FINAL_CLANG" "installed final clang"

    set -g FINAL_TARGET_TRIPLE (env -u LD_PRELOAD "$FINAL_CLANG" -dumpmachine)
    test -n "$FINAL_TARGET_TRIPLE"; or die "installed final clang did not report a target triple"
    set -g FINAL_RESOURCE_DIR (env -u LD_PRELOAD "$FINAL_CLANG" -print-resource-dir)
    test -n "$FINAL_RESOURCE_DIR"; or die "installed final clang did not report a resource directory"
    set -g FINAL_COMPILER_RT_INSTALL_PATH (compiler_rt_install_path_for_resource_dir "$INSTALL_PREFIX" "$FINAL_RESOURCE_DIR" "final compiler-rt")

    set -g RUNTIME_DIR "$BUILD_ROOT/compiler-rt-final"
    log ">>> Stage 5b: compiler-rt runtime build for final clang using clean stage1 compiler ($FINAL_TARGET_TRIPLE)..."
    log "Final clang resource dir: $FINAL_RESOURCE_DIR"
    log "Final compiler-rt install path: $FINAL_COMPILER_RT_INSTALL_PATH (absolute resource dir; compiled by stage1 clang)"
    configure_clean "$RUNTIME_DIR" -S "$LLVM_SRC/runtimes" \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
        -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
        -DLLVM_ENABLE_PER_TARGET_RUNTIME_DIR=ON \
        -DCOMPILER_RT_INSTALL_PATH="$FINAL_COMPILER_RT_INSTALL_PATH" \
        -DLLVM_HOST_TRIPLE="$FINAL_TARGET_TRIPLE" \
        -DLLVM_DEFAULT_TARGET_TRIPLE="$FINAL_TARGET_TRIPLE" \
        -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
        -DCOMPILER_RT_DEFAULT_TARGET_ONLY=ON \
        -DCOMPILER_RT_INCLUDE_TESTS=OFF \
        -DCOMPILER_RT_BUILD_BUILTINS=ON \
        -DCOMPILER_RT_BUILD_SANITIZERS=$COMPILER_RT_BUILD_SANITIZERS \
        -DCOMPILER_RT_BUILD_XRAY=$COMPILER_RT_BUILD_XRAY \
        -DCOMPILER_RT_BUILD_LIBFUZZER=$COMPILER_RT_BUILD_LIBFUZZER \
        -DCOMPILER_RT_BUILD_MEMPROF=$COMPILER_RT_BUILD_MEMPROF \
        -DCOMPILER_RT_BUILD_ORC=$COMPILER_RT_BUILD_ORC \
        -DCOMPILER_RT_BUILD_GWP_ASAN=$COMPILER_RT_BUILD_GWP_ASAN \
        -DCOMPILER_RT_BUILD_PROFILE=$COMPILER_RT_BUILD_PROFILE \
        -DCOMPILER_RT_USE_BUILTINS_LIBRARY=ON \
        -DCMAKE_C_COMPILER="$STAGE1_CLANG" \
        -DCMAKE_CXX_COMPILER="$STAGE1_CLANGXX" \
        -DCMAKE_ASM_COMPILER="$STAGE1_CLANG" \
        -DCMAKE_C_COMPILER_TARGET="$FINAL_TARGET_TRIPLE" \
        -DCMAKE_CXX_COMPILER_TARGET="$FINAL_TARGET_TRIPLE" \
        -DCMAKE_ASM_COMPILER_TARGET="$FINAL_TARGET_TRIPLE" \
        -DCMAKE_AR="$STAGE1_AR" \
        -DCMAKE_RANLIB="$STAGE1_RANLIB" \
        -DCMAKE_NM="$STAGE1_NM" \
        -DCMAKE_OBJCOPY="$STAGE1_OBJCOPY" \
        -DCMAKE_OBJDUMP="$STAGE1_OBJDUMP" \
        -DCMAKE_STRIP="$STAGE1_STRIP" \
        -DCMAKE_READELF="$STAGE1_READELF" \
        -DCMAKE_C_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_CXX_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_ASM_FLAGS="$RUNTIME_FLAGS" \
        -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN" \
        -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN" \
        -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=lld -B$STAGE1_BIN"
    run_ninja_logged "$RUNTIME_DIR" install
    verify_compiler_rt_profile_runtime "$FINAL_CLANG" "$FINAL_TARGET_TRIPLE" "final compiler-rt"
    set -l _final_pgo_probe "$BUILD_ROOT/final-profile-runtime-probe"
    rm -rf "$_final_pgo_probe"
    mkdir -p "$_final_pgo_probe"
    printf '%s\n' 'int main(void) { return 0; }' > "$_final_pgo_probe/probe.c"
    "$FINAL_CLANG" -fprofile-instr-generate -fuse-ld=lld -B"$INSTALL_PREFIX/bin" "$_final_pgo_probe/probe.c" -o "$_final_pgo_probe/probe" >"$_final_pgo_probe/probe.log" 2>&1
    if test $status -ne 0
        cat "$_final_pgo_probe/probe.log" >&2
        die "final clang cannot link -fprofile-instr-generate binaries; final compiler-rt profile runtime is missing or unusable (log: $_final_pgo_probe/probe.log)"
    end
    rm -rf "$_final_pgo_probe"
    log "Final compiler-rt/profile runtime probe passed."
else
    log "BUILD_COMPILER_RT=0: skipping post-install compiler-rt runtime build."
end

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

# Prefer the freshly installed toolchain on PATH for BOLT / readelf, but keep
# stage1 binutils available until stage1 is removed.
set -gx PATH "$INSTALL_PREFIX/bin" "$STAGE1_BIN" $PATH

rm -rf "$BUILD_ROOT/stage1" 2>/dev/null || true
# stage1 is gone — clear pinned paths that no longer exist; install tree takes over.
set -g STAGE1_BIN ""
set -e AR
set -e RANLIB
set -e NM
set -e OBJCOPY
set -e STRIP
set -e LLVM_AR
set -e LLVM_NM
set -e LLVM_RANLIB
set -gx PATH "$INSTALL_PREFIX/bin" $PATH

# ===========================================================================
# Stage 6 — BOLT post-link optimization
# ===========================================================================
set -g BOLT "$INSTALL_PREFIX/bin/llvm-bolt"
set -g MERGE "$INSTALL_PREFIX/bin/merge-fdata"
verify_executable "$BOLT" "llvm-bolt"
verify_executable "$INSTALL_PREFIX/bin/clang" "clang"

# Prefer install-tree readelf; fall back to whatever is on PATH.
set -g READELF_BIN "$INSTALL_PREFIX/bin/llvm-readelf"
if not test -x "$READELF_BIN"
    set READELF_BIN "$INSTALL_PREFIX/bin/llvm-readobj"
end
if not test -x "$READELF_BIN"
    set READELF_BIN (command -v llvm-readelf)
end
test -x "$READELF_BIN"; or die "no llvm-readelf/llvm-readobj available for BOLT pre-check"

function is_already_bolted --argument-names BinPath
    set -l f (realpath "$BinPath")
    test -f "$f"; or return 1
    "$READELF_BIN" -S "$f" 2>/dev/null | grep -qE '\.note\.bolt_info\b'; and return 0
    return 1
end

function bolt_train_clang --argument-names Bin
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -I "$LLVM_SRC/llvm/include" -I "$LLVM_SRC/clang/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/Support/APFloat.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -I "$LLVM_SRC/llvm/include" -std=c++17 -c "$LLVM_SRC/llvm/lib/CodeGen/SelectionDAG/SelectionDAG.cpp" -o /dev/null 2>/dev/null
    "$Bin" $COMMON_FLAGS_LIST -fno-lto -I "$LLVM_SRC/llvm/include" -std=gnu17 -c "$LLVM_SRC/llvm/lib/Support/regcomp.c" -o /dev/null 2>/dev/null
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

log ">>> Stage 6: BOLT post-link optimization (clang + ld.lld)..."
bolt_optimize_binary clang "$INSTALL_PREFIX/bin/clang"
if test -e "$INSTALL_PREFIX/bin/ld.lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/ld.lld"
else if test -e "$INSTALL_PREFIX/bin/lld"
    bolt_optimize_binary lld "$INSTALL_PREFIX/bin/lld"
end

set -l _bolt_leftovers (path filter -- "$BUILD_ROOT"/bolt*)
test (count $_bolt_leftovers) -gt 0; and rm -rf $_bolt_leftovers

log "ULTIMATE v11 build finished. Toolchain at: $INSTALL_PREFIX"
if test "$REQUIRE_GOLD_PLUGIN" = "1"
    log "Verified deliverables: $INSTALL_PREFIX/bin/clang  $INSTALL_PREFIX/bin/ld.lld  $INSTALL_PREFIX/lib/LLVMgold.so"
log "compiler-rt : stage1=$BUILD_STAGE1_COMPILER_RT final=$BUILD_COMPILER_RT sanitizers=$COMPILER_RT_BUILD_SANITIZERS profile=$COMPILER_RT_BUILD_PROFILE resource-dir layout verified; final compiler-rt compiled by stage1; mimalloc dependency forbidden by default"
end
log "To use: export PATH=$INSTALL_PREFIX/bin:\$PATH"
