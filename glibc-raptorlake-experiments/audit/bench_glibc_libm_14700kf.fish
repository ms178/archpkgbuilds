#!/usr/bin/env fish
# CachyOS / Arch fish A/B benchmark for installed optimized system libm vs
# generated old glibc-2.43 standalone baseline implementations.
# Self-contained: generates old baseline C files at runtime.
# Benchmarks atan2f, sincosf, and powf.
# Usage:
#   fish bench_glibc_libm_14700kf.fish --quick --no-perf
#   fish bench_glibc_libm_14700kf.fish --reps 10
# Recommended packages:
#   sudo pacman -S --needed fish gcc binutils coreutils gzip util-linux perf python

function die
    set_color red
    echo "ERROR: $argv" >&2
    set_color normal
    exit 1
end
function info
    set_color cyan
    echo "==> $argv"
    set_color normal
end
function warn
    set_color yellow
    echo "WARN: $argv" >&2
    set_color normal
end
function ok
    set_color green
    echo "PASS: $argv"
    set_color normal
end
function have_cmd
    command -q $argv[1]
end
function decode_payload
    set -l out $argv[1]
    set -l payload $argv[2]
    printf '%s' "$payload" | base64 -d | gzip -dc > "$out"
    or die "failed to generate $out"
end
function run_capture
    set -l outfile $argv[1]
    set -e argv[1]
    echo "+ $argv | tee $outfile"
    $argv 2>&1 | tee "$outfile"
    return $pipestatus[1]
end
function run_bench_for_cpuset
    set -l label $argv[1]
    set -l cpuset $argv[2]
    set -l outfile "$RESULT_DIR/bench_$label.txt"
    set -l cmd "$BENCH_EXE" --bench --n "$N_INPUTS" --blocks "$BLOCKS" --inner "$INNER"
    if test -n "$cpuset"
        set cmd taskset -c "$cpuset" $cmd
    end
    if test "$USE_PERF" = 1
        echo "+ perf stat [$label] $cmd"
        perf stat -r "$PERF_REPS" -e cycles,instructions,branches,branch-misses,cache-misses -- $cmd 2>&1 | tee "$outfile"
        set -l st $pipestatus[1]
        if test $st -eq 0
            return 0
        end
        warn "perf failed for $label with status $st; retrying without perf"
        echo "" >> "$outfile"
        echo "--- perf failed; fallback raw benchmark ---" >> "$outfile"
        $cmd 2>&1 | tee -a "$outfile"
        return $pipestatus[1]
    else
        echo "+ run [$label] $cmd"
        $cmd 2>&1 | tee "$outfile"
        return $pipestatus[1]
    end
end
function summarize_file
    set -l label $argv[1]
    set -l file $argv[2]
    if not test -f "$file"
        return 0
    end
    echo ""
    info "A/B summary: $label"
    grep '^AB ' "$file"; or true
end
function write_sources
    info "Generating old baseline and A/B benchmark C sources"
    decode_payload "$WORK_DIR/audit_math_compat.h" 'H4sICJM9GGoAA2F1ZGl0X21hdGhfY29tcGF0LmgAvVRtT9swEP7OrziKhFKtQ2VDfFj3oqwNEAmSqQQY+2LlxW6tJXaVOKWF8d93TtI0fUGgatqHKL577p57fGf7gDMRUQbmzcD2yJXpXZC+e/XD9MjF3gECXNDtGBdhnEcUPtM0FfJo/LXhYlRMVz2Jr8arnkxFgZTxhpMLte5LuRgVvkqPr9AV5IqSMY8iKmqgNAlaxqxde5FAihHxY+5nht8JlkjMg6T0E7S5oqvwlKYZl4JGJJsngYw12gk70XaCWPqKSDWm6SrLtdX3bNepbc+1HY/gN7Sda7t/Dd0aujMRGbo3zsB2zuF41W8Nh4671Uluzi7dOzBW0w8PGyEohzM9Z0JGKDokuYj5bxrP9w4gF9sBKiLO1vNeyNrIKVCeZXwksD9ihBmV7oYTp4QMQc5jxQVpAFDMryKr8tYFGqEUUTOfziY0VGDs75dQB7rtjew35x4vc/XhJcJPU/lA6NSPtezmAStwOVEkwBiO89f4E7Kr+YRKBuU2Z/BFr3q49LOEEPxPZewrHlNCjFYLPkHrXdICA0PbRdisB89rVZhMQ1qLiCQ8wYKkrofUaxWNqeTFdjUjPIx1tIHNyRRmhsCxo1ggx6v38QNR4Gd6CUZxpIG1sUiN5T1IaBJO5mAc5h04ZB3I+KPeZY6FUqryVOig5zXyksvPyr+x5NPsVaEGNdJq+oqaLanZJrXmOj2pdZ+egBHJPMA91tILeFfpFZmfVQtjyaj5F7V2VF/unZDyEKH1wBrt0TdCFykeWRynNTSdc6sHjKY+zyidhXSC7TyziHtrDYtn4A+gZTvWT7PvLStrIvgG723nzHZs7x5P22L5iqZ8d034Er1JVPeoy1CQ/r0iJvHnLwv6HwIiPn2kqdytHwP79vv9L2vo/ou5cIGvAI/Y4p7OtIbqpuM9r9UM3KttWmzn1ry0B0sljunoitWr+xdBumVJGAgAAA=='
    decode_payload "$WORK_DIR/old_atan2f.c" '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'
    decode_payload "$WORK_DIR/old_sincosf.c" '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'
    decode_payload "$WORK_DIR/old_powf.c" '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'
    decode_payload "$WORK_DIR/ab_bench.c" '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'
    decode_payload "$WORK_DIR/summarize_ab.py" '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'
end
function build_bench
    info "Building old baseline objects and A/B benchmark"
    write_sources
    $CC $CFLAGS -c "$WORK_DIR/old_atan2f.c" -o "$WORK_DIR/old_atan2f.o" 2> "$RESULT_DIR/old_atan2f_warnings.txt"
    or begin
        cat "$RESULT_DIR/old_atan2f_warnings.txt" >&2
        die "old_atan2f build failed"
    end
    $CC $CFLAGS -c "$WORK_DIR/old_sincosf.c" -o "$WORK_DIR/old_sincosf.o" 2> "$RESULT_DIR/old_sincosf_warnings.txt"
    or begin
        cat "$RESULT_DIR/old_sincosf_warnings.txt" >&2
        die "old_sincosf build failed"
    end
    $CC $CFLAGS -c "$WORK_DIR/old_powf.c" -o "$WORK_DIR/old_powf.o" 2> "$RESULT_DIR/old_powf_warnings.txt"
    or begin
        cat "$RESULT_DIR/old_powf_warnings.txt" >&2
        die "old_powf build failed"
    end
    $CC $CFLAGS $WARNFLAGS "$WORK_DIR/ab_bench.c" "$WORK_DIR/old_atan2f.o" "$WORK_DIR/old_sincosf.o" "$WORK_DIR/old_powf.o" -lm -o "$BENCH_EXE" 2> "$RESULT_DIR/build_warnings.txt"
    or begin
        cat "$RESULT_DIR/build_warnings.txt" >&2
        die "A/B benchmark build failed"
    end
    if test -s "$RESULT_DIR/build_warnings.txt"
        warn "A/B benchmark emitted warnings; see $RESULT_DIR/build_warnings.txt"
    else
        ok "A/B benchmark built warning-clean"
    end
    $CC $CFLAGS -S -fno-asynchronous-unwind-tables -fno-stack-protector "$WORK_DIR/ab_bench.c" -o "$RESULT_DIR/ab_bench.s"
    objdump -dr --no-show-raw-insn "$BENCH_EXE" > "$RESULT_DIR/ab_bench.objdump"
end
function find_libm
    info "Resolving linked system libm"
    ldd "$BENCH_EXE" | tee "$RESULT_DIR/ldd.txt"
    set -l libm (ldd "$BENCH_EXE" | string match -r 'libm\.so[^ ]* => ([^ ]+)' | string replace -r '.* => ([^ ]+).*' '$1' | head -n 1)
    if test -n "$libm"; and test -f "$libm"
        echo "$libm" > "$RESULT_DIR/libm_path.txt"
        objdump -T "$libm" | grep -E ' (atan2f|sincosf|powf)($|@@|@)' > "$RESULT_DIR/libm_symbols.txt"; or true
    else
        warn "Could not resolve libm from ldd"
    end
end
function run_correctness
    info "Running old-vs-system correctness checks"
    run_capture "$RESULT_DIR/correctness.txt" "$BENCH_EXE" --check --samples "$CHECK_SAMPLES"
    or die "old-vs-system correctness failed"
    ok "Correctness checks passed"
end
function run_all_benches
    run_bench_for_cpuset pcores "$PCORES"
    if test "$QUICK" = 0
        run_bench_for_cpuset ecores "$ECORES"
        run_bench_for_cpuset mixed "$MIXED"
    end
end
function write_summary
    info "Writing guaranteed integrated summary.txt"
    set -l summary "$RESULT_DIR/summary.txt"
    set -l rich "$RESULT_DIR/summary.rich.tmp"

    # First write a guaranteed fish-native fallback summary.  This ensures
    # summary.txt exists even if Python, the rich summarizer, or a parser fails.
    begin
        echo "# glibc libm A/B Benchmark Summary"
        echo ""
        echo "Result directory: `$RESULT_DIR`"
        echo "Generated by: `bench_glibc_libm_14700kf.fish`"
        echo ""
        echo "## Quick verdict guide"
        echo ""
        echo "- Positive speedup means the installed system libm is faster than the generated old glibc-2.43 baseline."
        echo "- Negative speedup means the installed system libm is slower for that micro-workload."
        echo "- Focus on P-core rows for gaming, E-core rows for background/simulation efficiency, and p99 rows for 1% low / tail-latency risk."
        echo ""
        echo "## Correctness"
        echo ""
        if test -f "$RESULT_DIR/correctness.txt"
            echo '```text'
            cat "$RESULT_DIR/correctness.txt"
            echo '```'
        else
            echo "⚠️ correctness.txt missing"
        end
        echo ""
        echo "## Build warnings"
        echo ""
        if test -s "$RESULT_DIR/build_warnings.txt"
            echo '```text'
            cat "$RESULT_DIR/build_warnings.txt"
            echo '```'
        else
            echo "✅ Build warnings: none"
        end
        echo ""
        echo "## Linked libm"
        echo ""
        if test -f "$RESULT_DIR/libm_path.txt"
            echo "libm path: `"(cat "$RESULT_DIR/libm_path.txt")"`"
        else
            echo "libm path: unknown"
        end
        if test -f "$RESULT_DIR/libm_symbols.txt"
            echo '```text'
            cat "$RESULT_DIR/libm_symbols.txt"
            echo '```'
        end
        echo ""
        echo "## Environment"
        echo ""
        if test -f "$RESULT_DIR/environment.txt"
            echo '```text'
            grep -E '^(date=|compiler=|gcc |cflags=|quick=|perf=|pcores=|n=|kernel=|Modellname:|CPU\(s\):|Maximale Taktfrequenz|L3 Cache)' "$RESULT_DIR/environment.txt"; or true
            echo '```'
        else
            echo "⚠️ environment.txt missing"
        end
        echo ""
        echo "## Raw A/B benchmark lines"
        echo ""
        set -l saw_ab 0
        for group in pcores ecores mixed
            set -l file "$RESULT_DIR/bench_$group.txt"
            if test -f "$file"
                echo "### $group"
                echo '```text'
                if grep '^AB ' "$file"
                    set saw_ab 1
                else
                    echo "No AB lines found in $file"
                end
                echo '```'
                echo ""
            end
        end
        if test $saw_ab -eq 0
            echo "⚠️ No AB lines were found. The benchmark may have failed before producing A/B results."
            echo ""
        end
        echo "## Perf excerpts"
        echo ""
        for group in pcores ecores mixed
            set -l file "$RESULT_DIR/bench_$group.txt"
            if test -f "$file"
                if grep -q 'Performance counter stats' "$file"
                    echo "### $group"
                    echo '```text'
                    grep -E 'cycles|instructions|branches|branch-misses|cache-misses|seconds time elapsed' "$file" | tail -n 16; or true
                    echo '```'
                    echo ""
                end
            end
        end
        echo "## Script recommendation"
        echo ""
        echo 'Upload this `summary.txt` for analysis. If correctness is PASS, build warnings are empty, and the rich summary below shows no important red regression rows for your gaming-relevant workloads (`atan2_unit`, `atan2_axis`, `sincos_game`, `sincos_small`, `pow_game`, `pow_gamma`, `pow_xone`), keep the optimized libm. Investigate any negative p99 speedups worse than about -10% on P-cores.'
    end > "$summary"

    # Try to replace the fallback with the richer Python report.  If this fails,
    # keep the fallback summary.txt that already exists.
    if test -f "$WORK_DIR/summarize_ab.py"
        python "$WORK_DIR/summarize_ab.py" "$RESULT_DIR" > "$rich"
        set -l py_status $status
        if test $py_status -eq 0
            if test -s "$rich"
                mv "$rich" "$summary"
                echo "" >> "$summary"
                echo "## Script recommendation" >> "$summary"
                echo "" >> "$summary"
                echo "Upload this \`summary.txt\` for analysis. Positive speedup means installed system libm is faster than the generated old glibc-2.43 baseline. Keep the optimized libm if correctness passed and P-core gaming-relevant rows are neutral or positive; inspect any red regression-risk rows, especially p99 regressions worse than about -10%." >> "$summary"
            else
                echo "" >> "$summary"
                echo "⚠️ Rich Python summary was empty; kept fallback summary." >> "$summary"
            end
        else
            echo "" >> "$summary"
            echo "⚠️ Rich Python summary failed with status $py_status; kept fallback summary." >> "$summary"
        end
    else
        echo "" >> "$summary"
        echo "⚠️ Rich Python summarizer missing; kept fallback summary." >> "$summary"
    end

    if not test -s "$summary"
        die "summary generation failed: $summary was not created"
    end

    echo ""
    echo "================ summary.txt ================"
    cat "$summary"
    echo "================ end summary.txt ============"
end

function final_report
    echo ""
    info "Final report"
    echo "Results: $RESULT_DIR"
    echo "Integrated summary: $RESULT_DIR/summary.txt"
    echo "Compiler: $CC"
    echo "CFLAGS: $CFLAGS"
    echo "Perf: $USE_PERF reps=$PERF_REPS"
    echo "Inputs: n=$N_INPUTS blocks=$BLOCKS inner=$INNER check_samples=$CHECK_SAMPLES"
    echo "CPU sets: pcores=$PCORES ecores=$ECORES mixed=$MIXED"
    summarize_file pcores "$RESULT_DIR/bench_pcores.txt"
    summarize_file ecores "$RESULT_DIR/bench_ecores.txt"
    summarize_file mixed "$RESULT_DIR/bench_mixed.txt"
    echo ""
    echo "Recommendation: upload $RESULT_DIR/summary.txt. Positive speedup means installed system libm is faster than the generated old glibc-2.43 baseline. If correctness passed and P-core gaming-relevant rows are neutral or positive, keep the optimized libm; investigate any large negative p99 rows."

    if test -f "$RESULT_DIR/summary.txt"
        begin
            echo ""
            echo "## End-of-run note"
            echo ""
            echo "Script completed successfully. The integrated report is complete and ready to upload."
        end >> "$RESULT_DIR/summary.txt"
    end
end

set -g QUICK 0
set -g CC gcc
set -g PERF_REPS 10
set -g USE_PERF 1
set -g PCORES "0-15"
set -g ECORES "16-27"
set -g MIXED "0-27"
set -g N_INPUTS 262144
set -g BLOCKS 64
set -g INNER 16
set -g CHECK_SAMPLES 200000
argparse 'quick' 'no-perf' 'cc=' 'reps=' 'pcores=' 'ecores=' 'mixed=' 'n=' 'blocks=' 'inner=' 'check-samples=' -- $argv
or exit 2
if set -q _flag_quick
    set QUICK 1
    set PERF_REPS 3
    set N_INPUTS 131072
    set BLOCKS 32
    set INNER 8
    set CHECK_SAMPLES 50000
end
if set -q _flag_no_perf
    set USE_PERF 0
end
if set -q _flag_cc
    set CC $_flag_cc
end
if set -q _flag_reps
    set PERF_REPS $_flag_reps
end
if set -q _flag_pcores
    set PCORES $_flag_pcores
end
if set -q _flag_ecores
    set ECORES $_flag_ecores
end
if set -q _flag_mixed
    set MIXED $_flag_mixed
end
if set -q _flag_n
    set N_INPUTS $_flag_n
end
if set -q _flag_blocks
    set BLOCKS $_flag_blocks
end
if set -q _flag_inner
    set INNER $_flag_inner
end
if set -q _flag_check_samples
    set CHECK_SAMPLES $_flag_check_samples
end

set -g ROOT (pwd)
set -g DATE_TAG (date +%Y%m%d_%H%M%S)
set -g RESULT_DIR "$ROOT/bench-results-$DATE_TAG"
set -g WORK_DIR "$RESULT_DIR/generated-old-baseline"
set -g BENCH_EXE "$RESULT_DIR/ab_bench"
mkdir -p "$RESULT_DIR" "$WORK_DIR"
set -g CFLAGS -O3 -march=raptorlake -mno-avx512f -fno-builtin-atan2f -fno-builtin-sincosf -fno-builtin-powf
set -g WARNFLAGS -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wundef -Wdouble-promotion -Wformat -Wvla -Wmissing-field-initializers -Wnull-dereference -Wstrict-aliasing=2
have_cmd "$CC"; or die "Compiler '$CC' not found. Install with: sudo pacman -S --needed gcc"
have_cmd objdump; or die "objdump not found. Install with: sudo pacman -S --needed binutils"
have_cmd taskset; or die "taskset not found. Install with: sudo pacman -S --needed util-linux"
have_cmd base64; or die "base64 not found. Install coreutils."
have_cmd gzip; or die "gzip not found. Install gzip."
have_cmd ldd; or die "ldd not found."
have_cmd python; or die "python not found. Install with: sudo pacman -S --needed python"
if test "$USE_PERF" = 1
    if not have_cmd perf
        warn "perf not found; running without perf. Install with: sudo pacman -S perf"
        set USE_PERF 0
    end
end
begin
    echo "date="(date -Is)
    echo "root=$ROOT"
    echo "compiler=$CC"
    $CC --version | head -n 1
    echo "cflags=$CFLAGS"
    echo "warnflags=$WARNFLAGS"
    echo "quick=$QUICK"
    echo "perf=$USE_PERF reps=$PERF_REPS"
    echo "pcores=$PCORES ecores=$ECORES mixed=$MIXED"
    echo "n=$N_INPUTS blocks=$BLOCKS inner=$INNER check_samples=$CHECK_SAMPLES"
    echo "kernel="(uname -a)
    if have_cmd lscpu
        lscpu
    end
end > "$RESULT_DIR/environment.txt"
info "Results directory: $RESULT_DIR"
build_bench
find_libm
run_correctness
run_all_benches
write_summary
final_report
ok "A/B benchmark completed"
