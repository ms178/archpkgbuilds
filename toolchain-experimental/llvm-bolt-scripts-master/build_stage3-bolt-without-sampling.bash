#!/bin/bash
set -euo pipefail

TOPLEV=~/toolchain/llvm
STAGE3_DIR="${TOPLEV}/stage3-without-sampling"
INST_DATA="${STAGE3_DIR}/instrumentdata"
CPATH="${TOPLEV}/stage2-prof-use-lto/install/bin"
BOLTPATH="${TOPLEV}/llvm-bolt/bin"

# Verify BOLT and compilers exist
if [ ! -x "${BOLTPATH}/llvm-bolt" ]; then
    echo "Error: llvm-bolt not found at ${BOLTPATH}"
    exit 1
fi

if [ ! -x "${CPATH}/clang-22" ]; then
    echo "Error: clang-22 not found at ${CPATH}"
    exit 1
fi

mkdir -p "${INST_DATA}"
mkdir -p "${STAGE3_DIR}"
cd "${STAGE3_DIR}"

instrument_binary() {
    local binary="$1"
    local output="${binary}.inst"
    local profile="${INST_DATA}/${binary##*/}.fdata"

    # Safety: don't re-instrument if backup exists
    if [ -f "${binary}.org" ]; then
        echo "Backup ${binary}.org already exists, skipping instrumentation"
        return 0
    fi

    echo "Instrumenting ${binary}"
    "${BOLTPATH}/llvm-bolt" \
        --instrument \
        --lite=false \
        --instrumentation-file-append-pid \
        --instrumentation-file="${profile}" \
        "${binary}" \
        -o "${output}" || { echo "Instrumentation failed for ${binary}"; exit 1; }

    mv "${binary}" "${binary}.org"
    mv "${output}" "${binary}"
}

# Instrument binaries
instrument_binary "${CPATH}/clang-22"
instrument_binary "${CPATH}/lld"

# Configure build (FIX: Use absolute path)
cmake -G Ninja "${TOPLEV}/llvm-project/llvm" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DCMAKE_C_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=60000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1" \
    -DCMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=60000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1" \
    -DCMAKE_EXE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -DCMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -DCMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -DLLVM_VP_COUNTERS_PER_SITE=10 \
    -DCMAKE_AR="${CPATH}/llvm-ar" \
    -DCMAKE_C_COMPILER="${CPATH}/clang-22" \
    -DCMAKE_CXX_COMPILER="${CPATH}/clang++" \
    -DLLVM_USE_LINKER="${CPATH}/ld.lld" \
    -DCMAKE_RANLIB="${CPATH}/llvm-ranlib" \
    -DCMAKE_INSTALL_PREFIX="${STAGE3_DIR}/install" || { echo "CMake failed"; exit 1; }

# FIX: Increase timeout (30 min should be enough for partial build)
echo "Building for 30 minutes to generate BOLT profiles..."
timeout 1800s ninja || echo "Build stopped (timeout or error) - profiles collected"

echo "Merging generated profiles"
cd "${INST_DATA}" || { echo "Cannot enter ${INST_DATA}"; exit 1; }

# Check if any profiles exist
if ! ls *.fdata 1> /dev/null 2>&1; then
    echo "ERROR: No .fdata files found in ${INST_DATA}"
    exit 1
fi

LD_PRELOAD=/usr/lib/libmimalloc.so "${BOLTPATH}/merge-fdata" *.fdata > combined.fdata

optimize_binary() {
    local binary="$1"
    local profile="combined.fdata"

    if [ ! -f "${binary}.org" ]; then
        echo "ERROR: Original binary ${binary}.org not found!"
        exit 1
    fi

    echo "Optimizing ${binary}"
    LD_PRELOAD=/usr/lib/libmimalloc.so "${BOLTPATH}/llvm-bolt" "${binary}.org" \
        --data "${profile}" \
        -o "${binary}" \
        --lite=false \
        --reorder-functions=cdsort \
        --reorder-functions-use-hot-size \
        --reorder-blocks=ext-tsp \
        --split-functions --split-strategy=cdsplit \
        --hugify \
        --icf=all \
        --peepholes=all \
        --reg-reassign --use-aggr-reg-reassign \
        --align-functions=32 --align-blocks --block-alignment=16 \
        --indirect-call-promotion=all --indirect-call-promotion-topn=3 \
        --jump-tables=move \
        --dyno-stats \
        --plt=all || { echo "BOLT optimization failed for ${binary}"; exit 1; }
}

# Optimize binaries
optimize_binary "${CPATH}/clang-22"
optimize_binary "${CPATH}/lld"

echo "Optimized binaries are ready at ${CPATH}"
echo "Add to PATH with: export PATH=${CPATH}:\${PATH}"
