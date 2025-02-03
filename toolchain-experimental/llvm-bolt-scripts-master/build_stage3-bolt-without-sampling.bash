#!/bin/bash
set -euo pipefail

TOPLEV=~/toolchain/llvm
STAGE3_DIR="${TOPLEV}/stage3-without-sampling"
INST_DATA="${STAGE3_DIR}/instrumentdata"
CPATH="${TOPLEV}/stage2-prof-use-lto/install/bin"
BOLTPATH="${TOPLEV}/llvm-bolt/bin"

mkdir -p "${INST_DATA}"
cd "${STAGE3_DIR}"

instrument_binary() {
    local binary="$1"
    local output="${binary}.inst"
    local profile="${INST_DATA}/${binary##*/}.fdata"

    echo "Instrumenting ${binary}"
    "${BOLTPATH}/llvm-bolt" \
        --instrument \
        --lite=false \
        --instrumentation-file-append-pid \
        --instrumentation-file="${profile}" \
        "${binary}" \
        -o "${output}"

    mv "${binary}" "${binary}.org"
    mv "${output}" "${binary}"
}

# Instrument binaries
instrument_binary "${CPATH}/clang-21"
instrument_binary "${CPATH}/lld"

# Configure build
cmake -G Ninja ../llvm-project/llvm \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=60000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=60000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -DLLVM_VP_COUNTERS_PER_SITE=10 \
    -DCMAKE_AR="${CPATH}/llvm-ar" \
    -DCMAKE_C_COMPILER="${CPATH}/clang-21" \
    -DCMAKE_CXX_COMPILER="${CPATH}/clang++" \
    -DLLVM_USE_LINKER="${CPATH}/ld.lld" \
    -DCMAKE_RANLIB="${CPATH}/llvm-ranlib" \
    -DCMAKE_INSTALL_PREFIX="${STAGE3_DIR}/install"

# Build with timeout
timeout 120s ninja || true

echo "Merging generated profiles"
cd ${TOPLEV}/stage3-without-sampling/instrumentdata
LD_PRELOAD=/usr/lib/libmimalloc.so ${BOLTPATH}/merge-fdata *.fdata > combined.fdata

optimize_binary() {
    local binary="$1"
    local profile="combined.fdata"

    echo "Optimizing ${binary}"
    LD_PRELOAD=/usr/lib/libmimalloc.so "${BOLTPATH}/llvm-bolt" "${binary}.org" \
        --data "${profile}" \
        -o "${binary}" \
        --dyno-stats \
        --lite=false \
        --icf=all \
        --plt=all \
        --hugify \
        --peepholes=all \
        --x86-strip-redundant-address-size \
        --indirect-call-promotion=all \
        --reorder-blocks=ext-tsp \
        --reorder-functions=cdsort \
        --split-all-cold \
        --split-eh \
        --split-functions \
        --split-strategy=cdsplit \
        --redirect-never-taken-jumps || return 1
}

# Optimize binaries
optimize_binary "${CPATH}/clang-21"
optimize_binary "${CPATH}/lld"

echo "Optimized binaries are ready at ${CPATH}"
echo "Add to PATH with: export PATH=${CPATH}:\${PATH}"
