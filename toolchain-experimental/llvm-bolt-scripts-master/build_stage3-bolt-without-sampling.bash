#!/bin/bash

TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir -p ${TOPLEV}/stage3-without-sampling/instrumentdata || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-without-sampling
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/llvm-bolt/bin

echo "Instrument Clang with llvm-bolt"
${BOLTPATH}/llvm-bolt \
    --lite=false \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/stage3-without-sampling/instrumentdata/clang-19.fdata \
    ${CPATH}/clang-19 \
    -o ${CPATH}/clang-19.inst

echo "Moving instrumented Clang binary"
mv ${CPATH}/clang-19 ${CPATH}/clang-19.org
mv ${CPATH}/clang-19.inst ${CPATH}/clang-19

echo "Instrument LLD with llvm-bolt"
${BOLTPATH}/llvm-bolt \
    --lite=false \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/stage3-without-sampling/instrumentdata/lld-19.fdata \
    ${CPATH}/lld \
    -o ${CPATH}/lld.inst

echo "Moving instrumented LLD binary"
mv ${CPATH}/lld ${CPATH}/lld.org
mv ${CPATH}/lld.inst ${CPATH}/lld

echo "== Configure Build"
echo "== Build with stage2-prof-use-lto instrumented Clang and LLD -- $CPATH"

cmake -G Ninja ../llvm-project/llvm \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;compiler-rt" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -DLLVM_VP_COUNTERS_PER_SITE=6 \
    -DCMAKE_AR=${CPATH}/llvm-ar \
    -DCMAKE_C_COMPILER=${CPATH}/clang-19 \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-without-sampling/install

echo "== Start Training Build"
ninja & read -t 500 || kill $!

echo "Merging generated profiles"
cd ${TOPLEV}/stage3-without-sampling/instrumentdata
LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/merge-fdata *.fdata > combined.fdata
echo "Optimizing Clang and LLD with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-19.org \
    --data combined.fdata \
    -o ${CPATH}/clang-19 \
    -reorder-blocks=ext-tsp \
    -reorder-functions=cdsort \
    -split-functions \
    -split-strategy=cdsplit \
    -split-all-cold \
    -split-eh \
    -hugify \
    -dyno-stats \
    -strip-rep-ret \
    -icf=1 \
    -peepholes=all \
    -group-stubs -align-blocks -sctc-mode=heuristic -jump-tables=aggressive -simplify-conditional-tail-calls -simplify-rodata-loads \
    -align-macro-fusion=all -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    -plt=hot || (echo "Could not optimize Clang binary"; exit 1)

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/lld.org \
    --data combined.fdata \
    -o ${CPATH}/lld \
    -reorder-blocks=ext-tsp \
    -reorder-functions=cdsort \
    -split-functions \
    -split-strategy=cdsplit \
    -split-all-cold \
    -split-eh \
    -hugify \
    -dyno-stats \
    -strip-rep-ret \
    -icf=1 \
    -peepholes=all \
    -group-stubs -align-blocks -sctc-mode=heuristic -jump-tables=aggressive -simplify-conditional-tail-calls -simplify-rodata-loads \
    -align-macro-fusion=all -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    -plt=hot || (echo "Could not optimize LLD binary"; exit 1)

echo "You can now use the optimized Clang and LLD with export PATH=${CPATH}:${PATH}"
