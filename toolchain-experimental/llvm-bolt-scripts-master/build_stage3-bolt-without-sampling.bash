#!/bin/bash

TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir -p ${TOPLEV}/stage3-without-sampling/intrumentdata || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-without-sampling
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/llvm-bolt/bin


echo "Instrument clang with llvm-bolt"

${BOLTPATH}/llvm-bolt \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/stage3-without-sampling/intrumentdata/clang-18.fdata \
    ${CPATH}/clang-18 \
    -o ${CPATH}/clang-18.inst

echo "mooving instrumented binary"
mv ${CPATH}/clang-18 ${CPATH}/clang-18.org
mv ${CPATH}/clang-18.inst ${CPATH}/clang-18

echo "== Configure Build"
echo "== Build with stage2-prof-use-lto instrumented clang -- $CPATH"

cmake -G Ninja ../llvm-project/llvm \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;compiler-rt" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -mllvm -vp-counters-per-site=6 -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -mllvm -vp-counters-per-site=6 -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -maes -mbmi2 -mpclmul -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -DCMAKE_AR=${CPATH}/llvm-ar \
    -DCMAKE_C_COMPILER=${CPATH}/clang-18 \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-without-sampling/install

echo "== Start Training Build"
ninja & read -t 360 || kill $!

echo "Merging generated profiles"
cd ${TOPLEV}/stage3-without-sampling/intrumentdata
LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/merge-fdata *.fdata > combined.fdata
echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-18.org \
    --data combined.fdata \
    -o ${CPATH}/clang-18 \
    -reorder-blocks=ext-tsp \
    -reorder-functions=cds \
    -split-functions \
    -split-all-cold \
    -split-eh \
    -dyno-stats \
    -icf=1 \
    -use-gnu-stack \
    -peepholes=all \
    -elim-link-veneers \
    -group-stubs -align-blocks -sctc-mode=heuristic -jump-tables=aggressive -simplify-conditional-tail-calls -simplify-rodata-loads \
    -align-macro-fusion=all -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy -cg-from-perf-data \
    -plt=hot|| (echo "Could not optimize binary for clang"; exit 1)

echo "You can now use the compiler with export PATH=${CPATH}:${PATH}"
