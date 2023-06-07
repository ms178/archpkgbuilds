#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage3-bolt  || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-bolt
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/llvm-bolt/bin



echo "== Configure Build"
echo "== Build with stage2-prof-use-tools -- $CPATH"

cmake -G Ninja \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$(pwd)/install" \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DLLVM_USE_PERF=ON \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="clang" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mllvm -inline-threshold=1000 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -mllvm -vp-counters-per-site=6 -fcf-protection=none -mharden-sls=none -flto=thin -fno-split-lto-unit -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mllvm -inline-threshold=1000 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -mllvm -vp-counters-per-site=6 -fcf-protection=none -mharden-sls=none -flto=thin -fno-split-lto-unit -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--thinlto-jobs=all" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--thinlto-jobs=all" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--thinlto-jobs=all" \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=11G -F 1600 -e cycles:u -j any,u -- ninja clang || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggreated form suitable to be consumed by BOLT"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/clang-17 \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/clang-17.fdata || (echo "Could not convert perf-data to bolt for clang-17"; exit 1)

echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-17 \
    -o ${CPATH}/clang-17.bolt \
    --data ${TOPLEV}/clang-17.fdata \
    -reorder-blocks=ext-tsp \
    -reorder-functions=hfsort+ \
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
    -hot-data -x86-strip-redundant-address-size -lite=false \
    -plt=hot || (echo "Could not optimize binary for clang"; exit 1)

echo "move bolted binary to clang-17"
mv ${CPATH}/clang-17 ${CPATH}/clang-17.org
mv ${CPATH}/clang-17.bolt ${CPATH}/clang-17

echo "You can now use the compiler with export PATH=${CPATH}"
