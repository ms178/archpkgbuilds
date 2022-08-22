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
    -DLLVM_TARGETS_TO_BUILD="X86;AMDGPU" \
    -DLLVM_ENABLE_PROJECTS="lld;polly;compiler-rt;clang;openmp" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -flto=thin" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -flto=thin" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -flto=thin" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -flto=thin" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -flto=thin" \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=30G -F 1500 -e cycles:u -j any,u -- ninja clang || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggreated form suitable to be consumed by BOLT"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/clang-16 \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/clang-16.fdata || (echo "Could not convert perf-data to bolt for clang-16"; exit 1)

echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-16 \
    -o ${CPATH}/clang-16.bolt \
    --data ${TOPLEV}/clang-16.fdata \
    -relocs \
    -split-functions \
    -split-all-cold \
    -icf=1 \
    -lite=1 \
    -split-eh \
    -use-gnu-stack \
    -jump-tables=move \
    -dyno-stats \
    -reorder-functions=hfsort \
    -reorder-blocks=ext-tsp \
    -plt=all || (echo "Could not optimize binary for clang"; exit 1)

echo "move bolted binary to clang-16"
mv ${CPATH}/clang-16 ${CPATH}/clang-16.org
mv ${CPATH}/clang-16.bolt ${CPATH}/clang-16

echo "You can now use the compiler with export PATH=${CPATH}"
