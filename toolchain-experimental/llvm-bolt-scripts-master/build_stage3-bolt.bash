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
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="lld;polly;compiler-rt;clang" \
        -D CMAKE_CXX_STANDARD=17 \
        -D CMAKE_C_STANDARD=17 \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=30G -F 1500 -e cycles:u -j any,u -- ninja clang || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggreated form suitable to be consumed by BOLT"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/clang-15 \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/clang-15.fdata || (echo "Could not convert perf-data to bolt for clang-15"; exit 1)

echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-15 \
    -o ${CPATH}/clang-15.bolt \
    --data ${TOPLEV}/clang-15.fdata \
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
    -tail-duplication=cache || (echo "Could not optimize binary for clang"; exit 1)

echo "move bolted binary to clang-15"
mv ${CPATH}/clang-15 ${CPATH}/clang-15.org
mv ${CPATH}/clang-15.bolt ${CPATH}/clang-15

echo "You can now use the compiler with export PATH=${CPATH}"
