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
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;compiler-rt" \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=6G -F 1700 -e cycles:u -j any,u -- ninja clang || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggreated form suitable to be consumed by BOLT"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/clang-20 \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/clang-20.fdata || (echo "Could not convert perf-data to bolt for clang"; exit 1)

echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-20 \
    -o ${CPATH}/clang-20.bolt \
    --data ${TOPLEV}/clang-20.fdata \
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
    -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    --match-profile-with-function-hash -use-gnu-stack --reg-reassign --use-aggr-reg-reassign \
    -plt=hot || (echo "Could not optimize LLD binary"; exit 1)

echo "Optimizing LLD with the generated profile"

LD_PRELOAD=/usr/lib/libjemalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/lld \
    -o ${CPATH}/lld.bolt \
    --data ${TOPLEV}/clang-20.fdata \
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
    -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    --match-profile-with-function-hash -use-gnu-stack --reg-reassign --use-aggr-reg-reassign \
    -plt=hot || (echo "Could not optimize LLD binary"; exit 1)

echo "move bolted binary to clang-20"
mv ${CPATH}/clang-20 ${CPATH}/clang-20.org
mv ${CPATH}/clang-20.bolt ${CPATH}/clang-20
mv ${CPATH}/lld ${CPATH}/lld.org
mv ${CPATH}/lld.bolt ${CPATH}/lld

echo "You can now use the compiler with export PATH=${CPATH}"
