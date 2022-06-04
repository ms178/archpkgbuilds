#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage3-bolt  || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-bolt
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/stage1/bin



echo "== Configure Build"
echo "== Build with stage2-prof-use-tools -- $CPATH"

cmake -G Ninja \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$(pwd)/install" \
    -DCMAKE_AR=${CPATH}/llvm-ar \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCLANG_TABLEGEN=${CPATH}/clang-tblgen \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DLLVM_TABLEGEN=${CPATH}/llvm-tblgen \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
    -DLLVM_HOST_TRIPLE=x86_64-unknown-linux \
    -DCMAKE_PREFIX_PATH=${TOPLEV}/stage1/lib/cmake/llvm \
    -DLLVM_BUILD_RUNTIME=ON \
    -DLLVM_POLLY_LINK_INTO_TOOLS=ON \
    -DLLVM_ENABLE_RUNTIMES="openmp" \
    -DOPENMP_ENABLE_LIBOMPTARGET=OFF \
    -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;polly;compiler-rt;clang-tools-extra" \
    -D LLVM_BUILD_LLVM_DYLIB=ON \
    -D LLVM_LINK_LLVM_DYLIB=ON \
    -D CLANG_LINK_CLANG_DYLIB=ON \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=10G -F 1500 -e cycles:u -j any,u -- ninja clang || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggreated form suitable to be consumed by BOLT"

${BOLTPATH}/perf2bolt ${CPATH}/clang-15 \
    -p ${TOPLEV}perf.data \
    -o ${TOPLEV}/clang-15.fdata || (echo "Could not convert perf-data to bolt for clang-15"; exit 1)

echo "Optimizing Clang with the generated profile"

${BOLTPATH}/llvm-bolt ${CPATH}/clang-15 \
    -o ${CPATH}/clang-15.bolt \
    --data ${TOPLEV}/clang-15.fdata \
    -reorder-blocks=ext-tsp \
    -reorder-functions=hfsort+ \
    -split-functions=3 \
    -split-all-cold \
    -dyno-stats \
    -icf=1 \
    -use-gnu-stack || (echo "Could not optimize binary for clang-15"; exit 1)

echo "move bolted binary to clang-15"
mv ${CPATH}/clang-15 ${CPATH}/clang-15.org
mv ${CPATH}/clang-15.bolt ${CPATH}/clang-15

echo "You can now use the compiler with export PATH=${CPATH}"
