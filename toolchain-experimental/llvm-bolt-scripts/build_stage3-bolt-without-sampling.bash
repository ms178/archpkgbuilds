#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir -p ${TOPLEV}/stage3-without-sampling/intrumentdata || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-without-sampling
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/stage1/bin


echo "Instrument clang with llvm-bolt"

${BOLTPATH}/llvm-bolt \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/stage3-without-sampling/intrumentdata/clang-15.fdata \
    ${CPATH}/clang-15 \
    -o ${CPATH}/clang-15.inst

echo "mooving instrumented binary"
mv ${CPATH}/clang-15 ${CPATH}/clang-15.org
mv ${CPATH}/clang-15.inst ${CPATH}/clang-15

echo "== Configure Build"
echo "== Build with stage2-prof-use-lto instrumented clang -- $CPATH"

cmake -G Ninja ../llvm-project/llvm \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_HOST_TRIPLE=x86_64-unknown-linux \
    -DLLVM_BUILD_RUNTIME=ON \
    -DLLVM_POLLY_LINK_INTO_TOOLS=ON \
    -DLLVM_ENABLE_RUNTIMES="openmp" \
    -DOPENMP_ENABLE_LIBOMPTARGET=OFF \
    -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;polly;compiler-rt;clang-tools-extra" \
    -D LLVM_BUILD_LLVM_DYLIB=ON \
    -D LLVM_LINK_LLVM_DYLIB=ON \
    -D CLANG_LINK_CLANG_DYLIB=ON \
    -DCMAKE_AR=${CPATH}/llvm-ar \
    -DCMAKE_C_COMPILER=${CPATH}/clang-15 \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-without-sampling/install

echo "== Start Training Build"
ninja & read -t 240 || kill $!

echo "Merging generated profiles"
cd ${TOPLEV}/stage3-without-sampling/intrumentdata
${BOLTPATH}/merge-fdata *.fdata > combined.fdata
echo "Optimizing Clang with the generated profile"

${BOLTPATH}/llvm-bolt ${CPATH}/clang-15.org \
    --data combined.fdata \
    -o ${CPATH}/clang-15 \
    -reorder-blocks=ext-tsp \
    -reorder-functions=hfsort+ \
    -split-functions=3 \
    -split-all-cold \
    -dyno-stats \
    -icf=1 \
    -use-gnu-stack || (echo "Could not optimize binary for clang-15"; exit 1)

echo "You can now use the compiler with export PATH=${CPATH}:${PATH}"
