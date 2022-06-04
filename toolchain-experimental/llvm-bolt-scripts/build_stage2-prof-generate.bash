#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage2-prof-gen || (echo "Could not create stage2-prof-generate directory"; exit 1)
cd ${TOPLEV}/stage2-prof-gen
CPATH=${TOPLEV}/stage1/bin

echo "== Configure Build"
echo "== Build with stage1-tools -- $CPATH"

cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCLANG_ENABLE_ARCMT=OFF \
    -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DCMAKE_AR=${CPATH}/llvm-ar \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCLANG_TABLEGEN=${CPATH}/clang-tblgen \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage2-prof-gen/install \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_LINK_LLVM_DYLIB=ON \
    -DLLVM_VP_COUNTERS_PER_SITE=6 \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_ENABLE_PLUGINS=ON \
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
    -DLLVM_BUILD_RUNTIME=Yes || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)
