#!/bin/bash
export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage3-train
cd ${TOPLEV}/stage3-train
CPATH=${TOPLEV}/stage2-prof-gen/bin

echo "Generating Profile for PGO"

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
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DCMAKE_RANLIB=${CPATH}/llvm-ranlib \
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
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-train/install || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)

echo "Merging PGO-Profiles"

cd ${TOPLEV}/stage2-prof-gen/profiles
${TOPLEV}/stage1/install/bin/llvm-profdata merge -output=clang.profdata *
