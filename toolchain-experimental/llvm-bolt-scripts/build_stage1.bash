#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || (echo "Could not enter ${TOPLEV} directory"; exit 1)

mkdir -p stage1 || (echo "Could not create stage1 directory"; exit 1)
cd stage1 || (echo "Could not enter stage 1 directory"; exit 1)

echo "== Configure Build"
echo "== Build with system clang"

cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_BUILD_TYPE=Release \
    -DCLANG_ENABLE_ARCMT=OFF \
    -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_BUILD_UTILS=OFF \
    -DLLVM_ENABLE_BACKTRACES=OFF \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_ENABLE_TERMINFO=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage1/install || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)

echo "== Install to $(pwd)/install"
ninja install || (echo "Could not install project!"; exit 1)
