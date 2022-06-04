#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || (echo "Could not enter ${TOPLEV} directory"; exit 1)

mkdir -p stage1 || (echo "Could not create stage1 directory"; exit 1)
cd stage1 || (echo "Could not enter stage 1 directory"; exit 1)

echo "== Configure Build"
echo "== Build with system clang"

cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_HOST_TRIPLE=x86_64-unknown-linux \
    -DLLVM_ENABLE_RUNTIMES="openmp" \
    -DOPENMP_ENABLE_LIBOMPTARGET=OFF \
    -DLLVM_POLLY_LINK_INTO_TOOLS=ON \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;polly;compiler-rt" \
        -D CMAKE_C_FLAGS="-O3 -march=native -g0 -Wp,-D_FORTIFY_SOURCE=0" \
        -D CMAKE_CXX_FLAGS="-O3 -march=native -g0 -Wp,-D_FORTIFY_SOURCE=0" \
        -D CMAKE_EXE_LINKER_FLAGS="-O3 -march=native -g0 -Wp,-D_FORTIFY_SOURCE=0 -Wl,--as-needed" \
        -D CMAKE_MODULE_LINKER_FLAGS="-O3 -march=native -g0 -Wp,-D_FORTIFY_SOURCE=0 -Wl,--as-needed" \
        -D CMAKE_SHARED_LINKER_FLAGS="-O3 -march=native -g0 -Wp,-D_FORTIFY_SOURCE=0 -Wl,--as-needed" \
        -D LLVM_BUILD_LLVM_DYLIB:BOOL=ON \
        -D LLVM_LINK_LLVM_DYLIB:BOOL=ON \
        -D CLANG_LINK_CLANG_DYLIB=ON \
        -D LLVM_BUILD_TOOLS:BOOL=ON \
        -D LLVM_BUILD_UTILS:BOOL=ON \
        -D CLANG_ENABLE_ARCMT:BOOL=OFF \
        -D CLANG_ENABLE_STATIC_ANALYZER:BOOL=OFF \
        -D COMPILER_RT_BUILD_SANITIZERS:BOOL=OFF \
        -D COMPILER_RT_BUILD_XRAY:BOOL=OFF \
        -D LLVM_INCLUDE_BENCHMARKS=OFF \
        -D LLVM_INCLUDE_GO_TESTS=OFF \
        -D LLVM_INCLUDE_TESTS=OFF \
        -D LLVM_INCLUDE_EXAMPLES=OFF \
        -D LLVM_BUILD_DOCS=OFF \
        -D LLVM_INCLUDE_DOCS=OFF \
        -D LLVM_ENABLE_OCAMLDOC=OFF \
        -D LLVM_ENABLE_SPHINX=OFF \
        -D LLVM_ENABLE_DOXYGEN=OFF \
        -D LLVM_ENABLE_BINDINGS=OFF \
        -D LLVM_ENABLE_Z3_SOLVER=OFF \
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
