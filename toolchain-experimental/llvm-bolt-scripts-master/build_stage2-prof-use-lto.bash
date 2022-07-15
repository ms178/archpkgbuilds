#!/bin/bash
TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

echo "Building Clang with PGO and LTO"

mkdir ${TOPLEV}/stage2-prof-use-lto
cd ${TOPLEV}/stage2-prof-use-lto
CPATH=${TOPLEV}/llvm-bolt/bin

echo "== Configure Build"
echo "== Build with stage1-tools -- $CPATH"

cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCLANG_ENABLE_ARCMT=OFF \
    -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF  \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
        -D LLVM_ENABLE_PROJECTS="polly;lld;compiler-rt;clang" \
        -D LLVM_ENABLE_RUNTIMES="openmp" \
        -D CMAKE_CXX_STANDARD=17 \
        -D CMAKE_C_STANDARD=17 \
        -D LLVM_TARGETS_TO_BUILD="AMDGPU;X86" \
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
        -D POLLY_ENABLE_GPGPU_CODEGEN=OFF \
        -D LLVM_ENABLE_Z3_SOLVER=OFF \
        -D LLVM_POLLY_LINK_INTO_TOOLS=ON \
        -D LLVM_ENABLE_ZLIB:BOOL=ON \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage2-prof-use-lto/install \
    -DLLVM_PROFDATA_FILE=${TOPLEV}/stage2-prof-gen/profiles/clang.profdata \
    -DLLVM_ENABLE_LTO=Thin \
    -DCMAKE_EXE_LINKER_FLAGS="-Wl,--emit-relocs" \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DLLVM_ENABLE_TERMINFO=OFF  || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja install || (echo "Could not build project!"; exit 1)
