#!/bin/bash

TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage2-prof-gen || (echo "Could not create stage2-prof-generate directory"; exit 1)
cd ${TOPLEV}/stage2-prof-gen
CPATH=${TOPLEV}/llvm-bolt/bin

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
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
        -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
        -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
        -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -maes -flto=thin -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
        -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -maes -flto=thin -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
        -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fuse-ld=lld -maes -flto=thin -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
        -DLLVM_ENABLE_PROJECTS="polly;lld;clang" \
        -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86;BPF" \
        -DLLVM_ENABLE_RUNTIMES="openmp;compiler-rt" \
        -D CLANG_ENABLE_ARCMT:BOOL=OFF \
        -D CLANG_ENABLE_STATIC_ANALYZER:BOOL=OFF \
        -D COMPILER_RT_BUILD_SANITIZERS:BOOL=OFF \
        -D COMPILER_RT_BUILD_XRAY:BOOL=OFF \
        -D LLVM_INCLUDE_BENCHMARKS=OFF \
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
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage2-prof-gen/install \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_BUILD_RUNTIME=OFF \
    -DLLVM_LINK_LLVM_DYLIB=ON \
    -DLLVM_VP_COUNTERS_PER_SITE=6 \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DLLVM_BUILD_RUNTIME=No || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)
