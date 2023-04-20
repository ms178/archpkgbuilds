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
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DLLVM_USE_PERF=ON \
        -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin" \
        -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin" \
        -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -flto=thin" \
        -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -flto=thin" \
        -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -flto=thin" \
    -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86;BPF" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;openmp;compiler-rt" \
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
    -DLLVM_ENABLE_PLUGINS=ON \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-train/install || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)

echo "Merging PGO-Profiles"

cd ${TOPLEV}/stage2-prof-gen/profiles
${TOPLEV}/stage1/bin/llvm-profdata merge -output=clang.profdata *
