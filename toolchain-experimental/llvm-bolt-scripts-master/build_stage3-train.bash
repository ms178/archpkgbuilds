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
        -D CMAKE_C_FLAGS="-O3 -march=native -mllvm -polly -mllvm -polly-position=early -mllvm -polly-parallel=true -fopenmp -fopenmp-version=50 -mllvm -polly-dependences-computeout=0 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-omp-backend=LLVM -mllvm -polly-num-threads=36 -mllvm -polly-scheduling=dynamic -mllvm -polly-scheduling-chunksize=1 -mllvm -polly-ast-use-context -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -flto=thin" \
        -D CMAKE_CXX_FLAGS="-O3 -march=native -mllvm -polly -mllvm -polly-position=early -mllvm -polly-parallel=true -fopenmp -fopenmp-version=50 -mllvm -polly-dependences-computeout=0 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-omp-backend=LLVM -mllvm -polly-num-threads=36 -mllvm -polly-scheduling=dynamic -mllvm -polly-scheduling-chunksize=1 -mllvm -polly-ast-use-context -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -flto=thin" \
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
    -DLLVM_ENABLE_PLUGINS=ON \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-train/install || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)

echo "Merging PGO-Profiles"

cd ${TOPLEV}/stage2-prof-gen/profiles
${TOPLEV}/stage1/bin/llvm-profdata merge -output=clang.profdata *
