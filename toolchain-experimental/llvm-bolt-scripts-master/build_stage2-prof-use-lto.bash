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
    -DCLANG_ENABLE_ARCMT=ON \
    -DCLANG_ENABLE_STATIC_ANALYZER=ON \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF  \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_USE_PERF=OFF \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread -ffp-contract=fast -fdata-sections -ffunction-sections -fno-unique-section-names -fprofile-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread -ffp-contract=fast -fdata-sections -ffunction-sections -fno-unique-section-names -fprofile-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -fprofile-use=/home/marcus/Downloads/llvm17.profdata -Wl,-mllvm,-enable-ext-tsp-block-placement -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -fprofile-use=/home/marcus/Downloads/llvm17.profdata -Wl,-mllvm,-enable-ext-tsp-block-placement -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -fprofile-use=/home/marcus/Downloads/llvm17.profdata -Wl,-mllvm,-enable-ext-tsp-block-placement -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
        -DLLVM_ENABLE_PROJECTS="polly;lld;clang;openmp;compiler-rt;bolt" \
        -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86;BPF" \
        -DLIBOMP_ARCH="i386;x86_64" \
        -DLIBOMP_USE_DEBUGGER=OFF \
        -D LLVM_INCLUDE_BENCHMARKS=OFF \
        -D LLVM_INCLUDE_TESTS=OFF \
        -D LLVM_ENABLE_DIA_SDK=OFF \
        -D LLVM_BUILD_DOCS=OFF \
        -D LLVM_ENABLE_SPHINX=OFF \
        -D LLVM_ENABLE_DOXYGEN=OFF \
        -D LLVM_POLLY_LINK_INTO_TOOLS=ON \
        -D LLVM_ENABLE_ZLIB=ON \
        -D LLVM_ENABLE_ZSTD=ON \
        -D LIBCLANG_BUILD_STATIC=ON \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage2-prof-use-lto/install \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DLLVM_ENABLE_TERMINFO=OFF  || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja install || (echo "Could not build project!"; exit 1)
