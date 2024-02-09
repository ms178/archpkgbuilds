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
        -D CMAKE_C_FLAGS="-O3 -g3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fno-omit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread -Wno-error -ffp-contract=fast -fdata-sections -ffunction-sections -fno-unique-section-names -fsplit-machine-functions" \
        -D CMAKE_CXX_FLAGS="-O3 -g3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fno-omit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement -mllvm -enable-gvn-hoist -mllvm -enable-dfa-jump-thread -Wno-error -ffp-contract=fast -fdata-sections -ffunction-sections -fno-unique-section-names -fsplit-machine-functions" \
        -D CMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-interleave-small-loop-scalar-reduction -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement -Wl,-mllvm -Wl,-enable-gvn-hoist -Wl,-mllvm -Wl,-enable-dfa-jump-thread -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--undefined-version" \
        -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-interleave-small-loop-scalar-reduction -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement -Wl,-mllvm -Wl,-enable-gvn-hoist -Wl,-mllvm -Wl,-enable-dfa-jump-thread -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--undefined-version" \
        -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-interleave-small-loop-scalar-reduction -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement -Wl,-mllvm -Wl,-enable-gvn-hoist -Wl,-mllvm -Wl,-enable-dfa-jump-thread -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--undefined-version" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;openmp;compiler-rt;bolt" \
        -D CLANG_ENABLE_ARCMT=ON \
        -D CLANG_ENABLE_STATIC_ANALYZER=ON \
        -D LLVM_INCLUDE_BENCHMARKS=OFF \
        -D LLVM_INCLUDE_TESTS=OFF \
        -D LLVM_INCLUDE_EXAMPLES=OFF \
        -D LLVM_BUILD_DOCS=OFF \
        -D LLVM_INCLUDE_DOCS=OFF \
        -D LLVM_ENABLE_OCAMLDOC=OFF \
        -D LLVM_ENABLE_SPHINX=OFF \
        -D LLVM_ENABLE_DOXYGEN=OFF \
        -D LLVM_ENABLE_BINDINGS=OFF \
        -D LLVM_ENABLE_Z3_SOLVER=OFF \
        -D LLVM_ENABLE_ZSTD=ON \
    -DLLVM_VP_COUNTERS_PER_SITE=6 \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-train/install || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)

echo "Merging PGO-Profiles"

cd ${TOPLEV}/stage2-prof-gen/profiles
${TOPLEV}/stage1/bin/llvm-profdata merge -output=clang.profdata *
