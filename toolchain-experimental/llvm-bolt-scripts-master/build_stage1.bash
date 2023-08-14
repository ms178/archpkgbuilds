#!/bin/bash

TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || (echo "Could not enter ${TOPLEV} directory"; exit 1)
mkdir -p ${TOPLEV}
git clone --depth=1 https://github.com/llvm/llvm-project.git

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
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_USE_PERF=ON \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -fvisibility=hidden -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -maes -mbmi2 -mpclmul -mllvm -extra-vectorizer-passes -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fomit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -fvisibility=hidden -freroll-loops -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fvisibility=hidden -Wl,--thinlto-jobs=8 -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fvisibility=hidden -Wl,--thinlto-jobs=8 -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fuse-ld=lld -maes -mbmi2 -mpclmul -flto=thin -fwhole-program-vtables -fvisibility=hidden -Wl,--thinlto-jobs=8 -Wl,--push-state -Wl,-whole-archive -ljemalloc_pic -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -fprofile-instr-use=/home/marcus/Downloads/llvm17.profdata" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_BUILD_UTILS=OFF \
    -DLLVM_ENABLE_BACKTRACES=OFF \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_ENABLE_TERMINFO=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/llvm-bolt || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja install || (echo "Could not build project!"; exit 1)
