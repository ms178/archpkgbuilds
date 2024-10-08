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
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
        -D CMAKE_C_FLAGS="-O2 -g3 -march=native -mtune=native -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
        -D CMAKE_CXX_FLAGS="-O2 -g3 -march=native -mtune=native -falign-functions=32 -fno-semantic-interposition -fno-omit-frame-pointer -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
        -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O2,-O2,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
        -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O2,-O2,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
        -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O2,-O2,-Bsymbolic-functions,--as-needed -fuse-ld=lld -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
        -DLLVM_ENABLE_PROJECTS="lld;clang" \
        -DLLVM_TARGETS_TO_BUILD="X86" \
        -D CLANG_ENABLE_ARCMT=OFF \
        -D CLANG_ENABLE_STATIC_ANALYZER=OFF \
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
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_USE_PERF=ON \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage2-prof-gen/install \
    -DLLVM_BUILD_INSTRUMENTED=IR \
    -DLLVM_VP_COUNTERS_PER_SITE=6 \
    -DLLVM_LINK_LLVM_DYLIB=ON \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DLLVM_BUILD_RUNTIME=No || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja || (echo "Could not build project!"; exit 1)
