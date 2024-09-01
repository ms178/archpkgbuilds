#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV}

mkdir ${TOPLEV}/stage3-bolt || (echo "Could not create stage3-bolt directory"; exit 1)
cd ${TOPLEV}/stage3-bolt
CPATH=${TOPLEV}/stage2-prof-use-lto/install/bin
BOLTPATH=${TOPLEV}/llvm-bolt/bin

echo "== Configure Build"
echo "== Build with stage2-prof-use-tools -- $CPATH"

cmake -G Ninja \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$(pwd)/install" \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    -DLLVM_USE_PERF=ON \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;compiler-rt" \
    -D CMAKE_C_FLAGS="-O3 -g3 -march=native -mtune=native -mllvm -inline-threshold=1500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=6000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fno-omit-frame-pointer -mprefer-vector-width=256 -flto -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -Wno-error -fprofile-use=/home/marcus/Downloads/all.profdata -fdata-sections -ffunction-sections -fno-unique-section-names -fsplit-machine-functions -mtls-dialect=gnu2 -w -fno-plt -fvisibility=hidden -fexcess-precision=fast -freciprocal-math -fcx-limited-range" \
    -D CMAKE_CXX_FLAGS="-O3 -g3 -march=native -mtune=native -mllvm -inline-threshold=1500 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-dependences-computeout=6000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -polly -fmerge-all-constants -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -mllvm -enable-interleaved-mem-accesses -mllvm -enable-masked-interleaved-mem-accesses -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fno-omit-frame-pointer -mprefer-vector-width=256 -flto -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -Wno-error -fprofile-use=/home/marcus/Downloads/all.profdata -fdata-sections -ffunction-sections -fno-unique-section-names -fsplit-machine-functions -mtls-dialect=gnu2 -w -fno-plt -fvisibility=hidden -fexcess-precision=fast -freciprocal-math -fcx-limited-range" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--emit-relocs -Wl,-znow -fvisibility=hidden" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--emit-relocs -Wl,-znow -fvisibility=hidden" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -march=native -mtune=native -fuse-ld=lld -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,--emit-relocs -Wl,-znow -fvisibility=hidden" \
    ../llvm-project/llvm || (echo "Could not configure project!"; exit 1)

echo "== Start Training Build"
perf record -o ${TOPLEV}/perf.data --max-size=10G -F 1600 -e cycles:u -j any,u -- ninja clang lld || (echo "Could not build project for training!"; exit 1)

cd ${TOPLEV}

echo "Converting profile to a more aggregated form suitable to be consumed by BOLT"

# Converting Clang perf data
LD_PRELOAD=/usr/lib/libmimalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/clang-20 \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/clang-20.fdata || (echo "Could not convert perf-data to bolt for clang-20"; exit 1)

# Converting LLD perf data
LD_PRELOAD=/usr/lib/libmimalloc.so ${BOLTPATH}/perf2bolt ${CPATH}/ld.lld \
    -p ${TOPLEV}/perf.data \
    -o ${TOPLEV}/lld.fdata || (echo "Could not convert perf-data to bolt for ld.lld"; exit 1)

echo "Optimizing Clang with the generated profile"

LD_PRELOAD=/usr/lib/libmimalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/clang-20 \
    -o ${CPATH}/clang-20.bolt \
    --data ${TOPLEV}/clang-20.fdata \
    -reorder-blocks=ext-tsp \
    -reorder-functions=cdsort \
    -split-functions \
    -split-strategy=cdsplit \
    -split-all-cold \
    -split-eh \
    -hugify \
    -dyno-stats \
    -strip-rep-ret \
    -icf=1 \
    -peepholes=all \
    -group-stubs -align-blocks -sctc-mode=heuristic -jump-tables=aggressive -simplify-conditional-tail-calls -simplify-rodata-loads \
    -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    --match-profile-with-function-hash -use-gnu-stack --reg-reassign --use-aggr-reg-reassign \
    -plt=hot || (echo "Could not optimize Clang binary"; exit 1)

echo "move bolted binary to clang-20"
mv ${CPATH}/clang-20 ${CPATH}/clang-20.org
mv ${CPATH}/clang-20.bolt ${CPATH}/clang-20

echo "Optimizing LLD with the generated profile"

LD_PRELOAD=/usr/lib/libmimalloc.so ${BOLTPATH}/llvm-bolt ${CPATH}/ld.lld \
    -o ${CPATH}/ld.lld.bolt \
    --data ${TOPLEV}/lld.fdata \
    -reorder-blocks=ext-tsp \
    -reorder-functions=cdsort \
    -split-functions \
    -split-strategy=cdsplit \
    -split-all-cold \
    -split-eh \
    -hugify \
    -dyno-stats \
    -strip-rep-ret \
    -icf=1 \
    -peepholes=all \
    -group-stubs -align-blocks -sctc-mode=heuristic -jump-tables=aggressive -simplify-conditional-tail-calls -simplify-rodata-loads \
    -eliminate-unreachable -tail-duplication=cache -indirect-call-promotion=all -icp-eliminate-loads \
    -hot-data -x86-strip-redundant-address-size -lite=false -reorder-data-algo=funcs -inline-memcpy \
    --match-profile-with-function-hash -use-gnu-stack --reg-reassign --use-aggr-reg-reassign \
    -plt=hot || (echo "Could not optimize LLD binary"; exit 1)

echo "move bolted binary to ld.lld"
mv ${CPATH}/ld.lld ${CPATH}/ld.lld.org
mv ${CPATH}/ld.lld.bolt ${CPATH}/ld.lld

echo "You can now use the compiler and linker with export PATH=${CPATH}"
