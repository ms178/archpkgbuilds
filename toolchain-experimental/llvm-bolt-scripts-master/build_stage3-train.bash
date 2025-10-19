#!/bin/bash
set -euo pipefail

export TOPLEV=~/toolchain/llvm
cd "${TOPLEV}" || { echo "Could not enter TOPLEV directory: ${TOPLEV}"; exit 1; }

# FIX: Correct path to instrumented compiler
INSTRUMENTED_CLANG="${TOPLEV}/stage2-prof-gen/bin/clang"
if [[ ! -x "${INSTRUMENTED_CLANG}" ]]; then
    echo "ERROR: Instrumented compiler not found at ${INSTRUMENTED_CLANG}"
    echo "Please ensure build_stage2-prof-generate.bash ran successfully."
    exit 1
fi

# FIX: Set profile output location BEFORE build
PROFILE_SOURCE_DIR_1="${TOPLEV}/stage2-prof-gen/profiles"
PROFILE_SOURCE_DIR_2="${TOPLEV}/stage3-train/profiles"
FINAL_PROFILE_DIR="${TOPLEV}/stage2-prof-gen/profiles"
FINAL_PROFILE_PATH="${FINAL_PROFILE_DIR}/clang.profdata"

mkdir -p "${PROFILE_SOURCE_DIR_1}" || { echo "Could not create profile dir 1"; exit 1; }
mkdir -p "${PROFILE_SOURCE_DIR_2}" || { echo "Could not create profile dir 2"; exit 1; }

# FIX: Tell instrumented compiler where to write profiles
export LLVM_PROFILE_FILE="${PROFILE_SOURCE_DIR_2}/code-%p.profraw"

mkdir -p "${TOPLEV}/stage3-train" || { echo "Could not create stage3-train directory"; exit 1; }
cd "${TOPLEV}/stage3-train" || { echo "Could not enter stage3-train directory"; exit 1; }
CPATH="${TOPLEV}/stage2-prof-gen/bin"

echo "========================================================================="
echo " Starting Stage 3 Training Run"
echo " Using instrumented compiler from: ${CPATH}"
echo " Profiles will be written to: ${LLVM_PROFILE_FILE}"
echo " Final merged profile: ${FINAL_PROFILE_PATH}"
echo "========================================================================="

# [Keep all your cmake config - it's fine]
cmake -G Ninja "${TOPLEV}/llvm-project/llvm" \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_C_COMPILER="${CPATH}/clang" \
    -DCMAKE_CXX_COMPILER="${CPATH}/clang++" \
    -DLLVM_USE_LINKER="${CPATH}/ld.lld" \
    -DCMAKE_C_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -DCMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -DCMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    -DCMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    -DCMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="lld;clang" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DCLANG_ENABLE_OBJC_REWRITER=OFF \
    -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_USE_PERF=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_BUILD_DOCS=OFF \
    -DLLVM_ENABLE_SPHINX=OFF \
    -DLLVM_ENABLE_DOXYGEN=OFF \
    -DLLVM_ENABLE_Z3_SOLVER=ON \
    -DCLANG_BUILD_TOOLS=OFF \
    -DLLVM_ENABLE_ZLIB=ON \
    -DLLVM_ENABLE_ZSTD=ON \
    -DLLVM_ENABLE_LIBXML2=ON \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_ENABLE_PLUGINS=ON \
    -DCMAKE_INSTALL_PREFIX="${TOPLEV}/stage3-train/install" \
    || { echo "Could not configure project for training run!"; exit 1; }

echo "== Starting Training Build (ninja)... =="
# FIX: Fail on error
ninja || { echo "Training build failed!"; exit 1; }

echo "== Merging PGO Profiles... =="

PROFDATA_TOOL="${TOPLEV}/llvm-bolt/bin/llvm-profdata"
if [[ ! -x "${PROFDATA_TOOL}" ]]; then
    echo "ERROR: llvm-profdata tool not found at ${PROFDATA_TOOL}"
    exit 1
fi

echo "Searching for *.profraw files in:"
echo "  1: ${PROFILE_SOURCE_DIR_1}"
echo "  2: ${PROFILE_SOURCE_DIR_2}"

find "${PROFILE_SOURCE_DIR_1}" "${PROFILE_SOURCE_DIR_2}" -maxdepth 1 -name "*.profraw" -print0 | \
    xargs -0 --no-run-if-empty "${PROFDATA_TOOL}" merge -output="${FINAL_PROFILE_PATH}"

if [[ ! -f "${FINAL_PROFILE_PATH}" ]]; then
    echo "ERROR: PGO profile NOT created at ${FINAL_PROFILE_PATH}"
    exit 1
fi

echo "PGO profile successfully generated at ${FINAL_PROFILE_PATH}"
ls -lh "${FINAL_PROFILE_PATH}"

echo "========================================================================="
echo " Stage 3 Training Run Complete."
echo "========================================================================="
