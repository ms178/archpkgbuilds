#!/bin/bash
export TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || { echo "Could not enter TOPLEV directory: ${TOPLEV}"; exit 1; }

# --- Ensure the instrumented compiler exists ---
INSTRUMENTED_CLANG="${TOPLEV}/stage2-prof-gen/bin/clang"
if [[ ! -x "${INSTRUMENTED_CLANG}" ]]; then
    echo "ERROR: Instrumented compiler not found at ${INSTRUMENTED_CLANG}"
    echo "Please ensure build_stage2-prof-generate.bash ran successfully."
    exit 1
fi

# --- Setup Build Directory for the WORKLOAD ---
# We still build the workload in stage3-train, NOT in a profile directory
mkdir -p ${TOPLEV}/stage3-train || { echo "Could not create stage3-train directory"; exit 1; }
cd ${TOPLEV}/stage3-train || { echo "Could not enter stage3-train directory"; exit 1; }
CPATH=${TOPLEV}/stage2-prof-gen/bin # Path to the INSTRUMENTED compiler binaries

# --- Define Profile Source and Target Directories ---
# Based on your description, these are the two locations containing raw profiles:
PROFILE_SOURCE_DIR_1="${TOPLEV}/stage2-prof-gen/profiles"
PROFILE_SOURCE_DIR_2="${TOPLEV}/stage3-train/profiles"
# This is the target directory for the final merged profile:
FINAL_PROFILE_DIR="${TOPLEV}/stage2-prof-gen/profiles"
FINAL_PROFILE_PATH="${FINAL_PROFILE_DIR}/clang.profdata"

# Ensure the source and target directories exist (especially the target)
mkdir -p "${PROFILE_SOURCE_DIR_1}" || { echo "Could not create profile source dir 1: ${PROFILE_SOURCE_DIR_1}"; exit 1; }
mkdir -p "${PROFILE_SOURCE_DIR_2}" || { echo "Could not create profile source dir 2: ${PROFILE_SOURCE_DIR_2}"; exit 1; }
# Note: FINAL_PROFILE_DIR is usually the same as PROFILE_SOURCE_DIR_1, mkdir -p handles that.

echo "========================================================================="
echo " Starting Stage 3 Training Run"
echo " Using instrumented compiler from: ${CPATH}"
echo " Workload: Building LLVM/Clang/LLD in $(pwd)"
echo " Raw profiles expected in: ${PROFILE_SOURCE_DIR_1} AND ${PROFILE_SOURCE_DIR_2}"
echo " Final Output Profile Will Be Stored As: ${FINAL_PROFILE_PATH}"
echo "========================================================================="

# --- Configure the workload build using the instrumented compiler ---
# (CMake configuration remains the same as before - builds the workload)
cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_C_COMPILER=${CPATH}/clang \
    -DCMAKE_CXX_COMPILER=${CPATH}/clang++ \
    -DLLVM_USE_LINKER=${CPATH}/ld.lld \
    \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=2 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow" \
    \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="lld;clang" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    \
    -DCLANG_ENABLE_ARCMT=OFF \
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
    \
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/stage3-train/install \
    || { echo "Could not configure project for training run!"; exit 1; }

# --- Build the workload using the instrumented compiler ---
# NOTE: This ninja run likely generates .profraw files in the CURRENT
# directory (${TOPLEV}/stage3-train). The merge step below DOES NOT
# look here unless PROFILE_SOURCE_DIR_2 points here instead of profiles subdir.
echo "== Starting Training Build (ninja)... =="
ninja || echo "WARNING: Training build (ninja) finished with non-zero status. Profile merging will still be attempted."

# --- Merge Raw Profiles from the TWO SPECIFIED SOURCE Directories ---
echo "== Merging PGO Profiles... =="

# Use llvm-profdata from Stage 1 (or stage1/llvm-bolt) build - ensure it exists
PROFDATA_TOOL="${TOPLEV}/llvm-bolt/bin/llvm-profdata"
if [[ ! -x "${PROFDATA_TOOL}" ]]; then
    echo "ERROR: llvm-profdata tool not found at ${PROFDATA_TOOL}"
    exit 1
fi

echo "Searching for *.profraw files in:"
echo "  1: ${PROFILE_SOURCE_DIR_1}"
echo "  2: ${PROFILE_SOURCE_DIR_2}"
echo "Output will be: ${FINAL_PROFILE_PATH}"

# *** THE ACTUAL MERGE COMMAND ***
# Find *.profraw in BOTH specified source directories. Limit search depth.
# Use --no-run-if-empty to handle cases where no profiles exist in EITHER location.
find "${PROFILE_SOURCE_DIR_1}" "${PROFILE_SOURCE_DIR_2}" -maxdepth 1 -name "*.profraw" -print0 | \
    xargs -0 --no-run-if-empty "${PROFDATA_TOOL}" merge -output="${FINAL_PROFILE_PATH}"
MERGE_STATUS=$?

# --- Verify Profile Creation ---
if [[ ${MERGE_STATUS} -ne 0 ]]; then
    echo "ERROR: llvm-profdata merge command failed with status ${MERGE_STATUS}."
    echo "Check if any *.profraw files existed in ${PROFILE_SOURCE_DIR_1} or ${PROFILE_SOURCE_DIR_2}"
    ls -l "${PROFILE_SOURCE_DIR_1}"/*.profraw 2>/dev/null || echo "No *.profraw files found in ${PROFILE_SOURCE_DIR_1}."
    ls -l "${PROFILE_SOURCE_DIR_2}"/*.profraw 2>/dev/null || echo "No *.profraw files found in ${PROFILE_SOURCE_DIR_2}."
    exit 1
elif [[ ! -f "${FINAL_PROFILE_PATH}" ]]; then
    echo "ERROR: PGO profile merge ran, but output file ${FINAL_PROFILE_PATH} was NOT created."
    echo "Check permissions and available disk space. Also check if any input profiles were found."
    ls -l "${PROFILE_SOURCE_DIR_1}"/*.profraw 2>/dev/null || echo "No *.profraw files found in ${PROFILE_SOURCE_DIR_1}."
    ls -l "${PROFILE_SOURCE_DIR_2}"/*.profraw 2>/dev/null || echo "No *.profraw files found in ${PROFILE_SOURCE_DIR_2}."
    exit 1
else
    echo "PGO profile successfully generated (or updated) at ${FINAL_PROFILE_PATH}"
    ls -lh "${FINAL_PROFILE_PATH}"
    # Optional: Clean up raw profiles *after* successful merge from the source locations
    # find "${PROFILE_SOURCE_DIR_1}" -maxdepth 1 -name "*.profraw" -delete
    # find "${PROFILE_SOURCE_DIR_2}" -maxdepth 1 -name "*.profraw" -delete
    # echo "Cleaned up *.profraw files from source profile directories."
fi

echo "========================================================================="
echo " Stage 3 Training Run Complete."
echo "========================================================================="
