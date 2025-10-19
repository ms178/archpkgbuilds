#!/bin/bash
set -euo pipefail

TOPLEV=~/toolchain/llvm
STAGE2_COMPILER_PATH="${TOPLEV}/llvm-bolt/bin"
PROFILE_DATA="${TOPLEV}/stage2-prof-gen/profiles/clang.profdata"

# Validate profile exists
if [ ! -f "${PROFILE_DATA}" ]; then
    echo "Error: PGO profile not found at ${PROFILE_DATA}"
    echo "Run build_stage3-train.bash first to generate profiles"
    exit 1
fi

echo "Building Clang with PGO and LTO"
echo "Using profile: ${PROFILE_DATA}"

cd "${TOPLEV}" || { echo "Could not enter ${TOPLEV}"; exit 1; }

mkdir -p "${TOPLEV}/stage2-prof-use-lto"
cd "${TOPLEV}/stage2-prof-use-lto" || { echo "Could not enter stage2-prof-use-lto"; exit 1; }

echo "== Configure Build"
echo "== Build with stage1-tools -- ${STAGE2_COMPILER_PATH}"

cmake -G Ninja "${TOPLEV}/llvm-project/llvm" \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DCMAKE_BUILD_TYPE=Release \
    -DCLANG_ENABLE_STATIC_ANALYZER=ON \
    -DCLANG_PLUGIN_SUPPORT=OFF \
    -DLLVM_ENABLE_BINDINGS=OFF  \
    -DLLVM_ENABLE_OCAMLDOC=OFF \
    -DLLVM_INCLUDE_DOCS=OFF \
    -DLLVM_INCLUDE_EXAMPLES=OFF \
    -DLLVM_USE_PERF=ON \
    -DCMAKE_C_COMPILER="${STAGE2_COMPILER_PATH}/clang" \
    -DCMAKE_CXX_COMPILER="${STAGE2_COMPILER_PATH}/clang++" \
    -DLLVM_USE_LINKER="${STAGE2_COMPILER_PATH}/ld.lld" \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DCMAKE_C_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -DCMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -mllvm -inline-threshold=500 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto=thin -fwhole-program-vtables -fsplit-lto-unit -mllvm -adce-remove-loops -mllvm -enable-ext-tsp-block-placement=1 -mllvm -enable-gvn-hoist=1 -mllvm -enable-dfa-jump-thread=1 -fdata-sections -ffunction-sections -fno-unique-section-names" \
    -DCMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
    -DCMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
    -DCMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=4 -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -fcf-protection=none -mharden-sls=none -Wl,-mllvm -Wl,-extra-vectorizer-passes -Wl,-mllvm -Wl,-enable-cond-stores-vec -Wl,-mllvm -Wl,-slp-vectorize-hor-store -Wl,-mllvm -Wl,-enable-loopinterchange -Wl,-mllvm -Wl,-enable-loop-distribute -Wl,-mllvm -Wl,-enable-unroll-and-jam -Wl,-mllvm -Wl,-enable-loop-flatten -Wl,-mllvm -Wl,-unroll-runtime-multi-exit -Wl,-mllvm -Wl,-aggressive-ext-opt -Wl,-mllvm -Wl,-enable-interleaved-mem-accesses -Wl,-mllvm -Wl,-enable-masked-interleaved-mem-accesses -march=native -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,-mllvm -Wl,-adce-remove-loops -Wl,-mllvm -Wl,-enable-ext-tsp-block-placement=1 -Wl,-mllvm -Wl,-enable-gvn-hoist=1 -Wl,-mllvm -Wl,-enable-dfa-jump-thread=1 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl -Wl,-znow -Wl,--emit-relocs" \
    -DLLVM_ENABLE_PROJECTS="polly;lld;clang;bolt" \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt;openmp" \
    -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86;BPF" \
    -DLIBOMP_ARCH="i386;x86_64" \
    -DLIBOMP_USE_DEBUGGER=OFF \
    -DLLVM_INCLUDE_BENCHMARKS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
    -DLLVM_ENABLE_DIA_SDK=OFF \
    -DLLVM_BUILD_DOCS=OFF \
    -DLLVM_ENABLE_SPHINX=OFF \
    -DLLVM_ENABLE_DOXYGEN=OFF \
    -DLLVM_ENABLE_Z3_SOLVER=ON \
    -DLLVM_POLLY_LINK_INTO_TOOLS=ON \
    -DCLANG_BUILD_TOOLS=ON \
    -DCLANG_TOOL_APINOTES_TEST_BUILD=OFF \
    -DCLANG_ENABLE_OBJC_REWRITER=OFF \
    -DCLANG_TOOL_C_INDEX_TEST_BUILD=OFF \
    -DCLANG_TOOL_CLANG_CHECK_BUILD=OFF \
    -DCLANG_TOOL_CLANG_DIFF_BUILD=OFF \
    -DCLANG_TOOL_CLANG_EXTDEF_MAPPING_BUILD=OFF \
    -DCLANG_TOOL_CLANG_FORMAT_BUILD=OFF \
    -DCLANG_TOOL_CLANG_FUZZER_BUILD=OFF \
    -DCLANG_TOOL_CLANG_IMPORT_TEST_BUILD=OFF \
    -DCLANG_TOOL_CLANG_INSTALLAPI_BUILD=OFF \
    -DCLANG_TOOL_CLANG_LINKER_WRAPPER_BUILD=OFF \
    -DCLANG_TOOL_CLANG_NVLINK_WRAPPER_BUILD=OFF \
    -DCLANG_TOOL_CLANG_OFFLOAD_BUNDLER_BUILD=OFF \
    -DCLANG_TOOL_CLANG_OFFLOAD_PACKAGER_BUILD=OFF \
    -DCLANG_TOOL_CLANG_REFACTOR_BUILD=OFF \
    -DCLANG_TOOL_CLANG_REPL_BUILD=OFF \
    -DCLANG_TOOL_CLANG_SCAN_DEPS_BUILD=OFF \
    -DCLANG_TOOL_CLANG_SHLIB_BUILD=OFF \
    -DCLANG_TOOL_CLANG_SYCL_LINKER_BUILD=OFF \
    -DCLANG_TOOL_DIAGTOOL_BUILD=OFF \
    -DCLANG_TOOL_LIBCLANG_BUILD=OFF \
    -DCLANG_TOOL_SCAN_BUILD_BUILD=OFF \
    -DCLANG_TOOL_SCAN_BUILD_PY_BUILD=OFF \
    -DCLANG_TOOL_SCAN_VIEW_BUILD=OFF \
    -DLLVM_TOOL_BUGPOINT_BUILD=OFF \
    -DLLVM_TOOL_BUGPOINT_PASSES_BUILD=OFF \
    -DLLVM_TOOL_DSYMUTIL_BUILD=OFF \
    -DLLVM_TOOL_DXIL_DIS_BUILD=OFF \
    -DLLVM_TOOL_LLVM_AS_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_BCANALYZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_C_TEST_BUILD=OFF \
    -DLLVM_TOOL_LLVM_CAT_BUILD=OFF \
    -DLLVM_TOOL_LLVM_CFI_VERIFY_BUILD=OFF \
    -DLLVM_TOOL_LLVM_EXEGESIS_BUILD=OFF \
    -DLLVM_TOOL_LLVM_EXTRACT_BUILD=OFF \
    -DLLVM_TOOL_LLVM_GSYMUTIL_BUILD=OFF \
    -DLLVM_TOOL_LLVM_IFS_BUILD=OFF \
    -DLLVM_TOOL_LLVM_ISEL_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_ITANIUM_DEMANGLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_JITLINK_BUILD=OFF \
    -DLLVM_TOOL_LLVM_JITLISTENER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_LIBTOOL_DARWIN_BUILD=OFF \
    -DLLVM_TOOL_LLVM_MC_ASSEMBLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_MC_DISASSEMBLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_MCA_BUILD=OFF \
    -DLLVM_TOOL_LLVM_MICROSOFT_DEMANGLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_OPT_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_OPT_REPORT_BUILD=OFF \
    -DLLVM_TOOL_LLVM_PDBUTIL_BUILD=OFF \
    -DLLVM_TOOL_LLVM_READTAPI_BUILD=OFF \
    -DLLVM_TOOL_LLVM_REDUCE_BUILD=OFF \
    -DLLVM_TOOL_LLVM_REMARKUTIL_BUILD=OFF \
    -DLLVM_TOOL_LLVM_RUST_DEMANGLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_SPECIAL_CASE_LIST_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_TLI_CHECKER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_UNDNAME_BUILD=OFF \
    -DLLVM_TOOL_LLVM_XRAY_BUILD=OFF \
    -DLLVM_TOOL_LLVM_YAML_NUMERIC_PARSER_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_LLVM_YAML_PARSER_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_SPIRV_TOOLS_BUILD=OFF \
    -DLLVM_TOOL_VFABI_DEMANGLE_FUZZER_BUILD=OFF \
    -DLLVM_TOOL_XCODE_TOOLCHAIN_BUILD=OFF \
    -DLLVM_ENABLE_ZLIB=ON \
    -DLLVM_ENABLE_ZSTD=ON \
    -DLLVM_ENABLE_LIBXML2=ON \
    -DLLVM_THINLTO_CACHE_PATH="${TOPLEV}/llvm-thinlto" \
    -DLLVM_PROFDATA_FILE="${PROFILE_DATA}" \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DCMAKE_INSTALL_PREFIX="${TOPLEV}/stage2-prof-use-lto/install" \
    -DLLVM_ENABLE_PLUGINS=ON

if [ $? -ne 0 ]; then
    echo "CMake configuration failed!"
    exit 1
fi

echo "== Start Build"
ninja install || { echo "Build failed!"; exit 1; }

echo "== PGO+LTO optimized compiler ready at ${TOPLEV}/stage2-prof-use-lto/install/bin"
