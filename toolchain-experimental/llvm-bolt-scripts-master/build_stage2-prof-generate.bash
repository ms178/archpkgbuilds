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
        -D LLVM_ENABLE_Z3_SOLVER=ON \
        -DCLANG_BUILD_TOOLS=ON \
        -DCLANG_TOOL_AMDGPU_ARCH_BUILD=OFF \
        -DCLANG_TOOL_APINOTES_TEST_BUILD=OFF \
        -DCLANG_TOOL_ARCMT_TEST_BUILD=OFF \
        -DCLANG_TOOL_C_ARCMT_TEST_BUILD=OFF \
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
        -DCLANG_TOOL_NVPTX_ARCH_BUILD=OFF \
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
        "-DLLVM_THINLTO_CACHE_PATH='${CMAKE_INSTALL_PREFIX}/llvm-thinlto'" \
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
