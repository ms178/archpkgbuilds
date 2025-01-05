#!/bin/bash

TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || (echo "Could not enter ${TOPLEV} directory"; exit 1)
mkdir -p ${TOPLEV}
git clone https://github.com/llvm/llvm-project.git

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
    -D LLVM_BUILD_DOCS=OFF \
    -D LLVM_ENABLE_SPHINX=OFF \
    -D LLVM_ENABLE_DOXYGEN=OFF \
    -D LLVM_ENABLE_Z3_SOLVER=ON \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_DEFAULT_TARGET_TRIPLE="x86_64-pc-linux-gnu" \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_USE_PERF=OFF \
    -DLLVM_ENABLE_PROJECTS="clang;lld;bolt;compiler-rt" \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -D CMAKE_C_FLAGS="-O3 -march=native -mtune=native -fprofile-use=/home/marcus/Downloads/clang.profdata -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_CXX_FLAGS="-O3 -march=native -mtune=native -fprofile-use=/home/marcus/Downloads/clang.profdata -fcf-protection=none -mharden-sls=none -flto=thin -fwhole-program-vtables" \
    -D CMAKE_EXE_LINKER_FLAGS="-Wl,--thinlto-jobs=8 -fprofile-use=/home/marcus/Downloads/clang.profdata -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -D CMAKE_MODULE_LINKER_FLAGS="-Wl,--thinlto-jobs=8 -fprofile-use=/home/marcus/Downloads/clang.profdata -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -D CMAKE_SHARED_LINKER_FLAGS="-Wl,--thinlto-jobs=8 -fprofile-use=/home/marcus/Downloads/clang.profdata -Wl,--lto-CGO3 -Wl,--gc-sections -Wl,--icf=all -Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fwhole-program-vtables -fuse-ld=lld -Wl,-zmax-page-size=0x200000 -Wl,--push-state -Wl,-whole-archive -lmimalloc -Wl,--pop-state -lpthread -lstdc++ -lm -ldl" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_BUILD_UTILS=OFF \
    -DLLVM_ENABLE_BACKTRACES=OFF \
    -DLLVM_ENABLE_WARNINGS=OFF \
    -DLLVM_INCLUDE_TESTS=OFF \
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
    -DCMAKE_INSTALL_PREFIX=${TOPLEV}/llvm-bolt || (echo "Could not configure project!"; exit 1)

echo "== Start Build"
ninja install || (echo "Could not build project!"; exit 1)
