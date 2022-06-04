#!/bin/bash

export TOPLEV=~/toolchain/llvm
cd ${TOPLEV} || (echo "Could not enter ${TOPLEV} directory"; exit 1)

mkdir -p bolt || (echo "Could not create stage1 directory"; exit 1)
cd bolt || (echo "Could not enter stage 1 directory"; exit 1)

echo "== Configure Build"
echo "== Build with system clang"

cmake -G Ninja ${TOPLEV}/llvm-project/llvm \
    -DBUILD_SHARED_LIBS:BOOL=OFF \
    -DLIB_INSTALL_DIR=/usr/lib64 \
    -DLIB_SUFFIX=64 \
    -DLLVM_LIBDIR_SUFFIX=64 \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_INSTALL_LIBDIR=/usr/lib64 \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DCMAKE_INSTALL_SBINDIR=/usr/bin \
    -DLLVM_PARALLEL_COMPILE_JOBS:STRING=20 \
    -DLLVM_PARALLEL_LINK_JOBS:STRING=4 \
    -DCMAKE_AR=/usr/bin/llvm-ar \
    -DCMAKE_NM=/usr/bin/llvm-nm \
    -DCMAKE_RANLIB=/usr/bin/llvm-ranlib \
    -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
    -DENABLE_LINKER_BUILD_ID:BOOL=ON \
    -DCLANG_DEFAULT_LINKER=lld \
    -DLLVM_ENABLE_LIBCXX:BOOL=OFF \
    -DLLVM_STATIC_LINK_CXX_STDLIB:BOOL=ON \
    -DCLANG_DEFAULT_CXX_STDLIB:STRING=libstdc++ \
    -DCLANG_LINK_CLANG_DYLIB:BOOL=OFF \
    -DLLVM_LINK_LLVM_DYLIB:BOOL=OFF \
    -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF \
    -DLIBCLANG_BUILD_STATIC:BOOL=ON \
    -DLLVM_BINUTILS_INCDIR=/usr/include \
    -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON \
    -DLLVM_ENABLE_NEW_PASS_MANAGER:BOOL=ON \
    -DLLVM_ENABLE_PIC:BOOL=OFF \
    -DLLVM_TARGETS_TO_BUILD="X86" \
    -DLLVM_USE_LINKER:STRING=lld \
    -DLLVM_HOST_TRIPLE="x86_64-generic-linux" \
    -DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python3 \
    -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF \
    -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF \
    -DLLVM_BUILD_EXAMPLES:BOOL=OFF \
    -DLLVM_BUILD_DOCS:BOOL=OFF \
    -DLLVM_BUILD_RUNTIME:BOOL=OFF \
    -DLLVM_BUILD_TOOLS:BOOL=OFF \
    -DCLANG_BUILD_TOOLS:BOOL=OFF \
    -DLLVM_BUILD_UTILS:BOOL=OFF \
    -DCOMPILER_RT_BUILD_SANITIZERS:BOOL=OFF \
    -DCOMPILER_RT_BUILD_XRAY:BOOL=OFF \
    -DCOMPILER_RT_BUILD_CRT:BOOL=OFF \
    -DCOMPILER_RT_BUILD_LIBFUZZER:BOOL=OFF \
    -DLLVM_INSTALL_BINUTILS_SYMLINKS:BOOL=OFF \
    -DLLVM_INSTALL_UTILS:BOOL=OFF \
    -DLLVM_ENABLE_UNWIND_TABLES:BOOL=ON \
    -DLLVM_ENABLE_EH:BOOL=ON \
    -DLLVM_ENABLE_RTTI:BOOL=ON \
    -DLLVM_ENABLE_THREADS:BOOL=ON \
    -DLLVM_ENABLE_Z3_SOLVER:BOOL=OFF \
    -DLLVM_ENABLE_LIBEDIT:BOOL=OFF \
    -DLLVM_ENABLE_TERMINFO:BOOL=OFF \
    -DLLVM_ENABLE_ZLIB:BOOL=ON \
    -DLLVM_ENABLE_FFI:BOOL=ON \
    -DLLVM_ENABLE_ASSERTIONS:BOOL=OFF \
    -DLLVM_ENABLE_LTO:STRING="Thin" \
    -DLLVM_ENABLE_PROJECTS="bolt" \
    -DCMAKE_C_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie" \
    -DCMAKE_CXX_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie" \
    -DCMAKE_EXE_LINKER_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie -Wl,-mllvm,-x86-use-vzeroupper=0" \
    -DCMAKE_MODULE_LINKER_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie -Wl,-mllvm,-x86-use-vzeroupper=0" \
    -DCMAKE_SHARED_LINKER_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie -Wl,-mllvm,-x86-use-vzeroupper=0" \
    -DCMAKE_ASM_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie" \
    -DCMAKE_Fortran_FLAGS_RELEASE="-Wno-unused-command-line-argument -DNDEBUG -O3 -falign-functions=32 -fasynchronous-unwind-tables -fexceptions -flto=thin -fno-plt -fno-semantic-interposition -fno-stack-protector -fomit-frame-pointer -fuse-ld=lld -fslp-vectorize -ftree-slp-vectorize -ftree-vectorize -fvectorize -march=native -mcpu=native -mno-vzeroupper -mprefer-vector-width=256 -mtune=native -pipe -pthread -static-libgcc -static-libstdc++ -Wno-error -Wp,-D_REENTRANT -Wall -Wl,--build-id=sha1 -Wl,--enable-new-dtags -Wl,-Bsymbolic-functions -Wl,-O2 -Wl,-z,now,-z,relro,-z,max-page-size=0x1000,-z,separate-code -Wl,--emit-relocs -fno-PIC -fno-PIE -fno-pic -fno-pie" \
    -DGCC_INSTALL_PREFIX="/usr" \
    -Wno-dev
