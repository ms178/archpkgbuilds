#!/bin/bash

export TOPLEV=~/toolchain/llvm
mkdir -p "${TOPLEV}"
cd "${TOPLEV}" || { echo "Could not enter ${TOPLEV} directory"; exit 1; }

echo "== Cloning LLVM (efficient shallow clone)"

# Shallow clone with blobless filter (much faster than full clone)
git clone --filter=blob:none --depth=1 \
    https://github.com/llvm/llvm-project.git || { echo "Clone failed"; exit 1; }

cd llvm-project || { echo "Could not enter llvm-project directory"; exit 1; }

# Patch lld to only build ELF backend (skip MachO/COFF/wasm that need platform headers)
if [ -f lld/CMakeLists.txt ]; then
    echo "== Patching lld/CMakeLists.txt to disable MachO/COFF/wasm backends"
    sed -i.bak \
        -e '/add_subdirectory(MachO)/d' \
        -e '/add_subdirectory(COFF)/d' \
        -e '/add_subdirectory(wasm)/d' \
        -e '/add_subdirectory(MinGW)/d' \
        lld/CMakeLists.txt
    echo "== lld configured for ELF-only (Linux native)"
fi

echo "== LLVM repo ready"
