#!/bin/bash

SCRIPT_PATH=$(pwd)
mkdir -p ~/toolchain/llvm

echo "Cloning llvm-project"

./setup_llvm_repo.bash
cd ${SCRIPT_PATH}

echo "Create vanilla build of LLVM toolchain, comparitivly slow, but contains the necessary tools we use (without BOLT)"
./build_stage1.bash || (echo "Building Stage1-Toolchain failed!"; exit 1)
cd ${SCRIPT_PATH}

echo "Build a new instrumented LLVM with stage1, instrumentation allows PGO"
./build_stage2-prof-generate.bash || (echo "Building instrumented Stage2-Toolchain failed!"; exit 1)
cd ${SCRIPT_PATH}

echo "  Rebuild LLVM with instrumented stage2 -> Gather performance data that can be fed into the optimizer "
./build_stage3-train.bash || (echo "Generating training-data failed!"; exit 1)
cd ${SCRIPT_PATH}

echo " Build an optimized LLVM (PGO+LTO) with the stage1 compiler. (faster ~30%)"
./build_stage2-prof-use-lto.bash || (echo "Building optimized LTO+PGO Stage2-Toolchain failed!"; exit 1)
cd ${SCRIPT_PATH}

# If possible, measure the runtime of the optimized stage2 compiler with perf
# and feed these measurements into BOLT, that will optimize the binary layout
# of clang for improved cache friendlyness

perf record -e cycles:u -j any,u -- sleep 1 &>/dev/null;
if [[ $? == "0" ]]; then
    echo "BOLTING with Profile!"
    ./build_stage3-bolt.bash || (echo "Optimizing Stage2-Toolchain further with llvm-bolt failed!"; exit 1)
else
    echo "Optimizing Stage2-Toolchain with instrumenting"
    ./build_stage3-bolt-without-sampling.bash || (echo "Optimizing Stage2-Toolchain with bolt failed!"; exit 1)
fi
