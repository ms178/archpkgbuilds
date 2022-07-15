# Experimental Toolchain Section

While my other packages are also somewhat experimental, the toolchain in this directory is even a step up from this. This set of scripts which were made originally by JonasToth and ptr1337 create a 60% faster LLVM toolchain that can be trained to any project.

The full_workflow.bash will autodetect if your CPU supports a special profiling instruction (LBR - Intel CPUs as of Haswell but not on even very recent AMD CPUs) and choose the correct script which suits to your hardware.

## LLVM

### How to build

Be sure to have jemalloc and perf installed.

Download the scripts via the Code > Download option
extract the archive into a directory
cd llvm-bolt-scripts
./full_workflow.bash

Instead of the default above, I use the following line with additional optimizations which are overriden or extended in specific bash scripts: CC=clang CFLAGS="-O3 -march=native -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -flto=thin" CXX=clang++ CXXFLAGS="-O3 -march=native -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -flto=thin" LDFLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -flto=thin -fuse-ld=lld" ./full_workflow.bash

The goal of all of these techniques is to utilize the CPU better that allows faster code execution.

Measure performance gains and evaluate if its worth the hazzle :)
You can experiment with technologies, maybe `ThinLTO` is better then `FullLTO`,

For the last bit of performance, you can run several different workloads and then merge the resulted profiles with 'merge-fdata \*.fdata > combined.fdata' and then optimize the libary with llvm-bolt again.
and nothing else! The same goes for `BOLT`.

## GCC

If you want to bolt gcc, you need to disable the language `lto` in the GCC PKGBUILD, you can still use the gcc lto function but gcc itself wont build with lto. Enabling lto will crash llvm-bolt.

Also you need to add following to your compileflags:

```
LDFLAGS+="--emit-relocs"
```

### Bolting other binarys/\*.so files

There is an included script which makes it possible to bolt any binary or .so file which got compiled with --emit-relocs.
You need to adjust it with the target binary name, its path and the STAGE number.

After you ran stage 1 you need to run a workload with the instrumented binary/.so file.

When the workload is running, you will see that many profiles are created in the FDATA path. In STAGE2 these files will be merged and then used on your binary/.so file.

#### Example:

As an example we want to BOLT LLVM:

    - Compile it with relocations (LDFLAGS+="--emit-relocs") enabled
    - Install your package
    - Change the to BINARY=libLLVM.so and the BINARYPATH=/usr/lib to your suits to the target you want to optimize and set STAGE=1
    - Run the script
    - After that it will backup your file and will move the instrumented target to the original path
    - Run a workload with the target, in this case compile something with clang
    - You will get several files into the FDATA path, when you run the workload !!! ATTENTION !!! the size of the data can get quite big, so be prepared to have enough disk capacity left
    - After you are done with the workload edit the script to STAGE=1
    - Execute the script again and the created data from the instrumentiation will be merged and used for llvm-bolt to optimize the target
    - After that it will automatically move it tor your binary/libary location, a backup and the bolted binary can be found at the binary path.
    - That's it, now repeat the worklow for other targets you want to optimize.
    - Tip: if you for example instrumented libLLVM the profile is also useable for other llvm based files which where active in the profiling process
