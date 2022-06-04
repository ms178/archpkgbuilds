# How does it work

This set of scripts (credits go to @JonasToth and @ptr1337) which I refined a bit to work as LLVM system compiler replacement create a 60% faster LLVM toolchain that can be customly trained to any project.

The full_workflow.bash will autodetect if your CPU supports special profiling features and chooses the correct script which suits your hardware.

## How to build

    Download this repository via the main Github page under >Code >Download ZIP and extract the ZIP archive to a location on your disk where you want it, delete the stuff you don't want
    locate the llvm-bolt-scripts directory
    execute the bash script with ./full_workflow.bash

You will get a toolchain directory in your home directory with the build compilers. If something went wrong during the build process, simply delete some or all of folders there. The script is clever enough to detect the already built stage1 compiler if you want to keep that.

This sequence will give you (hopefully) a faster LLVM toolchain.
Technologies used:

-   LLVM Link Time Optimization (LTO)
-   Binary Instrumentation and Profile-Guided-Optimization (PGO)
-   perf-measurement and branch-sampling/profiling and final binary reordering (BOLT)

The goal of the techniques is to utilize the CPU better and layout the code in a way, that allows faster execution.

Measure performance gains and evaluate if its worth the hazzle :)
You can experiment with technologies, maybe `ThinLTO` is better then `FullLTO`,

For the last bit of performance, you can run several different workloads and then merge the resulted profiles with 'merge-fdata \*.fdata > combined.fdata' and then optimize the libary with llvm-bolt again.
        and nothing else! The same goes for `BOLT`.
