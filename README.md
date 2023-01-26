# ms178's archpkgbuilds
This repository contains a couple of customized PKGBUILDs for Arch Linux. Some are heavily modified and might only be usable on CPUs which are compatible with Intel's Haswell CPU architecture. This repository is first and foremost focused for my personal use. It is also meant to demonstrate some ideas for further refinements and performance optimizations which could trickle down to the official PKGBUILDS eventually. These PKGBUILDS used to produce working binaries at the time they were uploaded. Nevertheless, Arch is a rolling release and some might carry outdated versions or patches with them. Please take a look at the PKGBUILD first to make sure they fit your personal needs. Be aware that most packages are targeting the head of a development branch, it can happen that you end up with an unusable package even though everything went smoothly at first. Be prepared to get back to a safe state and use them at your own risk. I also cannot guarantee that they work everywhere and at any future point in time. I'll try to keep them up to date as long as I use them myself. I cannot make any promises for timely future updates though. The stable toolchain packages should be safer to use wheras the experimental toolchain is more experimental by nature.

***WARNING: Be aware that the toolchain packages, the Kernel and my used compiler flags deliberatly favor performance over security. You might want to change some options/compiler flags for your machine.***

For a more in-depth-look on optimizing your Arch Linux installation, take a look over at my Linux gaming tweaks series in my personal Blog (https://seylaw.blogspot.com/search/label/Gaming).

Both toolchain folders contain everything you need to build an optimized GCC and LLVM on Arch Linux. As I took the liberty to slim them down a bit (e.g. language support and some subprojects are missing, but the packages should work for most users only interested in C/C++), you can take them as a source of inspiration and edit the official PKGBUILDS with some of my alterations if you want to try out some ideas yourself. While I did some research on my changes and took some inspiration from Clear Linux and Allen McRae's alternative GCC toolchain for Arch (https://github.com/allanmcrae/toolchain), I know that they work for me and my purposes only, your mileage my vary. As I cut some corners regarding the checks, you should use either less aggressive compiler flags for both toolchains to play it safe or run the checks to verify that your toolchain works as expected. Be aware that a profiledbootstrap with GCC takes a lot of time (1 hour on my 18-Core Xeon), even more so when including the checks.

The build order is:

linux-api-headers > glibc > binutils > gcc > glibc > binutils > gcc

If you want to speed up the LLVM build process considerably at the cost of some disk space, you should use ThinLTO (-flto=thin) as that makes multi-threaded linking possible wheras FullLTO is single-threaded. A FullLTO build takes around an hour whereas a ThinLTO build takes around 17 Minutes on my hardware.

Another important change is that the GCC, LLVM and kernel packages are meant to run on Intel's Haswell CPU or compatible architectures, they also require the latest Glibc and a very recent Kernel, if you want to optimize these packages for another CPU architecture or older Kernels, you need to edit the PKGBUILD for GCC, Glibc and the Kernel or the haswell.patch for LLVM. As for any additional patches used, these were mainly taken or adapted from Clear Linux for both toolchains or taken from the respective mailing lists. 

The Linux Kernel builds carry some additional patches on top, mostly from the Linux Kernel Mailing List (LKML) directly. 

I've used the following compiler flags in my /etc/makepkg.conf,

for GCC, linux-api-headers and the Linux Kernel:
```
CFLAGS="-O3 -march=native -mtune=native -maes -fno-semantic-interposition -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed,-Bsymbolic-functions"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
```

for Binutils:
```
CFLAGS="-O3 -march=native -mtune=native -maes -fno-semantic-interposition -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -pipe -flto=auto -floop-parallelize-all -ftree-parallelize-loops=18 -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed,-Bsymbolic-functions,-flto=auto -fopenmp"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
```

for Glibc:
```
CFLAGS="-O3 -march=native -mtune=native -maes -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
```

for LLVM:
```
export CC=clang
export CXX=clang++
export CC_LD=lld
export CXX_LD=lld
export AR=llvm-ar
export NM=llvm-nm
export STRIP=llvm-strip
export OBJCOPY=llvm-objcopy
export OBJDUMP=llvm-objdump
export READELF=llvm-readelf
export RANLIB=llvm-ranlib
export HOSTCC=clang
export HOSTCXX=clang++
export HOSTAR=llvm-ar
export CFLAGS="-O3 -march=native -mtune=native -maes -mllvm -inline-threshold=1000 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -flto"
export CXXFLAGS="${CFLAGS}"
export LDFLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -Wl,-mllvm,-march=native -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -flto -fuse-ld=lld"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
export ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -msse2avx -D__FMA__=1"
```

I've also seen great results with more advanced Polly-flags on most packages (e.g. mesa-git) using the mold linker:

```
export CC=clang
export CXX=clang++
export CC_LD=mold
export CXX_LD=mold
export AR=llvm-ar
export NM=llvm-nm
export STRIP=llvm-strip
export OBJCOPY=llvm-objcopy
export OBJDUMP=llvm-objdump
export READELF=llvm-readelf
export RANLIB=llvm-ranlib
export HOSTCC=clang
export HOSTCXX=clang++
export HOSTAR=llvm-ar
export CFLAGS="-O3 -march=native -mtune=native -maes -mllvm -inline-threshold=1000 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-parallel=true -fopenmp -fopenmp-version=50 -mllvm -polly-dependences-computeout=5000000 -mllvm -polly-detect-profitability-min-per-loop-insts=40 -mllvm -polly-tiling=true -mllvm -polly-prevect-width=256 -mllvm -polly-vectorizer=stripmine -mllvm -polly-omp-backend=LLVM -mllvm -polly-num-threads=36 -mllvm -polly-scheduling=dynamic -mllvm -polly-scheduling-chunksize=1 -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -mllvm -polly-enable-delicm=true -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -mprefer-vector-width=256 -flto"
export CXXFLAGS="${CFLAGS}"
export LDFLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -flto -fuse-ld=mold -Wl,-zmax-page-size=0x200000"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
export ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -msse2avx -D__FMA__=1"
```
