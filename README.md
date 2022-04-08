# archpkgbuilds
This repository is highly experimental and contains a couple of customized PKGBUILDs for Arch Linux. It is meant to demonstrate some ideas for further refinements which could trickle down to the official or respective AUR PKGBUILDS eventually. These PKGBUILDS used to produce working binaries at the time they were uploaded. Nevertheless, you need to take a look at them anyway, the ones with custom patches need manual adjustment to the file path, sorry, I wasn't clever enough to get a relative path to work yet. As packages targeting the head of a development branch are a fast moving target, it can happen that you end up with an unusable toolchain even though everything went smoothly at first. Be prepared to get back to a safe state. I also cannot guarantee that they work everywhere and at any future point in time. I'll try to keep them up to date as long as I use them myself. I cannot make any promises for timely future updates though.

The toolchain folder contains everything you need to build an optimized GCC and LLVM toolchain on Arch Linux. As I took the liberty to slim them down a bit (e.g. language support and some subprojects are missing, but the packages should work for most users), you can take them as a source of inspiration and edit the official PKGBUILDS with some of my alterations if you want to try out some ideas yourself on a productive system. While I did some research on my changes and took some inspiration from Clear Linux and Allen McRae's alternative GCC toolchain for Arch, I know that they work for me and my purposes only, your mileage my vary. As I cut some corners regarding the checks, you should use either less aggressive compiler flags for both toolchains to play it safe or run the checks to verify that your toolchain works as expected. Be aware that a profiledbootstrap with GCC takes a lot of time, even more so when including the checks. For GCC, I strongly advise you to use the new mold linker to speed up the linking process, it really helped. Also the build flags below for LLVM are very slow as FullLTO is used which only supports single-threaded linking. If you want to speed up the LLVM build process at the cost of around 25 MB of disk space, you can use ThinLTO instead (-flto=thin) which speeds up the build times considerably.

Another important change is that the GCC, LLVM and kernel packages are optimized for Intel's Haswell CPU architecture, they also require the latest Glibc and a brand new 5.17 Kernel, if you want to optimize these packages for another CPU architecture or older Kernels, you need to edit the PKGBUILD for GCC, Glibc and the Kernel or the haswell.patch for LLVM. As for any additional patches used, these were taken or adapted from Clear Linux for both toolchains. 

The Linux Kernel builds upon the already modified Xanmod sources, but carries some additional patches on top, e.g. the ProjectC scheduler, more Clear Linux patches and other patches taken from SirLucjan's excellent repository (https://github.com/sirlucjan/kernel-patches), but I also got some from the Linux Kernel Mailing List (LKML) directly. 

I've used the following compiler flags in my /etc/makepkg.conf,

for GCC, linux-api-headers and the Linux Kernel:
<CODE>
CFLAGS="-O3 -mtune=native -march=native -fno-semantic-interposition -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -fdevirtualize-at-ltrans -mharden-sls=none" [#-mharden-sls=none is only supported from GCC-12 onwards, delete this option for the first run]
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed,-Bsymbolic-functions" [#add -B/usr/libexec/mold here to use the Mold linker for the first GCC-12 build]
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
</CODE>

for Binutils:
<CODE>
CFLAGS="-O3 -mtune=native -march=native -fno-semantic-interposition -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -pipe -flto=auto -floop-parallelize-all -ftree-parallelize-loops=18 -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed,-Bsymbolic-functions,-flto=auto -fopenmp"
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
</CODE>

for Glibc:
<CODE>
CFLAGS="-O3 -mtune=native -march=native -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed"
ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
</CODE>

for LLVM (use a minimal CFLAGS/CXXFLAGS, e.g. -O2 -march=native, for the first run and for the second build, use the following flags as Polly is not available that way with the default LLVM toolchain):
<CODE>
export CC=clang
export CXX=clang++
export CC_LD=lld
export CXX_LD=lld
export LD=lld
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
export CFLAGS="-O3 -march=native -mtune=native -mllvm -polly -mllvm -polly-parallel -fopenmp -mllvm -polly-vectorizer=stripmine -mllvm -polly-omp-backend=LLVM -mllvm -polly-num-threads=36 -mllvm -polly-scheduling=dynamic -mllvm -polly-scheduling-chunksize=1 -mllvm -polly-ast-use-context -mllvm -polly-invariant-load-hoisting -mllvm -polly-loopfusion-greedy -mllvm -polly-run-inliner -mllvm -polly-run-dce -fno-math-errno -fno-trapping-math -falign-functions=32 -fno-semantic-interposition -fcf-protection=none -flto"
export CXXFLAGS="${CFLAGS}"
export LDFLAGS="-Wl,--lto-O3,-Bsymbolic-functions,--as-needed -flto -fuse-ld=lld"
export ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -D__FMA__=1"
</CODE>
