# archpkgbuilds
This repository contains a couple of custom PKGBUILDs for Arch Linux and its derivatives. These PKGBUILDS used to produce working binaries at the time they were uploaded. Nevertheless, you need to take a look at them anyway and change the patch directory names in the PKGBUILD to the ones where you placed the files as I haven't been clever enough to get a relative path to work yet. So the patches are using my home directory by default, sorry. As software is a moving target, I also cannot guarantee that they work everywhere and at any future point in time. I'll try to keep them up to date as long as I use them myself. I cannot make any promises for timely future updates though.

The toolchain folder contains everything you need to build an optimized GCC and LLVM toolchain on Arch Linux. As I took the liberty to slim them down a bit (some often unused languages and subprojects are missing, but the packages should work for most users), you can take them as a source of inspiration and edit the official PKGBUILDS with some of my alterations if you have special needs. While I did some research on my changes and took some inspiration from Clear Linux, I know that they work for me and my purposes only, your mileage my vary. Be aware that a profiledbootstrap with GCC takes a lot of time, I strongly advise you to use the new mold linker to speed up the linking process, that really helped. Also the build flags used for LLVM are very slow as FullLTO is used which only supports single-threaded linking. If you want to speed up the LLVM build process at the cost of around 25 MB of disk space, you can use ThinLTO instead (-flto=thin) which speeds up the build times considerably.

Another important change is that both LLVM and GCC packages are highly optimized for Intel's Haswell CPU architecture running on the latest Glibc and Kernel, if you want to optimize these packages for another CPU architecture or older Kernels, you need to edit the PKGBUILD for GCC and Glibc or the haswell.patch for LLVM. As for the additional patches used, these were taken from Clear Linux and for the Linux Kernel can be downloaded from SirLucjan's excellent repository at https://github.com/sirlucjan/kernel-patches 

I've used the following compiler flags in my /etc/makepkg.conf,

for GCC and Linux-api-headers:
<CODE>
CFLAGS="-O3 -mtune=native -march=native -fno-semantic-interposition -falign-functions=32 -fipa-pta -flive-range-shrinkage -fno-math-errno -fno-trapping-math -mtls-dialect=gnu2 -feliminate-unused-debug-types -floop-nest-optimize -fgraphite-identity -fcf-protection=none -fdevirtualize-at-ltrans -mharden-sls=none"
CXXFLAGS="$CFLAGS"
LDFLAGS="-Wl,-O3,--as-needed,-Bsymbolic-functions"
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

for LLVM (use minimal CFLAGS/CXXFLAGS, -O2 -march=native, for the first run and fort he second build, use the following flags as Polly is not usable that way with the default LLVM toolchain):
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
