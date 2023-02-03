This section contains several profiles for LLVM/Clang-15.0.7 which you can use to squeeze more performance out of your system. Instrumentation was run on my hard- and software, so these might not be perfect for your needs. Especially the Mesa profile is only instrumented for my AMD Vega and might not help you if you are using a different GPU. It is normal that you will get some warnings that a couple of functions do not match and could not be optimized.

To demonstrate their use, I use Mesa in the following as an example and my home directory as the place where the profiles are located, adjust it to your needs. I currently use these flags in my '/etc/makepkg.conf' file and included the command for using the Mesa profile: 

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
export CPPFLAGS="-D_FORTIFY_SOURCE=0"
export CFLAGS="-O3 -march=native -mtune=native -maes -mllvm -inline-threshold=1000 -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -fno-math-errno -fno-trapping-math -falign-functions=32 -funroll-loops -fno-semantic-interposition -fcf-protection=none -mharden-sls=none -fomit-frame-pointer -flto -fprofile-instr-use=/home/marcus/Downloads/mesa.profdata"
export CXXFLAGS="${CFLAGS}"
export LDFLAGS="-Wl,--lto-O3,-O3,-Bsymbolic-functions,--as-needed -mllvm -extra-vectorizer-passes -mllvm -enable-cond-stores-vec -mllvm -slp-vectorize-hor-store -mllvm -enable-loopinterchange -mllvm -enable-loop-distribute -mllvm -enable-unroll-and-jam -mllvm -enable-loop-flatten -mllvm -interleave-small-loop-scalar-reduction -mllvm -unroll-runtime-multi-exit -mllvm -aggressive-ext-opt -flto -fuse-ld=mold -Wl,-zmax-page-size=0x200000 -fprofile-instr-use=/home/marcus/Downloads/mesa.profdata"
CCLDFLAGS="$LDFLAGS"
CXXLDFLAGS="$LDFLAGS"
export ASFLAGS="-D__AVX__=1 -D__AVX2__=1 -msse2avx -D__FMA__=1"
```

In general, my LLVM-PGO workflow for Mesa looks like this:

1. Compile Mesa with -fprofile-instr-generate=/home/marcus/Downloads/mesa-%p.profraw
2. Use the instrumented binary on common workloads (e.g. running game benchmarks, video, websurfing etc.) to collect the profile data.
3. Merge the profraw files with llvm-profdata merge -output=mesa.profdata mesa-*.profraw
4. Compile the project again with -fprofile-instr-use=/home/marcus/Downloads/mesa.profdata
