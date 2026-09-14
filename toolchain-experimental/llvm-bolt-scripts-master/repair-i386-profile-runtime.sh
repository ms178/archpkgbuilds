#!/usr/bin/env bash
# Build/install only the missing i386 profile runtime; do not rebuild LLVM.
set -euo pipefail
if (( $# < 3 || $# > 4 )); then
  echo "Usage: $0 /installed/bin/clang /matching/llvm-project /fresh/build-dir [/clean/build/bin/clang]" >&2
  exit 2
fi
clang=$(realpath -s "$1")
source=$(cd "$2" && pwd)
build=$(realpath -m "$3")
builder=$(realpath -s "${4:-$clang}")
bin=$(dirname "$builder")
final_bin=$(dirname "$clang")
for p in "$clang" "$builder" "$bin/clang++" "$bin/ld.lld" "$bin/llvm-ar" "$bin/llvm-ranlib" "$final_bin/llvm-profdata"; do
  [[ -x $p ]] || { echo "Missing executable: $p" >&2; exit 1; }
done
[[ -f $source/runtimes/CMakeLists.txt && -f $source/compiler-rt/lib/profile/CMakeLists.txt ]] || { echo 'Expected matching llvm-project source checkout' >&2; exit 1; }
[[ ! -e $build ]] || { echo "Use a NEW build directory (refusing to reuse/delete $build)" >&2; exit 1; }
# A runtime must not inherit application LTO/PGO/native-ISA/linker flags.
unset CC CXX CFLAGS CXXFLAGS CPPFLAGS LDFLAGS CCLDFLAGS CXXLDFLAGS ASFLAGS LLVM_PROFILE_FILE LD_PRELOAD
resource=$("$clang" -print-resource-dir)
triple=$("$clang" -m32 -print-target-triple)
[[ $resource = /* && $triple = i?86-*-linux-* ]] || { echo "Unexpected resource dir or 32-bit target: $resource / $triple" >&2; exit 1; }
mkdir -p "$build"
printf 'Resource: %s\n32-bit target: %s\n' "$resource" "$triple"
"$clang" --version
"$builder" --version
printf 'int main(int argc, char **argv) { (void)argv; return argc < 1; }\n' > "$build/probe.c"
# Fail early for missing multilib CRT/libgcc/libc rather than hiding this in CMake.
"$builder" --target="$triple" -m32 -fuse-ld=lld -B"$bin" "$build/probe.c" -o "$build/plain-probe"
"$build/plain-probe"
cmake -S "$source/runtimes" -B "$build/cmake" -G Ninja -Wno-dev \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$(dirname "$final_bin")" \
  -DLLVM_ENABLE_RUNTIMES=compiler-rt \
  -DLLVM_ENABLE_PER_TARGET_RUNTIME_DIR=ON \
  -DLLVM_HOST_TRIPLE="$triple" \
  -DLLVM_DEFAULT_TARGET_TRIPLE="$triple" \
  -DCOMPILER_RT_INSTALL_PATH="$resource" \
  -DCOMPILER_RT_DEFAULT_TARGET_ONLY=ON \
  -DCOMPILER_RT_INCLUDE_TESTS=OFF \
  -DCOMPILER_RT_BUILD_PROFILE=ON \
  -DCOMPILER_RT_BUILD_BUILTINS=OFF \
  -DCOMPILER_RT_BUILD_SANITIZERS=OFF \
  -DCOMPILER_RT_BUILD_XRAY=OFF \
  -DCOMPILER_RT_BUILD_LIBFUZZER=OFF \
  -DCOMPILER_RT_BUILD_MEMPROF=OFF \
  -DCOMPILER_RT_BUILD_ORC=OFF \
  -DCOMPILER_RT_BUILD_GWP_ASAN=OFF \
  -DCOMPILER_RT_USE_BUILTINS_LIBRARY=OFF \
  -DCMAKE_C_COMPILER="$builder" \
  -DCMAKE_CXX_COMPILER="$bin/clang++" \
  -DCMAKE_ASM_COMPILER="$builder" \
  -DCMAKE_C_COMPILER_TARGET="$triple" \
  -DCMAKE_CXX_COMPILER_TARGET="$triple" \
  -DCMAKE_ASM_COMPILER_TARGET="$triple" \
  -DCMAKE_AR="$bin/llvm-ar" \
  -DCMAKE_RANLIB="$bin/llvm-ranlib" \
  -DCMAKE_C_FLAGS='-O2 -fPIC -fno-lto' \
  -DCMAKE_CXX_FLAGS='-O2 -fPIC -fno-lto' \
  -DCMAKE_ASM_FLAGS='-fPIC -fno-lto' \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld -B$bin" \
  -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld -B$bin" \
  -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=lld -B$bin"
cmake --build "$build/cmake" --target install-profile --parallel "${RUNTIME_JOBS:-4}"
archive="$resource/lib/$triple/libclang_rt.profile.a"
[[ -s $archive ]] || { echo "Runtime was not installed in Clang's expected per-target layout: $archive" >&2; exit 1; }
# Verify actual archive architecture; a renamed/symlinked x86-64 archive is not a fix.
members=$("$bin/llvm-ar" t "$archive")
first_member=${members%%$'\n'*}
"$bin/llvm-ar" p "$archive" "$first_member" > "$build/member.o"
readelf -h "$build/member.o" | tee "$build/member-header.txt"
grep -q 'Class:.*ELF32' "$build/member-header.txt"
grep -q 'Machine:.*Intel 80386' "$build/member-header.txt"
# Actual PGO+LTO link, execute, merge and consume, not just archive existence.
"$clang" -m32 -O2 -flto -fuse-ld=lld -B"$final_bin" -fprofile-generate "$build/probe.c" -o "$build/pgo-probe"
LLVM_PROFILE_FILE="$build/probe.profraw" "$build/pgo-probe"
test -s "$build/probe.profraw"
"$final_bin/llvm-profdata" merge -o "$build/probe.profdata" "$build/probe.profraw"
"$clang" -m32 -O2 -flto -fuse-ld=lld -B"$final_bin" -fprofile-use="$build/probe.profdata" "$build/probe.c" -o "$build/use-probe"
"$build/use-probe"
"$clang" -m32 -O2 -flto -fuse-ld=lld -B"$final_bin" -fprofile-use="$build/probe.profdata" -fcs-profile-generate "$build/probe.c" -o "$build/cs-probe"
LLVM_PROFILE_FILE="$build/cs.profraw" "$build/cs-probe"
test -s "$build/cs.profraw"
"$final_bin/llvm-profdata" merge -o "$build/merged.profdata" "$build/probe.profdata" "$build/cs.profraw"
"$clang" -m32 -O2 -flto -fuse-ld=lld -B"$final_bin" -fprofile-use="$build/merged.profdata" "$build/probe.c" -o "$build/final-probe"
"$build/final-probe"
echo "PASS: installed ELF32 runtime; 32-bit PGO and CS-PGO with LTO generate/run/merge/use passed: $archive"
