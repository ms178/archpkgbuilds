# llvm-mingw-ms178

CachyOS/Arch PKGBUILD that builds a minimal x86/AMD64 LLVM-based
mingw-w64 toolchain from source, pinned to llvm-project commit
**`92f01b2` (2026-06-28)** — the same snapshot ms178's Unix toolchain
uses — and tuned to your local CPU with `-march=native`.

Includes the six ms178 performance patches (Threading/corecount, BOLT +
regalloc fixes, SmallPtrSet optimizations, Polly cleanups, Raptor Lake
scheduling, X86 ISel/lowering) applied on top of that commit. Nominal
version string is `22.99.0-92f01b2` — a snapshot ahead of the
`llvmorg-22.1.8` tag.

Recipe is a fish port of NTULINUX/llvm-mingw with all its optimizations:

- ThinLTO + parallel link
- Polly (`-O3 -mllvm -polly`) baked into the mingw cross wrappers
- Static everything (no `.so` runtime dependencies inside the toolchain)
- ucrt runtime, `_WIN32_WINNT=0x0A00` (Windows 10) default
- Only `X86` + `i686` + `x86_64-w64-mingw32` targets built

Installs the full toolchain to **`/opt/llvm-mingw`** (same path as the
regular `llvm-mingw` / `-git` / `-bin` packages, so ms178's existing
`export PATH=/opt/llvm-mingw/bin:$PATH` in his shell config keeps
working). The package **name** is `llvm-mingw-ms178` so it can be
tracked separately in the local pacman database and coexist with those
other variants as a `conflicts=('llvm-mingw' 'llvm-mingw-git'
'llvm-mingw-bin')` alternative.

**Nothing is added to system `$PATH` automatically.** `proton-cachyos`
and other builds that must use the host clang are unaffected. Activate
the toolchain per-shell yourself:

```bash
export PATH=/opt/llvm-mingw/bin:$PATH
```

or per-project via `.envrc`, a wrapper script, or a shell alias.

## Contents

```
llvm-mingw-ms178/
├── PKGBUILD                          # main entry point for makepkg
├── README.md                         # this file
├── build-llvm-mingw-22.1.8.fish      # the actual build recipe
├── patches/                          # ms178 perf patches
│   ├── 01-corecount.patch            # Threading + smart worker pinning
│   ├── 02-fixes.patch                # BOLT + regalloc CSR-cost cleanups
│   ├── 03-optimizations.patch        # SmallPtrSet grow inlining
│   ├── 04-polly.patch                # Polly analysis passes
│   ├── 05-raptorlake.patch           # X86 tuning for Raptor Lake
│   └── 06-x86isellowcpp.patch        # X86 ISel + lowering + VPlan/SLP
└── wrappers/                         # NTULINUX toolchain wrappers
    ├── clang-target-wrapper.sh
    ├── ld-wrapper.sh
    ├── objdump-wrapper.sh
    ├── i686-w64-windows-gnu.cfg
    └── x86_64-w64-windows-gnu.cfg
```

## Requirements

- CachyOS or Arch (`base-devel`, `pacman`)
- ~30 GB free space on `/tmp` (ThinLTO link is memory- and disk-hungry)
- Working `clang`/`lld`/`llvm` in `$PATH` (`makedepends` will pull them
  in if missing)

## Build

Extract the archive, `cd` in, and let makepkg do its thing. **Do not
remove the `wrappers/` subdirectory** — the PKGBUILD's `prepare()` copies
it from `$startdir` at build time.

```bash
makepkg -si                              # standard build + install
makepkg -si --skipchecksums              # if you regenerated the fish script
makepkg -sif --cleanbuild                # force a clean rebuild
```

### Compile flags

The PKGBUILD explicitly overrides `/etc/makepkg.conf` because ThinLTO +
`-march=native` don't mix well with makepkg's default `-flto=auto` and
`-fstack-protector-strong`. The flags actually used:

```
CFLAGS  = -O3 -march=native -mtune=native -pipe -fno-plt -fno-semantic-interposition
CXXFLAGS = (same as CFLAGS)
LDFLAGS = -Wl,-O1 -Wl,--as-needed -fuse-ld=lld
```

These are exported around the fish recipe so any autotools invocations
(mingw-w64 configure, `gendef`, `widl`, `winpthreads`) inherit them.
The CMake-based cross builds (compiler-rt / libcxx / openmp) clear
`CFLAGS` internally so they don't leak host flags into Windows PE code.

Additionally, the host LLVM build gets `-fPIC` unconditionally and is
explicitly pointed at the shared `libz.so`/`libzstd.so` (rather than
CachyOS's `zlib-ng-compat` static archives, which are non-PIC/LTO-bitcode
and cause `R_X86_64_PC32` relocation errors when ThinLTO merges them
into `libclang-cpp.so`). `LLVM_BUILD_STATIC=ON` is deliberately NOT set
(unlike upstream NTULINUX): we're building an integrated CachyOS package,
not a redistributable tarball, so linking against the host's
`libc/libz/libzstd.so` is the correct choice.

This will:

1. Populate `/tmp/llvm-mingw-ms178-build-22.1.8/src/{llvm-project,mingw-w64}`
2. Run every stage of the fish recipe (`deps → fetch → llvm → strip →
   wrappers → tools → crt → compiler-rt → libcxx → libs → compiler-rt →
   openmp → package`)
3. Produce
   - `llvm-mingw-ms178-22.1.8-1-x86_64.pkg.tar.zst` in the PKGBUILD dir
     (installable via `pacman -U`)
   - `/tmp/llvm-mingw-ms178-build-22.1.8/dist/llvm-mingw-22.1.8-native.tar.zst`
     as a bonus reproducible-tree tarball

Activate per-shell, then use as normal:

```bash
export PATH=/opt/llvm-mingw/bin:$PATH

x86_64-w64-mingw32-clang --version
# clang version 22.1.8 ...
# Target: x86_64-w64-windows-gnu

i686-w64-mingw32-clang hello.c -o hello.exe
```

Or invoke via absolute path without touching `$PATH`:

```bash
/opt/llvm-mingw/bin/x86_64-w64-mingw32-clang hello.c -o hello.exe
```

For DXVK / vkd3d-proton / dxvk-nvapi builds that look up
`x86_64-w64-mingw32-clang` by name (meson's `find_program`), make sure
`/opt/llvm-mingw/bin` is on `$PATH` in the shell where you run `meson`
or `makepkg`:

```bash
export PATH=/opt/llvm-mingw/bin:$PATH
cd dxvk/
meson setup --cross-file build-win64.txt build/x64
ninja -C build/x64
```

## Rebuild speed-ups

- `pkgver`-locked src checkouts persist across `makepkg -f` runs, so a
  rebuild that only changes the recipe skips the ~5-min clone
- Set `NO_LTO=1` in your shell before `makepkg` for a faster smoke build
  (drops ~40% of link time; final binaries are ~10% slower)
- Set `NO_NATIVE=1` if you want a portable `x86-64-v3` build instead of
  native (host clang stays v3, mingw cfg falls back to `prescott`)

## Uninstall

```bash
sudo pacman -R llvm-mingw-ms178
```

## Notes

- `options=('!buildflags' '!makeflags')` is deliberate --- makepkg's
  default `CFLAGS`/`LDFLAGS` would fight ThinLTO and can OOM the linker.
- `-march=native` is applied both to the host binaries (clang.exe / lld /
  polly) AND to the mingw wrappers' default `.cfg` files, so `.exe`
  binaries you compile with this toolchain will also target your CPU. If
  you plan to ship those binaries to other machines, either recompile
  with an explicit `-march=x86-64-v3` on the command line or set
  `NO_NATIVE=1` and rebuild the package.
- The stage-based fish recipe can be invoked directly outside of
  PKGBUILD for iteration:

  ```bash
  cd llvm-mingw-ms178/
  ./build-llvm-mingw-22.1.8.fish ~/llvm-mingw-test --stage=llvm
  ./build-llvm-mingw-22.1.8.fish ~/llvm-mingw-test --stage=crt
  ```

## Credits

- Original bash recipe: [NTULINUX/llvm-mingw](https://github.com/NTULINUX/llvm-mingw)
  by Alec Ari, itself forked from
  [mstorsjo/llvm-mingw](https://github.com/mstorsjo/llvm-mingw) by
  Martin Storsjö.
- Fish port + PKGBUILD: CachyOS packaging (for ms178).
