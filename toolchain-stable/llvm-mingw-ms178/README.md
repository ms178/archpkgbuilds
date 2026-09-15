# llvm-mingw-ms178

CachyOS/Arch PKGBUILD that builds a minimal x86/AMD64 LLVM-based
mingw-w64 toolchain from source, pinned to a specific llvm-project commit
— the same snapshot ms178's Unix toolchain
uses — and tuned to your local CPU with `-march=native`.

Includes the six ms178 performance patches (Threading/corecount, BOLT +
regalloc fixes, SmallPtrSet optimizations, Polly cleanups, Raptor Lake
scheduling, X86 ISel/lowering) applied on top of that commit.

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

### Usage

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

## Credits

- Original bash recipe: [NTULINUX/llvm-mingw](https://github.com/NTULINUX/llvm-mingw)
  by Alec Ari, itself forked from
  [mstorsjo/llvm-mingw](https://github.com/mstorsjo/llvm-mingw) by
  Martin Storsjö.
- Fish port + PKGBUILD: CachyOS packaging (for ms178).
