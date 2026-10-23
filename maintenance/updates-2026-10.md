# October 2026 maintenance record

This record documents the deliberately narrow release updates and integrity
repairs in this maintenance pass.  It is not a claim that every package in the
collection received a version bump.  Current CachyOS package sources and Arch
repository-state metadata were used as reference evidence; local behavior was
retained unless a source/tag/checksum check demonstrated a specific defect.

## Release updates

| Package | Local update | Evidence and pre-flight result |
| --- | --- | --- |
| `7zip` | `26.02-3.1` → `26.05-1.1` | Upstream `ip7z/7zip` tag `26.05` resolves to `b88c6bc09339de32f7d18bbab900686b244f42ba`; Arch state also reported 26.05.  The local five-component clang/uasm make graph was dry-run against the tag. |
| `doxygen` | `1.13.2-3.1` → `1.18.0-1.1` | Arch state reports 1.18.0-2; upstream `Release_1_18_0` resolves to `8e760943e5d9581a444cf327f43a0b4d20d29482`.  The exact configured GitHub source archive was downloaded and hashed; all retained CMake feature options and the installed `LICENSE` path remain present. |
| `brotli` | `1.2.0-4.4` → `1.2.1-1.1` | Arch state reports 1.2.1-1; upstream tag `v1.2.1` resolves to `db24a98aed38a16de78cc05ca15cdd8dc2a0d92d`.  The retained CMake build layout and runtime library paths used by the local PGO workload remain present. |
| `expat` | `2.8.5-6.1` → `2.9.0-1.1` | Arch state reports 2.9.0-1.  GitHub release `R_2_9_0` publishes SHA-256 `775e8a68f81a748a401dc3168318d4a39484eb4f954726f21517c8f42c0ba984` for the exact `.tar.bz2`; the tagged source retains the CMake options, public-header path, benchmark location, and documentation files used by the local PGO workflow. |
| `c-ares` | `1.34.8-3.1` → `1.34.9-1.1` | Upstream signed tag `v1.34.9` resolves to `bc91829cce983a35ec01f5dbc05465858a3df469`, released 2026-10-20.  The makepkg-style Git archive checksums were regenerated from that exact tag; the CMake and installed-document source layout remains present. |
| `abseil-cpp-main` | `20260526.0-3.1` → `20260817.0-1.1` | Upstream release/tag `20260817.0` resolves to `2065f4ded0558c6f89fee67c8e5228feb4eb960e`.  The exact GitHub archive SHA-256 is `f7e05179df39c45434cad433f5783840bb3788ef322976f9138bc6b72b3a107d`.  `scoped-mock-log.patch` applies to the new source (at the expected offset); the old GCC-16 commit patch no longer applies because its change is upstreamed, so it was removed rather than carried forward. |
| `libunwind` | `1.8.2-4.1` → `1.8.3-1.1` | GitHub release `v1.8.3` publishes the exact release asset SHA-256 `be30d910e67f58d82e753231f1357f326a1a088acf126b21ff77e60aab19b90b`.  The `v1.8.3` source tag retains the configure options used locally. |
| `icu-main` | `78.1-2.1` → `78.3-1.1` | Arch state reports 78.3-1.  GitHub release `release-78.3` publishes SHA-256 `3a2e7a47604ba702f345878308e6fefeca612ee895cf4a5f222e7955fabfe0c0` for the exact source archive; the tagged source retains `icu/source/configure`.  The local PGO functions were not changed. |
| `libuv` | `1.52.1-3.2` → `1.53.0-1.1` | Upstream signed tag `v1.53.0` resolves to `840404ce8ba7cc0204be52389a6cfff9f2c90fb6`, released 2026-09-24.  The retained prepare-time test-list edits remain valid or harmless against that tag, and the source layout used by the package remains present. |
| `at-spi2-core-git` | `2.60.7-6.1` → `2.62.1-1.1` | Arch state reports 2.62.1-1.  An Arch package snapshot supplies the GitLab VCS BLAKE2 `3dd6040fbd507e684d16579d982e84b6f481c11dd8808154364fb3b6ee9276881ef122e4c6abb5f17dff50f0302e950d2fd009ba7ccc0bf37b2f97a11d4f5f6f`; the mirror tag was inspected only for layout and option compatibility. |
| `fontconfig` | `2.18.2-5.3` → `2.18.3-1.1` | Arch state reports `2:2.18.3-2`.  Arch-compatible packaging supplies the GitLab VCS BLAKE2 `7635f6d32ed8ffb5ad4a8614c5cd87949935c3743635af9749faf93dc82aab57c7e09b46132edf8939a5ce5d38ef7ee12be457aa474d74e61af04fb96fb87f1a`; the exact 2.18.3 source retains the PGO source/test layout and passes the local prepare-time source transformations. |
| `glib2` | `2.88.2-2.1` → `2.90.1-1.1` | Arch state reports 2.90.1-1.  An Arch package snapshot supplies the GitLab VCS BLAKE2 `d2a3b7c9745b4473621bc77d1fa690c062510cb53dd24555abceb0216ddb9c3f18214774a6c4a948daf0edcf4738db9ed1cba47ec92481d4eee18906bc0e78a3`; both retained local patches apply to the inspected source and the retained test/source paths exist. |
| `wireplumber` | `0.5.17-4.1` → `0.5.18-1.1` | Arch state reports 0.5.18-1.  An Arch package snapshot supplies the GitLab VCS BLAKE2 `eea6a3e022c0b8918a72b56a9cdb1c02bca4411a9394fc2a8732faad2c0c71d1443b2a37b22e60467aaab62c154150d18b185b2690271d1d4830dee6513e2e8c`; the 0.5.18 source retains every Meson option and installed-path family used locally. |
| `fish-main` | `4.8.1-8.1` → `4.9.3-1.1` | Arch state reports 4.9.3-1; exact upstream tag `4.9.3` resolves to `6cf31373e9468f1a261319aceb8730e5d1c47e58`.  The configured source layout used by the retained PGO workflow was inspected at that tag. |
| `glslang-main` | `1.4.350.0-12` → `1.4.363.0-1.1` | Arch state reports `1:1.4.363.0-1`; upstream `vulkan-sdk-1.4.363.0` resolves to `e1b562a8bed273a02f30b59b66a5d499793cede5`.  The PGO source paths remain present. |
| `libpng-git` | `1.6.57-3.1` → `1.6.59-1.1` | Arch state reports 1.6.59-1; upstream `v1.6.59` resolves to `cd952f49f95bb27154ae77dbb103032d95f6e580`.  The VCS archive checksums were regenerated with the makepkg-compatible archive method. |
| `nodejs` | `26.8.2-3.1` → `26.12.0-1.1` | Arch state reports 26.12.0-1.  The exact `v26.12.0` tag resolves to `26e72d85a611e222e3f06025de8a3c496923bacd`; the chosen package-channel release deliberately does not jump to the later upstream 26.12.1 release. |
| `openssl` | `3.6.3-19` → `3.6.5-1.1` | Arch state reports 3.6.5-1.  The upstream 3.6.5 release publishes SHA-256 `a2157c2830efdec3788939b00c9b0638306d3f0bbb76dc4832ee503bb397df98` for the exact configured archive. |
| `pcre2-main` | `10.48-3.1` → `10.49-1.1` | Arch state reports 10.49-1.  The exact `pcre2-10.49` VCS archive was inspected and both configured makepkg-style digests were regenerated. |
| `lib32-expat` | `2.8.5-2.1` → `2.9.0-1.1` | The exact `R_2_9_0` VCS archive was inspected and its SHA-512 and BLAKE2 digests regenerated; the retained multilib PGO layout remains available. |
| `python` | `3.14.5-4.2` → `3.14.8-1.1` | Arch state reports 3.14.8-1.  Only release metadata and the configured source integrity entry changed; no Python PGO logic or dependency arrays changed. |
| `shaderc` | `2026.3-3.1` → `2026.4-1.1` | Arch state reports 2026.4-1.  The configured source archive for upstream `v2026.4` was inspected and its SHA-512/BLAKE2 digests regenerated. |
| `simdjson` | `4.6.6-3.1` → `5.0.5-1.1` | Arch state reports `1:5.0.5-1`.  The exact configured `v5.0.5` archive was hashed (`50bb17769e1054ff1bfcd5dbbfcf33544c77f6ebdb31a318436ea0e47f78f571`); retained build arguments await a clean runner configuration check. |
| `systemd` | `261.3-2.3` → `262.1-1.1` | Arch state reports 262.1-1.  The active `0001-Use-Arch-Linux-device-access-groups.patch` and `0002-install-nvpcr-without-bootloader.patch` both apply to the exact v262.1 source; the unreferenced `clear.patch` remains inactive. |
| `nettle` | `3.10.1-2.1` → `4.0-1.1` | Arch state reports 4.0-1.  Arch-compatible packaging uses the same GNU archive and SHA-256 `3addbc00da01846b232fb3bc453538ea5468da43033f21bb345cb1e9073f5094`; the 4.0 source retains the local configure switches. |
| `libsoup3` | `3.6.5-2.1` → `3.8.0-1.1` | Arch state reports 3.8.0-1.  The configured GitLab VCS source remains `SKIP`; the exact 3.8.0 tag was inspected in the GitHub mirror only for source-layout compatibility, not to synthesize a GitLab checksum. |
| `networkmanager` | `1.58.0-5.1` → `1.58.1-1.1` | Arch state reports 1.58.1-2.  Current Arch-compatible ARM packaging supplies BLAKE2 `71ca8a6b298df777841b6f5560bf6ef9100ac7b6b4c48a46c995685a53cb09732d1e8c6e4139f572bbb3f66352eb59737f260ca5738202591b4f25703976f141` for the exact GitLab tag; the source layout was also checked at the upstream mirror tag. |
| `pipewire` | `1.6.9-4.1` → `1.6.10-1.1` | Arch state reports `1:1.6.10-1`.  Current Arch-compatible ARM packaging supplies BLAKE2 `3c4645252b9affb46852be4d3103fc94664f76df1a1b64b7a8aee011f26b38f10d5bd189fdef65b239c9e13e016dbd25070b791c7ec4e2870777231aac3a8bbd` for the exact GitLab tag; the PGO source layout was checked at the upstream mirror tag. |

No `depends` or `makedepends` entries changed in these release updates.

## Integrity repairs

The static pre-flight found source/checksum arity defects, stale digests for
repository-local files, and missing package-local Proton auxiliary files.
Repairs only align each recipe with files already selected by its `source`
array; they do not alter build options, dependencies, or optimization code.

- Corrected determinate checksum arity and local-file digest mappings across
  the affected release, VCS, multilib, Qt, and PGO recipes.  This includes the
  `power-profiles-daemon` local patch: it is a second declared source and now
  has its matching SHA-256 entry rather than silently relying on a shortened
  checksum array.
- Hardened the validator's array tokenizer so a `#tag=` URL fragment cannot
  hide later `source` items as a shell comment.  A regression test covers the
  failure mode that revealed the power-profiles-daemon arity defect.
- Restored `compatibilitytool.vdf.template` and `ntsync.conf` to
  `proton-cachyos-patches/`.  Both are byte-identical to the corresponding
  current CachyOS native Proton auxiliary files and match their declared BLAKE2
  hashes.  The package release was incremented to `3.1` because those files are
  installed by the recipe.
- Removed source entries for local files that were absent and unused
  (`cmake-cppflags.patch`, `vfgradfunasm.patch`), while retaining the unrelated
  archived FFmpeg patch under `packages/ffmpeg/patches/`.
- Replaced stale remote release checksums only where the exact fetched artifact
  or an upstream GitHub release-asset digest was available.  Other remote
  hashes remain for clean-chroot `makepkg --verifysource` validation.

The PGO/BOLT lock check was run after every batch and passes for all 115
protected recipes.  The local sandbox has no Arch `makepkg`, so clean-chroot
source and prepare validation is supplied through the documented script and CI
rather than being claimed as completed locally.

## Deliberate deferrals and limits

- `linux-cachymod-6.18` remains entirely untouched, as requested.
- `nss` remains at 3.126: the local `ms178-1.patch` does not apply to the exact
  NSS 3.132 source, and rebasing it would violate the metadata-only PGO/BOLT
  update constraint.
- `mimalloc` remains at 3.5.3 because `ms178-1.patch` fails against 3.5.4.
- `freetype2` remains at 2.14.3: its protected prepare logic explicitly names
  `ft2demos-2.14.3`, so a metadata-only bump to 2.15.0 would knowingly break
  the frozen PGO workflow.
- `gzip` remains at 1.14 because the configured GNU release artifact checksum
  for 1.15 could not be independently verified from an allowed authoritative
  source; the Arch VCS checksum is not a checksum for the local `.tar.xz`.
- `kwin` remains at 6.7.5: the active `ms178-1.patch` fails extensively against
  the exact 6.8.1 source, so its metadata was not advanced.
- `samba` remains at 4.24.5: `ms178-1.patch` fails against the inspected 4.25.0
  source.  `ollama` remains at 0.23.2 because upstream 0.40.9 removed the
  CPU/HIP/Vulkan CMake install components on which the frozen split-package
  functions rely.
- The Proton and Wine CachyOS recipes remain on their pinned source tags: newer
  source tags require revalidating their large local patch and payload stacks,
  which is not a metadata-only update.
- XLibre `xf86-video-amdgpu` 25.1.2 and XLibre xserver 25.2.2 were examined but
  deferred: the active driver patch fails at the newer driver source, and one
  of the active xserver patches fails at the newer xserver source.  Neither was
  rebased under this pass.
- Rolling `*-git` recipes with a functioning `pkgver()` are intentionally left
  to resolve their version during build.  For GitLab VCS sources, checksums are
  never fabricated from a GitHub mirror; a clean Arch/CachyOS runner must still
  run `makepkg --verifysource`, `prepare`, and package builds.
