# systemd 261.3-2.3 — integrated NVPCR packaging repair

## Build and install

This is a complete PKGBUILD directory, including the original companion files
and the new source patch. Extract the ZIP and run as your normal build user:

```sh
sha256sum -c SHA256SUMS
makepkg --cleanbuild --syncdeps --install
```

Use a fresh build so that old generated sources/profiles are not reused. Install
the matching split-package set together, as in your existing workflow. Do not
skip the package checks. Normal dependencies and signed upstream source-tag
verification still apply.

## What changed

- Package release bumped from `261.3-2.2` to `261.3-2.3`.
- `0002-install-nvpcr-without-bootloader.patch` changes only the condition around
  NVPCR **data-file** generation/install in upstream systemd's Meson rules.
- `-Dbootloader=disabled` and `-Dukify=disabled` remain unchanged. This does not
  enable systemd-boot or its bootloader-dependent TPM setup executables.
- OpenSSL and TPM2 support are explicitly enabled, with OpenSSL also declared
  as a direct build dependency. TPM2 development dependencies were already present.
- The main systemd package owns these real, upstream-generated definitions:
  `/usr/lib/nvpcr/{cryptsetup,hardware,login,verity}.nvpcr`.
- `package_systemd()` refuses to produce the package if any definition is missing
  or empty after Meson installation.
- All local source-array entries now have real SHA-512 checksums. The signed Git
  tag retains the usual `SKIP` content checksum and PGP verification.

The existing PGO training/generation/use flow and other package options are
preserved. No pacman install script edits another package's files. No dummy
NVPCR definitions, shell-option workarounds, or host TPM provisioning are added.

## Why this fixes the confirmed installation error

Your earlier configuration disabled bootloader support. Upstream systemd v261.3
also gated the NVPCR definitions behind that option. The mkinitcpio systemd hook
then passed an unmatched `/usr/lib/nvpcr/*.nvpcr` glob to `add_file`.

The package now includes the intended data files, so the unmodified hook has real
files to include. This fixes the package-content side of that interaction and
survives subsequent systemd rebuilds/installations. It is specific to this
TPM2/OpenSSL-enabled package configuration, not a universal fix for mkinitcpio's
handling of every possible reduced systemd build.

The earlier local mkinitcpio hook repair may remain in place; it is harmless.
It is no longer required by this rebuilt package. If restoring that hook, use
its package's current pristine version rather than blindly restoring an old
backup after intervening package updates. Do not remove the working repair
before the replacement systemd package is ready and its contents checked.

After installation, confirm the normal initramfs/Limine hooks succeed before
rebooting. This package repair does not by itself establish that the separate
kernel-module BTF problems have been resolved.

## Validation

`VALIDATION.json` contains the focused test results. Reproduce them with Python3,
Meson, Ninja, Jinja2 and GNU patch installed:

```sh
python3 validation/run-tests.py
```

Seven real Meson data-target generation/install cases passed, using upstream
v261.3 Meson code, templates and Jinja renderer. Before the patch, bootloader-off
installs zero definitions; after it, four. Patched bootloader-off output equals
unmodified bootloader-on output. A custom NV index base also renders correctly.
The tests preserve the executable gating conditions and check missing-feature
cases, the actual PKGBUILD package guard, source hashes and Bash syntax.

This is **not** a complete systemd/PGO build or an initramfs/boot test. Those must
still pass on the Arch/CachyOS build host. The test uses source-presence stubs for
TPM setup C files; it never compiles or claims to test those executables.

## Provenance

Original PKGBUILD and companion assets:
https://github.com/ms178/archpkgbuilds/tree/1075c32a798d57edf2dd13cc673b8a311ec7e75c/packages/systemd

Patched build rule:
https://github.com/systemd/systemd/blob/v261.3/src/tpm2-setup/meson.build

The validation directory includes the corresponding upstream templates and
renderer. Their upstream licensing applies. The new patch is a local packaging
change, not claimed to have been merged upstream.
