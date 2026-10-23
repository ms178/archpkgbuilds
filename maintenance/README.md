# PKGBUILD maintenance safeguards

This repository intentionally diverges from stock Arch and CachyOS packaging in
many places.  In particular, a sizeable subset contains hand-tuned PGO,
CS-PGO, BOLT, workload, or toolchain-selection logic.  Reference PKGBUILDs are
used for **evidence**, not as files to copy wholesale.

## Scope

Automated maintenance tooling only discovers `packages/**/PKGBUILD`.
`toolchain-stable/` and `toolchain-experimental/` are deliberately outside this
maintenance scope until their toolchain work is explicitly scheduled.

## Immutable optimization lock

`optimization-lock.json` is a SHA-256 manifest for the local PGO/BOLT surface:

- marked Bash functions and their transitive local helper calls;
- PGO/BOLT marker lines outside functions; and
- local auxiliary files that contain optimization markers.

Run the guard before and after every metadata update:

```bash
python3 tools/pkgbuild_guard.py check
```

Do **not** casually regenerate the lock.  A refresh is only appropriate after a
reviewed, intentional change to the local optimization implementation:

```bash
python3 tools/pkgbuild_guard.py snapshot --force
python3 tools/pkgbuild_guard.py check
```

The guard is intentionally byte-sensitive.  A source-version, checksum, or
non-optimization packaging change can proceed only when this check stays
clean; a PGO/BOLT implementation change must have its lock change reviewed as
part of the same patch.

## Static pre-flight checks

```bash
python3 tools/pkgbuild_validate.py
python3 -m unittest -v tests/test_pkgbuild_tools.py
```

The validator runs `bash -n`, checks required metadata, checks determinate
source/checksum arity, verifies repository-local patch/helper digests, and
reports missing local sources.  It deliberately leaves dynamic source arrays,
VCS state, signatures, and remote checksums to `makepkg` in an Arch
environment.  `SKIP` integrity entries are warnings so intentional VCS package
choices remain visible without being silently normalized.

When `makepkg` is available, a selected package can also be evaluated by
makepkg without building it:

```bash
python3 tools/pkgbuild_validate.py --path packages/7zip --makepkg
```

## Update procedure

1. Establish upstream release/tag and integrity evidence.  Compare the local
   recipe with the relevant CachyOS and Arch packaging references line by line,
   retaining deliberate local behavior.
2. Run the optimization guard and static validator before editing.
3. Make the narrowest metadata/source/patch adjustment.  Preserve the local
   `.1` package-release convention when resetting a versioned recipe unless a
   package-specific reason says otherwise.
4. Do not change `depends` or `makedepends` without a concrete build or runtime
   reason documented in the change review.
5. Re-run the guard, validator, tests, and `git diff --check`.
6. Use the clean Arch pre-flight script or CI for changed package directories:

   ```bash
   tools/validate_changed_pkgbuilds.sh --base origin/main --mode source
   # Optional: runs prepare() but replaces build/check/package functions in a copy.
   tools/validate_changed_pkgbuilds.sh --base origin/main --mode prepare
   ```

`source` uses `makepkg --verifysource`; `prepare` additionally exercises the
recipe's `prepare()` phase in an isolated copy while preventing a PGO/BOLT
build.  Neither mode is a substitute for a full clean-chroot build.  Full PGO
and BOLT builds remain an explicit, resource-intensive release validation step.
The GitHub source-preflight job is intentionally opt-in because some changed
packages download multi-gigabyte source sets: dispatch it manually, or apply
the `pkgbuild-source-preflight` PR label after reviewing the affected package
set.
