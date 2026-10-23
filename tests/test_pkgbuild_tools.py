#!/usr/bin/env python3
"""Regression tests for repository-local PKGBUILD maintenance tools."""

from __future__ import annotations

import hashlib
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "tools"))

import pkgbuild_guard as guard  # noqa: E402
import pkgbuild_validate as validate  # noqa: E402


class OptimizationGuardTests(unittest.TestCase):
    def test_function_parser_skips_quoted_heredoc_body(self) -> None:
        text = """\
build() {
  cat <<'PROGRAM'
int main(void) {
  return 0;
}
PROGRAM
  echo PGO
}
next_function() {
  :
}
"""
        regions = guard.function_regions(text)
        self.assertEqual([region.name for region in regions], ["build", "next_function"])
        self.assertTrue(regions[0].text.rstrip().endswith("}"))
        self.assertNotIn("next_function", regions[0].text)

    def test_lock_detects_added_protected_content(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            pkgdir = repo / "packages" / "demo"
            pkgdir.mkdir(parents=True)
            pkgbuild = pkgdir / "PKGBUILD"
            pkgbuild.write_text(
                "pkgname=demo\npkgver=1\npkgrel=1\n"
                "pgo_build() {\n  echo PGO\n}\n"
                "build() { pgo_build; }\n",
                encoding="utf-8",
            )
            entry = guard.entry_for_pkgbuild(repo, pkgbuild)
            self.assertIsNotNone(entry)
            assert entry is not None
            self.assertEqual([], guard.check_entry(repo, "packages/demo/PKGBUILD", entry))

            pkgbuild.write_text(
                pkgbuild.read_text(encoding="utf-8") + "bolt_extra() { echo BOLT; }\n",
                encoding="utf-8",
            )
            errors = guard.check_entry(repo, "packages/demo/PKGBUILD", entry)
            self.assertTrue(any("bolt_extra" in error for error in errors), errors)

    def test_lock_detects_new_auxiliary_optimization_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            pkgdir = repo / "packages" / "demo"
            pkgdir.mkdir(parents=True)
            pkgbuild = pkgdir / "PKGBUILD"
            pkgbuild.write_text(
                "pkgname=demo\npkgver=1\npkgrel=1\n"
                "build() { echo PGO; }\n",
                encoding="utf-8",
            )
            entry = guard.entry_for_pkgbuild(repo, pkgbuild)
            assert entry is not None
            (pkgdir / "training.sh").write_text("# PGO workload\n", encoding="utf-8")
            errors = guard.check_entry(repo, "packages/demo/PKGBUILD", entry)
            self.assertTrue(any("auxiliary" in error for error in errors), errors)


class PkgbuildValidatorTests(unittest.TestCase):
    def test_literal_brace_expansion_ignores_parameter_expansion(self) -> None:
        arrays = validate.arrays(
            "source=(https://example.invalid/${pkgver}/bundle{,.sig} local.patch)\n"
            "sha256sums=(SKIP SKIP SKIP)\n"
        )
        values, uncertain = validate.source_values(arrays) or ([], True)
        self.assertEqual(
            values,
            [
                "https://example.invalid/${pkgver}/bundle",
                "https://example.invalid/${pkgver}/bundle.sig",
                "local.patch",
            ],
        )
        self.assertFalse(uncertain)

    def test_url_fragment_does_not_hide_following_source_entry(self) -> None:
        arrays = validate.arrays(
            "source=(git+https://example.invalid/project.git#tag=v1 local.patch)\n"
            "# an ordinary comment after the completed array\n"
            "sha256sums=(SKIP SKIP)\n"
        )
        values, uncertain = validate.source_values(arrays) or ([], True)
        self.assertEqual(
            values,
            ["git+https://example.invalid/project.git#tag=v1", "local.patch"],
        )
        self.assertFalse(uncertain)

    def test_conditional_source_append_is_not_assumed_to_have_fixed_arity(self) -> None:
        arrays = validate.arrays(
            "source=(upstream.tar.gz)\nif true; then\n  source+=(optional.patch)\nfi\n"
        )
        values, uncertain = validate.source_values(arrays) or ([], False)
        self.assertEqual(values, ["upstream.tar.gz", "optional.patch"])
        self.assertTrue(uncertain)

    def test_validator_checks_local_source_digest(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            pkgdir = repo / "packages" / "demo"
            pkgdir.mkdir(parents=True)
            patch = pkgdir / "fix.patch"
            patch.write_text("first version\n", encoding="utf-8")
            digest = hashlib.sha256(patch.read_bytes()).hexdigest()
            pkgbuild = pkgdir / "PKGBUILD"
            pkgbuild.write_text(
                "pkgname=demo\npkgver=1\npkgrel=1\n"
                "source=(fix.patch)\n"
                f"sha256sums=('{digest}')\n",
                encoding="utf-8",
            )
            result = validate.ValidationResult()
            validate.validate_recipe(repo, pkgbuild, False, result)
            self.assertEqual(result.errors, [])

            patch.write_text("changed version\n", encoding="utf-8")
            result = validate.ValidationResult()
            validate.validate_recipe(repo, pkgbuild, False, result)
            self.assertTrue(any("does not match local source" in finding.message for finding in result.errors))

    def test_embedded_workload_is_not_reported_as_missing_source(self) -> None:
        text = "cat > workload.cpp <<'EOF'\nint main() {}\nEOF\n"
        self.assertTrue(validate.is_generated_source(text, "workload.cpp"))


if __name__ == "__main__":
    unittest.main()
