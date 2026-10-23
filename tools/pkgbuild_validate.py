#!/usr/bin/env python3
"""Fast, side-effect-free validation for the repository's PKGBUILDs.

This is intentionally a pre-flight validator, not a replacement for makepkg in
an Arch clean chroot.  It catches malformed recipes, missing local source files,
and determinable source/checksum arity errors before an expensive PGO build is
started.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shlex
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

ARRAY_ASSIGN_RE = re.compile(
    r"(?m)^[ \t]*(?P<name>[A-Za-z_][A-Za-z0-9_]*)(?P<append>\+)?=\("
)
SIMPLE_ASSIGN_RE = re.compile(r"(?m)^[ \t]*(?P<name>pkg(?:name|base|ver|rel))[ \t]*=")
CHECKSUM_RE = re.compile(r"^(?P<algorithm>md5|sha1|sha256|sha512|b2)sums(?:_[A-Za-z0-9_]+)?$")
HEX_LENGTH = {"md5": 32, "sha1": 40, "sha256": 64, "sha512": 128, "b2": 128}


@dataclass
class Finding:
    level: str
    path: str
    message: str


@dataclass
class ValidationResult:
    errors: list[Finding] = field(default_factory=list)
    warnings: list[Finding] = field(default_factory=list)

    def add(self, level: str, path: Path, repo: Path, message: str) -> None:
        target = self.errors if level == "error" else self.warnings
        target.append(Finding(level, path.relative_to(repo).as_posix(), message))


def _matching_paren(text: str, open_index: int) -> int | None:
    """Find a shell array's closing parenthesis without evaluating the recipe."""

    depth = 0
    quote: str | None = None
    escaped = False
    i = open_index
    while i < len(text):
        char = text[i]
        if quote:
            if quote == '"' and escaped:
                escaped = False
            elif quote == '"' and char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            i += 1
            continue
        if char in "'\"":
            quote = char
        elif char == "#" and (i == 0 or text[i - 1].isspace()):
            # A hash inside a source URL (for example git+#tag=v1) is part of
            # the word, not a shell comment.  Shell comments begin at a word
            # boundary, which this deliberately conservative test captures.
            newline = text.find("\n", i)
            i = len(text) if newline == -1 else newline
            continue
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return None


def _strip_shell_comments(text: str) -> str:
    """Remove conservative shell comments without eating URL fragments.

    ``shlex.commenters = '#'`` treats ``git+https://host/project#tag=v1`` as a
    comment even though the hash is part of the source URL.  In the PKGBUILD
    array syntax used here, a real comment begins at the start of a line or
    after whitespace.  Preserve hashes embedded in an unquoted word and let
    ``shlex`` handle the remaining tokenization.
    """

    output: list[str] = []
    quote: str | None = None
    escaped = False
    index = 0
    while index < len(text):
        char = text[index]
        if quote:
            output.append(char)
            if quote == '"' and escaped:
                escaped = False
            elif quote == '"' and char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            index += 1
            continue
        if char in "'\"":
            quote = char
            output.append(char)
            index += 1
            continue
        if char == "#" and (index == 0 or text[index - 1].isspace()):
            newline = text.find("\n", index)
            if newline == -1:
                break
            output.append("\n")
            index = newline + 1
            continue
        output.append(char)
        index += 1
    return "".join(output)


def _tokens(body: str) -> tuple[list[str], bool]:
    """Tokenize a literal-ish Bash array body.

    The boolean is true where array expansion or command substitution makes an
    arity check unsafe.  Those recipes are left to makepkg in CI rather than
    producing an unreliable local warning.
    """

    # Ordinary parameter substitutions preserve one array element.  Array
    # expansion and command substitution do not have a statically knowable
    # arity, so only those make a comparison unsafe.
    uncertain = bool(re.search(r"(?:\$\(|`|\[@\]|\[\*\])", body))
    lexer = shlex.shlex(_strip_shell_comments(body), posix=True)
    lexer.whitespace_split = True
    lexer.commenters = ""
    try:
        return list(lexer), uncertain
    except ValueError:
        return [], True


def arrays(text: str) -> dict[str, tuple[list[str], bool]]:
    """Collect global literal array assignments, including += additions."""

    result: dict[str, tuple[list[str], bool]] = {}
    for match in ARRAY_ASSIGN_RE.finditer(text):
        # The opening parenthesis is the final character in this regex match.
        open_index = match.end() - 1
        close_index = _matching_paren(text, open_index)
        if close_index is None:
            continue
        values, uncertain = _tokens(text[open_index + 1 : close_index])
        name = match.group("name")
        # source+= is often guarded by build options (kernel profiles and
        # optional patch sets are common examples).  A text-only parser cannot
        # know whether that branch executes, so leave its final arity to
        # makepkg rather than constructing a false-positive count.
        if name == "source" and match.group("append"):
            uncertain = True
        if match.group("append") and name in result:
            previous_values, previous_uncertain = result[name]
            result[name] = (previous_values + values, previous_uncertain or uncertain)
        else:
            result[name] = (values, uncertain)
    return result


def expand_braces(value: str) -> tuple[list[str], bool]:
    """Expand simple {a,b} forms only for count/local-file checks."""

    if re.search(r"\$\(|`|\[@\]|\[\*\]", value):
        return [value], True
    # Skip parameter expansions such as ${pkgver}; expand only literal shell
    # brace alternatives such as wine-gecko-x86{,_64}.tar.xz.
    match = re.search(r"\{([^${}]*,[^${}]*)\}", value)
    if not match:
        return [value], bool(re.search(r"(?:\$\(|`|\[@\]|\[\*\])", value))
    pieces = match.group(1).split(",")
    expanded: list[str] = []
    uncertain = False
    for piece in pieces:
        values, is_uncertain = expand_braces(value[: match.start()] + piece + value[match.end() :])
        expanded.extend(values)
        uncertain = uncertain or is_uncertain
    return expanded, uncertain


def expanded_values(values: list[str], uncertain: bool) -> tuple[list[str], bool]:
    output: list[str] = []
    for value in values:
        expanded, is_uncertain = expand_braces(value)
        output.extend(expanded)
        uncertain = uncertain or is_uncertain
    return output, uncertain


def source_values(array_map: dict[str, tuple[list[str], bool]]) -> tuple[list[str], bool] | None:
    if "source" not in array_map:
        return None
    values, uncertain = array_map["source"]
    return expanded_values(values, uncertain)


def is_remote_source(value: str) -> bool:
    value = value.split("::", 1)[-1]
    return bool(
        re.match(
            r"(?:[A-Za-z][A-Za-z0-9+.-]*\+)?https?://|(?:git|ssh)://|(?:git|bzr|svn|hg)\+",
            value,
        )
    )


def local_source_name(value: str) -> str | None:
    value = value.split("::", 1)[-1]
    if is_remote_source(value) or "$" in value or value.startswith("/"):
        return None
    # A plain source name is local.  Makepkg also accepts relative subpaths.
    return value


def is_generated_source(text: str, local_name: str) -> bool:
    """Recognize a source file generated while the PKGBUILD is sourced.

    A few established recipes intentionally embed a small PGO workload with a
    top-level here document and list that generated file in source=().  This
    has to remain distinguishable from an accidentally missing patch.
    """

    name = re.escape(local_name)
    destination = rf"(?:\$\{{srcdir\}}/|\$srcdir/)?['\"]?{name}(?:['\"]|\s|$)"
    return bool(re.search(rf"(?m)^.*>\s*{destination}", text))


def local_digest(path: Path, algorithm: str) -> str:
    hasher = hashlib.blake2b() if algorithm == "b2" else hashlib.new(algorithm)
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def validate_recipe(repo: Path, path: Path, use_makepkg: bool, result: ValidationResult) -> None:
    text = path.read_text(encoding="utf-8", errors="replace")
    syntax = subprocess.run(
        ["bash", "-n", "-c", text], text=True, capture_output=True, check=False
    )
    if syntax.returncode:
        result.add("error", path, repo, f"bash syntax error: {syntax.stderr.strip() or 'unknown error'}")
        return

    present = {match.group("name") for match in SIMPLE_ASSIGN_RE.finditer(text)}
    if not ({"pkgname", "pkgbase"} & present):
        result.add("error", path, repo, "missing pkgname/pkgbase assignment")
    if "pkgver" not in present and not re.search(r"(?m)^pkgver\s*\(\)", text):
        result.add("error", path, repo, "missing pkgver assignment or pkgver() function")
    if "pkgrel" not in present:
        result.add("error", path, repo, "missing pkgrel assignment")

    array_map = arrays(text)
    sources = source_values(array_map)
    if sources:
        source_items, source_uncertain = sources
        for item in source_items:
            local_name = local_source_name(item)
            if (
                local_name
                and not (path.parent / local_name).is_file()
                and not is_generated_source(text, local_name)
            ):
                result.add("error", path, repo, f"listed local source is missing: {local_name}")

        for name, (checksum_values, checksum_uncertain) in array_map.items():
            checksum_match = CHECKSUM_RE.match(name)
            if not checksum_match:
                continue
            expanded_checksums, expanded_uncertain = expanded_values(checksum_values, checksum_uncertain)
            if not source_uncertain and not expanded_uncertain and len(expanded_checksums) != len(source_items):
                result.add(
                    "error",
                    path,
                    repo,
                    f"{name} has {len(expanded_checksums)} entries for {len(source_items)} source entries",
                )
            algorithm = checksum_match.group("algorithm")
            expected_length = HEX_LENGTH[algorithm]
            for checksum in expanded_checksums:
                if checksum == "SKIP" or "$" in checksum:
                    # Variable-backed digests are evaluated by makepkg; their
                    # final value cannot be checked safely without sourcing.
                    continue
                if not re.fullmatch(r"[0-9a-fA-F]+", checksum) or len(checksum) != expected_length:
                    result.add("error", path, repo, f"invalid {name} digest: {checksum!r}")

            # We can verify repository-resident patches and helper files
            # immediately.  Remote/VCS sources are intentionally left to
            # makepkg --verifysource in the clean chroot.
            if (
                not source_uncertain
                and not expanded_uncertain
                and len(expanded_checksums) == len(source_items)
            ):
                for source_item, checksum in zip(source_items, expanded_checksums):
                    local_name = local_source_name(source_item)
                    local_path = path.parent / local_name if local_name else None
                    if (
                        not local_path
                        or not local_path.is_file()
                        or checksum == "SKIP"
                        or "$" in checksum
                    ):
                        continue
                    if re.fullmatch(r"[0-9a-fA-F]+", checksum) and len(checksum) == expected_length:
                        actual = local_digest(local_path, algorithm)
                        if actual.lower() != checksum.lower():
                            result.add(
                                "error",
                                path,
                                repo,
                                f"{name} does not match local source {local_name}: "
                                f"expected {checksum}, got {actual}",
                            )

        if not source_uncertain:
            checksums = [name for name in array_map if CHECKSUM_RE.match(name)]
            if not checksums:
                result.add("warning", path, repo, "source array has no recognized checksum array")
            elif all(
                value == "SKIP"
                for name in checksums
                for value in expanded_values(*array_map[name])[0]
            ) and any(not is_remote_source(item) or not item.split("::", 1)[-1].startswith("git+") for item in source_items):
                result.add("warning", path, repo, "all source integrity checks are SKIP")

    if use_makepkg:
        command = ["makepkg", "--printsrcinfo"]
        completed = subprocess.run(command, cwd=path.parent, text=True, capture_output=True, check=False)
        if completed.returncode:
            detail = (completed.stderr or completed.stdout).strip().splitlines()
            result.add("error", path, repo, f"makepkg --printsrcinfo failed: {detail[-1] if detail else 'unknown error'}")


def pkgbuild_paths(repo: Path, selected: list[str] | None) -> list[Path]:
    if not selected:
        return sorted((repo / "packages").rglob("PKGBUILD"))
    paths: list[Path] = []
    for item in selected:
        path = (repo / item).resolve()
        if path.is_dir():
            path /= "PKGBUILD"
        if not path.is_file() or path.name != "PKGBUILD":
            raise ValueError(f"not a PKGBUILD: {item}")
        paths.append(path)
    return sorted(set(paths))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=".", help="repository root")
    parser.add_argument("--path", action="append", help="PKGBUILD/package to validate; repeatable")
    parser.add_argument("--makepkg", action="store_true", help="also run makepkg --printsrcinfo when makepkg is installed")
    parser.add_argument("--warnings-as-errors", action="store_true")
    parser.add_argument("--json", action="store_true", help="emit findings as JSON")
    args = parser.parse_args()
    repo = Path(args.repo).resolve()
    if args.makepkg and not shutil.which("makepkg"):
        print("error: --makepkg requested but makepkg is not installed", file=sys.stderr)
        return 2

    result = ValidationResult()
    try:
        paths = pkgbuild_paths(repo, args.path)
    except ValueError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    for path in paths:
        validate_recipe(repo, path, args.makepkg, result)

    findings = result.errors + result.warnings
    if args.json:
        print(json.dumps([finding.__dict__ for finding in findings], indent=2))
    else:
        for finding in findings:
            print(f"{finding.level.upper():7} {finding.path}: {finding.message}")
        print(
            f"Validated {len(paths)} PKGBUILDs: {len(result.errors)} error(s), "
            f"{len(result.warnings)} warning(s)."
        )

    return 1 if result.errors or (args.warnings_as_errors and result.warnings) else 0


if __name__ == "__main__":
    raise SystemExit(main())
