#!/usr/bin/env python3
"""Lock and verify local PGO/BOLT implementation regions.

This guard is deliberately conservative.  It never tries to understand whether an
optimization is still useful; it makes an update fail when a protected local
implementation has changed.  Upstream metadata can therefore be refreshed
without silently replacing a locally trained PGO/BOLT pipeline.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

LOCK_SCHEMA = 2
MARKER_RE = re.compile(
    r"(?:\bpgo\b|\bbolt\b|f(?:cs-)?profile(?:-[a-z]+)?|"
    r"llvm-profdata|llvm-bolt|merge-fdata|profile-opt)",
    re.IGNORECASE,
)
FUNCTION_RE = re.compile(
    r"(?m)^(?:[ \t]*function[ \t]+)?[ \t]*"
    r"(?P<name>[A-Za-z_][A-Za-z0-9_]*)[ \t]*(?:\(\s*\))?[ \t]*\{"
)
HEREDOC_RE = re.compile(
    r"<<-?[ \t]*(?:'(?P<single>[A-Za-z_][A-Za-z0-9_]*)'|"
    r'"(?P<double>[A-Za-z_][A-Za-z0-9_]*)"|(?P<bare>[A-Za-z_][A-Za-z0-9_]*))'
)


@dataclass(frozen=True)
class FunctionRegion:
    name: str
    start: int
    end: int
    text: str


def sha256(text: str | bytes) -> str:
    if isinstance(text, str):
        text = text.encode()
    return hashlib.sha256(text).hexdigest()


def _line_without_quotes_or_comments(line: str, quote: str | None) -> tuple[str, str | None]:
    """Return shell-ish code suitable for brace counting.

    It is not a shell parser.  The only purpose is locating a function body that
    Bash has already syntax-checked.  Removing quoted strings and comments keeps
    braces in awk, sed, and documentation snippets from affecting the count.
    """

    result: list[str] = []
    i = 0
    escaped = False
    while i < len(line):
        char = line[i]
        if quote:
            if quote == '"' and escaped:
                escaped = False
            elif quote == '"' and char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            result.append(" ")
            i += 1
            continue

        if char in "'\"":
            quote = char
            result.append(" ")
            i += 1
            continue
        if char == "#":
            # This may also hide the pattern-removal form ${var#prefix}; that
            # construct does not occur in protected function headers/bodies in
            # this tree.  Favor a false refusal over a false pass.
            result.extend(" " * (len(line) - i))
            break
        result.append(char)
        i += 1
    return "".join(result), quote


def _heredoc_delimiters(line: str) -> list[str]:
    """Find simple here-document delimiters on one shell source line.

    The delimiter is commonly quoted (``<<'EOF'``), so it cannot be extracted
    from the quote-stripped text used for brace counting.  Walk only enough
    shell syntax to ignore comments and ordinary quoted strings while looking
    for a real ``<<`` operator.
    """

    delimiters: list[str] = []
    quote: str | None = None
    escaped = False
    index = 0
    while index < len(line):
        char = line[index]
        if quote:
            if quote == '"' and escaped:
                escaped = False
            elif quote == '"' and char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            index += 1
            continue
        if char == "#":
            break
        if char == "<" and index + 1 < len(line) and line[index + 1] == "<":
            match = HEREDOC_RE.match(line, index)
            if match:
                delimiters.append(next(value for value in match.groupdict().values() if value))
                index = match.end()
                continue
        if char in "'\"":
            quote = char
        index += 1
    return delimiters


def _function_end(text: str, open_brace: int) -> int | None:
    """Find the matching closing brace for a Bash function body.

    Here-doc bodies are skipped line-wise.  This supports the training programs
    embedded in several of the local PGO recipes and intentionally errs on the
    safe side: an unparseable body is omitted from a snapshot and causes the
    snapshot operation to fail.
    """

    lines = text.splitlines(keepends=True)
    offsets: list[int] = []
    offset = 0
    for line in lines:
        offsets.append(offset)
        offset += len(line)

    line_index = 0
    for index, start in enumerate(offsets):
        if start <= open_brace < start + len(lines[index]):
            line_index = index
            break

    depth = 0
    quote: str | None = None
    pending_heredocs: list[str] = []
    started = False

    for index in range(line_index, len(lines)):
        line = lines[index]
        line_start = offsets[index]
        stripped = line.rstrip("\n").lstrip("\t")
        if pending_heredocs:
            if stripped == pending_heredocs[0]:
                pending_heredocs.pop(0)
            continue

        if started:
            pending_heredocs.extend(_heredoc_delimiters(line))
        code, quote = _line_without_quotes_or_comments(line, quote)
        begin = 0
        if index == line_index:
            begin = open_brace - line_start

        for character_index in range(begin, len(code)):
            char = code[character_index]
            if char == "{":
                depth += 1
                started = True
            elif char == "}" and started:
                depth -= 1
                if depth == 0:
                    return line_start + character_index + 1
    return None


def function_regions(text: str) -> list[FunctionRegion]:
    """Return top-level function regions in source order."""

    regions: list[FunctionRegion] = []
    covered_until = -1
    for match in FUNCTION_RE.finditer(text):
        if match.start() < covered_until:
            continue
        open_brace = text.find("{", match.start(), match.end())
        if open_brace < 0:
            continue
        end = _function_end(text, open_brace)
        if end is None:
            continue
        regions.append(FunctionRegion(match.group("name"), match.start(), end, text[match.start() : end]))
        covered_until = end
    return regions


def protected_function_regions(text: str) -> list[FunctionRegion]:
    """Return PGO/BOLT functions and helpers transitively called by them."""

    regions = function_regions(text)
    by_name = {region.name: region for region in regions}
    protected = {region.name for region in regions if MARKER_RE.search(region.text)}

    # Helpers such as _build_components() often have no literal "PGO" in their
    # body, but are directly called by build().  Freeze that call closure too.
    changed = True
    while changed:
        changed = False
        for name in list(protected):
            body = by_name[name].text
            for possible_helper in by_name:
                if possible_helper in protected:
                    continue
                if re.search(rf"(?<![A-Za-z0-9_]){re.escape(possible_helper)}(?![A-Za-z0-9_])", body):
                    protected.add(possible_helper)
                    changed = True
    return [region for region in regions if region.name in protected]


def _line_offsets(text: str) -> Iterable[tuple[int, int, str]]:
    offset = 0
    for line in text.splitlines(keepends=True):
        yield offset, offset + len(line), line
        offset += len(line)


def orphan_marker_lines(text: str) -> list[str]:
    regions = function_regions(text)
    result: list[str] = []
    for start, end, line in _line_offsets(text):
        if any(region.start <= start and end <= region.end for region in regions):
            continue
        if MARKER_RE.search(line):
            result.append(line)
    return result


def package_auxiliary_entries(package_dir: Path) -> dict[str, str]:
    """Return hashes for tracked-looking local files that mention PGO/BOLT."""

    entries: dict[str, str] = {}
    for path in sorted(package_dir.rglob("*")):
        if not path.is_file() or path.name == "PKGBUILD" or path.stat().st_size > 2_000_000:
            continue
        raw = path.read_bytes()
        if b"\0" in raw:
            continue
        text = raw.decode("utf-8", errors="replace")
        if MARKER_RE.search(text):
            entries[path.relative_to(package_dir).as_posix()] = sha256(raw)
    return entries


def entry_for_pkgbuild(repo: Path, pkgbuild: Path) -> dict[str, object] | None:
    text = pkgbuild.read_text(encoding="utf-8", errors="replace")
    functions = protected_function_regions(text)
    orphan_lines = orphan_marker_lines(text)
    auxiliary = package_auxiliary_entries(pkgbuild.parent)
    if not functions and not orphan_lines and not auxiliary:
        return None
    return {
        "functions": {region.name: sha256(region.text) for region in functions},
        "orphan_marker_lines": [sha256(line) for line in orphan_lines],
        "auxiliary_files": auxiliary,
    }


def pkgbuild_paths(repo: Path, selected: list[str] | None = None) -> list[Path]:
    if selected:
        result: list[Path] = []
        for item in selected:
            path = (repo / item).resolve()
            if path.is_dir():
                path /= "PKGBUILD"
            if not path.is_file() or path.name != "PKGBUILD":
                raise ValueError(f"not a PKGBUILD: {item}")
            result.append(path)
        return sorted(set(result))
    return sorted((repo / "packages").rglob("PKGBUILD"))


def make_lock(repo: Path, selected: list[str] | None = None) -> dict[str, object]:
    entries: dict[str, object] = {}
    for pkgbuild in pkgbuild_paths(repo, selected):
        entry = entry_for_pkgbuild(repo, pkgbuild)
        if entry:
            entries[pkgbuild.relative_to(repo).as_posix()] = entry
    return {
        "schema": LOCK_SCHEMA,
        "hash": "sha256",
        "entries": dict(sorted(entries.items())),
    }


def _current_entry_for_text(repo: Path, relative_path: str, text: str) -> dict[str, object] | None:
    pkgbuild = repo / relative_path
    functions = protected_function_regions(text)
    orphan_lines = orphan_marker_lines(text)
    auxiliary = package_auxiliary_entries(pkgbuild.parent)
    if not functions and not orphan_lines and not auxiliary:
        return None
    return {
        "functions": {region.name: sha256(region.text) for region in functions},
        "orphan_marker_lines": [sha256(line) for line in orphan_lines],
        "auxiliary_files": auxiliary,
    }


def check_entry(repo: Path, relative_path: str, expected: dict[str, object], text: str | None = None) -> list[str]:
    """Validate one lock entry.  ``text`` supports update-candidate checking."""

    pkgbuild = repo / relative_path
    if not pkgbuild.exists():
        return [f"{relative_path}: locked PKGBUILD is missing"]
    if text is None:
        text = pkgbuild.read_text(encoding="utf-8", errors="replace")
    current = _current_entry_for_text(repo, relative_path, text)
    if current is None:
        return [f"{relative_path}: PGO/BOLT markers disappeared from a locked recipe"]

    errors: list[str] = []
    expected_functions = expected.get("functions", {})
    current_functions = current.get("functions", {})
    if not isinstance(expected_functions, dict) or not isinstance(current_functions, dict):
        return [f"{relative_path}: malformed optimization lock entry"]
    if expected_functions != current_functions:
        expected_names = set(expected_functions)
        current_names = set(current_functions)
        for name in sorted(expected_names | current_names):
            if expected_functions.get(name) != current_functions.get(name):
                errors.append(f"{relative_path}: protected function changed, added, or disappeared: {name}()")

    expected_lines = expected.get("orphan_marker_lines", [])
    current_lines = current.get("orphan_marker_lines", [])
    if not isinstance(expected_lines, list) or not isinstance(current_lines, list):
        errors.append(f"{relative_path}: malformed orphan marker lock entry")
    elif expected_lines != current_lines:
        errors.append(f"{relative_path}: protected top-level PGO/BOLT marker lines changed, added, or disappeared")

    expected_auxiliary = expected.get("auxiliary_files", {})
    current_auxiliary = current.get("auxiliary_files", {})
    if not isinstance(expected_auxiliary, dict) or not isinstance(current_auxiliary, dict):
        errors.append(f"{relative_path}: malformed auxiliary-file lock entry")
    elif expected_auxiliary != current_auxiliary:
        changed = sorted(set(expected_auxiliary) | set(current_auxiliary))
        errors.append(
            f"{relative_path}: protected auxiliary optimization file changed, added, or disappeared: "
            f"{', '.join(changed)}"
        )
    return errors


def check_lock(repo: Path, lock: dict[str, object], selected: list[str] | None = None) -> list[str]:
    if lock.get("schema") != LOCK_SCHEMA:
        return [f"unsupported optimization lock schema: {lock.get('schema')!r}"]
    entries = lock.get("entries")
    if not isinstance(entries, dict):
        return ["optimization lock has no entries object"]

    wanted: set[str] | None = None
    if selected:
        wanted = {path.relative_to(repo).as_posix() for path in pkgbuild_paths(repo, selected)}

    errors: list[str] = []
    for relative_path, expected in entries.items():
        if wanted is not None and relative_path not in wanted:
            continue
        if not isinstance(relative_path, str) or not isinstance(expected, dict):
            errors.append("optimization lock contains a malformed entry")
            continue
        errors.extend(check_entry(repo, relative_path, expected))

    # A newly-added optimization recipe must be deliberately snapshotted rather
    # than quietly bypassing the guard.
    for pkgbuild in pkgbuild_paths(repo, selected):
        relative_path = pkgbuild.relative_to(repo).as_posix()
        has_markers = MARKER_RE.search(pkgbuild.read_text(encoding="utf-8", errors="replace"))
        if has_markers and relative_path not in entries:
            errors.append(f"{relative_path}: contains PGO/BOLT markers but has no lock entry")
    return errors


def read_lock(path: Path) -> dict[str, object]:
    return json.loads(path.read_text(encoding="utf-8"))


def cmd_snapshot(args: argparse.Namespace) -> int:
    repo = Path(args.repo).resolve()
    target = (repo / args.lock).resolve()
    if target.exists() and not args.force:
        print(f"refusing to overwrite {target}; pass --force after reviewing the diff", file=sys.stderr)
        return 2
    lock = make_lock(repo, args.path)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(lock, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"wrote {target.relative_to(repo)} ({len(lock['entries'])} protected PKGBUILDs)")
    return 0


def cmd_check(args: argparse.Namespace) -> int:
    repo = Path(args.repo).resolve()
    lock_path = repo / args.lock
    if not lock_path.is_file():
        print(f"optimization lock not found: {lock_path}", file=sys.stderr)
        return 2
    errors = check_lock(repo, read_lock(lock_path), args.path)
    if errors:
        print("PGO/BOLT guard failed:", file=sys.stderr)
        print("\n".join(f"  - {error}" for error in errors), file=sys.stderr)
        return 1
    print("PGO/BOLT guard: all protected regions are byte-for-byte intact")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=".", help="repository root (default: current directory)")
    parser.add_argument("--lock", default="maintenance/optimization-lock.json", help="lock path relative to --repo")
    subparsers = parser.add_subparsers(dest="command", required=True)
    for name, handler in (("snapshot", cmd_snapshot), ("check", cmd_check)):
        subparser = subparsers.add_parser(name)
        subparser.add_argument("--path", action="append", help="PKGBUILD or package directory; repeatable")
        if name == "snapshot":
            subparser.add_argument("--force", action="store_true", help="replace an existing lock")
        subparser.set_defaults(handler=handler)
    args = parser.parse_args()
    try:
        return args.handler(args)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
