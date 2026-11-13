#!/usr/bin/env python3
"""Keep every file attached to the nightly release verifiable while Windows and macOS builds are off (#2618).

    python3 scripts/carry_checksums.py --fresh dist/SHA256SUMS --previous prev/SHA256SUMS --release-json release.json

With CROSS_OS_CI off the nightly uploads only the Linux files and the SBOM, and the Windows and macOS assets from an
earlier build stay attached. `--fresh` holds the lines for what was just built; this adds a line for each of those older
assets, taken from the previous release's SHA256SUMS, and rewrites `--fresh` in place.

A carried line must agree with the digest GitHub recorded for the attached asset (`--release-json` is the release as the
REST API returns it). A line that disagrees, or is missing, is replaced by the recorded digest, and an asset with neither
is left out; each of those prints a `::warning::` so the run says what it could not vouch for. A missing previous file or
release is normal on the first nightly and leaves the fresh lines as they are.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any, Sequence

CARRIED = (
    "Serenity-windows-x64.jar",
    "Serenity-windows-x64.zip",
    "Serenity-macos-arm64.jar",
    "Serenity-macos-arm64.zip",
)

_LINE = re.compile(r"^([0-9a-f]{64}) [ *](.+)$")


def _lines(text: str | None) -> dict[str, str]:
    found: dict[str, str] = {}
    for line in (text or "").splitlines():
        match = _LINE.match(line)
        if match is not None:
            found[match.group(2)] = match.group(1)
    return found


def _recorded(digest: str | None) -> str | None:
    return digest.removeprefix("sha256:") if digest is not None and digest.startswith("sha256:") else None


def merge(fresh: str, previous: str | None, attached: dict[str, str | None]) -> tuple[str, list[str]]:
    """The fresh lines followed by one line for each carried asset that is still attached, and the warnings."""
    before = _lines(previous)
    carried: list[str] = []
    warnings: list[str] = []
    for name in CARRIED:
        if name not in attached:
            continue
        recorded = _recorded(attached[name])
        listed = before.get(name)
        if recorded is None and listed is None:
            warnings.append(f"{name} is attached but neither the previous SHA256SUMS nor GitHub has its checksum; left out.")
            continue
        if recorded is None:
            warnings.append(f"GitHub recorded no digest for {name}; carrying the previous SHA256SUMS line unchecked.")
            chosen = listed
        elif listed is None:
            warnings.append(f"The previous SHA256SUMS has no line for {name}; using the digest GitHub recorded.")
            chosen = recorded
        elif listed != recorded:
            warnings.append(f"The previous SHA256SUMS line for {name} does not match the attached asset; using the digest GitHub recorded.")
            chosen = recorded
        else:
            chosen = listed
        carried.append(f"{chosen}  {name}\n")
    head = fresh if fresh == "" or fresh.endswith("\n") else fresh + "\n"
    return head + "".join(carried), warnings


def _read(path: Path) -> str | None:
    return path.read_text() if path.is_file() else None


def _attached(release: Any) -> dict[str, str | None]:
    assets = release.get("assets") if isinstance(release, dict) else None
    return {
        asset["name"]: asset.get("digest")
        for asset in (assets if isinstance(assets, list) else [])
        if isinstance(asset, dict) and isinstance(asset.get("name"), str)
    }


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--fresh", type=Path, required=True)
    parser.add_argument("--previous", type=Path, required=True)
    parser.add_argument("--release-json", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        release = json.loads(_read(args.release_json) or "null")
    except json.JSONDecodeError:
        release = None
    text, warnings = merge(args.fresh.read_text(), _read(args.previous), _attached(release))
    args.fresh.write_text(text)
    for warning in warnings:
        print(f"::warning::{warning}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
