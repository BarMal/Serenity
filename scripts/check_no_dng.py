#!/usr/bin/env python3
"""Fails when a Skiko native library carries the Adobe DNG SDK or the code that pulls it in.

The stock Skiko 0.153.0 natives for Linux and macOS contain the DNG SDK (Skia's RAW codec), whose licence conflicts
with the GPL (#2028). The natives Serenity ships are rebuilt without it. This scans native libraries, or jars holding
them, for what the DNG SDK leaves in a binary: its class and function names, Skia's RAW codec and PIEX, which only the
RAW codec uses.

    python3 scripts/check_no_dng.py path/to/skiko-awt-runtime-linux-x64.jar ...         # must find nothing
    python3 scripts/check_no_dng.py --expect-dng path/to/stock/skiko-awt-runtime-....jar  # must find something

`--expect-dng` is the positive control: run on a stock library it proves the scan can see what it looks for, so a
scanner that has quietly stopped working cannot pass the real check.
"""

from __future__ import annotations

import argparse
import sys
import zipfile
from pathlib import Path

# In report order. `dng_` is the prefix of every DNG SDK class and function; the others are what only exist beside it.
MARKERS = ("dng_", "SkRawCodec", "piex")


def scan_bytes(data: bytes) -> list[str]:
    """The markers present in `data`, as ASCII or as the UTF-16 a Windows binary stores its strings in."""
    return [
        marker
        for marker in MARKERS
        if marker.encode("ascii") in data or marker.encode("utf-16-le") in data
    ]


def scan_path(path: Path) -> dict[str, list[str]]:
    """Markers by library name, for a library or for each entry of a jar; libraries with none are left out."""
    if zipfile.is_zipfile(path):
        with zipfile.ZipFile(path) as archive:
            found = {
                info.filename: scan_bytes(archive.read(info))
                for info in archive.infolist()
                if not info.is_dir()
            }
    else:
        found = {path.name: scan_bytes(path.read_bytes())}
    return {name: markers for name, markers in found.items() if markers}


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("paths", nargs="*", type=Path, help="native libraries or jars holding them")
    parser.add_argument(
        "--expect-dng",
        action="store_true",
        help="positive control: succeed only if every path does carry DNG markers",
    )
    args = parser.parse_args(argv)

    if not args.paths:
        print("check_no_dng: no paths given, nothing was checked", file=sys.stderr)
        return 2
    missing = [path for path in args.paths if not path.is_file()]
    if missing:
        print(f"check_no_dng: not a file: {', '.join(map(str, missing))}", file=sys.stderr)
        return 2

    failed = False
    for path in args.paths:
        found = scan_path(path)
        if args.expect_dng:
            if found:
                print(f"OK    {path}: the scanner sees {sorted({m for ms in found.values() for m in ms})}")
            else:
                print(f"FAIL  {path}: nothing found in a stock library, so the scanner cannot be trusted")
                failed = True
        elif found:
            for name, markers in found.items():
                print(f"FAIL  {path}: {name} contains {', '.join(markers)}")
            failed = True
        else:
            print(f"OK    {path}: no DNG markers")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
