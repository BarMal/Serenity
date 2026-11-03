#!/usr/bin/env python3
"""Capture and compare the shipped build outputs (#2502).

    python3 -m scripts.build_outputs capture OUT_DIR --jar Serenity.jar --jar serenity-perf.jar --file THIRD-PARTY-NOTICES.md
    python3 -m scripts.build_outputs compare BEFORE_DIR AFTER_DIR

`capture` writes, per jar, a sorted `name<TAB>size<TAB>crc` entry list and its manifest, and a copy of each plain file.
`compare` prints every difference and exits 1 if there is any. Entry timestamps are ignored: only names, sizes and
contents count.
"""

from __future__ import annotations

import argparse
import shutil
import sys
import zipfile
from pathlib import Path


def jar_entries(jar: Path) -> list[str]:
    with zipfile.ZipFile(jar) as archive:
        return sorted(f"{info.filename}\t{info.file_size}\t{info.CRC:08x}" for info in archive.infolist())


def _manifest(jar: Path) -> str:
    with zipfile.ZipFile(jar) as archive:
        try:
            return archive.read("META-INF/MANIFEST.MF").decode("utf-8", "replace")
        except KeyError:
            return ""


def capture(out: Path, jars: list[Path], files: list[Path]) -> None:
    out.mkdir(parents=True, exist_ok=True)
    for jar in jars:
        (out / f"{jar.name}.entries").write_text("\n".join(jar_entries(jar)) + "\n", encoding="utf-8")
        (out / f"{jar.name}.manifest").write_text(_manifest(jar), encoding="utf-8")
    for file in files:
        shutil.copyfile(file, out / file.name)


def compare(before: Path, after: Path) -> list[str]:
    problems: list[str] = []
    names = sorted({p.name for p in before.iterdir()} | {p.name for p in after.iterdir()})
    for name in names:
        left, right = before / name, after / name
        if not left.exists() or not right.exists():
            problems.append(f"{name}: present only {'before' if left.exists() else 'after'}")
        elif name.endswith(".entries"):
            old, new = set(left.read_text().splitlines()), set(right.read_text().splitlines())
            gone, added = sorted(old - new), sorted(new - old)
            if gone or added:
                problems.append(
                    f"{name}: {len(gone)} entries gone or changed, {len(added)} added or changed\n"
                    + "\n".join(f"  - {line}" for line in gone[:20])
                    + "\n"
                    + "\n".join(f"  + {line}" for line in added[:20])
                )
        elif left.read_bytes() != right.read_bytes():
            problems.append(f"{name}: contents differ")
    return problems


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    cap = sub.add_parser("capture")
    cap.add_argument("out", type=Path)
    cap.add_argument("--jar", action="append", default=[], type=Path)
    cap.add_argument("--file", action="append", default=[], type=Path)
    cmp = sub.add_parser("compare")
    cmp.add_argument("before", type=Path)
    cmp.add_argument("after", type=Path)
    args = parser.parse_args(argv)
    if args.command == "capture":
        capture(args.out, args.jar, args.file)
        return 0
    problems = compare(args.before, args.after)
    print("\n".join(problems) if problems else "build outputs identical")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
