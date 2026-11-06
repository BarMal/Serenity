#!/usr/bin/env python3
"""Writes `count` documents of distinct lines for bench-scaling.sh: `kind` prose (long lines, one paragraph a line) or
code (short lines). Distinct, because the app's text caches are keyed by text, so documents that repeat each other's
lines would share every entry and hide what many open files cost (issue #2478). The same arguments write the same files.

    gen-scaling-docs.py <dir> <count> prose|code [--lines N]
"""

import argparse
import os
import random
import string
import sys

PROSE_LINES_PER_PARAGRAPH = 8
WORDS_PER_PARAGRAPH = 100


def code_line(index: int, n: int) -> str:
    rng = random.Random(index * 1_000_003 + n)
    token = "".join(rng.choices(string.ascii_letters + string.digits, k=24))
    return f'val item_{index}_{n} = compute({index}, {n}, "{token}") // line {n} of file {index}'


def paragraph(index: int, n: int) -> str:
    rng = random.Random(index * 1_000_003 + n)
    words = ["".join(rng.choices(string.ascii_lowercase + string.digits, k=rng.randint(3, 9))) for _ in range(WORDS_PER_PARAGRAPH)]
    return " ".join(words).capitalize() + "."


def document(kind: str, index: int, lines: int) -> str:
    if kind == "code":
        return "\n".join(code_line(index, n) for n in range(1, lines + 1)) + "\n"
    return "\n\n".join(paragraph(index, n) for n in range(1, lines // PROSE_LINES_PER_PARAGRAPH + 1)) + "\n"


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("directory")
    parser.add_argument("count", type=int)
    parser.add_argument("kind", choices=("prose", "code"))
    parser.add_argument("--lines", type=int, default=2400, help="code lines, or prose lines of which one in eight is a paragraph")
    args = parser.parse_args(argv)
    os.makedirs(args.directory, exist_ok=True)
    extension = "txt" if args.kind == "prose" else "scala"
    for index in range(1, args.count + 1):
        with open(os.path.join(args.directory, f"scaling-{index:02d}.{extension}"), "w", encoding="utf-8") as handle:
            handle.write(document(args.kind, index, args.lines))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
