#!/usr/bin/env python3
"""Summarises async-profiler "collapsed" stack files (`root;frame;...;leaf <count>` per line): the top frames by self
samples and by inclusive samples, and the share of samples under any frame containing a given substring.

Usage:
    collapsed_top_frames.py [--top N] [--under SUBSTRING ...] FILE...
"""

from __future__ import annotations

import argparse
import sys
from collections import Counter
from collections.abc import Iterable

Stack = tuple[tuple[str, ...], int]


def parse(lines: Iterable[str]) -> list[Stack]:
    stacks: list[Stack] = []
    for line in lines:
        text = line.strip()
        stack, _, count = text.rpartition(" ")
        if not stack or not count.isdigit():
            continue
        stacks.append((tuple(stack.split(";")), int(count)))
    return stacks


def self_totals(stacks: list[Stack]) -> dict[str, int]:
    totals: Counter[str] = Counter()
    for frames, count in stacks:
        totals[frames[-1]] += count
    return dict(totals)


def inclusive_totals(stacks: list[Stack]) -> dict[str, int]:
    totals: Counter[str] = Counter()
    for frames, count in stacks:
        for frame in set(frames):
            totals[frame] += count
    return dict(totals)


def share_under(stacks: list[Stack], needle: str) -> float:
    total = sum(count for _, count in stacks)
    if total == 0:
        return 0.0
    matched = sum(count for frames, count in stacks if any(needle in frame for frame in frames))
    return matched / total


def percent(fraction: float) -> str:
    return f"{fraction * 100:.1f}%"


def _table(title: str, totals: dict[str, int], top: int, total: int) -> list[str]:
    ranked = sorted(totals.items(), key=lambda item: (-item[1], item[0]))[:top]
    rows = [f"{count:>8} {percent(count / total if total else 0.0):>7}  {frame}" for frame, count in ranked]
    return [title, f"{'samples':>8} {'percent':>7}  frame", *rows]


def report(stacks: list[Stack], top: int, needles: list[str]) -> str:
    total = sum(count for _, count in stacks)
    lines = [f"total samples: {total}", ""]
    lines += _table(f"top {top} self frames", self_totals(stacks), top, total)
    lines.append("")
    lines += _table(f"top {top} inclusive frames", inclusive_totals(stacks), top, total)
    if needles:
        lines.append("")
        lines += [f"under {needle}: {percent(share_under(stacks, needle))}" for needle in needles]
    return "\n".join(lines)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--top", type=int, default=10)
    parser.add_argument("--under", action="append", default=[], metavar="SUBSTRING")
    parser.add_argument("files", nargs="+", metavar="FILE")
    args = parser.parse_args(argv)
    stacks: list[Stack] = []
    for path in args.files:
        with open(path) as handle:
            stacks += parse(handle)
    print(report(stacks, args.top, args.under))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
