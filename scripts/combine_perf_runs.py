#!/usr/bin/env python3
"""Folds several `PerformanceBenchmarks` CSV runs of the same commit into one, keeping each benchmark's fastest (or,
with `--pick slowest`, slowest) p50 together with that run's p95.

CI benchmarks a pull request's base and head on the same runner, alternating between them, then compares the head's
fastest run against the base's slowest: a benchmark is flagged only when every head run is slower than every base run
by the threshold. Some benchmarks swing 2x between runs of identical code, so comparing fastest with fastest still let
one noisy run decide; a real regression slows every head run and is still caught.

Usage:
    combine_perf_runs.py [--pick fastest|slowest] <out.csv> <run.csv> [<run.csv> ...]

A run that is missing or has no rows (a benchmark process that crashed) is skipped. Exits 1, writing nothing, when no
run has any rows.
"""

from __future__ import annotations

import argparse
import csv
import sys

from check_perf_regression import Row, parse_csv


def pick(runs: list[dict[str, Row]], slowest: bool) -> dict[str, Row]:
    combined: dict[str, Row] = {}
    for run in runs:
        for name, row in run.items():
            kept = combined.get(name)
            if kept is None or (row.p50_ms > kept.p50_ms if slowest else row.p50_ms < kept.p50_ms):
                combined[name] = row
    return combined


def write_csv(path: str, rows: dict[str, Row]) -> None:
    with open(path, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["name", "warmups", "iterations", "batch", "min_ms", "p50_ms", "p95_ms", "max_ms"])
        for name, row in sorted(rows.items()):
            writer.writerow([name, "", "", "", "", row.p50_ms, row.p95_ms, ""])


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--pick", choices=("fastest", "slowest"), default="fastest")
    parser.add_argument("out")
    parser.add_argument("runs", nargs="+")
    args = parser.parse_args(argv)
    out, paths = args.out, args.runs
    runs: list[dict[str, Row]] = []
    for path in paths:
        try:
            run = parse_csv(path)
        except FileNotFoundError:
            print(f"::warning::Benchmark run {path} is missing; skipped.")
            continue
        if not run:
            print(f"::warning::Benchmark run {path} has no rows; skipped.")
            continue
        runs.append(run)
    if not runs:
        print(f"::error::None of {', '.join(paths)} has any benchmark rows.")
        return 1
    write_csv(out, pick(runs, slowest=args.pick == "slowest"))
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
