#!/usr/bin/env python3
"""Folds several `PerformanceBenchmarks` CSV runs of the same commit into one, keeping each benchmark's fastest p50
(with that run's p95). CI benchmarks a pull request's base and head on the same runner, alternating between them;
the fastest of each side's runs is the one least disturbed by whatever else the shared runner was doing, so comparing
fastest against fastest isolates the code change from runner noise.

Usage:
    combine_perf_runs.py <out.csv> <run.csv> [<run.csv> ...]

A run that is missing or has no rows (a benchmark process that crashed) is skipped. Exits 1, writing nothing, when no
run has any rows.
"""

from __future__ import annotations

import csv
import sys

from check_perf_regression import Row, parse_csv


def fastest(runs: list[dict[str, Row]]) -> dict[str, Row]:
    combined: dict[str, Row] = {}
    for run in runs:
        for name, row in run.items():
            if name not in combined or row.p50_ms < combined[name].p50_ms:
                combined[name] = row
    return combined


def write_csv(path: str, rows: dict[str, Row]) -> None:
    with open(path, "w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["name", "warmups", "iterations", "batch", "min_ms", "p50_ms", "p95_ms", "max_ms"])
        for name, row in sorted(rows.items()):
            writer.writerow([name, "", "", "", "", row.p50_ms, row.p95_ms, ""])


def main_with_args(argv: list[str]) -> int:
    out, *paths = argv
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
    write_csv(out, fastest(runs))
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
