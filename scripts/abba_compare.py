#!/usr/bin/env python3
"""Applies the ABBA protocol's blocking thresholds to the eight CSVs `bench/abba.sh` collects (docs/performance-benchmarks.md,
"ABBA protocol"). A is the baseline build and B the candidate; the runs ran A B B A A B B A, so each side has four.

A benchmark blocks when B is worse than A by all of:

* time: the p50 ranges over each side's four runs do not overlap (B's fastest run is slower than A's slowest), B's median
  p50 is at least 5% slower, and at least 0.03 ms slower;
* allocation: B's median `allocation_p50_bytes` is at least 5% and at least 8 KiB above A's;
* footprint: B's median `retained_bytes` is more than 10% above A's.

Usage:
    abba_compare.py --a a1.csv a2.csv a3.csv a4.csv --b b1.csv b2.csv b3.csv b4.csv [--time-delta-ms 0.03]

Exit codes: 0 nothing blocks, 1 at least one benchmark blocks, 2 a side has no runs or the CSVs share no benchmark.
"""

from __future__ import annotations

import argparse
import statistics
import sys
from dataclasses import dataclass

from check_perf_regression import Row, parse_csv

TIME_RATIO = 1.05
TIME_DELTA_MS = 0.03
ALLOCATION_RATIO = 1.05
ALLOCATION_DELTA_BYTES = 8 * 1024
RETAINED_RATIO = 1.10


@dataclass(frozen=True)
class Finding:
    name: str
    metric: str
    detail: str


def _values(runs: list[dict[str, Row]], name: str, field: str) -> list[float]:
    found = [getattr(run[name], field) for run in runs if name in run]
    return [float(v) for v in found if v is not None]


def time_finding(
    name: str, a: list[dict[str, Row]], b: list[dict[str, Row]], time_delta_ms: float = TIME_DELTA_MS
) -> Finding | None:
    a_p50, b_p50 = _values(a, name, "p50_ms"), _values(b, name, "p50_ms")
    if not a_p50 or not b_p50 or max(a_p50) <= 0.0:
        return None
    a_median, b_median = statistics.median(a_p50), statistics.median(b_p50)
    if min(b_p50) > max(a_p50) and b_median >= a_median * TIME_RATIO and b_median - a_median >= time_delta_ms:
        return Finding(
            name,
            "p50_ms",
            f"A {min(a_p50):.4f}-{max(a_p50):.4f} (median {a_median:.4f}), "
            f"B {min(b_p50):.4f}-{max(b_p50):.4f} (median {b_median:.4f}) ms",
        )
    return None


def allocation_finding(name: str, a: list[dict[str, Row]], b: list[dict[str, Row]]) -> Finding | None:
    a_bytes, b_bytes = _values(a, name, "allocation_p50_bytes"), _values(b, name, "allocation_p50_bytes")
    if not a_bytes or not b_bytes:
        return None
    a_median, b_median = statistics.median(a_bytes), statistics.median(b_bytes)
    if b_median >= a_median * ALLOCATION_RATIO and b_median - a_median >= ALLOCATION_DELTA_BYTES:
        return Finding(name, "allocation_p50_bytes", f"A median {a_median:.0f}, B median {b_median:.0f} bytes")
    return None


def retained_finding(name: str, a: list[dict[str, Row]], b: list[dict[str, Row]]) -> Finding | None:
    a_bytes, b_bytes = _values(a, name, "retained_bytes"), _values(b, name, "retained_bytes")
    if not a_bytes or not b_bytes:
        return None
    a_median, b_median = statistics.median(a_bytes), statistics.median(b_bytes)
    if a_median > 0 and b_median > a_median * RETAINED_RATIO:
        return Finding(name, "retained_bytes", f"A median {a_median:.0f}, B median {b_median:.0f} bytes")
    return None


def compare(
    a: list[dict[str, Row]], b: list[dict[str, Row]], time_delta_ms: float = TIME_DELTA_MS
) -> list[Finding]:
    names = sorted({name for run in a for name in run} & {name for run in b for name in run})
    checks = (lambda n, x, y: time_finding(n, x, y, time_delta_ms), allocation_finding, retained_finding)
    return [finding for name in names for check in checks if (finding := check(name, a, b)) is not None]


def _load_runs(paths: list[str]) -> list[dict[str, Row]]:
    runs = []
    for path in paths:
        try:
            run = parse_csv(path)
        except FileNotFoundError:
            print(f"::warning::Benchmark run {path} is missing; skipped.")
            continue
        if run:
            runs.append(run)
    return runs


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--a", nargs="+", required=True, help="CSVs of the baseline build, in run order")
    parser.add_argument("--b", nargs="+", required=True, help="CSVs of the candidate build, in run order")
    parser.add_argument(
        "--time-delta-ms",
        type=float,
        default=TIME_DELTA_MS,
        help=f"Smallest p50 slowdown that can block (default {TIME_DELTA_MS} ms; start-up rows use a much larger one)",
    )
    args = parser.parse_args(argv)
    a_runs, b_runs = _load_runs(args.a), _load_runs(args.b)
    if not a_runs or not b_runs:
        print("::error::A side of the comparison has no benchmark rows.")
        return 2
    shared = {n for run in a_runs for n in run} & {n for run in b_runs for n in run}
    if not shared:
        print("::error::The A and B runs share no benchmark.")
        return 2
    findings = compare(a_runs, b_runs, args.time_delta_ms)
    for finding in findings:
        print(f"::error::{finding.name} {finding.metric}: {finding.detail}")
    print(f"{len(shared)} benchmarks compared, {len(findings)} blocking.")
    return 1 if findings else 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
