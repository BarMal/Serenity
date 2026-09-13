#!/usr/bin/env python3
"""Compares a fresh `PerformanceBenchmarks` CSV run against a stored baseline CSV, failing (exit 1) when a
benchmark's median (p50) latency regresses past a multiplier -- issue #1453: the benchmark suite had no CI
regression gate at all. The threshold (default 2x) is still generous because these numbers come from a shared CI
runner, not a dedicated perf box, but a 3x bar (the original #1503 wiring) let an actual 2x regression -- an
accidentally reintroduced O(n^2) path, a lost fast path -- through silently; 2x keeps that same noise tolerance while
actually catching that class of regression.

A missing baseline (a cache-cold run: cache eviction, or a branch with none yet) fails loudly by default rather than
silently passing with nothing compared -- that silence was the second gap an audit of #1503 found. Pass
`--allow-missing-baseline` only for the run that is meant to establish a fresh baseline (a push to master, where the
CI workflow saves this run's results as the new baseline regardless); every other caller should leave it unset.

Usage:
    check_perf_regression.py <baseline.csv> <current.csv> [--threshold 2.0] [--allow-missing-baseline]

Exit codes:
    0 - no regression past the threshold
    1 - at least one benchmark regressed past the threshold, or no baseline was found and
        --allow-missing-baseline was not passed, or the current run produced no benchmark rows
"""

from __future__ import annotations

import argparse
import csv
import sys
from dataclasses import dataclass


@dataclass(frozen=True)
class Row:
    name: str
    p50_ms: float
    p95_ms: float


def parse_csv(path: str) -> dict[str, Row]:
    """Parses `BenchmarkRunner.printResults`'s CSV: a title line, four `context,...` lines, a header line, then one
    data row per benchmark. Only the header/data rows (identifiable by having a numeric `p50_ms` field) are kept.
    """
    rows: dict[str, Row] = {}
    with open(path, newline="") as handle:
        reader = csv.reader(handle)
        for fields in reader:
            if len(fields) < 7 or fields[0] in ("context", "name"):
                continue
            try:
                name = fields[0]
                p50_ms = float(fields[5])
                p95_ms = float(fields[6])
            except (ValueError, IndexError):
                continue
            rows[name] = Row(name, p50_ms, p95_ms)
    return rows


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("baseline", help="Path to the previous run's CSV (may not exist yet)")
    parser.add_argument("current", help="Path to this run's CSV")
    parser.add_argument(
        "--threshold",
        type=float,
        default=2.0,
        help="Fail a benchmark whose current p50_ms exceeds baseline p50_ms times this multiplier (default: 2.0)",
    )
    # A benchmark's absolute floor is often sub-microsecond, where run-to-run jitter is dominated by timer
    # resolution/JIT noise rather than real cost -- guard against flagging that noise as a "regression" by ignoring
    # baselines faster than this floor.
    parser.add_argument(
        "--min-baseline-ms",
        type=float,
        default=0.001,
        help="Ignore benchmarks whose baseline p50_ms is below this floor (default: 0.001ms = 1us)",
    )
    parser.add_argument(
        "--allow-missing-baseline",
        action="store_true",
        help=(
            "If no baseline file is found, establish this run as the new baseline (exit 0) instead of failing. "
            "Use only for the run meant to seed/reseed the baseline (a push to master); leave unset everywhere "
            "else so a cache-cold run cannot pass without ever being compared."
        ),
    )
    return parser


def main_with_args(argv: list[str]) -> int:
    args = build_parser().parse_args(argv)

    try:
        baseline = parse_csv(args.baseline)
    except FileNotFoundError:
        if args.allow_missing_baseline:
            print(
                f"::notice::No prior baseline found at {args.baseline} -- establishing this run as the new "
                "baseline. Nothing was compared."
            )
            return 0
        print(
            f"::error::No baseline found at {args.baseline} -- cannot check this run for regressions. "
            "This is expected on the very first run or after a cache eviction; push to master to (re-)establish "
            "a baseline, then re-run this check."
        )
        return 1

    current = parse_csv(args.current)
    if not current:
        print(f"::error::No benchmark rows parsed from {args.current} -- the benchmark run likely failed.")
        return 1

    regressions: list[tuple[str, float, float, float]] = []
    print(f"{'benchmark':<55} {'baseline p50':>14} {'current p50':>14} {'ratio':>8}")
    for name, current_row in sorted(current.items()):
        baseline_row = baseline.get(name)
        if baseline_row is None:
            print(f"{name:<55} {'(new)':>14} {current_row.p50_ms:>14.5f} {'-':>8}")
            continue
        if baseline_row.p50_ms < args.min_baseline_ms:
            continue
        ratio = current_row.p50_ms / baseline_row.p50_ms
        flag = " <-- REGRESSION" if ratio > args.threshold else ""
        print(f"{name:<55} {baseline_row.p50_ms:>14.5f} {current_row.p50_ms:>14.5f} {ratio:>7.2f}x{flag}")
        if ratio > args.threshold:
            regressions.append((name, baseline_row.p50_ms, current_row.p50_ms, ratio))

    if regressions:
        print(f"\n::error::{len(regressions)} benchmark(s) regressed past {args.threshold}x baseline p50:")
        for name, base_ms, cur_ms, ratio in regressions:
            print(f"  - {name}: {base_ms:.5f}ms -> {cur_ms:.5f}ms ({ratio:.2f}x)")
        return 1

    print("\nNo benchmark regressed past the threshold.")
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
