#!/usr/bin/env python3
"""Compares a fresh `PerformanceBenchmarks` CSV run against a stored baseline CSV, failing (exit 1) when a
benchmark's median (p50) latency regresses past a multiplier -- issue #1453: the benchmark suite had no CI
regression gate at all. The threshold (default 2x) is still generous because these numbers come from a shared CI
runner, not a dedicated perf box, but a 3x bar (the original #1503 wiring) let an actual 2x regression -- an
accidentally reintroduced O(n^2) path, a lost fast path -- through silently; 2x keeps that same noise tolerance while
actually catching that class of regression.

A regression must clear the ratio threshold AND an absolute p50 delta floor (`--min-regression-delta-ms`) before it is
flagged. The ratio alone over-fires on benchmarks whose baseline is small in absolute terms: a false-positive audit
found `reducer.*`, `damage.*`, `lsp.framer.large_batch`, and `render.markdown.inline_lens` flagging on baselines in
the low single-digit milliseconds where ordinary run-to-run jitter (a few hundred microseconds on a shared CI runner)
is itself enough to clear 2x. `--min-baseline-ms` (below) is a different guard -- it ignores a benchmark whose
*baseline* sits at the timer-resolution floor; this floor ignores a regression whose *delta* is too small to be
anything but noise, regardless of how large the baseline is. Both guards must pass for a ratio-flagged benchmark to
actually fail the build.

Memory: `footprint.*` rows carry `retained_bytes` (heap held by a built state after GC) and fail when current exceeds
1.25x baseline AND grows by more than 1 MiB. `allocation_p50_bytes` regressions past 1.5x with a delta over 64 KiB are
printed as warnings only, unless `--gate-allocation` is passed. A baseline lacking these columns is skipped with a note.

A missing baseline (a cache-cold run: cache eviction, or a branch with none yet) fails loudly by default rather than
silently passing with nothing compared -- that silence was the second gap an audit of #1503 found. Pass
`--allow-missing-baseline` only for the run that is meant to establish a fresh baseline (a push to master, where the
CI workflow saves this run's results as the new baseline regardless); every other caller should leave it unset.

Usage:
    check_perf_regression.py <baseline.csv> <current.csv> [--threshold 2.0] [--min-regression-delta-ms 0.05]
        [--allow-missing-baseline]

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
    allocation_p50_bytes: int | None = None
    allocation_p95_bytes: int | None = None
    retained_bytes: int | None = None


def _optional_bytes(fields: list[str], index: int) -> int | None:
    if index >= len(fields) or not fields[index].strip():
        return None
    try:
        return int(float(fields[index]))
    except ValueError:
        return None


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
            rows[name] = Row(
                name,
                p50_ms,
                p95_ms,
                _optional_bytes(fields, 8),
                _optional_bytes(fields, 9),
                _optional_bytes(fields, 10),
            )
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
    # The false-positive audit (issue #1453 follow-up) found five ratio-only flags on `reducer.*`, `damage.*`,
    # `lsp.framer.large_batch`, and `render.markdown.inline_lens` -- all benchmarks whose baseline p50 sits in the
    # low single-digit milliseconds or below, where run-to-run jitter on a shared CI runner is itself on the order of
    # tens to a couple hundred microseconds. 0.05ms (50us) sits above that jitter band (see the empirical multi-run
    # analysis in docs/performance-benchmarks.md) while still catching a real regression: a benchmark would need to
    # both clear the ratio threshold *and* move by at least 50us in absolute terms, which noise alone should not do.
    parser.add_argument(
        "--min-regression-delta-ms",
        type=float,
        default=0.05,
        help=(
            "Ignore a ratio-flagged regression whose absolute p50 delta (current - baseline) is below this floor "
            "(default: 0.05ms = 50us). This is a separate guard from --min-baseline-ms: that one ignores benchmarks "
            "with a tiny baseline, this one ignores regressions with a tiny delta regardless of the baseline size."
        ),
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
    parser.add_argument(
        "--retained-threshold",
        type=float,
        default=1.25,
        help="Fail a footprint.* row whose retained_bytes exceeds baseline times this multiplier (default: 1.25)",
    )
    parser.add_argument(
        "--min-retained-delta-bytes",
        type=int,
        default=1024 * 1024,
        help="Ignore a retained-heap regression whose absolute growth is below this floor (default: 1 MiB)",
    )
    parser.add_argument(
        "--allocation-threshold",
        type=float,
        default=1.5,
        help="Flag a benchmark whose allocation_p50_bytes exceeds baseline times this multiplier (default: 1.5)",
    )
    parser.add_argument(
        "--min-allocation-delta-bytes",
        type=int,
        default=64 * 1024,
        help="Ignore an allocation regression whose absolute growth is below this floor (default: 64 KiB)",
    )
    parser.add_argument(
        "--gate-allocation",
        action="store_true",
        help="Fail the build on allocation regressions instead of only printing warnings (default: report-only)",
    )
    return parser


def _exceeds(baseline: int, current: int, ratio: float, min_delta: int) -> bool:
    return baseline > 0 and current / baseline > ratio and current - baseline > min_delta


def check_memory(baseline: dict[str, Row], current: dict[str, Row], args: argparse.Namespace) -> int:
    """Gates retained heap and reports allocation; returns the number of failures. A side without the column is
    skipped with a note, so an old baseline cannot crash or fail the check."""
    failures = 0
    skipped_retained = skipped_allocation = 0
    for name, now in sorted(current.items()):
        before = baseline.get(name)
        if before is None:
            continue
        if now.retained_bytes is not None:
            if before.retained_bytes is None:
                skipped_retained += 1
            elif _exceeds(before.retained_bytes, now.retained_bytes, args.retained_threshold, args.min_retained_delta_bytes):
                failures += 1
                print(
                    f"::error::{name}: retained heap {before.retained_bytes} -> {now.retained_bytes} bytes "
                    f"({now.retained_bytes / before.retained_bytes:.2f}x, +{now.retained_bytes - before.retained_bytes} bytes)"
                )
        if now.allocation_p50_bytes is not None:
            if before.allocation_p50_bytes is None:
                skipped_allocation += 1
            elif _exceeds(
                before.allocation_p50_bytes, now.allocation_p50_bytes, args.allocation_threshold, args.min_allocation_delta_bytes
            ):
                level = "error" if args.gate_allocation else "warning"
                failures += 1 if args.gate_allocation else 0
                print(
                    f"::{level}::{name}: allocation p50 {before.allocation_p50_bytes} -> {now.allocation_p50_bytes} "
                    f"bytes/invocation ({now.allocation_p50_bytes / before.allocation_p50_bytes:.2f}x)"
                )
    if skipped_retained:
        print(f"::notice::{skipped_retained} footprint row(s) skipped: baseline has no retained_bytes column/value.")
    if skipped_allocation:
        print(f"::notice::{skipped_allocation} row(s) skipped for allocation: baseline has no allocation value.")
    return failures


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
        delta_ms = current_row.p50_ms - baseline_row.p50_ms
        is_regression = ratio > args.threshold and delta_ms > args.min_regression_delta_ms
        flag = " <-- REGRESSION" if is_regression else ""
        print(f"{name:<55} {baseline_row.p50_ms:>14.5f} {current_row.p50_ms:>14.5f} {ratio:>7.2f}x{flag}")
        if is_regression:
            regressions.append((name, baseline_row.p50_ms, current_row.p50_ms, ratio))

    memory_failures = check_memory(baseline, current, args)

    if regressions:
        print(f"\n::error::{len(regressions)} benchmark(s) regressed past {args.threshold}x baseline p50:")
        for name, base_ms, cur_ms, ratio in regressions:
            print(f"  - {name}: {base_ms:.5f}ms -> {cur_ms:.5f}ms ({ratio:.2f}x)")
        return 1

    if memory_failures:
        return 1

    print("\nNo benchmark regressed past the threshold.")
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
