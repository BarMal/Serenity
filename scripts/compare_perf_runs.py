#!/usr/bin/env python3
"""Decides whether a pull request slowed a benchmark, from `PerformanceBenchmarks` CSV runs of its base and head commits
taken in alternation on one runner (issue #1731).

The base and head runs are paired by position: `base-1` and `head-1` ran back to back, so a slow spell on the runner
tends to hit both of a pair. For each benchmark the verdict is the median of the pairs' head/base ratios, compared with
the threshold. A benchmark fails only when

  * that median ratio exceeds `--threshold`,
  * the median head p50 exceeds the median base p50 by more than `--min-regression-delta-ms`, and
  * the head and base runs were each measured at least `--min-pairs` times.

Why the median of paired ratios, and not the earlier "head's fastest p50 against base's slowest":

  * It tests the quantity a regression changes, the ratio, and a run that was slow for a reason of its own (a GC pause,
    a JIT plateau, a noisy neighbour) moves one pair, not the verdict. Using the extremes asks the fastest head run to
    beat the slowest base run, so a base that swings 5 to 30 ms hides a 2x slowdown that lands inside the swing.
  * A threshold below 2x still catches a 2x regression, because the verdict no longer has to clear a swing the size of
    the regression. `scripts/study_perf_gate.py` reports the false-positive and false-negative rates of both rules on
    recorded runs.

A benchmark whose base runs themselves disagree by more than `--unstable-spread` is reported as unstable rather than
silently trusted either way.

Usage:
    compare_perf_runs.py --base b1.csv b2.csv b3.csv --head h1.csv h2.csv h3.csv [--threshold 1.75]

Exit codes: 0 no regression, 1 a regression, or fewer than `--min-pairs` usable pairs for every benchmark.
"""

from __future__ import annotations

import argparse
import statistics
import sys
from dataclasses import dataclass

from check_perf_regression import Row, build_parser, check_memory, parse_csv

DEFAULT_THRESHOLD = 1.75
DEFAULT_MIN_DELTA_MS = 0.05
DEFAULT_MIN_PAIRS = 2
DEFAULT_UNSTABLE_SPREAD = 2.0
MIN_BASELINE_MS = 0.001


@dataclass(frozen=True)
class Verdict:
    name: str
    base_ms: float
    head_ms: float
    ratio: float
    pairs: int
    base_spread: float
    regressed: bool
    unstable: bool


def load_runs(paths: list[str]) -> list[dict[str, Row]]:
    """A missing or empty run is kept as an empty dict so the pairing by position survives it."""
    runs: list[dict[str, Row]] = []
    for path in paths:
        try:
            runs.append(parse_csv(path))
        except FileNotFoundError:
            runs.append({})
    return runs


def _spread(values: list[float]) -> float:
    return max(values) / min(values) if values and min(values) > 0 else 1.0


def judge(
    base_runs: list[dict[str, Row]],
    head_runs: list[dict[str, Row]],
    threshold: float = DEFAULT_THRESHOLD,
    min_delta_ms: float = DEFAULT_MIN_DELTA_MS,
    min_pairs: int = DEFAULT_MIN_PAIRS,
    unstable_spread: float = DEFAULT_UNSTABLE_SPREAD,
) -> list[Verdict]:
    verdicts: list[Verdict] = []
    names = sorted(set().union(*[set(run) for run in base_runs + head_runs]))
    for name in names:
        pairs = [
            (base[name].p50_ms, head[name].p50_ms)
            for base, head in zip(base_runs, head_runs)
            if name in base and name in head
        ]
        if len(pairs) < min_pairs:
            continue
        bases = [b for b, _ in pairs]
        heads = [h for _, h in pairs]
        base_ms, head_ms = statistics.median(bases), statistics.median(heads)
        if base_ms < MIN_BASELINE_MS:
            continue
        ratio = statistics.median(h / b for b, h in pairs if b > 0)
        spread = _spread(bases)
        verdicts.append(
            Verdict(
                name=name,
                base_ms=base_ms,
                head_ms=head_ms,
                ratio=ratio,
                pairs=len(pairs),
                base_spread=spread,
                regressed=ratio > threshold and head_ms - base_ms > min_delta_ms,
                unstable=spread > unstable_spread,
            )
        )
    return verdicts


def _median_int(values: list[int | None]) -> int | None:
    present = [v for v in values if v is not None]
    return int(statistics.median(present)) if present else None


def median_rows(runs: list[dict[str, Row]]) -> dict[str, Row]:
    """Each benchmark's median p50, p95, allocation and retained heap across the runs that measured it."""
    rows: dict[str, Row] = {}
    for name in sorted(set().union(*[set(run) for run in runs])):
        seen = [run[name] for run in runs if name in run]
        rows[name] = Row(
            name,
            statistics.median(r.p50_ms for r in seen),
            statistics.median(r.p95_ms for r in seen),
            _median_int([r.allocation_p50_bytes for r in seen]),
            _median_int([r.allocation_p95_bytes for r in seen]),
            _median_int([r.retained_bytes for r in seen]),
        )
    return rows


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--base", nargs="+", required=True, help="Base-commit run CSVs, in run order")
    parser.add_argument("--head", nargs="+", required=True, help="Head-commit run CSVs, paired with --base by position")
    parser.add_argument("--threshold", type=float, default=DEFAULT_THRESHOLD)
    parser.add_argument("--min-regression-delta-ms", type=float, default=DEFAULT_MIN_DELTA_MS)
    parser.add_argument("--min-pairs", type=int, default=DEFAULT_MIN_PAIRS)
    parser.add_argument("--unstable-spread", type=float, default=DEFAULT_UNSTABLE_SPREAD)
    args = parser.parse_args(argv)

    if len(args.base) != len(args.head):
        print(f"::error::{len(args.base)} base runs but {len(args.head)} head runs; they are compared in pairs.")
        return 1
    base_runs, head_runs = load_runs(args.base), load_runs(args.head)
    if not any(head_runs):
        print("::error::No head run produced any benchmark rows -- the suite failed on this commit.")
        return 1
    verdicts = judge(
        base_runs, head_runs, args.threshold, args.min_regression_delta_ms, args.min_pairs, args.unstable_spread
    )
    if not verdicts:
        print(f"::error::No benchmark has {args.min_pairs} usable base/head pairs -- nothing was compared.")
        return 1

    print(f"{'benchmark':<55} {'base p50':>12} {'head p50':>12} {'ratio':>7} {'pairs':>5}")
    for v in verdicts:
        flag = " <-- REGRESSION" if v.regressed else (" (unstable base)" if v.unstable else "")
        print(f"{v.name:<55} {v.base_ms:>12.5f} {v.head_ms:>12.5f} {v.ratio:>6.2f}x {v.pairs:>5}{flag}")
    regressions = [v for v in verdicts if v.regressed]
    for v in regressions:
        print(
            f"::error::{v.name}: median head/base ratio {v.ratio:.2f}x over {v.pairs} paired runs "
            f"({v.base_ms:.5f}ms -> {v.head_ms:.5f}ms), past {args.threshold}x."
        )
    unstable = [v for v in verdicts if v.unstable and not v.regressed]
    if unstable:
        print(f"::notice::{len(unstable)} benchmark(s) had base runs more than {args.unstable_spread}x apart; see log.")
    memory_failures = check_memory(median_rows(base_runs), median_rows(head_runs), build_parser().parse_args(["-", "-"]))
    if regressions or memory_failures:
        return 1
    print("\nNo benchmark regressed past the threshold.")
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
