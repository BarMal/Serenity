#!/usr/bin/env python3
"""Measures the false-positive and false-negative rates of the performance gates on recorded `PerformanceBenchmarks`
runs (issue #1731), so a gate is chosen by what it did to real run-to-run noise rather than by argument.

Usage:
    study_perf_gate.py --plain run1.csv run2.csv ... [--slowed slow1.csv ...] [--pairs 3] [--samples 2000]

`--plain` runs are one commit measured repeatedly. A trial draws `--pairs` of them as the base side and `--pairs`
others as the head side (so base and head are the same code and any flag is a false positive), and judges them with
each rule. With `--slowed`, the head side is instead drawn from runs made with `-Dserenity.bench.slowdown=2`, and a
rule that does not flag is a false negative. Without them, `--inject FACTOR` multiplies a plain head run's p50 by the
factor instead -- a synthetic slowdown, for factors there is no recorded run of.

A trial is counted per suite (any benchmark flagged fails the build, as in CI) and per benchmark.

Rules compared:
    old       head's fastest p50 against base's slowest, ratio > 2.0 and delta > 0.05 ms, over 2 runs a side
    paired-R  median of paired head/base ratios over `--pairs` pairs, ratio > R (R in --thresholds)
"""

from __future__ import annotations

import argparse
import random
import sys
from dataclasses import replace

from check_perf_regression import Row, parse_csv
from combine_perf_runs import pick
from compare_perf_runs import judge

OLD_THRESHOLD = 2.0
DELTA_MS = 0.05
MIN_BASELINE_MS = 0.001


def old_rule(base_runs: list[dict[str, Row]], head_runs: list[dict[str, Row]]) -> set[str]:
    base, head = pick(base_runs[:2], slowest=True), pick(head_runs[:2], slowest=False)
    flagged = set()
    for name, now in head.items():
        before = base.get(name)
        if before is None or before.p50_ms < MIN_BASELINE_MS:
            continue
        if now.p50_ms / before.p50_ms > OLD_THRESHOLD and now.p50_ms - before.p50_ms > DELTA_MS:
            flagged.add(name)
    return flagged


def paired_rule(threshold: float):
    def rule(base_runs: list[dict[str, Row]], head_runs: list[dict[str, Row]]) -> set[str]:
        return {v.name for v in judge(base_runs, head_runs, threshold=threshold) if v.regressed}

    return rule


def slowed(run: dict[str, Row], factor: float) -> dict[str, Row]:
    return {name: replace(row, p50_ms=row.p50_ms * factor) for name, row in run.items()}


def trial(rng: random.Random, plain: list[dict[str, Row]], heads: list[dict[str, Row]] | None, pairs: int, inject: float):
    if heads is None:
        drawn = rng.sample(plain, 2 * pairs)
        base, head = drawn[:pairs], drawn[pairs:]
        if inject != 1.0:
            head = [slowed(run, inject) for run in head]
    else:
        base = rng.sample(plain, pairs)
        head = rng.sample(heads, pairs)
    return base, head


def study(
    plain: list[dict[str, Row]],
    slowed_runs: list[dict[str, Row]] | None,
    pairs: int,
    samples: int,
    thresholds: list[float],
    inject: float,
    seed: int = 1731,
) -> dict[str, tuple[float, float]]:
    """Per rule: (share of trials that flag anything, mean benchmarks flagged per trial)."""
    rules = {"old": old_rule, **{f"paired-{t}": paired_rule(t) for t in thresholds}}
    flags: dict[str, list[int]] = {name: [] for name in rules}
    rng = random.Random(seed)
    for _ in range(samples):
        base, head = trial(rng, plain, slowed_runs, pairs, inject)
        for name, rule in rules.items():
            flags[name].append(len(rule(base, head)))
    return {name: (sum(1 for n in counts if n) / len(counts), sum(counts) / len(counts)) for name, counts in flags.items()}


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--plain", nargs="+", required=True)
    parser.add_argument("--slowed", nargs="*", default=None)
    parser.add_argument("--pairs", type=int, default=3)
    parser.add_argument("--samples", type=int, default=2000)
    parser.add_argument("--thresholds", type=float, nargs="+", default=[1.3, 1.5, 1.75, 2.0])
    parser.add_argument("--inject", type=float, default=1.0)
    args = parser.parse_args(argv)
    plain = [run for run in map(parse_csv, args.plain) if run]
    slowed_runs = [run for run in map(parse_csv, args.slowed) if run] if args.slowed else None
    need = 2 * args.pairs if slowed_runs is None else max(args.pairs, 2)
    if len(plain) < need or (slowed_runs is not None and len(slowed_runs) < args.pairs):
        print(f"::error::Need at least {need} plain runs (and {args.pairs} slowed ones with --slowed).")
        return 1
    result = study(plain, slowed_runs, args.pairs, args.samples, args.thresholds, args.inject)
    print(f"{len(plain)} plain runs, {len(slowed_runs or [])} slowed, {args.pairs} pairs, {args.samples} trials")
    verdicts = judge(plain[: args.pairs], plain[args.pairs : 2 * args.pairs])
    eligible = sum(1 for v in verdicts if v.base_ms * 2 - v.base_ms > DELTA_MS)
    print(f"{eligible} benchmarks are slow enough that doubling them clears the {DELTA_MS} ms delta floor")
    print(f"{'rule':<14} {'builds that fail':>17} {'benchmarks flagged / trial':>28} {'per-benchmark rate':>20}")
    for name, (rate, mean) in result.items():
        print(f"{name:<14} {rate:>17.3f} {mean:>28.2f} {mean / eligible:>20.3f}")
    print(
        "A build fails when any benchmark is flagged. For a regression confined to one benchmark, the chance it is "
        "caught is the per-benchmark rate."
    )
    return 0


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
