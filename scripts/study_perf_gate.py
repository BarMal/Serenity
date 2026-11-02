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
    fastest-R the gate now: each side's fastest p50 over `--pairs` runs, ratio > R (R in --thresholds)

With `--confirm`, a trial's flags are re-judged on fresh pairs (the next `2 * pairs` plain runs not used for the first
pass, so it needs `4 * pairs` of them) and the build fails only for what is flagged again, as in CI.
"""

from __future__ import annotations

import argparse
import random
import statistics
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
    """The gate before issue #2443: the median of the pairs' head/base ratios."""

    def rule(base_runs: list[dict[str, Row]], head_runs: list[dict[str, Row]]) -> set[str]:
        flagged = set()
        for name in set().union(*[set(run) for run in base_runs + head_runs]):
            pairs = [(b[name].p50_ms, h[name].p50_ms) for b, h in zip(base_runs, head_runs) if name in b and name in h]
            if len(pairs) < 2 or statistics.median(b for b, _ in pairs) < MIN_BASELINE_MS:
                continue
            ratio = statistics.median(h / b for b, h in pairs if b > 0)
            delta = statistics.median(h for _, h in pairs) - statistics.median(b for b, _ in pairs)
            if ratio > threshold and delta > DELTA_MS:
                flagged.add(name)
        return flagged

    return rule


def fastest_rule(threshold: float):
    def rule(base_runs: list[dict[str, Row]], head_runs: list[dict[str, Row]]) -> set[str]:
        return {v.name for v in judge(base_runs, head_runs, threshold=threshold) if v.regressed}

    return rule


def slowed(run: dict[str, Row], factor: float) -> dict[str, Row]:
    return {name: replace(row, p50_ms=row.p50_ms * factor) for name, row in run.items()}


def trial(
    rng: random.Random, plain: list[dict[str, Row]], heads: list[dict[str, Row]] | None, pairs: int, inject: float
):
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
    confirm: bool = False,
) -> dict[str, tuple[float, float]]:
    """Per rule: (share of trials that flag anything, mean benchmarks flagged per trial)."""
    if confirm and (slowed_runs is not None or len(plain) < 4 * pairs):
        raise ValueError(f"--confirm needs {4 * pairs} plain runs and no --slowed ones.")
    rules = {
        "old": old_rule,
        **{f"paired-{t}": paired_rule(t) for t in thresholds},
        **{f"fastest-{t}": fastest_rule(t) for t in thresholds},
    }
    flags: dict[str, list[int]] = {name: [] for name in rules}
    rng = random.Random(seed)
    for _ in range(samples):
        if confirm:
            drawn = rng.sample(plain, 4 * pairs)
            first = (drawn[:pairs], drawn[pairs : 2 * pairs])
            again = (drawn[2 * pairs : 3 * pairs], drawn[3 * pairs :])
            if inject != 1.0:
                first = (first[0], [slowed(run, inject) for run in first[1]])
                again = (again[0], [slowed(run, inject) for run in again[1]])
        else:
            first, again = trial(rng, plain, slowed_runs, pairs, inject), None
        for name, rule in rules.items():
            flagged = rule(*first)
            if again is not None and name != "old":
                flagged = flagged & rule(*again)
            flags[name].append(len(flagged))
    return {name: (sum(1 for n in counts if n) / len(counts), sum(counts) / len(counts)) for name, counts in flags.items()}


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--plain", nargs="+", required=True)
    parser.add_argument("--slowed", nargs="*", default=None)
    parser.add_argument("--pairs", type=int, default=3)
    parser.add_argument("--samples", type=int, default=2000)
    parser.add_argument("--thresholds", type=float, nargs="+", default=[1.3, 1.5, 1.75, 2.0])
    parser.add_argument("--inject", type=float, default=1.0)
    parser.add_argument("--confirm", action="store_true", help="re-judge each trial's flags on fresh pairs, as CI does")
    args = parser.parse_args(argv)
    plain = [run for run in map(parse_csv, args.plain) if run]
    slowed_runs = [run for run in map(parse_csv, args.slowed) if run] if args.slowed else None
    need = (4 if args.confirm else 2) * args.pairs if slowed_runs is None else max(args.pairs, 2)
    if len(plain) < need or (slowed_runs is not None and len(slowed_runs) < args.pairs):
        print(f"::error::Need at least {need} plain runs (and {args.pairs} slowed ones with --slowed).")
        return 1
    result = study(plain, slowed_runs, args.pairs, args.samples, args.thresholds, args.inject, confirm=args.confirm)
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
