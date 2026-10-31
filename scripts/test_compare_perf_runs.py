#!/usr/bin/env python3
"""Unit tests for compare_perf_runs.py:

    python3 -m unittest scripts.test_compare_perf_runs -v

The injected-slowdown cases are the red tests of issue #1731: the head is the base with every p50 doubled, which is what
`-Dserenity.bench.slowdown=2` does to a real run, and the earlier rule (the head's fastest p50 against the base's slowest,
`combine_perf_runs.py` then `check_perf_regression.py`) is shown passing it.
"""

from __future__ import annotations

import importlib.util
import os
import sys
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name: str):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / f"{name}.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


check_perf_regression = _load("check_perf_regression")
combine_perf_runs = _load("combine_perf_runs")
compare_perf_runs = _load("compare_perf_runs")


class PairedGateTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmpdir.cleanup)

    def _write(self, name: str, p50: float, retained: int | None = None) -> str:
        path = os.path.join(self._tmpdir.name, name)
        with open(path, "w") as handle:
            handle.write("Serenity performance benchmarks\n")
            handle.write("name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes,retained_bytes\n")
            handle.write(f"op,1,1,1,0,{p50},{p50 * 1.2},0,,,{'' if retained is None else retained}\n")
        return path

    def _runs(self, prefix: str, p50s: list[float]) -> list[str]:
        return [self._write(f"{prefix}-{i}.csv", p50) for i, p50 in enumerate(p50s, 1)]

    def _new_gate(self, base: list[float], head: list[float], *extra: str) -> int:
        return compare_perf_runs.main_with_args(
            ["--base", *self._runs("base", base), "--head", *self._runs("head", head), *extra]
        )

    def _old_gate(self, base: list[float], head: list[float]) -> int:
        """The rule this replaces: head's fastest against base's slowest, 2.0x, over the first two runs of each side."""
        base_csv, head_csv = os.path.join(self._tmpdir.name, "base.csv"), os.path.join(self._tmpdir.name, "head.csv")
        combine_perf_runs.main_with_args(["--pick", "slowest", base_csv, *self._runs("ob", base[:2])])
        combine_perf_runs.main_with_args([head_csv, *self._runs("oh", head[:2])])
        return check_perf_regression.main_with_args([base_csv, head_csv, "--threshold", "2.0"])

    def test_injected_2x_slowdown_inside_a_swinging_base_is_caught(self) -> None:
        base = [5.0, 30.0, 6.0]  # one commit's p50 swinging 5 to 30 ms between runs (#1731)
        head = [p * 2 for p in base]
        self.assertEqual(self._old_gate(base, head), 0, "the earlier rule lets a real 2x slowdown through")
        self.assertEqual(self._new_gate(base, head), 1)

    def test_injected_2x_slowdown_on_a_steady_benchmark_is_caught(self) -> None:
        self.assertEqual(self._new_gate([10.0, 10.2, 9.9], [20.0, 20.5, 19.8]), 1)

    def test_a_slow_spell_on_one_base_run_is_not_a_regression(self) -> None:
        self.assertEqual(self._new_gate([5.0, 5.1, 30.0], [5.0, 5.1, 5.0]), 0)

    def test_a_slow_spell_on_one_head_run_is_not_a_regression(self) -> None:
        self.assertEqual(self._new_gate([5.0, 5.1, 5.0], [5.0, 30.0, 5.1]), 0)

    def test_unchanged_code_passes(self) -> None:
        self.assertEqual(self._new_gate([5.0, 5.4, 4.8], [5.2, 4.9, 5.3]), 0)

    def test_a_modest_slowdown_below_the_threshold_passes(self) -> None:
        self.assertEqual(self._new_gate([10.0, 10.0, 10.0], [13.0, 13.0, 13.0]), 0)

    def test_a_ratio_past_the_threshold_with_a_tiny_delta_passes(self) -> None:
        self.assertEqual(self._new_gate([0.010, 0.010, 0.010], [0.025, 0.025, 0.025]), 0)

    def test_a_single_pair_is_not_enough_to_compare(self) -> None:
        self.assertEqual(self._new_gate([10.0], [30.0]), 1)

    def test_a_crashed_run_keeps_the_remaining_pairs_aligned(self) -> None:
        base = self._runs("base", [10.0, 10.0, 10.0])
        head = self._runs("head", [10.0, 10.0, 10.0])
        open(head[1], "w").close()
        self.assertEqual(compare_perf_runs.main_with_args(["--base", *base, "--head", *head]), 0)

    def test_mismatched_run_counts_fail_loudly(self) -> None:
        self.assertEqual(
            compare_perf_runs.main_with_args(["--base", *self._runs("b", [1.0, 1.0]), "--head", *self._runs("h", [1.0])]), 1
        )

    def test_retained_heap_growth_still_fails(self) -> None:
        mib = 1024 * 1024
        base = [self._write(f"mb{i}.csv", 1.0, retained=10 * mib) for i in range(3)]
        head = [self._write(f"mh{i}.csv", 1.0, retained=20 * mib) for i in range(3)]
        self.assertEqual(compare_perf_runs.main_with_args(["--base", *base, "--head", *head]), 1)

    # The verdict compares each side's fastest p50 (issue #2443): a benchmark's slow runs are a JIT or runner state that
    # one side can land in and the other not, and the fastest run is the one least affected by it.

    def test_the_default_threshold_is_one_and_a_half(self) -> None:
        self.assertEqual(compare_perf_runs.DEFAULT_THRESHOLD, 1.5)

    def test_a_slowdown_past_one_and_a_half_on_every_run_is_caught(self) -> None:
        self.assertEqual(self._new_gate([10.0, 10.0, 10.0], [16.0, 16.0, 16.0]), 1)

    def test_a_slowdown_that_a_slow_head_run_would_have_hidden_from_a_median_is_caught(self) -> None:
        self.assertEqual(self._new_gate([5.0, 5.0, 5.0], [8.0, 8.0, 30.0]), 1)

    def test_the_fastest_head_run_decides_so_one_lucky_run_clears_the_head(self) -> None:
        self.assertEqual(self._new_gate([10.0, 10.0, 10.0], [20.0, 20.0, 9.0]), 0)

    def test_the_delta_floor_is_taken_between_the_fastest_runs(self) -> None:
        self.assertEqual(self._new_gate([0.10, 0.10, 0.10], [0.16, 0.16, 0.16]), 1)
        self.assertEqual(self._new_gate([0.10, 0.10, 0.10], [0.14, 0.14, 0.14]), 0)

    def test_the_verdict_reports_the_fastest_runs_and_their_ratio(self) -> None:
        verdicts = compare_perf_runs.judge(
            [{"op": check_perf_regression.Row("op", p, p)} for p in (6.0, 5.0, 30.0)],
            [{"op": check_perf_regression.Row("op", p, p)} for p in (12.0, 11.0, 40.0)],
        )
        self.assertEqual((verdicts[0].base_ms, verdicts[0].head_ms), (5.0, 11.0))
        self.assertAlmostEqual(verdicts[0].ratio, 2.2)


class ConfirmBeforeFailingTests(unittest.TestCase):
    """The CI path: a first pass of three pairs that only flags, then fresh pairs of just the flagged benchmarks."""

    def setUp(self) -> None:
        self._tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmpdir.cleanup)
        self.flagged = os.path.join(self._tmpdir.name, "flagged.txt")

    def _run(self, prefix: str, side: str, p50s: dict[str, float]) -> str:
        path = os.path.join(self._tmpdir.name, f"{prefix}-{side}.csv")
        with open(path, "w") as handle:
            handle.write("Serenity performance benchmarks\n")
            handle.write("name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes,retained_bytes\n")
            for name, p50 in p50s.items():
                handle.write(f"{name},1,1,1,0,{p50},{p50 * 1.2},0,,,\n")
        return path

    def _runs(self, prefix: str, side: str, series: list[dict[str, float]]) -> list[str]:
        return [self._run(f"{prefix}{i}", side, p50s) for i, p50s in enumerate(series, 1)]

    def _first_pass(self, base: list[dict[str, float]], head: list[dict[str, float]]) -> int:
        return compare_perf_runs.main_with_args(
            [
                "--base", *self._runs("first", "base", base),
                "--head", *self._runs("first", "head", head),
                "--write-flagged", self.flagged,
            ]
        )

    def _confirm(self, base: list[dict[str, float]], head: list[dict[str, float]]) -> int:
        return compare_perf_runs.main_with_args(
            [
                "--base", *self._runs("again", "base", base),
                "--head", *self._runs("again", "head", head),
                "--only", self.flagged,
            ]
        )

    @staticmethod
    def _series(values: list[float], name: str = "op", steady: float = 4.0) -> list[dict[str, float]]:
        return [{name: value, "steady": steady} for value in values]

    def _flagged_names(self) -> list[str]:
        with open(self.flagged) as handle:
            return handle.read().split()

    def test_an_injected_2x_slowdown_is_flagged_and_then_confirmed(self) -> None:
        base = self._series([5.0, 30.0, 6.0])
        head = self._series([10.0, 60.0, 12.0])
        self.assertEqual(self._first_pass(base, head), 2)
        self.assertEqual(self._flagged_names(), ["op"])
        # fresh pairs on the same runner: the slowdown is still there
        self.assertEqual(self._confirm(self._series([5.5, 5.0, 5.2]), self._series([11.0, 10.2, 10.5])), 1)

    def test_a_one_off_spike_is_flagged_and_then_forgiven(self) -> None:
        base = self._series([5.0, 5.0, 5.0])
        head = self._series([10.0, 10.0, 10.0])  # a slow spell over every head run: past 1.5x on the fastest
        self.assertEqual(self._first_pass(base, head), 2)
        self.assertEqual(self._confirm(self._series([5.0, 5.1, 5.0]), self._series([5.1, 5.0, 5.2])), 0)

    def test_nothing_flagged_passes_without_a_confirmation(self) -> None:
        self.assertEqual(self._first_pass(self._series([5.0, 5.1, 5.0]), self._series([5.1, 5.0, 5.2])), 0)
        self.assertFalse(os.path.exists(self.flagged))

    def test_only_the_flagged_benchmarks_are_judged_on_the_confirmation_runs(self) -> None:
        self.assertEqual(self._first_pass(self._series([5.0] * 3), self._series([10.0] * 3)), 2)
        # `steady` is slower on every confirmation pair, but it was not flagged, so it is not this pass's business
        again_base = [{"op": 5.0, "steady": 4.0}] * 3
        again_head = [{"op": 5.0, "steady": 40.0}] * 3
        self.assertEqual(self._confirm(again_base, again_head), 0)

    def test_the_confirmation_does_not_count_the_first_pass_again(self) -> None:
        # three bad pairs first, three good pairs after: judged on the new pairs alone, so it is forgiven
        self.assertEqual(self._first_pass(self._series([5.0] * 3), self._series([10.0] * 3)), 2)
        self.assertEqual(self._confirm(self._series([5.0] * 3), self._series([5.0] * 3)), 0)

    def test_a_flagged_benchmark_missing_from_the_confirmation_runs_fails(self) -> None:
        self.assertEqual(self._first_pass(self._series([5.0] * 3), self._series([10.0] * 3)), 2)
        self.assertEqual(self._confirm([{"steady": 4.0}] * 3, [{"steady": 4.0}] * 3), 1)

    def test_a_retained_heap_regression_fails_the_first_pass_outright(self) -> None:
        mib = 1024 * 1024

        def with_heap(prefix: str, side: str, retained: int) -> list[str]:
            paths = []
            for i in range(3):
                path = os.path.join(self._tmpdir.name, f"{prefix}{i}-{side}.csv")
                with open(path, "w") as handle:
                    handle.write("name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes,retained_bytes\n")
                    handle.write(f"footprint.doc,0,0,0,0,0.5,0.5,0,,,{retained}\n")
                paths.append(path)
            return paths

        code = compare_perf_runs.main_with_args(
            ["--base", *with_heap("h", "base", 10 * mib), "--head", *with_heap("h", "head", 20 * mib), "--write-flagged", self.flagged]
        )
        self.assertEqual(code, 1)


if __name__ == "__main__":
    unittest.main()
