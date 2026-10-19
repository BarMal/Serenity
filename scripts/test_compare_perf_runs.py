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


if __name__ == "__main__":
    unittest.main()
