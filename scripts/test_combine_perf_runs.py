#!/usr/bin/env python3
"""Unit tests for combine_perf_runs.py:

    python3 -m unittest scripts.test_combine_perf_runs -v
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


def _write_csv(path: str, rows: list[tuple[str, float, float]]) -> None:
    with open(path, "w") as handle:
        handle.write("Serenity performance benchmarks\n")
        handle.write("context,display,1920x1200\n")
        handle.write("name,warmups,iterations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes\n")
        for name, p50, p95 in rows:
            handle.write(f"{name},0,0,0,0,{p50},{p95},0,,\n")


class CombinePerfRunsTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmpdir.cleanup)

    def _path(self, name: str) -> str:
        return os.path.join(self._tmpdir.name, name)

    def test_keeps_each_benchmarks_fastest_p50_across_runs(self) -> None:
        _write_csv(self._path("a.csv"), [("x", 2.0, 3.0), ("y", 1.0, 1.5)])
        _write_csv(self._path("b.csv"), [("x", 1.0, 4.0), ("y", 1.2, 1.3)])
        out = self._path("out.csv")
        self.assertEqual(combine_perf_runs.main_with_args([out, self._path("a.csv"), self._path("b.csv")]), 0)
        rows = check_perf_regression.parse_csv(out)
        self.assertEqual(rows["x"].p50_ms, 1.0)
        self.assertEqual(rows["y"].p50_ms, 1.0)

    def test_p95_comes_from_the_same_run_as_the_chosen_p50(self) -> None:
        _write_csv(self._path("a.csv"), [("x", 2.0, 2.5)])
        _write_csv(self._path("b.csv"), [("x", 1.0, 4.0)])
        out = self._path("out.csv")
        combine_perf_runs.main_with_args([out, self._path("a.csv"), self._path("b.csv")])
        self.assertEqual(check_perf_regression.parse_csv(out)["x"].p95_ms, 4.0)

    def test_a_benchmark_present_in_only_some_runs_is_kept(self) -> None:
        _write_csv(self._path("a.csv"), [("x", 2.0, 3.0)])
        _write_csv(self._path("b.csv"), [("x", 2.0, 3.0), ("new", 0.5, 0.6)])
        out = self._path("out.csv")
        combine_perf_runs.main_with_args([out, self._path("a.csv"), self._path("b.csv")])
        self.assertEqual(set(check_perf_regression.parse_csv(out)), {"x", "new"})

    def test_a_missing_or_empty_run_is_skipped(self) -> None:
        _write_csv(self._path("a.csv"), [("x", 2.0, 3.0)])
        Path(self._path("empty.csv")).write_text("")
        out = self._path("out.csv")
        result = combine_perf_runs.main_with_args(
            [out, self._path("a.csv"), self._path("empty.csv"), self._path("absent.csv")]
        )
        self.assertEqual(result, 0)
        self.assertEqual(check_perf_regression.parse_csv(out)["x"].p50_ms, 2.0)

    def test_no_rows_in_any_run_fails_and_writes_nothing(self) -> None:
        Path(self._path("empty.csv")).write_text("")
        out = self._path("out.csv")
        self.assertEqual(combine_perf_runs.main_with_args([out, self._path("empty.csv")]), 1)
        self.assertFalse(os.path.exists(out))

    def test_slowest_keeps_each_benchmarks_slowest_p50_across_runs(self) -> None:
        _write_csv(self._path("a.csv"), [("x", 2.0, 3.0), ("y", 1.0, 1.5)])
        _write_csv(self._path("b.csv"), [("x", 1.0, 4.0), ("y", 1.2, 1.3)])
        out = self._path("out.csv")
        combine_perf_runs.main_with_args(["--pick", "slowest", out, self._path("a.csv"), self._path("b.csv")])
        rows = check_perf_regression.parse_csv(out)
        self.assertEqual((rows["x"].p50_ms, rows["x"].p95_ms), (2.0, 3.0))
        self.assertEqual(rows["y"].p50_ms, 1.2)

    def test_one_noisy_run_per_side_does_not_read_as_a_regression(self) -> None:
        # Identical code on both sides: base 0.20/0.44, head 0.43/0.21.
        _write_csv(self._path("base1.csv"), [("x", 0.20, 0.5)])
        _write_csv(self._path("base2.csv"), [("x", 0.44, 0.6)])
        _write_csv(self._path("head1.csv"), [("x", 0.43, 0.6)])
        _write_csv(self._path("head2.csv"), [("x", 0.21, 0.5)])
        base, head = self._path("base.csv"), self._path("head.csv")
        combine_perf_runs.main_with_args(["--pick", "slowest", base, self._path("base1.csv"), self._path("base2.csv")])
        combine_perf_runs.main_with_args([head, self._path("head1.csv"), self._path("head2.csv")])
        self.assertEqual(check_perf_regression.main_with_args([base, head]), 0)

    def test_a_regression_in_every_head_run_still_fails(self) -> None:
        _write_csv(self._path("base1.csv"), [("x", 1.0, 1.1)])
        _write_csv(self._path("base2.csv"), [("x", 1.2, 1.3)])
        _write_csv(self._path("head1.csv"), [("x", 2.6, 2.7)])
        _write_csv(self._path("head2.csv"), [("x", 2.5, 2.6)])
        base, head = self._path("base.csv"), self._path("head.csv")
        combine_perf_runs.main_with_args(["--pick", "slowest", base, self._path("base1.csv"), self._path("base2.csv")])
        combine_perf_runs.main_with_args([head, self._path("head1.csv"), self._path("head2.csv")])
        self.assertEqual(check_perf_regression.main_with_args([base, head]), 1)

    def test_combined_output_round_trips_through_the_regression_check(self) -> None:
        _write_csv(self._path("base1.csv"), [("x", 1.0, 1.1)])
        _write_csv(self._path("base2.csv"), [("x", 1.1, 1.2)])
        _write_csv(self._path("head1.csv"), [("x", 3.0, 3.1)])
        _write_csv(self._path("head2.csv"), [("x", 2.9, 3.0)])
        base, head = self._path("base.csv"), self._path("head.csv")
        combine_perf_runs.main_with_args([base, self._path("base1.csv"), self._path("base2.csv")])
        combine_perf_runs.main_with_args([head, self._path("head1.csv"), self._path("head2.csv")])
        self.assertEqual(check_perf_regression.main_with_args([base, head]), 1)


if __name__ == "__main__":
    unittest.main()
