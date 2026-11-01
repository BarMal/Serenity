#!/usr/bin/env python3
"""Unit tests for study_perf_gate.py:

    python3 -m unittest scripts.test_study_perf_gate -v
"""

from __future__ import annotations

import importlib.util
import sys
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
_load("combine_perf_runs")
_load("compare_perf_runs")
study_perf_gate = _load("study_perf_gate")


def _run(**p50s: float) -> dict:
    return {name: check_perf_regression.Row(name, p50, p50) for name, p50 in p50s.items()}


class StudyTests(unittest.TestCase):
    def test_the_fastest_run_rule_is_scored_beside_the_others(self) -> None:
        plain = [_run(op=10.0)] * 8
        result = study_perf_gate.study(plain, None, pairs=3, samples=20, thresholds=[1.5], inject=2.0)
        self.assertIn("fastest-1.5", result)
        self.assertEqual(result["fastest-1.5"][0], 1.0)

    def test_a_steady_benchmark_is_never_flagged_without_an_injected_slowdown(self) -> None:
        plain = [_run(op=10.0)] * 8
        result = study_perf_gate.study(plain, None, pairs=3, samples=20, thresholds=[1.5], inject=1.0)
        self.assertEqual(result["fastest-1.5"], (0.0, 0.0))
        self.assertEqual(result["paired-1.5"], (0.0, 0.0))

    def test_confirming_forgives_a_flag_that_fresh_pairs_do_not_repeat(self) -> None:
        # four runs a side-of-three would need are not available, so confirm needs 4 * pairs runs
        plain = [_run(op=10.0)] * 8
        with self.assertRaises(ValueError):
            study_perf_gate.study(plain, None, pairs=3, samples=5, thresholds=[1.5], inject=1.0, confirm=True)

    def test_confirming_still_fails_a_slowdown_that_is_really_there(self) -> None:
        plain = [_run(op=10.0)] * 12
        result = study_perf_gate.study(plain, None, pairs=3, samples=20, thresholds=[1.5], inject=2.0, confirm=True)
        self.assertEqual(result["fastest-1.5"][0], 1.0)

    def test_confirming_forgives_a_row_that_is_only_slow_on_some_runs(self) -> None:
        # every second run is 3x slower: a trial flags often, but the fresh pairs rarely repeat it
        plain = [_run(op=10.0 if i % 2 == 0 else 30.0) for i in range(12)]
        first = study_perf_gate.study(plain, None, pairs=3, samples=300, thresholds=[1.5], inject=1.0)
        confirmed = study_perf_gate.study(plain, None, pairs=3, samples=300, thresholds=[1.5], inject=1.0, confirm=True)
        self.assertLessEqual(confirmed["fastest-1.5"][0], first["fastest-1.5"][0])


if __name__ == "__main__":
    unittest.main()
