#!/usr/bin/env python3
"""Unit tests for check_perf_regression.py -- issue #1453/#1503 follow-up (audit: threshold too loose, cache-cold
baseline silently passed). No test framework is added; this repo has no Python test dependency yet, so these use only
the stdlib `unittest` runner:

    python3 -m unittest scripts.test_check_perf_regression -v

(run from the repo root, or `python3 scripts/test_check_perf_regression.py -v` directly).
"""

from __future__ import annotations

import contextlib
import importlib.util
import io
import os
import sys
import tempfile
import unittest
from pathlib import Path

_MODULE_PATH = Path(__file__).resolve().parent / "check_perf_regression.py"
_spec = importlib.util.spec_from_file_location("check_perf_regression", _MODULE_PATH)
assert _spec is not None and _spec.loader is not None
check_perf_regression = importlib.util.module_from_spec(_spec)
sys.modules["check_perf_regression"] = check_perf_regression  # dataclass() needs the module registered to resolve types
_spec.loader.exec_module(check_perf_regression)


def _write_csv(path: str, rows: list[tuple[str, float, float]]) -> None:
    with open(path, "w") as handle:
        handle.write("name,warmups,iterations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes\n")
        for name, p50, p95 in rows:
            handle.write(f"{name},0,0,0,0,{p50},{p95},0,,\n")


class CheckPerfRegressionTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmpdir.cleanup)
        self.baseline_path = os.path.join(self._tmpdir.name, "baseline.csv")
        self.current_path = os.path.join(self._tmpdir.name, "current.csv")

    def _run(self, argv: list[str]) -> int:
        return check_perf_regression.main_with_args(
            [self.baseline_path, self.current_path, *argv]
        )

    def test_default_threshold_is_two_x_not_three_x(self) -> None:
        # Locks in the tightened bar (audit: 3x let a 2x regression through silently).
        parser = check_perf_regression.build_parser()
        args = parser.parse_args([self.baseline_path, self.current_path])
        self.assertEqual(args.threshold, 2.0)

    def test_no_regression_within_threshold_passes(self) -> None:
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        _write_csv(self.current_path, [("rope.search", 15.0, 17.0)])  # 1.5x, under 2.0x
        self.assertEqual(self._run([]), 0)

    def test_regression_past_threshold_fails(self) -> None:
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        _write_csv(self.current_path, [("rope.search", 25.0, 27.0)])  # 2.5x, over 2.0x
        self.assertEqual(self._run([]), 1)

    def test_regression_that_would_have_passed_old_threshold_now_fails(self) -> None:
        # The exact gap the audit flagged: a 2x regression used to pass the 3x gate silently.
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        _write_csv(self.current_path, [("rope.search", 20.0, 22.0)])  # exactly 2.0x
        self.assertEqual(self._run([]), 0)  # boundary: > threshold fails, == does not
        _write_csv(self.current_path, [("rope.search", 20.001, 22.0)])
        self.assertEqual(self._run([]), 1)

    def test_missing_baseline_without_allow_flag_fails_loudly(self) -> None:
        # Audit's second finding: a cache-cold run must not silently pass.
        _write_csv(self.current_path, [("rope.search", 10.0, 12.0)])
        self.assertFalse(os.path.exists(self.baseline_path))
        self.assertEqual(self._run([]), 1)

    def test_missing_baseline_with_allow_flag_establishes_baseline_and_passes(self) -> None:
        _write_csv(self.current_path, [("rope.search", 10.0, 12.0)])
        self.assertFalse(os.path.exists(self.baseline_path))
        self.assertEqual(self._run(["--allow-missing-baseline"]), 0)

    def test_missing_current_results_still_fails(self) -> None:
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        # current.csv left unwritten / empty
        open(self.current_path, "w").close()
        self.assertEqual(self._run([]), 1)

    def test_sub_floor_baseline_is_ignored_as_noise(self) -> None:
        _write_csv(self.baseline_path, [("tiny.op", 0.0001, 0.0002)])
        _write_csv(self.current_path, [("tiny.op", 0.01, 0.02)])  # 100x on paper, but under the floor
        self.assertEqual(self._run([]), 0)

    def test_new_benchmark_with_no_baseline_entry_does_not_fail(self) -> None:
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        _write_csv(self.current_path, [("rope.search", 10.0, 12.0), ("new.scenario", 5.0, 6.0)])
        self.assertEqual(self._run([]), 0)

    def test_ratio_past_threshold_but_tiny_absolute_delta_does_not_fail(self) -> None:
        # A benchmark near the noise floor (sub-microsecond baseline) can swing past the ratio threshold on jitter
        # alone even though the absolute cost barely moved -- e.g. 0.010ms -> 0.025ms is 2.5x but only +0.015ms. The
        # ratio guard alone flags this as a regression; the absolute-ms-delta floor exists to catch exactly this case.
        _write_csv(self.baseline_path, [("tiny.op", 0.010, 0.012)])
        _write_csv(self.current_path, [("tiny.op", 0.025, 0.030)])  # 2.5x ratio, +0.015ms absolute
        self.assertEqual(self._run(["--min-regression-delta-ms", "0.02"]), 0)

    def test_ratio_past_threshold_and_absolute_delta_past_floor_fails(self) -> None:
        _write_csv(self.baseline_path, [("real.op", 10.0, 12.0)])
        _write_csv(self.current_path, [("real.op", 25.0, 27.0)])  # 2.5x ratio, +15ms absolute
        self.assertEqual(self._run(["--min-regression-delta-ms", "0.02"]), 1)

    def test_absolute_delta_floor_defaults_to_a_value_that_does_not_relax_existing_behaviour(self) -> None:
        # No explicit --min-regression-delta-ms passed: the existing ratio-only regression tests above must still
        # fail exactly as before, so the default floor must not be so large it swallows a genuine 15ms regression.
        _write_csv(self.baseline_path, [("rope.search", 10.0, 12.0)])
        _write_csv(self.current_path, [("rope.search", 25.0, 27.0)])  # 2.5x ratio, +15ms absolute
        self.assertEqual(self._run([]), 1)

    def test_absolute_delta_floor_is_a_separate_guard_from_min_baseline_ms(self) -> None:
        # --min-baseline-ms ignores benchmarks with a tiny *baseline*; --min-regression-delta-ms ignores regressions
        # with a tiny *delta* even when the baseline itself is well above the min-baseline-ms floor. A benchmark whose
        # baseline is comfortably above min-baseline-ms can still be pure noise in absolute terms.
        _write_csv(self.baseline_path, [("borderline.op", 1.0, 1.2)])
        _write_csv(self.current_path, [("borderline.op", 2.5, 3.0)])  # 2.5x ratio, +1.5ms absolute
        self.assertEqual(self._run(["--min-baseline-ms", "0.001", "--min-regression-delta-ms", "2.0"]), 0)
        self.assertEqual(self._run(["--min-baseline-ms", "0.001", "--min-regression-delta-ms", "1.0"]), 1)


def _write_memory_csv(path: str, rows: list[tuple[str, float, int | None, int | None]], legacy_header: bool = False) -> None:
    """Rows are (name, p50_ms, allocation_p50_bytes, retained_bytes). `legacy_header` writes the pre-memory layout."""
    with open(path, "w") as handle:
        if legacy_header:
            handle.write("name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms\n")
        else:
            handle.write(
                "name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,"
                "allocation_p50_bytes,allocation_p95_bytes,retained_bytes\n"
            )
        for name, p50, allocation, retained in rows:
            if legacy_header:
                handle.write(f"{name},0,0,0,0,{p50},{p50},0\n")
            else:
                alloc = "" if allocation is None else allocation
                held = "" if retained is None else retained
                handle.write(f"{name},0,0,0,0,{p50},{p50},0,{alloc},{alloc},{held}\n")


MIB = 1024 * 1024
KIB = 1024


class MemoryMetricTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmpdir.cleanup)
        self.baseline_path = os.path.join(self._tmpdir.name, "baseline.csv")
        self.current_path = os.path.join(self._tmpdir.name, "current.csv")

    def _run(self, argv: list[str] | None = None) -> int:
        return check_perf_regression.main_with_args([self.baseline_path, self.current_path, *(argv or [])])

    def _retained(self, baseline: int, current: int) -> int:
        _write_memory_csv(self.baseline_path, [("footprint.doc", 0.0, None, baseline)])
        _write_memory_csv(self.current_path, [("footprint.doc", 0.0, None, current)])
        return self._run()

    def _allocation(self, baseline: int, current: int, argv: list[str] | None = None) -> int:
        _write_memory_csv(self.baseline_path, [("laptop.frame", 1.0, baseline, None)])
        _write_memory_csv(self.current_path, [("laptop.frame", 1.0, current, None)])
        return self._run(argv)

    def test_parse_reads_allocation_and_retained_columns(self) -> None:
        _write_memory_csv(self.current_path, [("a", 1.0, 4096, 8192)])
        row = check_perf_regression.parse_csv(self.current_path)["a"]
        self.assertEqual((row.allocation_p50_bytes, row.retained_bytes), (4096, 8192))

    def test_parse_leaves_memory_columns_unset_on_a_legacy_csv(self) -> None:
        _write_memory_csv(self.current_path, [("a", 1.0, None, None)], legacy_header=True)
        row = check_perf_regression.parse_csv(self.current_path)["a"]
        self.assertEqual((row.allocation_p50_bytes, row.retained_bytes), (None, None))

    def test_retained_growth_past_ratio_and_one_mib_fails(self) -> None:
        self.assertEqual(self._retained(10 * MIB, 14 * MIB), 1)

    def test_retained_growth_exactly_at_the_ratio_passes(self) -> None:
        self.assertEqual(self._retained(8 * MIB, 10 * MIB), 0)

    def test_retained_growth_past_ratio_but_under_one_mib_passes(self) -> None:
        self.assertEqual(self._retained(100 * KIB, 500 * KIB), 0)

    def test_retained_growth_past_one_mib_but_under_ratio_passes(self) -> None:
        self.assertEqual(self._retained(100 * MIB, 110 * MIB), 0)

    def test_retained_shrinking_passes(self) -> None:
        self.assertEqual(self._retained(10 * MIB, 2 * MIB), 0)

    def test_retained_thresholds_are_configurable(self) -> None:
        _write_memory_csv(self.baseline_path, [("footprint.doc", 0.0, None, 10 * MIB)])
        _write_memory_csv(self.current_path, [("footprint.doc", 0.0, None, 12 * MIB)])
        self.assertEqual(self._run(), 0)
        self.assertEqual(self._run(["--retained-threshold", "1.1", "--min-retained-delta-bytes", str(MIB)]), 1)

    def test_allocation_regression_only_warns(self) -> None:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            self.assertEqual(self._allocation(100 * KIB, 400 * KIB), 0)
        self.assertIn("::warning::", out.getvalue())
        self.assertIn("laptop.frame", out.getvalue())

    def test_allocation_regression_fails_when_gated(self) -> None:
        self.assertEqual(self._allocation(100 * KIB, 400 * KIB, ["--gate-allocation"]), 1)

    def test_allocation_under_ratio_does_not_warn(self) -> None:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            self.assertEqual(self._allocation(1000 * KIB, 1400 * KIB, ["--gate-allocation"]), 0)
        self.assertNotIn("::warning::", out.getvalue())

    def test_allocation_under_64_kib_delta_does_not_warn(self) -> None:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            self.assertEqual(self._allocation(10 * KIB, 60 * KIB, ["--gate-allocation"]), 0)
        self.assertNotIn("::warning::", out.getvalue())

    def test_baseline_without_memory_columns_is_skipped_with_a_note(self) -> None:
        _write_memory_csv(self.baseline_path, [("footprint.doc", 0.0, None, None)], legacy_header=True)
        _write_memory_csv(self.current_path, [("footprint.doc", 0.0, 50 * MIB, 50 * MIB)])
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            self.assertEqual(self._run(["--gate-allocation"]), 0)
        self.assertIn("::notice::", out.getvalue())

    def test_current_without_memory_columns_does_not_crash(self) -> None:
        _write_memory_csv(self.baseline_path, [("footprint.doc", 0.0, 1 * MIB, 10 * MIB)])
        _write_memory_csv(self.current_path, [("footprint.doc", 0.0, None, None)], legacy_header=True)
        self.assertEqual(self._run(["--gate-allocation"]), 0)

    def test_latency_gate_still_applies_to_rows_with_memory_columns(self) -> None:
        _write_memory_csv(self.baseline_path, [("laptop.frame", 10.0, 1 * MIB, None)])
        _write_memory_csv(self.current_path, [("laptop.frame", 25.0, 1 * MIB, None)])
        self.assertEqual(self._run(), 1)


if __name__ == "__main__":
    sys.path.insert(0, str(_MODULE_PATH.parent.parent))
    unittest.main()
