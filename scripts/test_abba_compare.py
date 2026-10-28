#!/usr/bin/env python3
"""Unit tests for abba_compare.py:

    python3 -m unittest scripts.test_abba_compare -v
"""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
sys.path.insert(0, str(_SCRIPTS))


def _load(name: str):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / f"{name}.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


abba = _load("abba_compare")


def _run(name: str, p50: float, alloc: int | None = None, retained: int | None = None) -> dict:
    return {name: abba.Row(name, p50, p50, alloc, alloc, retained)}


def _runs(name: str, p50s: list[float], alloc: int | None = None, retained: int | None = None) -> list[dict]:
    return [_run(name, p50, alloc, retained) for p50 in p50s]


class TimeThresholdTest(unittest.TestCase):
    def test_non_overlapping_slower_range_blocks(self):
        found = abba.compare(_runs("x", [1.00, 1.01, 1.02, 1.01]), _runs("x", [1.10, 1.12, 1.11, 1.13]))
        self.assertEqual([f.metric for f in found], ["p50_ms"])

    def test_overlapping_ranges_do_not_block_however_far_the_medians_are(self):
        found = abba.compare(_runs("x", [1.00, 1.30, 1.01, 1.02]), _runs("x", [1.20, 1.40, 1.25, 1.22]))
        self.assertEqual(found, [])

    def test_under_five_percent_does_not_block(self):
        found = abba.compare(_runs("x", [10.00, 10.01, 10.02, 10.01]), _runs("x", [10.30, 10.31, 10.32, 10.33]))
        self.assertEqual(found, [])

    def test_under_thirty_microseconds_does_not_block(self):
        found = abba.compare(_runs("x", [0.100, 0.101, 0.102, 0.101]), _runs("x", [0.120, 0.121, 0.122, 0.123]))
        self.assertEqual(found, [])

    def test_a_faster_candidate_does_not_block(self):
        found = abba.compare(_runs("x", [2.0, 2.1, 2.0, 2.1]), _runs("x", [1.0, 1.1, 1.0, 1.1]))
        self.assertEqual(found, [])


class AllocationThresholdTest(unittest.TestCase):
    def test_five_percent_and_eight_kib_blocks(self):
        found = abba.compare(_runs("x", [1.0] * 4, alloc=100_000), _runs("x", [1.0] * 4, alloc=109_000))
        self.assertEqual([f.metric for f in found], ["allocation_p50_bytes"])

    def test_under_eight_kib_does_not_block(self):
        found = abba.compare(_runs("x", [1.0] * 4, alloc=10_000), _runs("x", [1.0] * 4, alloc=17_000))
        self.assertEqual(found, [])

    def test_under_five_percent_does_not_block(self):
        found = abba.compare(_runs("x", [1.0] * 4, alloc=1_000_000), _runs("x", [1.0] * 4, alloc=1_040_000))
        self.assertEqual(found, [])

    def test_rows_without_allocation_are_skipped(self):
        self.assertEqual(abba.compare(_runs("x", [1.0] * 4), _runs("x", [1.0] * 4)), [])


class RetainedThresholdTest(unittest.TestCase):
    def test_more_than_ten_percent_blocks(self):
        found = abba.compare(
            _runs("footprint.x", [0.0] * 4, retained=10_000_000), _runs("footprint.x", [0.0] * 4, retained=11_100_000)
        )
        self.assertEqual([f.metric for f in found], ["retained_bytes"])

    def test_exactly_ten_percent_does_not_block(self):
        found = abba.compare(
            _runs("footprint.x", [0.0] * 4, retained=10_000_000), _runs("footprint.x", [0.0] * 4, retained=11_000_000)
        )
        self.assertEqual(found, [])


class TimeDeltaOptionTest(unittest.TestCase):
    """Start-up rows are seconds long, so a 0.03 ms floor means nothing; the wrapper passes a larger one."""

    a = _runs("x", [1000.0, 1001.0, 1002.0, 1001.0])
    six_percent_slower = _runs("x", [1060.0, 1061.0, 1062.0, 1063.0])

    def test_default_floor_is_the_protocols_thirty_microseconds(self):
        self.assertEqual(len(abba.compare(self.a, self.six_percent_slower)), 1)

    def test_a_larger_floor_ignores_a_smaller_slowdown(self):
        self.assertEqual(abba.compare(self.a, self.six_percent_slower, time_delta_ms=100.0), [])

    def test_a_larger_floor_still_blocks_a_bigger_slowdown(self):
        slower = _runs("x", [1150.0, 1151.0, 1152.0, 1153.0])
        self.assertEqual(len(abba.compare(self.a, slower, time_delta_ms=100.0)), 1)

    def test_command_line_option_sets_the_floor(self):
        with tempfile.TemporaryDirectory() as directory:
            paths = {}
            for side, base in (("a", 1000.0), ("b", 1060.0)):
                paths[side] = []
                for i in range(4):
                    path = Path(directory) / f"{side}{i}.csv"
                    path.write_text(
                        "name,warmups,iterations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,"
                        "allocation_p95_bytes,retained_bytes\n"
                        f"x,0,0,0,0,{base + i},{base + i},0,,,\n"
                    )
                    paths[side].append(str(path))
            args = ["--a", *paths["a"], "--b", *paths["b"]]
            self.assertEqual(abba.main_with_args(args), 1)
            self.assertEqual(abba.main_with_args([*args, "--time-delta-ms", "100"]), 0)


class CommandLineTest(unittest.TestCase):
    def _csv(self, directory: str, name: str, p50: float) -> str:
        path = Path(directory) / name
        path.write_text(
            "name,warmups,iterations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes,retained_bytes\n"
            f"x,0,0,0,0,{p50},{p50},0,,,\n"
        )
        return str(path)

    def test_exit_codes(self):
        with tempfile.TemporaryDirectory() as directory:
            a = [self._csv(directory, f"a{i}.csv", 1.0 + i * 0.01) for i in range(4)]
            slower = [self._csv(directory, f"b{i}.csv", 1.5 + i * 0.01) for i in range(4)]
            same = [self._csv(directory, f"c{i}.csv", 1.0 + i * 0.01) for i in range(4)]
            self.assertEqual(abba.main_with_args(["--a", *a, "--b", *slower]), 1)
            self.assertEqual(abba.main_with_args(["--a", *a, "--b", *same]), 0)
            self.assertEqual(abba.main_with_args(["--a", *a, "--b", str(Path(directory) / "missing.csv")]), 2)


if __name__ == "__main__":
    unittest.main()
