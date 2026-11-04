#!/usr/bin/env python3
"""Checks on the `perf` job's matrix in .github/workflows/ci.yml:

    python3 -m unittest scripts.test_perf_matrix -v

The 1x suite must run as complete, non-overlapping shards (`BenchmarkShard` selects a contiguous slice by
`index/count`). The 2x HiDPI run is one unsharded entry over the HiDPI-sensitive row-name prefixes only (#2557: 128 of
131 rows read the same at 2x as at 1x, so the 1x shards already judge them). The entries must not collide on check name
or artifact name, or one entry's results would overwrite another's. Every entry carries an explicit timeout: GitHub's
default of six hours hides a stuck job.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_CI = Path(__file__).resolve().parent.parent / ".github" / "workflows" / "ci.yml"

_SCALE_SENSITIVE_ROWS = (
    "laptop.present.swing_fresh_frame_synced_1500x1000",
    "laptop.present.swing_paint_caret_1500x1000",
    "layout.cold_font.shaping_barriers",
)


def _perf_job(text: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line == "  perf:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


def _matrix_entries(job: str) -> list[dict[str, str]]:
    entries: list[dict[str, str]] = []
    for line in job.splitlines():
        entry_start = re.match(r"^\s+- (\w+): (.*)$", line)
        field = re.match(r"^\s{12}(\w+): (.*)$", line)
        if entry_start and line.startswith("          - "):
            entries.append({entry_start.group(1): entry_start.group(2).strip()})
        elif field and entries:
            entries[-1][field.group(1)] = field.group(2).strip()
    return entries


class PerfMatrixTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.job = _perf_job(_CI.read_text())
        cls.entries = _matrix_entries(cls.job)

    def _at(self, scale: str) -> list[dict[str, str]]:
        return [entry for entry in self.entries if entry["scale"] == scale]

    def _two_x_prefixes(self) -> tuple[str, ...]:
        return tuple(self._at("2")[0]["names"].strip("'\"").split())

    def test_the_matrix_is_read(self) -> None:
        self.assertEqual(3, len(self.entries))
        for entry in self._at("1"):
            self.assertEqual({"label", "scale", "screen", "dpi", "shard", "results", "timeout"}, set(entry))
        for entry in self._at("2"):
            self.assertEqual({"label", "scale", "screen", "dpi", "names", "results", "timeout"}, set(entry))

    def test_the_one_x_suite_runs_as_two_complete_shards(self) -> None:
        shards = [tuple(int(part) for part in entry["shard"].split("/")) for entry in self._at("1")]
        self.assertEqual([(1, 2), (2, 2)], sorted(shards))

    def test_the_two_x_run_is_one_unsharded_entry_over_name_prefixes(self) -> None:
        self.assertEqual(1, len(self._at("2")))
        entry = self._at("2")[0]
        self.assertNotIn("shard", entry)
        self.assertEqual("3840x2400x24", entry["screen"])
        self.assertEqual("192", entry["dpi"])
        self.assertEqual(
            ("laptop.present.", "render.hidpi_frame", "render.full_frame", "layout.cold_font."), self._two_x_prefixes()
        )

    def test_every_scale_sensitive_row_is_covered_by_the_two_x_prefixes(self) -> None:
        # The only rows whose median p50 at 2x was outside 0.8 to 1.25 times the 1x one over 1,592 artifacts (#2557),
        # plus the two rows that exist to exercise the HiDPI render path.
        for row in _SCALE_SENSITIVE_ROWS + ("render.hidpi_frame.java2d", "render.full_frame.java2d"):
            with self.subTest(row=row):
                self.assertTrue(row.startswith(self._two_x_prefixes()))

    def test_no_two_x_prefix_selects_a_footprint_row_so_those_stay_with_the_one_x_first_shard(self) -> None:
        self.assertFalse("footprint.empty_editor".startswith(self._two_x_prefixes()))
        first_shard = next(entry for entry in self._at("1") if entry["shard"] == "1/2")
        self.assertEqual("perf-benchmark-results-shard-1", first_shard["results"])

    def test_labels_and_artifacts_are_unique(self) -> None:
        labels = [entry["label"] for entry in self.entries]
        results = [entry["results"] for entry in self.entries]
        self.assertEqual(len(labels), len(set(labels)), labels)
        self.assertEqual(len(results), len(set(results)), results)

    def test_a_sharded_entry_names_its_shard_in_its_label(self) -> None:
        for entry in self._at("1"):
            index, count = entry["shard"].split("/")
            self.assertIn(f"shard {index}/{count}", entry["label"])

    def test_the_two_x_entry_says_so_in_its_label(self) -> None:
        self.assertEqual("Performance benchmarks (2x HiDPI)", self._at("2")[0]["label"])

    def test_the_job_timeout_is_set_per_entry_from_the_matrix(self) -> None:
        self.assertRegex(self.job, r"(?m)^    timeout-minutes: \$\{\{ matrix\.timeout \}\}$")

    def test_the_one_x_shards_keep_the_limit_above_the_measured_longest_run(self) -> None:
        # The longest 1x run seen was 58.3 minutes (run 41350357868), a bound that then has to clear.
        for entry in self._at("1"):
            self.assertGreaterEqual(int(entry["timeout"]), 75)

    def test_the_two_x_run_has_a_short_bound(self) -> None:
        self.assertLessEqual(int(self._at("2")[0]["timeout"]), 40)

    def test_the_benchmark_run_passes_the_names_and_the_empty_shard_means_the_whole_suite(self) -> None:
        self.assertEqual(1, self.job.count("NAMES: ${{ matrix.names }}"))
        self.assertEqual(2, self.job.count("SHARD: ${{ matrix.shard }}"))


class PerfRunStepTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        job = _perf_job(_CI.read_text())
        start = job.index("- name: Run performance benchmarks on base and head")
        end = job.index("- name: Check for a performance regression")
        cls.run_step = job[start:end]

    def test_a_head_run_with_no_rows_stops_the_job_before_the_next_pass(self) -> None:
        loop_end = self.run_step.index("\n            done\n          done")
        check = self.run_step.index('[ ! -s "perf-runs/head-$pair.csv" ]')
        self.assertLess(self.run_step.index("run-perf-benchmark.sh"), check)
        self.assertLess(check, loop_end)

    def test_the_stop_is_a_red_error_that_shows_the_raw_output(self) -> None:
        check = self.run_step[self.run_step.index('[ ! -s "perf-runs/head-$pair.csv" ]'):]
        block = check[: check.index("fi")]
        self.assertIn("::error::", block)
        self.assertIn('tail -n 60 "perf-runs/head-$pair.csv.raw"', block)
        self.assertIn("exit 1", block)


if __name__ == "__main__":
    unittest.main()
