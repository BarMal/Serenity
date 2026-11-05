#!/usr/bin/env python3
"""Checks on the `perf` job's matrix in .github/workflows/ci.yml:

    python3 -m unittest scripts.test_perf_matrix -v

Every scale's suite must run as complete, non-overlapping shards (`BenchmarkShard` selects a contiguous slice by
`index/count`), and the entries must not collide on check name or artifact name, or one shard's results would
overwrite another's. The job must also carry an explicit timeout: GitHub's default of six hours hides a stuck job.
"""

from __future__ import annotations

import re
import unittest
from collections import defaultdict
from pathlib import Path

_CI = Path(__file__).resolve().parent.parent / ".github" / "workflows" / "ci.yml"


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

    def test_the_matrix_is_read(self) -> None:
        self.assertGreaterEqual(len(self.entries), 2)
        for entry in self.entries:
            self.assertEqual({"label", "scale", "screen", "dpi", "shard", "results"}, set(entry))

    def test_every_scale_runs_as_complete_shards(self) -> None:
        shards_by_scale: dict[str, list[tuple[int, int]]] = defaultdict(list)
        for entry in self.entries:
            index, count = (int(part) for part in entry["shard"].split("/"))
            shards_by_scale[entry["scale"]].append((index, count))
        for scale, shards in shards_by_scale.items():
            counts = {count for _, count in shards}
            self.assertEqual(1, len(counts), f"scale {scale}: shards disagree on the count: {shards}")
            count = counts.pop()
            self.assertEqual(list(range(1, count + 1)), sorted(index for index, _ in shards), f"scale {scale}")

    def test_the_one_x_suite_is_sharded_like_the_two_x_suite(self) -> None:
        counts = {
            entry["scale"]: int(entry["shard"].split("/")[1])
            for entry in self.entries
        }
        self.assertEqual({"1": 2, "2": 2}, counts)

    def test_labels_and_artifacts_are_unique(self) -> None:
        labels = [entry["label"] for entry in self.entries]
        results = [entry["results"] for entry in self.entries]
        self.assertEqual(len(labels), len(set(labels)), labels)
        self.assertEqual(len(results), len(set(results)), results)

    def test_a_sharded_entry_names_its_shard_in_its_label(self) -> None:
        for entry in self.entries:
            index, count = entry["shard"].split("/")
            if count != "1":
                self.assertIn(f"shard {index}/{count}", entry["label"])

    def test_the_job_has_an_explicit_timeout_above_the_measured_longest_run(self) -> None:
        match = re.search(r"^    timeout-minutes: (\d+)$", self.job, re.MULTILINE)
        self.assertIsNotNone(match, "perf job has no timeout-minutes")
        # The longest 1x run seen was 58.3 minutes (run 41350357868), a bound that then has to clear.
        self.assertGreaterEqual(int(match.group(1)), 75)


class PerfCompileStepTests(unittest.TestCase):
    """The first benchmark run on each side compiles the whole project, about 400 s (#2618). Compiling both sides in one
    step, side by side, costs about one of them; the benchmarks themselves still run one at a time."""

    @classmethod
    def setUpClass(cls) -> None:
        job = _perf_job(_CI.read_text())
        cls.job = job
        start = job.index("- name: Compile base and head side by side")
        end = job.index("- name: Run performance benchmarks on base and head")
        cls.compile_step = job[start:end]

    def test_it_runs_after_the_base_is_checked_out_and_before_the_first_benchmark(self) -> None:
        self.assertLess(self.job.index("- name: Check out the base commit"), self.job.index("- name: Compile base and head side by side"))
        self.assertLess(self.job.index("- name: Compile base and head side by side"), self.job.index("- name: Run performance benchmarks on base and head"))

    def test_it_compiles_the_base_checkout_and_the_head_checkout(self) -> None:
        self.assertEqual(2, self.compile_step.count("sbt -v Test/compile"))
        self.assertIn('cd "$RUNNER_TEMP/base"', self.compile_step)

    def test_the_two_compiles_run_at_the_same_time_and_both_are_waited_for(self) -> None:
        self.assertEqual(2, len(re.findall(r"Test/compile[^\n]*&\n", self.compile_step)))
        self.assertEqual(2, len(re.findall(r'^\s+wait "\$\w+"', self.compile_step, re.MULTILINE)))

    def test_a_failed_compile_does_not_fail_the_step(self) -> None:
        # The run that follows fails the way it always did, with the head's raw output shown (see PerfRunStepTests).
        for wait in re.findall(r'^\s+wait "\$\w+".*$', self.compile_step, re.MULTILINE):
            with self.subTest(wait=wait):
                self.assertIn("||", wait)

    def test_each_sides_output_is_kept_for_a_reader(self) -> None:
        self.assertIn("compile-base.log", self.compile_step)
        self.assertIn("compile-head.log", self.compile_step)

    def test_it_is_a_bash_step_so_background_jobs_and_wait_exist(self) -> None:
        self.assertIn("shell: bash", self.compile_step)


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
