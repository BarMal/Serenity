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


if __name__ == "__main__":
    unittest.main()
