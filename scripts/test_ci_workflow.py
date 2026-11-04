#!/usr/bin/env python3
"""Checks on which CI jobs run for draft pull requests (owner decision, #2484, 2026-11-03):

    python3 -m unittest scripts.test_ci_workflow -v

Drafts skip the four performance-benchmark shards; ready pull requests, master and dispatches keep them. The script
tests used to run inside the perf job, so they now have their own job, which every pull request runs.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()
_DRAFT_GATE = "(github.event_name != 'pull_request' || !github.event.pull_request.draft)"
# Once the merge train is live it runs the benchmarks once per batch (#2553 item 2), so a pull request runs them only when
# it carries the `perf` label.
_PERF_GATE = (
    "(github.event_name != 'pull_request' || (!github.event.pull_request.draft && vars.MERGE_TRAIN_MODE != 'live')"
    " || contains(github.event.pull_request.labels.*.name, 'perf'))"
)


def _job(name: str) -> str:
    lines = _CI.splitlines()
    start = next(i for i, line in enumerate(lines) if line == f"  {name}:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


class PerfDraftGateTests(unittest.TestCase):
    def test_the_perf_shards_skip_drafts_and_once_the_train_is_live_skip_unlabelled_pull_requests(self) -> None:
        self.assertIn(f"    if: {_PERF_GATE}\n", _job("perf") + "\n")

    def test_master_is_benchmarked_nightly(self) -> None:
        self.assertRegex(_CI, r"\n  schedule:\n    - cron: '[^']+'\n")

    def test_the_perf_job_no_longer_carries_the_script_tests(self) -> None:
        self.assertNotIn("python3 -m unittest", _job("perf"))


class ScriptTestsJobTests(unittest.TestCase):
    def test_script_tests_run_on_every_pull_request_including_drafts(self) -> None:
        job = _job("scripts")
        self.assertIn("    name: Script tests\n", job + "\n")
        self.assertNotIn("    if:", job)

    def test_every_script_test_module_is_run(self) -> None:
        run = re.search(r"python3 -m unittest ([^\n]+) -v", _job("scripts"))
        self.assertIsNotNone(run)
        listed = set(run.group(1).split())
        on_disk = {f"scripts.{p.stem}" for p in (_ROOT / "scripts").glob("test_*.py")}
        self.assertEqual(set(), on_disk - listed, "test modules on disk that CI never runs")


if __name__ == "__main__":
    unittest.main()
