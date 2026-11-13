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


def _unittest_command() -> str:
    step = re.search(r"- name: Test the scripts\n\s+run: \|\n((?:\s{10}.*\n?)+)", _job("scripts"))
    assert step is not None, "the scripts job has no multi-line `run: |` step"
    return step.group(1)


def _listed_modules() -> list[str]:
    return re.findall(r"scripts\.test_\w+", _unittest_command())


class PerfDraftGateTests(unittest.TestCase):
    def test_the_perf_shards_skip_drafts_and_once_the_train_is_live_skip_unlabelled_pull_requests(self) -> None:
        self.assertIn(f"    if: {_PERF_GATE}\n", _job("perf") + "\n")

    def test_master_is_benchmarked_nightly(self) -> None:
        self.assertRegex(_CI, r"\n  schedule:\n    - cron: '[^']+'\n")

    def test_the_perf_job_no_longer_carries_the_script_tests(self) -> None:
        self.assertNotIn("python3 -m unittest", _job("perf"))


# The 2x HiDPI shards cost two jobs and about 40 minutes per batch, so they run nightly on master and on pull requests
# that carry the `perf` label, and not on merge trains, on master pushes or on unlabelled pull requests (owner decision,
# #2618, 2026-11-05).
_PERF_2X_GATE = (
    "(github.event_name == 'schedule'"
    " || (github.event_name == 'workflow_dispatch' && !startsWith(github.ref, 'refs/heads/train/'))"
    " || (github.event_name == 'pull_request' && contains(github.event.pull_request.labels.*.name, 'perf')))"
)
_NIGHTLY_REPORT_STEP = "      - name: Report a failed nightly 2x run\n"


def _steps_of(job: str) -> str:
    return job.split("\n    steps:\n", 1)[1].rstrip()


class Perf2xGateTests(unittest.TestCase):
    def test_the_2x_shards_are_their_own_job_with_their_own_gate(self) -> None:
        self.assertIn(f"    if: {_PERF_2X_GATE}\n", _job("perf-2x") + "\n")

    def test_a_job_gate_cannot_read_the_matrix_so_each_scale_is_its_own_job(self) -> None:
        self.assertNotIn("scale: 2", _job("perf"))
        self.assertNotIn("scale: 1", _job("perf-2x"))

    def test_the_2x_job_runs_the_same_steps_as_the_1x_job_and_one_more(self) -> None:
        one_x, two_x = _steps_of(_job("perf")), _steps_of(_job("perf-2x"))
        self.assertTrue(two_x.startswith(one_x), "the two jobs' steps have drifted apart; edit both")
        extra = two_x[len(one_x):]
        self.assertEqual(1, extra.count("      - name: "), extra)
        self.assertIn(_NIGHTLY_REPORT_STEP, extra)

    def test_the_2x_job_has_the_same_timeout_and_runner_as_the_1x_job(self) -> None:
        for line in ("    runs-on: ubuntu-latest\n", "    timeout-minutes: 75\n"):
            with self.subTest(line=line):
                self.assertIn(line, _job("perf-2x") + "\n")


class Nightly2xReportTests(unittest.TestCase):
    """A red nightly run is one scheduled run nobody opens, so a failed 2x shard comments on a ledger issue."""

    @classmethod
    def setUpClass(cls) -> None:
        job = _job("perf-2x")
        cls.job = job
        cls.step = job[job.index(_NIGHTLY_REPORT_STEP):]

    def test_it_is_the_last_step_and_runs_only_when_a_nightly_run_failed(self) -> None:
        self.assertEqual(1, self.job.count(_NIGHTLY_REPORT_STEP))
        self.assertIn("        if: failure() && github.event_name == 'schedule'\n", self.step)

    def test_it_comments_on_the_ledger_issue_with_the_shard_the_run_and_the_commit(self) -> None:
        self.assertIn("gh issue comment 2643", self.step)
        for needle in ("matrix.label", "github.server_url", "github.run_id", "github.sha"):
            with self.subTest(needle=needle):
                self.assertIn(needle, self.step)

    def test_the_job_may_write_issues_and_nothing_else_beyond_reading_the_repository(self) -> None:
        self.assertIn("    permissions:\n      contents: read\n      issues: write\n", self.job)

    def test_the_1x_job_does_not_carry_the_report_step_or_the_permission(self) -> None:
        one_x = _job("perf")
        self.assertNotIn(_NIGHTLY_REPORT_STEP, one_x)
        self.assertNotIn("issues: write", one_x)


class StyleJobOrderTests(unittest.TestCase):
    """sbt-scalafix drops -Werror from scalacOptions while scalafix runs, so a plain compile after it recompiles every
    source in a fresh JVM (#2618). A compile first leaves scalafix with nothing to rebuild."""

    def test_the_shard_table_is_checked_before_scalafmt_and_scalafix_run(self) -> None:
        names = re.findall(r"(?m)^      - name: (.+)$", _job("checks"))
        self.assertIn("Check the test shard table", names)
        self.assertIn("Apply scalafmt and scalafix", names)
        self.assertLess(names.index("Check the test shard table"), names.index("Apply scalafmt and scalafix"))

    def test_the_style_diff_check_still_runs_after_both(self) -> None:
        names = re.findall(r"(?m)^      - name: (.+)$", _job("checks"))
        after = names.index("Check style tasks did not change files")
        self.assertGreater(after, names.index("Apply scalafmt and scalafix"))
        self.assertGreater(after, names.index("Check the test shard table"))


class ScriptTestsJobTests(unittest.TestCase):
    def test_script_tests_run_on_every_pull_request_including_drafts(self) -> None:
        job = _job("scripts")
        self.assertIn("    name: Script tests\n", job + "\n")
        self.assertNotIn("    if:", job)

    def test_every_script_test_module_is_run(self) -> None:
        listed = set(_listed_modules())
        on_disk = {f"scripts.{p.stem}" for p in (_ROOT / "scripts").glob("test_*.py")}
        self.assertEqual(set(), on_disk - listed, "test modules on disk that CI never runs")

    def test_the_modules_are_listed_one_per_line_in_sorted_order_so_pull_requests_adding_one_merge_cleanly(self) -> None:
        # A single long line made every pull request that adds a module conflict with the one merged before it.
        command = _unittest_command()
        lines = [line.strip() for line in command.splitlines()]
        self.assertEqual("python3 -m unittest \\", lines[0])
        self.assertEqual("-v", lines[-1], "-v closes the list, so appending a module never edits a neighbouring line")
        module_lines = lines[1:-1]
        self.assertTrue(all(re.fullmatch(r"scripts\.test_\w+ \\", line) for line in module_lines), module_lines)
        modules = [line.removesuffix(" \\") for line in module_lines]
        self.assertEqual(sorted(modules), modules)
        self.assertEqual(len(modules), len(set(modules)), "a module is listed twice")


if __name__ == "__main__":
    unittest.main()
