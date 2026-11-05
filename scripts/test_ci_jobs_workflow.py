#!/usr/bin/env python3
"""Checks on how ci.yml groups its Linux checks (#2553 item 6):

    python3 -m unittest scripts.test_ci_jobs_workflow -v

Every job pays for its own runner, checkout, JDK and sbt boot, and under the shared runner cap a job slot is the scarce
thing. So the cheap source checks share one job, and the specs `sbt test` excludes run in the Test job after it, reusing
its compiled classes. Each check is still its own step and runs even when an earlier one failed, so one red check never
hides another.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_CI = (Path(__file__).resolve().parent.parent / ".github" / "workflows" / "ci.yml").read_text()


def _jobs() -> list[str]:
    return re.findall(r"^  ([A-Za-z0-9_-]+):\s*$", _CI.split("\njobs:\n", 1)[1], re.M)


def _job(name: str) -> str:
    body = _CI.split("\njobs:\n", 1)[1]
    lines = body.splitlines()
    start = lines.index(f"  {name}:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


def _steps(job: str) -> list[str]:
    return re.split(r"\n      - ", job)[1:]


def _step_running(job: str, command: str) -> str:
    return next(step for step in _steps(job) if command in step)


class JobLayoutTests(unittest.TestCase):
    def test_the_sbt_check_jobs_are_folded_into_one(self) -> None:
        # wake-train is not a check: it only dispatches the next merge train step on a train branch.
        self.assertEqual(["checks", "scripts", "test", "perf", "perf-2x", "wake-train"], _jobs())

    def test_every_job_has_a_time_limit_so_a_hang_cannot_hold_a_runner_for_six_hours(self) -> None:
        for name in _jobs():
            with self.subTest(job=name):
                self.assertRegex(_job(name), r"\n    timeout-minutes: \d+\n")

    def test_every_source_check_still_runs_in_the_checks_job(self) -> None:
        job = _job("checks")
        for command in (
            'scalafmtAll "Compile / scalafix" "Test / scalafix"',
            "sbt -v checkTestShards",
            "git diff --exit-code",
            "sbt -v architectureCheck",
            "project/architecture-baseline.tsv",
            "'^+# paydown:'",
            "sbt -v checkThirdPartyNotices",
        ):
            with self.subTest(command=command):
                self.assertIn(command, job)

    def test_a_failed_check_does_not_hide_the_ones_after_it(self) -> None:
        steps = _steps(_job("checks"))
        first_check = next(i for i, step in enumerate(steps) if "scalafmtAll" in step)
        for step in steps[first_check + 1 :]:
            with self.subTest(step=step.splitlines()[0]):
                self.assertRegex(step, r"\n        if: (\$\{\{ !cancelled\(\) \}\}|github.event_name == 'pull_request' && !cancelled\(\))\n")

    def test_the_excluded_specs_run_after_the_suite_on_its_compiled_classes(self) -> None:
        job = _job("test")
        steps = _steps(job)
        suite = next(i for i, step in enumerate(steps) if "clean test" in step)
        for command in ("sbt -v realBoundaryTest", "sbt -v heapMeasurementTest"):
            with self.subTest(command=command):
                step = _step_running(job, command)
                self.assertGreater(steps.index(step), suite)
                self.assertNotIn("clean", step)
                self.assertIn("        if: ${{ !cancelled() }}\n", step + "\n")

    def test_they_run_after_the_suite_diagnostics_are_uploaded_so_their_jvm_cannot_overwrite_the_gc_log(self) -> None:
        steps = _steps(_job("test"))
        upload = next(i for i, step in enumerate(steps) if "name: jvm-diag-linux" in step)
        boundary = next(i for i, step in enumerate(steps) if "realBoundaryTest" in step)
        self.assertGreater(boundary, upload)


if __name__ == "__main__":
    unittest.main()
