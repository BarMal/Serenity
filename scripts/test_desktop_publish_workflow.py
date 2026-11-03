#!/usr/bin/env python3
"""Checks that Desktop Publish gates nightly builds on the Cross-OS nightly instead of re-running the suite (#2557):

    python3 -m unittest scripts.test_desktop_publish_workflow -v
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_WORKFLOW = (_ROOT / ".github" / "workflows" / "desktop-publish.yml").read_text()
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()


def _job(text: str, name: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line == f"  {name}:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


def _steps(job: str) -> list[str]:
    return re.split(r"(?m)^      - name: ", job)[1:]


def _step(job: str, name: str) -> str:
    return next(step for step in _steps(job) if step.startswith(name))


class GateJobTests(unittest.TestCase):
    def test_workflow_can_read_actions_runs(self) -> None:
        self.assertIn("  actions: read\n", _WORKFLOW.split("jobs:")[0])

    def test_gate_job_queries_the_scheduled_master_runs_of_cross_os_tests(self) -> None:
        job = _job(_WORKFLOW, "gate")
        self.assertIn("actions/workflows/cross-os-tests.yml/runs?branch=master&event=schedule", job)
        self.assertIn("gh api", job)
        self.assertIn("GH_TOKEN: ${{ github.token }}", job)

    def test_gate_job_pipes_the_runs_to_the_script_with_the_sha_being_built(self) -> None:
        step = _step(_job(_WORKFLOW, "gate"), "Require a green Cross-OS nightly")
        self.assertIn("scripts/cross_os_nightly_gate.py --sha", step)
        self.assertIn("set -euo pipefail", step)

    def test_gate_only_applies_to_master_builds(self) -> None:
        step = _step(_job(_WORKFLOW, "gate"), "Require a green Cross-OS nightly")
        self.assertIn("if: github.ref == 'refs/heads/master'", step)

    def test_build_and_publish_wait_for_the_gate(self) -> None:
        self.assertIn("    needs: gate\n", _job(_WORKFLOW, "build") + "\n")
        self.assertIn("    needs: [gate, build]\n", _job(_WORKFLOW, "publish") + "\n")

    def test_build_and_publish_use_the_sha_the_gate_checked(self) -> None:
        for name in ("build", "publish"):
            with self.subTest(job=name):
                self.assertIn("ref: ${{ needs.gate.outputs.sha || github.ref }}", _job(_WORKFLOW, name))


class TestStepTests(unittest.TestCase):
    def test_master_builds_do_not_run_the_suite(self) -> None:
        step = _step(_job(_WORKFLOW, "build"), "Build runnable JAR")
        self.assertIn("if: github.ref == 'refs/heads/master'", step)
        self.assertIn("sbt -v clean assembly writePackageVersion", step)

    def test_every_other_build_keeps_the_suite(self) -> None:
        step = _step(_job(_WORKFLOW, "build"), "Test and build runnable JAR")
        self.assertIn("if: github.ref != 'refs/heads/master'", step)
        self.assertIn("sbt -v clean test assembly writePackageVersion", step)

    def test_linux_runs_the_suite_under_a_virtual_display(self) -> None:
        step = _step(_job(_WORKFLOW, "build"), "Test and build runnable JAR")
        self.assertIn("apt-get install -y xvfb", step)
        self.assertIn('xvfb-run -a -s "-screen 0 1920x1200x24" sbt -v clean test assembly writePackageVersion', step)

    def test_no_step_runs_the_suite_on_a_master_build(self) -> None:
        for step in _steps(_job(_WORKFLOW, "build")):
            if "clean test" in step:
                self.assertIn("if: github.ref != 'refs/heads/master'", step)


class CiWiringTests(unittest.TestCase):
    def test_ci_runs_the_new_script_tests(self) -> None:
        for module in ("scripts.test_cross_os_nightly_gate", "scripts.test_desktop_publish_workflow"):
            with self.subTest(module=module):
                self.assertIn(module, _CI)


if __name__ == "__main__":
    unittest.main()
