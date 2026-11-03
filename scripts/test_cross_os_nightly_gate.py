#!/usr/bin/env python3
"""Unit tests for cross_os_nightly_gate.py:

    python3 -m unittest scripts.test_cross_os_nightly_gate -v
"""

from __future__ import annotations

import importlib.util
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name: str):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / f"{name}.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


gate = _load("cross_os_nightly_gate")

BUILT = "a" * 40
OTHER = "b" * 40


def run(created_at: str, status: str = "completed", conclusion: str | None = "success", sha: str = BUILT) -> dict:
    return {"created_at": created_at, "status": status, "conclusion": conclusion, "head_sha": sha}


def payload(*runs: dict) -> dict:
    return {"total_count": len(runs), "workflow_runs": list(runs)}


class VerdictTests(unittest.TestCase):
    def test_passes_when_newest_completed_run_is_green_on_the_built_sha(self) -> None:
        verdict = gate.check(payload(run("2026-11-03T03:17:00Z")), BUILT)
        self.assertTrue(verdict.passed)

    def test_fails_when_newest_completed_run_is_red(self) -> None:
        verdict = gate.check(payload(run("2026-11-03T03:17:00Z", conclusion="failure")), BUILT)
        self.assertFalse(verdict.passed)
        self.assertIn("failure", verdict.reason)

    def test_cancelled_and_skipped_runs_are_not_green(self) -> None:
        for conclusion in ("cancelled", "skipped", "timed_out", None):
            with self.subTest(conclusion=conclusion):
                verdict = gate.check(payload(run("2026-11-03T03:17:00Z", conclusion=conclusion)), BUILT)
                self.assertFalse(verdict.passed)

    def test_fails_when_there_is_no_run_at_all(self) -> None:
        verdict = gate.check(payload(), BUILT)
        self.assertFalse(verdict.passed)
        self.assertIn("no completed", verdict.reason)

    def test_fails_when_only_unfinished_runs_exist(self) -> None:
        verdict = gate.check(payload(run("2026-11-03T03:17:00Z", status="in_progress", conclusion=None)), BUILT)
        self.assertFalse(verdict.passed)
        self.assertIn("no completed", verdict.reason)

    def test_fails_when_the_nightly_tested_a_different_sha(self) -> None:
        verdict = gate.check(payload(run("2026-11-03T03:17:00Z", sha=OTHER)), BUILT)
        self.assertFalse(verdict.passed)
        self.assertIn(OTHER[:12], verdict.reason)
        self.assertIn(BUILT[:12], verdict.reason)

    def test_judges_the_newest_completed_run_whatever_order_the_api_lists_them(self) -> None:
        old_green = run("2026-11-02T03:17:00Z")
        new_red = run("2026-11-03T03:17:00Z", conclusion="failure")
        self.assertFalse(gate.check(payload(old_green, new_red), BUILT).passed)
        self.assertFalse(gate.check(payload(new_red, old_green), BUILT).passed)

    def test_a_run_still_in_progress_is_ignored_in_favour_of_the_last_completed_one(self) -> None:
        verdict = gate.check(
            payload(
                run("2026-11-03T03:17:00Z", status="in_progress", conclusion=None, sha=OTHER),
                run("2026-11-02T03:17:00Z"),
            ),
            BUILT,
        )
        self.assertTrue(verdict.passed)

    def test_reason_is_one_line(self) -> None:
        for body in (payload(), payload(run("2026-11-03T03:17:00Z", conclusion="failure"))):
            self.assertNotIn("\n", gate.check(body, BUILT).reason)


class MainTests(unittest.TestCase):
    def _main(self, args: list[str], stdin: str = "") -> tuple[int, str, str]:
        out, err = io.StringIO(), io.StringIO()
        original = sys.stdin
        sys.stdin = io.StringIO(stdin)
        try:
            with redirect_stdout(out), redirect_stderr(err):
                code = gate.main(args)
        finally:
            sys.stdin = original
        return code, out.getvalue(), err.getvalue()

    def test_reads_stdin_and_exits_zero_on_pass(self) -> None:
        code, out, _ = self._main(["--sha", BUILT], json.dumps(payload(run("2026-11-03T03:17:00Z"))))
        self.assertEqual(code, 0)
        self.assertEqual(len(out.strip().splitlines()), 1)

    def test_reads_a_file_and_exits_one_on_fail(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "runs.json"
            path.write_text(json.dumps(payload(run("2026-11-03T03:17:00Z", conclusion="failure"))))
            code, _, err = self._main(["--sha", BUILT, str(path)])
        self.assertEqual(code, 1)
        self.assertEqual(len(err.strip().splitlines()), 1)

    def test_unparseable_input_fails_with_a_reason(self) -> None:
        code, _, err = self._main(["--sha", BUILT], "not json")
        self.assertEqual(code, 1)
        self.assertIn("not valid JSON", err)


if __name__ == "__main__":
    unittest.main()
