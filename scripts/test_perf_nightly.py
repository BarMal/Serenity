#!/usr/bin/env python3
"""Unit tests for perf_nightly.py and the nightly ABBA workflow's shape:

    python3 -m unittest scripts.test_perf_nightly -v
"""

from __future__ import annotations

import contextlib
import io
import json
import os
import re
import sys
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
_ROOT = _SCRIPTS.parent
_WORKFLOW = _ROOT / ".github" / "workflows" / "perf-nightly-abba.yml"
sys.path.insert(0, str(_SCRIPTS))

import perf_nightly  # noqa: E402


def _run(sha: str, created: str, conclusion: str | None = "success", status: str = "completed") -> dict:
    return {"head_sha": sha, "created_at": created, "conclusion": conclusion, "status": status}


class PreviousNightlyTests(unittest.TestCase):
    def test_the_latest_successful_run_is_the_baseline(self) -> None:
        runs = {"workflow_runs": [_run("old", "2026-11-01T03:00:00Z"), _run("new", "2026-11-03T03:00:00Z"), _run("mid", "2026-11-02T03:00:00Z")]}
        self.assertEqual("new", perf_nightly.previous_nightly_sha(runs))

    def test_a_failed_night_is_not_a_baseline(self) -> None:
        runs = {"workflow_runs": [_run("good", "2026-11-01T03:00:00Z"), _run("bad", "2026-11-02T03:00:00Z", "failure")]}
        self.assertEqual("good", perf_nightly.previous_nightly_sha(runs))

    def test_a_run_still_in_progress_is_not_a_baseline(self) -> None:
        runs = {"workflow_runs": [_run("good", "2026-11-01T03:00:00Z"), _run("now", "2026-11-02T03:00:00Z", None, "in_progress")]}
        self.assertEqual("good", perf_nightly.previous_nightly_sha(runs))

    def test_no_runs_gives_an_empty_answer(self) -> None:
        self.assertEqual("", perf_nightly.previous_nightly_sha({"workflow_runs": []}))
        self.assertEqual("", perf_nightly.previous_nightly_sha({}))


class VerdictTests(unittest.TestCase):
    def test_a_clean_abba_passes(self) -> None:
        code, message = perf_nightly.verdict(0, ["a.b"])
        self.assertEqual(0, code)
        self.assertNotIn("::error::", message)

    def test_a_blocking_abba_fails_and_names_the_rows(self) -> None:
        code, message = perf_nightly.verdict(1, ["a.b", "c.d"])
        self.assertEqual(1, code)
        self.assertTrue(message.startswith("::error::"))
        self.assertIn("a.b, c.d", message)

    def test_no_data_fails_rather_than_passing_silently(self) -> None:
        code, message = perf_nightly.verdict(2, ["a.b"])
        self.assertEqual(1, code)
        self.assertIn("no comparable data", message)


class CommandTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)

    def _file(self, name: str, text: str) -> str:
        path = os.path.join(self._tmp.name, name)
        with open(path, "w") as handle:
            handle.write(text)
        return path

    def _main(self, *argv: str) -> tuple[int, str]:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = perf_nightly.main_with_args(list(argv))
        return code, out.getvalue()

    def test_prefixes_are_the_exclusion_entries_without_comments(self) -> None:
        path = self._file("ex.txt", "# why\nrow.one\n\nrow.two  # trailing\n")
        self.assertEqual((0, "row.one\nrow.two\n"), self._main("prefixes", path))

    def test_the_checked_in_exclusion_file_yields_prefixes(self) -> None:
        _, out = self._main("prefixes", str(_SCRIPTS / "perf-gate-excluded-rows.txt"))
        self.assertIn("rope.large_json.cursor_offset", out.split())

    def test_previous_sha_reads_the_api_response(self) -> None:
        path = self._file("runs.json", json.dumps({"workflow_runs": [_run("abc123", "2026-11-01T03:00:00Z")]}))
        self.assertEqual((0, "abc123\n"), self._main("previous-sha", path))

    def test_verdict_exits_with_the_decision(self) -> None:
        ex = self._file("ex.txt", "row.one\n")
        self.assertEqual(0, self._main("verdict", "0", "--exclude-file", ex)[0])
        self.assertEqual(1, self._main("verdict", "1", "--exclude-file", ex)[0])


class WorkflowShapeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = _WORKFLOW.read_text()

    def test_it_runs_on_a_schedule_and_by_hand(self) -> None:
        self.assertRegex(self.text, r"(?m)^  schedule:\n    - cron: ")
        self.assertRegex(self.text, r"(?m)^  workflow_dispatch:")

    def test_every_job_has_a_commented_timeout(self) -> None:
        lines = self.text.splitlines()
        jobs = [i for i, line in enumerate(lines) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", line) and i > lines.index("jobs:")]
        self.assertTrue(jobs)
        for start in jobs:
            timeout = next((i for i in range(start + 1, len(lines)) if lines[i].startswith("    timeout-minutes: ")), None)
            self.assertIsNotNone(timeout, f"job at line {start + 1} has no timeout-minutes")
            self.assertTrue(lines[timeout - 1].lstrip().startswith("#"), "the timeout has no comment above it")

    def test_the_excluded_rows_come_from_the_checked_in_list(self) -> None:
        self.assertIn("perf_nightly.py prefixes scripts/perf-gate-excluded-rows.txt", self.text)

    def test_the_decision_is_made_by_the_script_and_the_csvs_are_uploaded_even_on_failure(self) -> None:
        self.assertIn("perf_nightly.py verdict", self.text)
        self.assertRegex(self.text, r"if: always\(\)\n\s+uses: actions/upload-artifact@v4")


if __name__ == "__main__":
    unittest.main()
