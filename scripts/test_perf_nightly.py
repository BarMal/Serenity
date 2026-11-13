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
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
_ROOT = _SCRIPTS.parent
_WORKFLOW = _ROOT / ".github" / "workflows" / "perf-nightly-abba.yml"
sys.path.insert(0, str(_SCRIPTS))

import perf_nightly  # noqa: E402


def _run(
    sha: str,
    created: str,
    conclusion: str | None = "success",
    status: str = "completed",
    branch: str = "master",
    event: str = "schedule",
) -> dict:
    return {
        "head_sha": sha,
        "created_at": created,
        "conclusion": conclusion,
        "status": status,
        "head_branch": branch,
        "event": event,
    }


def _git(repo: str, *args: str) -> str:
    env = {
        **os.environ,
        "GIT_AUTHOR_NAME": "t",
        "GIT_AUTHOR_EMAIL": "t@example.com",
        "GIT_COMMITTER_NAME": "t",
        "GIT_COMMITTER_EMAIL": "t@example.com",
    }
    return subprocess.run(["git", "-C", repo, *args], env=env, capture_output=True, text=True, check=True).stdout.strip()


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

    def test_a_manual_run_on_another_branch_is_not_a_baseline(self) -> None:
        # The shape of nights 17-21 (2026-11-09 to 2026-11-13): the newest success was a dispatch on a topic branch whose
        # commit the nightly's clone never had, so every night failed building it.
        runs = {
            "workflow_runs": [
                _run("master-night", "2026-11-08T12:02:17Z"),
                _run("topic", "2026-11-08T21:23:16Z", branch="claude/kept-clip-allocation", event="workflow_dispatch"),
                _run("failed-night", "2026-11-09T14:07:19Z", "failure"),
            ]
        }
        self.assertEqual("master-night", perf_nightly.previous_nightly_sha(runs))

    def test_only_topic_branch_successes_give_an_empty_answer(self) -> None:
        runs = {"workflow_runs": [_run("topic", "2026-11-08T21:23:16Z", branch="claude/x", event="workflow_dispatch")]}
        self.assertEqual("", perf_nightly.previous_nightly_sha(runs))

    def test_a_manual_run_on_master_is_a_baseline(self) -> None:
        runs = {"workflow_runs": [_run("by-hand", "2026-11-07T11:23:38Z", event="workflow_dispatch")]}
        self.assertEqual("by-hand", perf_nightly.previous_nightly_sha(runs))

    def test_the_newest_run_that_the_check_accepts_is_the_baseline(self) -> None:
        runs = {"workflow_runs": [_run("a", "2026-11-01T03:00:00Z"), _run("b", "2026-11-02T03:00:00Z"), _run("c", "2026-11-03T03:00:00Z")]}
        self.assertEqual("b", perf_nightly.previous_nightly_sha(runs, is_ancestor=lambda sha: sha != "c"))

    def test_when_the_check_accepts_nothing_the_answer_is_empty(self) -> None:
        runs = {"workflow_runs": [_run("a", "2026-11-01T03:00:00Z")]}
        self.assertEqual("", perf_nightly.previous_nightly_sha(runs, is_ancestor=lambda sha: False))


class AncestryTests(unittest.TestCase):
    """The commit to compare against has to be one the nightly can check out: part of the history it stands on."""

    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.repo = self._tmp.name
        _git(self.repo, "init", "-q", "-b", "master")
        for name in ("first", "second"):
            Path(self.repo, f"{name}.txt").write_text(name)
            _git(self.repo, "add", ".")
            _git(self.repo, "commit", "-q", "-m", name)
        self.first = _git(self.repo, "rev-parse", "HEAD~1")
        self.second = _git(self.repo, "rev-parse", "HEAD")
        _git(self.repo, "checkout", "-q", "-b", "side", self.first)
        Path(self.repo, "side.txt").write_text("side")
        _git(self.repo, "add", ".")
        _git(self.repo, "commit", "-q", "-m", "side")
        self.side = _git(self.repo, "rev-parse", "HEAD")
        _git(self.repo, "checkout", "-q", "master")

    def test_a_commit_in_the_history_is_accepted(self) -> None:
        check = perf_nightly.git_ancestor_of("HEAD", self.repo)
        self.assertTrue(check(self.first))
        self.assertTrue(check(self.second))

    def test_a_commit_on_a_branch_that_is_not_in_the_history_is_refused(self) -> None:
        self.assertFalse(perf_nightly.git_ancestor_of("HEAD", self.repo)(self.side))

    def test_a_commit_the_clone_does_not_have_is_refused(self) -> None:
        self.assertFalse(perf_nightly.git_ancestor_of("HEAD", self.repo)("d7706865893bedf7d6e83e145af733dd59798ddc"))

    def test_the_command_skips_a_baseline_it_cannot_check_out_for_an_older_one_it_can(self) -> None:
        runs = {
            "workflow_runs": [
                _run(self.first, "2026-11-08T12:02:17Z"),
                _run(self.side, "2026-11-09T12:00:00Z"),
                _run("d7706865893bedf7d6e83e145af733dd59798ddc", "2026-11-10T12:00:00Z"),
            ]
        }
        path = os.path.join(self._tmp.name, "..", f"runs-{os.getpid()}.json")
        with open(path, "w") as handle:
            json.dump(runs, handle)
        self.addCleanup(os.remove, path)
        out = io.StringIO()
        before = os.getcwd()
        os.chdir(self.repo)
        try:
            with contextlib.redirect_stdout(out):
                code = perf_nightly.main_with_args(["previous-sha", path, "--within", "HEAD"])
        finally:
            os.chdir(before)
        self.assertEqual((0, f"{self.first}\n"), (code, out.getvalue()))


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

    def test_previous_sha_without_a_check_does_not_need_a_repository(self) -> None:
        runs = {"workflow_runs": [_run("not-a-commit", "2026-11-01T03:00:00Z")]}
        self.assertEqual((0, "not-a-commit\n"), self._main("previous-sha", self._file("runs.json", json.dumps(runs))))

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

    def test_the_baseline_has_to_be_part_of_the_history_the_job_checked_out(self) -> None:
        self.assertRegex(self.text, r'perf_nightly\.py previous-sha "\$RUNNER_TEMP/runs\.json" --within HEAD')

    def test_the_decision_is_made_by_the_script_and_the_csvs_are_uploaded_even_on_failure(self) -> None:
        self.assertIn("perf_nightly.py verdict", self.text)
        self.assertRegex(self.text, r"if: always\(\)\n\s+uses: actions/upload-artifact@v4")


    def test_it_builds_the_app_project_for_a_ref_that_has_one(self) -> None:
        # bench-artifacts.yml does the same since the module split (#2502): the root aggregate has no sources to pack.
        self.assertIn("grep -Eq '^lazy val app\\b'", self.text)
        self.assertIn("perf-assembly-app.sbt", self.text)
        self.assertIn('sbt -v "app/Test / assembly"', self.text)

    def test_it_still_builds_a_ref_from_before_the_split_with_the_root_assembly(self) -> None:
        self.assertIn("perf-assembly.sbt", self.text)
        self.assertIn('sbt -v "Test / assembly"', self.text)

    def test_it_looks_for_the_jar_under_the_app_project_as_well_as_the_root(self) -> None:
        self.assertIn("modules/app/target/scala-*/serenity-perf.jar", self.text)
        self.assertIn("target/scala-*/serenity-perf.jar", self.text)


class AbbaScriptTests(unittest.TestCase):
    """A setup failure must not read as a blocking change: abba.sh exits 1 for blocking and 2 for no data."""

    def _run(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run(
            ["bash", str(_ROOT / "bench" / "abba.sh"), *args], capture_output=True, text=True, check=False
        )

    def test_a_missing_jar_is_no_data_not_a_blocking_change(self) -> None:
        with tempfile.TemporaryDirectory() as out:
            result = self._run("/nonexistent/a.jar", "/nonexistent/b.jar", out)
        self.assertEqual(2, result.returncode)
        self.assertIn("jar not found", result.stderr)

    def test_an_empty_jar_path_is_no_data_too(self) -> None:
        with tempfile.TemporaryDirectory() as out:
            result = self._run("", "", out)
        self.assertEqual(2, result.returncode)


if __name__ == "__main__":
    unittest.main()
