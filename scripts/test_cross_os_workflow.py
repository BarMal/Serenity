#!/usr/bin/env python3
"""Checks on how the Windows and macOS tests are split across jobs (#2484):

    python3 -m unittest scripts.test_cross_os_workflow -v

The required checks "Test (windows)" and "Test (macos)" must still exist under exactly those names (branch protection
lists them), must wait for every shard of their own OS, and must fail when a shard is cancelled or skipped as well as
when it fails. The shard count in the workflow, in build.sbt and in the SERENITY_TEST_SHARD value must agree, or part of
the suite would not run.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_CALLER = (_ROOT / ".github" / "workflows" / "cross-os-tests.yml").read_text()
_SHARDS = (_ROOT / ".github" / "workflows" / "cross-os-test-shards.yml").read_text()
_BUILD = (_ROOT / "build.sbt").read_text()


def _job(text: str, name: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line == f"  {name}:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


class AggregateJobTests(unittest.TestCase):
    def test_each_os_keeps_its_required_check_name_and_waits_for_its_own_shards(self) -> None:
        for label in ("windows", "macos"):
            with self.subTest(label=label):
                job = _job(_CALLER, f"test-{label}")
                self.assertIn(f"    name: Test ({label})\n", job + "\n")
                self.assertIn(f"    needs: shards-{label}\n", job + "\n")
                self.assertIn("    if: always() && ", job + "\n")

    def test_a_shard_that_did_not_succeed_fails_the_check(self) -> None:
        for label in ("windows", "macos"):
            with self.subTest(label=label):
                job = _job(_CALLER, f"test-{label}")
                self.assertIn(f"needs.shards-{label}.result", job)
                self.assertIn('test "$RESULT" = success', job)

    def test_each_os_calls_the_shard_workflow_on_its_own_runner(self) -> None:
        self.assertIn("os: windows-latest", _job(_CALLER, "shards-windows"))
        self.assertIn("os: macos-latest", _job(_CALLER, "shards-macos"))
        for label in ("windows", "macos"):
            job = _job(_CALLER, f"shards-{label}")
            self.assertIn("uses: ./.github/workflows/cross-os-test-shards.yml", job)
            self.assertIn(f"label: {label}", job)

    def test_there_are_no_other_jobs_to_claim_the_required_names(self) -> None:
        jobs = re.findall(r"^  ([A-Za-z0-9_-]+):\s*$", _CALLER.split("\njobs:\n", 1)[1], re.M)
        # wake-train only dispatches the next merge train step on a train branch; its name is not a required check.
        self.assertEqual(["shards-windows", "shards-macos", "test-windows", "test-macos", "wake-train"], jobs)
        self.assertIn("    name: Wake the merge train\n", _job(_CALLER, "wake-train"))


_DRAFT_GATE = "(github.event_name != 'pull_request' || !github.event.pull_request.draft)"
# Once the merge train is live it runs Windows and macOS once per batch instead (#2553 item 3).
_TRAIN_GATE = (
    "(github.event_name != 'pull_request' || (!github.event.pull_request.draft && vars.MERGE_TRAIN_MODE != 'live'))"
)
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()
_PACKAGE = (_ROOT / ".github" / "workflows" / "desktop-release.yml").read_text()


def _pull_request_types(text: str) -> list[str]:
    match = re.search(r"^  pull_request:\n(?:    #[^\n]*\n)*    types: \[([a-z_, ]+)\]\n", text, re.M)
    return [] if match is None else [t.strip() for t in match.group(1).split(",")]


class DraftGateTests(unittest.TestCase):
    """Draft pull requests run Linux CI only; Windows and macOS start when the pull request is marked ready (owner
    decision on #2484, 2026-11-03, while shared runners were starved)."""

    def test_marking_ready_starts_cross_os_ci_and_the_package_check(self) -> None:
        for name, text in (("cross-os", _CALLER), ("ci", _CI), ("package", _PACKAGE)):
            with self.subTest(workflow=name):
                self.assertEqual(
                    ["opened", "synchronize", "reopened", "ready_for_review"], _pull_request_types(text)
                )

    def test_shards_and_their_checks_skip_drafts_and_live_train_pull_requests_together(self) -> None:
        for label in ("windows", "macos"):
            with self.subTest(label=label):
                self.assertIn(f"    if: {_TRAIN_GATE}\n", _job(_CALLER, f"shards-{label}") + "\n")
                self.assertIn(f"    if: always() && {_TRAIN_GATE}\n", _job(_CALLER, f"test-{label}") + "\n")

    def test_the_three_os_package_build_skips_drafts(self) -> None:
        self.assertIn(_DRAFT_GATE, _job(_PACKAGE, "build"))


class FailureSummaryTests(unittest.TestCase):
    """A failing test's name and message go in the step summary and one annotation per failure, instead of ~380 lines
    of raw JUnit XML on Windows and macOS and nothing at all on Linux (#2557)."""

    _COMMAND = 'scripts/summarize_test_failures.py target/test-reports --summary "$GITHUB_STEP_SUMMARY"'

    def _step(self, job: str) -> str:
        return next(step for step in re.split(r"\n      - ", job) if "summarize_test_failures.py" in step)

    def test_the_linux_test_job_summarises_right_after_running_the_tests(self) -> None:
        job = _job(_CI, "test")
        step = self._step(job)
        self.assertIn(self._COMMAND, step)
        self.assertIn("if: always()", step)
        self.assertLess(job.index("name: Run tests"), job.index(step))
        self.assertLess(job.index(step), job.index("name: Upload test reports"))

    def test_each_shard_summarises_right_after_running_the_tests(self) -> None:
        job = _job(_SHARDS, "test-shard")
        step = self._step(job)
        self.assertIn(self._COMMAND, step)
        self.assertIn("if: always()", step)
        self.assertLess(job.index("name: Run tests"), job.index(step))
        self.assertLess(job.index(step), job.index("name: Upload test reports"))

    def test_the_raw_xml_dump_is_gone_but_the_reports_are_still_uploaded(self) -> None:
        self.assertNotIn("Print test failure detail", _SHARDS)
        self.assertNotIn('cat "$report"', _SHARDS)
        self.assertIn("path: target/test-reports/**", _SHARDS)

    def test_a_failed_aggregate_says_which_result_it_saw_and_where_the_shards_are(self) -> None:
        for label in ("windows", "macos"):
            with self.subTest(label=label):
                job = _job(_CALLER, f"test-{label}")
                self.assertIn("RUN_URL: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}", job)
                self.assertIn('"$RESULT"', job.split("::error", 1)[1])
                self.assertIn("$RUN_URL", job.split("::error", 1)[1])
                self.assertIn(f"Test ({label}, shard", job)


def _concurrency(text: str) -> dict[str, str]:
    block = re.search(r"^concurrency:\n((?:  [^\n]*\n)+)", text, re.M).group(1)
    return dict(re.findall(r"^  ([a-z-]+): ([^\n]+)$", block, re.M))


class ConcurrencyTests(unittest.TestCase):
    """A newer push may cancel a pull request's older run, but never a master, nightly or dispatched run: one shared,
    cancelling group left master with no completed Windows or macOS result after the 11-02 nightly (#2557)."""

    def test_only_pull_request_runs_cancel_an_older_run(self) -> None:
        self.assertEqual("${{ github.event_name == 'pull_request' }}", _concurrency(_CALLER)["cancel-in-progress"])

    def test_the_nightly_has_its_own_group_so_a_master_push_cannot_replace_it_while_it_waits(self) -> None:
        self.assertEqual(
            "${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}"
            "${{ github.event_name == 'schedule' && '-nightly' || '' }}",
            _concurrency(_CALLER)["group"],
        )


class ShardCountTests(unittest.TestCase):
    def test_the_shard_count_agrees_in_build_workflow_and_environment(self) -> None:
        count = int(re.search(r"val TestShardCount = (\d+)", _BUILD).group(1))
        matrix = re.search(r"shard: \[([0-9, ]+)\]", _SHARDS).group(1)
        self.assertEqual(list(range(1, count + 1)), [int(n) for n in matrix.split(",")])
        self.assertIn(f"SERENITY_TEST_SHARD=${{{{ matrix.shard }}}}/{count}", _SHARDS)
        self.assertIn(f"shard ${{{{ matrix.shard }}}}/{count})", _SHARDS)


class ShardJobTests(unittest.TestCase):
    def test_artifacts_carry_the_shard_so_one_shard_cannot_overwrite_another(self) -> None:
        names = re.findall(r"^\s+name: ((?:test-reports|jvm-diag)-[^\n]+)$", _SHARDS, re.M)
        self.assertEqual(2, len(names))
        for name in names:
            self.assertIn("${{ matrix.shard }}", name)
            self.assertIn("${{ inputs.label }}", name)

    def test_the_hang_guard_and_job_limit_are_kept(self) -> None:
        self.assertIn("HANG_MINUTES: '52'", _SHARDS)
        self.assertIn("timeout-minutes: 60", _SHARDS)

    def test_the_shards_run_to_completion_independently(self) -> None:
        self.assertIn("fail-fast: false", _SHARDS)

    def test_the_extra_jvm_options_still_reach_the_run(self) -> None:
        self.assertIn("jvm_opts: ${{ inputs.jvm_opts }}", _job(_CALLER, "shards-macos"))
        self.assertIn("EXTRA_JVM_OPTS: ${{ inputs.jvm_opts }}", _SHARDS)


class CompileOnceTests(unittest.TestCase):
    """One compile job per OS builds the classes and the shards reuse them (#2484), so each shard stops spending a
    quarter to a third of its time compiling the same 2,766 sources."""

    def test_a_compile_job_builds_the_tests_and_uploads_the_result_named_for_its_os(self) -> None:
        job = _job(_SHARDS, "compile")
        self.assertIn("runs-on: ${{ inputs.os }}", job)
        self.assertIn("sbt -v Test/compile", job)
        self.assertNotIn("clean", job)
        self.assertIn("uses: actions/upload-artifact@v4", job)
        self.assertIn("name: compiled-${{ inputs.label }}", job)

    def test_the_compile_job_has_its_own_hang_guard_inside_its_job_limit(self) -> None:
        job = _job(_SHARDS, "compile")
        guard = int(re.search(r"HANG_MINUTES: '(\d+)'", job).group(1))
        limit = int(re.search(r"timeout-minutes: (\d+)", job).group(1))
        self.assertLess(guard, limit)
        self.assertIn("run-tests-with-hang-dump.sh sbt", job)

    def test_the_shards_wait_for_the_compile_job_and_restore_its_classes_before_testing(self) -> None:
        job = _job(_SHARDS, "test-shard")
        self.assertIn("needs: compile", job)
        restore = job.index("actions/download-artifact@v4")
        self.assertIn("name: compiled-${{ inputs.label }}", job[restore:])
        self.assertLess(restore, job.index("Run tests"))

    def test_the_shards_run_test_without_clean_so_the_restored_classes_count(self) -> None:
        job = _job(_SHARDS, "test-shard")
        run = re.search(r"run: bash \.github/scripts/run-tests-with-hang-dump\.sh (sbt [^\n]+)", job).group(1)
        self.assertEqual("sbt -v test", run)

    def test_a_shard_that_had_to_recompile_says_so_but_still_runs(self) -> None:
        job = _job(_SHARDS, "test-shard")
        self.assertIn("compiling", job)
        self.assertIn("::warning::", job)

    def test_the_extracted_files_are_the_whole_build_output_and_nothing_else(self) -> None:
        compile_job = _job(_SHARDS, "compile")
        self.assertRegex(compile_job, r"tar [^\n]*compiled\.tar[^\n]* target project/target")
        self.assertIn("path: compiled.tar", compile_job)


if __name__ == "__main__":
    unittest.main()
