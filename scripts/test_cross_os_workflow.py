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
                self.assertIn("    if: always()\n", job + "\n")

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
        self.assertEqual(["shards-windows", "shards-macos", "test-windows", "test-macos"], jobs)


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
