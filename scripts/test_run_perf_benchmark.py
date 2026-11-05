#!/usr/bin/env python3
"""Unit tests for .github/scripts/run-perf-benchmark.sh's shard handling:

    python3 -m unittest scripts.test_run_perf_benchmark -v

A base commit from before `BenchmarkShard` existed would read `--shard=2/2` as a benchmark name and measure nothing, so
the script passes the option only to a checkout that has the class (issue #2443, seen on the PR that introduced it).
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / ".github" / "scripts" / "run-perf-benchmark.sh"
_SHARD_CLASS = Path("src/test/scala/com/serenity/perf/BenchmarkShard.scala")


class ShardOptionTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)

    def _checkout(self, with_shards: bool) -> str:
        root = Path(self._tmp.name) / ("new" if with_shards else "old")
        (root / _SHARD_CLASS.parent).mkdir(parents=True, exist_ok=True)
        if with_shards:
            (root / _SHARD_CLASS).write_text("// the class\n")
        return str(root)

    def _option(self, checkout: str, shard: str | None) -> str:
        env = {k: v for k, v in os.environ.items() if k != "SHARD"}
        if shard is not None:
            env["SHARD"] = shard
        result = subprocess.run(
            ["bash", str(_SCRIPT), "--shard-option", checkout], env=env, capture_output=True, text=True, check=True
        )
        return result.stdout.strip()

    def test_a_checkout_with_shards_is_given_the_option(self) -> None:
        self.assertEqual(self._option(self._checkout(True), "2/2"), "--shard=2/2")

    def test_a_checkout_from_before_shards_runs_the_whole_suite(self) -> None:
        self.assertEqual(self._option(self._checkout(False), "2/2"), "")

    def test_one_shard_of_one_is_the_whole_suite_either_way(self) -> None:
        self.assertEqual(self._option(self._checkout(True), "1/1"), "")
        self.assertEqual(self._option(self._checkout(True), None), "")


class SbtProjectPrefixTests(unittest.TestCase):
    """A ref from before the module split has one root project; one after it has `app` and an aggregate root (#2502).
    The benchmark runs on both in the same job, so the sbt project to address is read from each checkout's build."""

    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)

    def _prefix(self, build_sbt: str) -> str:
        checkout = Path(self._tmp.name)
        (checkout / "build.sbt").write_text(build_sbt)
        result = subprocess.run(
            ["bash", str(_SCRIPT), "--sbt-project-prefix", str(checkout)], capture_output=True, text=True, check=True
        )
        return result.stdout.strip()

    def test_a_build_with_an_app_project_is_addressed_through_it(self) -> None:
        self.assertEqual(self._prefix('lazy val app = (project in file("modules/app"))\n'), "app/")

    def test_a_build_with_only_a_root_project_is_addressed_unqualified(self) -> None:
        self.assertEqual(self._prefix('lazy val root = (project in file("."))\n'), "")

    def test_a_comment_mentioning_app_does_not_count(self) -> None:
        self.assertEqual(self._prefix('// lazy val app = (project in file("x"))\nlazy val root = (project in file("."))\n'), "")


if __name__ == "__main__":
    unittest.main()
