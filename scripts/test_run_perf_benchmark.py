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
        (root / _SHARD_CLASS.parent).mkdir(parents=True)
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


if __name__ == "__main__":
    unittest.main()
