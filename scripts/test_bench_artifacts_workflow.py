#!/usr/bin/env python3
"""Checks on how bench-artifacts.yml builds serenity-perf.jar (#2502):

    python3 -m unittest scripts.test_bench_artifacts_workflow -v

The workflow benchmarks arbitrary refs, including ones from before the module split. A ref with an `app` project needs the
assembly settings aimed at `app`; an older ref, whose root project holds the sources, needs the original file.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_WORKFLOW = (_ROOT / ".github" / "workflows" / "bench-artifacts.yml").read_text()


class PerfAssemblyTests(unittest.TestCase):
    def test_both_layouts_have_their_own_settings_file(self) -> None:
        self.assertTrue((_ROOT / "bench" / "ci" / "perf-assembly.sbt").exists())
        self.assertTrue((_ROOT / "bench" / "ci" / "perf-assembly-app.sbt").exists())

    def test_the_app_file_aims_every_setting_at_app(self) -> None:
        text = (_ROOT / "bench" / "ci" / "perf-assembly-app.sbt").read_text()
        code = [l for l in text.splitlines() if l.strip() and not l.lstrip().startswith("//")]
        for line in code:
            if "assembly" in line and ":=" in line:
                self.assertIn('LocalProject("app")', line, line)

    def test_the_workflow_picks_the_file_and_command_by_the_ref_s_layout(self) -> None:
        self.assertRegex(_WORKFLOW, r"grep -Eq '\^lazy val app\\b' build\.sbt")
        self.assertIn("perf-assembly-app.sbt perf-assembly.sbt", _WORKFLOW)
        self.assertIn('sbt -v assembly "app/Test / assembly"', _WORKFLOW)
        self.assertIn('sbt -v assembly "Test / assembly"', _WORKFLOW)

    def test_the_workflow_still_copies_the_file_into_the_build_root_rather_than_committing_it(self) -> None:
        self.assertEqual(len(re.findall(r"cp \.\./workflow-src/bench/ci/perf-assembly(-app)?\.sbt perf-assembly\.sbt", _WORKFLOW)), 2)


class SpikeAssemblyTests(unittest.TestCase):
    """The spike jar takes its merge rules from the project that holds the sources: `root` before the split, `app` after."""

    def test_both_layouts_have_their_own_spike_settings_file(self) -> None:
        self.assertTrue((_ROOT / "bench" / "ci" / "spike-assembly.sbt").exists())
        self.assertTrue((_ROOT / "bench" / "ci" / "spike-assembly-app.sbt").exists())

    def test_each_spike_file_takes_the_merge_rules_of_its_own_project(self) -> None:
        old = (_ROOT / "bench" / "ci" / "spike-assembly.sbt").read_text()
        new = (_ROOT / "bench" / "ci" / "spike-assembly-app.sbt").read_text()
        self.assertIn('LocalProject("root") / assembly / assemblyMergeStrategy', old)
        self.assertIn('LocalProject("app") / assembly / assemblyMergeStrategy', new)
        self.assertNotIn('LocalProject("root")', new)

    def test_the_workflow_picks_the_spike_file_by_the_spike_checkout_s_layout(self) -> None:
        step = _WORKFLOW.split("name: Build serenity-spike.jar", 1)[1].split("- name:", 1)[0]
        self.assertRegex(step, r"grep -Eq '\^lazy val app\\b' build\.sbt")
        self.assertIn("spike-assembly-app.sbt spike-assembly.sbt", step)
        self.assertIn("spike-assembly.sbt spike-assembly.sbt", step)
        self.assertIn("sbt -v spike/assembly", step)


if __name__ == "__main__":
    unittest.main()
