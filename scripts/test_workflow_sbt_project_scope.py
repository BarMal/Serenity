#!/usr/bin/env python3
"""Checks that workflows run sbt input tasks on the `app` project (#2502):

    python3 -m unittest scripts.test_workflow_sbt_project_scope -v

The root project aggregates `app` but, being an aggregate, does not run an input task such as `Test/runMain` or
`testOnly` for it. A workflow that says `sbt "Test/runMain ..."` fails once the build is split, so every one must say
`app/Test/runMain` or `app/testOnly`.

The same goes for `Test / assembly`, which the benchmark workflows add through `perf-assembly.sbt`: the root packs
nothing. A workflow may still call it unqualified for a ref from before the split, but only inside the branch that
first checks for the `app` project (`^lazy val app\\b`). A plain `assembly` is not affected: it is an ordinary task,
so the root runs `app`'s.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"
_INPUT_TASK = re.compile(r"(?<![\w/])((?:app ?/ ?)?(?:Test ?/ ?)?)(?:runMain|testOnly)\b")


_TEST_ASSEMBLY = re.compile(r"(?<![\w/])((?:app ?/ ?)?)Test ?/ ?assembly\b")
_LAYOUT_CHECK = re.compile(r"grep -Eq '\^lazy val app\\b'")


def unscoped_input_tasks(workflow: str) -> list[str]:
    lines = (line.strip() for line in workflow.splitlines())
    sbt_commands = (line for line in lines if "sbt" in line and not line.startswith("#"))
    return [line for line in sbt_commands if any(not m.group(1).startswith("app") for m in _INPUT_TASK.finditer(line))]


def unscoped_test_assemblies(workflow: str) -> list[str]:
    if _LAYOUT_CHECK.search(workflow):
        return []
    lines = (line.strip() for line in workflow.splitlines())
    sbt_commands = (line for line in lines if "sbt" in line and not line.startswith("#"))
    return [line for line in sbt_commands if any(not m.group(1).startswith("app") for m in _TEST_ASSEMBLY.finditer(line))]


class InputTaskScopeTests(unittest.TestCase):
    def test_the_matcher_flags_an_input_task_run_from_the_root(self) -> None:
        self.assertTrue(unscoped_input_tasks('sbt "Test/runMain com.serenity.perf.Profile"'))
        self.assertTrue(unscoped_input_tasks('sbt "Test / runMain com.serenity.perf.Profile"'))
        self.assertTrue(unscoped_input_tasks('sbt "testOnly com.serenity.SomeSpec"'))

    def test_the_matcher_accepts_an_input_task_scoped_to_app(self) -> None:
        self.assertEqual(unscoped_input_tasks('sbt "app/Test/runMain com.serenity.perf.Profile"'), [])
        self.assertEqual(unscoped_input_tasks('sbt "app / Test / runMain com.serenity.perf.Profile"'), [])
        self.assertEqual(unscoped_input_tasks('sbt -v "app/testOnly com.serenity.SomeSpec"'), [])

    def test_the_matcher_ignores_comments_and_lines_without_sbt(self) -> None:
        self.assertEqual(unscoped_input_tasks('# sbt "Test/runMain a.B" would run on the root'), [])
        self.assertEqual(unscoped_input_tasks("run: java -cp x Test/runMain"), [])

    def test_every_workflow_runs_its_input_tasks_on_app(self) -> None:
        for path in sorted(_WORKFLOWS.glob("*.yml")):
            with self.subTest(workflow=path.name):
                self.assertEqual(unscoped_input_tasks(path.read_text()), [])


class TestAssemblyScopeTests(unittest.TestCase):
    def test_the_matcher_flags_a_root_test_assembly(self) -> None:
        self.assertTrue(unscoped_test_assemblies('sbt -v "Test / assembly"'))
        self.assertTrue(unscoped_test_assemblies("(cd dir && sbt -v Test/assembly)"))

    def test_the_matcher_accepts_a_test_assembly_scoped_to_app(self) -> None:
        self.assertEqual(unscoped_test_assemblies('sbt -v assembly "app/Test / assembly"'), [])
        self.assertEqual(unscoped_test_assemblies('sbt -v "app / Test / assembly"'), [])

    def test_the_matcher_ignores_a_plain_assembly_and_comments(self) -> None:
        self.assertEqual(unscoped_test_assemblies("sbt -v clean assembly writePackageVersion"), [])
        self.assertEqual(unscoped_test_assemblies('# sbt "Test / assembly" packs the test classpath'), [])

    def test_a_workflow_that_checks_the_layout_may_keep_the_old_branch(self) -> None:
        both = """
          if grep -Eq '^lazy val app\\b' build.sbt; then
            sbt -v "app/Test / assembly"
          else
            sbt -v "Test / assembly"
          fi
        """
        self.assertEqual(unscoped_test_assemblies(both), [])
        self.assertTrue(unscoped_test_assemblies('sbt -v "Test / assembly"'))

    def test_every_workflow_aims_its_test_assembly_at_app(self) -> None:
        for path in sorted(_WORKFLOWS.glob("*.yml")):
            with self.subTest(workflow=path.name):
                self.assertEqual(unscoped_test_assemblies(path.read_text()), [])


if __name__ == "__main__":
    unittest.main()
