#!/usr/bin/env python3
"""Checks that workflows run sbt input tasks on the `app` project (#2502):

    python3 -m unittest scripts.test_workflow_sbt_project_scope -v

The root project aggregates `app` but, being an aggregate, does not run an input task such as `Test/runMain` or
`testOnly` for it. A workflow that says `sbt "Test/runMain ..."` fails once the build is split, so every one must say
`app/Test/runMain` or `app/testOnly`.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"
_INPUT_TASK = re.compile(r"(?<![\w/])((?:app ?/ ?)?(?:Test ?/ ?)?)(?:runMain|testOnly)\b")


def unscoped_input_tasks(workflow: str) -> list[str]:
    lines = (line.strip() for line in workflow.splitlines())
    sbt_commands = (line for line in lines if "sbt" in line and not line.startswith("#"))
    return [line for line in sbt_commands if any(not m.group(1).startswith("app") for m in _INPUT_TASK.finditer(line))]


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


if __name__ == "__main__":
    unittest.main()
