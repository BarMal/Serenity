#!/usr/bin/env python3
"""A passing run of the script tests must not print GitHub workflow commands (#2557):

    python3 -m unittest scripts.test_ci_log_is_quiet -v

Tests that call a script's `main` print its `::error::` lines to stdout, and GitHub turns every such line into an
annotation, so a green run showed fifteen red `##[error]` lines. This runs the module list ci.yml runs, in a subprocess,
and fails if any line of its output is a workflow command.
"""

from __future__ import annotations

import re
import subprocess
import sys
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()
_THIS = "scripts.test_ci_log_is_quiet"
_ANNOTATION = re.compile(r"^::(error|warning|notice)\b")


def _ci_modules() -> list[str]:
    """The modules of the `Test the scripts` step, which lists one `scripts.test_<name> \\` per line."""
    step = re.search(r"- name: Test the scripts\n\s+run: \|\n((?:\s{10}.*\n?)+)", _CI)
    assert step is not None, "ci.yml has no multi-line `python3 -m unittest ... -v` step"
    return re.findall(r"scripts\.test_\w+", step.group(1))


class CiLogIsQuietTests(unittest.TestCase):
    def test_the_modules_ci_runs_print_no_annotations(self) -> None:
        modules = [module for module in _ci_modules() if module != _THIS]
        result = subprocess.run(
            [sys.executable, "-m", "unittest", *modules], cwd=_ROOT, capture_output=True, text=True, check=False
        )
        self.assertEqual(0, result.returncode, result.stderr[-2000:])
        lines = (result.stdout + "\n" + result.stderr).splitlines()
        self.assertEqual([], [line for line in lines if _ANNOTATION.match(line)])

    def test_ci_runs_this_module(self) -> None:
        self.assertIn(_THIS, _ci_modules())


if __name__ == "__main__":
    unittest.main()
