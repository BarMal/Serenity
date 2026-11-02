#!/usr/bin/env python3
"""Checks on the module-graph baseline growth rule (#2502):

    python3 -m unittest scripts.test_module_graph_baseline_growth -v

The baseline lists edges from a lower layer to a higher one. It may shrink freely; adding an entry needs a
`# paydown:` line in the same diff, as for the architecture baseline. A baseline that does not exist on the base
branch yet is being introduced, so it is not growth.
"""

from __future__ import annotations

import unittest
from pathlib import Path

from scripts import module_graph_baseline_growth as growth

_ROOT = Path(__file__).resolve().parent.parent
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()

_HEADER = "# Module-graph baseline.\n# Columns: from-module\tfrom-file\tto-module\tto-file\n"
_EDGE_A = "theme\tui/theme/A.scala\tapp\tstate/models/AppState.scala\n"
_EDGE_B = "markdown\tmarkdown/B.scala\tapp\tconfig/DisplayEnums.scala\n"


class EntriesTests(unittest.TestCase):
    def test_comments_and_blank_lines_are_not_entries(self) -> None:
        self.assertEqual(growth.entries(_HEADER + "\n" + _EDGE_A), {_EDGE_A.rstrip("\n")})


class RequiresJustificationTests(unittest.TestCase):
    def test_an_added_entry_without_a_paydown_line_is_rejected(self) -> None:
        self.assertTrue(growth.needs_justification(_HEADER + _EDGE_A, _HEADER + _EDGE_A + _EDGE_B, "+" + _EDGE_B))

    def test_an_added_entry_with_a_paydown_line_is_accepted(self) -> None:
        diff = "+# paydown: moved ChapterOpenings, #2502\n+" + _EDGE_B
        self.assertFalse(growth.needs_justification(_HEADER + _EDGE_A, _HEADER + _EDGE_A + _EDGE_B, diff))

    def test_removing_an_entry_needs_no_justification(self) -> None:
        self.assertFalse(growth.needs_justification(_HEADER + _EDGE_A + _EDGE_B, _HEADER + _EDGE_A, "-" + _EDGE_B))

    def test_swapping_one_entry_for_another_counts_as_growth(self) -> None:
        self.assertTrue(growth.needs_justification(_HEADER + _EDGE_A, _HEADER + _EDGE_B, "-" + _EDGE_A + "+" + _EDGE_B))

    def test_a_baseline_new_on_this_branch_is_not_growth(self) -> None:
        self.assertFalse(growth.needs_justification(None, _HEADER + _EDGE_A + _EDGE_B, "+" + _EDGE_A + "+" + _EDGE_B))


class WorkflowTests(unittest.TestCase):
    def test_ci_runs_the_module_graph_check_in_its_own_job(self) -> None:
        self.assertIn("  module-graph:\n    name: Module graph ratchet\n", _CI)
        self.assertIn("sbt -v moduleGraphCheck", _CI)

    def test_ci_applies_the_growth_rule_to_the_baseline_file(self) -> None:
        self.assertIn("python3 -m scripts.module_graph_baseline_growth", _CI)
        self.assertIn("project/module-graph-baseline.tsv", _CI)

    def test_this_test_file_is_in_the_ci_unittest_list(self) -> None:
        self.assertIn("scripts.test_module_graph_baseline_growth", _CI)


if __name__ == "__main__":
    unittest.main()
