#!/usr/bin/env python3
"""Tests for .github/scripts/summarize-test-durations.sh on fixtures shaped like the JUnit reports sbt writes:

    python3 -m unittest scripts.test_summarize_test_durations -v
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
_SCRIPT = _SCRIPTS.parent / ".github" / "scripts" / "summarize-test-durations.sh"
_FIXTURES = _SCRIPTS / "fixtures" / "test-reports"


def _summarize(directory: Path, *count: str) -> str:
    result = subprocess.run(
        ["bash", str(_SCRIPT), str(directory), *count], capture_output=True, text=True, check=True
    )
    return result.stdout


class SummarizeTestDurationsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.output = _summarize(_FIXTURES)
        self.rows = [line for line in self.output.splitlines() if line.startswith("| ") and "Spec" in line]

    def test_lists_the_slowest_suite_first(self) -> None:
        self.assertEqual(
            [
                "| 12.500 | 3 | com.example.SlowSpec |",
                "| 3.125 | 7 | com.example.MiddleSpec |",
                "| 0.250 | 40 | com.example.FastSpec |",
            ],
            self.rows,
        )

    def test_reads_the_suite_time_not_a_test_case_time(self) -> None:
        self.assertNotIn("9999", self.output)

    def test_reads_the_suite_name_not_the_hostname(self) -> None:
        self.assertIn("com.example.MiddleSpec", self.output)
        self.assertNotIn("name-host", self.output)

    def test_reads_attributes_wrapped_over_several_lines(self) -> None:
        self.assertIn("| 3.125 | 7 | com.example.MiddleSpec |", self.output)

    def test_reports_the_suite_count_and_summed_time(self) -> None:
        self.assertIn("3 suites, 15.875 s in total", self.output)

    def test_keeps_only_the_requested_number_of_suites(self) -> None:
        rows = [line for line in _summarize(_FIXTURES, "2").splitlines() if "Spec |" in line]
        self.assertEqual(["| 12.500 | 3 | com.example.SlowSpec |", "| 3.125 | 7 | com.example.MiddleSpec |"], rows)

    def test_missing_reports_are_reported_not_fatal(self) -> None:
        with tempfile.TemporaryDirectory() as empty:
            output = _summarize(Path(empty))
        self.assertIn("No JUnit reports", output)


if __name__ == "__main__":
    unittest.main()
