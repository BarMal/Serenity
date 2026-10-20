#!/usr/bin/env python3
"""Tests for .github/scripts/summarize-jvm-diag.sh on fixtures shaped like the CI logs:

    python3 -m unittest scripts.test_summarize_jvm_diag -v
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
_SCRIPT = _SCRIPTS.parent / ".github" / "scripts" / "summarize-jvm-diag.sh"
_FIXTURES = _SCRIPTS / "fixtures" / "jvm-diag"


def _summarize(directory: Path) -> str:
    result = subprocess.run(["bash", str(_SCRIPT), str(directory)], capture_output=True, text=True, check=True)
    return result.stdout


class SummarizeJvmDiagTest(unittest.TestCase):
    def setUp(self) -> None:
        self.output = _summarize(_FIXTURES)

    def test_counts_pauses_over_thresholds(self) -> None:
        self.assertIn("6 logged, 2 over 100 ms, 1 over 1 s", self.output)

    def test_lists_longest_pause_first_with_cause_and_timestamp(self) -> None:
        rows = [line for line in self.output.splitlines() if line.startswith("| 1500.250")]
        self.assertEqual(["| 1500.250 | gc | Full (System.gc()) | 2026-10-10T12:00:04.000+0000 |"], rows)

    def test_reads_safepoint_total_in_milliseconds(self) -> None:
        self.assertIn("| 250.000 | safepoint | ICBufferFull | 2026-10-10T12:00:05.000+0000 |", self.output)

    def test_ignores_gc_lines_without_a_pause_duration(self) -> None:
        self.assertNotIn("G1 Evacuation Pause)\n", self.output)
        self.assertNotIn("Version", self.output)

    def test_lists_longest_stall_first(self) -> None:
        self.assertLess(self.output.index("| 2400 |"), self.output.index("| 900 |"))
        self.assertIn("| 2400 | 2026-10-10T12:00:05.100Z | 5100ms |", self.output)

    def test_missing_logs_are_reported_not_fatal(self) -> None:
        with tempfile.TemporaryDirectory() as empty:
            output = _summarize(Path(empty))
        self.assertIn("gc-safepoint.log.", output)
        self.assertIn("stalls.log.", output)


if __name__ == "__main__":
    unittest.main()
