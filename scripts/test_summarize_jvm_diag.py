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

    def test_counts_full_gcs_and_their_total_time(self) -> None:
        self.assertIn("Full GCs: 1, 1500.250 ms in total.", self.output)

    def test_reports_the_heap_left_after_each_collection(self) -> None:
        self.assertIn("Heap after a collection: smallest 3 MB, largest 7 MB, capacity 64 MB.", self.output)

    def test_reports_the_heap_after_full_gcs_alone(self) -> None:
        self.assertIn("Heap after a Full GC: 3 MB.", self.output)

    def test_lists_longest_stall_first(self) -> None:
        self.assertLess(self.output.index("| 2400 |"), self.output.index("| 900 |"))
        self.assertIn("| 2400 | 2026-10-10T12:00:05.100Z | 5100ms |", self.output)

    def test_no_pauses_means_no_heap_figures(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            (Path(directory) / "gc-safepoint.log").write_text("[2026-10-10T12:00:01.100+0000][1.100s][info ][gc,init] Version: 21\n")
            output = _summarize(Path(directory))
        self.assertIn("Full GCs: 0, 0.000 ms in total.", output)
        self.assertIn("Heap after a collection: none logged.", output)
        self.assertIn("Heap after a Full GC: none logged.", output)

    def test_summarises_os_memory_samples(self) -> None:
        self.assertIn("OS memory: 4 samples over 90 s.", self.output)
        self.assertIn("Lowest free memory: 8%.", self.output)
        self.assertIn("Most swap in use: 2048.00 MB.", self.output)
        self.assertIn("Swapouts: 1500 to 4200 pages (2700 during the run).", self.output)
        self.assertIn("Most memory held by the compressor: 61000 pages (953 MB at 16384 bytes a page).", self.output)

    def test_a_run_without_os_memory_samples_says_so(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            output = _summarize(Path(directory))
        self.assertIn("No OS memory samples.", output)

    def test_missing_logs_are_reported_not_fatal(self) -> None:
        with tempfile.TemporaryDirectory() as empty:
            output = _summarize(Path(empty))
        self.assertIn("gc-safepoint.log.", output)
        self.assertIn("stalls.log.", output)


if __name__ == "__main__":
    unittest.main()
