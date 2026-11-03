#!/usr/bin/env python3
"""Tests for bench/startup-spread.py, which pools the `runs ms:` lines that bench/ttfw.py prints:

    python3 -m unittest scripts.test_startup_spread -v
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

_SPREAD = Path(__file__).resolve().parent.parent / "bench" / "startup-spread.py"


def _run(*outputs: str) -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory() as directory:
        paths = []
        for index, text in enumerate(outputs):
            path = Path(directory) / f"round{index}.txt"
            path.write_text(text)
            paths.append(str(path))
        return subprocess.run([sys.executable, str(_SPREAD), *paths], capture_output=True, text=True, check=False)


def _ttfw(label: str, *runs: int) -> str:
    return (
        f"TTFW {label:<34s} first frame: median 0 ms  min 0  max 0  (n={len(runs)})\n"
        f"TTFW {label:<34s} process exit: median 0 ms  min 0  max 0  (n={len(runs)})\n"
        f"TTFW {label:<34s} runs ms: {' '.join(str(r) for r in runs)}\n"
    )


class StartupSpreadTest(unittest.TestCase):
    def test_pools_the_runs_of_a_label_across_files(self) -> None:
        result = _run(_ttfw("warm", 100, 100), _ttfw("cold", 300), _ttfw("warm", 100, 100))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"warm\s+n=4\b")
        self.assertRegex(result.stdout, r"cold\s+n=1\b")

    def test_reports_median_and_coefficient_of_variation(self) -> None:
        result = _run(_ttfw("warm", 90, 100, 110))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"median 100 ms")
        # population standard deviation of 90, 100, 110 is 8.16; 8.16 / 100 = 8.2%
        self.assertRegex(result.stdout, r"cv 8\.2%")

    def test_reports_the_slowest_over_fastest_ratio(self) -> None:
        result = _run(_ttfw("warm", 100, 250))
        self.assertRegex(result.stdout, r"max/min 2\.50x")

    def test_ignores_lines_that_are_not_runs_lines(self) -> None:
        result = _run("noise\n" + _ttfw("warm", 100, 120) + "more noise\n")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"warm\s+n=2\b")

    def test_fails_when_no_runs_were_found(self) -> None:
        result = _run("TTFW warm first frame: no successful run\n")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no runs", result.stderr)


if __name__ == "__main__":
    unittest.main()
