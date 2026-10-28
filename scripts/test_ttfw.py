#!/usr/bin/env python3
"""Tests for bench/ttfw.py against fake launchers:

    python3 -m unittest scripts.test_ttfw -v
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

_TTFW = Path(__file__).resolve().parent.parent / "bench" / "ttfw.py"


def _run(launcher_body: str, *args: str) -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory() as directory:
        launcher = Path(directory) / "Serenity"
        launcher.write_text("#!/usr/bin/env bash\n" + launcher_body)
        launcher.chmod(0o755)
        return subprocess.run(
            [sys.executable, str(_TTFW), str(launcher), *args], capture_output=True, text=True, check=False
        )


class TimeToFirstFrameTest(unittest.TestCase):
    def test_reports_the_median_first_frame_of_the_timed_runs(self) -> None:
        result = _run("sleep 0.2\necho SERENITY_SMOKE_READY\n", "3", "arm")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"arm\s+first frame: median \d+ ms.*\(n=3\)")

    def test_discards_the_warm_up_runs(self) -> None:
        # Each run appends to a counter file kept beside the launcher; the first run is the slow one.
        body = 'f="$(dirname "$0")/count"\necho x >> "$f"\n[ "$(wc -l < "$f")" -eq 1 ] && sleep 1.5\necho SERENITY_SMOKE_READY\n'
        result = _run(body, "2", "arm", "1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("(n=2)", result.stdout)
        medians = [int(word) for word in result.stdout.split("median ")[1].split(" ms")[:1]]
        self.assertLess(medians[0], 1000)

    def test_fails_when_no_run_reaches_the_first_frame(self) -> None:
        result = _run("exit 1\n", "2", "arm")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no successful run", result.stdout)


if __name__ == "__main__":
    unittest.main()
