#!/usr/bin/env python3
"""Tests for .github/scripts/run-tests-with-hang-dump.sh, the guard that stops a hung test run:

    python3 -m unittest scripts.test_hang_dump_script -v

A command that finishes must release the guard's output at once. Its watchdog's `sleep` used to outlive the watchdog,
holding the step's stdout open, so a step that pipes the guard into `tee` waited out the whole limit after an 8-second
command (#2521's "Check the restored build output" took exactly 40 minutes on every shard).
"""

from __future__ import annotations

import os
import subprocess
import time
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / ".github" / "scripts" / "run-tests-with-hang-dump.sh"


def _run_through_pipe(command: str, hang_seconds: int) -> tuple[int, float, str]:
    env = dict(os.environ, HANG_SECONDS=str(hang_seconds))
    started = time.monotonic()
    result = subprocess.run(
        ["bash", "-o", "pipefail", "-c", f'bash "{_SCRIPT}" {command} | cat'],
        env=env,
        capture_output=True,
        text=True,
        timeout=hang_seconds + 30,
    )
    return result.returncode, time.monotonic() - started, result.stdout


class HangGuardTests(unittest.TestCase):
    def test_a_command_that_finishes_releases_a_piped_output_at_once(self) -> None:
        status, elapsed, out = _run_through_pipe("echo done", hang_seconds=20)
        self.assertEqual(0, status)
        self.assertIn("done", out)
        self.assertLess(elapsed, 10)

    def test_the_command_exit_status_is_kept(self) -> None:
        status, _, _ = _run_through_pipe("bash -c 'exit 3'", hang_seconds=20)
        self.assertEqual(3, status)

    def test_a_command_still_running_at_the_limit_is_stopped_with_124(self) -> None:
        status, elapsed, out = _run_through_pipe("sleep 60", hang_seconds=2)
        self.assertEqual(124, status)
        self.assertIn("Tests still running after 2s", out)
        self.assertLess(elapsed, 30)


if __name__ == "__main__":
    unittest.main()
