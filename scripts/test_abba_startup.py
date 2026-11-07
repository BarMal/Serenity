#!/usr/bin/env python3
"""Tests for bench/abba-startup.sh against fake app images:

    python3 -m unittest scripts.test_abba_startup -v
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / "bench" / "abba-startup.sh"

_LAUNCHER = """#!/usr/bin/env bash
image="$(cd "$(dirname "$0")/.." && pwd)"
echo {label} >> "$image/../../order"
sleep {sleep}
echo SERENITY_SMOKE_READY
"""


def _image(root: Path, label: str, sleep: float) -> Path:
    image = root / label / "Serenity"
    (image / "bin").mkdir(parents=True)
    (image / "lib" / "app").mkdir(parents=True)
    (image / "lib" / "app" / "serenity.aot").write_text("cache")
    launcher = image / "bin" / "Serenity"
    launcher.write_text(_LAUNCHER.format(label=label, sleep=sleep))
    launcher.chmod(0o755)
    return image / "bin" / "Serenity"


def _run(*args: str, env: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["bash", str(_SCRIPT), *args], capture_output=True, text=True, check=False, env=env)


class AbbaStartupTest(unittest.TestCase):
    def test_runs_the_launchers_in_a_b_b_a_a_b_b_a_order(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            a, b = _image(root, "a", 0.01), _image(root, "b", 0.01)
            _run(str(a), str(b), str(root / "out"), "1")
            launches = (root / "order").read_text().split()
            # One run of startup.py with 1 timed run and the default 1 discarded one launches warm and cold twice each.
            groups = [launches[i : i + 4] for i in range(0, len(launches), 4)]
            self.assertTrue(all(len(set(group)) == 1 for group in groups), groups)
            self.assertEqual([group[0] for group in groups], ["a", "b", "b", "a", "a", "b", "b", "a"])

    def test_writes_four_csvs_per_side(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            a, b = _image(root, "a", 0.01), _image(root, "b", 0.01)
            _run(str(a), str(b), str(root / "out"), "1")
            names = sorted(path.name for path in (root / "out").glob("*.csv"))
            self.assertEqual(names, [f"{side}-{n}.csv" for side in "ab" for n in range(1, 5)])

    def test_identical_builds_do_not_block(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            a, b = _image(root, "a", 0.05), _image(root, "b", 0.05)
            result = _run(str(a), str(b), str(root / "out"), "2")
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_a_much_slower_candidate_blocks(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            a, b = _image(root, "a", 0.05), _image(root, "b", 0.6)
            result = _run(str(a), str(b), str(root / "out"), "2")
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertIn("startup.warm.first_frame", result.stdout)

    def test_report_only_never_fails_even_when_the_candidate_is_much_slower(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            a, b = _image(root, "a", 0.05), _image(root, "b", 0.6)
            env = {**os.environ, "ABBA_STARTUP_REPORT_ONLY": "1"}
            result = _run(str(a), str(b), str(root / "out"), "2", env=env)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("::warning::startup.warm.first_frame", result.stdout)
            self.assertNotIn("::error::", result.stdout + result.stderr)

    def test_report_only_turns_a_missing_launcher_into_a_warning(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            b = _image(root, "b", 0.01)
            env = {**os.environ, "ABBA_STARTUP_REPORT_ONLY": "1"}
            result = _run(str(root / "nope"), str(b), str(root / "out"), "1", env=env)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("::warning::", result.stdout + result.stderr)

    def test_missing_launcher_fails_before_running_anything(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            b = _image(root, "b", 0.01)
            result = _run(str(root / "nope"), str(b), str(root / "out"), "1")
            self.assertNotEqual(result.returncode, 0)
            self.assertFalse((root / "order").exists())


if __name__ == "__main__":
    unittest.main()
