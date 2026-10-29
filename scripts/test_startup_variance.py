#!/usr/bin/env python3
"""Tests for bench/startup-variance.sh against a fake app image:

    python3 -m unittest scripts.test_startup_variance -v
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / "bench" / "startup-variance.sh"

# Logs whether the AOT cache is beside the launcher, then reports ready.
_LAUNCHER = """#!/usr/bin/env bash
image="$(cd "$(dirname "$0")/.." && pwd)"
if [ -f "$image/lib/app/serenity.aot" ]; then echo warm >> "$image/../launches"; else echo cold >> "$image/../launches"; fi
echo SERENITY_SMOKE_READY
"""


def _image(root: Path, with_cache: bool = True) -> Path:
    image = root / "Serenity"
    (image / "bin").mkdir(parents=True)
    (image / "lib" / "app").mkdir(parents=True)
    launcher = image / "bin" / "Serenity"
    launcher.write_text(_LAUNCHER)
    launcher.chmod(0o755)
    if with_cache:
        (image / "lib" / "app" / "serenity.aot").write_text("cache")
    return image


def _run(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["bash", str(_SCRIPT), *args], capture_output=True, text=True, check=False)


class StartupVarianceTest(unittest.TestCase):
    def test_times_the_same_number_of_warm_and_cold_launches(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root)
            result = _run(str(image), "2", str(root / "out"))
            self.assertEqual(result.returncode, 0, result.stderr)
            launches = (root / "launches").read_text().split()
            # 6 rounds of 2 timed runs, each preceded by one discarded warm-up run; half the rounds per mode.
            self.assertEqual(launches.count("warm"), launches.count("cold"))
            self.assertEqual(launches.count("warm"), 3 * (2 + 1))

    def test_rounds_run_in_warm_cold_cold_warm_warm_cold_order(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root)
            _run(str(image), "1", str(root / "out"))
            modes = (root / "launches").read_text().split()
            rounds = [modes[i] for i in range(0, len(modes), 2)]
            self.assertEqual(rounds, ["warm", "cold", "cold", "warm", "warm", "cold"])

    def test_puts_the_cache_back_afterwards(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root)
            _run(str(image), "1", str(root / "out"))
            self.assertEqual((image / "lib" / "app" / "serenity.aot").read_text(), "cache")

    def test_writes_the_spread_and_the_conditions(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root)
            result = _run(str(image), "2", str(root / "out"))
            self.assertEqual(result.returncode, 0, result.stderr)
            spread = (root / "out" / "spread.txt").read_text()
            self.assertRegex(spread, r"warm\s+n=6\b")
            self.assertRegex(spread, r"cold\s+n=6\b")
            self.assertTrue((root / "out" / "conditions.txt").read_text().strip())

    def test_refuses_an_image_without_a_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root, with_cache=False)
            result = _run(str(image), "1", str(root / "out"))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("serenity.aot", result.stderr)


if __name__ == "__main__":
    unittest.main()
