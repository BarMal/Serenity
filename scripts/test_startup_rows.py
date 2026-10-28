#!/usr/bin/env python3
"""Tests for bench/startup.py, which writes the startup.{cold,warm}.first_frame rows in the PerformanceBenchmarks CSV
layout, against a fake app image:

    python3 -m unittest scripts.test_startup_rows -v
"""

from __future__ import annotations

import importlib.util
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_SCRIPTS = _ROOT / "scripts"
_STARTUP = _ROOT / "bench" / "startup.py"
sys.path.insert(0, str(_SCRIPTS))

import check_perf_regression  # noqa: E402

# Logs whether the AOT cache is beside the launcher; starting without it takes longer, as a real image does.
_LAUNCHER = """#!/usr/bin/env bash
image="$(cd "$(dirname "$0")/.." && pwd)"
if [ -f "$image/lib/app/serenity.aot" ]; then echo warm >> "$image/../launches"; else echo cold >> "$image/../launches"; sleep {cold_sleep}; fi
{ready}
"""


def _image(root: Path, with_cache: bool = True, ready: bool = True, cold_sleep: float = 0.4) -> Path:
    image = root / "Serenity"
    (image / "bin").mkdir(parents=True)
    (image / "lib" / "app").mkdir(parents=True)
    launcher = image / "bin" / "Serenity"
    launcher.write_text(_LAUNCHER.format(cold_sleep=cold_sleep, ready="echo SERENITY_SMOKE_READY" if ready else "true"))
    launcher.chmod(0o755)
    if with_cache:
        (image / "lib" / "app" / "serenity.aot").write_text("cache")
    return image


def _run(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run([sys.executable, str(_STARTUP), *args], capture_output=True, text=True, check=False)


def _load_module():
    spec = importlib.util.spec_from_file_location("startup", _STARTUP)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(_ROOT / "bench"))
    spec.loader.exec_module(module)
    return module


class StartupRowsTest(unittest.TestCase):
    def test_writes_a_cold_and_a_warm_row_the_perf_scripts_can_parse(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            csv = root / "startup.csv"
            result = _run(str(_image(root) / "bin" / "Serenity"), "3", "--csv", str(csv))
            self.assertEqual(result.returncode, 0, result.stderr)
            rows = check_perf_regression.parse_csv(str(csv))
            self.assertEqual(sorted(rows), ["startup.cold.first_frame", "startup.warm.first_frame"])
            self.assertGreater(rows["startup.cold.first_frame"].p50_ms, rows["startup.warm.first_frame"].p50_ms)

    def test_header_matches_the_one_the_benchmark_runner_prints(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            csv = root / "startup.csv"
            _run(str(_image(root) / "bin" / "Serenity"), "1", "--csv", str(csv), "--discard", "0")
            header = next(line for line in csv.read_text().splitlines() if line.startswith("name,"))
            source = (_ROOT / "src/test/scala/com/serenity/perf/BenchmarkRunner.scala").read_text()
            joined = re.sub(r'"\s*\+\s*"', "", source)
            self.assertIn(header, joined)
            self.assertEqual(
                header,
                "name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,"
                "allocation_p50_bytes,allocation_p95_bytes,retained_bytes",
            )

    def test_counts_every_timed_run_and_records_the_discarded_ones(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            csv = root / "startup.csv"
            result = _run(str(_image(root) / "bin" / "Serenity"), "3", "--csv", str(csv), "--discard", "2")
            self.assertEqual(result.returncode, 0, result.stderr)
            launches = (root / "launches").read_text().split()
            self.assertEqual(launches.count("warm"), 5)
            self.assertEqual(launches.count("cold"), 5)
            text = csv.read_text()
            self.assertIn("context,discarded_runs,2", text)
            warm = next(line for line in text.splitlines() if line.startswith("startup.warm.first_frame,"))
            self.assertEqual(warm.split(",")[1:4], ["3", "2", "1"])

    def test_alternates_warm_and_cold_launches(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _run(str(_image(root) / "bin" / "Serenity"), "3", "--discard", "0", "--csv", str(root / "s.csv"))
            self.assertEqual((root / "launches").read_text().split(), ["warm", "cold"] * 3)

    def test_puts_the_cache_back(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            image = _image(root)
            _run(str(image / "bin" / "Serenity"), "2", "--csv", str(root / "s.csv"))
            self.assertEqual((image / "lib" / "app" / "serenity.aot").read_text(), "cache")

    def test_refuses_an_image_without_a_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            result = _run(str(_image(root, with_cache=False) / "bin" / "Serenity"), "1", "--csv", str(root / "s.csv"))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("serenity.aot", result.stderr)
            self.assertFalse((root / "s.csv").exists())

    def test_fails_without_writing_rows_when_no_launch_reaches_a_first_frame(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            csv = root / "s.csv"
            result = _run(str(_image(root, ready=False) / "bin" / "Serenity"), "1", "--discard", "0", "--csv", str(csv))
            self.assertNotEqual(result.returncode, 0)
            self.assertFalse(csv.exists())

    def test_percentiles_use_the_benchmark_runners_nearest_rank(self) -> None:
        startup = _load_module()
        samples = [100.0, 200.0, 300.0, 400.0]
        self.assertEqual(startup.percentile(samples, 0.50), 200.0)
        self.assertEqual(startup.percentile(samples, 0.95), 400.0)


if __name__ == "__main__":
    unittest.main()
