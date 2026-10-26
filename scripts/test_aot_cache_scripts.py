#!/usr/bin/env python3
"""Tests for scripts/train-aot-cache.sh and scripts/check-aot-cache.sh against a fake Linux app image:

    python3 -m unittest scripts.test_aot_cache_scripts -v

The fake launcher behaves like the JVM does with a JDK 25 AOT cache: a training run (-XX:AOTCacheOutput) writes the
cache, a later run opens it while the jar is older than the cache, and a missing or stale cache is ignored. Its
FAKE_* environment variables switch on the failure modes the scripts must catch.
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent

_LAUNCHER = """\
#!/usr/bin/env bash
appdir="$(cd "$(dirname "$0")/../lib/app" && pwd)"
cache="$appdir/serenity.aot"
options="${JAVA_TOOL_OPTIONS:-}"
if [[ "$options" =~ -XX:AOTCacheOutput=([^[:space:]]+) ]]; then
  [[ "${FAKE_WRITES_NO_CACHE:-}" == 1 ]] || echo "aot cache" > "${BASH_REMATCH[1]}"
elif [[ -f "$cache" && ! "$appdir/Serenity.jar" -nt "$cache" && "${FAKE_IGNORES_CACHE:-}" != 1 ]]; then
  [[ "$options" == *-Xlog:aot* ]] && echo "[0.010s][info][aot] Opened AOT cache $cache."
  [[ "${FAKE_ERRORS_ON_VALID:-}" == 1 ]] && echo "[0.011s][error][aot] something went wrong"
else
  [[ "${FAKE_FAILS_WITHOUT_CACHE:-}" == 1 ]] && { echo "Error occurred during initialization of VM"; exit 1; }
fi
echo SERENITY_SMOKE_READY
exit "${FAKE_EXIT:-0}"
"""

_CFG = """\
[Application]
app.classpath=$APPDIR/Serenity.jar

[JavaOptions]
java-options=-XX:+UseG1GC
java-options=-Djpackage.app-version=1.0.0
"""


def _make_image(directory: str, with_cache: bool = False) -> Path:
    image = Path(directory) / "Serenity"
    (image / "bin").mkdir(parents=True)
    (image / "lib" / "app").mkdir(parents=True)
    launcher = image / "bin" / "Serenity"
    launcher.write_text(_LAUNCHER)
    launcher.chmod(0o755)
    (image / "lib" / "app" / "Serenity.cfg").write_text(_CFG)
    jar = image / "lib" / "app" / "Serenity.jar"
    jar.write_text("jar")
    os.utime(jar, (1_000_000, 1_000_000))
    if with_cache:
        (image / "lib" / "app" / "serenity.aot").write_text("aot cache")
    return image


def _run(script: str, image: Path, **fake: str) -> subprocess.CompletedProcess[str]:
    env = {**os.environ, **fake}
    return subprocess.run(
        ["bash", str(_SCRIPTS / script), str(image), "30"], capture_output=True, text=True, check=False, env=env
    )


class TrainAotCacheTest(unittest.TestCase):
    def test_writes_the_cache_and_points_the_launcher_at_it(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            result = _run("train-aot-cache.sh", image)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((image / "lib" / "app" / "serenity.aot").is_file())
            cfg = (image / "lib" / "app" / "Serenity.cfg").read_text().splitlines()
            self.assertEqual(cfg.count("java-options=-XX:AOTCache=$APPDIR/serenity.aot"), 1)
            self.assertLess(cfg.index("[JavaOptions]"), cfg.index("java-options=-XX:AOTCache=$APPDIR/serenity.aot"))

    def test_fails_when_the_training_run_writes_no_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            result = _run("train-aot-cache.sh", image, FAKE_WRITES_NO_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("did not write", result.stderr)
            self.assertNotIn("AOTCache", (image / "lib" / "app" / "Serenity.cfg").read_text())

    def test_fails_when_the_training_run_exits_non_zero(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("train-aot-cache.sh", _make_image(directory), FAKE_EXIT="3")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("status 3", result.stderr)

    def test_refuses_an_image_that_already_names_a_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            cfg = image / "lib" / "app" / "Serenity.cfg"
            cfg.write_text(cfg.read_text() + "java-options=-XX:AOTCache=$APPDIR/serenity.aot\n")
            result = _run("train-aot-cache.sh", image)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("already", result.stderr)


class CheckAotCacheTest(unittest.TestCase):
    def test_passes_an_image_whose_cache_opens_and_whose_fallbacks_start(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory, with_cache=True))
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_fails_when_the_valid_cache_is_not_opened(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory, with_cache=True), FAKE_IGNORES_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Opened AOT cache", result.stderr)

    def test_fails_when_the_valid_cache_logs_an_aot_error(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory, with_cache=True), FAKE_ERRORS_ON_VALID="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("[error][aot]", result.stderr)

    def test_fails_when_a_missing_cache_stops_the_app_starting(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory, with_cache=True), FAKE_FAILS_WITHOUT_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("missing", result.stderr)

    def test_fails_when_there_is_no_cache_to_check(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("serenity.aot", result.stderr)

    def test_leaves_the_image_as_it_found_it(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True)
            jar = image / "lib" / "app" / "Serenity.jar"
            _run("check-aot-cache.sh", image)
            self.assertTrue((image / "lib" / "app" / "serenity.aot").is_file())
            self.assertEqual(int(jar.stat().st_mtime), 1_000_000)


if __name__ == "__main__":
    unittest.main()
