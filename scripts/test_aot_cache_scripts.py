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
here="$(cd "$(dirname "$0")" && pwd)"
if [[ -d "$here/app" ]]; then appdir="$here/app"
elif [[ -d "$here/../lib/app" ]]; then appdir="$here/../lib/app"
else appdir="$here/../app"; fi
appdir="$(cd "$appdir" && pwd)"
cache="$appdir/serenity.aot"
options="${JAVA_TOOL_OPTIONS:-}"
[[ "${FAKE_HANGS:-}" == 1 ]] && sleep 30
if [[ "$options" =~ -XX:AOTCacheOutput=([^[:space:]]+) ]]; then
  if [[ "${FAKE_NO_JAVA_BINARY:-}" == 1 ]]; then
    echo "[error][aot] java.io.IOException: Cannot run program runtime/Contents/Home/bin/java: Exec failed, error: 2"
    exit 1
  fi
  [[ "${FAKE_WRITES_NO_CACHE:-}" == 1 ]] || echo "aot cache" > "${BASH_REMATCH[1]}"
elif [[ "$options" =~ -XX:AOTMode=record ]]; then
  [[ "$options" =~ -XX:AOTConfiguration=([^[:space:]]+) ]] && echo "recorded" > "${BASH_REMATCH[1]}"
  [[ "${FAKE_RECORD_FAILS:-}" == 1 ]] && exit 4
elif [[ "$options" =~ -XX:AOTMode=create ]]; then
  [[ "$options" =~ -XX:AOTConfiguration=([^[:space:]]+) ]] && [[ -f "${BASH_REMATCH[1]}" ]] || exit 5
  [[ "$options" =~ -XX:AOTCache=([^[:space:]]+) ]] || exit 6
  [[ "${FAKE_WRITES_NO_CACHE:-}" == 1 ]] || echo "aot cache" > "${BASH_REMATCH[1]}"
  exit 0
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


def _make_image(directory: str, with_cache: bool = False, windows: bool = False, macos: bool = False) -> Path:
    """A Linux image (bin/Serenity, lib/app), a Windows one (Serenity.exe, app) or a macOS bundle (Contents/MacOS/Serenity,
    Contents/app)."""
    if macos:
        image = Path(directory) / "Serenity.app"
        appdir = image / "Contents" / "app"
        launcher = image / "Contents" / "MacOS" / "Serenity"
    elif windows:
        image = Path(directory) / "Serenity"
        appdir = image / "app"
        launcher = image / "Serenity.exe"
    else:
        image = Path(directory) / "Serenity"
        appdir = image / "lib" / "app"
        launcher = image / "bin" / "Serenity"
    appdir.mkdir(parents=True)
    launcher.parent.mkdir(parents=True, exist_ok=True)
    launcher.write_text(_LAUNCHER)
    launcher.chmod(0o755)
    (appdir / "Serenity.cfg").write_text(_CFG)
    jar = appdir / "Serenity.jar"
    jar.write_text("jar")
    os.utime(jar, (1_000_000, 1_000_000))
    if with_cache:
        (appdir / "serenity.aot").write_text("aot cache")
    return image


def _appdir(image: Path) -> Path:
    if image.suffix == ".app":
        return image / "Contents" / "app"
    return image / "app" if (image / "Serenity.exe").exists() else image / "lib" / "app"


def _run(script: str, image: Path, **fake: str) -> subprocess.CompletedProcess[str]:
    env = {**os.environ, **fake}
    return subprocess.run(
        ["bash", str(_SCRIPTS / script), str(image), "30"], capture_output=True, text=True, check=False, env=env
    )


def _run_from_parent(script: str, image: Path) -> subprocess.CompletedProcess[str]:
    """Runs the script the way the workflows do: from the checkout root, with the image given as a relative path."""
    return subprocess.run(
        ["bash", str(_SCRIPTS / script), image.name, "30"],
        capture_output=True, text=True, check=False, cwd=image.parent,
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

    def test_accepts_a_relative_image_path(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            result = _run_from_parent("train-aot-cache.sh", image)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((image / "lib" / "app" / "serenity.aot").is_file())

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


class WindowsLayoutTest(unittest.TestCase):
    """The Windows app image keeps its launcher at Serenity.exe and its jar and config in app/ (jpackage layout)."""

    def test_trains_a_windows_image_and_points_the_launcher_at_the_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, windows=True)
            result = _run("train-aot-cache.sh", image)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((image / "app" / "serenity.aot").is_file())
            cfg = (image / "app" / "Serenity.cfg").read_text().splitlines()
            self.assertEqual(cfg.count("java-options=-XX:AOTCache=$APPDIR/serenity.aot"), 1)

    def test_training_a_windows_image_fails_when_no_cache_is_written(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, windows=True)
            result = _run("train-aot-cache.sh", image, FAKE_WRITES_NO_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("did not write", result.stderr)
            self.assertNotIn("AOTCache", (image / "app" / "Serenity.cfg").read_text())

    def test_refuses_an_image_that_still_names_an_appcds_archive(self) -> None:
        for option in ("-XX:SharedArchiveFile=$APPDIR/serenity.jsa", "-XX:+AutoCreateSharedArchive"):
            with self.subTest(option=option), tempfile.TemporaryDirectory() as directory:
                image = _make_image(directory, windows=True)
                cfg = image / "app" / "Serenity.cfg"
                cfg.write_text(cfg.read_text() + f"java-options={option}\n")
                result = _run("train-aot-cache.sh", image)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("AppCDS", result.stderr)
                self.assertNotIn("AOTCache", cfg.read_text())

    def test_checks_a_windows_image_and_leaves_it_as_found(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True, windows=True)
            jar = image / "app" / "Serenity.jar"
            result = _run("check-aot-cache.sh", image)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((image / "app" / "serenity.aot").is_file())
            self.assertEqual(int(jar.stat().st_mtime), 1_000_000)

    def test_check_fails_a_windows_image_whose_cache_is_not_opened(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True, windows=True)
            result = _run("check-aot-cache.sh", image, FAKE_IGNORES_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Opened AOT cache", result.stderr)

    def test_an_image_with_no_launcher_is_not_an_app_image(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("train-aot-cache.sh", Path(directory))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Not an app image", result.stderr)


class MacosBundleTrainingTest(unittest.TestCase):
    """The bundled macOS runtime has no bin/java, so -XX:AOTCacheOutput cannot fork the JVM that assembles the cache:
    a bundle is trained in two steps through its launcher (record, then create). Spike: #2522 comment 6625934280."""

    def test_trains_a_bundle_in_two_steps_and_points_the_launcher_at_the_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, macos=True)
            result = _run("train-aot-cache.sh", image, FAKE_NO_JAVA_BINARY="1")
            self.assertEqual(result.returncode, 0, result.stderr)
            appdir = _appdir(image)
            self.assertTrue((appdir / "serenity.aot").is_file())
            cfg = (appdir / "Serenity.cfg").read_text().splitlines()
            self.assertEqual(cfg.count("java-options=-XX:AOTCache=$APPDIR/serenity.aot"), 1)

    def test_leaves_no_recorded_configuration_in_the_bundle(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, macos=True)
            _run("train-aot-cache.sh", image)
            self.assertEqual(sorted(p.name for p in _appdir(image).iterdir()), ["Serenity.cfg", "Serenity.jar", "serenity.aot"])

    def test_fails_when_the_recording_run_fails(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, macos=True)
            result = _run("train-aot-cache.sh", image, FAKE_RECORD_FAILS="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("status 4", result.stderr)
            self.assertNotIn("AOTCache", (_appdir(image) / "Serenity.cfg").read_text())
            self.assertFalse((_appdir(image) / "serenity.aotconf").exists())

    def test_fails_when_assembling_writes_no_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, macos=True)
            result = _run("train-aot-cache.sh", image, FAKE_WRITES_NO_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("did not write", result.stderr)
            self.assertNotIn("AOTCache", (_appdir(image) / "Serenity.cfg").read_text())

    def test_refuses_a_bundle_that_already_names_a_cache(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, macos=True)
            cfg = _appdir(image) / "Serenity.cfg"
            cfg.write_text(cfg.read_text() + "java-options=-XX:AOTCache=$APPDIR/serenity.aot\n")
            result = _run("train-aot-cache.sh", image)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("already", result.stderr)

    def test_checks_a_bundle_and_leaves_it_as_found(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True, macos=True)
            jar = _appdir(image) / "Serenity.jar"
            result = _run("check-aot-cache.sh", image)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((_appdir(image) / "serenity.aot").is_file())
            self.assertEqual(int(jar.stat().st_mtime), 1_000_000)

    def test_check_fails_a_bundle_whose_cache_is_not_opened(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True, macos=True)
            result = _run("check-aot-cache.sh", image, FAKE_IGNORES_CACHE="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Opened AOT cache", result.stderr)

    def test_a_directory_with_no_launcher_is_not_an_app_image(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("train-aot-cache.sh", Path(directory))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Not an app image", result.stderr)


class TimeLimitTest(unittest.TestCase):
    """macOS has no timeout(1), so the scripts enforce the limit themselves; the launcher here hangs for 30 s."""

    def test_training_stops_a_launcher_that_outlives_the_limit(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            result = subprocess.run(
                ["bash", str(_SCRIPTS / "train-aot-cache.sh"), str(image), "2"],
                capture_output=True, text=True, check=False, env={**os.environ, "FAKE_HANGS": "1"}, timeout=20,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("timed out after 2s", result.stderr)

    def test_checking_stops_a_launcher_that_outlives_the_limit(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory, with_cache=True)
            result = subprocess.run(
                ["bash", str(_SCRIPTS / "check-aot-cache.sh"), str(image), "2"],
                capture_output=True, text=True, check=False, env={**os.environ, "FAKE_HANGS": "1"}, timeout=20,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("timed out after 2s", result.stderr)


class CheckAotCacheTest(unittest.TestCase):
    def test_passes_an_image_whose_cache_opens_and_whose_fallbacks_start(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run("check-aot-cache.sh", _make_image(directory, with_cache=True))
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_accepts_a_relative_image_path(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run_from_parent("check-aot-cache.sh", _make_image(directory, with_cache=True))
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
