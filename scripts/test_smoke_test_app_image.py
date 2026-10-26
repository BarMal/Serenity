#!/usr/bin/env python3
"""Tests for scripts/smoke-test-app-image.sh against launchers that print what a packaged JVM prints:

    python3 -m unittest scripts.test_smoke_test_app_image -v

The script must pass a clean launch and fail one where the JVM itself warns about a restricted method or a deprecated
sun.misc.Unsafe call, because that warning means the package is missing a launch option the bumped JDK expects.
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent / "smoke-test-app-image.sh"

_RESTRICTED_METHOD = """\
WARNING: A restricted method in java.lang.System has been called
WARNING: java.lang.System::load has been called by org.jetbrains.skiko.LibraryLoader in an unnamed module
WARNING: Use --enable-native-access=ALL-UNNAMED to avoid a warning for callers in this module
WARNING: Restricted methods will be blocked in a future release unless native access is enabled
"""

_UNSAFE = """\
WARNING: A terminally deprecated method in sun.misc.Unsafe has been called
WARNING: sun.misc.Unsafe::objectFieldOffset has been called by scala.runtime.LazyVals$ (file:/app/Serenity.jar)
WARNING: Please consider reporting this to the maintainers of class scala.runtime.LazyVals$
WARNING: sun.misc.Unsafe::objectFieldOffset will be removed in a future release
"""

_APPLICATION_WARNING = "WARNING: the spell-check dictionary could not be refreshed\n"


def _run_smoke_test(launcher_output: str) -> subprocess.CompletedProcess[str]:
    """Runs the script against a launcher that prints `launcher_output` then the ready line, and exits cleanly."""
    with tempfile.TemporaryDirectory() as directory:
        launcher = Path(directory) / "Serenity"
        launcher.write_text(f"#!/usr/bin/env bash\ncat <<'OUTPUT'\n{launcher_output}SERENITY_SMOKE_READY\nOUTPUT\n")
        launcher.chmod(0o755)
        return subprocess.run(
            ["bash", str(_SCRIPT), str(launcher), "30"], capture_output=True, text=True, check=False
        )


class SmokeTestAppImageTest(unittest.TestCase):
    def test_passes_a_launch_that_prints_nothing_but_the_ready_line(self) -> None:
        result = _run_smoke_test("")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_fails_when_the_jvm_warns_about_a_restricted_method(self) -> None:
        result = _run_smoke_test(_RESTRICTED_METHOD)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("JVM warning", result.stderr)

    def test_fails_when_the_jvm_warns_about_sun_misc_unsafe(self) -> None:
        result = _run_smoke_test(_UNSAFE)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("JVM warning", result.stderr)

    def test_ignores_a_warning_line_the_application_itself_prints(self) -> None:
        result = _run_smoke_test(_APPLICATION_WARNING)
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
