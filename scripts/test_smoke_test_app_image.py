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

_CDS_MISMATCH_INFO = """\
[0.007s][info][cds] Mismatched values for property jdk.module.enable.native.access: ALL-UNNAMED specified during runtime but not during dump time
[0.007s][info][cds] Disabling optimized module handling
"""

_CDS_MISMATCH_ERROR = """\
[0.006s][error][cds] Mismatched values for property jdk.module.enable.native.access: ALL-UNNAMED specified during runtime but not during dump time
[0.006s][error][cds] Disabling optimized module handling
"""

_APPLICATION_WARNING = "WARNING: the spell-check dictionary could not be refreshed\n"


def _run_smoke_test(launcher_output: str, only_when_cds_logged: bool = False) -> subprocess.CompletedProcess[str]:
    """Runs the script against a launcher that prints `launcher_output` then the ready line, and exits cleanly.

    With `only_when_cds_logged` the launcher prints the output only when JAVA_TOOL_OPTIONS asks for -Xlog:cds, as the
    JVM does: its info-level CDS lines are invisible unless the script requests them.
    """
    guard = '[[ "${JAVA_TOOL_OPTIONS:-}" == *-Xlog:cds* ]] && ' if only_when_cds_logged else ""
    with tempfile.TemporaryDirectory() as directory:
        launcher = Path(directory) / "Serenity"
        launcher.write_text(
            f"#!/usr/bin/env bash\n{guard}cat <<'OUTPUT'\n{launcher_output}OUTPUT\necho SERENITY_SMOKE_READY\n"
        )
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

    def test_fails_on_a_cds_error_line(self) -> None:
        result = _run_smoke_test(_CDS_MISMATCH_ERROR)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("class-data archive", result.stderr)

    def test_fails_on_an_info_level_cds_mismatch_by_asking_the_jvm_to_log_cds(self) -> None:
        result = _run_smoke_test(_CDS_MISMATCH_INFO, only_when_cds_logged=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("class-data archive", result.stderr)

    def test_passes_when_the_cds_log_is_clean(self) -> None:
        result = _run_smoke_test("[0.007s][info][cds] optimized module handling: enabled\n", only_when_cds_logged=True)
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_ignores_a_warning_line_the_application_itself_prints(self) -> None:
        result = _run_smoke_test(_APPLICATION_WARNING)
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
