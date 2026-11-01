#!/usr/bin/env python3
"""Tests for scripts/regenerate-base-cds-archive.sh against a fake app image whose runtime `java` behaves like the JVM:

    python3 -m unittest scripts.test_regenerate_base_cds_archive -v

jlink dumps the runtime's base class-data archive without the launcher's module options, so the JVM logs a mismatch for
every run that passes --enable-native-access. The fake java reports that mismatch until it has been asked to dump the
archive again with the option, and records how it was called.
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent / "regenerate-base-cds-archive.sh"

_JAVA = """\
#!/usr/bin/env bash
state="$(dirname "$0")/dumped"
echo "$*" >> "$(dirname "$0")/calls"
if [[ " $* " == *" -Xshare:dump "* ]]; then
  [[ "${FAKE_DUMP_FAILS:-}" == 1 ]] && { echo "dump failed"; exit 1; }
  [[ " $* " == *" --enable-native-access=ALL-UNNAMED "* ]] && touch "$state"
  exit 0
fi
if [[ " $* " == *" -version "* ]]; then
  if [[ " $* " == *" --enable-native-access=ALL-UNNAMED "* && ( ! -f "$state" || "${FAKE_STILL_MISMATCHED:-}" == 1 ) ]]; then
    echo "[0.007s][info][cds] Mismatched values for property jdk.module.enable.native.access: ALL-UNNAMED specified during runtime but not during dump time"
  fi
  echo "openjdk version fake" >&2
fi
exit 0
"""


def _make_image(directory: str, runtime_layout: str = "lib/runtime") -> Path:
    image = Path(directory) / "Serenity"
    bin_dir = image / runtime_layout / "bin"
    bin_dir.mkdir(parents=True)
    java = bin_dir / "java"
    java.write_text(_JAVA)
    java.chmod(0o755)
    return image


def _run(image: Path, **fake: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["bash", str(_SCRIPT), str(image)], capture_output=True, text=True, check=False, env={**os.environ, **fake}
    )


class RegenerateBaseCdsArchiveTest(unittest.TestCase):
    def test_dumps_the_base_archive_with_the_launchers_native_access_option(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            image = _make_image(directory)
            result = _run(image)
            self.assertEqual(result.returncode, 0, result.stderr)
            calls = (image / "lib" / "runtime" / "bin" / "calls").read_text()
            self.assertIn("-Xshare:dump", calls)
            self.assertIn("--enable-native-access=ALL-UNNAMED", calls.splitlines()[0])

    def test_finds_a_windows_style_runtime_directory(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run(_make_image(directory, runtime_layout="runtime"))
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_fails_when_the_dump_fails(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run(_make_image(directory), FAKE_DUMP_FAILS="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("dump failed", result.stderr)

    def test_fails_when_the_jvm_still_reports_a_mismatch_afterwards(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run(_make_image(directory), FAKE_STILL_MISMATCHED="1")
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Mismatched values", result.stderr)

    def test_fails_on_an_image_without_a_runtime(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            result = _run(Path(directory))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("runtime", result.stderr)


if __name__ == "__main__":
    unittest.main()
