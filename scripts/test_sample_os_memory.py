#!/usr/bin/env python3
"""Tests for .github/scripts/sample-os-memory.sh, which records the macOS runner's memory state during a test run
(#2484), run against stand-ins for vm_stat, sysctl and memory_pressure that print what macOS prints:

    python3 -m unittest scripts.test_sample_os_memory -v
"""

from __future__ import annotations

import shutil
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / ".github" / "scripts" / "sample-os-memory.sh"

_VM_STAT = """Mach Virtual Memory Statistics: (page size of 16384 bytes)
Pages free:                               12345.
Pages active:                            200000.
Pages inactive:                          150000.
Pages speculative:                         1000.
Pages occupied by compressor:             54321.
Swapins:                                    700.
Swapouts:                                  1500.
"""
_SWAP = "vm.swapusage: total = 2048.00M  used = 1024.50M  free = 1023.50M  (encrypted)\n"
_PRESSURE = "The system has 7516192768 (458752 pages with a page size of 16384).\nSystem-wide memory free percentage: 41%\n"


def _executable(directory: Path, name: str, output: str) -> None:
    path = directory / name
    path.write_text(f"#!/bin/sh\ncat <<'EOF'\n{output}EOF\n")
    path.chmod(path.stat().st_mode | stat.S_IXUSR)


_BASICS = ("awk", "cat", "date", "mkdir", "sleep")


def _run(arguments: list[str], path: list[Path]) -> subprocess.CompletedProcess[str]:
    environment = {"PATH": ":".join(str(directory) for directory in path)}
    bash = shutil.which("bash")
    assert bash is not None
    return subprocess.run([bash, str(_SCRIPT), *arguments], capture_output=True, text=True, env=environment, check=True)


class SampleOsMemoryTest(unittest.TestCase):
    def setUp(self) -> None:
        self._temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self._temporary.cleanup)
        self.root = Path(self._temporary.name)
        self.basics = self.root / "basics"
        self.basics.mkdir()
        for name in _BASICS:
            (self.basics / name).symlink_to(shutil.which(name) or "")
        self.tools = self.root / "tools"
        self.tools.mkdir()
        _executable(self.tools, "vm_stat", _VM_STAT)
        _executable(self.tools, "sysctl", _SWAP)
        _executable(self.tools, "memory_pressure", _PRESSURE)

    def test_one_sample_is_a_tab_separated_record_of_the_figures_that_matter(self) -> None:
        fields = _run(["once"], [self.tools, self.basics]).stdout.rstrip("\n").split("\t")
        self.assertEqual(7, len(fields))
        self.assertRegex(fields[0], r"^\d{10,}$")
        self.assertEqual(["16384", "12345", "54321", "1500", "1024.50", "41"], fields[1:])

    def test_a_machine_without_the_macos_tools_records_nothing_and_does_not_fail(self) -> None:
        result = _run(["once"], [self.basics])
        self.assertEqual("", result.stdout)

    def test_the_loop_appends_the_requested_number_of_samples_to_os_mem_log(self) -> None:
        directory = self.root / "diag"
        _run(["loop", str(directory), "0", "3"], [self.tools, self.basics])
        lines = (directory / "os-mem.log").read_text().splitlines()
        self.assertEqual(3, len(lines))
        self.assertTrue(all(len(line.split("\t")) == 7 for line in lines))

    def test_the_loop_on_a_machine_without_the_tools_creates_no_file(self) -> None:
        directory = self.root / "diag"
        _run(["loop", str(directory), "0", "3"], [self.basics])
        self.assertFalse((directory / "os-mem.log").exists())


if __name__ == "__main__":
    unittest.main()
