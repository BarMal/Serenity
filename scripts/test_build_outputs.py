#!/usr/bin/env python3
"""Checks on the before/after build-output comparison used by the #2502 module split:

    python3 -m unittest scripts.test_build_outputs -v

A refactor of the build must leave the shipped artifacts alone: the same entries in Serenity.jar and
serenity-perf.jar (names, sizes, CRCs), the same manifest, and the same generated licence files.
"""

from __future__ import annotations

import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts import build_outputs


def _jar(path: Path, entries: dict[str, bytes]) -> Path:
    with zipfile.ZipFile(path, "w") as jar:
        for name, data in entries.items():
            jar.writestr(zipfile.ZipInfo(name, date_time=(2020, 1, 1, 0, 0, 0)), data)
    return path


class JarEntriesTests(unittest.TestCase):
    def test_entries_are_sorted_name_size_crc_lines(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            jar = _jar(Path(tmp) / "a.jar", {"b.txt": b"bb", "a.txt": b"a"})
            lines = build_outputs.jar_entries(jar)
            self.assertEqual([line.split("\t")[0] for line in lines], ["a.txt", "b.txt"])
            self.assertEqual(lines[0].split("\t")[1], "1")

    def test_timestamps_do_not_matter(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            first = _jar(Path(tmp) / "a.jar", {"x": b"1"})
            second = Path(tmp) / "b.jar"
            with zipfile.ZipFile(second, "w") as jar:
                jar.writestr(zipfile.ZipInfo("x", date_time=(2024, 5, 5, 5, 5, 5)), b"1")
            self.assertEqual(build_outputs.jar_entries(first), build_outputs.jar_entries(second))


class CompareTests(unittest.TestCase):
    def _capture(self, tmp: str, name: str, entries: dict[str, bytes], text: str = "notices") -> Path:
        jars = Path(tmp) / f"{name}-jars"
        jars.mkdir()
        jar = _jar(jars / "Serenity.jar", entries)
        files = Path(tmp) / f"{name}-files"
        files.mkdir()
        notice = files / "notices.md"
        notice.write_text(text)
        out = Path(tmp) / name
        build_outputs.capture(out, jars=[jar], files=[notice])
        return out

    def test_identical_outputs_compare_clean(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            before = self._capture(tmp, "before", {"a": b"1"})
            after = self._capture(tmp, "after", {"a": b"1"})
            self.assertEqual(build_outputs.compare(before, after), [])

    def test_a_missing_entry_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            before = self._capture(tmp, "before", {"a": b"1", "b": b"2"})
            after = self._capture(tmp, "after", {"a": b"1"})
            problems = build_outputs.compare(before, after)
            self.assertEqual(len(problems), 1)
            self.assertIn("Serenity.jar", problems[0])
            self.assertIn("b\t", problems[0])

    def test_a_changed_entry_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            before = self._capture(tmp, "before", {"a": b"1"})
            after = self._capture(tmp, "after", {"a": b"2"})
            self.assertTrue(build_outputs.compare(before, after))

    def test_a_changed_licence_file_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            before = self._capture(tmp, "before", {"a": b"1"}, text="old")
            after = self._capture(tmp, "after", {"a": b"1"}, text="new")
            problems = build_outputs.compare(before, after)
            self.assertEqual(len(problems), 1)
            self.assertIn("notices.md", problems[0])


if __name__ == "__main__":
    unittest.main()
