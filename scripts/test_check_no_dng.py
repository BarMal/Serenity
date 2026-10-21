#!/usr/bin/env python3
"""Tests for scripts/check_no_dng.py on synthetic natives and jars:

    python3 -m unittest scripts.test_check_no_dng -v
"""

from __future__ import annotations

import io
import tempfile
import unittest
import zipfile
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

from scripts import check_no_dng as dng

# Shaped like what the stock Linux library carries: Itanium-mangled RTTI and a message literal.
_DNG_ITANIUM = b"\x00_ZTI13dng_exception\x00_ZN10SkRawCodec14MakeFromStreamE\x00"
_DNG_LITERAL = b"\x00Overflow in dng_rect constructor\x00"
_CLEAN = b"\x00_ZN7SkCodec11MakeFromDataE\x00SkPngCodec\x00SkJpegCodec\x00ordinary text\x00"


def _jar(entries: dict[str, bytes]) -> bytes:
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as archive:
        for name, content in entries.items():
            archive.writestr(name, content)
    return buffer.getvalue()


class ScanBytesTest(unittest.TestCase):
    def test_finds_dng_class_names(self) -> None:
        self.assertIn("dng_", dng.scan_bytes(_DNG_ITANIUM))

    def test_finds_the_raw_codec(self) -> None:
        self.assertIn("SkRawCodec", dng.scan_bytes(_DNG_ITANIUM))

    def test_finds_a_message_literal(self) -> None:
        self.assertEqual(["dng_"], dng.scan_bytes(_DNG_LITERAL))

    def test_finds_piex(self) -> None:
        self.assertEqual(["piex"], dng.scan_bytes(b"\x00third_party/externals/piex/src\x00"))

    def test_finds_utf16_strings_as_a_windows_binary_stores_them(self) -> None:
        self.assertEqual(["dng_"], dng.scan_bytes("dng_negative".encode("utf-16-le")))

    def test_a_clean_library_has_no_markers(self) -> None:
        self.assertEqual([], dng.scan_bytes(_CLEAN))

    def test_other_words_that_end_in_dng_are_not_markers(self) -> None:
        self.assertEqual([], dng.scan_bytes(b"padding_ ending_ wedding_ binding_"))

    def test_lists_each_marker_once_in_a_fixed_order(self) -> None:
        self.assertEqual(["dng_", "SkRawCodec", "piex"], dng.scan_bytes(_DNG_ITANIUM + b"piex"))


class ScanPathTest(unittest.TestCase):
    def setUp(self) -> None:
        self._dir = tempfile.TemporaryDirectory()
        self.addCleanup(self._dir.cleanup)
        self.dir = Path(self._dir.name)

    def _write(self, name: str, content: bytes) -> Path:
        path = self.dir / name
        path.write_bytes(content)
        return path

    def test_scans_a_plain_library(self) -> None:
        path = self._write("libskiko-linux-x64.so", _DNG_ITANIUM)
        self.assertEqual({path.name: ["dng_", "SkRawCodec"]}, dng.scan_path(path))

    def test_scans_every_entry_of_a_jar_by_entry_name(self) -> None:
        jar = self._write(
            "skiko-awt-runtime-macos-arm64.jar",
            _jar(
                {
                    "libskiko-macos-arm64.dylib": _DNG_ITANIUM,
                    "libskiko-macos-x64.dylib": _CLEAN,
                    "META-INF/MANIFEST.MF": b"Manifest-Version: 1.0\n",
                }
            ),
        )
        self.assertEqual({"libskiko-macos-arm64.dylib": ["dng_", "SkRawCodec"]}, dng.scan_path(jar))

    def test_a_clean_jar_reports_nothing(self) -> None:
        jar = self._write("clean.jar", _jar({"skiko-windows-x64.dll": _CLEAN, "icudtl.dat": b"\x00\x01"}))
        self.assertEqual({}, dng.scan_path(jar))


class MainTest(unittest.TestCase):
    def setUp(self) -> None:
        self._dir = tempfile.TemporaryDirectory()
        self.addCleanup(self._dir.cleanup)
        self.dir = Path(self._dir.name)
        self.clean = self.dir / "clean.so"
        self.clean.write_bytes(_CLEAN)
        self.dirty = self.dir / "dirty.so"
        self.dirty.write_bytes(_DNG_ITANIUM)

    def _run(self, *argv: str) -> tuple[int, str]:
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            code = dng.main(list(argv))
        return code, out.getvalue() + err.getvalue()

    def test_passes_on_a_clean_library(self) -> None:
        code, output = self._run(str(self.clean))
        self.assertEqual(0, code)
        self.assertIn("clean.so", output)

    def test_fails_and_names_the_library_and_markers(self) -> None:
        code, output = self._run(str(self.clean), str(self.dirty))
        self.assertEqual(1, code)
        self.assertIn("dirty.so", output)
        self.assertIn("SkRawCodec", output)

    def test_expect_dng_passes_when_the_scanner_sees_it(self) -> None:
        self.assertEqual(0, self._run("--expect-dng", str(self.dirty))[0])

    def test_expect_dng_fails_when_the_scanner_sees_nothing(self) -> None:
        code, output = self._run("--expect-dng", str(self.clean))
        self.assertEqual(1, code)
        self.assertIn("scanner", output)

    def test_a_missing_path_is_an_error_not_a_pass(self) -> None:
        self.assertEqual(2, self._run(str(self.dir / "absent.so"))[0])

    def test_no_paths_is_an_error_not_a_pass(self) -> None:
        self.assertEqual(2, self._run()[0])


if __name__ == "__main__":
    unittest.main()
