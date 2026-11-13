#!/usr/bin/env python3
"""Unit tests for carry_checksums.py (the nightly's SHA256SUMS while Windows and macOS are off, #2618):

    python3 -m unittest scripts.test_carry_checksums -v
"""

from __future__ import annotations

import importlib.util
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stderr
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent


def _load(name: str):
    spec = importlib.util.spec_from_file_location(name, _SCRIPTS / f"{name}.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


carry = _load("carry_checksums")

_FRESH = f"{'a' * 64}  Serenity-linux-x64.jar\n{'b' * 64}  Serenity-linux-x64.zip\n{'c' * 64}  Serenity.cdx.json\n"
_WIN_JAR, _WIN_ZIP, _MAC_JAR, _MAC_ZIP = ("1" * 64, "2" * 64, "3" * 64, "4" * 64)
_PREVIOUS = (
    f"{'0' * 64}  Serenity-linux-x64.jar\n"
    f"{_WIN_JAR}  Serenity-windows-x64.jar\n{_MAC_JAR}  Serenity-macos-arm64.jar\n"
    f"{_WIN_ZIP}  Serenity-windows-x64.zip\n{_MAC_ZIP}  Serenity-macos-arm64.zip\n"
)


def _attached(**digests: str | None) -> dict[str, str | None]:
    names = {
        "win_jar": "Serenity-windows-x64.jar",
        "win_zip": "Serenity-windows-x64.zip",
        "mac_jar": "Serenity-macos-arm64.jar",
        "mac_zip": "Serenity-macos-arm64.zip",
    }
    return {names[key]: digest for key, digest in digests.items()}


def _all_attached() -> dict[str, str | None]:
    return _attached(
        win_jar=f"sha256:{_WIN_JAR}", win_zip=f"sha256:{_WIN_ZIP}", mac_jar=f"sha256:{_MAC_JAR}", mac_zip=f"sha256:{_MAC_ZIP}"
    )


class MergeTests(unittest.TestCase):
    def test_the_old_windows_and_macos_lines_follow_the_fresh_linux_and_sbom_lines(self) -> None:
        text, warnings = carry.merge(_FRESH, _PREVIOUS, _all_attached())
        self.assertEqual(
            _FRESH
            + f"{_WIN_JAR}  Serenity-windows-x64.jar\n{_WIN_ZIP}  Serenity-windows-x64.zip\n"
            + f"{_MAC_JAR}  Serenity-macos-arm64.jar\n{_MAC_ZIP}  Serenity-macos-arm64.zip\n",
            text,
        )
        self.assertEqual([], warnings)

    def test_the_previous_linux_lines_are_not_carried(self) -> None:
        text, _ = carry.merge(_FRESH, _PREVIOUS, _all_attached())
        self.assertEqual(1, text.count("Serenity-linux-x64.jar"))
        self.assertNotIn("0" * 64, text)

    def test_an_asset_that_is_no_longer_attached_is_left_out(self) -> None:
        text, warnings = carry.merge(_FRESH, _PREVIOUS, _attached(win_jar=f"sha256:{_WIN_JAR}"))
        self.assertEqual(_FRESH + f"{_WIN_JAR}  Serenity-windows-x64.jar\n", text)
        self.assertEqual([], warnings)

    def test_a_missing_previous_file_falls_back_to_the_digest_github_recorded_and_says_so(self) -> None:
        text, warnings = carry.merge(_FRESH, None, _attached(win_zip=f"sha256:{_WIN_ZIP}"))
        self.assertEqual(_FRESH + f"{_WIN_ZIP}  Serenity-windows-x64.zip\n", text)
        self.assertEqual(1, len(warnings))
        self.assertIn("Serenity-windows-x64.zip", warnings[0])

    def test_a_line_that_disagrees_with_the_attached_asset_is_replaced_by_the_recorded_digest(self) -> None:
        stale = f"{'9' * 64}  Serenity-macos-arm64.jar\n"
        text, warnings = carry.merge(_FRESH, stale, _attached(mac_jar=f"sha256:{_MAC_JAR}"))
        self.assertEqual(_FRESH + f"{_MAC_JAR}  Serenity-macos-arm64.jar\n", text)
        self.assertEqual(1, len(warnings))
        self.assertIn("does not match", warnings[0])

    def test_without_a_recorded_digest_the_previous_line_is_carried_and_flagged(self) -> None:
        text, warnings = carry.merge(_FRESH, _PREVIOUS, _attached(win_jar=None))
        self.assertEqual(_FRESH + f"{_WIN_JAR}  Serenity-windows-x64.jar\n", text)
        self.assertEqual(1, len(warnings))

    def test_an_asset_with_neither_a_line_nor_a_digest_is_left_out_and_flagged(self) -> None:
        text, warnings = carry.merge(_FRESH, None, _attached(win_jar=None))
        self.assertEqual(_FRESH, text)
        self.assertEqual(1, len(warnings))

    def test_a_binary_mode_marker_in_the_previous_file_is_read(self) -> None:
        previous = f"{_WIN_JAR} *Serenity-windows-x64.jar\n"
        text, _ = carry.merge(_FRESH, previous, _attached(win_jar=f"sha256:{_WIN_JAR}"))
        self.assertEqual(_FRESH + f"{_WIN_JAR}  Serenity-windows-x64.jar\n", text)

    def test_a_fresh_file_without_a_trailing_newline_still_gets_one_line_per_entry(self) -> None:
        text, _ = carry.merge(_FRESH.rstrip("\n"), _PREVIOUS, _attached(win_jar=f"sha256:{_WIN_JAR}"))
        self.assertEqual(_FRESH + f"{_WIN_JAR}  Serenity-windows-x64.jar\n", text)


class CommandLineTests(unittest.TestCase):
    def _run(self, previous: str | None, release: object, *, release_file: bool = True) -> tuple[int, str, str]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "fresh").write_text(_FRESH)
            args = ["--fresh", str(root / "fresh"), "--previous", str(root / "prev"), "--release-json", str(root / "rel.json")]
            if previous is not None:
                (root / "prev").write_text(previous)
            if release_file:
                (root / "rel.json").write_text(json.dumps(release))
            stderr = io.StringIO()
            with redirect_stderr(stderr):
                code = carry.main(args)
            return code, (root / "fresh").read_text(), stderr.getvalue()

    def test_it_rewrites_the_fresh_file_in_place(self) -> None:
        code, text, _ = self._run(
            _PREVIOUS, {"assets": [{"name": "Serenity-windows-x64.jar", "digest": f"sha256:{_WIN_JAR}"}]}
        )
        self.assertEqual(0, code)
        self.assertEqual(_FRESH + f"{_WIN_JAR}  Serenity-windows-x64.jar\n", text)

    def test_a_missing_previous_file_and_release_leave_the_fresh_lines_alone(self) -> None:
        code, text, stderr = self._run(None, None, release_file=False)
        self.assertEqual(0, code)
        self.assertEqual(_FRESH, text)
        self.assertEqual("", stderr)

    def test_a_release_file_that_is_not_json_is_treated_as_no_release(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "fresh").write_text(_FRESH)
            (root / "rel.json").write_text("not json")
            with redirect_stderr(io.StringIO()):
                code = carry.main(
                    ["--fresh", str(root / "fresh"), "--previous", str(root / "prev"), "--release-json", str(root / "rel.json")]
                )
            self.assertEqual(0, code)
            self.assertEqual(_FRESH, (root / "fresh").read_text())

    def test_warnings_are_printed_as_github_warning_annotations(self) -> None:
        _, _, stderr = self._run(None, {"assets": [{"name": "Serenity-windows-x64.jar", "digest": f"sha256:{_WIN_JAR}"}]})
        self.assertTrue(stderr.startswith("::warning::"), stderr)


if __name__ == "__main__":
    unittest.main()
