#!/usr/bin/env python3
"""Checks on how the package workflows ship the AOT cache inside the signed macOS bundle (#2522, item 1c):

    python3 -m unittest scripts.test_aot_macos_workflow -v

The cache is trained before signing, so nothing writes into the bundle once it carries a signature, and it is checked
on the signed bundle afterwards: the check must start the bundle with its cache, then prove the signature still holds.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"
_PACKAGE_WORKFLOWS = ("desktop-publish.yml", "desktop-release.yml")


def _names(text: str) -> list[str]:
    return re.findall(r"^      - name: (.+)$", text, re.M)


def _step(text: str, name: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line.strip() == f"- name: {name}")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^      - name: ", lines[i])), len(lines))
    return "\n".join(lines[start:end])


class MacosAotCacheTest(unittest.TestCase):
    def test_the_bundle_is_trained_after_jpackage_and_before_it_is_signed(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                names = _names((_WORKFLOWS / workflow).read_text())
                train = names.index("Train macOS AOT cache")
                self.assertLess(names.index("Prepare app image"), train)
                self.assertLess(train, names.index("Sign macOS app image"))
                if "Import macOS Developer ID certificate" in names:
                    self.assertLess(train, names.index("Import macOS Developer ID certificate"))

    def test_training_runs_on_macos_only_against_the_bundle(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                step = _step((_WORKFLOWS / workflow).read_text(), "Train macOS AOT cache")
                self.assertIn("if: runner.os == 'macOS'", step)
                self.assertIn("scripts/train-aot-cache.sh dist/Serenity.app", step)

    def test_the_signed_bundle_is_checked_and_its_signature_verified_afterwards(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                text = (_WORKFLOWS / workflow).read_text()
                names = _names(text)
                check = names.index("Check macOS AOT cache on the signed bundle")
                self.assertLess(names.index("Sign macOS app image"), check)
                if "Smoke-test macOS app image" in names:
                    self.assertLess(names.index("Smoke-test macOS app image"), check)
                self.assertLess(check, names.index("Archive Unix app image"))
                step = _step(text, "Check macOS AOT cache on the signed bundle")
                self.assertIn("if: runner.os == 'macOS'", step)
                self.assertLess(
                    step.index("scripts/check-aot-cache.sh dist/Serenity.app"),
                    step.index("codesign --verify --deep --strict"),
                )

    def test_the_new_steps_are_bounded_in_time(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                text = (_WORKFLOWS / workflow).read_text()
                for name in ("Train macOS AOT cache", "Check macOS AOT cache on the signed bundle"):
                    self.assertIn("timeout-minutes:", _step(text, name))

    def test_the_comment_no_longer_says_macos_has_no_archive(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                self.assertNotIn("macOS has neither", (_WORKFLOWS / workflow).read_text())


if __name__ == "__main__":
    unittest.main()
