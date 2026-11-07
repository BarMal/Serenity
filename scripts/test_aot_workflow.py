#!/usr/bin/env python3
"""Checks on how the package workflows ship the AOT cache on Windows (#2522, item 1b):

    python3 -m unittest scripts.test_aot_workflow -v

Windows ships the JDK 25 AOT cache instead of an AppCDS archive. The JVM refuses -XX:AOTCache together with the AppCDS
options, so the jpackage step must not set them on any OS, and both package workflows must train and check the cache on
the Windows runner as they do on Linux.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"
_PACKAGE_WORKFLOWS = ("desktop-publish.yml", "desktop-release.yml")


def _step(text: str, name: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line.strip() == f"- name: {name}")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^      - name: ", lines[i])), len(lines))
    return "\n".join(lines[start:end])


class WindowsAotCacheTest(unittest.TestCase):
    def test_no_package_workflow_sets_the_appcds_options(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                text = (_WORKFLOWS / workflow).read_text()
                self.assertNotIn("--java-options '-XX:SharedArchiveFile", text)
                self.assertNotIn("--java-options '-XX:+AutoCreateSharedArchive", text)

    def test_windows_trains_then_checks_the_cache_in_git_bash(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                step = _step((_WORKFLOWS / workflow).read_text(), "Train and check Windows AOT cache")
                self.assertIn("if: runner.os == 'Windows'", step)
                self.assertIn("shell: bash", step)
                self.assertLess(
                    step.index("scripts/train-aot-cache.sh dist/Serenity"),
                    step.index("scripts/check-aot-cache.sh dist/Serenity"),
                )

    def test_windows_trains_after_the_image_is_smoke_tested_and_before_it_is_archived(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                text = (_WORKFLOWS / workflow).read_text()
                names = re.findall(r"^      - name: (.+)$", text, re.M)
                train = names.index("Train and check Windows AOT cache")
                self.assertLess(names.index("Smoke-test Windows app image"), train)
                self.assertLess(train, names.index("Archive Windows app image"))

    def test_windows_still_regenerates_the_base_archive(self) -> None:
        for workflow in _PACKAGE_WORKFLOWS:
            with self.subTest(workflow=workflow):
                text = (_WORKFLOWS / workflow).read_text()
                self.assertIn("scripts/regenerate-base-cds-archive.sh dist/Serenity", text)


if __name__ == "__main__":
    unittest.main()
