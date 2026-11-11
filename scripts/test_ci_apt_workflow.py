#!/usr/bin/env python3
"""Tests for how .github/workflows/ci.yml installs the Mesa EGL packages the SDL native specs need (#2618, [ci-apt]):

    python3 -m unittest scripts.test_ci_apt_workflow -v

The step used to be a bare `apt-get update && apt-get install`, which failed the job whenever the runner could not
resolve the Ubuntu mirrors. It now restores the downloaded packages from a cache and goes through
ci/apt/install-packages.sh, which retries a fetch.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_CI = (_ROOT / ".github" / "workflows" / "ci.yml").read_text()


def _sdl_native_job() -> str:
    start = _CI.index("  sdl-native:\n")
    following = re.search(r"^  [a-z0-9-]+:\n", _CI[start + 1 :], re.MULTILINE)
    return _CI[start : start + 1 + following.start()] if following else _CI[start:]


class CiAptWorkflowTest(unittest.TestCase):
    def test_the_mesa_packages_are_not_installed_with_a_bare_apt_get(self) -> None:
        self.assertNotRegex(_sdl_native_job(), r"apt-get install[^\n]*libegl1")

    def test_the_mesa_packages_go_through_the_install_script(self) -> None:
        job = _sdl_native_job()
        self.assertIn("ci/apt/install-packages.sh", job)
        for package in ("libegl1", "libgl1", "libgl1-mesa-dri", "libgles2", "sway"):
            self.assertRegex(job, rf"\b{re.escape(package)}\b")

    def test_the_downloaded_packages_are_cached_before_the_install_step(self) -> None:
        job = _sdl_native_job()
        cache = job.index("actions/cache@v4")
        install = job.index("ci/apt/install-packages.sh")
        self.assertLess(cache, install)

    def test_the_cache_key_names_the_runner_image_and_the_package_list(self) -> None:
        job = _sdl_native_job()
        cache = job[job.index("actions/cache@v4") :]
        key = re.search(r"key:\s*(.+)", cache)
        self.assertIsNotNone(key)
        self.assertRegex(key.group(1), r"ImageVersion|image")
        self.assertRegex(key.group(1), r"packages|hash")

    def test_the_cache_and_the_script_agree_on_the_archive_directory(self) -> None:
        job = _sdl_native_job()
        path = re.search(r"path:\s*(\S+)", job[job.index("actions/cache@v4") :])
        self.assertIsNotNone(path)
        self.assertIn(path.group(1), job[job.index("ci/apt/install-packages.sh") :])

    def test_the_script_is_executable_by_the_job(self) -> None:
        script = _ROOT / "ci" / "apt" / "install-packages.sh"
        self.assertTrue(script.exists())
        self.assertTrue(script.stat().st_mode & 0o111, "the job runs the script directly")


if __name__ == "__main__":
    unittest.main()
