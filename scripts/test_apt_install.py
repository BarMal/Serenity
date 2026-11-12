#!/usr/bin/env python3
"""Tests for ci/apt/install-packages.sh against fake apt-get and dpkg commands:

    python3 -m unittest scripts.test_apt_install -v

The fakes append every call to a log and fail on request (FAKE_APT_FAILS_INSTALL counts how many installs fail before
one works; FAKE_APT_FAILS_UPDATE and FAKE_DPKG_FAILS switch the others on), so the script is held to its contract without
a network: a cache hit installs the cached packages without touching the network, a miss fetches with retries and
timeouts, and a fetch that never works fails the step.
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent.parent / "ci" / "apt" / "install-packages.sh"

_APT_GET = """\
#!/usr/bin/env bash
echo "apt-get $*" >> "$FAKE_LOG"
case " $* " in
  *" update "*)
    [[ "${FAKE_APT_FAILS_UPDATE:-}" == 1 ]] && exit 100
    exit 0 ;;
  *" install "*)
    counter="$FAKE_LOG.installs"
    n=$(( $(cat "$counter" 2>/dev/null || echo 0) + 1 ))
    echo "$n" > "$counter"
    (( n <= ${FAKE_APT_FAILS_INSTALL:-0} )) && exit 100
    exit 0 ;;
esac
exit 0
"""

_DPKG = """\
#!/usr/bin/env bash
echo "dpkg $*" >> "$FAKE_LOG"
[[ "${FAKE_DPKG_FAILS:-}" == 1 ]] && exit 1
exit 0
"""


class InstallPackagesTest(unittest.TestCase):
    def setUp(self) -> None:
        self._directory = tempfile.TemporaryDirectory()
        self.addCleanup(self._directory.cleanup)
        self.root = Path(self._directory.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        for name, body in (("apt-get", _APT_GET), ("dpkg", _DPKG)):
            fake = self.bin / name
            fake.write_text(body)
            fake.chmod(0o755)
        self.archive = self.root / "archive"
        self.log = self.root / "calls.log"

    def _cached(self, *names: str) -> None:
        self.archive.mkdir(exist_ok=True)
        for name in names:
            (self.archive / name).write_text("deb")

    def _run(self, cache_hit: str, *packages: str, **env: str) -> subprocess.CompletedProcess[str]:
        environment = {
            **os.environ,
            "PATH": f"{self.bin}{os.pathsep}{os.environ['PATH']}",
            "SUDO": "",
            "APT_RETRY_SLEEP": "0",
            "FAKE_LOG": str(self.log),
            **env,
        }
        return subprocess.run(
            ["bash", str(_SCRIPT), str(self.archive), cache_hit, *(packages or ("libegl1", "sway"))],
            capture_output=True,
            text=True,
            env=environment,
            timeout=60,
        )

    def _calls(self) -> list[str]:
        return self.log.read_text().splitlines() if self.log.exists() else []

    def test_a_cache_hit_installs_the_cached_packages_and_never_touches_the_network(self) -> None:
        self._cached("a.deb", "b.deb")
        result = self._run("true")
        self.assertEqual(0, result.returncode, result.stderr)
        calls = self._calls()
        self.assertEqual(1, len(calls), calls)
        self.assertRegex(calls[0], r"^dpkg -i .*a\.deb .*b\.deb$")
        self.assertFalse(any(call.startswith("apt-get") for call in calls), calls)

    def test_a_cache_hit_whose_packages_will_not_install_falls_back_to_a_fetch(self) -> None:
        self._cached("a.deb")
        result = self._run("true", FAKE_DPKG_FAILS="1")
        self.assertEqual(0, result.returncode, result.stderr)
        calls = self._calls()
        self.assertTrue(any(call.startswith("dpkg -i") for call in calls), calls)
        self.assertTrue(any(" update" in call for call in calls), calls)
        self.assertTrue(any(" install" in call for call in calls), calls)

    def test_a_cache_hit_with_nothing_in_the_archive_is_a_miss(self) -> None:
        self._cached()
        result = self._run("true")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertFalse(any(call.startswith("dpkg") for call in self._calls()), self._calls())
        self.assertTrue(any(" install" in call for call in self._calls()), self._calls())

    def test_a_miss_fetches_into_the_archive_with_retries_and_timeouts(self) -> None:
        result = self._run("false", "libegl1", "sway")
        self.assertEqual(0, result.returncode, result.stderr)
        install = next(call for call in self._calls() if " install" in call)
        for expected in (
            f"Dir::Cache::archives={self.archive}",
            "Acquire::Retries=5",
            "Acquire::http::Timeout=20",
            "Acquire::https::Timeout=20",
            "--no-install-recommends",
            "libegl1 sway",
        ):
            self.assertIn(expected, install)
        update = next(call for call in self._calls() if " update" in call)
        self.assertIn("Acquire::Retries=5", update)
        self.assertTrue((self.archive / "partial").is_dir())

    def test_a_failed_fetch_is_tried_again_from_a_fresh_update(self) -> None:
        result = self._run("false", FAKE_APT_FAILS_INSTALL="1")
        self.assertEqual(0, result.returncode, result.stderr)
        updates = [call for call in self._calls() if " update" in call]
        installs = [call for call in self._calls() if " install" in call]
        self.assertEqual(2, len(updates), self._calls())
        self.assertEqual(2, len(installs), self._calls())

    def test_a_fetch_that_never_works_fails_after_three_attempts_naming_the_packages(self) -> None:
        result = self._run("false", "libegl1", "sway", FAKE_APT_FAILS_INSTALL="99")
        self.assertNotEqual(0, result.returncode)
        self.assertEqual(3, len([call for call in self._calls() if " install" in call]), self._calls())
        self.assertIn("libegl1 sway", result.stderr)

    def test_an_update_that_never_works_fails_the_same_way(self) -> None:
        result = self._run("false", FAKE_APT_FAILS_UPDATE="1")
        self.assertNotEqual(0, result.returncode)
        self.assertEqual(0, len([call for call in self._calls() if " install" in call]), self._calls())

    def test_it_says_whether_the_cache_was_used_so_a_run_log_shows_a_miss_then_a_hit(self) -> None:
        self._cached("a.deb", "b.deb")
        hit = self._run("true")
        self.assertIn("cache hit: installing 2 cached packages", hit.stderr)
        self._cached()
        for stale in self.archive.glob("*.deb"):
            stale.unlink()
        miss = self._run("false")
        self.assertIn("cache miss: fetching libegl1 sway", miss.stderr)
        empty = self._run("true")
        self.assertIn("cache miss: fetching libegl1 sway", empty.stderr)


if __name__ == "__main__":
    unittest.main()
