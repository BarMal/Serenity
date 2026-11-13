#!/usr/bin/env python3
"""Checks on the repository variable CROSS_OS_CI, the one switch that turns Windows and macOS CI off (#2618, [linux-only-ci]):

    python3 -m unittest scripts.test_linux_only_ci -v

Unset (or anything but "off") means every job runs exactly as before. "off" means Linux only: the Windows and macOS jobs
are skipped, the merge train and Auto Squash Merge stop waiting for them, and Desktop Publish builds and publishes the
Linux jar and zip. The jobs themselves are not deleted, so deleting the variable turns them all back on.
"""

from __future__ import annotations

import json
import re
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
_WORKFLOWS = _ROOT / ".github" / "workflows"
_OFF = "vars.CROSS_OS_CI != 'off'"


def _workflow(name: str) -> str:
    return (_WORKFLOWS / name).read_text()


def _job(text: str, name: str) -> str:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if line == f"  {name}:")
    end = next((i for i in range(start + 1, len(lines)) if re.match(r"^  [A-Za-z0-9_-]+:\s*$", lines[i])), len(lines))
    return "\n".join(lines[start:end])


def _steps(job: str) -> list[str]:
    return re.split(r"(?m)^      - name: ", job)[1:]


def _step(job: str, name: str) -> str:
    return next(step for step in _steps(job) if step.startswith(name))


def _run_block(step: str) -> str:
    """The shell script of a step's `run: |` block."""
    lines = step.splitlines()
    start = next(i for i, line in enumerate(lines) if line.strip() == "run: |") + 1
    body: list[str] = []
    for line in lines[start:]:
        if line.strip() and not line.startswith("          "):
            break
        body.append(line)
    return textwrap.dedent("\n".join(body))


_FAKE_GH = """#!/usr/bin/env bash
echo "$@" >> "$FAKE_GH_LOG"
case "$1" in
  release)
    while [ "$#" -gt 0 ]; do [ "$1" = "--dir" ] && DIR="$2"; shift; done
    [ -f "$FAKE_GH_PREVIOUS" ] || exit 1
    mkdir -p "$DIR" && cp "$FAKE_GH_PREVIOUS" "$DIR/SHA256SUMS" ;;
  api)
    [ -f "$FAKE_GH_RELEASE" ] || exit 1
    cat "$FAKE_GH_RELEASE" ;;
  *) exit 1 ;;
esac
"""


def _fake_gh(directory: Path) -> Path:
    """A stand-in `gh` that serves the previous release's SHA256SUMS and release JSON from files, and logs its calls."""
    directory.mkdir(parents=True, exist_ok=True)
    gh = directory / "gh"
    gh.write_text(_FAKE_GH)
    gh.chmod(0o755)
    return directory


def _bash(script: str, cwd: Path, **env: str) -> subprocess.CompletedProcess[str]:
    tools = _fake_gh(cwd / "fake-bin")
    return subprocess.run(
        ["bash", "--noprofile", "--norc", "-e", "-o", "pipefail", "-c", script],
        cwd=cwd,
        env={
            "PATH": f"{tools}:/usr/local/bin:/usr/bin:/bin",
            "GITHUB_WORKSPACE": str(_ROOT),
            "GITHUB_REPOSITORY": "owner/repo",
            "FAKE_GH_LOG": str(cwd / "gh.log"),
            "FAKE_GH_PREVIOUS": str(cwd / "previous-sums"),
            "FAKE_GH_RELEASE": str(cwd / "release.json"),
            **env,
        },
        capture_output=True,
        text=True,
        check=False,
    )


def _matrix_choice(job: str) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    """The matrix include list when CROSS_OS_CI is off, and when it is not."""
    match = re.search(r"include: \$\{\{ fromJSON\(vars\.CROSS_OS_CI == 'off' && '(\[.*?\])' \|\| '(\[.*?\])'\) \}\}", job)
    assert match is not None, "the matrix is not chosen by CROSS_OS_CI"
    return json.loads(match.group(1)), json.loads(match.group(2))


class CrossOsTestsTests(unittest.TestCase):
    _text = _workflow("cross-os-tests.yml")

    def test_every_windows_and_macos_job_is_skipped_when_the_switch_is_off(self) -> None:
        for name in ("shards-windows", "shards-macos", "test-windows", "test-macos"):
            with self.subTest(job=name):
                self.assertIn(f"&& {_OFF}\n", _job(self._text, name) + "\n")

    def test_the_wake_job_is_not_switched_off_so_a_train_still_hears_a_run_finish(self) -> None:
        self.assertNotIn("CROSS_OS_CI", _job(self._text, "wake-train"))


class OtherOsJobsTests(unittest.TestCase):
    def test_the_package_check_builds_only_linux_when_off_and_the_three_os_matrix_otherwise(self) -> None:
        off, on = _matrix_choice(_job(_workflow("desktop-release.yml"), "build"))
        self.assertEqual(["ubuntu-latest"], [entry["os"] for entry in off])
        self.assertEqual(["ubuntu-latest", "windows-latest", "macos-latest"], [entry["os"] for entry in on])
        self.assertEqual(["linux-x64", "windows-x64", "macos-arm64"], [entry["artifact_suffix"] for entry in on])

    def test_the_ligature_oracle_keeps_its_linux_leg_when_off(self) -> None:
        off, on = _matrix_choice(_job(_workflow("ligature-oracle.yml"), "ligature-oracle"))
        self.assertEqual(["linux"], [entry["label"] for entry in off])
        self.assertEqual(["linux", "windows", "macos"], [entry["label"] for entry in on])
        self.assertEqual(["ubuntu-latest", "windows-latest", "macos-latest"], [entry["os"] for entry in on])

    def test_the_skia_natives_probe_and_the_non_linux_skiko_builds_are_skipped_when_off(self) -> None:
        self.assertIn(f"    if: {_OFF}\n", _job(_workflow("skia-natives-probe.yml"), "windows") + "\n")
        skiko = _workflow("skiko-natives.yml")
        for name, group in (("build-macos", "macos"), ("build-windows", "windows")):
            with self.subTest(job=name):
                self.assertIn(
                    f"    if: needs.plan.outputs.{group} == 'true' && {_OFF}\n", _job(skiko, name) + "\n"
                )

    def test_the_linux_skiko_builds_are_untouched(self) -> None:
        for name in ("build-linux-x64", "build-linux-arm64"):
            with self.subTest(job=name):
                self.assertNotIn("CROSS_OS_CI", _job(_workflow("skiko-natives.yml"), name))


class MergeGateTests(unittest.TestCase):
    def test_the_train_workflow_hands_the_switch_to_the_train(self) -> None:
        self.assertIn("CROSS_OS_CI: ${{ vars.CROSS_OS_CI }}", _workflow("merge-train.yml"))

    def test_auto_squash_merge_requires_the_cross_os_checks_only_when_the_switch_is_not_off(self) -> None:
        text = _workflow("auto-squash-merge.yml")
        self.assertIn("CROSS_OS_CI: ${{ vars.CROSS_OS_CI }}", text)
        self.assertRegex(
            text,
            r'if \[ "\$\{CROSS_OS_CI,,\}" = "off" \]; then\s+required_checks=\(\)\s+else\s+'
            r'required_checks=\("Cross-OS Tests: Test \(windows\)" "Cross-OS Tests: Test \(macos\)"\)\s+fi',
        )


class AutoSquashSwitchTests(unittest.TestCase):
    def test_the_cross_os_checks_are_dropped_whatever_the_letter_case_of_off(self) -> None:
        text = _workflow("auto-squash-merge.yml")
        start = text.index('if [ "${CROSS_OS_CI')
        snippet = text[start : text.index("\n          fi\n", start) + len("\n          fi\n")]
        script = textwrap.dedent(snippet) + 'echo "${#required_checks[@]}"'
        with tempfile.TemporaryDirectory() as tmp:
            for value, expected in (("off", "0"), ("OFF", "0"), ("Off", "0"), ("", "2"), ("on", "2")):
                with self.subTest(CROSS_OS_CI=value):
                    result = _bash(script, Path(tmp), CROSS_OS_CI=value)
                    self.assertEqual(expected, result.stdout.strip(), result.stderr)


class DesktopPublishTests(unittest.TestCase):
    _text = _workflow("desktop-publish.yml")

    def test_the_build_matrix_is_linux_only_when_off(self) -> None:
        off, on = _matrix_choice(_job(self._text, "build"))
        self.assertEqual([{"os": "ubuntu-latest", "artifact_suffix": "linux-x64"}], off)
        self.assertEqual(
            [
                {"os": "ubuntu-latest", "artifact_suffix": "linux-x64"},
                {"os": "windows-latest", "artifact_suffix": "windows-x64"},
                {"os": "macos-latest", "artifact_suffix": "macos-arm64"},
            ],
            on,
        )

    def test_the_cross_os_nightly_gate_is_skipped_when_off_so_the_ref_itself_is_built(self) -> None:
        gate = _job(self._text, "gate")
        for name in ("Check out the gate script", "Require a green Cross-OS nightly"):
            with self.subTest(step=name):
                self.assertIn(f"if: github.ref == 'refs/heads/master' && {_OFF}", _step(gate, name))
        self.assertIn("ref: ${{ needs.gate.outputs.sha || github.ref }}", _job(self._text, "build"))

    def test_the_checksums_the_release_and_the_nightly_name_only_linux_assets_when_off(self) -> None:
        publish = _job(self._text, "publish")
        for name in ("Write SHA256SUMS", "Publish versioned release", "Publish nightly desktop build"):
            with self.subTest(step=name):
                step = _step(publish, name)
                self.assertIn("CROSS_OS_CI: ${{ vars.CROSS_OS_CI }}", step)
                self.assertRegex(step, r'if \[ "\$\{CROSS_OS_CI,,\}" = "off" \]; then')
                self.assertIn("Serenity-linux-x64.jar", step)
                self.assertIn("Serenity-linux-x64.zip", step)

    def test_the_checksums_are_written_for_the_linux_assets_whatever_the_letter_case_of_off(self) -> None:
        script = _run_block(_step(_job(self._text, "publish"), "Write SHA256SUMS"))
        for spelling in ("off", "OFF", "Off"):
            with self.subTest(CROSS_OS_CI=spelling), tempfile.TemporaryDirectory() as tmp:
                dist = Path(tmp) / "dist"
                dist.mkdir()
                for name in ("Serenity-linux-x64.jar", "Serenity-linux-x64.zip", "Serenity.cdx.json"):
                    (dist / name).write_bytes(b"x")
                result = _bash(script, Path(tmp), CROSS_OS_CI=spelling, GITHUB_REF_TYPE="branch")
                self.assertEqual(0, result.returncode, result.stderr)
                listed = [line.split()[-1] for line in (dist / "SHA256SUMS").read_text().splitlines()]
                self.assertEqual(["Serenity-linux-x64.jar", "Serenity-linux-x64.zip", "Serenity.cdx.json"], listed)

    def _write_sums_off(self, tmp: str, **env: str) -> subprocess.CompletedProcess[str]:
        script = _run_block(_step(_job(self._text, "publish"), "Write SHA256SUMS"))
        dist = Path(tmp) / "dist"
        dist.mkdir(exist_ok=True)
        for name in ("Serenity-linux-x64.jar", "Serenity-linux-x64.zip", "Serenity.cdx.json"):
            (dist / name).write_bytes(b"x")
        return _bash(script, Path(tmp), CROSS_OS_CI="off", GH_TOKEN="t", **{"GITHUB_REF_TYPE": "branch", **env})

    def test_the_old_windows_and_macos_checksums_are_carried_into_the_nightly_when_off(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            old = {"Serenity-windows-x64.jar": "1" * 64, "Serenity-macos-arm64.zip": "4" * 64}
            (Path(tmp) / "previous-sums").write_text("".join(f"{h}  {n}\n" for n, h in old.items()))
            (Path(tmp) / "release.json").write_text(
                json.dumps({"assets": [{"name": n, "digest": f"sha256:{h}"} for n, h in old.items()]})
            )
            result = self._write_sums_off(tmp)
            self.assertEqual(0, result.returncode, result.stderr)
            listed = (Path(tmp) / "dist" / "SHA256SUMS").read_text().splitlines()
            self.assertEqual(
                [
                    "Serenity-linux-x64.jar",
                    "Serenity-linux-x64.zip",
                    "Serenity.cdx.json",
                    "Serenity-windows-x64.jar",
                    "Serenity-macos-arm64.zip",
                ],
                [line.split()[-1] for line in listed],
            )
            self.assertIn(f"{'1' * 64}  Serenity-windows-x64.jar", listed)

    def test_a_nightly_with_no_previous_release_still_publishes_the_linux_checksums_when_off(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            result = self._write_sums_off(tmp)
            self.assertEqual(0, result.returncode, result.stderr)
            listed = (Path(tmp) / "dist" / "SHA256SUMS").read_text().splitlines()
            self.assertEqual(3, len(listed))

    def test_a_versioned_release_carries_nothing_because_it_attaches_only_linux_assets(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            (Path(tmp) / "previous-sums").write_text(f"{'1' * 64}  Serenity-windows-x64.jar\n")
            result = self._write_sums_off(tmp, GITHUB_REF_TYPE="tag")
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertEqual(3, len((Path(tmp) / "dist" / "SHA256SUMS").read_text().splitlines()))
            self.assertFalse((Path(tmp) / "gh.log").exists(), "a tag build must not ask the nightly release for anything")

    def test_nothing_is_fetched_when_the_switch_is_not_off(self) -> None:
        script = _run_block(_step(_job(self._text, "publish"), "Write SHA256SUMS"))
        with tempfile.TemporaryDirectory() as tmp:
            dist = Path(tmp) / "dist"
            dist.mkdir()
            for name in (
                "Serenity-linux-x64.jar", "Serenity-windows-x64.jar", "Serenity-macos-arm64.jar",
                "Serenity-linux-x64.zip", "Serenity-windows-x64.zip", "Serenity-macos-arm64.zip",
            ):
                (dist / name).write_bytes(b"x")
            result = _bash(script, Path(tmp), CROSS_OS_CI="", GITHUB_REF_TYPE="branch")
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertEqual(6, len((dist / "SHA256SUMS").read_text().splitlines()))
            self.assertFalse((Path(tmp) / "gh.log").exists())

    def test_the_checksums_still_need_every_platform_when_the_switch_is_not_off(self) -> None:
        script = _run_block(_step(_job(self._text, "publish"), "Write SHA256SUMS"))
        for value in ("", "on", "offline"):
            with self.subTest(CROSS_OS_CI=value), tempfile.TemporaryDirectory() as tmp:
                dist = Path(tmp) / "dist"
                dist.mkdir()
                for name in ("Serenity-linux-x64.jar", "Serenity-linux-x64.zip"):
                    (dist / name).write_bytes(b"x")
                result = _bash(script, Path(tmp), CROSS_OS_CI=value, GITHUB_REF_TYPE="branch")
                self.assertNotEqual(0, result.returncode)
                self.assertIn("Serenity-windows-x64.jar", result.stderr)

    def test_existing_windows_and_macos_assets_are_never_deleted(self) -> None:
        self.assertNotIn("gh release delete-asset", self._text)
        self.assertNotIn("--clobber=false", self._text)
        self.assertIn("gh release upload desktop-latest", self._text)

    def test_the_nightly_notes_say_which_assets_a_build_carries(self) -> None:
        notes = _step(_job(self._text, "build"), "Generate release notes")
        self.assertIn('"${CROSS_OS_CI,,}" = "off"', notes)
        self.assertIn("Linux only", notes)


class DocumentationTests(unittest.TestCase):
    def test_development_md_names_the_switch(self) -> None:
        text = (_ROOT / "DEVELOPMENT.md").read_text()
        self.assertIn("CROSS_OS_CI", text)
        self.assertIn("off", text[text.index("CROSS_OS_CI") :][:1500])


if __name__ == "__main__":
    unittest.main()
