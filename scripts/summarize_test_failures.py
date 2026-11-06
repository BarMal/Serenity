#!/usr/bin/env python3
"""Puts the failing tests of a run where its reader looks (#2557).

    summarize_test_failures.py [DIR] [--summary FILE]

Reads the JUnit XML reports in DIR (default target/test-reports), appends a Markdown section to FILE (pass
$GITHUB_STEP_SUMMARY; without it the Markdown goes to stdout) and prints one `::error` workflow command per failing test
on stdout. ScalaTest's console reporter names only the failing suites, so without this the test name and message are
found by downloading the artifact.

Always exits 0: it explains a failed step, so it must not fail one, and a missing or malformed report is reported rather
than raised. Stdlib only, so it runs on the Linux, macOS and Windows runners as they come.
"""

from __future__ import annotations

import argparse
import html
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from pathlib import Path

ANNOTATION_LIMIT = 20
_STACK_LINES = 8
_STACK_LINE_WIDTH = 200
_MESSAGE_WIDTH = 300
_UNREADABLE_LIMIT = 5
_DASH = "—"


@dataclass(frozen=True)
class Problem:
    suite: str
    test: str
    kind: str
    message: str
    detail: str


@dataclass(frozen=True)
class Report:
    tests: int = 0
    failures: int = 0
    errors: int = 0
    skipped: int = 0
    cancelled: int = 0
    problems: list[Problem] = field(default_factory=list)
    cancelled_tests: list[tuple[str, str]] = field(default_factory=list)
    unreadable: list[str] = field(default_factory=list)
    has_reports: bool = False


def read_reports(directory: Path) -> Report:
    paths = sorted(directory.glob("TEST-*.xml")) if directory.is_dir() else []
    tests = failures = errors = skipped = cancelled = 0
    problems: list[Problem] = []
    cancelled_tests: list[tuple[str, str]] = []
    unreadable: list[str] = []
    for path in paths:
        try:
            root = ET.parse(path).getroot()
        except (ET.ParseError, OSError) as error:
            unreadable.append(f"{path.name}: {error}")
            continue
        for suite in root.iter("testsuite"):
            suite_name = suite.get("name") or path.stem
            for case in suite.findall("testcase"):
                tests += 1
                test_name = case.get("name") or "(unnamed)"
                for kind in ("failure", "error"):
                    for element in case.findall(kind):
                        if kind == "failure":
                            failures += 1
                        else:
                            errors += 1
                        problems.append(_problem(suite_name, test_name, kind, element))
                for element in case.findall("skipped"):
                    skipped += 1
                    if _is_cancelled(element):
                        cancelled += 1
                        cancelled_tests.append((suite_name, test_name))
    return Report(tests, failures, errors, skipped, cancelled, problems, cancelled_tests, unreadable, bool(paths))


def _problem(suite: str, test: str, kind: str, element: ET.Element) -> Problem:
    detail = (element.text or "").strip()
    message = element.get("message") or detail or element.get("type") or "(no message)"
    return Problem(suite, test, kind, _first_line(message), detail)


def _first_line(text: str) -> str:
    lines = text.strip().splitlines()
    return lines[0].strip() if lines else ""


# Caution, here be imagine dragons: how ScalaTest's JUnit XML marks a cancelled test is inferred, not read from its
# source. A skipped element whose message or type mentions "cancel" (TestCanceledException) is taken to be one.
def _is_cancelled(element: ET.Element) -> bool:
    return "cancel" in f"{element.get('message', '')} {element.get('type', '')}".lower()


def render_markdown(report: Report, directory: Path | str = "target/test-reports") -> str:
    out = ["## Test results", ""]
    if not report.has_reports:
        out.append(f"No JUnit reports in {directory}.")
    else:
        cancelled = f" ({report.cancelled} cancelled)" if report.cancelled else ""
        out.append(
            f"{report.tests} tests, {report.failures} failed, {report.errors} errored, {report.skipped} skipped{cancelled}."
        )
    if report.problems:
        out += ["", "| Suite | Test | Message |", "|---|---|---|"]
        out += [f"| `{_cell(p.suite)}` | {_cell(p.test)} | {_cell(_clip(p.message, _MESSAGE_WIDTH))} |" for p in report.problems]
        out += [""] + [_details(p) for p in report.problems]
    if report.cancelled_tests:
        out += ["", "Cancelled:", ""] + [f"- `{_cell(suite)}`: {_cell(test)}" for suite, test in report.cancelled_tests]
    if report.unreadable:
        out += ["", "Unreadable reports:", ""] + [f"- {_cell(entry)}" for entry in report.unreadable]
    return "\n".join(out) + "\n"


def _cell(text: str) -> str:
    return html.escape(text, quote=False).replace("|", "\\|").replace("\n", " ")


def _clip(text: str, width: int) -> str:
    return text if len(text) <= width else text[: width - 1] + "\u2026"


def _details(problem: Problem) -> str:
    stack = [_clip(line.rstrip(), _STACK_LINE_WIDTH) for line in problem.detail.splitlines()[:_STACK_LINES]]
    body = html.escape("\n".join(stack), quote=False)
    title = html.escape(f"{problem.suite} / {problem.test}", quote=False)
    return f"<details><summary>{title}</summary>\n\n<pre>{body}</pre>\n\n</details>\n"


def annotations(report: Report, limit: int = ANNOTATION_LIMIT) -> list[str]:
    lines = [
        f"::error title={_escape_property(p.suite)}::{_escape_data(p.test + ' ' + _DASH + ' ' + p.message)}"
        for p in report.problems[:limit]
    ]
    left_out = len(report.problems) - limit
    if left_out > 0:
        lines.append(f"::warning title=Test failures::{left_out} more failing tests; see the step summary.")
    lines += [
        f"::warning title=Unreadable test report::{_escape_data(entry)}" for entry in report.unreadable[:_UNREADABLE_LIMIT]
    ]
    return lines


def _escape_data(text: str) -> str:
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def _escape_property(text: str) -> str:
    return _escape_data(text).replace(":", "%3A").replace(",", "%2C")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Summarise failing tests from JUnit XML reports.")
    parser.add_argument("directory", nargs="?", default="target/test-reports")
    parser.add_argument("--summary", help="file to append the Markdown to, for example $GITHUB_STEP_SUMMARY")
    args = parser.parse_args(argv)
    # A diagnostic must not fail the job, so anything unforeseen is reported as an unreadable report instead.
    try:
        report = read_reports(Path(args.directory))
    except Exception as error:  # noqa: BLE001
        report = Report(unreadable=[f"{args.directory}: {error}"], has_reports=True)
    markdown = render_markdown(report, args.directory)
    if args.summary:
        try:
            with open(args.summary, "a", encoding="utf-8") as handle:
                handle.write(markdown)
        except OSError as error:
            print(f"Could not write {args.summary}: {error}", file=sys.stderr)
            print(markdown)
    else:
        print(markdown)
    for line in annotations(report):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
