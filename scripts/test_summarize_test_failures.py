#!/usr/bin/env python3
"""Tests for summarize_test_failures.py on JUnit XML shaped like the reports sbt's ScalaTest `-u` option writes:

    python3 -m unittest scripts.test_summarize_test_failures -v
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

_SCRIPTS = Path(__file__).resolve().parent
sys.path.insert(0, str(_SCRIPTS))
import summarize_test_failures as summary  # noqa: E402

_PASSING = """<?xml version="1.0" encoding="UTF-8" ?>
<testsuite errors="0" failures="0" hostname="vm" name="com.example.GoodSpec" tests="2" time="0.5">
  <properties><property name="java.version" value="25"/></properties>
  <testcase classname="com.example.GoodSpec" name="adds" time="0.1"></testcase>
  <testcase classname="com.example.GoodSpec" name="subtracts" time="0.1"></testcase>
</testsuite>
"""

_FAILING = """<?xml version="1.0" encoding="UTF-8" ?>
<testsuite errors="1" failures="1" hostname="vm" name="com.example.BadSpec" tests="5" time="1.5">
  <testcase classname="com.example.BadSpec" name="passes" time="0.1"></testcase>
  <testcase classname="com.example.BadSpec" name="compares &lt;two&gt; rows | in a table" time="0.2">
    <failure message="3 did not equal 4&#10;second line of the message" type="org.scalatest.exceptions.TestFailedException">org.scalatest.exceptions.TestFailedException: 3 did not equal 4
\tat com.example.BadSpec.$anonfun$1(BadSpec.scala:12)
\tat org.scalatest.Assertions.fail(Assertions.scala:1)</failure>
  </testcase>
  <testcase classname="com.example.BadSpec" name="explodes" time="0.2">
    <error message="boom: 100% broken, really" type="java.lang.IllegalStateException">java.lang.IllegalStateException: boom
\tat com.example.BadSpec.explode(BadSpec.scala:30)</error>
  </testcase>
  <testcase classname="com.example.BadSpec" name="needs a display" time="0.0">
    <skipped message="cancelled: no display" type="org.scalatest.exceptions.TestCanceledException"/>
  </testcase>
  <testcase classname="com.example.BadSpec" name="is ignored" time="0.0"><skipped/></testcase>
</testsuite>
"""


class ReadReportsTest(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.dir = Path(self._tmp.name)

    def _write(self, name: str, text: str) -> None:
        (self.dir / name).write_text(text, encoding="utf-8")

    def test_counts_tests_failures_errors_skipped_and_cancelled(self) -> None:
        self._write("TEST-good.xml", _PASSING)
        self._write("TEST-bad.xml", _FAILING)
        report = summary.read_reports(self.dir)
        self.assertEqual((7, 1, 1, 2, 1), (report.tests, report.failures, report.errors, report.skipped, report.cancelled))

    def test_lists_each_failing_test_with_its_suite_and_message(self) -> None:
        self._write("TEST-bad.xml", _FAILING)
        problems = summary.read_reports(self.dir).problems
        self.assertEqual(
            [
                ("com.example.BadSpec", "compares <two> rows | in a table", "failure"),
                ("com.example.BadSpec", "explodes", "error"),
            ],
            [(p.suite, p.test, p.kind) for p in problems],
        )
        self.assertEqual("3 did not equal 4", problems[0].message)
        self.assertEqual("boom: 100% broken, really", problems[1].message)

    def test_lists_the_cancelled_tests(self) -> None:
        self._write("TEST-bad.xml", _FAILING)
        self.assertEqual([("com.example.BadSpec", "needs a display")], summary.read_reports(self.dir).cancelled_tests)

    def test_a_failure_without_a_message_attribute_uses_the_first_line_of_its_text(self) -> None:
        self._write(
            "TEST-x.xml",
            '<testsuite name="S" tests="1"><testcase name="t"><failure type="E">first\nsecond</failure></testcase></testsuite>',
        )
        self.assertEqual("first", summary.read_reports(self.dir).problems[0].message)

    def test_a_malformed_file_is_reported_and_the_rest_are_still_read(self) -> None:
        self._write("TEST-bad.xml", _FAILING)
        self._write("TEST-broken.xml", "<testsuite><testcase")
        report = summary.read_reports(self.dir)
        self.assertEqual(2, len(report.problems))
        self.assertEqual(1, len(report.unreadable))
        self.assertIn("TEST-broken.xml", report.unreadable[0])

    def test_a_missing_directory_and_an_empty_one_have_no_reports(self) -> None:
        self.assertFalse(summary.read_reports(self.dir / "absent").has_reports)
        self.assertFalse(summary.read_reports(self.dir).has_reports)

    def test_other_xml_in_the_directory_is_not_a_report(self) -> None:
        self._write("notes.xml", "<root/>")
        self._write("TEST-good.xml", _PASSING)
        report = summary.read_reports(self.dir)
        self.assertEqual(2, report.tests)
        self.assertEqual([], report.unreadable)


class RenderMarkdownTest(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.dir = Path(self._tmp.name)
        (self.dir / "TEST-bad.xml").write_text(_FAILING, encoding="utf-8")
        self.markdown = summary.render_markdown(summary.read_reports(self.dir))

    def test_has_a_counts_line(self) -> None:
        self.assertIn("5 tests, 1 failed, 1 errored, 2 skipped (1 cancelled)", self.markdown)

    def test_has_a_row_per_failing_test_with_the_first_line_of_the_message(self) -> None:
        self.assertIn("| `com.example.BadSpec` |", self.markdown)
        self.assertIn("3 did not equal 4 |", self.markdown)
        self.assertNotIn("second line of the message", self.markdown.split("<details>")[0])

    def test_a_pipe_in_a_test_name_does_not_break_the_table(self) -> None:
        row = next(line for line in self.markdown.splitlines() if "rows" in line and line.startswith("|"))
        self.assertIn("rows \\| in a table", row)
        self.assertIn("&lt;two&gt;", row)

    def test_puts_a_trimmed_stack_in_a_details_block(self) -> None:
        self.assertIn("<details>", self.markdown)
        self.assertIn("BadSpec.scala:12", self.markdown)
        self.assertIn("</details>", self.markdown)

    def test_names_the_cancelled_tests_so_they_are_not_lost_in_the_count(self) -> None:
        self.assertIn("needs a display", self.markdown)

    def test_a_passing_run_is_one_line_and_has_no_table(self) -> None:
        (self.dir / "TEST-bad.xml").unlink()
        (self.dir / "TEST-good.xml").write_text(_PASSING, encoding="utf-8")
        markdown = summary.render_markdown(summary.read_reports(self.dir))
        self.assertIn("2 tests, 0 failed, 0 errored, 0 skipped", markdown)
        self.assertNotIn("| Suite |", markdown)

    def test_an_empty_directory_says_there_are_no_reports(self) -> None:
        (self.dir / "TEST-bad.xml").unlink()
        self.assertIn("No JUnit reports in", summary.render_markdown(summary.read_reports(self.dir), self.dir))

    def test_unreadable_files_are_listed(self) -> None:
        (self.dir / "TEST-broken.xml").write_text("<testsuite", encoding="utf-8")
        markdown = summary.render_markdown(summary.read_reports(self.dir))
        self.assertIn("Unreadable reports", markdown)
        self.assertIn("TEST-broken.xml", markdown)


class AnnotationsTest(unittest.TestCase):
    def _problem(self, suite: str, test: str, message: str) -> summary.Problem:
        return summary.Problem(suite=suite, test=test, kind="failure", message=message, detail="")

    def _report(self, problems: list[summary.Problem]) -> summary.Report:
        return summary.Report(tests=len(problems), failures=len(problems), problems=problems, has_reports=True)

    def test_one_error_command_per_failure(self) -> None:
        lines = summary.annotations(self._report([self._problem("com.example.S", "adds", "3 did not equal 4")]))
        self.assertEqual(["::error title=com.example.S::adds — 3 did not equal 4"], lines)

    def test_escapes_percent_and_line_breaks_in_the_message(self) -> None:
        lines = summary.annotations(self._report([self._problem("S", "t", "100% a\r\nb")]))
        self.assertEqual(["::error title=S::t — 100%25 a%0D%0Ab"], lines)

    def test_escapes_colons_and_commas_in_the_title_as_well(self) -> None:
        lines = summary.annotations(self._report([self._problem("a:b,c", "t", "m")]))
        self.assertTrue(lines[0].startswith("::error title=a%3Ab%2Cc::"))

    def test_caps_the_errors_and_says_how_many_were_left_out(self) -> None:
        problems = [self._problem("S", f"t{i}", "m") for i in range(25)]
        lines = summary.annotations(self._report(problems), limit=20)
        self.assertEqual(20, sum(line.startswith("::error ") for line in lines))
        self.assertEqual(1, sum(line.startswith("::warning ") for line in lines))
        self.assertIn("5 more", lines[-1])

    def test_a_passing_run_has_no_annotations(self) -> None:
        self.assertEqual([], summary.annotations(self._report([])))

    def test_an_unreadable_report_is_a_warning(self) -> None:
        report = summary.Report(unreadable=["TEST-x.xml: no element found"], has_reports=True)
        lines = summary.annotations(report)
        self.assertEqual(["::warning title=Unreadable test report::TEST-x.xml: no element found"], lines)


class CommandLineTest(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.dir = Path(self._tmp.name) / "reports"
        self.dir.mkdir()
        self.summary = Path(self._tmp.name) / "step-summary.md"

    def _run(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(_SCRIPTS / "summarize_test_failures.py"), *args], capture_output=True, text=True, check=False
        )

    def test_appends_markdown_to_the_summary_file_and_prints_only_annotations(self) -> None:
        (self.dir / "TEST-bad.xml").write_text(_FAILING, encoding="utf-8")
        self.summary.write_text("earlier section\n", encoding="utf-8")
        result = self._run(str(self.dir), "--summary", str(self.summary))
        self.assertEqual(0, result.returncode)
        text = self.summary.read_text(encoding="utf-8")
        self.assertTrue(text.startswith("earlier section\n"))
        self.assertIn("com.example.BadSpec", text)
        self.assertEqual(2, sum(line.startswith("::error ") for line in result.stdout.splitlines()))
        self.assertNotIn("| Suite |", result.stdout)

    def test_without_a_summary_file_the_markdown_goes_to_stdout(self) -> None:
        (self.dir / "TEST-bad.xml").write_text(_FAILING, encoding="utf-8")
        self.assertIn("| Suite |", self._run(str(self.dir)).stdout)

    def test_a_missing_directory_exits_zero(self) -> None:
        result = self._run(str(self.dir / "absent"), "--summary", str(self.summary))
        self.assertEqual(0, result.returncode)
        self.assertIn("No JUnit reports in", self.summary.read_text(encoding="utf-8"))

    def test_an_unwritable_summary_file_exits_zero(self) -> None:
        result = self._run(str(self.dir), "--summary", str(self.dir / "no" / "such" / "dir" / "s.md"))
        self.assertEqual(0, result.returncode)


if __name__ == "__main__":
    unittest.main()
