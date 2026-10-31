#!/usr/bin/env python3
"""Tests for scripts/collapsed_top_frames.py on a fixed async-profiler collapsed fixture:

    python3 -m unittest scripts.test_collapsed_top_frames -v
"""

from __future__ import annotations

import unittest

from scripts import collapsed_top_frames as ctf

_FIXTURE = [
    "java/Thread.run;com/A.paint;org/jetbrains/skia/Canvas.drawImage;Java_org_jetbrains_skia_Canvas__1nDrawImage;sse2::blit_mask_d32_a8_opaque 40",
    "java/Thread.run;com/A.paint;com/A.paint;com/B.fill 25",
    "",
    "java/Thread.run;com/A.paint;Java_org_jetbrains_skia_Canvas__1nDrawRect;sse2::blit_mask_d32_a8_opaque 15",
    "no count on this line",
    "a;b notanumber",
    "[vdso] 10",
    "java/Thread.run;com/B.fill 10",
]

_BLIT = "sse2::blit_mask_d32_a8_opaque"

_EXPECTED_REPORT = "\n".join(
    [
        "total samples: 100",
        "",
        "top 3 self frames",
        " samples percent  frame",
        "      55   55.0%  sse2::blit_mask_d32_a8_opaque",
        "      35   35.0%  com/B.fill",
        "      10   10.0%  [vdso]",
        "",
        "top 3 inclusive frames",
        " samples percent  frame",
        "      90   90.0%  java/Thread.run",
        "      80   80.0%  com/A.paint",
        "      55   55.0%  sse2::blit_mask_d32_a8_opaque",
        "",
        "under Java_org_jetbrains_skia: 55.0%",
        "under blit_mask: 55.0%",
    ]
)


class ParseTest(unittest.TestCase):
    def test_skips_blank_and_malformed_lines(self) -> None:
        stacks = ctf.parse(_FIXTURE)
        self.assertEqual(len(stacks), 5)
        self.assertEqual(sum(count for _, count in stacks), 100)

    def test_splits_frames_and_reads_count_after_last_space(self) -> None:
        stacks = ctf.parse(["a;b c;d 7"])
        self.assertEqual(stacks, [(("a", "b c", "d"), 7)])

    def test_keeps_a_bare_native_frame_as_a_one_frame_stack(self) -> None:
        self.assertIn((("[vdso]",), 10), ctf.parse(_FIXTURE))


class TotalsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.stacks = ctf.parse(_FIXTURE)

    def test_self_totals_count_samples_by_leaf(self) -> None:
        self.assertEqual(ctf.self_totals(self.stacks), {_BLIT: 55, "com/B.fill": 35, "[vdso]": 10})

    def test_inclusive_totals_count_a_repeated_frame_once_per_stack(self) -> None:
        totals = ctf.inclusive_totals(self.stacks)
        self.assertEqual(totals["com/A.paint"], 80)
        self.assertEqual(totals["java/Thread.run"], 90)
        self.assertEqual(totals[_BLIT], 55)
        self.assertEqual(totals["com/B.fill"], 35)
        self.assertEqual(totals["[vdso]"], 10)
        self.assertEqual(totals["Java_org_jetbrains_skia_Canvas__1nDrawRect"], 15)

    def test_share_under_matches_substring_of_any_frame(self) -> None:
        self.assertEqual(ctf.share_under(self.stacks, "Java_org_jetbrains_skia"), 0.55)
        self.assertEqual(ctf.share_under(self.stacks, "blit_mask"), 0.55)
        self.assertEqual(ctf.share_under(self.stacks, "com/B"), 0.35)
        self.assertEqual(ctf.share_under(self.stacks, "absent"), 0.0)

    def test_share_under_of_no_samples_is_zero(self) -> None:
        self.assertEqual(ctf.share_under([], "x"), 0.0)


class ReportTest(unittest.TestCase):
    def test_report_text_for_the_fixture(self) -> None:
        stacks = ctf.parse(_FIXTURE)
        text = ctf.report(stacks, 3, ["Java_org_jetbrains_skia", "blit_mask"])
        self.assertEqual(text, _EXPECTED_REPORT)

    def test_percent_format_has_one_decimal(self) -> None:
        self.assertEqual(ctf.percent(0.123), "12.3%")
        self.assertEqual(ctf.percent(1.0), "100.0%")


if __name__ == "__main__":
    unittest.main()
