#!/usr/bin/env python3
"""Tests for scripts/footprint_parse.py on captures shaped like what measure-footprint.sh reads:

    python3 -m unittest scripts.test_footprint_parse -v
"""

from __future__ import annotations

import csv
import io
import unittest
from pathlib import Path

from scripts import footprint_parse as fp

_FIXTURES = Path(__file__).resolve().parent / "fixtures" / "footprint"


def _read(name: str) -> str:
    return (_FIXTURES / name).read_text()


class SmapsRollupTest(unittest.TestCase):
    def test_reads_resident_and_proportional_sizes(self) -> None:
        sizes = fp.parse_smaps_rollup(_read("smaps_rollup.txt"))
        self.assertEqual(sizes["rss_kb"], 69304)
        self.assertEqual(sizes["pss_kb"], 57567)

    def test_splits_proportional_size_into_anonymous_and_file_backed(self) -> None:
        sizes = fp.parse_smaps_rollup(_read("smaps_rollup.txt"))
        self.assertGreater(sizes["pss_anon_kb"], 0)
        self.assertGreater(sizes["pss_file_kb"], 0)

    def test_old_kernel_without_split_fields_leaves_them_blank(self) -> None:
        sizes = fp.parse_smaps_rollup("Rss:   10 kB\nPss:   8 kB\n")
        self.assertEqual(sizes["pss_kb"], 8)
        self.assertIsNone(sizes["pss_anon_kb"])


class HeapInfoTest(unittest.TestCase):
    def test_g1_reports_committed_and_used(self) -> None:
        heap = fp.parse_heap_info(_read("heap_info_g1.txt"))
        self.assertEqual(heap["heap_committed_kb"], 221184)
        self.assertEqual(heap["heap_used_kb"], 33022)

    def test_g1_reports_metaspace(self) -> None:
        heap = fp.parse_heap_info(_read("heap_info_g1.txt"))
        self.assertEqual(heap["metaspace_used_kb"], 78)
        self.assertEqual(heap["metaspace_committed_kb"], 320)

    def test_serial_sums_young_and_tenured_generations(self) -> None:
        heap = fp.parse_heap_info(_read("heap_info_serial.txt"))
        self.assertEqual(heap["heap_committed_kb"], 65728 + 146112)
        self.assertEqual(heap["heap_used_kb"], 35140 + 1073)

    def test_jdk_22_style_total_reserved_and_committed(self) -> None:
        text = " garbage-first heap   total reserved 4194304K, committed 262144K, used 12345K [0x0, 0x1)\n"
        heap = fp.parse_heap_info(text)
        self.assertEqual(heap["heap_committed_kb"], 262144)
        self.assertEqual(heap["heap_used_kb"], 12345)

    def test_unrecognised_collector_leaves_heap_blank(self) -> None:
        heap = fp.parse_heap_info("1234:\n ZHeap used 100M, capacity 200M\n")
        self.assertIsNone(heap["heap_used_kb"])


class NativeMemoryTest(unittest.TestCase):
    def test_reads_total_and_categories(self) -> None:
        nmt = fp.parse_nmt(_read("nmt_summary.txt"))
        self.assertEqual(nmt["nmt_total_committed_kb"], 291896)
        self.assertEqual(nmt["nmt_java_heap_kb"], 221184)
        self.assertEqual(nmt["nmt_code_kb"], 7592)
        self.assertEqual(nmt["nmt_thread_kb"], 821)
        self.assertEqual(nmt["nmt_class_kb"], 201)
        self.assertEqual(nmt["nmt_metaspace_kb"], 204)
        self.assertEqual(nmt["nmt_cds_kb"], 12912)

    def test_gc_includes_the_card_set_category(self) -> None:
        self.assertEqual(fp.parse_nmt(_read("nmt_summary.txt"))["nmt_gc_kb"], 46237 + 25)

    def test_thread_count(self) -> None:
        self.assertEqual(fp.parse_nmt(_read("nmt_summary.txt"))["threads"], 18)

    def test_absent_category_is_zero_not_blank(self) -> None:
        self.assertEqual(fp.parse_nmt(_read("nmt_summary.txt"))["nmt_other_kb"], 0)

    def test_everything_unnamed_lands_in_misc(self) -> None:
        nmt = fp.parse_nmt(_read("nmt_summary.txt"))
        named = sum(nmt[key] for key in fp.NMT_NAMED)
        self.assertEqual(nmt["nmt_misc_kb"], 291896 - named)

    def test_disabled_tracking_leaves_everything_blank(self) -> None:
        nmt = fp.parse_nmt(_read("nmt_disabled.txt"))
        self.assertIsNone(nmt["nmt_total_committed_kb"])
        self.assertIsNone(nmt["nmt_code_kb"])

    def test_missing_capture_leaves_everything_blank(self) -> None:
        self.assertIsNone(fp.parse_nmt("")["nmt_total_committed_kb"])


class RowTest(unittest.TestCase):
    def _row(self, nmt: str) -> dict[str, str]:
        line = fp.build_row(
            label="baseline",
            mode="gui",
            checkpoint="open_text",
            uptime_s=42,
            cpu_s=7.5,
            smaps=_read("smaps_rollup.txt"),
            heap=_read("heap_info_g1.txt"),
            nmt=nmt,
        )
        return next(csv.DictReader(io.StringIO(fp.HEADER + "\n" + line + "\n")))

    def test_row_matches_header_column_for_column(self) -> None:
        row = self._row(_read("nmt_summary.txt"))
        self.assertEqual(list(row.keys()), fp.COLUMNS)
        self.assertEqual(row["label"], "baseline")
        self.assertEqual(row["cpu_s"], "7.5")
        self.assertEqual(row["rss_kb"], "69304")
        self.assertEqual(row["heap_used_kb"], "33022")
        self.assertEqual(row["nmt_code_kb"], "7592")

    def test_anon_minus_nmt_is_the_gap_between_resident_anonymous_memory_and_committed_native(self) -> None:
        row = self._row(_read("nmt_summary.txt"))
        pss_anon = int(row["pss_anon_kb"])
        self.assertEqual(int(row["anon_minus_nmt_kb"]), pss_anon - 291896)

    def test_status_defaults_to_ok_and_can_carry_a_failure(self) -> None:
        self.assertEqual(self._row(_read("nmt_summary.txt"))["status"], "ok")
        line = fp.build_row("x", "gui", "start", 1, None, "", "", "", status="oom")
        self.assertEqual(next(csv.DictReader(io.StringIO(fp.HEADER + "\n" + line + "\n")))["status"], "oom")

    def test_row_without_nmt_has_blank_nmt_columns(self) -> None:
        row = self._row("")
        self.assertEqual(row["nmt_code_kb"], "")
        self.assertEqual(row["anon_minus_nmt_kb"], "")


if __name__ == "__main__":
    unittest.main()
