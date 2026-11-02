#!/usr/bin/env python3
"""Tests for scripts/present_abba_summary.py on CSVs shaped like the ones bench/present-abba.sh writes:

    python3 -m unittest scripts.test_present_abba_summary -v
"""

from __future__ import annotations

import contextlib
import io
import tempfile
import unittest
from pathlib import Path

from scripts import present_abba_summary as summary

PLAIN = "render.present.1080p_2x.typing_frame,20,100,1,1.0,{p50},3.0,4.0,1000,2000,"
HEADER = "Serenity performance benchmarks\ncontext,os,Linux\nname,iterations\n"


def _write(directory: str, name: str, row: str) -> None:
    Path(directory, name).write_text(HEADER + row + "\n")


def _summarise(directory: str) -> str:
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        summary.main(directory)
    return out.getvalue()


class PhasesTest(unittest.TestCase):
    def test_prints_the_median_of_each_phase_per_row_and_backend(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            for run, (plan, copy_out) in enumerate([(0.05, 1.0), (0.07, 1.4), (0.06, 1.2)], start=1):
                _write(directory, f"1080_2-skia-{run}.csv", PLAIN.format(p50=8.0) + f",{plan},0.0,0.0,{copy_out},6.0")
            _write(directory, "1080_2-java2d-1.csv", PLAIN.format(p50=2.0) + ",,,,,")
            text = _summarise(directory)
        self.assertIn("config | row | backend | plan ms | surface move ms | image shift ms | copy-out ms | rest ms", text)
        self.assertIn("1080_2 | typing_frame | skia | 0.06 | 0.00 | 0.00 | 1.20 | 6.00", text)

    def test_leaves_the_phase_table_out_for_csvs_without_phases(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            _write(directory, "1080_2-skia-1.csv", PLAIN.format(p50=8.0))
            text = _summarise(directory)
        self.assertNotIn("plan ms", text)
        self.assertIn("1080_2 | typing_frame | skia | 8.00", text)


if __name__ == "__main__":
    unittest.main()
