#!/usr/bin/env python3
"""Summarises the CSVs `bench/present-abba.sh` writes: per config and row, the median across runs of each backend's p50
and p95, the spread of the p50s, the allocation per frame, and Skia's ratio to Java2D.

Usage:
    present_abba_summary.py <out-dir>
"""

from __future__ import annotations

import glob
import os
import statistics
import sys


def read(out_dir: str) -> dict[tuple[str, str, str], list[tuple[float, float, float]]]:
    rows: dict[tuple[str, str, str], list[tuple[float, float, float]]] = {}
    for path in sorted(glob.glob(os.path.join(out_dir, "*.csv"))):
        config, backend, _ = os.path.basename(path)[:-4].rsplit("-", 2)
        with open(path) as handle:
            for line in handle:
                if line.startswith("render.present"):
                    fields = line.strip().split(",")
                    row = fields[0].split(".", 3)[3]
                    rows.setdefault((config, row, backend), []).append(
                        (float(fields[5]), float(fields[6]), float(fields[8] or 0))
                    )
    return rows


def main(out_dir: str) -> None:
    rows = read(out_dir)
    print("config | row | backend | p50 ms median [min-max] | p95 ms median | alloc MB | runs")
    for config, row in sorted({(c, r) for c, r, _ in rows}):
        medians: dict[str, tuple[float, float]] = {}
        for backend in ("java2d", "skia"):
            runs = rows.get((config, row, backend), [])
            if not runs:
                continue
            p50s = [run[0] for run in runs]
            p95s = [run[1] for run in runs]
            medians[backend] = (statistics.median(p50s), statistics.median(p95s))
            allocated = statistics.median(run[2] for run in runs) / 1e6
            print(
                f"{config} | {row} | {backend} | {medians[backend][0]:.2f} [{min(p50s):.2f}-{max(p50s):.2f}]"
                f" | {medians[backend][1]:.2f} | {allocated:.2f} | {len(runs)}"
            )
        if len(medians) == 2:
            p50 = medians["skia"][0] / medians["java2d"][0]
            p95 = medians["skia"][1] / medians["java2d"][1]
            print(f"{config} | {row} | skia / java2d | p50 {p50:.2f}x | p95 {p95:.2f}x")


if __name__ == "__main__":
    main(sys.argv[1])
