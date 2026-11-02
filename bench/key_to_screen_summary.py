#!/usr/bin/env python3
"""Summarises key-to-screen timing from benchmark run files, per backend.

Reads the tagged `[LATENCY] summary` and `[FRAME]` lines (`ui.render.latency_trace`, `ui.render.frame_timing`) that
bench-laptop.sh keeps, takes the median of each run's windows, then the median of the runs of each backend.

    python3 -I bench/key_to_screen_summary.py bench/bench-results/<label>-run*.txt
"""

import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from statistics import median
from typing import Dict, List, Optional

LATENCY = re.compile(
    r"\[LATENCY\] summary backend=(\S+) presented=(\S+) window=\S+ keys=(\d+) unpainted=(\d+) \|.*?"
    r"\btotal p50=([\d.]+) p95=([\d.]+)"
)
FRAME = re.compile(r"\[FRAME\] backend=(\S+) .*?\binput-to-paint p50=([\d.]+) p95=([\d.]+)")

PRESENTED_NOTE = (
    "presented: paint-end is the end of the canvas paint; render-callback-end is the end of Skiko's render callback "
    "(the Skia layer), not GPU completion and not display scan-out."
)


@dataclass(frozen=True)
class LatencyWindow:
    backend: str
    presented: str
    keys: int
    unpainted: int
    p50: float
    p95: float


@dataclass(frozen=True)
class FrameWindow:
    backend: str
    p50: float
    p95: float


@dataclass(frozen=True)
class BackendResult:
    presented: str
    runs: int
    keys: int
    unpainted: int
    p50: float
    p95: float
    input_to_paint_p50: Optional[float]
    input_to_paint_p95: Optional[float]


def parse_latency(line: str) -> Optional[LatencyWindow]:
    match = LATENCY.search(line)
    if not match:
        return None
    backend, presented, keys, unpainted, p50, p95 = match.groups()
    if int(keys) == 0:
        return None
    return LatencyWindow(backend, presented, int(keys), int(unpainted), float(p50), float(p95))


def parse_frame(line: str) -> Optional[FrameWindow]:
    match = FRAME.search(line)
    if not match:
        return None
    backend, p50, p95 = match.groups()
    return FrameWindow(backend, float(p50), float(p95))


def summarise(runs: Dict[str, List[str]]) -> Dict[str, BackendResult]:
    latency: Dict[str, Dict[str, List[LatencyWindow]]] = defaultdict(lambda: defaultdict(list))
    frames: Dict[str, Dict[str, List[FrameWindow]]] = defaultdict(lambda: defaultdict(list))
    for name, lines in runs.items():
        for line in lines:
            window = parse_latency(line)
            if window:
                latency[window.backend][name].append(window)
            frame = parse_frame(line)
            if frame:
                frames[frame.backend][name].append(frame)
    result = {}
    for backend, per_run in latency.items():
        windows = [w for run in per_run.values() for w in run]
        frame_runs = frames.get(backend, {})
        result[backend] = BackendResult(
            presented=windows[0].presented,
            runs=len(per_run),
            keys=sum(w.keys for w in windows),
            unpainted=sum(w.unpainted for w in windows),
            p50=median(median(w.p50 for w in run) for run in per_run.values()),
            p95=median(median(w.p95 for w in run) for run in per_run.values()),
            input_to_paint_p50=(
                median(median(w.p50 for w in run) for run in frame_runs.values()) if frame_runs else None
            ),
            input_to_paint_p95=(
                median(median(w.p95 for w in run) for run in frame_runs.values()) if frame_runs else None
            ),
        )
    return result


def table(result: Dict[str, BackendResult]) -> str:
    def cell(value: Optional[float]) -> str:
        return "-" if value is None else f"{value:.2f}"

    lines = [
        "Key to screen (ms; median of each run's windows, then median of runs)",
        f"{'backend':<12} {'presented':<20} {'runs':>4} {'keys':>6} {'unpainted':>9} "
        f"{'total p50':>10} {'p95':>8} {'input-to-paint p50':>19} {'p95':>8}",
    ]
    for backend in sorted(result):
        r = result[backend]
        lines.append(
            f"{backend:<12} {r.presented:<20} {r.runs:>4} {r.keys:>6} {r.unpainted:>9} "
            f"{cell(r.p50):>10} {cell(r.p95):>8} {cell(r.input_to_paint_p50):>19} {cell(r.input_to_paint_p95):>8}"
        )
    lines.append(PRESENTED_NOTE)
    return "\n".join(lines)


def main(paths: List[str]) -> int:
    runs = {}
    for path in paths:
        with open(path, encoding="utf-8", errors="replace") as handle:
            runs[path] = handle.read().splitlines()
    result = summarise(runs)
    if not result:
        print("No tagged [LATENCY] summary with keystrokes in the given files.", file=sys.stderr)
        return 1
    print(table(result))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
