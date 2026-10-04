#!/usr/bin/env python3
"""Summarise the per-keystroke [LATENCY] lines `ui.render.latency_trace` writes to serenity.log.

Usage: python3 bench/latency-summary.py serenity.log [more.log ...]

Prints p50/p95/max per stage over every keystroke line, plus each stage's share of the summed p50s, so the stage
holding the unexplained time stands out. `when_to_edt` is wall-clock milliseconds between the OS event and the
event thread; `total` is monotonic from the event thread to the end of the paint, so the two add up to the
keystroke's in-app latency.
"""

import re
import sys

LINE = re.compile(r"\[LATENCY\] seq=\d+ (.*) ms$")
FIELD = re.compile(r"(\w+)=(-?[0-9.]+)")


def nearest_rank(sorted_values, percentile):
    rank = max(1, -(-len(sorted_values) * percentile // 1))
    return sorted_values[int(rank) - 1]


def main(paths):
    stages = {}
    order = []
    for path in paths:
        with open(path, encoding="utf-8", errors="replace") as log:
            for line in log:
                match = LINE.search(line.rstrip())
                if not match:
                    continue
                for name, value in FIELD.findall(match.group(1)):
                    if name not in stages:
                        stages[name] = []
                        order.append(name)
                    stages[name].append(float(value))

    if not order:
        print("No [LATENCY] keystroke lines found. Is ui.render.latency_trace = true?")
        return 1

    count = len(stages[order[0]])
    print(f"{count} keystrokes")
    print(f"{'stage':<18} {'p50':>8} {'p95':>8} {'max':>8} {'p50 share':>10}  (ms)")
    parts = [name for name in order if name != "total"]
    p50s = {name: nearest_rank(sorted(stages[name]), 0.50) for name in order}
    summed = sum(p50s[name] for name in parts) or 1.0
    for name in order:
        values = sorted(stages[name])
        share = "" if name == "total" else f"{100.0 * p50s[name] / summed:>9.1f}%"
        print(f"{name:<18} {p50s[name]:>8.2f} {nearest_rank(values, 0.95):>8.2f} {values[-1]:>8.2f} {share:>10}")
    return 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(2)
    sys.exit(main(sys.argv[1:]))
