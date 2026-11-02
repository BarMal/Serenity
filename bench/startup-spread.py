#!/usr/bin/env python3
"""Pools the `runs ms:` lines that bench/ttfw.py prints and reports the spread per label.

    bench/startup-spread.py <ttfw-output> [<ttfw-output> ...]

Each file is the stdout of one or more ttfw.py invocations. Runs with the same label are pooled across files, so rounds
of the same mode that were interleaved with other modes (to spread machine drift over all of them) read as one sample.
The coefficient of variation and the slowest-over-fastest ratio are what decide whether a start-up row can gate on a
runner or only be tracked as a trend. Exits non-zero when no file held any runs.
"""
import re
import statistics
import sys

RUNS_LINE = re.compile(r"^TTFW (\S+)\s+runs ms: ([\d.\s]+)$")


def pooled(paths):
    runs = {}
    for path in paths:
        with open(path) as handle:
            for line in handle:
                match = RUNS_LINE.match(line.rstrip("\n"))
                if match:
                    runs.setdefault(match.group(1), []).extend(float(v) for v in match.group(2).split())
    return {label: values for label, values in runs.items() if values}


def describe(label, values):
    median = statistics.median(values)
    cv = statistics.pstdev(values) / statistics.mean(values) * 100
    return "%-12s n=%-3d median %.0f ms  cv %.1f%%  max/min %.2fx  min %.0f  max %.0f" % (
        label, len(values), median, cv, max(values) / min(values), min(values), max(values))


def main(argv):
    runs = pooled(argv[1:])
    if not runs:
        print("no runs found in the given files", file=sys.stderr)
        return 1
    for label in sorted(runs):
        print(describe(label, runs[label]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
