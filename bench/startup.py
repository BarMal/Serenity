#!/usr/bin/env python3
"""Writes the startup.cold.first_frame and startup.warm.first_frame rows for a packaged Serenity image (#2420).

    bench/startup.py <launcher> <runs> [--discard N] [--csv PATH]

The rows use the PerformanceBenchmarks CSV layout, so check_perf_regression.py, compare_perf_runs.py and abba_compare.py
read them like any other benchmark. "Warm" starts the image with its JDK 25 AOT cache, "cold" with the cache moved
aside; the OS file cache is warm for both. Launches alternate warm, cold, warm, cold so drift falls on both rows alike.
The first --discard launches of each mode (default 1) are not timed, and the number is written as a `context` line
rather than hidden. Needs a display ($DISPLAY) and an image whose cache has been trained (scripts/train-aot-cache.sh).
Exits non-zero, writing nothing, when a mode has no launch that reached its first frame.
"""
import argparse
import math
import os
import sys

import ttfw

HEADER = ("name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,"
          "allocation_p50_bytes,allocation_p95_bytes,retained_bytes")


def percentile(samples, fraction):
    ordered = sorted(samples)
    index = math.ceil(max(0.0, min(1.0, fraction)) * len(ordered)) - 1
    return ordered[max(0, min(len(ordered) - 1, index))]


def row(name, samples, discarded):
    return "%s,%d,%d,1,%.7f,%.7f,%.7f,%.7f,,," % (
        name, len(samples), discarded, min(samples), percentile(samples, 0.50), percentile(samples, 0.95), max(samples))


def measure(launcher, cache, hidden, runs, discard):
    samples = {"warm": [], "cold": []}
    for index in range(runs + discard):
        for mode in ("warm", "cold"):
            if mode == "cold":
                os.replace(cache, hidden)
            try:
                result = ttfw.run_once(launcher)
            finally:
                if mode == "cold":
                    os.replace(hidden, cache)
            if result is not None and index >= discard:
                samples[mode].append(result[0])
    return samples


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("launcher")
    parser.add_argument("runs", type=int)
    parser.add_argument("--discard", type=int, default=1)
    parser.add_argument("--csv")
    args = parser.parse_args(argv[1:])

    launcher = os.path.abspath(args.launcher)
    cache = os.path.join(os.path.dirname(os.path.dirname(launcher)), "lib", "app", "serenity.aot")
    if not os.path.isfile(cache):
        print("No AOT cache at %s; train the image first (scripts/train-aot-cache.sh)." % cache, file=sys.stderr)
        return 1

    samples = measure(launcher, cache, cache + ".hidden", args.runs, args.discard)
    empty = [mode for mode, values in samples.items() if not values]
    if empty:
        print("No %s launch reached its first frame; no rows written." % " or ".join(empty), file=sys.stderr)
        return 1

    lines = ["Serenity startup benchmarks", "context,discarded_runs,%d" % args.discard, HEADER]
    lines += [row("startup.%s.first_frame" % mode, samples[mode], args.discard) for mode in ("cold", "warm")]
    text = "\n".join(lines) + "\n"
    if args.csv:
        with open(args.csv, "w") as handle:
            handle.write(text)
    sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
