#!/usr/bin/env python3
"""Writes the startup.cold.first_frame and startup.warm.first_frame rows for a packaged Serenity image (#2420).

    bench/startup.py <launcher> <runs> [--discard N] [--interactive] [--csv PATH]

The rows use the PerformanceBenchmarks CSV layout, so check_perf_regression.py, compare_perf_runs.py and abba_compare.py
read them like any other benchmark. "Warm" starts the image with its JDK 25 AOT cache, "cold" with the cache moved
aside; the OS file cache is warm for both. Launches alternate warm, cold, warm, cold so drift falls on both rows alike.
The first --discard launches of each mode (default 1) are not in the rows. The number is written as a `context` line,
and so are the discarded times (`discarded_warm_ms`, `discarded_cold_ms`), so a first-launch penalty stays visible. Needs a display ($DISPLAY) and an image whose cache has been trained (scripts/train-aot-cache.sh).
--interactive launches with --startup-probe instead of --smoke-test: the app injects one key after its first frame and
prints SERENITY_PROBE_INTERACTIVE once that key is painted, and the same launch yields startup.*.first_frame and
startup.*.interactive. Exits non-zero, writing nothing, when a mode has no launch that reached a metric.
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


def measure(launcher, cache, hidden, runs, discard, interactive):
    metrics = ("first_frame", "interactive") if interactive else ("first_frame",)
    samples = {mode: {metric: [] for metric in metrics} for mode in ("warm", "cold")}
    discarded = {mode: {metric: [] for metric in metrics} for mode in ("warm", "cold")}
    for index in range(runs + discard):
        for mode in ("warm", "cold"):
            if mode == "cold":
                os.replace(cache, hidden)
            try:
                times = launch_times(launcher, interactive)
            finally:
                if mode == "cold":
                    os.replace(hidden, cache)
            for metric, value in times.items():
                (samples if index >= discard else discarded)[mode][metric].append(value)
    return samples, discarded


def launch_times(launcher, interactive):
    """{metric: ms} for the markers a successful launch printed; empty when it did not exit cleanly."""
    if not interactive:
        result = ttfw.run_once(launcher)
        return {} if result is None else {"first_frame": result[0]}
    seen, _, status = ttfw.launch(launcher, ttfw.PROBE_ARGS, (ttfw.READY, ttfw.INTERACTIVE))
    if status != 0:
        return {}
    names = {ttfw.READY: "first_frame", ttfw.INTERACTIVE: "interactive"}
    return {names[marker]: value for marker, value in seen.items()}


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("launcher")
    parser.add_argument("runs", type=int)
    parser.add_argument("--discard", type=int, default=1)
    parser.add_argument("--interactive", action="store_true")
    parser.add_argument("--csv")
    args = parser.parse_args(argv[1:])

    launcher = os.path.abspath(args.launcher)
    cache = os.path.join(os.path.dirname(os.path.dirname(launcher)), "lib", "app", "serenity.aot")
    if not os.path.isfile(cache):
        print("No AOT cache at %s; train the image first (scripts/train-aot-cache.sh)." % cache, file=sys.stderr)
        return 1

    samples, discarded = measure(launcher, cache, cache + ".hidden", args.runs, args.discard, args.interactive)
    empty = ["%s launch reached its %s" % (mode, metric) for mode in ("warm", "cold") for metric, values in samples[mode].items() if not values]
    if empty:
        print("No %s; no rows written." % " and no ".join(empty), file=sys.stderr)
        return 1

    lines = ["Serenity startup benchmarks", "context,discarded_runs,%d" % args.discard]
    for mode in ("warm", "cold"):
        for metric, values in discarded[mode].items():
            if values:
                suffix = "" if metric == "first_frame" else "_" + metric
                lines.append("context,discarded_%s%s_ms,%s" % (mode, suffix, ",".join("%.1f" % v for v in values)))
    lines.append(HEADER)
    lines += [row("startup.%s.%s" % (mode, metric), samples[mode][metric], args.discard)
              for mode in ("cold", "warm") for metric in samples[mode]]
    text = "\n".join(lines) + "\n"
    if args.csv:
        with open(args.csv, "w") as handle:
            handle.write(text)
    sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
