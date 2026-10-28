#!/usr/bin/env python3
"""Time from launching a packaged Serenity image to its first painted frame.

    bench/ttfw.py <launcher> <runs> <label> [warm-up-runs]

Starts the launcher with --smoke-test --safe-mode, which prints SERENITY_SMOKE_READY after the first painted frame and
quits, and reports the wall time to that line and to process exit. The warm-up runs (default 1) are not counted: they
let an AppCDS image write its archive and let the OS cache the files, so the timed runs compare like with like. Needs
a display ($DISPLAY). Exits non-zero when no timed run reached the first frame.
"""
import os
import statistics
import subprocess
import sys
import tempfile
import time


READY = "SERENITY_SMOKE_READY"
INTERACTIVE = "SERENITY_PROBE_INTERACTIVE"
SMOKE_ARGS = ("--smoke-test", "--safe-mode")
PROBE_ARGS = ("--startup-probe", "--safe-mode")


def launch(launcher, args, markers):
    """Runs the launcher and returns ({marker: ms to its first stdout line}, ms to exit, exit status)."""
    with tempfile.TemporaryDirectory() as work:
        started = time.perf_counter()
        proc = subprocess.Popen([launcher, *args], cwd=work, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        seen = {}
        for line in proc.stdout:
            marker = line.strip()
            if marker in markers and marker not in seen:
                seen[marker] = (time.perf_counter() - started) * 1000
        proc.wait()
        done = (time.perf_counter() - started) * 1000
    return seen, done, proc.returncode


def run_once(launcher):
    seen, done, status = launch(launcher, SMOKE_ARGS, (READY,))
    return (seen[READY], done) if READY in seen and status == 0 else None


def describe(values):
    if not values:
        return "no successful run"
    return "median %.0f ms  min %.0f  max %.0f  (n=%d)" % (statistics.median(values), min(values), max(values), len(values))


def main(argv):
    launcher, runs, label = os.path.abspath(argv[1]), int(argv[2]), argv[3]
    warm_up = int(argv[4]) if len(argv) > 4 else 1
    for _ in range(warm_up):
        run_once(launcher)
    timed = [r for r in (run_once(launcher) for _ in range(runs)) if r is not None]
    ready = [r[0] for r in timed]
    print("TTFW %-34s first frame: %s" % (label, describe(ready)))
    print("TTFW %-34s process exit: %s" % (label, describe([r[1] for r in timed])))
    print("TTFW %-34s runs ms: %s" % (label, " ".join("%.0f" % v for v in ready)))
    return 0 if ready else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
