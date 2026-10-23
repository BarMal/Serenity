#!/usr/bin/env python3
"""Throwaway (#2380): time from launching a packaged Serenity image to its first painted frame.

    ttfw.py <launcher> <runs> <label>

Starts the launcher with --smoke-test --safe-mode, which prints SERENITY_SMOKE_READY after the first painted frame and
quits, and reports the wall time to that line and to process exit. Needs a display ($DISPLAY).
"""
import os
import statistics
import subprocess
import sys
import tempfile
import time

launcher, runs, label = os.path.abspath(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
ready, total = [], []
for _ in range(runs):
    with tempfile.TemporaryDirectory() as work:
        started = time.perf_counter()
        proc = subprocess.Popen([launcher, "--smoke-test", "--safe-mode"], cwd=work, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        seen = None
        for line in proc.stdout:
            if line.strip() == "SERENITY_SMOKE_READY" and seen is None:
                seen = time.perf_counter() - started
        proc.wait()
        done = time.perf_counter() - started
    if seen is not None and proc.returncode == 0:
        ready.append(seen * 1000)
        total.append(done * 1000)

def stats(values):
    return "median %.0f ms  min %.0f  max %.0f  (n=%d)" % (statistics.median(values), min(values), max(values), len(values)) if values else "no successful run"

print("TTFW %-34s first frame: %s" % (label, stats(ready)))
print("TTFW %-34s process exit: %s" % (label, stats(total)))
print("TTFW %-34s runs ms: %s" % (label, " ".join("%.0f" % v for v in ready)))
