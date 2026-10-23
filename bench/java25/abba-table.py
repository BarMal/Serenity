#!/usr/bin/env python3
"""Throwaway: per-row medians (p50, p95, allocation, retained) of A and B ABBA runs, with B/A ratios and p50 ranges."""
import statistics, sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))
from check_perf_regression import parse_csv

out = sys.argv[1]
def load(side):
    runs = []
    for i in range(1, 5):
        try:
            runs.append(parse_csv(f"{out}/{side}-{i}.csv"))
        except Exception as e:  # noqa
            print(f"# {side}-{i}: {e}", file=sys.stderr)
    return runs
a, b = load("a"), load("b")
names = sorted(set().union(*[r.keys() for r in a + b]))
def vals(runs, n, f):
    return [getattr(r[n], f) for r in runs if n in r and getattr(r[n], f) is not None]
def med(v): return statistics.median(v) if v else None
print("name | A p50 [min-max] | B p50 [min-max] | B/A p50 | B/A p95 | B/A alloc | B/A retained")
for n in names:
    ap, bp = vals(a, n, "p50_ms"), vals(b, n, "p50_ms")
    if not ap or not bp: continue
    def ratio(f):
        x, y = med(vals(a, n, f)), med(vals(b, n, f))
        return f"{y / x:.2f}" if x and y else "-"
    print(f"{n} | {med(ap):.4f} [{min(ap):.4f}-{max(ap):.4f}] | {med(bp):.4f} [{min(bp):.4f}-{max(bp):.4f}] | {ratio('p50_ms')} | {ratio('p95_ms')} | {ratio('allocation_p50_bytes')} | {ratio('retained_bytes')}")
