# Job 7 — JVM GC flags A/B

Same code throughout (`master` at `66ff3735`), no code change — only JVM flags differ.

- **Variant A** (baseline, matches the shipped desktop-package flags):
  `-XX:+UseG1GC -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000`
- **Variant B**: baseline plus `-XX:MaxHeapFreeRatio=30 -XX:MinHeapFreeRatio=10`, with
  `-XX:G1PeriodicGCInterval=30000` replacing the 60000 interval

Run entirely on battery power (same session as Job 6). Both parts below use the same two
variants; Part 1 is the custom `PerformanceBenchmarks` harness (8-run ABBA), Part 2 is the
real packaged-app footprint probe (`scripts/measure-footprint.sh` from
`ccr-157ed55f-u6khhf-mem-probe`, 3 runs per variant).

## Part 1 — rendering/input timing (8-run ABBA: A,B,B,A,A,B,B,A)

| benchmark | A median | B median | ratio (B/A) |
|---|---|---|---|
| typing_frame_2x | 3.791 | 3.889 | 1.026 (even) |
| typing_frame_2x.chapter_ghosts | 2.480 | 2.608 | 1.052 (even/slightly slower) |
| typing_frame_2x.ereader_railed | 4.900 | 4.755 | 0.971 (even) |
| typing_frame_2x.ereader_railed_ghosts | 4.235 | 4.328 | 1.022 (even) |
| typing_frame_2x.ligature_font | 10.901 | 8.078 | 0.741 (noisy — not reliable) |
| scroll_frame_2x | 8.827 | 8.872 | 1.005 (even) |
| prose_frame_2x | 10.794 | 10.552 | 0.978 (even) |
| state_manager.typing_random_letters | 2.280 | 3.313 | 1.453 (noisy row again — not reliable) |

**Allocation** (p50 bytes): `typing_frame_2x` B +3.0%, `scroll_frame_2x` B +2.5% — both minor.

**Footprint (`retained_bytes`)**: essentially identical across all 8 runs/both variants (one
single-run +72 byte blip on `after_1000_edits`, otherwise byte-identical).

**Memory sampler**: A avg peak-used ≈9.42GB / min-available ≈6.32GB / peak RSS ≈3.66GB;
B avg peak-used ≈9.18GB / min-available ≈6.56GB / peak RSS ≈3.43GB — **B runs with ~6% lower
peak RSS** during the benchmark itself, consistent with the more aggressive heap-shrinking
flags doing their job. No GC pause info was printed by the harness in any run.

## Part 2 — packaged-app footprint (`measure-footprint.sh`, 3 runs per variant)

| checkpoint | A avg RSS | A avg PSS | B avg RSS | B avg PSS |
|---|---|---|---|---|
| idle | 769,252 kB | 740,151 kB | 445,543 kB | 416,461 kB |
| open_markdown (after opening a 15k-line text file + a markdown file) | 769,365 kB | 740,261 kB | 479,324 kB | 450,245 kB |

**Variant B uses ~40% less resident memory than Variant A**, both idle and after opening
files — far larger than the ~6% difference seen in Part 1's harness (which measures total
system memory, not just this process). The committed heap explains it directly: A's
`heap_committed_kb` sits at 410-465MB throughout every checkpoint, while B's drops to
108-162MB — the more aggressive `MaxHeapFreeRatio=30`/`MinHeapFreeRatio=10` plus
twice-as-frequent periodic GC (30s vs 60s) actively returns unused heap to the OS rather than
holding onto it. The trade-off: B's cumulative CPU time at idle averaged ~59.8s vs A's
~54.0s — modestly more CPU spent collecting, in exchange for dramatically less memory held.
All 6 runs captured cleanly (`status=ok` throughout, no OOM, no dead processes).

## Conclusion

The proposed GC flag change has no meaningful cost to rendering or input latency, but
delivers a large, real reduction in resident memory at the packaged-app level for a modest
CPU trade-off — worth adopting if memory footprint matters more than the small extra GC
overhead.

## Data files

- `conditions.txt` — per-ABBA-run environment (time, load, package temp, avg MHz, AC status)
- `A-{1,4,5,8}.csv`, `B-{2,3,6,7}.csv` — Part 1 benchmark output per ABBA run (same column
  format as Job 6's CSVs)
- `footprint.csv` — Part 2 raw output from `measure-footprint.sh` (24 rows: 2 variants ×
  3 runs × 4 checkpoints `start/open_text/open_markdown/idle`; columns documented in its
  header, include RSS/PSS/heap/metaspace/NMT breakdown and process status)
