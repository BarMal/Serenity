# Job 6 — Scrollbar overlay A/B

**base**: `origin/master` at `66ff3735`
**head**: `ccr-157ed55f-u6khhf-scrollbar-overlay` at `d8e9f44c`

8-run ABBA sequence (base, head, head, base, base, head, head, base), each run with a
5-second-interval memory sampler and a pre-run conditions line (load, package temp, avg CPU
MHz, AC status). **Run entirely on battery power** (CPU averaging ~800-900MHz, occasionally
up to ~1.7-1.8GHz) — not comparable in absolute terms to AC-powered runs elsewhere in this
session, though the base-vs-head comparison within this run is internally valid since both
sides saw the same conditions. See `conditions.txt` for the full per-run environment log.

Two runs needed a retry (documented in the main session transcript): run 4 died at startup
and retried clean; run 8 died mid-benchmark on its first two attempts (a different,
unexplained failure signature than the usual quick-death-at-startup pattern — no OOM, no
crash trace, no PSI memory pressure) and succeeded on the third.

## Results (p50_ms, median of 4 runs per side)

| benchmark | base median | head median | ratio (head/base) |
|---|---|---|---|
| typing_frame_2x | 3.755 | 3.737 | 0.995 (even) |
| typing_frame_2x.chapter_ghosts | 2.693 | 2.669 | 0.991 (even) |
| typing_frame_2x.ereader_railed | 4.857 | 4.911 | 1.011 (even) |
| typing_frame_2x.ereader_railed_ghosts | 4.283 | 4.188 | 0.978 (even) |
| typing_frame_2x.ligature_font | 7.820 | 9.011 | 1.152 (noisy — high variance both sides) |
| **scroll_frame_2x** | **8.696** | **9.049** | **1.041 — head ~4% slower, the one real signal** |
| prose_frame_2x | 10.700 | 10.729 | 1.003 (even) |
| state_manager.typing_random_letters | 2.752 | 2.050 | 0.745 (very noisy — base itself varies 2.18-3.73 across its 4 runs) |

**Allocation** (p50 bytes, avg of 4 runs/side): `typing_frame_2x` base ≈449,618B vs head
≈471,752B (head **+4.9%**); `scroll_frame_2x` base ≈822,988B vs head ≈816,106B (even).

**Footprint (`retained_bytes`)** — identical across every single run on both builds:
`empty_editor`=1,408B · `plain_document_15k_lines`=1,340,408B · `rich_document`=6,793,640B ·
`after_1000_edits`=1,380,904B · `ten_buffers`=13,392,096B. No retained-memory regression.

**Memory sampler**: base avg peak-used ≈10.6GB / min-available ≈5.15GB / peak Java RSS
≈3.84GB; head avg peak-used ≈11.1GB / min-available ≈4.66GB / peak RSS ≈3.96GB — head runs
slightly hotter, consistent with the allocation bump above.

**Reading**: mostly noise-level differences. The one modest, consistent signal is
`scroll_frame_2x` (+4% p50 on head) — plausible given the overlay has to draw on every
scroll frame. `typing_frame_2x` itself is flat (unlike an earlier same-vintage comparison
against an older base commit, which showed +11% — the newer scrollbar-overlay commits may
have optimized the typing path since then).

## Data files

- `conditions.txt` — per-run environment (time, load, package temp, avg MHz, AC status)
- `base-{1,4,5,8}.csv`, `head-{2,3,6,7}.csv` — full benchmark output per run (97 rows each,
  columns: `name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes,retained_bytes`)
