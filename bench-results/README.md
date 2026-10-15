# Benchmark results — Jobs 5, 6, 7 (2026-10-14/15)

Ad-hoc benchmarking session run directly on the owner's laptop (Intel i7-8550U, 8 threads,
NixOS, Hyprland/Wayland via XWayland), confirming the white-command-runner fix and
A/B-testing two performance-relevant changes. Each job's directory has a `README.md` with
the full findings plus the raw CSV/log data behind them.

## [Job 5 — white command-runner fix](job5-runner-fix/README.md)

Confirms `ccr-157ed55f-u6khhf-runner-fix` (commit `97d818eb`) fixes the command-runner/modal
white-block bug seen on `master`. Root cause per the fix's own commit message: accessibility
proxies painting an opaque back buffer over the directly-presented canvas. Verified across
command runner, find/replace, menu tooltips, and resize/minimize/restore — no regressions.

## [Job 6 — scrollbar overlay A/B](job6-scrollbar-ab/README.md)

`origin/master` (`66ff3735`) vs `ccr-157ed55f-u6khhf-scrollbar-overlay` (`d8e9f44c`), 8-run
ABBA sequence. The overlay adds a real, modest cost to `scroll_frame_2x` (+4% p50, roughly
+5% typing-frame allocation); everything else is noise-level. No footprint/retained-memory
regression. **Run on battery power** — timings aren't comparable to AC-powered runs
elsewhere in this session, though the A/B comparison itself is valid.

## [Job 7 — JVM GC flags A/B](job7-jvm-flags-ab/README.md)

Same code (`master` `66ff3735`), two JVM flag sets, no code change. Variant B
(`MaxHeapFreeRatio=30`/`MinHeapFreeRatio=10`/30s periodic GC) costs essentially nothing in
rendering/input latency but cuts the packaged app's resident memory by **~40%** (idle RSS
~769MB → ~446MB) for a modest extra-CPU trade-off. Worth adopting if memory footprint
matters more than the small GC overhead.

## Known issue during this session

Six background benchmark runs were killed unexpectedly during the session (quick, silent
stops with no OOM/crash/idle-suspend signature — see the main session transcript for full
diagnosis). Five resolved cleanly on a single retry with no data loss; one (Job 6 run 8)
needed two retries. All full 8-run ABBA sequences for Jobs 6 and 7 completed successfully in
the end, as reflected in the reports above.
