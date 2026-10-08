# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The cursor-only scenario opens a temporary Swing window so it can measure Serenity's real overlay publication path. Run it in a graphical session; a headless Linux environment can use `xvfb-run -a sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"`.

The `iterations` column is the number of samples actually taken (see "Run-to-run stability audit" below), and the millisecond columns carry seven decimals so a 10 ns benchmark is not rounded to a multiple of 10 ns.

The harness prints CSV rows with `min_ms`, `p50_ms`, `p95_ms`, `max_ms`, `allocation_p50_bytes`, and `allocation_p95_bytes`. Allocation columns are populated for the long measured-line scenario and are sampled separately from timing so the allocation probe does not distort p50/p95. Every scenario builds its immutable document, state, search, LSP, and project-task fixtures before timing, runs the measured operation once, and asserts its observable result before warmup. Java2D frame drawing, reusable backing-buffer acquisition, cursor-overlay composition, and repaint requests remain inside their timed paths because they are part of the user-visible work being measured.

Scenarios cover:

- large JSON rope search and cursor-offset lookup
- visible multiline layout, normal editing, and deep plain/rich-text scrolling reducers
- real Java2D full frames, cursor-overlay composition, diagnostics/comments, and HiDPI buffers
- long measured single-line Java2D rendering, including proportional caret placement and run construction
- authoritative-scene reuse for cursor-only Java2D overlays
- large find/replace result-set presentation and complete find-query updates, including grapheme filtering, offset-to-position conversion, and selected-result application
- LSP frame decoding and project-task detection/terminal preparation
- Markdown preview and inline-lens rendering
- `command_runner.*`: one keystroke and one arrow press in the command palette and the settings view, each measured as
  the reducer step plus the renderer's read of the result, against a 720-family font catalogue
- visible animation tick advancement
- `laptop.*`: the editor at a real laptop's size (1500x1000 logical at 2x, word-wrapped lorem ipsum, default config):
  full frames with and without a Frosted pinned panel, keystrokes applied through a live `StateManager`, and Swing
  presenting into a 1500x1000 window. The window needs a screen at least that large, so run it with
  `xvfb-run -a -s "-screen 0 1920x1200x24"` (at 2x, `-screen 0 3840x2400x24 -dpi 192` and `-Dsun.java2d.uiScale=2`).
- `laptop.present.swing_fresh_frame_synced_1500x1000`: a full-window present of a device-size frame changed since
  it was last shown, as every rendered frame is, timed until the X server has finished it (`Toolkit.sync`). It
  replaced `swing_paint_window_1500x1000`, which re-presented one untouched image: Java2D caches such an image as an
  X pixmap, so that benchmark skipped the upload a real frame pays and timed only how far the client ran ahead of
  Xvfb -- bimodal within one run (0.02 vs 0.4 ms on CI), and a 7x p50 spread across five local runs.

- `laptop.input.state_manager.typing_random_letters`: seeded-random letters (about one in six a space) typed at the
  prose cursor, so nearly every keystroke misses the wrap cache that `continuous_typing`'s cyclic a..z and
  `type_and_delete`'s re-typed text hit.
- `laptop.input.state_manager.typing_long_paragraph`: the same typing into one 4000-character paragraph with no
  newlines, to show per-keystroke cost against paragraph length.

Pass name prefixes to run only some scenarios, e.g. `sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks laptop."`.

## Profiling the keystroke path

`com.serenity.perf.TypingProfile` (test scope, not run by CI) drives one keystroke scenario (random typing, long
paragraph, cold paced typing, move, page) for long enough to profile with JFR and prints p50/p95/mean per event;
`bench/jfr-aggregate.py` turns the recording into phase shares. Commands are in `bench/README.md`.

## Frame timing in the running app

Set `ui.render.frame_timing = true` in `~/.serenity/config.conf` and the app logs a `[FRAME]` line to
`~/.serenity/serenity.log` every 5 seconds: frame counts and rates, then p50/p95/max per stage -- `input-queue`,
`input-apply`, `render-wait`, `sync`, `render` (sync included), `paint-wait`, `paint`, and `input-to-paint`, the time
from an input arriving to the end of the first paint that shows it. The `laptop.*` benchmarks measure the same stages,
so a laptop log and a CI run can be compared stage by stage.

This remains a manual comparison tool for local before/after investigation. CI additionally runs this harness on every
push/PR and gates on it: `scripts/check_perf_regression.py` compares the run's p50 against a stored baseline and
fails the build when a benchmark both exceeds the ratio threshold (default 2x) and moves by more than an absolute
p50 delta floor (default 0.05ms) -- see that script's module docstring for the full rationale, including why both
guards are required together.

## Iteration-count derivation: false-positive audit -- 2026-09-18

An audit of CI's regression gate (`scripts/check_perf_regression.py`, #1453/#1503) found five spurious "regression"
flags across recent runs, all on benchmarks in the `reducer.*`, `damage.*`, `lsp.framer.large_batch`, and
`render.markdown.inline_lens` families -- the CI job's own harness runs are the only historical p50 data this
project retains (the raw per-run CSVs are not archived beyond CI's artifact retention window), so this derivation
instead measures the same effect directly: twelve local `PerformanceBenchmarks` runs under `xvfb-run`, on the same
class of shared, low-core-count CPU the CI runner uses -- six at each family's previous iteration count, six at the
candidate count -- comparing each scenario's own run-to-run p50 coefficient of variation (CV) and max/min spread.

Before (previous counts: `reducer.*` 20, `damage.*` 30, `lsp.framer.large_batch` 12, `render.markdown.inline_lens` 8):

| Scenario | mean p50 (ms) | CV | max/min spread |
| --- | ---: | ---: | ---: |
| `reducer.multi_cursor_move` (worst in family) | 0.084 | 23.72% | 1.74x |
| `damage.single_char_short_line.rows` (worst in family) | 0.005 | 6.99% | 1.23x |
| `lsp.framer.large_batch` | 0.826 | 6.43% | 1.22x |
| `render.markdown.inline_lens` | 1.374 | 1.46% | 1.04x |

After (candidate counts: `reducer.*` 60, `damage.*` 60, `lsp.framer.large_batch` 48, `render.markdown.inline_lens` 24):

| Scenario | mean p50 (ms) | CV | max/min spread |
| --- | ---: | ---: | ---: |
| `reducer.normal_editing` (worst in family; `multi_cursor_move` fell to 0.58% CV / 1.02x) | 0.014 | 11.59% | 1.38x |
| `damage.single_char_short_line.cells` (worst in family) | 0.007 | 2.85% | 1.10x |
| `lsp.framer.large_batch` | 0.760 | 5.25% | 1.16x |
| `render.markdown.inline_lens` | 1.396 | 1.83% | 1.06x |

None of the twelve runs, at either the old or new counts, pushed any scenario's max/min spread to 2x on its own --
these are the *worst-case within-family* scenarios, not proof that a real baseline-vs-current comparison could never
land two unlucky draws far enough apart to clear the ratio gate. What the data shows is that the most volatile
scenarios got markedly less volatile (`reducer.multi_cursor_move`'s 23.7% CV, traced to
`BenchmarkRunner.calibrate`'s batch-size doubling locking in an unlucky batch size for an entire run, fell to 0.58%),
while a few small-absolute-magnitude scenarios (`reducer.normal_editing` at ~0.014ms mean) still show real
percentage swings that a 3x-4x iteration bump does not fully remove -- their absolute swings stay in the thousandths
of a millisecond, which is exactly what `check_perf_regression.py --min-regression-delta-ms` (default 0.05ms) exists
to absorb. The two changes are a matched pair: iteration counts reduce how often a scenario's own noise gets close
to the ratio gate, and the absolute-delta floor stops whatever noise remains from being read as a regression on
benchmarks too small in absolute terms for a ratio to mean anything. Full per-family reasoning, including why
`damage.*` and `render.markdown.inline_lens` use one count for their whole family while `reducer.*`'s ten scenarios
needed a single shared count sized to their worst case, is in
`src/test/scala/com/serenity/perf/BenchmarkIterationCounts.scala`.

**CI job runtime impact:** the bump adds the extra iterations' own p50 cost across all touched scenarios -- computed
from the "before" table above, roughly 0.5s of pure added computation. Measured end-to-end (the `Test/runMain`
step's own wall-clock time, six runs each side, JIT/sbt already warm): before averaged ~31s/run, after averaged
~39s/run, so call it a ~8s increase to that one CI step, comfortably inside the run-to-run noise the ~4-minute perf
job already has from checkout, dependency resolution, and Xvfb setup on shared runners.

## `layout.large_multiline.visible_viewport`: issue #1586 investigation -- 2026-09-20

Issue #1586 reports the same spurious-regression pattern #1576 fixed for `reducer.*`/`damage.*`/`lsp.framer.large_batch`/
`render.markdown.inline_lens`, now on `layout.large_multiline.visible_viewport`, which flagged a false "regression"
on two unrelated PRs in one session (PR #1583: 2.19ms -> 4.41ms, 2.01x; PR #1585: 2.19ms -> 4.47ms, 2.04x; both
re-ran clean, a third instance was seen on PR #1580, and a fourth (2.19ms -> 4.64ms, 2.12x) on PR #1601 while this
investigation was in progress). Applying #1576's own methodology to this benchmark:

**The absolute-delta floor from #1576 does not cover this benchmark, and cannot be tuned to cover it without
weakening the gate elsewhere.** `check_perf_regression.py --min-regression-delta-ms` is a single global floor
(default 0.05ms) applied to every benchmark's ratio-flagged delta identically -- there is no per-benchmark or
per-family override in that script. It was sized against the four families audited in #1576, whose baselines sit at
low-single-digit milliseconds or below and whose run-to-run jitter is on the order of tens to a couple hundred
microseconds, so 50us sits comfortably above their noise band while still catching a real regression. But
`layout.large_multiline.visible_viewport`'s own baseline (2.19ms -- see `PerformanceBenchmarks.scala`'s
`layout.large_multiline.visible_viewport` scenario) is on the same order of magnitude as the #1576 families'
baselines, and its *observed jitter* in issue #1586 was 2.22ms and 2.28ms -- more than 40x the 0.05ms floor. Raising
the floor high enough to absorb that would raise it for every other benchmark sharing the same global argument, most
of which have no evidence of needing it, and would blunt the gate's ability to catch a real multi-millisecond
regression on any of them. That is not a fix; it is trading one false-positive source for a worse false-negative
risk gate-wide. The floor guard is therefore not the applicable tool here -- unlike the #1576 families, this one
needs the other half of that methodology: a higher iteration count for this specific scenario.

**Iteration-count derivation -- 2026-09-20, 2nd pass.** A prior pass of this investigation could not run the twelve-run
`sbt`/`xvfb-run` comparison because that sandbox had no `sbt` binary and no network path to install one. This pass's
sandbox had both, so the same process `BenchmarkIterationCounts.scala` documents for the four #1576 families was run
here: local `PerformanceBenchmarks` runs under `xvfb-run`, comparing this scenario's own run-to-run p50 CV and
max/min spread at the current count (20) against a candidate (60, matching the `reducer.*`/`damage.*` multiplier).
The sample size is four runs per side, not six -- smaller than #1576's, for time budget reasons -- so treat the
numbers below as directionally consistent with, rather than as precise as, that derivation.

| Config | p50 samples (ms) | mean p50 | CV | max/min spread |
| --- | --- | ---: | ---: | ---: |
| `iterations = 20` (previous) | 1.909, 2.186, 1.978, 1.890 | 1.991 | 5.91% | 1.157x |
| `iterations = 60` (adopted) | 1.882, 1.895, 2.050, 1.898 | 1.931 | 3.58% | 1.090x |

A real but partial improvement, the same shape #1576 found for `lsp.framer.large_batch` (6.43% CV / 1.22x spread ->
5.25% CV / 1.16x spread). As with the four-family derivation, neither side of this four-run comparison reproduced a
2x-plus run-to-run spread locally, despite CI itself observing exactly that on four separate PRs -- this is not proof
the bump eliminates the flag outright, only that it makes the scenario's own noise markedly less volatile, consistent
with #1576's finding that local runs under-represent a shared CI runner's noise.

Several runs at both counts showed `p95`/`max` spike sharply out of proportion to `p50` -- one 60-iteration run hit an
11.66ms max against a ~2ms p50 -- which is consistent with GC-pause sensitivity: `TextLayoutSnapshot.fromBuffer`'s
allocation for a 15,000-line deep-scrolled buffer is large enough that a GC cycle landing mid-sample can corrupt
whichever rank it hits. More iterations move the odds of a pause landing on the *median* rank down (30-31st of 60
vs. 10-11th of 20), which is the most likely mechanism behind the CV/spread improvement above, and is also why the
tail (`p95`/`max`) stayed noisy while `p50` tightened.

**Change landed:** `layout.large_multiline.visible_viewport` now runs at `BenchmarkIterationCounts.LayoutVisibleViewport`
(60), with a `BenchmarkIterationsSpec.scala` bound test alongside the other four families' lock-ins.

**If CI still flags this benchmark after the bump:** per the mechanism above, the next thing to check is allocation
pressure from `TextLayoutSnapshot.fromBuffer` specifically -- the harness's existing allocation-sampling pass (see
`BenchmarkRunner.AllocationTracked`) could confirm whether this scenario is more GC-sensitive than the others, which
would point toward reducing its allocation rather than further raising the iteration count.

## Repeatable before/after workflow

1. Close CPU-intensive applications and use the same power and display-scale settings for both captures.
2. Run the command above once and discard that output if dependencies or classes were cold.
3. Run it again, save the CSV output with the commit SHA, and compare p50 and p95 for like-named scenarios.
4. Record the printed `context` rows with the results. Treat changes as signals for investigation, not pass/fail thresholds.

## Baseline: 2026-07-21

Captured on x86_64 Linux 5.15.153.1-microsoft-standard-WSL2, AMD Ryzen 5 5600X 6-Core Processor (12 available processors), Microsoft OpenJDK Runtime 21.0.8+9-LTS. Times are milliseconds.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `rope.large_json.search` | 12.789 | 14.422 |
| `rope.large_json.cursor_offset` | 0.693 | 0.864 |
| `layout.large_multiline.visible_viewport` | 4.292 | 6.355 |
| `render.full_frame.java2d` | 11.284 | 15.223 |
| `render.cursor_only.java2d_overlay` | 5.158 | 10.270 |
| `render.diagnostics_and_comments.java2d` | 7.169 | 10.973 |
| `render.hidpi_frame.java2d` | 10.277 | 20.254 |
| `reducer.normal_editing` | 5.144 | 10.683 |
| `reducer.deep_scroll.plain` | 0.006 | 0.048 |
| `reducer.deep_scroll.rich_text` | 0.011 | 0.036 |
| `find_replace.large_result_set` | 0.255 | 0.460 |
| `lsp.framer.large_batch` | 3.864 | 5.803 |
| `project_task.responsiveness` | 0.025 | 0.028 |
| `markdown.preview.window_mapping` | 1.058 | 2.602 |
| `markdown.preview.html_fragment` | 0.523 | 0.550 |
| `render.markdown.inline_lens` | 10.894 | 13.942 |
| `animation.large_visible_tick` | 1.354 | 1.783 |

## After #833: scene reuse validation — 2026-07-29

Captured with the same WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime under `xvfb-run -a`. The cursor-only
fixture renders the base frame before timing, so overlay iterations exercise the cached authoritative scene and its
prepared text snapshots.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `render.cursor_only.java2d_overlay` (baseline) | 5.158 | 10.270 |
| `render.cursor_only.scene_reuse.java2d_overlay` | 2.162 | 2.221 |

## After #835: linear measured-line rendering validation — 2026-07-29

Captured with the standard warmed harness workflow under Xvfb on the same WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `render.long_measured_line.java2d` | 2.146 | 2.538 |

The same warmed run reported `allocation_p50_bytes = 5,935,640` and `allocation_p95_bytes = 5,950,824` for `render.long_measured_line.java2d`; allocation is reported per operation by the harness's thread-allocation counter.

## After #834: backing-buffer reuse validation

The full-frame benchmark now alternates reusable device-scaled backing images, while cursor-only rendering publishes a reusable transparent overlay over the authoritative base frame. Resize or image-type changes still allocate replacement storage. Captured with the same WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime under `xvfb-run -a`; times are milliseconds.

| Scenario | Before p50 | Before p95 | After p50 | After p95 |
| --- | ---: | ---: | ---: | ---: |
| `render.full_frame.java2d` | 11.284 | 15.223 | 6.087 | 7.916 |
| `render.cursor_only.java2d_overlay` | 5.158 | 10.270 | 0.403 | 0.829 |
| `render.hidpi_frame.java2d` | 10.277 | 20.254 | 5.680 | 14.010 |

## After #827: 2026-07-22

Captured on the same x86_64 Linux WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime as the baseline. The changed rope paths were measured with the same harness invocation after warmup; times are milliseconds.

| Scenario | Before p50 | Before p95 | After p50 | After p95 |
| --- | ---: | ---: | ---: | ---: |
| `rope.large_json.search` | 12.789 | 14.422 | 1.626 | 1.779 |
| `layout.large_multiline.visible_viewport` | 4.292 | 6.355 | 3.355 | 5.303 |

## After #828: 2026-07-22

Captured on the same x86_64 Linux WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime. `find_replace.large_query_update` measures the complete work that previously ran synchronously during a find update: sequential rope search, grapheme filtering, offset-to-position conversion, and applying the selected result. `find_replace.large_query_keystroke` measures the reducer path after that work was moved behind the debounced, cancellable request boundary.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `find_replace.large_query_update` | 16.198 | 19.746 |
| `find_replace.large_query_keystroke` | 0.026 | 0.039 |

## #836 validation: 2026-07-28

The focused viewport regressions are covered by indexed-line tests: deep scrolling keeps source reads bounded, and visible annotation projections avoid expanding comment ranges outside the rendered lines. The Markdown block suite also covers active fenced blocks through 4,500 interior lines and long paragraph/list blocks without materializing line sets.

| Scenario | Result |
| --- | --- |
| `markdown.deep_scroll.no_fence.reads` | bounded indexed reads; 10,000-line fixture |
| `markdown.long_fenced_block.interior` | passes through 4,500 lines |
| `renderer.focused_body.long_block` | range predicate; no block-sized `Set` allocation |
| `renderer.diagnostics.visible_projection` | visible-line projection; annotation index reuse remains pending |

The high-count correctness fixture retains annotations positioned after 1,024 unrelated entries; this guards against truncating source lists while scene-owned index reuse is completed.

Measured harness results (same WSL2 host, warmed run):

| Scenario | p50 (ms) | p95 (ms) |
| --- | ---: | ---: |
| `reducer.deep_scroll.plain` | 0.006 | 0.048 |
| `reducer.deep_scroll.rich_text` | 0.011 | 0.036 |
| `render.diagnostics_and_comments.java2d` | 7.169 | 10.973 |

## `command_runner.*` baseline (#1854): 2026-10-05

Captured on a 4-core x86_64 Linux container under Xvfb, OpenJDK 21.0.11, with
`sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks command_runner"`. "Before" is the same benchmark run on
f79b6d17, the commit before #1970 moved the settings index and per-query results into a carried cache; "after" is
master at dc5bfd01. Times are milliseconds.

| Scenario | Before p50 | Before p95 | After p50 | After p95 |
| --- | ---: | ---: | ---: | ---: |
| `command_runner.palette.keystroke` | 14.349 | 15.497 | 1.420 | 1.546 |
| `command_runner.palette.arrow_press` | 15.651 | 16.228 | 0.065 | 0.197 |
| `command_runner.settings.keystroke` | 8.207 | 13.074 | 0.511 | 0.554 |
| `command_runner.settings.arrow_press` | 15.375 | 21.216 | 0.0003 | 0.0003 |

## Run-to-run stability audit -- 2026-10-08

#2147 stabilised `rope.large_json.cursor_offset`, whose p50 sat on one of several JIT plateaus. This pass applied the same
audit to every benchmark in `PerformanceBenchmarks`.

**Method.** Ten separate JVMs per variant, run alternately at 1x (`-screen 0 1920x1200x24 -dpi 96`) and 2x
(`-screen 0 3840x2400x24 -dpi 192 -Dsun.java2d.uiScale=2`), the configuration of the CI perf job, on a 4-core Linux
container shared with other jobs (hence `nice -n -20` on the benchmark JVM; an un-niced first pass was dominated by
CPU contention, with load averages up to 15). Per benchmark: max/min across the ten per-run p50s, and the number of
modes (p50s more than 15% apart start a new mode). The "before" set is master at d1542169; "after" is this change.

**What the audit found.**

| Class | Benchmarks | Evidence |
| --- | --- | --- |
| Sampling window shorter than a stall | nearly every benchmark | The timed phase was `iterations x sample`, 22 ms for `render.full_frame.java2d` and at most 210 ms for all but two benchmarks. One GC pause or CPU steal that long moved every sample and so the median. The runs that were slow were slow all the way through, at random: 1 or 2 runs in 10 at 2.5 to 7 ms against a 2.7 ms mode. |
| JIT plateau that outlasts the 500 ms warmup | `rope.large_json.search`, `lsp.framer.large_batch`, `richdoc.open.docx`, `command_runner.palette.keystroke`, `reducer.multi_cursor_move_down`, `damage.caret_move.repaint_region.pinned_status`, `render.cursor_only.scene_reuse.java2d_overlay`, `render.diagnostics_and_comments.java2d` | Forcing the settle phase on every benchmark collapsed their modes and moved the median down (`lsp.framer.large_batch` 1.83 to 0.62 ms), so the old p50 was a pre-C2 number. |
| Printed resolution | the four `equals.*.same_reference`/`shared_fields_*` benchmarks | `%.5f` ms prints multiples of 10 ns; a 7 ns benchmark read as 10 or 20 ns, a 2x "spread" that was rounding. |
| Cross-thread hand-off through `StateManager` | `laptop.input.state_manager.*` | Settling made these worse (spread 4 to 6x, and `continuous_typing` doubled as the document grew), so it is not a warmup effect. p95/p50 is 2 to 3 within one run. |
| Steady-state bimodality below the gate's 0.05 ms floor | `reducer.backspace`, `damage.single_char_long_line.*`, `find_replace.large_result_set` | Warmup counts are already at the cap; neither a longer warmup nor settling moved them. The gate cannot fire on them. |

**Changes.**

- `BenchmarkRunner` samples until the samples span 400 ms (cap 1,000 samples), not just for the configured count. The
  configured count is the minimum. `fixedSampleCount` opts out the three benchmarks that type without deleting
  (`continuous_typing`, `typing_random_letters`, `typing_long_paragraph`): each extra sample grows their document, and
  with the window `typing_random_letters` moved from 1.30 to 2.05 ms.
- `BenchmarkIterationCounts.JitSettled` lists the eight benchmarks above, which run the settle phase (#2147) before
  sampling. The others do not: settling every benchmark added 170 s to a 72 s run.
- `printResults` prints seven decimals and reports the number of samples taken.

**Measured effect** (ten runs each; 71 benchmarks):

| | 1x before | 1x after | 2x before | 2x after |
| --- | ---: | ---: | ---: | ---: |
| benchmarks with max/min > 1.3 | 48 | 19 | 41 | 18 |
| of those, p50 >= 0.05 ms (where the gate can fire) | 26 | 12 | 24 | 12 |
| worst max/min | 3.24 | 1.66 | 3.93 | 1.87 |
| suite wall-clock, mean of ten | 71.7 s | 108.2 s | 72.9 s | 107.6 s |

Every remaining spread above 1.3 at or over 0.05 ms is either one slow run in ten (`page_down_up`,
`type_and_delete`, `typing_long_paragraph`, `paint_caret`), the `StateManager` hand-off scatter (`go_to_line`,
`typewriter_keystroke`, `typing_random_letters`, 1.4 to 1.9 with no outlier), or `markdown.preview.window_mapping`,
whose p50 is 0.22 ms or 0.33 ms depending on what ran before it in the JVM (0.19 to 0.22 ms when run alone). The latter
is why its median moved from 0.23 to 0.32 ms although the benchmark is unchanged: the longer sampling and settling of
earlier benchmarks changed the JIT state it inherits. Medians of the other benchmarks that were neither settled nor
fixed-count moved by at most 12% (all downward except `damage.scroll.*` and `damage.markdown.*`, up 5 to 7%), apart from
the `StateManager` keystroke family (down 12 to 37%, now sampled for longer after the JIT has finished with them) and
`window_mapping`.

**CI time.** The suite takes 36 s longer per run (+18 s from the sampling window, +18 s from the settle phases). The perf job runs it four
times (two base, two head), so +2.4 min per perf job once the base commit has this harness (+1.2 min on the PR that
introduces it, whose base still runs the old one). The job's timeout is 30 minutes.

