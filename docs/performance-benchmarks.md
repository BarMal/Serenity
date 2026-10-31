# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The cursor-only scenario opens a temporary Swing window so it can measure Serenity's real overlay publication path. Run it in a graphical session; a headless Linux environment can use `xvfb-run -a sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"`.

The `iterations` column is the number of samples actually taken (see "Run-to-run stability audit" below), and the millisecond columns carry seven decimals so a 10 ns benchmark is not rounded to a multiple of 10 ns.

The harness prints CSV rows with `min_ms`, `p50_ms`, `p95_ms`, `max_ms`, `allocation_p50_bytes`, and `allocation_p95_bytes`. Allocation columns are populated for every `laptop.*`, `render.*` and `command_runner.*` benchmark plus a hand-picked set from other families (see [Memory metrics](#memory-metrics-footprint-and-allocation)) and are sampled separately from timing so the allocation probe does not distort p50/p95. Every scenario builds its immutable document, state, search, LSP, and project-task fixtures before timing, runs the measured operation once, and asserts its observable result before warmup. Java2D frame drawing, reusable backing-buffer acquisition, cursor-overlay composition, and repaint requests remain inside their timed paths because they are part of the user-visible work being measured.

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
- `render.modal_wrap.*`: the frame height plus the composition of three representative dialogs (the layout-override
  prompt, a long Confirm, a Save As file workflow) in a 72-cell frame, wrapped by cells against by the GUI font's pixels,
  with a `nowrap` baseline and `cold_*` variants whose text the pixel measurements have not seen. Run alone with
  `xvfb-run -a sbt "Test/runMain com.serenity.perf.ModalWrapBenchmarks"`. Wrapping by cells costs the same as not
  wrapping; by pixels adds a few microseconds warm and about 1.7 ms on the first frame of a changed prompt, so the GUI
  wraps by cells (`TextMeasure.forState`).
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

## Memory metrics: footprint and allocation

The CSV ends with a `retained_bytes` column (header: `...,allocation_p50_bytes,allocation_p95_bytes,retained_bytes`). Older ten-column files still parse; a missing value is treated as absent.

**Footprint (`footprint.*`)** is heap the app keeps resident. `FootprintBenchmarks` builds a real `AppState`, drops everything else, runs `System.gc()` until three consecutive used-heap readings agree (within 64 KiB), and records the growth over a baseline settled before the build, holding the state strongly reachable across the reading. One build is discarded to pay for static caches, and the median of three trials is reported. Because of that discard, process-wide caches never show up in a `footprint.*` figure; see [Footprint rows and static caches](#footprint-rows-and-static-caches). Scenarios: `empty_editor`, `plain_document_15k_lines`, `rich_document`, `after_1000_edits` (undo history), `ten_buffers`, and the `prose_frame.*` rows that draw a frame so the caches have something to hold. Their timing columns are zero, so the latency gate ignores them. Run only these with `sbt "Test / runMain com.serenity.perf.FootprintBenchmarks"` or pass the `footprint.` prefix to `PerformanceBenchmarks`.

**Allocation** (`allocation_p50_bytes`, `allocation_p95_bytes`) is bytes allocated per invocation, measured in a separate pass with `ThreadMXBean.getThreadAllocatedBytes`. Every benchmark whose name starts with `laptop.`, `render.` or `command_runner.` is tracked by prefix; others opt in through `AllocationTracked` in `BenchmarkRunner`.

**CI gate (`scripts/check_perf_regression.py`)**

| Metric | Condition | Effect |
| --- | --- | --- |
| `retained_bytes` | current > 1.25x baseline AND delta > 1 MiB | fails the build |
| `allocation_p50_bytes` | current > 1.5x baseline AND delta > 64 KiB | `::warning::` only; `--gate-allocation` makes it fail |

Thresholds are `--retained-threshold`, `--min-retained-delta-bytes`, `--allocation-threshold` and `--min-allocation-delta-bytes`. A baseline without the columns is skipped with a `::notice::` instead of failing. `combine_perf_runs.py` carries the columns through and keeps the largest retained value for a baseline (`--pick slowest`) and the smallest for a head.

## Cache-ownership scenarios

These rows exist so a change that moves a process-wide cache into state a runtime owns can be measured before and after. Run them with `sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks layout.anchor_wrap. layout.cold_font. laptop.input.state_manager.two_runtimes. footprint."`.

| Row | What it times | What a cache move should change |
| --- | --- | --- |
| `layout.anchor_wrap.warm_caret_lookup` | `LayoutEngine.laidOutPanes(...).screenPosition` for one unchanged state, which `LayoutEngine.anchorWrappedLines` answers after the first call | a lookup that no longer finds the cache a previous frame filled gets slower |
| `layout.anchor_wrap.after_edit.caret_lookup` / `.reduce_only` | the same lookup after a typed letter, so the edited line's wrap misses; `reduce_only` is the typing alone, to subtract | the difference is the anchor wrap's miss cost |
| `layout.cold_font.glyph_advances` / `shaping_barriers` / `font_line_extents` / `measured_layout_memo.{proportional,monospaced}` / `prose_column` | the first use of a font: each call gets a font (or `FontConfig`) whose size no earlier call used, so the probe behind `GlyphAdvances`, `ShapingBarriers`, `FontLineExtents`, `TextLayoutSnapshot.shouldUseMeasuredLayout` and `ProseColumn` runs cold | a cache a runtime owns is cold for every new runtime, so this is what each one pays on its first frame |
| `layout.cold_font.text_layout_snapshot` | `TextLayoutSnapshot.fromBuffer` over a three-paragraph document in a font nothing has seen: every probe above at once | the end-to-end first-use cost |
| `laptop.input.state_manager.two_runtimes.same_text` / `.different_text` | two live `StateManager`s in one process, each typing a letter per iteration. `same_text` types the same letter into both, so the second runtime's edited line is one the first just wrapped; `different_text` types a different letter, so nobody has wrapped it | `different_text - same_text` is what the runtimes save each other by sharing caches; owned caches shrink it to zero |

Every `layout.anchor_wrap.*`, `layout.cold_font.*` and `laptop.*` row has allocation columns.

One run of these rows on the cloud container that wrote them (shared CPU, Xvfb, so compare ratios, not absolute times):

| Row | p50 ms | allocation p50 |
| --- | ---: | ---: |
| `layout.anchor_wrap.warm_caret_lookup` | 1.19 | 131 KiB |
| `layout.anchor_wrap.after_edit.caret_lookup` / `.reduce_only` | 1.30 / 0.019 | 579 KiB / 9 KiB |
| `layout.cold_font.glyph_advances` | 1.16 | 47 KiB |
| `layout.cold_font.shaping_barriers` | 1.33 | 31 KiB |
| `layout.cold_font.font_line_extents` | 0.40 | 14 KiB |
| `layout.cold_font.measured_layout_memo.proportional` / `.monospaced` | 0.21 / 1.54 | 5 KiB / 53 KiB |
| `layout.cold_font.prose_column` | 8.19 | 5.2 MiB |
| `layout.cold_font.text_layout_snapshot` | 1.86 | 330 KiB |
| `laptop.input.state_manager.two_runtimes.same_text` | 4.17 | 2.3 KiB |
| `laptop.input.state_manager.two_runtimes.different_text` | 7.08 | 2.3 KiB |

The last two rows show the sharing directly: when the second runtime types what the first just typed, the pair of keystrokes costs 2.9 ms less than when it types something of its own. The 2.3 KiB allocation is probably only the calling thread's share (the `StateManager`s do their work on other threads); that is inferred, not checked, so do not read it as the rows' total allocation.

### Footprint rows and static caches

`FootprintBenchmarks` discards its first build "to pay for static caches" and settles its baseline after it, so **a process-wide cache is invisible to every `footprint.*` row**. Moving a cache into owned state makes it reachable from the measured value, so `retained_bytes` rises by what the cache holds, with nothing else having changed. Three rows make that visible:

* `footprint.prose_frame.stable_text`: a `RenderCaches` and an `AppState` after one laptop-sized frame of the prose fixture. Every build draws the same text, so process-wide caches hit and add nothing: any rise after a cache move is the moved cache's size.
* `footprint.two_runtimes.prose_frame.stable_text`: two such runtimes over one document. Today they share the process-wide caches; once owned each holds its own copy, so this row should rise by about twice the cache's size where the single-runtime row rises by once.
* `footprint.prose_frame.fresh_text`: the same frame over text no earlier build drew. Process-wide caches grow during the build, and that growth is counted whether the cache is static or owned, so this is the row to compare across a move: it should stay roughly level while `stable_text` rises. A bounded cache that is full evicts as it grows, so its fresh-text figure caps out at its bound.

Measured on the build before any cache moves (three runs agree to within 1 KiB): `stable_text` 29.99 MB, `fresh_text` 30.02 MB and `two_runtimes` 59.97 MB. The process-wide caches therefore hold about 30 KB after this frame, 0.1% of the row, and the two runtimes already cost twice one because `RenderCaches` and the state are per runtime. What a move raises is small beside the state; what it changes is that the figure now depends on the move, which is why the rows are worth recording.

An expected rise in the `stable_text` rows is not a regression to fix; record it in the PR. The footprint threshold below still applies to any row that is not explained that way.

## Text-boundary and rope-count scenarios

These rows exist so a change to how the per-thread `BreakIterator`s are owned (the `TextEditing`, `RopeCharacterSource` and `TextCounts` entries of `PurityAllowlist`, #2350) can be measured before and after. They need no window. Run them with `sbt "Test/runMain com.serenity.perf.BoundaryCacheBenchmarks"`, or through `PerformanceBenchmarks` with the prefixes `text.boundary.` and `text.counts.`.

The prose has a multi-char cluster (e with a combining acute, a thumbs-up with a skin tone, the Devanagari syllable ni) after every sixth word, and the probes sit next to those clusters. That is deliberate: `TextEditing.nextGraphemeBoundary` answers a plain neighbour without touching a `BreakIterator`, so a probe in ASCII text would time the fast path and not the thing being moved. Each grapheme row's `verify` asserts that the answer crosses a whole cluster.

| Row | What it times | What an iterator-ownership change should change |
| --- | --- | --- |
| `text.boundary.word_motion` | 64 `nextWordBoundary` + `previousWordBoundary` pairs over one unchanged rope | the word `BreakIterator` lookup and its identity-anchor hit |
| `text.boundary.grapheme_motion` | 64 `nextGraphemeBoundary` + `previousGraphemeBoundary` pairs across clusters of one unchanged rope | the grapheme `BreakIterator` lookup and its identity-anchor hit |
| `text.boundary.keystroke_after_edit` | one insert into the rope, then the four boundary questions a keystroke asks of the new text | the anchor miss: a new rope each time, so `setText` runs every time |
| `text.boundary.find_whole_word` | `FindSearch.search` with whole-word on over about 3000 paragraphs, which asks `isWholeWordRange` of every candidate | the word iterator on the find path |
| `text.counts.rope_build` | `Rope(text).wordCount` for a fresh rope, so every leaf's `TextSummary` is computed | the word and character iterators in `TextCounts.of` |
| `text.counts.rope_edit` | one insert into a counted rope, then its word count: only the leaf and the joins on its path recompute | the same iterators on the incremental path |

Every `text.boundary.*` and `text.counts.*` row has allocation columns. Three runs of `BoundaryCacheBenchmarks` on the cloud container that wrote them (shared CPU, so compare ratios, not absolute times), p50 ms and allocation p50:

| Row | p50 ms (three runs) | allocation p50 |
| --- | ---: | ---: |
| `text.boundary.word_motion` | 0.125 / 0.145 / 0.120 | 7.6 KiB |
| `text.boundary.grapheme_motion` | 0.047 / 0.053 / 0.042 | 7.9 KiB |
| `text.boundary.keystroke_after_edit` | 0.0076 / 0.0077 / 0.0067 | 7.3 KiB |
| `text.boundary.find_whole_word` | 5.57 / 5.73 / 5.17 | 474 KiB |
| `text.counts.rope_build` | 18.2 / 18.2 / 18.6 | 820 KiB |
| `text.counts.rope_edit` | 0.169 / 0.162 / 0.168 | 8.7 KiB |

The run-to-run spread on the sub-millisecond boundary rows is up to about 12%, so a change on them needs the ABBA protocol below, not a single before/after pair.

## ABBA protocol

Use it for any change that moves a cache, a lock or an allocation off the hot path, where the CI gate (a 2x ratio) is too coarse to see the effect. A is the baseline build and B the candidate; build both with `bench-artifacts.yml` (`serenity-perf.jar`) from the merge base and the candidate head.

```text
bench/abba.sh <A serenity-perf.jar> <B serenity-perf.jar> <out-dir> [name-prefix...]
```

The script runs `PerformanceBenchmarks` eight times in the order **A B B A A B B A** (so drift falls on both builds alike), writes `a-1..4.csv` and `b-1..4.csv`, then runs `scripts/abba_compare.py`. Pass the prefixes of the rows the change touches plus `footprint.`; with none the whole suite runs, which takes much longer. `ABBA_JAVA_OPTS` adds JVM flags (for example `-Dsun.java2d.uiScale=2`). Keep the machine otherwise idle.

A benchmark **blocks** the change when B is worse than A by:

| Metric | Blocks when |
| --- | --- |
| time (`p50_ms`) | the four-run p50 ranges do not overlap (B's fastest run is slower than A's slowest), B's median is at least 5% slower, **and** at least 0.03 ms slower |
| allocation (`allocation_p50_bytes`) | B's median is at least 5% **and** at least 8 KiB above A's |
| footprint (`retained_bytes`) | B's median is more than 10% above A's |

The 5% and 0.03 ms floors are separate guards on purpose, as in the CI gate: the ratio alone over-fires on microsecond rows and the delta alone on multi-millisecond ones. Overlapping ranges never block however far the medians are, because the eight runs could not tell the builds apart. A blocked benchmark needs either a fix or a note in the PR that explains it (the `stable_text` footprint rows above are the expected case); `abba_compare.py` exits 1 and prints one `::error::` line per blocking metric.

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

The same line carries what each frame drew, as `avg` and `max` per frame over the window: `rows-drawn` and
`rows-preserved` (document rows redrawn against rows kept from the frame before, copied scroll rows included), `runs`
(text runs painted), `shapings` (strings shaped rather than served from the shaping cache), `layer-composites`,
`chrome-painted` (its average is the share of frames that repainted any chrome), `present-px` (logical pixels published
to the screen; the whole cell grid when the frame was not bounded) and `tui-bytes` (UTF-8 bytes the terminal frontend
wrote, escape sequences included; a caret flush made after a frame's content flush is counted with the next frame).
Only counters that were ever non-zero are listed. A typing frame that lists `rows-drawn max=` far above a handful, or a
`present-px` near the whole window, is over-repainting however fast it happens to be. The `laptop.render.typing_frame_2x`
and `scroll_frame_2x` benchmarks assert a ceiling on `rows-drawn` while verifying their frames (`MaxTypingRowsDrawn`,
`MaxScrollRowsDrawn` in `LaptopFrameBenchmarks`), so lost row reuse fails the benchmark rather than only slowing it.

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


## Paired-ratio gate -- issue #1731

**Problem.** The gate failed a benchmark only when the head's *fastest* p50 exceeded 2.0x the base's *slowest* p50 over
two runs a side. A benchmark whose p50 swings between runs of one commit (the issue saw 5 to 30 ms) therefore hid any
slowdown that landed inside the swing, and the 2.0x bar was also the size of the regression to be caught. On ten
recorded runs of the commit measured then, the old rule flagged 45% of the benchmarks that a real, injected 2x
slowdown slowed.

**Gate now.** The base and head commits run in three back-to-back pairs that alternate which side goes first (B H, H B,
B H). For each benchmark the verdict compares each side's **fastest p50** over its runs: it fails when the fastest head
p50 exceeds the fastest base p50 by more than **1.5x** and by more than 50 us (the existing delta floor). Retained-heap
and allocation checks run on the per-benchmark medians as before. `scripts/compare_perf_runs.py` is the gate;
`scripts/study_perf_gate.py` scores it against recorded runs.

**Why the fastest run (issue #2443).** A benchmark's p50 is bimodal from one JVM to the next: depending on how far the
JIT had got when it was measured, the same code reads about 1.4 ms or 3 to 7 ms (`render.cell_grid.frame_emoji_clusters`
is the example the investigation started from). The slow mode is a state a run lands in; it only ever makes a run slower,
so the fastest run is the one least affected by it. The median of three paired ratios, which this section described
before, turns two runs in different modes into a verdict. The cost is that one lucky-fast head run clears a real
slowdown (a head of 20, 20 and 9 ms against a base of 10, 10, 10 passes); `test_the_fastest_head_run_decides_...` states
it.

**Confirming before failing.** The first live runs of this gate on `ubuntu-latest` produced a false positive:
`render.markdown.live_preview` read 1.93x (0.593 to 1.024 ms) in one run of a commit and 1.35x (0.365 to 0.442 ms) when
the same commit's job was re-run, with `layer_cache_on.idle_frame_2x` at 1.69x beside it. So a benchmark past the threshold is
only *flagged* on the first three pairs. The job then re-measures just the flagged benchmarks (`PerformanceBenchmarks`
takes benchmark names as arguments) in three fresh pairs on the same runner, `confirm-*` in the uploaded artifact, and
fails only if those pairs are past the threshold too. The confirmation is judged on its own pairs, not pooled with the first
three: the flagged benchmark was picked for being the worst of about ninety, and pooling would count that luck twice. A
retained-heap regression is not noisy this way and fails on the first pass. Cost: nothing when no benchmark is
flagged, and about 5 minutes a run for six runs of a handful of benchmarks when one is. `scripts/test_compare_perf_runs.py`
(`ConfirmBeforeFailingTests`) shows an injected 2x flagged and then confirmed, and a one-off spike flagged and then
forgiven. By construction the confirmation can only turn failures into passes; the table below measures both passes
(first pass, and after the confirmation on fresh pairs) rather than assuming them independent.

**Injecting a slowdown.** `-Dserenity.bench.slowdown=2` makes every timed call take twice as long (it spins for the
extra time), so a gate can be shown a regression it must catch. `scripts/test_compare_perf_runs.py` carries the same
case as a red test: the old rule passes base `[5, 30, 6]` ms against head `[10, 60, 12]`, the new one fails it.

**Measurement (container figures).** Twelve full `PerformanceBenchmarks` runs of one commit (master `3a7a88b7`), one JVM
per run, back to back, on a 4-core Linux container under Xvfb at 1x (the CI perf job's configuration), 6.7 to 6.9 minutes
each; 121 benchmarks are judged and 83 are slow enough for a 2x slowdown to clear the 50 us floor. Trials draw 3 runs as
the base side and 3 others as the head side, 2,000 times (seeded) with `study_perf_gate.py`; "A/A" is base and head being
the same code, so any build that fails is a false positive. With `--confirm`, a trial's flags are re-judged on 3 fresh
pairs drawn from the other 6 runs, as the CI job does. An injected slowdown multiplies the head runs' p50s, which is what
`-Dserenity.bench.slowdown=2` does to a real run. The figures below are from that container, not from a CI runner, and
the runs are resampled, so the trials are not independent.

| rule | A/A builds failing, first pass | A/A builds failing after confirm | injected 2x: benchmarks caught, first pass | after confirm | injected 1.5x after confirm |
| --- | ---: | ---: | ---: | ---: | ---: |
| old (fastest head vs slowest base, 2.0x, 2 runs; it had no confirmation) | 32.7% | n/a | 17.7% | n/a | n/a |
| median of paired ratios, 1.75x (the gate before #2443) | 76.6% | 17.6% | 80.2% | 61.2% | 4.1% |
| median of paired ratios, 1.5x | 90.3% | 29.6% | 91.7% | 80.3% | 23.7% |
| **fastest vs fastest, 1.5x (now)** | 78.5% | **11.3%** | 94.7% | **89.2%** | 23.9% |
| fastest vs fastest, 1.75x | 69.8% | 9.0% | 85.3% | 72.4% | 2.1% |
| fastest vs fastest, 2.0x | 65.0% | 8.8% | 50.6% | 24.8% | 0.4% |

The benchmark columns are the share of the 83 eligible benchmarks a build with every head run slowed fails on, which is
the chance of catching a slowdown confined to one benchmark.

The earlier version of this section reported 0.0% A/A and 95% at 2x for the median at 1.75x, from ten runs of an older,
smaller suite (56 eligible benchmarks). The twelve runs above give 17.6% and 61% for the same rule after the confirmation.
I do not know why they differ: the suite has grown, and these runs are all from one container.

**What this does and does not show.**

- The false-positive rate is that of one container. A CI runner is a different machine; the rate there is unmeasured
  until the job has run enough times on it. The one CI job inspected for #2443 flagged one benchmark on the first pass.
- The injected slowdown is uniform. A regression confined to one benchmark is caught at that benchmark's own rate, the
  per-benchmark share in the table.
- A 1.5x regression is mostly missed by any rule that keeps false positives low (24% at best here); the gate is for the
  2x class.
- A first pass that fails four builds in five on unchanged code is only usable because of the confirmation; the fraction
  that survives it (11% here) is the number to watch. The remaining false positives come from benchmarks whose run-to-run
  state differs, which a better warm-up in the runner (investigated in #2443, not changed) would address at the source.
- Cost: six benchmark runs instead of four, about 50% more perf-job time; unchanged by #2443.
