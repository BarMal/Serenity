package com.serenity.perf

/** Per-family iteration counts for the benchmarks a false-positive audit of #1453/#1503's CI regression gate found
  * flagging spurious 2x-ratio "regressions" from run-to-run p50 jitter alone: `reducer.*`, `damage.*`,
  * `lsp.framer.large_batch`, and `render.markdown.inline_lens`. Kept in one file, apart from the benchmarks themselves,
  * for the same reason [[BenchmarkFixtures]] is: the derivation is reviewed as a unit, separately from what each family
  * measures.
  *
  * Each count is a multiple of that family's previous count (`damage.*` 30, `reducer.*` 20, `lsp.framer.large_batch`
  * 12, `render.markdown.inline_lens` 8) chosen from twelve local `PerformanceBenchmarks` runs -- six at the old count,
  * six at the new one, same Xvfb/shared-CPU conditions the CI runner uses -- comparing each family's worst-case
  * run-to-run p50 spread (max/min across the six runs) before and after:
  *
  *   - `damage.*` (30 -> 60, 2x): worst scenario (`single_char_short_line`) fell from 7.0% CV / 1.23x spread to 2.85%
  *     CV / 1.10x spread. One count for the whole family: the worst scenario needed only a 2x bump, and applying it
  *     uniformly leaves every other scenario with margin rather than under-provisioned.
  *   - `reducer.*` (20 -> 60, 3x): the worst scenario shifted from `multi_cursor_move` (23.7% CV / 1.74x spread, traced
  *     to `BenchmarkRunner.calibrate`'s batch-size doubling: a noisy early sample can lock in half the batch size of a
  *     clean run, and that choice then holds for every sample in the run) to `reducer.normal_editing` (11.6% CV / 1.38x
  *     spread) -- `multi_cursor_move` itself dropped to 0.58% CV / 1.02x, but at ~0.014ms mean, `normal_editing` is
  *     small enough that raw sampling still shows real spread; 3x is not a complete fix by itself here, which is
  *     exactly why `check_perf_regression.py`'s absolute-ms floor exists as a second, required guard:
  *     `normal_editing`'s worst observed absolute delta was ~0.005ms, far under that floor.
  *   - `lsp.framer.large_batch` (12 -> 48, 4x): 6.4% CV / 1.22x spread fell to 5.25% CV / 1.16x spread -- a real but
  *     partial improvement, still comfortably under the 2x gate.
  *   - `render.markdown.inline_lens` (8 -> 24, 3x): already tight at 1.46% CV / 1.04x spread before the bump; it
  *     measured 1.83% CV / 1.06x spread after, i.e. no further stabilization was actually needed or observed here
  *     (within the noise of a 6-run sample) -- the count was still raised for consistency with the other flagged
  *     families and headroom against future noise, not because this family's own data demanded it.
  *
  * None of the four families reached a 2x run-to-run spread in either the six before-runs or the six after-runs, so
  * this data cannot show these counts eliminate 2x false positives outright -- only that the more volatile scenarios
  * got markedly less volatile. Full before/after per-scenario figures are in docs/performance-benchmarks.md's
  * "Iteration-count derivation" section, including the measured CI job runtime impact of the combined bump.
  */
private[perf] object BenchmarkIterationCounts:
  val Damage         = 60
  val Reducer        = 60
  val LspFramer      = 48
  val RenderMarkdown = 24
