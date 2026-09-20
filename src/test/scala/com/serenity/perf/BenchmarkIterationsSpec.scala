package com.serenity.perf

import com.serenity.perf.BenchmarkFixtures.{
  deepViewport,
  editorState,
  editorStateForRichDocument,
  largeFindDocument,
  largeMultilineDocument,
  largeRichTextDocument
}
import com.serenity.rope.Balance
import com.serenity.state.models.{CursorPosition, EditingState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Locks in the per-family iteration counts derived in `BenchmarkIterationCounts` -- a benchmark's `iterations` field
  * silently drifting back to a noisy count (via a misplaced literal, or a new scenario added by copy-paste from the old
  * value) would reopen the exact spurious-regression risk that audit measured, with no other test able to catch it.
  */
class BenchmarkIterationsSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "every damage.* scenario" should "use BenchmarkIterationCounts.Damage" in {
    val benchmarks = DamageBenchmarks.benchmarks()
    benchmarks.map(_.name) should contain allOf (
      "damage.single_char_long_line.rows",
      "damage.single_char_short_line.rows",
      "damage.multi_cursor.rows",
      "damage.markdown.rows",
      "damage.scroll.rows"
    )
    all(benchmarks.map(_.iterations)) shouldBe BenchmarkIterationCounts.Damage
  }

  "every reducer.* scenario" should "use BenchmarkIterationCounts.Reducer" in {
    val benchmarks = reducerFixtureBenchmarks()
    benchmarks should have size 10
    all(benchmarks.map(_.iterations)) shouldBe BenchmarkIterationCounts.Reducer
  }

  "the chosen iteration counts" should "actually be larger than the previous, false-positive-prone counts" in {
    // Regression guard on the audit's conclusion itself: these must stay well above the old 8-30 range, not just be
    // internally consistent with each other.
    BenchmarkIterationCounts.Damage should be > 30
    BenchmarkIterationCounts.Reducer should be > 20
    BenchmarkIterationCounts.LspFramer should be > 12
    BenchmarkIterationCounts.RenderMarkdown should be > 8
    BenchmarkIterationCounts.LayoutVisibleViewport should be > 20
  }

  /** Rebuilds the same shapes of state `PerformanceBenchmarks.benchmarks()` passes to `reducerBenchmarks` -- a
    * 12,000-line find document with a cursor 6,000 lines in, and 15,000-line plain/rich documents scrolled to
    * `deepViewport` -- without the Swing window and Java2D rendering fixtures the rest of that method needs, so this
    * spec runs under plain `sbt test` (headless, no Xvfb) rather than only under the CI job that has one.
    */
  private def reducerFixtureBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val findText  = largeFindDocument(matches = 12_000)
    val findState = editorState(findText, None)
    val editingState = findState.copy(persisted =
      findState.persisted.copy(buffers =
        findState.persisted.buffers.view
          .mapValues(buffer => buffer.copy(editing = EditingState(List(CursorPosition(6_000, 12)))))
          .toMap
      )
    )
    val multilineState = editorState(largeMultilineDocument(lines = 15_000), None)
    val plainScrollState = multilineState.copy(persisted =
      multilineState.persisted.copy(buffers =
        multilineState.persisted.buffers.view.mapValues(_.copy(viewport = deepViewport)).toMap
      )
    )
    val deepRichState = editorStateForRichDocument(largeRichTextDocument(lines = 15_000))
    val richScrollState = deepRichState.copy(persisted =
      deepRichState.persisted.copy(buffers =
        deepRichState.persisted.buffers.view.mapValues(_.copy(viewport = deepViewport)).toMap
      )
    )
    PerformanceBenchmarks.reducerBenchmarks(editingState, plainScrollState, richScrollState, deepViewport)

end BenchmarkIterationsSpec
