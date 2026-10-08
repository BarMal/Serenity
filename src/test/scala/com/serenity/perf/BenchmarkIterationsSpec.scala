package com.serenity.perf

import com.serenity.rope.Balance
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
    val benchmarks = ReducerBenchmarks.benchmarks()
    benchmarks should have size 10
    all(benchmarks.map(_.iterations)) shouldBe BenchmarkIterationCounts.Reducer
  }

  "every laptop typing scenario" should "use BenchmarkIterationCounts.RandomTyping" in {
    import cats.effect.unsafe.implicits.global
    val benchmarks = LaptopFrameBenchmarks.typingBenchmarks
    benchmarks.map(_.name) shouldBe List(
      "laptop.input.state_manager.typing_random_letters",
      "laptop.input.state_manager.typing_long_paragraph"
    )
    all(benchmarks.map(_.iterations)) shouldBe BenchmarkIterationCounts.RandomTyping
  }

  "the laptop go-to-line scenario" should "use its own warmup and sample counts" in {
    import cats.effect.unsafe.implicits.global
    val benchmarks = LaptopFrameBenchmarks.goToLineBenchmarks
    benchmarks.map(_.name) shouldBe List("laptop.input.state_manager.go_to_line_3000_paragraphs")
    all(benchmarks.map(_.iterations)) shouldBe BenchmarkIterationCounts.GoToLine
    all(benchmarks.map(_.warmups)) shouldBe BenchmarkIterationCounts.GoToLineWarmups
    all(benchmarks.map(_.minBatch)) shouldBe BenchmarkIterationCounts.GoToLineBatch
  }

  "rope.large_json.cursor_offset" should "warm up past the C2 compilation threshold" in {
    val benchmark = PerformanceBenchmarks.ropeBenchmarks().find(_.name == "rope.large_json.cursor_offset")
    benchmark.map(_.warmups) shouldBe Some(BenchmarkIterationCounts.RopeCursorOffsetWarmups)
    BenchmarkIterationCounts.RopeCursorOffsetWarmups should be >= 10_000
    benchmark.map(_.settleJit) shouldBe Some(true)
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

end BenchmarkIterationsSpec
