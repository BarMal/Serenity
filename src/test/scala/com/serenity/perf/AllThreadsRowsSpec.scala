package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows whose work runs off the benchmark's own thread report what every thread allocated (#2618). */
class AllThreadsRowsSpec extends AnyFlatSpec with Matchers:

  private def declared(name: String): BenchmarkRunner.Benchmark =
    BenchmarkRunner.Benchmark(name, 1, 1, () => (), () => ())

  "AllThreadsRows" should "flag the listed rows and leave the others measuring their own thread" in {
    val flagged = AllThreadsRows(List(declared("laptop.input.state_manager.move_down_up"), declared("a.b")))
    flagged.map(_.allThreads) shouldBe List(true, false)
  }

  it should "flag a row's variants under a listed prefix" in {
    val flagged = AllThreadsRows(List(declared("index.walk.cold.100k"), declared("index.walk.warm.small")))
    flagged.map(_.allThreads) shouldBe List(true, true)
  }

  it should "keep the rows in order and change nothing else about them" in {
    val rows = List(declared("x"), declared("laptop.present.swing_paint_caret_1500x1000"))
    AllThreadsRows(rows).map(_.name) shouldBe rows.map(_.name)
  }

  it should "name only rows the suite declares" in {
    AllThreadsRows.unknownIn(PerformanceBenchmarks.names) shouldBe empty
  }

  it should "flag the fifteen rows the board listed" in {
    val inSuite = PerformanceBenchmarks.names.filter(name => AllThreadsRows(List(declared(name))).head.allThreads)
    inSuite should contain allElementsOf List(
      "laptop.input.state_manager.move_down_up",
      "laptop.input.state_manager.type_and_delete",
      "laptop.input.state_manager.continuous_typing",
      "laptop.input.state_manager.page_down_up",
      "laptop.input.state_manager.typing_random_letters",
      "laptop.input.state_manager.typing_long_paragraph",
      "laptop.input.state_manager.typewriter_keystroke",
      "laptop.input.state_manager.go_to_line_3000_paragraphs",
      "laptop.input.state_manager.two_runtimes.same_text",
      "laptop.input.state_manager.two_runtimes.different_text",
      "laptop.present.swing_fresh_frame_synced_1500x1000",
      "laptop.present.swing_paint_caret_1500x1000",
      "laptop.render.typing_frame_2x.ligature_font.skia_raster",
      "index.walk.cold.100k",
      "index.walk.warm.100k"
    )
    inSuite should have size 15
  }

  "a benchmark declared without the flag" should "measure only its own thread" in {
    declared("laptop.test.plain").allThreads shouldBe false
  }
