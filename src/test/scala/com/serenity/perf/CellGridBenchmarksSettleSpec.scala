package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the cell-grid frame rows are timed once the JIT has settled, like the layout rows beside them.
  * Unsettled, the gate timed the first few hundred frames of a JVM, where code that runs once per frame is still
  * interpreted and a chrome change shows up as a cold-start cost that a user's warm editor never pays.
  */
class CellGridBenchmarksSettleSpec extends AnyFlatSpec with Matchers:

  private val frameRows = Set(
    "render.cell_grid.frame_ascii.skia_raster",
    "render.cell_grid.frame_mixed.skia_raster",
    "render.cell_grid.frame_emoji.skia_raster",
    "render.cell_grid.frame_emoji_clusters.skia_raster",
    "render.cell_grid.frame_ascii.floating_chrome.skia_raster"
  )

  private def rows = CellGridBenchmarks.benchmarks() ++ CellGridBenchmarks.floatingChromeBenchmarks()

  "the cell-grid frame rows" should "all settle the JIT before sampling" in {
    val frames = rows.filter(_.name.startsWith("render.cell_grid.frame_"))

    frames.map(_.name).toSet shouldBe frameRows
    all(frames.map(_.settleJit)) shouldBe true
  }

  "the cell-grid layout rows" should "settle the JIT before sampling, as they did" in {
    val layouts = rows.filter(_.name.startsWith("layout.cell_grid."))

    layouts should not be empty
    all(layouts.map(_.settleJit)) shouldBe true
  }
