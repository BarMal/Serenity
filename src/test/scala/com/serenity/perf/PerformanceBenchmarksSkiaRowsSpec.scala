package com.serenity.perf

import java.awt.GraphicsEnvironment

import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skia is the default renderer, so the suite times its CPU raster surface (#2575). The rows sit under `skia.` and
  * `scripts/perf-gate-excluded-rows.txt` keeps them off the per-PR gate.
  */
class PerformanceBenchmarksSkiaRowsSpec extends AnyFlatSpec with Matchers:

  private def suiteNames: List[String] =
    assume(!GraphicsEnvironment.isHeadless, "Building the suite needs a window, which needs a display")
    DisplayExclusive(PerformanceBenchmarks.withSuite(_.map(_.name)))

  private val frameRows = List("typing_frame_2x", "scroll_frame_2x", "typing_new_letter_2x")
  private val fonts     = List("system_monospace", "bundled_ligature_font")

  "The performance suite" should "time Skia's raster surface on the typing and scroll frames, in both fonts" in {
    val names = suiteNames
    for
      font <- fonts
      row  <- frameRows
    do names should contain(s"skia.compare.skia_raster.$font.$row")
  }

  it should "keep every Skia row under the skia. prefix, except the ported rows that end in .skia_raster" in
    suiteNames
      .filter(_.toLowerCase.contains("skia"))
      .filterNot(_.endsWith(".skia_raster"))
      .foreach(_ should startWith("skia."))
