package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.FontSpec

/** The first-use measurements of a font that are `private[layout]`, reachable from the cache-ownership benchmarks in
  * `com.serenity.perf`. Each runs the same call the layout code makes, through the measurer layout measures with, so a
  * benchmark calling it with a font no cache has seen times its cold path. The render context the benchmarks pass is
  * what layout read off a font before it measured with the Skia measurer, which needs none.
  */
private[serenity] object ColdFontProbes:

  private val AdvanceText = "Hoxi quick brown fox"

  private val measurer = TestMeasure.measurer
  private val extents  = FontLineExtents(measurer.cellMetrics)

  def glyphAdvances(font: Font, frc: FontRenderContext): (Boolean, Boolean, Boolean, Int) =
    val spec = FontSpec.fromAwt(font)
    val measured =
      GlyphAdvances.measure(AdvanceText, 0, AdvanceText.length, 0, singleFontResolver(spec), measurer)
    (
      measured.isContextFree(0, measured.length),
      measured.isMeasurable(0, measured.length),
      measurer.measuresProportionally(spec),
      measured.length
    )

  def shapingBarriers(font: Font, frc: FontRenderContext): Int =
    ShapingBarriers.cutAtOrAfter(AdvanceText, 1, FontSpec.fromAwt(font), measurer)

  def measuredLayoutDecision(font: Font, frc: FontRenderContext): Boolean =
    TextLayoutSnapshot.shouldUseMeasuredLayout(FontSpec.fromAwt(font), TestMeasure.layoutMemos())

  def lineExtents(font: Font, frc: FontRenderContext): FontLineExtents.Extent = extents.of(FontSpec.fromAwt(font))
