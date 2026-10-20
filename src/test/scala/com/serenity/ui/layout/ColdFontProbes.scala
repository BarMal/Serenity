package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.ShapedTextCache

/** The first-use probes of a font that are `private[layout]`, reachable from the cache-ownership benchmarks in
  * `com.serenity.perf`. Each runs the same call the layout code makes, so a benchmark calling it with a font no cache
  * has seen times a probe's cold path.
  */
private[serenity] object ColdFontProbes:

  private val AdvanceText = "Hoxi quick brown fox"

  private val shapedText = ShapedTextCache()
  private val extents    = FontLineExtents()

  def glyphAdvances(font: Font, frc: FontRenderContext): (Boolean, Boolean, Boolean, Int) =
    val measured =
      GlyphAdvances.measure(AdvanceText, 0, AdvanceText.length, 0, singleFontResolver(font), frc, shapedText)
    (
      GlyphAdvances.layoutAdvanceMatchesSum(font, frc),
      GlyphAdvances.shapingMaySubstitute(font, frc),
      GlyphAdvances.advancesAreUniform(font, frc),
      measured.length
    )

  def shapingBarriers(font: Font, frc: FontRenderContext): Int =
    ShapingBarriers.cutAtOrAfter(AdvanceText, 1, font, frc, shapedText)

  def measuredLayoutDecision(font: Font, frc: FontRenderContext): Boolean =
    TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc)

  def lineExtents(font: Font, frc: FontRenderContext): FontLineExtents.Extent = extents.of(font, frc)
