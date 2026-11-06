package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontProbes, ShapedTextCache}

/** The measuring paths `com.serenity.perf.FontFirstUseRepro` races, which `GlyphAdvances` keeps to this package. */
private[serenity] object FontFirstUseProbes:

  def measure(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): Unit =
    val _ = measured(text, font, frc, shaped)

  def advanceChecks(font: Font, frc: FontRenderContext): (Boolean, Boolean) =
    (
      GlyphAdvances.layoutAdvanceMatchesSum(font, frc, FontProbes()),
      GlyphAdvances.advancesAreUniform(font, frc, FontProbes())
    )

  def advancesDigest(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): String =
    val advances = measured(text, font, frc, shaped)
    advances.caretsFrom(0, advances.length).mkString(",") + "|" +
      (0 until advances.length).map(index => if advances.isContextFree(index, index + 1) then "c" else "-").mkString

  private def measured(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): GlyphAdvances =
    GlyphAdvances.measure(text, 0, text.length, 0, LineFontResolver(font, Vector.empty), frc, shaped)
