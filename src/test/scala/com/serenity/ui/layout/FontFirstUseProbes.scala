package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontSpec, ShapedTextCache}

/** The measuring paths `com.serenity.perf.FontFirstUseRepro` races, which `GlyphAdvances` keeps to this package. Layout
  * measures with the Skia measurer, so the render context and shaped-text cache the repro hands in are what the AWT
  * paths it also races read.
  */
private[serenity] object FontFirstUseProbes:

  def measure(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): Unit =
    val _ = measured(text, font)

  def advanceChecks(font: Font, frc: FontRenderContext): (Boolean, Boolean) =
    val spec = FontSpec.fromAwt(font)
    (TestMeasure.measurer.measuresProportionally(spec), TestMeasure.measurer.mayMeasureByAdvances("Hoxi "))

  def advancesDigest(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): String =
    val advances = measured(text, font)
    advances.caretsFrom(0, advances.length).mkString(",") + "|" +
      (0 until advances.length).map(index => if advances.isContextFree(index, index + 1) then "c" else "-").mkString

  private def measured(text: String, font: Font): GlyphAdvances =
    GlyphAdvances.measure(
      text,
      0,
      text.length,
      0,
      LineFontResolver(FontSpec.fromAwt(font), Vector.empty),
      TestMeasure.measurer
    )
