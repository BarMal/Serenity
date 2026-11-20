package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.renderer.FontSpec

/** Skia's own vertical metrics of a font, for specs that name what a line of it measures: ascent (negative above the
  * baseline), descent and leading, read from the faces the measurer lays out with.
  */
object SkiaTestMetrics:

  def of(font: Font): (Float, Float, Float) =
    SkiaVerticalMetrics.of(com.serenity.testkit.TestMeasure.faces.faceFor(FontSpec.fromAwt(font)).font)
