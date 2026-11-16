package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.FontSpec

/** The cell metrics the GUI's grid is sized by: what the Skia measurer reports for an AWT font, so the grid, the layout
  * and the glyphs a surface draws all come from one measurement.
  */
object SkiaCellMetrics:

  def measuring(): Font => CellMetrics =
    val measurer = new SkiaTextMeasurer()
    font => measurer.cellMetrics(FontSpec.fromAwt(font))
