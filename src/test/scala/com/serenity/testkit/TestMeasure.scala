package com.serenity.testkit

import java.awt.Font

import com.serenity.ui.layout.CellMetrics

/** What a spec measures a font with. Layout specs ask here, and not the measuring code directly, for the numbers they
  * lay text out in, so the day layout moves to the Skia measurer (#2660 item 2, step D2) is one change in this file and
  * not in every spec.
  */
object TestMeasure:

  def cellMetrics(font: Font): CellMetrics = CellMetrics.fromFont(font)
