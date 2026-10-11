package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

/** The line height and ascent a font contributes to a visual line, probed once per `(font, render context)`.
  *
  * Both are constants of the font, yet every row of every snapshot build used to allocate a fresh `LineMetrics` for
  * each font covering it.
  */
private[layout] object FontLineExtents:

  final case class Key(font: Font, frc: FontRenderContext)

  final case class Extent(heightPx: Int, ascentPx: Int)

  val shared: MeasurementMemo[Key, Extent] =
    MeasurementMemo[Key, Extent](maxEntries = 512) { key =>
      val metrics = key.font.getLineMetrics("Mg", key.frc)
      Extent(math.ceil(metrics.getHeight.toDouble).toInt, math.ceil(metrics.getAscent.toDouble).toInt)
    }
