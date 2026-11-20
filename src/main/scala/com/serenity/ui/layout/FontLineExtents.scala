package com.serenity.ui.layout

import com.serenity.ui.renderer.FontSpec

/** The ascent and descent a font contributes to a visual line, read once per font from the cell metrics its owner's
  * measurer gives it.
  *
  * Both are constants of the font, yet every row of every snapshot build would otherwise ask the measurer again for
  * each font covering it.
  */
final private[serenity] class FontLineExtents(cellMetricsOf: FontSpec => CellMetrics):
  import FontLineExtents.*

  private val memo = MeasurementMemo[FontSpec, Extent](maxEntries = 512) { font =>
    val cell = cellMetricsOf(font)
    Extent(cell.ascent, (cell.lineHeight - cell.ascent).max(0))
  }

  /** How many fonts were probed, as opposed to served from the memo. */
  def measurements: Long = memo.measurements

  def of(font: FontSpec): Extent = memo(font)

private[serenity] object FontLineExtents:

  /** A font's line box split at the baseline: a row of several fonts is as tall as the highest ascent over the deepest
    * descent.
    */
  final case class Extent(ascentPx: Int, descentPx: Int):
    def heightPx: Int = ascentPx + descentPx
