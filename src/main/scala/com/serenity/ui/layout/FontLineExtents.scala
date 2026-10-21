package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.FontFirstUse

/** The ascent and descent a font contributes to a visual line, probed once per `(font, render context)` by the owner of
  * this instance.
  *
  * Both are constants of the font, yet every row of every snapshot build used to allocate a fresh `LineMetrics` for
  * each font covering it.
  */
final private[serenity] class FontLineExtents:
  import FontLineExtents.*

  private val memo = MeasurementMemo[Key, Extent](maxEntries = 512) { key =>
    FontFirstUse.settle(key.font, key.frc)
    val metrics = key.font.getLineMetrics("Mg", key.frc)
    Extent(
      math.ceil(metrics.getAscent.toDouble).toInt,
      math.ceil((metrics.getDescent + metrics.getLeading).toDouble).toInt
    )
  }

  /** How many fonts were probed, as opposed to served from the memo. */
  def measurements: Long = memo.measurements

  def of(font: Font, frc: FontRenderContext): Extent = memo(Key(font, frc))

private[serenity] object FontLineExtents:

  final case class Key(font: Font, frc: FontRenderContext)

  /** Rounded separately so the room below the baseline can never be a pixel short of the font's descent and leading. */
  final case class Extent(ascentPx: Int, descentPx: Int):
    def heightPx: Int = ascentPx + descentPx
