package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, TextLayout}
import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.layout.SegmentTitleFit

/** Cuts a segment's text by pixels to the room its cells have, so a title the layout counted in cells can never run into
  * the cells beside it. The room is the cells' width less the shift the glyph pass is drawn with. A surface with no font
  * render context (a cell grid) or no known cell width leaves the text as it is: there the layout's count is exact.
  */
private[renderer] object SegmentTextFit:

  final private case class Key(text: String, font: Font, context: FontRenderContext)

  private val Capacity = 512

  private val advances = new ConcurrentHashMap[Key, java.lang.Double]()

  private def advanceOf(text: String, font: Font, context: FontRenderContext): Double =
    if text.isEmpty then 0.0
    else
      if advances.size() >= Capacity then advances.clear()
      advances.computeIfAbsent(
        Key(text, font, context),
        key => java.lang.Double.valueOf(TextLayout(key.text, key.font, key.context).getAdvance.toDouble)
      )

  def fit(
    text: String,
    fit: SegmentTitleFit,
    font: Font,
    context: Option[FontRenderContext],
    cellWidthPx: Int,
    insetPx: Double
  ): String =
    context match
      case Some(renderContext) if cellWidthPx > 0 =>
        val availablePx = fit.cells * cellWidthPx - insetPx
        if advanceOf(text, font, renderContext) <= availablePx then text
        else
          Iterator
            .range(text.length - 1, -1, -1)
            .map(length => text.take(length) + fit.ellipsis)
            .find(advanceOf(_, font, renderContext) <= availablePx)
            .getOrElse("")
      case _ => text
