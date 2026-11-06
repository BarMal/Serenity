package com.serenity.ui.renderer

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.LayoutRect

/** The mark a wrapped row carries in a line-number counter. On a pixel surface it is a bar inside the row's own band,
  * so no pixel of it can land in a neighbouring row's, which a frame that reuses scrolled rows repaints differently
  * from a cold one: the glyph `│` ran as tall as the face drew it, and a face whose bars join, or whose antialiasing
  * spills a pixel row, did exactly that. A cell grid keeps the glyph.
  */
private[renderer] object ContinuationMark:

  /** What the counter says on a wrapped row: nothing on a pixel surface, where [[paint]] draws the mark. */
  def text(context: RenderContext, width: Int): String =
    if context.surface.addressesPixels then " " * math.max(1, width) else glyphText(width)

  /** The bar, where the glyph's cell is, shifted by `shiftPx`; nothing on a cell grid. */
  def paint(
    context: RenderContext,
    counter: LayoutRect,
    topPx: Int,
    heightPx: Int,
    shiftPx: Int,
    color: RenderColor
  ): Unit =
    if context.surface.addressesPixels then
      val cells       = context.cellMetrics
      val thicknessPx = math.max(1, heightPx / 16)
      val barHeightPx = heightPx * 4 / 5
      val cellX       = cells.toPixelX(counter.x + (math.max(1, counter.width) - 1) / 2)
      context.surface.pixels.fillPixelRect(
        cellX + (cells.charWidth - thicknessPx) / 2 + shiftPx,
        topPx + (heightPx - barHeightPx) / 2,
        thicknessPx,
        barHeightPx,
        color
      )

  private def glyphText(width: Int): String =
    val safeWidth = math.max(1, width)
    val leftWidth = (safeWidth - 1) / 2
    " " * leftWidth + "│" + " " * (safeWidth - leftWidth - 1)
