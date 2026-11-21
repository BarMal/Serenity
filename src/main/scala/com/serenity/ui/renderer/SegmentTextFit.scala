package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.ui.layout.SegmentTitleFit

/** Cuts a segment's text by pixels to the room its cells have, so a title the layout counted in cells can never run
  * into the cells beside it. The room is the cells' width less the shift the glyph pass is drawn with. A surface that
  * measures no text (a cell grid) or no known cell width leaves the text as it is: there the layout's count is exact.
  */
private[renderer] object SegmentTextFit:

  /** `advances` is what measures the text: none is a cell grid. */
  def fit(
    text: String,
    fit: SegmentTitleFit,
    font: Font,
    advances: Option[MeasuredTextAdvances],
    cellWidthPx: Int,
    insetPx: Double
  ): String =
    advances match
      case Some(measured) if cellWidthPx > 0 =>
        def advanceOf(candidate: String): Double =
          if candidate.isEmpty then 0.0 else measured.advanceOf(candidate, font).toDouble
        val availablePx = fit.cells * cellWidthPx - insetPx
        if advanceOf(text) <= availablePx then text
        else
          Iterator
            .range(text.length - 1, -1, -1)
            .map(length => text.take(length) + fit.ellipsis)
            .find(advanceOf(_) <= availablePx)
            .getOrElse("")
      case _ => text
