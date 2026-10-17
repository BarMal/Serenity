package com.serenity.ui.layout

import java.awt.Font

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.theme.RichTextStyling

/** Reads a heading's space onto its rows: the room above the paragraph's first row and below its last. Like alignment,
  * it is applied when the cached rows are placed and never enters the wrap cache's key, so a change to the space wraps
  * nothing again, and the rows -- and every count of them -- are those of the heading without space.
  */
private[layout] object HeadingSpacingLayout:

  /** `rows` are a heading paragraph's rows from its first. Space below is left off a paragraph `complete` does not say
    * `rows` end, since the last row of an unfinished wrap is not the paragraph's.
    */
  def apply(
    rows: Vector[TextVisualLine],
    level: Option[Int],
    complete: Boolean,
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    richText: RichTextContext
  ): Vector[TextVisualLine] =
    level match
      case Some(heading) if rows.nonEmpty =>
        val (beforeEm, afterEm) = richText.headings.spacing(heading)
        def px(em: Float): Int  = spacePx(em, heading, font, measuredLayout, cellMetrics, richText)
        val before              = px(beforeEm)
        val after               = if complete then px(afterEm) else 0
        if before == 0 && after == 0 then rows
        else
          val opened = rows.headOption.fold(rows)(first => rows.updated(0, first.copy(spaceBeforePx = before)))
          opened.lastOption.fold(opened)(last => opened.updated(opened.length - 1, last.copy(spaceAfterPx = after)))
      case _ => rows

  /** The first row of a window has no room above it: a page opens flush, however much space its first paragraph has. */
  def droppingLeadingSpace(rows: Vector[TextVisualLine]): Vector[TextVisualLine] =
    rows.headOption match
      case Some(first) if first.spaceBeforePx != 0 => first.copy(spaceBeforePx = 0) +: rows.drop(1)
      case _                                       => rows

  /** Em of the heading's own font size on a pixel layout; whole rows on a cell grid, a half row or more being one. */
  private def spacePx(
    em: Float,
    level: Int,
    font: Font,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    richText: RichTextContext
  ): Int =
    if em <= 0.0f then 0
    else if measuredLayout then
      val bodyPx =
        if richText.document.isDefined then RichTextStyling.ProseZoomBaselinePx * richText.proseScale
        else font.getSize2D
      math.round(em * richText.headings.sizeMultiple(level) * bodyPx)
    else math.floor(em + 0.5f).toInt * cellMetrics.lineHeight
