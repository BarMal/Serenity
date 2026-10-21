package com.serenity.ui.layout

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextParagraph}
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.theme.HeadingSettings

/** Shifts a logical line's rows by its paragraph's alignment, within the room each row has left of the offset it
  * already carries (the indent a drop cap reserves).
  */
private[layout] object ParagraphAlignmentLayout:

  def apply(
    lines: Vector[TextVisualLine],
    lineIndex: Int,
    panelWidthPx: Int,
    richText: RichTextContext
  ): Vector[TextVisualLine] =
    val alignment = richText.document
      .flatMap(_.paragraphAt(lineIndex))
      .map(paragraph => paragraphAlignment(paragraph, richText.headings))
      .getOrElse(ParagraphAlignment.Left)

    if alignment == ParagraphAlignment.Left then lines
    else lines.map(line => applyAlignment(line, alignment, panelWidthPx))

  /** A paragraph's own alignment wins; left is what an unaligned paragraph holds, so only then does its heading style
    * apply.
    */
  private def paragraphAlignment(paragraph: RichTextParagraph, headings: HeadingSettings): ParagraphAlignment =
    paragraph.role match
      case ParagraphRole.Heading(level) if paragraph.alignment == ParagraphAlignment.Left =>
        headings.alignment(level).getOrElse(ParagraphAlignment.Left)
      case _ => paragraph.alignment

  private def applyAlignment(
    line: TextVisualLine,
    alignment: ParagraphAlignment,
    panelWidthPx: Int
  ): TextVisualLine =
    val availablePx = math.max(0.0f, panelWidthPx.toFloat - line.xOffsetPx - line.widthPx)
    val offsetPx =
      alignment match
        case ParagraphAlignment.Left | ParagraphAlignment.Justify => 0.0f
        case ParagraphAlignment.Center                            => availablePx / 2.0f
        case ParagraphAlignment.Right                             => availablePx

    if offsetPx <= 0.0f then line else line.copy(xOffsetPx = line.xOffsetPx + offsetPx)
