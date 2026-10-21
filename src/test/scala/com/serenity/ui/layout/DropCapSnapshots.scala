package com.serenity.ui.layout

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.state.models.{TextCaretStop, TextVisualLine}

/** Measured snapshots of paragraphs whose row counts are given outright, so the rows a drop cap glyph spans can be
  * checked without wrapping any text.
  */
object DropCapSnapshots:

  val RowHeightPx = 16

  /** A paragraph of `rows` visual rows. `firstRow` skips that many leading rows, as a paragraph scrolled partly out of
    * view does; `heightsPx` gives rows heights of their own. A drop cap's home row records the height of its glyph:
    * `glyphHeightPx`, or the room of the cap's own lines when none is given.
    */
  final case class Paragraph(
      role: ParagraphRole,
      rows: Int,
      firstRow: Int = 0,
      heightsPx: Map[Int, Int] = Map.empty,
      glyphHeightPx: Option[Int] = None
  )

  def dropCap(lines: Int, rows: Int): Paragraph = Paragraph(ParagraphRole.dropCap(lines), rows)

  def body(rows: Int): Paragraph = Paragraph(ParagraphRole.Body, rows)

  def snapshot(paragraphs: Paragraph*): TextLayoutSnapshot =
    snapshotOf(documentOf(paragraphs), paragraphs)

  /** As [[snapshot]], over the document `earlier` was laid out from: row reuse compares frames of one document. */
  def snapshotSharingDocument(earlier: TextLayoutSnapshot)(paragraphs: Paragraph*): TextLayoutSnapshot =
    snapshotOf(earlier.richTextDocument.getOrElse(documentOf(paragraphs)), paragraphs)

  private def documentOf(paragraphs: Seq[Paragraph]): RichTextDocument =
    RichTextDocument(paragraphs.toList.zipWithIndex.map { (paragraph, line) =>
      RichTextParagraph.plain(s"paragraph $line", role = paragraph.role)
    })

  private def snapshotOf(document: RichTextDocument, paragraphs: Seq[Paragraph]): TextLayoutSnapshot =
    TextLayoutSnapshot(
      visualLines = paragraphs.toVector.zipWithIndex.flatMap((paragraph, line) => rowsOf(paragraph, line)),
      panelWidthPx = 320,
      lineHeightPx = RowHeightPx,
      ascentPx = 13,
      usesMeasuredLayout = true,
      richTextDocument = Some(document)
    )

  private def rowsOf(paragraph: Paragraph, bufferLine: Int): Vector[TextVisualLine] =
    (paragraph.firstRow until paragraph.firstRow + paragraph.rows).toVector.map { row =>
      val line = TextVisualLine(
        bufferLine = bufferLine,
        startColumn = row * 10,
        endColumn = row * 10 + 10,
        text = "0123456789",
        widthPx = 80.0f,
        caretStops = (0 to 10).toVector.map(offset => TextCaretStop(row * 10 + offset, offset * 8.0f)),
        heightPx = paragraph.heightsPx.getOrElse(row, RowHeightPx),
        ascentPx = 13
      )
      if row == 0 then line.copy(row = line.row.copy(dropCapHeightPx = homeGlyphHeightPx(paragraph))) else line
    }

  private def homeGlyphHeightPx(paragraph: Paragraph): Int =
    paragraph.glyphHeightPx.getOrElse(DropCapLayout.glyphHeightPx(paragraph.role, RowHeightPx))
