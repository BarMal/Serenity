package com.serenity.state.models

import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}
import com.serenity.richtext.{InlineMark, RichTextStyle}

/** The formatting the caret shows: what the next character typed at the insertion point would get. */
final case class CaretStyle(
    italic: Boolean = false,
    bold: Boolean = false,
    underline: Boolean = false,
    strikethrough: Boolean = false
):
  def isPlain: Boolean = !(italic || bold || underline || strikethrough)

object CaretStyle:

  val Plain: CaretStyle = CaretStyle()

  /** A rich-text buffer's style at `position`: the style toggled for the next character if there is one, otherwise
    * that of the text before the caret. Plain for a buffer with no rich text, or whose rich text no longer matches its
    * content.
    */
  def forRichText(buffer: Buffer, position: CursorPosition): CaretStyle =
    buffer.richText.richTextDocument.filter(_ => buffer.richTextInSync).fold(Plain) { document =>
      buffer.richText.insertionRichTextStyle
        .orElse(ContextualToolbarRichTextStyle.styleAtCursor(position, document))
        .fold(Plain)(fromRichText)
    }

  /** The emphasis and strikethrough around `column` on a Markdown line: a caret at the end of a styled stretch, or in
    * it, shows its style, and the marker characters themselves show none.
    */
  def forMarkdownRuns(runs: Vector[Run], column: Int): CaretStyle =
    val content = runs.filterNot(_.isMarker)
    content
      .find(run => run.startColumn < column && column <= run.endColumn)
      .orElse(content.find(run => run.startColumn <= column && column < run.endColumn))
      .fold(Plain)(run => fromMarkdown(run.style))

  private def fromRichText(style: RichTextStyle): CaretStyle =
    CaretStyle(
      italic = style.marks.contains(InlineMark.Italic),
      bold = style.marks.contains(InlineMark.Bold),
      underline = style.marks.contains(InlineMark.Underline)
    )

  private def fromMarkdown(style: Style): CaretStyle =
    CaretStyle(italic = style.italic, bold = style.bold, strikethrough = style.strike)
