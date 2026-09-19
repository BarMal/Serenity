package com.serenity.state.models

import com.serenity.richtext.*

/** Where the toolbar reads a prose buffer's active rich-text style and paragraph from: the selection when there is one,
  * otherwise the caret. Split out of `ContextualToolbar` so each stays under the architecture size targets.
  */
private[models] object ContextualToolbarRichTextStyle:

  def richTextDocumentFor(buffer: Buffer): RichTextDocument =
    val text = buffer.document.content.collect()
    buffer.richText.richTextDocument
      .filter(_.matchesPlainText(text))
      .getOrElse(RichTextDocument.fromPlainText(text))

  def activeParagraph(
    buffer: Buffer,
    document: RichTextDocument
  ): Option[com.serenity.richtext.RichTextParagraph] =
    val paragraphIndex = currentRange(buffer, document).start.paragraphIndex
    document.paragraphs.lift(paragraphIndex)

  def activeStyle(
    buffer: Buffer,
    document: RichTextDocument
  ): RichTextStyle =
    buffer.primarySelection match
      case Some(selection) if selection.start != selection.end =>
        styleForSelection(selection, document)
      case _ =>
        buffer.editing.cursorPositions.headOption
          .flatMap(cursor => styleAtCursor(cursor, document))
          .getOrElse(RichTextStyle.empty)

  def styleForSelection(
    selection: Selection,
    document: RichTextDocument
  ): RichTextStyle =
    val range = richTextRange(selection)
    document.paragraphs
      .lift(range.start.paragraphIndex)
      .flatMap(paragraph => styleAtParagraphOffset(paragraph, range.start.offset))
      .getOrElse(styleAtCursor(selection.focus, document).getOrElse(RichTextStyle.empty))

  def styleAtCursor(
    cursor: CursorPosition,
    document: RichTextDocument
  ): Option[RichTextStyle] =
    val paragraphIndex = cursor.line.max(0).min(document.paragraphs.length - 1)
    document.paragraphs
      .lift(paragraphIndex)
      .flatMap(paragraph => styleAtParagraphOffset(paragraph, (cursor.column - 1).max(0)))

  def styleAtParagraphOffset(
    paragraph: com.serenity.richtext.RichTextParagraph,
    offset: Int
  ): Option[RichTextStyle] =
    val clampedOffset = offset.max(0).min(paragraph.plainText.length)
    val targetOffset =
      if clampedOffset == paragraph.plainText.length && clampedOffset > 0 then clampedOffset - 1
      else clampedOffset
    paragraph.runs
      .foldLeft((0, Option.empty[RichTextStyle])) {
        case ((currentOffset, found), run) =>
          val nextOffset     = currentOffset + run.text.length
          val containsOffset = targetOffset >= currentOffset && targetOffset < nextOffset
          (nextOffset, found.orElse(Option.when(containsOffset)(run.style)))
      }
      ._2

  def currentRange(
    buffer: Buffer,
    document: RichTextDocument
  ): RichTextRange =
    buffer.primarySelection
      .map(richTextRange)
      .orElse(
        buffer.editing.cursorPositions.headOption
          .map(cursor => RichTextRange(richTextPosition(cursor, document), richTextPosition(cursor, document)))
      )
      .getOrElse(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 0)))

  def richTextRange(selection: Selection): RichTextRange =
    RichTextRange(
      start = RichTextPosition(selection.start.line, selection.start.column),
      end = RichTextPosition(selection.end.line, selection.end.column)
    ).normalized

  def richTextPosition(
    cursor: CursorPosition,
    document: RichTextDocument
  ): RichTextPosition =
    val paragraphIndex = cursor.line.max(0).min((document.paragraphs.length - 1).max(0))
    val offset = document.paragraphs
      .lift(paragraphIndex)
      .map(_.plainText.length)
      .map(length => cursor.column.max(0).min(length))
      .getOrElse(0)
    RichTextPosition(paragraphIndex, offset)

end ContextualToolbarRichTextStyle
