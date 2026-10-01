package com.serenity.state.models

import com.serenity.markdown.{Emphasis, MarkdownFormatting, SourceRange}
import com.serenity.richtext.InlineMark

/** A Markdown buffer's cursors and selections as offsets into its source, and the formatting the source gives them. */
object MarkdownSelection:

  def ranges(buffer: Buffer): List[SourceRange] =
    val content = buffer.document.content
    buffer.editing.cursors.toList.map { cursor =>
      val focus = content.lineColumnToOffset(cursor.position.line, cursor.position.column)
      val anchor =
        cursor.selectionAnchor.fold(focus)(position => content.lineColumnToOffset(position.line, position.column))
      SourceRange(anchor, focus)
    }

  def emphasisOf(mark: InlineMark): Emphasis =
    mark match
      case InlineMark.Bold      => Emphasis.Bold
      case InlineMark.Italic    => Emphasis.Italic
      case InlineMark.Underline => Emphasis.Underline

  def marksAt(buffer: Buffer): Set[InlineMark] =
    val emphasis = MarkdownFormatting.emphasisAt(buffer.document.content, ranges(buffer))
    InlineMark.values.toSet.filter(mark => emphasis.contains(emphasisOf(mark)))

  def headingLevelAt(buffer: Buffer): Int =
    MarkdownFormatting.headingLevelAt(buffer.document.content, ranges(buffer))
