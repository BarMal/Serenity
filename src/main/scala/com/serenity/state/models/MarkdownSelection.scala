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

  /** `None` for a mark Markdown has no syntax for, such as a shadow. */
  def emphasisOf(mark: InlineMark): Option[Emphasis] =
    mark match
      case InlineMark.Bold      => Some(Emphasis.Bold)
      case InlineMark.Italic    => Some(Emphasis.Italic)
      case InlineMark.Underline => Some(Emphasis.Underline)
      case InlineMark.Shadow    => None

  def marksAt(buffer: Buffer): Set[InlineMark] =
    val emphasis = MarkdownFormatting.emphasisAt(buffer.document.content, ranges(buffer))
    InlineMark.values.toSet.filter(mark => emphasisOf(mark).exists(emphasis.contains))

  def headingLevelAt(buffer: Buffer): Int =
    MarkdownFormatting.headingLevelAt(buffer.document.content, ranges(buffer))
