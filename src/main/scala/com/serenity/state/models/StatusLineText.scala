package com.serenity.state.models

import com.serenity.config.{AppMode, StatusSegment}
import com.serenity.text.TextStatistics

/** Renders the configured status segments for the active buffer into the one line both placements show. */
object StatusLineText:

  val Separator: String = " | "

  def render(state: AppState, segments: List[StatusSegment]): Option[String] =
    Option
      .when(segments.nonEmpty)(state.activeBuffer.map(buffer => segments.map(segment(state, buffer, _))))
      .flatten
      .map(_.mkString(Separator))

  private def segment(state: AppState, buffer: Buffer, segment: StatusSegment): String =
    segment match
      case StatusSegment.Position =>
        val cursor = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
      case StatusSegment.Title =>
        buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Unsaved")
      case StatusSegment.Language =>
        buffer.document.language.fold("Plain Text")(_.displayName)
      case StatusSegment.Mode =>
        state.editingContext.mode match
          case AppMode.Code  => "Code"
          case AppMode.Prose => "Prose"
      case StatusSegment.WordCount =>
        val total = TextStatistics.of(buffer.document.content)
        state.activeSelectionTextStatistics match
          case Some(selection) => s"${selection.wordCount} of ${total.wordCount} words selected"
          case None            => s"${total.wordCount} words"
      case StatusSegment.CharCount =>
        s"${TextStatistics.of(buffer.document.content).characterCount} chars"
      case StatusSegment.ReadingTime =>
        s"~${TextStatistics.of(buffer.document.content).readingTimeMinutes} min read"
