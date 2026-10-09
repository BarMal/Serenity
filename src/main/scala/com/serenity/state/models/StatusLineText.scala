package com.serenity.state.models

import com.serenity.config.{AppMode, StatusSegment}
import com.serenity.lsp.config.LanguageId
import com.serenity.text.TextStatistics

/** Renders the configured status segments for the active buffer into the one line both placements show. */
object StatusLineText:

  val Separator: String = " | "

  val SafeModeLabel: String = "Safe mode"

  def render(state: AppState, segments: List[StatusSegment]): Option[String] =
    Option
      .when(segments.nonEmpty)(state.activeBuffer.map(buffer => segments.map(segment(state, buffer, _))))
      .flatten
      .map(_.mkString(Separator))

  private def segment(state: AppState, buffer: Buffer, segment: StatusSegment): String =
    segment match
      case StatusSegment.Position =>
        val cursor = buffer.editing.cursorPositions.headOption.getOrElse(CursorPosition(0, 0))
        s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
      case StatusSegment.Title =>
        buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Unsaved")
      case StatusSegment.Language =>
        buffer.document.language.fold("Plain Text")(language => withServerWork(state, language))
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
      case StatusSegment.WordGoal =>
        state.persisted.config.documentConfig.wordGoal match
          case None => "No word goal set"
          case Some(goal) =>
            val total   = TextStatistics.of(buffer.document.content).wordCount
            val percent = if goal <= 0 then 0 else math.min(100, total * 100 / goal)
            s"$total / $goal words ($percent%)"
      case StatusSegment.LineEnding => lineEnding(buffer.document)

  private def lineEnding(document: Document): String =
    val label = document.lineEnding.label
    if document.mixedLineEndings.isDefined then s"$label (file was mixed)" else label

  /** The language name followed by what its server is doing right now, such as `Scala (Indexing workspace 40%)`. */
  private def withServerWork(state: AppState, language: LanguageId): String =
    state.runtime.languageService.progress.get(language).flatMap(_.headOption) match
      case Some(task) => s"${language.displayName} (${task.display})"
      case None       => language.displayName
