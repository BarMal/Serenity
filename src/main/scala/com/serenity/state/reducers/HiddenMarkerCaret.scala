package com.serenity.state.reducers

import com.serenity.markdown.{MarkdownBlockSpans, MarkdownInlineSpans, MarkdownInlineView, MarkerMode}
import com.serenity.state.models.{AppState, Buffer, CursorPosition}

/** Keeps the caret off Markdown markers the read view hides. A hidden marker has no width, so a caret before it would
  * sit at the same x as the caret after it and one key press would appear to do nothing; a caret may therefore only be
  * in front of a character the writer can see, or at the end of the line. Live preview needs none of this: it shows
  * every marker on the lines the caret is on.
  */
private[reducers] object HiddenMarkerCaret:

  /** `landed`, or the nearest position in front of a visible character when it falls in front of a hidden one: further
    * along the line when the caret moved forward, back along it when the caret moved back.
    */
  def settle(state: AppState, buffer: Buffer, from: CursorPosition, landed: CursorPosition): CursorPosition =
    if !readModeHidesMarkers(state, buffer) then landed
    else
      val hidden = hiddenColumns(state, buffer, landed.line)
      if !hidden.contains(landed.column) then landed
      else
        val lineLength = buffer.document.content.getLine(landed.line).fold(0)(_.length)
        val forward    = (landed.column + 1 to lineLength).find(!hidden.contains(_))
        val back       = (landed.column - 1 to 0 by -1).find(!hidden.contains(_))
        val movedBack  = landed.line < from.line || (landed.line == from.line && landed.column < from.column)
        val column     = (if movedBack then back.orElse(forward) else forward.orElse(back)).getOrElse(landed.column)
        landed.copy(column = column)

  private def readModeHidesMarkers(state: AppState, buffer: Buffer): Boolean =
    MarkerMode.of(
      state.persisted.config.inlineMarkdownViewMode,
      buffer.document.language,
      state.runtime.capabilities.isCellGrid
    ) == MarkerMode.Read

  private def hiddenColumns(state: AppState, buffer: Buffer, line: Int): Set[Int] =
    val content = buffer.document.content
    val text    = content.getLine(line).getOrElse("")
    if !MarkdownBlockSpans.mayContainMarkup(text) then Set.empty
    else
      val fences = state.runtime.markdownBlockIndexes.of(content)
      MarkdownInlineSpans
        .hiddenColumns(MarkdownInlineView(MarkerMode.Read, Vector.empty, fences).runsOn(line, text))
        .toSet
