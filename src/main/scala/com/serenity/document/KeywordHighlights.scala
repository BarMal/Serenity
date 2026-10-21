package com.serenity.document

import com.serenity.rope.Rope
import com.serenity.state.models.{AppState, BufferId, CursorPosition, NoteKey}

/** One occurrence of a keyword, painted over the prose. */
final case class KeywordHighlight(start: CursorPosition, end: CursorPosition)

/** Keyword occurrences are highlighted only while the notes pane is open on the document that owns the notes, so the
  * highlights appear when the writer is looking at notes and not otherwise.
  */
object KeywordHighlights:

  /** The keywords painted in `bufferId`: those of its keyword notes, while the notes pane follows it and the highlights
    * are switched on.
    */
  def paintedTerms(state: AppState, bufferId: BufferId): Set[String] =
    if !state.runtime.keywordHighlightsVisible then Set.empty
    else
      val terms =
        for
          pane   <- state.runtime.notesPane
          source <- state.persisted.layout.editorPanes.get(pane.source).flatMap(_.bufferId)
          if source == bufferId
          buffer <- state.persisted.buffers.get(bufferId)
        yield buffer.annotations.notes.keySet.collect { case NoteKey.Keyword(term) => term }
      terms.getOrElse(Set.empty)

  /** The highlights on `lines`, where the longer of two overlapping terms wins. */
  def onLines(content: Rope, terms: Set[String], lines: Set[Int]): Map[Int, List[KeywordHighlight]] =
    if terms.isEmpty then Map.empty
    else
      lines.iterator
        .flatMap(line => content.getLine(line).map(text => line -> highlightsOn(line, text, terms)))
        .filter(_._2.nonEmpty)
        .toMap

  private def highlightsOn(line: Int, text: String, terms: Set[String]): List[KeywordHighlight] =
    val found = terms.toList.flatMap(term => KeywordMatches.occurrences(text, term))
    val kept = found
      .sortBy(occurrence => (occurrence.start, -(occurrence.end - occurrence.start)))
      .foldLeft(List.empty[KeywordOccurrence]) { (acc, occurrence) =>
        if acc.headOption.exists(_.end > occurrence.start) then acc else occurrence :: acc
      }
    kept.reverse.map(o => KeywordHighlight(CursorPosition(line, o.start), CursorPosition(line, o.end)))
