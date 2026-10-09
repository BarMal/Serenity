package com.serenity.state.core

import com.serenity.document.{DocumentNavigation, DocumentOutline, KeywordMatches}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{SplitAxis, Symbol}

/** Opening the note for the chapter the cursor is in. A note's text lives in a hidden buffer, so it edits and scrolls
  * like any other pane; the manuscript's [[Annotations.notes]] records which buffer belongs to which chapter.
  */
object ChapterNoteTransitions:

  /** Shows the current chapter's note in the notes pane -- a pane split beside the focused one the first time, the same
    * pane after that -- creating the note the first time, and makes that pane follow the cursor's chapter. Does nothing
    * outside a chapter (before the first heading, or in a document with none) and inside a note, which has no chapters
    * of its own.
    */
  def openCurrentChapterNote(state: AppState, axis: SplitAxis)(using Balance): AppState =
    openNote(state, axis)(buffer => chapterKeyAt(buffer, buffer.editing.cursors.head.position))

  /** Shows the note for the word under the cursor (or the words selected on one line) the same way, creating it the
    * first time. The note is keyed by the words' normalised text, so every spelling and every occurrence reaches it.
    * Does nothing when the cursor is not on a word, and inside a note.
    */
  def openCurrentKeywordNote(state: AppState, axis: SplitAxis)(using Balance): AppState =
    openNote(state, axis)(keywordUnderCursor)

  private def openNote(state: AppState, axis: SplitAxis)(keyFor: Buffer => Option[NoteKey])(using
    Balance
  ): AppState =
    val opened =
      for
        buffer <- state.activeBuffer.filterNot(_.hidden)
        key    <- keyFor(buffer)
      yield
        val sourcePane         = state.persisted.layout.activeEditorPaneId
        val (withNote, noteId) = noteBuffer(state, buffer.id, key)
        registered(showInPane(withNote, noteId, axis), sourcePane)
    opened.getOrElse(state)

  def toggleGhosts(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(chapterGhostsVisible = !state.runtime.chapterGhostsVisible))

  def toggleTermHighlights(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(keywordHighlightsVisible = !state.runtime.keywordHighlightsVisible))

  /** Pins the notes pane to the note it shows, or lets it follow the cursor's chapter again. */
  def toggleNotesPin(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(notesPane = state.runtime.notesPane.map(pane => pane.copy(pinned = !pane.pinned)))
    )

  /** The keyword note a position touches: the longest of the buffer's keywords occurring there. */
  def keywordKeyAt(buffer: Buffer, cursor: CursorPosition): Option[NoteKey] =
    val terms = buffer.annotations.notes.keys.collect { case NoteKey.Keyword(term) => term }
    Option
      .when(terms.nonEmpty)(terms)
      .flatMap(listed =>
        buffer.document.content
          .getLine(cursor.line)
          .flatMap(line => KeywordMatches.termAt(line, cursor.column, listed))
      )
      .map(NoteKey.Keyword(_))

  /** A buffer's chapter headings in document order, parsed again only when its text changes (#1848). */
  val chapterHeadings: DerivedValue[Buffer, Buffer, List[(HeadingIdentity, Symbol)]] =
    DerivedValue(
      inputs = identity,
      references = buffer => List(buffer.document.content, buffer.document.language, buffer.richText.richTextDocument),
      compute = buffer => HeadingIdentity.forHeadings(DocumentOutline.forBuffer(buffer))
    )

  /** The note of the chapter whose heading is the last at or before a position in `buffer`. */
  def chapterKeyAt(buffer: Buffer, cursor: CursorPosition): Option[NoteKey] =
    chapterKeyIn(chapterHeadings.compute(buffer), cursor)

  def chapterKeyIn(headings: List[(HeadingIdentity, Symbol)], cursor: CursorPosition): Option[NoteKey] =
    DocumentNavigation
      .currentSymbol(headings.map(_._2), cursor)
      .flatMap(current =>
        headings.collectFirst { case (found, heading) if heading == current => NoteKey.Chapter(found) }
      )

  private def keywordUnderCursor(buffer: Buffer): Option[NoteKey] =
    val cursor = buffer.editing.cursors.head
    buffer.document.content
      .getLine(cursor.position.line)
      .flatMap { line =>
        val selected = cursor.selection
          .filter(selection => selection.anchor.line == selection.focus.line)
          .map { selection =>
            val start = selection.anchor.column.min(selection.focus.column)
            val end   = selection.anchor.column.max(selection.focus.column)
            line.slice(start, end)
          }
        selected
          .map(KeywordMatches.normalized)
          .filter(_.nonEmpty)
          .orElse(KeywordMatches.wordAt(line, cursor.position.column).map(KeywordMatches.normalized))
      }
      .filter(_.nonEmpty)
      .map(NoteKey.Keyword(_))

  private def noteBuffer(state: AppState, ownerId: BufferId, key: NoteKey)(using Balance): (AppState, BufferId) =
    val existing = state.persisted.buffers.get(ownerId).flatMap(_.annotations.notes.get(key))
    existing.map(_.overview).filter(state.persisted.buffers.contains) match
      case Some(noteId) => (state, noteId)
      case None         => createNote(state, ownerId, key)

  private def createNote(state: AppState, ownerId: BufferId, key: NoteKey)(using Balance): (AppState, BufferId) =
    val noteId = state.runtime.nextBufferId
    val buffers = state.persisted.buffers
      .updated(noteId, Buffer.empty(noteId).copy(hidden = true))
      .updatedWith(ownerId)(_.map(owner => owner.copy(annotations = withNote(owner.annotations, key, noteId))))
    val created = state.copy(
      persisted = state.persisted.copy(buffers = buffers),
      runtime = state.runtime.copy(nextBufferId = BufferId(noteId.value + 1))
    )
    (created, noteId)

  private def withNote(annotations: Annotations, key: NoteKey, noteId: BufferId): Annotations =
    annotations.copy(notes = annotations.notes.updated(key, Notes(noteId)))

  /** Focuses the pane already showing the note, else reuses the notes pane for it, else splits a new pane beside the
    * focused one. A split that does not happen leaves the layout alone rather than putting the note over the
    * manuscript.
    */
  private def showInPane(state: AppState, noteId: BufferId, axis: SplitAxis): AppState =
    val panes     = state.persisted.layout.editorPanes
    val notesPane = state.runtime.notesPane.map(_.paneId).filter(panes.contains)
    if panes.values.exists(_.bufferId.contains(noteId)) then EditorState.focusBuffer(state, noteId)
    else
      notesPane.fold(splitForNote(state, noteId, axis))(paneId =>
        EditorState.focusBuffer(withPaneBuffer(state, paneId, noteId), noteId)
      )

  private def splitForNote(state: AppState, noteId: BufferId, axis: SplitAxis): AppState =
    val split = EditorState.splitFocusedPane(state, axis)
    split.persisted.layout.editorPanes.keySet.diff(state.persisted.layout.editorPanes.keySet).toList match
      case List(paneId) =>
        val layout = split.persisted.layout
        split.copy(persisted =
          split.persisted.copy(layout =
            layout.copy(editorPanes = layout.editorPanes.updated(paneId, EditorPane.withBuffer(paneId, noteId)))
          )
        )
      case _ => state

  private def withPaneBuffer(state: AppState, paneId: PaneId, bufferId: BufferId): AppState =
    val layout = state.persisted.layout
    state.copy(persisted =
      state.persisted.copy(layout =
        layout.copy(editorPanes = layout.editorPanes.updatedWith(paneId)(_.map(_.copy(bufferId = Some(bufferId)))))
      )
    )

  /** Makes the pane now showing the note the notes pane, following `sourcePane`. */
  private def registered(shown: AppState, sourcePane: Option[PaneId]): AppState =
    (shown.persisted.layout.activeEditorPaneId, sourcePane) match
      case (Some(paneId), Some(source)) if paneId != source =>
        shown.copy(runtime = shown.runtime.copy(notesPane = Some(NotesPane(paneId, source))))
      case _ => shown
