package com.serenity.state.core

import com.serenity.document.{DocumentNavigation, DocumentOutline}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.SplitAxis

/** Opening the note for the chapter the cursor is in. A note's text lives in a hidden buffer, so it edits and scrolls
  * like any other pane; the manuscript's [[Annotations.notes]] records which buffer belongs to which chapter.
  */
object ChapterNoteTransitions:

  /** Shows the current chapter's note in a pane split beside the focused one, creating the note the first time. Does
    * nothing outside a chapter (before the first heading, or in a document with none) and inside a note, which has no
    * chapters of its own.
    */
  def openCurrentChapterNote(state: AppState, axis: SplitAxis)(using Balance): AppState =
    val opened =
      for
        buffer <- state.activeBuffer.filterNot(_.hidden)
        cursor <- state.activeCursorPosition
        key    <- chapterKey(buffer, cursor)
      yield
        val (withNote, noteId) = noteBuffer(state, buffer.id, key)
        showInPane(withNote, noteId, axis)
    opened.getOrElse(state)

  def toggleGhosts(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(chapterGhostsVisible = !state.runtime.chapterGhostsVisible))

  private def chapterKey(buffer: Buffer, cursor: CursorPosition): Option[NoteKey] =
    val headings = HeadingIdentity.forHeadings(DocumentOutline.forBuffer(buffer))
    DocumentNavigation
      .currentSymbol(headings.map(_._2), cursor)
      .flatMap(current =>
        headings.collectFirst { case (found, heading) if heading == current => NoteKey.Chapter(found) }
      )

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

  /** Focuses the pane already showing the note, or splits a new one for it. A split that does not happen leaves the
    * layout alone rather than putting the note over the manuscript.
    */
  private def showInPane(state: AppState, noteId: BufferId, axis: SplitAxis): AppState =
    if state.persisted.layout.editorPanes.values.exists(_.bufferId.contains(noteId)) then
      EditorState.focusBuffer(state, noteId)
    else
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
