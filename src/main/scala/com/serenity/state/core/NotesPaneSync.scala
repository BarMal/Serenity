package com.serenity.state.core

import com.serenity.state.models.*
import com.serenity.ui.layout.Symbol

/** Keeps the notes pane on the note for the chapter its source pane's cursor is in. Runs on every commit, so no event
  * source has to remember to retarget it, and does nothing unless the cursor changed line, the source buffer changed,
  * or the notes pane or the notes themselves changed -- so a keystroke inside a chapter never re-parses the document,
  * and the headings are re-parsed only when the text changed since the last retarget (#1848). A heading typed or
  * deleted under the cursor is picked up when the cursor next changes line.
  *
  * Only the pane's buffer is swapped. Scroll position and cursor live on each note's own buffer, so a note comes back
  * exactly as it was left.
  */
object NotesPaneSync:

  def synced(state: AppState, previous: AppState): AppState =
    state.runtime.notesPane.fold(state)(pane => followed(state, previous, pane))

  private def followed(state: AppState, previous: AppState, pane: NotesPane): AppState =
    val panes = state.persisted.layout.editorPanes
    if !panes.contains(pane.paneId) || !panes.contains(pane.source) then forgotten(state)
    else if pane.pinned || fingerprint(state, pane) == fingerprint(previous, pane) then state
    else sourceBuffer(state, pane).fold(showing(state, pane, None))(source => retargeted(state, pane, source))

  /** What the notes pane's target depends on. `previous` is read against the same `pane` so a newly registered or newly
    * unpinned pane always differs from before.
    */
  private def fingerprint(
    state: AppState,
    pane: NotesPane
  ): (Option[NotesPane], Option[BufferId], Option[Int], Option[Map[NoteKey, Notes]]) =
    val source = sourceBuffer(state, pane)
    (
      state.runtime.notesPane,
      source.map(_.id),
      source.flatMap(_.editing.cursorPositions.headOption).map(_.line),
      source.map(_.annotations.notes)
    )

  private def sourceBuffer(state: AppState, pane: NotesPane): Option[Buffer] =
    state.persisted.layout.editorPanes
      .get(pane.source)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .filterNot(_.hidden)

  private def retargeted(state: AppState, pane: NotesPane, source: Buffer): AppState =
    val memos      = state.runtime.chapterHeadingMemo
    val headings   = ChapterNoteTransitions.chapterHeadings.refreshed(memos.forBuffer(source.id), source)
    val remembered = memos.holding(source.id, headings)
    val withMemo =
      if remembered eq memos then state else state.copy(runtime = state.runtime.copy(chapterHeadingMemo = remembered))
    showing(withMemo, pane, wantedNote(source, headings.value))

  private def wantedNote(source: Buffer, headings: List[(HeadingIdentity, Symbol)]): Option[BufferId] =
    for
      cursor <- source.editing.cursorPositions.headOption
      key    <- ChapterNoteTransitions.chapterKeyIn(headings, cursor)
      notes  <- source.annotations.notes.get(key)
    yield notes.overview

  private def showing(state: AppState, pane: NotesPane, wanted: Option[BufferId]): AppState =
    val layout = state.persisted.layout
    layout.editorPanes.get(pane.paneId).filter(_.bufferId != wanted).fold(state) { current =>
      state.copy(persisted =
        state.persisted.copy(layout =
          layout.copy(editorPanes = layout.editorPanes.updated(pane.paneId, current.copy(bufferId = wanted)))
        )
      )
    }

  private def forgotten(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(notesPane = None))
