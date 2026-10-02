package com.serenity.state.models

/** The editor pane showing chapter notes, and what it shows. Unpinned, it follows `source` -- the pane holding the
  * manuscript -- and shows the note for the chapter that pane's cursor is in; pinned, it stays on whatever it shows.
  * Runtime only: a restored session keeps the pane and its note, and Open Chapter Note makes it the notes pane again.
  */
final case class NotesPane(paneId: PaneId, source: PaneId, pinned: Boolean = false)
