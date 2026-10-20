package com.serenity.state.reducers

import com.serenity.lsp.model.LspTextEdit
import com.serenity.state.models.{CursorPosition, Focus, PaneId}

/** A rename part-way through: the open buffers have taken their edits, and `edits` are those for files that had to be
  * opened first. `applied`, `editedBuffers` and `heldBack` are what the first part did, for the summary shown once the
  * rest is done; `focus` and `activePane` are where the user was, since opening a file moves focus to it.
  */
final case class PendingRename(
    edits: Map[String, List[LspTextEdit]],
    anchor: CursorPosition,
    applied: Int,
    editedBuffers: Int,
    heldBack: Int,
    focus: Focus,
    activePane: Option[PaneId]
)
