package com.serenity.state.models

/** One clause of pre-edit text, as UTF-16 offsets into [[Composition.text]]. The clause the input method is converting
  * right now is `selected`.
  */
final case class CompositionClause(start: Int, end: Int, selected: Boolean)

/** Text an input method has not committed yet (#2008). It is held here and never in the buffer, so the rope, its
  * version, the dirty flag, undo, language-server sync and auto-save see nothing until the input method commits.
  *
  * `anchor` and `bufferVersion` are the primary cursor and the document version when the composition began: a commit
  * that arrives after the cursor has moved goes to `anchor` if the buffer has not changed since, and to the cursor
  * otherwise. `caret` is an offset into `text`.
  */
final case class Composition(
    paneId: PaneId,
    bufferId: BufferId,
    anchor: CursorPosition,
    bufferVersion: Long,
    text: String,
    clauses: List[CompositionClause],
    caret: Option[Int]
)
