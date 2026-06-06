package com.serenity.state.undo

import com.serenity.rope.Rope
import com.serenity.state.models.*

case class BufferSnapshot(
    content: Rope,
    cursors: List[CursorPosition],
    selection: Option[Selection],
    selections: List[Selection],
    preferredColumn: Option[Int],
    preferredXPx: Option[Float],
    viewport: Viewport,
    findState: Option[FindState],
    isNewEmpty: Boolean
):

  def restoreInto(buffer: Buffer): Buffer =
    buffer.copy(
      content = content,
      cursors = cursors,
      selection = selection,
      selections = selections,
      preferredColumn = preferredColumn,
      preferredXPx = preferredXPx,
      viewport = viewport,
      findState = findState,
      isDirty = true,
      isNewEmpty = isNewEmpty,
      multiCursorVerticalStates = Nil
    )

object BufferSnapshot:

  def fromBuffer(buffer: Buffer): BufferSnapshot =
    BufferSnapshot(
      content = buffer.content,
      cursors = buffer.cursors,
      selection = buffer.selection,
      selections = buffer.selections,
      preferredColumn = buffer.preferredColumn,
      preferredXPx = buffer.preferredXPx,
      viewport = buffer.viewport,
      findState = buffer.findState,
      isNewEmpty = buffer.isNewEmpty
    )

/** A snapshot of a buffer at a point in time, sufficient to restore that state. Used for both undo and redo stacks.
  */
case class HistoryEntry(
    bufferId: BufferId,
    paneId: PaneId,
    snapshot: BufferSnapshot
)

/** An open group accumulating consecutive InsertChar events. Holds the before-state (content and cursor prior to the
  * first char in the run). Sealed when a non-InsertChar mutation event arrives, or on undo/redo.
  */
case class PendingGroup(
    bufferId: BufferId,
    paneId: PaneId,
    beforeSnapshot: BufferSnapshot
)

/** Full undo/redo state, held separately from AppState in StateManager. Never persisted to disk — always starts fresh.
  */
case class UndoState(
    undoStack: List[HistoryEntry] = Nil,
    redoStack: List[HistoryEntry] = Nil,
    pendingGroup: Option[PendingGroup] = None
):

  def flushPendingGroup: UndoState =
    pendingGroup match
      case None => this
      case Some(group) =>
        val entry = HistoryEntry(group.bufferId, group.paneId, group.beforeSnapshot)
        copy(undoStack = entry :: undoStack, pendingGroup = None)

  def clearRedo: UndoState = copy(redoStack = Nil)
