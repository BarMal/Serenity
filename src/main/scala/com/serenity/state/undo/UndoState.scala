package com.serenity.state.undo

import com.serenity.rope.Rope
import com.serenity.state.models.*

final case class BufferSnapshot(
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
final case class HistoryEntry(
    bufferId: BufferId,
    paneId: PaneId,
    snapshot: BufferSnapshot
)

/** An open group accumulating consecutive InsertChar events. Holds the before-state (content and cursor prior to the
  * first char in the run). Sealed when a non-InsertChar mutation event arrives, or on undo/redo.
  */
final case class PendingGroup(
    bufferId: BufferId,
    paneId: PaneId,
    beforeSnapshot: BufferSnapshot
)

/** Full undo/redo state, held separately from AppState in StateManager. Never persisted to disk — always starts fresh.
  */
final case class UndoState(
    undoStack: List[HistoryEntry] = Nil,
    redoStack: List[HistoryEntry] = Nil,
    pendingGroup: Option[PendingGroup] = None,
    maxUndoDepth: Int = UndoState.DefaultMaxUndoDepth
):

  def flushPendingGroup: UndoState =
    pendingGroup match
      case None => this
      case Some(group) =>
        val entry = HistoryEntry(group.bufferId, group.paneId, group.beforeSnapshot)
        pushUndo(entry, clearRedo = false).copy(pendingGroup = None)

  def clearRedo: UndoState = copy(redoStack = Nil)

  def pushUndo(entry: HistoryEntry, clearRedo: Boolean = true): UndoState =
    copy(
      undoStack = boundedPush(entry, undoStack),
      redoStack = if clearRedo then Nil else redoStack
    )

  def pushRedo(entry: HistoryEntry): UndoState =
    copy(redoStack = boundedPush(entry, redoStack))

  private def boundedPush(entry: HistoryEntry, stack: List[HistoryEntry]): List[HistoryEntry] =
    entry :: stack.take(effectiveMaxUndoDepth - 1)

  private def effectiveMaxUndoDepth: Int =
    math.max(1, maxUndoDepth)

object UndoState:
  val DefaultMaxUndoDepth: Int = 1000
