package com.serenity.state.undo

import com.serenity.rope.Rope
import com.serenity.state.models.{BufferId, CursorPosition, PaneId}

/**
 * A snapshot of a buffer at a point in time, sufficient to restore that state.
 * Used for both undo and redo stacks.
 */
case class HistoryEntry(
    bufferId: BufferId,
    paneId: PaneId,
    content: Rope,
    cursor: CursorPosition
)

/**
 * An open group accumulating consecutive InsertChar events.
 * Holds the before-state (content and cursor prior to the first char in the run).
 * Sealed when a non-InsertChar mutation event arrives, or on undo/redo.
 */
case class PendingGroup(
    bufferId: BufferId,
    paneId: PaneId,
    beforeContent: Rope,
    beforeCursor: CursorPosition
)

/**
 * Full undo/redo state, held separately from AppState in StateManager.
 * Never persisted to disk — always starts fresh.
 */
case class UndoState(
    undoStack: List[HistoryEntry] = Nil,
    redoStack: List[HistoryEntry] = Nil,
    pendingGroup: Option[PendingGroup] = None
):
  def flushPendingGroup(currentContent: Rope, currentCursor: CursorPosition): UndoState =
    pendingGroup match
      case None => this
      case Some(group) =>
        val entry = HistoryEntry(group.bufferId, group.paneId, group.beforeContent, group.beforeCursor)
        copy(undoStack = entry :: undoStack, pendingGroup = None)

  def clearRedo: UndoState = copy(redoStack = Nil)
