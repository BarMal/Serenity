package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}

/** State the event pipeline exposes for recording and replaying undo/redo history. */
private[manager] trait UndoRecordingPort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]

/** Records undoable changes and replays undo/redo history, independent of event dispatch and focus routing.
  * `recordUndoBoundary` applies the fact a reducer (or other direct call site, e.g. the replace workflow) already
  * declared via `AppEffect.Undo(UndoEffect.RecordBoundary(...))` -- #1016: this class no longer decides which events
  * are undoable or diffs state to infer a change, since the code that made the change is the only code with that
  * knowledge. `applyUndo` and `applyRedo` replay history entries back into state via `HistoryEntry.restore`, agnostic
  * to which kind of entry (buffer edit, pane close, ...) they're replaying.
  */
final private[manager] class UndoRecording(port: UndoRecordingPort):
  import port.*

  def recordUndoBoundary(entry: HistoryEntry, groupable: Boolean): IO[Unit] =
    undoRef.update { undo =>
      (groupable, entry) match
        case (true, bufferEdit: HistoryEntry.BufferEdit) =>
          val sameGroup = undo.pendingGroup.exists(g => g.bufferId == bufferEdit.bufferId && g.paneId == bufferEdit.paneId)
          if sameGroup then undo.clearRedo
          else undo.flushPendingGroup.copy(pendingGroup = Some(bufferEdit), redoStack = Nil)
        case _ =>
          undo.flushPendingGroup.pushUndo(entry)
    }

  def applyUndo(@annotation.unused prevState: AppState): IO[Unit] =
    undoRef.get.flatMap { undo =>
      val flushed = undo.flushPendingGroup
      flushed.undoStack match
        case Nil => IO.unit
        case entry :: rest =>
          restoreAndPush(entry, state => flushed.copy(undoStack = rest).pushRedo(state))
    }

  def applyRedo(@annotation.unused prevState: AppState): IO[Unit] =
    undoRef.get.flatMap { undo =>
      undo.redoStack match
        case Nil => IO.unit
        case entry :: rest =>
          restoreAndPush(entry, inverse => undo.copy(redoStack = rest).pushUndo(inverse, clearRedo = false))
    }

  /** Restores `entry` into the current state and commits both the new `UndoState` (via `nextUndoState`, applied to the
    * inverse entry `HistoryEntry.restore` hands back) and the new `AppState`. A missing target (`restore` returning
    * `None`) is a no-op that leaves the stack untouched, same as before this type existed.
    */
  private def restoreAndPush(entry: HistoryEntry, nextUndoState: HistoryEntry => UndoState): IO[Unit] =
    stateRef.get.flatMap { state =>
      entry.restore(state) match
        case None => IO.unit
        case Some((restoredState, inverse)) =>
          undoRef.set(nextUndoState(inverse)) >> validateAndUpdateState(restoredState, state)
    }
