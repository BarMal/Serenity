package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}

/** State the event pipeline exposes for recording and replaying undo/redo history. */
private[manager] trait UndoRecordingPort:
  def undoRef: Ref[IO, UndoState]
  def updateModelValidated(transition: Model => Option[Model]): IO[Unit]

/** Records undoable changes and replays undo/redo history, independent of event dispatch and focus routing.
  * `recordUndoBoundary` applies the fact a reducer (or other direct call site, e.g. the replace workflow) already
  * declared via `AppEffect.Undo(UndoEffect.RecordBoundary(...))` -- #1016: this class no longer decides which events
  * are undoable or diffs state to infer a change, since the code that made the change is the only code with that
  * knowledge. `applyUndo` and `applyRedo` replay history entries back into state via `HistoryEntry.restore`, agnostic
  * to which kind of entry (buffer edit, pane close, ...) they're replaying, committing the restored state and the new
  * history in one model write.
  */
final private[manager] class UndoRecording(port: UndoRecordingPort):
  import port.*

  def recordUndoBoundary(entry: HistoryEntry, groupable: Boolean): IO[Unit] =
    undoRef.update(UndoRecording.recorded(_, entry, groupable))

  def applyUndo(@annotation.unused prevState: AppState): IO[Unit] =
    updateModelValidated(UndoRecording.undone)

  def applyRedo(@annotation.unused prevState: AppState): IO[Unit] =
    updateModelValidated(UndoRecording.redone)

private[manager] object UndoRecording:

  def recorded(undo: UndoState, entry: HistoryEntry, groupable: Boolean): UndoState =
    (groupable, entry) match
      case (true, bufferEdit: HistoryEntry.BufferEdit) =>
        val sameGroup =
          undo.pendingGroup.exists(g => g.bufferId == bufferEdit.bufferId && g.paneId == bufferEdit.paneId)
        if sameGroup then undo.clearRedo
        else undo.flushPendingGroup.copy(pendingGroup = Some(bufferEdit), redoStack = Vector.empty)
      case _ =>
        undo.flushPendingGroup.pushUndo(entry)

  def undone(model: Model): Option[Model] =
    val flushed = model.undo.flushPendingGroup
    flushed.undoStack.headOption.flatMap { entry =>
      val rest = flushed.undoStack.drop(1)
      restored(model, entry, inverse => flushed.copy(undoStack = rest).pushRedo(inverse))
    }

  def redone(model: Model): Option[Model] =
    model.undo.redoStack.headOption.flatMap { entry =>
      val rest = model.undo.redoStack.drop(1)
      restored(model, entry, inverse => model.undo.copy(redoStack = rest).pushUndo(inverse, clearRedo = false))
    }

  /** Restores `entry` into the model's app state and pairs it with the `UndoState` `nextUndoState` builds from the
    * inverse entry `HistoryEntry.restore` hands back. A missing target (`restore` returning `None`) leaves the whole
    * model untouched, the stack included.
    */
  private def restored(model: Model, entry: HistoryEntry, nextUndoState: HistoryEntry => UndoState): Option[Model] =
    entry
      .restore(model.app)
      .map((restoredState, inverse) => model.copy(app = restoredState, undo = nextUndoState(inverse)))
