package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, HistoryOwner, UndoState}

/** State the event pipeline exposes for recording and replaying undo/redo history. */
private[manager] trait UndoRecordingPort:
  def updateUndo(update: UndoState => UndoState): IO[Unit]
  def updateModelValidated(transition: Model => Option[Model]): IO[Unit]

/** Records undoable changes and replays undo/redo history, independent of event dispatch and focus routing.
  * `recordUndoBoundary` applies the fact a reducer (or other direct call site, e.g. the replace workflow) already
  * declared via `AppEffect.Undo(UndoEffect.RecordBoundary(...))` -- #1016: this class no longer decides which events
  * are undoable or diffs state to infer a change, since the code that made the change is the only code with that
  * knowledge. `applyUndo` and `applyRedo` replay history entries back into state via `HistoryEntry.restore`, agnostic
  * to which kind of entry (buffer edit, pane close, ...) they're replaying, committing the restored state and the new
  * history in one model write. They act on the working buffer's history or the layout history, never another
  * buffer's (#1930).
  */
final private[manager] class UndoRecording(port: UndoRecordingPort):
  import port.*

  def recordUndoBoundary(entry: HistoryEntry, groupable: Boolean): IO[Unit] =
    updateUndo(UndoRecording.recorded(_, entry, groupable))

  def applyUndo(@annotation.unused prevState: AppState): IO[Unit] =
    updateModelValidated(UndoRecording.undone)

  def applyRedo(@annotation.unused prevState: AppState): IO[Unit] =
    updateModelValidated(UndoRecording.redone)

private[manager] object UndoRecording:

  def recorded(undo: UndoState, entry: HistoryEntry, groupable: Boolean): UndoState =
    undo.recorded(entry, groupable)

  def undone(model: Model): Option[Model] =
    model.undo.nextUndo(model.app).flatMap((owner, entry) => restored(model, owner, entry, model.undo.undone(owner, _)))

  def redone(model: Model): Option[Model] =
    model.undo.nextRedo(model.app).flatMap((owner, entry) => restored(model, owner, entry, model.undo.redone(owner, _)))

  /** Restores `entry` into the model's app state and pairs it with the `UndoState` `nextUndoState` builds from the
    * inverse entry `HistoryEntry.restore` hands back. A buffer edit is restored into the pane undo was invoked in,
    * which may not be the pane it was made in. A missing target (`restore` returning `None`) leaves the whole model
    * untouched, the stack included.
    */
  private def restored(
    model: Model,
    owner: HistoryOwner,
    entry: HistoryEntry,
    nextUndoState: HistoryEntry => UndoState
  ): Option[Model] =
    val target = (owner, entry) match
      case (HistoryOwner.OfBuffer(_, paneId), edit: HistoryEntry.BufferEdit) => edit.copy(paneId = paneId)
      case _                                                                 => entry
    target
      .restore(model.app)
      .map((restoredState, inverse) => model.copy(app = restoredState, undo = nextUndoState(inverse)))
