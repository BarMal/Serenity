package com.serenity.state.manager

import scala.annotation.unused

import cats.effect.{IO, Ref}
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, PendingGroup, UndoState}

/** State the event pipeline exposes for recording and replaying undo/redo history. */
private[manager] trait UndoRecordingPort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]

/** Records undoable content mutations and replays undo/redo history, independent of event dispatch and focus routing.
  * `recordUndoBoundary` applies the fact a reducer already declared via
  * `AppEffect.Undo(UndoEffect.RecordBoundary(...))` -- #1016: this class no longer decides which events are undoable or
  * diffs buffer state to infer a change, since the reducer that made the edit is the only code with that knowledge.
  * `applyUndo` and `applyRedo` replay history entries back into state.
  */
final private[manager] class UndoRecording(port: UndoRecordingPort):
  import port.*

  def recordUndoBoundary(bufferId: BufferId, paneId: PaneId, before: BufferSnapshot, groupable: Boolean): IO[Unit] =
    undoRef.update { undo =>
      if groupable then
        val sameGroup = undo.pendingGroup.exists(g => g.bufferId == bufferId && g.paneId == paneId)
        if sameGroup then undo.clearRedo
        else
          val flushed  = undo.flushPendingGroup
          val newGroup = PendingGroup(bufferId, paneId, before)
          flushed.copy(pendingGroup = Some(newGroup), redoStack = Nil)
      else
        val flushed = undo.flushPendingGroup
        val entry   = HistoryEntry(bufferId, paneId, before)
        flushed.pushUndo(entry)
    }

  def applyUndo(@unused prevState: AppState): IO[Unit] =
    undoRef.get.flatMap { undo =>
      val flushed = undo.flushPendingGroup
      flushed.undoStack match
        case Nil => IO.unit
        case entry :: rest =>
          stateRef.get.flatMap { state =>
            state.persisted.buffers.get(entry.bufferId) match
              case None => IO.unit
              case Some(current) =>
                val redoEntry      = HistoryEntry(entry.bufferId, entry.paneId, BufferSnapshot.fromBuffer(current))
                val restoredBuffer = entry.snapshot.restoreInto(current)
                val snappedState   = snapFocusToPane(state, entry.paneId)
                undoRef.set(flushed.copy(undoStack = rest).pushRedo(redoEntry)) >>
                  validateAndUpdateState(
                    snappedState.copy(persisted =
                      snappedState.persisted
                        .copy(buffers = snappedState.persisted.buffers + (entry.bufferId -> restoredBuffer))
                    ),
                    state
                  )
          }
    }

  def applyRedo(@unused prevState: AppState): IO[Unit] =
    undoRef.get.flatMap { undo =>
      undo.redoStack match
        case Nil => IO.unit
        case entry :: rest =>
          stateRef.get.flatMap { state =>
            state.persisted.buffers.get(entry.bufferId) match
              case None => IO.unit
              case Some(current) =>
                val undoEntry      = HistoryEntry(entry.bufferId, entry.paneId, BufferSnapshot.fromBuffer(current))
                val restoredBuffer = entry.snapshot.restoreInto(current)
                val snappedState   = snapFocusToPane(state, entry.paneId)
                undoRef.set(undo.copy(redoStack = rest).pushUndo(undoEntry, clearRedo = false)) >>
                  validateAndUpdateState(
                    snappedState.copy(persisted =
                      snappedState.persisted
                        .copy(buffers = snappedState.persisted.buffers + (entry.bufferId -> restoredBuffer))
                    ),
                    state
                  )
          }
    }

  private def snapFocusToPane(state: AppState, paneId: PaneId): AppState =
    if state.persisted.focus == Focus.EditorPane(paneId) then state
    else
      state.copy(persisted =
        state.persisted.copy(
          focus = Focus.EditorPane(paneId),
          layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
        )
      )
