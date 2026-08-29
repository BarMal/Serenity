package com.serenity.state.manager

import scala.annotation.unused

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, PendingGroup, UndoState}

/** State the event pipeline exposes for recording and replaying undo/redo history. */
private[manager] trait UndoRecordingPort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]

/** Records undoable content mutations and replays undo/redo history, independent of event dispatch and focus routing.
  * `recordUndoableEdit` observes what a just-dispatched event changed in the focused buffer; `applyUndo` and
  * `applyRedo` replay history entries back into state.
  */
final private[manager] class UndoRecording(port: UndoRecordingPort):
  import port.*

  def recordUndoableEdit(event: Event, prevState: AppState): IO[Unit] =
    focusedBufferAndPane(prevState) match
      case None => IO.unit
      case Some((bufferId, paneId, buffer)) =>
        stateRef.get.flatMap { currentState =>
          currentState.persisted.buffers.get(bufferId) match
            case Some(currentBuffer) if isUndoableContentMutation(event) && bufferChanged(buffer, currentBuffer) =>
              val beforeSnapshot = BufferSnapshot.fromBuffer(buffer)
              event match
                case InsertChar(_) | TabKey =>
                  undoRef.update { undo =>
                    val sameGroup = undo.pendingGroup.exists(g => g.bufferId == bufferId && g.paneId == paneId)
                    if sameGroup then undo.clearRedo
                    else
                      val flushed  = undo.flushPendingGroup
                      val newGroup = PendingGroup(bufferId, paneId, beforeSnapshot)
                      flushed.copy(pendingGroup = Some(newGroup), redoStack = Nil)
                  }
                case _ =>
                  undoRef.update { undo =>
                    val flushed = undo.flushPendingGroup
                    val entry   = HistoryEntry(bufferId, paneId, beforeSnapshot)
                    flushed.pushUndo(entry)
                  }
            case _ => IO.unit
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

  private def focusedBufferAndPane(state: AppState): Option[(BufferId, PaneId, Buffer)] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        state.persisted.layout.editorPanes.get(paneId).flatMap { pane =>
          pane.bufferId.flatMap(state.persisted.buffers.get).map(buf => (buf.id, paneId, buf))
        }
      case _ => None

  private def isUndoableContentMutation(event: Event): Boolean =
    event match
      case InsertChar(_) | TabKey | ReverseTabKey | DeleteBackward | DeleteForward | DeleteWordBackward |
          DeleteWordForward | NewLine | Enter | Paste | Cut =>
        true
      case _ => false

  private def bufferChanged(before: Buffer, after: Buffer): Boolean =
    before.document.content != after.document.content ||
      before.editing.cursors != after.editing.cursors ||
      before.editing.selection != after.editing.selection ||
      before.editing.selections != after.editing.selections

  private def snapFocusToPane(state: AppState, paneId: PaneId): AppState =
    if state.persisted.focus == Focus.EditorPane(paneId) then state
    else
      state.copy(persisted =
        state.persisted.copy(
          focus = Focus.EditorPane(paneId),
          layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
        )
      )
