package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Locks in #1016's acceptance criterion for buffer-content edits: the reducer that performs an edit is the only code
  * that decides whether it was undoable and whether it groups with an adjacent edit, by emitting
  * `AppEffect.Undo(UndoEffect.RecordBoundary(...))` itself. `UndoRecording` no longer classifies by event type or diffs
  * buffer state to infer this.
  */
class EditorUndoEffectSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(text: String, cursor: CursorPosition): AppState =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(cursors = List(cursor)))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def recordedBoundary(effects: List[AppEffect]): Option[UndoEffect.RecordBoundary] =
    effects.collectFirst { case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary }

  private def bufferEditEntry(boundary: UndoEffect.RecordBoundary): HistoryEntry.BufferEdit =
    boundary.entry match
      case bufferEdit: HistoryEntry.BufferEdit => bufferEdit
      case other                               => fail(s"expected a HistoryEntry.BufferEdit, got $other")

  "InsertChar" should "emit a groupable undo boundary snapshotting the pre-edit buffer" in {
    val before = stateWith("hi", CursorPosition(0, 2))
    val result = EditorEventReducer.reduce(InsertChar('!'), paneId, before)

    val boundary = recordedBoundary(result.effects).value
    val entry    = bufferEditEntry(boundary)
    entry.bufferId shouldBe bufferId
    entry.paneId shouldBe paneId
    boundary.groupable shouldBe true
    entry.snapshot shouldBe BufferSnapshot.fromBuffer(before.persisted.buffers(bufferId))
  }

  "DeleteBackward" should "emit a non-groupable undo boundary" in {
    val before   = stateWith("hi", CursorPosition(0, 2))
    val result   = EditorEventReducer.reduce(DeleteBackward, paneId, before)
    val boundary = recordedBoundary(result.effects).value

    boundary.groupable shouldBe false
    bufferEditEntry(boundary).snapshot shouldBe BufferSnapshot.fromBuffer(before.persisted.buffers(bufferId))
  }

  "NewLine" should "emit a non-groupable undo boundary" in {
    val before   = stateWith("line", CursorPosition(0, 4))
    val result   = EditorEventReducer.reduce(NewLine, paneId, before)
    val boundary = recordedBoundary(result.effects).value

    boundary.groupable shouldBe false
  }

  "TabKey with an active selection" should "still emit a groupable boundary, matching plain Tab insertion" in {
    val buffer = Buffer
      .fromString(bufferId, "line one\nline two")
      .copy(editing =
        EditingState(
          cursors = List(CursorPosition(1, 0)),
          selection = Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
        )
      )
    val before = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val result = EditorEventReducer.reduce(TabKey, paneId, before)

    recordedBoundary(result.effects).value.groupable shouldBe true
  }

  "Paste" should "emit a non-groupable undo boundary" in {
    val before =
      stateWith("ab", CursorPosition(0, 1)).copy(runtime = AppState.initial.runtime.copy(clipboard = Some("Z")))
    val result   = EditorEventReducer.reduce(Paste, paneId, before)
    val boundary = recordedBoundary(result.effects).value

    boundary.groupable shouldBe false
  }

  "Cut" should "emit a non-groupable undo boundary" in {
    val buffer = Buffer
      .fromString(bufferId, "alpha beta")
      .copy(editing =
        EditingState(
          cursors = List(CursorPosition(0, 5)),
          selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
        )
      )
    val before = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val result = EditorEventReducer.reduce(Cut, paneId, before)

    recordedBoundary(result.effects).value.groupable shouldBe false
  }

  "Copy" should "emit no undo boundary, since it never mutates the buffer" in {
    val before = stateWith("alpha", CursorPosition(0, 5))
    val result = EditorEventReducer.reduce(Copy, paneId, before)

    recordedBoundary(result.effects) shouldBe None
  }

  "MoveRight" should "emit no undo boundary, since it never mutates the buffer" in {
    val before = stateWith("alpha", CursorPosition(0, 0))
    val result = EditorEventReducer.reduce(MoveRight, paneId, before)

    recordedBoundary(result.effects) shouldBe None
  }
