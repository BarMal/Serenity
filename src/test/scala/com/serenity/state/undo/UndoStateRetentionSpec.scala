package com.serenity.state.undo

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UndoStateRetentionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val snapshot = BufferSnapshot.fromBuffer(AppState.initial.persisted.buffers.values.head)

  private def edit(bufferId: Int): HistoryEntry.BufferEdit =
    HistoryEntry.BufferEdit(BufferId(bufferId), PaneId(0), snapshot)

  private val paneClose: HistoryEntry =
    HistoryEntry.PaneClose(AppState.initial.persisted.layout, AppState.initial.persisted.focus)

  private def liveOnly(ids: Int*): BufferId => Boolean = id => ids.contains(id.value)

  private def bufferIds(entries: Vector[HistoryEntry]): Vector[BufferId] =
    entries.collect { case edit: HistoryEntry.BufferEdit => edit.bufferId }

  private def undoing(entries: HistoryEntry*): UndoState =
    entries.reverse.foldLeft(UndoState())(_.pushUndo(_))

  "retainingBuffers" should "drop the steps of closed buffers from the undo and redo stacks, keeping the order" in {
    val state = undoing(edit(1), edit(2), edit(1), edit(3)).pushRedo(edit(3)).pushRedo(edit(2))

    val retained = state.retainingBuffers(liveOnly(1, 3))

    bufferIds(retained.undoStack) shouldBe Vector(BufferId(1), BufferId(1), BufferId(3))
    bufferIds(retained.redoStack) shouldBe Vector(BufferId(3))
  }

  it should "forget the history of a closed buffer and keep a live one" in {
    val state = UndoState().pushUndo(edit(2))

    state.retainingBuffers(liveOnly(1)).buffers.keySet shouldBe Set.empty
    state.retainingBuffers(liveOnly(2)).buffers.keySet shouldBe Set(BufferId(2))
  }

  it should "keep steps that belong to no buffer" in {
    val state = UndoState().pushUndo(edit(2)).pushUndo(paneClose).pushRedo(paneClose)

    val retained = state.retainingBuffers(liveOnly())

    retained.undoStack shouldBe Vector(paneClose)
    retained.redoStack shouldBe Vector(paneClose)
  }

  it should "return the same instance when every history is live" in {
    val state = UndoState().pushUndo(edit(1)).pushUndo(paneClose).pushRedo(edit(1))

    state.retainingBuffers(liveOnly(1)) should be theSameInstanceAs state
  }

  it should "keep the depth limit" in {
    UndoState(maxUndoDepth = 7).pushUndo(edit(1)).retainingBuffers(liveOnly()).maxUndoDepth shouldBe 7
  }
