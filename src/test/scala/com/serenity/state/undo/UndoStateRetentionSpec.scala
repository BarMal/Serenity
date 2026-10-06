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

  "retainingBuffers" should "drop buffer edits of closed buffers from the undo and redo stacks, keeping the order" in {
    val state = UndoState(
      undoStack = Vector(edit(1), edit(2), edit(1), edit(3)),
      redoStack = Vector(edit(2), edit(3))
    )

    val retained = state.retainingBuffers(liveOnly(1, 3))

    retained.undoStack shouldBe Vector(edit(1), edit(1), edit(3))
    retained.redoStack shouldBe Vector(edit(3))
  }

  it should "drop the pending group of a closed buffer and keep a live one" in {
    val pending = UndoState(pendingGroup = Some(edit(2)))

    pending.retainingBuffers(liveOnly(1)).pendingGroup shouldBe None
    pending.retainingBuffers(liveOnly(2)).pendingGroup shouldBe Some(edit(2))
  }

  it should "keep entries that belong to no buffer" in {
    val state = UndoState(undoStack = Vector(paneClose, edit(2)), redoStack = Vector(paneClose))

    val retained = state.retainingBuffers(liveOnly())

    retained.undoStack shouldBe Vector(paneClose)
    retained.redoStack shouldBe Vector(paneClose)
  }

  it should "return the same instance when every entry is live" in {
    val state =
      UndoState(undoStack = Vector(edit(1), paneClose), redoStack = Vector(edit(1)), pendingGroup = Some(edit(1)))

    state.retainingBuffers(liveOnly(1)) should be theSameInstanceAs state
  }

  it should "keep the depth limit" in {
    UndoState(maxUndoDepth = 7).retainingBuffers(liveOnly()).maxUndoDepth shouldBe 7
  }
