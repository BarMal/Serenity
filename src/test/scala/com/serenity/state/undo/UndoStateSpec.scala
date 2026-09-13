package com.serenity.state.undo

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1455 (steady-state follow-up): locks in `boundedPush`'s eviction contract independent of how it's implemented
  * internally. The prior partial fix (see git history on `UndoState.scala`) only avoided the O(maxUndoDepth) copy
  * below the cap -- every push once the stack is AT the cap (the steady state for any session longer than
  * `maxUndoDepth` edits) still rebuilt the whole stack. These specs pin down the same bounded/ordering contract
  * across many pushes at and beyond the cap, so a genuinely O(1)-amortized steady-state implementation can't
  * accidentally change what gets kept or in what order.
  */
class UndoStateSpec extends AnyFlatSpec with Matchers:

  private def entry(marker: Int): HistoryEntry =
    HistoryEntry.PanelChange(
      uiSurfaces = Nil,
      workspaceTree = None,
      maximizedWorkspaceNodeId = None,
      focus = Focus.EditorPane(PaneId(marker))
    )

  private def markerOf(e: HistoryEntry): Int =
    e.asInstanceOf[HistoryEntry.PanelChange].focus.asInstanceOf[Focus.EditorPane].paneId.value

  "pushUndo" should "cons without dropping while under the cap" in {
    val state = (1 to 5).foldLeft(UndoState(maxUndoDepth = 10))((s, i) => s.pushUndo(entry(i)))

    state.undoStack.size shouldBe 5
    state.undoStack.map(markerOf) shouldBe List(5, 4, 3, 2, 1)
  }

  it should "bound the stack to maxUndoDepth once pushes exceed it, keeping the most recent entries" in {
    val state = (1 to 12).foldLeft(UndoState(maxUndoDepth = 10))((s, i) => s.pushUndo(entry(i)))

    state.undoStack.size shouldBe 10
    state.undoStack.map(markerOf) shouldBe (12 to 3 by -1).toList
  }

  it should "keep dropping the single oldest entry on every push once at the cap (steady state)" in {
    val atCap = (1 to 10).foldLeft(UndoState(maxUndoDepth = 10))((s, i) => s.pushUndo(entry(i)))

    // Continue pushing one at a time well past the point the stack first hit the cap, checking after each push --
    // not just once after a single overflow -- that exactly the oldest entry is evicted and every other entry (and
    // its order) is preserved. This is the steady-state behavior the prior partial fix left unfixed.
    val afterManyMore = (11 to 500).foldLeft(atCap) { (s, i) =>
      val next = s.pushUndo(entry(i))
      next.undoStack.size shouldBe 10
      next.undoStack.head shouldBe entry(i)
      next.undoStack.map(markerOf) shouldBe (i to i - 9 by -1).toList
      next
    }

    afterManyMore.undoStack.map(markerOf) shouldBe (500 to 491 by -1).toList
  }

  it should "treat maxUndoDepth = 1 as keeping only the single most recent entry" in {
    val state = (1 to 5).foldLeft(UndoState(maxUndoDepth = 1))((s, i) => s.pushUndo(entry(i)))

    state.undoStack.toList shouldBe List(entry(5))
  }

  "pushRedo" should "bound the redo stack the same way pushUndo bounds the undo stack" in {
    val state = (1 to 15).foldLeft(UndoState(maxUndoDepth = 10))((s, i) => s.pushRedo(entry(i)))

    state.redoStack.size shouldBe 10
    state.redoStack.map(markerOf) shouldBe (15 to 6 by -1).toList
  }

  it should "keep evicting only the oldest redo entry once at the cap across many further pushes" in {
    val atCap = (1 to 10).foldLeft(UndoState(maxUndoDepth = 10))((s, i) => s.pushRedo(entry(i)))

    val afterManyMore = (11 to 300).foldLeft(atCap) { (s, i) =>
      val next = s.pushRedo(entry(i))
      next.redoStack.size shouldBe 10
      next.redoStack.map(markerOf) shouldBe (i to i - 9 by -1).toList
      next
    }

    afterManyMore.redoStack.map(markerOf) shouldBe (300 to 291 by -1).toList
  }

  "pushUndo with clearRedo" should "clear the redo stack by default and preserve it when told not to" in {
    val withRedo = UndoState(maxUndoDepth = 10).pushRedo(entry(1))

    withRedo.pushUndo(entry(2)).redoStack.toList shouldBe Nil
    withRedo.pushUndo(entry(2), clearRedo = false).redoStack.map(markerOf) shouldBe List(1)
  }
