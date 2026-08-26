package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Movement and shift-movement compute the same landing position and differ only in what they do with it: one collapses
  * the selection, the other extends it from the existing anchor. These pin that split.
  */
class EditorEventReducerSelectionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def reduce(buffer: Buffer, events: EditorEvent*): Buffer =
    val initial = AppState.initial.copy(buffers = Map(bufferId -> buffer))
    events
      .foldLeft(initial)((state, event) => com.serenity.VerticalNavSupport.dispatch(event, paneId, state).state)
      .buffers(bufferId)

  private def bufferOf(text: String, cursor: CursorPosition, selection: Option[Selection] = None): Buffer =
    val buffer = Buffer.fromString(bufferId, text)
    buffer.copy(editing = buffer.editing.copy(cursors = List(cursor), selection = selection))

  "ExtendSelectionLeft" should "anchor at the cursor and move the focus left" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft)

    extended.editing.cursors shouldBe List(CursorPosition(0, 2))
    extended.editing.selection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 2)))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft, ExtendSelectionLeft)

    extended.editing.selection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 1)))
  }

  "ExtendSelectionDown" should "anchor at the cursor and move the focus onto the next line" in {
    val extended = reduce(bufferOf("abc\ndef", CursorPosition(0, 1)), ExtendSelectionDown)

    extended.editing.cursors shouldBe List(CursorPosition(1, 1))
    extended.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 1)))
  }

  "Extending a selection" should "hold the anchor when the direction reverses" in {
    val extended = reduce(bufferOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft)

    extended.editing.cursors shouldBe List(CursorPosition(0, 2))
    extended.editing.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
  }

  it should "drop secondary selections rather than extending each of them" in {
    val base = bufferOf("abcdef", CursorPosition(0, 1))
    val multiSelected =
      base.copy(editing = base.editing.copy(selections = List(Selection(CursorPosition(0, 3), CursorPosition(0, 5)))))

    reduce(multiSelected, ExtendSelectionRight).editing.selections shouldBe Nil
  }

  "Movement with a selection active" should "resume from the selection focus, not the head cursor" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).editing.cursors shouldBe List(CursorPosition(0, 5))
    reduce(selected, MoveLeft).editing.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "collapse the selection" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).editing.selection shouldBe None
    reduce(selected, MoveWordRight).editing.selection shouldBe None
    reduce(selected, MoveDown).editing.selection shouldBe None
  }

  "Horizontal movement" should "set the preferred column and forget the measured x-offset" in {
    val moved = reduce(bufferOf("abcdef", CursorPosition(0, 1)), MoveRight)

    moved.editing.preferredColumn shouldBe Some(2)
    moved.editing.preferredXPx shouldBe None
  }

  "Vertical movement" should "carry the preferred column across a shorter intervening line" in {
    val roundTrip = reduce(bufferOf("abcdef\nab\nabcdef", CursorPosition(0, 6)), MoveDown, MoveDown)

    roundTrip.editing.cursors shouldBe List(CursorPosition(2, 6))
  }

  "SelectAll" should "select nothing on an empty buffer" in {
    val selected = reduce(bufferOf("", CursorPosition(0, 0)), SelectAll)

    selected.editing.cursors shouldBe List(CursorPosition(0, 0))
    selected.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 0)))
  }

  /** The single-cursor arms read the buffer back out of the state, so anything the dispatcher adjusted on the way in --
    * a collapsed selection set, cleared vertical state -- is lost unless the adjusted buffer is seeded there first.
    */
  "Adjustments made on the way into a single-cursor arm" should "survive the arm reading the buffer back" in {
    val staleBase = bufferOf("abc", CursorPosition(0, 1))
    val stale = staleBase.copy(editing =
      staleBase.editing.copy(multiCursorVerticalStates = List(VerticalCursorState(CursorPosition(0, 1), 1, 0f)))
    )

    reduce(stale, InsertChar('x')).editing.multiCursorVerticalStates shouldBe Nil
    reduce(stale, DeleteBackward).editing.multiCursorVerticalStates shouldBe Nil
    reduce(stale, MoveRight).editing.multiCursorVerticalStates shouldBe Nil
  }

  it should "collapse secondary selections before select-all rebuilds the selection" in {
    val multiSelectedBase = bufferOf("abc\ndefg", CursorPosition(0, 1))
    val multiSelected = multiSelectedBase.copy(editing =
      multiSelectedBase.editing.copy(selections = List(Selection(CursorPosition(0, 1), CursorPosition(0, 3))))
    )

    reduce(multiSelected, SelectAll).allSelections shouldBe
      List(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }
