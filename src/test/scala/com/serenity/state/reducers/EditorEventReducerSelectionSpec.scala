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
    Buffer.fromString(bufferId, text).copy(cursors = List(cursor), selection = selection)

  "ExtendSelectionLeft" should "anchor at the cursor and move the focus left" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft)

    extended.cursors shouldBe List(CursorPosition(0, 2))
    extended.selection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 2)))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft, ExtendSelectionLeft)

    extended.selection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 1)))
  }

  "ExtendSelectionDown" should "anchor at the cursor and move the focus onto the next line" in {
    val extended = reduce(bufferOf("abc\ndef", CursorPosition(0, 1)), ExtendSelectionDown)

    extended.cursors shouldBe List(CursorPosition(1, 1))
    extended.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 1)))
  }

  "Extending a selection" should "hold the anchor when the direction reverses" in {
    val extended = reduce(bufferOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft)

    extended.cursors shouldBe List(CursorPosition(0, 2))
    extended.selection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
  }

  it should "drop secondary selections rather than extending each of them" in {
    val multiSelected = bufferOf("abcdef", CursorPosition(0, 1))
      .copy(selections = List(Selection(CursorPosition(0, 3), CursorPosition(0, 5))))

    reduce(multiSelected, ExtendSelectionRight).selections shouldBe Nil
  }

  "Movement with a selection active" should "resume from the selection focus, not the head cursor" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).cursors shouldBe List(CursorPosition(0, 5))
    reduce(selected, MoveLeft).cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "collapse the selection" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).selection shouldBe None
    reduce(selected, MoveWordRight).selection shouldBe None
    reduce(selected, MoveDown).selection shouldBe None
  }

  "Horizontal movement" should "set the preferred column and forget the measured x-offset" in {
    val moved = reduce(bufferOf("abcdef", CursorPosition(0, 1)), MoveRight)

    moved.preferredColumn shouldBe Some(2)
    moved.preferredXPx shouldBe None
  }

  "Vertical movement" should "carry the preferred column across a shorter intervening line" in {
    val roundTrip = reduce(bufferOf("abcdef\nab\nabcdef", CursorPosition(0, 6)), MoveDown, MoveDown)

    roundTrip.cursors shouldBe List(CursorPosition(2, 6))
  }

  "SelectAll" should "select nothing on an empty buffer" in {
    val selected = reduce(bufferOf("", CursorPosition(0, 0)), SelectAll)

    selected.cursors shouldBe List(CursorPosition(0, 0))
    selected.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 0)))
  }

  /** The single-cursor arms read the buffer back out of the state, so anything the dispatcher adjusted on the way in --
    * a collapsed selection set, cleared vertical state -- is lost unless the adjusted buffer is seeded there first.
    */
  "Adjustments made on the way into a single-cursor arm" should "survive the arm reading the buffer back" in {
    val stale = bufferOf("abc", CursorPosition(0, 1))
      .copy(multiCursorVerticalStates = List(VerticalCursorState(CursorPosition(0, 1), 1, 0f)))

    reduce(stale, InsertChar('x')).multiCursorVerticalStates shouldBe Nil
    reduce(stale, DeleteBackward).multiCursorVerticalStates shouldBe Nil
    reduce(stale, MoveRight).multiCursorVerticalStates shouldBe Nil
  }

  it should "collapse secondary selections before select-all rebuilds the selection" in {
    val multiSelected = bufferOf("abc\ndefg", CursorPosition(0, 1))
      .copy(selections = List(Selection(CursorPosition(0, 1), CursorPosition(0, 3))))

    reduce(multiSelected, SelectAll).allSelections shouldBe
      List(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }
