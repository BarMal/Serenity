package com.serenity.state.reducers

import com.serenity.testkit.EditingStateFixtures

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
    val initial = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    events
      .foldLeft(initial)((state, event) => com.serenity.VerticalNavSupport.dispatch(event, paneId, state).state)
      .persisted
      .buffers(bufferId)

  private def bufferOf(text: String, cursor: CursorPosition, selection: Option[Selection] = None): Buffer =
    val buffer = Buffer.fromString(bufferId, text)
    buffer.copy(editing = EditingStateFixtures(cursors = List(cursor), selection = selection))

  "ExtendSelectionLeft" should "anchor at the cursor and move the focus left" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 2)))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended = reduce(bufferOf("abcd", CursorPosition(0, 3)), ExtendSelectionLeft, ExtendSelectionLeft)

    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 3), CursorPosition(0, 1)))
  }

  "ExtendSelectionToLineStart" should "anchor at the cursor and move the focus to the start of the line" in {
    val extended = reduce(bufferOf("abcdef", CursorPosition(0, 4)), ExtendSelectionToLineStart)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 0))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 4), CursorPosition(0, 0)))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended =
      reduce(bufferOf("abcdef", CursorPosition(0, 4)), ExtendSelectionToLineStart, ExtendSelectionToLineStart)

    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 4), CursorPosition(0, 0)))
  }

  /** The focus lands `upstream`, exactly as End's own does: the column ending a wrapped row is the column starting the
    * next one, and the affinity is what says the selection stops at the end of the row it was extended along rather
    * than at the far left of the row below. Unwrapped, as here, the two rows never meet -- but Shift+End shares End's
    * landing place now (#1292), and that includes how it marks it.
    */
  "ExtendSelectionToLineEnd" should "anchor at the cursor and move the focus to the end of the line" in {
    val extended = reduce(bufferOf("abcdef", CursorPosition(0, 2)), ExtendSelectionToLineEnd)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 6).upstream)
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 6).upstream))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended =
      reduce(bufferOf("abcdef", CursorPosition(0, 2)), ExtendSelectionToLineEnd, ExtendSelectionToLineEnd)

    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 6).upstream))
  }

  "ExtendSelectionWordLeft" should "anchor at the cursor and move the focus to the previous word boundary" in {
    val extended = reduce(bufferOf("foo bar baz", CursorPosition(0, 11)), ExtendSelectionWordLeft)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 8))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 11), CursorPosition(0, 8)))
  }

  it should "keep the original anchor across repeated presses" in {
    val extended =
      reduce(bufferOf("foo bar baz", CursorPosition(0, 11)), ExtendSelectionWordLeft, ExtendSelectionWordLeft)

    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 11), CursorPosition(0, 4)))
  }

  "ExtendSelectionWordRight" should "anchor at the cursor and move the focus to the next word boundary" in {
    val extended = reduce(bufferOf("foo bar baz", CursorPosition(0, 0)), ExtendSelectionWordRight)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 4))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 4)))
  }

  "Extending a selection by word" should "hold the anchor when the direction reverses" in {
    val extended =
      reduce(bufferOf("foo bar baz", CursorPosition(0, 4)), ExtendSelectionWordRight, ExtendSelectionWordLeft)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 4))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 4), CursorPosition(0, 4)))
  }

  it should "drop secondary selections rather than extending each of them" in {
    val base = bufferOf("foo bar baz", CursorPosition(0, 4))
    val multiSelected =
      base.copy(editing = EditingStateFixtures(selections = List(Selection(CursorPosition(0, 8), CursorPosition(0, 11)))))

    val result = reduce(multiSelected, ExtendSelectionWordRight)
    result.allSelections shouldBe result.primarySelection.toList
  }

  "ExtendSelectionDown" should "anchor at the cursor and move the focus onto the next line" in {
    val extended = reduce(bufferOf("abc\ndef", CursorPosition(0, 1)), ExtendSelectionDown)

    extended.editing.cursorPositions shouldBe List(CursorPosition(1, 1))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(1, 1)))
  }

  "Extending a selection" should "hold the anchor when the direction reverses" in {
    val extended = reduce(bufferOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft)

    extended.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
    extended.primarySelection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
  }

  it should "drop secondary selections rather than extending each of them" in {
    val base = bufferOf("abcdef", CursorPosition(0, 1))
    val multiSelected =
      base.copy(editing = EditingStateFixtures(selections = List(Selection(CursorPosition(0, 3), CursorPosition(0, 5)))))

    val result = reduce(multiSelected, ExtendSelectionRight)
    result.allSelections shouldBe result.primarySelection.toList
  }

  "Movement with a selection active" should "resume from the selection focus, not the head cursor" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).editing.cursorPositions shouldBe List(CursorPosition(0, 5))
    reduce(selected, MoveLeft).editing.cursorPositions shouldBe List(CursorPosition(0, 3))
  }

  it should "collapse the selection" in {
    val selected = bufferOf("abcdef", CursorPosition(0, 1), Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4))))

    reduce(selected, MoveRight).primarySelection shouldBe None
    reduce(selected, MoveWordRight).primarySelection shouldBe None
    reduce(selected, MoveDown).primarySelection shouldBe None
  }

  "Horizontal movement" should "set the preferred column and forget the measured x-offset" in {
    val moved = reduce(bufferOf("abcdef", CursorPosition(0, 1)), MoveRight)

    moved.editing.cursors.head.preferredColumn shouldBe Some(2)
    moved.editing.cursors.head.preferredXPx shouldBe None
  }

  "Vertical movement" should "carry the preferred column across a shorter intervening line" in {
    val roundTrip = reduce(bufferOf("abcdef\nab\nabcdef", CursorPosition(0, 6)), MoveDown, MoveDown)

    roundTrip.editing.cursorPositions shouldBe List(CursorPosition(2, 6))
  }

  "SelectAll" should "select nothing on an empty buffer" in {
    val selected = reduce(bufferOf("", CursorPosition(0, 0)), SelectAll)

    selected.editing.cursorPositions shouldBe List(CursorPosition(0, 0))
    selected.primarySelection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 0)))
  }

  /** The single-cursor arms read the buffer back out of the state, so anything the dispatcher adjusted on the way in --
    * a collapsed selection set, cleared vertical state -- is lost unless the adjusted buffer is seeded there first.
    * Before `#1577` this leftover vertical state lived in a separate `multiCursorVerticalStates` collection that could
    * go stale independently of the cursor it was for; now that a cursor's own preferred column/x travels with it
    * directly, staleness of that kind is impossible by construction -- these pin that every single-cursor op still
    * replaces the primary cursor's preferred state outright, a sentinel value included, rather than somehow retaining it.
    */
  "Adjustments made on the way into a single-cursor arm" should "survive the arm reading the buffer back" in {
    val staleBase = bufferOf("abc", CursorPosition(0, 1))
    val stale = staleBase.copy(editing =
      EditingState.fromCursors(List(Cursor(CursorPosition(0, 1), None, Some(999), Some(999f))))
    )

    reduce(stale, InsertChar('x')).editing.cursors.head.preferredColumn should not be Some(999)
    reduce(stale, DeleteBackward).editing.cursors.head.preferredColumn should not be Some(999)
    reduce(stale, MoveRight).editing.cursors.head.preferredColumn should not be Some(999)
  }

  it should "collapse secondary selections before select-all rebuilds the selection" in {
    val multiSelectedBase = bufferOf("abc\ndefg", CursorPosition(0, 1))
    val multiSelected = multiSelectedBase.copy(editing =
      EditingStateFixtures(selections = List(Selection(CursorPosition(0, 1), CursorPosition(0, 3))))
    )

    reduce(multiSelected, SelectAll).allSelections shouldBe
      List(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }
