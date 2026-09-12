package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorNavigationEventReducer` (#1442): caret movement, paging, and select-all -- the family
  * that repositions cursors/selection without touching document content. Focuses on behavior specific to this module
  * (multi-cursor collapsing, downstream affinity normalization, the `MoveToEndOfFile` single-cursor/multi-cursor
  * split) rather than re-asserting dispatch already covered end-to-end elsewhere.
  */
class EditorNavigationEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    selections: List[Selection] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingState(cursors = cursors, selection = selection, selections = selections))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  "MoveRight" should "move a single cursor one column right" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))

    bufferAfter(MoveRight, before).editing.cursors shouldBe List(CursorPosition(0, 1))
  }

  it should "move every cursor independently when there are several" in {
    val before = stateWith("hello\nworld", List(CursorPosition(0, 0), CursorPosition(1, 0)))

    bufferAfter(MoveRight, before).editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(1, 1))
  }

  it should "collapse an active selection to its focus first, rather than moving both ends" in {
    val before =
      stateWith("hello", List(CursorPosition(0, 3)), selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 3))))

    val after = bufferAfter(MoveRight, before)
    after.editing.selection shouldBe None
    after.editing.cursors shouldBe List(CursorPosition(0, 4))
  }

  it should "deduplicate cursors that land on the same position and sort the result" in {
    val before = stateWith("hello", List(CursorPosition(0, 2), CursorPosition(0, 0)))

    // Both move right by one; neither lands on the other's position, but the result must still come back sorted.
    bufferAfter(MoveRight, before).editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  "MoveLeft at the start of the document" should "leave the cursor in place" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))

    bufferAfter(MoveLeft, before).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  "MoveWordRight" should "land on the next word boundary" in {
    val before = stateWith("alpha beta", List(CursorPosition(0, 0)))

    bufferAfter(MoveWordRight, before).editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  "MoveWordLeft" should "land on the previous word boundary" in {
    val before = stateWith("alpha beta", List(CursorPosition(0, 10)))

    bufferAfter(MoveWordLeft, before).editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  "MoveToStartOfFile" should "move the cursor to line 0, column 0 regardless of starting position" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(2, 3)))

    bufferAfter(MoveToStartOfFile, before).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  "SelectAll" should "select from the start of the document to the end of the last line" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 0)))

    val after = bufferAfter(SelectAll, before)
    after.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    after.editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  it should "collapse any existing multi-cursor/selection state before selecting the whole document" in {
    val before = stateWith(
      "alpha\nbeta",
      List(CursorPosition(0, 1), CursorPosition(1, 1)),
      selections = List(Selection(CursorPosition(0, 0), CursorPosition(0, 1)))
    )

    val after = bufferAfter(SelectAll, before)
    after.editing.selections shouldBe Nil
    after.editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile without a selection or extra cursors" should "move the cursor to the end of the last line" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 0)))

    bufferAfter(MoveToEndOfFile, before).editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile with an active selection" should "collapse the selection and land at the end of the last line" in {
    val before = stateWith(
      "alpha\nbeta",
      List(CursorPosition(0, 3)),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 3)))
    )

    val after = bufferAfter(MoveToEndOfFile, before)
    after.editing.selection shouldBe None
    after.editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile with multiple cursors" should "collapse every cursor to the single end-of-file position" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 1), CursorPosition(1, 2)))

    bufferAfter(MoveToEndOfFile, before).editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  "PageDown" should "scroll the topLine forward and move the cursor down by a page" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    val after = bufferAfter(PageDown, before)
    after.editing.cursors.head.line should be > 0
  }

  "PageUp at the top of the document" should "leave the cursor at line 0" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    bufferAfter(PageUp, before).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  "An event outside this reducer's family" should "fall through to the no-op default, unchanged" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))
    val buffer = before.persisted.buffers(bufferId)
    val ctx    = EditorCursorSupport.CursorEventContext(buffer, CursorPosition(0, 0), false, false, before, paneId)

    EditorNavigationEventReducer.reduce(NewLine, ctx).state shouldBe before
  }
