package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorTextEditReducer` (#1442), focused on behavior specific to this module rather than
  * re-asserting the multi-cursor/multi-selection dispatch already covered end-to-end by `EditorEventReducerSpec`:
  * `ReverseTabKey`'s per-line unindent rule (`unindentLine`), and the "nothing to do" cases that must leave the buffer
  * and undo stack untouched rather than recording a no-op edit.
  */
class EditorTextEditReducerSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(text: String, cursor: CursorPosition, selection: Option[Selection] = None): AppState =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(cursors = List(cursor), selection = selection))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  private def recordedBoundary(event: TextEntryEvent, state: AppState): Option[UndoEffect.RecordBoundary] =
    EditorEventReducer.reduce(event, paneId, state).effects.collectFirst {
      case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary
    }

  "ReverseTabKey" should "remove a literal leading tab character in preference to spaces" in {
    val before = stateWith("\tabc", CursorPosition(0, 4))

    bufferAfter(ReverseTabKey, before).document.content.collect() shouldBe "abc"
  }

  it should "remove only the spaces actually present when fewer than a full indent level's worth lead the line" in {
    val before = stateWith("  abc", CursorPosition(0, 5)) // two leading spaces, less than TabInsertion's four

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "abc"
    after.editing.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "remove at most one full indent level's worth of leading spaces, not every leading space" in {
    val before = stateWith("        abc", CursorPosition(0, 11)) // eight leading spaces, two indent levels

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "    abc"
    after.editing.cursors shouldBe List(CursorPosition(0, 7))
  }

  it should "leave the line and the cursor untouched when it has no leading whitespace" in {
    val before = stateWith("abc", CursorPosition(0, 2))

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "abc"
    after.editing.cursors shouldBe List(CursorPosition(0, 2))
  }

  it should "record no undo boundary when every targeted line has nothing to unindent" in {
    val before = stateWith("abc", CursorPosition(0, 2))

    recordedBoundary(ReverseTabKey, before) shouldBe None
  }

  it should "record an undo boundary when at least one targeted line is actually unindented" in {
    val before = stateWith("    abc", CursorPosition(0, 6))

    recordedBoundary(ReverseTabKey, before) should not be None
  }

  it should "unindent only the lines a multi-line selection spans, leaving lines outside it alone" in {
    val before = stateWith(
      "    one\n    two\n    three",
      CursorPosition(1, 4),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    )

    bufferAfter(ReverseTabKey, before).document.content.collect() shouldBe "one\ntwo\n    three"
  }

  "TabKey with a multi-line selection" should "indent every selected line by one full indent level" in {
    val before = stateWith(
      "one\ntwo\nthree",
      CursorPosition(1, 3),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(1, 3)))
    )

    bufferAfter(TabKey, before).document.content.collect() shouldBe "    one\n    two\nthree"
  }

  "InsertChar without a selection or extra cursors" should "insert one character at the single cursor" in {
    val before = stateWith("hllo", CursorPosition(0, 1))

    val after = bufferAfter(InsertChar('e'), before)
    after.document.content.collect() shouldBe "hello"
    after.editing.cursors shouldBe List(CursorPosition(0, 2))
  }

  "DeleteWordBackward without a selection" should "clear any stale selection state alongside deleting the word" in {
    val before = stateWith(
      "alpha beta",
      CursorPosition(0, 10),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    )

    val after = bufferAfter(DeleteWordBackward, before)
    after.document.content.collect() shouldBe "alpha "
    after.editing.selection shouldBe None
    after.editing.selections shouldBe Nil
  }

  "An event this reducer does not own" should "leave the buffer untouched" in {
    val before = stateWith("abc", CursorPosition(0, 1))

    EditorTextEditReducer.reduce(MoveRight, EditorCursorSupport.contextFor(paneId, before, before.persisted.buffers(bufferId)).value)
