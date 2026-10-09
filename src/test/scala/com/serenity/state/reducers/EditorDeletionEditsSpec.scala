package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What every deletion event must leave behind when it removes text: the new content, a dirty document and an undo
  * entry. A deletion that changed nothing must leave all three alone.
  */
class EditorDeletionEditsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateOf(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None
  ): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    val seeded = buffer.copy(editing = EditingStateFixtures(cursors = cursors, selection = selection))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> seeded)))

  private def stateOf(text: String, cursor: CursorPosition): AppState = stateOf(text, List(cursor))

  private def reduce(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def bufferOf(result: ReducerResult): Buffer = result.state.persisted.buffers(bufferId)

  private def textOf(result: ReducerResult): String = bufferOf(result).document.content.collect()

  private def undoEntries(result: ReducerResult): Int =
    result.effects.count {
      case AppEffect.Undo(_: UndoEffect.RecordBoundary) => true
      case _                                            => false
    }

  private def shouldEdit(result: ReducerResult, expectedText: String, expectedCursors: List[CursorPosition]): Unit =
    textOf(result) shouldBe expectedText
    bufferOf(result).editing.cursorPositions shouldBe expectedCursors
    bufferOf(result).document.isDirty shouldBe true
    undoEntries(result) shouldBe 1

  private def shouldChangeNothing(result: ReducerResult, text: String): Unit =
    textOf(result) shouldBe text
    bufferOf(result).document.isDirty shouldBe false
    undoEntries(result) shouldBe 0

  "DeleteBackward" should "change the content, dirty the document and record an undo entry" in
    shouldEdit(reduce(DeleteBackward, stateOf("abc", CursorPosition(0, 2))), "ac", List(CursorPosition(0, 1)))

  "DeleteForward" should "change the content, dirty the document and record an undo entry" in
    shouldEdit(reduce(DeleteForward, stateOf("abc", CursorPosition(0, 1))), "ac", List(CursorPosition(0, 1)))

  "DeleteWordBackward" should "change the content, dirty the document and record an undo entry" in
    shouldEdit(
      reduce(DeleteWordBackward, stateOf("alpha beta", CursorPosition(0, 10))),
      "alpha ",
      List(CursorPosition(0, 6))
    )

  "DeleteWordForward" should "change the content, dirty the document and record an undo entry" in
    shouldEdit(
      reduce(DeleteWordForward, stateOf("alpha beta", CursorPosition(0, 0))),
      "beta",
      List(CursorPosition(0, 0))
    )

  "DeleteToLineStart" should "delete from the cursor back to the start of the line" in
    shouldEdit(
      reduce(DeleteToLineStart, stateOf("one\ntwo three\nfour", CursorPosition(1, 3))),
      "one\n three\nfour",
      List(CursorPosition(1, 0))
    )

  it should "join the line to the previous one when the cursor is already at the line start" in
    shouldEdit(
      reduce(DeleteToLineStart, stateOf("one\ntwo", CursorPosition(1, 0))),
      "onetwo",
      List(CursorPosition(0, 3))
    )

  it should "change nothing at the start of the document" in
    shouldChangeNothing(reduce(DeleteToLineStart, stateOf("one", CursorPosition(0, 0))), "one")

  it should "delete the selection and nothing else when there is one" in {
    val state = stateOf(
      "one two three",
      List(CursorPosition(0, 7)),
      selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
    )

    shouldEdit(reduce(DeleteToLineStart, state), "one  three", List(CursorPosition(0, 4)))
  }

  it should "delete to the start of each cursor's own line with several cursors" in {
    val state = stateOf("one two\nthree four", List(CursorPosition(0, 3), CursorPosition(1, 5)))

    val result = reduce(DeleteToLineStart, state)
    textOf(result) shouldBe " two\n four"
    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(0, 0), CursorPosition(1, 0))
    bufferOf(result).document.isDirty shouldBe true
    undoEntries(result) shouldBe 1
  }

  "DeleteToLineEnd" should "delete from the cursor to the end of the line" in
    shouldEdit(
      reduce(DeleteToLineEnd, stateOf("one\ntwo three\nfour", CursorPosition(1, 3))),
      "one\ntwo\nfour",
      List(CursorPosition(1, 3))
    )

  it should "join the next line when the cursor is already at the line end" in
    shouldEdit(
      reduce(DeleteToLineEnd, stateOf("one\ntwo", CursorPosition(0, 3))),
      "onetwo",
      List(CursorPosition(0, 3))
    )

  it should "change nothing at the end of the document" in
    shouldChangeNothing(reduce(DeleteToLineEnd, stateOf("one", CursorPosition(0, 3))), "one")

  it should "delete the selection and nothing else when there is one" in {
    val state = stateOf(
      "one two three",
      List(CursorPosition(0, 4)),
      selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
    )

    shouldEdit(reduce(DeleteToLineEnd, state), "one  three", List(CursorPosition(0, 4)))
  }

  it should "delete to the end of each cursor's own line with several cursors" in {
    val state = stateOf("one two\nthree four", List(CursorPosition(0, 3), CursorPosition(1, 5)))

    val result = reduce(DeleteToLineEnd, state)
    textOf(result) shouldBe "one\nthree"
    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(0, 3), CursorPosition(1, 5))
    bufferOf(result).document.isDirty shouldBe true
    undoEntries(result) shouldBe 1
  }

  it should "merge the ranges of cursors that share a line" in {
    val state = stateOf("one two three", List(CursorPosition(0, 3), CursorPosition(0, 7)))

    textOf(reduce(DeleteToLineEnd, state)) shouldBe "one"
  }
