package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A selection whose anchor and focus coincide selects nothing, so it must never steer an edit down the
  * delete-the-selection path: that path has no range to delete and used to swallow Backspace, Delete and typing.
  */
class EditorCollapsedSelectionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateOf(text: String, cursor: CursorPosition, selection: Option[Selection] = None): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    val seeded = buffer.copy(editing = EditingStateFixtures(cursors = List(cursor), selection = selection))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> seeded)))

  private def run(state: AppState, events: EditorEvent*): ReducerResult =
    events.foldLeft(ReducerResult.noEffects(state))((result, event) =>
      val next = com.serenity.VerticalNavSupport.dispatch(event, paneId, result.state)
      next.copy(effects = result.effects ++ next.effects)
    )

  private def bufferOf(result: ReducerResult): Buffer = result.state.persisted.buffers(bufferId)

  private def textOf(result: ReducerResult): String = bufferOf(result).document.content.collect()

  private def recordsUndo(result: ReducerResult): Boolean =
    result.effects.exists {
      case AppEffect.Undo(_: UndoEffect.RecordBoundary) => true
      case _                                            => false
    }

  "Shift+Right at the end of the document" should "leave no selection behind" in {
    val result = run(stateOf("abc", CursorPosition(0, 3)), ExtendSelectionRight)

    bufferOf(result).primarySelection shouldBe None
    bufferOf(result).editing.cursors.head.selectionAnchor shouldBe None
  }

  "Shift+Left at the start of the document" should "leave no selection behind" in {
    val result = run(stateOf("abc", CursorPosition(0, 0)), ExtendSelectionLeft)

    bufferOf(result).editing.cursors.head.selectionAnchor shouldBe None
  }

  "Shift+End at the end of a line" should "leave no selection behind" in {
    val result = run(stateOf("abc\ndef", CursorPosition(0, 3)), ExtendSelectionToLineEnd)

    bufferOf(result).editing.cursors.head.selectionAnchor shouldBe None
  }

  "Shift+Right then Shift+Left" should "leave no selection behind" in {
    val result = run(stateOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft)

    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(0, 2))
    bufferOf(result).editing.cursors.head.selectionAnchor shouldBe None
  }

  it should "anchor a fresh selection at the cursor on the next Shift press" in {
    val result =
      run(stateOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft, ExtendSelectionRight)

    bufferOf(result).primarySelection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 3)))
  }

  "DeleteBackward after Shift+Right at the end of the document" should "delete the character before the cursor" in {
    val result = run(stateOf("abc", CursorPosition(0, 3)), ExtendSelectionRight, DeleteBackward)

    textOf(result) shouldBe "ab"
    bufferOf(result).document.isDirty shouldBe true
    recordsUndo(result) shouldBe true
  }

  "DeleteBackward after Shift+Right then Shift+Left" should "delete the character before the cursor" in {
    val result = run(stateOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft, DeleteBackward)

    textOf(result) shouldBe "acdef"
    bufferOf(result).document.isDirty shouldBe true
    recordsUndo(result) shouldBe true
  }

  "DeleteForward after Shift+Right then Shift+Left" should "delete the character after the cursor" in {
    val result = run(stateOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft, DeleteForward)

    textOf(result) shouldBe "abdef"
    bufferOf(result).document.isDirty shouldBe true
    recordsUndo(result) shouldBe true
  }

  "InsertChar after Shift+Right then Shift+Left" should "insert at the cursor" in {
    val result =
      run(stateOf("abcdef", CursorPosition(0, 2)), ExtendSelectionRight, ExtendSelectionLeft, InsertChar('X'))

    textOf(result) shouldBe "abXcdef"
    bufferOf(result).document.isDirty shouldBe true
    recordsUndo(result) shouldBe true
  }

  "InsertChar after Shift+Right at the end of the document" should "append" in {
    val result = run(stateOf("abc", CursorPosition(0, 3)), ExtendSelectionRight, InsertChar('X'))

    textOf(result) shouldBe "abcX"
  }

  "A zero-width selection that is already in the buffer" should "not swallow DeleteBackward" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
    val result    = run(stateOf("abcdef", CursorPosition(0, 2), collapsed), DeleteBackward)

    textOf(result) shouldBe "acdef"
    bufferOf(result).document.isDirty shouldBe true
    recordsUndo(result) shouldBe true
  }

  it should "not swallow DeleteForward" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
    val result    = run(stateOf("abcdef", CursorPosition(0, 2), collapsed), DeleteForward)

    textOf(result) shouldBe "abdef"
  }

  it should "not swallow InsertChar" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
    val result    = run(stateOf("abcdef", CursorPosition(0, 2), collapsed), InsertChar('X'))

    textOf(result) shouldBe "abXcdef"
  }

  it should "count as no selection when copying, so the whole line is copied" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
    val result    = run(stateOf("abcdef", CursorPosition(0, 2), collapsed), Copy)

    result.state.runtime.clipboard shouldBe Some("abcdef")
  }

  it should "count as no selection when cutting, so the whole line is cut" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))
    val result    = run(stateOf("abcdef\nxyz", CursorPosition(0, 2), collapsed), Cut)

    result.state.runtime.clipboard shouldBe Some("abcdef")
    textOf(result) shouldBe "xyz"
  }

  it should "report no selection to the editing context" in {
    val collapsed = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2)))

    EditingContext.of(stateOf("abcdef", CursorPosition(0, 2), collapsed)).hasSelection shouldBe false
  }
