package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.EditorPaneComponent
import com.serenity.state.models.*
import com.serenity.state.undo.{EditGrouping, EditKind}
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `InsertText` (#2008): a committed string -- an input method's commit, or a terminal's code point outside the BMP --
  * goes in as one edit at every cursor, groups for undo like typed text, and leaves a lone character to behave exactly
  * as typing it would.
  */
class InsertTextReducerSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val Astral = "𠮷"

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    smartPunctuation: Boolean = false
  ): AppState =
    val buffer =
      Buffer.fromString(bufferId, text).copy(editing = EditingStateFixtures(cursors = cursors, selection = selection))
    val state = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    if smartPunctuation then
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withSmartPunctuation(true)))
    else state

  private def single(text: String, column: Int, smartPunctuation: Boolean = false): AppState =
    stateWith(text, List(CursorPosition(0, column)), smartPunctuation = smartPunctuation)

  private def reduced(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    reduced(event, state).state.persisted.buffers(bufferId)

  private def boundaries(event: TextEntryEvent, state: AppState): List[UndoEffect.RecordBoundary] =
    reduced(event, state).effects.collect { case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary }

  private def offsetOfCursor(buffer: Buffer): Int =
    val cursor = buffer.cursorList.head.position
    buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)

  "InsertText at a single cursor" should "insert the whole string and leave the cursor after it" in {
    val after = bufferAfter(InsertText("こんにちは"), single("ab", 1))

    after.document.content.collect() shouldBe "aこんにちはb"
    offsetOfCursor(after) shouldBe 6
  }

  it should "insert a code point outside the BMP whole, with the cursor after both surrogate halves" in {
    val after = bufferAfter(InsertText(Astral), single("ab", 1))

    after.document.content.collect() shouldBe s"a${Astral}b"
    offsetOfCursor(after) shouldBe 3
  }

  it should "record exactly one undo boundary and one content version step for the commit" in {
    val before = single("ab", 1)
    val result = reduced(InsertText("こんにちは"), before)

    result.effects.collect { case AppEffect.Undo(_: UndoEffect.RecordBoundary) => 1 } should have size 1
    result.state.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "change nothing for an empty string" in {
    val before = single("ab", 1)
    val result = reduced(InsertText(""), before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "ab"
    result.effects shouldBe empty
  }

  it should "insert text containing a line break and leave the cursor after it" in {
    val after = bufferAfter(InsertText("x\ny"), single("ab", 1))

    after.document.content.collect() shouldBe "ax\nyb"
    after.cursorList.head.position shouldBe CursorPosition(1, 1)
  }

  "InsertText with a selection" should "replace the selection as a standalone undo step" in {
    val before =
      stateWith(
        "hello",
        List(CursorPosition(0, 4)),
        Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4)))
      )

    bufferAfter(InsertText("你好"), before).document.content.collect() shouldBe "h你好o"
    boundaries(InsertText("你好"), before).map(_.grouping) shouldBe List(EditGrouping.Standalone)
  }

  "InsertText with several cursors" should "insert the string at every cursor as one undo step" in {
    val before = stateWith("ab\ncd", List(CursorPosition(0, 1), CursorPosition(1, 1)))

    bufferAfter(InsertText(Astral), before).document.content.collect() shouldBe s"a${Astral}b\nc${Astral}d"
    boundaries(InsertText(Astral), before) should have size 1
  }

  "InsertText undo grouping" should "group a committed word with typed text" in {
    val before = single("hello", 5)

    boundaries(InsertText("ab"), before).map(_.grouping) shouldBe
      List(EditGrouping.Coalescing(EditKind.Typing, afterWord = false))
  }

  it should "open a new step for committed whitespace straight after a word, as typed whitespace does" in {
    val before = single("hello", 5)

    boundaries(InsertText("  "), before).map(_.grouping) shouldBe
      List(EditGrouping.Coalescing(EditKind.Typing, afterWord = true))
    boundaries(InsertChar(' '), before).map(_.grouping) shouldBe
      List(EditGrouping.Coalescing(EditKind.Typing, afterWord = true))
  }

  "InsertText of one character" should "turn a second hyphen into an en dash when smart punctuation is on, like typing" in {
    val before = single("a-", 2, smartPunctuation = true)

    bufferAfter(InsertText("-"), before).document.content.collect() shouldBe
      bufferAfter(InsertChar('-'), before).document.content.collect()
    bufferAfter(InsertText("-"), before).document.content.collect() shouldBe "a–"
  }

  "InsertText of several characters" should "be inserted literally even when smart punctuation is on" in {
    val before = single("a-", 2, smartPunctuation = true)

    bufferAfter(InsertText("-."), before).document.content.collect() shouldBe "a--."
  }

  "InsertText in a pane with no buffer" should "create a buffer holding the text" in {
    val initial = AppState.initial
    val empty   = EditorPane.withBuffer(paneId, bufferId).copy(bufferId = None)
    val state = initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map.empty,
        layout = initial.persisted.layout.copy(editorPanes = Map(paneId -> empty))
      )
    )
    val result = reduced(InsertText(Astral), state)

    result.state.persisted.buffers.values.map(_.document.content.collect()).toList shouldBe List(Astral)
  }

  "EditorPaneComponent.joinsRun" should "batch committed text with typed characters" in {
    EditorPaneComponent.joinsRun(InsertText("ab")) shouldBe true
  }
