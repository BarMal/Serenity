package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.richtext.{DocumentFeature, FidelityReport, RichTextDocument, RichTextParagraph, SaveTarget}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A block line (a table kept read-only) takes no text: an edit that would put anything beside it is refused, and an
  * edit that removes the whole line is the user taking the block out (#1896).
  */
class OpaqueBlockEditGuardSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("before"),
      RichTextParagraph.plain("middle"),
      RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables),
      RichTextParagraph.plain("after")
    )
  )

  private def bufferAt(cursor: CursorPosition, anchor: Option[CursorPosition] = None): Buffer =
    Buffer(
      id = bufferId,
      document = Document(content = Rope(document.plainText)),
      editing = EditingState.fromCursors(List(Cursor(cursor, anchor))),
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L)
    )

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def reduced(event: TextEntryEvent, buffer: Buffer): Buffer =
    EditorEventReducer.reduce(event, paneId, stateWith(buffer)).state.persisted.buffers(bufferId)

  private def untouched(event: TextEntryEvent, buffer: Buffer): Unit =
    val result = reduced(event, buffer)
    result.document.content.toString shouldBe buffer.document.content.toString
    result.richText.richTextDocument shouldBe buffer.richText.richTextDocument
    result.editing.cursorPositions shouldBe buffer.editing.cursorPositions

  "Typing beside a block" should "do nothing before the block" in
    untouched(InsertChar('x'), bufferAt(CursorPosition(2, 0)))

  it should "do nothing after the block" in
    untouched(InsertChar('x'), bufferAt(CursorPosition(2, 1)))

  it should "do nothing for a tab" in
    untouched(TabKey, bufferAt(CursorPosition(2, 1)))

  "Joining a line to a block" should "be refused by Backspace at the start of the next line" in
    untouched(DeleteBackward, bufferAt(CursorPosition(3, 0)))

  it should "be refused by Delete at the end of the line before" in
    untouched(DeleteForward, bufferAt(CursorPosition(1, 6)))

  it should "be refused when a selection ends at the start of the block" in
    untouched(DeleteBackward, bufferAt(CursorPosition(2, 0), Some(CursorPosition(1, 3))))

  "Pasting beside a block" should "do nothing" in {
    val state = stateWith(bufferAt(CursorPosition(2, 1)))
    val withClipboard =
      state.copy(runtime = state.runtime.copy(clipboard = Some("pasted")))

    val result = EditorEventReducer.reduce(Paste, paneId, withClipboard).state.persisted.buffers(bufferId)

    result.document.content.toString shouldBe document.plainText
  }

  "Deleting a whole block line" should "remove the block with Backspace after it" in {
    val result = reduced(DeleteBackward, bufferAt(CursorPosition(2, 1)))

    result.document.content.getLine(2) shouldBe Some("")
    result.richText.richTextDocument.value.paragraphs.exists(_.isOpaqueBlock) shouldBe false
  }

  it should "remove the block with Delete before it" in {
    val result = reduced(DeleteForward, bufferAt(CursorPosition(2, 0)))

    result.richText.richTextDocument.value.paragraphs.exists(_.isOpaqueBlock) shouldBe false
  }

  it should "remove the block when a selection covers it with text before" in {
    val result = reduced(InsertChar('x'), bufferAt(CursorPosition(3, 0), Some(CursorPosition(1, 3))))

    result.document.content.getLine(1) shouldBe Some("midxafter")
    result.richText.richTextDocument.value.paragraphs.exists(_.isOpaqueBlock) shouldBe false
  }

  "Enter beside a block" should "add an empty line after it" in {
    val result = reduced(Enter, bufferAt(CursorPosition(2, 1)))

    val paragraphs = result.richText.richTextDocument.value.paragraphs
    paragraphs.map(_.isOpaqueBlock) shouldBe List(false, false, true, false, false)
    paragraphs(3).plainText shouldBe ""
  }

  it should "add an empty line before it" in {
    val result = reduced(Enter, bufferAt(CursorPosition(2, 0)))

    result.richText.richTextDocument.value.paragraphs.map(_.isOpaqueBlock) shouldBe
      List(false, false, false, true, false)
  }

  "Editing away from a block" should "work as usual and leave the block alone" in {
    val result = reduced(InsertChar('x'), bufferAt(CursorPosition(1, 6)))

    result.document.content.getLine(1) shouldBe Some("middlex")
    result.richText.richTextDocument.value.paragraphAt(2).exists(_.isOpaqueBlock) shouldBe true
  }

  it should "keep reporting the block once the text next to it was edited" in {
    val result = reduced(InsertChar('x'), bufferAt(CursorPosition(3, 5)))

    FidelityReport
      .forSave(result.richText.richTextDocument.value, SaveTarget.Rtf)
      .dropSummary shouldBe "1 table"
  }
