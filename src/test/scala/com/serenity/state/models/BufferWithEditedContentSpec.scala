package com.serenity.state.models

import com.serenity.richtext.RichTextDocument
import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Buffer.withEditedContent` (`#1072`) centralises the five near-identical post-edit `buffer.copy(...)` blocks in
  * `EditorEventReducer`. It had already drifted before centralisation: `applyTrackedEdits` carried its recomputed
  * `richTextDocument` forward while `applyMergedDeletionEdits` silently dropped it (and left stale
  * `multiCursorVerticalStates` in place). This spec pins the corrected, uniform behaviour directly on `Buffer`; the
  * accompanying `EditorEventReducerSpec` regression test proves the reducer's merged-deletion path also has it.
  */
class BufferWithEditedContentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val original = Buffer
    .fromString(BufferId(0), "alpha beta")
    .copy(
      editing = EditingState(
        cursors = List(CursorPosition(0, 3)),
        selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5))),
        selections = List(Selection(CursorPosition(0, 0), CursorPosition(0, 5))),
        preferredColumn = Some(99),
        preferredXPx = Some(123f),
        multiCursorVerticalStates = List(VerticalCursorState(CursorPosition(0, 3), 3, 10f))
      ),
      annotations =
        Annotations(documentComments = List(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 3), "stale note")))
    )

  "withEditedContent" should "swap in the new content and mark the buffer dirty" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)))
    edited.document.content.collect() shouldBe "gamma delta"
    edited.document.isDirty shouldBe true
  }

  it should "clear isNewEmpty on an edit" in {
    val newEmpty = Buffer.newEmpty(BufferId(0))
    val edited   = newEmpty.withEditedContent(Rope("x"), List(CursorPosition(0, 1)))
    edited.document.isNewEmpty shouldBe false
  }

  it should "replace the cursor list and clear selection state" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1), CursorPosition(0, 7)))
    edited.editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 7))
    edited.editing.selection shouldBe None
    edited.editing.selections shouldBe Nil
  }

  it should "set preferredColumn from the primary (first) cursor and reset preferredXPx" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 7), CursorPosition(0, 1)))
    edited.editing.preferredColumn shouldBe Some(7)
    edited.editing.preferredXPx shouldBe None
  }

  it should "default preferredColumn to the document origin for an empty cursor list" in {
    val edited = original.withEditedContent(Rope("gamma delta"), Nil)
    edited.editing.preferredColumn shouldBe Some(0)
  }

  it should "always clear multiCursorVerticalStates, not just leave them at the caller's mercy" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)))
    edited.editing.multiCursorVerticalStates shouldBe Nil
  }

  it should "leave documentComments and richTextDocument unchanged when the caller doesn't supply them" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)))
    edited.annotations.documentComments shouldBe original.annotations.documentComments
    edited.richText.richTextDocument shouldBe original.richText.richTextDocument
  }

  it should "adopt the caller's remapped documentComments when supplied" in {
    val remapped = List(DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "remapped"))
    val edited =
      original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)), documentComments = remapped)
    edited.annotations.documentComments shouldBe remapped
  }

  it should "adopt the caller's recomputed richTextDocument when supplied" in {
    val document = RichTextDocument.fromPlainText("gamma delta")
    val edited = original.withEditedContent(
      Rope("gamma delta"),
      List(CursorPosition(0, 1)),
      richTextDocument = Some(document)
    )
    edited.richText.richTextDocument shouldBe Some(document)
  }

  it should "clear richTextDocument when the caller explicitly passes None" in {
    val withRichText = original.copy(richText =
      original.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("alpha beta")))
    )
    val edited =
      withRichText.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)), richTextDocument = None)
    edited.richText.richTextDocument shouldBe None
  }
