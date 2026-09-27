package com.serenity.state.models

import com.serenity.richtext.RichTextDocument
import com.serenity.rope.{Balance, Rope}
import com.serenity.testkit.{EditingStateFixtures, VerticalCursorState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Buffer.withEditedContent` (`#1072`) centralises the five near-identical post-edit `buffer.copy(...)` blocks in
  * `EditorEventReducer`. It had already drifted before centralisation: `applyTrackedEdits` carried its recomputed
  * `richTextDocument` forward while `applyMergedDeletionEdits` silently dropped it (and left stale
  * `multiCursorVerticalStates` in place). This spec pins the corrected, uniform behaviour directly on `Buffer`; the
  * accompanying `EditorDocumentCommentTrackingSpec` regression test proves the reducer's merged-deletion path also has
  * it.
  */
class BufferWithEditedContentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val original = Buffer
    .fromString(BufferId(0), "alpha beta")
    .copy(
      editing = EditingStateFixtures(
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
    edited.editing.cursorPositions shouldBe List(CursorPosition(0, 1), CursorPosition(0, 7))
    edited.primarySelection shouldBe None
    edited.allSelections shouldBe edited.primarySelection.toList
  }

  /** `preferredColumn` is `None` on every fresh post-edit cursor rather than an explicit `Some` of its own column --
    * equivalent because every reader falls back to the cursor's own current column when it is `None` (`#1577`).
    */
  it should "reset preferredColumn/preferredXPx so they fall back to the primary (first) cursor's own column" in {
    val edited  = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 7), CursorPosition(0, 1)))
    val primary = edited.editing.cursors.head
    primary.preferredColumn shouldBe None
    primary.preferredColumn.getOrElse(primary.position.column) shouldBe 7
    primary.preferredXPx shouldBe None
  }

  it should "default preferredColumn to the document origin for an empty cursor list" in {
    val edited  = original.withEditedContent(Rope("gamma delta"), Nil)
    val primary = edited.editing.cursors.head
    primary.preferredColumn.getOrElse(primary.position.column) shouldBe 0
  }

  it should "always clear every cursor's preferred vertical-navigation state, not just leave it at the caller's mercy" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)))
    edited.editing.cursors.toList.forall(cursor =>
      cursor.preferredColumn.isEmpty && cursor.preferredXPx.isEmpty
    ) shouldBe true
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

  "Document.withContent" should "bump contentVersion on every call, even to identical text" in {
    val once  = original.document.withContent(Rope("gamma delta"))
    val twice = once.withContent(Rope("gamma delta"))
    once.contentVersion shouldBe original.document.contentVersion + 1
    twice.contentVersion shouldBe original.document.contentVersion + 2
  }

  "withEditedContent" should "bump the buffer's contentVersion on every edit" in {
    val edited = original.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)))
    edited.document.contentVersion shouldBe original.document.contentVersion + 1
  }

  it should "mark a supplied richTextDocument as in sync with the new content version" in {
    val document = RichTextDocument.fromPlainText("gamma delta")
    val edited = original.withEditedContent(
      Rope("gamma delta"),
      List(CursorPosition(0, 1)),
      richTextDocument = Some(document)
    )
    edited.richTextInSync shouldBe true
    edited.richText.richTextSyncedVersion shouldBe Some(edited.document.contentVersion)
  }

  it should "leave richTextInSync false when the caller clears richTextDocument" in {
    val withRichText = original.copy(richText =
      original.richText.withSyncedDocument(Some(RichTextDocument.fromPlainText("alpha beta")), original.document.contentVersion)
    )
    val edited =
      withRichText.withEditedContent(Rope("gamma delta"), List(CursorPosition(0, 1)), richTextDocument = None)
    edited.richTextInSync shouldBe false
  }

  "Buffer.richTextInSync" should "be false when there is no richTextDocument at all" in {
    original.richText.richTextDocument shouldBe None
    original.richTextInSync shouldBe false
  }

  it should "be false when richTextDocument is present but was never stamped as synced" in {
    val drifted = original.copy(richText =
      original.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("alpha beta")))
    )
    drifted.richTextInSync shouldBe false
  }

  it should "be false when the stamped version no longer matches the current content version" in {
    val synced = original.copy(richText =
      original.richText.withSyncedDocument(Some(RichTextDocument.fromPlainText("alpha beta")), original.document.contentVersion)
    )
    val driftedVersion = synced.copy(document = synced.document.withContent(synced.document.content))
    driftedVersion.richTextInSync shouldBe false
  }

  it should "be true when richTextDocument was stamped as synced against the current content version" in {
    val synced = original.copy(richText =
      original.richText.withSyncedDocument(Some(RichTextDocument.fromPlainText("alpha beta")), original.document.contentVersion)
    )
    synced.richTextInSync shouldBe true
  }
