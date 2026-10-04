package com.serenity.state.undo

import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{InlineMark, RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, EditorEventReducer, UndoEffect}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Undo swaps whole content back in without replaying the edit (#1935), so the snapshot has to carry the rich-text
  * document that described that content -- otherwise the post-edit document stays attached to the pre-edit text while
  * `richTextInSync` still vouches for it, and the next save writes formatting at the wrong offsets.
  */
class BufferSnapshotRichTextSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val boldHello =
    RichTextDocument
      .fromPlainText("hello world")
      .toggleMark(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 5)), InlineMark.Bold)

  private def formattedBuffer: Buffer =
    val base = Buffer.fromString(bufferId, "hello world").copy(editing = EditingState(List(CursorPosition(0, 11))))
    base.copy(richText = base.richText.withSyncedDocument(Some(boldHello), base.document.contentVersion))

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  /** The buffer after typing `!`, the state after undoing it, and the redo entry that undo handed back. */
  private def typedAndUndone: (Buffer, AppState, HistoryEntry) =
    val result   = EditorEventReducer.reduce(InsertChar('!'), paneId, stateWith(formattedBuffer))
    val boundary = result.effects.collectFirst { case AppEffect.Undo(recorded: UndoEffect.RecordBoundary) => recorded }

    val (undoneState, redo) = boundary.value.entry.restore(result.state).value
    (result.state.persisted.buffers(bufferId), undoneState, redo)

  "Undoing typing in a formatted buffer" should "restore the rich-text document that described the restored text" in {
    val (typed, undoneState, _) = typedAndUndone

    val undone = undoneState.persisted.buffers(bufferId)
    typed.richText.richTextDocument.map(_.plainText) shouldBe Some("hello world!")
    undone.document.content.collect() shouldBe "hello world"
    undone.richText.richTextDocument shouldBe Some(boldHello)
    undone.richTextInSync shouldBe true
  }

  it should "advance contentVersion, so anything stamped against the typed text is seen as stale" in {
    val (typed, undoneState, _) = typedAndUndone

    undoneState.persisted.buffers(bufferId).document.contentVersion should be > typed.document.contentVersion
  }

  "Redoing the undone typing" should "bring back the typed text together with its formatting" in {
    val (typed, undoneState, redo) = typedAndUndone

    val redone = redo.restore(undoneState).value._1.persisted.buffers(bufferId)

    redone.document.content.collect() shouldBe "hello world!"
    redone.richText.richTextDocument shouldBe typed.richText.richTextDocument
    redone.richTextInSync shouldBe true
  }

  "A snapshot taken while the rich text was stale" should "restore it unstamped rather than vouch for it" in {
    val base   = Buffer.fromString(bufferId, "hello world")
    val stale  = base.copy(richText = base.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("x"))))
    val edited = base.withEditedContent(Rope("hello"), List(CursorPosition(0, 5)))

    val restored = BufferSnapshot.fromBuffer(stale).restoreInto(edited)

    restored.richText.richTextDocument shouldBe stale.richText.richTextDocument
    restored.richTextInSync shouldBe false
  }
