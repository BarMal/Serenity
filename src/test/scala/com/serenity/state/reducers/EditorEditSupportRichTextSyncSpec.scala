package com.serenity.state.reducers

import com.serenity.richtext.RichTextDocument
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `EditorEditSupport.richTextDocumentAfterEdit`'s staleness guard (`#1663`): it used to re-derive
  * `buffer.document.content.collect()` and compare it character-for-character against `richTextDocument.plainText` on
  * every edit -- the O(n) "sync discipline" the issue asks to replace. It now trusts `Buffer.richTextInSync`, an O(1)
  * comparison of two `Long`s, instead. These specs prove that trust is real: the decision follows the version stamp
  * even when it disagrees with what a full-text comparison would have found, in both directions.
  */
class EditorEditSupportRichTextSyncSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def bufferWith(text: String, richTextDocument: Option[RichTextDocument], inSync: Boolean): Buffer =
    val base = Buffer.fromString(BufferId(0), text)
    if inSync then
      base.copy(richText = base.richText.withSyncedDocument(richTextDocument, base.document.contentVersion))
    else base.copy(richText = base.richText.copy(richTextDocument = richTextDocument))

  "richTextDocumentAfterEdit" should
    "apply the edit using a version-stamped document even though its text no longer matches the buffer" in {
      // A `RichTextDocument` whose plain text ("wrong text entirely") is nothing like the buffer's ("hello world")
      // -- a full-text `matchesPlainText` re-scan would have failed this and returned `None`. The version stamp
      // says it's still in sync, so the edit is trusted and applied regardless: proof the text is never re-read.
      val mismatchedText =
        bufferWith("hello world", Some(RichTextDocument.fromPlainText("wrong text entirely")), inSync = true)

      val updated = EditorEditSupport.richTextDocumentAfterEdit(mismatchedText, 0, 0, "X")

      updated shouldBe defined
      updated.map(_.plainText) shouldBe Some("Xwrong text entirely")
    }

  it should "drop the document when it isn't stamped as synced, even though its text still matches the buffer" in {
    // The inverse: text-identical to the buffer, but never stamped (e.g. attached via a raw `.copy` instead of
    // `withSyncedDocument`/`withEditedContent`). A full-text comparison would have accepted this; the version
    // stamp does not, so the edit is declined -- proof the decision is the stamp, not the text.
    val unstamped = bufferWith("hello world", Some(RichTextDocument.fromPlainText("hello world")), inSync = false)

    EditorEditSupport.richTextDocumentAfterEdit(unstamped, 0, 0, "X") shouldBe None
  }

  it should "decline when there is no richTextDocument at all" in {
    val noDocument = Buffer.fromString(BufferId(0), "hello world")

    EditorEditSupport.richTextDocumentAfterEdit(noDocument, 0, 0, "X") shouldBe None
  }

  it should "stay correct across a chained sequence of edits without ever re-deriving the buffer's plain text" in {
    val start = bufferWith("alpha", Some(RichTextDocument.fromPlainText("alpha")), inSync = true)

    // Each step threads the previous step's (document, content) pair forward exactly as
    // `EditorEditSupport.foldEditsWithRichText` does: re-synthesizing a buffer whose `contentVersion`/
    // `richTextSyncedVersion` are left exactly as `start` had them, never re-bumped or re-verified mid-fold.
    val afterFirst = EditorEditSupport.richTextDocumentAfterEdit(start, 5, 5, " beta")
    afterFirst.map(_.plainText) shouldBe Some("alpha beta")

    val bufferAfterFirst = start.copy(
      document = start.document.copy(content = com.serenity.rope.Rope("alpha beta")),
      richText = start.richText.copy(richTextDocument = afterFirst)
    )
    bufferAfterFirst.richTextInSync shouldBe true

    val afterSecond = EditorEditSupport.richTextDocumentAfterEdit(bufferAfterFirst, 10, 10, " gamma")
    afterSecond.map(_.plainText) shouldBe Some("alpha beta gamma")
  }
