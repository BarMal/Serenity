package com.serenity

import com.serenity.keystroke.events.{Copy, Cut}
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.text.TextStatistics
import com.serenity.ui.theme.RichTextStyling
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A soft break is a rope placeholder (U+FFFC); every path that leaves the editor as plain text must show it as the
  * line break it stands for, and counts must not read it as a letter.
  */
class SoftBreakPlainTextPathsSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val document = RichTextDocument(
    List(
      RichTextParagraph(List(RichTextRun("first"), RichTextRun.softBreak(), RichTextRun("line"))),
      RichTextParagraph.plain("second")
    )
  )

  private def bufferSelecting(anchor: CursorPosition, caret: CursorPosition): Buffer =
    Buffer(
      id = bufferId,
      document = Document(content = Rope(document.plainText)),
      editing = EditingState.fromCursors(List(Cursor(caret, Some(anchor)))),
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L)
    )

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  "Copy" should "put a newline on the clipboard for a selected soft break" in {
    val buffer = bufferSelecting(CursorPosition(0, 3), CursorPosition(0, 8))

    val result = EditorEventReducer.reduce(Copy, paneId, stateWith(buffer))

    result.state.runtime.clipboard shouldBe Some("st\nli")
  }

  "Cut" should "put a newline on the clipboard and remove the soft break from the buffer" in {
    val buffer = bufferSelecting(CursorPosition(0, 3), CursorPosition(0, 8))

    val result = EditorEventReducer.reduce(Cut, paneId, stateWith(buffer))

    result.state.runtime.clipboard shouldBe Some("st\nli")
    result.state.persisted.buffers(bufferId).document.content.getLine(0) shouldBe Some("firne")
  }

  "Copy with no selection" should "copy the whole line with the soft break as a newline" in {
    val buffer = bufferSelecting(CursorPosition(0, 2), CursorPosition(0, 2))
      .copy(editing = EditingState(List(CursorPosition(0, 2))))

    val result = EditorEventReducer.reduce(Copy, paneId, stateWith(buffer))

    result.state.runtime.clipboard shouldBe Some("first\nline")
  }

  "Plain-text export of a plain buffer" should "leave a literal object-replacement character alone" in {
    val plain = Buffer(id = bufferId, document = Document(content = Rope("a￼b")))

    plain.plainTextExport("a￼b") shouldBe "a￼b"
  }

  "The accessibility and export text of a rich buffer" should "show the soft break as a newline" in {
    bufferSelecting(CursorPosition(0, 0), CursorPosition(0, 0))
      .plainTextExport(document.plainText) shouldBe "first\nline\nsecond"
  }

  "Word and character counts" should "treat the soft break as a separator, not a character" in {
    val statistics = TextStatistics.of(Rope(document.plainText))

    statistics.wordCount shouldBe 3
    statistics.characterCountExcludingWhitespace shouldBe "firstlinesecond".length
    TextStatistics.ofString("first￼line").wordCount shouldBe 2
  }

  "Painting and measuring a soft break" should "use a visible return glyph in the placeholder's one-character slot" in {
    val spans = RichTextStyling.styledFontSpans(document, bufferLine = 0, startColumn = 0, endColumn = 10)

    spans.map(_.text).mkString shouldBe s"first${RichTextStyling.SoftBreakGlyph}line"
    spans.map(_.text.length).sum shouldBe 10
  }

  "Find" should "still locate the words on either side of a soft break" in {
    val content = Rope(document.plainText)

    FindSearch.results(content, "line").map(_.column) shouldBe Vector(6)
    FindSearch.results(content, "first").map(_.column) shouldBe Vector(0)
  }

end SoftBreakPlainTextPathsSpec
