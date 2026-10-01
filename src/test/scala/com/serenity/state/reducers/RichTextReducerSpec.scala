package com.serenity.state.reducers

import java.nio.file.Paths

import com.serenity.command.{RichTextCommands, RichTextIntent}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[RichTextReducer]] as a plain function of `AppState`: every changed result is also checked against
  * `AppStateValidation`, since the shell used to commit these writes without validating them.
  */
class RichTextReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        )
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
    )

  private def selected(text: String, startLine: Int, startCol: Int, endLine: Int, endCol: Int): Buffer =
    val selection = Selection(CursorPosition(startLine, startCol), CursorPosition(endLine, endCol))
    Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingStateFixtures(cursors = List(selection.focus), selection = Some(selection)))

  private def validReduce(intent: RichTextIntent, state: AppState): Buffer =
    val result = RichTextReducer.reduce(intent, state)
    AppStateValidation.validationErrors(result.state) shouldBe Nil
    result.effects shouldBe Nil
    result.state.persisted.buffers.getOrElse(bufferId, fail("expected the edited buffer"))

  private def documentOf(buffer: Buffer): RichTextDocument =
    buffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))

  private def firstRunStyle(buffer: Buffer): RichTextStyle =
    documentOf(buffer).paragraphAt(0).flatMap(_.runs.headOption).getOrElse(fail("expected a run")).style

  private val range0to5 = RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 5))

  "RichTextReducer" should "toggle a mark across the selection, materializing the document and dirtying the buffer" in {
    val after =
      validReduce(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), stateWith(selected("hello world", 0, 0, 0, 5)))

    documentOf(after) shouldBe RichTextDocument.fromPlainText("hello world").toggleMark(range0to5, InlineMark.Bold)
    after.document.isDirty shouldBe true
    after.document.isNewEmpty shouldBe false
    after.richText.insertionRichTextStyle shouldBe Some(RichTextStyle.empty)
  }

  it should "keep a freshly materialized document in sync across a second, chained formatting command" in {
    // Regression for #1663: `currentDocument` builds a fresh `RichTextDocument` the first time a plain buffer is
    // formatted. If that fresh document isn't stamped as synced when it's written back, the *second* formatting
    // command sees `richTextInSync == false` and silently rebuilds from plain text again, discarding the first
    // command's mark.
    val state = stateWith(selected("hello world", 0, 0, 0, 5))

    val afterBold   = validReduce(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), state)
    val afterItalic = validReduce(RichTextIntent.ToggleRichTextMark(InlineMark.Italic), stateWith(afterBold))

    documentOf(afterItalic).paragraphAt(0).map(_.runs.headOption.map(_.style.marks)) shouldBe
      Some(Some(Set(InlineMark.Bold, InlineMark.Italic)))
  }

  it should "rebuild a stale rich-text document that no longer matches the buffer text" in {
    val stale  = RichTextDocument.fromPlainText("something else")
    val buffer = selected("hello world", 0, 0, 0, 5)
    val state  = stateWith(buffer.copy(richText = buffer.richText.copy(richTextDocument = Some(stale))))

    val after = validReduce(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), state)

    documentOf(after) shouldBe RichTextDocument.fromPlainText("hello world").toggleMark(range0to5, InlineMark.Bold)
  }

  it should "toggle a pending insertion mark on and off when nothing is selected" in {
    val state = stateWith(Buffer.fromString(bufferId, "hello world"))

    val on  = RichTextReducer.reduce(RichTextIntent.ToggleRichTextMark(InlineMark.Italic), state).state
    val off = validReduce(RichTextIntent.ToggleRichTextMark(InlineMark.Italic), on)

    on.persisted.buffers.get(bufferId).flatMap(_.richText.insertionRichTextStyle) shouldBe
      Some(RichTextStyle(marks = Set(InlineMark.Italic)))
    off.richText.insertionRichTextStyle shouldBe Some(RichTextStyle.empty)
    documentOf(off) shouldBe RichTextDocument.fromPlainText("hello world")
  }

  it should "set the font family, size and color of the selection" in {
    val state = stateWith(selected("hello world", 0, 0, 0, 5))

    firstRunStyle(validReduce(RichTextIntent.SetRichTextFontFamily("Menlo"), state)).fontFamily shouldBe Some("Menlo")
    firstRunStyle(validReduce(RichTextIntent.SetRichTextFontSize(18f), state)).fontSize shouldBe Some(18f)
    firstRunStyle(validReduce(RichTextIntent.SetRichTextColor("#ff0000"), state)).color shouldBe Some("#ff0000")
  }

  it should "adjust an un-sized run's font size relative to the default body size" in {
    val after = validReduce(RichTextIntent.AdjustRichTextFontSize(2f), stateWith(selected("hello world", 0, 0, 0, 5)))

    firstRunStyle(after).fontSize shouldBe Some(RichTextReducer.DefaultBodyFontSize + 2f)
  }

  it should "set the paragraph role at the cursor when there is no selection" in {
    val buffer = Buffer
      .fromString(bufferId, "line one\nline two")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))

    val after = validReduce(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2)), stateWith(buffer))

    documentOf(after).paragraphs.map(_.role) shouldBe List(ParagraphRole.Body, ParagraphRole.Heading(2))
    after.document.isDirty shouldBe true
  }

  it should "align every paragraph a multi-line selection spans" in {
    val state = stateWith(selected("line one\nline two\nline three", 0, 0, 2, 4))

    val after = validReduce(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center), state)

    documentOf(after).paragraphs.map(_.alignment) shouldBe List.fill(3)(ParagraphAlignment.Center)
  }

  it should "return the state unchanged when an inline style is requested with no selection" in {
    val state = stateWith(Buffer.fromString(bufferId, "hello world"))

    RichTextReducer.reduce(RichTextIntent.SetRichTextColor("#ff0000"), state) shouldBe ReducerResult(state, Nil)
  }

  it should "return the state unchanged when the change would be a no-op" in {
    val state = stateWith(selected("hello world", 0, 0, 0, 5))

    RichTextReducer.reduce(RichTextIntent.SetRichTextFontFamily(""), state) shouldBe ReducerResult(state, Nil)
  }

  it should "return the state unchanged without an active editor buffer" in {
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(layout = AppState.initial.persisted.layout.copy(editorPanes = Map.empty, activeEditorPaneId = None))
    )

    RichTextReducer.reduce(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), state) shouldBe ReducerResult(state, Nil)
    RichTextReducer.reduce(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body), state) shouldBe
      ReducerResult(state, Nil)
  }

  private val bold = RichTextIntent.ToggleRichTextMark(InlineMark.Bold)

  private def savedAs(buffer: Buffer, fileName: String, language: Option[LanguageId] = None): Buffer =
    buffer.copy(document = buffer.document.copy(filePath = Some(Paths.get(fileName)), language = language))

  "Routing a rich-text command" should "apply it to a rich-text buffer, or an untitled one that has no format yet" in {
    val untitled = selected("hello world", 0, 0, 0, 5)
    val richText = validReduce(bold, stateWith(untitled))

    RichTextReducer.route(bold, stateWith(untitled)) shouldBe RichTextRoute.Apply
    RichTextReducer.route(bold, stateWith(richText)) shouldBe RichTextRoute.Apply
  }

  it should "ask before converting a plain-text file, offering Save As, converting, or cancelling" in {
    val state = stateWith(savedAs(selected("hello world", 0, 0, 0, 5), "notes.txt"))

    RichTextReducer.route(bold, state) match
      case RichTextRoute.AskToConvert(prompt) =>
        prompt.message.mkString(" ") should include("notes.txt")
        prompt.choices.items.map(_.action) shouldBe Vector(
          ConfirmAction.Run(RichTextCommands.saveAsRichDocument),
          ConfirmAction.Run(RichTextCommands.convertToRichText(Some(bold))),
          ConfirmAction.Dismiss
        )
      case other => fail(s"expected a conversion prompt, got $other")
  }

  it should "refuse formatting in a code file" in {
    val state = stateWith(savedAs(selected("val x = 1", 0, 0, 0, 3), "A.scala", Some(LanguageId.Scala)))

    RichTextReducer.route(bold, state) shouldBe RichTextRoute.Refuse("Formatting isn't available in code files.")
  }

  it should "convert a plain-text file without asking when conversion is what was asked for" in {
    val state = stateWith(savedAs(selected("hello world", 0, 0, 0, 5), "notes.txt"))

    RichTextReducer.route(RichTextIntent.ConvertToRichText(Some(bold)), state) shouldBe RichTextRoute.Apply
  }

  "Converting to rich text" should "give the buffer an unformatted document without dirtying it, then run the follow-up" in {
    val state = stateWith(savedAs(selected("hello world", 0, 0, 0, 5), "notes.txt"))

    val converted = validReduce(RichTextIntent.ConvertToRichText(None), state)
    val formatted = validReduce(RichTextIntent.ConvertToRichText(Some(bold)), state)

    documentOf(converted) shouldBe RichTextDocument.fromPlainText("hello world")
    converted.document.isDirty shouldBe false
    EditingContext.bufferKind(converted) shouldBe BufferKind.RichText
    documentOf(formatted) shouldBe RichTextDocument.fromPlainText("hello world").toggleMark(range0to5, InlineMark.Bold)
  }

  "Dropping formatting" should "leave the buffer plain text with no rich-text document, its text untouched" in {
    val formatted = validReduce(bold, stateWith(savedAs(selected("hello world", 0, 0, 0, 5), "notes.txt")))

    val dropped = RichTextReducer.withoutFormatting(bufferId, stateWith(formatted))
    val buffer  = dropped.persisted.buffers.getOrElse(bufferId, fail("expected the buffer"))

    AppStateValidation.validationErrors(dropped) shouldBe Nil
    buffer.richText.richTextDocument shouldBe None
    EditingContext.bufferKind(buffer) shouldBe BufferKind.PlainText
    buffer.document.content.collect() shouldBe "hello world"
  }

  it should "leave the state untouched for an unknown buffer" in {
    val state = stateWith(selected("hello world", 0, 0, 0, 5))

    RichTextReducer.withoutFormatting(BufferId(99), state) shouldBe state
  }

  "A buffer's save" should "lose formatting only when it carries formatting its file's format can't store" in {
    val formatted = validReduce(bold, stateWith(selected("hello world", 0, 0, 0, 5)))

    savedAs(formatted, "notes.txt").formattingLostOnSave shouldBe true
    savedAs(formatted, "notes.odt").formattingLostOnSave shouldBe false
    savedAs(selected("hello world", 0, 0, 0, 5), "notes.txt").formattingLostOnSave shouldBe false
    formatted.formattingLostOnSave shouldBe false
  }
