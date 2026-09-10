package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.RichTextIntent
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerRichTextEffects]] on its own: mark toggling, font/color changes, and paragraph
  * role/alignment changes, each asserted on the resulting `RichTextDocument` (or `insertionRichTextStyle`) rather than
  * through a fully composed `StateManager`.
  */
class StateManagerRichTextEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  final private class Harness(val stateRef: Ref[IO, AppState], val richText: StateManagerRichTextEffects):
    def currentBuffer: Buffer = stateRef.get.unsafeRunSync().persisted.buffers(bufferId)

  private def harness(buffer: Buffer): Harness =
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = com.serenity.ui.layout.Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        )
      )
    )
    val stateRef = Ref.of[IO, AppState](state).unsafeRunSync()
    new Harness(stateRef, new StateManagerRichTextEffects(stateRef))

  private def bufferWithSelection(text: String, selection: Selection): Buffer =
    Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingState(cursors = List(selection.focus), selection = Some(selection)))

  private def selection(startLine: Int, startCol: Int, endLine: Int, endCol: Int): Selection =
    Selection(CursorPosition(startLine, startCol), CursorPosition(endLine, endCol))

  "StateManagerRichTextEffects" should "toggle a mark on across the selected range" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 0, 0, 5))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    val after = fixture.currentBuffer
    after.richText.richTextDocument shouldBe Some(
      RichTextDocument
        .fromPlainText("hello world")
        .toggleMark(RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 5)), InlineMark.Bold)
    )
    after.document.isDirty shouldBe true
  }

  it should "toggle a mark back off when the whole selection already carries it" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 0, 0, 5))
    val fixture = harness(buffer)
    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document
      .paragraphAt(0)
      .getOrElse(fail("expected a paragraph"))
      .runs
      .forall(!_.style.marks.contains(InlineMark.Bold)) shouldBe true
  }

  it should "accumulate an insertion style rather than touch the document when there is no selection" in {
    val buffer  = Buffer.fromString(bufferId, "hello world")
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Italic)).unsafeRunSync()

    val after = fixture.currentBuffer
    after.richText.insertionRichTextStyle shouldBe Some(RichTextStyle(marks = Set(InlineMark.Italic)))
    after.richText.richTextDocument shouldBe Some(RichTextDocument.fromPlainText("hello world"))
  }

  it should "remove an already-pending mark from the insertion style on a second toggle with no selection" in {
    val buffer  = Buffer.fromString(bufferId, "hello world")
    val fixture = harness(buffer)
    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Italic)).unsafeRunSync()

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Italic)).unsafeRunSync()

    fixture.currentBuffer.richText.insertionRichTextStyle shouldBe Some(RichTextStyle.empty)
  }

  it should "leave the state untouched when there is no active editor buffer" in {
    val noActivePane = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(layout = AppState.initial.persisted.layout.copy(editorPanes = Map.empty, activeEditorPaneId = None))
    )
    val stateRef = Ref.of[IO, AppState](noActivePane).unsafeRunSync()
    val richText = new StateManagerRichTextEffects(stateRef)

    richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    stateRef.get.unsafeRunSync() shouldBe noActivePane
  }

  it should "apply a font family to the selected range" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 0, 0, 5))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextFontFamily("Menlo")).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document.paragraphAt(0).getOrElse(fail("expected a paragraph")).runs.head.style.fontFamily shouldBe Some("Menlo")
    fixture.currentBuffer.document.isDirty shouldBe true
  }

  it should "leave the state untouched setting a font family with no selection" in {
    val buffer  = Buffer.fromString(bufferId, "hello world")
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextFontFamily("Menlo")).unsafeRunSync()

    fixture.currentBuffer.richText.richTextDocument shouldBe None
    fixture.currentBuffer.document.isDirty shouldBe false
  }

  it should "apply a text color to the selected range" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 6, 0, 11))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextColor("#ff0000")).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document.paragraphAt(0).getOrElse(fail("expected a paragraph")).runs.map(_.style.color) shouldBe List(
      None,
      Some("#ff0000")
    )
  }

  it should "apply a font size to the selected range" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 0, 0, 11))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextFontSize(18f)).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document.paragraphAt(0).getOrElse(fail("expected a paragraph")).runs.head.style.fontSize shouldBe Some(18f)
  }

  it should "set a paragraph role at the cursor position when there is no selection" in {
    val buffer = Buffer
      .fromString(bufferId, "line one\nline two")
      .copy(editing = EditingState(cursors = List(CursorPosition(1, 2))))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2))).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document.paragraphAt(0).map(_.role) shouldBe Some(ParagraphRole.Body)
    document.paragraphAt(1).map(_.role) shouldBe Some(ParagraphRole.Heading(2))
  }

  it should "set the alignment for every paragraph spanned by a multi-line selection" in {
    val buffer  = bufferWithSelection("line one\nline two\nline three", selection(0, 0, 2, 4))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center)).unsafeRunSync()

    val document = fixture.currentBuffer.richText.richTextDocument.getOrElse(fail("expected a rich text document"))
    document.paragraphs.map(_.alignment) shouldBe List(
      ParagraphAlignment.Center,
      ParagraphAlignment.Center,
      ParagraphAlignment.Center
    )
  }

  it should "leave the state untouched when the requested change is a no-op" in {
    val buffer  = bufferWithSelection("hello world", selection(0, 0, 0, 5))
    val fixture = harness(buffer)

    fixture.richText.interpret(RichTextIntent.SetRichTextFontFamily("")).unsafeRunSync()

    fixture.currentBuffer.richText.richTextDocument shouldBe None
    fixture.currentBuffer.document.isDirty shouldBe false
  }
