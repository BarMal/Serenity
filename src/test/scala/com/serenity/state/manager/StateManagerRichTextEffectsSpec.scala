package com.serenity.state.manager

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.RichTextIntent
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import com.serenity.state.undo.UndoState
import com.serenity.testkit.EditingStateFixtures
import com.serenity.ui.layout.{PeekContent, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
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

  final private class Harness(
      val modelRef: Ref[IO, Model],
      val notices: Ref[IO, List[PeekContent]],
      val richText: StateManagerRichTextEffects
  ):
    def state: AppState       = modelRef.get.unsafeRunSync().app
    def currentBuffer: Buffer = state.persisted.buffers(bufferId)

  private def validatingUpdate(modelRef: Ref[IO, Model])(transition: Model => Option[Model]): IO[Unit] =
    modelRef.update(model =>
      transition(model).filter(next => AppStateValidation.validated(next.app).isRight).getOrElse(model)
    )

  private val noEffectsExpected: AppEffect => IO[Unit] =
    effect => IO.raiseError(new IllegalStateException(s"unexpected effect $effect"))

  private def harness(buffer: Buffer): Harness =
    // nextBufferId must not collide with the fixture's buffer, or validation rejects every commit.
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = com.serenity.ui.layout.Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        )
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
    )
    val modelRef = Ref.of[IO, Model](Model(state, UndoState())).unsafeRunSync()
    val notices  = Ref.of[IO, List[PeekContent]](Nil).unsafeRunSync()
    new Harness(modelRef, notices, effectsOver(modelRef, notices))

  private def effectsOver(modelRef: Ref[IO, Model], notices: Ref[IO, List[PeekContent]]) =
    new StateManagerRichTextEffects(
      modelRef.get.map(_.app),
      validatingUpdate(modelRef),
      noEffectsExpected,
      (notice, _) => notices.update(_ :+ notice)
    )

  private def bufferWithSelection(text: String, selection: Selection): Buffer =
    Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingStateFixtures(cursors = List(selection.focus), selection = Some(selection)))

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
    val modelRef = Ref.of[IO, Model](Model(noActivePane, UndoState())).unsafeRunSync()
    val richText = effectsOver(modelRef, Ref.of[IO, List[PeekContent]](Nil).unsafeRunSync())

    richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    modelRef.get.unsafeRunSync().app shouldBe noActivePane
  }

  it should "commit through validation, keeping the prior state when the result is invalid" in {
    val fixture = harness(bufferWithSelection("hello world", selection(0, 0, 0, 5)))
    val invalid = fixture.modelRef
      .updateAndGet(model =>
        model.copy(app = model.app.copy(persisted = model.app.persisted.copy(bufferOrder = List(bufferId, bufferId))))
      )
      .unsafeRunSync()
      .app
    AppStateValidation.validationErrors(invalid) should not be empty

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    fixture.state shouldBe invalid
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
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
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

  private def savedAs(buffer: Buffer, fileName: String, language: Option[LanguageId] = None): Buffer =
    buffer.copy(document = buffer.document.copy(filePath = Some(Paths.get(fileName)), language = language))

  it should "ask before formatting a plain-text file, leaving it unformatted until answered" in {
    val fixture = harness(savedAs(bufferWithSelection("hello world", selection(0, 0, 0, 5)), "notes.txt"))

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    fixture.state.runtime.uiSurfaces.map(_.content) should matchPattern {
      case List(SurfaceContent.ModalWorkflow(Modal.Confirm(_))) =>
    }
    fixture.currentBuffer.richText.richTextDocument shouldBe None
    fixture.currentBuffer.document.isDirty shouldBe false
  }

  it should "refuse formatting in a code file with a notice, leaving it untouched" in {
    val code    = savedAs(bufferWithSelection("val x = 1", selection(0, 0, 0, 3)), "A.scala", Some(LanguageId.Scala))
    val fixture = harness(code)

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()

    fixture.currentBuffer shouldBe code
    fixture.state.runtime.uiSurfaces shouldBe empty
    fixture.notices.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Formatting isn't available in code files.")
    )
  }

  it should "record a formatting command as an undo step that restores the previous formatting" in {
    val fixture = harness(bufferWithSelection("hello world", selection(0, 0, 0, 5)))
    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Italic)).unsafeRunSync()
    val italic = fixture.currentBuffer

    fixture.richText.interpret(RichTextIntent.ToggleRichTextMark(InlineMark.Bold)).unsafeRunSync()
    val undone = UndoRecording.undone(fixture.modelRef.get.unsafeRunSync()).getOrElse(fail("expected an undo step"))

    val restored = undone.app.persisted.buffers(bufferId)
    restored.richText.richTextDocument shouldBe italic.richText.richTextDocument
    restored.richTextInSync shouldBe true
    undone.undo.redoStack should have size 1
  }
