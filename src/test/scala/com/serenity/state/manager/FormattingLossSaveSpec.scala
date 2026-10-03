package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{CommandRegistry, RichTextCommands}
import com.serenity.config.PreferredWindowSize
import com.serenity.keystroke.events.{CloseTab, Enter, InsertChar}
import com.serenity.richtext.{InlineMark, RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.testkit.AwaitCondition
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.ui.widget.ButtonEmphasis
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** A plain Save of a buffer whose file can't store its formatting asks first, rather than silently dropping it. */
class FormattingLossSaveSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Fixture(stateManager: StateManager, directory: Path):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def buffer(id: BufferId): Buffer = state.persisted.buffers.getOrElse(id, fail(s"buffer $id is not open"))

    def open(name: String, content: String): (BufferId, Path) =
      val path = Files.writeString(directory.resolve(name), content)
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      val id =
        state.persisted.buffers.values.find(_.document.filePath.contains(path)).map(_.id).getOrElse(fail("not opened"))
      (id, path)

    def send(event: com.serenity.keystroke.events.Event): Unit =
      stateManager.applyEvent(event).timeout(20.seconds).unsafeRunSync()

    def run(command: com.serenity.command.Command): Unit =
      stateManager.executeCommand(command).timeout(20.seconds).unsafeRunSync()

    def save(): Unit = run(CommandRegistry.withToggleUI.findCommand("save").getOrElse(fail("no save command")))

    def bolden(id: BufferId): Unit =
      stateManager
        .updateStateValidated { state =>
          state.persisted.buffers.get(id).fold(state) { buffer =>
            val text  = buffer.document.content.collect()
            val range = RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, text.length))
            val bold  = RichTextDocument.fromPlainText(text).toggleMark(range, InlineMark.Bold)
            val formatted = buffer.copy(
              document = buffer.document.copy(isDirty = true),
              richText = buffer.richText.withSyncedDocument(Some(bold), buffer.document.contentVersion)
            )
            state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(id, formatted)))
          }
        }
        .unsafeRunSync()

  private def fixture(): Fixture =
    val directory = Files.createTempDirectory("formatting-loss-save-spec")
    val program =
      for
        modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState()))
        themeNamesRef       <- Ref.of[IO, List[String]](Nil)
        quitSignal          <- cats.effect.Deferred[IO, Unit]
        lspQueue            <- LspEffectQueue.create
        mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
        runtime = StateManagerRuntime.create(
          modelRef = modelRef,
          themeNamesRef = themeNamesRef,
          quitSignal = quitSignal,
          logger = NoOpLogger.impl[IO],
          policy = SessionManager.SessionPolicy(),
          sessionRootOverride = Some(directory.resolve("session")),
          themeManager = AppThemeManager.create,
          lspQueue = lspQueue,
          mouseTargetCacheRef = mouseTargetCacheRef,
          onFontConfigChanged = (_: FontConfig) => IO.unit,
          deviceTextScaleProvider = IO.pure(1.0),
          configPersistencePath = None,
          uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
          windowSizeProvider = IO.pure(None),
          onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
          fileDialog = None
        )
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, directory)
    program.unsafeRunSync()

  private def floatingConfirm(state: AppState): Option[ConfirmPrompt] =
    state.runtime.uiSurfaces.map(_.content).collectFirst {
      case SurfaceContent.ModalWorkflow(Modal.Confirm(prompt)) =>
        prompt
    }

  /** A save, had one been submitted, lands on a background lane; give it the chance to before asserting it didn't. */
  private def settle(): Unit = IO.sleep(300.millis).unsafeRunSync()

  "Saving a formatted plain-text file" should "ask with a non-blocking prompt instead of writing the file" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.bolden(id)

    f.save()
    settle()

    val prompt = floatingConfirm(f.state).getOrElse(fail("expected the formatting prompt"))
    prompt.blocking shouldBe false
    prompt.message.mkString(" ") should include("notes.txt")
    f.state.runtime.modalStack shouldBe empty
    Files.readString(path) shouldBe "draft"
    f.buffer(id).richText.richTextDocument.exists(_.hasFormatting) shouldBe true
    f.buffer(id).document.isDirty shouldBe true
  }

  it should "offer Save As, saving without formatting, or cancelling" in {
    val prompt = ConfirmPrompt.formattingWouldBeLost(BufferId(7), "notes.txt")

    prompt.choices.items.map(_.action) shouldBe Vector(
      ConfirmAction.Run(RichTextCommands.saveAsRichDocument),
      ConfirmAction.Run(RichTextCommands.saveWithoutFormatting(BufferId(7))),
      ConfirmAction.Dismiss
    )
    prompt.choices.items.headOption.map(_.emphasis) shouldBe Some(ButtonEmphasis.Primary)
  }

  it should "write the plain text and leave the buffer clean and plain when saved without formatting" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.send(InsertChar('a'))
    f.bolden(id)

    f.run(RichTextCommands.saveWithoutFormatting(id))

    AwaitCondition.awaitValue(IO(Files.readString(path)))(_ == "adraft").unsafeRunSync()
    val saved = AwaitCondition.awaitValue(IO(f.buffer(id)))(!_.document.isDirty).unsafeRunSync()
    saved.richText.richTextDocument shouldBe None
    EditingContext.bufferKind(saved) shouldBe BufferKind.PlainText
    floatingConfirm(f.state) shouldBe None
  }

  "Saving an unformatted plain-text file" should "write it directly without asking" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.send(InsertChar('a'))

    f.save()

    AwaitCondition.awaitValue(IO(Files.readString(path)))(_ == "adraft").unsafeRunSync()
    AwaitCondition.awaitValue(IO(f.buffer(id)))(!_.document.isDirty).unsafeRunSync()
    floatingConfirm(f.state) shouldBe None
  }

  "Saving from the close prompt" should "open Save As for a formatted plain-text file rather than write it" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.bolden(id)

    f.send(CloseTab)
    com.serenity.ClosePromptFixtures.closePromptHighlight(f.state) shouldBe Some("Save")
    f.send(Enter)
    settle()

    f.state.topModal.map(_.modal) should matchPattern {
      case Some(Modal.FileWorkflow(workflow)) if workflow.mode == FileWorkflowMode.SaveAs =>
    }
    Files.readString(path) shouldBe "draft"
    f.buffer(id).richText.richTextDocument.exists(_.hasFormatting) shouldBe true
  }
end FormattingLossSaveSpec
