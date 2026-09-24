package com.serenity.state.manager

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{CloseTab, Enter, Escape, InsertChar, Quit}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Choosing Save in the unsaved-changes prompt closes a buffer only once its save has landed (#1708): a failed or
  * conflicting save keeps the buffer open with its edits, and stops a quit or close-all where it is.
  */
class CloseSaveFailureSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** Refuses every write to `failing`, as a full disk or a revoked permission would. */
  final private class FailingFileManager(failing: Set[Path]) extends FileManager:

    override def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
      if failing.contains(path) then IO.raiseError(new IOException(s"cannot write $path"))
      else super.saveBuffer(buffer, path)

  final private case class Fixture(stateManager: StateManager, quitSignal: Deferred[IO, Unit], directory: Path):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def buffer(id: BufferId): Option[Buffer] = state.persisted.buffers.get(id)

    def open(path: Path): BufferId =
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      state.persisted.buffers.values.find(_.document.filePath.contains(path)).map(_.id).getOrElse(fail("not opened"))

    def send(event: com.serenity.keystroke.events.Event): Unit =
      stateManager.applyEvent(event).timeout(20.seconds).unsafeRunSync()

    def quitRequested: Boolean = quitSignal.tryGet.unsafeRunSync().isDefined

  private def fixture(failingNames: Set[String] = Set.empty): Fixture =
    val directory = Files.createTempDirectory("close-save-failure-spec")
    val program =
      for
        modelRef             <- Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty))
        themeNamesRef        <- Ref.of[IO, List[String]](Nil)
        quitSignal           <- Deferred[IO, Unit]
        lspQueue             <- LspEffectQueue.create
        projectTaskFiberRef  <- Ref.of[IO, Option[ManagedProjectTask]](None)
        projectTaskSemaphore <- Semaphore[IO](1)
        mouseTargetCacheRef  <- Ref.of[IO, Option[MouseTargetCache]](None)
        runtime = StateManagerRuntime
          .create(
            modelRef = modelRef,
            themeNamesRef = themeNamesRef,
            quitSignal = quitSignal,
            logger = NoOpLogger.impl[IO],
            policy = SessionManager.SessionPolicy(),
            sessionRootOverride = Some(directory.resolve("session")),
            themeManager = AppThemeManager.create,
            lspQueue = lspQueue,
            projectTaskFiberRef = projectTaskFiberRef,
            projectTaskSemaphore = projectTaskSemaphore,
            mouseTargetCacheRef = mouseTargetCacheRef,
            onFontConfigChanged = (_: FontConfig) => IO.unit,
            deviceTextScaleProvider = IO.pure(1.0),
            configPersistencePath = None,
            uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
            windowSizeProvider = IO.pure(None),
            onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
            fileDialog = None
          )
          .copy(fileManager = new FailingFileManager(failingNames.map(directory.resolve)))
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, quitSignal, directory)
    program.unsafeRunSync()

  private def file(directory: Path, name: String, content: String): Path =
    Files.writeString(directory.resolve(name), content)

  private def closePrompt(state: AppState): Option[CloseWorkflowState] =
    state.runtime.modalStack.lastOption.map(_.modal).collect { case Modal.CloseWorkflow(workflow) => workflow }

  private def hasCloseAction(state: AppState): Boolean =
    state.runtime.actionStack.exists { case AppAction.CloseWorkflow(_) => true }

  "Saving from the close prompt" should "keep the buffer open with its edits when the save fails (#1708)" in {
    val f    = fixture(failingNames = Set("notes.txt"))
    val path = file(f.directory, "notes.txt", "draft")
    val id   = f.open(path)
    f.send(InsertChar('a'))

    f.send(CloseTab)
    closePrompt(f.state).map(_.selectedChoice) shouldBe Some(CloseWorkflowChoice.Save)
    f.send(Enter)

    f.buffer(id).map(_.document.content.collect()) shouldBe Some("adraft")
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    f.state.runtime.modalStack shouldBe empty
    hasCloseAction(f.state) shouldBe false
    Files.readString(path) shouldBe "draft"
  }

  it should "keep the buffer open when the save finds the file changed on disk and the conflict is dismissed" in {
    val f    = fixture()
    val path = file(f.directory, "notes.txt", "draft")
    val id   = f.open(path)
    f.send(InsertChar('a'))
    Files.writeString(path, "changed elsewhere")

    f.send(CloseTab)
    f.send(Enter)

    f.state.runtime.modalStack.map(_.modal) should matchPattern { case List(_: Modal.ReloadConflict) => }
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)

    f.send(Escape)

    f.state.runtime.modalStack shouldBe empty
    hasCloseAction(f.state) shouldBe false
    f.buffer(id).map(_.document.content.collect()) shouldBe Some("adraft")
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    Files.readString(path) shouldBe "changed elsewhere"
  }

  "Quitting with two unsaved buffers" should "neither quit nor close anything when the first save fails" in {
    val f     = fixture(failingNames = Set("first.txt"))
    val first = f.open(file(f.directory, "first.txt", "one"))
    f.send(InsertChar('a'))
    val second = f.open(file(f.directory, "second.txt", "two"))
    f.send(InsertChar('b'))

    f.send(Quit)
    closePrompt(f.state).map(_.currentBufferId) shouldBe Some(first)
    f.send(Enter)

    f.state.runtime.modalStack shouldBe empty
    hasCloseAction(f.state) shouldBe false
    f.buffer(first).map(_.document.isDirty) shouldBe Some(true)
    f.buffer(second).map(_.document.isDirty) shouldBe Some(true)
    Files.readString(f.directory.resolve("second.txt")) shouldBe "two"
    f.quitRequested shouldBe false
  }

  it should "quit once both saves land" in {
    val f     = fixture()
    val first = f.open(file(f.directory, "first.txt", "one"))
    f.send(InsertChar('a'))
    val second = f.open(file(f.directory, "second.txt", "two"))
    f.send(InsertChar('b'))

    f.send(Quit)
    f.send(Enter)
    closePrompt(f.state).map(_.currentBufferId) shouldBe Some(second)
    f.send(Enter)

    Files.readString(f.directory.resolve("first.txt")) shouldBe "aone"
    Files.readString(f.directory.resolve("second.txt")) shouldBe "btwo"
    f.quitRequested shouldBe true
    f.buffer(first) shouldBe None
  }
end CloseSaveFailureSpec
