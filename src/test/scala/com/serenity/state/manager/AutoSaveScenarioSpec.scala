package com.serenity.state.manager

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.{AppConfig, AutoSaveMode, PreferredWindowSize}
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{Event, InsertChar, NewTab}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.NoticeReducer
import com.serenity.state.undo.UndoState
import com.serenity.testkit.{AwaitCondition, SharedDictionary}
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Auto-save writes a file through the normal save once its trigger fires (#1992): a pause after the last edit, or the
  * user leaving the buffer or the window. Each spec drives a composed `StateManager` over real files.
  */
class AutoSaveScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val Delay = 150.millis

  final private class FailingFileManager(failing: Set[Path]) extends FileManager:

    override def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
      if failing.contains(path) then IO.raiseError(new IOException("No space left on device"))
      else super.saveBuffer(buffer, path)

  final private case class Fixture(stateManager: StateManager, directory: Path):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def open(name: String, content: String): (BufferId, Path) =
      val path = Files.writeString(directory.resolve(name), content)
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      val id = state.persisted.buffers.values
        .find(_.document.filePath.contains(path))
        .map(_.id)
        .getOrElse(fail("not opened"))
      (id, path)

    def send(event: Event): Unit = stateManager.applyEvent(event).timeout(20.seconds).unsafeRunSync()

    def autoSave(mode: AutoSaveMode): Unit =
      stateManager
        .updateState(state =>
          state.copy(persisted =
            state.persisted
              .copy(config = state.persisted.config.withAutoSaveMode(mode).withAutoSaveDelayMillis(Delay.toMillis))
          )
        )
        .timeout(20.seconds)
        .unsafeRunSync()

    def awaitText(path: Path, expected: String): String =
      AwaitCondition.awaitValue(IO.blocking(Files.readString(path)))(_ == expected).unsafeRunSync()

    def textAfterQuiet(path: Path): String =
      (IO.sleep(Delay * 5) >> IO.blocking(Files.readString(path))).unsafeRunSync()

    def isDirty(id: BufferId): Boolean = state.persisted.buffers.get(id).exists(_.document.isDirty)

    def notices: List[Notice] = NoticeReducer.visible(state)

  private def fixture(failingNames: Set[String] = Set.empty): Fixture =
    val directory = Files.createTempDirectory("auto-save-scenario-spec")
    val program =
      for
        modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState()))
        themeNamesRef       <- Ref.of[IO, List[String]](Nil)
        quitSignal          <- Deferred[IO, Unit]
        lspQueue            <- LspEffectQueue.create
        mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
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
            mouseTargetCacheRef = mouseTargetCacheRef,
            onFontConfigChanged = (_: FontConfig) => IO.unit,
            deviceTextScaleProvider = IO.pure(1.0),
            configPersistencePath = None,
            uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
            windowSizeProvider = IO.pure(None),
            onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
            fileDialog = None,
            dictionaryCache = SharedDictionary.default
          )
          .copy(fileManager = new FailingFileManager(failingNames.map(directory.resolve)))
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, directory)
    program.unsafeRunSync()

  "auto-save" should "default to off, so a pause after typing leaves the file alone" in {
    AppConfig.default.autoSaveConfig.mode shouldBe AutoSaveMode.Off

    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.send(InsertChar('a'))

    f.textAfterQuiet(path) shouldBe "draft"
    f.isDirty(id) shouldBe true
  }

  "after-delay auto-save" should "write the file once typing pauses and leave the buffer clean" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.AfterDelay)

    f.send(InsertChar('a'))

    f.awaitText(path, "adraft")
    AwaitCondition.awaitValue(IO(f.isDirty(id)))(dirty => !dirty).unsafeRunSync() shouldBe false
  }

  it should "never save an untitled buffer, nor raise a Save As prompt for it" in {
    val f = fixture()
    f.autoSave(AutoSaveMode.AfterDelay)

    f.send(InsertChar('a'))
    IO.sleep(Delay * 5).unsafeRunSync()

    f.state.persisted.buffers.values.exists(_.document.isDirty) shouldBe true
    f.state.hasBlockingModal shouldBe false
    f.state.persisted.buffers.values.flatMap(_.document.filePath) shouldBe empty
  }

  it should "show the failure as a notice and keep the buffer unsaved when the write is refused" in {
    val f          = fixture(failingNames = Set("notes.txt"))
    val (id, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.AfterDelay)

    f.send(InsertChar('a'))

    AwaitCondition
      .awaitValue(IO(f.notices))(_.exists(_.message == "Couldn't save notes.txt: the disk is full."))
      .unsafeRunSync()
    f.isDirty(id) shouldBe true
    Files.readString(path) shouldBe "draft"
  }

  it should "leave a file that changed on disk alone, with a notice rather than a prompt" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.AfterDelay)
    Files.writeString(path, "edited elsewhere, and longer than before")

    f.send(InsertChar('a'))

    AwaitCondition
      .awaitValue(IO(f.notices))(_.exists(_.message.startsWith("Couldn't save notes.txt")))
      .unsafeRunSync()
    f.state.hasBlockingModal shouldBe false
    f.isDirty(id) shouldBe true
    Files.readString(path) shouldBe "edited elsewhere, and longer than before"
  }

  "on-focus-change auto-save" should "write a buffer when the user switches to another one" in {
    val f          = fixture()
    val (id, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.OnFocusChange)
    f.send(InsertChar('a'))
    f.textAfterQuiet(path) shouldBe "draft"

    f.send(NewTab)

    f.awaitText(path, "adraft")
    AwaitCondition.awaitValue(IO(f.isDirty(id)))(dirty => !dirty).unsafeRunSync() shouldBe false
  }

  it should "write unsaved buffers when the window loses focus" in {
    val f         = fixture()
    val (_, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.OnFocusChange)
    f.send(InsertChar('a'))

    f.stateManager.fileService.autoSaveOnWindowFocusLost.unsafeRunSync()

    f.awaitText(path, "adraft")
  }

  "on-window-change auto-save" should "ignore a switch between buffers but write when the window loses focus" in {
    val f         = fixture()
    val (_, path) = f.open("notes.txt", "draft")
    f.autoSave(AutoSaveMode.OnWindowChange)
    f.send(InsertChar('a'))

    f.send(NewTab)
    f.textAfterQuiet(path) shouldBe "draft"
    f.stateManager.fileService.autoSaveOnWindowFocusLost.unsafeRunSync()

    f.awaitText(path, "adraft")
  }

  "a window losing focus with auto-save off" should "write nothing" in {
    val f         = fixture()
    val (_, path) = f.open("notes.txt", "draft")
    f.send(InsertChar('a'))

    f.stateManager.fileService.autoSaveOnWindowFocusLost.unsafeRunSync()

    f.textAfterQuiet(path) shouldBe "draft"
  }
