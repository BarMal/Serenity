package com.serenity.state.manager

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.{Command, CommandRegistry, ExternalChangeCommands}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{CloseTab, Enter, Escape, Event, InsertChar}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.reducers.NoticeReducer
import com.serenity.state.undo.UndoState
import com.serenity.testkit.AwaitCondition
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Every way a save, open or reload can fail ends in a notice the user can see (#1717, #2015) -- not only a log line.
  * Each spec drives one failure path through a composed `StateManager`, with a storage fake that refuses the write or a
  * file that cannot be read.
  */
class SaveFailureNoticeRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** Refuses every write to `failing`, as a full disk or a revoked permission would. */
  final private class FailingFileManager(failing: Set[Path]) extends FileManager:

    override def saveBuffer(buffer: Buffer, path: Path): IO[Buffer] =
      if failing.contains(path) then IO.raiseError(new IOException("No space left on device"))
      else super.saveBuffer(buffer, path)

  final private case class Fixture(stateManager: StateManager, directory: Path):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def buffer(id: BufferId): Option[Buffer] = state.persisted.buffers.get(id)

    def open(path: Path): BufferId =
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      state.persisted.buffers.values.find(_.document.filePath.contains(path)).map(_.id).getOrElse(fail("not opened"))

    def send(event: Event): Unit =
      stateManager.applyEvent(event).timeout(20.seconds).unsafeRunSync()

    def run(commandName: String): Unit =
      val command: Command = CommandRegistry.default.findCommand(commandName).getOrElse(fail(s"no $commandName"))
      stateManager.executeCommand(command).timeout(20.seconds).unsafeRunSync()

    // `executeCommand` returns once the lanes settle, and a notice's expiry timer is a lane job: a command that raises
    // a self-dismissing notice returns only after the notice has gone.
    def runWithoutWaiting(commandName: String): Unit =
      val command: Command = CommandRegistry.default.findCommand(commandName).getOrElse(fail(s"no $commandName"))
      stateManager.executeCommand(command).timeout(20.seconds).unsafeRunAndForget()

    def notices: List[Notice] = NoticeReducer.visible(state)

    def awaitNotice(matching: Notice => Boolean): Notice =
      AwaitCondition
        .awaitValue(IO(NoticeReducer.visible(state)))(_.exists(matching))
        .unsafeRunSync()
        .find(matching)
        .getOrElse(fail(s"no matching notice in $notices"))

  private def fixture(
    failingNames: Set[String] = Set.empty,
    unwritableSession: Boolean = false,
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
  ): Fixture =
    val directory = Files.createTempDirectory("save-failure-notice-spec")
    // A regular file where the session directory should be: every session write fails to create its folder.
    val sessionRoot =
      if unwritableSession then Files.writeString(directory.resolve("session"), "not a directory")
      else directory.resolve("session")
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
            policy = policy,
            sessionRootOverride = Some(sessionRoot),
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
          .copy(fileManager = new FailingFileManager(failingNames.map(directory.resolve)))
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, directory)
    program.unsafeRunSync()

  private def file(directory: Path, name: String, content: String): Path =
    Files.writeString(directory.resolve(name), content)

  private def fileSaveFailure(name: String)(notice: Notice): Boolean =
    notice.level == NoticeLevel.Error && notice.message == s"Couldn't save $name: the disk is full."

  "Saving with Ctrl+S" should "show an error naming the file and the cause, and keep the buffer dirty" in {
    val f  = fixture(failingNames = Set("notes.txt"))
    val id = f.open(file(f.directory, "notes.txt", "draft"))
    f.send(InsertChar('a'))

    f.run("save")

    val notice = f.awaitNotice(fileSaveFailure("notes.txt"))
    notice.hint.getOrElse(fail("no hint")) should include("save as")
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    f.state.persisted.focus shouldBe a[Focus.EditorPane]
  }

  it should "clear the notice once the buffer is saved somewhere that works" in {
    val f  = fixture(failingNames = Set("notes.txt"))
    val id = f.open(file(f.directory, "notes.txt", "draft"))
    f.send(InsertChar('a'))
    f.run("save")
    f.awaitNotice(fileSaveFailure("notes.txt"))

    f.stateManager.fileService.saveBufferAs(id, f.directory.resolve("elsewhere.txt")).unsafeRunSync()

    AwaitCondition.awaitValue(IO(f.notices))(_.isEmpty).unsafeRunSync() shouldBe empty
    Files.readString(f.directory.resolve("elsewhere.txt")) shouldBe "adraft"
  }

  "Choosing Save in the close prompt" should "show the same error and keep the buffer open" in {
    val f    = fixture(failingNames = Set("notes.txt"))
    val path = file(f.directory, "notes.txt", "draft")
    val id   = f.open(path)
    f.send(InsertChar('a'))

    f.send(CloseTab)
    f.send(Enter)

    f.awaitNotice(fileSaveFailure("notes.txt"))
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    f.state.runtime.modalStack shouldBe empty
    Files.readString(path) shouldBe "draft"
  }

  "Saving as a new file" should "show an error naming the file it could not write" in {
    val f  = fixture(failingNames = Set("copy.txt"))
    val id = f.open(file(f.directory, "notes.txt", "draft"))

    f.stateManager.fileService.saveBufferAs(id, f.directory.resolve("copy.txt")).unsafeRunSync()

    f.awaitNotice(fileSaveFailure("copy.txt"))
    f.buffer(id).flatMap(_.document.filePath) shouldBe Some(f.directory.resolve("notes.txt"))
  }

  "Saving the session" should "show an error when the session cannot be written" in {
    val f = fixture(unwritableSession = true)

    f.run("save-session")

    val notice = f.awaitNotice(_.message.startsWith("Couldn't save the session:"))
    notice.level shouldBe NoticeLevel.Error
  }

  "Saving the session under a new name" should "show an error when the session cannot be written" in {
    val f = fixture(unwritableSession = true)

    f.run("save-session-as")
    "Draft".foreach(char => f.send(InsertChar(char)))
    f.send(Enter)

    f.awaitNotice(_.message.startsWith("Couldn't save the session:")).level shouldBe NoticeLevel.Error
  }

  "Backing up unsaved edits once typing pauses" should "warn when the backup cannot be written" in {
    val f = fixture(
      unwritableSession = true,
      policy = SessionManager.SessionPolicy(saveOnFileChange = false, saveOnEditIdle = Some(20.millis))
    )
    f.open(file(f.directory, "notes.txt", "draft"))

    f.send(InsertChar('a'))

    f.awaitNotice(_.message.startsWith("Couldn't back up this session:")).level shouldBe NoticeLevel.Warning
  }

  "Updating the session after a file save" should "warn when the session cannot be written" in {
    val f    = fixture(unwritableSession = true)
    val path = file(f.directory, "notes.txt", "draft")
    f.open(path)
    f.send(InsertChar('a'))

    f.runWithoutWaiting("save")

    f.awaitNotice(_.message.startsWith("Couldn't back up this session:")).level shouldBe NoticeLevel.Warning
    AwaitCondition.awaitValue(IO(Files.readString(path)))(_ == "adraft").unsafeRunSync()
  }

  "Opening a file" should "show an error when the file is not text" in {
    val f      = fixture()
    val binary = Files.write(f.directory.resolve("photo.txt"), Array[Byte](0, 1, 2, 0, 3))

    f.stateManager.fileOpener.openFile(binary).unsafeRunSync()

    f.awaitNotice(_.message == "Couldn't open photo.txt: it isn't a text file.").level shouldBe NoticeLevel.Error
  }

  it should "show an error when the file does not exist" in {
    val f = fixture()

    f.stateManager.fileOpener.openFile(f.directory.resolve("missing.txt")).unsafeRunSync()

    f.awaitNotice(_.message == "Couldn't open missing.txt: it doesn't exist or can't be read.")
  }

  "Reloading a file from disk" should "show an error when the file has gone" in {
    val f    = fixture()
    val path = file(f.directory, "notes.txt", "draft")
    val id   = f.open(path)
    Files.delete(path)

    f.stateManager.executeCommand(ExternalChangeCommands.reloadFromDisk(id)).timeout(20.seconds).unsafeRunSync()

    f.awaitNotice(_.message.startsWith("Couldn't reload notes.txt:")).level shouldBe NoticeLevel.Error
    f.buffer(id).map(_.document.content.collect()) shouldBe Some("draft")
  }

  "A save failure notice" should "stay while the user keeps typing, and go on Escape" in {
    val f  = fixture(failingNames = Set("notes.txt"))
    val id = f.open(file(f.directory, "notes.txt", "draft"))
    f.send(InsertChar('a'))
    f.run("save")
    f.awaitNotice(fileSaveFailure("notes.txt"))

    f.send(InsertChar('b'))

    f.notices.exists(fileSaveFailure("notes.txt")) shouldBe true
    f.buffer(id).map(_.document.content.collect()) shouldBe Some("abdraft")

    f.send(Escape)

    f.notices shouldBe empty
    f.buffer(id).map(_.document.content.collect()) shouldBe Some("abdraft")
  }
end SaveFailureNoticeRoutingSpec
