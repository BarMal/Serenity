package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.{Command, CommandCategory, CommandIntent, FileIntent, SessionIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.keystroke.events.{InsertChar, SaveFile}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.core.EditorState
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.typelevel.log4cats.noop.NoOpLogger

/** File saves and loads run on `EffectLanes` and come back as versioned results (#1697 Wave 3: #1671, #1672): the
  * dispatcher never waits on the disk, and a result merges into whatever the state is when it arrives.
  */
class FileIoLanesSpec extends AnyFlatSpec with Matchers with Eventually:

  given Balance = Balance.default

  override given patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(20, Seconds), interval = Span(20, Millis))

  /** Holds each save of `gatedPath` open, before its disk write, until the spec releases it. Every save start and end
    * is recorded, tagged with the content being written.
    */
  final private class GatedFileManager(
      gatedPath: Path,
      gates: Ref[IO, List[Deferred[IO, Unit]]],
      log: Ref[IO, Vector[String]]
  ) extends FileManager:

    override def saveBuffer(buffer: Buffer): IO[Buffer] =
      val content = buffer.document.content.collect()
      val gate =
        if buffer.document.filePath.contains(gatedPath) then
          Deferred[IO, Unit].flatMap(release => gates.update(_ :+ release) >> release.get)
        else IO.unit
      log.update(_ :+ s"start:$content") >> gate >> super.saveBuffer(buffer) <* log.update(_ :+ s"end:$content")

  final private case class Fixture(
      stateManager: StateManager,
      gates: Ref[IO, List[Deferred[IO, Unit]]],
      log: Ref[IO, Vector[String]],
      directory: Path
  ):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def buffer(id: BufferId): Option[Buffer] = state.persisted.buffers.get(id)

    def focused: BufferId = state.focusedBufferId.getOrElse(fail("no focused buffer"))

    /** Waits until `count` gated saves are held open, then releases the oldest. */
    def releaseNext(count: Int = 1): Unit =
      eventually(gates.get.unsafeRunSync().size should be >= count)
      gates.modify(held => (held.drop(1), held.headOption)).unsafeRunSync().foreach(_.complete(()).unsafeRunSync())

    def awaitHeld(count: Int): Unit = eventually(gates.get.unsafeRunSync().size shouldBe count)

    def open(path: Path): BufferId =
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      eventually(state.focusedBufferId.flatMap(buffer).flatMap(_.document.filePath) shouldBe Some(path))
      focused

    def focus(id: BufferId): Unit =
      stateManager
        .updateStateValidated(state => EditorState.focusBuffer(EditorState.rebalancePanes(state, Some(id)), id))
        .unsafeRunSync()
      state.focusedBufferId shouldBe Some(id)

    def type_(char: Char): Unit = stateManager.applyEvent(InsertChar(char)).timeout(20.seconds).unsafeRunSync()

    def fireSave(): Unit = stateManager.applyEvent(SaveFile).timeout(20.seconds).unsafeRunSync()

  private def fixture(gatedName: String = "gated.txt", fileDialog: Option[FileDialog] = None): Fixture =
    val directory = Files.createTempDirectory("file-io-lanes-spec")
    val program =
      for
        gates               <- Ref.of[IO, List[Deferred[IO, Unit]]](Nil)
        log                 <- Ref.of[IO, Vector[String]](Vector.empty)
        modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty))
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
            fileDialog = fileDialog
          )
          .copy(fileManager = new GatedFileManager(directory.resolve(gatedName), gates, log))
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, gates, log, directory)
    program.unsafeRunSync()

  private def file(directory: Path, name: String, content: String): Path =
    Files.writeString(directory.resolve(name), content)

  private def diskRevision(path: Path) = new FileManager().currentRevision(path).unsafeRunSync()

  "A save" should "keep text typed while it is writing, and leave the buffer dirty (#1671)" in {
    val f  = fixture()
    val id = f.open(file(f.directory, "gated.txt", "draft"))
    f.type_('a')
    val saved = f.buffer(id).map(_.document.content.collect()).getOrElse(fail("buffer vanished"))

    f.fireSave()
    f.awaitHeld(1)
    f.type_('b')
    f.releaseNext()

    val path = f.directory.resolve("gated.txt")
    eventually(f.log.get.unsafeRunSync() should contain(s"end:$saved"))
    eventually(f.buffer(id).flatMap(_.document.revision) shouldBe diskRevision(path))
    Files.readString(path) shouldBe saved
    f.buffer(id).map(_.document.content.collect()) shouldBe Some(saved.patch(1, "b", 0))
    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    f.state.runtime.modalStack shouldBe empty
  }

  it should "mark the buffer clean when nothing was typed while it was writing" in {
    val f  = fixture()
    val id = f.open(file(f.directory, "gated.txt", "draft"))
    f.type_('a')

    f.fireSave()
    f.releaseNext()

    eventually(f.buffer(id).map(_.document.isDirty) shouldBe Some(false))
  }

  it should "not bring back a buffer closed while it was writing (#1671)" in {
    val f    = fixture()
    val path = file(f.directory, "gated.txt", "draft")
    val id   = f.open(path)
    f.type_('a')

    f.fireSave()
    f.awaitHeld(1)
    f.stateManager.updateStateValidated(EditorState.closeFocusedTab).timeout(5.seconds).unsafeRunSync()
    f.releaseNext()

    eventually(f.log.get.unsafeRunSync() should contain("end:adraft"))
    // The result is posted after the write; a later dispatch runs behind it.
    IO.sleep(200.millis).unsafeRunSync()
    f.stateManager.updateStateValidated(identity).unsafeRunSync()
    f.buffer(id) shouldBe None
    AppStateValidation.validationErrors(f.state) shouldBe Nil
  }

  private def quitIn(f: Fixture): IO[Unit] =
    f.stateManager.applyEvent(com.serenity.keystroke.events.Quit)

  private def quitCompleted(f: Fixture): Boolean =
    f.stateManager.runtimeLifecycle.awaitQuit.timeout(5.seconds).attempt.unsafeRunSync().isRight

  "Quitting right after a save" should "wait for the save to land and quit without an unsaved-changes prompt" in {
    val f  = fixture()
    val id = f.open(file(f.directory, "gated.txt", "draft"))
    f.type_('a')

    f.fireSave()
    f.awaitHeld(1)
    val quit = quitIn(f).start.unsafeRunSync()
    IO.sleep(200.millis).unsafeRunSync()
    f.releaseNext()
    quit.joinWithNever.timeout(20.seconds).unsafeRunSync()

    f.state.runtime.modalStack shouldBe empty
    Files.readString(f.directory.resolve("gated.txt")) shouldBe "adraft"
    f.buffer(id).map(_.document.isDirty) shouldBe Some(false)
    quitCompleted(f) shouldBe true
  }

  it should "keep the buffer open and dirty, and prompt, when that save fails" in {
    val f    = fixture()
    val path = file(f.directory, "gated.txt", "draft")
    val id   = f.open(path)
    f.type_('a')

    f.fireSave()
    f.awaitHeld(1)
    Files.writeString(path, "changed elsewhere")
    val quit = quitIn(f).start.unsafeRunSync()
    IO.sleep(200.millis).unsafeRunSync()
    f.releaseNext()
    quit.joinWithNever.timeout(20.seconds).unsafeRunSync()

    f.buffer(id).map(_.document.isDirty) shouldBe Some(true)
    f.state.runtime.modalStack.map(_.modal) should matchPattern { case List(_: Modal.CloseWorkflow) => }
    Files.readString(path) shouldBe "changed elsewhere"
    quitCompleted(f) shouldBe false
  }

  "A forced quit (window close) with a save in flight" should "wait for the save to land before quitting" in {
    val f  = fixture()
    val id = f.open(file(f.directory, "gated.txt", "draft"))
    f.type_('a')

    f.fireSave()
    f.awaitHeld(1)
    val forced = f.stateManager.runtimeLifecycle.forceQuit.start.unsafeRunSync()
    IO.sleep(200.millis).unsafeRunSync()
    val quitBeforeRelease = forced.join.map(_ => true).timeoutTo(1.milli, IO.pure(false)).unsafeRunSync()
    f.releaseNext()
    forced.joinWithNever.timeout(20.seconds).unsafeRunSync()

    quitBeforeRelease shouldBe false
    Files.readString(f.directory.resolve("gated.txt")) shouldBe "adraft"
    f.buffer(id).map(_.document.isDirty) shouldBe Some(false)
    quitCompleted(f) shouldBe true
  }

  "Two saves to one file" should "write in submission order, the second checked against the first's revision" in {
    val f  = fixture()
    val id = f.open(file(f.directory, "gated.txt", "draft"))
    f.type_('1')
    f.fireSave()
    f.awaitHeld(1)
    f.type_('2')
    f.fireSave()

    // The second must not start until the first is released.
    IO.sleep(200.millis).unsafeRunSync()
    f.log.get.unsafeRunSync() shouldBe Vector("start:1draft")
    f.releaseNext()
    f.releaseNext()

    eventually(f.buffer(id).map(_.document.isDirty) shouldBe Some(false))
    f.log.get.unsafeRunSync() shouldBe Vector("start:1draft", "end:1draft", "start:12draft", "end:12draft")
    Files.readString(f.directory.resolve("gated.txt")) shouldBe "12draft"
    f.state.runtime.modalStack shouldBe empty
  }

  "Saves to different files" should "run in parallel" in {
    val f       = fixture()
    val gatedId = f.open(file(f.directory, "gated.txt", "gated"))
    f.type_('g')
    val otherPath = file(f.directory, "other.txt", "other")
    val otherId   = f.open(otherPath)
    f.type_('o')

    f.focus(gatedId)
    f.fireSave()
    f.awaitHeld(1)
    f.focus(otherId)
    f.fireSave()

    eventually(Files.readString(otherPath) shouldBe "oother")
    eventually(f.buffer(otherId).map(_.document.isDirty) shouldBe Some(false))
    f.buffer(gatedId).map(_.document.isDirty) shouldBe Some(true)
    f.releaseNext()
    eventually(f.buffer(gatedId).map(_.document.isDirty) shouldBe Some(false))
  }

  "The external-change check" should "still prompt for a dirty buffer changed on disk after its save landed" in {
    val f    = fixture()
    val path = file(f.directory, "gated.txt", "draft")
    val id   = f.open(path)
    f.type_('a')
    f.fireSave()
    f.releaseNext()
    eventually(f.buffer(id).map(_.document.isDirty) shouldBe Some(false))

    f.type_('b')
    Files.writeString(path, "changed elsewhere")
    f.stateManager.fileService.checkBufferForExternalChanges(id).unsafeRunSync()

    f.state.runtime.modalStack.map(_.modal) should matchPattern {
      case List(Modal.ReloadConflict(ReloadConflictState(`id`, _, _))) =>
    }
  }

  it should "still reload a clean buffer changed on disk after its save landed" in {
    val f    = fixture()
    val path = file(f.directory, "gated.txt", "draft")
    val id   = f.open(path)
    f.type_('a')
    f.fireSave()
    f.releaseNext()
    eventually(f.buffer(id).map(_.document.isDirty) shouldBe Some(false))

    Files.writeString(path, "changed elsewhere")
    f.stateManager.fileService.checkBufferForExternalChanges(id).unsafeRunSync()

    eventually(f.buffer(id).map(_.document.content.collect()) shouldBe Some("changed elsewhere"))
  }

  "Opening a file through the native dialog" should "keep docked panels and commit a valid state (#1672)" in {
    val directory = Files.createTempDirectory("file-io-lanes-dialog")
    val target    = file(directory, "picked.txt", "picked")
    val dialog    = FileDialog(chooseOpenFile = _ => IO.pure(Some(target)), chooseSaveFile = (_, _) => IO.pure(None))
    val f         = fixture(fileDialog = Some(dialog))
    f.stateManager.panelManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 20).unsafeRunSync()
    val docked = f.state.runtime.uiSurfaces.map(_.id)
    docked should not be empty

    f.stateManager.commandExecutor
      .executeCommand(
        Command.typed("open", "Open a file.", CommandIntent.File(FileIntent.OpenFile), CommandCategory.File)
      )
      .timeout(5.seconds)
      .unsafeRunSync()

    eventually(f.state.persisted.buffers.values.flatMap(_.document.filePath).toList should contain(target))
    f.state.runtime.uiSurfaces.map(_.id) should contain allElementsOf docked
    AppStateValidation.validationErrors(f.state) shouldBe Nil
  }

  private def restoreSession(f: Fixture): Unit =
    f.stateManager.saveSession.unsafeRunSync()
    f.stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "restore",
          "Restore the session.",
          CommandIntent.Session(SessionIntent.RestoreSession),
          CommandCategory.File
        )
      )
      .timeout(5.seconds)
      .unsafeRunSync()

  private def restoredBuffer(f: Fixture, path: Path): Buffer =
    f.state.persisted.buffers.values.find(_.document.filePath.contains(path)).getOrElse(fail(s"$path not restored"))

  private def reloadConflicts(f: Fixture): List[Modal] =
    f.state.runtime.modalStack.map(_.modal).collect { case conflict: Modal.ReloadConflict => conflict }

  "A session-restored buffer" should "refuse to overwrite a file changed on disk after the restore (#1670)" in {
    val f    = fixture()
    val path = file(f.directory, "notes.txt", "draft")
    f.open(path)
    restoreSession(f)
    f.focus(restoredBuffer(f, path).id)

    Files.writeString(path, "changed elsewhere")
    f.type_('x')
    f.fireSave()

    eventually(reloadConflicts(f) should have size 1)
    Files.readString(path) shouldBe "changed elsewhere"
  }

  it should "keep a dirty buffer's unsaved text and still refuse to overwrite a file changed since it was edited" in {
    val f    = fixture()
    val path = file(f.directory, "notes.txt", "draft")
    f.open(path)
    f.type_('x')
    restoreSession(f)
    Files.writeString(path, "changed elsewhere")
    val restored = restoredBuffer(f, path)
    restored.document.content.collect() shouldBe "xdraft"
    f.focus(restored.id)

    f.fireSave()

    eventually(reloadConflicts(f) should have size 1)
    Files.readString(path) shouldBe "changed elsewhere"
  }

  it should "show the disk's content for a clean buffer whose file changed since the session was saved (#1670)" in {
    val f    = fixture()
    val path = file(f.directory, "notes.txt", "draft")
    f.open(path)
    f.stateManager.saveSession.unsafeRunSync()
    Files.writeString(path, "rewritten\non disk")
    restoreSession(f)

    val restored = restoredBuffer(f, path)
    restored.document.content.collect() shouldBe "rewritten\non disk"
    restored.document.revision shouldBe diskRevision(path)
    restored.document.isDirty shouldBe false
    AppStateValidation.validationErrors(f.state) shouldBe Nil
  }
