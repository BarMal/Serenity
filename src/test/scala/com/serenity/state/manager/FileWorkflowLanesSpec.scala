package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileEntry, FileManager}
import com.serenity.keystroke.events.{Escape, InsertChar}
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

/** The Open dialog lists directories on a lane (#1697 Wave 3): a slow listing never holds the dispatcher, and a listing
  * that lands after the dialog has moved on is dropped.
  */
class FileWorkflowLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** Holds every directory listing until the spec opens `gate`. */
  final private class GatedListing(gate: Deferred[IO, Unit], started: Ref[IO, Int]) extends FileManager:
    override def listDirectory(directory: Path): IO[List[FileEntry]] =
      started.update(_ + 1) >> gate.get >> super.listDirectory(directory)

  final private case class Fixture(
      stateManager: StateManager,
      gate: Deferred[IO, Unit],
      started: Ref[IO, Int],
      directory: Path
  ):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def dialog: Option[FileWorkflowState] =
      state.topModal.map(_.modal).collect { case Modal.FileWorkflow(workflow) => workflow }

    /** Fails rather than hangs if the event waits on the held listing. */
    def send(event: com.serenity.keystroke.events.Event): Unit =
      stateManager.applyEvent(event).timeout(10.seconds).unsafeRunSync()

    def awaitListingHeld(): Unit =
      (IO.cede >> started.get).iterateUntil(_ > 0).timeout(10.seconds).unsafeRunSync(): Unit

    def releaseAndSettle(): Unit =
      (gate.complete(()) >> stateManager.runtimeLifecycle.awaitEffects).timeout(20.seconds).unsafeRunSync()

  private def fixture(): Fixture =
    val directory = Files.createTempDirectory("file-workflow-lanes-spec")
    val program =
      for
        gate                 <- Deferred[IO, Unit]
        started              <- Ref.of[IO, Int](0)
        modelRef             <- Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty))
        themeNamesRef        <- Ref.of[IO, List[String]](Nil)
        quitSignal           <- Deferred[IO, Unit]
        lspQueue             <- LspEffectQueue.create
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
            mouseTargetCacheRef = mouseTargetCacheRef,
            onFontConfigChanged = (_: FontConfig) => IO.unit,
            deviceTextScaleProvider = IO.pure(1.0),
            configPersistencePath = None,
            uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
            windowSizeProvider = IO.pure(None),
            onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
            fileDialog = None
          )
          .copy(fileManager = new GatedListing(gate, started))
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, gate, started, directory)
    program.unsafeRunSync()

  private def showOpenDialog(f: Fixture): Unit =
    f.stateManager.modalService
      .showModal(
        Modal.FileWorkflow(
          FileWorkflowState(
            mode = FileWorkflowMode.Open,
            path = f.directory.toString + java.io.File.separator,
            activeField = FileWorkflowField.Path
          )
        )
      )
      .unsafeRunSync()

  "Typing in the Open dialog" should "keep taking input while its directory listing is slow, then show the listing" in {
    val f = fixture()
    Files.writeString(f.directory.resolve("notes.txt"), "notes")
    showOpenDialog(f)

    f.send(InsertChar('n'))
    f.awaitListingHeld()
    f.send(InsertChar('o'))

    f.dialog.map(_.path) shouldBe Some(f.directory.toString + java.io.File.separator + "no")
    f.releaseAndSettle()
    f.dialog.map(_.suggestions.map(_.value)) shouldBe Some(List(f.directory.resolve("notes.txt").toString))
  }

  it should "drop a listing that lands after the dialog was closed" in {
    val f = fixture()
    showOpenDialog(f)

    f.send(InsertChar('n'))
    f.awaitListingHeld()
    f.send(Escape)

    f.releaseAndSettle()

    f.state.runtime.modalStack shouldBe empty
  }
end FileWorkflowLanesSpec
