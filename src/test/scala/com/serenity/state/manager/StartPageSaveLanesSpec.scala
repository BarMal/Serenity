package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.TestTemp
import com.serenity.command.CommandRegistry
import com.serenity.config.PreferredWindowSize
import com.serenity.keystroke.events.{Enter, TabKey}
import com.serenity.rope.{Balance, Rope}
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Returning to the start page snapshots the session on the Session lane, not on the dispatcher (#1911): the keys typed
  * meanwhile are not queued behind the write, and the start page replaces the editor only if the editor is still what
  * was saved.
  */
class StartPageSaveLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quiet = NoOpLogger.impl[IO]

  /** Each `saveSession` returns once the next of `outcomes` has completed, as a write to a slow disk would; once they
    * are used up a save succeeds at once. `started` counts the saves begun.
    */
  final private class GatedSessionManager(
      root: Path,
      outcomes: Ref[IO, List[Deferred[IO, Either[Throwable, Unit]]]],
      val started: Ref[IO, Int]
  ) extends SessionManager(root, AppThemeManager.create, quiet):

    override def saveSession(appState: AppState, persistUnsavedBuffers: Boolean): IO[Unit] =
      started.update(_ + 1) >>
        outcomes
          .modify {
            case next :: rest => (rest, Some(next))
            case Nil          => (Nil, None)
          }
          .flatMap(_.fold(IO.unit)(_.get.rethrow))

  private def stateManagerSaving(
    outcomes: List[Deferred[IO, Either[Throwable, Unit]]]
  ): IO[(StateManager, Ref[IO, Int])] =
    for
      queued              <- Ref.of[IO, List[Deferred[IO, Either[Throwable, Unit]]]](outcomes)
      started             <- Ref.of[IO, Int](0)
      directory           <- IO.blocking(TestTemp.directory("start-page-save-lanes"))
      modelRef            <- Ref.of[IO, Model](Model(AppState.initial, com.serenity.state.undo.UndoState()))
      themeNamesRef       <- Ref.of[IO, List[String]](Nil)
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      runtime = StateManagerRuntime.create(
        modelRef = modelRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = quiet,
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
      gated = new GatedSessionManager(directory.resolve("gated-session"), queued, started)
      stateManager <- StateManager.fromRuntime(
        runtime.copy(sessionManager = gated, sessionPersistence = new SessionPersistence(gated, runtime.policy))
      )
    yield (stateManager, started)

  private def returnCommand =
    CommandRegistry.withToggleUI
      .findCommand("return-to-start-page")
      .getOrElse(fail("expected a return-to-start-page command to be registered"))

  private def markBufferDirty(stateManager: StateManager, content: String): IO[Unit] =
    stateManager.updateState { state =>
      val buffer = state.persisted.buffers(BufferId(0))
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            BufferId(0),
            buffer.copy(document =
              buffer.document.withContent(Rope(content)).copy(filePath = None, isDirty = true, isNewEmpty = false)
            )
          )
        )
      )
    }

  /** Closes the prompt the command opens with Close Anyway, with the session write still waiting on its disk. */
  private def discardingWhileSaving(
    outcomes: List[Deferred[IO, Either[Throwable, Unit]]]
  ): IO[(StateManager, Ref[IO, Int])] =
    for
      (stateManager, started) <- stateManagerSaving(outcomes)
      _                       <- markBufferDirty(stateManager, "draft")
      _                       <- stateManager.executeCommand(returnCommand)
      _                       <- stateManager.applyEvent(TabKey)
      _                       <- stateManager.applyEvent(Enter).timeout(5.seconds)
    yield (stateManager, started)

  private def savesStarted(started: Ref[IO, Int], count: Int): IO[Unit] =
    started.get
      .flatMap(n => if n >= count then IO.unit else IO.sleep(10.millis) >> savesStarted(started, count))
      .timeout(5.seconds)

  // A warning sweeps itself away after a few seconds, which `awaitEffects` would wait out: watch for it instead.
  private def noticeShown(stateManager: StateManager): IO[Unit] =
    stateManager.getCurrentState
      .flatMap(state =>
        if noticeMessages(state).nonEmpty then IO.unit else IO.sleep(10.millis) >> noticeShown(stateManager)
      )
      .timeout(5.seconds)

  private def noticeMessages(state: AppState): List[String] =
    state.runtime.uiSurfaces.collect { case UiSurface(_, SurfaceContent.Notice(notice, _), _, _) => notice.message }

  "Returning to the start page" should "not hold the dispatcher while the session is written" in {
    val program =
      for
        outcome           <- Deferred[IO, Either[Throwable, Unit]]
        (stateManager, _) <- discardingWhileSaving(List(outcome))
        saving            <- stateManager.getCurrentState
      yield saving

    val saving = program.unsafeRunSync()

    saving.startPageSurface shouldBe None
    saving.persisted.buffers.get(BufferId(0)).exists(_.document.isDirty) shouldBe true
  }

  it should "show the start page once the session has been written" in {
    val program =
      for
        outcome           <- Deferred[IO, Either[Throwable, Unit]]
        (stateManager, _) <- discardingWhileSaving(List(outcome))
        _                 <- outcome.complete(Right(()))
        _                 <- stateManager.runtimeLifecycle.awaitEffects
        shown             <- stateManager.getCurrentState
      yield shown

    program.unsafeRunSync().startPageSurface.isDefined shouldBe true
  }

  it should "keep the editor when the session could not be written" in {
    val program =
      for
        outcome           <- Deferred[IO, Either[Throwable, Unit]]
        (stateManager, _) <- discardingWhileSaving(List(outcome))
        _                 <- outcome.complete(Left(new java.io.IOException("disk full")))
        _                 <- stateManager.runtimeLifecycle.awaitEffects
        kept              <- stateManager.getCurrentState
      yield kept

    val kept = program.unsafeRunSync()

    kept.startPageSurface shouldBe None
    kept.persisted.buffers.get(BufferId(0)).exists(_.document.isDirty) shouldBe true
  }

  it should "write the session again and show the start page when the editor changed while it was being written" in {
    val program =
      for
        outcome               <- Deferred[IO, Either[Throwable, Unit]]
        (stateManager, saves) <- discardingWhileSaving(List(outcome))
        _                     <- markBufferDirty(stateManager, "draft, then more")
        _                     <- outcome.complete(Right(()))
        _                     <- stateManager.runtimeLifecycle.awaitEffects
        shown                 <- stateManager.getCurrentState
        written               <- saves.get
      yield (shown, written)

    val (shown, written) = program.unsafeRunSync()

    shown.startPageSurface.isDefined shouldBe true
    written shouldBe 2
  }

  it should "stay in the editor and say why when it changed again while the session was written a second time" in {
    val program =
      for
        first                 <- Deferred[IO, Either[Throwable, Unit]]
        second                <- Deferred[IO, Either[Throwable, Unit]]
        (stateManager, saves) <- discardingWhileSaving(List(first, second))
        _                     <- markBufferDirty(stateManager, "draft, then more")
        _                     <- first.complete(Right(()))
        _                     <- savesStarted(saves, 2)
        _                     <- markBufferDirty(stateManager, "draft, then more, and more")
        _                     <- second.complete(Right(()))
        _                     <- noticeShown(stateManager)
        kept                  <- stateManager.getCurrentState
      yield kept

    val kept = program.unsafeRunSync()

    kept.startPageSurface shouldBe None
    noticeMessages(kept) should contain("Stayed in the editor because it changed while the session was saving.")
  }
