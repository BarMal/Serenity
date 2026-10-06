package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
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

  /** `saveSession` returns once `outcome` has completed, as a write to a slow disk would. */
  final private class GatedSessionManager(root: Path, outcome: Deferred[IO, Either[Throwable, Unit]])
      extends SessionManager(root, AppThemeManager.create, quiet):
    override def saveSession(appState: AppState, persistUnsavedBuffers: Boolean): IO[Unit] =
      outcome.get.rethrow

  private def stateManagerSaving(outcome: Deferred[IO, Either[Throwable, Unit]]): IO[StateManager] =
    for
      directory           <- IO.blocking(Files.createTempDirectory("start-page-save-lanes"))
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
      gated = new GatedSessionManager(directory.resolve("gated-session"), outcome)
      stateManager <- StateManager.fromRuntime(
        runtime.copy(sessionManager = gated, sessionPersistence = new SessionPersistence(gated, runtime.policy))
      )
    yield stateManager

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
  private def discardingWhileSaving(outcome: Deferred[IO, Either[Throwable, Unit]]): IO[StateManager] =
    for
      stateManager <- stateManagerSaving(outcome)
      _            <- markBufferDirty(stateManager, "draft")
      _            <- stateManager.executeCommand(returnCommand)
      _            <- stateManager.applyEvent(TabKey)
      _            <- stateManager.applyEvent(Enter).timeout(5.seconds)
    yield stateManager

  "Returning to the start page" should "not hold the dispatcher while the session is written" in {
    val program =
      for
        outcome      <- Deferred[IO, Either[Throwable, Unit]]
        stateManager <- discardingWhileSaving(outcome)
        saving       <- stateManager.getCurrentState
      yield saving

    val saving = program.unsafeRunSync()

    saving.startPageSurface shouldBe None
    saving.persisted.buffers.get(BufferId(0)).exists(_.document.isDirty) shouldBe true
  }

  it should "show the start page once the session has been written" in {
    val program =
      for
        outcome      <- Deferred[IO, Either[Throwable, Unit]]
        stateManager <- discardingWhileSaving(outcome)
        _            <- outcome.complete(Right(()))
        _            <- stateManager.runtimeLifecycle.awaitEffects
        shown        <- stateManager.getCurrentState
      yield shown

    program.unsafeRunSync().startPageSurface.isDefined shouldBe true
  }

  it should "keep the editor when the session could not be written" in {
    val program =
      for
        outcome      <- Deferred[IO, Either[Throwable, Unit]]
        stateManager <- discardingWhileSaving(outcome)
        _            <- outcome.complete(Left(new java.io.IOException("disk full")))
        _            <- stateManager.runtimeLifecycle.awaitEffects
        kept         <- stateManager.getCurrentState
      yield kept

    val kept = program.unsafeRunSync()

    kept.startPageSurface shouldBe None
    kept.persisted.buffers.get(BufferId(0)).exists(_.document.isDirty) shouldBe true
  }

  it should "keep the editor when it changed while the session was being written" in {
    val program =
      for
        outcome      <- Deferred[IO, Either[Throwable, Unit]]
        stateManager <- discardingWhileSaving(outcome)
        _            <- markBufferDirty(stateManager, "draft, then more")
        _            <- outcome.complete(Right(()))
        _            <- stateManager.runtimeLifecycle.awaitEffects
        kept         <- stateManager.getCurrentState
      yield kept

    val kept = program.unsafeRunSync()

    kept.startPageSurface shouldBe None
    kept.persisted.buffers.get(BufferId(0)).map(_.document.content.toString) shouldBe Some("draft, then more")
  }
