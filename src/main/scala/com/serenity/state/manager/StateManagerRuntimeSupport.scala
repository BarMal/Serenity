package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import com.serenity.io.FileManager
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

private[manager] trait StateManagerRuntimeSupport:
  this: StateManager =>

  protected def stateRef: Ref[IO, AppState]
  protected def undoRef: Ref[IO, UndoState]
  protected def balance: Balance
  protected def themeNamesRef: Ref[IO, List[String]]
  protected def quitSignal: Deferred[IO, Unit]
  protected def logger: Logger[IO]
  protected def policy: SessionManager.SessionPolicy
  protected def sessionRootOverride: Option[Path]
  protected def themeManager: AppThemeManager
  protected def lspQueue: Queue[IO, LspEffect]
  protected def onFontConfigChanged: FontConfig => IO[Unit]
  protected def configPersistencePath: Option[Path]

  protected def fileManager: FileManager
  protected def sessionManager: SessionManager
  protected def sessionPersistence: SessionPersistence

  protected def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  protected def ensureCommandRunnerSurface(state: AppState): AppState
  protected def saveBufferEffect(bufferId: BufferId): IO[Unit]
  protected def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]

private[manager] trait StateManagerBehavior extends StateManagerFileFacadeBehavior:
  this: StateManager =>
