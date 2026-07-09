package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.Queue
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

private[manager] trait StateManagerRuntimeSupport:
  this: StateManager =>

  protected def runtime: StateManagerRuntime
  protected def balance: Balance

  protected def stateRef: Ref[IO, AppState]          = runtime.stateRef
  protected def undoRef: Ref[IO, UndoState]          = runtime.undoRef
  protected def themeNamesRef: Ref[IO, List[String]] = runtime.themeNamesRef
  protected def quitSignal: Deferred[IO, Unit]       = runtime.quitSignal
  protected def logger: Logger[IO]                   = runtime.logger
  protected def policy: SessionManager.SessionPolicy = runtime.policy
  protected def themeManager: AppThemeManager        = runtime.themeManager
  protected def lspQueue: Queue[IO, LspEffect]       = runtime.lspQueue
  protected def documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]] =
    runtime.documentAnalysisFiberRef
  protected def onFontConfigChanged: FontConfig => IO[Unit]                   = runtime.onFontConfigChanged
  protected def deviceTextScaleProvider: IO[Double]                           = runtime.deviceTextScaleProvider
  protected def configPersistencePath: Option[Path]                           = runtime.configPersistencePath
  protected def uiPresetStore: UiPresetStore                                  = runtime.uiPresetStore
  protected def windowSizeProvider: IO[Option[PreferredWindowSize]]           = runtime.windowSizeProvider
  protected def onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit] = runtime.onPreferredWindowSizeChanged
  protected def fileDialog: com.serenity.io.FileDialog                        = runtime.fileDialog

  protected def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]] = runtime.mouseTargetCacheRef

  protected def fileManager: FileManager               = runtime.fileManager
  protected def sessionManager: SessionManager         = runtime.sessionManager
  protected def sessionPersistence: SessionPersistence = runtime.sessionPersistence

  protected def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  protected def scheduleDocumentAnalysis(): IO[Unit]
  protected def ensureCommandRunnerSurface(state: AppState): AppState
  protected def saveBufferEffect(bufferId: BufferId): IO[Unit]
  protected def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]

  protected def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

private[manager] trait StateManagerBehavior extends StateManagerFileFacadeBehavior:
  this: StateManager =>
