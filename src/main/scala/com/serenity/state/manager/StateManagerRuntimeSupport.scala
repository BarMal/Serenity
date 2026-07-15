package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.Queue
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.Logger

/** Operations required across behavior boundaries without depending on the public manager façade. */
private[manager] trait StateManagerBehaviorDependencies:
  def updateState(update: AppState => AppState): IO[Unit]
  def applyEvent(event: Event): IO[Unit]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createNewEmptyBuffer(): IO[BufferId]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]
  def expandPinnedPanel(position: PanelPosition): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def clearSession(): IO[Unit]
  def executeCommand(command: com.serenity.command.Command): IO[Unit]

private[manager] trait StateManagerRuntimeSupport extends StateManagerBehaviorDependencies:
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

/** Internal behavior implementation with explicit runtime dependencies. */
private[manager] class StateManagerBehavior(protected val runtime: StateManagerRuntime)(using providedBalance: Balance)
    extends StateManagerFileFacadeBehavior:

  protected val balance: Balance = providedBalance

  def lspEffectStream: Stream[IO, LspEffect] =
    Stream
      .fromQueueUnterminated(lspQueue)
      .interruptWhen(Stream.eval(quitSignal.get).as(true))

  def executeCommand(command: com.serenity.command.Command): IO[Unit] =
    stateRef.get.flatMap(state => interpretCommand(command, state))

  def saveSession(): IO[Unit] =
    getCurrentState.flatMap { state =>
      sessionManager.saveSession(state, persistUnsavedBuffers = true) >>
        logger.info("[SESSION] Session saved")
    }.void

  def loadSession(): IO[Option[AppState]] =
    sessionManager.loadSession()

  def currentSessionThemeName: IO[Option[String]] =
    sessionManager.currentSessionThemeName

  def sessionExists: IO[Boolean] =
    sessionManager.sessionExists

  def clearSession(): IO[Unit] =
    sessionManager.clearSession()

  def awaitQuit: IO[Unit] = quitSignal.get

  def forceQuit(): IO[Unit] =
    stateRef.get.flatMap { state =>
      sessionPersistence
        .onAppClose(clearCloseActions(state))
        .handleErrorWith(error => logger.error(error)("[SESSION] Failed to save session during forced quit")) >>
        quitSignal.complete(()).attempt.void
    }

  def intervalSaveStream: Stream[IO, Unit] =
    policy.saveInterval match
      case None => Stream.empty
      case Some(interval) =>
        Stream
          .fixedRate[IO](interval)
          .interruptWhen(Stream.eval(quitSignal.get).as(true))
          .evalMap(_ =>
            stateRef.get.flatMap(
              sessionPersistence.maybeSaveSession(_, com.serenity.session.SessionSaveTrigger.Interval)
            )
          )

private[manager] object StateManagerBehavior:
  def apply(runtime: StateManagerRuntime)(using Balance): StateManagerBehavior =
    new StateManagerBehavior(runtime)
