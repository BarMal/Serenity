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
  def closeBuffer(bufferId: BufferId): IO[Unit]
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
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def scheduleDocumentAnalysis(): IO[Unit]
  def ensureCommandRunnerSurface(state: AppState): AppState
  def applyAnimationHooks(previousState: AppState): IO[Unit]
  def advanceSurfaceAnimations(state: AppState): AppState
  def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit]
  def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit]
  def directLoadFileEffect(path: Path): IO[Unit]
  def saveBufferEffect(bufferId: BufferId): IO[Unit]
  def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]
  def clearCloseActions(state: AppState): AppState
  def updateFontConfig(update: FontConfig => FontConfig): IO[Unit]

  def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig]

  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
  def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit]
  def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState
  def createStartupSession(): IO[Unit]
  def restoreStartupSession(): IO[Unit]
  def activeEditorBufferId(state: AppState): Option[BufferId]

/** Dependencies used by event processing, excluding file, session, and theme services. */
private[manager] trait StateManagerEventPipelineDependencies:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def logger: Logger[IO]
  def lspQueue: Queue[IO, LspEffect]
  def documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]]
  def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]
  def uiPresetStore: UiPresetStore
  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
  def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit]
  def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]

  def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig]

  def executeCommand(command: com.serenity.command.Command): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]

private[manager] trait StateManagerRuntimeSupport:
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

  protected def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

/** Internal behavior implementation with explicit runtime dependencies. */
private[manager] class StateManagerBehavior(protected val runtime: StateManagerRuntime)(using providedBalance: Balance)
    extends StateManagerRuntimeSupport,
      StateManagerBehaviorDependencies:

  protected val balance: Balance = providedBalance

  private val dependencies: StateManagerBehaviorDependencies = this

  private lazy val eventPipelineDependencies: StateManagerEventPipelineDependencies =
    new StateManagerEventPipelineDependencies:
      val stateRef                 = StateManagerBehavior.this.stateRef
      val undoRef                  = StateManagerBehavior.this.undoRef
      val logger                   = StateManagerBehavior.this.logger
      val lspQueue                 = StateManagerBehavior.this.lspQueue
      val documentAnalysisFiberRef = StateManagerBehavior.this.documentAnalysisFiberRef
      val mouseTargetCacheRef      = StateManagerBehavior.this.mouseTargetCacheRef
      val uiPresetStore            = StateManagerBehavior.this.uiPresetStore
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
        StateManagerBehavior.this.beginCloseAction(scope, state)
      def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit] =
        StateManagerBehavior.this.interpretEffect(effect)
      def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
        StateManagerBehavior.this.interpretCommand(command, state)
      def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
        StateManagerBehavior.this.createBuffer(content, filePath)
      def createPane(bufferId: Option[BufferId]): IO[PaneId] = StateManagerBehavior.this.createPane(bufferId)
      def updateConfig(
        update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
      ): IO[com.serenity.config.AppConfig] =
        StateManagerBehavior.this.updateConfig(update)
      def executeCommand(command: com.serenity.command.Command): IO[Unit] =
        StateManagerBehavior.this.executeCommand(command)
      def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
        StateManagerBehavior.this.resizePinnedPanel(position, newSize)

  private lazy val workflow = new StateManagerWorkflowBehavior(runtime, dependencies)
  private lazy val effects  = new StateManagerEffectHandlers(runtime, dependencies)
  private lazy val events   = new StateManagerEventPipelineBehavior(eventPipelineDependencies)
  private lazy val editor   = new StateManagerEditorFacadeBehavior(runtime, dependencies)
  private lazy val surfaces = new StateManagerSurfaceFacadeBehavior(runtime, dependencies)
  private lazy val viewport = new StateManagerViewportBehavior(runtime, dependencies)
  private lazy val files    = new StateManagerFileFacadeBehavior(runtime, dependencies)

  export editor.*
  export events.applyEvent
  export files.*
  export surfaces.*
  export viewport.*

  def lspEffectStream: Stream[IO, LspEffect] =
    Stream
      .fromQueueUnterminated(lspQueue)
      .interruptWhen(Stream.eval(quitSignal.get).as(true))

  def executeCommand(command: com.serenity.command.Command): IO[Unit] =
    stateRef.get.flatMap(state => effects.interpretCommand(command, state))

  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    events.validateAndUpdateState(newState, fallbackState)

  def scheduleDocumentAnalysis(): IO[Unit]                  = events.scheduleDocumentAnalysis()
  def ensureCommandRunnerSurface(state: AppState): AppState = events.ensureCommandRunnerSurface(state)
  def applyAnimationHooks(previousState: AppState): IO[Unit] =
    events.applyAnimationHooks(previousState)
  def advanceSurfaceAnimations(state: AppState): AppState = events.advanceSurfaceAnimations(state)
  def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit] =
    effects.interpretEffect(effect)
  def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
    effects.interpretCommand(command, state)
  def directLoadFileEffect(path: Path): IO[Unit]     = effects.directLoadFileEffect(path)
  def saveBufferEffect(bufferId: BufferId): IO[Unit] = effects.saveBufferEffect(bufferId)
  def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
    effects.saveBufferAsEffect(bufferId, path)
  def clearCloseActions(state: AppState): AppState                 = workflow.clearCloseActions(state)
  def updateFontConfig(update: FontConfig => FontConfig): IO[Unit] = effects.updateFontConfig(update)

  def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] = effects.updateConfig(update)

  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
    workflow.beginCloseAction(scope, state)
  def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
    workflow.requestSaveAsFileDialog(state, bufferIdOverride)
  def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    workflow.refreshFileWorkflowEffect(surfaceId)
  def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    workflow.submitFileWorkflowEffect(surfaceId)
  def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    workflow.submitReplaceWorkflowEffect(surfaceId)
  def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
    workflow.submitCloseWorkflowEffect(surfaceId)
  def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
    workflow.restoreSessionIntoCurrentViewport(restoredState, currentState)
  def createStartupSession(): IO[Unit]                        = workflow.createStartupSession()
  def restoreStartupSession(): IO[Unit]                       = workflow.restoreStartupSession()
  def activeEditorBufferId(state: AppState): Option[BufferId] = workflow.activeEditorBufferId(state)

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
