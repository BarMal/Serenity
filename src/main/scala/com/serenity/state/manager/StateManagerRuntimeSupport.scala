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

/** File and close-workflow operations used by the file capability. */
private[manager] trait FileCapabilityPort:
  def closeBuffer(bufferId: BufferId): IO[Unit]
  def directLoadFileEffect(path: Path): IO[Unit]
  def saveBufferEffect(bufferId: BufferId): IO[Unit]
  def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]

/** Surface transitions owned by the surface capability. */
private[manager] trait SurfaceCapabilityPort:
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def applyAnimationHooks(previousState: AppState): IO[Unit]

/** Runtime-owned services used while interpreting command effects. */
private[manager] trait EffectRuntimePort:
  def stateRef: Ref[IO, AppState]
  def themeNamesRef: Ref[IO, List[String]]
  def quitSignal: Deferred[IO, Unit]
  def logger: Logger[IO]
  def themeManager: AppThemeManager
  def lspQueue: Queue[IO, LspEffect]
  def onFontConfigChanged: FontConfig => IO[Unit]
  def deviceTextScaleProvider: IO[Double]
  def configPersistencePath: Option[Path]
  def uiPresetStore: UiPresetStore
  def windowSizeProvider: IO[Option[PreferredWindowSize]]
  def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

/** Editor capability calls used by command effects. */
private[manager] trait EffectEditorPort:
  def updateState(update: AppState => AppState): IO[Unit]
  def applyEvent(event: Event): IO[Unit]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def closeBuffer(bufferId: BufferId): IO[Unit]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def scheduleDocumentAnalysis(): IO[Unit]
  def ensureCommandRunnerSurface(state: AppState): AppState
  def advanceSurfaceAnimations(state: AppState): AppState

/** Surface capability calls used by command effects. */
private[manager] trait EffectSurfacePort:
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]
  def expandPinnedPanel(position: PanelPosition): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]
  def applyAnimationHooks(previousState: AppState): IO[Unit]

/** File capability calls used by command effects. */
private[manager] trait EffectFilePort:
  def fileDialog: com.serenity.io.FileDialog
  def fileManager: FileManager
  def directLoadFileEffect(path: Path): IO[Unit]
  def saveBufferEffect(bufferId: BufferId): IO[Unit]
  def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]

/** Session persistence operations used by command effects. */
private[manager] trait EffectSessionPort:
  def sessionPersistence: SessionPersistence
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def clearSession(): IO[Unit]

/** Modal file workflow operations used by command effects. */
private[manager] trait EffectModalWorkflowPort:
  def clearCloseActions(state: AppState): AppState
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

/** Operations used by editor façade methods. */
private[manager] trait EditorCapabilityPort:
  def stateRef: Ref[IO, AppState]
  def lspQueue: Queue[IO, LspEffect]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createNewEmptyBuffer(): IO[BufferId]
  def closeBuffer(bufferId: BufferId): IO[Unit]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def ensureCommandRunnerSurface(state: AppState): AppState
  def advanceSurfaceAnimations(state: AppState): AppState

/** Operations used by the viewport capability. */
private[manager] trait ViewportCapabilityPort:
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def updateFontConfig(update: FontConfig => FontConfig): IO[Unit]

/** Operations used by file/session workflow planning. */
private[manager] trait WorkflowCapabilityPort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def quitSignal: Deferred[IO, Unit]
  def logger: Logger[IO]
  def fileDialog: com.serenity.io.FileDialog
  def fileManager: FileManager
  def sessionPersistence: SessionPersistence
  def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)
  def updateState(update: AppState => AppState): IO[Unit]
  def createNewEmptyBuffer(): IO[BufferId]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def ensureCommandRunnerSurface(state: AppState): AppState
  def saveBufferEffect(bufferId: BufferId): IO[Unit]
  def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit]
  def clearCloseActions(state: AppState): AppState
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

/** State and analysis ownership required while routing editor events. */
private[manager] trait EventStatePort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def logger: Logger[IO]
  def documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]]
  def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]

/** Effects and commands triggered by event routing. */
private[manager] trait EventEffectPort:
  def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit]
  def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit]
  def executeCommand(command: com.serenity.command.Command): IO[Unit]

/** Workflow operations requested by event routing. */
private[manager] trait EventWorkflowPort:
  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]

/** UI configuration operations requested by event routing. */
private[manager] trait EventUiPort:
  def uiPresetStore: UiPresetStore

  def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig]

  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]

/** Explicit composition root for StateManager capabilities. */
private[manager] class StateManagerBehavior(
    val stateRef: Ref[IO, AppState],
    val undoRef: Ref[IO, UndoState],
    val themeNamesRef: Ref[IO, List[String]],
    val quitSignal: Deferred[IO, Unit],
    val logger: Logger[IO],
    val policy: SessionManager.SessionPolicy,
    val themeManager: AppThemeManager,
    val lspQueue: Queue[IO, LspEffect],
    val mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    val documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    val onFontConfigChanged: FontConfig => IO[Unit],
    val deviceTextScaleProvider: IO[Double],
    val configPersistencePath: Option[Path],
    val uiPresetStore: UiPresetStore,
    val windowSizeProvider: IO[Option[PreferredWindowSize]],
    val fileDialog: com.serenity.io.FileDialog,
    val fileManager: FileManager,
    val sessionManager: SessionManager,
    val sessionPersistence: SessionPersistence
)(using providedBalance: Balance):

  protected val balance: Balance = providedBalance

  private lazy val effectRuntimePort: EffectRuntimePort = new EffectRuntimePort:
    val stateRef                = StateManagerBehavior.this.stateRef
    val themeNamesRef           = StateManagerBehavior.this.themeNamesRef
    val quitSignal              = StateManagerBehavior.this.quitSignal
    val logger                  = StateManagerBehavior.this.logger
    val themeManager            = StateManagerBehavior.this.themeManager
    val lspQueue                = StateManagerBehavior.this.lspQueue
    val onFontConfigChanged     = StateManagerBehavior.this.onFontConfigChanged
    val deviceTextScaleProvider = StateManagerBehavior.this.deviceTextScaleProvider
    val configPersistencePath   = StateManagerBehavior.this.configPersistencePath
    val uiPresetStore           = StateManagerBehavior.this.uiPresetStore
    val windowSizeProvider      = StateManagerBehavior.this.windowSizeProvider

  private lazy val effectEditorPort: EffectEditorPort = new EffectEditorPort:
    def updateState(update: AppState => AppState): IO[Unit] = StateManagerBehavior.this.updateState(update)
    def applyEvent(event: Event): IO[Unit]                  = StateManagerBehavior.this.applyEvent(event)
    def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
      StateManagerBehavior.this.createBuffer(content, filePath)
    def closeBuffer(bufferId: BufferId): IO[Unit]          = StateManagerBehavior.this.closeBuffer(bufferId)
    def createPane(bufferId: Option[BufferId]): IO[PaneId] = StateManagerBehavior.this.createPane(bufferId)
    def switchToPane(paneId: PaneId): IO[Unit]             = StateManagerBehavior.this.switchToPane(paneId)
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      StateManagerBehavior.this.validateAndUpdateState(newState, fallbackState)
    def scheduleDocumentAnalysis(): IO[Unit] = StateManagerBehavior.this.scheduleDocumentAnalysis()
    def ensureCommandRunnerSurface(state: AppState): AppState =
      StateManagerBehavior.this.ensureCommandRunnerSurface(state)
    def advanceSurfaceAnimations(state: AppState): AppState = StateManagerBehavior.this.advanceSurfaceAnimations(state)

  private lazy val effectSurfacePort: EffectSurfacePort = new EffectSurfacePort:
    def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] = StateManagerBehavior.this.showPeek(content, at)
    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      StateManagerBehavior.this.pinPanel(content, position, size)
    def unpinPanel(position: PanelPosition): IO[Unit]          = StateManagerBehavior.this.unpinPanel(position)
    def expandPinnedPanel(position: PanelPosition): IO[Unit]   = StateManagerBehavior.this.expandPinnedPanel(position)
    def collapseExpandedPanel(): IO[Unit]                      = StateManagerBehavior.this.collapseExpandedPanel()
    def switchToPinnedPanel(position: PanelPosition): IO[Unit] = StateManagerBehavior.this.switchToPinnedPanel(position)
    def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
      StateManagerBehavior.this.resizePinnedPanel(position, newSize)
    def applyAnimationHooks(previousState: AppState): IO[Unit] =
      StateManagerBehavior.this.applyAnimationHooks(previousState)

  private lazy val effectFilePort: EffectFilePort = new EffectFilePort:
    val fileDialog                                     = StateManagerBehavior.this.fileDialog
    val fileManager                                    = StateManagerBehavior.this.fileManager
    def directLoadFileEffect(path: Path): IO[Unit]     = StateManagerBehavior.this.directLoadFileEffect(path)
    def saveBufferEffect(bufferId: BufferId): IO[Unit] = StateManagerBehavior.this.saveBufferEffect(bufferId)
    def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
      StateManagerBehavior.this.saveBufferAsEffect(bufferId, path)

  private lazy val effectSessionPort: EffectSessionPort = new EffectSessionPort:
    val sessionPersistence                  = StateManagerBehavior.this.sessionPersistence
    def saveSession(): IO[Unit]             = StateManagerBehavior.this.saveSession()
    def loadSession(): IO[Option[AppState]] = StateManagerBehavior.this.loadSession()
    def clearSession(): IO[Unit]            = StateManagerBehavior.this.clearSession()

  private lazy val effectModalWorkflowPort: EffectModalWorkflowPort = new EffectModalWorkflowPort:
    def clearCloseActions(state: AppState): AppState = StateManagerBehavior.this.clearCloseActions(state)
    def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
      StateManagerBehavior.this.beginCloseAction(scope, state)
    def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
      StateManagerBehavior.this.requestSaveAsFileDialog(state, bufferIdOverride)
    def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.refreshFileWorkflowEffect(surfaceId)
    def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitFileWorkflowEffect(surfaceId)
    def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitReplaceWorkflowEffect(surfaceId)
    def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitCloseWorkflowEffect(surfaceId)
    def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
      StateManagerBehavior.this.restoreSessionIntoCurrentViewport(restoredState, currentState)
    def createStartupSession(): IO[Unit]                        = StateManagerBehavior.this.createStartupSession()
    def restoreStartupSession(): IO[Unit]                       = StateManagerBehavior.this.restoreStartupSession()
    def activeEditorBufferId(state: AppState): Option[BufferId] = StateManagerBehavior.this.activeEditorBufferId(state)

  private lazy val filePort: FileCapabilityPort = new FileCapabilityPort:
    def closeBuffer(bufferId: BufferId): IO[Unit]      = StateManagerBehavior.this.closeBuffer(bufferId)
    def directLoadFileEffect(path: Path): IO[Unit]     = StateManagerBehavior.this.directLoadFileEffect(path)
    def saveBufferEffect(bufferId: BufferId): IO[Unit] = StateManagerBehavior.this.saveBufferEffect(bufferId)
    def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
      StateManagerBehavior.this.saveBufferAsEffect(bufferId, path)

  private lazy val editorPort: EditorCapabilityPort = new EditorCapabilityPort:
    val stateRef = StateManagerBehavior.this.stateRef
    val lspQueue = StateManagerBehavior.this.lspQueue
    def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
      StateManagerBehavior.this.createBuffer(content, filePath)
    def createNewEmptyBuffer(): IO[BufferId]               = StateManagerBehavior.this.createNewEmptyBuffer()
    def closeBuffer(bufferId: BufferId): IO[Unit]          = StateManagerBehavior.this.closeBuffer(bufferId)
    def createPane(bufferId: Option[BufferId]): IO[PaneId] = StateManagerBehavior.this.createPane(bufferId)
    def switchToPane(paneId: PaneId): IO[Unit]             = StateManagerBehavior.this.switchToPane(paneId)
    def ensureCommandRunnerSurface(state: AppState): AppState =
      StateManagerBehavior.this.ensureCommandRunnerSurface(state)
    def advanceSurfaceAnimations(state: AppState): AppState = StateManagerBehavior.this.advanceSurfaceAnimations(state)

  private lazy val workflowPort: WorkflowCapabilityPort = new WorkflowCapabilityPort:
    val stateRef                                            = StateManagerBehavior.this.stateRef
    val undoRef                                             = StateManagerBehavior.this.undoRef
    val quitSignal                                          = StateManagerBehavior.this.quitSignal
    val logger                                              = StateManagerBehavior.this.logger
    val fileDialog                                          = StateManagerBehavior.this.fileDialog
    val fileManager                                         = StateManagerBehavior.this.fileManager
    val sessionPersistence                                  = StateManagerBehavior.this.sessionPersistence
    def updateState(update: AppState => AppState): IO[Unit] = StateManagerBehavior.this.updateState(update)
    def createNewEmptyBuffer(): IO[BufferId]                = StateManagerBehavior.this.createNewEmptyBuffer()
    def createPane(bufferId: Option[BufferId]): IO[PaneId]  = StateManagerBehavior.this.createPane(bufferId)
    def switchToPane(paneId: PaneId): IO[Unit]              = StateManagerBehavior.this.switchToPane(paneId)
    def loadSession(): IO[Option[AppState]]                 = StateManagerBehavior.this.loadSession()
    def ensureCommandRunnerSurface(state: AppState): AppState =
      StateManagerBehavior.this.ensureCommandRunnerSurface(state)
    def saveBufferEffect(bufferId: BufferId): IO[Unit] = StateManagerBehavior.this.saveBufferEffect(bufferId)
    def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
      StateManagerBehavior.this.saveBufferAsEffect(bufferId, path)
    def clearCloseActions(state: AppState): AppState = StateManagerBehavior.this.clearCloseActions(state)
    def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
      StateManagerBehavior.this.beginCloseAction(scope, state)
    def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
      StateManagerBehavior.this.requestSaveAsFileDialog(state, bufferIdOverride)
    def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.refreshFileWorkflowEffect(surfaceId)
    def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitFileWorkflowEffect(surfaceId)
    def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitReplaceWorkflowEffect(surfaceId)
    def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      StateManagerBehavior.this.submitCloseWorkflowEffect(surfaceId)
    def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
      StateManagerBehavior.this.restoreSessionIntoCurrentViewport(restoredState, currentState)
    def createStartupSession(): IO[Unit]                        = StateManagerBehavior.this.createStartupSession()
    def restoreStartupSession(): IO[Unit]                       = StateManagerBehavior.this.restoreStartupSession()
    def activeEditorBufferId(state: AppState): Option[BufferId] = StateManagerBehavior.this.activeEditorBufferId(state)

  private lazy val surfacePort: SurfaceCapabilityPort = new SurfaceCapabilityPort:
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      StateManagerBehavior.this.validateAndUpdateState(newState, fallbackState)
    def applyAnimationHooks(previousState: AppState): IO[Unit] =
      StateManagerBehavior.this.applyAnimationHooks(previousState)

  private lazy val viewportPort: ViewportCapabilityPort = new ViewportCapabilityPort:
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      StateManagerBehavior.this.validateAndUpdateState(newState, fallbackState)
    def updateFontConfig(update: FontConfig => FontConfig): IO[Unit] =
      StateManagerBehavior.this.updateFontConfig(update)

  private lazy val eventStatePort: EventStatePort =
    new EventStatePort:
      val stateRef                 = StateManagerBehavior.this.stateRef
      val undoRef                  = StateManagerBehavior.this.undoRef
      val logger                   = StateManagerBehavior.this.logger
      val documentAnalysisFiberRef = StateManagerBehavior.this.documentAnalysisFiberRef
      val mouseTargetCacheRef      = StateManagerBehavior.this.mouseTargetCacheRef

  private lazy val eventEffectPort: EventEffectPort =
    new EventEffectPort:
      def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit] =
        StateManagerBehavior.this.interpretEffect(effect)
      def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
        StateManagerBehavior.this.interpretCommand(command, state)
      def executeCommand(command: com.serenity.command.Command): IO[Unit] =
        StateManagerBehavior.this.executeCommand(command)

  private lazy val eventWorkflowPort: EventWorkflowPort =
    new EventWorkflowPort:
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
        StateManagerBehavior.this.beginCloseAction(scope, state)
      def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
        StateManagerBehavior.this.createBuffer(content, filePath)
      def createPane(bufferId: Option[BufferId]): IO[PaneId] = StateManagerBehavior.this.createPane(bufferId)

  private lazy val eventUiPort: EventUiPort =
    new EventUiPort:
      val uiPresetStore = StateManagerBehavior.this.uiPresetStore
      def updateConfig(
        update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
      ): IO[com.serenity.config.AppConfig] =
        StateManagerBehavior.this.updateConfig(update)
      def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
        StateManagerBehavior.this.resizePinnedPanel(position, newSize)

  private lazy val workflow = new StateManagerWorkflowBehavior(workflowPort)

  private lazy val effects = new StateManagerEffectHandlers(
    effectRuntimePort,
    effectEditorPort,
    effectSurfacePort,
    effectFilePort,
    effectSessionPort,
    effectModalWorkflowPort
  )

  private lazy val events =
    new StateManagerEventPipelineBehavior(eventStatePort, eventEffectPort, eventWorkflowPort, eventUiPort)
  private lazy val editor   = new StateManagerEditorFacadeBehavior(editorPort)
  private lazy val surfaces = new StateManagerSurfaceFacadeBehavior(stateRef, logger, surfacePort)
  private lazy val viewport = new StateManagerViewportBehavior(stateRef, logger, deviceTextScaleProvider, viewportPort)
  private lazy val files    = new StateManagerFileFacadeBehavior(stateRef, filePort)

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
