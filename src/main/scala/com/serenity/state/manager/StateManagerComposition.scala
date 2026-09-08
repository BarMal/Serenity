package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.foldable.*
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.Logger

/** Explicit composition boundary for the StateManager capabilities. */
private[manager] class StateManagerComposition(
    val stateRef: Ref[IO, AppState],
    val undoRef: Ref[IO, UndoState],
    val themeNamesRef: Ref[IO, List[String]],
    val quitSignal: Deferred[IO, Unit],
    val logger: Logger[IO],
    val policy: SessionManager.SessionPolicy,
    val themeManager: AppThemeManager,
    val lspQueue: LspEffectQueue,
    val projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    val projectTaskSemaphore: Semaphore[IO],
    val mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    val documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    val bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]],
    val onFontConfigChanged: FontConfig => IO[Unit],
    val deviceTextScaleProvider: IO[Double],
    val configPersistencePath: Option[Path],
    val uiPresetStore: UiPresetStore,
    val windowSizeProvider: IO[Option[PreferredWindowSize]],
    val fileDialog: Option[com.serenity.io.FileDialog],
    val markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability,
    val fileManager: FileManager,
    val sessionManager: SessionManager,
    val sessionPersistence: SessionPersistence,
    operations: StateManagerOperationBoundary
)(using providedBalance: Balance):

  private val runtimeStateRef                 = stateRef
  private val runtimeUndoRef                  = undoRef
  private val runtimeThemeNamesRef            = themeNamesRef
  private val runtimeQuitSignal               = quitSignal
  private val runtimeLogger                   = logger
  private val runtimeThemeManager             = themeManager
  private val runtimeLspQueue                 = lspQueue
  private val runtimeProjectTaskFiberRef      = projectTaskFiberRef
  private val runtimeProjectTaskSemaphore     = projectTaskSemaphore
  private val runtimeMouseTargetCacheRef      = mouseTargetCacheRef
  private val runtimeDocumentAnalysisFiberRef = documentAnalysisFiberRef
  private val runtimeBufferAnimationsRef      = bufferAnimationsRef
  private val runtimeOnFontConfigChanged      = onFontConfigChanged
  private val runtimeDeviceTextScaleProvider  = deviceTextScaleProvider
  private val runtimeConfigPersistencePath    = configPersistencePath
  private val runtimeUiPresetStore            = uiPresetStore
  private val runtimeWindowSizeProvider       = windowSizeProvider
  private val runtimeFileDialog               = fileDialog
  private val runtimeMarkdownPreviewWindow    = markdownPreviewWindow
  private val runtimeFileManager              = fileManager
  private val runtimeSessionPersistence       = sessionPersistence

  private val filePersistence =
    new StateManagerFilePersistence(
      runtimeStateRef,
      runtimeFileManager,
      runtimeSessionPersistence,
      runtimeLogger,
      runtimeLspQueue
    )

  // Stateless facade over stateRef/bufferAnimationsRef -- shared by `editor` (which the old
  // EditorCapabilityPort routed through `events` for no functional reason, the only edge in the
  // effects -> workflow -> editor -> events -> effects cycle that forced deferred `def` port wiring)
  // and by `events`, which builds its own copy from the same refs. Constructing it here breaks that
  // cycle so every component below can be built directly, in dependency order, with plain `val`s.
  private val animations = new AnimationChoreography(new AnimationChoreographyPort:
    val stateRef            = runtimeStateRef
    val bufferAnimationsRef = runtimeBufferAnimationsRef)

  private val effectRuntimePort: EffectRuntimePort = new EffectRuntimePort:
    val stateRef                = runtimeStateRef
    val themeNamesRef           = runtimeThemeNamesRef
    val quitSignal              = runtimeQuitSignal
    val logger                  = runtimeLogger
    val themeManager            = runtimeThemeManager
    val lspQueue                = runtimeLspQueue
    val projectTaskFiberRef     = runtimeProjectTaskFiberRef
    val projectTaskSemaphore    = runtimeProjectTaskSemaphore
    val onFontConfigChanged     = runtimeOnFontConfigChanged
    val deviceTextScaleProvider = runtimeDeviceTextScaleProvider
    val configPersistencePath   = runtimeConfigPersistencePath
    val uiPresetStore           = runtimeUiPresetStore
    val windowSizeProvider      = runtimeWindowSizeProvider
    val bufferAnimationsRef     = runtimeBufferAnimationsRef
    val markdownPreviewWindow   = runtimeMarkdownPreviewWindow

  private val effectEditorPort: EffectEditorPort = new EffectEditorPort:
    def updateState(update: AppState => AppState): IO[Unit] = runtimeStateRef.update(update)
    def enqueueEvent(event: Event): IO[Unit]                = operations.enqueueEvent(event)
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      operations.validateAndUpdateState(newState, fallbackState)
    def scheduleDocumentAnalysis(): IO[Unit]                     = operations.scheduleDocumentAnalysis()
    def scheduleFindSearch(request: FindSearchRequest): IO[Unit] = operations.scheduleFindSearch(request)

  private val surfaces = new StateManagerSurfaceCapability(stateRef, logger, operations)

  private val editor = new StateManagerEditorCapability(
    runtimeStateRef,
    runtimeLspQueue,
    runtimeBufferAnimationsRef,
    animations
  )

  private val workflow = new StateManagerWorkflowCapability(
    runtimeStateRef,
    runtimeUndoRef,
    runtimeQuitSignal,
    runtimeLogger,
    runtimeFileDialog,
    runtimeFileManager,
    runtimeSessionPersistence,
    sessionManager,
    operations,
    editor,
    filePersistence
  )

  private val effectSurfacePort: EffectSurfacePort = new EffectSurfacePort:
    def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] = surfaces.showPeek(content, at)
    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      surfaces.pinPanel(content, position, size)
    def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
      surfaces.pinOrUpdateTerminalPanel(text, position, size)
    def unpinPanel(target: PanelTarget): IO[Unit]          = surfaces.unpinPanel(target)
    def expandPinnedPanel(target: PanelTarget): IO[Unit]   = surfaces.expandPinnedPanel(target)
    def collapseExpandedPanel(): IO[Unit]                  = surfaces.collapseExpandedPanel()
    def switchToPinnedPanel(target: PanelTarget): IO[Unit] = surfaces.switchToPinnedPanel(target)
    def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
      surfaces.resizePinnedPanel(target, newSize)

  private val effectFilePort: EffectFilePort = new EffectFilePort:
    val fileDialog                                             = runtimeFileDialog
    val fileManager                                            = runtimeFileManager
    def saveExistingBuffer(bufferId: BufferId): IO[Unit]       = filePersistence.saveExistingBuffer(bufferId)
    def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit] = filePersistence.saveBufferAs(bufferId, path)

  private val effectSessionPort: EffectSessionPort = new EffectSessionPort:
    val sessionPersistence = runtimeSessionPersistence
    def saveSession(): IO[Unit] =
      runtimeStateRef.get.flatMap { state =>
        sessionManager.saveSession(state, persistUnsavedBuffers = true) >>
          runtimeLogger.info("[SESSION] Session saved")
      }.void
    def loadSession(): IO[Option[AppState]] = sessionManager.loadSession()
    def clearSession(): IO[Unit]            = sessionManager.clearSession()

  private val effectModalWorkflowPort: EffectModalWorkflowPort = new EffectModalWorkflowPort:
    def clearCloseActions(state: AppState): AppState = workflow.clearCloseActions(state)
    def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
      workflow.beginCloseAction(scope, state)
    def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit] =
      workflow.showSaveAsWorkflow(state, bufferId, statusMessage)
    def openFileWorkflowModal(mode: FileWorkflowMode, state: AppState): IO[Unit] =
      workflow.openFileWorkflowModal(mode, state)
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
    def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
      workflow.createFileWorkflowDirectoriesEffect(surfaceId)
    def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
      workflow.restoreSessionIntoCurrentViewport(restoredState, currentState)
    def createStartupSession(): IO[Unit]                        = workflow.createStartupSession()
    def restoreStartupSession(): IO[Unit]                       = workflow.restoreStartupSession()
    def activeEditorBufferId(state: AppState): Option[BufferId] = workflow.activeEditorBufferId(state)

  private val eventStatePort: EventStatePort =
    new EventStatePort:
      val stateRef                 = runtimeStateRef
      val undoRef                  = runtimeUndoRef
      val logger                   = runtimeLogger
      val documentAnalysisFiberRef = runtimeDocumentAnalysisFiberRef
      val mouseTargetCacheRef      = runtimeMouseTargetCacheRef
      val bufferAnimationsRef      = runtimeBufferAnimationsRef

  private val eventEffectPort: EventEffectPort =
    new EventEffectPort:
      def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit] =
        effects.interpretEffect(effect)
      def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
        effects.interpretCommand(command, state)
      def executeCommand(command: com.serenity.command.Command): IO[Unit] =
        runtimeStateRef.get.flatMap(state => effects.interpretCommand(command, state))

  private val eventWorkflowPort: EventWorkflowPort =
    new EventWorkflowPort:
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
        workflow.beginCloseAction(scope, state)
      def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
        editor.bufferManager.createBuffer(content, filePath)
      def createPane(bufferId: Option[BufferId]): IO[PaneId] = editor.createPane(bufferId)

  private val eventUiPort: EventUiPort =
    new EventUiPort:
      val uiPresetStore = runtimeUiPresetStore
      def updateConfig(
        update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
      ): IO[com.serenity.config.AppConfig] =
        effects.updateConfig(update)
      def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
        surfaces.resizePinnedPanel(target, newSize)

  private val effects = new StateManagerEffectHandlers(
    effectRuntimePort,
    effectEditorPort,
    effectSurfacePort,
    effectFilePort,
    effectSessionPort,
    effectModalWorkflowPort
  )

  private val events =
    new StateManagerEventPipeline(eventStatePort, eventEffectPort, eventWorkflowPort, eventUiPort, operations)
  private val viewport =
    new StateManagerViewportCapability(stateRef, logger, deviceTextScaleProvider, events, effects)
  private val files = new StateManagerFileCapability(stateRef, effects)

  export editor.*
  export events.applyEvent
  export files.*
  export viewport.*

  val lspEffectSource: LspEffectSource = LspEffectSource(lspEffectStream = lspEffectStream)

  private def lspEffectStream: Stream[IO, LspEffect] =
    lspQueue.stream
      .interruptWhen(Stream.eval(quitSignal.get).as(true))

  def executeCommand(command: com.serenity.command.Command): IO[Unit] =
    stateRef.get.flatMap(state => effects.interpretCommand(command, state)) >> drainPendingOperations

  private def drainPendingOperations: IO[Unit] =
    operations.takeOperations.flatMap {
      case Nil => IO.unit
      case pendingOperations =>
        pendingOperations.traverse_ {
          case StateManagerOperation.Event(event)                       => events.applyEvent(event)
          case StateManagerOperation.ApplyAnimationHooks(previousState) => events.applyAnimationHooks(previousState)
        } >> drainPendingOperations
    }

  private def runSurfaceOperation(operation: IO[Unit]): IO[Unit] =
    operation >> drainPendingOperations

  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] =
    runSurfaceOperation(surfaces.showPeek(content, at))
  def dismissPeek(): IO[Unit]                      = runSurfaceOperation(surfaces.dismissPeek())
  def peekToPin(position: PanelPosition): IO[Unit] = runSurfaceOperation(surfaces.peekToPin(position))
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
    runSurfaceOperation(surfaces.pinPanel(content, position, size))
  def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
    runSurfaceOperation(surfaces.pinOrUpdateTerminalPanel(text, position, size))
  def unpinPanel(target: PanelTarget): IO[Unit] = runSurfaceOperation(surfaces.unpinPanel(target))
  def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit] =
    runSurfaceOperation(surfaces.movePinnedPanel(surfaceId, position))
  def expandPinnedPanel(target: PanelTarget): IO[Unit] = runSurfaceOperation(surfaces.expandPinnedPanel(target))
  def collapseExpandedPanel(): IO[Unit]                = runSurfaceOperation(surfaces.collapseExpandedPanel())
  def showModal(modal: Modal): IO[Unit]                = runSurfaceOperation(surfaces.showModal(modal))
  def dismissModal(): IO[Unit]                         = runSurfaceOperation(surfaces.dismissModal())
  def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
    runSurfaceOperation(surfaces.switchToPinnedPanel(target))
  def loadDirectoryTree(path: Path, files: List[String]): IO[Unit] =
    runSurfaceOperation(surfaces.loadDirectoryTree(path, files))
  def selectFileInExplorer(filePath: Path): IO[Unit] = runSurfaceOperation(surfaces.selectFileInExplorer(filePath))
  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
    runSurfaceOperation(surfaces.resizePinnedPanel(target, newSize))
  def dragFileToDirectory(sourceFile: Path, targetDir: Path): IO[Unit] =
    runSurfaceOperation(surfaces.dragFileToDirectory(sourceFile, targetDir))

  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    events.validateAndUpdateState(newState, fallbackState)

  def scheduleDocumentAnalysis(): IO[Unit]                  = events.scheduleDocumentAnalysis()
  def ensureCommandRunnerSurface(state: AppState): AppState = operations.ensureCommandRunnerSurface(state)
  def applyAnimationHooks(previousState: AppState): IO[Unit] =
    events.applyAnimationHooks(previousState)
  def advanceSurfaceAnimations(state: AppState): AppState = events.advanceSurfaceAnimations(state)
  def interpretEffect(effect: com.serenity.state.reducers.AppEffect): IO[Unit] =
    effects.interpretEffect(effect) >> drainPendingOperations
  def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
    effects.interpretCommand(command, state) >> drainPendingOperations
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

  val sessionStartupInfo: SessionStartupInfo = SessionStartupInfo(
    currentSessionThemeName = sessionManager.currentSessionThemeName,
    sessionExists = sessionManager.sessionExists
  )

  def clearSession(): IO[Unit] =
    sessionManager.clearSession()

  def awaitQuit: IO[Unit] = quitSignal.get

  def forceQuit(): IO[Unit] =
    cancelProjectTask() >> operations.cancelDocumentAnalysis() >> stateRef.get.flatMap { state =>
      sessionPersistence
        .onAppClose(clearCloseActions(state))
        .handleErrorWith(error => logger.error(error)("[SESSION] Failed to save session during forced quit")) >>
        quitSignal.complete(()).attempt.void
    }

  private def cancelProjectTask(): IO[Unit] =
    ProjectTaskOwnership.cancel(projectTaskFiberRef, projectTaskSemaphore).void

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
