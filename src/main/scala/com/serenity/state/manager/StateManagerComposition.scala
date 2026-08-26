package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.foldable.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.PreferredWindowSize
import com.serenity.diagnostics.Trace
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{Direction, Event}
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.state.reducers.{CommandRunnerPanelSelections, ModalEventReducer}
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PeekContent, WorkspaceNodeId}
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
  def lspQueue: LspEffectQueue
  def projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]]
  def projectTaskSemaphore: Semaphore[IO]
  def onFontConfigChanged: FontConfig => IO[Unit]
  def deviceTextScaleProvider: IO[Double]
  def configPersistencePath: Option[Path]
  def uiPresetStore: UiPresetStore
  def windowSizeProvider: IO[Option[PreferredWindowSize]]
  def bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]
  def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

/** Editor capability calls used by command effects. */
private[manager] trait EffectEditorPort:
  def updateState(update: AppState => AppState): IO[Unit]
  def enqueueEvent(event: Event): IO[Unit]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def scheduleDocumentAnalysis(): IO[Unit]
  def scheduleFindSearch(request: FindSearchRequest): IO[Unit]

/** Operations emitted by capabilities for ordered interpretation at the event boundary. */
private[manager] enum StateManagerOperation:
  case Event(event: com.serenity.keystroke.events.Event)
  case ApplyAnimationHooks(previousState: AppState)

/** One-directional hand-off for operations emitted while interpreting effects. */
final private[manager] class StateManagerOperationBoundary private (
    pendingOperations: Ref[IO, List[StateManagerOperation]],
    stateRef: Ref[IO, AppState],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    documentAnalysisInputsRef: Ref[IO, Option[Map[String, SpellCheckFingerprint]]],
    findSearchFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    markdownPreviewCommitFibersRef: Ref[IO, Map[BufferId, Fiber[IO, Throwable, Unit]]],
    logger: Logger[IO],
    analysisLifecycleLock: Semaphore[IO],
    documentAnalysisShutdownRef: Ref[IO, Boolean],
    beforeDocumentAnalysisStart: IO[Unit],
    beforeDocumentAnalysisShutdown: IO[Unit]
):
  private val DocumentAnalysisDebounce      = 150.millis
  private val FindSearchDebounce            = 50.millis
  private val MarkdownPreviewCommitDebounce = 150.millis

  def enqueueEvent(event: Event): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.Event(event))

  def enqueueAnimationHooks(previousState: AppState): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.ApplyAnimationHooks(previousState))

  def takeOperations: IO[List[StateManagerOperation]] = pendingOperations.getAndSet(Nil)

  def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry        = CommandRegistry.default
    val activatedRunner = CommandRunner.empty.activate(registry, state.config)
    val runner = activatedRunner.copy(
      optionSelections = activatedRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
    )
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(runner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId
      .copy(uiSurfaces = stateWithId.uiSurfaces.filterNot(_.id == surfaceId) :+ surface)
      .pushFocus(Focus.Surface(surfaceId))

  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    normalizeCommandRunnerFocus(newState).validated match
      case Right(validState) =>
        val modalTransitionLog =
          (fallbackState.modalSurface, validState.modalSurface) match
            case (before, after) if before != after =>
              logger.info(
                s"[STATE MODAL] before=${before.map(_.id).getOrElse("none")} " +
                  s"after=${after.map(_.id).getOrElse("none")} focus=${validState.focus}"
              )
            case _ => IO.unit
        modalTransitionLog >> stateRef.set(validState) >> scheduleDocumentAnalysis()
      case Left(errors) =>
        logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
          stateRef.set(fallbackState)

  def scheduleDocumentAnalysis(): IO[Unit] =
    stateRef.get.flatMap { state =>
      IO.blocking(SpellChecker.analysisFingerprints(state)).flatMap { inputs =>
        documentAnalysisInputsRef.modify(previous => Some(inputs) -> previous.forall(_ != inputs)).flatMap {
          inputsChanged =>
            if !inputsChanged || !requiresDocumentAnalysis(state) then IO.unit
            else
              analysisLifecycleLock.permit.use { _ =>
                documentAnalysisShutdownRef.get.ifM(
                  IO.unit,
                  for
                    previous <- documentAnalysisFiberRef.getAndSet(None)
                    _        <- previous.traverse_(_.cancel)
                    _        <- beforeDocumentAnalysisStart
                    fiber    <- documentAnalysisJob.start
                    _        <- documentAnalysisFiberRef.set(Some(fiber))
                  yield ()
                )
              }
        }
      }
    }

  def cancelDocumentAnalysis(): IO[Unit] =
    beforeDocumentAnalysisShutdown >> analysisLifecycleLock.permit.use { _ =>
      documentAnalysisShutdownRef.set(true) >>
        documentAnalysisFiberRef.getAndSet(None).flatMap(_.traverse_(_.cancel))
    }

  def scheduleFindSearch(request: FindSearchRequest): IO[Unit] =
    findSearchFiberRef.getAndSet(None).flatMap(_.traverse_(_.cancel)) >>
      (IO.sleep(FindSearchDebounce) >>
        IO.blocking(FindSearch.results(request.content, request.query)).flatMap { results =>
          stateRef.update { before =>
            val after = ModalEventReducer.applyFindSearchResults(before, request, results)
            CursorViewport.ensureVisibleCursors(before, after)
          }
        }).start.flatMap(fiber => findSearchFiberRef.set(Some(fiber)))

  /** Cancels any pending markdown-preview commit for `bufferId` and schedules a new one that, after
    * `MarkdownPreviewCommitDebounce` of no further supersession, records `generation` as this buffer's committed
    * markdown-preview generation. The renderer compares this against `Buffer.markdownPreviewEditGeneration` to decide
    * whether an edit burst is still in flight -- see `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  def scheduleMarkdownPreviewCommit(bufferId: BufferId, generation: Long): IO[Unit] =
    markdownPreviewCommitFibersRef.modify(fibers => (fibers - bufferId, fibers.get(bufferId))).flatMap { prior =>
      prior.traverse_(_.cancel) >>
        (IO.sleep(MarkdownPreviewCommitDebounce) >>
          stateRef.update { state =>
            state.buffers.get(bufferId).fold(state) { buffer =>
              state.copy(buffers =
                state.buffers.updated(bufferId, buffer.copy(markdownPreviewCommittedGeneration = generation))
              )
            }
          }).start.flatMap(fiber => markdownPreviewCommitFibersRef.update(_ + (bufferId -> fiber)))
    }

  private def documentAnalysisJob: IO[Unit] =
    given Logger[IO] = logger
    (IO.sleep(DocumentAnalysisDebounce) >>
      Trace.timed("analysis.documentAnalysisJob") {
        stateRef.get.flatMap { snapshot =>
          val expected = SpellChecker.analysisFingerprints(snapshot)
          IO.blocking(SpellChecker.refreshDiagnostics(snapshot))
            .flatMap(analyzed => stateRef.update(current => SpellChecker.applyIfCurrent(current, analyzed, expected)))
        }
      }).handleErrorWith(error =>
      documentAnalysisInputsRef.set(None) >> logger.error(error)("[ANALYSIS] Document analysis refresh failed")
    )

  private def requiresDocumentAnalysis(state: AppState): Boolean =
    state.config.spellCheck.enabled || state.spellCheckCache.nonEmpty

  private def normalizeCommandRunnerFocus(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus.fold(state)(focus => state.copy(focus = focus))
    else state

private[manager] object StateManagerOperationBoundary:

  def create(
    stateRef: Ref[IO, AppState],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    logger: Logger[IO],
    beforeDocumentAnalysisStart: IO[Unit] = IO.unit,
    beforeDocumentAnalysisShutdown: IO[Unit] = IO.unit
  ): IO[StateManagerOperationBoundary] =
    for
      pendingOperations              <- Ref.of[IO, List[StateManagerOperation]](Nil)
      analysisLifecycleLock          <- Semaphore[IO](1)
      documentAnalysisShutdownRef    <- Ref.of[IO, Boolean](false)
      documentAnalysisInputsRef      <- Ref.of[IO, Option[Map[String, SpellCheckFingerprint]]](None)
      findSearchFiberRef             <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      markdownPreviewCommitFibersRef <- Ref.of[IO, Map[BufferId, Fiber[IO, Throwable, Unit]]](Map.empty)
    yield new StateManagerOperationBoundary(
      pendingOperations,
      stateRef,
      documentAnalysisFiberRef,
      documentAnalysisInputsRef,
      findSearchFiberRef,
      markdownPreviewCommitFibersRef,
      logger,
      analysisLifecycleLock,
      documentAnalysisShutdownRef,
      beforeDocumentAnalysisStart,
      beforeDocumentAnalysisShutdown
    )

/** Owns file persistence state updates shared by command effects and file workflows. */
final private[manager] class StateManagerFilePersistence(
    stateRef: Ref[IO, AppState],
    fileManager: FileManager,
    sessionPersistence: SessionPersistence,
    logger: Logger[IO],
    lspQueue: LspEffectQueue
):

  def saveExistingBuffer(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.buffers.get(bufferId).flatMap(_.filePath) match
        case Some(_) =>
          state.buffers.get(bufferId).fold(IO.unit) { buffer =>
            fileManager
              .saveBuffer(buffer)
              .flatMap(saved =>
                stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> saved)))
              )
              .flatTap(_ => persistAfterSave)
          }
        case None => IO.unit
    }

  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.buffers.get(bufferId).fold(IO.unit) { buffer =>
        fileManager
          .saveBuffer(buffer, path)
          .flatMap { saved =>
            stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> saved))) >>
              refreshLspBindingAfterSaveAs(buffer, saved)
          }
          .flatTap(_ =>
            stateRef.update(current => current.copy(recentFiles = trackRecentFile(current.recentFiles, path)))
          )
          .flatTap(_ => persistAfterSave)
      }
    }

  private def refreshLspBindingAfterSaveAs(before: Buffer, saved: Buffer): IO[Unit] =
    val previous = for
      path       <- before.filePath
      languageId <- before.language
    yield (path.toUri.toString, languageId)
    val next = for
      path       <- saved.filePath
      languageId <- saved.language
    yield (path.toUri.toString, languageId, saved.content.collect())
    val nextIdentity = next.map { case (uri, languageId, _) => (uri, languageId) }
    if previous == nextIdentity then IO.unit
    else
      previous.fold(IO.unit) {
        case (uri, languageId) =>
          lspQueue.enqueue(LspEffect.FileClosed(uri, languageId))
      } >>
        next.fold(IO.unit) {
          case (uri, languageId, text) =>
            lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
        }

  private def persistAfterSave: IO[Unit] =
    stateRef.get
      .flatMap(sessionPersistence.onBufferChange)
      .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after file save failed"))

  private def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

/** Surface capability calls used by command effects. */
private[manager] trait EffectSurfacePort:
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]
  def expandPinnedPanel(position: PanelPosition): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]

/** File infrastructure used by command effects. */
private[manager] trait EffectFilePort:
  def fileDialog: Option[com.serenity.io.FileDialog]
  def fileManager: FileManager
  def saveExistingBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit]

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
  def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit]
  def openFileWorkflowModal(mode: FileWorkflowMode, state: AppState): IO[Unit]
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
  def lspQueue: LspEffectQueue
  def bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]
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
  def fileDialog: Option[com.serenity.io.FileDialog]
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

/** State and analysis ownership required while routing editor events. */
private[manager] trait EventStatePort:
  def stateRef: Ref[IO, AppState]
  def undoRef: Ref[IO, UndoState]
  def logger: Logger[IO]
  def documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]]
  def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]
  def bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]

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

  private val effectEditorPort: EffectEditorPort = new EffectEditorPort:
    def updateState(update: AppState => AppState): IO[Unit] = runtimeStateRef.update(update)
    def enqueueEvent(event: Event): IO[Unit]                = operations.enqueueEvent(event)
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      operations.validateAndUpdateState(newState, fallbackState)
    def scheduleDocumentAnalysis(): IO[Unit]                     = operations.scheduleDocumentAnalysis()
    def scheduleFindSearch(request: FindSearchRequest): IO[Unit] = operations.scheduleFindSearch(request)

  private val effectSurfacePort: EffectSurfacePort = new EffectSurfacePort:
    def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] = surfaces.showPeek(content, at)
    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      surfaces.pinPanel(content, position, size)
    def unpinPanel(position: PanelPosition): IO[Unit]          = surfaces.unpinPanel(position)
    def expandPinnedPanel(position: PanelPosition): IO[Unit]   = surfaces.expandPinnedPanel(position)
    def collapseExpandedPanel(): IO[Unit]                      = surfaces.collapseExpandedPanel()
    def switchToPinnedPanel(position: PanelPosition): IO[Unit] = surfaces.switchToPinnedPanel(position)
    def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
      surfaces.resizePinnedPanel(position, newSize)

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
    def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
      workflow.restoreSessionIntoCurrentViewport(restoredState, currentState)
    def createStartupSession(): IO[Unit]                        = workflow.createStartupSession()
    def restoreStartupSession(): IO[Unit]                       = workflow.restoreStartupSession()
    def activeEditorBufferId(state: AppState): Option[BufferId] = workflow.activeEditorBufferId(state)

  private val filePort: FileCapabilityPort = new FileCapabilityPort:
    def closeBuffer(bufferId: BufferId): IO[Unit]      = editor.closeBuffer(bufferId)
    def directLoadFileEffect(path: Path): IO[Unit]     = effects.directLoadFileEffect(path)
    def saveBufferEffect(bufferId: BufferId): IO[Unit] = effects.saveBufferEffect(bufferId)
    def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
      effects.saveBufferAsEffect(bufferId, path)

  private val editorPort: EditorCapabilityPort = new EditorCapabilityPort:
    val stateRef            = runtimeStateRef
    val lspQueue            = runtimeLspQueue
    val bufferAnimationsRef = runtimeBufferAnimationsRef
    def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
      editor.createBuffer(content, filePath)
    def createNewEmptyBuffer(): IO[BufferId]               = editor.createNewEmptyBuffer()
    def closeBuffer(bufferId: BufferId): IO[Unit]          = editor.closeBuffer(bufferId)
    def createPane(bufferId: Option[BufferId]): IO[PaneId] = editor.createPane(bufferId)
    def switchToPane(paneId: PaneId): IO[Unit]             = editor.switchToPane(paneId)
    def ensureCommandRunnerSurface(state: AppState): AppState =
      operations.ensureCommandRunnerSurface(state)
    def advanceSurfaceAnimations(state: AppState): AppState = events.advanceSurfaceAnimations(state)

  private val workflowPort: WorkflowCapabilityPort = new WorkflowCapabilityPort:
    val stateRef                                            = runtimeStateRef
    val undoRef                                             = runtimeUndoRef
    val quitSignal                                          = runtimeQuitSignal
    val logger                                              = runtimeLogger
    val fileDialog                                          = runtimeFileDialog
    val fileManager                                         = runtimeFileManager
    val sessionPersistence                                  = runtimeSessionPersistence
    def updateState(update: AppState => AppState): IO[Unit] = runtimeStateRef.update(update)
    def createNewEmptyBuffer(): IO[BufferId]                = editor.createNewEmptyBuffer()
    def createPane(bufferId: Option[BufferId]): IO[PaneId]  = editor.createPane(bufferId)
    def switchToPane(paneId: PaneId): IO[Unit]              = editor.switchToPane(paneId)
    def loadSession(): IO[Option[AppState]]                 = sessionManager.loadSession()
    def ensureCommandRunnerSurface(state: AppState): AppState =
      operations.ensureCommandRunnerSurface(state)
    def saveBufferEffect(bufferId: BufferId): IO[Unit] =
      filePersistence.saveExistingBuffer(bufferId).handleErrorWith {
        case error: com.serenity.richtext.LossyRichTextOverwriteException =>
          runtimeStateRef.get.flatMap(current => workflow.showSaveAsWorkflow(current, bufferId, error.getMessage))
        case error =>
          runtimeLogger.error(error)(s"[FILE] Failed to save buffer $bufferId")
      }
    def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] = filePersistence.saveBufferAs(bufferId, path)

  private val surfacePort: SurfaceCapabilityPort = new SurfaceCapabilityPort:
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      operations.validateAndUpdateState(newState, fallbackState)
    def applyAnimationHooks(previousState: AppState): IO[Unit] =
      operations.enqueueAnimationHooks(previousState)

  private val viewportPort: ViewportCapabilityPort = new ViewportCapabilityPort:
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      events.validateAndUpdateState(newState, fallbackState)
    def updateFontConfig(update: FontConfig => FontConfig): IO[Unit] =
      effects.updateFontConfig(update)

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
        editor.createBuffer(content, filePath)
      def createPane(bufferId: Option[BufferId]): IO[PaneId] = editor.createPane(bufferId)

  private val eventUiPort: EventUiPort =
    new EventUiPort:
      val uiPresetStore = runtimeUiPresetStore
      def updateConfig(
        update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
      ): IO[com.serenity.config.AppConfig] =
        effects.updateConfig(update)
      def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
        surfaces.resizePinnedPanel(position, newSize)

  private val workflow = new StateManagerWorkflowCapability(workflowPort)

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
  private val editor   = new StateManagerEditorCapability(editorPort)
  private val surfaces = new StateManagerSurfaceCapability(stateRef, logger, surfacePort)
  private val viewport =
    new StateManagerViewportCapability(stateRef, logger, deviceTextScaleProvider, viewportPort)
  private val files = new StateManagerFileCapability(stateRef, filePort)

  export editor.{focusPaneInDirection as _, resizePaneSplit as _, *}
  export events.applyEvent
  export files.*
  export viewport.*

  def resizePaneSplit(splitId: WorkspaceNodeId, ratio: Double): IO[Unit] =
    editor.resizePaneSplit(splitId, ratio)

  def focusPaneInDirection(direction: Direction): IO[Unit] =
    editor.focusPaneInDirection(direction)

  def lspEffectStream: Stream[IO, LspEffect] =
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
  def unpinPanel(surfaceId: SurfaceId): IO[Unit]    = runSurfaceOperation(surfaces.unpinPanel(surfaceId))
  def unpinPanel(position: PanelPosition): IO[Unit] = runSurfaceOperation(surfaces.unpinPanel(position))
  def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit] =
    runSurfaceOperation(surfaces.movePinnedPanel(surfaceId, position))
  def expandPinnedPanel(surfaceId: SurfaceId): IO[Unit] =
    runSurfaceOperation(surfaces.expandPinnedPanel(surfaceId))
  def expandPinnedPanel(position: PanelPosition): IO[Unit] = runSurfaceOperation(surfaces.expandPinnedPanel(position))
  def collapseExpandedPanel(): IO[Unit]                    = runSurfaceOperation(surfaces.collapseExpandedPanel())
  def showModal(modal: Modal): IO[Unit]                    = runSurfaceOperation(surfaces.showModal(modal))
  def dismissModal(): IO[Unit]                             = runSurfaceOperation(surfaces.dismissModal())
  def switchToPinnedPanel(position: PanelPosition): IO[Unit] =
    runSurfaceOperation(surfaces.switchToPinnedPanel(position))
  def switchToPinnedPanel(surfaceId: SurfaceId): IO[Unit] =
    runSurfaceOperation(surfaces.switchToPinnedPanel(surfaceId))
  def loadDirectoryTree(path: String, files: List[String]): IO[Unit] =
    runSurfaceOperation(surfaces.loadDirectoryTree(path, files))
  def selectFileInExplorer(filePath: String): IO[Unit] = runSurfaceOperation(surfaces.selectFileInExplorer(filePath))
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
    runSurfaceOperation(surfaces.resizePinnedPanel(position, newSize))
  def resizePinnedPanel(surfaceId: SurfaceId, newSize: Int): IO[Unit] =
    runSurfaceOperation(surfaces.resizePinnedPanel(surfaceId, newSize))
  def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit] =
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

  def currentSessionThemeName: IO[Option[String]] =
    sessionManager.currentSessionThemeName

  def sessionExists: IO[Boolean] =
    sessionManager.sessionExists

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
