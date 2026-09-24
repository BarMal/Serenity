package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.*
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability
import org.typelevel.log4cats.noop.NoOpLogger

/** Shared harness for [[StateManagerEffectHandlers]] specs: wires a `StateManagerEffectHandlers` whose capability ports
  * (runtime, editor, surfaces, files, sessions, modal-workflow) are all recording doubles, so
  * `StateManagerEffectHandlersSpec` and `StateManagerExternalChangeEffectHandlersSpec` can each check "which
  * collaborator fired, with what" or "what landed in state" without duplicating the wiring.
  */
private[manager] trait StateManagerEffectHandlersHarness:

  given Balance = Balance.default

  protected val bufferId: BufferId = BufferId(0)

  final protected class RecordingSessionPersistence(triggers: Ref[IO, List[SessionSaveTrigger]], root: Path)
      extends SessionPersistence(
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
      triggers.update(_ :+ trigger)

  final protected class Harness(
      val stateRef: Ref[IO, AppState],
      val committedStates: Ref[IO, List[AppState]],
      val events: Ref[IO, List[Event]],
      val calls: Ref[IO, List[String]],
      val fontConfigs: Ref[IO, List[com.serenity.ui.fonts.FontLoader.FontConfig]],
      val sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
      val quitSignal: Deferred[IO, Unit],
      val lspQueue: LspEffectQueue,
      val handlers: StateManagerEffectHandlers
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()

  protected def harness(
    initialState: AppState = AppState.initial,
    fileDialogOpt: Option[FileDialog] = None,
    saveExistingBufferHook: BufferId => IO[Unit] = _ => IO.unit,
    loadSessionResult: IO[Option[AppState]] = IO.pure(None)
  ): Harness =
    val modelRefVar             = Ref.of[IO, Model](Model(initialState, UndoState(), Map.empty)).unsafeRunSync()
    val stateRefVar             = Model.appRef(modelRefVar)
    val committedVar            = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val eventsVar               = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val callsVar                = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val fontConfigsVar          = Ref.of[IO, List[com.serenity.ui.fonts.FontLoader.FontConfig]](Nil).unsafeRunSync()
    val sessionRoot             = Files.createTempDirectory("effect-handlers-spec")
    val sessionTriggersVar      = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    val themeNamesRefVar        = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val bufferAnimationsRefVar  = Model.bufferAnimationsRef(modelRefVar)
    val quitSignalVar           = Deferred[IO, Unit].unsafeRunSync()
    val lspQueueVar             = LspEffectQueue.create.unsafeRunSync()
    val projectTaskFiberRefVar  = Ref.of[IO, Option[ManagedProjectTask]](None).unsafeRunSync()
    val projectTaskSemaphoreVar = Semaphore[IO](1).unsafeRunSync()

    val runtime = new EffectRuntimePort:
      val stateRef             = stateRefVar
      val themeNamesRef        = themeNamesRefVar
      val quitSignal           = quitSignalVar
      val logger               = NoOpLogger.impl[IO]
      val themeManager         = AppThemeManager.create
      val lspQueue             = lspQueueVar
      val projectTaskFiberRef  = projectTaskFiberRefVar
      val projectTaskSemaphore = projectTaskSemaphoreVar
      val onFontConfigChanged =
        (config: com.serenity.ui.fonts.FontLoader.FontConfig) => fontConfigsVar.update(_ :+ config)
      val deviceTextScaleProvider = IO.pure(1.0)
      val configPersistencePath   = None
      val uiPresetStore           = UiPresetStore(sessionRoot.resolve("ui-presets.json"))
      val windowSizeProvider      = IO.pure(None)
      val bufferAnimationsRef     = bufferAnimationsRefVar
      val markdownPreviewWindow   = MarkdownPreviewWindowAvailability.Unavailable

    val editor = new EffectEditorPort:
      def updateState(update: AppState => AppState): IO[Unit] = stateRefVar.update(update)
      def enqueueEvent(event: Event): IO[Unit]                = eventsVar.update(_ :+ event)
      def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
        committedVar.update(_ :+ newState) >> stateRefVar.set(newState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        modelRefVar.get.flatMap(model =>
          transition(model).fold(IO.unit)(next => committedVar.update(_ :+ next.app) >> modelRefVar.set(next))
        )
      def scheduleDocumentAnalysis(): IO[Unit] = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit] =
        callsVar.update(_ :+ s"scheduleFindSearch:$request")
      def submitEffect(lane: com.serenity.state.effects.Lane.Keyed, job: IO[Unit]): IO[Unit] = job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        stateRefVar.get.flatMap { current =>
          val next = EffectResult.applyIfCurrent(current, result)
          if next eq current then IO.unit else stateRefVar.set(next) >> onApplied(next)
        }

    val surfaces = new EffectSurfacePort:
      def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] = callsVar.update(_ :+ s"showPeek:$content")
      def showModal(modal: Modal): IO[Unit]                            = callsVar.update(_ :+ s"showModal:$modal")
      def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
        callsVar.update(_ :+ s"pinPanel:$content:$position:$size")
      def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
        callsVar.update(_ :+ s"pinOrUpdateTerminalPanel:$text")
      def unpinPanel(target: PanelTarget): IO[Unit]        = callsVar.update(_ :+ s"unpinPanel:$target")
      def expandPinnedPanel(target: PanelTarget): IO[Unit] = callsVar.update(_ :+ s"expandPinnedPanel:$target")
      def collapseExpandedPanel(): IO[Unit]                = callsVar.update(_ :+ "collapseExpandedPanel")
      def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
        callsVar.update(_ :+ s"switchToPinnedPanel:$target")
      def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
        callsVar.update(_ :+ s"resizePinnedPanel:$target:$newSize")

    val sessionPersistenceVar = new RecordingSessionPersistence(sessionTriggersVar, sessionRoot)

    // Real file loading, with every lane job and posted result run inline on the caller.
    val inlineLanes = new FileEffectLanes:
      val fileWrites                                                  = FileWriteLedger.create.unsafeRunSync()
      def submitToLane(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] = job
      def post(update: IO[Unit]): IO[Unit]                            = update
      def dispatchUpdate(update: IO[Unit]): IO[Unit]                  = update
      def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
        editor.validateAndUpdateState(newState, fallbackState)
    val filePersistence = new StateManagerFilePersistence(
      stateRefVar,
      new FileManager(),
      sessionPersistenceVar,
      NoOpLogger.impl[IO],
      lspQueueVar,
      inlineLanes
    )

    val files = new EffectFilePort:
      val fileDialog  = fileDialogOpt
      val fileManager = new FileManager()
      def submitSave(id: BufferId, onFailure: Throwable => IO[Unit]): IO[Unit] =
        callsVar.update(_ :+ s"saveExistingBuffer:$id") >> saveExistingBufferHook(id).handleErrorWith(onFailure)
      def saveBufferAs(id: BufferId, path: Path): IO[Unit] = callsVar.update(_ :+ s"saveBufferAs:$id:$path")
      def reloadBuffer(id: BufferId): IO[Unit]             = callsVar.update(_ :+ s"reloadBuffer:$id")
      def loadFile(path: Path): IO[Unit]                   = filePersistence.loadFile(path)
      def openFromDialog(dialog: FileDialog): IO[Unit]     = filePersistence.openFromDialog(dialog)
      def isSaving(path: Path): IO[Boolean]                = filePersistence.isSaving(path)

    val sessions = new EffectSessionPort:
      val sessionPersistence                  = sessionPersistenceVar
      def saveSession(): IO[Unit]             = callsVar.update(_ :+ "saveSession")
      def loadSession(): IO[Option[AppState]] = callsVar.update(_ :+ "loadSession") >> loadSessionResult
      def clearSession(): IO[Unit]            = callsVar.update(_ :+ "clearSession")

    val workflow = new EffectModalWorkflowPort:
      def clearCloseActions(state: AppState): AppState = state
      def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
        callsVar.update(_ :+ s"beginCloseAction:$scope")
      def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit] =
        callsVar.update(_ :+ s"showSaveAsWorkflow:$bufferId:$statusMessage")
      def openFileWorkflowModal(mode: FileWorkflowMode, state: AppState): IO[Unit] =
        callsVar.update(_ :+ s"openFileWorkflowModal:$mode")
      def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit] =
        callsVar.update(_ :+ s"requestSaveAsFileDialog:$bufferIdOverride")
      def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"refreshFileWorkflowEffect:$surfaceId")
      def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitFileWorkflowEffect:$surfaceId")
      def openFileWorkflowAsProjectRootEffect(surfaceId: SurfaceId, openProjectRoot: Path => IO[Unit]): IO[Unit] =
        callsVar.update(_ :+ s"openFileWorkflowAsProjectRootEffect:$surfaceId")
      def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitReplaceWorkflowEffect:$surfaceId")
      def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitCloseWorkflowEffect:$surfaceId")
      def openReloadConflictModal(state: AppState, bufferId: BufferId, bufferLabel: String): IO[Unit] =
        callsVar.update(_ :+ s"openReloadConflictModal:$bufferId:$bufferLabel")
      def submitReloadConflictEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitReloadConflictEffect:$surfaceId")
      def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"createFileWorkflowDirectoriesEffect:$surfaceId")
      def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState =
        restoredState
      def createStartupSession(): IO[Unit]                        = callsVar.update(_ :+ "createStartupSession")
      def restoreStartupSession(): IO[Unit]                       = callsVar.update(_ :+ "restoreStartupSession")
      def activeEditorBufferId(state: AppState): Option[BufferId] = state.focusedBufferId
      def openSaveSessionAsPrompt(state: AppState): IO[Unit]      = callsVar.update(_ :+ "openSaveSessionAsPrompt")
      def openSessionPicker(state: AppState, purpose: SessionListPurpose): IO[Unit] =
        callsVar.update(_ :+ s"openSessionPicker:$purpose")
      def submitSessionNamePromptEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitSessionNamePromptEffect:$surfaceId")
      def submitSessionListEffect(surfaceId: SurfaceId): IO[Unit] =
        callsVar.update(_ :+ s"submitSessionListEffect:$surfaceId")

    new Harness(
      stateRefVar,
      committedVar,
      eventsVar,
      callsVar,
      fontConfigsVar,
      sessionTriggersVar,
      quitSignalVar,
      lspQueueVar,
      new StateManagerEffectHandlers(runtime, editor, surfaces, files, sessions, workflow)
    )

  protected def command(intent: CommandIntent, category: CommandCategory = CommandCategory.Edit): Command =
    Command.typed("test-command", "A test command.", intent, category)
