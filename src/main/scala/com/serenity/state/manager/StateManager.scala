package com.serenity.state.manager

import java.nio.file.{Files, Path, Paths}

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import fs2.Stream
import com.serenity.animation.AnimationConfig
import com.serenity.command.{Command, CommandIntent, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.io.{FileManager, FileUtils}
import com.serenity.keystroke.events.{Event, FileEvent, GlobalAppEvent, SystemEvent, ThemeEvent}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, FileEventReducer, ModalStateReducer, PanelStateReducer, PeekStateReducer, ReducerResult, SystemEventReducer, ThemeEventReducer}
import com.serenity.ui.layout.*
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

trait StateManager:
  def applyEvent(event: Event): IO[Unit]
  def getCurrentState: IO[AppState]
  def getCurrentFocus: IO[Focus]
  def switchFocus(newFocus: Focus): IO[Unit]
  def getActiveBuffer: IO[Option[Buffer]]
  def getActivePane: IO[Option[EditorPane]]
  def awaitQuit: IO[Unit]
  def intervalSaveStream: Stream[IO, Unit]
  def updateState(update: AppState => AppState): IO[Unit]
  def handleTerminalResize(newSize: TerminalSize): IO[Unit]
  def advanceAnimationFrames(): IO[Unit]
  def advanceAnimationsOnTick(): IO[Boolean]

  // Buffer operations
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createNewEmptyBuffer(): IO[BufferId]
  def updateBuffer(bufferId: BufferId, content: String): IO[Unit]
  def closeBuffer(bufferId: BufferId): IO[Unit]

  // Pane operations
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def closePane(paneId: PaneId): IO[Unit]
  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit]
  def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit]
  def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit]
  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit]

  // Peek operations
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def dismissPeek(): IO[Unit]
  def peekToPin(position: PanelPosition): IO[Unit]

  // Session persistence operations
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def sessionExists: IO[Boolean]
  def clearSession(): IO[Unit]

  // Panel operations
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]

  // Modal operations
  def showModal(modal: Modal): IO[Unit]
  def dismissModal(): IO[Unit]

  // File operations (stubs for test compilation)
  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit]
  def saveBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit]
  def markBufferSaved(bufferId: BufferId): IO[Unit]
  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean]
  def forceCloseBuffer(bufferId: BufferId): IO[Unit]

  // Tab operations (stubs for test compilation)
  def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]
  def getTabOrder(): IO[List[PaneId]]

  // Pane splitting operations (stubs for test compilation)
  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]

  // Panel operations (stubs for test compilation)
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def loadDirectoryTree(path: String, files: List[String]): IO[Unit]
  def selectFileInExplorer(filePath: String): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]
  def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit]

  // Scrolling operations (stubs for test compilation)
  def ensureCursorVisible(paneId: PaneId): IO[Unit]
  def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit]
  def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit]
  def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit]

object StateManager:

  def apply(
      parentLogger: Logger[IO],
      policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
  )(using Balance, LoggerFactory[IO]): IO[StateManager] =
    for
      stateRef   <- Ref.of[IO, AppState](AppState.initial)
      quitSignal <- Deferred[IO, Unit]
    yield new StateManagerImpl(
      stateRef,
      quitSignal,
      LoggerFactory[IO].getLogger(using LoggerName("com.serenity.state.manager.StateManager")),
      policy
    )

  def describeCommandRunnerEvent(event: Event, runner: CommandRunner): String =
    val modePart =
      if runner.searchTerm.isEmpty then s"mode=browse category=${runner.activeCategory}"
      else s"mode=search query=${runner.searchTerm} category=${runner.activeCategory}"
    val selectedPart =
      runner.selectedItem match
        case Some(CommandSurfaceItem.CommandItem(command)) => s"selected=command:${command.name}"
        case Some(option: CommandSurfaceItem.OptionItem)   => s"selected=option:${option.id}"
        case None                                          => "selected=none"

    s"event=$event $modePart $selectedPart"

  def describeCommandExecution(command: Command): String =
    val intentName =
      command.intent match
        case CommandIntent.Custom(_) => "Custom"
        case other                   => other.toString

    s"command=${command.name} category=${command.category} intent=$intentName"

  private class StateManagerImpl(
      stateRef: Ref[IO, AppState],
      quitSignal: Deferred[IO, Unit],
      logger: Logger[IO],
      policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
  )(using Balance)
      extends StateManager:
    private val fileManager = new FileManager()
    private val themeManager = AppThemeManager.create
    private val sessionManager = SessionManager.create(themeManager, logger, policy)
    private val sessionPersistence = new SessionPersistence(sessionManager, policy, logger)

    def getCurrentState: IO[AppState] = stateRef.get

    def getCurrentFocus: IO[Focus] = stateRef.get.map(_.focus)

    def switchFocus(newFocus: Focus): IO[Unit] =
      stateRef.update(_.copy(focus = newFocus))

    def updateState(update: AppState => AppState): IO[Unit] =
      stateRef.update(update)

    def advanceAnimationFrames(): IO[Unit] =
      for
        state <- stateRef.get
        updatedBuffers = state.buffers.view.mapValues { buffer =>
          buffer.copy(animations = buffer.animations.advanceAnimations())
        }.toMap
        _ <- stateRef.set(state.copy(buffers = updatedBuffers))
      yield ()

    def advanceAnimationsOnTick(): IO[Boolean] =
      stateRef.get.flatMap { state =>
        // Check if any buffer has active animations
        val hasAnyAnimations = state.buffers.values.exists(_.animations.hasActiveAnimations)
        if !hasAnyAnimations then IO.pure(false)
        else
          // Advance animations for all buffers
          val updatedBuffers = state.buffers.view.mapValues { buffer =>
            buffer.copy(animations = buffer.animations.advanceAllAnimations())
          }.toMap

          val newState           = state.copy(buffers = updatedBuffers)
          val stillHasAnimations = newState.buffers.values.exists(_.animations.hasActiveAnimations)

          stateRef.set(newState).as(stillHasAnimations)
      }

    def getActiveBuffer: IO[Option[Buffer]] =
      for
        state      <- stateRef.get
        activePane <- getActivePane
        buffer = activePane.flatMap(pane => pane.bufferId.flatMap(state.buffers.get))
      yield buffer

    def getActivePane: IO[Option[EditorPane]] =
      stateRef.get.map(state => state.layout.activeEditorPaneId.flatMap(state.layout.editorPanes.get))

    def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId] =
      stateRef.modify { state =>
        val bufferId = state.nextBufferId
        val buffer   = Buffer.fromString(bufferId, content).copy(filePath = filePath)
        val newState = state.copy(
          buffers = state.buffers + (bufferId -> buffer),
          nextBufferId = BufferId(bufferId.value + 1)
        )
        (newState, bufferId)
      }

    def createNewEmptyBuffer(): IO[BufferId] =
      stateRef.modify { state =>
        val bufferId = state.nextBufferId
        val buffer   = Buffer.newEmpty(bufferId)
        val newState = state.copy(
          buffers = state.buffers + (bufferId -> buffer),
          nextBufferId = BufferId(bufferId.value + 1)
        )
        (newState, bufferId)
      }

    def updateBuffer(bufferId: BufferId, content: String): IO[Unit] =
      stateRef.update { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            val updatedBuffer = buffer.copy(
              content = Rope(content),
              isDirty = true
            )
            state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
          case None => state
      }

    def closeBuffer(bufferId: BufferId): IO[Unit] =
      stateRef.update { state =>
        val updatedPanes = state.layout.editorPanes.view.mapValues { pane =>
          if pane.bufferId.contains(bufferId) then pane.copy(bufferId = None)
          else pane
        }.toMap

        state.copy(
          buffers = state.buffers - bufferId,
          layout = state.layout.copy(editorPanes = updatedPanes)
        )
      }

    def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
      stateRef.modify { state =>
        val paneId = state.nextPaneId
        val pane = bufferId match
          case Some(id) => EditorPane.withBuffer(paneId, id)
          case None     => EditorPane.empty(paneId)

        val newState = state.copy(
          layout = state.layout.copy(
            editorPanes = state.layout.editorPanes + (paneId -> pane),
            activeEditorPaneId = Some(paneId)
          ),
          focus = Focus.EditorPane(paneId),
          nextPaneId = PaneId(paneId.value + 1)
        )
        (newState, paneId)
      }

    def switchToPane(paneId: PaneId): IO[Unit] =
      stateRef.update { state =>
        if state.layout.editorPanes.contains(paneId) then
          state.copy(
            layout = state.layout.copy(activeEditorPaneId = Some(paneId)),
            focus = Focus.EditorPane(paneId)
          )
        else state
      }

    def closePane(paneId: PaneId): IO[Unit] =
      stateRef.update { state =>
        val updatedPanes = state.layout.editorPanes - paneId
        val newActivePaneId =
          if state.layout.activeEditorPaneId.contains(paneId) then updatedPanes.keys.headOption
          else state.layout.activeEditorPaneId

        val updatedState = state.copy(
          layout = state.layout.copy(
            editorPanes = updatedPanes,
            activeEditorPaneId = newActivePaneId
          )
        )
        newActivePaneId match
          case Some(id) => updatedState.copy(focus = Focus.EditorPane(id))
          case None     => ensureCommandRunnerSurface(updatedState)
      }

    def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            val updatedPane = pane.copy(bufferId = Some(bufferId))
            state.copy(
              layout = state.layout.copy(
                editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          case None => state // Pane doesn't exist, no change
      }

    def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val newCursor     = CursorPosition(line, column)
                val updatedBuffer = buffer.copy(cursors = List(newCursor))
                state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
              case None => state // No buffer assigned to pane
          case None => state // Pane doesn't exist, no change
      }

    def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val updatedBuffer = buffer.copy(viewport = viewport)
                state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
              case None => state // No buffer assigned to pane
          case None => state
      }

    def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            state.copy(
              layout = state.layout.copy(
                editorPanes = state.layout.editorPanes + (paneId -> update(pane))
              )
            )
          case None => state
      }

    def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PeekStateReducer.show(content, at, state).state, state))

    def dismissPeek(): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PeekStateReducer.dismiss(state).state, state))

    def peekToPin(position: PanelPosition): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.pinPeekOverlay(position, state).state, state))

    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.pin(content, position, size, state).state, state))

    def unpinPanel(position: PanelPosition): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.unpin(position, state).state, state))

    def showModal(modal: Modal): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(ModalStateReducer.show(modal, state).state, state))

    def dismissModal(): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(ModalStateReducer.dismiss(state).state, state))

    // Session persistence operations
    def saveSession(): IO[Unit] =
      getCurrentState.flatMap { state =>
        sessionManager.saveSession(state) >> 
        logger.info("[SESSION] Session saved")
      }.void

    def loadSession(): IO[Option[AppState]] =
      sessionManager.loadSession()

    def sessionExists: IO[Boolean] =
      sessionManager.sessionExists

    def clearSession(): IO[Unit] =
      sessionManager.clearSession()

    def awaitQuit: IO[Unit] = quitSignal.get

    def intervalSaveStream: Stream[IO, Unit] =
      policy.saveInterval match
        case None => Stream.empty
        case Some(interval) =>
          Stream.fixedRate[IO](interval)
            .interruptWhen(Stream.eval(quitSignal.get).as(true))
            .evalMap(_ => stateRef.get.flatMap(sessionPersistence.maybeSaveSession(_, SessionSaveTrigger.Interval)))

    def applyEvent(event: Event): IO[Unit] =
      stateRef.get.flatMap { currentState =>
        event match
          case systemEvent: SystemEvent =>
            applyReducerResult(SystemEventReducer.reduce(systemEvent, currentState), currentState)
          case com.serenity.keystroke.events.CloseTab =>
            beginCloseAction(CloseScope.Current, currentState)
          case com.serenity.keystroke.events.Quit =>
            beginCloseAction(CloseScope.Quit, currentState)
          case appEvent: GlobalAppEvent =>
            val registry = CommandRegistry.withToggleUI
            applyReducerResult(AppEventReducer.reduce(appEvent, currentState, registry), currentState)
          case themeEvent: ThemeEvent =>
            applyReducerResult(ThemeEventReducer.reduce(themeEvent, currentState), currentState)
          case fileEvent: FileEvent =>
            applyReducerResult(FileEventReducer.reduce(fileEvent, currentState), currentState)
          case _ =>
            val logCommandRunnerEvent =
              focusedCommandRunner(currentState) match
                case Some(runner) =>
                  logger.info(s"[COMMAND-RUNNER] ${StateManager.describeCommandRunnerEvent(event, runner)}")
                case None =>
                  IO.unit

            val result =
              getTypedLocalHandlerForFocus(currentState.focus, currentState) match
                case Some(handler) =>
                  handler.processEvent(event, currentState)
                case None =>
                  val component = getLegacyComponentForFocus(currentState.focus, currentState)
                  component.processEvent(event, currentState)

            logCommandRunnerEvent >>
              applyComponentResult(result, currentState).flatMap { newState =>
                validateAndUpdateState(newState, currentState)
              }
      }

    private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      newState.validated match
        case Right(validState) =>
          val modalTransitionLog =
            (fallbackState.modalSurface, validState.modalSurface) match
              case (before, after) if before != after =>
                logger.info(
                  s"[STATE MODAL] before=${before.map(_.id).getOrElse("none")} " +
                    s"after=${after.map(_.id).getOrElse("none")} focus=${validState.focus}"
                )
              case _ =>
                IO.unit
          modalTransitionLog >> stateRef.set(validState)
        case Left(errors) =>
          // Log validation errors and keep unchanged state
          logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
            stateRef.set(fallbackState)

    private def getTypedLocalHandlerForFocus(focus: Focus, state: AppState): Option[LocalEventHandler] =
      focus match
        case Focus.EditorPane(paneId) => Some(new EditorPaneComponent(paneId))
        case Focus.Surface(surfaceId) =>
          state.surfaceById(surfaceId) match
            case Some(surface) =>
              surface.presentation match
                case SurfacePresentation.Pinned(position, _) =>
                  Some(new PinnedPanelComponent(position))
                case SurfacePresentation.Floating(_, _) =>
                  surface.content match
                    case SurfaceContent.CommandPalette(_) =>
                      val registry = CommandRegistry.withToggleUI
                      Some(new CommandRunnerComponent(registry))
                    case SurfaceContent.StartPage(_) =>
                      Some(new StartupPageComponent())
                    case SurfaceContent.ModalWorkflow(modal) =>
                      Some(new ModalComponent(modalType(modal)))
                    case _ =>
                      Some(new PeekOverlayComponent())
            case None =>
              None

    private def getLegacyComponentForFocus(focus: Focus, state: AppState): FocusedComponent =
      focus match
        case Focus.EditorPane(paneId) => new EditorPaneComponent(paneId)
        case Focus.Surface(surfaceId) =>
          state.surfaceById(surfaceId) match
            case Some(surface) =>
              surface.presentation match
                case SurfacePresentation.Pinned(position, _) => new PinnedPanelComponent(position)
                case SurfacePresentation.Floating(_, _) =>
                  surface.content match
                    case SurfaceContent.StartPage(_) => new StartupPageComponent()
                    case _                           => new PeekOverlayComponent()
            case None =>
              new PeekOverlayComponent()

    private def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
      for
        _ <- validateAndUpdateState(result.state, fallbackState)
        _ <- result.effects.traverse_(interpretEffect)
      yield ()

    private def interpretEffect(effect: AppEffect): IO[Unit] =
      effect match
        case AppEffect.CompleteQuit =>
          quitSignal.complete(()).attempt.void
        case AppEffect.ExecuteCommand(command) =>
          logger.info(s"[COMMAND] ${StateManager.describeCommandExecution(command)}") >>
            stateRef.get.flatMap(state => interpretCommand(command, state))
        case AppEffect.SwitchTheme(themeName) =>
          applyThemeByName(themeName)
        case AppEffect.ReloadTheme(themeName) =>
          reloadThemeByName(themeName)
        case AppEffect.SaveBuffer(bufferId) =>
          saveBufferEffect(bufferId)
        case AppEffect.SaveBufferAs(bufferId, path) =>
          saveBufferAsEffect(bufferId, path)
        case AppEffect.RequestOpenFile =>
          logger.debug("[FILE] Open file requested")
        case AppEffect.RefreshFileWorkflow(surfaceId) =>
          refreshFileWorkflowEffect(surfaceId)
        case AppEffect.SubmitFileWorkflow(surfaceId) =>
          submitFileWorkflowEffect(surfaceId)
        case AppEffect.SubmitReplaceWorkflow(surfaceId) =>
          submitReplaceWorkflowEffect(surfaceId)
        case AppEffect.SubmitCloseWorkflow(surfaceId) =>
          submitCloseWorkflowEffect(surfaceId)

    private def applyComponentResult(result: ComponentResult, state: AppState): IO[AppState] =
      result match
        case ComponentResult.NoChange                => IO.pure(state)
        case ComponentResult.StateChange(update)     => IO.pure(update(state))
        case ComponentResult.ReducerUpdate(result)   =>
          applyReducerResult(result, state) >> stateRef.get
        case ComponentResult.FocusTransfer(newFocus) => IO.pure(state.copy(focus = newFocus))
        case ComponentResult.Dismiss =>
          val dismissedState = dismissCurrentFocus(state)
          dismissedState.layout.activeEditorPaneId match
            case Some(paneId) =>
              // There's an existing editor pane to focus on
              IO.pure(dismissedState.copy(focus = Focus.EditorPane(paneId)))
            case None =>
              // No editor panes exist, create a default one
              for
                _        <- stateRef.set(dismissedState)
                bufferId <- createBuffer("")
                paneId   <- createPane(Some(bufferId))
                newState <- stateRef.get.map(_.copy(focus = Focus.EditorPane(paneId)))
              yield newState
        case ComponentResult.ExecuteCommand(command) =>
          for
            _            <- stateRef.set(state)
            _            <- interpretCommand(command, state)
            updatedState <- stateRef.get
          yield updatedState
        case ComponentResult.Composite(results) =>
          results.foldLeftM(state)((s, r) => applyComponentResult(r, s))


    private def dismissCurrentFocus(state: AppState): AppState =
      state.focus match
        case Focus.Surface(surfaceId) =>
          state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId))
        case _ =>
          state

    private def focusedCommandRunner(state: AppState): Option[CommandRunner] =
      state.activeSurface.flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }

    private def ensureCommandRunnerSurface(state: AppState): AppState =
      val registry = CommandRegistry.default
      val runner = CommandRunner.empty.activate(registry).withPreviousFocus(Focus.EditorPane(PaneId(0)))
      val (stateWithId, surfaceId) =
        state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
      val surface = UiSurface(
        id = surfaceId,
        content = SurfaceContent.CommandPalette(runner),
        presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
      )
      stateWithId.copy(
        uiSurfaces = stateWithId.uiSurfaces.filterNot(_.id == surfaceId) :+ surface,
        focus = Focus.Surface(surfaceId)
      )

    private def modalType(modal: Modal): ModalType =
      modal match
        case Modal.GotoLine(_)      => ModalType.GotoLine
        case Modal.Find(_, _, _)    => ModalType.Find
        case Modal.FileWorkflow(_)  => ModalType.FileWorkflow
        case Modal.ReplaceWorkflow(_) => ModalType.ReplaceWorkflow
        case Modal.CloseWorkflow(_) => ModalType.CloseWorkflow
        case Modal.Custom(name, _)  => ModalType.Custom(name)

    private def interpretCommand(command: com.serenity.command.Command, state: AppState): IO[Unit] =
      import com.serenity.command.{AnimationMode, CommandIntent}

      command.intent match
        case CommandIntent.Custom(run) =>
          run(state)
        case CommandIntent.ToggleLineNumbers =>
          updateState(s => s.copy(config = s.config.copy(showLineNumbers = !s.config.showLineNumbers)))
        case CommandIntent.ToggleGutter =>
          updateState(s => s.copy(config = s.config.copy(showGutter = !s.config.showGutter)))
        case CommandIntent.SaveCurrentFile =>
          state.focusedBufferId match
            case Some(bufferId) => saveBufferEffect(bufferId)
            case None           => logger.debug("[CMD] No focused buffer to save")
        case CommandIntent.SaveCurrentFileAs =>
          openFileWorkflowModal(FileWorkflowMode.SaveAs, state)
        case CommandIntent.OpenFile =>
          openFileWorkflowModal(FileWorkflowMode.Open, state)
        case CommandIntent.QuitApp =>
          beginCloseAction(CloseScope.Quit, state)
        case CommandIntent.CloseAll =>
          beginCloseAction(CloseScope.All, state)
        case CommandIntent.CloseOthers =>
          beginCloseAction(CloseScope.Others, state)
        case CommandIntent.NewFile =>
          val registry = CommandRegistry.withToggleUI
          updateState(current => AppEventReducer.reduce(com.serenity.keystroke.events.NewTab, current, registry).state)
        case CommandIntent.CloseCurrentFile =>
          beginCloseAction(CloseScope.Current, state)
        case CommandIntent.FindInCurrentFile =>
          updateState(current => ModalStateReducer.show(Modal.Find("", Nil, 0), current).state)
        case CommandIntent.ReplaceInCurrentFile =>
          updateState(current => ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), current).state)
        case CommandIntent.OpenGotoLine =>
          updateState(current => ModalStateReducer.show(Modal.GotoLine(""), current).state)
        case CommandIntent.ToggleTheme =>
          toggleThemeEffect(state)
        case CommandIntent.ReloadTheme =>
          reloadThemeEffect(state)
        case CommandIntent.FormatCurrentFile =>
          logger.debug("[CMD] Format command requested")
        case CommandIntent.SetAnimationMode(mode) =>
          updateState { s =>
            mode match
              case AnimationMode.None =>
                s.copy(config = s.config.withoutCharacterAnimation)
              case AnimationMode.Quick =>
                s.copy(config = s.config.copy(characterAnimation = AnimationConfig.quick))
              case AnimationMode.Smooth =>
                s.copy(config = s.config.copy(characterAnimation = AnimationConfig.smooth))
              case AnimationMode.Subtle =>
                s.copy(config = s.config.copy(characterAnimation = AnimationConfig.subtle))
          }
        case CommandIntent.StartupNewSession =>
          updateState(_.copy(uiSurfaces = List.empty)) >>
          createNewEmptyBuffer().flatMap { bufferId =>
            updateState(s => s.copy(bufferOrder = s.bufferOrder :+ bufferId)) >>
            createPane(Some(bufferId)).flatMap(paneId =>
              switchToPane(paneId)
            )
          }
        case CommandIntent.StartupRestoreSession =>
          logger.info("[CMD] Session restore requested") >>
          loadSession().flatMap {
            case Some(restoredState) =>
              logger.info("[CMD] Session loaded successfully") >>
              updateState(_ => restoredState.copy(uiSurfaces = List.empty)) // Clear startup page
            case None =>
              logger.info("[CMD] No session found - creating default session") >>
              updateState(_.copy(uiSurfaces = List.empty)) >>
              createNewEmptyBuffer().flatMap { bufferId =>
                updateState(s => s.copy(bufferOrder = s.bufferOrder :+ bufferId)) >>
                createPane(Some(bufferId)).flatMap(paneId =>
                  switchToPane(paneId)
                )
              }
          }
        case CommandIntent.StartupOpenFile =>
          openFileWorkflowModal(FileWorkflowMode.Open, state)

    // File operations
    def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit] =
      stateRef.update { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            state.copy(buffers = state.buffers + (bufferId -> buffer.copy(filePath = Some(Path.of(filePath)))))
          case None =>
            state
      }

    def saveBuffer(bufferId: BufferId): IO[Unit] =
      saveBufferEffect(bufferId)

    def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit] =
      saveBufferAsEffect(bufferId, Path.of(filePath))

    def markBufferSaved(bufferId: BufferId): IO[Unit] =
      stateRef.update { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            state.copy(buffers = state.buffers + (bufferId -> buffer.copy(isDirty = false)))
          case None =>
            state
      }

    def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean] =
      stateRef.get.map { state =>
        bufferId match
          case Some(id) => state.buffers.get(id).exists(_.isDirty)
          case None     => state.buffers.values.exists(_.isDirty)
      }

    def forceCloseBuffer(bufferId: BufferId): IO[Unit] =
      closeBuffer(bufferId) // Reuse existing implementation for now

    // Tab operation stubs (TODO: implement)
    def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
      createPane(bufferId) // Fallback to regular createPane for now

    def getTabOrder(): IO[List[PaneId]] =
      stateRef.get.map(_.layout.editorPanes.keys.toList.sortBy(_.value))

    // Pane splitting operation stubs (TODO: implement)
    def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
      createPane(bufferId) // Fallback to regular createPane for now

    // Panel operation stubs (TODO: implement)
    def switchToPinnedPanel(position: PanelPosition): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.focus(position, state).state, state))

    def loadDirectoryTree(path: String, files: List[String]): IO[Unit] =
      logger.debug(s"TODO: loadDirectoryTree($path, ${files.size} files)")

    def selectFileInExplorer(filePath: String): IO[Unit] =
      logger.debug(s"TODO: selectFileInExplorer($filePath)")

    def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
      logger.debug(s"TODO: resizePinnedPanel($position, $newSize)")

    def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit] =
      logger.debug(s"TODO: dragFileToDirectory($sourceFile, $targetDir)")

    def ensureCursorVisible(paneId: PaneId): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val cursor   = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
                val viewport = buffer.viewport
                val newLeftColumn =
                  if cursor.column < viewport.leftColumn then cursor.column
                  else if cursor.column >= viewport.leftColumn + viewport.visibleColumns then
                    cursor.column - viewport.visibleColumns + 1
                  else viewport.leftColumn
                val newTopLine =
                  if cursor.line < viewport.topLine then cursor.line
                  else if cursor.line >= viewport.topLine + viewport.visibleLines then
                    cursor.line - viewport.visibleLines + 1
                  else viewport.topLine
                val newViewport = viewport.copy(
                  topLine = math.max(0, newTopLine),
                  leftColumn = math.max(0, newLeftColumn)
                )
                val updatedBuffer = buffer.copy(viewport = newViewport)
                state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
              case None => state // No buffer assigned to pane
          case None => state
      }

    def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            val updatedPane = pane.copy(
              smoothScrolling = Some(SmoothScrollState(targetTopLine = targetLine, progress = 0.0))
            )
            state.copy(layout =
              state.layout.copy(
                editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          case None => state
      }

    def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                pane.smoothScrolling match
                  case Some(SmoothScrollState(targetTopLine, _)) =>
                    val currentTopLine = buffer.viewport.topLine // ← Read from BUFFER viewport
                    val (newTopLine, newSmoothing) =
                      if progress >= 1.0 then (targetTopLine, None)
                      else
                        val interpolated =
                          math.round(currentTopLine + progress * (targetTopLine - currentTopLine)).toInt
                        (interpolated, Some(SmoothScrollState(targetTopLine, progress)))

                    // Update BUFFER viewport
                    val updatedBuffer = buffer.copy(viewport = buffer.viewport.copy(topLine = newTopLine))

                    // Update PANE smoothScrolling state
                    val updatedPane = pane.copy(smoothScrolling = newSmoothing)

                    state.copy(
                      buffers = state.buffers + (buffer.id -> updatedBuffer),
                      layout = state.layout.copy(
                        editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                      )
                    )
                  case None => state
              case None => state // No buffer assigned to pane
          case None => state
      }

    def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
      stateRef.update { state =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val halfVisible = buffer.viewport.visibleLines / 2 // ← Read from BUFFER viewport
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),        // ← Update BUFFER cursors
                  viewport = buffer.viewport.copy(topLine = newTopLine) // ← Update BUFFER viewport
                )
                state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
              case None => state // No buffer assigned to pane
          case None => state
      }

    def handleTerminalResize(newSize: TerminalSize): IO[Unit] =
      for
        _ <- logger.debug(s"Handling terminal resize to ${newSize.width}x${newSize.height}")
        currentState <- stateRef.get
        resizedState = SystemEventReducer.reduce(com.serenity.keystroke.events.ResizeEvent(newSize), currentState).state
        rebalancedState = AppEventReducer.rebalancePanes(resizedState, resizedState.focusedBufferId)
        _ <- validateAndUpdateState(rebalancedState, currentState)
      yield ()

    private def saveBufferEffect(bufferId: BufferId): IO[Unit] =
      stateRef.get.flatMap { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) if buffer.filePath.isDefined =>
            fileManager
              .saveBuffer(buffer)
              .flatMap(savedBuffer =>
                stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> savedBuffer)))
              )
              .flatTap(_ => stateRef.get.flatMap(sessionPersistence.onBufferChange).handleErrorWith(ex =>
                logger.error(ex)("[SESSION] Auto-save after file save failed")
              ))
              .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId"))
          case Some(_) =>
            logger.debug(s"[FILE] Buffer $bufferId has no file path; opening Save As workflow") >>
              openFileWorkflowModal(FileWorkflowMode.SaveAs, state, Some(bufferId))
          case None =>
            logger.debug(s"[FILE] Buffer $bufferId not found for save")
      }

    private def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
      stateRef.get.flatMap { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            fileManager
              .saveBuffer(buffer, path)
              .flatMap(savedBuffer =>
                stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> savedBuffer)))
              )
              .flatTap(_ => stateRef.get.flatMap(sessionPersistence.onBufferChange).handleErrorWith(ex =>
                logger.error(ex)("[SESSION] Auto-save after file save failed")
              ))
              .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId as $path"))
          case None =>
            logger.debug(s"[FILE] Buffer $bufferId not found for save as")
      }

    private def toggleThemeEffect(state: AppState): IO[Unit] =
      val targetThemeName =
        state.theme.name match
          case "light"         => "dark"
          case "dark"          => "light"
          case "default-light" => "default-dark"
          case "default-dark"  => "default-light"
          case name if name.toLowerCase.contains("light") => "default-dark"
          case _                                          => "default-light"

      interpretEffect(AppEffect.SwitchTheme(targetThemeName))

    private def reloadThemeEffect(state: AppState): IO[Unit] =
      interpretEffect(AppEffect.ReloadTheme(state.theme.name))

    private def applyThemeByName(themeName: String): IO[Unit] =
      themeManager
        .loadTheme(themeName)
        .flatMap(theme => updateState(_.copy(theme = theme)))
        .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

    private def reloadThemeByName(themeName: String): IO[Unit] =
      themeManager
        .loadTheme(themeName)
        .flatMap(theme => updateState(_.copy(theme = theme)))
        .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

    private def openFileWorkflowModal(
        mode: FileWorkflowMode,
        state: AppState,
        bufferIdOverride: Option[BufferId] = None
    ): IO[Unit] =
      val targetBufferId = bufferIdOverride.orElse(state.focusedBufferId)
      val focusedPath = targetBufferId.flatMap(id => state.buffers.get(id)).flatMap(_.filePath)
      val filename = mode match
        case FileWorkflowMode.SaveAs => focusedPath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("")
        case FileWorkflowMode.Open   => ""

        val pathIO =
          mode match
            case FileWorkflowMode.SaveAs =>
              focusedPath
                .flatMap(path => Option(path.getParent))
                .map(IO.pure)
                .getOrElse(com.serenity.io.FileUtils.getCurrentDirectory)
            case FileWorkflowMode.Open =>
              com.serenity.io.FileUtils.getCurrentDirectory

        pathIO.flatMap { basePath =>
          val workflow = FileWorkflowState(
            mode = mode,
            filename = filename,
            path = basePath.toString
          )
          val predictedState = ModalStateReducer.show(Modal.FileWorkflow(workflow), state).state
          logger.info(
            s"[FILE-WORKFLOW OPENED] mode=$mode filename=${workflow.filename} path=${workflow.path} " +
              s"surfaceId=${predictedState.modalSurface.map(_.id).getOrElse("none")} focus=${predictedState.focus}"
          ) >>
            updateState(current => ModalStateReducer.show(Modal.FileWorkflow(workflow), current).state)
        }

    private def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] =
      val targetBufferIds = closeTargets(scope, state)
      val dirtyBufferIds = targetBufferIds.filter(bufferId => state.buffers.get(bufferId).exists(_.isDirty))
      val cleanBufferIds =
        if scope == CloseScope.Quit then Nil
        else targetBufferIds.filterNot(dirtyBufferIds.contains)
      val stateAfterClean = cleanBufferIds.foldLeft(state)(closeBufferUsingExistingFlow)

      dirtyBufferIds match
        case Nil =>
          val finalState = clearCloseActions(stateAfterClean)
          stateRef.set(finalState) >>
            IO.whenA(scope == CloseScope.Quit)(
              sessionPersistence.onAppClose(finalState) >> 
              quitSignal.complete(()).attempt.void
            )
        case currentBufferId :: remaining =>
          promptCloseWorkflow(stateAfterClean, CloseWorkflowState(
            scope = scope,
            currentBufferId = currentBufferId,
            currentBufferLabel = closeBufferLabel(stateAfterClean, currentBufferId),
            remainingBufferIds = remaining
          ))

    private def closeTargets(scope: CloseScope, state: AppState): List[BufferId] =
      scope match
        case CloseScope.Current => state.focusedBufferId.toList
        case CloseScope.All     => state.bufferOrder
        case CloseScope.Others  => state.focusedBufferId.toList match
            case focused :: Nil => state.bufferOrder.filterNot(_ == focused)
            case Nil            => state.bufferOrder
        case CloseScope.Quit    => state.bufferOrder

    private def promptCloseWorkflow(state: AppState, workflow: CloseWorkflowState): IO[Unit] =
      val focusedState = focusBufferForWorkflow(state, workflow.currentBufferId)
      val modalState = ModalStateReducer.show(Modal.CloseWorkflow(workflow), withCloseAction(focusedState, workflow)).state
      stateRef.set(modalState)

    private def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      stateRef.get.flatMap { state =>
        closeWorkflowSurface(state, surfaceId) match
          case Some((_, workflow)) =>
            workflow.selectedChoice match
              case CloseWorkflowChoice.Cancel =>
                dismissSurfaceAndFocusEditor(surfaceId) >>
                  stateRef.update(clearCloseActions)
              case CloseWorkflowChoice.Discard =>
                val dismissedState = clearCloseActions(dismissModalSurface(state))
                val nextState =
                  if workflow.scope == CloseScope.Quit then dismissedState
                  else closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
                stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
              case CloseWorkflowChoice.Save =>
                state.buffers.get(workflow.currentBufferId) match
                  case Some(buffer) if buffer.filePath.isDefined =>
                    saveBufferEffect(workflow.currentBufferId) >>
                      stateRef.get.flatMap { savedState =>
                        val dismissedState = clearCloseActions(dismissModalSurface(savedState))
                        val nextState =
                          if workflow.scope == CloseScope.Quit then dismissedState
                          else closeBufferUsingExistingFlow(dismissedState, workflow.currentBufferId)
                        stateRef.set(nextState) >> continueCloseWorkflow(workflow, nextState)
                      }
                  case Some(_) =>
                    val dismissedState = withCloseAction(dismissModalSurface(state), workflow)
                    stateRef.set(dismissedState) >>
                      openFileWorkflowModal(FileWorkflowMode.SaveAs, dismissedState, Some(workflow.currentBufferId))
                  case None =>
                    stateRef.set(clearCloseActions(dismissModalSurface(state)))
          case None =>
            IO.unit
      }

    private def continueCloseWorkflow(workflow: CloseWorkflowState, state: AppState): IO[Unit] =
      workflow.remainingBufferIds match
        case nextBufferId :: remaining =>
          promptCloseWorkflow(
            state,
            CloseWorkflowState(
              scope = workflow.scope,
              currentBufferId = nextBufferId,
              currentBufferLabel = closeBufferLabel(state, nextBufferId),
              remainingBufferIds = remaining
            )
          )
        case Nil =>
          val finalState = clearCloseActions(state)
          stateRef.set(finalState) >>
            IO.whenA(workflow.scope == CloseScope.Quit)(
              sessionPersistence.onAppClose(finalState) >> 
              quitSignal.complete(()).attempt.void
            )

    private def focusBufferForWorkflow(state: AppState, bufferId: BufferId): AppState =
      AppEventReducer.rebalancePanes(state, Some(bufferId))

    private def closeBufferUsingExistingFlow(state: AppState, bufferId: BufferId): AppState =
      val registry = CommandRegistry.withToggleUI
      val focusedState = focusBufferForWorkflow(state, bufferId)
      AppEventReducer.reduce(com.serenity.keystroke.events.CloseTab, focusedState, registry).state

    private def closeBufferLabel(state: AppState, bufferId: BufferId): String =
      state.buffers.get(bufferId)
        .flatMap(_.filePath.flatMap(path => Option(path.getFileName).map(_.toString)))
        .getOrElse(s"Buffer ${bufferId.value} - unsaved")

    private def withCloseAction(state: AppState, workflow: CloseWorkflowState): AppState =
      state.copy(actionStack = AppAction.CloseWorkflow(workflow) :: clearCloseActions(state).actionStack)

    private def clearCloseActions(state: AppState): AppState =
      state.copy(actionStack = state.actionStack.filter {
        case AppAction.CloseWorkflow(_) => false
        case _                          => true
      })

    private def dismissModalSurface(state: AppState): AppState =
      state.copy(uiSurfaces = state.uiSurfaces.filterNot {
        case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
        case _                                                   => false
      })

    private def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      stateRef.get.flatMap { state =>
        fileWorkflowSurface(state, surfaceId) match
          case Some((_, workflow)) =>
            refreshWorkflowState(workflow).flatMap { refreshed =>
              updateFileWorkflowSurface(surfaceId, refreshed)
            }
          case None =>
            IO.unit
      }

    private def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      stateRef.get.flatMap { state =>
        fileWorkflowSurface(state, surfaceId) match
          case Some((_, workflow)) =>
            workflow.mode match
              case FileWorkflowMode.Open =>
                completeOpenWorkflow(surfaceId, workflow)
              case FileWorkflowMode.SaveAs =>
                completeSaveAsWorkflow(surfaceId, workflow, state)
          case None =>
            IO.unit
      }

    private def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit] =
      stateRef.get.flatMap { state =>
        replaceWorkflowSurface(state, surfaceId) match
          case Some((_, workflow)) =>
            activeEditorBufferId(state) match
              case None =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some("No active buffer"))
                )
              case Some(bufferId) if workflow.findText.isEmpty =>
                updateReplaceWorkflowSurface(
                  surfaceId,
                  workflow.copy(statusMessage = Some("Enter text to find"))
                )
              case Some(bufferId) =>
                state.buffers.get(bufferId) match
                  case Some(buffer) =>
                    val matches = buffer.content.searchAll(workflow.findText)
                    if matches.isEmpty then
                      updateReplaceWorkflowSurface(
                        surfaceId,
                        workflow.copy(statusMessage = Some("No matches found"))
                      )
                    else
                      val updatedBuffer = buffer.copy(
                        content = buffer.content.replaceAll(workflow.findText, workflow.replacementText),
                        isDirty = true
                      )
                      stateRef.update { current =>
                        val updatedState = current.copy(
                          buffers = current.buffers + (bufferId -> updatedBuffer),
                          uiSurfaces = current.uiSurfaces.filterNot(_.id == surfaceId)
                        )
                        current.layout.activeEditorPaneId match
                          case Some(paneId) => updatedState.copy(focus = Focus.EditorPane(paneId))
                          case None         => updatedState
                      }
                  case None =>
                    updateReplaceWorkflowSurface(
                      surfaceId,
                      workflow.copy(statusMessage = Some("No active buffer"))
                    )
                
          case None =>
            IO.unit
      }

    private def refreshWorkflowState(workflow: FileWorkflowState): IO[FileWorkflowState] =
      for
        directoryPath <- workflowDirectoryPath(workflow)
        suggestions <- workflow.activeField match
          case FileWorkflowField.Path => pathSuggestions(workflow.path)
          case FileWorkflowField.Filename if workflow.mode == FileWorkflowMode.Open => filenameSuggestions(workflow)
          case FileWorkflowField.Filename => IO.pure(Nil)
        missingSegments <- missingDirectorySegments(directoryPath)
      yield workflow.copy(
        suggestions = suggestions,
        selectedSuggestionIndex = if suggestions.isEmpty then 0 else math.min(workflow.selectedSuggestionIndex, suggestions.length - 1),
        missingPathSegments = missingSegments,
        confirmCreateDirectories = false,
        statusMessage = None
      )

    private def pathSuggestions(pathInput: String): IO[List[FileWorkflowSuggestion]] =
      for
        currentDirectory <- FileUtils.getCurrentDirectory
        basePathInput = if pathInput.trim.isEmpty then currentDirectory.toString else pathInput
        resolvedPath <- FileUtils.resolvePath(basePathInput)
        isDirectoryPath <- IO.blocking(Files.exists(resolvedPath) && Files.isDirectory(resolvedPath))
        endsWithSeparator = pathInput.endsWith("/") || pathInput.endsWith("\\")
        baseDirectory = if endsWithSeparator || isDirectoryPath then resolvedPath else Option(resolvedPath.getParent).getOrElse(currentDirectory)
        prefix = if endsWithSeparator || isDirectoryPath then "" else Option(resolvedPath.getFileName).map(_.toString).getOrElse("")
        entries <- fileManager.getFileBrowser.listDirectory(baseDirectory)
      yield entries
        .filter(_.isDirectory)
        .filter(entry => prefix.isEmpty || entry.name.toLowerCase.startsWith(prefix.toLowerCase))
        .map(entry => FileWorkflowSuggestion(entry.path.toString, isDirectory = true))

    private def filenameSuggestions(workflow: FileWorkflowState): IO[List[FileWorkflowSuggestion]] =
      for
        directoryPath <- workflowDirectoryPath(workflow)
        entries <- fileManager.getFileBrowser.listDirectory(directoryPath)
      yield entries
        .filterNot(_.isDirectory)
        .filter(entry => workflow.filename.trim.isEmpty || entry.name.toLowerCase.startsWith(workflow.filename.toLowerCase))
        .filter(entry => FileUtils.isReadableFile(entry.path))
        .map(entry => FileWorkflowSuggestion(entry.name, isDirectory = false))

    private def workflowDirectoryPath(workflow: FileWorkflowState): IO[Path] =
      if workflow.filename.trim.nonEmpty then
        FileUtils.resolvePath(workflow.path)
      else
        FileUtils.resolvePath(workflow.path).map(path => Option(path.getParent).getOrElse(path))

    private def workflowTargetPath(workflow: FileWorkflowState): IO[Path] =
      if workflow.filename.trim.nonEmpty then
        FileUtils.resolvePath(workflow.path).map(_.resolve(workflow.filename.trim).normalize())
      else
        FileUtils.resolvePath(workflow.path)

    private def missingDirectorySegments(directoryPath: Path): IO[List[String]] =
      IO.blocking {
        val normalized = directoryPath.normalize()
        val segmentNames = (0 until normalized.getNameCount).toList.map(index => normalized.getName(index).toString)
        val initialPath =
          Option(normalized.getRoot).getOrElse(Paths.get(""))

        segmentNames.foldLeft((initialPath, false, List.empty[String])) { case ((currentPath, alreadyMissing, missing), segment) =>
          val nextPath =
            if currentPath.toString.isEmpty then Paths.get(segment)
            else currentPath.resolve(segment)
          val nextMissing =
            if alreadyMissing || !Files.exists(nextPath) then missing :+ segment
            else missing
          val nextAlreadyMissing = alreadyMissing || !Files.exists(nextPath)
          (nextPath, nextAlreadyMissing, nextMissing)
        }._3
      }

    private def completeOpenWorkflow(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
      workflowTargetPath(workflow).flatMap { targetPath =>
        if !FileUtils.isReadableFile(targetPath) then
          updateFileWorkflowSurface(
            surfaceId,
            workflow.copy(statusMessage = Some(s"File not found: $targetPath"))
          ) >>
            logger.debug(s"[FILE-WORKFLOW] Open target is not readable: $targetPath")
        else
          fileManager
            .loadFile(targetPath)
            .flatMap { loadedBuffer =>
              stateRef.modify { state =>
                val newBufferId = state.nextBufferId
                val bufferToInsert = loadedBuffer.copy(id = newBufferId)
                val updatedState = state.copy(
                  buffers = state.buffers + (newBufferId -> bufferToInsert),
                  bufferOrder = insertBufferInOrder(state, newBufferId),
                  nextBufferId = BufferId(newBufferId.value + 1),
                  uiSurfaces = List.empty
                )
                val rebalanced = AppEventReducer.rebalancePanes(updatedState, Some(newBufferId))
                val focused = focusBuffer(rebalanced, newBufferId)
                (focused, ())
              }
            }
            .handleErrorWith(ex => logger.error(ex)(s"[FILE-WORKFLOW] Failed to open $targetPath"))
      }

    private def completeSaveAsWorkflow(surfaceId: SurfaceId, workflow: FileWorkflowState, state: AppState): IO[Unit] =
      activeEditorBufferId(state) match
        case Some(bufferId) =>
          if workflow.missingPathSegments.nonEmpty && !workflow.confirmCreateDirectories then
            updateFileWorkflowSurface(surfaceId, workflow.copy(confirmCreateDirectories = true))
          else
            workflowTargetPath(workflow).flatMap { targetPath =>
              saveBufferAsEffect(bufferId, targetPath) >>
                stateRef.get.flatMap { savedState =>
                  savedState.actionStack.collectFirst { case AppAction.CloseWorkflow(closeWorkflow) => closeWorkflow } match
                    case Some(closeWorkflow) if closeWorkflow.currentBufferId == bufferId =>
                      val dismissedState = dismissModalSurface(savedState)
                      val nextState =
                        if closeWorkflow.scope == CloseScope.Quit then dismissedState
                        else closeBufferUsingExistingFlow(dismissedState, bufferId)
                      stateRef.set(nextState) >> continueCloseWorkflow(closeWorkflow, nextState)
                    case _ =>
                      dismissSurfaceAndFocusEditor(surfaceId)
                }
            }
        case None =>
          logger.debug("[FILE-WORKFLOW] No focused buffer available for save-as")

    private def activeEditorBufferId(state: AppState): Option[BufferId] =
      state.layout.activeEditorPaneId
        .flatMap(state.layout.editorPanes.get)
        .flatMap(_.bufferId)

    private def updateFileWorkflowSurface(surfaceId: SurfaceId, workflow: FileWorkflowState): IO[Unit] =
      stateRef.update { state =>
        state.surfaceById(surfaceId) match
          case Some(surface) =>
            val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)))
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
          case None =>
            state
      }

    private def updateReplaceWorkflowSurface(surfaceId: SurfaceId, workflow: ReplaceWorkflowState): IO[Unit] =
      stateRef.update { state =>
        state.surfaceById(surfaceId) match
          case Some(surface) =>
            val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)))
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId) :+ updatedSurface)
          case None =>
            state
      }

    private def dismissSurfaceAndFocusEditor(surfaceId: SurfaceId): IO[Unit] =
      stateRef.update { state =>
        val baseState = state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surfaceId))
        state.layout.activeEditorPaneId match
          case Some(paneId) => baseState.copy(focus = Focus.EditorPane(paneId))
          case None         => baseState
      }

    private def fileWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, FileWorkflowState)] =
      state.surfaceById(surfaceId).flatMap { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some((surface, workflow))
          case _                                                          => None
      }

    private def replaceWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, ReplaceWorkflowState)] =
      state.surfaceById(surfaceId).flatMap { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)) => Some((surface, workflow))
          case _                                                             => None
      }

    private def closeWorkflowSurface(state: AppState, surfaceId: SurfaceId): Option[(UiSurface, CloseWorkflowState)] =
      state.surfaceById(surfaceId).flatMap { surface =>
        surface.content match
          case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some((surface, workflow))
          case _                                                           => None
      }

    private def insertBufferInOrder(state: AppState, newBufferId: BufferId): List[BufferId] =
      state.focusedBufferId match
        case Some(currentBufferId) =>
          val currentIndex = state.bufferOrder.indexOf(currentBufferId)
          if currentIndex == -1 then state.bufferOrder :+ newBufferId
          else
            val (before, after) = state.bufferOrder.splitAt(currentIndex + 1)
            before ++ List(newBufferId) ++ after
        case None =>
          state.bufferOrder :+ newBufferId

    private def focusBuffer(state: AppState, bufferId: BufferId): AppState =
      state.layout.editorPanes.find(_._2.bufferId.contains(bufferId)) match
        case Some((paneId, _)) =>
          state.copy(
            focus = Focus.EditorPane(paneId),
            layout = state.layout.copy(activeEditorPaneId = Some(paneId))
          )
        case None =>
          state
