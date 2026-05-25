package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{Event, FileEvent, GlobalAppEvent, SystemEvent}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, CommandRunnerReducer, FileEventReducer, ModalStateReducer, PanelStateReducer, PeekStateReducer, ReducerResult, SystemEventReducer}
import com.serenity.ui.layout.*
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

trait StateManager:
  def applyEvent(event: Event): IO[Unit]
  def getCurrentState: IO[AppState]
  def getCurrentFocus: IO[Focus]
  def switchFocus(newFocus: Focus): IO[Unit]
  def getActiveBuffer: IO[Option[Buffer]]
  def getActivePane: IO[Option[EditorPane]]
  def awaitQuit: IO[Unit]
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

  // Session operations (stubs for test compilation)
  def serializeSession(): IO[String]
  def restoreSession(sessionData: String): IO[Unit]

object StateManager:

  def apply(parentLogger: Logger[IO])(using Balance, LoggerFactory[IO]): IO[StateManager] =
    for
      stateRef   <- Ref.of[IO, AppState](AppState.initial)
      quitSignal <- Deferred[IO, Unit]
    yield new StateManagerImpl(
      stateRef,
      quitSignal,
      LoggerFactory[IO].getLogger(using LoggerName("com.serenity.state.manager.StateManager"))
    )

  private class StateManagerImpl(
      stateRef: Ref[IO, AppState],
      quitSignal: Deferred[IO, Unit],
      logger: Logger[IO]
  )(using Balance)
      extends StateManager:
    private val fileManager = new FileManager()

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

        val (newFocus, newCommandRunner) = newActivePaneId match
          case Some(id) => (Focus.EditorPane(id), state.commandRunner)
          case None     => (Focus.CommandRunner, CommandRunner.empty.activate(CommandRegistry.default))

        state.copy(
          layout = state.layout.copy(
            editorPanes = updatedPanes,
            activeEditorPaneId = newActivePaneId
          ),
          focus = newFocus,
          commandRunner = newCommandRunner
        )
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

    def awaitQuit: IO[Unit] = quitSignal.get

    def applyEvent(event: Event): IO[Unit] =
      for
        currentState <- stateRef.get
        _ <- event match
          case systemEvent: SystemEvent =>
            applyReducerResult(SystemEventReducer.reduce(systemEvent, currentState), currentState)
          case appEvent: GlobalAppEvent =>
            val registry = CommandRegistry.withToggleUI
            applyReducerResult(AppEventReducer.reduce(appEvent, currentState, registry), currentState)
          case fileEvent: FileEvent =>
            applyReducerResult(FileEventReducer.reduce(fileEvent, currentState), currentState)
          case _ if currentState.focus == Focus.CommandRunner =>
            val registry = CommandRegistry.withToggleUI
            applyReducerResult(CommandRunnerReducer.reduce(event, currentState, registry), currentState)
          case _ =>
            val component = getComponentForFocus(currentState.focus)
            val result    = component.processEvent(event, currentState)
            for
              newState <- applyComponentResult(result, currentState)
              _        <- validateAndUpdateState(newState, currentState)
            yield ()
      yield ()

    private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      newState.validated match
        case Right(validState) =>
          stateRef.set(validState)
        case Left(errors) =>
          // Log validation errors and keep unchanged state
          logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
            stateRef.set(fallbackState)

    private def getComponentForFocus(focus: Focus): FocusedComponent =
      focus match
        case Focus.EditorPane(paneId)    => new EditorPaneComponent(paneId)
        case Focus.PinnedPanel(position) => new PinnedPanelComponent(position)
        case Focus.PeekOverlay           => new PeekOverlayComponent()
        case Focus.Modal(modalType)      => new ModalComponent(modalType)
        case Focus.CommandRunner =>
          val registry = CommandRegistry.withToggleUI
          new CommandRunnerComponent(registry)

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
          stateRef.get.flatMap(state => interpretCommand(command, state))
        case AppEffect.SaveBuffer(bufferId) =>
          saveBufferEffect(bufferId)
        case AppEffect.SaveBufferAs(bufferId, path) =>
          saveBufferAsEffect(bufferId, path)
        case AppEffect.RequestOpenFile =>
          logger.debug("[FILE] Open file requested")

    private def applyComponentResult(result: ComponentResult, state: AppState): IO[AppState] =
      result match
        case ComponentResult.NoChange                => IO.pure(state)
        case ComponentResult.StateChange(update)     => IO.pure(update(state))
        case ComponentResult.FocusTransfer(newFocus) => IO.pure(state.copy(focus = newFocus))
        case ComponentResult.Dismiss =>
          val newFocus = determineFallbackFocus(state)
          IO.pure(dismissCurrentFocus(state).copy(focus = newFocus))
        case ComponentResult.ExecuteCommand(command) =>
          for
            _            <- interpretCommand(command, state)
            updatedState <- stateRef.get
          yield updatedState
        case ComponentResult.Composite(results) =>
          results.foldLeftM(state)((s, r) => applyComponentResult(r, s))

    private def determineFallbackFocus(state: AppState): Focus =
      state.layout.activeEditorPaneId match
        case Some(paneId) => Focus.EditorPane(paneId)
        case None         => state.focus // fallback to current focus if no active pane

    private def dismissCurrentFocus(state: AppState): AppState =
      state.focus match
        case Focus.PeekOverlay   => state.copy(peekOverlay = None)
        case Focus.Modal(_)      => state.copy(modal = None)
        case Focus.CommandRunner => state.copy(commandRunner = CommandRunner.empty)
        case _                   => state // Other focus types don't have dismissible state

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
          logger.debug("[CMD] Save As command requested")
        case CommandIntent.OpenFile =>
          logger.debug("[CMD] Open command requested")
        case CommandIntent.QuitApp =>
          quitSignal.complete(()).attempt.void
        case CommandIntent.NewFile =>
          logger.debug("[CMD] New file command requested")
        case CommandIntent.CloseCurrentFile =>
          logger.debug("[CMD] Close file command requested")
        case CommandIntent.FindInCurrentFile =>
          logger.debug("[CMD] Find command requested")
        case CommandIntent.ReplaceInCurrentFile =>
          logger.debug("[CMD] Replace command requested")
        case CommandIntent.OpenGotoLine =>
          logger.debug("[CMD] Go to line command requested")
        case CommandIntent.ToggleTheme =>
          logger.debug("[CMD] Toggle theme command requested")
        case CommandIntent.ReloadTheme =>
          logger.debug("[CMD] Reload theme command requested")
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

    // Session operation stubs (TODO: implement)
    def serializeSession(): IO[String] =
      IO.pure("{\"TODO\": \"implement session serialization\"}")

    def restoreSession(sessionData: String): IO[Unit] =
      logger.debug(s"TODO: restoreSession($sessionData)")

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
              .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId"))
          case Some(_) =>
            logger.debug(s"[FILE] Buffer $bufferId has no file path; Save As required")
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
              .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId as $path"))
          case None =>
            logger.debug(s"[FILE] Buffer $bufferId not found for save as")
      }
