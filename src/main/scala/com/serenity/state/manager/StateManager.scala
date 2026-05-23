package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.{Event, Quit, ResizeEvent}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.*
import com.serenity.state.models.*
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
      stateRef.update { state =>
        state.copy(
          peekOverlay = Some(PeekOverlay(content, at)),
          focus = Focus.PeekOverlay
        )
      }

    def dismissPeek(): IO[Unit] =
      stateRef.update { state =>
        val newFocus = state.layout.activeEditorPaneId match
          case Some(paneId) => Focus.EditorPane(paneId)
          case None         => state.focus

        state.copy(
          peekOverlay = None,
          focus = newFocus
        )
      }

    def peekToPin(position: PanelPosition): IO[Unit] =
      stateRef.update { state =>
        state.peekOverlay match
          case Some(overlay) =>
            val panelContent = overlay.content match
              case PeekContent.DirectoryListing(path, entries) =>
                Some(
                  PanelContent.DirectoryTree(
                    com.serenity.ui.layout.DirectoryTreeData(path),
                    Some(path)
                  )
                )
              case _ => None

            panelContent match
              case Some(content) =>
                val panel    = PinnedPanel(position, content, 30)
                val newFocus = Focus.PinnedPanel(position)
                state.copy(
                  layout = state.layout.copy(
                    pinnedPanels = state.layout.pinnedPanels + (position -> panel)
                  ),
                  peekOverlay = None,
                  focus = newFocus
                )
              case None => state.copy(peekOverlay = None)
          case None => state
      }

    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      stateRef.update { state =>
        val panel = PinnedPanel(position, content, size)
        state.copy(
          layout = state.layout.copy(
            pinnedPanels = state.layout.pinnedPanels + (position -> panel)
          )
        )
      }

    def unpinPanel(position: PanelPosition): IO[Unit] =
      stateRef.update { state =>
        val newFocus =
          if state.focus == Focus.PinnedPanel(position) then
            state.layout.activeEditorPaneId match
              case Some(paneId) => Focus.EditorPane(paneId)
              case None         => state.focus
          else state.focus

        state.copy(
          layout = state.layout.copy(
            pinnedPanels = state.layout.pinnedPanels - position
          ),
          focus = newFocus
        )
      }

    def showModal(modal: Modal): IO[Unit] =
      stateRef.update { state =>
        state.copy(
          modal = Some(modal),
          focus = Focus.Modal(modal match
            case Modal.CommandRunner(_, _, _) => ModalType.CommandPalette
            case Modal.FileSearch(_, _, _)    => ModalType.FileSearch
            case Modal.GotoLine(_)            => ModalType.GotoLine
            case Modal.Find(_, _, _)          => ModalType.Find)
        )
      }

    def dismissModal(): IO[Unit] =
      stateRef.update { state =>
        val newFocus = state.layout.activeEditorPaneId match
          case Some(paneId) => Focus.EditorPane(paneId)
          case None         => state.focus

        state.copy(
          modal = None,
          focus = newFocus
        )
      }

    def awaitQuit: IO[Unit] = quitSignal.get

    def applyEvent(event: Event): IO[Unit] =
      event match
        case Quit => quitSignal.complete(()).void
        case resizeEvent: ResizeEvent =>
          for
            currentState <- stateRef.get
            resizeComponent = new ResizeComponent()
            result          = resizeComponent.processEvent(resizeEvent, currentState)
            newState       <- applyComponentResult(result, currentState)
            validatedState <- validateAndUpdateState(newState, currentState)
          yield ()
        case _ =>
          for
            currentState <- stateRef.get
            result = handleGlobalEvent(event, currentState) match
              case Some(globalResult) => globalResult
              case None               =>
                // Route to focused component
                val component = getComponentForFocus(currentState.focus)
                component.processEvent(event, currentState)
            newState       <- applyComponentResult(result, currentState)
            validatedState <- validateAndUpdateState(newState, currentState)
          yield ()

    private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      newState.validated match
        case Right(validState) =>
          stateRef.set(validState)
        case Left(errors) =>
          // Log validation errors and keep unchanged state
          logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
            stateRef.set(fallbackState)

    private def handleGlobalEvent(event: Event, state: AppState): Option[ComponentResult] =
      import com.serenity.keystroke.events.*
      event match
        case ToggleCommandRunner =>
          val commandRunnerComponent = new CommandRunnerComponent()
          Some(commandRunnerComponent.processEvent(event, state))
        case NewTab =>
          Some(handleNewTabGlobally(state))
        case CloseTab =>
          Some(handleCloseTabGlobally(state))
        case NextTab =>
          Some(handleNextTabGlobally(state))
        case PreviousTab =>
          Some(handlePreviousTabGlobally(state))
        case _ => None

    private def getComponentForFocus(focus: Focus): FocusedComponent =
      focus match
        case Focus.EditorPane(paneId)    => new EditorPaneComponent(paneId)
        case Focus.PinnedPanel(position) => new PinnedPanelComponent(position)
        case Focus.PeekOverlay           => new PeekOverlayComponent()
        case Focus.Modal(modalType)      => new ModalComponent(modalType)
        case Focus.CommandRunner         => new CommandRunnerComponent()

    private def applyComponentResult(result: ComponentResult, state: AppState): IO[AppState] =
      result match
        case ComponentResult.NoChange                => IO.pure(state)
        case ComponentResult.StateChange(update)     => IO.pure(update(state))
        case ComponentResult.FocusTransfer(newFocus) => IO.pure(state.copy(focus = newFocus))
        case ComponentResult.Dismiss =>
          val newFocus = determineFallbackFocus(state)
          IO.pure(dismissCurrentFocus(state).copy(focus = newFocus))
        case ComponentResult.ExecuteCommand(command) =>
          command.execute(state) *> stateRef.get
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

    // File operation stubs (TODO: implement)
    def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit] =
      logger.debug(s"TODO: setBufferFilePath($bufferId, $filePath)")

    def saveBuffer(bufferId: BufferId): IO[Unit] =
      logger.debug(s"TODO: saveBuffer($bufferId)")

    def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit] =
      logger.debug(s"TODO: saveBufferAs($bufferId, $filePath)")

    def markBufferSaved(bufferId: BufferId): IO[Unit] =
      logger.debug(s"TODO: markBufferSaved($bufferId)")

    def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean] =
      IO.pure(false) // TODO: implement actual logic

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
      switchFocus(Focus.PinnedPanel(position))

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

    // Global tab management event handlers
    private def handleNewTabGlobally(state: AppState): ComponentResult =
      // Create a command that creates a new buffer and manages pane assignment
      val command = com.serenity.command.Command(
        "NewTab",
        "Create new buffer and assign to pane based on layout",
        _ =>
          for
            newBufferId <- createNewEmptyBuffer()
            _           <- insertBufferInOrder(newBufferId)
            _           <- assignBuffersToPanes(Some(newBufferId))
            _           <- focusBuffer(newBufferId)
          yield ()
      )
      ComponentResult.executeCommand(command)

    private def handleCloseTabGlobally(state: AppState): ComponentResult =
      state.focus match
        case Focus.EditorPane(paneId) =>
          state.layout.editorPanes.get(paneId) match
            case Some(pane) =>
              pane.bufferId match
                case Some(bufferId) =>
                  val buffer = state.buffers.get(bufferId)
                  buffer match
                    case Some(buf) if buf.isDirty =>
                      println("[TAB] Warning: Closing tab with unsaved changes")
                      createCloseTabAndBufferCommand(paneId, bufferId)
                    case _ =>
                      createCloseTabAndBufferCommand(paneId, bufferId)
                case None =>
                  createCloseTabOnlyCommand(paneId)
            case None =>
              ComponentResult.noChange
        case _ =>
          ComponentResult.noChange

    private def createCloseTabAndBufferCommand(paneId: PaneId, bufferId: BufferId): ComponentResult =
      val command = com.serenity.command.Command(
        "CloseTabAndBuffer",
        "Close tab and its buffer",
        _ =>
          for
            _ <- closeBuffer(bufferId)
            _ <- closePane(paneId)
          yield ()
      )
      ComponentResult.executeCommand(command)

    private def createCloseTabOnlyCommand(paneId: PaneId): ComponentResult =
      val command = com.serenity.command.Command(
        "CloseTabOnly",
        "Close tab only",
        _ =>
          for _ <- closePane(paneId)
          yield ()
      )
      ComponentResult.executeCommand(command)

    private def handleNextTabGlobally(state: AppState): ComponentResult =
      if state.bufferOrder.isEmpty then ComponentResult.noChange
      else
        state.focusedBufferId match
          case Some(currentBufferId) =>
            state.nextBufferInOrder(currentBufferId) match
              case Some(nextBufferId) =>
                val command = com.serenity.command.Command(
                  "NextTab",
                  "Navigate to next buffer",
                  _ =>
                    for
                      _ <- assignBuffersToPanes(Some(nextBufferId))
                      _ <- focusBuffer(nextBufferId)
                    yield ()
                )
                ComponentResult.executeCommand(command)
              case None =>
                ComponentResult.noChange
          case None =>
            // No current focus, focus first buffer
            val firstBufferId = state.bufferOrder.head
            val command = com.serenity.command.Command(
              "NextTab",
              "Focus first buffer",
              _ => focusBuffer(firstBufferId)
            )
            ComponentResult.executeCommand(command)

    private def handlePreviousTabGlobally(state: AppState): ComponentResult =
      if state.bufferOrder.isEmpty then ComponentResult.noChange
      else
        state.focusedBufferId match
          case Some(currentBufferId) =>
            state.previousBufferInOrder(currentBufferId) match
              case Some(prevBufferId) =>
                val command = com.serenity.command.Command(
                  "PreviousTab",
                  "Navigate to previous buffer",
                  _ =>
                    for
                      _ <- assignBuffersToPanes(Some(prevBufferId))
                      _ <- focusBuffer(prevBufferId)
                    yield ()
                )
                ComponentResult.executeCommand(command)
              case None =>
                ComponentResult.noChange
          case None =>
            // No current focus, focus first buffer
            val firstBufferId = state.bufferOrder.head
            val command = com.serenity.command.Command(
              "PreviousTab",
              "Focus first buffer",
              _ => focusBuffer(firstBufferId)
            )
            ComponentResult.executeCommand(command)

    // Buffer management methods for new tab behavior
    private def insertBufferInOrder(newBufferId: BufferId): IO[Unit] =
      stateRef.update { state =>
        state.focusedBufferId match
          case Some(currentBufferId) =>
            // Insert after the currently focused buffer
            val currentIndex = state.bufferOrder.indexOf(currentBufferId)
            if currentIndex == -1 then
              // Current buffer not in order, append to end
              state.copy(bufferOrder = state.bufferOrder :+ newBufferId)
            else
              // Insert after current position
              val (before, after) = state.bufferOrder.splitAt(currentIndex + 1)
              state.copy(bufferOrder = before ++ List(newBufferId) ++ after)
          case None =>
            // No current focus, append to end
            state.copy(bufferOrder = state.bufferOrder :+ newBufferId)
      }

    private def assignBuffersToPanes(focusedBufferId: Option[BufferId] = None): IO[Unit] =
      for
        state <- stateRef.get
        terminalSize = state.terminalSize.getOrElse(TerminalSize(80, 24))
        layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(state, terminalSize)
        // Calculate how many panes CAN fit based on minimum width
        maxPossiblePanes    = math.max(1, layout.editorPanelRect.width / state.config.minimumPaneWidth)
        targetFocusedBuffer = focusedBufferId.orElse(state.focusedBufferId)
        _ <- updatePaneAssignments(state, maxPossiblePanes, targetFocusedBuffer)
      yield ()

    private def updatePaneAssignments(
      state: AppState,
      maxVisiblePanes: Int,
      targetFocusedBuffer: Option[BufferId]
    ): IO[Unit] =
      stateRef.update { currentState =>
        // Find the focused buffer and determine which buffers should be visible
        targetFocusedBuffer match
          case Some(focusedBufferId) =>
            val focusedIndex = currentState.bufferOrder.indexOf(focusedBufferId)
            val startIndex =
              if focusedIndex == -1 then 0
              else
                // Center the focused buffer in the visible range
                math.max(0, focusedIndex - maxVisiblePanes / 2)
            val visibleBuffers = currentState.bufferOrder.slice(startIndex, startIndex + maxVisiblePanes)

            // Ensure we have enough panes for visible buffers
            val neededPanes  = visibleBuffers.size
            val currentPanes = currentState.layout.editorPanes
            val paneIds      = currentPanes.keys.toList.sortBy(_.value)

            // Create additional panes if needed
            val updatedLayout = if paneIds.size < neededPanes then
              val additionalPanes = (paneIds.size until neededPanes).map { i =>
                val paneId = PaneId(currentState.nextPaneId.value + i - paneIds.size)
                paneId -> EditorPane.empty(paneId)
              }.toMap
              val newNextPaneId = PaneId(
                math.max(currentState.nextPaneId.value, currentState.nextPaneId.value + neededPanes - paneIds.size)
              )
              currentState.copy(
                layout = currentState.layout.copy(
                  editorPanes = currentPanes ++ additionalPanes
                ),
                nextPaneId = newNextPaneId
              )
            else currentState

            // Assign buffers to panes
            val finalPanes      = updatedLayout.layout.editorPanes.keys.toList.sortBy(_.value)
            val paneAssignments = finalPanes.take(visibleBuffers.size).zip(visibleBuffers).toMap

            val assignedPanes = finalPanes.map { paneId =>
              paneAssignments.get(paneId) match
                case Some(bufferId) =>
                  paneId -> EditorPane.withBuffer(paneId, bufferId)
                case None =>
                  paneId -> EditorPane.empty(paneId)
            }.toMap

            val finalState = updatedLayout.copy(
              layout = updatedLayout.layout.copy(editorPanes = assignedPanes)
            )

            // Restore focus to the target buffer if it's visible in a pane
            val paneWithFocusedBuffer = assignedPanes.find(_._2.bufferId.contains(focusedBufferId))
            paneWithFocusedBuffer match
              case Some((paneId, _)) =>
                finalState.copy(
                  layout = finalState.layout.copy(activeEditorPaneId = Some(paneId)),
                  focus = Focus.EditorPane(paneId)
                )
              case None =>
                finalState
          case None =>
            currentState
      }

    private def focusBuffer(bufferId: BufferId): IO[Unit] =
      stateRef.update { state =>
        // Find a pane that shows this buffer
        val paneWithBuffer = state.layout.editorPanes.find(_._2.bufferId.contains(bufferId))
        paneWithBuffer match
          case Some((paneId, _)) =>
            state.copy(
              focus = Focus.EditorPane(paneId),
              layout = state.layout.copy(activeEditorPaneId = Some(paneId))
            )
          case None =>
            state
      }

    // Session operation stubs (TODO: implement)
    def serializeSession(): IO[String] =
      IO.pure("{\"TODO\": \"implement session serialization\"}")

    def restoreSession(sessionData: String): IO[Unit] =
      logger.debug(s"TODO: restoreSession($sessionData)")

    def handleTerminalResize(newSize: TerminalSize): IO[Unit] =
      for
        _ <- logger.debug(s"Handling terminal resize to ${newSize.width}x${newSize.height}")
        _ <- stateRef.update(_.copy(terminalSize = Some(newSize)))
        _ <- assignBuffersToPanes()
      yield ()
