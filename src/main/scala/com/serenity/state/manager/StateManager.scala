package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, IO, Ref}
import com.serenity.keystroke.events.{Event, Quit, ResizeEvent}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.command.CommandRunner

trait StateManager:
  def applyEvent(event: Event): IO[Unit]
  def getCurrentState: IO[AppState]
  def getCurrentFocus: IO[Focus]
  def switchFocus(newFocus: Focus): IO[Unit]
  def getActiveBuffer: IO[Option[Buffer]]
  def getActivePane: IO[Option[EditorPane]]
  def awaitQuit: IO[Unit]
  def updateState(update: AppState => AppState): IO[Unit]
//  def cleanupCompletedAnimations(): IO[Unit]

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

  def apply(using Balance): IO[StateManager] =
    for
      stateRef   <- Ref.of[IO, AppState](AppState.initial)
      quitSignal <- Deferred[IO, Unit]
    yield new StateManagerImpl(stateRef, quitSignal)

  private class StateManagerImpl(
      stateRef: Ref[IO, AppState],
      quitSignal: Deferred[IO, Unit]
  )(using Balance)
      extends StateManager:

    def getCurrentState: IO[AppState] = stateRef.get

    def getCurrentFocus: IO[Focus] = stateRef.get.map(_.focus)

    def switchFocus(newFocus: Focus): IO[Unit] =
      stateRef.update(_.copy(focus = newFocus))

    def updateState(update: AppState => AppState): IO[Unit] =
      stateRef.update(update)

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

        val newFocus = newActivePaneId match
          case Some(id) => Focus.EditorPane(id)
          case None     => state.focus

        state.copy(
          layout = state.layout.copy(
            editorPanes = updatedPanes,
            activeEditorPaneId = newActivePaneId
          ),
          focus = newFocus
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
            val newCursor   = CursorPosition(line, column)
            val updatedPane = pane.copy(cursors = List(newCursor))
            state.copy(
              layout = state.layout.copy(
                editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          case None => state // Pane doesn't exist, no change
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
            case Modal.GotoLine(_)            => ModalType.GotoLine)
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
          stateRef.modify { currentState =>
            val resizeComponent = new ResizeComponent()
            val result          = resizeComponent.processEvent(resizeEvent, currentState)
            val newState        = applyComponentResult(result, currentState)

            // Validate the new state
            newState.validated match
              case Right(validState) => (validState, ())
              case Left(errors)      =>
                // Log validation errors and return unchanged state
                println(s"State validation failed: ${errors.mkString(", ")}")
                (currentState, ())
          }.void
        case _ =>
          stateRef.modify { currentState =>
            // Check for global events first (like ToggleCommandRunner)
            val result = handleGlobalEvent(event, currentState) match
              case Some(globalResult) => globalResult
              case None =>
                // Route to focused component
                val component = getComponentForFocus(currentState.focus)
                component.processEvent(event, currentState)
            
            val newState = applyComponentResult(result, currentState)

            // Validate the new state
            newState.validated match
              case Right(validState) => (validState, ())
              case Left(errors)      =>
                // Log validation errors and return unchanged state
                // In a real implementation, you'd want proper logging here
                println(s"State validation failed: ${errors.mkString(", ")}")
                (currentState, ())
          }.void

    private def handleGlobalEvent(event: Event, state: AppState): Option[ComponentResult] =
      import com.serenity.keystroke.events.*
      event match
        case ToggleCommandRunner =>
          val commandRunnerComponent = new CommandRunnerComponent()
          Some(commandRunnerComponent.processEvent(event, state))
        case _ => None

    private def getComponentForFocus(focus: Focus): FocusedComponent =
      focus match
        case Focus.EditorPane(paneId)    => new EditorPaneComponent(paneId)
        case Focus.PinnedPanel(position) => new PinnedPanelComponent(position)
        case Focus.PeekOverlay           => new PeekOverlayComponent()
        case Focus.Modal(modalType)      => new ModalComponent(modalType)
        case Focus.CommandRunner         => new CommandRunnerComponent()

    private def applyComponentResult(result: ComponentResult, state: AppState): AppState =
      result match
        case ComponentResult.NoChange                => state
        case ComponentResult.StateChange(update)     => update(state)
        case ComponentResult.FocusTransfer(newFocus) => state.copy(focus = newFocus)
        case ComponentResult.Dismiss =>
          val newFocus = determineFallbackFocus(state)
          dismissCurrentFocus(state).copy(focus = newFocus)
        case ComponentResult.Composite(results) =>
          results.foldLeft(state)((s, r) => applyComponentResult(r, s))

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
      IO.println(s"TODO: setBufferFilePath($bufferId, $filePath)")

    def saveBuffer(bufferId: BufferId): IO[Unit] =
      IO.println(s"TODO: saveBuffer($bufferId)")

    def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit] =
      IO.println(s"TODO: saveBufferAs($bufferId, $filePath)")

    def markBufferSaved(bufferId: BufferId): IO[Unit] =
      IO.println(s"TODO: markBufferSaved($bufferId)")

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
      IO.println(s"TODO: loadDirectoryTree($path, ${files.size} files)")

    def selectFileInExplorer(filePath: String): IO[Unit] =
      IO.println(s"TODO: selectFileInExplorer($filePath)")

    def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
      IO.println(s"TODO: resizePinnedPanel($position, $newSize)")

    def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit] =
      IO.println(s"TODO: dragFileToDirectory($sourceFile, $targetDir)")

    // Scrolling operation stubs (TODO: implement)
    def ensureCursorVisible(paneId: PaneId): IO[Unit] =
      IO.println(s"TODO: ensureCursorVisible($paneId)")

    def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit] =
      IO.println(s"TODO: smoothScrollTo($paneId, $targetLine)")

    def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit] =
      IO.println(s"TODO: progressSmoothScroll($paneId, $progress)")

    def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
      IO.println(s"TODO: clickMinimap($paneId, $targetLine)")

    // Session operation stubs (TODO: implement)
    def serializeSession(): IO[String] =
      IO.pure("{\"TODO\": \"implement session serialization\"}")

    def restoreSession(sessionData: String): IO[Unit] =
      IO.println(s"TODO: restoreSession($sessionData)")
