package com.serenity.state.manager

import java.awt.Color
import java.nio.file.{Files, Path, Paths}

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import fs2.Stream
import com.serenity.animation.{AnimatedCell, AnimationConfig, AnimationState, CellAnimation, CharacterKey, FlowAnimationBuilder, FlowDirection, RgbInterpolator, SweepDirection}
import com.serenity.command.{Command, CommandIntent, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.io.{FileManager, FileUtils}
import com.serenity.keystroke.events.{Copy, Cut, DeleteBackward, DeleteForward, Enter, Event, FileEvent, GlobalAppEvent, InsertChar, MouseClick, NewLine, NextTab, Paste, PreviousTab, Redo, SystemEvent, TabKey, ThemeEvent, Undo}
import com.serenity.state.undo.{HistoryEntry, PendingGroup, UndoState}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.components.*
import com.serenity.state.models.ThemePickerState
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
  def handleViewportResize(newSize: ViewportSize): IO[Unit]
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

  def getRecentFiles: IO[List[java.nio.file.Path]]

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
    val themeManager = AppThemeManager.create
    for
      stateRef      <- Ref.of[IO, AppState](AppState.initial)
      undoRef       <- Ref.of[IO, UndoState](UndoState())
      themeNamesRef <- themeManager.listAvailableThemes
                         .handleErrorWith(_ => IO.pure(Nil))
                         .flatMap(Ref.of[IO, List[String]])
      quitSignal    <- Deferred[IO, Unit]
    yield new StateManagerImpl(
      stateRef,
      undoRef,
      themeNamesRef,
      quitSignal,
      LoggerFactory[IO].getLogger(using LoggerName("com.serenity.state.manager.StateManager")),
      policy,
      themeManager
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
      undoRef: Ref[IO, UndoState],
      themeNamesRef: Ref[IO, List[String]],
      quitSignal: Deferred[IO, Unit],
      logger: Logger[IO],
      policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy(),
      themeManager: AppThemeManager = AppThemeManager.create
  )(using Balance)
      extends StateManager:
    private val fileManager    = new FileManager()
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
        val hasBufferAnimations  = state.buffers.values.exists(_.animations.hasActiveAnimations)
        val hasThemeTransition   = state.themeTransition.isDefined
        val hasSurfaceAnimations = state.surfaceAnimations.nonEmpty
        if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations then IO.pure(false)
        else
          val updatedBuffers = state.buffers.view.mapValues { buffer =>
            buffer.copy(animations = buffer.animations.advanceAllAnimations())
          }.toMap
          val updatedTransition = state.themeTransition.map(_.advance).filterNot(_.isComplete)
          val stateWithAdvancedBuffers = state.copy(buffers = updatedBuffers, themeTransition = updatedTransition)
          val newState = advanceSurfaceAnimations(stateWithAdvancedBuffers)
          val stillActive =
            newState.buffers.values.exists(_.animations.hasActiveAnimations) ||
              newState.themeTransition.isDefined ||
              newState.surfaceAnimations.nonEmpty
          stateRef.set(newState).as(stillActive)
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
          bufferOrder = state.bufferOrder :+ bufferId,
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
      stateRef.get.flatMap { prevState =>
        val handleEvent: IO[Unit] = event match
          case Undo => applyUndo(prevState)
          case Redo => applyRedo(prevState)
          case resize: com.serenity.keystroke.events.ResizeEvent =>
            applyReducerResult(SystemEventReducer.reduce(resize, prevState), prevState) >>
              stateRef.update(s => AppEventReducer.rebalancePanes(s, s.focusedBufferId))
          case systemEvent: SystemEvent =>
            applyReducerResult(SystemEventReducer.reduce(systemEvent, prevState), prevState)
          case com.serenity.keystroke.events.CloseTab =>
            beginCloseAction(CloseScope.Current, prevState)
          case com.serenity.keystroke.events.Quit =>
            beginCloseAction(CloseScope.Quit, prevState)
          case appEvent: GlobalAppEvent =>
            val registry = CommandRegistry.withToggleUI
            applyReducerResult(AppEventReducer.reduce(appEvent, prevState, registry), prevState) >>
              (appEvent match
                case NextTab     => applyPaneFlowAnimation(SweepDirection.Backward)
                case PreviousTab => applyPaneFlowAnimation(SweepDirection.Forward)
                case _           => IO.unit)
          case themeEvent: ThemeEvent =>
            applyReducerResult(ThemeEventReducer.reduce(themeEvent, prevState), prevState)
          case fileEvent: FileEvent =>
            applyReducerResult(FileEventReducer.reduce(fileEvent, prevState), prevState)
          case click: MouseClick =>
            handleMouseClick(click, prevState)
          case _ =>
            val logCommandRunnerEvent =
              focusedCommandRunner(prevState) match
                case Some(runner) =>
                  logger.info(s"[COMMAND-RUNNER] ${StateManager.describeCommandRunnerEvent(event, runner)}")
                case None =>
                  IO.unit

            val result =
              getTypedLocalHandlerForFocus(prevState.focus, prevState) match
                case Some(handler) =>
                  handler.processEvent(event, prevState)
                case None =>
                  val component = getLegacyComponentForFocus(prevState.focus, prevState)
                  component.processEvent(event, prevState)

            logCommandRunnerEvent >>
              applyComponentResult(result, prevState).flatMap { newState =>
                validateAndUpdateState(newState, prevState)
              }
        handleEvent >> recordUndoableEdit(event, prevState) >> applyAnimationHooks(prevState)
      }

    private def recordUndoableEdit(event: Event, prevState: AppState): IO[Unit] =
      focusedBufferAndPane(prevState) match
        case None => IO.unit
        case Some((bufferId, paneId, buffer)) =>
          val beforeContent = buffer.content
          val beforeCursor  = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
          event match
            case InsertChar(_) | TabKey =>
              undoRef.update { undo =>
                val sameGroup = undo.pendingGroup.exists(g => g.bufferId == bufferId && g.paneId == paneId)
                if sameGroup then undo.clearRedo
                else
                  val flushed   = undo.flushPendingGroup(beforeContent, beforeCursor)
                  val newGroup  = PendingGroup(bufferId, paneId, beforeContent, beforeCursor)
                  flushed.copy(pendingGroup = Some(newGroup), redoStack = Nil)
              }
            case DeleteBackward | DeleteForward | NewLine | Enter =>
              undoRef.update { undo =>
                val flushed = undo.flushPendingGroup(beforeContent, beforeCursor)
                val entry   = HistoryEntry(bufferId, paneId, beforeContent, beforeCursor)
                flushed.copy(undoStack = entry :: flushed.undoStack, redoStack = Nil)
              }
            case _ => IO.unit

    private def applyUndo(prevState: AppState): IO[Unit] =
      undoRef.get.flatMap { undo =>
        val flushed = undo.pendingGroup match
          case None => undo
          case Some(group) =>
            focusedBufferAndPane(prevState).fold(undo) { (_, _, buffer) =>
              val cur = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
              undo.flushPendingGroup(buffer.content, cur)
            }
        flushed.undoStack match
          case Nil => IO.unit
          case entry :: rest =>
            stateRef.get.flatMap { state =>
              state.buffers.get(entry.bufferId) match
                case None => IO.unit
                case Some(current) =>
                  val currentCursor  = current.cursors.headOption.getOrElse(CursorPosition(0, 0))
                  val redoEntry      = HistoryEntry(entry.bufferId, entry.paneId, current.content, currentCursor)
                  val restoredBuffer = current.copy(
                    content = entry.content,
                    cursors = List(entry.cursor),
                    isDirty = true,
                    viewport = adjustViewportForUndoCursor(current.viewport, entry.cursor)
                  )
                  val snappedState = snapFocusToPane(state, entry.paneId)
                  undoRef.set(flushed.copy(undoStack = rest, redoStack = redoEntry :: flushed.redoStack)) >>
                    validateAndUpdateState(
                      snappedState.copy(buffers = snappedState.buffers + (entry.bufferId -> restoredBuffer)),
                      state
                    )
            }
      }

    private def applyRedo(prevState: AppState): IO[Unit] =
      undoRef.get.flatMap { undo =>
        undo.redoStack match
          case Nil => IO.unit
          case entry :: rest =>
            stateRef.get.flatMap { state =>
              state.buffers.get(entry.bufferId) match
                case None => IO.unit
                case Some(current) =>
                  val currentCursor = current.cursors.headOption.getOrElse(CursorPosition(0, 0))
                  val undoEntry     = HistoryEntry(entry.bufferId, entry.paneId, current.content, currentCursor)
                  val restoredBuffer = current.copy(
                    content = entry.content,
                    cursors = List(entry.cursor),
                    isDirty = true,
                    viewport = adjustViewportForUndoCursor(current.viewport, entry.cursor)
                  )
                  val snappedState = snapFocusToPane(state, entry.paneId)
                  undoRef.set(undo.copy(undoStack = undoEntry :: undo.undoStack, redoStack = rest)) >>
                    validateAndUpdateState(
                      snappedState.copy(buffers = snappedState.buffers + (entry.bufferId -> restoredBuffer)),
                      state
                    )
            }
      }

    private def focusedBufferAndPane(state: AppState): Option[(BufferId, PaneId, Buffer)] =
      state.focus match
        case Focus.EditorPane(paneId) =>
          state.layout.editorPanes.get(paneId).flatMap { pane =>
            pane.bufferId.flatMap(state.buffers.get).map(buf => (buf.id, paneId, buf))
          }
        case _ => None

    private def snapFocusToPane(state: AppState, paneId: PaneId): AppState =
      if state.focus == Focus.EditorPane(paneId) then state
      else state.copy(
        focus = Focus.EditorPane(paneId),
        layout = state.layout.copy(activeEditorPaneId = Some(paneId))
      )

    private def adjustViewportForUndoCursor(viewport: Viewport, cursor: CursorPosition): Viewport =
      val half       = viewport.visibleLines / 2
      val newTopLine = math.max(0, cursor.line - half)
      viewport.copy(topLine = newTopLine)

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
                    case SurfaceContent.ThemePicker(_) =>
                      Some(new ThemePickerComponent())
                    case SurfaceContent.FileSearch(_) =>
                      Some(new FileSearchComponent())
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

    private def applyPaneFlowAnimation(sweep: SweepDirection): IO[Unit] =
      stateRef.get.flatMap { state =>
        val steps = AnimationConfig.smooth.get.steps
        val animOpt = for
          paneId <- state.layout.activeEditorPaneId
          pane   <- state.layout.editorPanes.get(paneId)
          buffId <- pane.bufferId
          buffer <- state.buffers.get(buffId)
          vp      = buffer.viewport
          cells   = (vp.topLine until (vp.topLine + vp.visibleLines)).flatMap { lineIdx =>
            val line = buffer.content.getLine(lineIdx).getOrElse("")
            line.zipWithIndex.take(vp.visibleColumns).map { (ch, col) =>
              CharacterKey(col, lineIdx) -> CellAnimation(ch, state.theme.background, state.theme.foreground)
            }
          }.toMap
          if cells.nonEmpty
        yield
          val animated  = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, sweep, steps)
          val newAnims  = buffer.animations.clearAll().mergeAnimations(animated)
          state.copy(buffers = state.buffers.updated(buffId, buffer.copy(animations = newAnims)))
        animOpt match
          case Some(newState) => stateRef.set(newState)
          case None           => IO.unit
      }

    private def applyAnimationHooks(prevState: AppState): IO[Unit] =
      stateRef.get.flatMap { currentState =>
        val runnerOpened = prevState.commandRunnerSurface.isEmpty && currentState.commandRunnerSurface.isDefined
        val runnerClosed = prevState.commandRunnerSurface.isDefined && currentState.commandRunnerSurface.isEmpty
        (if runnerOpened then
          applyCommandRunnerOpenAnimation(currentState.commandRunnerSurface.get, currentState)
        else IO.unit) >>
        (if runnerClosed then
          applyCommandRunnerCloseAnimation(prevState.commandRunnerSurface.get, prevState, currentState)
        else IO.unit)
      }

    private def withUpdatedRunnerConfig(state: AppState, config: com.serenity.config.AppConfig): AppState =
      state.commandRunnerSurface match
        case Some(surface) =>
          surface.content match
            case SurfaceContent.CommandPalette(runner) =>
              val updatedRunner  = runner.updateInputItems(config)
              val updatedSurface = surface.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)
            case _ => state
        case None => state

    private def applyCommandRunnerOpenAnimation(surface: UiSurface, state: AppState): IO[Unit] =
      val steps = AnimationConfig.smooth.get.steps
      stateRef.update { s =>
        val tSize = s.viewportSize.getOrElse(ViewportSize(80, 24))
        val layout = LayoutEngine.calculateLayoutWithUI(s, tSize)
        val overlayHeight = layout.belowCursorOverlayRect.map(_.height).getOrElse(4)
        val overlayFadeIn = (0 until overlayHeight).map { rowOffset =>
          val delay    = rowOffset
          val panelBg  = s.theme.panel.background.toColor()
          val panelFg  = s.theme.panel.foreground.toColor()
          val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
          val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
          val bgSteps  = List.fill(delay)(transpBg) ++
            RgbInterpolator.interpolateRgba(transpBg, panelBg, steps)
          val fgSteps  = List.fill(delay)(transpFg) ++
            RgbInterpolator.interpolateRgba(transpFg, panelFg, steps)
          CharacterKey(0, rowOffset) -> AnimatedCell(
            content = None,
            foregroundSteps = fgSteps,
            backgroundSteps = bgSteps
          )
        }.toMap
        val surfAnim = SurfaceAnimationState(
          phase = SurfacePhase.Visible,
          animationState = AnimationState(overlayFadeIn),
          overlayHeight = overlayHeight,
          bufferFadeLength = 0,
          phaseTick = 0
        )
        s.copy(surfaceAnimations = s.surfaceAnimations + (surface.id -> surfAnim))
      }

    private def applyCommandRunnerCloseAnimation(
      closedSurface: UiSurface,
      prevState: AppState,
      currentState: AppState
    ): IO[Unit] =
      val steps = AnimationConfig.smooth.get.steps
      stateRef.update { s =>
        val tSize = prevState.viewportSize.orElse(s.viewportSize).getOrElse(ViewportSize(80, 24))
        val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
        val overlayHeight = prevState.surfaceAnimations
          .get(closedSurface.id)
          .map(_.overlayHeight)
          .orElse(previousLayout.belowCursorOverlayRect.map(_.height))
          .getOrElse(4)
        val cachedRect = previousLayout.belowCursorOverlayRect
          .getOrElse(LayoutRect(12, 2, 56, overlayHeight))
        val overlayFadeOutAnims = (0 until overlayHeight).map { rowOffset =>
          val delay      = overlayHeight - 1 - rowOffset
          val panelBg    = s.theme.panel.background.toColor()
          val panelFg    = s.theme.panel.foreground.toColor()
          val transpBg   = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
          val transpFg   = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
          val bgSteps    = List.fill(delay)(panelBg) ++
            RgbInterpolator.interpolateRgba(panelBg, transpBg, steps)
          val fgSteps    = List.fill(delay)(panelFg) ++
            RgbInterpolator.interpolateRgba(panelFg, transpFg, steps)
          CharacterKey(0, rowOffset) -> AnimatedCell(
            content = None,
            foregroundSteps = fgSteps,
            backgroundSteps = bgSteps
          )
        }.toMap
        val (stateWithId, ghostId) = s.allocateSurfaceId
        val ghostSurface = UiSurface(
          id = ghostId,
          content = SurfaceContent.GhostOverlay(closedSurface.content, cachedRect),
          presentation = closedSurface.presentation
        )
        val ghostAnimState = SurfaceAnimationState(
          phase = SurfacePhase.Exiting,
          animationState = AnimationState(overlayFadeOutAnims),
          overlayHeight = overlayHeight,
          bufferFadeLength = 0,
          phaseTick = 0
        )
        stateWithId.copy(
          uiSurfaces = stateWithId.uiSurfaces :+ ghostSurface,
          surfaceAnimations = stateWithId.surfaceAnimations
            - closedSurface.id
            + (ghostId -> ghostAnimState)
        )
      }

    private def buildBufferFadeOut(state: AppState, steps: Int): AppState =
      (for
        paneId   <- state.layout.activeEditorPaneId
        pane     <- state.layout.editorPanes.get(paneId)
        bufferId <- pane.bufferId
        buffer   <- state.buffers.get(bufferId)
      yield
        val vp = buffer.viewport
        val theme = state.theme
        val lineRange = vp.topLine until math.min(vp.topLine + vp.visibleLines, buffer.content.lineCount)
        val newAnims = lineRange.flatMap { bufferLine =>
          val lineContent = buffer.content.getLine(bufferLine).getOrElse("")
          (vp.leftColumn until (vp.leftColumn + vp.visibleColumns)).map { bufferCol =>
            val char = if bufferCol < lineContent.length then lineContent(bufferCol) else ' '
            CharacterKey(bufferCol, bufferLine) -> AnimatedCell(
              content = Some(char),
              foregroundSteps = RgbInterpolator.interpolate(theme.foreground, theme.background, steps).map(_.toColor()),
              backgroundSteps = List.empty
            )
          }
        }.toMap
        if newAnims.isEmpty then state
        else
          val updatedBuffer = buffer.copy(animations = buffer.animations.mergeAnimations(newAnims))
          state.copy(buffers = state.buffers.updated(bufferId, updatedBuffer))
      ).getOrElse(state)

    private def buildBufferFadeIn(state: AppState, delayTicks: Int, steps: Int): AppState =
      (for
        paneId   <- state.layout.activeEditorPaneId
        pane     <- state.layout.editorPanes.get(paneId)
        bufferId <- pane.bufferId
        buffer   <- state.buffers.get(bufferId)
      yield
        val vp = buffer.viewport
        val theme = state.theme
        val lineRange = vp.topLine until math.min(vp.topLine + vp.visibleLines, buffer.content.lineCount)
        val newAnims = lineRange.flatMap { bufferLine =>
          val lineContent = buffer.content.getLine(bufferLine).getOrElse("")
          (vp.leftColumn until (vp.leftColumn + vp.visibleColumns)).map { bufferCol =>
            val char = if bufferCol < lineContent.length then lineContent(bufferCol) else ' '
            CharacterKey(bufferCol, bufferLine) -> AnimatedCell(
              content = Some(char),
              foregroundSteps = List.fill(delayTicks)(theme.background.toColor()) ++
                RgbInterpolator.interpolate(theme.background, theme.foreground, steps).map(_.toColor()),
              backgroundSteps = List.empty
            )
          }
        }.toMap
        if newAnims.isEmpty then state
        else
          val updatedBuffer = buffer.copy(animations = buffer.animations.mergeAnimations(newAnims))
          state.copy(buffers = state.buffers.updated(bufferId, updatedBuffer))
      ).getOrElse(state)

    private def advanceSurfaceAnimations(state: AppState): AppState =
      state.surfaceAnimations.foldLeft(state) { case (s, (surfaceId, surfAnim)) =>
        surfAnim.phase match
          case SurfacePhase.BufferFadingOut =>
            val newTick = surfAnim.phaseTick + 1
            if newTick >= surfAnim.bufferFadeLength then
              val overlayFadeIn = (0 until surfAnim.overlayHeight).map { rowOffset =>
                val delay    = rowOffset
                val panelBg  = s.theme.panel.background.toColor()
                val panelFg  = s.theme.panel.foreground.toColor()
                val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
                val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
                val bgSteps  = List.fill(delay)(transpBg) ++
                  RgbInterpolator.interpolateRgba(transpBg, panelBg, AnimationConfig.smooth.get.steps)
                val fgSteps  = List.fill(delay)(transpFg) ++
                  RgbInterpolator.interpolateRgba(transpFg, panelFg, AnimationConfig.smooth.get.steps)
                CharacterKey(0, rowOffset) -> AnimatedCell(
                  content = None,
                  foregroundSteps = fgSteps,
                  backgroundSteps = bgSteps
                )
              }.toMap
              val newSurfAnim = surfAnim.copy(
                phase = SurfacePhase.Visible,
                animationState = AnimationState(overlayFadeIn),
                phaseTick = 0
              )
              s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> newSurfAnim))
            else
              s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> surfAnim.copy(phaseTick = newTick)))

          case SurfacePhase.Visible =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then
              s.copy(surfaceAnimations = s.surfaceAnimations - surfaceId)
            else
              s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState)))

          case SurfacePhase.Exiting =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then
              s.copy(
                uiSurfaces = s.uiSurfaces.filterNot(_.id == surfaceId),
                surfaceAnimations = s.surfaceAnimations - surfaceId
              )
            else
              s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState)))
      }

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
        case AppEffect.OpenThemePicker =>
          stateRef.get.flatMap(openThemePickerEffect)
        case AppEffect.OpenFileSearch =>
          stateRef.get.flatMap(openFileSearchEffect)
        case AppEffect.RequestOpenFile =>
          stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.Open, state))
        case AppEffect.RequestSaveAs =>
          stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.SaveAs, state))
        case AppEffect.DirectLoadFile(path) =>
          directLoadFileEffect(path)
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

    private def handleMouseClick(click: MouseClick, state: AppState): IO[Unit] =
      state.viewportSize match
        case None => IO.unit
        case Some(tSize) =>
          val layout     = LayoutEngine.calculateLayoutWithUI(state, tSize)
          val paneLayouts = LayoutEngine.calculatePaneLayouts(state, layout)
          paneLayouts.find { case (_, rect) =>
            click.col >= rect.x && click.col < rect.x + rect.width &&
            click.row > rect.y && click.row < rect.y + rect.height
          } match
            case Some((paneId, paneRect)) =>
              state.layout.editorPanes.get(paneId).flatMap(pane => pane.bufferId.flatMap(state.buffers.get)) match
                case Some(buffer) =>
                  val vp         = buffer.viewport
                  val contentY   = paneRect.y + 1
                  val bufferLine = (vp.topLine + (click.row - contentY)).max(0)
                  val bufferCol  = (vp.leftColumn + (click.col - paneRect.x)).max(0)
                  val clampedLine = bufferLine.min(math.max(0, buffer.content.lineCount - 1))
                  val lineLen    = buffer.content.getLine(clampedLine).getOrElse("").length
                  val clampedCol = bufferCol.min(lineLen)
                  stateRef.update { s =>
                    s.buffers.get(buffer.id) match
                      case Some(current) =>
                        s.copy(
                          buffers = s.buffers.updated(
                            buffer.id,
                            current.copy(cursors = List(CursorPosition(clampedLine, clampedCol)))
                          ),
                          focus = Focus.EditorPane(paneId),
                          layout = s.layout.copy(activeEditorPaneId = Some(paneId))
                        )
                      case None => s
                  }
                case None =>
                  stateRef.update(s => s.copy(
                    focus = Focus.EditorPane(paneId),
                    layout = s.layout.copy(activeEditorPaneId = Some(paneId))
                  ))
            case None => IO.unit

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
        case CommandIntent.OpenThemeChooser =>
          openThemePickerEffect(state)
        case CommandIntent.ReloadThemes =>
          themeManager.listAvailableThemes
            .flatMap(themeNamesRef.set)
            .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))
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
        case CommandIntent.SetBlurRadius(r) =>
          updateState { s =>
            val newConfig = s.config.withBlurRadius(r)
            withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
          }
        case CommandIntent.SetAnimationDuration(ms) =>
          updateState { s =>
            val newAnim =
              if ms <= 0 then None
              else Some(s.config.characterAnimation.fold(
                AnimationConfig(steps = 12, totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
              )(existing => existing.copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))))
            val newConfig = s.config.copy(characterAnimation = newAnim)
            withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
          }
        case CommandIntent.SetAnimationSteps(n) =>
          updateState { s =>
            val newAnim =
              if n <= 0 then None
              else Some(s.config.characterAnimation.fold(
                AnimationConfig(steps = n, totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000L))
              )(existing => existing.copy(steps = n)))
            val newConfig = s.config.copy(characterAnimation = newAnim)
            withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
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
      val rootPath = Path.of(path)
      val entries = files.map { name =>
        val isDir = name.endsWith("/")
        DirEntry(rootPath.resolve(name), name, isDirectory = isDir)
      }
      val tree    = DirectoryTreeData(rootPath, entries = Map(rootPath -> entries))
      val content = PanelContent.DirectoryTree(tree, selectedPath = None)
      pinPanel(content, PanelPosition.Left, 30)

    def selectFileInExplorer(filePath: String): IO[Unit] =
      val targetPath = Path.of(filePath)
      stateRef.get.flatMap { state =>
        val updated = state.pinnedSurfaces.find { surface =>
          surface.presentation match
            case SurfacePresentation.Pinned(PanelPosition.Left, _) => true
            case _                                                  => false
        }.flatMap { surface =>
          surface.content match
            case SurfaceContent.DirectoryListing(rootPath, entries, _) =>
              val newContent  = SurfaceContent.DirectoryListing(rootPath, entries, Some(targetPath))
              val newSurface  = surface.copy(content = newContent)
              Some(state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ newSurface))
            case _ => None
        }.getOrElse(state)
        validateAndUpdateState(updated, state)
      }

    def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
      stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.resize(position, newSize, state).state, state))

    def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit] =
      val src    = Path.of(sourceFile)
      val dst    = Path.of(targetDir).resolve(src.getFileName)
      val srcDir = src.getParent
      IO.blocking(Files.move(src, dst)).flatMap { _ =>
        stateRef.update { state =>
          state.pinnedSurfaces.foldLeft(state) { (s, surface) =>
            surface.content match
              case SurfaceContent.DirectoryListing(root, entries, sel) if root == srcDir =>
                val updated = surface.copy(
                  content = SurfaceContent.DirectoryListing(root, entries.filterNot(_.path == src), sel)
                )
                s.copy(uiSurfaces = s.uiSurfaces.filterNot(_.id == surface.id) :+ updated)
              case _ => s
          }
        }
      }.handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to move $sourceFile to $targetDir"))

    def getRecentFiles: IO[List[java.nio.file.Path]] =
      stateRef.get.map(_.recentFiles)

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

    def handleViewportResize(newSize: ViewportSize): IO[Unit] =
      for
        _ <- logger.debug(s"Handling viewport resize to ${newSize.width}x${newSize.height}")
        currentState <- stateRef.get
        resizedState = SystemEventReducer.reduce(com.serenity.keystroke.events.ResizeEvent(newSize), currentState).state
        rebalancedState = AppEventReducer.rebalancePanes(resizedState, resizedState.focusedBufferId)
        _ <- validateAndUpdateState(rebalancedState, currentState)
      yield ()

    private def directLoadFileEffect(path: Path): IO[Unit] =
      if !FileUtils.isReadableFile(path) then
        logger.debug(s"[FILE] DirectLoad: file not readable: $path")
      else
        fileManager
          .loadFile(path)
          .flatMap { loadedBuffer =>
            stateRef.modify { state =>
              val newBufferId    = state.nextBufferId
              val bufferToInsert = loadedBuffer.copy(id = newBufferId)
              val updatedState   = state.copy(
                buffers      = state.buffers + (newBufferId -> bufferToInsert),
                bufferOrder  = insertBufferInOrder(state, newBufferId),
                nextBufferId = BufferId(newBufferId.value + 1)
              )
              val rebalanced = AppEventReducer.rebalancePanes(updatedState, Some(newBufferId))
              val focused    = focusBuffer(rebalanced, newBufferId)
              (focused, ())
            }
          }
          .flatTap(_ => stateRef.update(s => s.copy(recentFiles = trackRecentFile(s.recentFiles, path))))
          .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load file at $path"))

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
              .flatTap(_ => stateRef.update(s => s.copy(recentFiles = trackRecentFile(s.recentFiles, path))))
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
        .flatMap { newTheme =>
          updateState { state =>
            val transition =
              if state.theme == newTheme then None
              else Some(ThemeTransition(state.theme, 0, AnimationConfig.smooth.get.steps))
            state.copy(theme = newTheme, themeTransition = transition)
          }
        }
        .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

    private def reloadThemeByName(themeName: String): IO[Unit] =
      themeManager
        .loadTheme(themeName)
        .flatMap(theme => updateState(_.copy(theme = theme)))
        .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

    private def openThemePickerEffect(state: AppState): IO[Unit] =
      themeNamesRef.get.flatMap { themeNames =>
        if themeNames.isEmpty then IO.unit
        else
          val currentTheme  = state.theme.name
          val selectedIndex = themeNames.indexOf(currentTheme).max(0)
          val pickerState   = ThemePickerState(themeNames, selectedIndex, currentTheme)
          val (stateWithId, surfaceId) = state.allocateSurfaceId
          val surface = UiSurface(
            id           = surfaceId,
            content      = SurfaceContent.ThemePicker(pickerState),
            presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
          )
          validateAndUpdateState(
            stateWithId.copy(
              uiSurfaces = stateWithId.uiSurfaces :+ surface,
              focus      = Focus.Surface(surfaceId)
            ),
            state
          )
      }

    private def openFileSearchEffect(state: AppState): IO[Unit] =
      val (stateWithId, surfaceId) = state.allocateSurfaceId
      val surface = UiSurface(
        id           = surfaceId,
        content      = SurfaceContent.FileSearch(FileSearchState("", Nil, 0)),
        presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
      )
      validateAndUpdateState(
        stateWithId.copy(
          uiSurfaces = stateWithId.uiSurfaces :+ surface,
          focus      = Focus.Surface(surfaceId)
        ),
        state
      )

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

    private def trackRecentFile(current: List[java.nio.file.Path], path: java.nio.file.Path): List[java.nio.file.Path] =
      (path :: current.filterNot(_ == path)).take(20)

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
