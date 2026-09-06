package com.serenity.state.manager

import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.presets.UiPreset

/** Minimal state boundary for resize routing. */
private[manager] trait ResizeEventPort:
  def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit]
  def rebalancePanes(): cats.effect.IO[Unit]

/** Routes resize transitions without depending on command, workflow, or runtime services. */
final private[manager] class ResizeEventHandler(port: ResizeEventPort):

  def apply(event: ResizeEvent, previousState: AppState): cats.effect.IO[Unit] =
    port.applyReducerResult(SystemEventReducer.reduce(event, previousState), previousState) >>
      port.rebalancePanes()

private[manager] object StateManagerEventPipeline:

  private def focusedBufferId(state: AppState): Option[BufferId] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) => state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _                        => None

  /** The buffer(s) a single event dispatch could plausibly have changed: the focused buffer before dispatch, and the
    * focused buffer after (covering focus-snapping events like undo/redo). Every content-mutating reducer in this
    * codebase updates exactly one buffer per event -- always the one it's targeting via focus -- so checking these two
    * candidates instead of every open buffer is exhaustive, not a heuristic.
    */
  private[manager] def candidateLspBufferIds(previousState: AppState, currentState: AppState): Set[BufferId] =
    Set(focusedBufferId(previousState), focusedBufferId(currentState)).flatten

final private[manager] class StateManagerEventPipeline(
    state: EventStatePort,
    effects: EventEffectPort,
    workflow: EventWorkflowPort,
    ui: EventUiPort,
    operations: StateManagerOperationBoundary
)(using balance: com.serenity.rope.Balance):

  import state.*
  import ui.*
  import workflow.*

  private def drainPendingOperations: cats.effect.IO[Unit] =
    operations.takeOperations.flatMap {
      case Nil => cats.effect.IO.unit
      case pendingOperations =>
        pendingOperations.traverse_ {
          case StateManagerOperation.Event(event)                       => applyEvent(event)
          case StateManagerOperation.ApplyAnimationHooks(previousState) => applyAnimationHooks(previousState)
        } >> drainPendingOperations
    }

  private def interpretEffect(effect: AppEffect): cats.effect.IO[Unit] =
    effects.interpretEffect(effect) >> drainPendingOperations

  private def interpretCommand(command: com.serenity.command.Command, state: AppState): cats.effect.IO[Unit] =
    effects.interpretCommand(command, state) >> drainPendingOperations

  private def executeCommand(command: com.serenity.command.Command): cats.effect.IO[Unit] =
    effects.executeCommand(command) >> drainPendingOperations

  private val resizeEvents = new ResizeEventHandler(new ResizeEventPort:
    def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.applyReducerResult(result, fallbackState)
    def rebalancePanes(): cats.effect.IO[Unit] =
      stateRef.update(s => AppEventReducer.rebalancePanes(s, s.focusedBufferId)))

  private val undoRecording = new UndoRecording(new UndoRecordingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState]                         = state.stateRef
    def undoRef: cats.effect.Ref[cats.effect.IO, com.serenity.state.undo.UndoState] = state.undoRef
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.validateAndUpdateState(newState, fallbackState))

  private val lspDocumentSync = new LspDocumentSync(new LspDocumentSyncPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState]      = state.stateRef
    def interpretEffect(effect: AppEffect): cats.effect.IO[Unit] = effects.interpretEffect(effect)
    def candidateLspBufferIds(previousState: AppState, currentState: AppState): Set[BufferId] =
      StateManagerEventPipeline.candidateLspBufferIds(previousState, currentState))

  private val animations = new AnimationChoreography(new AnimationChoreographyPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def bufferAnimationsRef: cats.effect.Ref[cats.effect.IO, Map[BufferId, AnimationState]] =
      state.bufferAnimationsRef)

  private val editorMouseTargeting = new EditorMouseTargeting(new EditorMouseTargetingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState]                            = state.stateRef
    def mouseTargetCacheRef: cats.effect.Ref[cats.effect.IO, Option[MouseTargetCache]] = state.mouseTargetCacheRef)

  private val modalMouseHitTesting = new ModalMouseHitTesting(new ModalMouseHitTestingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.applyReducerResult(result, fallbackState))

  private val startupPageMouseHitTesting = new StartupPageMouseHitTesting(
    new StartupPageMouseHitTestingPort:
      def executeCommand(command: com.serenity.command.Command): cats.effect.IO[Unit] =
        StateManagerEventPipeline.this.executeCommand(command)
  )

  private val editorContextMenuHitTesting = new EditorContextMenuHitTesting(new EditorContextMenuHitTestingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def executeCommand(command: com.serenity.command.Command): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.executeCommand(command)
    def resolveMouseTarget(
      click: MouseInputEvent,
      state: AppState
    ): cats.effect.IO[Option[(PaneId, Buffer, CursorPosition)]] =
      editorMouseTargeting.resolveMouseTarget(click, state))

  private val contextualToolbarHitTesting = new ContextualToolbarHitTesting(new ContextualToolbarHitTestingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def executeCommand(command: com.serenity.command.Command): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.executeCommand(command))

  private val commandRunnerMouseHitTesting = new CommandRunnerMouseHitTesting(new CommandRunnerMouseHitTestingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.applyReducerResult(result, fallbackState))

  private val pinnedPanelMouseHitTesting = new PinnedPanelMouseHitTesting(new PinnedPanelMouseHitTestingPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def applyComponentResult(result: ComponentResult, state: AppState): cats.effect.IO[AppState] =
      StateManagerEventPipeline.this.applyComponentResult(result, state)
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.validateAndUpdateState(newState, fallbackState)
    def updateConfig(
      update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
    ): cats.effect.IO[com.serenity.config.AppConfig] = ui.updateConfig(update)
    def resizePinnedPanel(target: com.serenity.ui.layout.PanelTarget, newSize: Int): cats.effect.IO[Unit] =
      ui.resizePinnedPanel(target, newSize))

  private val commentLensMouseHitTesting = new CommentLensMouseHitTesting(
    new CommentLensMouseHitTestingPort:
      def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
  )

  private val mouseHitTesting = new MouseHitTesting(
    new MouseHitTestingPort:
      def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    ,
    editorMouseTargeting,
    editorContextMenuHitTesting,
    contextualToolbarHitTesting,
    commandRunnerMouseHitTesting,
    pinnedPanelMouseHitTesting,
    startupPageMouseHitTesting,
    commentLensMouseHitTesting
  )

  def applyEvent(event: Event): cats.effect.IO[Unit] =
    given org.typelevel.log4cats.Logger[cats.effect.IO] = logger
    def eventLabel                                      = s"event.${event.getClass.getSimpleName}"
    Trace.timed(eventLabel) {
      stateRef.get.flatMap { rawState =>
        val prevState = normalizeCommandRunnerFocus(rawState)
        val syncFocus = if prevState == rawState then cats.effect.IO.unit else stateRef.set(prevState)
        val handleEvent: cats.effect.IO[Unit] =
          if prevState.hasBlockingModal && !allowedWhileBlockingModal(event) then cats.effect.IO.unit
          else Trace.timed(s"$eventLabel.dispatch")(dispatchEvent(event, prevState))
        syncFocus >> handleEvent >>
          undoRecording.recordUndoableEdit(event, prevState) >>
          Trace.timed(s"$eventLabel.enqueueChangedLspDocuments")(
            lspDocumentSync.enqueueChangedLspDocuments(prevState)
          ) >>
          Trace.timed(s"$eventLabel.scheduleMarkdownPreviewCommits")(scheduleMarkdownPreviewCommits(prevState)) >>
          Trace.timed(s"$eventLabel.applyAnimationHooks")(applyAnimationHooks(prevState))
      }
    }

  private def allowedWhileBlockingModal(event: Event): Boolean =
    event match
      case _: SystemEvent | _: MouseInputEvent => true
      case _                                   => ModalInputEvent.fromEvent(event).nonEmpty

  private def dispatchEvent(event: Event, prevState: AppState): cats.effect.IO[Unit] =
    event match
      case Undo                     => undoRecording.applyUndo(prevState)
      case Redo                     => undoRecording.applyRedo(prevState)
      case resize: ResizeEvent      => resizeEvents.apply(resize, prevState)
      case appEvent: GlobalAppEvent => dispatchGlobalAppEvent(appEvent, prevState)
      case systemEvent: SystemEvent =>
        applyReducerResult(SystemEventReducer.reduce(systemEvent, prevState), prevState)
      case themeEvent: ThemeEvent =>
        applyReducerResult(ThemeEventReducer.reduce(themeEvent, prevState), prevState)
      case fileEvent: FileEvent =>
        applyReducerResult(FileEventReducer.reduce(fileEvent, prevState), prevState)
      case mouse: MouseInputEvent if prevState.hasBlockingModal =>
        modalMouseHitTesting.handleModalMouseInput(mouse, prevState)
      case click: MouseClick
          if modalMouseHitTesting.focusedFloatingModalWorkflow(prevState).nonEmpty &&
            modalMouseHitTesting.modalHitAt(click, prevState).nonEmpty =>
        modalMouseHitTesting.handleModalMouseInput(click, prevState)
      case click: MouseClick =>
        mouseHitTesting.handleMouseClick(click, prevState)
      case press: MousePress =>
        mouseHitTesting.handleMousePress(press, prevState)
      case drag: MouseDrag =>
        mouseHitTesting.handleMouseDrag(drag, prevState)
      case move: MouseMove =>
        mouseHitTesting.handleMouseMove(move, prevState)
      case vertical: VerticalNavigationEvent =>
        prevState.persisted.focus match
          case Focus.EditorPane(paneId) =>
            EditorGeometryProducer.forPane(prevState, paneId) match
              case Some(geometry) =>
                val reducedState =
                  EditorEventReducer.reduceVerticalNavigation(vertical, paneId, prevState, geometry).state
                // #1042 carved vertical nav out to dispatch here directly rather than through
                // dispatchToFocusedHandler/EditorPaneComponent, which is the only place that otherwise applies this
                // pass -- without it, MoveUp/MoveDown/ExtendSelectionUp/ExtendSelectionDown move the cursor but never
                // scroll the viewport to follow it.
                validateAndUpdateState(CursorViewport.ensureVisibleCursors(prevState, reducedState), prevState)
              case None => dispatchToFocusedHandler(vertical, prevState)
          case _ => dispatchToFocusedHandler(vertical, prevState)

      case _: (TextEntryEvent | SurfaceEvent) =>
        dispatchToFocusedHandler(event, prevState)

  private def dispatchToFocusedHandler(event: Event, prevState: AppState): cats.effect.IO[Unit] =
    val logCommandRunnerEvent =
      focusedCommandRunner(prevState) match
        case Some(runner) =>
          logger.debug(s"[COMMAND-RUNNER] ${StateManager.describeCommandRunnerEvent(event, runner)}")
        case None =>
          cats.effect.IO.unit

    val result =
      getLocalHandlerForFocus(prevState.persisted.focus, prevState).processEvent(event, prevState)

    logCommandRunnerEvent >>
      applyComponentResult(result, prevState).flatMap(newState => validateAndUpdateState(newState, prevState))

  /** Routed by type alone: `CloseTab` and `Quit` previously had to precede the `GlobalAppEvent` branch. */
  private def dispatchGlobalAppEvent(event: GlobalAppEvent, prevState: AppState): cats.effect.IO[Unit] =
    val registry = CommandRegistry.withToggleUI
    def reduced  = applyReducerResult(AppEventReducer.reduce(event, prevState, registry)(using balance), prevState)
    event match
      case CloseTab            => beginCloseAction(CloseScope.Current, prevState)
      case Quit                => beginCloseAction(CloseScope.Quit, prevState)
      case ToggleCommandRunner => reduced >> hydrateCommandRunnerUiPresets
      case NextTab             => reduced >> applyPaneFlowAnimation(SweepDirection.Backward)
      case PreviousTab         => reduced >> applyPaneFlowAnimation(SweepDirection.Forward)
      case ToggleContextualToolbar | ToggleShortcutsHelp | ToggleTabList | ToggleRecentFilesInMode | NewTab |
          FileSearch | TogglePanel(_) =>
        reduced
      case _: CursorPeekModifierPressed | _: CursorPeekModifierReleased | CursorPeekOtherKeyPressed =>
        // Resolving the frozen cursor anchor to a screen position needs LayoutEngine, which reducers may not touch
        // (ArchitectureChecks.ForbiddenImports) -- done here, once, right after the reduce that may have set
        // cursorPeekAnchor; CursorPeekAnchorResolution.resolve is a no-op unless exactly that just happened.
        reduced >> stateRef.update(CursorPeekAnchorResolution.resolve)

  /** Bumps `markdownPreviewEditGeneration` synchronously for any buffer this event's dispatch changed the content of,
    * provided that buffer currently has a live markdown preview -- and schedules a debounced commit of that generation
    * via the operation boundary (cancel-and-restart, mirroring `scheduleFindSearch`). While a buffer's edit generation
    * and committed generation differ, the renderer reuses its last preview image instead of paying for a fresh
    * flying-saucer layout pass on every keystroke. See `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  private[manager] def scheduleMarkdownPreviewCommits(previousState: AppState): cats.effect.IO[Unit] =
    stateRef.get.flatMap { currentState =>
      StateManagerEventPipeline.candidateLspBufferIds(previousState, currentState).toList.traverse_ { bufferId =>
        currentState.persisted.buffers.get(bufferId) match
          case Some(buffer)
              if hasLiveMarkdownPreview(currentState, bufferId) &&
                previousState.persisted.buffers.get(bufferId).exists(_.document.content != buffer.document.content) =>
            val nextGeneration = buffer.markdownPreviewEditGeneration + 1
            stateRef.update { s =>
              s.persisted.buffers.get(bufferId).fold(s) { b =>
                s.copy(persisted =
                  s.persisted.copy(buffers =
                    s.persisted.buffers.updated(bufferId, b.copy(markdownPreviewEditGeneration = nextGeneration))
                  )
                )
              }
            } >> operations.scheduleMarkdownPreviewCommit(bufferId, nextGeneration)
          case _ => cats.effect.IO.unit
      }
    }

  private[manager] def hasLiveMarkdownPreview(state: AppState, bufferId: BufferId): Boolean =
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.MarkdownPreview(id, _), _, _) => id == bufferId
      case _                                                         => false
    } || (
      state.persisted.config.markdownViewMode == com.serenity.config.MarkdownViewMode.InlineLens &&
        state.persisted.buffers
          .get(bufferId)
          .exists(_.document.language.contains(com.serenity.lsp.config.LanguageId.Markdown))
    )

  private[manager] def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  private[manager] def scheduleDocumentAnalysis(): cats.effect.IO[Unit] =
    operations.scheduleDocumentAnalysis()

  /** `paneId` is per-editor-pane identity, not a closed set of kinds -- unlike the `SurfaceContent`/ `PanelPosition`
    * associations in [[FocusHandlerRouting]], it cannot be pooled, so this branch alone still builds a component per
    * dispatch.
    */
  private def getLocalHandlerForFocus(focus: Focus, state: AppState): LocalEventHandler =
    focus match
      case Focus.EditorPane(paneId) => new EditorPaneComponent(paneId)(using balance)
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId) match
          case None =>
            NoOpLocalEventHandler
          case Some(surface) =>
            surface.presentation match
              case SurfacePresentation.Pinned(position, _)   => FocusHandlerRouting.forPinnedPanel(position)
              case SurfacePresentation.Expanded(position, _) => FocusHandlerRouting.forPinnedPanel(position)
              case SurfacePresentation.Modal | SurfacePresentation.Floating(_, _) =>
                FocusHandlerRouting.forSurfaceContent(surface.content)

  private[manager] def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
    for
      _ <- validateAndUpdateState(result.state, fallbackState)
      _ <- result.effects.traverse_(interpretEffect)
    yield ()

  private def hydrateCommandRunnerUiPresets: cats.effect.IO[Unit] =
    uiPresetStore
      .list()
      .map(_.map(UiPreset.Preview.fromPreset))
      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to list UI presets").map(_ => Nil))
      .flatMap(previews => stateRef.update(state => updateCommandRunnerUiPresetPreviews(state, previews)))

  private def updateCommandRunnerUiPresetPreviews(state: AppState, previews: List[UiPreset.Preview]): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.withUiPresetPreviews(previews)
            val updatedSurfaces = state.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current =>
                current
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  private def normalizeCommandRunnerFocus(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus match
        case Some(focus) => state.copy(persisted = state.persisted.copy(focus = focus))
        case None        => state
    else state

  private def applyPaneFlowAnimation(sweep: SweepDirection): cats.effect.IO[Unit] =
    animations.applyPaneFlowAnimation(sweep)

  private[manager] def applyAnimationHooks(prevState: AppState): cats.effect.IO[Unit] =
    animations.applyAnimationHooks(prevState)

  private[manager] def shouldApplySurfaceAnimationHooks(state: AppState): Boolean =
    animations.shouldApplySurfaceAnimationHooks(state)

  private[manager] def advanceSurfaceAnimations(state: AppState): AppState =
    animations.advanceSurfaceAnimations(state)

  private def applyComponentResult(result: ComponentResult, state: AppState): cats.effect.IO[AppState] =
    result match
      case ComponentResult.NoChange            => cats.effect.IO.pure(state)
      case ComponentResult.StateChange(update) => cats.effect.IO.pure(update(state))
      case ComponentResult.ReducerUpdate(result) =>
        applyReducerResult(result, state) >> stateRef.get
      case ComponentResult.FocusTransfer(newFocus) =>
        cats.effect.IO.pure(state.copy(persisted = state.persisted.copy(focus = newFocus)))
      case ComponentResult.Dismiss =>
        val dismissedState = dismissCurrentFocus(state)
        dismissedState.persisted.layout.activeEditorPaneId match
          case Some(paneId) =>
            cats.effect.IO.pure(
              dismissedState.copy(persisted = dismissedState.persisted.copy(focus = Focus.EditorPane(paneId)))
            )
          case None =>
            for
              _        <- stateRef.set(dismissedState)
              bufferId <- createBuffer("")
              paneId   <- createPane(Some(bufferId))
              newState <- stateRef.get.map(s => s.copy(persisted = s.persisted.copy(focus = Focus.EditorPane(paneId))))
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
    state.persisted.focus match
      case Focus.Surface(surfaceId) =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId)))
      case _ =>
        state

  private def focusedCommandRunner(state: AppState): Option[CommandRunner] =
    state.activeSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }

  private[manager] def ensureCommandRunnerSurface(state: AppState): AppState =
    operations.ensureCommandRunnerSurface(state)
