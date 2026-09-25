package com.serenity.state.manager

import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}

/** Minimal state boundary for resize routing. */
private[manager] trait ResizeEventPort:
  def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit]

/** Routes resize transitions without depending on command, workflow, or runtime services. */
final private[manager] class ResizeEventHandler(port: ResizeEventPort):

  def apply(event: ResizeEvent, previousState: AppState): cats.effect.IO[Unit] =
    port.applyReducerResult(EventPipelineTransitions.resized(event, previousState), previousState)

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
    uiPresetStore: UiPresetStore,
    updateConfig: (com.serenity.config.AppConfig => com.serenity.config.AppConfig) => cats.effect.IO[
      com.serenity.config.AppConfig
    ],
    resizePinnedPanel: (com.serenity.ui.layout.PanelTarget, Int) => cats.effect.IO[Unit],
    operations: StateManagerOperationBoundary,
    undoRecording: UndoRecording
)(using balance: com.serenity.rope.Balance):

  import state.*
  import workflow.*

  private val modelCommit = new ModelCommit(state.modelRef, operations)

  private def drainPendingOperations: cats.effect.IO[Unit] =
    operations.takeOperations.flatMap {
      case Nil => cats.effect.IO.unit
      case pendingOperations =>
        pendingOperations.traverse_ {
          // Already on the dispatcher: offering through the public `applyEvent` would queue behind this very dispatch
          // and deadlock waiting for it.
          case StateManagerOperation.Event(event)                       => applyEventOnDispatcher(event)
          case StateManagerOperation.ApplyAnimationHooks(previousState) => applyAnimationHooks(previousState)
        } >> drainPendingOperations
    }

  private def interpretEffect(effect: AppEffect): cats.effect.IO[Unit] =
    effect match
      case AppEffect.Undo(UndoEffect.RecordBoundary(entry, groupable)) =>
        undoRecording.recordUndoBoundary(entry, groupable)
      case other =>
        effects.interpretEffect(other) >> drainPendingOperations

  private def interpretCommand(command: com.serenity.command.Command, state: AppState): cats.effect.IO[Unit] =
    effects.interpretCommand(command, state) >> drainPendingOperations

  private val resizeEvents = new ResizeEventHandler(
    new ResizeEventPort:
      def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
        StateManagerEventPipeline.this.applyReducerResult(result, fallbackState)
  )

  private val lspDocumentSync = new LspDocumentSync(
    LspDocumentSyncPort(
      currentState = state.stateRef.get,
      interpretEffect = effects.interpretEffect,
      candidateLspBufferIds = StateManagerEventPipeline.candidateLspBufferIds
    )
  )

  private val animations = new AnimationChoreography(new AnimationChoreographyPort:
    def stateRef: cats.effect.Ref[cats.effect.IO, AppState] = state.stateRef
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.validateAndUpdateState(newState, fallbackState))

  private val editorMouseTargeting = new EditorMouseTargeting(
    EditorMouseTargetingPort(mouseTargetCacheRef = state.mouseTargetCacheRef)
  )

  private val modalMouseHitTesting = new ModalMouseHitTesting(
    ModalMouseHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val startupPageMouseHitTesting = new StartupPageMouseHitTesting(
    StartupPageMouseHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val editorContextMenuHitTesting = new EditorContextMenuHitTesting(
    EditorContextMenuHitTestingPort(
      stateRef = state.stateRef,
      applyReducerResult = applyReducerResult,
      resolveMouseTarget = editorMouseTargeting.resolveMouseTarget
    )
  )

  private val contextualToolbarHitTesting = new ContextualToolbarHitTesting(
    ContextualToolbarHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val commandRunnerMouseHitTesting = new CommandRunnerMouseHitTesting(
    CommandRunnerMouseHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val pinnedPanelMouseHitTesting = new PinnedPanelMouseHitTesting(
    PinnedPanelMouseHitTestingPort(
      stateRef = state.stateRef,
      applyComponentResult = applyComponentResult,
      validateAndUpdateState = validateAndUpdateState,
      updateConfig = updateConfig,
      resizePinnedPanel = resizePinnedPanel
    )
  )

  private val commentLensMouseHitTesting = new CommentLensMouseHitTesting(
    CommentLensMouseHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val tabBarDragHitTesting = new TabBarDragHitTesting(
    TabBarDragHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult)
  )

  private val mouseHitTesting = new MouseHitTesting(
    MouseHitTestingPort(stateRef = state.stateRef, applyReducerResult = applyReducerResult),
    editorMouseTargeting,
    editorContextMenuHitTesting,
    contextualToolbarHitTesting,
    commandRunnerMouseHitTesting,
    pinnedPanelMouseHitTesting,
    startupPageMouseHitTesting,
    commentLensMouseHitTesting,
    tabBarDragHitTesting
  )

  /** Offers `event` to the state dispatcher and returns once it has been applied (#1570, #1697). Never called from code
    * already on the dispatcher -- follow-up events enqueued there are replayed by `drainPendingOperations`.
    */
  def applyEvent(event: Event): cats.effect.IO[Unit] =
    operations.dispatch(applyEventOnDispatcher(event))

  /** Runs a state decision on the dispatcher, replaying whatever operations it enqueues there too. */
  def dispatch(decision: cats.effect.IO[Unit]): cats.effect.IO[Unit] =
    operations.dispatch(decision >> drainPendingOperations)

  private def applyEventOnDispatcher(event: Event): cats.effect.IO[Unit] =
    given org.typelevel.log4cats.Logger[cats.effect.IO] = logger
    def eventLabel                                      = s"event.${event.getClass.getSimpleName}"
    Trace.timed(eventLabel) {
      stateRef.get.flatMap { rawState =>
        // Not written back on its own: every handler builds on `prevState`, so the normalised focus lands in the
        // event's own commit (and `prepareCommit` normalises every commit anyway).
        val prevState = EventPipelineTransitions.commandRunnerFocusNormalized(rawState)
        val handleEvent: cats.effect.IO[Unit] =
          if prevState.hasBlockingModal && !allowedWhileBlockingModal(event) then cats.effect.IO.unit
          else Trace.timed(s"$eventLabel.dispatch")(dispatchEvent(event, prevState))
        handleEvent >>
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
    def result   = AppEventReducer.reduce(event, prevState, registry)(using balance)
    def reduced  = applyReducerResult(result, prevState)
    def tabCycled(sweep: SweepDirection) =
      commitReducerResult(result, prevState, EventPipelineTransitions.withPaneFlow(_, sweep))
    event match
      case CloseTab => beginCloseAction(CloseScope.Current, prevState)
      case Quit     => beginCloseAction(CloseScope.Quit, prevState)
      case ToggleCommandRunner =>
        uiPresetPreviews.flatMap(previews =>
          commitReducerResult(
            result,
            prevState,
            EventPipelineTransitions.withCommandRunnerUiPresetPreviews(_, previews)
          )
        )
      case NextTab     => tabCycled(SweepDirection.Backward)
      case PreviousTab => tabCycled(SweepDirection.Forward)
      case ToggleContextualToolbar | ToggleShortcutsHelp | ToggleTabList | ToggleRecentFilesInMode | NewTab |
          FileSearch | TogglePanel(_) | SplitPaneHorizontal | SplitPaneVertical | ClosePane | _: CloseTabById |
          MoveTabLeft | MoveTabRight =>
        reduced
      case _: CursorPeekModifierPressed | _: CursorPeekModifierReleased | CursorPeekOtherKeyPressed =>
        applyReducerResult(EventPipelineTransitions.withCursorPeekAnchorResolved(result), prevState)

  /** Bumps `markdownPreviewEditGeneration` synchronously for any buffer this event's dispatch changed the content of,
    * provided that buffer currently has a live markdown preview -- and schedules a debounced commit of that generation
    * via the operation boundary (cancel-and-restart, mirroring `scheduleFindSearch`). While a buffer's edit generation
    * and committed generation differ, the renderer reuses its last preview image instead of paying for a fresh
    * flying-saucer layout pass on every keystroke. See `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  private[manager] def scheduleMarkdownPreviewCommits(previousState: AppState): cats.effect.IO[Unit] =
    stateRef.get.flatMap { currentState =>
      val edited =
        StateManagerEventPipeline.candidateLspBufferIds(previousState, currentState).toList.filter { bufferId =>
          hasLiveMarkdownPreview(currentState, bufferId) &&
          currentState.persisted.buffers
            .get(bufferId)
            .exists(buffer =>
              previousState.persisted.buffers.get(bufferId).exists(_.document.content != buffer.document.content)
            )
        }
      if edited.isEmpty then cats.effect.IO.unit
      else
        modelCommit.updateValidated(model =>
          Some(model.copy(app = EventPipelineTransitions.withMarkdownPreviewEditsBumped(model.app, edited)))
        ) >> stateRef.get.flatMap { committed =>
          edited.traverse_ { bufferId =>
            val bumped = committed.persisted.buffers
              .get(bufferId)
              .map(_.markdownPreviewEditGeneration)
              .filterNot(currentState.persisted.buffers.get(bufferId).map(_.markdownPreviewEditGeneration).contains)
            bumped.fold(cats.effect.IO.unit)(operations.scheduleMarkdownPreviewCommit(bufferId, _))
          }
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
      case Focus.Modal =>
        state.topModal match
          case None         => NoOpLocalEventHandler
          case Some(dialog) => FocusHandlerRouting.forModalType(ModalMouseHitTesting.modalType(dialog.modal))
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId) match
          case None =>
            NoOpLocalEventHandler
          case Some(surface) =>
            surface.presentation match
              case SurfacePresentation.Docked =>
                state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)) match
                  case Some(position) => FocusHandlerRouting.forPinnedPanel(position)
                  case None           => FocusHandlerRouting.forSurfaceContent(surface.content)
              case SurfacePresentation.Floating(_, _) =>
                FocusHandlerRouting.forSurfaceContent(surface.content)

  private[manager] def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
    commitReducerResult(result, fallbackState, identity)

  /** Commits the result's state together with its model-only effects (animations, undo bookkeeping) and `alongside` in
    * one model write, then interprets its remaining effects in order.
    */
  private def commitReducerResult(
    result: ReducerResult,
    fallbackState: AppState,
    alongside: Model => Model
  ): cats.effect.IO[Unit] =
    for
      _ <- modelCommit.commitValidated(fallbackState)(model =>
        alongside(EventPipelineTransitions.committed(model, result))
      )
      _ <- result.effects.filterNot(ModelCommit.isModelEffect).traverse_(interpretEffect)
    yield ()

  // Listed before the toggle commits so the runner opens with its previews in the same write.
  private def uiPresetPreviews: cats.effect.IO[List[UiPreset.Preview]] =
    uiPresetStore
      .list()
      .map(_.map(UiPreset.Preview.fromPreset))
      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to list UI presets").map(_ => Nil))

  private[manager] def applyAnimationHooks(prevState: AppState): cats.effect.IO[Unit] =
    animations.applyAnimationHooks(prevState)

  private[manager] def shouldApplySurfaceAnimationHooks(state: AppState): Boolean =
    animations.shouldApplySurfaceAnimationHooks(state)

  private[manager] def advanceSurfaceAnimations(state: AppState): AppState =
    animations.advanceSurfaceAnimations(state)

  private[manager] def applyComponentResult(result: ComponentResult, state: AppState): cats.effect.IO[AppState] =
    result match
      case ComponentResult.NoChange            => cats.effect.IO.pure(state)
      case ComponentResult.StateChange(update) => cats.effect.IO.pure(update(state))
      case ComponentResult.ReducerUpdate(result) =>
        stateRef.get.flatMap(committed => applyReducerResult(result, committed)) >> stateRef.get
      case ComponentResult.FocusTransfer(newFocus) =>
        cats.effect.IO.pure(state.copy(persisted = state.persisted.copy(focus = newFocus)))
      case ComponentResult.Dismiss =>
        cats.effect.IO.pure(EventPipelineTransitions.dismissedToEditor(dismissCurrentFocus(state)))
      case ComponentResult.ExecuteCommand(command) =>
        // The command reads the committed state, so the one built so far commits (validated) first.
        for
          committed    <- stateRef.get
          _            <- validateAndUpdateState(state, committed)
          current      <- stateRef.get
          _            <- interpretCommand(command, current)
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
