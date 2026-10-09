package com.serenity.state.manager

import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.diagnostics.Trace
import com.serenity.keystroke.events.*
import com.serenity.project.ProjectPresence
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.components.*
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.WrappedLineCache
import com.serenity.ui.presets.UiPresetStore

/** Minimal state boundary for resize routing. */
private[manager] trait ResizeEventPort:
  def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit]

/** Routes resize transitions without depending on command, workflow, or runtime services. */
final private[manager] class ResizeEventHandler(
    port: ResizeEventPort,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
):

  def apply(event: ResizeEvent, previousState: AppState): cats.effect.IO[Unit] =
    port.applyReducerResult(EventPipelineTransitions.resized(event, previousState, wrapCache), previousState)

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
    undoRecording: UndoRecording,
    detectProjectPresence: AppState => cats.effect.IO[ProjectPresence] = ProjectTaskStart.presence
)(using balance: com.serenity.rope.Balance):

  import state.*
  import workflow.*

  private val modelCommit = operations.modelCommit

  // The same cache the scene wraps with, so a keystroke and the frame that follows it measure each line once.
  private val wrappedLines = authoritativeScene.wrappedLines

  private val focusHandlers = new FocusHandlerRouting(wrappedLines)

  private def drainPendingOperations: cats.effect.IO[Unit] =
    operations.takeOperations.flatMap {
      case Nil => cats.effect.IO.unit
      case pendingOperations =>
        pendingOperations.traverse_ {
          // Already on the dispatcher: offering through the public `applyEvent` would queue behind this very dispatch
          // and deadlock waiting for it.
          case StateManagerOperation.Event(event) => applyEventOnDispatcher(event)
        } >> drainPendingOperations
    }

  private def interpretEffect(effect: AppEffect): cats.effect.IO[Unit] =
    effect match
      case AppEffect.Undo(UndoEffect.RecordBoundary(entry, grouping)) =>
        undoRecording.recordUndoBoundary(entry, grouping)
      case other =>
        effects.interpretEffect(other) >> drainPendingOperations

  private def interpretCommand(command: com.serenity.command.Command, state: AppState): cats.effect.IO[Unit] =
    effects.interpretCommand(command, state) >> drainPendingOperations

  // Called from lane jobs, off the dispatcher: the result is applied there, and what it enqueues replayed.
  private val lanePort = new EffectLanePort:
    def submitEffect(lane: Lane.Keyed, job: cats.effect.IO[Unit]): cats.effect.IO[Unit] =
      operations.submitEffect(lane, job)
    def dispatchEffectResult(result: EffectResult, onApplied: AppState => cats.effect.IO[Unit]): cats.effect.IO[Unit] =
      dispatch(modelCommit.applyResult(result, onApplied, effects.interpretEffect))

  private val commandRunnerLoads = new CommandRunnerOpeningLoads(uiPresetStore, detectProjectPresence, lanePort, logger)

  private val resizePort = new ResizeEventPort:
    def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
      StateManagerEventPipeline.this.applyReducerResult(result, fallbackState)

  private val resizeEvents = new ResizeEventHandler(resizePort, wrappedLines)

  private val lspDocumentSync = new LspDocumentSync(
    LspDocumentSyncPort(
      currentState = modelCommit.currentState,
      interpretEffect = effects.interpretEffect,
      candidateLspBufferIds = StateManagerEventPipeline.candidateLspBufferIds
    )
  )

  private val editorMouseTargeting = new EditorMouseTargeting(
    EditorMouseTargetingPort(
      mouseTargetCacheRef = state.mouseTargetCacheRef,
      authoritativeScene = state.authoritativeScene
    )
  )

  private val modalMouseHitTesting = new ModalMouseHitTesting(
    ModalMouseHitTestingPort(currentState = modelCommit.currentState, applyReducerResult = applyReducerResult)
  )

  private val startupPageMouseHitTesting = new StartupPageMouseHitTesting(
    StartupPageMouseHitTestingPort(currentState = modelCommit.currentState, applyReducerResult = applyReducerResult)
  )

  private val editorContextMenuHitTesting = new EditorContextMenuHitTesting(
    EditorContextMenuHitTestingPort(
      currentState = modelCommit.currentState,
      applyReducerResult = applyReducerResult,
      resolveMouseTarget = editorMouseTargeting.resolveMouseTarget,
      spellingItems = (state, buffer, at) =>
        SpellChecker
          .misspellingAt(state, buffer, at)
          .traverse(found => operations.spellingSuggestions(found.word).map(SpellingMenu.items(found, _)))
          .map(_.getOrElse(Nil))
    )
  )

  private val contextualToolbarHitTesting = new ContextualToolbarHitTesting(
    ContextualToolbarHitTestingPort(
      currentState = modelCommit.currentState,
      applyReducerResult = applyReducerResult,
      authoritativeScene = state.authoritativeScene
    )
  )

  private val commandRunnerMouseHitTesting = new CommandRunnerMouseHitTesting(
    CommandRunnerMouseHitTestingPort(
      currentState = modelCommit.currentState,
      applyReducerResult = applyReducerResult,
      authoritativeScene = state.authoritativeScene
    )
  )

  private val pinnedPanelMouseHitTesting = new PinnedPanelMouseHitTesting(
    PinnedPanelMouseHitTestingPort(
      currentState = modelCommit.currentState,
      applyComponentResult = applyComponentResult,
      commitState = modelCommit.commitState,
      updateConfig = updateConfig,
      resizePinnedPanel = resizePinnedPanel,
      authoritativeScene = state.authoritativeScene
    )
  )

  private val commentLensMouseHitTesting = new CommentLensMouseHitTesting(
    CommentLensMouseHitTestingPort(
      currentState = modelCommit.currentState,
      applyReducerResult = applyReducerResult,
      authoritativeScene = state.authoritativeScene
    )
  )

  private val tabBarDragHitTesting = new TabBarDragHitTesting(
    TabBarDragHitTestingPort(currentState = modelCommit.currentState, applyReducerResult = applyReducerResult)
  )

  private val mouseHitTesting = new MouseHitTesting(
    MouseHitTestingPort(
      currentState = modelCommit.currentState,
      applyReducerResult = applyReducerResult,
      authoritativeScene = state.authoritativeScene
    ),
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

  private[manager] def applyEventOnDispatcher(event: Event): cats.effect.IO[Unit] =
    given org.typelevel.log4cats.Logger[cats.effect.IO] = logger
    def eventLabel                                      = s"event.${event.getClass.getSimpleName}"
    Trace.timed(eventLabel) {
      cats.effect.IO.monotonic.product(modelCommit.model).flatMap { (now, model) =>
        typedRunStep(event, model, now.toNanos) match
          case Some(typed) => commitTypedRun(model, typed)
          case None        => dispatchGeneral(event, eventLabel, now.toNanos, model.app)
      }
    }

  private def dispatchGeneral(event: Event, eventLabel: String, nowNanos: Long, rawState: AppState)(using
    org.typelevel.log4cats.Logger[cats.effect.IO]
  ): cats.effect.IO[Unit] =
    // Not written back on its own: every handler builds on `prevState`, so the normalised focus and any typing
    // activity land in the event's own commit (and `prepareCommit` normalises every commit anyway).
    val prevState = EventPipelineTransitions.typingObserved(event, nowNanos)(
      EventPipelineTransitions.commandRunnerFocusNormalized(rawState)
    )
    val handleEvent: cats.effect.IO[Unit] =
      if prevState.hasBlockingModal && !allowedWhileBlockingModal(event) then cats.effect.IO.unit
      else Trace.timed(s"$eventLabel.dispatch")(dispatchEvent(event, prevState))
    handleEvent >>
      Trace.timed(s"$eventLabel.enqueueChangedLspDocuments")(
        lspDocumentSync.enqueueChangedLspDocuments(prevState)
      ) >>
      Trace.timed(s"$eventLabel.scheduleMarkdownPreviewCommits")(scheduleMarkdownPreviewCommits(prevState))

  /** The model a character typed into an editor pane commits, before centring, when its only effect is undo
    * bookkeeping; `None` for any other event, which takes the general dispatch. Every typed key goes through here,
    * whether dispatched alone or folded into a batch's run, and [[commitTypedRun]] centres and commits it (#1985).
    *
    * A key joins a run even when it records an undo snapshot: the buffer it leaves is marked `FollowCaret`, so the
    * snapshot holds a viewport waiting for the caret, which undo and redo place when they restore it.
    */
  private[manager] def typedRunStep(event: Event, model: Model, nowNanos: Long): Option[Model] =
    typedKeyModel(event, model, nowNanos)

  private def typedKeyModel(event: Event, model: Model, nowNanos: Long): Option[Model] =
    event match
      case key: InsertChar =>
        val prevState = EventPipelineTransitions.typingObserved(event, nowNanos)(
          EventPipelineTransitions.commandRunnerFocusNormalized(model.app)
        )
        if prevState.hasBlockingModal then None
        else
          FocusScopes.peekKeyOutcome(key, prevState) match
            case FocusScopes.PeekKeyOutcome.PassedOn(base) =>
              base.persisted.focus match
                case Focus.EditorPane(paneId) =>
                  new EditorPaneComponent(paneId, wrapCache = wrappedLines)(using balance)
                    .typedWithoutCentring(key, base)
                    .filter(_.effects.forall(ModelCommit.isModelEffect))
                    .flatMap(result =>
                      StateManagerOperationBoundary
                        .prepareCommit(result.state, model.app)
                        .toOption
                        .map(committed =>
                          EventPipelineTransitions
                            .committed(model, result)
                            .copy(app = ViewportResolution.markFollow(model.app, committed))
                        )
                    )
                case _ => None
            case FocusScopes.PeekKeyOutcome.Consumed(_) => None
      case _ => None

  /** Commits a run [[typedRunStep]] folded from `start`, with the cursor centred once for all of its keys. */
  private[manager] def commitTypedRun(start: Model, typed: Model): cats.effect.IO[Unit] =
    val centred =
      typed.copy(app = ViewportResolution.resolve(ViewportResolution.markFollow(start.app, typed.app), wrappedLines))
    modelCommit.commitValidated(start.app)(_ => centred) >>
      lspDocumentSync.enqueueChangedLspDocuments(start.app) >>
      scheduleMarkdownPreviewCommits(start.app)

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
      case wheel: MouseWheel =>
        dispatchWheel(wheel, prevState)
      case mouse: MouseInputEvent if prevState.hasBlockingModal =>
        modalMouseHitTesting.handleModalMouseInput(mouse, prevState)
      case click: MouseClick
          if modalMouseHitTesting.focusedFloatingModalWorkflow(prevState).nonEmpty &&
            modalMouseHitTesting.modalHitAt(click, prevState).nonEmpty =>
        modalMouseHitTesting.handleModalMouseInput(click, prevState)
      case click: MouseClick =>
        afterOutsideModalDismissed(click, prevState)(mouseHitTesting.handleMouseClick(click, _))
      case press: MousePress =>
        afterOutsideModalDismissed(press, prevState)(mouseHitTesting.handleMousePress(press, _))
      case drag: MouseDrag =>
        mouseHitTesting.handleMouseDrag(drag, prevState)
      case move: MouseMove =>
        mouseHitTesting.handleMouseMove(move, prevState)
      case key: (VerticalNavigationEvent | TextEntryEvent | SurfaceEvent) =>
        FocusScopes.peekKeyOutcome(key, prevState) match
          case FocusScopes.PeekKeyOutcome.Consumed(closed)     => modelCommit.commitState(closed, prevState)
          case FocusScopes.PeekKeyOutcome.PassedOn(afterPeeks) => dispatchToFocusedHandler(key, afterPeeks, prevState)

  /** The wheel over a docked list panel scrolls it; anywhere else -- or under a blocking modal, or over a floating
    * surface -- it is the plain scroll the focused component has always had, gated exactly as before.
    */
  private def dispatchWheel(wheel: MouseWheel, prevState: AppState): cats.effect.IO[Unit] =
    val overPanel =
      if prevState.hasBlockingModal || MouseHitTestGeometry.isInsideFloatingSurface(
            wheel,
            prevState,
            authoritativeScene
          )
      then cats.effect.IO.pure(false)
      else pinnedPanelMouseHitTesting.handlePinnedPanelWheel(wheel, prevState)
    overPanel.flatMap { scrolledPanel =>
      if scrolledPanel || (prevState.hasBlockingModal && !allowedWhileBlockingModal(wheel.scroll)) then
        cats.effect.IO.unit
      else dispatchEvent(wheel.scroll, prevState)
    }

  private def afterOutsideModalDismissed(event: MouseInputEvent, prevState: AppState)(
    handle: AppState => cats.effect.IO[Unit]
  ): cats.effect.IO[Unit] =
    def inside(surface: UiSurface): Boolean =
      prevState.runtime.viewportSize.forall(
        MouseHitTestGeometry.insideFloatingSurface(event, prevState, _, surface, authoritativeScene)
      )
    ModalMouseHitTesting.dismissedByOutsideInput(event, prevState, inside) match
      case Some(dismissed) => applyReducerResult(dismissed, prevState) >> modelCommit.currentState.flatMap(handle)
      case None            => handle(prevState)

  /** `base` is `prevState` with whatever the key already did to the peeks; the commit is checked against `prevState`. A
    * surface's unhandled key bubbles out to the editor pane (#1940).
    */
  private def dispatchToFocusedHandler(event: Event, base: AppState, prevState: AppState): cats.effect.IO[Unit] =
    base.persisted.focus match
      case Focus.EditorPane(paneId) => dispatchToEditorPane(event, paneId, base, prevState)
      case focus =>
        val logCommandRunnerEvent =
          focusedCommandRunner(base) match
            case Some(runner) =>
              logger.debug(s"[COMMAND-RUNNER] ${StateManager.describeCommandRunnerEvent(event, runner)}")
            case None =>
              cats.effect.IO.unit

        val result = getLocalHandlerForFocus(focus, base).processEvent(event, base)

        logCommandRunnerEvent >>
          applyComponentResult(result, base).flatMap { handled =>
            FocusScopes.bubbleTarget(focus, result, base).zip(FocusScopes.asEditorEvent(event)) match
              case Some((paneId, editorEvent)) => dispatchToEditorPane(editorEvent, paneId, handled, prevState)
              case None                        => modelCommit.commitState(handled, prevState)
          }

  private def dispatchToEditorPane(
    event: Event,
    paneId: PaneId,
    base: AppState,
    prevState: AppState
  ): cats.effect.IO[Unit] =
    val verticalGeometry = event match
      case vertical: VerticalNavigationEvent =>
        EditorGeometryProducer.forPane(base, paneId, wrapCache = wrappedLines).map(vertical -> _)
      case _ => None
    verticalGeometry match
      case Some((vertical, geometry)) =>
        val reducedState = EditorEventReducer.reduceVerticalNavigation(vertical, paneId, base, geometry).state
        // #1042 carved vertical nav out to dispatch here directly rather than through EditorPaneComponent, which is the
        // only place that otherwise applies this pass -- without it, MoveUp/MoveDown/ExtendSelectionUp/
        // ExtendSelectionDown move the cursor but never scroll the viewport to follow it.
        modelCommit.commitState(
          CursorViewport.ensureVisibleCursors(base, reducedState, wrapCache = wrappedLines),
          prevState
        )
      case None =>
        val result = new EditorPaneComponent(paneId, wrapCache = wrappedLines)(using balance).processEvent(event, base)
        applyComponentResult(result, base).flatMap(newState => modelCommit.commitState(newState, prevState))

  /** Routed by type alone: `CloseTab` and `Quit` previously had to precede the `GlobalAppEvent` branch. */
  private def dispatchGlobalAppEvent(event: GlobalAppEvent, prevState: AppState): cats.effect.IO[Unit] =
    val registry = CommandRegistry.withToggleUI
    def result   = AppEventReducer.reduce(event, prevState, registry)(using balance)
    def reduced  = applyReducerResult(result, prevState)
    event match
      case CloseTab            => beginCloseAction(CloseScope.Current, prevState)
      case Quit                => beginCloseAction(CloseScope.Quit, prevState)
      case ToggleCommandRunner => openCommandRunner(event, prevState, registry)
      case ToggleContextualToolbar | ToggleShortcutsHelp | ToggleTabList | ToggleRecentFilesInMode | NewTab | NextTab |
          PreviousTab | FileSearch | GoToFile | TogglePanel(_) | SplitPaneHorizontal | SplitPaneVertical | ClosePane |
          _: CloseTabById | MoveTabLeft | MoveTabRight | _: FocusInDirection | ToggleChapterGhosts | OpenChapterNote |
          ToggleNotesPin | _: RunCommand | SettingsPreviewAbandoned | _: ActivateBuffer | _: OpenRecentPath |
          _: OpenRecentFolder =>
        reduced
      case _: CursorPeekModifierPressed | _: CursorPeekModifierReleased | CursorPeekOtherKeyPressed =>
        applyReducerResult(EventPipelineTransitions.withCursorPeekAnchorResolved(result), prevState)

  /** Commits the toggle with project presence `Unchecked` and no presets listed, then starts the reads that fill them
    * in on their lanes (#1911): none of that disk work holds up the key that opened the palette. The project is probed
    * from the state before the toggle, since the palette takes focus from the buffer whose file locates it.
    */
  private def openCommandRunner(
    event: GlobalAppEvent,
    prevState: AppState,
    registry: CommandRegistry
  ): cats.effect.IO[Unit] =
    val unchecked = prevState.copy(runtime = prevState.runtime.copy(projectPresence = ProjectPresence.Unchecked))
    applyReducerResult(AppEventReducer.reduce(event, unchecked, registry)(using balance), prevState) >>
      modelCommit.currentState.flatMap(opened =>
        CommandRunnerOpening.openedBy(prevState, opened).traverse_(commandRunnerLoads.request(_, prevState))
      )

  /** Bumps `markdownPreviewEditGeneration` synchronously for any buffer this event's dispatch changed the content of,
    * provided that buffer currently has a live markdown preview -- and schedules a debounced commit of that generation
    * via the operation boundary (cancel-and-restart, mirroring `scheduleFindSearch`). While a buffer's edit generation
    * and committed generation differ, the renderer reuses its last preview image instead of paying for a fresh
    * flying-saucer layout pass on every keystroke. See `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  private[manager] def scheduleMarkdownPreviewCommits(previousState: AppState): cats.effect.IO[Unit] =
    modelCommit.currentState.flatMap { currentState =>
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
        ) >> modelCommit.currentState.flatMap { committed =>
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

  private[manager] def scheduleDocumentAnalysis(): cats.effect.IO[Unit] =
    operations.scheduleDocumentAnalysis()

  /** `paneId` is per-editor-pane identity, not a closed set of kinds -- unlike the `SurfaceContent`/ `PanelPosition`
    * associations in [[FocusHandlerRouting]], it cannot be pooled, so this branch alone still builds a component per
    * dispatch.
    */
  private def getLocalHandlerForFocus(focus: Focus, state: AppState): LocalEventHandler =
    focus match
      case Focus.EditorPane(paneId) => new EditorPaneComponent(paneId, wrapCache = wrappedLines)(using balance)
      case Focus.Modal =>
        state.topModal match
          case None         => NoOpLocalEventHandler
          case Some(dialog) => focusHandlers.forModalType(ModalEventReducer.modalType(dialog.modal))
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId) match
          case None =>
            NoOpLocalEventHandler
          case Some(surface) =>
            surface.presentation match
              case SurfacePresentation.Docked =>
                state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)) match
                  case Some(position) => focusHandlers.forPinnedPanel(position)
                  case None           => focusHandlers.forSurfaceContent(surface.content)
              case SurfacePresentation.Floating(_, _) =>
                focusHandlers.forSurfaceContent(surface.content)

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

  private[manager] def applyComponentResult(result: ComponentResult, state: AppState): cats.effect.IO[AppState] =
    result match
      case ComponentResult.NoChange            => cats.effect.IO.pure(state)
      case ComponentResult.Unhandled           => cats.effect.IO.pure(state)
      case ComponentResult.StateChange(update) => cats.effect.IO.pure(update(state))
      case ComponentResult.ReducerUpdate(result) =>
        modelCommit.currentState.flatMap(committed => applyReducerResult(result, committed)) >> modelCommit.currentState
      case ComponentResult.FocusTransfer(newFocus) =>
        cats.effect.IO.pure(state.copy(persisted = state.persisted.copy(focus = newFocus)))
      case ComponentResult.Dismiss =>
        cats.effect.IO.pure(EventPipelineTransitions.dismissedToPriorFocus(dismissCurrentFocus(state)))
      case ComponentResult.ExecuteCommand(command) =>
        // The command reads the committed state, so the one built so far commits (validated) first.
        for
          committed    <- modelCommit.currentState
          _            <- modelCommit.commitState(state, committed)
          current      <- modelCommit.currentState
          _            <- interpretCommand(command, current)
          updatedState <- modelCommit.currentState
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
