package com.serenity.state.manager

import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.command.{CommandCategory, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.diagnostics.Trace
import com.serenity.document.CommentRendering
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
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

  private[manager] def modalType(modal: Modal): ModalType =
    modal match
      case Modal.GotoLine(_)        => ModalType.GotoLine
      case Modal.Find(_, _, _)      => ModalType.Find
      case Modal.FileWorkflow(_)    => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_) => ModalType.ReplaceWorkflow
      case Modal.CloseWorkflow(_)   => ModalType.CloseWorkflow
      case Modal.Custom(name, _)    => ModalType.Custom(name)

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
  private val ContextMenuSurfaceId = SurfaceId("context-menu")

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

  private val EditorContextMenuCommands =
    List(
      "copy",
      "cut",
      "paste",
      "select-all",
      "save",
      "save-as",
      "find",
      "replace",
      "bold",
      "italic",
      "underline",
      "heading-1",
      "heading-2",
      "heading-3",
      "paragraph-body",
      "align-left",
      "align-center",
      "align-right",
      "align-justify",
      "goto-line",
      "toggle-bookmark",
      "next-bookmark",
      "previous-bookmark",
      "add-document-comment",
      "delete-document-comment",
      "next-document-comment",
      "previous-document-comment",
      "navigate-back",
      "navigate-forward",
      "next-document-symbol",
      "previous-document-symbol",
      "markdown-preview",
      "pin-outline"
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
        handleModalMouseInput(mouse, prevState)
      case click: MouseClick
          if focusedFloatingModalWorkflow(prevState).nonEmpty && modalHitAt(click, prevState).nonEmpty =>
        handleModalMouseInput(click, prevState)
      case click: MouseClick =>
        handleMouseClick(click, prevState)
      case press: MousePress =>
        handleMousePress(press, prevState)
      case drag: MouseDrag =>
        handleMouseDrag(drag, prevState)
      case move: MouseMove =>
        handleMouseMove(move, prevState)
      case vertical: VerticalNavigationEvent =>
        prevState.persisted.focus match
          case Focus.EditorPane(paneId) =>
            EditorGeometryProducer.forPane(prevState, paneId) match
              case Some(geometry) =>
                validateAndUpdateState(
                  EditorEventReducer.reduceVerticalNavigation(vertical, paneId, prevState, geometry).state,
                  prevState
                )
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
      case CloseTab                                      => beginCloseAction(CloseScope.Current, prevState)
      case Quit                                          => beginCloseAction(CloseScope.Quit, prevState)
      case ToggleCommandRunner                           => reduced >> hydrateCommandRunnerUiPresets
      case NextTab                                       => reduced >> applyPaneFlowAnimation(SweepDirection.Backward)
      case PreviousTab                                   => reduced >> applyPaneFlowAnimation(SweepDirection.Forward)
      case ToggleContextualToolbar | NewTab | FileSearch => reduced

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
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
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

  private def handleMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Unit] =
    click.button match
      case MouseButton.Secondary =>
        if isInsideFloatingSurface(click, state) then cats.effect.IO.unit
        else openEditorContextMenu(click, state)
      case MouseButton.Primary =>
        handleStartupPageMouseClick(click, state).flatMap {
          case true => cats.effect.IO.unit
          case false =>
            handleContextMenuMouseClick(click, state).flatMap {
              case true => cats.effect.IO.unit
              case false =>
                handleContextualToolbarMouseClick(click, state).flatMap {
                  case true => cats.effect.IO.unit
                  case false =>
                    handleCommandRunnerMouseClick(click, state).flatMap {
                      case true => cats.effect.IO.unit
                      case false =>
                        if isInsideFloatingSurface(click, state) then cats.effect.IO.unit
                        else
                          handlePinnedPanelMouseClick(click, state).flatMap {
                            case true => cats.effect.IO.unit
                            case false =>
                              handlePinnedPanelLocationClick(click, state).flatMap {
                                case true => cats.effect.IO.unit
                                case false =>
                                  resolveMouseTarget(click, state).flatMap {
                                    _.fold(dismissContextMenuIfOpen(state)) { (paneId, buffer, clickedCursor) =>
                                      stateRef.update { s =>
                                        s.persisted.buffers.get(buffer.id) match
                                          case Some(current) =>
                                            val selection =
                                              if click.shiftDown then rangeSelectionFromAnchor(current, clickedCursor)
                                              else if click.clickCount >= 3 then
                                                lineSelectionAtCursor(current, clickedCursor)
                                              else if click.clickCount >= 2 then
                                                wordSelectionAtCursor(current, clickedCursor)
                                              else None
                                            val focusCursor = selection.map(_.focus).getOrElse(clickedCursor)
                                            dismissContextMenu(
                                              s.copy(persisted =
                                                s.persisted.copy(
                                                  buffers = s.persisted.buffers.updated(
                                                    buffer.id,
                                                    current.copy(editing =
                                                      current.editing.copy(
                                                        cursors = List(focusCursor),
                                                        selection = selection,
                                                        selections = Nil,
                                                        preferredColumn = Some(focusCursor.column),
                                                        preferredXPx = None,
                                                        multiCursorVerticalStates = Nil
                                                      )
                                                    )
                                                  ),
                                                  focus = Focus.EditorPane(paneId),
                                                  layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                                                )
                                              )
                                            )
                                          case None => dismissContextMenu(s)
                                      }
                                    }
                                  }
                              }
                          }
                    }
                }
            }
        }
      case _ =>
        cats.effect.IO.unit

  private def handleModalMouseInput(event: MouseInputEvent, state: AppState): cats.effect.IO[Unit] =
    event match
      case click: MouseClick if click.button == MouseButton.Primary =>
        modalHitAt(click, state) match
          case Some((modal, hit)) =>
            val modalType = StateManagerEventPipeline.modalType(modal)
            val clicked = ModalEventReducer.reduce(
              modalType,
              ModalClick(hit.focusId.value, hit.actionId.map(_.value)),
              state
            )
            applyReducerResult(clicked, state) >>
              Option
                .when(modalType == ModalType.CloseWorkflow && hit.actionId.nonEmpty)(())
                .fold(
                  cats.effect.IO.unit
                )(_ =>
                  stateRef.get.flatMap { updatedState =>
                    applyReducerResult(
                      ModalEventReducer.reduce(ModalType.CloseWorkflow, ModalSubmit, updatedState),
                      updatedState
                    )
                  }
                )
          case None => cats.effect.IO.unit
      case _ =>
        cats.effect.IO.unit

  private def modalHitAt(click: MouseClick, state: AppState): Option[(Modal, SurfaceHitRegion)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.topModalSurface.orElse(focusedFloatingModalWorkflow(state))
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(surface.id))
      _ <- Option.when(node.frameRect.contains(click.col, click.row))(())
      modal <- surface.content match
        case SurfaceContent.ModalWorkflow(modal) => Some(modal)
        case _                                   => None
      targetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
      hit <- ModalSurfaceComposition
        .forModal(modal, node.frameRect, targetRows)
        .flatMap(_.hitAt(click.col.toDouble, click.row.toDouble))
    yield (modal, hit)

  private def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    for
      surfaceId <- state.persisted.focus match
        case Focus.Surface(id) => Some(id)
        case _                 => None
      surface <- state.runtime.uiSurfaces.find(_.id == surfaceId)
      _ <- surface.presentation match
        case SurfacePresentation.Floating(_, _) => Some(())
        case _                                  => None
      _ <- surface.content match
        case SurfaceContent.ModalWorkflow(_) => Some(())
        case _                               => None
    yield surface

  private def handleStartupPageMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    val action = state.startPageSurface.flatMap { surface =>
      surface.content match
        case SurfaceContent.StartPage(page) =>
          for
            viewportSize <- state.runtime.viewportSize
            pixelX       <- click.pixelX
            pixelY       <- click.pixelY
            metrics      <- click.renderMetrics
            actionIndex  <- page.actionIndexAtPixel(pixelX, pixelY, viewportSize, metrics.code, metrics.ui)
            action       <- page.launchActions.lift(actionIndex)
          yield action
        case _ =>
          None
    }
    action.fold(cats.effect.IO.pure(false))(selected => executeCommand(selected.command).as(true))

  private def handleMousePress(press: MousePress, state: AppState): cats.effect.IO[Unit] =
    if press.button != MouseButton.Primary then cats.effect.IO.unit
    else
      handleContextualToolbarMouseHover(press, state).flatMap {
        case true => cats.effect.IO.unit
        case false =>
          handleCommandRunnerMouseHover(press, state).flatMap {
            case true => cats.effect.IO.unit
            case false =>
              if isInsideFloatingSurface(press, state) then cats.effect.IO.unit
              else
                handlePinnedPanelMouseSelect(press, state, focusPanel = true).flatMap {
                  case true => cats.effect.IO.unit
                  case false =>
                    resolveMouseTarget(press, state).flatMap {
                      _.fold(cats.effect.IO.unit) { (paneId, buffer, pressedCursor) =>
                        stateRef.update { s =>
                          s.persisted.buffers.get(buffer.id) match
                            case Some(current) =>
                              val selection =
                                Option.when(press.shiftDown)(rangeSelectionFromAnchor(current, pressedCursor)).flatten
                              val focusCursor = selection.map(_.focus).getOrElse(pressedCursor)
                              s.copy(persisted =
                                s.persisted.copy(
                                  buffers = s.persisted.buffers.updated(
                                    buffer.id,
                                    current.copy(editing =
                                      current.editing.copy(
                                        cursors = List(focusCursor),
                                        selection = selection,
                                        selections = Nil,
                                        preferredColumn = Some(focusCursor.column),
                                        preferredXPx = None,
                                        multiCursorVerticalStates = Nil
                                      )
                                    )
                                  ),
                                  focus = Focus.EditorPane(paneId),
                                  layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                                )
                              )
                            case None => s
                        }
                      }
                    }
                }
          }
      }

  private def handleMouseDrag(drag: MouseDrag, state: AppState): cats.effect.IO[Unit] =
    if drag.button != MouseButton.Primary then cats.effect.IO.unit
    else
      handleTextAreaResizeDrag(drag, state).flatMap {
        case true => cats.effect.IO.unit
        case false =>
          handlePinnedPanelResizeDrag(drag, state).flatMap {
            case true => cats.effect.IO.unit
            case false =>
              if isInsideFloatingSurface(drag, state) then cats.effect.IO.unit
              else
                resolveMouseTarget(drag, state).flatMap {
                  _.fold(cats.effect.IO.unit) { (paneId, buffer, draggedCursor) =>
                    stateRef.update { s =>
                      s.persisted.buffers.get(buffer.id) match
                        case Some(current) =>
                          val anchor =
                            current.primarySelection
                              .map(_.anchor)
                              .orElse(current.editing.cursors.headOption)
                              .getOrElse(draggedCursor)
                          val selection =
                            Option.when(anchor != draggedCursor)(Selection(anchor, draggedCursor))
                          s.copy(persisted =
                            s.persisted.copy(
                              buffers = s.persisted.buffers.updated(
                                buffer.id,
                                current.copy(editing =
                                  current.editing.copy(
                                    cursors = List(draggedCursor),
                                    selection = selection,
                                    selections = Nil,
                                    preferredColumn = Some(draggedCursor.column),
                                    preferredXPx = None,
                                    multiCursorVerticalStates = Nil
                                  )
                                )
                              ),
                              focus = Focus.EditorPane(paneId),
                              layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                            )
                          )
                        case None => s
                    }
                  }
                }
          }
      }

  private def handleMouseMove(move: MouseMove, state: AppState): cats.effect.IO[Unit] =
    handleContextMenuMouseHover(move, state).flatMap {
      case true => clearEditorHoverTarget
      case false =>
        handleContextualToolbarMouseHover(move, state).flatMap {
          case true => clearEditorHoverTarget
          case false =>
            handleCommandRunnerMouseHover(move, state).flatMap {
              case true => clearEditorHoverTarget
              case false =>
                if isInsideFloatingSurface(move, state) then clearEditorHoverTarget
                else
                  handlePinnedPanelMouseHover(move, state).flatMap {
                    case true  => clearEditorHoverTarget
                    case false => updateEditorHoverTarget(move, state)
                  }
            }
        }
    }

  final private case class PinnedDirectoryMouseHit(
      surface: UiSurface,
      position: PanelPosition,
      tree: DirectoryTreeData,
      row: DirectoryTreeRow
  )

  private def handlePinnedPanelMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    if click.button != MouseButton.Primary then cats.effect.IO.pure(false)
    else
      handlePinnedPanelMouseSelect(click, state, focusPanel = true).flatMap {
        case false =>
          cats.effect.IO.pure(false)
        case true if click.clickCount < 2 =>
          cats.effect.IO.pure(true)
        case true =>
          stateRef.get.flatMap { selectedState =>
            pinnedDirectoryMouseHitAt(click, selectedState) match
              case Some(hit) =>
                val result = PinnedPanelComponent(hit.position).processEvent(PanelInputEvent.Activate, selectedState)
                applyComponentResult(result, selectedState)
                  .flatMap(validateAndUpdateState(_, selectedState))
                  .as(true)
              case None =>
                cats.effect.IO.pure(true)
          }
      }

  private def handlePinnedPanelMouseSelect(
    event: MouseInputEvent,
    state: AppState,
    focusPanel: Boolean
  ): cats.effect.IO[Boolean] =
    pinnedDirectoryMouseHitAt(event, state) match
      case Some(hit) =>
        stateRef.update(selectPinnedDirectoryRow(_, hit, focusPanel)).as(true)
      case None =>
        cats.effect.IO.pure(false)

  private def handlePinnedPanelMouseHover(
    event: MouseInputEvent,
    state: AppState
  ): cats.effect.IO[Boolean] =
    handlePinnedPanelMouseSelect(event, state, focusPanel = false).flatMap {
      case true => cats.effect.IO.pure(true)
      case false =>
        pinnedOutlineMouseHitAt(event, state) match
          case Some((surface, symbols, location)) =>
            stateRef.update(selectPinnedOutlineLocation(_, surface, symbols, location)).as(true)
          case None =>
            pinnedCommentsMouseHitAt(event, state) match
              case Some((surface, symbols, location)) =>
                stateRef.update(selectPinnedCommentsLocation(_, surface, symbols, location)).as(true)
              case None =>
                pinnedDiagnosticsMouseHitAt(event, state) match
                  case Some((surface, issues, location)) =>
                    stateRef.update(selectPinnedDiagnosticsLocation(_, surface, issues, location)).as(true)
                  case None =>
                    cats.effect.IO.pure(false)
    }

  private def handlePinnedPanelLocationClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    if click.button != MouseButton.Primary then cats.effect.IO.pure(false)
    else
      pinnedCommentsMouseHitAt(click, state) match
        case Some((_, _, location)) =>
          stateRef
            .update(current => CommentRendering.openLensAtCursor(navigateActiveEditorToLocation(current, location)))
            .as(true)
        case None =>
          pinnedLocationMouseHitAt(click, state) match
            case Some(location) =>
              stateRef.update(current => navigateActiveEditorToLocation(current, location)).as(true)
            case None =>
              cats.effect.IO.pure(false)

  private def selectPinnedDirectoryRow(
    state: AppState,
    hit: PinnedDirectoryMouseHit,
    focusPanel: Boolean
  ): AppState =
    val updatedContent = SurfaceContent.DirectoryTree(hit.tree, Some(hit.row.path))
    val updatedSurfaces = state.runtime.uiSurfaces.map {
      case surface if surface.id == hit.surface.id => surface.copy(content = updatedContent)
      case surface                                 => surface
    }
    val nextFocus = if focusPanel then Focus.Surface(hit.surface.id) else state.persisted.focus
    state.copy(
      persisted = state.persisted.copy(focus = nextFocus),
      runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)
    )

  private def selectPinnedOutlineLocation(
    state: AppState,
    surface: UiSurface,
    symbols: List[Symbol],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Outline(symbols, Some(location)))
      case existing =>
        existing
    }))

  private def selectPinnedCommentsLocation(
    state: AppState,
    surface: UiSurface,
    symbols: List[Symbol],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Comments(symbols, Some(location)))
      case existing =>
        existing
    }))

  private def selectPinnedDiagnosticsLocation(
    state: AppState,
    surface: UiSurface,
    issues: List[Diagnostic],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Diagnostics(issues, Some(location)))
      case existing =>
        existing
    }))

  private def pinnedDirectoryMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[PinnedDirectoryMouseHit] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      directoryHit <- hit.surface.content match
        case SurfaceContent.DirectoryTree(tree, _) =>
          DirectoryTreeData.visibleRows(tree).lift(hit.rowIndex).map { row =>
            PinnedDirectoryMouseHit(hit.surface, hit.position, tree, row)
          }
        case _ =>
          None
    yield directoryHit

  private def pinnedOutlineMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Outline(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(symbol => (hit.surface, symbols, symbol.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedCommentsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Comments(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(symbol => (hit.surface, symbols, symbol.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedDiagnosticsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Diagnostic], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Diagnostics(issues, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical =>
              issues.lift(hit.rowIndex).map(issue => (hit.surface, issues, issue.location))
            case SurfaceLayoutKind.Square =>
              Option
                .when(hit.rowIndex > 0)(hit.rowIndex - 1)
                .flatMap(issues.lift)
                .map(issue => (hit.surface, issues, issue.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedLocationMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[Location] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      location <- hit.surface.content match
        case SurfaceContent.Outline(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(_.location)
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case SurfaceContent.Diagnostics(issues, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical =>
              issues.lift(hit.rowIndex).map(_.location)
            case SurfaceLayoutKind.Square =>
              Option.when(hit.rowIndex > 0)(hit.rowIndex - 1).flatMap(issues.lift).map(_.location)
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield location

  private def navigateActiveEditorToLocation(state: AppState, location: Location): AppState =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get) match
          case Some(buffer) =>
            val line =
              math.max(0, math.min(location.line, math.max(0, buffer.document.content.lineCount - 1)))
            val column =
              math.max(0, math.min(location.column, buffer.document.content.getLine(line).getOrElse("").length))
            val cursor   = CursorPosition(line, column)
            val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
            val updatedBuffer = buffer.copy(
              editing = buffer.editing.copy(
                cursors = List(cursor),
                selection = None,
                selections = Nil,
                preferredColumn = Some(cursor.column),
                preferredXPx = None,
                multiCursorVerticalStates = Nil
              ),
              viewport = viewport
            )
            state.copy(persisted =
              state.persisted.copy(
                buffers = state.persisted.buffers.updated(buffer.id, updatedBuffer),
                focus = Focus.EditorPane(paneId),
                layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
              )
            )
          case None =>
            state
      case None =>
        state

  final private case class PinnedPanelRowHit(
      surface: UiSurface,
      position: PanelPosition,
      rowIndex: Int,
      layoutKind: SurfaceLayoutKind
  )

  private def panelPosition(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case _                                         => None

  private def pinnedPanelRowHitAt(event: MouseInputEvent, state: AppState): Option[PinnedPanelRowHit] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, hitRegions, _) =>
            for
              surface  <- state.surfaceById(surfaceId)
              position <- panelPosition(surface)
              contentRect <- hitRegions.collectFirst {
                case SceneHitRegion(SceneHitKind.Content, rect) if rect.contains(event.col, event.row) => rect
              }
              rowIndex <- pinnedPanelItemRowIndexAt(
                event,
                contentRect,
                scene.editorContract.panelRowSlots(surface.id)
              )
            yield PinnedPanelRowHit(surface, position, rowIndex, SurfaceLayoutKind.classify(frameRect))
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  private def pinnedPanelItemRowIndexAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot]
  ): Option[Int] =
    val insideColumns = event.col >= contentRect.x && event.col < contentRect.right
    Option
      .when(insideColumns)(())
      .flatMap(_ =>
        rowSlots.collectFirst {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if y == event.row =>
            index
        }
      )

  private def updateEditorHoverTarget(move: MouseMove, state: AppState): cats.effect.IO[Unit] =
    resolveMouseTarget(move, state).flatMap {
      case Some((paneId, buffer, cursor)) =>
        stateRef.update(s =>
          s.copy(runtime = s.runtime.copy(hoveredEditorTarget = Some(HoveredEditorTarget(paneId, buffer.id, cursor))))
        )
      case None =>
        clearEditorHoverTarget
    }

  private def clearEditorHoverTarget: cats.effect.IO[Unit] =
    stateRef.update(s => s.copy(runtime = s.runtime.copy(hoveredEditorTarget = None)))

  private def openEditorContextMenu(click: MouseClick, state: AppState): cats.effect.IO[Unit] =
    resolveMouseTarget(click, state).flatMap {
      case Some((paneId, _, clickedCursor)) =>
        editorContextMenu(Focus.EditorPane(paneId)) match
          case Some(menu) =>
            stateRef.update { current =>
              val surface = UiSurface(
                id = ContextMenuSurfaceId,
                content = SurfaceContent.ContextMenu(menu),
                presentation = SurfacePresentation.Floating(Some(clickedCursor), SurfacePlacement.BelowCursor)
              )
              current
                .copy(runtime =
                  current.runtime
                    .copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(isContextMenuSurface) :+ surface)
                )
                .pushFocus(Focus.Surface(ContextMenuSurfaceId))
            }
          case None =>
            cats.effect.IO.unit
      case None =>
        dismissContextMenuIfOpen(state)
    }

  private def handleContextMenuMouseHover(event: MouseInputEvent, state: AppState): cats.effect.IO[Boolean] =
    contextMenuSelectionAt(event, state) match
      case Some((surface, menu, index)) =>
        stateRef
          .update { current =>
            current.copy(runtime = current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.map {
              case existing if existing.id == surface.id =>
                existing.copy(content = SurfaceContent.ContextMenu(menu.withSelectedIndex(index)))
              case existing => existing
            }))
          }
          .as(true)
      case None =>
        cats.effect.IO.pure(false)

  private def handleContextMenuMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    contextMenuSelectionAt(click, state) match
      case Some((_, menu, index)) =>
        menu.items.lift(index) match
          case Some(item) =>
            stateRef.update { current =>
              val dismissed = dismissContextMenu(current)
              dismissed.copy(persisted = dismissed.persisted.copy(focus = menu.targetFocus))
            } >>
              executeCommand(item.command).as(true)
          case None =>
            cats.effect.IO.pure(false)
      case None if isContextMenuItemGap(click, state) =>
        cats.effect.IO.pure(true)
      case None if state.contextMenuSurface.isDefined =>
        stateRef.update(dismissContextMenu).as(true)
      case None =>
        cats.effect.IO.pure(false)

  private def contextMenuSelectionAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextMenu, Int)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      layout   = scene.calculatedLayout
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
      index <- overlayItemIndex(
        event,
        state,
        layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
        contentRect,
        contract.overlayRowSlots(surface.id),
        menu.items.length,
        menu.selectedIndex,
        hasHeader = true,
        hasFooter = menu.items.nonEmpty,
        itemGapRows = state.persisted.config.commandRunnerItemGapRows,
        itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
      )
    yield (surface, menu, index)

  private def isContextMenuItemGap(event: MouseInputEvent, state: AppState): Boolean =
    (for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      layout   = scene.calculatedLayout
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
    yield contentRect.contains(event.col, event.row) &&
      !contract.overlayRowSlots(surface.id).exists(_.y == event.row)).getOrElse(false)

  private def editorContextMenu(targetFocus: Focus): Option[ContextMenu] =
    val registry = CommandRegistry.withToggleUI
    val items = EditorContextMenuCommands.flatMap { name =>
      registry.findCommand(name).map(command => ContextMenuItem(command.name, command.label, command))
    }
    Option.when(items.nonEmpty)(ContextMenu("editor", targetFocus, items))

  private def dismissContextMenuIfOpen(state: AppState): cats.effect.IO[Unit] =
    if state.contextMenuSurface.isDefined then stateRef.update(dismissContextMenu)
    else cats.effect.IO.unit

  private def dismissContextMenu(state: AppState): AppState =
    state
      .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isContextMenuSurface)))
      .popFocus

  private def isContextMenuSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ContextMenu(_) => true
      case _                             => false

  private def handleContextualToolbarMouseHover(event: MouseInputEvent, state: AppState): cats.effect.IO[Boolean] =
    cats.effect.IO.pure(contextualToolbarSelectionAt(event, state).isDefined)

  private def handleContextualToolbarMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    contextualToolbarSelectionAt(click, state) match
      case Some((surface, toolbarState, ContextualToolbarHit.TopLevelItem(index))) =>
        val registry     = CommandRegistry.withToggleUI
        val items        = ContextualToolbar.itemsFor(state)
        val focusedState = toolbarState.withFocusedIndex(index, items)
        val focusedItem  = focusedState.normalized(items).focusedItem(items)
        stateRef.update { current =>
          val nextState =
            focusedItem match
              case Some(_: ContextualToolbarItem.Button)   => focusedState.closeDetail
              case Some(_: ContextualToolbarItem.Dropdown) => focusedState.openFocusedDetail(items)
              case Some(_: ContextualToolbarItem.Input)    => focusedState.openFocusedDetail(items)
              case None                                    => focusedState
          val updated = replaceContextualToolbar(current, surface, nextState)
          focusedItem match
            case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
              updated.pushFocus(Focus.Surface(surface.id))
            case Some(_: ContextualToolbarItem.Button) =>
              updated.copy(persisted = updated.persisted.copy(focus = editorFocus(current)))
            case _ =>
              updated
        } >>
          stateRef.get.flatMap { current =>
            focusedItem match
              case Some(_: ContextualToolbarItem.Button) =>
                ContextualToolbar.focusedCommand(focusedState, current, registry) match
                  case Some(command) => executeCommand(command).as(true)
                  case None          => cats.effect.IO.pure(false)
              case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
                cats.effect.IO.pure(true)
              case None =>
                cats.effect.IO.pure(false)
          }
      case Some((surface, toolbarState, ContextualToolbarHit.DropdownOption(itemId, optionIndex))) =>
        val detailState =
          toolbarState.copy(detailState = Some(ContextualToolbarDetailState.Dropdown(itemId, optionIndex)))
        stateRef.update { current =>
          val replaced = replaceContextualToolbar(current, surface, detailState.closeDetail)
          replaced.copy(persisted = replaced.persisted.copy(focus = editorFocus(current)))
        } >>
          stateRef.get.flatMap { current =>
            ContextualToolbar.detailCommand(detailState, current) match
              case Some(command) => executeCommand(command).as(true)
              case None          => cats.effect.IO.pure(false)
          }
      case Some((surface, toolbarState, ContextualToolbarHit.InputDetail(_))) =>
        stateRef
          .update(current =>
            replaceContextualToolbar(current, surface, toolbarState).pushFocus(Focus.Surface(surface.id))
          )
          .as(true)
      case None =>
        cats.effect.IO.pure(false)

  private def contextualToolbarSelectionAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextualToolbarState, ContextualToolbarHit)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextualToolbarSurface
      toolbarState <- surface.content match
        case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
        case _                                              => None
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      layout   = scene.calculatedLayout
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
      hit <- contextualToolbarItemHit(
        event,
        contentRect,
        state,
        toolbarState,
        contract.overlayRowSlots(surface.id),
        layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0)
      )
    yield (surface, toolbarState, hit)

  private def contextualToolbarItemHit(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    state: AppState,
    toolbarState: ContextualToolbarState,
    rowSlots: List[SurfaceContentRowSlot],
    floatingOffsetRows: Double
  ): Option[ContextualToolbarHit] =
    val rowIndex =
      if event.pixelX.isDefined && event.pixelY.isDefined then
        val metrics = floatingCellMetrics(state)
        val rowCount = rowSlots.count {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), _) => true
          case _                                                       => false
        }
        for
          pixelX <- event.pixelX
          pixelY <- event.pixelY
          geometry = FloatingSurfaceGeometry
            .fromCells(
              contentRect,
              metrics,
              borderCells = 0,
              itemCount = rowCount,
              hasHeader = false,
              hasFooter = false,
              itemGapRows = state.persisted.config.uiElementGap,
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(
                SurfaceContent.ContextualToolbar(toolbarState),
                state.persisted.config.interfaceDensity
              )
            )
            .translated(0.0, FloatingSurfaceGeometry.signedRowOffsetPixels(floatingOffsetRows, metrics))
          index <- geometry.itemIndexAt(pixelX, pixelY)
        yield index
      else
        overlayDisplayedRowIndexAt(
          event,
          contentRect,
          rowSlots,
          SurfaceFrameLayout.itemTargetRowsFor(
            SurfaceContent.ContextualToolbar(toolbarState),
            state.persisted.config.interfaceDensity
          )
        )
    rowIndex.flatMap { rowIndex =>
      ContextualToolbarLayout.hitAt(
        rowIndex = rowIndex,
        columnOffset = event.col - contentRect.x,
        contentWidth = contentRect.width.max(1),
        toolbarState = toolbarState,
        state = state
      )
    }

  private def isInsideFloatingSurface(event: MouseInputEvent, state: AppState): Boolean =
    state.runtime.viewportSize.exists { viewportSize =>
      val scene    = AuthoritativeUiScene.forState(state, viewportSize)
      val layout   = scene.calculatedLayout
      val contract = scene.editorContract
      val metrics  = floatingCellMetrics(state)
      state.floatingSurfaces.exists { surface =>
        contract.overlayRect(surface.id).exists { rect =>
          val geometry = FloatingSurfaceGeometry
            .fromCells(
              rect,
              metrics,
              borderCells = 0,
              itemCount = 0,
              hasHeader = false,
              hasFooter = false,
              itemGapRows = 0.0
            )
            .translated(
              0.0,
              FloatingSurfaceGeometry.signedRowOffsetPixels(
                layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                metrics
              )
            )
          (event.pixelX, event.pixelY) match
            case (Some(pixelX), Some(pixelY)) => geometry.frame.contains(pixelX, pixelY)
            case _                            => rect.contains(event.col, event.row)
        }
      }
    }

  private def floatingCellMetrics(state: AppState): CellMetrics =
    CellMetrics.fromFont(FontLoader.previewCodeFont(state.persisted.config.fontConfig))

  private def replaceContextualToolbar(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.ContextualToolbar(toolbarState))
      case existing =>
        existing
    }))

  private def editorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId
      .map(Focus.EditorPane.apply)
      .getOrElse(Focus.EditorPane(PaneId(0)))

  private def handleCommandRunnerMouseHover(event: MouseInputEvent, state: AppState): cats.effect.IO[Boolean] =
    commandRunnerSelectionAt(event, state) match
      case Some(selectEvent) =>
        val registry = CommandRegistry.withToggleUI
        applyReducerResult(CommandRunnerReducer.reduce(selectEvent, state, registry), state).map(_ => true)
      case None =>
        cats.effect.IO.pure(false)

  private def handleCommandRunnerMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    commandRunnerSelectionAt(click, state) match
      case Some(selectEvent) =>
        val registry = CommandRegistry.withToggleUI
        val selected = CommandRunnerReducer.reduce(selectEvent, state, registry)
        applyReducerResult(selected, state) >>
          (selectEvent match
            case _: RunnerSelectCategory => cats.effect.IO.unit
            case _ =>
              stateRef.get.flatMap { selectedState =>
                val submitted = CommandRunnerReducer.reduce(RunnerSubmit, selectedState, registry)
                applyReducerResult(submitted, selectedState)
              }
          ).map(_ => true)
      case None =>
        cats.effect.IO.pure(false)

  private def commandRunnerSelectionAt(event: MouseInputEvent, state: AppState): Option[CommandRunnerEvent] =
    val surfaces =
      event match
        case _: MouseMove
            if state.commandRunnerSubmenuSurface
              .exists(surface => state.persisted.focus == Focus.Surface(surface.id)) =>
          state.commandRunnerSubmenuSurface.toList
        case _ =>
          List(state.commandRunnerSubmenuSurface, state.commandRunnerSurface).flatten
    if surfaces.isEmpty then None
    else
      state.runtime.viewportSize.flatMap { viewportSize =>
        val scene    = AuthoritativeUiScene.forState(state, viewportSize)
        val layout   = scene.calculatedLayout
        val contract = scene.editorContract
        surfaces.view
          .flatMap(surface => commandRunnerSelectionForSurface(event, surface, layout, contract, state))
          .headOption
      }

  private def handleTextAreaResizeDrag(drag: MouseDrag, state: AppState): cats.effect.IO[Boolean] =
    textAreaInsetFromDrag(drag, state) match
      case Some(TextAreaInsetDrag.Left(value)) =>
        updateConfig(_.withTextAreaLeftInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Right(value)) =>
        updateConfig(_.withTextAreaRightInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Top(value)) =>
        updateConfig(_.withTextAreaTopInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Bottom(value)) =>
        updateConfig(_.withTextAreaBottomInset(value)).map(_ => true)
      case None =>
        cats.effect.IO.pure(false)

  private def handlePinnedPanelResizeDrag(drag: MouseDrag, state: AppState): cats.effect.IO[Boolean] =
    state.runtime.viewportSize.flatMap(viewportSize =>
      LayoutEngine.pinnedPanelResizeFromDrag(state, viewportSize, drag.col, drag.row)
    ) match
      case Some(LayoutEngine.PinnedPanelDragResize(position, size)) =>
        resizePinnedPanel(PanelTarget.ByPosition(position), size).as(true)
      case None =>
        cats.effect.IO.pure(false)

  private enum TextAreaInsetDrag:
    case Left(value: Double)
    case Right(value: Double)
    case Top(value: Double)
    case Bottom(value: Double)

  private def textAreaInsetFromDrag(drag: MouseDrag, state: AppState): Option[TextAreaInsetDrag] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val contract = EditorLayoutContract.from(state, viewportSize, layout)
      contract.activePaneLayout.flatMap { _ =>
        val workspaceX     = contract.leftSpacerRect.x
        val workspaceRight = contract.rightSpacerRect.right
        val workspaceWidth = (workspaceRight - workspaceX).max(1)
        val contentTop     = contract.topSpacerRect.y
        val contentBottom  = contract.workspace.editorPanelRect.bottom
        val contentHeight  = (contentBottom - contentTop).max(1)
        val withinWorkspaceY =
          drag.row >= contract.leftSpacerRect.y && drag.row < contract.leftSpacerRect.bottom
        val withinWorkspaceX =
          drag.col >= contract.topSpacerRect.x && drag.col < contract.topSpacerRect.right

        if withinWorkspaceY && drag.col >= contract.leftSpacerRect.x && drag.col < contract.leftSpacerRect.right then
          Some(TextAreaInsetDrag.Left((drag.col - workspaceX).toDouble / workspaceWidth.toDouble))
        else if withinWorkspaceY && drag.col >= contract.rightSpacerRect.x && drag.col < contract.rightSpacerRect.right
        then Some(TextAreaInsetDrag.Right((workspaceRight - drag.col).toDouble / workspaceWidth.toDouble))
        else if withinWorkspaceX &&
            drag.row >= contract.topSpacerRect.y &&
            drag.row < contract.topSpacerRect.bottom
        then Some(TextAreaInsetDrag.Top((drag.row - contentTop).toDouble / contentHeight.toDouble))
        else if withinWorkspaceX &&
            drag.row >= contract.bottomSpacerRect.y &&
            drag.row < contract.bottomSpacerRect.bottom
        then Some(TextAreaInsetDrag.Bottom((contentBottom - drag.row).toDouble / contentHeight.toDouble))
        else None
      }
    }

  private def commandRunnerSelectionForSurface(
    event: MouseInputEvent,
    surface: UiSurface,
    layout: CalculatedLayout,
    contract: EditorLayoutContract,
    state: AppState
  ): Option[CommandRunnerEvent] =
    contract.overlayContentRect(surface.id).flatMap { contentRect =>
      val rowSlots = contract.overlayRowSlots(surface.id)
      surface.content match
        case SurfaceContent.CommandPalette(runner) =>
          if runner.isSettingsSurface then
            overlayItemIndex(
              event,
              state,
              layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
              contentRect,
              rowSlots,
              runner.settingsSurfaceItems.length,
              runner.settingsSurfaceSelectedIndex,
              hasHeader = true,
              hasFooter = true,
              itemGapRows = state.persisted.config.commandRunnerItemGapRows,
              itemTargetRows =
                SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
            ).map { index =>
              if runner.activeSubmenu.nonEmpty then RunnerSelectSubmenuItem(index) else RunnerSelectVisibleItem(index)
            }
          else
            commandPaletteCategoryAt(event, contentRect, contract.overlayHeaderRect(surface.id), runner.searchTerm)
              .map(RunnerSelectCategory(_))
              .orElse(
                overlayItemIndex(
                  event,
                  state,
                  layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                  contentRect,
                  rowSlots,
                  runner.visibleItems.length,
                  runner.selectedIndex,
                  hasHeader = true,
                  hasFooter = runner.visibleItems.nonEmpty || runner.statusMessage.nonEmpty,
                  itemGapRows = state.persisted.config.commandRunnerItemGapRows,
                  itemTargetRows =
                    SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
                ).map(RunnerSelectVisibleItem(_))
              )
        case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) =>
          val submenuState = runner.activeSubmenu.filter(_.groupId == groupId)
          val items = submenuState
            .map(_.filteredItems(runner.submenuItems(groupId)))
            .getOrElse(runner.submenuItems(groupId))
          val selectedIndex = submenuState.map(_.selectedIndex).getOrElse(0)
          val group         = runner.submenuGroup(groupId)
          val detailRows    = commandRunnerSubmenuDetailRowCount(groupId, items.lift(selectedIndex))
          overlayItemIndex(
            event,
            state,
            layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
            contentRect,
            rowSlots,
            items.length,
            selectedIndex,
            hasHeader = group.nonEmpty,
            hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
            reservedContentRows = detailRows,
            itemGapRows = state.persisted.config.commandRunnerItemGapRows,
            itemTargetRows =
              SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
          ).map { index =>
            if previewOnly then RunnerSelectPreviewSubmenuItem(groupId, index)
            else RunnerSelectSubmenuItem(index)
          }
        case _ =>
          None
    }

  private def commandPaletteCategoryAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    headerRect: Option[LayoutRect],
    searchTerm: String
  ): Option[CommandCategory] =
    val categories = CommandCategory.values.toList
    val categoryIndex =
      Option.when(searchTerm.isEmpty && headerRect.exists(_.contains(event.col, event.row))) {
        ((event.col - contentRect.x) * categories.length) / contentRect.width.max(1)
      }
    categoryIndex.flatMap(categories.lift)

  private def overlayItemIndex(
    event: MouseInputEvent,
    state: AppState,
    floatingOffsetRows: Double,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot],
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1
  ): Option[Int] =
    val itemWindow = SurfaceFrameLayout(contentRect, borderCells = 0).itemWindow(
      itemCount,
      selectedIndex,
      hasHeader,
      hasFooter,
      reservedContentRows,
      itemGapRows,
      itemTargetRows
    )
    val pixelSelection = for
      pixelX <- event.pixelX
      pixelY <- event.pixelY
      metrics = floatingCellMetrics(state)
      geometry = FloatingSurfaceGeometry
        .fromCells(
          contentRect,
          metrics,
          borderCells = 0,
          itemCount = itemCount,
          hasHeader = hasHeader,
          hasFooter = hasFooter,
          itemGapRows = itemGapRows,
          itemTargetRows = itemTargetRows
        )
        .translated(0.0, FloatingSurfaceGeometry.signedRowOffsetPixels(floatingOffsetRows, metrics))
      displayedIndex <- geometry.itemIndexAt(pixelX, pixelY)
      absoluteIndex  <- itemWindow.absoluteIndexAt(displayedIndex)
    yield absoluteIndex
    if event.pixelX.isDefined && event.pixelY.isDefined then pixelSelection
    else overlayDisplayedRowIndexAt(event, contentRect, rowSlots, itemTargetRows).flatMap(itemWindow.absoluteIndexAt)

  private def overlayDisplayedRowIndexAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot],
    itemTargetRows: Int = 1
  ): Option[Int] =
    val insideColumns = event.col >= contentRect.x && event.col < contentRect.right
    Option
      .when(insideColumns)(())
      .flatMap(_ =>
        rowSlots.collectFirst {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y)
              if event.row >= y && event.row < y + math.max(1, itemTargetRows) =>
            index
        }
      )

  private def commandRunnerSubmenuDetailRowCount(
    groupId: String,
    selectedItem: Option[CommandSurfaceItem]
  ): Int =
    selectedItem.count {
      case group: CommandSurfaceItem.GroupItem
          if groupId == "settings-ui-presets" &&
            (group.id == "settings-preset-create" || group.id == "settings-preset-edit") =>
        true
      case option: CommandSurfaceItem.OptionItem
          if groupId == "settings-preset-select" && option.id == "ui-preset-select" =>
        true
      case _ =>
        false
    }

  private def resolveMouseTarget(
    click: MouseInputEvent,
    state: AppState
  ): cats.effect.IO[Option[(PaneId, Buffer, CursorPosition)]] =
    state.runtime.viewportSize match
      case None => cats.effect.IO.pure(None)
      case Some(tSize) =>
        mouseTargetLayout(state, tSize).flatMap { cache =>
          cache.scene.paneLayouts.find {
            case (_, paneLayout) =>
              paneLayout.contentRect.contains(click.col, click.row)
          } match
            case Some((paneId, paneLayout)) =>
              state.persisted.layout.editorPanes
                .get(paneId)
                .flatMap(pane => pane.bufferId.flatMap(state.persisted.buffers.get)) match
                case Some(buffer) =>
                  val contentRect = paneLayout.contentRect
                  val vp          = buffer.viewport
                  val visualRow   = (click.row - contentRect.y).max(0)
                  mouseTargetSnapshot(cache, paneId).map { snapshot =>
                    val cellWidthPx =
                      if contentRect.width > 0 then snapshot.panelWidthPx.toFloat / contentRect.width.toFloat else 1.0f
                    val xPx = click.pixelX match
                      case Some(pixelX) => pixelX.toFloat - (contentRect.x * cellWidthPx)
                      case None         => (click.col - contentRect.x).max(0) * cellWidthPx
                    val clickedCursor = snapshot
                      .cursorForVisualRowAndXPx(visualRow, xPx.max(0.0f))
                      .orElse {
                        val bufferLine  = (vp.topLine + visualRow).max(0)
                        val bufferCol   = (vp.leftColumn + (click.col - contentRect.x)).max(0)
                        val clampedLine = bufferLine.min(math.max(0, buffer.document.content.lineCount - 1))
                        val lineLen     = buffer.document.content.getLine(clampedLine).getOrElse("").length
                        Some(CursorPosition(clampedLine, bufferCol.min(lineLen)))
                      }
                    clickedCursor.map(cursor => (paneId, buffer, cursor))
                  }
                case None =>
                  cats.effect.IO.pure(None)
            case None => cats.effect.IO.pure(None)
        }

  private def mouseTargetLayout(state: AppState, viewportSize: ViewportSize): cats.effect.IO[MouseTargetCache] =
    val key = MouseTargetLayoutKey.from(state, viewportSize)
    mouseTargetCacheRef.modify {
      case Some(cache) if cache.layoutKey == key =>
        val scene = AuthoritativeUiScene.forState(state, viewportSize)
        val next  = if cache.scene eq scene then cache else cache.copy(scene = scene)
        Some(next) -> next
      case _ =>
        val next = MouseTargetCache.fromState(state, viewportSize)
        Some(next) -> next
    }

  private def mouseTargetSnapshot(
    cache: MouseTargetCache,
    paneId: PaneId
  ): cats.effect.IO[TextLayoutSnapshot] =
    cache.scene.textSnapshot(paneId) match
      case Some(snapshot) => cats.effect.IO.pure(snapshot)
      case None => cats.effect.IO.raiseError(new IllegalStateException(s"missing text snapshot for pane $paneId"))

  private def wordSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val source        = RopeCharacterSource(buffer.document.content)
    val clickedOffset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    if source.length == 0 then None
    else
      val probeOffset =
        if clickedOffset >= source.length then source.length - 1
        else if source.charAt(clickedOffset).isWhitespace && clickedOffset > 0 && !source
              .charAt(clickedOffset - 1)
              .isWhitespace
        then clickedOffset - 1
        else clickedOffset
      if probeOffset < 0 || source.charAt(probeOffset).isWhitespace then None
      else
        val start = TextEditing.previousWordBoundary(source, probeOffset)
        @annotation.tailrec
        def wordEndFrom(offset: Int): Int =
          if offset < source.length && !source.charAt(offset).isWhitespace then wordEndFrom(offset + 1)
          else offset
        val end = wordEndFrom(probeOffset)
        Some(
          Selection(
            offsetToCursorPosition(buffer.document.content, start),
            offsetToCursorPosition(buffer.document.content, end)
          )
        )

  private def lineSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    Some(
      Selection(
        CursorPosition(cursor.line, 0),
        CursorPosition(cursor.line, lineText.length)
      )
    )

  private def rangeSelectionFromAnchor(buffer: Buffer, focus: CursorPosition): Option[Selection] =
    val anchor = buffer.primarySelection.map(_.anchor).orElse(buffer.editing.cursors.headOption).getOrElse(focus)
    Option.when(anchor != focus)(Selection(anchor, focus))

  private def offsetToCursorPosition(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')
