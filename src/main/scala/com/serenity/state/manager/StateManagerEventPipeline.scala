package com.serenity.state.manager

import java.awt.Color

import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.command.{CommandCategory, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, PendingGroup}
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
    stateRef.get.flatMap { rawState =>
      val prevState = normalizeCommandRunnerFocus(rawState)
      val syncFocus = if prevState == rawState then cats.effect.IO.unit else stateRef.set(prevState)
      val handleEvent: cats.effect.IO[Unit] =
        if prevState.hasBlockingModal && !allowedWhileBlockingModal(event) then cats.effect.IO.unit
        else dispatchEvent(event, prevState)
      syncFocus >> handleEvent >>
        recordUndoableEdit(event, prevState) >>
        enqueueChangedLspDocuments(prevState) >>
        applyAnimationHooks(prevState)
    }

  private def allowedWhileBlockingModal(event: Event): Boolean =
    event match
      case _: SystemEvent | _: MouseInputEvent => true
      case _                                   => ModalInputEvent.fromEvent(event).nonEmpty

  private def dispatchEvent(event: Event, prevState: AppState): cats.effect.IO[Unit] =
    event match
      case Undo => applyUndo(prevState)
      case Redo => applyRedo(prevState)
      case resize: com.serenity.keystroke.events.ResizeEvent =>
        resizeEvents.apply(resize, prevState)
      case systemEvent: SystemEvent =>
        applyReducerResult(SystemEventReducer.reduce(systemEvent, prevState), prevState)
      case com.serenity.keystroke.events.CloseTab =>
        beginCloseAction(CloseScope.Current, prevState)
      case com.serenity.keystroke.events.Quit =>
        beginCloseAction(CloseScope.Quit, prevState)
      case appEvent: GlobalAppEvent =>
        val registry = CommandRegistry.withToggleUI
        applyReducerResult(AppEventReducer.reduce(appEvent, prevState, registry)(using balance), prevState) >>
          (appEvent match
            case ToggleCommandRunner => hydrateCommandRunnerUiPresets
            case _                   => cats.effect.IO.unit) >>
          (appEvent match
            case NextTab     => applyPaneFlowAnimation(SweepDirection.Backward)
            case PreviousTab => applyPaneFlowAnimation(SweepDirection.Forward)
            case _           => cats.effect.IO.unit)
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
      case _ =>
        val logCommandRunnerEvent =
          focusedCommandRunner(prevState) match
            case Some(runner) =>
              logger.info(s"[COMMAND-RUNNER] ${StateManager.describeCommandRunnerEvent(event, runner)}")
            case None =>
              cats.effect.IO.unit

        val result =
          getLocalHandlerForFocus(prevState.focus, prevState).processEvent(event, prevState)

        logCommandRunnerEvent >>
          applyComponentResult(result, prevState).flatMap(newState => validateAndUpdateState(newState, prevState))

  private def enqueueChangedLspDocuments(previousState: AppState): cats.effect.IO[Unit] =
    stateRef.get.flatMap { currentState =>
      currentState.buffers.values.toList.traverse_ { buffer =>
        val changedContent = previousState.buffers.get(buffer.id).exists(_.content != buffer.content)
        (for
          path       <- buffer.filePath
          languageId <- buffer.language
          if changedContent
        yield AppEffect.LspQueue(
          LspQueueEffect.DocumentChanged(path.toUri.toString, languageId, buffer.content.collect())
        ))
          .fold(cats.effect.IO.unit)(effects.interpretEffect)
      }
    }

  private def recordUndoableEdit(event: Event, prevState: AppState): cats.effect.IO[Unit] =
    focusedBufferAndPane(prevState) match
      case None => cats.effect.IO.unit
      case Some((bufferId, paneId, buffer)) =>
        stateRef.get.flatMap { currentState =>
          currentState.buffers.get(bufferId) match
            case Some(currentBuffer) if isUndoableContentMutation(event) && bufferChanged(buffer, currentBuffer) =>
              val beforeSnapshot = BufferSnapshot.fromBuffer(buffer)
              event match
                case InsertChar(_) | TabKey =>
                  undoRef.update { undo =>
                    val sameGroup = undo.pendingGroup.exists(g => g.bufferId == bufferId && g.paneId == paneId)
                    if sameGroup then undo.clearRedo
                    else
                      val flushed  = undo.flushPendingGroup
                      val newGroup = PendingGroup(bufferId, paneId, beforeSnapshot)
                      flushed.copy(pendingGroup = Some(newGroup), redoStack = Nil)
                  }
                case _ =>
                  undoRef.update { undo =>
                    val flushed = undo.flushPendingGroup
                    val entry   = HistoryEntry(bufferId, paneId, beforeSnapshot)
                    flushed.pushUndo(entry)
                  }
            case _ => cats.effect.IO.unit
        }

  private def applyUndo(prevState: AppState): cats.effect.IO[Unit] =
    undoRef.get.flatMap { undo =>
      val flushed = undo.flushPendingGroup
      flushed.undoStack match
        case Nil => cats.effect.IO.unit
        case entry :: rest =>
          stateRef.get.flatMap { state =>
            state.buffers.get(entry.bufferId) match
              case None => cats.effect.IO.unit
              case Some(current) =>
                val redoEntry      = HistoryEntry(entry.bufferId, entry.paneId, BufferSnapshot.fromBuffer(current))
                val restoredBuffer = entry.snapshot.restoreInto(current)
                val snappedState   = snapFocusToPane(state, entry.paneId)
                undoRef.set(flushed.copy(undoStack = rest).pushRedo(redoEntry)) >>
                  validateAndUpdateState(
                    snappedState.copy(buffers = snappedState.buffers + (entry.bufferId -> restoredBuffer)),
                    state
                  )
          }
    }

  private def applyRedo(prevState: AppState): cats.effect.IO[Unit] =
    undoRef.get.flatMap { undo =>
      undo.redoStack match
        case Nil => cats.effect.IO.unit
        case entry :: rest =>
          stateRef.get.flatMap { state =>
            state.buffers.get(entry.bufferId) match
              case None => cats.effect.IO.unit
              case Some(current) =>
                val undoEntry      = HistoryEntry(entry.bufferId, entry.paneId, BufferSnapshot.fromBuffer(current))
                val restoredBuffer = entry.snapshot.restoreInto(current)
                val snappedState   = snapFocusToPane(state, entry.paneId)
                undoRef.set(undo.copy(redoStack = rest).pushUndo(undoEntry, clearRedo = false)) >>
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

  private def isUndoableContentMutation(event: Event): Boolean =
    event match
      case InsertChar(_) | TabKey | ReverseTabKey | DeleteBackward | DeleteForward | DeleteWordBackward |
          DeleteWordForward | NewLine | Enter | Paste | Cut =>
        true
      case _ => false

  private def bufferChanged(before: Buffer, after: Buffer): Boolean =
    before.content != after.content ||
      before.cursors != after.cursors ||
      before.selection != after.selection ||
      before.selections != after.selections

  private def snapFocusToPane(state: AppState, paneId: PaneId): AppState =
    if state.focus == Focus.EditorPane(paneId) then state
    else
      state.copy(
        focus = Focus.EditorPane(paneId),
        layout = state.layout.copy(activeEditorPaneId = Some(paneId))
      )

  private[manager] def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  private[manager] def scheduleDocumentAnalysis(): cats.effect.IO[Unit] =
    operations.scheduleDocumentAnalysis()

  private def getLocalHandlerForFocus(focus: Focus, state: AppState): LocalEventHandler =
    focus match
      case Focus.EditorPane(paneId) => new EditorPaneComponent(paneId)(using balance)
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId) match
          case Some(surface) =>
            surface.presentation match
              case SurfacePresentation.Pinned(position, _) =>
                new PinnedPanelComponent(position)
              case SurfacePresentation.Expanded(position, _) =>
                new PinnedPanelComponent(position)
              case SurfacePresentation.Modal =>
                surface.content match
                  case SurfaceContent.ModalWorkflow(modal) =>
                    new ModalComponent(modalType(modal))
                  case _ =>
                    new PeekOverlayComponent()
              case SurfacePresentation.Floating(_, _) =>
                surface.content match
                  case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) =>
                    val registry = CommandRegistry.withToggleUI
                    new CommandRunnerComponent(registry)
                  case SurfaceContent.ThemePicker(_) =>
                    new ThemePickerComponent()
                  case SurfaceContent.ThemeCreator(_) =>
                    new ThemeCreatorComponent()
                  case SurfaceContent.FileSearch(_) =>
                    new FileSearchComponent()
                  case SurfaceContent.ContextualToolbar(_) =>
                    val registry = CommandRegistry.withToggleUI
                    new ContextualToolbarComponent(registry)
                  case SurfaceContent.CommentLens(_) =>
                    new CommentLensComponent()
                  case SurfaceContent.StartPage(_) =>
                    new StartupPageComponent()
                  case SurfaceContent.ModalWorkflow(modal) =>
                    new ModalComponent(modalType(modal))
                  case _ =>
                    new PeekOverlayComponent()
          case None =>
            NoOpLocalEventHandler

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
            val updatedSurfaces = state.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case current =>
                current
            }
            state.copy(uiSurfaces = updatedSurfaces)
          case _ =>
            state
      case None =>
        state

  private def normalizeCommandRunnerFocus(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus match
        case Some(focus) => state.copy(focus = focus)
        case None        => state
    else state

  private def applyPaneFlowAnimation(sweep: SweepDirection): cats.effect.IO[Unit] =
    stateRef.get.flatMap { state =>
      val animOpt = for
        config <- state.config.scaledUiAnimation
        paneId <- state.layout.activeEditorPaneId
        pane   <- state.layout.editorPanes.get(paneId)
        buffId <- pane.bufferId
        buffer <- state.buffers.get(buffId)
        cells = VisibleBufferAnimationCells.fromBuffer(
          buffer,
          state.config.wordWrapEnabled,
          state.theme.background,
          state.theme.foreground
        )
        if cells.nonEmpty
      yield
        val animated = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, sweep, config.steps)
        val uiAnimations =
          animated.view.mapValues(_.copy(owner = com.serenity.animation.AnimationOwner.UiTransitions)).toMap
        val newAnims =
          buffer.animations
            .clear(com.serenity.animation.AnimationOwner.UiTransitions)
            .mergeUiTransitionAnimations(uiAnimations)
        state.copy(buffers = state.buffers.updated(buffId, buffer.copy(animations = newAnims)))
      animOpt match
        case Some(newState) => stateRef.set(newState)
        case None           => cats.effect.IO.unit
    }

  private[manager] def applyAnimationHooks(prevState: AppState): cats.effect.IO[Unit] =
    if !shouldApplySurfaceAnimationHooks(prevState) then cats.effect.IO.unit
    else
      stateRef.get.flatMap { currentState =>
        val prevSurfaces    = animatedCommandSurfaces(prevState)
        val currentSurfaces = animatedCommandSurfaces(currentState)
        val openedSurfaces =
          currentSurfaces.filter(surface => !prevSurfaces.exists(_.id == surface.id))
        val transitionedSurfaces =
          currentSurfaces.filter(current =>
            prevSurfaces
              .find(_.id == current.id)
              .exists(previous => commandSurfaceTransitionKey(previous) != commandSurfaceTransitionKey(current))
          )
        val closedSurfaces =
          prevSurfaces.filter(surface => !currentSurfaces.exists(_.id == surface.id))
        val prevPanels    = animatedPanelSurfaces(prevState)
        val currentPanels = animatedPanelSurfaces(currentState)
        val openedPanels =
          currentPanels.filter(surface => !prevPanels.exists(_.id == surface.id))
        val closedPanels =
          prevPanels.filter(surface => !currentPanels.exists(_.id == surface.id))

        (openedSurfaces ++ transitionedSurfaces).distinct.traverse_(surface =>
          applyCommandRunnerOpenAnimation(surface, currentState)
        ) >>
          closedSurfaces.traverse_(surface => applyCommandRunnerCloseAnimation(surface, prevState)) >>
          openedPanels.traverse_(surface => applyPinnedPanelOpenAnimation(surface)) >>
          closedPanels.traverse_(surface => applyPinnedPanelCloseAnimation(surface, prevState))
      }

  private[manager] def shouldApplySurfaceAnimationHooks(state: AppState): Boolean =
    state.surfaceAnimations.nonEmpty ||
      state.config.scaledCommandRunnerAnimation.exists(config => !config.isDisabled) ||
      state.config.pinnedPanelTransitionSettings.enabled

  private def commandSurfaceTransitionKey(surface: UiSurface): Option[(String, Boolean, Option[String], List[String])] =
    surface.content match
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) =>
        Some(
          (
            groupId,
            previewOnly,
            runner.activeSubmenu.flatMap(_.parentGroupId),
            runner.activeSubmenu.fold(Nil)(_.ancestorGroupIds)
          )
        )
      case _ =>
        None

  private def applyCommandRunnerOpenAnimation(surface: UiSurface, state: AppState): cats.effect.IO[Unit] =
    state.config.scaledCommandRunnerAnimation match
      case Some(config) if !config.isDisabled =>
        stateRef.update { s =>
          val steps         = config.steps
          val tSize         = s.viewportSize.getOrElse(ViewportSize(80, 24))
          val layout        = LayoutEngine.calculateLayoutWithUI(s, tSize)
          val contract      = EditorLayoutContract.from(s, tSize, layout)
          val overlayRect   = contract.overlayRect(surface.id)
          val overlayHeight = overlayRect.map(_.height).getOrElse(4)
          val exitingGhost  = matchingExitingCommandGhost(surface, s)
          val revealKind    = s.config.effectiveCommandRunnerTransitionKind
          val animationState =
            if revealKind == TransitionKind.Fade then
              commandRunnerFadeInAnimation(
                overlayHeight,
                steps,
                s,
                exitingGhost.flatMap(ghost => s.surfaceAnimations.get(ghost.id).map(_.animationState))
              )
            else
              val plan = ElementTransitionPlanner.plan(
                ElementTransitionRequest(TransitionScope.CommandRunner),
                ElementTransitionSettings(
                  enabled = true,
                  baseTiming = TransitionTiming(durationMs = steps * 16, staggerMs = 16, delayMs = 0, speedScale = 1.0),
                  speedScale = 1.0,
                  overrides = Map(TransitionScope.CommandRunner -> revealKind)
                )
              )
              ElementTransitionLowerer.lower(
                plan,
                commandRunnerOpenCells(overlayRect.map(_.width).getOrElse(56), overlayHeight, s),
                tickRateMs = 16
              )
          val surfaceAnimations =
            if animationState.hasActiveAnimations then
              s.surfaceAnimations + (surface.id -> SurfaceAnimationState(
                phase = SurfacePhase.Visible,
                animationState = animationState,
                overlayHeight = overlayHeight,
                bufferFadeLength = 0,
                phaseTick = 0
              ))
            else s.surfaceAnimations - surface.id
          exitingGhost.fold(s.copy(surfaceAnimations = surfaceAnimations)) { ghost =>
            s.copy(
              uiSurfaces = s.uiSurfaces.filterNot(_.id == ghost.id),
              surfaceAnimations = surfaceAnimations - ghost.id
            )
          }
        }
      case _ =>
        stateRef.update(s => s.copy(surfaceAnimations = s.surfaceAnimations - surface.id))

  private def commandRunnerFadeInAnimation(
    overlayHeight: Int,
    steps: Int,
    state: AppState,
    previous: Option[AnimationState]
  ): AnimationState =
    val overlayFadeIn = (0 until overlayHeight).map { rowOffset =>
      val delay        = rowOffset
      val panelBg      = state.theme.panel.background
      val panelFg      = state.theme.panel.foreground
      val previousCell = previous.flatMap(_.getCell(0, rowOffset))
      val initialBg    = previousCell.flatMap(_.currentBackground).getOrElse(transparent(panelBg))
      val initialFg    = previousCell.flatMap(_.currentForeground).getOrElse(transparent(panelFg))
      val remainingSteps = previousCell
        .map(cell => completedFadeSteps(rowOffset + steps, cell.backgroundSteps.length))
        .getOrElse(steps)
      val bgSteps = List.fill(delay)(initialBg) ++ RgbInterpolator.interpolateRgba(initialBg, panelBg, remainingSteps)
      val fgSteps = List.fill(delay)(initialFg) ++ RgbInterpolator.interpolateRgba(initialFg, panelFg, remainingSteps)
      CharacterKey(0, rowOffset) -> AnimatedCell(
        content = None,
        foregroundSteps = fgSteps,
        backgroundSteps = bgSteps
      )
    }.toMap
    AnimationState(overlayFadeIn)

  private def commandRunnerOpenCells(width: Int, height: Int, state: AppState): ElementTransitionCells =
    val transparentPanelForeground = transparent(state.theme.panel.foreground)
    val transparentBorder          = transparent(state.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', transparentBorder, state.theme.border)
    val contentCells =
      (0 until math.max(0, height - 1)).flatMap { row =>
        (0 until math.max(1, width - 2)).map { column =>
          CharacterKey(column, row) ->
            CellAnimation(' ', transparentPanelForeground, state.theme.panel.foreground)
        }
      }.toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def applyCommandRunnerCloseAnimation(
    closedSurface: UiSurface,
    prevState: AppState
  ): cats.effect.IO[Unit] =
    prevState.config.scaledCommandRunnerAnimation match
      case Some(config) if !config.isDisabled =>
        stateRef.update { s =>
          val steps          = config.steps
          val tSize          = prevState.viewportSize.orElse(s.viewportSize).getOrElse(ViewportSize(80, 24))
          val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
          val contract       = EditorLayoutContract.from(prevState, tSize, previousLayout)
          val overlayHeight = prevState.surfaceAnimations
            .get(closedSurface.id)
            .map(_.overlayHeight)
            .orElse(contract.overlayRect(closedSurface.id).map(_.height))
            .getOrElse(4)
          val cachedRect = contract
            .overlayRect(closedSurface.id)
            .getOrElse(LayoutRect(12, 2, 56, overlayHeight))
          val overlayFadeOutAnims = (0 until overlayHeight).map { rowOffset =>
            val panelBg  = s.theme.panel.background
            val panelFg  = s.theme.panel.foreground
            val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
            val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
            val previousCell = prevState.surfaceAnimations
              .get(closedSurface.id)
              .flatMap(_.animationState.getCell(0, rowOffset))
            val currentBg = previousCell.flatMap(_.currentBackground).getOrElse(panelBg)
            val currentFg = previousCell.flatMap(_.currentForeground).getOrElse(panelFg)
            val reversedSteps = previousCell
              .map(cell =>
                completedFadeSteps(totalFadeFrames = rowOffset + steps, remainingFrames = cell.backgroundSteps.length)
              )
              .getOrElse(steps)
            val bgSteps = RgbInterpolator.interpolateRgba(currentBg, transpBg, reversedSteps)
            val fgSteps = RgbInterpolator.interpolateRgba(currentFg, transpFg, reversedSteps)
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
      case _ =>
        stateRef.update(s => s.copy(surfaceAnimations = s.surfaceAnimations - closedSurface.id))

  private def animatedCommandSurfaces(state: AppState): List[UiSurface] =
    state.uiSurfaces.filter {
      _.content match
        case SurfaceContent.CommandPalette(_)              => true
        case SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
        case _                                             => false
    }

  private def matchingExitingCommandGhost(surface: UiSurface, state: AppState): Option[UiSurface] =
    state.uiSurfaces.find {
      case UiSurface(id, SurfaceContent.GhostOverlay(content, _), _, _) =>
        state.surfaceAnimations.get(id).exists(_.phase == SurfacePhase.Exiting) &&
        ((surface.content, content) match
          case (SurfaceContent.CommandPalette(_), SurfaceContent.CommandPalette(_))                           => true
          case (SurfaceContent.CommandPaletteSubmenu(_, _, _), SurfaceContent.CommandPaletteSubmenu(_, _, _)) => true
          case _                                                                                              => false)
      case _ => false
    }

  private def animatedPanelSurfaces(state: AppState): List[UiSurface] =
    state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _)   => true
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }

  private def applyPinnedPanelOpenAnimation(surface: UiSurface): cats.effect.IO[Unit] =
    stateRef.update { state =>
      val viewportSize = state.viewportSize.getOrElse(ViewportSize(80, 24))
      val layout       = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val contract     = EditorLayoutContract.from(state, viewportSize, layout)
      val maybeAnimation =
        for
          position  <- panelPosition(surface)
          rect      <- contract.panelRect(surface.id)
          animation <- pinnedPanelOpenAnimation(position, rect, state)
        yield animation

      maybeAnimation
        .map(animation => state.copy(surfaceAnimations = state.surfaceAnimations + (surface.id -> animation)))
        .getOrElse(state)
    }

  private def applyPinnedPanelCloseAnimation(closedSurface: UiSurface, prevState: AppState): cats.effect.IO[Unit] =
    stateRef.update { state =>
      val tSize          = prevState.viewportSize.orElse(state.viewportSize).getOrElse(ViewportSize(80, 24))
      val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
      val contract       = EditorLayoutContract.from(prevState, tSize, previousLayout)
      val maybeGhost =
        for
          position  <- panelPosition(closedSurface)
          rect      <- contract.panelRect(closedSurface.id)
          animation <- pinnedPanelCloseAnimation(position, rect, state)
        yield
          val (stateWithId, ghostId) = state.allocateSurfaceId
          val ghostSurface = UiSurface(
            id = ghostId,
            content = SurfaceContent.GhostOverlay(closedSurface.content, rect),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
          stateWithId.copy(
            uiSurfaces = stateWithId.uiSurfaces :+ ghostSurface,
            surfaceAnimations = stateWithId.surfaceAnimations
              - closedSurface.id
              + (ghostId -> animation)
          )

      maybeGhost.getOrElse(state)
    }

  private def pinnedPanelOpenAnimation(
    position: PanelPosition,
    rect: LayoutRect,
    state: AppState
  ): Option[SurfaceAnimationState] =
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, Some(position)),
      state.config.pinnedPanelTransitionSettings
    )
    val animationState = ElementTransitionLowerer.lower(plan, pinnedPanelOpenCells(rect, state), tickRateMs = 16)
    Option.when(animationState.hasActiveAnimations)(
      SurfaceAnimationState(
        phase = SurfacePhase.Visible,
        animationState = animationState,
        overlayHeight = rect.height,
        bufferFadeLength = 0,
        phaseTick = 0
      )
    )

  private def pinnedPanelCloseAnimation(
    position: PanelPosition,
    rect: LayoutRect,
    state: AppState
  ): Option[SurfaceAnimationState] =
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelClose, Some(position)),
      state.config.pinnedPanelTransitionSettings
    )
    val animationState = ElementTransitionLowerer.lower(plan, pinnedPanelCloseCells(rect, state), tickRateMs = 16)
    Option.when(animationState.hasActiveAnimations)(
      SurfaceAnimationState(
        phase = SurfacePhase.Exiting,
        animationState = animationState,
        overlayHeight = rect.height,
        bufferFadeLength = 0,
        phaseTick = 0
      )
    )

  private def pinnedPanelOpenCells(rect: LayoutRect, state: AppState): ElementTransitionCells =
    val transparentPanelForeground = transparent(state.theme.panel.foreground)
    val transparentBorder          = transparent(state.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', transparentBorder, state.theme.border)
    val contentCells =
      (0 until math.max(0, rect.height - 1)).flatMap { row =>
        (0 until math.max(0, rect.width - 2)).map { column =>
          CharacterKey(column, row) ->
            CellAnimation(' ', transparentPanelForeground, state.theme.panel.foreground)
        }
      }.toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def pinnedPanelCloseCells(rect: LayoutRect, state: AppState): ElementTransitionCells =
    val transparentPanelForeground = transparent(state.theme.panel.foreground)
    val transparentBorder          = transparent(state.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', state.theme.border, transparentBorder)
    val contentCells =
      (0 until math.max(0, rect.height - 1)).flatMap { row =>
        (0 until math.max(0, rect.width - 2)).map { column =>
          CharacterKey(column, row) ->
            CellAnimation(' ', state.theme.panel.foreground, transparentPanelForeground)
        }
      }.toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def transparent(color: Color): Color =
    new Color(color.getRed, color.getGreen, color.getBlue, 0)

  private def completedFadeSteps(totalFadeFrames: Int, remainingFrames: Int): Int =
    (totalFadeFrames - remainingFrames + 1).max(1)

  private def panelPosition(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case _                                         => None

  private[manager] def advanceSurfaceAnimations(state: AppState): AppState =
    state.surfaceAnimations.foldLeft(state) {
      case (s, (surfaceId, surfAnim)) =>
        surfAnim.phase match
          case SurfacePhase.BufferFadingOut =>
            val newTick = surfAnim.phaseTick + 1
            if newTick >= surfAnim.bufferFadeLength then
              val overlayFadeIn = s.config.scaledUiAnimation.fold(Map.empty[CharacterKey, AnimatedCell]) { config =>
                (0 until surfAnim.overlayHeight).map { rowOffset =>
                  val delay    = rowOffset
                  val panelBg  = s.theme.panel.background
                  val panelFg  = s.theme.panel.foreground
                  val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
                  val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
                  val bgSteps = List.fill(delay)(transpBg) ++
                    RgbInterpolator.interpolateRgba(transpBg, panelBg, config.steps)
                  val fgSteps = List.fill(delay)(transpFg) ++
                    RgbInterpolator.interpolateRgba(transpFg, panelFg, config.steps)
                  CharacterKey(0, rowOffset) -> AnimatedCell(
                    content = None,
                    foregroundSteps = fgSteps,
                    backgroundSteps = bgSteps
                  )
                }.toMap
              }
              val newSurfAnim = surfAnim.copy(
                phase = SurfacePhase.Visible,
                animationState = AnimationState(overlayFadeIn),
                phaseTick = 0
              )
              s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> newSurfAnim))
            else s.copy(surfaceAnimations = s.surfaceAnimations + (surfaceId -> surfAnim.copy(phaseTick = newTick)))

          case SurfacePhase.Visible =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then s.copy(surfaceAnimations = s.surfaceAnimations - surfaceId)
            else
              s.copy(surfaceAnimations =
                s.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState))
              )

          case SurfacePhase.Exiting =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then
              s.copy(
                uiSurfaces = s.uiSurfaces.filterNot(_.id == surfaceId),
                surfaceAnimations = s.surfaceAnimations - surfaceId
              )
            else
              s.copy(surfaceAnimations =
                s.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState))
              )
    }

  private def applyComponentResult(result: ComponentResult, state: AppState): cats.effect.IO[AppState] =
    result match
      case ComponentResult.NoChange            => cats.effect.IO.pure(state)
      case ComponentResult.StateChange(update) => cats.effect.IO.pure(update(state))
      case ComponentResult.ReducerUpdate(result) =>
        applyReducerResult(result, state) >> stateRef.get
      case ComponentResult.FocusTransfer(newFocus) => cats.effect.IO.pure(state.copy(focus = newFocus))
      case ComponentResult.Dismiss =>
        val dismissedState = dismissCurrentFocus(state)
        dismissedState.layout.activeEditorPaneId match
          case Some(paneId) =>
            cats.effect.IO.pure(dismissedState.copy(focus = Focus.EditorPane(paneId)))
          case None =>
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
                                        s.buffers.get(buffer.id) match
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
                                              s.copy(
                                                buffers = s.buffers.updated(
                                                  buffer.id,
                                                  current.copy(
                                                    cursors = List(focusCursor),
                                                    selection = selection,
                                                    selections = Nil,
                                                    preferredColumn = Some(focusCursor.column),
                                                    preferredXPx = None,
                                                    multiCursorVerticalStates = Nil
                                                  )
                                                ),
                                                focus = Focus.EditorPane(paneId),
                                                layout = s.layout.copy(activeEditorPaneId = Some(paneId))
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
            val modalType = this.modalType(modal)
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
      viewportSize <- state.viewportSize
      surface      <- state.topModalSurface.orElse(focusedFloatingModalWorkflow(state))
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(surface.id))
      _ <- Option.when(node.frameRect.contains(click.col, click.row))(())
      modal <- surface.content match
        case SurfaceContent.ModalWorkflow(modal) => Some(modal)
        case _                                   => None
      targetRows = SurfaceFrameLayout.minimumTargetRows(state.config.interfaceDensity)
      hit <- ModalSurfaceComposition
        .forModal(modal, node.frameRect, targetRows)
        .flatMap(_.hitAt(click.col.toDouble, click.row.toDouble))
    yield (modal, hit)

  private def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    for
      surfaceId <- state.focus match
        case Focus.Surface(id) => Some(id)
        case _                 => None
      surface <- state.uiSurfaces.find(_.id == surfaceId)
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
            viewportSize <- state.viewportSize
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
                          s.buffers.get(buffer.id) match
                            case Some(current) =>
                              val selection =
                                Option.when(press.shiftDown)(rangeSelectionFromAnchor(current, pressedCursor)).flatten
                              val focusCursor = selection.map(_.focus).getOrElse(pressedCursor)
                              s.copy(
                                buffers = s.buffers.updated(
                                  buffer.id,
                                  current.copy(
                                    cursors = List(focusCursor),
                                    selection = selection,
                                    selections = Nil,
                                    preferredColumn = Some(focusCursor.column),
                                    preferredXPx = None,
                                    multiCursorVerticalStates = Nil
                                  )
                                ),
                                focus = Focus.EditorPane(paneId),
                                layout = s.layout.copy(activeEditorPaneId = Some(paneId))
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
                      s.buffers.get(buffer.id) match
                        case Some(current) =>
                          val anchor =
                            current.primarySelection
                              .map(_.anchor)
                              .orElse(current.cursors.headOption)
                              .getOrElse(draggedCursor)
                          val selection =
                            Option.when(anchor != draggedCursor)(Selection(anchor, draggedCursor))
                          s.copy(
                            buffers = s.buffers.updated(
                              buffer.id,
                              current.copy(
                                cursors = List(draggedCursor),
                                selection = selection,
                                selections = Nil,
                                preferredColumn = Some(draggedCursor.column),
                                preferredXPx = None,
                                multiCursorVerticalStates = Nil
                              )
                            ),
                            focus = Focus.EditorPane(paneId),
                            layout = s.layout.copy(activeEditorPaneId = Some(paneId))
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

  private case class PinnedDirectoryMouseHit(
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
            pinnedDiagnosticsMouseHitAt(event, state) match
              case Some((surface, issues, location)) =>
                stateRef.update(selectPinnedDiagnosticsLocation(_, surface, issues, location)).as(true)
              case None =>
                cats.effect.IO.pure(false)
    }

  private def handlePinnedPanelLocationClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    if click.button != MouseButton.Primary then cats.effect.IO.pure(false)
    else
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
    val updatedSurfaces = state.uiSurfaces.map {
      case surface if surface.id == hit.surface.id => surface.copy(content = updatedContent)
      case surface                                 => surface
    }
    val nextFocus = if focusPanel then Focus.Surface(hit.surface.id) else state.focus
    state.copy(uiSurfaces = updatedSurfaces, focus = nextFocus)

  private def selectPinnedOutlineLocation(
    state: AppState,
    surface: UiSurface,
    symbols: List[Symbol],
    location: Location
  ): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Outline(symbols, Some(location)))
      case existing =>
        existing
    })

  private def selectPinnedDiagnosticsLocation(
    state: AppState,
    surface: UiSurface,
    issues: List[Diagnostic],
    location: Location
  ): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Diagnostics(issues, Some(location)))
      case existing =>
        existing
    })

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
    state.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.buffers.get) match
          case Some(buffer) =>
            val line     = math.max(0, math.min(location.line, math.max(0, buffer.content.lineCount - 1)))
            val column   = math.max(0, math.min(location.column, buffer.content.getLine(line).getOrElse("").length))
            val cursor   = CursorPosition(line, column)
            val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
            val updatedBuffer = buffer.copy(
              cursors = List(cursor),
              selection = None,
              selections = Nil,
              preferredColumn = Some(cursor.column),
              preferredXPx = None,
              multiCursorVerticalStates = Nil,
              viewport = viewport
            )
            state.copy(
              buffers = state.buffers.updated(buffer.id, updatedBuffer),
              focus = Focus.EditorPane(paneId),
              layout = state.layout.copy(activeEditorPaneId = Some(paneId))
            )
          case None =>
            state
      case None =>
        state

  private case class PinnedPanelRowHit(
      surface: UiSurface,
      position: PanelPosition,
      rowIndex: Int,
      layoutKind: SurfaceLayoutKind
  )

  private def pinnedPanelRowHitAt(event: MouseInputEvent, state: AppState): Option[PinnedPanelRowHit] =
    state.viewportSize.flatMap { viewportSize =>
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
        stateRef.update(_.copy(hoveredEditorTarget = Some(HoveredEditorTarget(paneId, buffer.id, cursor))))
      case None =>
        clearEditorHoverTarget
    }

  private def clearEditorHoverTarget: cats.effect.IO[Unit] =
    stateRef.update(_.copy(hoveredEditorTarget = None))

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
                .copy(uiSurfaces = current.uiSurfaces.filterNot(isContextMenuSurface) :+ surface)
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
            current.copy(uiSurfaces = current.uiSurfaces.map {
              case existing if existing.id == surface.id =>
                existing.copy(content = SurfaceContent.ContextMenu(menu.withSelectedIndex(index)))
              case existing => existing
            })
          }
          .as(true)
      case None =>
        cats.effect.IO.pure(false)

  private def handleContextMenuMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Boolean] =
    contextMenuSelectionAt(click, state) match
      case Some((_, menu, index)) =>
        menu.items.lift(index) match
          case Some(item) =>
            stateRef.update(current => dismissContextMenu(current).copy(focus = menu.targetFocus)) >>
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
      viewportSize <- state.viewportSize
      surface      <- state.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
      layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      contract = EditorLayoutContract.from(state, viewportSize, layout)
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
        itemGapRows = state.config.commandRunnerItemGapRows,
        itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.config.interfaceDensity)
      )
    yield (surface, menu, index)

  private def isContextMenuItemGap(event: MouseInputEvent, state: AppState): Boolean =
    (for
      viewportSize <- state.viewportSize
      surface      <- state.contextMenuSurface
      layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      contract = EditorLayoutContract.from(state, viewportSize, layout)
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
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(isContextMenuSurface)).popFocus

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
              updated.copy(focus = editorFocus(current))
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
        stateRef.update(current =>
          replaceContextualToolbar(current, surface, detailState.closeDetail).copy(focus = editorFocus(current))
        ) >>
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
      viewportSize <- state.viewportSize
      surface      <- state.contextualToolbarSurface
      toolbarState <- surface.content match
        case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
        case _                                              => None
      layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      contract = EditorLayoutContract.from(state, viewportSize, layout)
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
              itemGapRows = state.config.uiElementGap,
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(
                SurfaceContent.ContextualToolbar(toolbarState),
                state.config.interfaceDensity
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
            state.config.interfaceDensity
          )
        )
    rowIndex.flatMap { rowIndex =>
      ContextualToolbar.hitAt(
        rowIndex = rowIndex,
        columnOffset = event.col - contentRect.x,
        contentWidth = contentRect.width.max(1),
        toolbarState = toolbarState,
        state = state
      )
    }

  private def isInsideFloatingSurface(event: MouseInputEvent, state: AppState): Boolean =
    state.viewportSize.exists { viewportSize =>
      val layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val contract = EditorLayoutContract.from(state, viewportSize, layout)
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
    CellMetrics.fromFont(FontLoader.previewCodeFont(state.config.fontConfig))

  private def replaceContextualToolbar(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.ContextualToolbar(toolbarState))
      case existing =>
        existing
    })

  private def editorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId
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
    state.viewportSize.flatMap { viewportSize =>
      val layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val contract = EditorLayoutContract.from(state, viewportSize, layout)
      val surfaces =
        event match
          case _: MouseMove
              if state.commandRunnerSubmenuSurface.exists(surface => state.focus == Focus.Surface(surface.id)) =>
            state.commandRunnerSubmenuSurface.toList
          case _ =>
            List(state.commandRunnerSubmenuSurface, state.commandRunnerSurface).flatten
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
    state.viewportSize.flatMap(viewportSize =>
      LayoutEngine.pinnedPanelResizeFromDrag(state, viewportSize, drag.col, drag.row)
    ) match
      case Some(LayoutEngine.PinnedPanelDragResize(position, size)) =>
        resizePinnedPanel(position, size).as(true)
      case None =>
        cats.effect.IO.pure(false)

  private enum TextAreaInsetDrag:
    case Left(value: Double)
    case Right(value: Double)
    case Top(value: Double)
    case Bottom(value: Double)

  private def textAreaInsetFromDrag(drag: MouseDrag, state: AppState): Option[TextAreaInsetDrag] =
    state.viewportSize.flatMap { viewportSize =>
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
              itemGapRows = state.config.commandRunnerItemGapRows,
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.config.interfaceDensity)
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
                  itemGapRows = state.config.commandRunnerItemGapRows,
                  itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.config.interfaceDensity)
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
            itemGapRows = state.config.commandRunnerItemGapRows,
            itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.config.interfaceDensity)
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
    state.viewportSize match
      case None => cats.effect.IO.pure(None)
      case Some(tSize) =>
        mouseTargetLayout(state, tSize).flatMap { cache =>
          cache.scene.paneLayouts.find {
            case (_, paneLayout) =>
              paneLayout.contentRect.contains(click.col, click.row)
          } match
            case Some((paneId, paneLayout)) =>
              state.layout.editorPanes.get(paneId).flatMap(pane => pane.bufferId.flatMap(state.buffers.get)) match
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
                        val clampedLine = bufferLine.min(math.max(0, buffer.content.lineCount - 1))
                        val lineLen     = buffer.content.getLine(clampedLine).getOrElse("").length
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
    val source        = RopeCharacterSource(buffer.content)
    val clickedOffset = buffer.content.lineColumnToOffset(cursor.line, cursor.column)
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
            offsetToCursorPosition(buffer.content, start),
            offsetToCursorPosition(buffer.content, end)
          )
        )

  private def lineSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val lineText = buffer.content.getLine(cursor.line).getOrElse("")
    Some(
      Selection(
        CursorPosition(cursor.line, 0),
        CursorPosition(cursor.line, lineText.length)
      )
    )

  private def rangeSelectionFromAnchor(buffer: Buffer, focus: CursorPosition): Option[Selection] =
    val anchor = buffer.primarySelection.map(_.anchor).orElse(buffer.cursors.headOption).getOrElse(focus)
    Option.when(anchor != focus)(Selection(anchor, focus))

  private def offsetToCursorPosition(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')

  private def modalType(modal: Modal): ModalType =
    modal match
      case Modal.GotoLine(_)        => ModalType.GotoLine
      case Modal.Find(_, _, _)      => ModalType.Find
      case Modal.FileWorkflow(_)    => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_) => ModalType.ReplaceWorkflow
      case Modal.CloseWorkflow(_)   => ModalType.CloseWorkflow
      case Modal.Custom(name, _)    => ModalType.Custom(name)
