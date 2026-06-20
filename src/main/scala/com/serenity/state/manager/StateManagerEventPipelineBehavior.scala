package com.serenity.state.manager

import java.awt.Color

import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, PendingGroup}
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset

private[manager] trait StateManagerEventPipelineBehavior extends StateManagerEffectBehavior:
  this: StateManager =>

  private val ContextMenuSurfaceId = SurfaceId("context-menu")

  private val EditorContextMenuCommands =
    List(
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
      val handleEvent: cats.effect.IO[Unit] = event match
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
      syncFocus >> handleEvent >> recordUndoableEdit(event, prevState) >> applyAnimationHooks(prevState)
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

  protected def validateAndUpdateState(newState: AppState, fallbackState: AppState): cats.effect.IO[Unit] =
    normalizeCommandRunnerFocus(newState).validated match
      case Right(validState) =>
        val diagnosticState = SpellChecker.refreshDiagnostics(validState)
        val modalTransitionLog =
          (fallbackState.modalSurface, diagnosticState.modalSurface) match
            case (before, after) if before != after =>
              logger.info(
                s"[STATE MODAL] before=${before.map(_.id).getOrElse("none")} " +
                  s"after=${after.map(_.id).getOrElse("none")} focus=${diagnosticState.focus}"
              )
            case _ =>
              cats.effect.IO.unit
        modalTransitionLog >> stateRef.set(diagnosticState)
      case Left(errors) =>
        logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
          stateRef.set(fallbackState)

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
              case SurfacePresentation.Floating(_, _) =>
                surface.content match
                  case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) =>
                    val registry = CommandRegistry.withToggleUI
                    new CommandRunnerComponent(registry)
                  case SurfaceContent.ThemePicker(_) =>
                    new ThemePickerComponent()
                  case SurfaceContent.FileSearch(_) =>
                    new FileSearchComponent()
                  case SurfaceContent.StartPage(_) =>
                    new StartupPageComponent()
                  case SurfaceContent.ModalWorkflow(modal) =>
                    new ModalComponent(modalType(modal))
                  case _ =>
                    new PeekOverlayComponent()
          case None =>
            NoOpLocalEventHandler

  private def applyReducerResult(result: ReducerResult, fallbackState: AppState): cats.effect.IO[Unit] =
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
      val steps = AnimationConfig.smooth.get.steps
      val animOpt = for
        paneId <- state.layout.activeEditorPaneId
        pane   <- state.layout.editorPanes.get(paneId)
        buffId <- pane.bufferId
        buffer <- state.buffers.get(buffId)
        vp = buffer.viewport
        cells = (vp.topLine until (vp.topLine + vp.visibleLines)).flatMap { lineIdx =>
          val line = buffer.content.getLine(lineIdx).getOrElse("")
          line.zipWithIndex.take(vp.visibleColumns).map { (ch, col) =>
            CharacterKey(col, lineIdx) -> CellAnimation(ch, state.theme.background, state.theme.foreground)
          }
        }.toMap
        if cells.nonEmpty
      yield
        val animated = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, sweep, steps)
        val newAnims = buffer.animations.clearAll().mergeAnimations(animated)
        state.copy(buffers = state.buffers.updated(buffId, buffer.copy(animations = newAnims)))
      animOpt match
        case Some(newState) => stateRef.set(newState)
        case None           => cats.effect.IO.unit
    }

  protected def applyAnimationHooks(prevState: AppState): cats.effect.IO[Unit] =
    stateRef.get.flatMap { currentState =>
      val prevSurfaces    = animatedCommandSurfaces(prevState)
      val currentSurfaces = animatedCommandSurfaces(currentState)
      val openedSurfaces =
        currentSurfaces.filter(surface => !prevSurfaces.exists(_.id == surface.id))
      val closedSurfaces =
        prevSurfaces.filter(surface => !currentSurfaces.exists(_.id == surface.id))
      val prevPanels    = animatedPanelSurfaces(prevState)
      val currentPanels = animatedPanelSurfaces(currentState)
      val openedPanels =
        currentPanels.filter(surface => !prevPanels.exists(_.id == surface.id))

      openedSurfaces.traverse_(surface => applyCommandRunnerOpenAnimation(surface, currentState)) >>
        closedSurfaces.traverse_(surface => applyCommandRunnerCloseAnimation(surface, prevState, currentState)) >>
        openedPanels.traverse_(surface => applyPinnedPanelOpenAnimation(surface))
    }

  private def applyCommandRunnerOpenAnimation(surface: UiSurface, state: AppState): cats.effect.IO[Unit] =
    val steps = AnimationConfig.smooth.get.steps
    stateRef.update { s =>
      val tSize         = s.viewportSize.getOrElse(ViewportSize(80, 24))
      val layout        = LayoutEngine.calculateLayoutWithUI(s, tSize)
      val overlayHeight = overlayRectForSurface(layout, surface.id).map(_.height).getOrElse(4)
      val overlayFadeIn = (0 until overlayHeight).map { rowOffset =>
        val delay    = rowOffset
        val panelBg  = s.theme.panel.background
        val panelFg  = s.theme.panel.foreground
        val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
        val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
        val bgSteps = List.fill(delay)(transpBg) ++
          RgbInterpolator.interpolateRgba(transpBg, panelBg, steps)
        val fgSteps = List.fill(delay)(transpFg) ++
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
  ): cats.effect.IO[Unit] =
    val steps = AnimationConfig.smooth.get.steps
    stateRef.update { s =>
      val tSize          = prevState.viewportSize.orElse(s.viewportSize).getOrElse(ViewportSize(80, 24))
      val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
      val overlayHeight = prevState.surfaceAnimations
        .get(closedSurface.id)
        .map(_.overlayHeight)
        .orElse(overlayRectForSurface(previousLayout, closedSurface.id).map(_.height))
        .getOrElse(4)
      val cachedRect = overlayRectForSurface(previousLayout, closedSurface.id)
        .getOrElse(LayoutRect(12, 2, 56, overlayHeight))
      val overlayFadeOutAnims = (0 until overlayHeight).map { rowOffset =>
        val panelBg  = s.theme.panel.background
        val panelFg  = s.theme.panel.foreground
        val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
        val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
        val bgSteps  = RgbInterpolator.interpolateRgba(panelBg, transpBg, steps)
        val fgSteps  = RgbInterpolator.interpolateRgba(panelFg, transpFg, steps)
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

  private def animatedCommandSurfaces(state: AppState): List[UiSurface] =
    state.uiSurfaces.filter {
      _.content match
        case SurfaceContent.CommandPalette(_)              => true
        case SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
        case _                                             => false
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
      val maybeAnimation =
        for
          position <- panelPosition(surface)
          rect <- panelRectForSurface(
            LayoutEngine.calculateLayoutWithUI(state, state.viewportSize.getOrElse(ViewportSize(80, 24))),
            surface
          )
          animation <- pinnedPanelOpenAnimation(position, rect, state)
        yield animation

      maybeAnimation
        .map(animation => state.copy(surfaceAnimations = state.surfaceAnimations + (surface.id -> animation)))
        .getOrElse(state)
    }

  private def pinnedPanelOpenAnimation(
    position: PanelPosition,
    rect: LayoutRect,
    state: AppState
  ): Option[SurfaceAnimationState] =
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, Some(position)),
      state.config.elementTransitionSettings
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

  private def transparent(color: Color): Color =
    new Color(color.getRed, color.getGreen, color.getBlue, 0)

  private def panelPosition(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case _                                         => None

  private def panelRectForSurface(layout: CalculatedLayout, surface: UiSurface): Option[LayoutRect] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _) =>
        layout.pinnedSurfaceRects.get(surface.id).orElse(layout.pinnedPanelRects.get(position))
      case SurfacePresentation.Expanded(_, _) =>
        layout.expandedPanelRect
      case _ =>
        None

  private def overlayRectForSurface(layout: CalculatedLayout, surfaceId: SurfaceId): Option[LayoutRect] =
    layout.aboveCursorOverlayStack
      .find(_._1 == surfaceId)
      .map(_._2)
      .orElse(layout.belowCursorOverlayStack.find(_._1 == surfaceId).map(_._2))

  protected def advanceSurfaceAnimations(state: AppState): AppState =
    state.surfaceAnimations.foldLeft(state) {
      case (s, (surfaceId, surfAnim)) =>
        surfAnim.phase match
          case SurfacePhase.BufferFadingOut =>
            val newTick = surfAnim.phaseTick + 1
            if newTick >= surfAnim.bufferFadeLength then
              val overlayFadeIn = (0 until surfAnim.overlayHeight).map { rowOffset =>
                val delay    = rowOffset
                val panelBg  = s.theme.panel.background
                val panelFg  = s.theme.panel.foreground
                val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
                val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
                val bgSteps = List.fill(delay)(transpBg) ++
                  RgbInterpolator.interpolateRgba(transpBg, panelBg, AnimationConfig.smooth.get.steps)
                val fgSteps = List.fill(delay)(transpFg) ++
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

  protected def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry = CommandRegistry.default
    val runner   = CommandRunner.empty.activate(registry, state.config)
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(runner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId
      .copy(
        uiSurfaces = stateWithId.uiSurfaces.filterNot(_.id == surfaceId) :+ surface
      )
      .pushFocus(Focus.Surface(surfaceId))

  private def handleMouseClick(click: MouseClick, state: AppState): cats.effect.IO[Unit] =
    click.button match
      case MouseButton.Secondary =>
        openEditorContextMenu(click, state)
      case MouseButton.Primary =>
        handleContextMenuMouseClick(click, state).flatMap {
          case true => cats.effect.IO.unit
          case false =>
            handleCommandRunnerMouseClick(click, state).flatMap {
              case true => cats.effect.IO.unit
              case false =>
                resolveMouseTarget(click, state).flatMap {
                  _.fold(dismissContextMenuIfOpen(state)) { (paneId, buffer, clickedCursor) =>
                    stateRef.update { s =>
                      s.buffers.get(buffer.id) match
                        case Some(current) =>
                          val selection =
                            if click.shiftDown then rangeSelectionFromAnchor(current, clickedCursor)
                            else if click.clickCount >= 3 then lineSelectionAtCursor(current, clickedCursor)
                            else if click.clickCount >= 2 then wordSelectionAtCursor(current, clickedCursor)
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
      case _ =>
        cats.effect.IO.unit

  private def handleMousePress(press: MousePress, state: AppState): cats.effect.IO[Unit] =
    if press.button != MouseButton.Primary then cats.effect.IO.unit
    else
      handleCommandRunnerMouseHover(press, state).flatMap {
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

  private def handleMouseDrag(drag: MouseDrag, state: AppState): cats.effect.IO[Unit] =
    if drag.button != MouseButton.Primary then cats.effect.IO.unit
    else
      handleTextAreaResizeDrag(drag, state).flatMap {
        case true => cats.effect.IO.unit
        case false =>
          resolveMouseTarget(drag, state).flatMap {
            _.fold(cats.effect.IO.unit) { (paneId, buffer, draggedCursor) =>
              stateRef.update { s =>
                s.buffers.get(buffer.id) match
                  case Some(current) =>
                    val anchor =
                      current.primarySelection.map(_.anchor).orElse(current.cursors.headOption).getOrElse(draggedCursor)
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

  private def handleMouseMove(move: MouseMove, state: AppState): cats.effect.IO[Unit] =
    handleContextMenuMouseHover(move, state).flatMap {
      case true => clearEditorHoverTarget
      case false =>
        handleCommandRunnerMouseHover(move, state).flatMap {
          case true  => clearEditorHoverTarget
          case false => updateEditorHoverTarget(move, state)
        }
    }

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
      layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      rect  <- overlayRectForSurface(layout, surface.id)
      index <- overlayItemIndex(event, rect, menu.items.length, menu.selectedIndex)
    yield (surface, menu, index)

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
          stateRef.get
            .flatMap { selectedState =>
              val submitted = CommandRunnerReducer.reduce(RunnerSubmit, selectedState, registry)
              applyReducerResult(submitted, selectedState)
            }
            .map(_ => true)
      case None =>
        cats.effect.IO.pure(false)

  private def commandRunnerSelectionAt(event: MouseInputEvent, state: AppState): Option[CommandRunnerEvent] =
    state.viewportSize.flatMap { viewportSize =>
      val layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val surfaces = List(state.commandRunnerSubmenuSurface, state.commandRunnerSurface).flatten
      surfaces.view.flatMap(surface => commandRunnerSelectionForSurface(event, layout, surface)).headOption
    }

  private def handleTextAreaResizeDrag(drag: MouseDrag, state: AppState): cats.effect.IO[Boolean] =
    textAreaInsetFromDrag(drag, state) match
      case Some(Left(value)) =>
        updateConfig(_.withTextAreaLeftInset(value)).map(_ => true)
      case Some(Right(value)) =>
        updateConfig(_.withTextAreaRightInset(value)).map(_ => true)
      case None =>
        cats.effect.IO.pure(false)

  private def textAreaInsetFromDrag(drag: MouseDrag, state: AppState): Option[Either[Double, Double]] =
    state.viewportSize.flatMap { viewportSize =>
      val layout         = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val workspaceX     = layout.leftSpacerRect.x
      val workspaceRight = layout.rightSpacerRect.right
      val workspaceWidth = (workspaceRight - workspaceX).max(1)
      val withinWorkspaceY =
        drag.row >= layout.leftSpacerRect.y && drag.row < layout.leftSpacerRect.bottom

      if withinWorkspaceY && drag.col >= layout.leftSpacerRect.x && drag.col < layout.leftSpacerRect.right then
        Some(Left((drag.col - workspaceX).toDouble / workspaceWidth.toDouble))
      else if withinWorkspaceY && drag.col >= layout.rightSpacerRect.x && drag.col < layout.rightSpacerRect.right then
        Some(Right((workspaceRight - drag.col).toDouble / workspaceWidth.toDouble))
      else None
    }

  private def commandRunnerSelectionForSurface(
    event: MouseInputEvent,
    layout: CalculatedLayout,
    surface: UiSurface
  ): Option[CommandRunnerEvent] =
    overlayRectForSurface(layout, surface.id).flatMap { rect =>
      surface.content match
        case SurfaceContent.CommandPalette(runner) =>
          overlayItemIndex(event, rect, runner.visibleItems.length, runner.selectedIndex)
            .map(RunnerSelectVisibleItem(_))
        case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) if !previewOnly =>
          val submenuState = runner.activeSubmenu.filter(_.groupId == groupId)
          val items = submenuState
            .map(_.filteredItems(runner.submenuItems(groupId)))
            .getOrElse(runner.submenuItems(groupId))
          val selectedIndex = submenuState.map(_.selectedIndex).getOrElse(0)
          overlayItemIndex(event, rect, items.length, selectedIndex).map(RunnerSelectSubmenuItem(_))
        case _ =>
          None
    }

  private def overlayItemIndex(
    event: MouseInputEvent,
    rect: LayoutRect,
    itemCount: Int,
    selectedIndex: Int
  ): Option[Int] =
    val insideColumns = event.col > rect.x && event.col < rect.right - 1
    val maxItemRows   = math.max(1, rect.height - 4)
    val itemRow       = event.row - rect.y - 2
    val windowSize    = math.min(itemCount, maxItemRows)
    if !insideColumns || itemRow < 0 || itemRow >= windowSize then None
    else
      val offset =
        if itemCount <= maxItemRows then 0
        else
          val half = maxItemRows / 2
          math.max(0, math.min(selectedIndex - half, itemCount - maxItemRows))
      Some(offset + itemRow)

  private def resolveMouseTarget(
    click: MouseInputEvent,
    state: AppState
  ): cats.effect.IO[Option[(PaneId, Buffer, CursorPosition)]] =
    state.viewportSize match
      case None => cats.effect.IO.pure(None)
      case Some(tSize) =>
        mouseTargetLayout(state, tSize).flatMap { cache =>
          cache.paneLayouts.find {
            case (_, rect) =>
              click.col >= rect.x && click.col < rect.x + rect.width &&
              click.row > rect.y && click.row < rect.y + rect.height
          } match
            case Some((paneId, paneRect)) =>
              state.layout.editorPanes.get(paneId).flatMap(pane => pane.bufferId.flatMap(state.buffers.get)) match
                case Some(buffer) =>
                  val vp           = buffer.viewport
                  val contentY     = paneRect.y + 1
                  val visualRow    = (click.row - contentY).max(0)
                  val font         = previewFontForBuffer(buffer, state.config.fontConfig)
                  val metrics      = CellMetrics.fromFont(font)
                  val panelWidthPx = paneRect.width * metrics.charWidth
                  mouseTargetSnapshot(cache.layoutKey, buffer, state.config.fontConfig, panelWidthPx, font).map {
                    snapshot =>
                      val xPx = click.pixelX match
                        case Some(pixelX) => (pixelX - (paneRect.x * metrics.charWidth)).toFloat
                        case None         => ((click.col - paneRect.x).max(0) * metrics.charWidth).toFloat
                      val clickedCursor = snapshot
                        .cursorForVisualRowAndXPx(visualRow, xPx.max(0.0f))
                        .orElse {
                          val bufferLine  = (vp.topLine + visualRow).max(0)
                          val bufferCol   = (vp.leftColumn + (click.col - paneRect.x)).max(0)
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
        Some(cache) -> cache
      case _ =>
        val next = MouseTargetCache.fromState(state, viewportSize)
        Some(next) -> next
    }

  private def mouseTargetSnapshot(
    layoutKey: MouseTargetLayoutKey,
    buffer: Buffer,
    fontConfig: FontConfig,
    panelWidthPx: Int,
    font: java.awt.Font
  ): cats.effect.IO[TextLayoutSnapshot] =
    val key = MouseTargetSnapshotKey.from(buffer, fontConfig, panelWidthPx)
    mouseTargetCacheRef.modify {
      case Some(cache) if cache.layoutKey == layoutKey =>
        cache.snapshots.get(key) match
          case Some(snapshot) =>
            Some(cache) -> snapshot
          case None =>
            val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, font)
            Some(cache.copy(snapshots = cache.snapshots.updated(key, snapshot))) -> snapshot
      case other =>
        val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, font)
        other -> snapshot
    }

  private def wordSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val text          = buffer.content.collect()
    val clickedOffset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
    if text.isEmpty then None
    else
      val probeOffset =
        if clickedOffset >= text.length then text.length - 1
        else if text.charAt(clickedOffset).isWhitespace && clickedOffset > 0 && !text
              .charAt(clickedOffset - 1)
              .isWhitespace
        then clickedOffset - 1
        else clickedOffset
      if probeOffset < 0 || text.charAt(probeOffset).isWhitespace then None
      else
        val start = TextEditing.previousWordBoundary(text, probeOffset)
        @annotation.tailrec
        def wordEndFrom(offset: Int): Int =
          if offset < text.length && !text.charAt(offset).isWhitespace then wordEndFrom(offset + 1)
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

  private def lineColumnToOffset(content: com.serenity.rope.Rope, line: Int, column: Int): Int =
    val textLines        = content.collect().split("\n", -1)
    val clampedLine      = math.max(0, math.min(line, textLines.length - 1))
    val precedingLengths = textLines.take(clampedLine).map(_.length + 1).sum
    precedingLengths + math.max(0, math.min(column, textLines(clampedLine).length))

  private def offsetToCursorPosition(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, content.weight))
    content.collect().take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }

  private def modalType(modal: Modal): ModalType =
    modal match
      case Modal.GotoLine(_)        => ModalType.GotoLine
      case Modal.Find(_, _, _)      => ModalType.Find
      case Modal.FileWorkflow(_)    => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_) => ModalType.ReplaceWorkflow
      case Modal.CloseWorkflow(_)   => ModalType.CloseWorkflow
      case Modal.Custom(name, _)    => ModalType.Custom(name)

  private def previewFontForBuffer(
    buffer: Buffer,
    config: FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)
