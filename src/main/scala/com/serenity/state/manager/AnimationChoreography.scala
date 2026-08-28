package com.serenity.state.manager

import java.awt.Color

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.animation.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for surface and panel animation choreography. */
private[manager] trait AnimationChoreographyPort:
  def stateRef: Ref[IO, AppState]
  def bufferAnimationsRef: Ref[IO, Map[BufferId, AnimationState]]

/** Drives command-runner and pinned-panel open/close/transition animations, the buffer sweep animation used for
  * tab-cycling, and advances all in-flight surface animations by one tick. This orchestrates the existing
  * motion/animation model from `com.serenity.animation` (`AnimationState`, `ElementTransitionPlanner`/`Lowerer`,
  * `RgbInterpolator`, etc. -- landed via #846/#874) against `AppState`'s surface-animation runtime state; it does not
  * introduce a competing duration/easing/animation abstraction of its own.
  */
final private[manager] class AnimationChoreography(port: AnimationChoreographyPort):
  import port.*

  def applyPaneFlowAnimation(sweep: SweepDirection): IO[Unit] =
    stateRef.get.flatMap { state =>
      val animOpt = for
        config <- state.persisted.config.scaledUiAnimation
        paneId <- state.persisted.layout.activeEditorPaneId
        pane   <- state.persisted.layout.editorPanes.get(paneId)
        buffId <- pane.bufferId
        buffer <- state.persisted.buffers.get(buffId)
        cells = VisibleBufferAnimationCells.fromBuffer(
          buffer,
          state.persisted.config.wordWrapEnabled,
          state.persisted.theme.background,
          state.persisted.theme.foreground
        )
        if cells.nonEmpty
      yield
        val animated = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, sweep, config.steps)
        val uiAnimations =
          animated.view.mapValues(_.copy(owner = AnimationOwner.UiTransitions)).toMap
        buffId -> ((animations: AnimationState) =>
          animations
            .clear(AnimationOwner.UiTransitions)
            .mergeUiTransitionAnimations(uiAnimations)
        )
      animOpt match
        case Some((buffId, f)) =>
          bufferAnimationsRef.update(map => map.updated(buffId, f(map.getOrElse(buffId, AnimationState.empty))))
        case None => IO.unit
    }

  def applyAnimationHooks(prevState: AppState): IO[Unit] =
    if !shouldApplySurfaceAnimationHooks(prevState) then IO.unit
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

  def shouldApplySurfaceAnimationHooks(state: AppState): Boolean =
    state.runtime.surfaceAnimations.nonEmpty ||
      state.persisted.config.scaledCommandRunnerAnimation.exists(config => !config.isDisabled) ||
      state.persisted.config.pinnedPanelTransitionSettings.enabled

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

  private def applyCommandRunnerOpenAnimation(surface: UiSurface, state: AppState): IO[Unit] =
    state.persisted.config.scaledCommandRunnerAnimation match
      case Some(config) if !config.isDisabled =>
        stateRef.update { s =>
          val steps         = config.steps
          val tSize         = s.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
          val layout        = LayoutEngine.calculateLayoutWithUI(s, tSize)
          val contract      = EditorLayoutContract.from(s, tSize, layout)
          val overlayRect   = contract.overlayRect(surface.id)
          val overlayHeight = overlayRect.map(_.height).getOrElse(4)
          val exitingGhost  = matchingExitingCommandGhost(surface, s)
          val revealKind    = s.persisted.config.effectiveCommandRunnerTransitionKind
          val animationState =
            if revealKind == TransitionKind.Fade then
              commandRunnerFadeInAnimation(
                overlayHeight,
                steps,
                s,
                exitingGhost.flatMap(ghost => s.runtime.surfaceAnimations.get(ghost.id).map(_.animationState))
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
              s.runtime.surfaceAnimations + (surface.id -> SurfaceAnimationState(
                phase = SurfacePhase.Visible,
                animationState = animationState,
                overlayHeight = overlayHeight,
                bufferFadeLength = 0,
                phaseTick = 0
              ))
            else s.runtime.surfaceAnimations - surface.id
          exitingGhost.fold(s.copy(runtime = s.runtime.copy(surfaceAnimations = surfaceAnimations))) { ghost =>
            s.copy(runtime =
              s.runtime.copy(
                uiSurfaces = s.runtime.uiSurfaces.filterNot(_.id == ghost.id),
                surfaceAnimations = surfaceAnimations - ghost.id
              )
            )
          }
        }
      case _ =>
        stateRef.update(s =>
          s.copy(runtime = s.runtime.copy(surfaceAnimations = s.runtime.surfaceAnimations - surface.id))
        )

  private def commandRunnerFadeInAnimation(
    overlayHeight: Int,
    steps: Int,
    state: AppState,
    previous: Option[AnimationState]
  ): AnimationState =
    val overlayFadeIn = (0 until overlayHeight).map { rowOffset =>
      val delay        = rowOffset
      val panelBg      = state.persisted.theme.panel.background
      val panelFg      = state.persisted.theme.panel.foreground
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
    val transparentPanelForeground = transparent(state.persisted.theme.panel.foreground)
    val transparentBorder          = transparent(state.persisted.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', transparentBorder, state.persisted.theme.border)
    val contentCells =
      (0 until math.max(0, height - 1))
        .flatMap { row =>
          (0 until math.max(1, width - 2)).map { column =>
            CharacterKey(column, row) ->
              CellAnimation(' ', transparentPanelForeground, state.persisted.theme.panel.foreground)
          }
        }
        .take(VisibleBufferAnimationCells.DefaultMaxAnimatedCells)
        .toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def applyCommandRunnerCloseAnimation(
    closedSurface: UiSurface,
    prevState: AppState
  ): IO[Unit] =
    prevState.persisted.config.scaledCommandRunnerAnimation match
      case Some(config) if !config.isDisabled =>
        stateRef.update { s =>
          val steps = config.steps
          val tSize = prevState.runtime.viewportSize.orElse(s.runtime.viewportSize).getOrElse(ViewportSize(80, 24))
          val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
          val contract       = EditorLayoutContract.from(prevState, tSize, previousLayout)
          val overlayHeight = prevState.runtime.surfaceAnimations
            .get(closedSurface.id)
            .map(_.overlayHeight)
            .orElse(contract.overlayRect(closedSurface.id).map(_.height))
            .getOrElse(4)
          val cachedRect = contract
            .overlayRect(closedSurface.id)
            .getOrElse(LayoutRect(12, 2, 56, overlayHeight))
          val overlayFadeOutAnims = (0 until overlayHeight).map { rowOffset =>
            val panelBg  = s.persisted.theme.panel.background
            val panelFg  = s.persisted.theme.panel.foreground
            val transpBg = new Color(panelBg.getRed, panelBg.getGreen, panelBg.getBlue, 0)
            val transpFg = new Color(panelFg.getRed, panelFg.getGreen, panelFg.getBlue, 0)
            val previousCell = prevState.runtime.surfaceAnimations
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
          stateWithId.copy(runtime =
            stateWithId.runtime.copy(
              uiSurfaces = stateWithId.runtime.uiSurfaces :+ ghostSurface,
              surfaceAnimations = stateWithId.runtime.surfaceAnimations
                - closedSurface.id
                + (ghostId -> ghostAnimState)
            )
          )
        }
      case _ =>
        stateRef.update(s =>
          s.copy(runtime = s.runtime.copy(surfaceAnimations = s.runtime.surfaceAnimations - closedSurface.id))
        )

  private def animatedCommandSurfaces(state: AppState): List[UiSurface] =
    state.runtime.uiSurfaces.filter {
      _.content match
        case SurfaceContent.CommandPalette(_)              => true
        case SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
        case _                                             => false
    }

  private def matchingExitingCommandGhost(surface: UiSurface, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.find {
      case UiSurface(id, SurfaceContent.GhostOverlay(content, _), _, _) =>
        state.runtime.surfaceAnimations.get(id).exists(_.phase == SurfacePhase.Exiting) &&
        ((surface.content, content) match
          case (SurfaceContent.CommandPalette(_), SurfaceContent.CommandPalette(_))                           => true
          case (SurfaceContent.CommandPaletteSubmenu(_, _, _), SurfaceContent.CommandPaletteSubmenu(_, _, _)) => true
          case _                                                                                              => false)
      case _ => false
    }

  private def animatedPanelSurfaces(state: AppState): List[UiSurface] =
    state.runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _)   => true
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }

  private def applyPinnedPanelOpenAnimation(surface: UiSurface): IO[Unit] =
    stateRef.update(state => PinnedPanelAnimations.open(surface, state))

  private def applyPinnedPanelCloseAnimation(closedSurface: UiSurface, prevState: AppState): IO[Unit] =
    stateRef.update(state => PinnedPanelAnimations.close(closedSurface, prevState, state))

  private def transparent(color: Color): Color =
    new Color(color.getRed, color.getGreen, color.getBlue, 0)

  private def completedFadeSteps(totalFadeFrames: Int, remainingFrames: Int): Int =
    (totalFadeFrames - remainingFrames + 1).max(1)

  def advanceSurfaceAnimations(state: AppState): AppState =
    state.runtime.surfaceAnimations.foldLeft(state) {
      case (s, (surfaceId, surfAnim)) =>
        surfAnim.phase match
          case SurfacePhase.BufferFadingOut =>
            val newTick = surfAnim.phaseTick + 1
            if newTick >= surfAnim.bufferFadeLength then
              val overlayFadeIn =
                s.persisted.config.scaledUiAnimation.fold(Map.empty[CharacterKey, AnimatedCell]) { config =>
                  (0 until surfAnim.overlayHeight).map { rowOffset =>
                    val delay    = rowOffset
                    val panelBg  = s.persisted.theme.panel.background
                    val panelFg  = s.persisted.theme.panel.foreground
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
              s.copy(runtime =
                s.runtime.copy(surfaceAnimations = s.runtime.surfaceAnimations + (surfaceId -> newSurfAnim))
              )
            else
              s.copy(runtime =
                s.runtime.copy(surfaceAnimations =
                  s.runtime.surfaceAnimations + (surfaceId -> surfAnim.copy(phaseTick = newTick))
                )
              )

          case SurfacePhase.Visible =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then
              s.copy(runtime = s.runtime.copy(surfaceAnimations = s.runtime.surfaceAnimations - surfaceId))
            else
              s.copy(runtime =
                s.runtime.copy(surfaceAnimations =
                  s.runtime.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState))
                )
              )

          case SurfacePhase.Exiting =>
            val newAnimState = surfAnim.animationState.advanceAllAnimations()
            if !newAnimState.hasActiveAnimations then
              s.copy(runtime =
                s.runtime.copy(
                  uiSurfaces = s.runtime.uiSurfaces.filterNot(_.id == surfaceId),
                  surfaceAnimations = s.runtime.surfaceAnimations - surfaceId
                )
              )
            else
              s.copy(runtime =
                s.runtime.copy(surfaceAnimations =
                  s.runtime.surfaceAnimations + (surfaceId -> surfAnim.copy(animationState = newAnimState))
                )
              )
    }
