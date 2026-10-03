package com.serenity.state.manager

import com.serenity.animation.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.config.ColorParser.transparent

/** Pure computation of the open/close transition for a pinned or expanded panel surface: lays out transition cells
  * against the panel's on-screen rect and lowers them via the existing `ElementTransitionPlanner`/
  * `ElementTransitionLowerer` motion model (`com.serenity.animation`, #846/#874). Folded into the surface transitions
  * by `AnimationChoreography.animateSurfaceTransitions`, whose shell validates and commits the result.
  */
private[manager] object PinnedPanelAnimations:

  def open(surface: UiSurface, state: AppState): AppState =
    val viewportSize = state.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
    val layout       = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val contract     = EditorLayoutContract.from(state, viewportSize, layout)
    val maybeContext =
      for
        position <- panelPosition(surface, state)
        rect     <- contract.panelRect(surface.id)
      yield (position, rect)

    maybeContext.fold(state) {
      case (position, rect) =>
        openAnimation(position, rect, state).fold(state)(animation =>
          state.copy(runtime =
            state.runtime.copy(motion =
              state.runtime.motion
                .copy(surfaceAnimations = state.runtime.motion.surfaceAnimations + (surface.id -> animation))
            )
          )
        )
    }

  def close(closedSurface: UiSurface, prevState: AppState, state: AppState): AppState =
    val tSize = prevState.runtime.viewportSize.orElse(state.runtime.viewportSize).getOrElse(ViewportSize(80, 24))
    val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
    val contract       = EditorLayoutContract.from(prevState, tSize, previousLayout)
    val maybeContext =
      for
        position <- panelPosition(closedSurface, prevState)
        rect     <- contract.panelRect(closedSurface.id)
      yield (position, rect)

    maybeContext.fold(state) {
      case (position, rect) =>
        closeAnimation(position, rect, state).fold(state) { fade =>
          val (stateWithId, ghostId) = state.allocateSurfaceId
          val ghostSurface = UiSurface(
            id = ghostId,
            content = SurfaceContent.GhostOverlay(closedSurface.content, rect),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
          stateWithId.copy(runtime =
            stateWithId.runtime.copy(
              uiSurfaces = stateWithId.runtime.uiSurfaces :+ ghostSurface,
              motion = stateWithId.runtime.motion.copy(
                surfaceAnimations =
                  (stateWithId.runtime.motion.surfaceAnimations - closedSurface.id) + (ghostId -> fade)
              )
            )
          )
        }
    }

  private def openAnimation(
    position: PanelPosition,
    rect: LayoutRect,
    state: AppState
  ): Option[SurfaceAnimationState] =
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, Some(position)),
      state.persisted.config.pinnedPanelTransitionSettings
    )
    val animationState = ElementTransitionLowerer.lower(plan, openCells(rect, state), tickRateMs = 16)
    Option.when(animationState.hasActiveAnimations)(
      SurfaceAnimationState(
        phase = SurfacePhase.Visible,
        animationState = animationState,
        overlayHeight = rect.height,
        bufferFadeLength = 0,
        phaseTick = 0
      )
    )

  private def closeAnimation(
    position: PanelPosition,
    rect: LayoutRect,
    state: AppState
  ): Option[SurfaceAnimationState] =
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelClose, Some(position)),
      state.persisted.config.pinnedPanelTransitionSettings
    )
    val animationState = ElementTransitionLowerer.lower(plan, closeCells(rect, state), tickRateMs = 16)
    Option.when(animationState.hasActiveAnimations)(
      SurfaceAnimationState(
        phase = SurfacePhase.Exiting,
        animationState = animationState,
        overlayHeight = rect.height,
        bufferFadeLength = 0,
        phaseTick = 0
      )
    )

  private def openCells(rect: LayoutRect, state: AppState): ElementTransitionCells =
    val transparentPanelForeground = transparent(state.persisted.theme.panel.foreground)
    val transparentBorder          = transparent(state.persisted.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', transparentBorder, state.persisted.theme.border)
    val contentCells =
      (0 until math.max(0, rect.height - 1))
        .flatMap { row =>
          (0 until math.max(0, rect.width - 2)).map { column =>
            CharacterKey(column, row) ->
              CellAnimation(' ', transparentPanelForeground, state.persisted.theme.panel.foreground)
          }
        }
        .take(VisibleBufferAnimationCells.DefaultMaxAnimatedCells)
        .toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def closeCells(rect: LayoutRect, state: AppState): ElementTransitionCells =
    val transparentPanelForeground = transparent(state.persisted.theme.panel.foreground)
    val transparentBorder          = transparent(state.persisted.theme.border)
    val borderCell =
      CharacterKey(-1, -1) -> CellAnimation(' ', state.persisted.theme.border, transparentBorder)
    val contentCells =
      (0 until math.max(0, rect.height - 1))
        .flatMap { row =>
          (0 until math.max(0, rect.width - 2)).map { column =>
            CharacterKey(column, row) ->
              CellAnimation(' ', state.persisted.theme.panel.foreground, transparentPanelForeground)
          }
        }
        .take(VisibleBufferAnimationCells.DefaultMaxAnimatedCells)
        .toMap
    ElementTransitionCells(frame = Map(borderCell), content = contentCells)

  private def panelPosition(surface: UiSurface, state: AppState): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Docked => state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
      case _                          => None
