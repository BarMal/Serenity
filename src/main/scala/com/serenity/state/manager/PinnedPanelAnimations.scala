package com.serenity.state.manager

import java.awt.Color

import com.serenity.animation.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Pure computation of the open/close transition for a pinned or expanded panel surface: lays out transition cells
  * against the panel's on-screen rect and lowers them via the existing `ElementTransitionPlanner`/
  * `ElementTransitionLowerer` motion model (`com.serenity.animation`, #846/#874). Called by [[AnimationChoreography]],
  * which owns the `stateRef` plumbing around these pure `AppState => AppState` transforms.
  */
private[manager] object PinnedPanelAnimations:

  def open(surface: UiSurface, state: AppState): AppState =
    val viewportSize = state.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
    val layout       = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val contract     = EditorLayoutContract.from(state, viewportSize, layout)
    val maybeAnimation =
      for
        position  <- panelPosition(surface)
        rect      <- contract.panelRect(surface.id)
        animation <- openAnimation(position, rect, state)
      yield animation

    maybeAnimation
      .map(animation =>
        state.copy(runtime =
          state.runtime.copy(surfaceAnimations = state.runtime.surfaceAnimations + (surface.id -> animation))
        )
      )
      .getOrElse(state)

  def close(closedSurface: UiSurface, prevState: AppState, state: AppState): AppState =
    val tSize = prevState.runtime.viewportSize.orElse(state.runtime.viewportSize).getOrElse(ViewportSize(80, 24))
    val previousLayout = LayoutEngine.calculateLayoutWithUI(prevState, tSize)
    val contract       = EditorLayoutContract.from(prevState, tSize, previousLayout)
    val maybeGhost =
      for
        position  <- panelPosition(closedSurface)
        rect      <- contract.panelRect(closedSurface.id)
        animation <- closeAnimation(position, rect, state)
      yield
        val (stateWithId, ghostId) = state.allocateSurfaceId
        val ghostSurface = UiSurface(
          id = ghostId,
          content = SurfaceContent.GhostOverlay(closedSurface.content, rect),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
        stateWithId.copy(runtime =
          stateWithId.runtime.copy(
            uiSurfaces = stateWithId.runtime.uiSurfaces :+ ghostSurface,
            surfaceAnimations = stateWithId.runtime.surfaceAnimations
              - closedSurface.id
              + (ghostId -> animation)
          )
        )

    maybeGhost.getOrElse(state)

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

  private def panelPosition(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case _                                         => None

  private def transparent(color: Color): Color =
    new Color(color.getRed, color.getGreen, color.getBlue, 0)
