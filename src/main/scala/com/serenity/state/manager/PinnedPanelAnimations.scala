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
        val withFade = openAnimation(position, rect, state).fold(state)(animation =>
          state.copy(runtime =
            state.runtime.copy(surfaceAnimations = state.runtime.surfaceAnimations + (surface.id -> animation))
          )
        )
        openGeometry(position, rect, withFade).fold(withFade)(geometry =>
          withFade.copy(runtime =
            withFade.runtime.copy(panelGeometry = withFade.runtime.panelGeometry + (surface.id -> geometry))
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
        val fade     = closeAnimation(position, rect, state)
        val geometry = closeGeometry(position, rect, state)
        if fade.isEmpty && geometry.isEmpty then state
        else
          val (stateWithId, ghostId) = state.allocateSurfaceId
          val ghostSurface = UiSurface(
            id = ghostId,
            content = SurfaceContent.GhostOverlay(closedSurface.content, rect),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
          stateWithId.copy(runtime =
            stateWithId.runtime.copy(
              uiSurfaces = stateWithId.runtime.uiSurfaces :+ ghostSurface,
              surfaceAnimations = (stateWithId.runtime.surfaceAnimations - closedSurface.id) ++ fade.map(ghostId -> _),
              panelGeometry = (stateWithId.runtime.panelGeometry - closedSurface.id) ++ geometry.map(ghostId -> _)
            )
          )
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

  /** Panel scale-in (issue #1085 phase 1): grows from a collapsed rect at `position`'s anchor edge to the panel's full
    * `rect`, gated by the independent `MotionFamily.PanelGeometry` family -- `None` when it is off (including by
    * accessibility, or `MotionPreset.Reduced`), which callers read as "open at the full rect immediately."
    */
  private def openGeometry(position: PanelPosition, rect: LayoutRect, state: AppState): Option[PanelGeometryState] =
    state.persisted.config.scaledPanelGeometryAnimation.map(animation =>
      PanelGeometryState.seeded(
        start = collapsedRect(position, rect),
        end = rect,
        curve = animation.curve,
        steps = animation.steps
      )
    )

  /** Panel scale-out (issue #1085 phase 1): mirrors [[openGeometry]], shrinking from the panel's full `rect` back to
    * the collapsed rect at `position`'s anchor edge.
    */
  private def closeGeometry(position: PanelPosition, rect: LayoutRect, state: AppState): Option[PanelGeometryState] =
    state.persisted.config.scaledPanelGeometryAnimation.map(animation =>
      PanelGeometryState.seeded(
        start = rect,
        end = collapsedRect(position, rect),
        curve = animation.curve,
        steps = animation.steps
      )
    )

  /** `rect` collapsed to zero width or height at `position`'s docked edge -- the anchor a panel scales in from or out
    * to. Lerping `LayoutRect`'s independent `x`/`width` (or `y`/`height`) fields between this and the full `rect` keeps
    * the opposite, non-anchor edge fixed for every frame in between (issue #1085 phase 1's chosen "grow/shrink from the
    * dock edge" shape): e.g. for `Right`, `x` moves from `rect.right` down to `rect.x` exactly as fast as `width` grows
    * from `0` to `rect.width`, so `x + width == rect.right` throughout.
    */
  private def collapsedRect(position: PanelPosition, rect: LayoutRect): LayoutRect =
    position match
      case PanelPosition.Left   => rect.copy(width = 0)
      case PanelPosition.Right  => rect.copy(x = rect.right, width = 0)
      case PanelPosition.Top    => rect.copy(height = 0)
      case PanelPosition.Bottom => rect.copy(y = rect.bottom, height = 0)

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
