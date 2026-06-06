package com.serenity.state.reducers

import com.serenity.state.models.*

object ModalStateReducer:

  def show(modal: Modal, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.ModalWorkflow(modal),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    ReducerResult.noEffects(
      stateWithId
        .copy(
          uiSurfaces = stateWithId.uiSurfaces.filterNot(isModalSurface) :+ surface
        )
        .pushFocus(Focus.Surface(surfaceId))
    )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state
        .copy(
          uiSurfaces = state.uiSurfaces.filterNot(isModalSurface)
        )
        .popFocus
    )

  private def isModalSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false
