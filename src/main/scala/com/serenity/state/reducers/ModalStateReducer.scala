package com.serenity.state.reducers

import com.serenity.state.models.*

object ModalStateReducer:

  def show(modal: Modal, state: AppState): ReducerResult =
    if state.hasBlockingModal && !isBlocking(modal) then ReducerResult.noEffects(state)
    else
      val (stateWithId, surfaceId) = state.allocateSurfaceId
      val surface = UiSurface(
        id = surfaceId,
        content = SurfaceContent.ModalWorkflow(modal),
        presentation =
          if isBlocking(modal) then SurfacePresentation.Modal
          else SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
      )
      ReducerResult.noEffects(
        stateWithId
          .copy(runtime =
            stateWithId.runtime.copy(uiSurfaces =
              if isBlocking(modal) then stateWithId.runtime.uiSurfaces :+ surface
              else stateWithId.runtime.uiSurfaces.filterNot(isModelessModalSurface) :+ surface
            )
          )
          .pushFocus(Focus.Surface(surfaceId))
      )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(state.dismissTopModal)

  private def isModelessModalSurface(surface: UiSurface): Boolean =
    surface.presentation != SurfacePresentation.Modal && (surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false)

  private def isBlocking(modal: Modal): Boolean =
    modal match
      case _: Modal.CloseWorkflow => true
      case _: Modal.FileWorkflow  => true
      case _                      => false
