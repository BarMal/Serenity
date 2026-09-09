package com.serenity.state.reducers

import com.serenity.state.models.*

object ModalStateReducer:

  def show(modal: Modal, state: AppState): ReducerResult =
    if state.hasBlockingModal && !isBlocking(modal) then ReducerResult.noEffects(state)
    else if isBlocking(modal) then
      val (stateWithId, dialogId) = state.allocateSurfaceId
      val dialog                  = ModalDialog(dialogId, modal, ModalPlacement.Centered)
      ReducerResult.noEffects(
        stateWithId
          .copy(runtime = stateWithId.runtime.copy(modalStack = stateWithId.runtime.modalStack :+ dialog))
          .pushFocus(Focus.Modal)
      )
    else
      val (stateWithId, surfaceId) = state.allocateSurfaceId
      val surface = UiSurface(
        id = surfaceId,
        content = SurfaceContent.ModalWorkflow(modal),
        presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
      )
      ReducerResult.noEffects(
        stateWithId
          .copy(runtime =
            stateWithId.runtime
              .copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot(isModelessModalSurface) :+ surface)
          )
          .pushFocus(Focus.Surface(surfaceId))
      )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(state.dismissTopModal)

  private def isModelessModalSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

  private def isBlocking(modal: Modal): Boolean =
    modal match
      case _: Modal.CloseWorkflow => true
      // A blocking modal is centred (`ModalPlacement.Centered`) and painted by the modal layer even from the startup
      // page, where a floating overlay anchored to a cursor that does not exist would be invisible (#1289).
      case _: Modal.FileWorkflow => true
      case _                     => false
