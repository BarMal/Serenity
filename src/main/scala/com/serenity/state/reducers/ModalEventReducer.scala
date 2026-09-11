package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** Thin dispatcher over the per-modal-type reducers -- [[ModalGotoLineReducer]], [[ModalFindReducer]],
  * [[ModalFileWorkflowReducer]], [[ModalCloseWorkflowReducer]], [[ModalReplaceWorkflowReducer]] -- plus the helpers
  * they all share for locating and updating the currently active modal. Each modal type used to have its reducer
  * inlined here; they were split into their own files when this file grew past its 600-line target.
  */
object ModalEventReducer:

  def selectCloseWorkflowChoice(choice: CloseWorkflowChoice, currentState: AppState): ReducerResult =
    ModalCloseWorkflowReducer.selectCloseWorkflowChoice(choice, currentState)

  def reducer(modalType: ModalType): Reducer[ModalInputEvent] =
    Reducer.instance((event, state) => reduce(modalType, event, state))

  def reduce(modalType: ModalType, event: Event, currentState: AppState): ReducerResult =
    ModalInputEvent
      .fromEvent(event)
      .map(reduce(modalType, _, currentState))
      .getOrElse(ReducerResult.noEffects(currentState))

  def reduce(modalType: ModalType, event: ModalInputEvent, currentState: AppState): ReducerResult =
    modalType match
      case ModalType.GotoLine        => ModalGotoLineReducer.reduce(event, currentState)
      case ModalType.Find            => ModalFindReducer.reduce(event, currentState)
      case ModalType.FileWorkflow    => ModalFileWorkflowReducer.reduce(event, currentState)
      case ModalType.ReplaceWorkflow => ModalReplaceWorkflowReducer.reduce(event, currentState)
      case ModalType.CloseWorkflow   => ModalCloseWorkflowReducer.reduce(event, currentState)
      case ModalType.Custom(_)       => ReducerResult.noEffects(currentState)

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: List[FindResult]
  ): AppState =
    ModalFindReducer.applyFindSearchResults(state, request, results)

  /** The id/payload owning input: a blocking `ModalDialog` (#814) if open, else the focused modeless workflow. */
  private[reducers] def currentModal(state: AppState): Option[(SurfaceId, Modal)] =
    state.topModal
      .map(dialog => (dialog.id, dialog.modal))
      .orElse(state.activeSurface.flatMap {
        case UiSurface(id, SurfaceContent.ModalWorkflow(modal), _, _) => Some((id, modal))
        case _                                                        => None
      })

  private[reducers] def updateModal(state: AppState, id: SurfaceId, modal: Modal): AppState =
    state.topModal.filter(_.id == id) match
      case Some(dialog) =>
        state.copy(runtime =
          state.runtime.copy(modalStack = state.runtime.modalStack.dropRight(1) :+ dialog.copy(modal = modal))
        )
      case None =>
        state.runtime.uiSurfaces.find(_.id == id) match
          case Some(surface) =>
            val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(modal))
            state.copy(runtime =
              state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == id)(updatedSurface))
            )
          case None => state

  private[reducers] def dismissToPane(state: AppState): AppState =
    state.dismissTopModal
