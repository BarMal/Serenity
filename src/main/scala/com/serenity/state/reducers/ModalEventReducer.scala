package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** Thin dispatcher over the per-modal-type reducers -- [[ModalTextPromptReducer]], [[ModalFindReducer]],
  * [[ModalFileWorkflowReducer]], [[ModalCloseWorkflowReducer]], [[ModalConfirmReducer]],
  * [[ModalReplaceWorkflowReducer]] -- plus the helpers they all share for locating and updating the currently active
  * modal. Each modal type used to have its reducer inlined here; they were split into their own files when this file
  * grew past its 600-line target.
  */
object ModalEventReducer:

  def reducer(modalType: ModalType): Reducer[ModalInputEvent] =
    Reducer.instance((event, state) => reduce(modalType, event, state))

  def reduce(modalType: ModalType, event: Event, currentState: AppState): ReducerResult =
    ModalInputEvent
      .fromEvent(event)
      .map(reduce(modalType, _, currentState))
      .getOrElse(ReducerResult.noEffects(currentState))

  def reduce(modalType: ModalType, event: ModalInputEvent, currentState: AppState): ReducerResult =
    modalType match
      case ModalType.TextPrompt       => ModalTextPromptReducer.reduce(event, currentState)
      case ModalType.Find             => ModalFindReducer.reduce(event, currentState)
      case ModalType.FileWorkflow     => ModalFileWorkflowReducer.reduce(event, currentState)
      case ModalType.ReplaceWorkflow  => ModalReplaceWorkflowReducer.reduce(event, currentState)
      case ModalType.Confirm          => ModalConfirmReducer.reduce(event, currentState)
      case ModalType.ListPicker       => ModalListPickerReducer.reduce(event, currentState)
      case ModalType.PanelArrangement => ModalPanelArrangementReducer.reduce(event, currentState)

  def modalType(modal: Modal): ModalType =
    modal match
      case Modal.TextPrompt(_)       => ModalType.TextPrompt
      case _: Modal.Find             => ModalType.Find
      case Modal.FileWorkflow(_)     => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_)  => ModalType.ReplaceWorkflow
      case Modal.Confirm(_)          => ModalType.Confirm
      case Modal.ListPicker(_)       => ModalType.ListPicker
      case Modal.PanelArrangement(_) => ModalType.PanelArrangement

  def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    for
      surfaceId <- state.persisted.focus match
        case Focus.Surface(id) => Some(id)
        case _                 => None
      surface <- state.runtime.uiSurfaces.find(_.id == surfaceId)
      _ <- surface.presentation match
        case SurfacePresentation.Floating(_, _) => Some(())
        case _                                  => None
      _ <- surface.content match
        case SurfaceContent.ModalWorkflow(_) => Some(())
        case _                               => None
    yield surface

  /** The focused modeless modal closed exactly as Escape closes it -- its `onDismiss` included -- so input aimed past
    * it (an outside click, a tab switch) does not leave it open but unfocused.
    */
  def dismissFocusedFloatingModalWorkflow(state: AppState): Option[ReducerResult] =
    focusedFloatingModalWorkflow(state).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(modal), _, _) =>
        reduce(modalType(modal), ModalDismiss, state)
    }

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: Vector[FindResult],
    capped: Boolean = false
  ): AppState =
    ModalFindReducer.applyFindSearchResults(state, request, results, capped)

  /** The search an open find needs after `before` -> `after`: it just opened on a stored query, or the document it
    * searches changed underneath it.
    */
  def findRefreshDue(before: AppState, after: AppState): Option[FindSearchRequest] =
    ModalFindReducer.refreshDue(before, after)

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
