package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

/** Named-session modal input (issue #1390): the single-field save-as/rename name prompt (`SessionNamePrompt`, styled
  * and driven exactly like `ModalGotoLineReducer`'s `GotoLine`), and the session-list picker (`SessionList`, navigated
  * and submitted like `ModalFindReducer`'s results list minus the query field). Split out of `ModalEventReducer`'s
  * per-modal-type dispatch, the same way every other modal type is.
  */
private[reducers] object ModalSessionReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduceNamePrompt(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        withNamePromptInput(currentState)(_ + char)
      case ModalDeleteBackward =>
        withNamePromptInput(currentState)(_.dropRight(1))
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        withNamePromptInput(currentState)(TextEditing.deleteWordBackward)
      case ModalDeleteWordForward =>
        withNamePromptInput(currentState)(TextEditing.deleteWordForward)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.SessionNamePrompt(_, input))) if input.trim.nonEmpty =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitSessionNamePrompt(id)))
          case Some((_, Modal.SessionNamePrompt(_, _))) =>
            ReducerResult.noEffects(dismissToPane(currentState))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def withNamePromptInput(state: AppState)(f: String => String): ReducerResult =
    currentModal(state) match
      case Some((id, Modal.SessionNamePrompt(mode, input))) =>
        ReducerResult.noEffects(updateModal(state, id, Modal.SessionNamePrompt(mode, f(input))))
      case _ =>
        ReducerResult.noEffects(state)

  def reduceList(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right) =>
        moveSelection(currentState, 1)
      case ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) =>
        moveSelection(currentState, -1)
      case ModalSubmit =>
        submitList(currentState)
      case ModalClick(_, Some(actionId)) if actionId.startsWith("session-list-") =>
        selectListIndex(currentState, actionId.stripPrefix("session-list-"))
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def submitList(state: AppState): ReducerResult =
    currentModal(state) match
      case Some((id, Modal.SessionList(sessions, index, SessionListPurpose.Open))) if sessions.lift(index).isDefined =>
        ReducerResult.withEffect(state, AppEffect.Workflow(WorkflowEffect.SubmitSessionList(id)))
      case Some((id, Modal.SessionList(sessions, index, SessionListPurpose.Rename))) =>
        sessions.lift(index) match
          case Some(session) =>
            val prompt = Modal.SessionNamePrompt(SessionNamePromptMode.Rename(session.id), session.displayName)
            ReducerResult.noEffects(updateModal(state, id, prompt))
          case None =>
            ReducerResult.noEffects(dismissToPane(state))
      case _ =>
        ReducerResult.noEffects(dismissToPane(state))

  private def selectListIndex(state: AppState, rawIndex: String): ReducerResult =
    currentModal(state) match
      case Some((id, Modal.SessionList(sessions, _, purpose))) =>
        rawIndex.toIntOption match
          case Some(index) if index >= 0 && index < sessions.length =>
            ReducerResult.noEffects(updateModal(state, id, Modal.SessionList(sessions, index, purpose)))
          case _ =>
            ReducerResult.noEffects(state)
      case _ =>
        ReducerResult.noEffects(state)

  private def moveSelection(state: AppState, delta: Int): ReducerResult =
    currentModal(state) match
      case Some((id, Modal.SessionList(sessions, index, purpose))) if sessions.nonEmpty =>
        val rawIndex     = (index + delta) % sessions.length
        val wrappedIndex = if rawIndex < 0 then sessions.length + rawIndex else rawIndex
        ReducerResult.noEffects(updateModal(state, id, Modal.SessionList(sessions, wrappedIndex, purpose)))
      case _ =>
        ReducerResult.noEffects(state)
