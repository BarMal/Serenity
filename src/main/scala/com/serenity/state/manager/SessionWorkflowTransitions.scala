package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.session.SessionMetadata
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.ui.layout.{LayoutEngine, SplitAxis}

/** Session restore and the named-session prompts (#1390) as pure functions of the state; the session reads and writes
  * themselves run on the Session lane.
  */
private[manager] object SessionWorkflowTransitions:

  /** `restoredState` with this run's runtime chrome: viewport, terminal mode and keyboard tier. Every floating surface
    * is dropped, the one the restore was chosen from included; docked panels stay, since the restored workspace tree
    * names them and would fail validation without them.
    */
  def restoredIntoViewport(restoredState: AppState, currentState: AppState): AppState =
    val restored = restoredState.copy(
      runtime = restoredState.runtime.copy(
        uiSurfaces = restoredState.runtime.uiSurfaces.filter(_.presentation == SurfacePresentation.Docked),
        viewportSize = currentState.runtime.viewportSize,
        capabilities = currentState.runtime.capabilities,
        projectTasks = ProjectTaskTransitions.acrossRestore(currentState)
      )
    )
    currentState.runtime.viewportSize
      .map(viewportSize => LayoutEngine.syncViewportDimensions(restored, viewportSize))
      .getOrElse(restored)

  /** A fresh empty buffer in a new focused pane, replacing whatever surfaces were up (the start page). */
  def withDefaultStartupBuffer(state: AppState)(using Balance): AppState =
    val (withBuffer, bufferId) = EditorState.createNewEmptyBuffer(state)
    val ordered =
      withBuffer.copy(persisted = withBuffer.persisted.copy(bufferOrder = withBuffer.persisted.bufferOrder :+ bufferId))
    val (withPane, paneId) = EditorTransitions.paneInserted(
      ordered,
      ordered.persisted.layout.orderedPaneIds.lastOption,
      Some(bufferId),
      SplitAxis.Horizontal
    )
    val switched = EditorTransitions.paneSwitched(withPane, paneId).getOrElse(withPane)
    switched.copy(runtime = switched.runtime.copy(uiSurfaces = List.empty))

  def withSessionPicker(state: AppState, purpose: SessionListPurpose, sessions: List[SessionMetadata]): AppState =
    ModalStateReducer.show(Modal.SessionList(sessions, 0, purpose), state).state

  /** Applies a session loaded from the picker `pickerId` only while that picker is still open: dismissing it first
    * abandons the load. A session that could not be read just closes the picker.
    */
  def withNamedSessionLoaded(state: AppState, pickerId: SurfaceId, restored: Option[AppState]): AppState =
    if sessionPicker(state, pickerId).isEmpty then state
    else restored.fold(WorkflowSurfaces.dismissedToEditor(state, pickerId))(restoredIntoViewport(_, state))

  def sessionNamePrompt(state: AppState, surfaceId: SurfaceId): Option[(SessionNamePromptMode, String)] =
    state.runtime.uiSurfaces
      .find(_.id == surfaceId)
      .collect {
        case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.TextPrompt(prompt)), _, _) =>
          prompt.purpose match
            case TextPromptPurpose.SessionName(mode) => Some((mode, prompt.input))
            case _                                   => None
      }
      .flatten

  def sessionPicker(
    state: AppState,
    surfaceId: SurfaceId
  ): Option[(List[SessionMetadata], Int, SessionListPurpose)] =
    state.runtime.uiSurfaces.find(_.id == surfaceId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.SessionList(sessions, index, purpose)), _, _) =>
        (sessions, index, purpose)
    }
