package com.serenity.state.manager

import com.serenity.command.SessionCommands
import com.serenity.rope.Balance
import com.serenity.session.SessionMetadata
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.ui.layout.{LayoutEngine, SplitAxis}
import com.serenity.ui.widget.Loadable

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

  /** The session picker for `purpose`, opened still loading, and its surface id -- none while a blocking modal holds
    * the screen.
    */
  def withSessionPickerOpened(state: AppState, purpose: SessionListPurpose): Option[(AppState, SurfaceId)] =
    val opened = ModalStateReducer.show(Modal.ListPicker(ListPicker.loading(pickerTitle(purpose))), state).state
    opened.runtime.uiSurfaces.map(_.id).find(id => !state.runtime.uiSurfaces.exists(_.id == id)).map(opened -> _)

  /** Fills the picker `pickerId` with one choice per listed session, while it is still open. */
  def withSessionsListed(
    state: AppState,
    pickerId: SurfaceId,
    purpose: SessionListPurpose,
    listing: Either[String, List[SessionMetadata]]
  ): AppState =
    sessionPicker(state, pickerId).fold(state) { picker =>
      val filled = listing.fold(
        reason => picker.copy(items = Loadable.Failed(reason)),
        sessions => picker.withChoices(sessions.map(sessionChoice(purpose, _)), "No saved sessions")
      )
      WorkflowSurfaces.withModal(state, pickerId, Modal.ListPicker(filled))
    }

  /** Applies a loaded session -- if it was picked from the picker `pickerId`, only while that picker still waits on it,
    * so dismissing the picker abandons the load. A session that could not be read just closes the picker.
    */
  def withNamedSessionLoaded(state: AppState, pickerId: Option[SurfaceId], restored: Option[AppState]): AppState =
    pickerId match
      case Some(id) if !sessionPicker(state, id).exists(_.pending.isDefined) => state
      case _ =>
        restored.fold(pickerId.fold(state)(WorkflowSurfaces.dismissedToEditor(state, _)))(
          restoredIntoViewport(_, state)
        )

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

  private def sessionPicker(state: AppState, surfaceId: SurfaceId): Option[ListPicker] =
    state.runtime.uiSurfaces.find(_.id == surfaceId).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) => picker
    }

  private def pickerTitle(purpose: SessionListPurpose): String =
    purpose match
      case SessionListPurpose.Open   => "Open session"
      case SessionListPurpose.Rename => "Rename session"

  private def sessionChoice(purpose: SessionListPurpose, session: SessionMetadata): ListChoice =
    purpose match
      case SessionListPurpose.Open =>
        ListChoice(
          session.displayName,
          None,
          SessionCommands.openNamedSession(session.id),
          waitingLabel = Some(s"Opening ${session.displayName}…")
        )
      case SessionListPurpose.Rename =>
        ListChoice(session.displayName, None, SessionCommands.renameNamedSession(session.id, session.displayName))
