package com.serenity.state.manager

import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** A close step's next state, and the close it completed if it resolved the last buffer: a quit or a return to the
  * start page follows once that state is committed.
  */
final private[manager] case class CloseTransition(state: AppState, completed: Option[CloseScope])

/** The close workflow's decisions -- which buffers close, which prompt comes next, what a cancel or a failed save
  * leaves -- as pure functions of the state. `ensureCommandRunnerSurface` is what closing the last editor falls back
  * to.
  */
final private[manager] class CloseWorkflowTransitions(ensureCommandRunnerSurface: AppState => AppState):

  /** Closes the clean targets and prompts for the first unsaved one, or completes the close if none is unsaved. */
  def begun(scope: CloseScope, state: AppState): CloseTransition =
    val targetBufferIds = closeTargets(scope, state)
    val dirtyBufferIds =
      targetBufferIds.filter(bufferId => state.persisted.buffers.get(bufferId).exists(_.hasUnsavedChanges))
    val cleanBufferIds =
      if preservesBuffers(scope) then Nil
      else targetBufferIds.filterNot(dirtyBufferIds.contains)
    val stateAfterClean = cleanBufferIds.foldLeft(state)(closeForScope(scope, _, _))
    dirtyBufferIds match
      case Nil => CloseTransition(clearCloseActions(stateAfterClean), Some(scope))
      case currentBufferId :: remaining =>
        CloseTransition(prompted(stateAfterClean, scope, currentBufferId, remaining), None)

  /** The prompt's buffer was saved or discarded: close it as the scope asks, then move on to the next one. */
  def resolved(workflow: CloseWorkflowState, state: AppState): CloseTransition =
    val dismissed = clearCloseActions(dismissModalSurface(state))
    val next =
      if workflow.scope == CloseScope.ReturnToStartPage then dismissed
      else closeForScope(workflow.scope, dismissed, workflow.currentBufferId)
    continued(workflow, refocused(next, dismissed.persisted.focus))

  /** The prompt's buffer was saved under a new path. Unlike [[resolved]], Quit keeps it open too. */
  def resolvedBySaveAs(workflow: CloseWorkflowState, state: AppState): CloseTransition =
    val dismissed = dismissModalSurface(state)
    val next =
      if preservesBuffers(workflow.scope) then dismissed
      else closeForScope(workflow.scope, dismissed, workflow.currentBufferId)
    continued(workflow, refocused(next, dismissed.persisted.focus))

  /** What cancelling the close leaves: no close pending, and a tab close's original tab active. The prompt has already
    * closed by the time its answer runs.
    */
  def abandoned(workflow: CloseWorkflowState, state: AppState): AppState =
    restoreActiveTab(workflow.scope, clearCloseActions(state))

  /** The save found the file changed on disk: abandon the close and ask how to resolve the conflict. */
  def conflicted(workflow: CloseWorkflowState, state: AppState): AppState =
    val bufferId = workflow.currentBufferId
    val conflict = ConfirmPrompt.reloadConflict(bufferId, closeBufferLabel(state, bufferId))
    ModalStateReducer.show(Modal.Confirm(conflict), abandoned(workflow, state)).state

  /** The prompt again for the close still waiting, after the Save As its Save opened was cancelled. */
  def reprompted(workflow: CloseWorkflowState, state: AppState): AppState =
    ModalStateReducer.show(Modal.Confirm(workflow.prompt), state).state

  /** The close waiting on an answer, if any. */
  def pending(state: AppState): Option[CloseWorkflowState] =
    state.runtime.actionStack.collectFirst { case AppAction.CloseWorkflow(closeWorkflow) => closeWorkflow }

  /** The close workflow waiting on a save of `bufferId`, if any. */
  def pendingOn(state: AppState, bufferId: BufferId): Option[CloseWorkflowState] =
    state.runtime.actionStack.collectFirst {
      case AppAction.CloseWorkflow(closeWorkflow) if closeWorkflow.currentBufferId == bufferId => closeWorkflow
    }

  def closeTargets(scope: CloseScope, state: AppState): List[BufferId] =
    scope match
      case CloseScope.Current => WorkflowSurfaces.activeEditorBufferId(state).toList
      case CloseScope.All     => state.persisted.bufferOrder
      case CloseScope.Others =>
        WorkflowSurfaces.activeEditorBufferId(state) match
          case Some(focused) => state.persisted.bufferOrder.filterNot(_ == focused)
          case None          => state.persisted.bufferOrder
      case CloseScope.Quit              => state.persisted.bufferOrder
      case CloseScope.ReturnToStartPage => state.persisted.bufferOrder
      case CloseScope.RestartInSafeMode => state.persisted.bufferOrder
      case CloseScope.Tab(bufferId, _)  => List(bufferId).filter(state.persisted.buffers.contains)

  def clearCloseActions(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(actionStack = Nil))

  /** Clears every modal: each caller is finishing a close/save chain, possibly a Save-As form over a close prompt. */
  def dismissModalSurface(state: AppState): AppState =
    WorkflowSurfaces.withFocusRepaired(
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces.filterNot {
            case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
            case _                                                   => false
          },
          modalStack = Nil
        )
      )
    )

  private def continued(workflow: CloseWorkflowState, state: AppState): CloseTransition =
    workflow.remainingBufferIds match
      case nextBufferId :: remaining =>
        CloseTransition(prompted(state, workflow.scope, nextBufferId, remaining), None)
      case Nil =>
        CloseTransition(clearCloseActions(state), Some(workflow.scope))

  private def prompted(state: AppState, scope: CloseScope, bufferId: BufferId, remaining: List[BufferId]): AppState =
    val workflow = CloseWorkflowState(scope, bufferId, closeBufferLabel(state, bufferId), remaining)
    val focused  = focusBufferForWorkflow(state, bufferId)
    // The prompt's own push records the focus it returns to, so it must see the focus the close began from.
    val recorded = refocused(focused, state.persisted.focus)
    val withAction =
      recorded.copy(runtime = recorded.runtime.copy(actionStack = List(AppAction.CloseWorkflow(workflow))))
    ModalStateReducer.show(Modal.Confirm(workflow.prompt), withAction).state

  /** `focus` back, after a close step moved focus to the editor to work on a buffer, if it outlived the step and an
    * editor is still open; otherwise the step's own focus stands.
    */
  private def refocused(state: AppState, focus: Focus): AppState =
    if state.persisted.layout.activeEditorPaneId.isDefined && WorkflowSurfaces.focusTargetExists(state, focus) then
      state.copy(persisted = state.persisted.copy(focus = focus))
    else state

  /** Scopes that leave clean buffers open rather than closing them as they go: Quit and RestartInSafeMode (state is
    * discarded on exit anyway) and ReturnToStartPage (the whole session is snapshotted, then replaced by the start
    * page).
    */
  private def preservesBuffers(scope: CloseScope): Boolean =
    scope == CloseScope.Quit || scope == CloseScope.ReturnToStartPage || scope == CloseScope.RestartInSafeMode

  private def closeForScope(scope: CloseScope, state: AppState, bufferId: BufferId): AppState =
    restoreActiveTab(scope, closeBuffer(state, bufferId))

  /** A `CloseScope.Tab` close hands the active tab back to the one active when the close began -- see its doc. */
  private def restoreActiveTab(scope: CloseScope, state: AppState): AppState =
    scope match
      case CloseScope.Tab(_, Some(returnTo)) if state.persisted.buffers.contains(returnTo) =>
        focusBufferForWorkflow(state, returnTo)
      case _ =>
        state

  private def focusBufferForWorkflow(state: AppState, bufferId: BufferId): AppState =
    EditorState.focusBuffer(EditorState.rebalancePanes(state, Some(bufferId)), bufferId)

  private def closeBuffer(state: AppState, bufferId: BufferId): AppState =
    val closedState = EditorState.closeFocusedTab(focusBufferForWorkflow(state, bufferId))
    if closedState.persisted.layout.activeEditorPaneId.isDefined then closedState
    else ensureCommandRunnerSurface(closedState)

  def closeBufferLabel(state: AppState, bufferId: BufferId): String =
    state.persisted.buffers
      .get(bufferId)
      .flatMap(_.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)))
      .getOrElse(s"Buffer ${bufferId.value} - unsaved")

/** Surface lookups and dismissals the file, close and session workflows share. */
private[manager] object WorkflowSurfaces:

  /** `modal` in place of the floating modal surface `surfaceId` shows; unchanged if that surface has gone. */
  def withModal(state: AppState, surfaceId: SurfaceId, modal: Modal): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(id, SurfaceContent.ModalWorkflow(_), _, _) if id == surfaceId =>
        surface.copy(content = SurfaceContent.ModalWorkflow(modal))
      case surface => surface
    }))

  def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  /** Removes `surfaceId`, modal or not, for a workflow whose result is a buffer now on show: focus goes to the active
    * editor pane, and the focus the surface would have handed back is dropped from history rather than left stale.
    */
  def dismissedToShownBuffer(state: AppState, surfaceId: SurfaceId): AppState =
    val popped = withoutSurface(state, surfaceId).popFocus
    state.persisted.layout.activeEditorPaneId.fold(popped)(paneId =>
      popped.copy(persisted = popped.persisted.copy(focus = Focus.EditorPane(paneId)))
    )

  /** Removes `surfaceId`, modal or not, and, if it held focus, hands focus back to whatever held it before. */
  def dismissedToPriorFocus(state: AppState, surfaceId: SurfaceId): AppState =
    withFocusRepaired(withoutSurface(state, surfaceId))

  /** `state` with focus handed back through its history if what it points at has gone. */
  def withFocusRepaired(state: AppState): AppState =
    if focusTargetExists(state, state.persisted.focus) then state else state.popFocus

  def focusTargetExists(state: AppState, focus: Focus): Boolean =
    focus match
      case Focus.EditorPane(paneId) => state.persisted.layout.editorPanes.contains(paneId)
      case Focus.Surface(surfaceId) => state.surfaceById(surfaceId).isDefined
      case Focus.Modal              => state.runtime.modalStack.nonEmpty

  private def withoutSurface(state: AppState, surfaceId: SurfaceId): AppState =
    state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId),
        modalStack = state.runtime.modalStack.filterNot(_.id == surfaceId)
      )
    )
