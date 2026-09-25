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
    continued(workflow, next)

  /** The prompt's buffer was saved under a new path. Unlike [[resolved]], Quit keeps it open too. */
  def resolvedBySaveAs(workflow: CloseWorkflowState, state: AppState): CloseTransition =
    val dismissed = dismissModalSurface(state)
    val next =
      if preservesBuffers(workflow.scope) then dismissed
      else closeForScope(workflow.scope, dismissed, workflow.currentBufferId)
    continued(workflow, next)

  /** What cancelling the prompt leaves: the prompt gone, no close pending, and a tab close's original tab active. */
  def abandoned(surfaceId: SurfaceId, workflow: CloseWorkflowState, state: AppState): AppState =
    restoreActiveTab(workflow.scope, clearCloseActions(WorkflowSurfaces.dismissedToEditor(state, surfaceId)))

  /** The save found the file changed on disk: abandon the close and ask how to resolve the conflict. */
  def conflicted(surfaceId: SurfaceId, workflow: CloseWorkflowState, state: AppState): AppState =
    val bufferId = workflow.currentBufferId
    val conflict = ReloadConflictState(bufferId, closeBufferLabel(state, bufferId))
    ModalStateReducer.show(Modal.ReloadConflict(conflict), abandoned(surfaceId, workflow, state)).state

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
      case CloseScope.Tab(bufferId, _)  => List(bufferId).filter(state.persisted.buffers.contains)

  def closePrompt(state: AppState, surfaceId: SurfaceId): Option[CloseWorkflowState] =
    state.runtime.modalStack.find(_.id == surfaceId).collect {
      case ModalDialog(_, Modal.CloseWorkflow(workflow), _) => workflow
    }

  def clearCloseActions(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(actionStack = Nil))

  /** Clears every modal: each caller is finishing a close/save chain, possibly a Save-As form over a close prompt. */
  def dismissModalSurface(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot {
          case UiSurface(_, SurfaceContent.ModalWorkflow(_), _, _) => true
          case _                                                   => false
        },
        modalStack = Nil
      )
    )

  private def continued(workflow: CloseWorkflowState, state: AppState): CloseTransition =
    workflow.remainingBufferIds match
      case nextBufferId :: remaining =>
        CloseTransition(prompted(state, workflow.scope, nextBufferId, remaining), None)
      case Nil =>
        CloseTransition(clearCloseActions(state), Some(workflow.scope))

  private def prompted(state: AppState, scope: CloseScope, bufferId: BufferId, remaining: List[BufferId]): AppState =
    val workflow   = CloseWorkflowState(scope, bufferId, closeBufferLabel(state, bufferId), remaining)
    val focused    = focusBufferForWorkflow(state, bufferId)
    val withAction = focused.copy(runtime = focused.runtime.copy(actionStack = List(AppAction.CloseWorkflow(workflow))))
    ModalStateReducer.show(Modal.CloseWorkflow(workflow), withAction).state

  /** Scopes that leave clean buffers open rather than closing them as they go: Quit (state is discarded on exit anyway)
    * and ReturnToStartPage (the whole session is snapshotted, then replaced by the start page).
    */
  private def preservesBuffers(scope: CloseScope): Boolean =
    scope == CloseScope.Quit || scope == CloseScope.ReturnToStartPage

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

  def activeEditorBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  /** Removes `surfaceId`, modal or not, and hands focus back to the active editor pane. */
  def dismissedToEditor(state: AppState, surfaceId: SurfaceId): AppState =
    val baseState = state.copy(runtime =
      state.runtime.copy(
        uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surfaceId),
        modalStack = state.runtime.modalStack.filterNot(_.id == surfaceId)
      )
    )
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) => baseState.copy(persisted = baseState.persisted.copy(focus = Focus.EditorPane(paneId)))
      case None         => baseState
