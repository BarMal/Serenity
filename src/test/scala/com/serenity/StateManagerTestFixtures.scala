package com.serenity

import cats.effect.IO
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.Direction
import com.serenity.state.core.EditorState
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerPanelSelections
import com.serenity.ui.layout.*

/** Pure `AppState` fixtures for positioning test scenarios -- assigning a buffer to a pane, placing a cursor or
  * viewport, building a pane tree -- that production code has no reason to perform directly. Production always reaches
  * this state through event/effect dispatch (reducers, `StateManager.applyEvent`), never through a direct setter, which
  * is exactly why the `StateManagerEditorCapability` methods these fixtures replace were deleted as dead production
  * code (see #1183 item 2): they had zero callers outside test-scenario setup.
  *
  * These extension methods exist so the ~230 existing test call sites that positioned fixtures via that façade didn't
  * need individual rewriting to route through full event dispatch, which the façade's own investigation found
  * impractical for pure fixture setup. `createBuffer` and `createPane` are NOT reproduced here -- unlike the rest of
  * the façade, both are still genuinely called from production (`StateManagerWorkflowCapability
  * .restoreStartupSession`, `StateManagerEventPipeline`'s empty-pane fallback on `ComponentResult.Dismiss`), so they
  * remain on `StateManager` unchanged.
  */
extension (stateManager: StateManager)

  /** Test-only equivalent of the deleted `StateManagerEditorCapability.closeBuffer` -- unlike the original, this does
    * not enqueue an `LspEffect.FileClosed` notification, since there is no production LSP consumer reachable from test
    * code and no test in the affected 6 call sites asserts on it.
    */
  def closeBuffer(bufferId: BufferId): IO[Unit] =
    stateManager.updateState(state => EditorState.removeBuffer(state, bufferId))

  def closePane(paneId: PaneId): IO[Unit] =
    stateManager.updateState { state =>
      val updated = EditorState.removePane(state, paneId)
      if state.persisted.layout.editorPanes.size == 1 && state.persisted.layout.editorPanes.contains(paneId) then
        StateManagerTestFixtures.ensureCommandRunnerSurface(updated)
      else updated
    }

  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.setBufferForPane(paneId, bufferId))

  def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.setCursorPosition(paneId, line, column))

  def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.setViewport(paneId, viewport))

  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.setPaneProperties(paneId, update))

  def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    stateManager.insertPaneFixture(Some(afterPaneId), bufferId, SplitAxis.Horizontal)

  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    stateManager.insertPaneFixture(Some(paneId), bufferId, SplitAxis.Horizontal)

  def splitPaneVertical(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    stateManager.insertPaneFixture(Some(paneId), bufferId, SplitAxis.Vertical)

  def resizePaneSplit(splitId: WorkspaceNodeId, ratio: Double): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.resizePaneSplit(splitId, ratio))

  def focusPaneInDirection(direction: Direction): IO[Unit] =
    stateManager.updateState(StateManagerTestFixtures.focusPaneInDirection(direction))

  // getCurrentState + updateState(_ => newState) is not atomic the way the original `stateRef.modify` was; fine for
  // the sequential, single-fiber fixture setup every existing call site uses.
  private def insertPaneFixture(
    after: Option[PaneId],
    bufferId: Option[BufferId],
    axis: SplitAxis
  ): IO[PaneId] =
    for
      current <- stateManager.getCurrentState
      (newState, paneId) = StateManagerTestFixtures.insertPane(current, after, bufferId, axis)
      _ <- stateManager.updateState(_ => newState)
    yield paneId

private[serenity] object StateManagerTestFixtures:

  def setBufferForPane(paneId: PaneId, bufferId: BufferId): AppState => AppState =
    state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(bufferId = Some(bufferId))
          val stateWithBuffer = state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(
                editorPanes = state.persisted.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          )
          LayoutEngine.syncViewportDimensions(
            stateWithBuffer,
            stateWithBuffer.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
          )
        case None => state

  def setCursorPosition(paneId: PaneId, line: Int, column: Int): AppState => AppState =
    state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.persisted.buffers.get) match
            case Some(buffer) =>
              val newCursor = CursorPosition(line, column)
              val updatedBuffer = buffer.copy(
                editing = buffer.editing.copy(
                  cursors = List(newCursor),
                  preferredColumn = Some(column),
                  preferredXPx = None,
                  multiCursorVerticalStates = Nil
                )
              )
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer))
              )
            case None => state
        case None => state

  def setViewport(paneId: PaneId, viewport: Viewport): AppState => AppState =
    state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.persisted.buffers.get) match
            case Some(buffer) =>
              val updatedBuffer = buffer.copy(viewport = viewport)
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer))
              )
            case None => state
        case None => state

  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): AppState => AppState =
    state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(
                editorPanes = state.persisted.layout.editorPanes + (paneId -> update(pane))
              )
            )
          )
        case None => state

  def resizePaneSplit(splitId: WorkspaceNodeId, ratio: Double): AppState => AppState =
    state =>
      state.persisted.layout.effectiveWorkspaceTree
        .flatMap(_.resize(splitId, ratio))
        .map(tree =>
          state.copy(persisted =
            state.persisted
              .copy(layout = state.persisted.layout.copy(workspaceTree = Some(tree), paneOrder = tree.paneIds))
          )
        )
        .getOrElse(state)

  def focusPaneInDirection(direction: Direction): AppState => AppState =
    state =>
      val currentPaneId =
        state.persisted.focus match
          case Focus.EditorPane(paneId) => Some(paneId)
          case _                        => state.persisted.layout.activeEditorPaneId
      val viewportSize = state.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
      val layout       = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      currentPaneId
        .flatMap(LayoutEngine.directionalPaneNeighbor(state, layout, _, direction))
        .map { paneId =>
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId)),
              focus = Focus.EditorPane(paneId)
            )
          )
        }
        .getOrElse(state)

  def insertPane(
    state: AppState,
    requestedAfter: Option[PaneId],
    bufferId: Option[BufferId],
    splitAxis: SplitAxis
  ): (AppState, PaneId) =
    val paneId = state.runtime.nextPaneId
    val pane = bufferId match
      case Some(id) => EditorPane.withBuffer(paneId, id)
      case None     => EditorPane.empty(paneId)
    val targetPaneId =
      requestedAfter
        .filter(state.persisted.layout.editorPanes.contains)
        .orElse(state.persisted.layout.orderedPaneIds.lastOption)

    val updatedTree =
      targetPaneId match
        case Some(target) =>
          state.persisted.layout.effectiveWorkspaceTree.flatMap(
            _.split(
              target,
              paneId,
              splitAxis,
              WorkspaceNodeId(s"split-${target.value}-${paneId.value}"),
              WorkspaceNodeId(s"editor-${paneId.value}")
            )
          )
        case None =>
          Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))

    updatedTree match
      case Some(tree) =>
        val updatedState = state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = state.persisted.layout.editorPanes.updated(paneId, pane),
              activeEditorPaneId = Some(paneId),
              paneOrder = tree.paneIds,
              workspaceTree = Some(tree)
            ),
            focus = Focus.EditorPane(paneId)
          ),
          runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
        )
        (updatedState, paneId)
      case None =>
        (state, paneId)

  /** Mirrors `StateManagerOperationBoundary.ensureCommandRunnerSurface` (also duplicated, `private[manager]`, in
    * production) -- only reachable here for the closePane fixture's "closed the last pane" case.
    */
  def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry        = CommandRegistry.default
    val activatedRunner = CommandRunner.empty.activate(registry, state.persisted.config, state.runtime.isTuiMode)
    val runner = activatedRunner.copy(
      optionSelections = activatedRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
    )
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(runner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId
      .copy(runtime =
        stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot(_.id == surfaceId) :+ surface)
      )
      .pushFocus(Focus.Surface(surfaceId))
