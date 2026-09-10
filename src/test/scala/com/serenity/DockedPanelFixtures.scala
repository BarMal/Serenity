package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Test-only helper for building docked-panel fixtures (issue #817): a docked surface's position and size are owned
  * solely by the workspace tree (a `WorkspaceNode.DockedSurface`'s position, and its owning split's ratio), not by
  * `UiSurface.presentation`, so a fixture that wants a specific surface id at a specific position/size has to dock it
  * into the tree itself -- mirroring what `PanelStateReducer.pin` does for the real pin path, but taking a
  * caller-chosen id instead of allocating a fresh one.
  */
object DockedPanelFixtures:

  /** Docks an already-present surface `id` at `position` into `state`'s workspace tree, seeding the owning split's
    * ratio from `size` via the same conversion `PanelStateReducer.pin` uses -- for a fixture that builds its
    * `UiSurface`s (e.g. alongside floating ones) up front and only needs the tree entry added separately.
    */
  def dockExisting(state: AppState, id: SurfaceId, position: PanelPosition, size: Int): AppState =
    val tree = state.persisted.layout.effectiveWorkspaceTree.flatMap { workspaceTree =>
      val (splitId, leafId) = workspaceTree.nextDockIds(id)
      workspaceTree.dockSized(id, position, splitId, leafId, size, state.runtime.viewportSize)
    }
    state.copy(persisted =
      state.persisted.copy(
        layout = state.persisted.layout.copy(workspaceTree = tree.orElse(state.persisted.layout.workspaceTree))
      )
    )

  /** Docks `content` as `id` at `position` in `state`'s workspace tree, seeding the owning split's ratio from `size`
    * via the same conversion `PanelStateReducer.pin` uses, and appends the resulting surface to `runtime.uiSurfaces`.
    */
  def dock(state: AppState, id: SurfaceId, content: SurfaceContent, position: PanelPosition, size: Int): AppState =
    val surface = UiSurface(id = id, content = content, presentation = SurfacePresentation.Docked)
    dockExisting(
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ surface)),
      id,
      position,
      size
    )

  def dock(state: AppState, id: SurfaceId, content: PanelContent, position: PanelPosition, size: Int): AppState =
    dock(state, id, content.asSurfaceContent, position, size)

  /** Docks every `(id, content, position, size)` fixture in turn, folding each dock over the previous state. */
  def dockAll(state: AppState, panels: List[(SurfaceId, PanelContent, PanelPosition, Int)]): AppState =
    panels.foldLeft(state) {
      case (currentState, (id, content, position, size)) => dock(currentState, id, content, position, size)
    }

  def dockAllContent(state: AppState, panels: List[(SurfaceId, SurfaceContent, PanelPosition, Int)]): AppState =
    panels.foldLeft(state) {
      case (currentState, (id, content, position, size)) => dock(currentState, id, content, position, size)
    }

  /** Marks `id` as the maximized/expanded workspace node, mirroring `PanelStateReducer.expand`. */
  def expand(state: AppState, id: SurfaceId): AppState =
    state.persisted.layout.workspaceTree.flatMap(_.nodeIdForSurface(id)) match
      case Some(nodeId) =>
        state.copy(persisted =
          state.persisted.copy(layout = state.persisted.layout.copy(maximizedWorkspaceNodeId = Some(nodeId)))
        )
      case None => state
