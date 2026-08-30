package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition, WorkspaceNodeId, WorkspaceTree}

object PanelStateReducer:

  def pin(content: PanelContent, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val panel                    = UiSurface.fromPanelContent(surfaceId, content, position, size)
    val workspaceTree = stateWithId.persisted.layout.effectiveWorkspaceTree.flatMap { tree =>
      tree.dock(surfaceId, position, nextSplitId(tree, surfaceId), WorkspaceNodeId(s"dock-${surfaceId.value}"))
    }
    ReducerResult.noEffects(
      stateWithId.copy(
        persisted = stateWithId.persisted.copy(
          layout = stateWithId.persisted.layout
            .copy(workspaceTree = workspaceTree.orElse(stateWithId.persisted.layout.workspaceTree))
        ),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ panel)
      )
    )

  def focus(surfaceId: SurfaceId, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(_) =>
        ReducerResult.noEffects(state.copy(persisted = state.persisted.copy(focus = Focus.Surface(surfaceId))))
      case None => ReducerResult.noEffects(state)

  def focus(position: PanelPosition, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface) => focus(surface.id, state)
      case None          => ReducerResult.noEffects(state)

  def resize(surfaceId: SurfaceId, newSize: Int, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, _), _)) =>
        val resized = surface.copy(presentation = SurfacePresentation.Pinned(position, newSize))
        val resizedTree = state.persisted.layout.workspaceTree.flatMap(
          _.resizeSurface(surfaceId, surfaceAllocationRatio(position, newSize, state))
        )
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout =
                state.persisted.layout.copy(workspaceTree = resizedTree.orElse(state.persisted.layout.workspaceTree))
            ),
            runtime = state.runtime.copy(uiSurfaces = replaceSurfaceInPlace(state.runtime.uiSurfaces, resized))
          )
        )
      case _ =>
        ReducerResult.noEffects(state)

  def resize(position: PanelPosition, newSize: Int, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state) match
      case Some(surface) => resize(surface.id, newSize, state)
      case None          => ReducerResult.noEffects(state)

  def unpin(surfaceId: SurfaceId, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(surface) =>
        val nextFocus =
          if state.persisted.focus == Focus.Surface(surface.id) then fallbackEditorFocus(state)
          else state.persisted.focus
        val nextTree = state.persisted.layout.workspaceTree.flatMap(_.removeSurface(surfaceId))
        val maximized = state.persisted.layout.maximizedWorkspaceNodeId.filterNot(nodeId =>
          state.persisted.layout.workspaceTree.flatMap(_.surfaceIdForNode(nodeId)).contains(surfaceId)
        )
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(
                workspaceTree = nextTree.orElse(state.persisted.layout.workspaceTree),
                maximizedWorkspaceNodeId = maximized
              ),
              focus = nextFocus
            ),
            runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id))
          )
        )
      case None =>
        ReducerResult.noEffects(state)

  def unpin(position: PanelPosition, state: AppState): ReducerResult =
    panelToUnpin(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface) => unpin(surface.id, state)
      case None          => ReducerResult.noEffects(state)

  def move(surfaceId: SurfaceId, position: PanelPosition, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(surface @ UiSurface(_, _, SurfacePresentation.Pinned(_, size), _)) =>
        val movedTree = state.persisted.layout.workspaceTree.flatMap { tree =>
          tree.moveSurface(surfaceId, position, nextSplitId(tree, surfaceId))
        }
        val moved           = surface.copy(presentation = SurfacePresentation.Pinned(position, size))
        val updatedSurfaces = replaceSurfaceInPlace(state.runtime.uiSurfaces, moved)
        val orderedSurfaces = movedTree
          .map(tree =>
            tree.dockedSurfaceIds.flatMap(surfaceId => updatedSurfaces.find(_.id == surfaceId)) ++
              updatedSurfaces.filterNot(surface => tree.dockedSurfaceIds.contains(surface.id))
          )
          .getOrElse(updatedSurfaces)
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout =
                state.persisted.layout.copy(workspaceTree = movedTree.orElse(state.persisted.layout.workspaceTree))
            ),
            runtime = state.runtime.copy(uiSurfaces = orderedSurfaces)
          )
        )
      case _ =>
        ReducerResult.noEffects(state)

  def expand(surfaceId: SurfaceId, state: AppState): ReducerResult =
    state.persisted.layout.workspaceTree.flatMap(_.nodeIdForSurface(surfaceId)) match
      case Some(nodeId) =>
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(maximizedWorkspaceNodeId = Some(nodeId)),
              focus = Focus.Surface(surfaceId)
            )
          )
        )
      case None =>
        ReducerResult.noEffects(state)

  def expand(position: PanelPosition, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface) => expand(surface.id, state)
      case None          => ReducerResult.noEffects(state)

  def collapseExpandedPanel(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(persisted =
        state.persisted.copy(layout = state.persisted.layout.copy(maximizedWorkspaceNodeId = None))
      )
    )

  def pinPeekOverlay(position: PanelPosition, state: AppState): ReducerResult =
    pinActiveFloatingSurface(position, state)

  def pinActiveFloatingSurface(position: PanelPosition, state: AppState): ReducerResult =
    activeFloatingSurface(state)
      .flatMap(surface => toPinnedSurface(surface, position))
      .map { panel =>
        val tree = state.persisted.layout.effectiveWorkspaceTree.flatMap { workspaceTree =>
          workspaceTree.dock(
            panel.id,
            position,
            nextSplitId(workspaceTree, panel.id),
            WorkspaceNodeId(s"dock-${panel.id.value}")
          )
        }
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(workspaceTree = tree.orElse(state.persisted.layout.workspaceTree)),
              focus = Focus.Surface(panel.id)
            ),
            runtime = state.runtime.copy(uiSurfaces = replaceSurface(state.runtime.uiSurfaces, panel))
          )
        )
      }
      .getOrElse(ReducerResult.noEffects(state))

  private def activeFloatingSurface(state: AppState): Option[UiSurface] =
    state.activeSurface.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, _) => true
        case _                                  => false
    }

  private def toPinnedSurface(surface: UiSurface, position: PanelPosition): Option[UiSurface] =
    surface.content match
      case SurfaceContent.DirectoryListing(path, entries, selectedPath) =>
        Some(
          surface.copy(
            content = SurfaceContent.DirectoryTree(
              DirectoryTreeData(path, entries = Map(path -> entries)),
              selectedPath.orElse(Some(path))
            ),
            presentation = SurfacePresentation.Pinned(position, 30),
            dismissOnMove = false
          )
        )
      case SurfaceContent.DirectoryTree(tree, selectedPath) =>
        Some(
          surface.copy(
            content = SurfaceContent.DirectoryTree(tree, selectedPath.orElse(Some(tree.rootPath))),
            presentation = SurfacePresentation.Pinned(position, 30),
            dismissOnMove = false
          )
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) |
          SurfaceContent.Diagnostics(_, _) | SurfaceContent.MarkdownPreview(_, _) =>
        Some(surface.copy(presentation = SurfacePresentation.Pinned(position, 30), dismissOnMove = false))
      case SurfaceContent.StartPage(_) | SurfaceContent.CommandPalette(_) | SurfaceContent.CommandRunnerPeek(_) |
          SurfaceContent.ThemePicker(_) | SurfaceContent.ThemeCreator(_) | SurfaceContent.FileSearch(_) |
          SurfaceContent.ContextualToolbar(_) | SurfaceContent.ContextMenu(_) | SurfaceContent.CommentLens(_) |
          SurfaceContent.ModalWorkflow(_) | SurfaceContent.QuickInfo(_) | SurfaceContent.FilePreview(_, _) |
          SurfaceContent.SymbolDefinition(_, _) | SurfaceContent.CursorInfoBar(_) | SurfaceContent.GhostOverlay(_, _) =>
        None

  private def replaceSurface(surfaces: List[UiSurface], updated: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == updated.id) :+ updated

  private def panelToUnpin(position: PanelPosition, state: AppState): Option[UiSurface] =
    focusedPinnedSurfaceAt(position, state).orElse(newestPinnedSurfaceAt(position, state))

  private def focusedPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.persisted.focus match
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId).filter(isPinnedAt(position))
      case _ =>
        None

  private def newestPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(isPinnedAt(position))

  private def isPinnedAt(position: PanelPosition)(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Pinned(pos, _) if pos == position => true
      case _                                                     => false

  private def isPinned(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Pinned(_, _) => true
      case _                                => false

  private def panelSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position   => true
        case SurfacePresentation.Expanded(pos, _) if pos == position => true
        case _                                                       => false
    }

  private def replaceSurfaceInPlace(surfaces: List[UiSurface], updated: UiSurface): List[UiSurface] =
    surfaces.map {
      case surface if surface.id == updated.id => updated
      case surface                             => surface
    }

  private def nextSplitId(tree: WorkspaceTree, surfaceId: SurfaceId): WorkspaceNodeId =
    WorkspaceNodeId(s"dock-split-${surfaceId.value}-${tree.nodeIds.size}")

  private def surfaceAllocationRatio(position: PanelPosition, requestedSize: Int, state: AppState): Double =
    val total = state.runtime.viewportSize.fold(100) { viewport =>
      position match
        case PanelPosition.Left | PanelPosition.Right => viewport.width
        case PanelPosition.Top | PanelPosition.Bottom => viewport.height
    }
    requestedSize.toDouble / total.max(1)

  private def fallbackEditorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
