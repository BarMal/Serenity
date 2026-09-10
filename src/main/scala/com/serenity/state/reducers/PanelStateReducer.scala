package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}

object PanelStateReducer:

  /** Default absolute size (in cells) given to a floating surface pinned via drag-to-edge or peek-to-pin, matching
    * every `toPinnedSurface` content case -- the tree has no notion of a content-specific default, so the requested
    * ratio is seeded from this constant immediately after docking.
    */
  private val PeekToPinDefaultSize = 30

  /** Pin/unpin declare their own undo boundary (#1016 PR4) -- the same "the code that performs the change declares it"
    * principle #1361 applied to buffer edits, generalized here to the non-reducer-only panel-pin surface. Move, resize,
    * and expand/collapse are left non-undoable: they're view adjustments to an already-pinned panel, not the pin/unpin
    * structural change the acceptance criterion names.
    */
  def pin(content: PanelContent, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val undoEntry                = HistoryEntry.PanelChange.capture(state)
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val panel                    = UiSurface.fromPanelContent(surfaceId, content)
    val workspaceTree = stateWithId.persisted.layout.workspaceTree.flatMap { tree =>
      val (splitId, leafId) = tree.nextDockIds(surfaceId)
      tree.dockSized(surfaceId, position, splitId, leafId, size, stateWithId.runtime.viewportSize)
    }
    ReducerResult.withEffect(
      stateWithId.copy(
        persisted = stateWithId.persisted.copy(
          layout = stateWithId.persisted.layout
            .copy(workspaceTree = workspaceTree.orElse(stateWithId.persisted.layout.workspaceTree))
        ),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ panel)
      ),
      AppEffect.Undo(UndoEffect.RecordBoundary(undoEntry, groupable = false))
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

  /** The current size of a pinned surface, or `None` if it isn't pinned -- used by `StateManagerEffectHandlers`'s
    * command/keyboard resize path (issue #1310) to compute a delta-adjusted absolute size before calling `resize`.
    * Reads back through the workspace tree's owning-split ratio (issue #817) -- the sole size record for a docked
    * surface -- rather than any size carried on the surface itself.
    */
  def currentSize(surfaceId: SurfaceId, state: AppState): Option[Int] =
    state.persisted.layout.workspaceTree.flatMap(_.currentSize(surfaceId, state.runtime.viewportSize))

  def resize(surfaceId: SurfaceId, newSize: Int, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(_) =>
        val resizedTree = for
          tree    <- state.persisted.layout.workspaceTree
          ratio   <- tree.allocationRatio(surfaceId, newSize, state.runtime.viewportSize)
          resized <- tree.resizeSurface(surfaceId, ratio)
        yield resized
        resizedTree match
          case Some(_) =>
            ReducerResult.noEffects(
              state.copy(persisted =
                state.persisted.copy(layout = state.persisted.layout.copy(workspaceTree = resizedTree))
              )
            )
          case None =>
            ReducerResult.noEffects(state)
      case None =>
        ReducerResult.noEffects(state)

  def resize(position: PanelPosition, newSize: Int, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state) match
      case Some(surface) => resize(surface.id, newSize, state)
      case None          => ReducerResult.noEffects(state)

  def unpin(surfaceId: SurfaceId, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(surface) =>
        val undoEntry = HistoryEntry.PanelChange.capture(state)
        val nextFocus =
          if state.persisted.focus == Focus.Surface(surface.id) then fallbackEditorFocus(state)
          else state.persisted.focus
        val nextTree = state.persisted.layout.workspaceTree.flatMap(_.removeSurface(surfaceId))
        val maximized = state.persisted.layout.maximizedWorkspaceNodeId.filterNot(nodeId =>
          state.persisted.layout.workspaceTree.flatMap(_.surfaceIdForNode(nodeId)).contains(surfaceId)
        )
        ReducerResult.withEffect(
          state.copy(
            persisted = state.persisted.copy(
              layout = state.persisted.layout.copy(
                workspaceTree = nextTree.orElse(state.persisted.layout.workspaceTree),
                maximizedWorkspaceNodeId = maximized
              ),
              focus = nextFocus
            ),
            runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id))
          ),
          AppEffect.Undo(UndoEffect.RecordBoundary(undoEntry, groupable = false))
        )
      case None =>
        ReducerResult.noEffects(state)

  def unpin(position: PanelPosition, state: AppState): ReducerResult =
    panelToUnpin(position, state) match
      case Some(surface) => unpin(surface.id, state)
      case None          => ReducerResult.noEffects(state)

  def move(surfaceId: SurfaceId, position: PanelPosition, state: AppState): ReducerResult =
    state.surfaceById(surfaceId).filter(isPinned) match
      case Some(_) =>
        // Preserve the panel's own size across the move (issue #817) -- `moveSurface` alone would otherwise leave
        // the new edge's split at `WorkspaceTree.dock`'s default ratio, silently shrinking/growing an already-sized
        // panel just because it changed edges.
        val previousSize = currentSize(surfaceId, state)
        val movedTree = state.persisted.layout.workspaceTree.flatMap { tree =>
          tree.moveSurface(surfaceId, position, tree.nextDockIds(surfaceId)._1).map { moved =>
            previousSize
              .flatMap(size => moved.allocationRatio(surfaceId, size, state.runtime.viewportSize))
              .flatMap(ratio => moved.resizeSurface(surfaceId, ratio))
              .getOrElse(moved)
          }
        }
        val orderedSurfaces = movedTree
          .map(tree =>
            tree.dockedSurfaceIds.flatMap(id => state.runtime.uiSurfaces.find(_.id == id)) ++
              state.runtime.uiSurfaces.filterNot(surface => tree.dockedSurfaceIds.contains(surface.id))
          )
          .getOrElse(state.runtime.uiSurfaces)
        ReducerResult.noEffects(
          state.copy(
            persisted = state.persisted.copy(
              layout =
                state.persisted.layout.copy(workspaceTree = movedTree.orElse(state.persisted.layout.workspaceTree))
            ),
            runtime = state.runtime.copy(uiSurfaces = orderedSurfaces)
          )
        )
      case None =>
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
      .flatMap(toPinnedSurface)
      .map { panel =>
        val tree = state.persisted.layout.workspaceTree.flatMap { workspaceTree =>
          val (splitId, leafId) = workspaceTree.nextDockIds(panel.id)
          workspaceTree.dockSized(panel.id, position, splitId, leafId, PeekToPinDefaultSize, state.runtime.viewportSize)
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

  private def toPinnedSurface(surface: UiSurface): Option[UiSurface] =
    surface.content match
      case SurfaceContent.DirectoryListing(path, entries, selectedPath) =>
        Some(
          surface.copy(
            content = SurfaceContent.DirectoryTree(
              DirectoryTreeData(path, entries = Map(path -> entries)),
              selectedPath.orElse(Some(path))
            ),
            presentation = SurfacePresentation.Docked,
            dismissOnMove = false
          )
        )
      case SurfaceContent.DirectoryTree(tree, selectedPath) =>
        Some(
          surface.copy(
            content = SurfaceContent.DirectoryTree(tree, selectedPath.orElse(Some(tree.rootPath))),
            presentation = SurfacePresentation.Docked,
            dismissOnMove = false
          )
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) |
          SurfaceContent.Diagnostics(_, _) | SurfaceContent.MarkdownPreview(_, _) =>
        Some(surface.copy(presentation = SurfacePresentation.Docked, dismissOnMove = false))
      case SurfaceContent.StartPage(_) | SurfaceContent.CommandPalette(_) | SurfaceContent.CommandRunnerPeek(_) |
          SurfaceContent.ThemePicker(_) | SurfaceContent.ThemeCreator(_) | SurfaceContent.FileSearch(_) |
          SurfaceContent.ContextualToolbar(_) | SurfaceContent.ContextMenu(_) | SurfaceContent.CommentLens(_) |
          SurfaceContent.ModalWorkflow(_) | SurfaceContent.QuickInfo(_) | SurfaceContent.FilePreview(_, _) |
          SurfaceContent.SymbolDefinition(_, _) | SurfaceContent.CursorInfoBar(_) | SurfaceContent.GhostOverlay(_, _) |
          SurfaceContent.ShortcutsHelp(_) | SurfaceContent.TabList(_, _) | SurfaceContent.RecentFilesInMode(_, _) |
          SurfaceContent.CompanionSprite =>
        None

  private def replaceSurface(surfaces: List[UiSurface], updated: UiSurface): List[UiSurface] =
    surfaces.movedToEndWhere(_.id == updated.id)(updated)

  private def panelToUnpin(position: PanelPosition, state: AppState): Option[UiSurface] =
    focusedPinnedSurfaceAt(position, state).orElse(newestPinnedSurfaceAt(position, state))

  private def focusedPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.persisted.focus match
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId).filter(isPinnedAt(position, state))
      case _ =>
        None

  private def newestPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(isPinnedAt(position, state))

  private def isPinnedAt(position: PanelPosition, state: AppState)(surface: UiSurface): Boolean =
    isPinned(surface) && state.persisted.layout.workspaceTree
      .flatMap(_.positionForSurface(surface.id))
      .contains(
        position
      )

  private def isPinned(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Docked => true
      case _                          => false

  private def panelSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.find(isPinnedAt(position, state))

  private def fallbackEditorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
