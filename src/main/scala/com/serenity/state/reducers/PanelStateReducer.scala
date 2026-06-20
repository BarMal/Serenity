package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}

object PanelStateReducer:

  def pin(content: PanelContent, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val panel                    = UiSurface.fromPanelContent(surfaceId, content, position, size)
    ReducerResult.noEffects(
      stateWithId.copy(
        uiSurfaces = stateWithId.uiSurfaces :+ panel
      )
    )

  def focus(position: PanelPosition, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface) =>
        ReducerResult.noEffects(state.copy(focus = Focus.Surface(surface.id)))
      case None =>
        ReducerResult.noEffects(state)

  def resize(position: PanelPosition, newSize: Int, state: AppState): ReducerResult =
    val hasPanelAtPosition = panelSurfaceAt(position, state).isDefined
    if hasPanelAtPosition then
      val resizedSurfaces = state.uiSurfaces.map {
        case surface if isPinnedAt(position)(surface) =>
          surface.copy(presentation = SurfacePresentation.Pinned(position, newSize))
        case surface @ UiSurface(_, _, SurfacePresentation.Expanded(pos, _), _) if pos == position =>
          surface.copy(presentation = SurfacePresentation.Expanded(pos, newSize))
        case surface =>
          surface
      }
      ReducerResult.noEffects(state.copy(uiSurfaces = resizedSurfaces))
    else ReducerResult.noEffects(state)

  def unpin(position: PanelPosition, state: AppState): ReducerResult =
    panelToUnpin(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface) =>
        val nextFocus =
          if state.focus == Focus.Surface(surface.id) then fallbackEditorFocus(state)
          else state.focus
        ReducerResult.noEffects(
          state.copy(
            uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id),
            focus = nextFocus
          )
        )
      case None =>
        ReducerResult.noEffects(state)

  def expand(position: PanelPosition, state: AppState): ReducerResult =
    newestPinnedSurfaceAt(position, state).orElse(panelSurfaceAt(position, state)) match
      case Some(surface @ UiSurface(_, _, SurfacePresentation.Pinned(_, size), _)) =>
        val expanded = surface.copy(presentation = SurfacePresentation.Expanded(position, size))
        ReducerResult.noEffects(
          state.copy(
            uiSurfaces = replaceSurface(collapseExpandedSurfaces(state.uiSurfaces), expanded),
            focus = Focus.Surface(surface.id)
          )
        )
      case Some(surface @ UiSurface(_, _, SurfacePresentation.Expanded(_, _), _)) =>
        ReducerResult.noEffects(state.copy(focus = Focus.Surface(surface.id)))
      case _ =>
        ReducerResult.noEffects(state)

  def collapseExpandedPanel(state: AppState): ReducerResult =
    state.expandedPanelSurface match
      case Some(surface @ UiSurface(_, _, SurfacePresentation.Expanded(position, size), _)) =>
        val collapsed = surface.copy(presentation = SurfacePresentation.Pinned(position, size))
        ReducerResult.noEffects(
          state.copy(uiSurfaces = replaceSurface(state.uiSurfaces, collapsed))
        )
      case _ =>
        ReducerResult.noEffects(state)

  def pinPeekOverlay(position: PanelPosition, state: AppState): ReducerResult =
    pinActiveFloatingSurface(position, state)

  def pinActiveFloatingSurface(position: PanelPosition, state: AppState): ReducerResult =
    activeFloatingSurface(state)
      .flatMap(surface => toPinnedSurface(surface, position))
      .map { panel =>
        ReducerResult.noEffects(
          state.copy(
            uiSurfaces = replaceSurface(state.uiSurfaces, panel),
            focus = Focus.Surface(panel.id)
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
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_) | SurfaceContent.Diagnostics(_) |
          SurfaceContent.MarkdownPreview(_, _) =>
        Some(surface.copy(presentation = SurfacePresentation.Pinned(position, 30), dismissOnMove = false))
      case SurfaceContent.StartPage(_) | SurfaceContent.CommandPalette(_) |
          SurfaceContent.CommandPaletteSubmenu(_, _, _) | SurfaceContent.ThemePicker(_) | SurfaceContent.FileSearch(_) |
          SurfaceContent.ContextMenu(_) | SurfaceContent.ModalWorkflow(_) | SurfaceContent.QuickInfo(_) |
          SurfaceContent.FilePreview(_, _) | SurfaceContent.SymbolDefinition(_, _) | SurfaceContent.CursorInfoBar(_) |
          SurfaceContent.GhostOverlay(_, _) =>
        None

  private def replaceSurface(surfaces: List[UiSurface], updated: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == updated.id) :+ updated

  private def replacePinnedAtPosition(
    surfaces: List[UiSurface],
    position: PanelPosition,
    updated: UiSurface
  ): List[UiSurface] =
    surfaces.filterNot {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position   => true
        case SurfacePresentation.Expanded(pos, _) if pos == position => true
        case _                                                       => false
    } :+ updated

  private def panelToUnpin(position: PanelPosition, state: AppState): Option[UiSurface] =
    focusedPinnedSurfaceAt(position, state).orElse(newestPinnedSurfaceAt(position, state))

  private def focusedPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.focus match
      case Focus.Surface(surfaceId) =>
        state.surfaceById(surfaceId).filter(isPinnedAt(position))
      case _ =>
        None

  private def newestPinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.uiSurfaces.reverse.find(isPinnedAt(position))

  private def isPinnedAt(position: PanelPosition)(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Pinned(pos, _) if pos == position => true
      case _                                                     => false

  private def panelSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position   => true
        case SurfacePresentation.Expanded(pos, _) if pos == position => true
        case _                                                       => false
    }

  private def collapseExpandedSurfaces(surfaces: List[UiSurface]): List[UiSurface] =
    surfaces.map {
      case surface @ UiSurface(_, _, SurfacePresentation.Expanded(position, size), _) =>
        surface.copy(presentation = SurfacePresentation.Pinned(position, size))
      case surface => surface
    }

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
