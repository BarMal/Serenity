package com.serenity.state.reducers

import com.serenity.state.models.{AppState, Focus, PaneId, SurfaceContent, SurfacePresentation, UiSurface}
import com.serenity.ui.layout.{PanelContent, PanelPosition}

object PanelStateReducer:

  def pin(content: PanelContent, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val panel = UiSurface.fromPanelContent(surfaceId, content, position, size)
    ReducerResult.noEffects(
      stateWithId.copy(
        uiSurfaces = replacePinnedAtPosition(stateWithId.uiSurfaces, position, panel)
      )
    )

  def focus(position: PanelPosition, state: AppState): ReducerResult =
    pinnedSurfaceAt(position, state) match
      case Some(surface) =>
        ReducerResult.noEffects(state.copy(focus = Focus.Surface(surface.id)))
      case None =>
        ReducerResult.noEffects(state)

  def resize(position: PanelPosition, newSize: Int, state: AppState): ReducerResult =
    pinnedSurfaceAt(position, state) match
      case Some(surface) =>
        val resized = surface.copy(presentation = SurfacePresentation.Pinned(position, newSize))
        ReducerResult.noEffects(
          state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ resized)
        )
      case None =>
        ReducerResult.noEffects(state)

  def unpin(position: PanelPosition, state: AppState): ReducerResult =
    val nextFocus =
      pinnedSurfaceAt(position, state).map(_.id) match
        case Some(surfaceId) if state.focus == Focus.Surface(surfaceId) => fallbackEditorFocus(state)
        case _                                                          => state.focus

    ReducerResult.noEffects(
      state.copy(
        uiSurfaces = state.uiSurfaces.filterNot {
          _.presentation match
            case SurfacePresentation.Pinned(pos, _) if pos == position => true
            case _                                                     => false
        },
        focus = nextFocus
      )
    )

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
            content = SurfaceContent.DirectoryListing(path, entries, selectedPath.orElse(Some(path))),
            presentation = SurfacePresentation.Pinned(position, 30),
            dismissOnMove = false
          )
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_) | SurfaceContent.Diagnostics(_) =>
        Some(surface.copy(presentation = SurfacePresentation.Pinned(position, 30), dismissOnMove = false))
      case SurfaceContent.StartPage(_) | SurfaceContent.CommandPalette(_) | SurfaceContent.ThemePicker(_) |
           SurfaceContent.FileSearch(_) | SurfaceContent.ModalWorkflow(_) | SurfaceContent.QuickInfo(_) |
           SurfaceContent.FilePreview(_, _) | SurfaceContent.SymbolDefinition(_, _) |
           SurfaceContent.GhostOverlay(_, _) =>
        None

  private def replaceSurface(surfaces: List[UiSurface], updated: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == updated.id) :+ updated

  private def replacePinnedAtPosition(surfaces: List[UiSurface], position: PanelPosition, updated: UiSurface): List[UiSurface] =
    surfaces.filterNot {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position => true
        case _                                                     => false
    } :+ updated

  private def pinnedSurfaceAt(position: PanelPosition, state: AppState): Option[UiSurface] =
    state.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position => true
        case _                                                     => false
    }

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
