package com.serenity.state.reducers

import com.serenity.state.models.{AppState, CursorPosition, Focus, PaneId, SurfaceContent, SurfacePlacement, SurfacePresentation, UiSurface}
import com.serenity.ui.layout.PeekContent

object PeekStateReducer:

  def show(content: PeekContent, at: CursorPosition, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = toSurfaceContent(content),
      presentation = SurfacePresentation.Floating(Some(at), SurfacePlacement.AboveCursor),
      dismissOnMove = true
    )
    ReducerResult.noEffects(
      stateWithId.copy(
        uiSurfaces = stateWithId.uiSurfaces.filterNot(isPeekSurface) :+ surface,
        focus = Focus.Surface(surfaceId)
      )
    )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        uiSurfaces = state.uiSurfaces.filterNot(isPeekSurface),
        focus = fallbackEditorFocus(state)
      )
    )

  private def isPeekSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
      case _                                                             => false

  private def toSurfaceContent(content: PeekContent): SurfaceContent =
    content match
      case PeekContent.QuickInfo(text) =>
        SurfaceContent.QuickInfo(text)
      case PeekContent.FilePreview(path, content) =>
        SurfaceContent.FilePreview(path, content)
      case PeekContent.SymbolDefinition(symbol, location) =>
        SurfaceContent.SymbolDefinition(symbol, location)
      case PeekContent.DirectoryListing(path, entries) =>
        SurfaceContent.DirectoryListing(path, entries)

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
