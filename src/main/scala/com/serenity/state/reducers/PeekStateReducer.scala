package com.serenity.state.reducers

import com.serenity.state.models.*
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
        persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
        runtime =
          stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot(isPeekSurface) :+ surface)
      )
    )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        persisted = state.persisted.copy(focus = fallbackEditorFocus(state)),
        runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isPeekSurface))
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
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))
