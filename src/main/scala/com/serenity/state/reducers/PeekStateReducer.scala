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
      stateWithId
        .copy(runtime =
          stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot(_.isFloatingPeek) :+ surface)
        )
        .pushFocusUnlessPeek(surface)
    )

  /** Removes the peek and, if it held focus, hands focus back to what held it before -- the active pane if nothing did.
    */
  def dismiss(state: AppState): ReducerResult =
    val dismissed =
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.isFloatingPeek)))
    val focusGone = dismissed.persisted.focus match
      case Focus.Surface(surfaceId) => dismissed.surfaceById(surfaceId).isEmpty
      case _                        => false
    ReducerResult.noEffects(if focusGone then dismissed.popFocus else dismissed)

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
