package com.serenity.state.reducers

import com.serenity.state.models.*

/** The header a terminal launch shows when Skia could not load: a floating row on the top edge that never takes focus
  * and stays until [[dismissed]] by Escape or a click. Like a corner notice it is a surface of its own, so showing and
  * dismissing it need nothing the renderer does not already do for a surface that comes and goes.
  */
object FallbackHeaderReducer:

  def shown(state: AppState, message: String): AppState =
    val (withId, surfaceId) = without(state).allocateSurfaceId
    val header = UiSurface(
      id = surfaceId,
      content = SurfaceContent.FallbackHeader(message),
      presentation = SurfacePresentation.Floating(None, SurfacePlacement.TopEdge)
    )
    withId.copy(runtime = withId.runtime.copy(uiSurfaces = withId.runtime.uiSurfaces :+ header))

  def dismissed(state: AppState): AppState =
    if isShown(state) then without(state) else state

  def isShown(state: AppState): Boolean =
    state.runtime.uiSurfaces.exists(isHeader)

  private def without(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isHeader)))

  private def isHeader(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.FallbackHeader(_) => true
      case _                                => false
