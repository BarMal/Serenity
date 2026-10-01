package com.serenity.state.components

import com.serenity.state.models.{SurfaceContent, UiSurface, movedToEndWhere}

private[components] object PanelSurfaces:

  /** `surface` showing `content` instead, moved to the end of the surface list as the one most recently used. */
  def replaced(surface: UiSurface, content: SurfaceContent): ComponentResult =
    val updated = surface.copy(content = content)
    ComponentResult.updateState(state =>
      state.copy(runtime =
        state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == updated.id)(updated))
      )
    )
