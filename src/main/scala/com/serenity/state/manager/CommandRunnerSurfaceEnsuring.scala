package com.serenity.state.manager

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.state.models.*

/** The floating command runner surface a commit falls back to when the last editor closes. */
private[manager] object CommandRunnerSurfaceEnsuring:

  def ensured(state: AppState): AppState =
    val registry = CommandRegistry.default
    val runner =
      CommandRunner.empty.activate(
        registry,
        state.persisted.config,
        state.runtime.capabilities,
        state.commandRunnerContext
      )
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(runner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId
      .copy(runtime =
        stateWithId.runtime
          .copy(uiSurfaces = stateWithId.runtime.uiSurfaces.movedToEndWhere(_.id == surfaceId)(surface))
      )
      .pushFocus(Focus.Surface(surfaceId))
