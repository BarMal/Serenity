package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition

private[serenity] object CommandRunnerPanelSelections:

  def fromState(state: AppState): Map[String, Int] =
    PanelId.values.toList.map(id => optionId(id) -> selectedIndex(id, state)).toMap

  private def selectedIndex(id: PanelId, state: AppState): Int =
    state.runtime.uiSurfaces.reverse
      .find(surface => PanelId.forContent(surface.content).contains(id))
      .flatMap(surface => positionOf(surface, state))
      .map(positionIndex)
      .getOrElse(0)

  private def optionId(id: PanelId): String = s"panel-${id.key}-pin"

  private def positionOf(surface: UiSurface, state: AppState): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Docked => state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
      case SurfacePresentation.Floating(_, _) => None

  private def positionIndex(position: PanelPosition): Int =
    position match
      case PanelPosition.Top    => 1
      case PanelPosition.Right  => 2
      case PanelPosition.Bottom => 3
      case PanelPosition.Left   => 4
