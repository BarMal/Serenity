package com.serenity.ui.layout

import com.serenity.keystroke.events.Direction
import com.serenity.state.models.{AppState, PaneId, PanelId, PanelRegistry, SurfaceId}

/** Something keyboard focus can rest on in the workspace: an editor pane or a docked panel. */
enum FocusTarget:
  case Pane(id: PaneId)
  case Panel(id: SurfaceId)

object DirectionalFocusLayout:

  /** Every on-screen target that can take focus, editor panes first in pane order, then docked panels in tree order. */
  def targets(state: AppState, layout: CalculatedLayout): List[(FocusTarget, LayoutRect)] =
    val paneRects = EditorPaneLayoutEngine.calculatePaneLayouts(state, layout)
    val panes = state.persisted.layout.orderedPaneIds.flatMap(id => paneRects.get(id).map(FocusTarget.Pane(id) -> _))
    val panels = state.pinnedSurfaces
      .filter(surface => PanelId.forContent(surface.content).exists(PanelRegistry.registrationFor(_).focusable))
      .flatMap(surface => layout.pinnedSurfaceRects.get(surface.id).map(FocusTarget.Panel(surface.id) -> _))
    (panes ++ panels).filter((_, rect) => rect.width > 0 && rect.height > 0)

  /** The nearest candidate lying wholly in `direction` from `from`: smallest gap first, then the one best aligned with
    * `from` across that direction, then the earliest in `candidates`.
    */
  def neighbour(
    from: LayoutRect,
    candidates: List[(FocusTarget, LayoutRect)],
    direction: Direction
  ): Option[FocusTarget] =
    candidates.zipWithIndex
      .flatMap {
        case ((target, rect), order) =>
          rank(from, rect, direction).map((gap, misalignment) => (target, gap, misalignment, order))
      }
      .sortBy((_, gap, misalignment, order) => (gap, misalignment, order))
      .headOption
      .map(_._1)

  private def rank(from: LayoutRect, candidate: LayoutRect, direction: Direction): Option[(Int, Int)] =
    direction match
      case Direction.Left if candidate.right <= from.x =>
        Some((from.x - candidate.right, math.abs(candidate.centerY - from.centerY)))
      case Direction.Right if candidate.x >= from.right =>
        Some((candidate.x - from.right, math.abs(candidate.centerY - from.centerY)))
      case Direction.Up if candidate.bottom <= from.y =>
        Some((from.y - candidate.bottom, math.abs(candidate.centerX - from.centerX)))
      case Direction.Down if candidate.y >= from.bottom =>
        Some((candidate.y - from.bottom, math.abs(candidate.centerX - from.centerX)))
      case _ =>
        None
