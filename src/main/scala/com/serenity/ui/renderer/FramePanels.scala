package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** A panel as the frame places it: the cells it is placed in, the pixels it paints, and those pixels with a margin.
  *
  * `painted` is what the panel can change on the frame; `bounds` is that with a margin, so a record of what the panel
  * painted still covers everything it draws.
  */
final private[renderer] case class PanelSite(
    id: SurfaceId,
    rect: LayoutRect,
    painted: PixelRect,
    bounds: PixelRect,
    content: Any
)

private[renderer] object PanelSite:
  private val MarginCells = 2

  def apply(
    id: SurfaceId,
    rect: LayoutRect,
    verticalOffsetRows: Double,
    metrics: CellMetrics,
    window: PixelRect,
    content: Any
  ): PanelSite =
    val left    = metrics.toPixelX(rect.x)
    val top     = metrics.toPixelY(rect.y)
    val width   = metrics.toPixelX(rect.right) - left
    val height  = metrics.toPixelY(rect.bottom) - top
    val shift   = math.ceil(math.abs(FloatingSurfaceGeometry.signedRowOffsetPixels(verticalOffsetRows, metrics))).toInt
    val painted = PixelRect(left, top - shift, width, height + 2 * shift)
    val marginX = MarginCells * metrics.charWidth
    val marginY = MarginCells * metrics.lineHeight
    val inflated =
      PixelRect(
        painted.xPx - marginX,
        painted.yPx - marginY,
        painted.widthPx + 2 * marginX,
        painted.heightPx + 2 * marginY
      )
    PanelSite(id, rect, painted, inflated.intersection(window).getOrElse(window), content)

/** What one frame knows about its panels, worked out once: the overlay view model every layer reads from, where each
  * panel sits, and which panels the frame keeps from the pixels it started with instead of compositing them again.
  */
final private[renderer] case class FramePanels(
    overlays: OverlayViews,
    sites: Map[SurfaceId, PanelSite],
    kept: Set[SurfaceId]
):

  /** Floating overlays in paint order. */
  def floating: List[TextOverlayView] =
    overlays.tabBar.toList ++ overlays.aboveCursorStack ++ belowCursorOverlays ++ overlays.cornerStack

  def belowCursorOverlays: List[TextOverlayView] =
    if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList

  def keeping(panelIds: Set[SurfaceId]): FramePanels = copy(kept = panelIds)

  /** Every panel on screen this frame, for dropping the records of those that have gone. */
  def activeIds(state: AppState): Set[SurfaceId] =
    RendererFloatingPanels.pinnedAndExpandedSurfaces(state).map(_.id).toSet ++
      (overlays.aboveCursorStack ++ overlays.belowCursorStack ++ overlays.cornerStack ++ overlays.tabBar.toList)
        .flatMap(_.surfaceId)

private[renderer] object FramePanels:

  def of(state: AppState, context: RenderContext, scene: UiSceneSnapshot): FramePanels =
    val overlays = OverlayViewModel.fromState(state, scene, context.caches.tabBarMeasures)
    FrameCounting.overlayModelBuilt(context.caches.frameCounters)
    val window = PixelRect(
      0,
      0,
      context.surface.viewportWidth * context.cellMetrics.charWidth,
      context.surface.viewportHeight * context.cellMetrics.lineHeight
    )
    val panels = FramePanels(overlays, Map.empty, Set.empty)
    val pinned = RendererFloatingPanels.pinnedAndExpandedSurfaces(state).flatMap { surface =>
      scene.workspace.collectFirst {
        case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) if surfaceId == surface.id =>
          PanelSite(surface.id, node.frameRect, 0.0, context.cellMetrics, window, surface)
      }
    }
    val floating = panels.floating.flatMap { overlay =>
      overlay.surfaceId.map(
        PanelSite(_, overlay.rect, overlay.verticalOffsetRows, context.cellMetrics, window, overlay)
      )
    }
    panels.copy(sites = (pinned ++ floating).map(site => site.id -> site).toMap)

/** Which panels a frame can leave exactly as the surface already holds them. */
private[renderer] object PanelReuse:

  final case class Frame(viewportWidth: Int, viewportHeight: Int, cursorVisible: Boolean)

  /** A panel is kept when what `holds` says the surface holds is current, and no other layer has repainted, or will
    * repaint, the pixels it covers: nothing in `repainted` touches it, no other panel shares them, and no other
    * floating panel stood there last frame (what it left behind would be kept with it).
    */
  def kept(
    panels: FramePanels,
    holds: PanelSite => Boolean,
    isDirty: SurfaceId => Boolean,
    repainted: List[PixelRect],
    previousFloating: Map[SurfaceId, PixelRect]
  ): Set[SurfaceId] =
    val sites = panels.sites.values.toList
    sites.collect {
      case site
          if !isDirty(site.id) && holds(site) && !repainted.exists(_.intersects(site.painted)) &&
            !sites.exists(other => other.id != site.id && other.painted.intersects(site.painted)) &&
            !previousFloating.exists((id, rect) => id != site.id && rect.intersects(site.painted)) =>
        site.id
    }.toSet
