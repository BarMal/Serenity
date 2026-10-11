package com.serenity.ui.renderer

import com.serenity.diagnostics.{FrameCounter, FrameCounters}
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}

/** Feeds [[FrameCounters]] from the two ends of [[RendererFramePlanner.renderFrame]]: what the plan decided before the
  * frame drew, and what the surface did once it flushed.
  */
private[renderer] object FrameCounting:

  /** `plan` is `None` for a frame that repaints everything, chrome and the whole cell grid included. */
  def planned(counters: FrameCounters, plan: Option[FramePlan], viewport: ViewportSize, metrics: CellMetrics): Unit =
    if counters.isEnabled then
      counters.add(FrameCounter.ChromePainted, if plan.forall(_.chrome.paintsAny) then 1L else 0L)
      counters.add(FrameCounter.PresentAreaPx, presentedArea(plan.flatMap(_.repaintRegion), viewport, metrics))

  def overlayModelBuilt(counters: FrameCounters): Unit = counters.add(FrameCounter.OverlayModelBuilds, 1L)

  /** `dirtyRows` is the dirty-region contract of [[RendererPaneContent]]: `None` draws all `total` rows. */
  def rowsPainted(counters: FrameCounters, total: Int, dirtyRows: Option[Set[Int]]): Unit =
    if counters.isEnabled then counters.rows(total, dirtyRows.fold(total)(_.size))

  /** Closes the frame once its surface has flushed, since the bytes a terminal wrote are known only then. The surface
    * is drained even with counting off, so what it gathered then is not charged to the first frame counted.
    */
  def flushed(counters: FrameCounters, surface: RenderSurface): Unit =
    surface.activity.drainInto(counters)
    counters.frameFinished()

  private def presentedArea(region: Option[List[PixelRect]], viewport: ViewportSize, metrics: CellMetrics): Long =
    region.fold(viewport.width.toLong * metrics.charWidth * viewport.height * metrics.lineHeight)(
      _.foldLeft(0L)((area, rect) => area + rect.widthPx.toLong * rect.heightPx)
    )
