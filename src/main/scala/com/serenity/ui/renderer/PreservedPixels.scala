package com.serenity.ui.renderer

import java.awt.Shape
import java.awt.geom.{Area, Rectangle2D}

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{DirtyLineDiff, PixelRect}

/** Keeps a frame's painting off the pixels it holds correct (#2481). A redrawn row's antialiased ink reaches a little
  * past its band; where the row beside it is kept or copied, that ink would land on pixels that already carry it, and a
  * partly covered edge composited twice is not what a frame drawn from scratch shows.
  */
final class PreservedPixels private (context: RenderContext, kept: List[PixelRect]):

  private def clipping: Option[PanelOutlineDrawing] =
    context.surface.panelOutlines.filter(_.keepsOffPixels).filter(_ => kept.nonEmpty && context.surface.addressesPixels)

  def apply(paint: => Unit): Unit = clipping.fold(paint)(_.withoutPixels(kept)(paint))

  /** The rows to paint for `dirtyRows`: under the clip, the kept rows beside them too, whose ink still reaches the band
    * that was cleared. Without a clip they would paint over pixels that are kept, so only the dirty rows are painted.
    */
  def rowsToPaint(dirtyRows: Map[PaneId, Set[Int]]): Map[PaneId, Set[Int]] =
    if clipping.isEmpty then dirtyRows
    else dirtyRows.view.mapValues(DirtyLineDiff.dilate(_, Int.MaxValue)).toMap

object PreservedPixels:

  /** A frame redrawing only some rows keeps the others; chrome it keeps is left out, as ink beside it is a row's. */
  def of(context: RenderContext, framePlan: Option[FramePlan]): PreservedPixels =
    new PreservedPixels(context, joinedRows(framePlan.fold(List.empty[PixelRect])(_.keptRows)))

  /** Kept rows are one rect each and mostly adjacent, so a clip of them all is the same few rects, not one per row. */
  private[renderer] def joinedRows(rects: List[PixelRect]): List[PixelRect] =
    rects
      .groupBy(rect => (rect.xPx, rect.widthPx))
      .values
      .flatMap(_.sortBy(_.yPx).foldLeft(List.empty[PixelRect]) {
        case (above :: earlier, rect) if above.yPx + above.heightPx >= rect.yPx =>
          above.copy(heightPx = math.max(above.heightPx, rect.yPx + rect.heightPx - above.yPx)) :: earlier
        case (joined, rect) => rect :: joined
      })
      .toList

  /** Everything inside `widthPx` x `heightPx` but `kept`. */
  private[renderer] def allowed(widthPx: Int, heightPx: Int, kept: List[PixelRect]): Shape =
    val area = new Area(new Rectangle2D.Double(0, 0, widthPx, heightPx))
    kept.foreach(rect =>
      area.subtract(new Area(new Rectangle2D.Double(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)))
    )
    area
