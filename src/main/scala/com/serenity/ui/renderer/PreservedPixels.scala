package com.serenity.ui.renderer

import java.awt.Shape
import java.awt.geom.{Area, Rectangle2D}

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{DirtyLineDiff, PixelRect}

/** Keeps a frame's painting off the pixels it holds correct (#2481). A redrawn row's antialiased ink reaches a little
  * past its band; where the row beside it is kept or copied, that ink would land on pixels that already carry it, and a
  * partly covered edge composited twice is not what a frame drawn from scratch shows.
  */
final class PreservedPixels private (context: RenderContext, kept: List[PixelRect], copiesRows: Boolean):

  private def clipping: Option[PanelOutlineDrawing] =
    context.surface.panelOutlines.filter(_.keepsOffPixels).filter(_ => kept.nonEmpty && context.surface.addressesPixels)

  def apply(paint: => Unit): Unit = clipping.fold(paint)(_.withoutPixels(kept)(paint))

  /** The rows to paint for `dirtyRows`. In a frame that copies rows, the kept rows beside the redrawn ones are painted
    * too, under the clip: a copied row carries its neighbour's ink, but the band a redrawn row cleared has lost the ink
    * its kept neighbour put there. A frame that copies nothing keeps its rows where they were, and paints `dirtyRows`;
    * so does a surface that cannot clip, as those rows would be painted over pixels that are kept.
    */
  def rowsToPaint(dirtyRows: Map[PaneId, Set[Int]]): Map[PaneId, Set[Int]] =
    if clipping.isEmpty || !copiesRows then dirtyRows
    else dirtyRows.view.mapValues(DirtyLineDiff.dilate(_, Int.MaxValue)).toMap

object PreservedPixels:

  /** A frame redrawing only some rows keeps the others; chrome it keeps is left out, as ink beside it is a row's. */
  def of(context: RenderContext, framePlan: Option[FramePlan]): PreservedPixels =
    new PreservedPixels(
      context,
      joinedRows(framePlan.fold(List.empty[PixelRect])(_.keptRows)),
      framePlan.exists(_.blits.nonEmpty)
    )

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

  /** The kept rects within `margin` of the box a draw is bounded to: where its ink can reach one. */
  private[renderer] def near(
    kept: List[PixelRect],
    left: Int,
    top: Int,
    right: Int,
    bottom: Int,
    margin: Int
  ): List[PixelRect] =
    kept.filter(rect =>
      rect.xPx < right + margin && rect.xPx + rect.widthPx > left - margin && rect.yPx < bottom + margin &&
        rect.yPx + rect.heightPx > top - margin
    )

  /** `box` but `kept`: the part of a draw's reach it may ink. The box stays small, as a clip shape is flattened into
    * rows, which a shape the size of the whole plane makes a million of.
    */
  private[renderer] def allowed(box: PixelRect, kept: List[PixelRect]): Shape =
    val area = new Area(new Rectangle2D.Double(box.xPx, box.yPx, box.widthPx, box.heightPx))
    kept.foreach(rect =>
      area.subtract(new Area(new Rectangle2D.Double(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)))
    )
    area
