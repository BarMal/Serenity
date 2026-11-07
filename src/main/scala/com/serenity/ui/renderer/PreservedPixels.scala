package com.serenity.ui.renderer

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

  /** The rows to paint for `dirtyRows`. Where the surface can clip, the kept rows beside the redrawn ones are painted
    * too, under the clip, whether or not the frame copies rows: a redrawn band is cleared, so it has lost the ink its
    * kept neighbour put there (a glyph's tail a pixel past its row), and the clip keeps that repaint off the pixels
    * that are kept. A surface that cannot clip paints `dirtyRows`, as those rows would be painted over pixels that are
    * kept.
    */
  def rowsToPaint(dirtyRows: Map[PaneId, Set[Int]]): Map[PaneId, Set[Int]] =
    if clipping.isEmpty then dirtyRows
    else dirtyRows.view.mapValues(DirtyLineDiff.dilate(_, Int.MaxValue)).toMap

object PreservedPixels:

  /** A frame redrawing only some rows keeps the others; chrome it keeps is left out, as ink beside it is a row's. */
  def of(context: RenderContext, framePlan: Option[FramePlan]): PreservedPixels =
    new PreservedPixels(context, joinedRows(framePlan.fold(List.empty[PixelRect])(_.keptBands)))

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
  private[ui] def near(
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
