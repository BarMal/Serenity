package com.serenity.ui.layout

/** An axis-aligned rectangle in logical (device-scale independent) pixels. */
final case class PixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int):

  def rightPx: Int = xPx + widthPx

  def bottomPx: Int = yPx + heightPx

  /** Whether this rectangle shares any pixels with `other`. Touching edges (zero-area overlap) do not count. */
  def intersects(other: PixelRect): Boolean =
    xPx < other.rightPx && other.xPx < rightPx && yPx < other.bottomPx && other.yPx < bottomPx

  /** The smallest rectangle covering both, used to fold a set of dirty rows into one repaint region. */
  def union(other: PixelRect): PixelRect =
    val left   = math.min(xPx, other.xPx)
    val top    = math.min(yPx, other.yPx)
    val right  = math.max(rightPx, other.rightPx)
    val bottom = math.max(bottomPx, other.bottomPx)
    PixelRect(left, top, right - left, bottom - top)

object PixelRect:

  def unionOf(rects: Iterable[PixelRect]): Option[PixelRect] =
    rects.reduceOption(_.union(_))

  /** The pixels of `bounds` no rectangle in `holes` covers, as disjoint rectangles: horizontal bands split at every
    * hole edge, with vertically adjacent bands of the same shape merged into one.
    */
  def uncoveredWithin(bounds: PixelRect, holes: List[PixelRect]): List[PixelRect] =
    val clipped = holes.flatMap(clippedTo(bounds))
    val edges =
      (bounds.yPx :: bounds.bottomPx :: clipped.flatMap(hole => List(hole.yPx, hole.bottomPx))).distinct.sorted
    val bands = edges.zip(edges.drop(1)).map { (top, bottom) =>
      val covered = clipped.filter(hole => hole.yPx < bottom && top < hole.bottomPx).map(h => (h.xPx, h.rightPx))
      Band(top, bottom, spansBetween(bounds.xPx, bounds.rightPx, covered.sortBy(_._1)))
    }
    mergeStackedBands(bands).flatMap { band =>
      band.spans.map((left, right) => PixelRect(left, band.top, right - left, band.bottom - band.top))
    }

  final private case class Band(top: Int, bottom: Int, spans: List[(Int, Int)])

  private def clippedTo(bounds: PixelRect)(rect: PixelRect): Option[PixelRect] =
    val left   = rect.xPx.max(bounds.xPx)
    val top    = rect.yPx.max(bounds.yPx)
    val right  = rect.rightPx.min(bounds.rightPx)
    val bottom = rect.bottomPx.min(bounds.bottomPx)
    Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

  private def spansBetween(left: Int, right: Int, coveredByStart: List[(Int, Int)]): List[(Int, Int)] =
    val (cursor, spans) = coveredByStart.foldLeft((left, List.empty[(Int, Int)])) {
      case ((cursor, spans), (coveredLeft, coveredRight)) =>
        val withGap = if coveredLeft > cursor then (cursor, coveredLeft) :: spans else spans
        (cursor.max(coveredRight), withGap)
    }
    (if cursor < right then (cursor, right) :: spans else spans).reverse

  private def mergeStackedBands(bands: List[Band]): List[Band] =
    bands
      .foldLeft(List.empty[Band]) {
        case (previous :: rest, band) if previous.bottom == band.top && previous.spans == band.spans =>
          previous.copy(bottom = band.bottom) :: rest
        case (merged, band) => band :: merged
      }
      .reverse
