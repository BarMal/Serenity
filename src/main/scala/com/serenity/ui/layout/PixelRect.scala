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
