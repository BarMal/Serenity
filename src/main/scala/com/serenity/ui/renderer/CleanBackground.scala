package com.serenity.ui.renderer

import java.awt.Color

import scala.annotation.tailrec

import com.serenity.ui.layout.PixelRect

/** The opaque colour a clear painted, and the rectangles it painted it into.
  *
  * A run paints its background before its glyphs, which repeats the clear for every run whose background is the colour
  * the viewport was just cleared to. Over pixels nothing has touched since, that second fill changes nothing and can be
  * skipped; which pixels have been touched is [[PaintedPixels]]'s to say.
  *
  * Asked once per run per frame, so [[covers]] takes the run's edges as plain ints and allocates nothing.
  */
final private[renderer] case class CleanBackground(color: Color, cleared: List[PixelRect]):

  def covers(left: Int, top: Int, right: Int, bottom: Int, background: Color): Boolean =
    background == color && clearedAround(cleared, left, top, right, bottom)

  @tailrec
  private def clearedAround(rects: List[PixelRect], left: Int, top: Int, right: Int, bottom: Int): Boolean =
    rects match
      case Nil => false
      case rect :: rest =>
        (rect.xPx <= left && rect.yPx <= top && right <= rect.rightPx && bottom <= rect.bottomPx) ||
        clearedAround(rest, left, top, right, bottom)

private[renderer] object CleanBackground:

  /** Only an opaque colour replaces what is under it; a translucent one blends, so painting it twice differs. */
  def cleared(color: Color, rects: List[PixelRect]): Option[CleanBackground] =
    Option.when(color.getAlpha == 255)(CleanBackground(color, rects))
