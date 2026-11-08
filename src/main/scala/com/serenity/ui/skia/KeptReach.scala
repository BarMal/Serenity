package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.PixelRect

/** The rows a frame keeps, and where a draw that reaches into some stretch of the surface may paint without touching
  * them.
  *
  * What is left of the surface once the kept rows are cut out is worked out once for the frame, across the whole
  * surface, and each draw takes the part of it within its own reach. The kept rows are few: they are joined before a
  * frame hands them in.
  */
final private[skia] class KeptReach(val rects: List[PixelRect]):
  import KeptReach.*

  private val uncovered = AtomicReference(Option.empty[List[PixelRect]])

  /** The rectangles within `reach` that no kept row covers, as disjoint rectangles, and on the part of the surface that
    * `visible` names: a draw has no pixels to paint outside it. `onSweep` is called when the uncovered part of the
    * surface had to be worked out, which is for the first draw of the frame near kept rows.
    */
  def uncoveredWithin(reach: PixelRect, visible: PixelRect, onSweep: () => Unit): List[PixelRect] =
    val whole = uncovered.get.getOrElse {
      onSweep()
      val found = PixelRect.uncoveredWithin(PixelRect(-Unbounded, -Unbounded, 2 * Unbounded, 2 * Unbounded), rects)
      uncovered.set(Some(found))
      found
    }
    PixelRect.joinedVertically(whole.flatMap(PixelRect.clippedTo(reach)).flatMap(PixelRect.clippedTo(visible)))

object KeptReach:

  /** Far enough either side of the surface to hold any draw. */
  private val Unbounded = 1 << 29
