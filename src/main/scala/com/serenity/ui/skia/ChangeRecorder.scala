package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.DeviceArea

/** What one frame did to the retained surface: the rows it moved, and where it painted. Paint is recorded generously, a
  * pixel wide of what it touches, because a pixel it missed would be left stale in a pooled image that follows the
  * surface by moving rows instead of copying them. Paint recorded before a move is moved by it, which this does not
  * follow, so such a frame reports nothing about its paint.
  */
final private[skia] class ChangeRecorder(
    deviceScaleX: Double,
    deviceScaleY: Double,
    surfaceWidthPx: Int,
    surfaceHeightPx: Int,
    val phases: PresentPhases
):
  import ChangeRecorder.*

  private val recorded = new AtomicReference(Recorded.Empty)

  def moved(area: DeviceArea, dyPx: Int): Unit =
    val move = RasterCopyPlan.Move(area, dyPx)
    val _ = recorded.updateAndGet(frame =>
      frame.copy(
        moves = frame.moves :+ move,
        paint = frame.paint ++ RasterCopyPlan.carried(frame.paint.toList, List(move))
      )
    )

  /** A rect of logical pixels, with `growPx` logical pixels more on every side. */
  def painted(xPx: Double, yPx: Double, widthPx: Double, heightPx: Double, growPx: Double = 0.0): Unit =
    painted(
      DeviceArea.around(
        com.serenity.ui.layout.PixelRect(
          math.floor(xPx - growPx).toInt,
          math.floor(yPx - growPx).toInt,
          math.ceil(widthPx + 2 * growPx).toInt + 1,
          math.ceil(heightPx + 2 * growPx).toInt + 1
        ),
        deviceScaleX,
        deviceScaleY,
        surfaceWidthPx,
        surfaceHeightPx
      )
    )

  /** A cell that continues the area recorded just before it, as a column of cells or a row of them does, extends it
    * instead of being a record of its own, so a frame that paints a thousand cells records a handful of areas.
    */
  def painted(area: DeviceArea): Unit =
    val _ = recorded.updateAndGet(frame =>
      frame.paint.lastOption.flatMap(joinedWith(_, area)) match
        case Some(joined) => frame.copy(paint = frame.paint.dropRight(1) :+ joined)
        case None         => frame.copy(paint = frame.paint :+ area)
    )

  def moves: List[RasterCopyPlan.Move] = recorded.get().moves.toList

  /** Merged on request, which is only worth doing for a frame that moved rows. */
  def paint: List[DeviceArea] = RasterCopyPlan.disjoint(paintRecords)

  /** Every area painted, as it was recorded: overlapping, and the caller's to merge with whatever else it has. */
  def paintRecords: List[DeviceArea] = recorded.get().paint.toList

object ChangeRecorder:

  private def joinedWith(last: DeviceArea, next: DeviceArea): Option[DeviceArea] =
    val sameColumn = last.left == next.left && last.width == next.width
    val sameRow    = last.top == next.top && last.height == next.height
    if sameColumn && next.top >= last.top && next.top <= last.top + last.height then
      Some(last.copy(height = (last.top + last.height).max(next.top + next.height) - last.top))
    else if sameRow && next.left >= last.left && next.left <= last.left + last.width then
      Some(last.copy(width = (last.left + last.width).max(next.left + next.width) - last.left))
    else None

  final private case class Recorded(moves: Vector[RasterCopyPlan.Move], paint: Vector[DeviceArea])

  private object Recorded:
    val Empty: Recorded = Recorded(Vector.empty, Vector.empty)
