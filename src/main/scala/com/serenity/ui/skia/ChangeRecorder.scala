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
    surfaceHeightPx: Int
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

  /** A line of text, across the surface: the line and a quarter of one more above and below, since the renderer already
    * treats the ink of a row as staying within it (a damaged row is redrawn alone).
    */
  def paintedText(yPx: Double, lineHeightPx: Int): Unit =
    painted(0.0, yPx - lineHeightPx / 4.0, surfaceWidthPx / deviceScaleX, lineHeightPx * 1.5)

  def painted(area: DeviceArea): Unit =
    val _ = recorded.updateAndGet(frame => frame.copy(paint = frame.paint :+ area))

  def moves: List[RasterCopyPlan.Move] = recorded.get().moves.toList

  /** Merged on request, which is only worth doing for a frame that moved rows. */
  def paint: List[DeviceArea] = RasterCopyPlan.disjoint(recorded.get().paint.toList)

object ChangeRecorder:

  final private case class Recorded(moves: Vector[RasterCopyPlan.Move], paint: Vector[DeviceArea])

  private object Recorded:
    val Empty: Recorded = Recorded(Vector.empty, Vector.empty)
