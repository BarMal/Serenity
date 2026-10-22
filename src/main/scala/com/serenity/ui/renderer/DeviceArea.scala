package com.serenity.ui.renderer

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.LabelAnchor.isWhole

/** The device pixels that hold a logical rect, clamped to the surface that holds them. */
final private[ui] case class DeviceArea(left: Int, top: Int, width: Int, height: Int):
  def isEmpty: Boolean = width <= 0 || height <= 0

private[ui] object DeviceArea:

  /** A fill at a fractional scale may land a device pixel outside the rect; at a whole scale it lands exactly, which
    * lets neighbouring rects be lifted and put back without touching each other.
    */
  def around(area: PixelRect, scaleX: Double, scaleY: Double, surfaceWidth: Int, surfaceHeight: Int): DeviceArea =
    val margin = if isWhole(scaleX) && isWhole(scaleY) then 0 else 1
    val left   = (math.floor(area.xPx * scaleX).toInt - margin).max(0)
    val top    = (math.floor(area.yPx * scaleY).toInt - margin).max(0)
    val right  = (math.ceil(area.rightPx * scaleX).toInt + margin).min(surfaceWidth)
    val bottom = (math.ceil(area.bottomPx * scaleY).toInt + margin).min(surfaceHeight)
    DeviceArea(left, top, (right - left).max(0), (bottom - top).max(0))
