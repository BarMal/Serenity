package com.serenity.ui.renderer

import com.serenity.ui.layout.PixelRect

/** Where a layer image sits in the window's device pixels, and how many device pixels it covers. Rounded outwards so
  * that a layer painted for logical `bounds` and composited back through the same numbers lands on exactly the pixels
  * it was drawn on.
  */
final case class DeviceOrigin(xPx: Int, yPx: Int)

object DeviceOrigin:
  val Zero: DeviceOrigin = DeviceOrigin(0, 0)

  def of(bounds: PixelRect, scaleX: Double, scaleY: Double): DeviceOrigin =
    DeviceOrigin(math.floor(bounds.xPx * scaleX).toInt, math.floor(bounds.yPx * scaleY).toInt)

  def widthOf(bounds: PixelRect, scaleX: Double): Int =
    (math.ceil(bounds.rightPx * scaleX).toInt - math.floor(bounds.xPx * scaleX).toInt).max(1)

  def heightOf(bounds: PixelRect, scaleY: Double): Int =
    (math.ceil(bounds.bottomPx * scaleY).toInt - math.floor(bounds.yPx * scaleY).toInt).max(1)
