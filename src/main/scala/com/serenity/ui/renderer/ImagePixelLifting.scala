package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.LabelAnchor.isWhole

/** A rect of a backing image as it was lifted, in device pixels. */
final private[renderer] case class DevicePixels(left: Int, top: Int, width: Int, height: Int, argb: Array[Int])
    extends LiftedPixels

/** Lifts pixels off a backing image and puts them back. `onRestored` is told when it has written to the image, so the
  * surface can forget which of its pixels it knows to be clean.
  */
final private[renderer] class ImagePixelLifting(
    image: BufferedImage,
    deviceScaleX: Double,
    deviceScaleY: Double,
    onRestored: () => Unit
) extends PixelLifting:

  /** A fill at a fractional scale may land a device pixel outside the rect; at a whole scale it lands exactly, which
    * lets neighbouring rects be lifted and put back without touching each other.
    */
  private val margin: Int = if isWhole(deviceScaleX) && isWhole(deviceScaleY) then 0 else 1

  override def lift(area: PixelRect, spare: Option[LiftedPixels]): LiftedPixels =
    val left   = (math.floor(area.xPx * deviceScaleX).toInt - margin).max(0)
    val top    = (math.floor(area.yPx * deviceScaleY).toInt - margin).max(0)
    val right  = (math.ceil(area.rightPx * deviceScaleX).toInt + margin).min(image.getWidth)
    val bottom = (math.ceil(area.bottomPx * deviceScaleY).toInt + margin).min(image.getHeight)
    val width  = (right - left).max(0)
    val height = (bottom - top).max(0)
    val argb = spare match
      case Some(DevicePixels(_, _, _, _, storage)) if storage.length == width * height => storage
      case _                                                                           => new Array[Int](width * height)
    if argb.nonEmpty then
      val _ = image.getRGB(left, top, width, height, argb, 0, width)
    DevicePixels(left, top, width, height, argb)

  override def putBack(lifted: LiftedPixels): Unit = lifted match
    case DevicePixels(left, top, width, height, argb) if width > 0 && height > 0 =>
      onRestored()
      image.setRGB(left, top, width, height, argb, 0, width)
    case _ => ()
