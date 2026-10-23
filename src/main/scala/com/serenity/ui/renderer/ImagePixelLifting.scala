package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import com.serenity.ui.layout.PixelRect

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

  override def lift(area: PixelRect, spare: Option[LiftedPixels]): LiftedPixels =
    val device = DeviceArea.around(area, deviceScaleX, deviceScaleY, image.getWidth, image.getHeight)
    val argb = spare match
      case Some(DevicePixels(_, _, _, _, storage)) if storage.length == device.width * device.height => storage
      case _ => new Array[Int](device.width * device.height)
    if argb.nonEmpty then
      val _ = image.getRGB(device.left, device.top, device.width, device.height, argb, 0, device.width)
    DevicePixels(device.left, device.top, device.width, device.height, argb)

  override def putBack(lifted: LiftedPixels): Unit = lifted match
    case DevicePixels(left, top, width, height, argb) if width > 0 && height > 0 =>
      onRestored()
      image.setRGB(left, top, width, height, argb, 0, width)
    case _ => ()
