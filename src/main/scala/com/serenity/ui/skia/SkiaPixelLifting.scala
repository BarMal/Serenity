package com.serenity.ui.skia

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.{DeviceArea, LiftedPixels, PixelLifting}
import org.jetbrains.skia.{Bitmap, ImageInfo, Surface}

/** A rect of a raster `Surface` as it was lifted, in device pixels. */
final private[skia] case class SkiaLiftedPixels(area: DeviceArea, bitmap: Bitmap) extends LiftedPixels

/** Lifts pixels off a raster `Surface` that keeps them from frame to frame, and puts them back. `onRestored` is told
  * where it has written to the surface, so the surface can forget which of its pixels it knows to be clean and note
  * what the frame painted.
  */
final private[skia] class SkiaPixelLifting(
    surface: Surface,
    deviceScaleX: Double,
    deviceScaleY: Double,
    onRestored: DeviceArea => Unit
) extends PixelLifting:

  override def lift(area: PixelRect, spare: Option[LiftedPixels]): LiftedPixels =
    val device = DeviceArea.around(area, deviceScaleX, deviceScaleY, surface.getWidth, surface.getHeight)
    val bitmap = spare
      .collect {
        case SkiaLiftedPixels(held, storage) if held.width == device.width && held.height == device.height => storage
      }
      .getOrElse(allocated(device))
    if !device.isEmpty then require(surface.readPixels(bitmap, device.left, device.top), "cannot lift the pixels")
    SkiaLiftedPixels(device, bitmap)

  private def allocated(device: DeviceArea): Bitmap =
    val bitmap = Bitmap()
    if !device.isEmpty then
      require(
        bitmap.allocPixels(ImageInfo.Companion.makeN32Premul(device.width, device.height)),
        "cannot allocate the pixels to lift"
      )
    bitmap

  override def putBack(lifted: LiftedPixels): Unit = lifted match
    case SkiaLiftedPixels(device, bitmap) if !device.isEmpty =>
      onRestored(device)
      surface.writePixels(bitmap, device.left, device.top)
    case _ => ()
