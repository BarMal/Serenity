package com.serenity.ui.skia

import java.awt.image.BufferedImage

import org.jetbrains.skia.{Bitmap, ColorAlphaType, ColorType, ImageInfo, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skia's native 32-bit order is BGRA on Linux and Windows and RGBA on macOS, so nothing that moves pixels between an
  * AWT image and Skia may lean on it. These specs hold Skia to both orders on every platform.
  */
class SkiaPixelOrderSpec extends AnyFlatSpec with Matchers:

  private val Red = 0xffff0000

  private val Orders = List(ColorType.BGRA_8888, ColorType.RGBA_8888)

  private def surfaceOf(order: ColorType): Surface =
    Surface.Companion.makeRaster(new ImageInfo(4, 4, order, ColorAlphaType.PREMUL))

  private def argbAt(surface: Surface, x: Int, y: Int): Int =
    val bitmap = Bitmap()
    val info   = new ImageInfo(4, 4, ColorType.BGRA_8888, ColorAlphaType.PREMUL)
    val read   = bitmap.allocPixels(info) && surface.readPixels(bitmap, 0, 0)
    read shouldBe true
    val bytes = bitmap.readPixels(info, 16, 0, 0)
    val at    = (y * 4 + x) * 4
    ((bytes(at + 3) & 0xff) << 24) | ((bytes(at + 2) & 0xff) << 16) | ((bytes(at + 1) & 0xff) << 8) | (bytes(at) & 0xff)

  "SkiaImageCache" should "convert an image into an explicitly BGRA Skia image" in {
    val source = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
    source.setRGB(0, 0, Red)
    new SkiaImageCache().imageOf(source).getImageInfo.getColorType shouldBe ColorType.BGRA_8888
  }

  it should "draw a red pixel red onto a surface of either order" in {
    val source = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
    source.setRGB(0, 0, Red)
    val image = new SkiaImageCache().imageOf(source)
    Orders.foreach { order =>
      val surface = surfaceOf(order)
      surface.getCanvas.drawImage(image, 0f, 0f)
      withClue(s"$order: ")(argbAt(surface, 0, 0) shouldBe Red)
    }
  }

  "RasterFrames.copyWhole" should "copy a red pixel into the image as red from a surface of either order" in
    Orders.foreach { order =>
      val surface = surfaceOf(order)
      surface.getCanvas.clear(Red)
      val image = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      RasterFrames.copyWhole(surface, image)
      withClue(s"$order: ")(image.getRGB(0, 0) shouldBe Red)
    }
