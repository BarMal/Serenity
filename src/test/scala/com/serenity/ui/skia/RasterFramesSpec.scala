package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.lang.management.ManagementFactory

import com.serenity.SkiaPixels
import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.{ColorAlphaType, ColorSpace, ColorType, ImageInfo, Paint, Rect, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RasterFramesSpec extends AnyFlatSpec with Matchers:

  private val width  = 301
  private val height = 157

  private def surfaceOfRandomRects(): Surface =
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    val random  = new scala.util.Random(2380L)
    val paint   = Paint()
    surface.getCanvas.clear(0xff000000)
    (1 to 300).foreach { _ =>
      paint.setColor(0xff000000 | random.nextInt(0x1000000))
      val _ = surface.getCanvas.drawRect(
        Rect.Companion.makeXYWH(
          random.nextInt(width).toFloat,
          random.nextInt(height).toFloat,
          1f + random.nextInt(60),
          1f + random.nextInt(40)
        ),
        paint
      )
    }
    surface

  private def untouched(): BufferedImage =
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val data  = image.getRaster.getDataBuffer.asInstanceOf[java.awt.image.DataBufferInt].getData
    java.util.Arrays.fill(data, 0x123456)
    image

  private def inside(area: DeviceArea, x: Int, y: Int): Boolean =
    x >= area.left && x < area.left + area.width && y >= area.top && y < area.top + area.height

  private val areas = List(
    "the whole surface"       -> DeviceArea(0, 0, width, height),
    "an interior block"       -> DeviceArea(17, 23, 120, 61),
    "one row"                 -> DeviceArea(0, 90, width, 1),
    "one column"              -> DeviceArea(150, 0, 1, height),
    "a narrow tall strip"     -> DeviceArea(280, 5, 21, 140),
    "the bottom right corner" -> DeviceArea(width - 33, height - 19, 33, 19),
    "a wide short strip"      -> DeviceArea(3, 40, width - 6, 7)
  )

  for (label, area) <- areas do
    s"Copying $label out of a surface" should "set those pixels to the surface's and leave every other pixel alone" in {
      val surface  = surfaceOfRandomRects()
      val expected = SkiaPixels.read(surface)
      val image    = untouched()
      RasterFrames.copyInto(surface, image, area)
      for y <- 0 until height; x <- 0 until width do
        val shown = image.getRGB(x, y) & 0xffffff
        val want  = if inside(area, x, y) then expected(y * width + x) & 0xffffff else 0x123456
        withClue(s"pixel $x,$y: ")(shown shouldBe want)
    }

  for (label, colorType) <- List("BGRA" -> ColorType.BGRA_8888, "RGBA" -> ColorType.RGBA_8888) do
    s"Copying out of a $label surface" should "show the colours that were painted, whatever the byte order of the surface" in {
      val info    = ImageInfo(width, height, colorType, ColorAlphaType.PREMUL, ColorSpace.Companion.getSRGB())
      val surface = Surface.Companion.makeRaster(info)
      surface.getCanvas.clear(0xff1e3c5a)
      val image = untouched()
      RasterFrames.copyInto(surface, image, DeviceArea(0, 0, width, height))
      val shown = for y <- 0 until height; x <- 0 until width yield image.getRGB(x, y) & 0xffffff
      shown.distinct shouldBe Vector(0x1e3c5a)
    }

  "Copying out of a surface" should "allocate little more than the pixels it copies" in {
    val surface = surfaceOfRandomRects()
    val image   = untouched()
    val area    = DeviceArea(0, 0, width, height)
    val bean    = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val thread  = Thread.currentThread().threadId()
    (1 to 50).foreach(_ => RasterFrames.copyInto(surface, image, area))
    val before = bean.getThreadAllocatedBytes(thread)
    (1 to 20).foreach(_ => RasterFrames.copyInto(surface, image, area))
    val perCopy = (bean.getThreadAllocatedBytes(thread) - before) / 20
    perCopy should be <= (width * height * 4L * 5 / 4 + 32 * 1024)
  }

  it should "read in chunks that stay well under the size of a humongous allocation" in
    List(301, 1920, 3840, 5120, 8192).foreach { surfaceWidth =>
      List(1, surfaceWidth / 4, surfaceWidth / 2, surfaceWidth).foreach { areaWidth =>
        val rows = RasterFrames.rowsPerRead(areaWidth, surfaceWidth)
        rows should be >= 1
        ((rows - 1).toLong * surfaceWidth * 4 + areaWidth * 4L) should be <= 256L * 1024
      }
    }

  it should "read a narrow area a row at a time" in {
    RasterFrames.rowsPerRead(10, 3840) shouldBe 1
  }
