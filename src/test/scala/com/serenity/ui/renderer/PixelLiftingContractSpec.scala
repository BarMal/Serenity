package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What lifting pixels off a surface that keeps them promises, whichever backend draws it: what was lifted comes back
  * exactly, at a whole device scale and at a fractional one (where a fill of the same area can land a device pixel
  * outside it), and nothing outside the lifted rect is touched. Every assertion compares a backend's pixels with its
  * own earlier pixels, so no rasteriser has to agree with another.
  */
abstract class PixelLiftingBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width   = 210
  private val height  = 130
  private val black   = RenderColor.fromRgba(0, 0, 0)
  private val white   = RenderColor.fromRgba(255, 255, 255)
  private val blue    = RenderColor.fromRgba(0, 0, 200)
  private val magenta = RenderColor.fromRgba(255, 0, 255)

  private def painted(scale: Double): (FramePixels, RenderSurface) =
    val image = frames.newFrame(
      Java2DRenderSurface.deviceImageDimension(width, scale),
      Java2DRenderSurface.deviceImageDimension(height, scale)
    )
    val surface = frames.surfaceOver(image, metrics, font, width, height, scale, persists = true, RenderCaches.create())
    surface.clearViewport(black)
    surface.setForegroundColor(white)
    surface.setBackgroundColor(black)
    (0 until 4).foreach { row =>
      surface.text.drawRunPx(7f, 13 + row * 26, 190f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMMMMMM")
    }
    surface.pixels.fillPixelRect(100, 40, 30, 20, blue)
    (image, surface)

  private def liftingOf(surface: RenderSurface): PixelLifting =
    surface.pixelLifting.getOrElse(fail("a surface that keeps its pixels should offer to lift them"))

  private def over(surface: RenderSurface, area: PixelRect): Unit =
    surface.pixels.fillPixelRect(area.xPx, area.yPx, area.widthPx, area.heightPx, magenta)

  private val scales = List(1.0, 1.5, 2.0)

  "A surface that keeps its pixels" should "offer to lift them" in {
    val (_, surface) = painted(1.0)

    surface.pixelLifting shouldBe defined
  }

  it should "give back exactly what was lifted once something was drawn over it, at every device scale" in
    scales.foreach { scale =>
      val (image, surface) = painted(scale)
      val before           = image.rgbPixels
      val area             = PixelRect(20, 20, 61, 35)

      val lifted = liftingOf(surface).lift(area, None)
      over(surface, area)
      withClue(s"at scale $scale the fill should show: ")(image.rgbPixels should not be before)
      liftingOf(surface).putBack(lifted)

      withClue(s"at scale $scale: ")(image.rgbPixels shouldBe before)
    }

  it should "touch nothing outside the lifted rect when it puts the pixels back" in {
    val (image, surface) = painted(1.0)
    val lifted           = liftingOf(surface).lift(PixelRect(20, 20, 40, 30), None)
    over(surface, PixelRect(150, 90, 20, 20))
    val elsewhere = image.rgbAt(155, 95)

    liftingOf(surface).putBack(lifted)

    image.rgbAt(155, 95) shouldBe elsewhere
  }

  it should "lift again into storage handed back, whatever size the new rect is" in
    scales.foreach { scale =>
      val (image, surface) = painted(scale)
      val before           = image.rgbPixels
      val lifting          = liftingOf(surface)

      val first = lifting.lift(PixelRect(20, 20, 61, 35), None)
      lifting.putBack(first)
      val sameSize = lifting.lift(PixelRect(90, 70, 61, 35), Some(first))
      over(surface, PixelRect(90, 70, 61, 35))
      lifting.putBack(sameSize)
      val otherSize = lifting.lift(PixelRect(10, 100, 33, 21), Some(sameSize))
      over(surface, PixelRect(10, 100, 33, 21))
      lifting.putBack(otherSize)

      withClue(s"at scale $scale: ")(image.rgbPixels shouldBe before)
    }

class Java2DPixelLiftingSpec extends PixelLiftingBehaviour:
  override protected def frames: FrameBackend = Java2DFrames

class SkiaPixelLiftingSpec extends PixelLiftingBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
