package com.serenity.ui.skia

import java.awt.{Color, Font}

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Row blits whose source rectangle overhangs the surface: the part inside must land where it belongs. */
class SkiaRenderSurfaceCopyPixelsSpec extends AnyFlatSpec with Matchers:

  private val width  = 210
  private val height = 130

  final private class Canvas:
    val surface: Surface = Surface.Companion.makeRasterN32Premul(width, height)

    val renderSurface: SkiaRenderSurface =
      SkiaRenderSurface.retaining(
        surface,
        CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10),
        FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
        width,
        height,
        1.0,
        SkiaTextCaches.fresh(),
        StyledFontCache()
      )

    def at(x: Int, y: Int): Int = SkiaPixels.read(surface)(y * width + x)

    def fill(x: Int, y: Int, w: Int, h: Int, colour: Color): Unit =
      renderSurface.pixels.fillPixelRect(x, y, w, h, RenderColor.fromAwt(colour))

    def copy(from: PixelRect, dyPx: Int): Unit =
      renderSurface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(from, dyPx)
      renderSurface.flush()

  "copyPixels" should "move a band whose source starts left of the surface without shifting it sideways" in {
    val canvas = new Canvas
    canvas.fill(0, 0, 1, 10, Color.RED)
    canvas.fill(5, 0, 1, 10, Color.GREEN)

    canvas.copy(PixelRect(-1, 0, width + 1, 10), 20)

    canvas.at(0, 25) shouldBe Color.RED.getRGB
    canvas.at(5, 25) shouldBe Color.GREEN.getRGB
    canvas.at(1, 25) shouldBe 0
    canvas.at(4, 25) shouldBe 0
  }

  it should "move a band whose source starts above the surface without shifting it vertically" in {
    val canvas = new Canvas
    canvas.fill(0, 0, width, 1, Color.RED)
    canvas.fill(0, 4, width, 1, Color.GREEN)

    canvas.copy(PixelRect(0, -2, width, 10), 20)

    canvas.at(5, 20) shouldBe Color.RED.getRGB
    canvas.at(5, 24) shouldBe Color.GREEN.getRGB
    canvas.at(5, 21) shouldBe 0
  }
