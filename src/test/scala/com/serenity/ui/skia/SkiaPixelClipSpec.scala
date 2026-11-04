package com.serenity.ui.skia

import java.awt.{Color, Font}

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaPixelClipSpec extends AnyFlatSpec with Matchers:

  private val width  = 210
  private val height = 130

  "withPixelClip" should "keep fills inside the pixel rectangle, between cells, and restore the clip afterwards" in {
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    val renderSurface = SkiaRenderSurface.retaining(
      surface,
      CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10),
      FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
      width,
      height,
      1.0,
      SkiaTextCaches.fresh(),
      StyledFontCache()
    )
    renderSurface.clearViewport(RenderColor.Black)
    renderSurface.panelOutlines.getOrElse(fail("expected rect clipping")).withPixelClip(13, 5, 20, 10) {
      renderSurface.pixels.fillPixelRect(0, 0, width, height, RenderColor.fromAwt(Color.RED))
    }
    renderSurface.pixels.fillPixelRect(100, 100, 4, 4, RenderColor.fromAwt(Color.BLUE))
    renderSurface.flush()
    val pixels                  = SkiaPixels.read(surface)
    def at(x: Int, y: Int): Int = pixels(y * width + x)

    at(13, 5) shouldBe Color.RED.getRGB
    at(32, 14) shouldBe Color.RED.getRGB
    at(12, 8) shouldBe Color.BLACK.getRGB
    at(33, 8) shouldBe Color.BLACK.getRGB
    at(20, 4) shouldBe Color.BLACK.getRGB
    at(20, 15) shouldBe Color.BLACK.getRGB
    at(101, 101) shouldBe Color.BLUE.getRGB
  }
