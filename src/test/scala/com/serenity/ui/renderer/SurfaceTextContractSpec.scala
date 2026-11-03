package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.CellMetrics
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What text drawing promises whichever backend draws it: a run clipped to its bounds stays inside them, a fallback
  * family draws as a surface whose font is that family would, and a string takes the cells its characters take. The
  * scenes are shared; each assertion compares what one backend draws with what the same backend draws another way, or
  * counts ink, so no rasteriser has to agree with another.
  */
abstract class SurfaceTextContractBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width   = 210
  private val height  = 130
  private val black   = RenderColor.fromAwt(Color.BLACK)
  private val white   = RenderColor.fromAwt(Color.WHITE)

  private def blank(base: Font = font): (FramePixels, RenderSurface) =
    val image   = frames.newFrame(width, height)
    val surface = frames.surfaceOver(image, metrics, base, width, height, 1.0, persists = false, RenderCaches.create())
    surface.clearViewport(black)
    surface.setForegroundColor(white)
    surface.setBackgroundColor(black)
    (image, surface)

  private def inkIn(image: FramePixels, left: Int, right: Int, top: Int, bottom: Int): Int =
    (for
      y <- top until bottom
      x <- left until right
      if image.rgbAt(x, y) != Color.BLACK.getRGB
    yield 1).size

  private def pixelsOf(image: FramePixels, left: Int, right: Int, top: Int, bottom: Int): Vector[Int] =
    (for
      y <- top until bottom
      x <- left until right
    yield image.rgbAt(x, y)).toVector

  private def emojiFont: Font =
    val family = FontLoader.emojiFontFamily.getOrElse(fail("the bundled emoji font did not load"))
    new Font(family, Font.PLAIN, 12)

  "A shaped run asked to clip to its run" should "paint no glyph outside the run's bounds" in {
    val (image, surface) = blank()

    surface.text.drawShapedRunPx(
      14f,
      26,
      14f,
      metrics.lineHeight,
      metrics.ascent,
      "MMMMMMMMMM",
      0,
      10,
      clipGlyphToRun = true
    )

    inkIn(image, 14, 28, 26, 39) should be > 0
    inkIn(image, 28, width, 26, 39) shouldBe 0
  }

  "A shaped run not asked to clip" should "paint its glyphs past the run, so the clip above is what stops them" in {
    val (image, surface) = blank()

    surface.text.drawShapedRunPx(14f, 26, 14f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMM", 0, 10)

    inkIn(image, 28, width, 26, 39) should be > 0
  }

  "Drawing through a fallback family" should "draw as a surface whose own font is that family draws, not as the surface font" in {
    val (viaFallback, surface) = blank()
    surface.text.withFallbackFamily(emojiFont.getFamily) {
      surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "Wiq")
    }

    val (direct, ownFont) = blank(emojiFont)
    ownFont.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "Wiq")

    val (plain, unchanged) = blank()
    unchanged.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "Wiq")

    val via = pixelsOf(viaFallback, 14, 70, 26, 39)
    via shouldBe pixelsOf(direct, 14, 70, 26, 39)
    via should not be pixelsOf(plain, 14, 70, 26, 39)
  }

  it should "leave the surface's own font in force afterwards" in {
    val (image, surface) = blank()
    surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "Wiq")
    surface.text.withFallbackFamily(emojiFont.getFamily) {
      surface.text.drawRunPx(14f, 52, 56f, metrics.lineHeight, metrics.ascent, "Wiq")
    }
    surface.text.drawRunPx(14f, 78, 56f, metrics.lineHeight, metrics.ascent, "Wiq")

    inkIn(image, 14, 70, 26, 39) should be > 0
    pixelsOf(image, 14, 70, 78, 91) shouldBe pixelsOf(image, 14, 70, 26, 39)
  }

  "A string with a wide character" should "have its background cover every cell it takes, and no more" in {
    val blue             = RenderColor.fromAwt(new Color(0, 0, 200))
    val (image, surface) = blank()
    surface.setBackgroundColor(blue)
    surface.setForegroundColor(RenderColor.fromRgba(0, 0, 0, 0))

    surface.putString(2, 1, "\u4e2da")

    val left   = metrics.toPixelX(2)
    val top    = metrics.toPixelY(1)
    val right  = left + 3 * metrics.charWidth
    val inside = pixelsOf(image, left, right, top, top + metrics.lineHeight)
    inside.distinct shouldBe Vector(0xff0000c8)
    pixelsOf(image, right, right + 1, top, top + metrics.lineHeight).distinct shouldBe Vector(Color.BLACK.getRGB)
  }

  it should "put a narrow character after it on the cell after the wide one's two" in {
    val (image, surface) = blank()

    surface.putString(2, 1, "中a")
    surface.putString(4, 3, "a")

    val cell = metrics.charWidth
    val wide =
      pixelsOf(image, metrics.toPixelX(4), metrics.toPixelX(4) + cell, metrics.toPixelY(1), metrics.toPixelY(2))
    val lone =
      pixelsOf(image, metrics.toPixelX(4), metrics.toPixelX(4) + cell, metrics.toPixelY(3), metrics.toPixelY(4))
    lone.exists(_ != Color.BLACK.getRGB) shouldBe true
    wide shouldBe lone
  }

class Java2DSurfaceTextContractSpec extends SurfaceTextContractBehaviour:
  override protected def frames: FrameBackend = Java2DFrames

class SkiaSurfaceTextContractSpec extends SurfaceTextContractBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
