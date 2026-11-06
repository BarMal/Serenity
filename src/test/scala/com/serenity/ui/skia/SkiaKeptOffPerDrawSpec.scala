package com.serenity.ui.skia

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.RenderSurface
import com.serenity.{FramePixels, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2481: painting kept off the pixels a frame holds correct. Skia keeps a draw off them per draw, as Java2D does, and
  * not with a clip on the canvas for the whole painting, which would send every antialiased edge through the clip-mask
  * path. A draw that can reach a kept rect leaves it as it was; a draw out of reach of all of them draws as it would
  * with nothing kept; and the kept rects are in the pixels the frame paints in, whatever the canvas has been moved by.
  */
class SkiaKeptOffPerDrawSpec extends AnyFlatSpec with Matchers:

  private val metrics  = CellMetrics(charWidth = 7, lineHeight = 14, ascent = 11)
  private val font     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width    = 120
  private val height   = 80
  private val page     = RenderColor.fromAwt(Color.WHITE)
  private val sentinel = RenderColor.fromAwt(new Color(0x12, 0x34, 0x56))
  private val ink      = RenderColor.fromAwt(new Color(0xc0, 0x20, 0x20))
  private val kept     = PixelRect(40, 28, 40, 14)

  private val sentinelRgb = 0x123456
  private val inkRgb      = 0xc02020

  /** A frame where `kept` holds the sentinel, then `paint` runs with `keptRects` kept off. */
  private def drawn(keptRects: List[PixelRect])(paint: RenderSurface => Unit): FramePixels =
    val image = SkiaFrames.newFrame(width, height)
    val surface =
      SkiaFrames.surfaceOver(image, metrics, font, width, height, 1.0, persists = true, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(page)
    surface.pixels.fillPixelRect(kept.xPx, kept.yPx, kept.widthPx, kept.heightPx, sentinel)
    outlines.withoutPixels(keptRects)(paint(surface))
    surface.flush()
    image

  private def rgb(frame: FramePixels, x: Int, y: Int): Int = frame.rgbAt(x, y) & 0xffffff

  private def keptPixels(frame: FramePixels): Seq[Int] =
    for
      x <- kept.xPx until kept.xPx + kept.widthPx
      y <- kept.yPx until kept.yPx + kept.heightPx
    yield rgb(frame, x, y)

  "A draw that reaches a kept rect" should "leave the kept pixels as they were" in {
    val frame = drawn(List(kept)) { surface =>
      surface.setBackgroundColor(ink)
      surface.fillRect(3, 1, 12, 3, ' ')
    }

    keptPixels(frame).distinct shouldBe Seq(sentinelRgb)
    rgb(frame, 25, 20) shouldBe inkRgb
  }

  it should "leave them as they were when it is text over them" in {
    val frame = drawn(List(kept)) { surface =>
      surface.setBackgroundColor(page)
      surface.setForegroundColor(ink)
      surface.putString(5, 2, "abcdefghijklmnopqrst")
    }

    keptPixels(frame).distinct shouldBe Seq(sentinelRgb)
  }

  "A draw out of reach of every kept rect" should "draw as it does with nothing kept" in {
    def paint(surface: RenderSurface): Unit =
      surface.setBackgroundColor(ink)
      surface.fillRect(0, 0, 3, 1, ' ')
    val withKept    = drawn(List(kept))(paint)
    val withoutKept = drawn(Nil)(paint)

    val differing = for
      x <- 0 until width
      y <- 0 until height
      if rgb(withKept, x, y) != rgb(withoutKept, x, y)
    yield (x, y)
    differing shouldBe empty
    rgb(withKept, 5, 5) shouldBe inkRgb
  }

  "A draw under a pixel translation" should "stay off the kept rect, which is in the pixels the frame paints in" in {
    val frame = drawn(List(kept)) { surface =>
      surface.pixels.withPixelTranslation(30.0, 20.0)(surface.pixels.fillPixelRect(6, 10, 70, 14, ink))
    }

    keptPixels(frame).distinct shouldBe Seq(sentinelRgb)
    rgb(frame, 37, 31) shouldBe inkRgb
  }
