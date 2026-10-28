package com.serenity.ui.skia

import java.awt.Font

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A batch of rects is the same picture as the fills drawn one by one, so the squiggle can be one call into Skia
  * without changing a pixel of what it shows.
  */
class SkiaFillPixelRectsSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val width   = 120
  private val height  = 60

  private val background = RenderColor.fromRgba(240, 240, 235)

  /** A zig-zag like a misspelling's squiggle: two-pixel steps, one pixel high, three deep. */
  private val zigzag: IArray[Int] =
    IArray.from((0 until 20).flatMap(step => List(10 + step * 2, 20 + (if step % 6 <= 3 then step % 6 else 6 - step % 6), 2, 1)))

  private final class Canvas(scale: Double):
    val surface: Surface = Surface.Companion.makeRaster(
      RasterFrames.frameInfo(SkiaRenderSurface.deviceDimension(width, scale), SkiaRenderSurface.deviceDimension(height, scale))
    )
    val renderSurface: SkiaRenderSurface =
      SkiaRenderSurface.retaining(
        surface,
        metrics,
        FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
        width,
        height,
        scale,
        SkiaTextCaches.fresh(),
        StyledFontCache()
      )
    renderSurface.pixels.fillPixelRect(0, 0, width, height, background)

    def pixels: Array[Int] = SkiaPixels.read(surface)

  private def oneByOne(canvas: Canvas, rects: IArray[Int], color: RenderColor): Array[Int] =
    rects.grouped(4).foreach(quad => canvas.renderSurface.pixels.fillPixelRect(quad(0), quad(1), quad(2), quad(3), color))
    canvas.pixels

  private def asBatch(canvas: Canvas, rects: IArray[Int], color: RenderColor): Array[Int] =
    canvas.renderSurface.pixels.fillPixelRects(rects, color)
    canvas.pixels

  private val colours = List(
    "an opaque colour"      -> RenderColor.fromRgba(200, 30, 30),
    "a translucent colour"  -> RenderColor.fromRgba(200, 30, 30, 128)
  )

  for
    scale          <- List(1.0, 2.0, 1.5)
    (label, color) <- colours
  do
    s"A batch of rects at ${scale}x in $label" should "show the same pixels as the same fills one by one" in {
      val expected = oneByOne(new Canvas(scale), zigzag, color)
      val actual   = asBatch(new Canvas(scale), zigzag, color)
      actual.length shouldBe expected.length
      val differing = actual.indices.filter(i => actual(i) != expected(i))
      withClue(s"${differing.size} pixels differ, first at ${differing.headOption}: ")(differing shouldBe empty)
    }

  "A batch of rects" should "change something when it is not empty and nothing when it is" in {
    val untouched = new Canvas(1.0).pixels
    asBatch(new Canvas(1.0), IArray.empty[Int], RenderColor.fromRgba(1, 2, 3)) shouldBe untouched
    asBatch(new Canvas(1.0), zigzag, RenderColor.fromRgba(1, 2, 3)) should not be untouched
  }

  it should "be clipped by a rect clip as the fills one by one are" in {
    def clipped(draw: Canvas => Unit): Array[Int] =
      val canvas = new Canvas(1.0)
      canvas.renderSurface.panelOutlines.foreach(_.withRectClip(0, 0, 3, 3)(draw(canvas)))
      canvas.pixels
    val color    = RenderColor.fromRgba(10, 90, 200)
    val expected = clipped(canvas => zigzag.grouped(4).foreach(q => canvas.renderSurface.pixels.fillPixelRect(q(0), q(1), q(2), q(3), color)))
    val actual   = clipped(canvas => canvas.renderSurface.pixels.fillPixelRects(zigzag, color))
    actual shouldBe expected
  }
