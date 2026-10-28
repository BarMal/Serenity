package com.serenity.ui.skia

import java.awt.Font

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{DeviceArea, FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A batch of rects is one place the frame painted, so it is one record, and the record covers every pixel the batch
  * changed: a pixel it missed would be left stale in a pooled image that follows the surface by moving rows.
  */
class SkiaBatchChangeRecordSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val width   = 120
  private val height  = 60

  /** A zig-zag like a misspelling's squiggle: two-pixel steps, one pixel high, three deep. */
  private val zigzag: IArray[Int] =
    IArray.from(
      (0 until 20).flatMap(step => List(10 + step * 2, 20 + (if step % 6 <= 3 then step % 6 else 6 - step % 6), 2, 1))
    )

  final private class Window(scale: Double):

    val surface: Surface = Surface.Companion.makeRaster(
      RasterFrames.frameInfo(
        SkiaRenderSurface.deviceDimension(width, scale),
        SkiaRenderSurface.deviceDimension(height, scale)
      )
    )

    val changes = new ChangeRecorder(scale, scale, surface.getWidth, surface.getHeight)

    val renderSurface: SkiaRenderSurface =
      SkiaRenderSurface.retaining(
        surface,
        metrics,
        FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
        width,
        height,
        scale,
        SkiaTextCaches.fresh(),
        StyledFontCache(),
        changes = Some(changes)
      )

    renderSurface.pixels.fillPixelRect(0, 0, width, height, RenderColor.fromRgba(240, 240, 235))

  private def inside(areas: List[DeviceArea], x: Int, y: Int): Boolean =
    areas.exists(a => x >= a.left && x < a.left + a.width && y >= a.top && y < a.top + a.height)

  for scale <- List(1.0, 2.0, 1.5) do
    s"A batch of rects at ${scale}x" should "be one change record that covers every pixel it changed" in {
      val window = new Window(scale)
      val before = SkiaPixels.read(window.surface)
      window.changes.paintRecords.size // the background fill's records are not this batch's
      val recordedBefore = window.changes.paintRecords.size
      window.renderSurface.pixels.fillPixelRects(zigzag, RenderColor.fromRgba(200, 30, 30))
      val after   = SkiaPixels.read(window.surface)
      val records = window.changes.paintRecords
      records.size shouldBe recordedBefore + 1
      val deviceWidth = window.surface.getWidth
      val missed =
        after.indices.filter(i => after(i) != before(i) && !inside(records, i % deviceWidth, i / deviceWidth))
      withClue(s"${missed.size} changed pixels are outside every record: ")(missed shouldBe empty)
    }
