package com.serenity.perf

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.LayoutEngine
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the mark a wrapped row carries in the line-number column stays inside its own row, on every renderer.
  * It used to be the glyph `│` set in the buffer's font, whose ink runs as tall as the face draws it: a face whose bar
  * joins the one on the next row, or whose antialiasing spills a pixel row past its own, leaves pixels in a neighbour's
  * band that a frame reusing scrolled rows does not repaint the way a cold frame does.
  */
class GutterContinuationMarkSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val state    = LaptopFrameBenchmarks.proseState
  private val gutter   = LayoutEngine.calculateLayout(state, LaptopFrameBenchmarks.viewport).lineNumberRect
  private val cells    = LaptopFrameBenchmarks.cellMetrics
  private val scale    = LaptopFrameBenchmarks.DeviceScale
  private val rowPx    = math.round(cells.lineHeight * scale).toInt
  private val backends = List[(String, FrameBackend)]("Java2D" -> Java2DFrames, "Skia" -> SkiaFrames)

  /** The tallest unbroken run of ink in any pixel column of the counter, leaving out its divider cell. */
  private def tallestInkRun(frame: FramePixels): Int =
    val rect   = gutter.getOrElse(fail("no line-number rect"))
    val left   = math.round(cells.toPixelX(rect.x) * scale).toInt
    val right  = math.round((cells.toPixelX(rect.right) - cells.charWidth) * scale).toInt
    val top    = math.round(cells.toPixelY(rect.y) * scale).toInt
    val bottom = math.round(cells.toPixelY(rect.bottom) * scale).toInt
    (left + 2 until right)
      .map { x =>
        val runs = (top until bottom).foldLeft((0, 0)) {
          case ((best, current), y) =>
            if frame.rgbAt(x, y) != frame.rgbAt(left + 1, y) then (best.max(current + 1), current + 1) else (best, 0)
        }
        runs._1
      }
      .maxOption
      .getOrElse(0)

  backends.foreach { (name, backend) =>
    s"A wrapped row's mark in the line-number column on $name" should "be shorter than its row, so no two rows' marks join" in {
      val frame = LaptopColdFrames.of(backend, state, RenderCaches.create(), cursorVisible = false)

      val tallest = tallestInkRun(frame)

      withClue(s"tallest unbroken ink in the counter, against a row of $rowPx device pixels: ")(
        tallest should (be > 0 and be < rowPx)
      )
    }
  }
