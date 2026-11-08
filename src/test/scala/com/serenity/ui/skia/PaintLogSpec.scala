package com.serenity.ui.skia

import java.awt.{Color, Font, Rectangle}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.{FramePixels, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PaintLogSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val width   = 210
  private val height  = 130
  private val runBox  = Rectangle(14, 26, 14, 13)

  private def overflowingRun: FramePixels =
    val frame = SkiaFrames.newFrame(width, height)
    val surface = SkiaFrames.surfaceOver(
      frame,
      metrics,
      new Font(Font.MONOSPACED, Font.PLAIN, 12),
      width,
      height,
      1.0,
      persists = true,
      RenderCaches.create()
    )
    surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.text.drawShapedRunPx(14f, 26, 14f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMM", 0, 10)
    frame

  private def inked(frame: FramePixels): List[(Int, Int)] =
    (for
      y <- 0 until height
      x <- 0 until width
      if frame.rgbAt(x, y) != Color.BLACK.getRGB
    yield (x, y)).toList

  "A frame drawn on a persisting Skia surface" should "report the paint of a glyph that overruns its run" in {
    val frame = overflowingRun
    val ink   = inked(frame)

    ink.exists((x, y) => !runBox.contains(x, y)) shouldBe true
    ink.filterNot((x, y) => frame.recordedPaint.exists(_.contains(x, y))) shouldBe empty
  }

  it should "leave a pixel stale on a screen that copies only the run's box" in {
    val frame = overflowingRun
    val ink   = inked(frame).filterNot((x, y) => runBox.contains(x, y))

    ink should not be empty
    ink.exists((x, y) =>
      !List(runBox).exists(_.contains(x, y)) && frame.recordedPaint.exists(_.contains(x, y))
    ) shouldBe true
  }
