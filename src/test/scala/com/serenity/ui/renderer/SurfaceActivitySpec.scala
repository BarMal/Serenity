package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.charset.StandardCharsets

import com.serenity.diagnostics.{FrameCounter, FrameCounters}
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.tui.TerminalRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a surface reports of its drawing to the frame counters: the runs it painted, the layers composited onto it and
  * the bytes a terminal flush wrote.
  */
class SurfaceActivitySpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width   = 210
  private val height  = 130

  private def surface(): Java2DRenderSurface =
    new Java2DRenderSurface(
      new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB),
      metrics,
      font,
      _ => (),
      width,
      height,
      1.0,
      1.0
    )

  private def frameOf(surface: RenderSurface): com.serenity.diagnostics.FrameCounts =
    val counters = FrameCounters()
    counters.setEnabled(true)
    surface.activity.drainInto(counters)
    counters.frameFinished()
    counters.lastFrame

  private def drawRun(surface: RenderSurface, text: String, xPx: Float = 14f): Unit =
    surface.text.drawRunPx(xPx, 26, 56f, metrics.lineHeight, metrics.ascent, text)

  "A Java2D surface" should "count each run it paints" in {
    val drawing = surface()

    drawRun(drawing, "one")
    drawRun(drawing, "two", xPx = 80f)
    drawRun(drawing, "three", xPx = 140f)

    frameOf(drawing)(FrameCounter.RunsDrawn) shouldBe 3L
  }

  it should "not count a run that lies wholly outside the surface" in {
    val drawing = surface()

    drawRun(drawing, "gone", xPx = (width + 100).toFloat)

    frameOf(drawing)(FrameCounter.RunsDrawn) shouldBe 0L
  }

  it should "count runs once, then start from zero again" in {
    val drawing = surface()
    drawRun(drawing, "one")
    val counters = FrameCounters()
    counters.setEnabled(true)
    drawing.activity.drainInto(counters)

    drawing.activity.drainInto(counters)
    counters.frameFinished()

    counters.lastFrame(FrameCounter.RunsDrawn) shouldBe 1L
  }

  it should "count each layer composited onto it" in {
    val drawing = surface()
    val layer   = RenderImage.fromAwt(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))

    drawing.compositeFullSurfaceLayer(layer)
    drawing.compositeFullSurfaceLayer(layer)

    frameOf(drawing)(FrameCounter.LayerComposites) shouldBe 2L
  }

  it should "count what a layer surface painted as its own" in {
    val drawing = surface()
    val layer   = drawing.layerBuffers.map(_.newLayerSurface(_ => (), None))

    layer.foreach(drawRun(_, "in a layer"))

    frameOf(drawing)(FrameCounter.RunsDrawn) shouldBe 1L
  }

  "A terminal surface" should "count the UTF-8 bytes each flush wrote, and none for a flush that wrote nothing" in {
    val written = new java.io.StringWriter()
    val tui     = new TerminalRenderSurface(20, 2, written, CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0))
    tui.putString(0, 0, "caf\u00e9 \u20ac")
    tui.flush()
    val writtenBytes = written.toString.getBytes(StandardCharsets.UTF_8).length.toLong
    val counted      = frameOf(tui)(FrameCounter.TuiBytes)
    tui.flush()

    writtenBytes should be > 0L
    counted shouldBe writtenBytes
    frameOf(tui)(FrameCounter.TuiBytes) shouldBe 0L
  }

  "SurfaceActivity.utf8Length" should "agree with the encoder for one-, two-, three- and four-byte characters" in {
    val samples = List("", "abc", "café", "€100", "a😀b", "é€😀x")

    samples.foreach { sample =>
      withClue(s"'$sample': ")(
        SurfaceActivity.utf8Length(sample) shouldBe sample.getBytes(StandardCharsets.UTF_8).length
      )
    }
  }

  "Bytes flushed" should "all be charged to the frame that closes next, and only once" in {
    val activity = SurfaceActivity()
    activity.flushed(120)
    activity.flushed(30)
    val counters = FrameCounters()
    counters.setEnabled(true)

    activity.drainInto(counters)
    counters.frameFinished()
    val charged = counters.lastFrame(FrameCounter.TuiBytes)
    activity.drainInto(counters)
    counters.frameFinished()

    charged shouldBe 150L
    counters.lastFrame(FrameCounter.TuiBytes) shouldBe 0L
  }
