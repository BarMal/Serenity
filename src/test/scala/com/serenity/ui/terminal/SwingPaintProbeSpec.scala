package com.serenity.ui.terminal

import java.awt.image.BufferedImage
import java.awt.{Color, GraphicsEnvironment}
import javax.swing.SwingUtilities

import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a window records about its own first paints, so a launch whose window stays unpainted (#2404) says whether the
  * canvas was painted at all, when, and whether it was showing and sized at the time the first frame was rendered.
  */
class SwingPaintProbeSpec extends AnyFlatSpec with Matchers:

  final private class Clock(start: Long):
    private val nanos = new java.util.concurrent.atomic.AtomicLong(start)

    def read(): Long           = nanos.get()
    def set(value: Long): Unit = nanos.set(value)

  private val millis = 1_000_000L

  "A paint probe" should "report no paint before the first one" in {
    val clock = Clock(5 * millis)
    val probe = new PaintProbe(clock.read)

    probe.diagnostics(showing = false, width = 0, height = 0) shouldBe
      PaintDiagnostics(paints = 0, firstPaintMillis = None, showing = false, canvasWidth = 0, canvasHeight = 0)
  }

  it should "count paints and time the first from the probe's creation" in {
    val clock = Clock(10 * millis)
    val probe = new PaintProbe(clock.read)

    clock.set(412 * millis + 10 * millis)
    probe.painted()
    clock.set(900 * millis)
    probe.painted()
    probe.painted()

    probe.diagnostics(showing = true, width = 1024, height = 768) shouldBe
      PaintDiagnostics(paints = 3, firstPaintMillis = Some(412), showing = true, canvasWidth = 1024, canvasHeight = 768)
  }

  "A paint diagnostics line" should "name the label, the count, the first paint and the canvas" in {
    PaintDiagnostics(3, Some(412), showing = true, canvasWidth = 1024, canvasHeight = 768).line("first frame") shouldBe
      "[FIRST-PAINT] first frame: paints=3 firstPaintMs=412 showing=true canvas=1024x768"
  }

  it should "say none when the canvas has not been painted" in {
    PaintDiagnostics(0, None, showing = false, canvasWidth = 0, canvasHeight = 0).line("2 s later") shouldBe
      "[FIRST-PAINT] 2 s later: paints=0 firstPaintMs=none showing=false canvas=0x0"
  }

  "A shown SwingWindow" should "count the canvas paints a published frame causes" in {
    assume(!GraphicsEnvironment.isHeadless, "Painting the canvas needs a real window, which needs a display")

    val diagnostics = DisplayExclusive {
      val window = new SwingWindow(
        new java.awt.Dimension(640, 480),
        com.serenity.ui.layout.CellMetrics(8, 16, 13),
        initialChromeMetrics = com.serenity.ui.layout.CellMetrics(8, 16, 13)
      )
      try
        window.start()
        val frame = new BufferedImage(window.canvas.getWidth.max(1), window.canvas.getHeight.max(1), 1)
        val g     = frame.createGraphics()
        try
          g.setColor(Color.BLUE)
          g.fillRect(0, 0, frame.getWidth, frame.getHeight)
        finally g.dispose()
        window.onImageReady(frame)
        SwingUtilities.invokeAndWait(() => ())
        SwingUtilities.invokeAndWait(() => ())
        window.paintDiagnostics
      finally
        window.stop()
        SwingUtilities.invokeAndWait(() => ())
    }

    diagnostics.paints should be > 0
    diagnostics.firstPaintMillis should not be empty
    diagnostics.showing shouldBe true
    diagnostics.canvasWidth should be > 0
    diagnostics.canvasHeight should be > 0
  }
