package com.serenity.perf

import java.awt.{Color, Rectangle, Toolkit}
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.SwingUtilities

import cats.effect.{IO, Resource}
import com.serenity.config.PreferredWindowSize
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.terminal.SwingWindow

/** The laptop-sized window's presents: the frame through to the display, and a caret blink over it. */
object LaptopPresentBenchmarks:

  /** The laptop-sized window [[presentBenchmark]] paints into. */
  def presentWindowResource(metrics: CellMetrics, chromeMetrics: CellMetrics): Resource[IO, SwingWindow] =
    SwingWindow.resource(
      metrics = metrics,
      chromeMetrics = chromeMetrics,
      preferredWindowSize =
        Some(PreferredWindowSize(LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.LogicalHeightPx))
    )

  def benchmarks(presentWindow: => SwingWindow): List[BenchmarkRunner.Benchmark] =
    List(presentBenchmark(presentWindow), presentCaretBenchmark(presentWindow))

  /** A full-window present of a freshly rendered frame, through to the display.
    *
    * `Toolkit.sync` waits for the X server to finish the image upload and composite the client only queues. Without it
    * the time is however far ahead of the server the client happened to run, which is what made the previous
    * `swing_paint_window` bimodal.
    */
  private def presentBenchmark(presentWindow: => SwingWindow): BenchmarkRunner.Benchmark =
    lazy val window = presentWindow
    lazy val canvas = window.canvas
    lazy val frames = PresentedFrames.forWindow(window)
    BenchmarkRunner.Benchmark.building((window, canvas, frames))(
      BenchmarkRunner.Benchmark(
        "laptop.present.swing_fresh_frame_synced_1500x1000",
        2,
        BenchmarkIterationCounts.Present,
        () =>
          assert(
            canvas.isShowing && canvas.getWidth > 0,
            s"canvas not showing: ${canvas.getWidth}x${canvas.getHeight}"
          ),
        () =>
          val frame = frames.next()
          SwingUtilities.invokeAndWait { () =>
            window.onBaseImageReady(frame)
            canvas.paintImmediately(0, 0, canvas.getWidth, canvas.getHeight)
            Toolkit.getDefaultToolkit.sync()
          }
      )
    )

  /** A caret blink tick as the app runs one: the caret is published from off the EDT and the window's own pending
    * repaint paints its rect over an unchanged base frame; the empty `invokeAndWait` queues behind that repaint.
    *
    * Unlike [[presentBenchmark]] it does not `Toolkit.sync`: the X server's share of a 2x18 rect is negligible, and the
    * extra round trip only made the time track how promptly the scheduler woke Xvfb.
    */
  private def presentCaretBenchmark(presentWindow: => SwingWindow): BenchmarkRunner.Benchmark =
    lazy val window = presentWindow
    val caret       = SwingWindow.CaretPaint(new Rectangle(700, 500, 2, 18), Color.WHITE)
    val ticks       = new AtomicInteger(0)
    def blink(): Boolean =
      window.onCursorOverlayReady(Some(Nil))(if ticks.incrementAndGet() % 2 == 0 then List(caret) else Nil)
    BenchmarkRunner.Benchmark.building(window)(
      BenchmarkRunner.Benchmark(
        "laptop.present.swing_paint_caret_1500x1000",
        2,
        BenchmarkIterationCounts.Present,
        () =>
          val frame = PresentedFrames.forWindow(window).next()
          SwingUtilities.invokeAndWait(() => window.onBaseImageReady(frame))
          assert(blink(), "no base frame to blink the caret over")
        ,
        () =>
          val _ = blink()
          SwingUtilities.invokeAndWait(() => ())
      )
    )
