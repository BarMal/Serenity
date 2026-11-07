package com.serenity.ui.terminal

import java.awt.{Dimension, Frame, GraphicsEnvironment, Point}
import javax.swing.SwingUtilities

import com.serenity.config.{PreferredWindowSize, WindowChromeMode}
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.layout.CellMetrics
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}

/** `resizeToPreferred` puts the window back in the middle of the screen, as a change of the preferred size always has;
  * a benchmark that resizes a window over and over asks for it to stay where it is (#2480). The toolkit applies a size
  * and a move after the call returns, so each check waits for the toolkit and not for the event queue.
  */
class SwingWindowResizeSpec extends AnyFlatSpec with Matchers with Eventually:

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(20, Millis))

  private val Titles    = new java.util.concurrent.atomic.AtomicInteger(0)
  private val Elsewhere = new Point(37, 41)
  private val Target    = PreferredWindowSize(800, 500)

  /** Starts a window at (37,41), asks for `Target` the way `ask` says, and hands over the frame once the toolkit has
    * taken the size, which is when a move requested with it would have been made too.
    */
  private def afterResize(
    label: String
  )(ask: com.serenity.ui.terminal.SwingWindow => Unit)(check: Frame => Unit): Unit =
    DisplayExclusive {
      // A title of its own: a window an earlier test closed can still be listed under the same one.
      val title   = s"Serenity resize spec ${Titles.incrementAndGet()}"
      val metrics = CellMetrics(8, 16, 12)
      val window =
        new SwingWindow(new Dimension(640, 400), metrics, WindowChromeMode.Auto, metrics, windowTitle = title)
      window.start()
      try
        val frame = Frame.getFrames.find(_.getTitle == title).getOrElse(fail("no window"))
        val trace = new PlacementTrace(label, frame)
        try
          // Until the toolkit has mapped the window it may still place it, so the move is asked for again until it holds.
          eventually(frame.isShowing shouldBe true)
          trace.mark("showing")
          eventually {
            SwingUtilities.invokeAndWait(() => frame.setLocation(Elsewhere.x, Elsewhere.y))
            trace.mark("setLocation(37,41) done")
            frame.getLocation shouldBe Elsewhere
          }
          trace.mark("location held once; asking")
          ask(window)
          trace.mark("ask returned")
          eventually((frame.getWidth, frame.getHeight) shouldBe (Target.width, Target.height))
          trace.mark("size reached; checking")
          check(frame)
          trace.mark("check passed")
        finally trace.finish()
      finally window.stop()
    }

  private def withDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "A window needs a display")

  "resizeToPreferred" should "keep the window where it is when it is asked not to recentre" in {
    withDisplay()
    afterResize("keep")(_.resizeToPreferred(Target, recentre = false))(_.getLocation shouldBe Elsewhere)
  }

  it should "put the window in the middle of the screen when it is asked to" in {
    withDisplay()
    afterResize("recentre")(_.resizeToPreferred(Target, recentre = true))(frame =>
      eventually(frame.getLocation should not be Elsewhere)
    )
  }

  it should "put the window in the middle of the screen by default, as every other caller has it" in {
    withDisplay()
    afterResize("default")(_.resizeToPreferred(Target))(frame => eventually(frame.getLocation should not be Elsewhere))
  }

  "canvasDevicePixels" should "be the canvas size in Swing units times the screen scale" in {
    withDisplay()
    afterResize("canvas")(_.resizeToPreferred(Target, recentre = false)) { _ => }
    DisplayExclusive {
      val metrics = CellMetrics(8, 16, 12)
      val window =
        new SwingWindow(
          new Dimension(640, 400),
          metrics,
          WindowChromeMode.Auto,
          metrics,
          windowTitle = s"Serenity canvas spec ${Titles.incrementAndGet()}"
        )
      window.start()
      try
        eventually(window.canvas.isShowing shouldBe true)
        eventually {
          val scale = com.serenity.ui.display.DisplayScale.forComponent(window.canvas)
          val size  = window.canvas.getSize
          window.canvasDevicePixels shouldBe (
            Math.round(size.width * scale.x).toInt,
            Math.round(size.height * scale.y).toInt
          )
          size.width should be > 0
        }
      finally window.stop()
    }
  }

/** DIAGNOSTIC, do not merge: the frame's location, size and visibility every 20 ms from the moment the spec has the
  * frame, with the spec's own steps marked on the same clock, appended to target/test-reports/diag-swing-resize.txt.
  */
final private class PlacementTrace(label: String, frame: Frame):
  import java.nio.charset.StandardCharsets
  import java.nio.file.{Files, Path, StandardOpenOption}
  import java.util.concurrent.ConcurrentLinkedQueue
  import java.util.concurrent.atomic.AtomicBoolean

  private val started = System.nanoTime()
  private val lines   = new ConcurrentLinkedQueue[String]()
  private val running = new AtomicBoolean(true)

  private def elapsedMs: Long = (System.nanoTime() - started) / 1000000

  def mark(what: String): Unit =
    val _ = lines.add(f"$elapsedMs%6d ms MARK   $what")

  private val sampler = new Thread(
    () =>
      while running.get do
        val at   = frame.getLocation
        val size = frame.getSize
        val _ = lines.add(
          f"$elapsedMs%6d ms sample (${at.x},${at.y}) ${size.width}x${size.height} showing=${frame.isShowing}"
        )
        Thread.sleep(20)
    ,
    "placement-trace"
  )

  sampler.setDaemon(true)
  sampler.start()

  def finish(): Unit =
    running.set(false)
    sampler.join(1000)
    val body = lines.toArray.mkString(s"== $label (${java.time.Instant.now})\n", "\n", "\n")
    val file = Path.of("target", "test-reports", "diag-swing-resize.txt")
    try
      val _ = Files.createDirectories(file.getParent)
      val _ = Files.write(
        file,
        body.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    catch case _: java.io.IOException => ()
