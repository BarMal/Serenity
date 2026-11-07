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
  private val MovePause = scala.concurrent.duration.DurationInt(150).millis

  /** Starts a window at (37,41), asks for `Target` the way `ask` says, and hands over the frame once the toolkit has
    * taken the size, which is when a move requested with it would have been made too.
    */
  private def afterResize(ask: com.serenity.ui.terminal.SwingWindow => Unit)(check: Frame => Unit): Unit =
    DisplayExclusive {
      // A title of its own: a window an earlier test closed can still be listed under the same one.
      val title   = s"Serenity resize spec ${Titles.incrementAndGet()}"
      val metrics = CellMetrics(8, 16, 12)
      val window =
        new SwingWindow(new Dimension(640, 400), metrics, WindowChromeMode.Auto, metrics, windowTitle = title)
      window.start()
      try
        val frame = Frame.getFrames.find(_.getTitle == title).getOrElse(fail("no window"))
        eventually(frame.isShowing shouldBe true)
        placeThenAsk(frame)(ask(window))
        check(frame)
      finally window.stop()
    }

  /** A window that is showing can still be taking its first place and size from the toolkit, which may then give the
    * frame that geometry back for good: with no window manager, as under Xvfb, about 1 window in 100 sat at its first
    * place and size through the whole wait, and the frame's own record of a move is no proof the toolkit took it. So
    * the move is proved by the frame still being there after a pause, and the move and the ask are made again until the
    * toolkit has taken the size too.
    */
  private def placeThenAsk(frame: Frame)(ask: => Unit): Unit =
    eventually {
      SwingUtilities.invokeAndWait(() => frame.setLocation(Elsewhere.x, Elsewhere.y))
      Thread.sleep(MovePause.toMillis)
      frame.getLocation shouldBe Elsewhere
      ask
      eventually(timeout(Span(300, Millis)))((frame.getWidth, frame.getHeight) shouldBe (Target.width, Target.height))
    }

  private def withDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "A window needs a display")

  "resizeToPreferred" should "keep the window where it is when it is asked not to recentre" in {
    withDisplay()
    afterResize(_.resizeToPreferred(Target, recentre = false))(_.getLocation shouldBe Elsewhere)
  }

  it should "put the window in the middle of the screen when it is asked to" in {
    withDisplay()
    afterResize(_.resizeToPreferred(Target, recentre = true))(frame =>
      eventually(frame.getLocation should not be Elsewhere)
    )
  }

  it should "put the window in the middle of the screen by default, as every other caller has it" in {
    withDisplay()
    afterResize(_.resizeToPreferred(Target))(frame => eventually(frame.getLocation should not be Elsewhere))
  }

  "canvasDevicePixels" should "be the canvas size in Swing units times the screen scale" in {
    withDisplay()
    afterResize(_.resizeToPreferred(Target, recentre = false)) { _ => }
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
