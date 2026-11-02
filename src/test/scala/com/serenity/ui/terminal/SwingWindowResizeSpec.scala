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

  private val Title     = "Serenity resize spec"
  private val Elsewhere = new Point(37, 41)
  private val Target    = PreferredWindowSize(800, 500)

  /** Starts a window at (37,41), asks for `Target` the way `ask` says, and hands over the frame once the toolkit has
    * taken the size, which is when a move requested with it would have been made too.
    */
  private def afterResize(ask: com.serenity.ui.terminal.SwingWindow => Unit)(check: Frame => Unit): Unit =
    DisplayExclusive {
      val metrics = CellMetrics(8, 16, 12)
      val window =
        new SwingWindow(new Dimension(640, 400), metrics, WindowChromeMode.Auto, metrics, windowTitle = Title)
      window.start()
      try
        val frame = Frame.getFrames.find(_.getTitle == Title).getOrElse(fail("no window"))
        SwingUtilities.invokeAndWait(() => frame.setLocation(Elsewhere.x, Elsewhere.y))
        eventually(frame.getLocation shouldBe Elsewhere)
        ask(window)
        eventually((frame.getWidth, frame.getHeight) shouldBe (Target.width, Target.height))
        check(frame)
      finally window.stop()
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
