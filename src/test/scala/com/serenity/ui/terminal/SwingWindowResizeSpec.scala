package com.serenity.ui.terminal

import java.awt.{Dimension, Frame, GraphicsEnvironment, Point}
import javax.swing.SwingUtilities

import com.serenity.config.{PreferredWindowSize, WindowChromeMode}
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `resizeToPreferred` puts the window back in the middle of the screen, as a change of the preferred size always has;
  * a benchmark that resizes a window over and over asks for it to stay where it is (#2480).
  */
class SwingWindowResizeSpec extends AnyFlatSpec with Matchers:

  private val Title = "Serenity resize spec"

  private def locationAfterResize(recentre: Option[Boolean]): Point =
    DisplayExclusive {
      val metrics = CellMetrics(8, 16, 12)
      val window =
        new SwingWindow(new Dimension(640, 400), metrics, WindowChromeMode.Auto, metrics, windowTitle = Title)
      window.start()
      try
        val frame = Frame.getFrames.find(_.getTitle == Title).getOrElse(fail("no window"))
        SwingUtilities.invokeAndWait(() => frame.setLocation(37, 41))
        recentre.fold(window.resizeToPreferred(PreferredWindowSize(800, 500)))(keep =>
          window.resizeToPreferred(PreferredWindowSize(800, 500), recentre = keep)
        )
        SwingUtilities.invokeAndWait(() => ())
        SwingUtilities.invokeAndWait(() => ())
        frame.getLocation
      finally window.stop()
    }

  private def withDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "A window needs a display")

  "resizeToPreferred" should "keep the window where it is when it is asked not to recentre" in {
    withDisplay()
    locationAfterResize(Some(false)) shouldBe new Point(37, 41)
  }

  it should "put the window in the middle of the screen when it is asked to, and by default" in {
    withDisplay()
    locationAfterResize(Some(true)) should not be new Point(37, 41)
    locationAfterResize(None) should not be new Point(37, 41)
  }
