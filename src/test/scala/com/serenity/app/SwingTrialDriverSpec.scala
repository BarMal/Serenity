package com.serenity.app

import java.awt.GraphicsEnvironment
import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.PixelClock.Region
import com.serenity.app.RendererWorkload.{Redraw, Step}
import com.serenity.config.WindowChromeMode
import com.serenity.diagnostics.FrameTimings
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.terminal.{SwingWindow, SwingWindowHost}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The trial driver against a real Swing window, on a display (Xvfb in the Test job). */
class SwingTrialDriverSpec extends AnyFlatSpec with Matchers:

  private def onEdt[A](body: => A): A =
    val result = new AtomicReference[Option[A]](None)
    SwingUtilities.invokeAndWait(() => result.set(Some(body)))
    result.get().getOrElse(fail("the event thread produced nothing"))

  private def withDriver[A](use: (SwingWindow, RendererTrialWindow.Driver) => A): A =
    assume(!GraphicsEnvironment.isHeadless, "A Swing window needs a display")
    DisplayExclusive:
      SwingWindowHost
        .resource(
          SwingWindow.DefaultMetrics,
          SwingWindow.DefaultMetrics,
          WindowChromeMode.Auto,
          None,
          FrameTimings(),
          "driver spec"
        )
        .use { host =>
          host match
            case swing: SwingWindowHost => IO(use(swing.window, SwingTrialDriver(swing)))
            case other => IO.raiseError(new IllegalStateException(s"expected a Swing host, got $other"))
        }
        .unsafeRunSync()

  private def widthChangedFrom(window: SwingWindow, from: Int): Option[Int] =
    def poll: IO[Int] = IO.defer {
      val width = window.currentPreferredWindowSize.width
      if width != from then IO.pure(width) else IO.sleep(50.millis) *> poll
    }
    poll.timeout(3.seconds).attempt.unsafeRunSync().toOption

  private def canvasBounds(window: SwingWindow): Region =
    onEdt {
      val origin = window.canvas.getLocationOnScreen
      Region(origin.x, origin.y, window.canvas.getWidth, window.canvas.getHeight)
    }

  private def inside(inner: Region, outer: Region): Boolean =
    inner.x >= outer.x && inner.y >= outer.y &&
      inner.x + inner.width <= outer.x + outer.width && inner.y + inner.height <= outer.y + outer.height

  "SwingTrialDriver" should "watch the first rows for typing and a band in the middle for scrolling, both on the canvas" in
    withDriver { (window, driver) =>
      val canvas = canvasBounds(window)
      val typing = driver.watchedFor(Step.Type('a')).unsafeRunSync()
      val scroll = driver.watchedFor(Step.PageDown).unsafeRunSync()
      inside(typing, canvas) shouldBe true
      inside(scroll, canvas) shouldBe true
      typing.y shouldBe canvas.y
      scroll.y should be > typing.y
    }

  it should "watch a spot that the new colours reach for a theme toggle, on the canvas" in
    withDriver { (window, driver) =>
      val canvas = canvasBounds(window)
      inside(driver.watchedFor(Step.FullRedraw(Redraw.ThemeToggle)).unsafeRunSync(), canvas) shouldBe true
    }

  it should "watch across the edge the next resize moves, and move the window's edge each time it resizes" in
    withDriver { (window, driver) =>
      val before = window.currentPreferredWindowSize.width
      val canvas = canvasBounds(window)
      val across = driver.watchedFor(Step.FullRedraw(Redraw.Resize)).unsafeRunSync()
      (across.x < canvas.x + canvas.width && across.x + across.width > canvas.x + canvas.width) shouldBe true
      driver.resize.unsafeRunSync()
      widthChangedFrom(window, before) should not be empty
      val grown = window.currentPreferredWindowSize.width
      driver.resize.unsafeRunSync()
      widthChangedFrom(window, grown) should not be empty
      window.currentPreferredWindowSize.width shouldBe before
    }
