package com.serenity.app

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.SwingUtilities

import cats.effect.IO
import com.serenity.app.PixelClock.Region
import com.serenity.app.RendererWorkload.{Redraw, Step}
import com.serenity.config.PreferredWindowSize
import com.serenity.ui.terminal.SwingWindowHost

/** Drives the Swing window for a renderer trial (#2618) and says where each step shows, in screen coordinates.
  *
  * The regions are small so a grab stays cheap; what is watched, not how big, is what makes a change visible: the first
  * rows for a typed character, a band in the middle for a scroll, a spot of the background for a theme change, and the
  * edge the window moves for a resize.
  */
final class SwingTrialDriver(host: SwingWindowHost) extends RendererTrialWindow.Driver:

  private val window = host.window
  private val grown  = new AtomicBoolean(false)

  private val ResizeStep = 80
  private val EdgeBand   = 32

  def typeCharacter(char: Char): IO[Unit] = host.typeCharacter(char)
  def pressPageDown: IO[Unit]             = host.pressPageDown
  def toggleTheme: IO[Unit]               = host.toggleTheme

  def resize: IO[Unit] =
    IO.blocking {
      val size    = window.currentPreferredWindowSize
      val growing = !grown.get()
      val delta   = if growing then ResizeStep else -ResizeStep
      window.resizeToPreferred(PreferredWindowSize(size.width + delta, size.height), recentre = false)
      grown.set(growing)
    }

  def watchedFor(step: Step): IO[Region] =
    IO.blocking(canvas()).map { (origin, width, height) =>
      val cell = host.target.metrics
      step match
        case _: Step.Type =>
          Region(origin._1, origin._2, width.min(40 * cell.charWidth), height.min(4 * cell.lineHeight))
        case Step.PageDown =>
          Region(
            origin._1 + width / 4,
            origin._2 + height / 3,
            (width / 2).min(40 * cell.charWidth),
            3 * cell.lineHeight
          )
        case Step.FullRedraw(Redraw.ThemeToggle) =>
          Region(origin._1 + width / 2, origin._2 + height / 2, 8, 8)
        case Step.FullRedraw(Redraw.Resize) =>
          val edge = origin._1 + width - (if grown.get() then ResizeStep else 0)
          Region(edge - EdgeBand / 2, origin._2 + height / 2, EdgeBand, 8)
    }

  /** The canvas's origin on the screen and its size, read on the event thread. */
  private def canvas(): ((Int, Int), Int, Int) =
    val result = new AtomicReference[Option[((Int, Int), Int, Int)]](None)
    SwingUtilities.invokeAndWait { () =>
      val origin = window.canvas.getLocationOnScreen
      result.set(Some(((origin.x, origin.y), window.canvas.getWidth, window.canvas.getHeight)))
    }
    result.get().getOrElse(((0, 0), 0, 0))
