package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, GraphicsEnvironment, Rectangle, Robot, Toolkit}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.WindowChromeMode
import com.serenity.testkit.{AwaitCondition, DisplayExclusive}
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2404: a launch with no config file was reported to leave the window flat Swing grey (#EEEEEE) on Windows. This asks
  * the screen, not the window: a frame published the moment the window is shown, as the app's first frame is, with no
  * later repaint or input, has to reach the canvas's pixels.
  *
  * It captures the screen, as [[SwingWindowProxyPaintSpec]] does, and finds the window by what it painted. A screen
  * that shows only the grey, or no trace of the frame, fails; a frame that arrives late is waited for.
  */
class SwingWindowFirstFrameScreenSpec extends AnyFlatSpec with Matchers:

  private val FrameColor = 0x2ea043
  private val SwingGrey  = 0xeeeeee

  private def screenCapture(): BufferedImage =
    new Robot().createScreenCapture(new Rectangle(Toolkit.getDefaultToolkit.getScreenSize))

  /** How many pixels of the capture are the colour of the frame, and how many are the default Swing grey. */
  private def tally(screen: BufferedImage): (Int, Int) =
    val pixels = screen.getRGB(0, 0, screen.getWidth, screen.getHeight, null, 0, screen.getWidth)
    pixels.foldLeft((0, 0)) {
      case ((frame, grey), argb) =>
        (argb & 0xffffff) match
          case FrameColor => (frame + 1, grey)
          case SwingGrey  => (frame, grey + 1)
          case _          => (frame, grey)
    }

  for mode <- List(WindowChromeMode.Auto, WindowChromeMode.Native, WindowChromeMode.Custom) do
    s"A frame published as the window is shown ($mode chrome)" should "reach the screen without any later repaint" in {
      assume(!GraphicsEnvironment.isHeadless, "Reading pixels back from the screen needs a display")
      assume(
        !System.getProperty("os.name", "").toLowerCase.contains("mac"),
        "macOS withholds other windows from a screen capture until the app is granted screen recording"
      )

      val (frameShare, greyShare, seen) = DisplayExclusive(
        SwingWindow
          .resource(chromeMode = mode)
          .use { window =>
            IO.blocking {
              val width  = window.canvas.getWidth.max(1)
              val height = window.canvas.getHeight.max(1)
              val frame  = window.acquireBaseImage(width, height, BufferedImage.TYPE_INT_RGB)
              val g      = frame.createGraphics()
              try
                g.setColor(new Color(FrameColor))
                g.fillRect(0, 0, width, height)
              finally g.dispose()
              // The way the render thread hands its first frame over: no repaint of our own, nothing waited for.
              window.onImageReady(frame)
              (width * height, () => tally(screenCapture()))
            }.flatMap { (canvasPixels, capture) =>
              AwaitCondition
                .awaitValue(IO.blocking(capture()), 10.seconds)((frame, _) => frame * 2 >= canvasPixels)
                .handleErrorWith(_ => IO.blocking(capture()))
                .map((frame, grey) =>
                  (
                    frame.toDouble / canvasPixels,
                    grey.toDouble / canvasPixels,
                    s"canvas $canvasPixels px, ${window.paintDiagnostics.line("after the wait")}"
                  )
                )
            }
          }
          .unsafeRunSync()
      )

      // The share is reported on every run so the bound below can be set from what Linux, Windows and macOS CI
      // measure. Until then half the canvas is a guess, not a measured bound (the owner's rule on numeric targets).
      info(
        f"$mode: the frame colour covered ${frameShare * 100}%.2f%% of the canvas area, Swing grey ${greyShare * 100}%.2f%%"
      )
      withClue(s"$seen; the screen showed ${(greyShare * 100).toInt}% Swing grey; ")(frameShare should be >= 0.5)
    }
