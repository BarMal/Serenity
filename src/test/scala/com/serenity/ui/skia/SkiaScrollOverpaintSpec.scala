package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.SkiaPixels
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FrameSurfaceRequest, RenderImage, RenderSurface}
import com.serenity.ui.terminal.SwingWindow.ReusableImagePool
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Frames that scroll rows and then paint over them, shown in the window's two alternating images. The image follows
  * the surface by moving rows, so each kind of paint over a moved row must reach it from the surface: after every frame
  * the image shown equals the surface.
  */
class SkiaScrollOverpaintSpec extends AnyFlatSpec with Matchers:

  private val width   = 140
  private val height  = 120
  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)

  private def colour(index: Int): RenderColor =
    RenderColor.fromRgba(30 + index * 23 % 200, 60 + index * 41 % 180, 90 + index * 17 % 150)

  final private class Window:
    val canvas = new JPanel
    canvas.setSize(width, height)
    val pool    = new ReusableImagePool()
    val caches  = RenderCaches.create()
    val changed = new AtomicReference[Option[List[PixelRect]]](None)
    val shown   = new AtomicReference[Option[BufferedImage]](None)

    private def request: FrameSurfaceRequest =
      FrameSurfaceRequest(
        metrics,
        new Font(Font.MONOSPACED, Font.PLAIN, 12),
        canvas,
        image =>
          pool.publish(image)
          shown.set(Some(image))
        ,
        pool.acquire,
        caches.surfaceText,
        caches.backendCaches,
        () => changed.get()
      )

    def surface(): RenderSurface = SkiaRasterSurfaceFactory.create(request)

    def retained: Array[Int] =
      SkiaPixels
        .read(SkiaRasterSurfaceFactory.retainedSurface(caches.backendCaches).getOrElse(fail("no surface")))
        .map(_ & 0xffffff)

  private def pixelsOf(image: BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)

  /** Paints a frame: the rows it moves, then `paint`; the damage it reports is the rows it moved to and `painted`. */
  private def frame(window: Window, moves: List[(PixelRect, Int)], painted: List[PixelRect])(
    paint: RenderSurface => Unit
  ): Unit =
    val moved = moves.map((area, dy) => area.copy(yPx = area.yPx + dy))
    window.changed.set(Some(moved ++ painted))
    val surface = window.surface()
    moves.foreach((area, dy) => surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(area, dy))
    paint(surface)
    surface.flush()
    pixelsOf(window.shown.get().getOrElse(fail("never presented"))) shouldBe window.retained

  private def stripes(window: Window): Unit =
    window.changed.set(None)
    val surface = window.surface()
    (0 until height / 10).foreach(row => surface.pixels.fillPixelRect(0, row * 10, width, 10, colour(row)))
    surface.flush()

  private val wholeWindow = PixelRect(0, 0, width, height)

  "A frame that scrolls and paints over the rows it moved" should "show the surface after a fill over them" in {
    val window = new Window
    stripes(window)
    val badge = PixelRect(100, 30, 20, 10)
    (1 to 6).foreach { step =>
      frame(window, List(wholeWindow -> -10), List(badge, PixelRect(0, height - 10, width, 10))) { surface =>
        surface.pixels.fillPixelRect(0, height - 10, width, 10, colour(step + 20))
        surface.pixels.fillPixelRect(badge.xPx, badge.yPx, badge.widthPx, badge.heightPx, colour(step + 40))
      }
    }
  }

  it should "show the surface after text, a stroke and an image over them" in {
    val window = new Window
    stripes(window)
    val image = new BufferedImage(3, 3, BufferedImage.TYPE_INT_ARGB)
    (0 until 3).foreach(i => image.setRGB(i, i, 0xff00ff00))
    (1 to 6).foreach { step =>
      val text   = PixelRect(10, 40, 60, 13)
      val stroke = PixelRect(84, 20, 42, 26)
      val icon   = PixelRect(14, 80, 14, 13)
      frame(window, List(wholeWindow -> (if step % 2 == 0 then 10 else -10)), List(text, stroke, icon)) { surface =>
        surface.setBackgroundColor(colour(step))
        surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
        surface.text.drawRunPx(10f, 40, 60f, metrics.lineHeight, metrics.ascent, "scroll")
        surface.panelOutlines
          .getOrElse(fail("expected outlines"))
          .strokeRect(12, 2, 6, 2, RenderColor.fromAwt(Color.ORANGE), 2f)
        surface.pixels.drawImage(RenderImage.fromAwt(image), 2, 6, 2, 1)
      }
    }
  }

  it should "show the surface after pixels lifted before the move are put back over the rows it moved" in {
    val window = new Window
    stripes(window)
    val area = PixelRect(30, 50, 50, 20)
    (1 to 6).foreach { _ =>
      window.changed.set(Some(List(wholeWindow)))
      val surface = window.surface()
      val lifting = surface.pixelLifting.getOrElse(fail("expected lifting"))
      val lifted  = lifting.lift(area, None)
      surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(PixelRect(0, 0, width, 100), -10)
      lifting.putBack(lifted)
      surface.flush()
      pixelsOf(window.shown.get().getOrElse(fail("never presented"))) shouldBe window.retained
    }
  }

  it should "show the surface when two moves in one frame overlap and one is painted over" in {
    val window = new Window
    stripes(window)
    (1 to 6).foreach { step =>
      frame(
        window,
        List(PixelRect(0, 0, width, 80) -> -10, PixelRect(0, 60, width, 60) -> 20),
        List(PixelRect(20, 60, 30, 10))
      )(surface => surface.pixels.fillPixelRect(20, 60, 30, 10, colour(step + 7)))
    }
  }

  it should "show the surface after a frame that paints before it moves" in {
    val window = new Window
    stripes(window)
    frame(window, Nil, Nil)(_ => ())
    frame(window, Nil, Nil)(_ => ())
    (1 to 4).foreach { step =>
      window.changed.set(Some(List(wholeWindow)))
      val surface = window.surface()
      surface.pixels.fillPixelRect(5, 5 + step * 10, 50, 10, colour(step + 3))
      surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(wholeWindow, -10)
      surface.flush()
      pixelsOf(window.shown.get().getOrElse(fail("never presented"))) shouldBe window.retained
    }
  }
