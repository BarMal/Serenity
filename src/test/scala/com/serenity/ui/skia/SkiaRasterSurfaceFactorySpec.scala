package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FrameSurfaceRequest, RenderSurface}
import com.serenity.ui.terminal.SwingWindow.ReusableImagePool
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaRasterSurfaceFactorySpec extends AnyFlatSpec with Matchers:

  private val red    = RenderColor.fromRgba(200, 30, 30)
  private val green  = RenderColor.fromRgba(30, 200, 30)
  private val blue   = RenderColor.fromRgba(30, 30, 200)
  private val yellow = RenderColor.fromRgba(220, 220, 30)

  private val first  = PixelRect(0, 0, 60, 10)
  private val second = PixelRect(10, 40, 50, 10)
  private val third  = PixelRect(20, 80, 40, 10)

  /** A window canvas, its two-image pool and the owner's caches: what a `SwingWindow` hands the factory. */
  final private class Window(widthPx: Int, heightPx: Int):
    val canvas = new JPanel
    canvas.setSize(widthPx, heightPx)
    val pool                                          = new ReusableImagePool()
    val caches                                        = RenderCaches.create()
    val changed                                       = new AtomicReference[Option[List[PixelRect]]](None)
    val shown: AtomicReference[Option[BufferedImage]] = new AtomicReference(None)

    def resize(widthPx: Int, heightPx: Int): Unit = canvas.setSize(widthPx, heightPx)

    private def request: FrameSurfaceRequest =
      FrameSurfaceRequest(
        CellMetrics(8, 16, 12),
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

    def copiedPixels: Option[Long] = SkiaRasterSurfaceFactory.lastCopiedPixels(caches.backendCaches)

  /** What a fill records as painted: its rectangle and the pixel a fractional edge could spill into. */
  private def paintedArea(rect: PixelRect): Int = (rect.widthPx + 1) * (rect.heightPx + 1)

  private def pixelsOf(image: BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)

  /** What the window should show: every fill so far, applied to a black canvas. */
  final private class Model(widthPx: Int, heightPx: Int):
    private val pixels = new Array[Int](widthPx * heightPx)

    def fill(rect: PixelRect, color: RenderColor): Unit =
      for
        y <- rect.yPx until (rect.yPx + rect.heightPx).min(heightPx)
        x <- rect.xPx until (rect.xPx + rect.widthPx).min(widthPx)
      do pixels(y * widthPx + x) = color.argb & 0xffffff

    def snapshot: Array[Int] = pixels.clone()

  private def paint(
    window: Window,
    model: Model,
    fills: List[(PixelRect, RenderColor)],
    damage: Option[List[PixelRect]]
  ): BufferedImage =
    window.changed.set(damage)
    val surface = window.surface()
    fills.foreach {
      case (rect, color) =>
        surface.pixels.fillPixelRect(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx, color)
        model.fill(rect, color)
    }
    surface.flush()
    window.shown.get().getOrElse(fail("the frame was never presented"))

  "SkiaRasterSurfaceFactory" should "present the pixels the surface painted" in {
    val window = new Window(100, 100)
    val model  = new Model(100, 100)
    val image  = paint(window, model, List(first -> red), None)
    image.getType shouldBe BufferedImage.TYPE_INT_RGB
    pixelsOf(image) shouldBe model.snapshot
  }

  it should "keep every presented frame equal to a whole-frame copy across a changing sequence" in {
    val window = new Window(100, 100)
    val model  = new Model(100, 100)
    val frames: List[(List[(PixelRect, RenderColor)], Option[List[PixelRect]])] = List(
      List(first -> red)    -> None,
      List(second -> green) -> Some(List(second)),
      List(third -> blue)   -> Some(List(third)),
      List(first -> yellow) -> Some(List(first)),
      List(second -> red)   -> Some(List(second)),
      List(third -> green)  -> Some(List(third)),
      Nil                   -> Some(Nil),
      List(first -> blue)   -> Some(List(first))
    )
    frames.zipWithIndex.foreach {
      case ((fills, damage), index) =>
        val image = paint(window, model, fills, damage)
        withClue(s"frame $index: ")(pixelsOf(image) shouldBe model.snapshot)
    }
  }

  it should "copy only the damage of the last two frames once the pool alternates" in {
    val window = new Window(100, 100)
    val model  = new Model(100, 100)
    val _      = paint(window, model, List(first -> red), None)
    window.copiedPixels shouldBe Some(100L * 100)
    val _ = paint(window, model, List(second -> green), Some(List(second)))
    window.copiedPixels shouldBe Some(100L * 100)
    val image = paint(window, model, List(third -> blue), Some(List(third)))
    window.copiedPixels shouldBe Some((paintedArea(third) + paintedArea(second)).toLong)
    pixelsOf(image) shouldBe model.snapshot
  }

  it should "copy the whole frame when the damage is unknown, and after a resize" in {
    val window = new Window(100, 100)
    val model  = new Model(100, 100)
    val _      = paint(window, model, List(first -> red), None)
    val _      = paint(window, model, List(second -> green), Some(List(second)))
    val _      = paint(window, model, List(third -> blue), Some(List(third)))
    val _      = paint(window, model, List(first -> yellow), None)
    window.copiedPixels shouldBe Some(100L * 100)

    window.resize(120, 110)
    val resized = new Model(120, 110)
    val image   = paint(window, resized, List(first -> red), Some(List(first)))
    window.copiedPixels shouldBe Some(120L * 110)
    pixelsOf(image) shouldBe resized.snapshot
  }

  it should "copy the whole frame when the previous frame was never presented" in {
    val window  = new Window(100, 100)
    val model   = new Model(100, 100)
    val _       = paint(window, model, List(first -> red), None)
    val _       = paint(window, model, List(second -> green), Some(List(second)))
    val dropped = window.surface()
    dropped.pixels.fillPixelRect(third.xPx, third.yPx, third.widthPx, third.heightPx, blue)
    model.fill(third, blue)
    val image = paint(window, model, List(first -> yellow), Some(List(first)))
    window.copiedPixels shouldBe Some(100L * 100)
    pixelsOf(image) shouldBe model.snapshot
  }

  it should "keep its text caches in the owner's backend slot" in {
    val window = new Window(100, 100)
    val _      = window.surface()
    SkiaTextCaches.heldBy(window.caches.backendCaches) shouldBe true
  }
