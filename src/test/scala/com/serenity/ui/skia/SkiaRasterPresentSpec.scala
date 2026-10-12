package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import javax.swing.JPanel

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FrameSurfaceFactory, FrameSurfaceRequest, RenderSurface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `skia-raster` backend: Skia draws on the CPU into a retained surface, and the Java2D canvas presents the image
  * it is handed. No window is involved, so it runs headless; what is asserted is the image the canvas would paint.
  */
class SkiaRasterPresentSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val red     = RenderColor.fromAwt(Color.RED)
  private val blue    = RenderColor.fromAwt(Color.BLUE)

  final private class Window(widthPx: Int, heightPx: Int):
    val canvas: JPanel = new JPanel()
    canvas.setSize(widthPx, heightPx)

    val presented: scala.collection.mutable.ListBuffer[BufferedImage]  = scala.collection.mutable.ListBuffer.empty
    val acquired: scala.collection.mutable.ListBuffer[(Int, Int, Int)] = scala.collection.mutable.ListBuffer.empty

    private val pool = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)

    def request: FrameSurfaceRequest =
      FrameSurfaceRequest(
        metrics,
        font,
        canvas,
        onFlush = image =>
          val _ = presented += image
        ,
        acquireImage = (width, height, imageType) =>
          val _ = acquired += ((width, height, imageType))
          pool
      )

    def frame(factory: FrameSurfaceFactory)(draw: RenderSurface => Unit): BufferedImage =
      val surface = factory.create(request)
      draw(surface)
      surface.flush()
      presented.last

  private def rgbAt(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  "The skia-raster factory" should "present the pixels Skia drew in the image the window's pool handed out" in {
    val window = new Window(210, 130)
    val image  = window.frame(SkiaSurfaces.raster())(surface => surface.pixels.fillPixelRect(10, 20, 30, 40, red))

    window.acquired.toList shouldBe List((210, 130, BufferedImage.TYPE_INT_RGB))
    window.presented.toList should have size 1
    rgbAt(image, 15, 25) shouldBe 0xff0000
    rgbAt(image, 9, 25) shouldBe 0
    rgbAt(image, 40, 25) shouldBe 0
  }

  it should "keep what the previous frame drew, so a frame may repaint only what changed" in {
    val window  = new Window(210, 130)
    val factory = SkiaSurfaces.raster()
    window.frame(factory)(_.pixels.fillPixelRect(10, 20, 30, 40, red))
    val second = window.frame(factory)(_.pixels.fillPixelRect(100, 20, 30, 40, blue))

    rgbAt(second, 15, 25) shouldBe 0xff0000
    rgbAt(second, 105, 25) shouldBe 0x0000ff
  }

  it should "report one persistent content key for every frame of a canvas, and a new one when the canvas is resized" in {
    val window  = new Window(210, 130)
    val factory = SkiaSurfaces.raster()
    val first   = factory.create(window.request)
    val second  = factory.create(window.request)
    first.persistentContentKey should not be empty
    first.persistentContentKey shouldBe second.persistentContentKey

    window.canvas.setSize(220, 130)
    factory.create(window.request).persistentContentKey should not be first.persistentContentKey
  }

  it should "give every frame of a canvas the same layer cache owner" in {
    val window  = new Window(210, 130)
    val factory = SkiaSurfaces.raster()
    factory.create(window.request).layerCacheOwner shouldBe factory.create(window.request).layerCacheOwner
  }

  it should "draw text into the presented image" in {
    val window = new Window(210, 130)
    val image = window.frame(SkiaSurfaces.raster()) { surface =>
      surface.setBackgroundColor(RenderColor.Black)
      surface.setForegroundColor(RenderColor.White)
      surface.text.drawRunPx(14f, 26, 70f, metrics.lineHeight, metrics.ascent, "Hello")
    }

    val inked = for
      y <- 26 until 26 + metrics.lineHeight
      x <- 14 until 84
      if rgbAt(image, x, y) != 0
    yield (x, y)
    inked should not be empty
  }

  "The skia-raster factory's hosted canvas" should "be absent: the Java2D canvas stays in the window" in {
    SkiaSurfaces.raster().hostedCanvas shouldBe None
  }
