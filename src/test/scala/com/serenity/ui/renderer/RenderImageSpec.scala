package com.serenity.ui.renderer

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the render seam's own image handle (#1812 step 3): wrapping a Java2D image neither copies nor reallocates it
  * (layer buffers are recycled by identity), and the Java2D surface draws, composites and recycles exactly the pixels
  * behind the handle.
  */
class RenderImageSpec extends AnyFlatSpec with Matchers:

  private val font    = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)

  "RenderImage" should "wrap and unwrap the very same BufferedImage" in {
    val awt = new BufferedImage(37, 21, BufferedImage.TYPE_INT_ARGB)

    RenderImage.fromAwt(awt).toAwt should be theSameInstanceAs awt
  }

  it should "report the image's own pixel dimensions" in {
    val image = RenderImage.fromAwt(new BufferedImage(37, 21, BufferedImage.TYPE_INT_ARGB))

    (image.widthPx, image.heightPx) shouldBe (37, 21)
  }

  "Java2DRenderSurface.drawImage" should "paint the pixels behind the handle into the target cells" in {
    val frame   = new BufferedImage(40, 40, BufferedImage.TYPE_INT_ARGB)
    val surface = new Java2DRenderSurface(frame, metrics, font, _ => ())

    surface.drawImage(RenderImage.fromAwt(filled(4, 4, Color.GREEN)), 1, 1, 1, 1)

    frame.getRGB(15, 15) shouldBe Color.GREEN.getRGB
    new Color(frame.getRGB(5, 5), true).getAlpha shouldBe 0
  }

  "Java2DRenderSurface.compositeFullSurfaceLayer" should "blit the handle's pixels one-for-one" in {
    val frame   = new BufferedImage(40, 40, BufferedImage.TYPE_INT_ARGB)
    val surface = new Java2DRenderSurface(frame, metrics, font, _ => ())

    surface.compositeFullSurfaceLayer(RenderImage.fromAwt(filled(40, 40, Color.BLUE)))

    frame.getRGB(39, 39) shouldBe Color.BLUE.getRGB
  }

  "Java2DRenderSurface.newLayerSurface" should "flush into the recycled image it was handed back" in {
    val surface =
      new Java2DRenderSurface(new BufferedImage(40, 40, BufferedImage.TYPE_INT_ARGB), metrics, font, _ => ())
    val recycled = new BufferedImage(40, 40, BufferedImage.TYPE_INT_ARGB)
    val flushed  = new AtomicReference[Option[RenderImage]](None)

    val layer = surface.newLayerSurface(image => flushed.set(Some(image)), Some(RenderImage.fromAwt(recycled)))
    layer.pixels.fillPixelRect(0, 0, 5, 5, RenderColor.fromAwt(Color.RED))
    layer.flush()

    flushed.get().map(_.toAwt).exists(_ eq recycled) shouldBe true
    recycled.getRGB(2, 2) shouldBe Color.RED.getRGB
  }

  private def filled(width: Int, height: Int, color: Color): BufferedImage =
    val image    = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    try
      graphics.setColor(color)
      graphics.fillRect(0, 0, width, height)
    finally graphics.dispose()
    image
