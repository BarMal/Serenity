package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.Java2DRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Layer buffers: how a modal/panel layer surface is built, recycled, blurs what's behind it, and which window it
  * caches against.
  */
class Java2DLayerSurfaceSpec extends AnyFlatSpec with Matchers:

  "Java2DRenderSurface.forLayer" should "build a surface at the given logical size and device scale without a JPanel" in {
    val metrics    = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font       = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val flushedRef = new AtomicReference[Option[BufferedImage]](None)

    val layer = Java2DRenderSurface.forLayer(
      metrics,
      font,
      logicalWidthPx = 100,
      logicalHeightPx = 50,
      deviceScaleX = 2.0,
      deviceScaleY = 2.0,
      onFlush = image => flushedRef.set(Some(image))
    )
    layer.viewportWidth shouldBe 10
    layer.viewportHeight shouldBe 5

    layer.flush()

    flushedRef.get().map(_.getWidth) shouldBe Some(200)
    flushedRef.get().map(_.getHeight) shouldBe Some(100)
  }

  it should "start fully transparent" in {
    val metrics    = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font       = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val flushedRef = new AtomicReference[Option[BufferedImage]](None)

    val layer = Java2DRenderSurface.forLayer(metrics, font, 40, 40, 1.0, 1.0, image => flushedRef.set(Some(image)))
    layer.flush()

    (new Color(flushedRef.get().get.getRGB(5, 5), true)).getAlpha shouldBe 0
  }

  "Java2DRenderSurface.forLayer" should "paint into a recycled image of the same size, cleared first" in {
    val metrics  = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val recycled = filledImage(20, 20, Color.RED)

    val flushedRef = new AtomicReference[Option[BufferedImage]](None)
    val layer = Java2DRenderSurface.forLayer(
      metrics,
      font,
      logicalWidthPx = 20,
      logicalHeightPx = 20,
      deviceScaleX = 1.0,
      deviceScaleY = 1.0,
      onFlush = image => flushedRef.set(Some(image)),
      recycled = Some(recycled)
    )
    layer.flush()

    flushedRef.get().exists(_ eq recycled) shouldBe true
    new Color(recycled.getRGB(5, 5), true).getAlpha shouldBe 0
  }

  it should "allocate afresh when the recycled image no longer matches the layer's size" in {
    val metrics  = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val recycled = filledImage(10, 10, Color.RED)

    val flushedRef = new AtomicReference[Option[BufferedImage]](None)
    val layer = Java2DRenderSurface.forLayer(
      metrics,
      font,
      logicalWidthPx = 20,
      logicalHeightPx = 20,
      deviceScaleX = 1.0,
      deviceScaleY = 1.0,
      onFlush = image => flushedRef.set(Some(image)),
      recycled = Some(recycled)
    )
    layer.flush()

    flushedRef.get().exists(_ eq recycled) shouldBe false
    flushedRef.get().map(image => (image.getWidth, image.getHeight)) shouldBe Some((20, 20))
    new Color(recycled.getRGB(5, 5), true) shouldBe Color.RED
  }

  "Java2DRenderSurface.newLayerSurface" should "stay transparent wherever it did not paint, compositing exactly like painting directly" in {
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val frame   = stripedImage(40, 40)
    val surface = new Java2DRenderSurface(frame, metrics, font, _ => ())

    val flushedRef = new AtomicReference[Option[BufferedImage]](None)
    val layer = surface.layerBuffers
      .getOrElse(fail("expected layer buffer support"))
      .newLayerSurface(image => flushedRef.set(Some(image)))
    layer.pixels.fillPixelRect(0, 0, 20, 20, Color.RED)
    layer.flush()
    val layerPixels = flushedRef.get().getOrElse(fail("layer was not flushed"))
    new Color(layerPixels.getRGB(30, 30), true).getAlpha shouldBe 0
    frame.getRGB(10, 10) shouldBe stripedImage(40, 40).getRGB(10, 10)

    val composited = stripedImage(40, 40)
    val graphics   = composited.createGraphics()
    try graphics.drawImage(layerPixels, 0, 0, null)
    finally graphics.dispose()
    val direct = stripedImage(40, 40)
    new Java2DRenderSurface(direct, metrics, font, _ => ()).fillPixelRect(0, 0, 20, 20, Color.RED)

    for
      x <- 0 until 40
      y <- 0 until 40
    do composited.getRGB(x, y) shouldBe direct.getRGB(x, y)
  }

  "Java2DRenderSurface.layerCacheOwner" should "be the canvas for every frame surface built over it" in {
    val canvas = new JPanel()
    canvas.setPreferredSize(new java.awt.Dimension(80, 60))
    val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)

    val first  = Java2DRenderSurface.forFrame(metrics, font, canvas, _ => ())
    val second = Java2DRenderSurface.forFrame(metrics, font, canvas, _ => ())
    val other  = Java2DRenderSurface.forFrame(metrics, font, new JPanel(), _ => ())

    first.layerCacheOwner shouldBe second.layerCacheOwner
    first.layerCacheOwner should not be other.layerCacheOwner
  }

  "Java2DRenderSurface.layerBuffers" should "expose a capability that builds a same-shaped layer surface" in {
    val image   = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    val flushedRef = new AtomicReference[Option[BufferedImage]](None)
    val layer = surface.layerBuffers.getOrElse(fail("expected layer buffer support")).newLayerSurface { image =>
      flushedRef.set(Some(image))
    }
    layer.viewportWidth shouldBe surface.viewportWidth
    layer.viewportHeight shouldBe surface.viewportHeight

    layer.flush()

    flushedRef.get() shouldBe defined
  }

  private def filledImage(width: Int, height: Int, color: Color): BufferedImage =
    val image    = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    try
      graphics.setColor(color)
      graphics.fillRect(0, 0, width, height)
    finally graphics.dispose()
    image

  /** Opaque vertical red/blue stripes, so a blur visibly mixes neighbouring columns. */
  private def stripedImage(width: Int, height: Int): BufferedImage =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for
      x <- 0 until width
      y <- 0 until height
    do image.setRGB(x, y, if (x / 3) % 2 == 0 then Color.RED.getRGB else Color.BLUE.getRGB)
    image
