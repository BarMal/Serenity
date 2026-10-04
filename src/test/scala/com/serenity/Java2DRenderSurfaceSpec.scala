package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{Java2DRenderSurface, RenderColor, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Java2DRenderSurface.strokeRect" should "respect the active alpha composite when drawing borders" in {
    val lowAlphaImage  = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val fullAlphaImage = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font           = new Font(Font.MONOSPACED, Font.PLAIN, 12)

    val lowAlphaSurface =
      new Java2DRenderSurface(lowAlphaImage, metrics, font, _ => ())
    lowAlphaSurface.setAlpha(0.25f)
    lowAlphaSurface.strokeRect(1, 1, 4, 3, color = RenderColor.fromAwt(java.awt.Color.WHITE), strokeWidth = 2.0f)
    lowAlphaSurface.flush()

    val fullAlphaSurface =
      new Java2DRenderSurface(fullAlphaImage, metrics, font, _ => ())
    fullAlphaSurface.setAlpha(1.0f)
    fullAlphaSurface.strokeRect(1, 1, 4, 3, color = RenderColor.fromAwt(java.awt.Color.WHITE), strokeWidth = 2.0f)
    fullAlphaSurface.flush()

    maxAlpha(lowAlphaImage) should be < maxAlpha(fullAlphaImage)
  }

  "Java2DRenderSurface.withRectClip" should "confine drawing to the clipped cells" in {
    val image   = new BufferedImage(120, 120, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(RenderColor.fromAwt(Color.WHITE))
    surface.withRectClip(x = 1, y = 1, width = 2, height = 2) {
      surface.fillPixelRect(0, 0, 120, 120, RenderColor.fromAwt(Color.BLACK))
    }
    surface.flush()

    new Color(image.getRGB(5, 5), true) shouldBe Color.WHITE
    new Color(image.getRGB(15, 15), true) shouldBe Color.BLACK
    new Color(image.getRGB(35, 35), true) shouldBe Color.WHITE
  }

  "Java2DRenderSurface.deviceImageDimension" should "scale logical pixels up to device pixels" in {
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 1024, deviceScale = 2.0) shouldBe 2048
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 801, deviceScale = 1.5) shouldBe 1202
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 0, deviceScale = 2.0) shouldBe 2
  }

  it should "keep viewport dimensions in logical cells for a high-DPI backing image" in {
    val image   = new BufferedImage(200, 100, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 20)
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      font,
      _ => (),
      logicalWidthPx = 100,
      logicalHeightPx = 50,
      deviceScaleX = 2.0,
      deviceScaleY = 2.0
    )

    surface.viewportWidth shouldBe 10
    surface.viewportHeight shouldBe 5
  }

  it should "preserve proportional glyph overhangs while clipping measured text to the cell grid" in {
    val image   = new BufferedImage(183, 70, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 40, ascent = 32)
    val font    = new Font(Font.SANS_SERIF, Font.BOLD, 40)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.setBackgroundColor(RenderColor.fromAwt(Color.WHITE))
    surface.setForegroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.drawRunPx(xPx = 10.0f, yPx = 5, bgWidthPx = 24.0f, lineHeightPx = 40, ascentPx = 32, s = "WWWWWW")
    surface.flush()

    val pixelsBeyondCellGrid =
      for
        y <- 5 until 45
        x <- 180 until image.getWidth
      yield (image.getRGB(x, y) >>> 24) & 0xff

    pixelsBeyondCellGrid.max shouldBe 0
  }

  it should "not cut off the right overhang of an italic prose glyph" in {
    val image   = new BufferedImage(80, 70, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 50, ascent = 38)
    val font    = new Font(Font.SANS_SERIF, Font.ITALIC, 40)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    val renderContext = surface.fontRenderContext.getOrElse(fail("Java2D surface must expose its font render context"))
    val backgroundWidth = font.getStringBounds("f", renderContext).getWidth.toFloat
    surface.setBackgroundColor(RenderColor.fromAwt(Color.WHITE))
    surface.setForegroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.drawRunPx(xPx = 20.0f, yPx = 5, bgWidthPx = backgroundWidth, lineHeightPx = 50, ascentPx = 38, s = "f")
    surface.flush()

    val pixelsInGlyphOverhang =
      for
        y <- 5 until 55
        x <- math.ceil(20.0f + backgroundWidth).toInt until 45
      yield (image.getRGB(x, y) >>> 24) & 0xff

    pixelsInGlyphOverhang.max should be > 0
  }

  it should "clip an italic selected glyph to its measured run" in {
    val image   = new BufferedImage(80, 70, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 50, ascent = 38)
    val font    = new Font(Font.SANS_SERIF, Font.ITALIC, 40)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    val renderContext = surface.fontRenderContext.getOrElse(fail("Java2D surface must expose its font render context"))
    val backgroundWidth = font.getStringBounds("f", renderContext).getWidth.toFloat
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(Color.BLUE))
    surface.drawRunPx(xPx = 20.0f, yPx = 5, bgWidthPx = backgroundWidth, lineHeightPx = 50, ascentPx = 38, s = "f")
    surface.setForegroundColor(RenderColor.fromAwt(Color.RED))
    surface.drawRunPx(
      xPx = 20.0f,
      yPx = 5,
      bgWidthPx = backgroundWidth,
      lineHeightPx = 50,
      ascentPx = 38,
      s = "f",
      clipGlyphToRun = true
    )
    surface.flush()

    val pixelsPastSelection =
      for
        y <- 5 until 55
        x <- math.ceil(20.0f + backgroundWidth).toInt until 45
      yield new Color(image.getRGB(x, y), true)

    pixelsPastSelection.forall(_.getRed == 0) shouldBe true
  }

  it should "use the canvas preferred size before Swing reports a non-zero runtime size" in {
    val canvas = new JPanel()
    canvas.setPreferredSize(new java.awt.Dimension(640, 480))

    val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = Java2DRenderSurface.forFrame(metrics, font, canvas, _ => ())

    surface.viewportWidth shouldBe 80
    surface.viewportHeight shouldBe 30
  }

  it should "accept a reusable frame-image provider with device-scaled dimensions" in {
    val canvas = new JPanel()
    canvas.setPreferredSize(new java.awt.Dimension(640, 480))
    val metrics  = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)
    val font     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val provided = AtomicReference[Option[BufferedImage]](None)
    val surface = Java2DRenderSurface.forFrame(
      metrics,
      font,
      canvas,
      _ => (),
      (width, height, imageType) =>
        val image = new BufferedImage(width, height, imageType)
        provided.set(Some(image))
        image
    )

    surface.viewportWidth shouldBe 80
    surface.viewportHeight shouldBe 30
    val image = provided.get().getOrElse(fail("frame image provider was not called"))
    image.getWidth shouldBe 640
    image.getHeight shouldBe 480
    image.getType shouldBe BufferedImage.TYPE_INT_RGB
  }

  "RendererEntryPoints.render" should "clear pixels outside the whole-cell grid to the theme background" in {
    val image   = new BufferedImage(83, 57, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        theme = Theme.light,
        config = AppConfig.default.withLineNumbers(false).withoutStatusLine
      )
    )

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(8, 5),
      font,
      font,
      metrics,
      None,
      com.serenity.state.manager.RenderCaches.create()
    )

    new Color(image.getRGB(82, 56), true) shouldBe Theme.light.background
  }

  // A background Color with alpha 0 is the transparency sentinel (#1240): `clearViewport`/`fillRect`/`putString` must
  // write genuinely zero-alpha pixels for it, not leave whatever opaque content a reused/pooled backing image already
  // held -- the default SRC_OVER composite makes a zero-alpha fill a no-op, which would silently fail to clear stale
  // opaque pixels from a previous frame.
  "Java2DRenderSurface.clearViewport" should "write fully transparent pixels over previously-opaque content" in {
    val image   = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(RenderColor.fromAwt(Color.RED))
    surface.flush()
    new Color(image.getRGB(5, 5), true).getAlpha shouldBe 255

    val reopened = new Java2DRenderSurface(image, metrics, font, _ => ())
    reopened.clearViewport(RenderColor.fromAwt(new Color(0, 0, 0, 0)))
    reopened.flush()

    new Color(image.getRGB(5, 5), true).getAlpha shouldBe 0
  }

  it should "leave an ordinary opaque clear fully opaque" in {
    val image   = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(RenderColor.fromAwt(Color.BLUE))
    surface.flush()

    new Color(image.getRGB(3, 3), true) shouldBe new Color(0, 0, 255, 255)
  }

  "Java2DRenderSurface.putString" should "clear a previously-opaque cell to transparent when the background is alpha 0" in {
    val image   = new BufferedImage(40, 20, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.setBackgroundColor(RenderColor.fromAwt(Color.RED))
    surface.putString(0, 0, "a")
    surface.flush()
    new Color(image.getRGB(2, 2), true).getAlpha shouldBe 255

    val reopened = new Java2DRenderSurface(image, metrics, font, _ => ())
    reopened.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
    reopened.setBackgroundColor(RenderColor.fromAwt(new Color(0, 0, 0, 0)))
    reopened.putString(0, 0, " ")
    reopened.flush()

    new Color(image.getRGB(2, 2), true).getAlpha shouldBe 0
  }

  private def maxAlpha(image: BufferedImage): Int =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    yield (image.getRGB(x, y) >>> 24) & 0xff).max
