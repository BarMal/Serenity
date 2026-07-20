package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import javax.swing.JPanel

import com.serenity.config.{AppConfig, PostProcessingEffect}
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{Java2DRenderSurface, Renderer}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Java2DRenderSurface.strokeRoundRect" should "respect the active alpha composite when drawing borders" in {
    val lowAlphaImage  = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val fullAlphaImage = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font           = new Font(Font.MONOSPACED, Font.PLAIN, 12)

    val lowAlphaSurface =
      new Java2DRenderSurface(lowAlphaImage, metrics, font, _ => ())
    lowAlphaSurface.setAlpha(0.25f)
    lowAlphaSurface.strokeRoundRect(1, 1, 4, 3, arcPx = 0, color = java.awt.Color.WHITE, strokeWidth = 2.0f)
    lowAlphaSurface.flush()

    val fullAlphaSurface =
      new Java2DRenderSurface(fullAlphaImage, metrics, font, _ => ())
    fullAlphaSurface.setAlpha(1.0f)
    fullAlphaSurface.strokeRoundRect(1, 1, 4, 3, arcPx = 0, color = java.awt.Color.WHITE, strokeWidth = 2.0f)
    fullAlphaSurface.flush()

    maxAlpha(lowAlphaImage) should be < maxAlpha(fullAlphaImage)
  }

  "Java2DRenderSurface.blurRegion" should "respect an active rounded clip" in {
    val image   = new BufferedImage(120, 120, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.WHITE)
    surface.fillPixelRect(25, 25, 10, 10, Color.BLACK)
    surface.withRoundRectClip(x = 1, y = 1, width = 10, height = 10, arcPx = 50) {
      surface.blurRegion(x = 1, y = 1, width = 10, height = 10, radius = 1.0f)
    }
    surface.flush()

    new Color(image.getRGB(20, 20), true) shouldBe Color.WHITE
  }

  it should "blur the translated device region for fractional floating offsets" in {
    val image   = new BufferedImage(12, 14, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.GREEN)
    (5 to 10).foreach(y => image.setRGB(6, y, Color.BLUE.getRGB))
    image.setRGB(6, 7, Color.RED.getRGB)

    surface.withPixelTranslation(0.0, 5.5) {
      surface.blurRegion(x = 0, y = 0, width = 12, height = 5, radius = 0.1f)
    }
    surface.flush()

    new Color(image.getRGB(6, 7), true) should not be Color.RED
    new Color(image.getRGB(6, 2), true) shouldBe Color.GREEN
  }

  "Java2DRenderSurface.deviceImageDimension" should "scale logical pixels up to device pixels" in {
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 1024, deviceScale = 2.0) shouldBe 2048
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 801, deviceScale = 1.5) shouldBe 1202
    Java2DRenderSurface.deviceImageDimension(logicalDimensionPx = 0, deviceScale = 2.0) shouldBe 2
  }

  "Java2DRenderSurface.deviceRegionFor" should "map logical pixel regions to clamped device pixels" in {
    Java2DRenderSurface.deviceRegionFor(
      logicalX = 10,
      logicalY = 5,
      logicalWidth = 20,
      logicalHeight = 10,
      imageWidth = 200,
      imageHeight = 100,
      deviceScaleX = 2.0,
      deviceScaleY = 1.5
    ) shouldBe Some(Java2DRenderSurface.DeviceRegion(xPx = 20, yPx = 7, widthPx = 40, heightPx = 16))
  }

  it should "discard device regions outside the backing image" in {
    Java2DRenderSurface.deviceRegionFor(
      logicalX = 50,
      logicalY = 50,
      logicalWidth = 10,
      logicalHeight = 10,
      imageWidth = 30,
      imageHeight = 30,
      deviceScaleX = 2.0,
      deviceScaleY = 2.0
    ) shouldBe None
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

  it should "clip measured text runs to their background width" in {
    val image   = new BufferedImage(180, 70, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 40, ascent = 32)
    val font    = new Font(Font.SANS_SERIF, Font.BOLD, 40)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.setBackgroundColor(Color.WHITE)
    surface.setForegroundColor(Color.BLACK)
    surface.drawRunPx(xPx = 10.0f, yPx = 5, bgWidthPx = 24.0f, lineHeightPx = 40, ascentPx = 32, s = "WWWWWW")
    surface.flush()

    val pixelsBeyondRun =
      for
        y <- 5 until 45
        x <- 35 until image.getWidth
      yield (image.getRGB(x, y) >>> 24) & 0xff

    pixelsBeyondRun.max shouldBe 0
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

  it should "darken alternating device rows for the scanline post-process" in {
    val image   = new BufferedImage(8, 8, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.WHITE)
    surface.applyPostProcessing(PostProcessingEffect.Scanlines)
    surface.flush()

    new Color(image.getRGB(0, 1), true).getRed should be < new Color(image.getRGB(0, 0), true).getRed
    new Color(image.getRGB(0, 4), true).getRed should be < new Color(image.getRGB(0, 3), true).getRed
  }

  it should "add a phosphor mask to the scanline post-process" in {
    val image   = new BufferedImage(9, 9, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.WHITE)
    surface.applyPostProcessing(PostProcessingEffect.Scanlines)
    surface.flush()

    val firstPhosphor  = new Color(image.getRGB(0, 0), true)
    val secondPhosphor = new Color(image.getRGB(1, 0), true)
    firstPhosphor.getRed should not be secondPhosphor.getRed
  }

  it should "spread bright UI pixels into a glow" in {
    val image   = new BufferedImage(9, 9, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.BLACK)
    surface.fillPixelRect(4, 4, 1, 1, Color.WHITE)
    surface.applyPostProcessing(PostProcessingEffect.Glow)
    surface.flush()

    new Color(image.getRGB(3, 4), true).getRed should be > 0
    new Color(image.getRGB(4, 4), true).getRed should be >= 250
  }

  it should "extend the glow halo beyond immediately adjacent pixels" in {
    val image   = new BufferedImage(11, 11, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.BLACK)
    surface.fillPixelRect(5, 5, 1, 1, Color.WHITE)
    surface.applyPostProcessing(PostProcessingEffect.Glow)
    surface.flush()

    new Color(image.getRGB(3, 5), true).getRed should be > 0
  }

  it should "spread dark glyphs into a halo on a light background" in {
    val image   = new BufferedImage(11, 11, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())

    surface.clearViewport(Color.WHITE)
    surface.fillPixelRect(5, 5, 1, 1, Color.BLACK)
    surface.applyPostProcessing(PostProcessingEffect.Glow)
    surface.flush()

    new Color(image.getRGB(3, 5), true).getRed should be < 255
    new Color(image.getRGB(5, 5), true) shouldBe Color.BLACK
  }

  "Renderer.render" should "clear pixels outside the whole-cell grid to the theme background" in {
    val image   = new BufferedImage(83, 57, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())
    val state = AppState.initial.copy(
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(8, 5), font, font, metrics, None)

    new Color(image.getRGB(82, 56), true) shouldBe Theme.light.background
  }

  it should "apply the configured post-process after rendering the frame" in {
    val image   = new BufferedImage(83, 57, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => ())
    val state = AppState.initial.copy(
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withPostProcessingEffect(PostProcessingEffect.Scanlines)
    )

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(8, 5), font, font, metrics, None)

    new Color(image.getRGB(82, 55), true).getRed should be < new Color(image.getRGB(82, 56), true).getRed
  }

  private def maxAlpha(image: BufferedImage): Int =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    yield (image.getRGB(x, y) >>> 24) & 0xff).max
