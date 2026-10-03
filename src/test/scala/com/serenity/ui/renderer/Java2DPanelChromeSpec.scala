package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Font}

import com.serenity.config.AppConfig
import com.serenity.ui.layout.{CellMetrics, LayoutRect, PixelRect}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The panel shadow and body paint far fewer translucent pixels than they used to; these pin that the picture did not
  * change beyond rounding, against a reference that paints the way the renderer used to.
  */
class Java2DPanelChromeSpec extends AnyFlatSpec with Matchers:

  private val metrics       = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font          = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val logicalWidth  = 300
  private val logicalHeight = 260
  private val tolerance     = 2
  private val shadowColour  = new Color(0, 0, 0)
  private val backdrop      = new Color(24, 28, 36)
  private val panelColour   = new Color(17, 24, 33)
  private val panelAlpha    = 0.94f
  private val cellRect      = LayoutRect(3, 2, 30, 15)
  private val arcPx         = 8

  for scale <- List(1.0, 2.0) do
    "Java2DRenderSurface.drawRoundRectShadow" should s"match the stacked translucent fills over a busy backdrop at scale $scale" in {
      val expected = noisyImage(scale)
      paintLegacy(expected, scale)(g => Java2DPanelChrome.drawShadowLayers(g, pixelRect, arcPx, shadowColour))

      val actual = noisyImage(scale)
      paintSurface(actual, scale)(
        _.drawRoundRectShadow(cellRect.x, cellRect.y, cellRect.width, cellRect.height, arcPx, shadowColour)
      )

      maxChannelDifference(expected, actual) should be <= tolerance
    }

    it should s"match the stacked translucent fills inside a transparent layer at scale $scale" in {
      val expected = transparentImage(scale)
      paintLegacy(expected, scale)(g => Java2DPanelChrome.drawShadowLayers(g, pixelRect, arcPx, shadowColour))

      val actual = transparentImage(scale)
      paintSurface(actual, scale)(
        _.drawRoundRectShadow(cellRect.x, cellRect.y, cellRect.width, cellRect.height, arcPx, shadowColour)
      )

      maxChannelDifference(expected, actual) should be <= tolerance
    }

    "Java2DRenderSurface.fillPanelBody" should s"match the shadow plus per-row body fills over a known opaque backdrop at scale $scale" in {
      val expected = backdropImage(scale)
      paintLegacy(expected, scale)(legacyShadowAndBody)

      val actual = backdropImage(scale)
      paintSurface(actual, scale)(
        _.fillPanelBody(
          cellRect.x,
          cellRect.y,
          cellRect.width,
          cellRect.height,
          arcPx,
          Some(shadowColour),
          PanelBodyFill(panelColour, panelAlpha, Some(backdrop))
        )
      )

      maxChannelDifference(expected, actual) should be <= tolerance
    }

    it should s"match the shadow plus per-row body fills when nothing is known beneath at scale $scale" in {
      val expected = noisyImage(scale)
      paintLegacy(expected, scale)(legacyShadowAndBody)

      val actual = noisyImage(scale)
      paintSurface(actual, scale)(
        _.fillPanelBody(
          cellRect.x,
          cellRect.y,
          cellRect.width,
          cellRect.height,
          arcPx,
          Some(shadowColour),
          PanelBodyFill(panelColour, panelAlpha, None)
        )
      )

      maxChannelDifference(expected, actual) should be <= tolerance
    }

    it should s"match an unshadowed body over a known opaque backdrop at scale $scale" in {
      val expected = backdropImage(scale)
      paintLegacy(expected, scale)(legacyBody)

      val actual = backdropImage(scale)
      paintSurface(actual, scale)(
        _.fillPanelBody(
          cellRect.x,
          cellRect.y,
          cellRect.width,
          cellRect.height,
          arcPx,
          None,
          PanelBodyFill(panelColour, panelAlpha, Some(backdrop))
        )
      )

      maxChannelDifference(expected, actual) should be <= tolerance
    }

    "PinnedPanelRenderer.render" should s"paint a docked panel as it did with the shadow stack and per-row body fills at scale $scale" in {
      val theme = Theme.dark.copy(background = backdrop)
      val panel = TextPanelView(rect = cellRect, contentRect = None, title = "Outline", rows = Nil)

      val expected = backdropImage(scale)
      val legacy   = new LegacyChromeSurface(expected, scale)
      PinnedPanelRenderer.render(legacy, panel, theme, AppConfig.default, metrics)
      legacy.flush()

      val actual  = backdropImage(scale)
      val surface = newSurface(actual, scale)
      PinnedPanelRenderer.render(surface, panel, theme, AppConfig.default, metrics, opaqueBeneath = Some(backdrop))
      surface.flush()

      maxChannelDifference(expected, actual) should be <= tolerance
    }

  private def pixelRect: PixelRect =
    PixelRect(
      metrics.toPixelX(cellRect.x),
      metrics.toPixelY(cellRect.y),
      cellRect.width * metrics.charWidth,
      cellRect.height * metrics.lineHeight
    )

  private def legacyShadowAndBody(g: java.awt.Graphics2D): Unit =
    Java2DPanelChrome.drawShadowLayers(g, pixelRect, arcPx, shadowColour)
    legacyBody(g)

  private def legacyBody(g: java.awt.Graphics2D): Unit =
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, panelAlpha))
    g.setColor(panelColour)
    (0 until cellRect.height).foreach { row =>
      g.fillRect(pixelRect.xPx, pixelRect.yPx + row * metrics.lineHeight, pixelRect.widthPx, metrics.lineHeight)
    }

  /** Paints the shadow as the stacked fills and the body row by row, the way the renderer did before. */
  private class LegacyChromeSurface(image: BufferedImage, scale: Double)
      extends Java2DRenderSurface(image, metrics, font, _ => (), logicalWidth, logicalHeight, scale, scale):
    override def panelBodies: Option[PanelBodyDrawing] = None

    override def drawRoundRectShadow(x: Int, y: Int, width: Int, height: Int, arcPx: Int, color: Color): Unit =
      val rect =
        PixelRect(metrics.toPixelX(x), metrics.toPixelY(y), width * metrics.charWidth, height * metrics.lineHeight)
      paintLegacy(image, scale)(Java2DPanelChrome.drawShadowLayers(_, rect, arcPx, color))

  private def newSurface(image: BufferedImage, scale: Double): Java2DRenderSurface =
    new Java2DRenderSurface(image, metrics, font, _ => (), logicalWidth, logicalHeight, scale, scale)

  private def paintSurface(image: BufferedImage, scale: Double)(paint: Java2DRenderSurface => Unit): Unit =
    val surface = newSurface(image, scale)
    paint(surface)
    surface.flush()

  private def paintLegacy(image: BufferedImage, scale: Double)(paint: java.awt.Graphics2D => Unit): Unit =
    val g = image.createGraphics()
    try
      g.scale(scale, scale)
      paint(g)
    finally g.dispose()

  private def blankImage(scale: Double): BufferedImage =
    new BufferedImage(
      Java2DRenderSurface.deviceImageDimension(logicalWidth, scale),
      Java2DRenderSurface.deviceImageDimension(logicalHeight, scale),
      BufferedImage.TYPE_INT_ARGB
    )

  private def transparentImage(scale: Double): BufferedImage = blankImage(scale)

  private def backdropImage(scale: Double): BufferedImage =
    val image = blankImage(scale)
    val g     = image.createGraphics()
    try
      g.setColor(backdrop)
      g.fillRect(0, 0, image.getWidth, image.getHeight)
    finally g.dispose()
    image

  private def noisyImage(scale: Double): BufferedImage =
    val image  = blankImage(scale)
    val random = new scala.util.Random(image.getWidth)
    for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    do image.setRGB(x, y, random.nextInt() | 0xff000000)
    image

  private def maxChannelDifference(expected: BufferedImage, actual: BufferedImage): Int =
    val differences =
      for
        y     <- 0 until expected.getHeight
        x     <- 0 until expected.getWidth
        shift <- List(0, 8, 16, 24)
      yield math.abs(((expected.getRGB(x, y) >>> shift) & 0xff) - ((actual.getRGB(x, y) >>> shift) & 0xff))
    differences.max
