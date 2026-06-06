package com.serenity.ui.renderer

import java.awt.*
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.image.{BufferedImage, ConvolveOp, Kernel}
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle

/** A RenderSurface backed by a BufferedImage via Graphics2D.
  *
  * All coordinates are in cell units (column, row). Pixel conversion uses CellMetrics. After all drawing is complete,
  * call flush() to hand the finished image to onFlush.
  *
  * Threading: draw methods are called from the Cats Effect thread pool (off-EDT). onFlush is responsible for scheduling
  * the EDT repaint (e.g. via SwingWindow.onImageReady).
  */
class Java2DRenderSurface(
    image: BufferedImage,
    metrics: CellMetrics,
    font: Font,
    onFlush: BufferedImage => Unit
) extends RenderSurface:
  private val g: Graphics2D = image.createGraphics()

  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
  g.setFont(font)

  /** The FontRenderContext this surface uses for text layout. Exposed so that TextLayoutSnapshot and other measurement
    * code can use the identical FRC, preventing cursor drift on proportional fonts.
    */
  private val renderContext: FontRenderContext = g.getFontRenderContext()

  private val fgRef       = AtomicReference(Color.WHITE)
  private val bgRef       = AtomicReference(Color.BLACK)
  private val baseFontRef = AtomicReference(font)

  override def setFont(newFont: Font): Unit =
    baseFontRef.set(newFont)
    g.setFont(newFont)

  override def fontRenderContext: Option[FontRenderContext] = Some(renderContext)

  override def drawRunPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    g.setColor(bgRef.get())
    g.fillRect(xPx.toInt, yPx, bgWidthPx.toInt.max(1), lineHeightPx)
    if s.nonEmpty then
      g.setColor(fgRef.get())
      g.drawString(s, xPx, (yPx + ascentPx).toFloat)

  def setForegroundColor(color: Color): Unit = fgRef.set(color)
  def setBackgroundColor(color: Color): Unit = bgRef.set(color)
  def getBackgroundColor: Color              = bgRef.get()

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px = metrics.toPixelX(x)
      val py = metrics.toPixelY(y)
      // Fill background for the whole string using nominal width
      g.setColor(bgRef.get())
      g.fillRect(px, py, s.length * metrics.charWidth, metrics.lineHeight)
      // Draw the foreground as one shaped string so font features like ligatures can apply.
      g.setColor(fgRef.get())
      g.drawString(s, px, py + metrics.ascent)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = metrics.toPixelY(y)
    val pw = width * metrics.charWidth
    val ph = height * metrics.lineHeight
    g.setColor(bgRef.get())
    g.fillRect(px, py, pw, ph)
    if char != ' ' then
      g.setColor(fgRef.get())
      (0 until height).foreach { row =>
        (0 until width).foreach { col =>
          g.drawString(char.toString, metrics.toPixelX(x + col), metrics.toPixelY(y + row) + metrics.ascent)
        }
      }

  def enableStyle(style: TextStyle): Unit =
    val styled = baseFontRef
      .get()
      .deriveFont(
        (if style.isBold then java.awt.Font.BOLD else 0) |
          (if style.isItalic then java.awt.Font.ITALIC else 0)
      )
    val derived =
      if style.isUnderlined then styled.deriveFont(Map(TextAttribute.UNDERLINE -> TextAttribute.UNDERLINE_ON).asJava)
      else styled
    g.setFont(derived)

  def disableStyle(style: TextStyle): Unit =
    g.setFont(baseFontRef.get())

  override def setAlpha(alpha: Float): Unit =
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha.max(0f).min(1f)))

  override def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit =
    if radius > 0f then
      val px       = metrics.toPixelX(x)
      val py       = metrics.toPixelY(y)
      val pw       = width * metrics.charWidth
      val ph       = height * metrics.lineHeight
      val clampedX = px.max(0).min(image.getWidth - 1)
      val clampedY = py.max(0).min(image.getHeight - 1)
      val clampedW = pw.min(image.getWidth - clampedX)
      val clampedH = ph.min(image.getHeight - clampedY)
      if clampedW > 0 && clampedH > 0 then
        val size    = (radius * 10).toInt.max(1) * 2 + 1
        val weight  = 1.0f / (size * size)
        val data    = Array.fill(size * size)(weight)
        val kernel  = new Kernel(size, size, data)
        val op      = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
        val src     = image.getSubimage(clampedX, clampedY, clampedW, clampedH)
        val blurred = op.filter(src, null)
        g.drawImage(blurred, clampedX, clampedY, null)

  override def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit =
    val px          = metrics.toPixelX(x)
    val py          = metrics.toPixelY(y)
    val pw          = width * metrics.charWidth
    val ph          = height * metrics.lineHeight
    val inset       = math.ceil(strokeWidth / 2).toInt
    val savedStroke = g.getStroke
    g.setColor(color)
    g.setStroke(new BasicStroke(strokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.drawRoundRect(px + inset, py + inset, pw - 2 * inset, ph - 2 * inset, arcPx * 2, arcPx * 2)
    g.setStroke(savedStroke)

  override def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: Color
  ): Unit =
    g.setColor(color)
    g.fillRect(xPx, yPx, widthPx.max(1), heightPx.max(1))

  def hideCursor(): Unit = ()

  def viewportWidth: Int  = image.getWidth / metrics.charWidth
  def viewportHeight: Int = image.getHeight / metrics.lineHeight

  def flush(): Unit =
    g.dispose()
    onFlush(image)

object Java2DRenderSurface:

  def forFrame(
    metrics: CellMetrics,
    font: Font,
    canvas: javax.swing.JPanel,
    onFlush: BufferedImage => Unit
  ): Java2DRenderSurface =
    val image = new BufferedImage(
      canvas.getWidth.max(1),
      canvas.getHeight.max(1),
      BufferedImage.TYPE_INT_ARGB
    )
    new Java2DRenderSurface(image, metrics, font, onFlush)
