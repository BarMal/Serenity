package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle
import java.awt.{AlphaComposite, Color, Font, FontMetrics, Graphics2D, RenderingHints}
import java.awt.image.{BufferedImage, ConvolveOp, Kernel}

/** A RenderSurface backed by a BufferedImage via Graphics2D.
 *
 *  All coordinates are in cell units (column, row). Pixel conversion uses CellMetrics.
 *  After all drawing is complete, call flush() to hand the finished image to onFlush.
 *
 *  Threading: draw methods are called from the Cats Effect thread pool (off-EDT).
 *  onFlush is responsible for scheduling the EDT repaint (e.g. via SwingWindow.onImageReady).
 */
class Java2DRenderSurface(
  image: BufferedImage,
  metrics: CellMetrics,
  font: Font,
  onFlush: BufferedImage => Unit
) extends RenderSurface:
  private val g: Graphics2D = image.createGraphics()
  private val fm: FontMetrics = g.getFontMetrics(font)

  // Enable text anti-aliasing for cleaner glyphs
  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  g.setFont(font)

  private var fg: Color = Color.WHITE
  private var bg: Color = Color.BLACK

  def setForegroundColor(color: TextColor): Unit = fg = color.toColor
  def setBackgroundColor(color: TextColor): Unit = bg = color.toColor
  def getBackgroundColor: TextColor = new TextColor.RGB(bg.getRed, bg.getGreen, bg.getBlue)

  override def setForegroundColor(color: java.awt.Color): Unit = fg = color
  override def setBackgroundColor(color: java.awt.Color): Unit = bg = color

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px = metrics.toPixelX(x)
      val py = metrics.toPixelY(y)
      // Fill background for the whole string using nominal width
      g.setColor(bg)
      g.fillRect(px, py, s.length * metrics.charWidth, metrics.lineHeight)
      // Draw each character with its actual advance width
      g.setColor(fg)
      var curX = px
      s.foreach { char =>
        val advance = fm.charWidth(char)
        g.drawString(char.toString, curX, py + metrics.ascent)
        curX += advance
      }

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = metrics.toPixelY(y)
    val pw = width * metrics.charWidth
    val ph = height * metrics.lineHeight
    g.setColor(bg)
    g.fillRect(px, py, pw, ph)
    if char != ' ' then
      g.setColor(fg)
      var row = 0
      while row < height do
        var col = 0
        while col < width do
          g.drawString(char.toString, metrics.toPixelX(x + col), metrics.toPixelY(y + row) + metrics.ascent)
          col += 1
        row += 1

  def enableStyle(style: TextStyle): Unit =
    val derived = font.deriveFont(
      (if style.isBold then java.awt.Font.BOLD else 0) |
      (if style.isItalic then java.awt.Font.ITALIC else 0)
    )
    g.setFont(derived)

  def disableStyle(style: TextStyle): Unit =
    g.setFont(font)

  override def setAlpha(alpha: Float): Unit =
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha.max(0f).min(1f)))

  override def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit =
    if radius > 0f then
      val px = metrics.toPixelX(x)
      val py = metrics.toPixelY(y)
      val pw = width  * metrics.charWidth
      val ph = height * metrics.lineHeight
      val clampedX = px.max(0).min(image.getWidth  - 1)
      val clampedY = py.max(0).min(image.getHeight - 1)
      val clampedW = pw.min(image.getWidth  - clampedX)
      val clampedH = ph.min(image.getHeight - clampedY)
      if clampedW > 0 && clampedH > 0 then
        val size   = (radius * 10).toInt.max(1) * 2 + 1
        val weight = 1.0f / (size * size)
        val data   = Array.fill(size * size)(weight)
        val kernel = new Kernel(size, size, data)
        val op     = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
        val src    = image.getSubimage(clampedX, clampedY, clampedW, clampedH)
        val blurred = op.filter(src, null)
        g.drawImage(blurred, clampedX, clampedY, null)

  def hideCursor(): Unit = ()

  def viewportWidth: Int  = image.getWidth / metrics.charWidth
  def viewportHeight: Int = image.getHeight / metrics.lineHeight

  def flush(): Unit =
    g.dispose()
    onFlush(image)

object Java2DRenderSurface:
  def forFrame(metrics: CellMetrics, font: Font, canvas: javax.swing.JPanel, onFlush: BufferedImage => Unit): Java2DRenderSurface =
    val image = new BufferedImage(
      canvas.getWidth.max(1),
      canvas.getHeight.max(1),
      BufferedImage.TYPE_INT_ARGB
    )
    new Java2DRenderSurface(image, metrics, font, onFlush)
