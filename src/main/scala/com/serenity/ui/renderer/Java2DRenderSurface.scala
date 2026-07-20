package com.serenity.ui.renderer

import java.awt.*
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.{Rectangle2D, RoundRectangle2D}
import java.awt.image.*
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import com.serenity.config.PostProcessingEffect
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
    onFlush: BufferedImage => Unit,
    logicalWidthPx: Int = -1,
    logicalHeightPx: Int = -1,
    deviceScaleX: Double = 1.0,
    deviceScaleY: Double = 1.0
) extends RenderSurface:
  private val g: Graphics2D = image.createGraphics()
  private val effectiveLogicalWidthPx =
    if logicalWidthPx > 0 then logicalWidthPx else image.getWidth
  private val effectiveLogicalHeightPx =
    if logicalHeightPx > 0 then logicalHeightPx else image.getHeight
  private val cellGridWidthPx  = (effectiveLogicalWidthPx / metrics.charWidth) * metrics.charWidth
  private val cellGridHeightPx = (effectiveLogicalHeightPx / metrics.lineHeight) * metrics.lineHeight

  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
  g.scale(deviceScaleX, deviceScaleY)
  g.setFont(font)

  /** The FontRenderContext this surface uses for text layout. Exposed so that TextLayoutSnapshot and other measurement
    * code can use the identical FRC, preventing cursor drift on proportional fonts.
    */
  private val renderContext: FontRenderContext = g.getFontRenderContext()

  private val fgRef                   = AtomicReference(Color.WHITE)
  private val bgRef                   = AtomicReference(Color.BLACK)
  private val baseFontRef             = AtomicReference(font)
  private val logicalPixelRowOverride = AtomicReference[Option[(Int, Int)]](None)

  override def setFont(newFont: Font): Unit =
    baseFontRef.set(newFont)
    g.setFont(newFont)

  override def fontRenderContext: Option[FontRenderContext] = Some(renderContext)

  override def drawRunPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    val clipX         = math.floor(xPx.toDouble).toInt
    val clipRight     = math.ceil((xPx + bgWidthPx).toDouble).toInt
    val clipBottom    = yPx + lineHeightPx
    val boundedLeft   = clipX.max(0).min(cellGridWidthPx)
    val boundedRight  = clipRight.max(0).min(cellGridWidthPx)
    val boundedTop    = yPx.max(0).min(cellGridHeightPx)
    val boundedBottom = clipBottom.max(0).min(cellGridHeightPx)

    if boundedLeft < boundedRight && boundedTop < boundedBottom then
      val boundedWidth  = boundedRight - boundedLeft
      val boundedHeight = boundedBottom - boundedTop
      g.setColor(bgRef.get())
      g.fillRect(boundedLeft, boundedTop, boundedWidth, boundedHeight)
      if s.nonEmpty then
        val savedClip = g.getClip
        g.setColor(fgRef.get())
        try
          g.clipRect(boundedLeft, boundedTop, boundedWidth, boundedHeight)
          g.drawString(s, xPx, (yPx + ascentPx).toFloat)
        finally g.setClip(savedClip)

  def setForegroundColor(color: Color): Unit = fgRef.set(color)
  def setBackgroundColor(color: Color): Unit = bgRef.set(color)
  def getBackgroundColor: Color              = bgRef.get()

  override def clearViewport(color: Color): Unit =
    bgRef.set(color)
    g.setColor(color)
    g.fillRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx)

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px = metrics.toPixelX(x)
      val py = pixelYForRow(y)
      // Fill background for the whole string using nominal width
      g.setColor(bgRef.get())
      g.fillRect(px, py, s.length * metrics.charWidth, metrics.lineHeight)
      // Draw the foreground as one shaped string so font features like ligatures can apply.
      g.setColor(fgRef.get())
      g.drawString(s, px, py + metrics.ascent)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = pixelYForRow(y)
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

  override def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit =
    val previous = logicalPixelRowOverride.getAndSet(Some(cellRow -> pixelY))
    try render
    finally logicalPixelRowOverride.set(previous)

  override def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    val savedTransform = g.getTransform
    try
      g.translate(xPx, yPx)
      render
    finally g.setTransform(savedTransform)

  private def pixelYForRow(row: Int): Int =
    logicalPixelRowOverride
      .get()
      .collect { case (cellRow, pixelY) if cellRow == row => pixelY }
      .getOrElse(metrics.toPixelY(row))

  def enableStyle(style: TextStyle): Unit =
    val base     = baseFontRef.get()
    val fontMode = style.fontMode
    val size     = style.fontSize.getOrElse(base.getSize2D).max(1.0f)
    val styled = style.fontFamily match
      case Some(family) =>
        Font(family, fontMode, size.round.max(1)).deriveFont(fontMode, size)
      case None =>
        base.deriveFont(fontMode, size)
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
      val px         = metrics.toPixelX(x)
      val py         = metrics.toPixelY(y)
      val pw         = width * metrics.charWidth
      val ph         = height * metrics.lineHeight
      val transform  = g.getTransform
      val activeClip = Option(g.getClip).map(transform.createTransformedShape)
      val bounds = transform
        .createTransformedShape(new Rectangle2D.Double(px, py, pw, ph))
        .getBounds2D
      val left   = math.floor(bounds.getMinX).toInt
      val top    = math.floor(bounds.getMinY).toInt
      val right  = math.ceil(bounds.getMaxX).toInt
      val bottom = math.ceil(bounds.getMaxY).toInt
      Java2DRenderSurface
        .deviceRegionFor(
          logicalX = left,
          logicalY = top,
          logicalWidth = right - left,
          logicalHeight = bottom - top,
          imageWidth = image.getWidth,
          imageHeight = image.getHeight,
          deviceScaleX = 1.0,
          deviceScaleY = 1.0
        )
        .foreach { region =>
          val size        = (radius * 10).toInt.max(1) * 2 + 1
          val weight      = 1.0f / (size * size)
          val data        = Array.fill(size * size)(weight)
          val kernel      = new Kernel(size, size, data)
          val op          = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
          val src         = image.getSubimage(region.xPx, region.yPx, region.widthPx, region.heightPx)
          val blurred     = op.filter(src, null)
          val rawGraphics = image.createGraphics()
          try
            activeClip.foreach(rawGraphics.clip)
            rawGraphics.drawImage(blurred, region.xPx, region.yPx, null)
          finally rawGraphics.dispose()
        }

  private def estimatedBackgroundColor: Color =
    val horizontalStep = (image.getWidth / 64).max(1)
    val verticalStep   = (image.getHeight / 64).max(1)
    val sampledColors =
      (0 until image.getWidth by horizontalStep)
        .iterator
        .flatMap(x => (0 until image.getHeight by verticalStep).iterator.map(y => image.getRGB(x, y)))
    val counts = sampledColors.foldLeft(Map.empty[Int, Int]) { (accumulator, color) =>
      accumulator.updated(color, accumulator.getOrElse(color, 0) + 1)
    }
    new Color(counts.maxBy(_._2)._1, true)

  private def contrastFrom(color: Color, background: Color): Int =
    math.abs(color.getRed - background.getRed) +
      math.abs(color.getGreen - background.getGreen) +
      math.abs(color.getBlue - background.getBlue)

  override def applyPostProcessing(effect: PostProcessingEffect): Unit =
    effect match
      case PostProcessingEffect.Off => ()
      case PostProcessingEffect.Scanlines =>
        val rawGraphics = image.createGraphics()
        try
          rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.22f))
          rawGraphics.setColor(Color.BLACK)
          (1 until image.getHeight by 3).foreach(y => rawGraphics.drawLine(0, y, image.getWidth - 1, y))
          rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.08f))
          (2 until image.getHeight by 3).foreach(y => rawGraphics.drawLine(0, y, image.getWidth - 1, y))
          rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.035f))
          (0 until image.getWidth).foreach { x =>
            rawGraphics.setColor(
              x % 3 match
                case 0 => Color.RED
                case 1 => Color.GREEN
                case _ => Color.BLUE
            )
            rawGraphics.drawLine(x, 0, x, image.getHeight - 1)
          }
        finally rawGraphics.dispose()
      case PostProcessingEffect.Glow =>
        val background = estimatedBackgroundColor
        val source     = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_ARGB)
        (0 until image.getHeight).foreach { y =>
          (0 until image.getWidth).foreach { x =>
            val color = new Color(image.getRGB(x, y), true)
            if contrastFrom(color, background) >= 96 then source.setRGB(x, y, color.getRGB)
          }
        }
        val blurred = new ConvolveOp(
          new Kernel(
            5,
            5,
            Array(1f, 4f, 6f, 4f, 1f, 4f, 16f, 24f, 16f, 4f, 6f, 24f, 36f, 24f, 6f, 4f, 16f, 24f, 16f, 4f, 1f, 4f, 6f, 4f, 1f)
              .map(_ / 256f)
          ),
          ConvolveOp.EDGE_NO_OP,
          null
        ).filter(source, null)
        val glow = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_ARGB)
        (0 until glow.getHeight).foreach { y =>
          (0 until glow.getWidth).foreach { x =>
            val color = new Color(blurred.getRGB(x, y), true)
            if color.getAlpha > 0 then
              glow.setRGB(
                x,
                y,
                new Color(
                  (color.getRed * 8).min(255),
                  (color.getGreen * 8).min(255),
                  (color.getBlue * 8).min(255)
                ).getRGB
              )
          }
        }
        val rawGraphics = image.createGraphics()
        try
          rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f))
          val _ = rawGraphics.drawImage(glow, 0, 0, null)
        finally rawGraphics.dispose()

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

  override def withRoundRectClip(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit =
    val px        = metrics.toPixelX(x)
    val py        = metrics.toPixelY(y)
    val pw        = width * metrics.charWidth
    val ph        = height * metrics.lineHeight
    val savedClip = g.getClip
    try
      g.clip(new RoundRectangle2D.Double(px, py, pw, ph, arcPx * 2.0, arcPx * 2.0))
      render
    finally g.setClip(savedClip)

  override def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: Color
  ): Unit =
    g.setColor(color)
    g.fillRect(xPx, yPx, widthPx.max(1), heightPx.max(1))

  override def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit =
    val px        = metrics.toPixelX(x)
    val py        = metrics.toPixelY(y)
    val pw        = width * metrics.charWidth
    val ph        = height * metrics.lineHeight
    val savedClip = g.getClip
    g.clipRect(px, py, pw, ph)
    g.drawImage(image, px, py, pw, ph, null)
    g.setClip(savedClip)

  def hideCursor(): Unit = ()

  def viewportWidth: Int                 = effectiveLogicalWidthPx / metrics.charWidth
  def viewportHeight: Int                = effectiveLogicalHeightPx / metrics.lineHeight
  override def devicePixelScaleX: Double = deviceScaleX
  override def devicePixelScaleY: Double = deviceScaleY

  def flush(): Unit =
    g.dispose()
    onFlush(image)

object Java2DRenderSurface:

  private[serenity] case class DeviceScale(x: Double, y: Double)
  private[serenity] case class DeviceRegion(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int)

  def forFrame(
    metrics: CellMetrics,
    font: Font,
    canvas: javax.swing.JPanel,
    onFlush: BufferedImage => Unit
  ): Java2DRenderSurface =
    val logicalWidth  = logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = deviceScaleFor(canvas)
    val image = new BufferedImage(
      deviceImageDimension(logicalWidth, scale.x),
      deviceImageDimension(logicalHeight, scale.y),
      BufferedImage.TYPE_INT_ARGB
    )
    new Java2DRenderSurface(
      image,
      metrics,
      font,
      onFlush,
      logicalWidthPx = logicalWidth,
      logicalHeightPx = logicalHeight,
      deviceScaleX = scale.x,
      deviceScaleY = scale.y
    )

  def forImage(
    image: BufferedImage,
    metrics: CellMetrics,
    font: Font,
    canvas: javax.swing.JPanel,
    onFlush: BufferedImage => Unit
  ): Java2DRenderSurface =
    val logicalWidth  = logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = deviceScaleFor(canvas)
    new Java2DRenderSurface(
      image,
      metrics,
      font,
      onFlush,
      logicalWidthPx = logicalWidth,
      logicalHeightPx = logicalHeight,
      deviceScaleX = scale.x,
      deviceScaleY = scale.y
    )

  private[serenity] def deviceImageDimension(logicalDimensionPx: Int, deviceScale: Double): Int =
    math.ceil(logicalDimensionPx.max(1) * deviceScale.max(1.0)).toInt.max(1)

  private[serenity] def logicalCanvasDimension(currentPx: Int, preferredPx: Int): Int =
    if currentPx > 0 then currentPx
    else preferredPx.max(1)

  private[serenity] def deviceRegionFor(
    logicalX: Int,
    logicalY: Int,
    logicalWidth: Int,
    logicalHeight: Int,
    imageWidth: Int,
    imageHeight: Int,
    deviceScaleX: Double,
    deviceScaleY: Double
  ): Option[DeviceRegion] =
    val x0 = scaledFloor(logicalX, deviceScaleX).max(0).min(imageWidth)
    val y0 = scaledFloor(logicalY, deviceScaleY).max(0).min(imageHeight)
    val x1 = scaledCeil(logicalX + logicalWidth, deviceScaleX).max(0).min(imageWidth)
    val y1 = scaledCeil(logicalY + logicalHeight, deviceScaleY).max(0).min(imageHeight)
    Option.when(x1 > x0 && y1 > y0)(DeviceRegion(x0, y0, x1 - x0, y1 - y0))

  private def scaledFloor(logicalPx: Int, deviceScale: Double): Int =
    math.floor(logicalPx.toDouble * deviceScale.max(1.0)).toInt

  private def scaledCeil(logicalPx: Int, deviceScale: Double): Int =
    math.ceil(logicalPx.toDouble * deviceScale.max(1.0)).toInt

  private def deviceScaleFor(canvas: javax.swing.JPanel): DeviceScale =
    Option(canvas.getGraphicsConfiguration)
      .map(_.getDefaultTransform)
      .map(transform => DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0)))
      .getOrElse(DeviceScale(1.0, 1.0))
