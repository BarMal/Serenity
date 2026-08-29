package com.serenity.ui.renderer

import java.awt.*
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.{Area, Rectangle2D, RoundRectangle2D}
import java.awt.image.*
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import com.serenity.config.PostProcessingEffect
import com.serenity.ui.layout.{CellMetrics, PixelRect}
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
    deviceScaleY: Double = 1.0,
    contentPersists: Boolean = false
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with RoundedRectDrawing
    with LayerBufferSupport:
  def text: TextDrawing                                 = this
  def pixels: PixelDrawing                              = this
  override def effects: Option[Effects]                 = Some(this)
  override def roundedRects: Option[RoundedRectDrawing] = Some(this)
  override def layerBuffers: Option[LayerBufferSupport] = Some(this)

  /** A fresh, fully transparent surface with this surface's own metrics/font/logical-size/device-scale -- derived
    * entirely from values this surface already computed, not from a `JPanel` (see [[Java2DRenderSurface.forLayer]] for
    * why that matters). Painting the same content into it at the same cell coordinates as painting directly into this
    * surface, then compositing the flushed image back on top of this surface at full opacity, is pixel-identical to
    * painting directly here -- standard "paint onto transparent, then composite over" associativity for `SRC_OVER` --
    * as long as the caller never reads this surface's own pixels back while painting the layer (no `blurRegion`, no
    * shadow sampling): `Renderer`'s modal layer, the first consumer of this seam, satisfies that.
    */
  override def newLayerSurface(onFlush: BufferedImage => Unit): RenderSurface =
    Java2DRenderSurface.forLayer(
      metrics,
      baseFontRef.get(),
      effectiveLogicalWidthPx,
      effectiveLogicalHeightPx,
      deviceScaleX,
      deviceScaleY,
      onFlush
    )

  /** As [[newLayerSurface]], but the returned buffer's backing image starts as a pixel copy of `image` -- this
    * surface's own current backing image -- rather than fully transparent. See
    * [[LayerBufferSupport.newSeededLayerSurface]]'s doc comment for why a layer that reads pixels back while painting
    * (`blurRegion`) needs this instead of the transparent buffer [[newLayerSurface]] hands out.
    */
  override def newSeededLayerSurface(onFlush: BufferedImage => Unit): RenderSurface =
    Java2DRenderSurface.forLayer(
      metrics,
      baseFontRef.get(),
      effectiveLogicalWidthPx,
      effectiveLogicalHeightPx,
      deviceScaleX,
      deviceScaleY,
      onFlush,
      seed = Some(image)
    )

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

  override def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit =
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
          if clipGlyphToRun then g.clipRect(boundedLeft, boundedTop, boundedWidth, boundedHeight)
          else g.clipRect(0, 0, cellGridWidthPx, cellGridHeightPx)
          g.drawString(s, xPx, (yPx + ascentPx).toFloat)
        finally g.setClip(savedClip)

  def setForegroundColor(color: Color): Unit = fgRef.set(color)
  def setBackgroundColor(color: Color): Unit = bgRef.set(color)
  def getBackgroundColor: Color              = bgRef.get()

  /** The backing image doubles as the persistence key: whoever hands the same image back next frame gets the pixels
    * this frame leaves behind. Only surfaces built with `contentPersists` advertise it, because an image the caller
    * intends to hand out once carries no promise about what it will contain next time.
    */
  override def persistentContentKey: Option[SurfaceContentIdentity] =
    Option.when(contentPersists)(SurfaceContentIdentity(image))

  override def clearViewport(color: Color): Unit =
    bgRef.set(color)
    g.setColor(color)
    g.fillRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx)

  override def clearViewportExcept(color: Color, preserved: scala.collection.immutable.List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      bgRef.set(color)
      val clearable = new Area(new Rectangle(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx))
      preserved.foreach(rect =>
        clearable.subtract(new Area(new Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)))
      )
      val savedClip = g.getClip
      try
        g.clip(clearable)
        g.setColor(color)
        g.fillRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx)
      finally g.setClip(savedClip)

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
      (0 until image.getWidth by horizontalStep).iterator
        .flatMap(x => (0 until image.getHeight by verticalStep).iterator.map(y => image.getRGB(x, y)))
    val counts = sampledColors.foldLeft(Map.empty[Int, Int]) { (accumulator, color) =>
      accumulator.updated(color, accumulator.getOrElse(color, 0) + 1)
    }
    new Color(counts.maxBy(_._2)._1, true)

  /** Standard Porter-Duff SRC_OVER, matching what `Graphics2D`'s default composite computes for opaque-source draws
    * onto a `TYPE_INT_ARGB` destination -- reimplemented here so `applyGlow` can blend directly on a raw pixel array
    * instead of issuing one `Graphics2D` draw call per pixel.
    */
  private def blendSrcOver(destArgb: Int, srcR: Int, srcG: Int, srcB: Int, srcA: Int): Int =
    if srcA <= 0 then destArgb
    else
      val dstA = (destArgb >>> 24) & 0xff
      val dstR = (destArgb >>> 16) & 0xff
      val dstG = (destArgb >>> 8) & 0xff
      val dstB = destArgb & 0xff
      val outA = srcA + dstA * (255 - srcA) / 255
      def blendChannel(srcC: Int, dstC: Int): Int =
        if outA == 0 then 0
        else ((srcC * srcA + dstC * dstA * (255 - srcA) / 255) / outA).min(255).max(0)
      (outA << 24) | (blendChannel(srcR, dstR) << 16) | (blendChannel(srcG, dstG) << 8) | blendChannel(srcB, dstB)

  override def applyPostProcessing(effect: PostProcessingEffect, animationPhase: Long): Unit =
    effect match
      case PostProcessingEffect.Off => ()
      case PostProcessingEffect.Scanlines =>
        applyScanlines(animationPhase)
      case PostProcessingEffect.Glow =>
        applyGlow()
      case PostProcessingEffect.ScanlinesAndGlow =>
        applyGlow()
        applyScanlines(animationPhase)

  private def applyScanlines(animationPhase: Long): Unit =
    val rawGraphics = image.createGraphics()
    try
      val phase = (animationPhase % 97L).toInt
      drawScanlines(rawGraphics, 1 + phase % 3, phase)
      rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.025f))
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

  private def drawScanlines(rawGraphics: Graphics2D, y: Int, phase: Int): Unit =
    if y < image.getHeight then
      val thickness = if (y + phase) % 11 <= 1 then 2 else 1
      val alpha     = if thickness == 2 then 0.18f else 0.13f
      rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
      rawGraphics.setColor(Color.BLACK)
      (y until math.min(image.getHeight, y + thickness)).foreach(row =>
        rawGraphics.drawLine(0, row, image.getWidth - 1, row)
      )
      val spacing = 3 + math.floorMod(y + phase, 4)
      drawScanlines(rawGraphics, y + thickness + spacing, phase)

  /** Spreads bright (or, on a light background, dark) pixels into a soft halo.
    *
    * Operates on raw ARGB `int[]` pixel arrays end to end -- one bulk `getRGB`/`setRGB` transfer in, one out -- and
    * blends with [[blendSrcOver]] instead of per-pixel `Graphics2D` calls: the previous implementation issued up to 24
    * `setColor`/`fillRect`/`drawImage` calls per masked source pixel, which is a well-known Java2D anti-pattern for
    * full-image compositing.
    */
  private def applyGlow(): Unit =
    val width       = image.getWidth
    val height      = image.getHeight
    val background  = estimatedBackgroundColor
    val backgroundR = background.getRed
    val backgroundG = background.getGreen
    val backgroundB = background.getBlue

    val basePixels   = image.getRGB(0, 0, width, height, null, 0, width)
    val sourcePixels = new Array[Int](width * height)
    val sourceMask   = new Array[Boolean](width * height)

    (0 until basePixels.length).foreach { index =>
      val argb     = basePixels(index)
      val r        = (argb >>> 16) & 0xff
      val g        = (argb >>> 8) & 0xff
      val b        = argb & 0xff
      val contrast = math.abs(r - backgroundR) + math.abs(g - backgroundG) + math.abs(b - backgroundB)
      if contrast >= 96 then
        sourcePixels(index) = argb
        sourceMask(index) = true
    }

    val source = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    source.setRGB(0, 0, width, height, sourcePixels, 0, width)

    val blurred = new ConvolveOp(
      new Kernel(
        5,
        5,
        Array(1f, 4f, 6f, 4f, 1f, 4f, 16f, 24f, 16f, 4f, 6f, 24f, 36f, 24f, 6f, 4f, 16f, 24f, 16f, 4f, 1f, 4f, 6f, 4f,
          1f).map(_ / 128f)
      ),
      ConvolveOp.EDGE_NO_OP,
      null
    ).filter(source, null)
    val blurredPixels = blurred.getRGB(0, 0, width, height, null, 0, width)

    val result = basePixels.clone()

    // Spread each masked source pixel into a soft halo across its 5x5 neighborhood.
    (0 until height).foreach { y =>
      (0 until width).foreach { x =>
        val index = y * width + x
        if sourceMask(index) then
          val srcColor = sourcePixels(index)
          val srcR     = (srcColor >>> 16) & 0xff
          val srcG     = (srcColor >>> 8) & 0xff
          val srcB     = srcColor & 0xff
          (-2 to 2).foreach { yOffset =>
            (-2 to 2).foreach { xOffset =>
              val distance = math.max(math.abs(xOffset), math.abs(yOffset))
              val nx       = x + xOffset
              val ny       = y + yOffset
              if distance > 0 && nx >= 0 && nx < width && ny >= 0 && ny < height then
                val alpha       = if distance == 1 then 14 else 6
                val targetIndex = ny * width + nx
                result(targetIndex) = blendSrcOver(result(targetIndex), srcR, srcG, srcB, alpha)
            }
          }
      }
    }

    // Composite the Gaussian-blurred glow on top, with per-pixel alpha derived from its brightest channel.
    (0 until blurredPixels.length).foreach { index =>
      val bc        = blurredPixels(index)
      val ba        = (bc >>> 24) & 0xff
      val br        = (bc >>> 16) & 0xff
      val bg        = (bc >>> 8) & 0xff
      val bb        = bc & 0xff
      val intensity = ba.max(br).max(bg).max(bb)
      if intensity > 0 then
        val alpha = math.max(1, (intensity * 0.8f).toInt)
        result(index) = blendSrcOver(result(index), br, bg, bb, alpha)
    }

    // Restore the sharp original source pixels exactly where masked, undoing any halo/blur bleed on top of them.
    (0 until sourcePixels.length).foreach { index =>
      if sourceMask(index) then
        val srcColor = sourcePixels(index)
        val srcA     = (srcColor >>> 24) & 0xff
        val srcR     = (srcColor >>> 16) & 0xff
        val srcG     = (srcColor >>> 8) & 0xff
        val srcB     = srcColor & 0xff
        result(index) = blendSrcOver(result(index), srcR, srcG, srcB, srcA)
    }

    image.setRGB(0, 0, width, height, result, 0, width)

  override def drawRoundRectShadow(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color
  ): Unit =
    val px             = metrics.toPixelX(x)
    val py             = metrics.toPixelY(y)
    val pw             = width * metrics.charWidth
    val ph             = height * metrics.lineHeight
    val savedComposite = g.getComposite
    try
      scala.collection.immutable.List(6 -> 0.025f, 5 -> 0.035f, 4 -> 0.05f, 3 -> 0.07f).foreach { (offset, alpha) =>
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
        g.setColor(color)
        g.fillRoundRect(px + offset, py + offset, pw, ph, arcPx * 2, arcPx * 2)
      }
    finally g.setComposite(savedComposite)

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

  final private[serenity] case class DeviceScale(x: Double, y: Double)
  final private[serenity] case class DeviceRegion(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int)

  def forFrame(
    metrics: CellMetrics,
    font: Font,
    canvas: javax.swing.JPanel,
    onFlush: BufferedImage => Unit
  ): Java2DRenderSurface =
    forFrame(
      metrics,
      font,
      canvas,
      onFlush,
      (width, height, imageType) => new BufferedImage(width, height, imageType),
      contentPersists = false
    )

  /** Build a frame surface over an image supplied by `acquireImage`.
    *
    * `contentPersists` says the acquired image is recycled rather than freshly allocated, so whatever was drawn into
    * that same image instance previously is still there. Callers pass a pooled acquirer together with `true`; a
    * single-use image must stay `false` so nothing downstream tries to reuse pixels that were never kept.
    */
  def forFrame(
    metrics: CellMetrics,
    font: Font,
    canvas: javax.swing.JPanel,
    onFlush: BufferedImage => Unit,
    acquireImage: (Int, Int, Int) => BufferedImage,
    contentPersists: Boolean = true
  ): Java2DRenderSurface =
    val logicalWidth  = logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = deviceScaleFor(canvas)
    val image = acquireImage(
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
      deviceScaleY = scale.y,
      contentPersists = contentPersists
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

  /** Build a layer surface from numbers alone -- no `JPanel` required, unlike [[forFrame]]/[[forImage]]. Stage 1 of
    * #1100 flagged those two as "tied to a Swing `JPanel` for device-scale/logical-size derivation" as the open design
    * problem blocking per-surface buffering; this resolves it by deriving the same inputs from an existing
    * [[Java2DRenderSurface]] that already computed them (see [[Java2DRenderSurface.newLayerSurface]]) instead of from a
    * canvas. The resulting image starts fully transparent (`TYPE_INT_ARGB`'s zero value) unless `seed` is given, in
    * which case it starts as a pixel copy of `seed` instead (#1100 stage 3, [[newSeededLayerSurface]]) -- and is never
    * reused across calls (`contentPersists = false`) -- each call to [[LayerBufferSupport.newLayerSurface]] /
    * [[LayerBufferSupport.newSeededLayerSurface]] hands back a brand-new buffer for its caller to composite and then
    * own the lifetime of.
    */
  def forLayer(
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScaleX: Double,
    deviceScaleY: Double,
    onFlush: BufferedImage => Unit,
    seed: Option[BufferedImage] = None
  ): Java2DRenderSurface =
    val image = new BufferedImage(
      deviceImageDimension(logicalWidthPx, deviceScaleX),
      deviceImageDimension(logicalHeightPx, deviceScaleY),
      BufferedImage.TYPE_INT_ARGB
    )
    seed.foreach { seedImage =>
      val seedGraphics = image.createGraphics()
      try seedGraphics.drawImage(seedImage, 0, 0, null)
      finally seedGraphics.dispose()
    }
    new Java2DRenderSurface(
      image,
      metrics,
      font,
      onFlush,
      logicalWidthPx = logicalWidthPx,
      logicalHeightPx = logicalHeightPx,
      deviceScaleX = deviceScaleX,
      deviceScaleY = deviceScaleY,
      contentPersists = false
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
