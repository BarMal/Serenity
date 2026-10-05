package com.serenity.ui.renderer

import java.awt.*
import java.awt.font.FontRenderContext
import java.awt.geom.Rectangle2D
import java.awt.image.*
import java.util.concurrent.atomic.AtomicReference

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
    contentPersists: Boolean = false,
    layerCacheOwnerOverride: Option[ScreenIdentity] = None
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing
    with LayerBufferSupport:
  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = Some(this)
  override def panelOutlines: Option[PanelOutlineDrawing] = Some(this)
  override def layerBuffers: Option[LayerBufferSupport]   = Some(this)

  override def layerCacheOwner: ScreenIdentity = layerCacheOwnerOverride.getOrElse(super.layerCacheOwner)

  /** A fresh, fully transparent surface with this surface's own metrics/font/logical-size/device-scale -- derived
    * entirely from values this surface already computed, not from a `JPanel` (see [[Java2DRenderSurface.forLayer]] for
    * why that matters).
    */
  override def newLayerSurface(onFlush: RenderImage => Unit, recycled: Option[RenderImage]): RenderSurface =
    Java2DRenderSurface.forLayer(
      metrics,
      baseFontRef.get(),
      effectiveLogicalWidthPx,
      effectiveLogicalHeightPx,
      deviceScaleX,
      deviceScaleY,
      image => onFlush(RenderImage.fromAwt(image)),
      recycled = recycled.map(_.toAwt)
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
  private val lastPaintColorRef       = AtomicReference(Color.WHITE)
  private val baseFontRef             = AtomicReference(font)
  private val logicalPixelRowOverride = AtomicReference[Option[(Int, Int)]](None)

  override def setFont(newFont: FontSpec): Unit =
    baseFontRef.set(newFont.toAwt)
    g.setFont(newFont.toAwt)

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
      fillBackground(bgRef.get(), boundedLeft, boundedTop, boundedWidth, boundedHeight)
      if s.nonEmpty then
        val savedClip = g.getClip
        g.setColor(fgRef.get())
        try
          if clipGlyphToRun then g.clipRect(boundedLeft, boundedTop, boundedWidth, boundedHeight)
          else g.clipRect(0, 0, cellGridWidthPx, cellGridHeightPx)
          g.drawString(s, xPx, (yPx + ascentPx).toFloat)
        finally g.setClip(savedClip)

  def setForegroundColor(color: RenderColor): Unit = fgRef.set(Java2DRenderSurface.awtColor(color, fgRef.get()))
  def setBackgroundColor(color: RenderColor): Unit = bgRef.set(Java2DRenderSurface.awtColor(color, bgRef.get()))
  def getBackgroundColor: RenderColor              = RenderColor.fromAwt(bgRef.get())

  private def paintColor(color: RenderColor): Color =
    val awt = Java2DRenderSurface.awtColor(color, lastPaintColorRef.get())
    lastPaintColorRef.set(awt)
    awt

  /** The backing image doubles as the persistence key: whoever hands the same image back next frame gets the pixels
    * this frame leaves behind. Only surfaces built with `contentPersists` advertise it, because an image the caller
    * intends to hand out once carries no promise about what it will contain next time.
    */
  override def persistentContentKey: Option[SurfaceContentIdentity] =
    Option.when(contentPersists)(SurfaceContentIdentity(image))

  /** Fill `px, py, pw, ph` with `color`, guaranteeing the pixels actually become exactly `color` -- including fully
    * transparent (alpha 0, the "use the terminal/desktop's own backdrop" sentinel, #1240) -- rather than the no-op the
    * default SRC_OVER composite gives a zero-alpha fill against a buffer that may already carry opaque pixels from a
    * previous frame (this surface's backing image can be pooled/reused across frames, see `contentPersists`). Restores
    * whatever composite (e.g. an active `setAlpha` translucency scale) was in effect before the fill, so this only
    * affects the one fill call, never anything drawn after it. A frame image has no alpha, and the canvas shows black
    * through a transparent pixel, so it gets that black.
    */
  private def fillBackground(color: Color, px: Int, py: Int, pw: Int, ph: Int): Unit =
    if color.getAlpha == 0 then
      val savedComposite = g.getComposite
      g.setComposite(AlphaComposite.Src)
      g.setColor(if image.getColorModel.hasAlpha then color else Color.BLACK)
      g.fillRect(px, py, pw, ph)
      g.setComposite(savedComposite)
    else
      g.setColor(color)
      g.fillRect(px, py, pw, ph)

  override def clearViewport(color: RenderColor): Unit =
    setBackgroundColor(color)
    fillBackground(bgRef.get(), 0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx)

  override def clearViewportExcept(color: RenderColor, preserved: scala.collection.immutable.List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      setBackgroundColor(color)
      val background = bgRef.get()
      PixelRect
        .uncoveredWithin(PixelRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx), preserved)
        .foreach(rect => fillBackground(background, rect.xPx, rect.yPx, rect.widthPx, rect.heightPx))

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px = metrics.toPixelX(x)
      val py = pixelYForRow(y)
      // Fill background for the whole string using nominal width
      fillBackground(bgRef.get(), px, py, s.length * metrics.charWidth, metrics.lineHeight)
      // Draw the foreground as one shaped string so font features like ligatures can apply.
      g.setColor(fgRef.get())
      g.drawString(s, px, py + metrics.ascent)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = pixelYForRow(y)
    val pw = width * metrics.charWidth
    val ph = height * metrics.lineHeight
    fillBackground(bgRef.get(), px, py, pw, ph)
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
    g.setFont(TextStyle.styledFont(baseFontRef.get(), style))

  def disableStyle(style: TextStyle): Unit =
    g.setFont(baseFontRef.get())

  override def setAlpha(alpha: Float): Unit =
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha.max(0f).min(1f)))

  override def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    val px          = metrics.toPixelX(x)
    val py          = metrics.toPixelY(y)
    val pw          = width * metrics.charWidth
    val ph          = height * metrics.lineHeight
    val inset       = math.ceil(strokeWidth / 2).toInt
    val savedStroke = g.getStroke
    g.setColor(paintColor(color))
    g.setStroke(new BasicStroke(strokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER))
    g.drawRect(px + inset, py + inset, pw - 2 * inset, ph - 2 * inset)
    g.setStroke(savedStroke)

  override def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    val px        = metrics.toPixelX(x)
    val py        = metrics.toPixelY(y)
    val pw        = width * metrics.charWidth
    val ph        = height * metrics.lineHeight
    val savedClip = g.getClip
    try
      g.clip(new Rectangle2D.Double(px, py, pw, ph))
      render
    finally g.setClip(savedClip)

  override def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: RenderColor
  ): Unit =
    g.setColor(paintColor(color))
    g.fillRect(xPx, yPx, widthPx.max(1), heightPx.max(1))

  override def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
    val px        = metrics.toPixelX(x)
    val py        = metrics.toPixelY(y)
    val pw        = width * metrics.charWidth
    val ph        = height * metrics.lineHeight
    val savedClip = g.getClip
    g.clipRect(px, py, pw, ph)
    g.drawImage(image.toAwt, px, py, pw, ph, Java2DRenderSurface.NoOpImageObserver)
    g.setClip(savedClip)

  /** Blit a whole-surface layer buffer straight onto this surface's backing image at device resolution, 1:1, bypassing
    * `g`'s cell-grid geometry and its `deviceScale` transform entirely. The layer buffer is produced by [[forLayer]] at
    * exactly this surface's own backing dimensions (`deviceImageDimension(logicalSize, deviceScale)`), so a raw
    * device-pixel copy reproduces it exactly -- see [[PixelDrawing.compositeFullSurfaceLayer]] for why routing it
    * through [[drawImage]] instead would shrink it a little per composite.
    */
  override def compositeFullSurfaceLayer(layerImage: RenderImage): Unit =
    val rawGraphics = image.createGraphics()
    try
      val _ = rawGraphics.drawImage(layerImage.toAwt, 0, 0, Java2DRenderSurface.NoOpImageObserver)
    finally rawGraphics.dispose()

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

  /** `Graphics2D.drawImage`'s `ImageObserver` callback exists for images that may still be loading asynchronously (e.g.
    * from a URL); every image this class ever draws is an already-fully-materialised `BufferedImage`, so the callback
    * can never fire and a `null` observer (Java's usual shorthand for "don't call me back") is equivalent to this
    * no-op. Returning `false` tells the (never-invoked) caller not to bother scheduling further notifications.
    */
  private[serenity] val NoOpImageObserver: ImageObserver = (_, _, _, _, _, _) => false

  /** Reuses `current` when it already carries `color`, so a renderer re-setting the same theme colour on every run does
    * not allocate a fresh `java.awt.Color` each time.
    */
  private def awtColor(color: RenderColor, current: Color): Color =
    if current.getRGB == color.argb then current else color.toAwt

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
    *
    * The image is `TYPE_INT_RGB`: the window is opaque, and Java2D blends glyphs into it markedly faster than into
    * ARGB.
    *
    * Every frame built over the same `canvas` shares one [[RenderSurface.layerCacheOwner]].
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
      BufferedImage.TYPE_INT_RGB
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
      contentPersists = contentPersists,
      layerCacheOwnerOverride = Some(ScreenIdentity(canvas))
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
    * canvas. The image starts fully transparent: `recycled` is cleared and reused when its size matches, otherwise a
    * new image is allocated.
    */
  def forLayer(
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScaleX: Double,
    deviceScaleY: Double,
    onFlush: BufferedImage => Unit,
    recycled: Option[BufferedImage] = None
  ): Java2DRenderSurface =
    val width  = deviceImageDimension(logicalWidthPx, deviceScaleX)
    val height = deviceImageDimension(logicalHeightPx, deviceScaleY)
    val image = recycled
      .filter(candidate => candidate.getWidth == width && candidate.getHeight == height)
      .map(clearedForReuse)
      .getOrElse(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))
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

  private def clearedForReuse(image: BufferedImage): BufferedImage =
    val graphics = image.createGraphics()
    try
      graphics.setComposite(AlphaComposite.Clear)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
    finally graphics.dispose()
    image

  private[serenity] def deviceImageDimension(logicalDimensionPx: Int, deviceScale: Double): Int =
    math.ceil(logicalDimensionPx.max(1) * deviceScale.max(1.0)).toInt.max(1)

  private[serenity] def logicalCanvasDimension(currentPx: Int, preferredPx: Int): Int =
    if currentPx > 0 then currentPx
    else preferredPx.max(1)

  private[serenity] def deviceScaleFor(canvas: javax.swing.JPanel): DeviceScale =
    Option(canvas.getGraphicsConfiguration)
      .map(_.getDefaultTransform)
      .map(transform => DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0)))
      .getOrElse(DeviceScale(1.0, 1.0))
