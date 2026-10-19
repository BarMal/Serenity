package com.serenity.ui.renderer

import java.awt.*
import java.awt.font.FontRenderContext
import java.awt.geom.Rectangle2D
import java.awt.image.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.LabelAnchor.isWhole
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
    layerCacheOwnerOverride: Option[ScreenIdentity] = None,
    imageOriginPx: DeviceOrigin = DeviceOrigin.Zero,
    labelSprites: LabelSprites = LabelSprites.shared,
    styledFonts: StyledFontCache = StyledFontCache.shared,
    shapedText: ShapedTextCache = ShapedTextCache.shared,
    override val activity: SurfaceActivity = SurfaceActivity()
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing
    with LayerBufferSupport
    with PixelCopying:
  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = Some(this)
  override def panelOutlines: Option[PanelOutlineDrawing] = Some(this)
  override def layerBuffers: Option[LayerBufferSupport]   = Some(this)

  override def addressesPixels: Boolean = true

  override def pixelCopying: Option[PixelCopying] =
    Option.when(isWhole(deviceScaleX) && isWhole(deviceScaleY))(this)

  private val lifting = new ImagePixelLifting(image, deviceScaleX, deviceScaleY, () => painted.forgetClear())

  override def pixelLifting: Option[PixelLifting] = Some(lifting)

  override def layerCacheOwner: ScreenIdentity = layerCacheOwnerOverride.getOrElse(super.layerCacheOwner)

  /** A fresh, fully transparent surface with this surface's own metrics/font/logical-size/device-scale -- derived
    * entirely from values this surface already computed, not from a `JPanel` (see [[Java2DRenderSurface.forLayer]] for
    * why that matters).
    */
  override def newLayerSurface(onFlush: RenderImage => Unit, recycled: Option[RenderImage]): RenderSurface =
    layerSurface(onFlush, recycled, None)

  override def newBoundedLayerSurface(
    bounds: PixelRect,
    onFlush: RenderImage => Unit,
    recycled: Option[RenderImage]
  ): RenderSurface =
    layerSurface(onFlush, recycled, Some(bounds))

  private def layerSurface(
    onFlush: RenderImage => Unit,
    recycled: Option[RenderImage],
    bounds: Option[PixelRect]
  ): RenderSurface =
    Java2DRenderSurface.forLayer(
      metrics,
      baseFontRef.get(),
      effectiveLogicalWidthPx,
      effectiveLogicalHeightPx,
      deviceScaleX,
      deviceScaleY,
      image => onFlush(RenderImage.fromAwt(image)),
      recycled = recycled.map(_.toAwt),
      activity = activity,
      bounds = bounds
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
  g.translate(-imageOriginPx.xPx.toDouble, -imageOriginPx.yPx.toDouble)
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

  private val painted       = PaintedPixels()
  private val underlineRef  = AtomicBoolean(false)
  private val userClipDepth = AtomicInteger(0)

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
    paintRun(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s.nonEmpty, clipGlyphToRun)(
      g.drawString(s, xPx, (yPx + ascentPx).toFloat)
    )

  override def drawShapedRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    stretch: String,
    from: Int,
    until: Int,
    clipGlyphToRun: Boolean
  ): Unit =
    paintRun(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, from < until, clipGlyphToRun) {
      val shapedBefore = shapedText.shapings
      val shaped       = shapedText.stretchOf(stretch, g.getFont, renderContext)
      activity.shaped(shapedText.shapings - shapedBefore)
      shaped.glyphsOf(from, until).foreach(g.drawGlyphVector(_, xPx, (yPx + ascentPx).toFloat))
    }

  override def withFallbackFamily(family: String)(draw: => Unit): Unit = styledFonts.drawingIn(g, family)(draw)

  override def shapesStretchesOf(text: String): Boolean =
    ShapedTextCache.worthShaping(baseFontRef.get(), text)

  private def paintRun(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    hasText: Boolean,
    clipGlyphToRun: Boolean
  )(drawGlyphs: => Unit): Unit =
    val boundedLeft   = runLeft(xPx)
    val boundedRight  = runRight(xPx, bgWidthPx)
    val boundedTop    = runTop(yPx)
    val boundedBottom = runBottom(yPx, lineHeightPx)

    if boundedLeft < boundedRight && boundedTop < boundedBottom then
      activity.runDrawn()
      val boundedWidth  = boundedRight - boundedLeft
      val boundedHeight = boundedBottom - boundedTop
      if painted.needsBackground(boundedLeft, boundedTop, boundedRight, boundedBottom, bgRef.get()) then
        fillBackground(bgRef.get(), boundedLeft, boundedTop, boundedWidth, boundedHeight)
        painted.ranOver(boundedLeft, boundedTop, boundedRight, boundedBottom)
      if hasText then
        g.setColor(fgRef.get())
        val savedClip = if userClipDepth.get() == 0 then None else Option(g.getClip)
        try
          if clipGlyphToRun then g.clipRect(boundedLeft, boundedTop, boundedWidth, boundedHeight)
          else g.clipRect(0, 0, cellGridWidthPx, cellGridHeightPx)
          drawGlyphs
        finally Java2DRenderSurface.restoreClip(g, savedClip)
        drawUnderline(boundedLeft, boundedWidth, yPx + ascentPx)
        painted.ranOver(boundedLeft, boundedTop, boundedRight, boundedBottom)

  private def runLeft(xPx: Float): Int = math.floor(xPx.toDouble).toInt.max(0).min(cellGridWidthPx)

  private def runRight(xPx: Float, bgWidthPx: Float): Int =
    math.ceil((xPx + bgWidthPx).toDouble).toInt.max(0).min(cellGridWidthPx)

  private def runTop(yPx: Int): Int = yPx.max(0).min(cellGridHeightPx)

  private def runBottom(yPx: Int, lineHeightPx: Int): Int = (yPx + lineHeightPx).max(0).min(cellGridHeightPx)

  /** An underline is a rule under the run rather than a font attribute, so an underlined run draws with the font of a
    * plain one. Its thickness never drops below one device pixel.
    */
  private def drawUnderline(leftPx: Int, widthPx: Int, baselinePx: Int): Unit =
    if underlineRef.get() && widthPx > 0 then
      val underline = styledFonts.underlineOf(g.getFont)
      val thickness = underline.thicknessPx.max((1.0 / deviceScaleY).toFloat)
      g.setColor(fgRef.get())
      g.fill(new Rectangle2D.Float(leftPx.toFloat, baselinePx + underline.offsetPx, widthPx.toFloat, thickness))

  override def drawLabelPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String
  ): Unit =
    LabelAnchor.of(g.getTransform, xPx, yPx + ascentPx) match
      case Some(anchor) if s.nonEmpty && userClipDepth.get() == 0 =>
        drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, "")
        blitLabel(anchor, s)
        val left = runLeft(xPx)
        drawUnderline(left, runRight(xPx, bgWidthPx) - left, yPx + ascentPx)
        painted.ranOver(left, runTop(yPx), runRight(xPx, bgWidthPx), runBottom(yPx, lineHeightPx))
      case _ => drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s)

  private def blitLabel(anchor: LabelAnchor, s: String): Unit =
    val sprite = labelSprites.spriteFor(
      LabelSprites.Key(g.getFont, anchor.scaleX, anchor.scaleY, fgRef.get().getRGB, s, anchor.phaseSixteenths),
      renderContext
    )
    val device = image.createGraphics()
    try
      device.setComposite(g.getComposite)
      device.clipRect(0, 0, cellGridWidthPx * anchor.scaleX, cellGridHeightPx * anchor.scaleY)
      val _ = device.drawImage(
        sprite.image,
        anchor.deviceX - sprite.originX,
        anchor.deviceBaseline - sprite.baseline,
        Java2DRenderSurface.NoOpImageObserver
      )
    finally device.dispose()

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
    painted.cleared(
      bgRef.get(),
      scala.collection.immutable.List(PixelRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx)),
      clipped = userClipDepth.get() > 0
    )

  override def clearViewportExcept(color: RenderColor, preserved: scala.collection.immutable.List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      setBackgroundColor(color)
      val background = bgRef.get()
      val uncovered =
        PixelRect.uncoveredWithin(PixelRect(0, 0, effectiveLogicalWidthPx, effectiveLogicalHeightPx), preserved)
      uncovered.foreach(rect => fillBackground(background, rect.xPx, rect.yPx, rect.widthPx, rect.heightPx))
      painted.cleared(background, uncovered, clipped = userClipDepth.get() > 0)

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px = metrics.toPixelX(x)
      val py = pixelYForRow(y)
      // Fill background for the whole string using nominal width
      fillBackground(bgRef.get(), px, py, s.length * metrics.charWidth, metrics.lineHeight)
      painted.touched(px, py, s.length * metrics.charWidth, metrics.lineHeight)
      // Draw the foreground as one shaped string so font features like ligatures can apply.
      g.setColor(fgRef.get())
      g.drawString(s, px, py + metrics.ascent)
      drawUnderline(px, s.length * metrics.charWidth, py + metrics.ascent)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = pixelYForRow(y)
    val pw = width * metrics.charWidth
    val ph = height * metrics.lineHeight
    fillBackground(bgRef.get(), px, py, pw, ph)
    painted.touched(px, py, pw, ph)
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
      painted.withTranslation(xPx, yPx)(render)
    finally g.setTransform(savedTransform)

  private def pixelYForRow(row: Int): Int =
    logicalPixelRowOverride
      .get()
      .collect { case (cellRow, pixelY) if cellRow == row => pixelY }
      .getOrElse(metrics.toPixelY(row))

  def enableStyle(style: TextStyle): Unit =
    underlineRef.set(style.isUnderlined)
    g.setFont(styledFonts.fontFor(baseFontRef.get(), style))

  def disableStyle(style: TextStyle): Unit =
    underlineRef.set(false)
    g.setFont(baseFontRef.get())

  override def setAlpha(alpha: Float): Unit =
    val clamped = alpha.max(0f).min(1f)
    painted.setTranslucent(clamped < 1f)
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, clamped))

  override def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    val px          = metrics.toPixelX(x)
    val py          = metrics.toPixelY(y)
    val pw          = width * metrics.charWidth
    val ph          = height * metrics.lineHeight
    val inset       = math.ceil(strokeWidth / 2).toInt
    val savedStroke = g.getStroke
    painted.touched(px, py, pw, ph)
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
    userClipDepth.incrementAndGet()
    try
      g.clip(new Rectangle2D.Double(px, py, pw, ph))
      render
    finally
      userClipDepth.decrementAndGet()
      g.setClip(savedClip)

  override def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: RenderColor
  ): Unit =
    painted.touched(xPx, yPx, widthPx.max(1), heightPx.max(1))
    g.setColor(paintColor(color))
    g.fillRect(xPx, yPx, widthPx.max(1), heightPx.max(1))

  override def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
    val px        = metrics.toPixelX(x)
    val py        = metrics.toPixelY(y)
    val pw        = width * metrics.charWidth
    val ph        = height * metrics.lineHeight
    val savedClip = g.getClip
    painted.touched(px, py, pw, ph)
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
    blitLayer(layerImage, DeviceOrigin.Zero)

  override def compositeLayer(layerImage: RenderImage, bounds: PixelRect): Unit =
    blitLayer(layerImage, DeviceOrigin.of(bounds, deviceScaleX, deviceScaleY))

  private def blitLayer(layerImage: RenderImage, origin: DeviceOrigin): Unit =
    activity.layerComposited()
    painted.forgetClear()
    val rawGraphics = image.createGraphics()
    try
      val _ = rawGraphics.drawImage(layerImage.toAwt, origin.xPx, origin.yPx, Java2DRenderSurface.NoOpImageObserver)
    finally rawGraphics.dispose()

  /** In device pixels on an untransformed graphics, because `copyArea` is not implemented for a scaled one. A copy onto
    * an overlapping area runs bottom-up when it moves down, so no source row is overwritten before it is read.
    */
  override def copyPixels(from: PixelRect, dyPx: Int): Unit =
    painted.forgetClear()
    val raw = image.createGraphics()
    try
      raw.setComposite(AlphaComposite.Src)
      raw.copyArea(
        math.round(from.xPx * deviceScaleX).toInt,
        math.round(from.yPx * deviceScaleY).toInt,
        math.round(from.widthPx * deviceScaleX).toInt,
        math.round(from.heightPx * deviceScaleY).toInt,
        0,
        math.round(dyPx * deviceScaleY).toInt
      )
    finally raw.dispose()

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

  /** `Graphics2D` can only express "no clip" as a null `Shape`. */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def restoreClip(g: Graphics2D, saved: Option[Shape]): Unit = g.setClip(saved.orNull)

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
    recycled: Option[BufferedImage] = None,
    activity: SurfaceActivity = SurfaceActivity(),
    bounds: Option[PixelRect] = None
  ): Java2DRenderSurface =
    LayerSurfaces.build(
      metrics,
      font,
      logicalWidthPx,
      logicalHeightPx,
      deviceScaleX,
      deviceScaleY,
      onFlush,
      recycled,
      activity,
      bounds
    )

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
