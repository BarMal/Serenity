package com.serenity.ui.skia

import java.awt.Color
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{
  CellRuns,
  Effects,
  FontSpec,
  Java2DRenderSurface,
  PaintedPixels,
  PanelOutlineDrawing,
  PixelCopying,
  PixelDrawing,
  PixelLifting,
  RenderImage,
  RenderSurface,
  ScreenIdentity,
  StyledFontCache,
  SurfaceActivity,
  SurfaceContentIdentity,
  TextDrawing
}
import com.serenity.ui.renderer.LabelAnchor.isWhole
import com.serenity.ui.theme.TextStyle
import org.jetbrains.skia.{BlendMode, Canvas, IRect, Paint, PaintMode, PaintStrokeCap, PaintStrokeJoin, Rect, Surface}

/** A [[RenderSurface]] that draws with Skia onto a `Canvas`.
  *
  * Coordinates, geometry and capabilities follow [[Java2DRenderSurface]]: cell units through [[CellMetrics]], logical
  * pixels for runs and fills, and a device scale applied once to the canvas. Text is positioned by the caller run by
  * run, from the advances layout measured with AWT, so Skia only has to draw the glyphs of the same font at the x it is
  * handed; [[SkiaFontCache]] makes the Skia font match the AWT one.
  *
  * What Skia does not offer here is declared rather than faked. There are no [[LayerBufferSupport]] layers: a layer is
  * a `BufferedImage` ([[RenderImage]]), and routing every modal and panel through a CPU image would cost the GPU path
  * its point, so those are painted straight into the canvas. Content persistence and [[PixelCopying]] need pixels that
  * outlive a frame, which a window's GPU canvas does not have, so they exist only for a surface built over a retained
  * raster [[Surface]] ([[SkiaRenderSurface.retaining]]).
  *
  * Threading is that of the render pool: one thread draws a frame, and `onFlush` is called once when it ends.
  */
final class SkiaRenderSurface(
    canvas: Canvas,
    metrics: CellMetrics,
    font: FontSpec,
    onFlush: () => Unit,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScaleX: Double = 1.0,
    deviceScaleY: Double = 1.0,
    retainedSurface: Option[Surface] = None,
    opaque: Boolean = true,
    layerCacheOwnerOverride: Option[ScreenIdentity] = None,
    caches: SkiaTextCaches,
    styledFonts: StyledFontCache,
    override val activity: SurfaceActivity = SurfaceActivity()
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing
    with PixelCopying:
  import SkiaRenderSurface.*
  import caches.{fonts, images, shapedText}

  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = Some(this)
  override def panelOutlines: Option[PanelOutlineDrawing] = Some(this)

  override def addressesPixels: Boolean = true

  override def pixelCopying: Option[PixelCopying] =
    retainedSurface.filter(_ => isWhole(deviceScaleX) && isWhole(deviceScaleY)).map(_ => this)

  private val lifting =
    retainedSurface.map(kept => SkiaPixelLifting(kept, deviceScaleX, deviceScaleY, () => painted.forgetClear()))

  override def pixelLifting: Option[PixelLifting] = lifting

  override def layerCacheOwner: ScreenIdentity = layerCacheOwnerOverride.getOrElse(super.layerCacheOwner)

  override def persistentContentKey: Option[SurfaceContentIdentity] =
    retainedSurface.map(SurfaceContentIdentity(_))

  private val effectiveWidthPx  = logicalWidthPx.max(1)
  private val effectiveHeightPx = logicalHeightPx.max(1)
  private val cellGridWidthPx   = (effectiveWidthPx / metrics.charWidth) * metrics.charWidth
  private val cellGridHeightPx  = (effectiveHeightPx / metrics.lineHeight) * metrics.lineHeight

  private val baseSaveCount = canvas.save()
  val _                     = canvas.scale(deviceScaleX.toFloat, deviceScaleY.toFloat)

  private val renderContext =
    FontRenderContext(AffineTransform.getScaleInstance(deviceScaleX, deviceScaleY), true, true)

  private val fill   = Paint()
  private val glyphs = Paint()
  private val stroke = Paint()
  fill.setAntiAlias(false)
  glyphs.setAntiAlias(true)
  stroke.setAntiAlias(false)
  stroke.setMode(PaintMode.STROKE)
  stroke.setStrokeCap(PaintStrokeCap.SQUARE)
  stroke.setStrokeJoin(PaintStrokeJoin.MITER)

  private val fgRef                   = AtomicReference(Color.WHITE)
  private val bgRef                   = AtomicReference(Color.BLACK)
  private val lastPaintColorRef       = AtomicReference(Color.WHITE)
  private val baseFontRef             = AtomicReference(font.toAwt)
  private val currentFontRef          = AtomicReference(font.toAwt)
  private val alphaRef                = AtomicInteger(OpaqueAlpha)
  private val logicalPixelRowOverride = AtomicReference[Option[(Int, Int)]](None)

  private val painted       = PaintedPixels()
  private val underlineRef  = AtomicBoolean(false)
  private val userClipDepth = AtomicInteger(0)

  override def setFont(newFont: FontSpec): Unit =
    baseFontRef.set(newFont.toAwt)
    currentFontRef.set(newFont.toAwt)

  override def fontRenderContext: Option[FontRenderContext] = Some(renderContext)

  def setForegroundColor(color: RenderColor): Unit = fgRef.set(awtColor(color, fgRef.get()))
  def setBackgroundColor(color: RenderColor): Unit = bgRef.set(awtColor(color, bgRef.get()))
  def getBackgroundColor: RenderColor              = RenderColor.fromAwt(bgRef.get())

  private def paintColor(color: RenderColor): Color =
    val awt = awtColor(color, lastPaintColorRef.get())
    lastPaintColorRef.set(awt)
    awt

  /** `argb` with this frame's translucency (see [[setAlpha]]) applied to its own alpha. */
  private def blended(argb: Int): Int =
    val scale = alphaRef.get()
    if scale == OpaqueAlpha then argb else (((argb >>> 24) * scale / OpaqueAlpha) << 24) | (argb & 0x00ffffff)

  private def filled(color: Color): Paint =
    fill.setBlendMode(BlendMode.SRC_OVER)
    fill.setColor(blended(color.getRGB))
    fill

  private def paintRect(area: Rect, paint: Paint): Unit =
    val _ = canvas.drawRect(area, paint)

  private def restoreTo(saveCount: Int): Unit =
    val _ = canvas.restoreToCount(saveCount)

  private def rect(x: Float, y: Float, width: Float, height: Float): Rect =
    Rect.Companion.makeLTRB(x, y, x + width, y + height)

  /** Fill `px, py, pw, ph` with `color`, making the pixels exactly `color` even when it is fully transparent (alpha 0,
    * the "use the desktop's own backdrop" sentinel, #1240), which a source-over fill would leave untouched. A surface
    * with no alpha of its own shows black through a transparent pixel, so it gets that black.
    */
  private def fillBackground(color: Color, px: Int, py: Int, pw: Int, ph: Int): Unit =
    if color.getAlpha == 0 then
      fill.setBlendMode(BlendMode.SRC)
      fill.setColor(if opaque then OpaqueBlack else 0)
      paintRect(rect(px.toFloat, py.toFloat, pw.toFloat, ph.toFloat), fill)
      fill.setBlendMode(BlendMode.SRC_OVER)
    else paintRect(rect(px.toFloat, py.toFloat, pw.toFloat, ph.toFloat), filled(color))

  override def clearViewport(color: RenderColor): Unit =
    setBackgroundColor(color)
    fillBackground(bgRef.get(), 0, 0, effectiveWidthPx, effectiveHeightPx)
    painted.cleared(
      bgRef.get(),
      scala.collection.immutable.List(PixelRect(0, 0, effectiveWidthPx, effectiveHeightPx)),
      clipped = userClipDepth.get() > 0
    )

  override def clearViewportExcept(color: RenderColor, preserved: scala.collection.immutable.List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      setBackgroundColor(color)
      val background = bgRef.get()
      val uncovered  = PixelRect.uncoveredWithin(PixelRect(0, 0, effectiveWidthPx, effectiveHeightPx), preserved)
      uncovered.foreach(area => fillBackground(background, area.xPx, area.yPx, area.widthPx, area.heightPx))
      painted.cleared(background, uncovered, clipped = userClipDepth.get() > 0)

  override def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit =
    paintRun(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s.nonEmpty, clipGlyphToRun) {
      drawText(s, 0, s.length, xPx, (yPx + ascentPx).toFloat)
    }

  override def drawShapedRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    stretch: String,
    from: Int,
    until: Int,
    clipGlyphToRun: Boolean = false
  ): Unit =
    paintRun(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, from < until, clipGlyphToRun) {
      val shapedBefore = shapedText.shapings
      val shaped       = shapedText.stretchOf(stretch, fonts.faceFor(currentFontRef.get()).font)
      activity.shaped(shapedText.shapings - shapedBefore)
      shaped.draw(canvas, from, until, xPx, (yPx + ascentPx).toFloat, glyphPaint())
    }

  override def withFallbackFamily(family: String)(draw: => Unit): Unit =
    val surfaceFont = currentFontRef.get()
    currentFontRef.set(styledFonts.fallbackFontFor(surfaceFont, family))
    try draw
    finally currentFontRef.set(surfaceFont)

  override def shapesStretchesOf(text: String): Boolean =
    SkiaShapedTextCache.worthShaping(baseFontRef.get(), text)

  /** Draws `s[from, until)` with its first glyph at `x`, shaped when the font or the text asks for it. */
  private def drawText(s: String, from: Int, until: Int, x: Float, baseline: Float): Unit =
    val face = fonts.faceFor(currentFontRef.get())
    val ink  = glyphPaint()
    if SkiaShapedTextCache.worthShaping(currentFontRef.get(), s) then
      val before = shapedText.shapings
      shapedText.stretchOf(s, face.font).draw(canvas, from, until, x, baseline, ink)
      activity.shaped(shapedText.shapings - before)
    else
      val _ = canvas.drawString(s.substring(from, until), x, baseline, face.font, ink)

  private def glyphPaint(): Paint =
    glyphs.setBlendMode(BlendMode.SRC_OVER)
    glyphs.setColor(blended(fgRef.get().getRGB))
    glyphs

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
        val saved = canvas.save()
        try
          if clipGlyphToRun then clipTo(boundedLeft, boundedTop, boundedWidth, boundedHeight)
          else clipTo(0, 0, cellGridWidthPx, cellGridHeightPx)
          drawGlyphs
        finally restoreTo(saved)
        drawRules(boundedLeft, boundedWidth, yPx + ascentPx)
        painted.ranOver(boundedLeft, boundedTop, boundedRight, boundedBottom)

  private def clipTo(x: Int, y: Int, width: Int, height: Int): Unit =
    val _ = canvas.clipRect(rect(x.toFloat, y.toFloat, width.toFloat, height.toFloat), false)

  private def runLeft(xPx: Float): Int = math.floor(xPx.toDouble).toInt.max(0).min(cellGridWidthPx)

  private def runRight(xPx: Float, bgWidthPx: Float): Int =
    math.ceil((xPx + bgWidthPx).toDouble).toInt.max(0).min(cellGridWidthPx)

  private def runTop(yPx: Int): Int = yPx.max(0).min(cellGridHeightPx)

  private def runBottom(yPx: Int, lineHeightPx: Int): Int = (yPx + lineHeightPx).max(0).min(cellGridHeightPx)

  /** Underline and strikethrough are rules under or through the run rather than font attributes, which Skia has none
    * of; each is never thinner than one device pixel.
    */
  private def drawRules(leftPx: Int, widthPx: Int, baselinePx: Int): Unit =
    if widthPx > 0 then
      val awt = currentFontRef.get()
      if underlineRef.get() then drawRule(leftPx, widthPx, baselinePx, styledFonts.underlineOf(awt))
      fonts.faceFor(awt).strikethrough.foreach(drawRule(leftPx, widthPx, baselinePx, _))

  private def drawRule(leftPx: Int, widthPx: Int, baselinePx: Int, rule: StyledFontCache.Underline): Unit =
    val thickness = rule.thicknessPx.max((1.0 / deviceScaleY).toFloat)
    paintRect(rect(leftPx.toFloat, baselinePx + rule.offsetPx, widthPx.toFloat, thickness), filled(fgRef.get()))

  def putString(x: Int, y: Int, s: String): Unit =
    if s.nonEmpty then
      val px    = metrics.toPixelX(x)
      val py    = pixelYForRow(y)
      val width = CellRuns.cellsIn(s) * metrics.charWidth
      fillBackground(bgRef.get(), px, py, width, metrics.lineHeight)
      painted.touched(px, py, width, metrics.lineHeight)
      CellRuns.pieces(s).foreach { (cell, piece) =>
        drawText(piece, 0, piece.length, (px + cell * metrics.charWidth).toFloat, (py + metrics.ascent).toFloat)
      }
      drawRules(px, width, py + metrics.ascent)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    val px = metrics.toPixelX(x)
    val py = pixelYForRow(y)
    val pw = width * metrics.charWidth
    val ph = height * metrics.lineHeight
    fillBackground(bgRef.get(), px, py, pw, ph)
    painted.touched(px, py, pw, ph)
    if char != ' ' then
      val glyph = char.toString
      (0 until height).foreach { row =>
        (0 until width).foreach { col =>
          drawText(
            glyph,
            0,
            1,
            metrics.toPixelX(x + col).toFloat,
            (metrics.toPixelY(y + row) + metrics.ascent).toFloat
          )
        }
      }

  override def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit =
    val previous = logicalPixelRowOverride.getAndSet(Some(cellRow -> pixelY))
    try render
    finally logicalPixelRowOverride.set(previous)

  override def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    val saved = canvas.save()
    try
      val _ = canvas.translate(xPx.toFloat, yPx.toFloat)
      painted.withTranslation(xPx, yPx)(render)
    finally restoreTo(saved)

  private def pixelYForRow(row: Int): Int =
    logicalPixelRowOverride
      .get()
      .collect { case (cellRow, pixelY) if cellRow == row => pixelY }
      .getOrElse(metrics.toPixelY(row))

  def enableStyle(style: TextStyle): Unit =
    underlineRef.set(style.isUnderlined)
    currentFontRef.set(styledFonts.fontFor(baseFontRef.get(), style))

  def disableStyle(style: TextStyle): Unit =
    underlineRef.set(false)
    currentFontRef.set(baseFontRef.get())

  override def setAlpha(alpha: Float): Unit =
    val clamped = alpha.max(0f).min(1f)
    painted.setTranslucent(clamped < 1f)
    alphaRef.set(math.round(clamped * OpaqueAlpha))

  override def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    val px    = metrics.toPixelX(x)
    val py    = metrics.toPixelY(y)
    val pw    = width * metrics.charWidth
    val ph    = height * metrics.lineHeight
    val inset = math.ceil(strokeWidth / 2).toInt
    painted.touched(px, py, pw, ph)
    stroke.setBlendMode(BlendMode.SRC_OVER)
    stroke.setColor(blended(paintColor(color).getRGB))
    stroke.setStrokeWidth(strokeWidth)
    paintRect(
      rect((px + inset).toFloat, (py + inset).toFloat, (pw - 2 * inset).toFloat, (ph - 2 * inset).toFloat),
      stroke
    )

  override def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    val saved = canvas.save()
    userClipDepth.incrementAndGet()
    try
      clipTo(metrics.toPixelX(x), metrics.toPixelY(y), width * metrics.charWidth, height * metrics.lineHeight)
      render
    finally
      userClipDepth.decrementAndGet()
      restoreTo(saved)

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
    painted.touched(xPx, yPx, widthPx.max(1), heightPx.max(1))
    paintRect(
      rect(xPx.toFloat, yPx.toFloat, widthPx.max(1).toFloat, heightPx.max(1).toFloat),
      filled(paintColor(color))
    )

  override def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
    val px     = metrics.toPixelX(x)
    val py     = metrics.toPixelY(y)
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val skia   = images.imageOf(image.toAwt)
    val saved  = canvas.save()
    val source = rect(0f, 0f, skia.getWidth.toFloat, skia.getHeight.toFloat)
    val target = rect(px.toFloat, py.toFloat, pw.toFloat, ph.toFloat)
    val ink    = filled(Color.WHITE)
    painted.touched(px, py, pw, ph)
    try
      clipTo(px, py, pw, ph)
      val _ = canvas.drawImageRect(skia, source, target, ink)
    finally restoreTo(saved)

  /** In device pixels on an untransformed canvas, since the copy is of whole device pixels. The source is snapshotted
    * before anything is drawn, so an overlapping copy reads every source pixel as it was. A clip in force (inside
    * [[withRectClip]]) still bounds the copy, which Java2D's raw graphics would ignore.
    */
  override def copyPixels(from: PixelRect, dyPx: Int): Unit =
    painted.forgetClear()
    retainedSurface.foreach { surface =>
      val left   = math.round(from.xPx * deviceScaleX).toInt
      val top    = math.round(from.yPx * deviceScaleY).toInt
      val width  = math.round(from.widthPx * deviceScaleX).toInt
      val height = math.round(from.heightPx * deviceScaleY).toInt
      val moved  = math.round(dyPx * deviceScaleY).toInt
      val snap   = surface.makeImageSnapshot(IRect.Companion.makeXYWH(left, top, width, height))
      val saved  = canvas.save()
      try
        val _ = canvas.resetMatrix()
        fill.setBlendMode(BlendMode.SRC)
        fill.setColor(OpaqueBlack)
        val _ = canvas.drawImage(snap, left.toFloat, (top + moved).toFloat, fill)
        fill.setBlendMode(BlendMode.SRC_OVER)
      finally
        restoreTo(saved)
        snap.close()
    }

  def hideCursor(): Unit = ()

  def viewportWidth: Int                 = effectiveWidthPx / metrics.charWidth
  def viewportHeight: Int                = effectiveHeightPx / metrics.lineHeight
  override def devicePixelScaleX: Double = deviceScaleX
  override def devicePixelScaleY: Double = deviceScaleY

  def flush(): Unit =
    restoreTo(baseSaveCount)
    onFlush()

object SkiaRenderSurface:

  private val OpaqueAlpha = 255
  private val OpaqueBlack = 0xff000000

  /** Reuses `current` when it already carries `color`, so a renderer re-setting the same theme colour on every run does
    * not allocate a fresh `java.awt.Color` each time.
    */
  private def awtColor(color: RenderColor, current: Color): Color =
    if current.getRGB == color.argb then current else color.toAwt

  /** A surface over the canvas of a window's render callback (a `SkiaLayer`'s `onRender`, or any `Canvas`). The canvas
    * is in device pixels and is scaled here by the device scale, so `logicalWidthPx` and `logicalHeightPx` are the
    * layer's size in logical pixels. Nothing is retained between frames: every frame starts from unknown pixels, so the
    * renderer draws everything, and a pixel copy is not offered.
    */
  def onCanvas(
    canvas: Canvas,
    metrics: CellMetrics,
    font: FontSpec,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScale: Double,
    caches: SkiaTextCaches,
    styledFonts: StyledFontCache,
    onFlush: () => Unit = () => (),
    layerCacheOwner: Option[ScreenIdentity] = None
  ): SkiaRenderSurface =
    new SkiaRenderSurface(
      canvas,
      metrics,
      font,
      onFlush,
      logicalWidthPx,
      logicalHeightPx,
      deviceScale,
      deviceScale,
      layerCacheOwnerOverride = layerCacheOwner,
      caches = caches,
      styledFonts = styledFonts
    )

  /** A surface over a raster `Surface` the caller keeps between frames. It promises what a retained image does: the
    * pixels of the last frame flushed through a surface over the same `Surface` are still there, so a renderer may skip
    * unchanged rows and copy scrolled ones. Hand out the same `Surface` for the frames that should share pixels, and a
    * different one (or [[onCanvas]]) for those that should not.
    */
  def retaining(
    surface: Surface,
    metrics: CellMetrics,
    font: FontSpec,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScale: Double,
    caches: SkiaTextCaches,
    styledFonts: StyledFontCache,
    onFlush: () => Unit = () => (),
    layerCacheOwner: Option[ScreenIdentity] = None
  ): SkiaRenderSurface =
    new SkiaRenderSurface(
      surface.getCanvas,
      metrics,
      font,
      onFlush,
      logicalWidthPx,
      logicalHeightPx,
      deviceScale,
      deviceScale,
      retainedSurface = Some(surface),
      layerCacheOwnerOverride = layerCacheOwner,
      caches = caches,
      styledFonts = styledFonts
    )

  /** The device size of a surface that holds `logicalPx` at `deviceScale`, as [[Java2DRenderSurface]] sizes its image.
    */
  def deviceDimension(logicalPx: Int, deviceScale: Double): Int =
    Java2DRenderSurface.deviceImageDimension(logicalPx, deviceScale)
