package com.serenity

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect, TextLayoutSnapshot}
import com.serenity.ui.renderer.{
  Effects,
  FontSpec,
  HardwareCursor,
  PanelOutlineDrawing,
  PixelDrawing,
  RenderImage,
  RenderSurface,
  SurfaceContentIdentity,
  TextDrawing
}
import com.serenity.ui.theme.TextStyle

/** In-memory RenderSurface for renderer tests. Records putString calls so assertions can inspect what was drawn at each
  * (x, y) position.
  *
  * `persistentContent` models a surface whose pixels survive between frames, which is what lets a test drive the
  * renderer's dirty-region path. It is off by default so that a plain mock behaves like a fresh image every frame.
  *
  * `fontRenderContextOverride` defaults to `Some(defaultFontRenderContext())` so existing specs keep exercising the
  * measured pixel-run path unchanged; pass `None` to model a cell-only surface (e.g. a terminal) for #1105's
  * cell-fallback specs.
  *
  * Implements every capability trait so renderer tests exercise the full drawing surface rather than silently no-op'ing
  * on operations a real surface supports -- see #1012.
  */
class MockRenderSurface(
    val width: Int,
    val height: Int,
    persistentContent: Boolean = false,
    fontRenderContextOverride: Option[FontRenderContext] = Some(TextLayoutSnapshot.defaultFontRenderContext()),
    hardwareCursorOverride: Option[HardwareCursor] = None
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing:

  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = Some(this)
  override def panelOutlines: Option[PanelOutlineDrawing] = Some(this)
  override def hardwareCursor: Option[HardwareCursor]     = hardwareCursorOverride
  final case class PixelTranslationCall(xPx: Double, yPx: Double)
  private val pixelTranslationCallsBuffer = scala.collection.mutable.ListBuffer.empty[PixelTranslationCall]
  private val currentPixelTranslation     = AtomicReference(PixelTranslationCall(0.0, 0.0))
  final case class PutStringCall(x: Int, y: Int, s: String)
  final case class PutStringPixelYCall(x: Int, y: Int, pixelY: Int, text: String)

  private val chars                      = Array.fill(height, width)(' ')
  private val fgs                        = Array.fill(height, width)(RenderColor.White.argb)
  private val bgs                        = Array.fill(height, width)(RenderColor.Black.argb)
  private val putStringCallsBuffer       = scala.collection.mutable.ListBuffer.empty[PutStringCall]
  private val putStringPixelYCallsBuffer = scala.collection.mutable.ListBuffer.empty[PutStringPixelYCall]

  private val currentFg          = AtomicReference[RenderColor](RenderColor.White)
  private val currentBg          = AtomicReference[RenderColor](RenderColor.Black)
  private val currentAlpha       = AtomicReference[Float](1.0f)
  private val currentFont        = AtomicReference[Option[Font]](None)
  private val setFontCallsBuffer = scala.collection.mutable.ListBuffer.empty[Font]
  final case class StyleCall(action: String, style: TextStyle)
  private val styleCallsBuffer = scala.collection.mutable.ListBuffer.empty[StyleCall]

  override def setFont(font: FontSpec): Unit =
    currentFont.set(Some(font.toAwt))
    setFontCallsBuffer += font.toAwt

  override def fontRenderContext: Option[FontRenderContext] = fontRenderContextOverride

  def setFontCalls: List[Font] = setFontCallsBuffer.toList

  override def persistentContentKey: Option[SurfaceContentIdentity] =
    Option.when(persistentContent)(SurfaceContentIdentity(this))

  override def clearViewportExcept(color: RenderColor, preserved: scala.collection.immutable.List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      val metrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      setBackgroundColor(color)
      for y <- 0 until height; x <- 0 until width do
        val cellLeftPx = x * metrics.charWidth
        val cellTopPx  = y * metrics.lineHeight
        val kept = preserved.exists { rect =>
          cellLeftPx >= rect.xPx && cellLeftPx < rect.rightPx &&
          cellTopPx >= rect.yPx && cellTopPx < rect.bottomPx
        }
        if !kept then
          chars(y)(x) = ' '
          bgs(y)(x) = color.argb

  def setForegroundColor(color: RenderColor): Unit = currentFg.set(color)
  def setBackgroundColor(color: RenderColor): Unit = currentBg.set(color)
  def getBackgroundColor: RenderColor              = currentBg.get()

  def putString(x: Int, y: Int, s: String): Unit =
    putStringCallsBuffer += PutStringCall(x, y, s)
    putStringPixelYCallsBuffer += PutStringPixelYCall(x, y, pixelYForRow(y), s)
    s.zipWithIndex.foreach { (c, i) =>
      val px = x + i
      if y >= 0 && y < height && px >= 0 && px < width then
        chars(y)(px) = c
        fgs(y)(px) = currentFg.get().argb
        bgs(y)(px) = currentBg.get().argb
    }

  final case class FillRectCall(
      x: Int,
      y: Int,
      w: Int,
      h: Int,
      char: Char,
      foreground: RenderColor,
      background: RenderColor
  )

  private val fillRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[FillRectCall]

  def fillRect(x: Int, y: Int, w: Int, h: Int, char: Char): Unit =
    fillRectCallsBuffer += FillRectCall(x, y, w, h, char, currentFg.get(), currentBg.get())
    for dy <- 0 until h; dx <- 0 until w do
      val px = x + dx
      val py = y + dy
      if py >= 0 && py < height && px >= 0 && px < width then
        chars(py)(px) = char
        bgs(py)(px) = currentBg.get().argb

  def fillRectCalls: List[FillRectCall] = fillRectCallsBuffer.toList

  final case class DrawRunPxCall(
      xPx: Float,
      yPx: Int,
      bgWidthPx: Float,
      lineHeightPx: Int,
      ascentPx: Int,
      s: String,
      foreground: RenderColor,
      background: RenderColor,
      font: Option[Font],
      clipGlyphToRun: Boolean,
      activeStyle: TextStyle,
      translationXPx: Double
  )

  private val drawRunPxCallsBuffer = scala.collection.mutable.ListBuffer.empty[DrawRunPxCall]

  /** The `TextStyle` a real `Java2DRenderSurface` would currently be painting with: whatever `enableStyle` last set, or
    * `TextStyle.normal` once `disableStyle` has reverted to the base (unstyled) font -- mirroring
    * `Java2DRenderSurface.disableStyle`'s unconditional `g.setFont(baseFontRef.get())`, which drops back to no style
    * override regardless of which style was passed in. Recorded onto every [[DrawRunPxCall]] so a test can assert two
    * draws of the same range resolved the same style, the way #1482 did not.
    */
  private val currentStyle = AtomicReference[TextStyle](TextStyle.normal)

  override def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit =
    drawRunPxCallsBuffer += DrawRunPxCall(
      xPx,
      yPx,
      bgWidthPx,
      lineHeightPx,
      ascentPx,
      s,
      currentFg.get(),
      currentBg.get(),
      currentFont.get(),
      clipGlyphToRun,
      currentStyle.get(),
      currentPixelTranslation.get().xPx
    )
    val metrics =
      currentFont
        .get()
        .map(CellMetrics.fromFont)
        .getOrElse(CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)))
    val startX = math.floor(xPx / metrics.charWidth.toDouble).toInt
    val endX   = math.max(startX + s.length, startX + 1)
    val row    = math.floor(yPx / metrics.lineHeight.toDouble).toInt

    if row >= 0 && row < height then
      (startX until endX).foreach { x => if x >= 0 && x < width then bgs(row)(x) = currentBg.get().argb }

      s.zipWithIndex.foreach {
        case (char, index) =>
          val x = startX + index
          if x >= 0 && x < width then
            chars(row)(x) = char
            fgs(row)(x) = currentFg.get().argb
      }

  def drawRunPxCalls: List[DrawRunPxCall] = drawRunPxCallsBuffer.toList

  private val pixelRowOverride = AtomicReference[Option[(Int, Int)]](None)

  override def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit =
    val previous = pixelRowOverride.getAndSet(Some(cellRow -> pixelY))
    try render
    finally pixelRowOverride.set(previous)

  override def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    val previous = currentPixelTranslation.getAndUpdate(translation =>
      PixelTranslationCall(translation.xPx + xPx, translation.yPx + yPx)
    )
    pixelTranslationCallsBuffer += PixelTranslationCall(xPx, yPx)
    try render
    finally currentPixelTranslation.set(previous)

  private def pixelYForRow(row: Int): Int =
    pixelRowOverride.get().collect { case (cellRow, pixelY) if cellRow == row => pixelY }.getOrElse {
      CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)).toPixelY(row)
    }

  final case class StrokeRectCall(x: Int, y: Int, w: Int, h: Int, color: RenderColor, strokeWidth: Float)
  private val strokeRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[StrokeRectCall]
  final case class FillPixelRectCall(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor)
  final case class DrawImageCall(image: BufferedImage, x: Int, y: Int, width: Int, height: Int)
  private val fillPixelRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[FillPixelRectCall]
  private val drawImageCallsBuffer     = scala.collection.mutable.ListBuffer.empty[DrawImageCall]
  private val alphaCallsBuffer         = scala.collection.mutable.ListBuffer.empty[Float]

  override def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    strokeRectCallsBuffer += StrokeRectCall(x, y, width, height, color, strokeWidth)

  def strokeRectCalls: List[StrokeRectCall] = strokeRectCallsBuffer.toList

  final case class RectClipCall(x: Int, y: Int, width: Int, height: Int)
  private val rectClipCallsBuffer = scala.collection.mutable.ListBuffer.empty[RectClipCall]

  override def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    rectClipCallsBuffer += RectClipCall(x, y, width, height)
    render

  def rectClipCalls: List[RectClipCall] = rectClipCallsBuffer.toList

  override def setAlpha(alpha: Float): Unit =
    currentAlpha.set(alpha)
    alphaCallsBuffer += alpha

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
    fillPixelRectCallsBuffer += FillPixelRectCall(xPx, yPx, widthPx, heightPx, color)

  override def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
    drawImageCallsBuffer += DrawImageCall(image.toAwt, x, y, width, height)

  def currentAlphaValue: Float                          = currentAlpha.get()
  def fillPixelRectCalls: List[FillPixelRectCall]       = fillPixelRectCallsBuffer.toList
  def drawImageCalls: List[DrawImageCall]               = drawImageCallsBuffer.toList
  def alphaCalls: List[Float]                           = alphaCallsBuffer.toList
  def putStringCalls: List[PutStringCall]               = putStringCallsBuffer.toList
  def putStringPixelYCalls: List[PutStringPixelYCall]   = putStringPixelYCallsBuffer.toList
  def pixelTranslationCalls: List[PixelTranslationCall] = pixelTranslationCallsBuffer.toList

  def enableStyle(style: TextStyle): Unit =
    styleCallsBuffer += StyleCall("enable", style)
    currentStyle.set(style)

  def disableStyle(style: TextStyle): Unit =
    styleCallsBuffer += StyleCall("disable", style)
    currentStyle.set(TextStyle.normal)

  def hideCursor(): Unit          = ()
  def viewportWidth: Int          = width
  def viewportHeight: Int         = height
  def flush(): Unit               = ()
  def styleCalls: List[StyleCall] = styleCallsBuffer.toList

  def getChar(x: Int, y: Int): Char =
    if y >= 0 && y < height && x >= 0 && x < width then chars(y)(x) else ' '

  def getFg(x: Int, y: Int): RenderColor =
    if y >= 0 && y < height && x >= 0 && x < width then RenderColor.fromArgb(fgs(y)(x)) else RenderColor.White

  def getBg(x: Int, y: Int): RenderColor =
    if y >= 0 && y < height && x >= 0 && x < width then
      val metrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      fillPixelRectCallsBuffer
        .findLast { call =>
          x * metrics.charWidth >= call.xPx && x * metrics.charWidth < call.xPx + call.widthPx &&
          y * metrics.lineHeight >= call.yPx && y * metrics.lineHeight < call.yPx + call.heightPx
        }
        .map(_.color)
        .getOrElse(RenderColor.fromArgb(bgs(y)(x)))
    else RenderColor.Black

  def getRow(y: Int): String =
    if y >= 0 && y < height then chars(y).mkString else ""

  def clear(): Unit =
    putStringCallsBuffer.clear()
    putStringPixelYCallsBuffer.clear()
    pixelTranslationCallsBuffer.clear()
    strokeRectCallsBuffer.clear()
    rectClipCallsBuffer.clear()
    fillPixelRectCallsBuffer.clear()
    drawImageCallsBuffer.clear()
    alphaCallsBuffer.clear()
    drawRunPxCallsBuffer.clear()
    styleCallsBuffer.clear()
    fillRectCallsBuffer.clear()
    currentStyle.set(TextStyle.normal)
    for y <- 0 until height; x <- 0 until width do
      chars(y)(x) = ' '
      fgs(y)(x) = RenderColor.White.argb
      bgs(y)(x) = RenderColor.Black.argb
