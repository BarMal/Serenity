package com.serenity

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.renderer.RenderSurface
import com.serenity.ui.theme.TextStyle

/** In-memory RenderSurface for renderer tests. Records putString calls so assertions can inspect what was drawn at each
  * (x, y) position.
  */
class MockRenderSurface(val width: Int, val height: Int) extends RenderSurface:
  case class PixelTranslationCall(xPx: Double, yPx: Double)
  private val pixelTranslationCallsBuffer = scala.collection.mutable.ListBuffer.empty[PixelTranslationCall]
  case class PutStringCall(x: Int, y: Int, s: String)
  case class PutStringPixelYCall(x: Int, y: Int, pixelY: Int, text: String)

  private val chars                      = Array.fill(height, width)(' ')
  private val fgs                        = Array.fill(height, width)(Color.WHITE)
  private val bgs                        = Array.fill(height, width)(Color.BLACK)
  private val putStringCallsBuffer       = scala.collection.mutable.ListBuffer.empty[PutStringCall]
  private val putStringPixelYCallsBuffer = scala.collection.mutable.ListBuffer.empty[PutStringPixelYCall]

  private val currentFg          = AtomicReference[Color](Color.WHITE)
  private val currentBg          = AtomicReference[Color](Color.BLACK)
  private val currentAlpha       = AtomicReference[Float](1.0f)
  private val currentFont        = AtomicReference[Option[Font]](None)
  private val setFontCallsBuffer = scala.collection.mutable.ListBuffer.empty[Font]
  case class StyleCall(action: String, style: TextStyle)
  private val styleCallsBuffer = scala.collection.mutable.ListBuffer.empty[StyleCall]

  override def setFont(font: Font): Unit =
    currentFont.set(Some(font))
    setFontCallsBuffer += font

  override def fontRenderContext: Option[FontRenderContext] =
    Some(TextLayoutSnapshot.defaultFontRenderContext())

  def setFontCalls: List[Font] = setFontCallsBuffer.toList

  def setForegroundColor(color: Color): Unit = currentFg.set(color)
  def setBackgroundColor(color: Color): Unit = currentBg.set(color)
  def getBackgroundColor: Color              = currentBg.get()

  def putString(x: Int, y: Int, s: String): Unit =
    putStringCallsBuffer += PutStringCall(x, y, s)
    putStringPixelYCallsBuffer += PutStringPixelYCall(x, y, pixelYForRow(y), s)
    s.zipWithIndex.foreach { (c, i) =>
      val px = x + i
      if y >= 0 && y < height && px >= 0 && px < width then
        chars(y)(px) = c
        fgs(y)(px) = currentFg.get()
        bgs(y)(px) = currentBg.get()
    }

  def fillRect(x: Int, y: Int, w: Int, h: Int, char: Char): Unit =
    for dy <- 0 until h; dx <- 0 until w do
      val px = x + dx
      val py = y + dy
      if py >= 0 && py < height && px >= 0 && px < width then
        chars(py)(px) = char
        bgs(py)(px) = currentBg.get()

  case class DrawRunPxCall(
      xPx: Float,
      yPx: Int,
      bgWidthPx: Float,
      lineHeightPx: Int,
      ascentPx: Int,
      s: String,
      foreground: Color,
      background: Color,
      font: Option[Font]
  )

  private val drawRunPxCallsBuffer = scala.collection.mutable.ListBuffer.empty[DrawRunPxCall]

  override def drawRunPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    drawRunPxCallsBuffer += DrawRunPxCall(
      xPx,
      yPx,
      bgWidthPx,
      lineHeightPx,
      ascentPx,
      s,
      currentFg.get(),
      currentBg.get(),
      currentFont.get()
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
      (startX until endX).foreach { x => if x >= 0 && x < width then bgs(row)(x) = currentBg.get() }

      s.zipWithIndex.foreach {
        case (char, index) =>
          val x = startX + index
          if x >= 0 && x < width then
            chars(row)(x) = char
            fgs(row)(x) = currentFg.get()
      }

  def drawRunPxCalls: List[DrawRunPxCall] = drawRunPxCallsBuffer.toList

  private val pixelRowOverride = AtomicReference[Option[(Int, Int)]](None)

  override def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit =
    val previous = pixelRowOverride.getAndSet(Some(cellRow -> pixelY))
    try render
    finally pixelRowOverride.set(previous)

  override def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    pixelTranslationCallsBuffer += PixelTranslationCall(xPx, yPx)
    render

  private def pixelYForRow(row: Int): Int =
    pixelRowOverride.get().collect { case (cellRow, pixelY) if cellRow == row => pixelY }.getOrElse {
      CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)).toPixelY(row)
    }

  case class StrokeRoundRectCall(x: Int, y: Int, w: Int, h: Int, arcPx: Int, color: Color, strokeWidth: Float)
  private val strokeRoundRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[StrokeRoundRectCall]
  case class BlurRegionCall(x: Int, y: Int, width: Int, height: Int, radius: Float)
  private val blurRegionCallsBuffer = scala.collection.mutable.ListBuffer.empty[BlurRegionCall]
  case class FillPixelRectCall(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color)
  case class DrawImageCall(image: BufferedImage, x: Int, y: Int, width: Int, height: Int)
  private val fillPixelRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[FillPixelRectCall]
  private val drawImageCallsBuffer     = scala.collection.mutable.ListBuffer.empty[DrawImageCall]
  private val alphaCallsBuffer         = scala.collection.mutable.ListBuffer.empty[Float]

  override def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit =
    strokeRoundRectCallsBuffer += StrokeRoundRectCall(x, y, width, height, arcPx, color, strokeWidth)

  def strokeRoundRectCalls: List[StrokeRoundRectCall] = strokeRoundRectCallsBuffer.toList

  override def withRoundRectClip(
    _x: Int,
    _y: Int,
    _width: Int,
    _height: Int,
    _arcPx: Int
  )(render: => Unit): Unit = render

  override def setAlpha(alpha: Float): Unit =
    currentAlpha.set(alpha)
    alphaCallsBuffer += alpha

  override def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit =
    blurRegionCallsBuffer += BlurRegionCall(x, y, width, height, radius)

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit =
    fillPixelRectCallsBuffer += FillPixelRectCall(xPx, yPx, widthPx, heightPx, color)

  override def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit =
    drawImageCallsBuffer += DrawImageCall(image, x, y, width, height)

  def currentAlphaValue: Float                          = currentAlpha.get()
  def blurRegionCalls: List[BlurRegionCall]             = blurRegionCallsBuffer.toList
  def fillPixelRectCalls: List[FillPixelRectCall]       = fillPixelRectCallsBuffer.toList
  def drawImageCalls: List[DrawImageCall]               = drawImageCallsBuffer.toList
  def alphaCalls: List[Float]                           = alphaCallsBuffer.toList
  def putStringCalls: List[PutStringCall]               = putStringCallsBuffer.toList
  def putStringPixelYCalls: List[PutStringPixelYCall]   = putStringPixelYCallsBuffer.toList
  def pixelTranslationCalls: List[PixelTranslationCall] = pixelTranslationCallsBuffer.toList

  def enableStyle(style: TextStyle): Unit  = styleCallsBuffer += StyleCall("enable", style)
  def disableStyle(style: TextStyle): Unit = styleCallsBuffer += StyleCall("disable", style)
  def hideCursor(): Unit                   = ()
  def viewportWidth: Int                   = width
  def viewportHeight: Int                  = height
  def flush(): Unit                        = ()
  def styleCalls: List[StyleCall]          = styleCallsBuffer.toList

  def getChar(x: Int, y: Int): Char =
    if y >= 0 && y < height && x >= 0 && x < width then chars(y)(x) else ' '

  def getFg(x: Int, y: Int): Color =
    if y >= 0 && y < height && x >= 0 && x < width then fgs(y)(x) else Color.WHITE

  def getBg(x: Int, y: Int): Color =
    if y >= 0 && y < height && x >= 0 && x < width then
      val metrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      fillPixelRectCallsBuffer
        .findLast { call =>
          x * metrics.charWidth >= call.xPx && x * metrics.charWidth < call.xPx + call.widthPx &&
          y * metrics.lineHeight >= call.yPx && y * metrics.lineHeight < call.yPx + call.heightPx
        }
        .map(_.color)
        .getOrElse(bgs(y)(x))
    else Color.BLACK

  def getRow(y: Int): String =
    if y >= 0 && y < height then chars(y).mkString else ""

  def clear(): Unit =
    putStringCallsBuffer.clear()
    putStringPixelYCallsBuffer.clear()
    pixelTranslationCallsBuffer.clear()
    strokeRoundRectCallsBuffer.clear()
    blurRegionCallsBuffer.clear()
    fillPixelRectCallsBuffer.clear()
    drawImageCallsBuffer.clear()
    alphaCallsBuffer.clear()
    drawRunPxCallsBuffer.clear()
    styleCallsBuffer.clear()
    for y <- 0 until height; x <- 0 until width do
      chars(y)(x) = ' '
      fgs(y)(x) = Color.WHITE
      bgs(y)(x) = Color.BLACK
