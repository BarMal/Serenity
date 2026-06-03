package com.serenity

import java.awt.Color
import java.awt.Font
import com.serenity.ui.renderer.RenderSurface
import com.serenity.ui.theme.TextStyle
import com.serenity.ui.layout.CellMetrics

/** In-memory RenderSurface for renderer tests. Records putString calls so assertions can inspect
  * what was drawn at each (x, y) position.
  */
class MockRenderSurface(val width: Int, val height: Int) extends RenderSurface:
  case class PutStringCall(x: Int, y: Int, s: String)

  private val chars = Array.fill(height, width)(' ')
  private val fgs   = Array.fill(height, width)(Color.WHITE)
  private val bgs   = Array.fill(height, width)(Color.BLACK)
  private val putStringCallsBuffer = scala.collection.mutable.ListBuffer.empty[PutStringCall]

  private var currentFg: Color = Color.WHITE
  private var currentBg: Color = Color.BLACK
  private var currentAlpha: Float = 1.0f
  private var currentFont: Option[Font] = None
  private val setFontCallsBuffer = scala.collection.mutable.ListBuffer.empty[Font]

  override def setFont(font: Font): Unit =
    currentFont = Some(font)
    setFontCallsBuffer += font

  def setFontCalls: List[Font] = setFontCallsBuffer.toList

  def setForegroundColor(color: Color): Unit = currentFg = color
  def setBackgroundColor(color: Color): Unit = currentBg = color
  def getBackgroundColor: Color              = currentBg

  def putString(x: Int, y: Int, s: String): Unit =
    putStringCallsBuffer += PutStringCall(x, y, s)
    s.zipWithIndex.foreach { (c, i) =>
      val px = x + i
      if y >= 0 && y < height && px >= 0 && px < width then
        chars(y)(px) = c
        fgs(y)(px)   = currentFg
        bgs(y)(px)   = currentBg
    }

  def fillRect(x: Int, y: Int, w: Int, h: Int, char: Char): Unit =
    for dy <- 0 until h; dx <- 0 until w do
      val px = x + dx
      val py = y + dy
      if py >= 0 && py < height && px >= 0 && px < width then
        chars(py)(px) = char
        bgs(py)(px)   = currentBg

  case class StrokeRoundRectCall(x: Int, y: Int, w: Int, h: Int, arcPx: Int, color: Color, strokeWidth: Float)
  private val strokeRoundRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[StrokeRoundRectCall]
  case class BlurRegionCall(x: Int, y: Int, width: Int, height: Int, radius: Float)
  private val blurRegionCallsBuffer = scala.collection.mutable.ListBuffer.empty[BlurRegionCall]
  case class FillPixelRectCall(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color)
  private val fillPixelRectCallsBuffer = scala.collection.mutable.ListBuffer.empty[FillPixelRectCall]
  private val alphaCallsBuffer      = scala.collection.mutable.ListBuffer.empty[Float]

  override def strokeRoundRect(x: Int, y: Int, width: Int, height: Int, arcPx: Int, color: Color, strokeWidth: Float = 1.5f): Unit =
    strokeRoundRectCallsBuffer += StrokeRoundRectCall(x, y, width, height, arcPx, color, strokeWidth)

  def strokeRoundRectCalls: List[StrokeRoundRectCall] = strokeRoundRectCallsBuffer.toList

  override def setAlpha(alpha: Float): Unit =
    currentAlpha = alpha
    alphaCallsBuffer += alpha

  override def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit =
    blurRegionCallsBuffer += BlurRegionCall(x, y, width, height, radius)

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit =
    fillPixelRectCallsBuffer += FillPixelRectCall(xPx, yPx, widthPx, heightPx, color)

  def currentAlphaValue: Float          = currentAlpha
  def blurRegionCalls: List[BlurRegionCall] = blurRegionCallsBuffer.toList
  def fillPixelRectCalls: List[FillPixelRectCall] = fillPixelRectCallsBuffer.toList
  def alphaCalls: List[Float]           = alphaCallsBuffer.toList
  def putStringCalls: List[PutStringCall] = putStringCallsBuffer.toList

  def enableStyle(style: TextStyle): Unit  = ()
  def disableStyle(style: TextStyle): Unit = ()
  def hideCursor(): Unit                   = ()
  def viewportWidth: Int                   = width
  def viewportHeight: Int                  = height
  def flush(): Unit                        = ()

  def getChar(x: Int, y: Int): Char =
    if y >= 0 && y < height && x >= 0 && x < width then chars(y)(x) else ' '

  def getFg(x: Int, y: Int): Color =
    if y >= 0 && y < height && x >= 0 && x < width then fgs(y)(x) else Color.WHITE

  def getBg(x: Int, y: Int): Color =
    if y >= 0 && y < height && x >= 0 && x < width then
      val metrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      fillPixelRectCallsBuffer.findLast { call =>
        x * metrics.charWidth >= call.xPx && x * metrics.charWidth < call.xPx + call.widthPx &&
        y * metrics.lineHeight >= call.yPx && y * metrics.lineHeight < call.yPx + call.heightPx
      }.map(_.color).getOrElse(bgs(y)(x))
    else Color.BLACK

  def getRow(y: Int): String =
    if y >= 0 && y < height then chars(y).mkString else ""

  def clear(): Unit =
    putStringCallsBuffer.clear()
    strokeRoundRectCallsBuffer.clear()
    blurRegionCallsBuffer.clear()
    fillPixelRectCallsBuffer.clear()
    alphaCallsBuffer.clear()
    for y <- 0 until height; x <- 0 until width do
      chars(y)(x) = ' '
      fgs(y)(x)   = Color.WHITE
      bgs(y)(x)   = Color.BLACK
