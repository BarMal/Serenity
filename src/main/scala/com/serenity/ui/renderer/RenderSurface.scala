package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.config.PostProcessingEffect
import com.serenity.ui.theme.TextStyle

trait RenderSurface:
  def setFont(font: Font): Unit                    = ()
  def fontRenderContext: Option[FontRenderContext] = None
  def setForegroundColor(color: Color): Unit
  def setBackgroundColor(color: Color): Unit
  def getBackgroundColor: Color

  def clearViewport(color: Color): Unit =
    setBackgroundColor(color)
    fillRect(0, 0, viewportWidth, viewportHeight, ' ')

  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit                                             = ()
  def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit = ()
  def applyPostProcessing(effect: PostProcessingEffect): Unit                  = ()
  def devicePixelScaleX: Double                                                = 1.0
  def devicePixelScaleY: Double                                                = 1.0

  def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit = ()

  /** Stroke a rounded rectangle at exact logical-pixel coordinates. */
  def strokeRoundRectAtPx(
    _xPx: Float,
    _yPx: Float,
    _widthPx: Float,
    _heightPx: Float,
    _arcPx: Int,
    _color: Color,
    _strokeWidth: Float = 1.5f
  ): Unit = ()

  /** Restrict drawing performed by `render` to a rounded rectangle in cell coordinates. */
  def withRoundRectClip(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit

  /** Restrict drawing performed by `render` to a rounded logical-pixel rectangle. */
  def withRoundRectClipAtPx(
    _xPx: Float,
    _yPx: Float,
    _widthPx: Float,
    _heightPx: Float,
    _arcPx: Int
  )(render: => Unit): Unit = render

  def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: Color
  ): Unit = ()

  /** Fill a rectangle at exact logical-pixel coordinates. */
  def fillPixelRectAtPx(
    xPx: Float,
    yPx: Float,
    widthPx: Float,
    heightPx: Float,
    color: Color
  ): Unit =
    fillPixelRect(
      math.floor(xPx).toInt,
      math.floor(yPx).toInt,
      math.ceil(widthPx).toInt,
      math.ceil(heightPx).toInt,
      color
    )

  /** Draw a proportional text run at exact pixel coordinates.
    *
    * Fills background [xPx, xPx + bgWidthPx) × [yPx, yPx + lineHeightPx) with the current background color, then draws
    * s at (xPx, yPx + ascent) with the current foreground color. Callers set fg/bg colors before calling.
    */
  def drawRunPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit = ()

  /** Draw a proportional text run at a fractional logical-pixel vertical position. */
  def drawRunAtPx(xPx: Float, yPx: Float, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    drawRunPx(xPx, math.round(yPx), bgWidthPx, lineHeightPx, ascentPx, s)

  def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit = ()

  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
