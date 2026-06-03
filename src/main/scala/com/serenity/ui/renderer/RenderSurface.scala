package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.ui.theme.TextStyle

trait RenderSurface:
  def setFont(font: Font): Unit                = ()
  def setForegroundColor(color: Color): Unit
  def setBackgroundColor(color: Color): Unit
  def getBackgroundColor: Color
  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit                                             = ()
  def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit = ()

  def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit = ()

  def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: Color
  ): Unit = ()

  /** Draw a proportional text run at exact pixel coordinates.
    *
    * Fills background [xPx, xPx + bgWidthPx) × [yPx, yPx + lineHeightPx) with the current
    * background color, then draws s at (xPx, yPx + ascent) with the current foreground color.
    * Callers set fg/bg colors before calling.
    */
  def drawRunPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, s: String): Unit = ()

  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
