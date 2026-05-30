package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.TextStyle
import java.awt.Color

trait RenderSurface:
  def setForegroundColor(color: TextColor): Unit
  def setBackgroundColor(color: TextColor): Unit
  def getBackgroundColor: TextColor

  def setForegroundColor(color: Color): Unit =
    setForegroundColor(new TextColor.RGB(color.getRed, color.getGreen, color.getBlue))
  def setBackgroundColor(color: Color): Unit =
    setBackgroundColor(new TextColor.RGB(color.getRed, color.getGreen, color.getBlue))
  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit = ()
  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
