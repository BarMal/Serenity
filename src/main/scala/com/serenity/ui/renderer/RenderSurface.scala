package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.TextStyle

trait RenderSurface:
  def setForegroundColor(color: TextColor): Unit
  def setBackgroundColor(color: TextColor): Unit
  def getBackgroundColor: TextColor
  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit = ()
  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
