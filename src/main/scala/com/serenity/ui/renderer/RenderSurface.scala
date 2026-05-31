package com.serenity.ui.renderer

import java.awt.Color
import com.serenity.ui.theme.TextStyle

trait RenderSurface:
  def setForegroundColor(color: Color): Unit
  def setBackgroundColor(color: Color): Unit
  def getBackgroundColor: Color
  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit = ()
  def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit = ()
  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
