package com.serenity.ui.renderer

import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.screen.Screen
import com.serenity.ui.theme.TextStyle

class LanternaRenderSurface(screen: Screen, graphics: TextGraphics) extends RenderSurface:
  def setForegroundColor(color: TextColor): Unit = graphics.setForegroundColor(color)
  def setBackgroundColor(color: TextColor): Unit = graphics.setBackgroundColor(color)
  def getBackgroundColor: TextColor              = graphics.getBackgroundColor

  def putString(x: Int, y: Int, s: String): Unit =
    graphics.putString(x, y, s)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    graphics.fillRectangle(new TerminalPosition(x, y), new TerminalSize(width, height), char)

  def enableStyle(style: TextStyle): Unit =
    val sgrs = toSGRs(style)
    if sgrs.nonEmpty then graphics.enableModifiers(sgrs*)

  def disableStyle(style: TextStyle): Unit =
    val sgrs = toSGRs(style)
    if sgrs.nonEmpty then graphics.disableModifiers(sgrs*)

  def hideCursor(): Unit  = screen.setCursorPosition(null)
  def viewportWidth: Int  = screen.getTerminalSize.getColumns
  def viewportHeight: Int = screen.getTerminalSize.getRows
  def flush(): Unit       = screen.refresh()

  private def toSGRs(style: TextStyle): Seq[SGR] =
    List(
      if style.isBold then Some(SGR.BOLD) else None,
      if style.isItalic then Some(SGR.ITALIC) else None,
      if style.isUnderlined then Some(SGR.UNDERLINE) else None
    ).flatten
