package com.serenity

import java.awt.Color
import com.serenity.ui.renderer.RenderSurface
import com.serenity.ui.theme.TextStyle

/** In-memory RenderSurface for renderer tests. Records putString calls so assertions can inspect
  * what was drawn at each (x, y) position.
  */
class MockRenderSurface(val width: Int, val height: Int) extends RenderSurface:
  private val chars = Array.fill(height, width)(' ')
  private val fgs   = Array.fill(height, width)(Color.WHITE)
  private val bgs   = Array.fill(height, width)(Color.BLACK)

  private var currentFg: Color = Color.WHITE
  private var currentBg: Color = Color.BLACK

  def setForegroundColor(color: Color): Unit = currentFg = color
  def setBackgroundColor(color: Color): Unit = currentBg = color
  def getBackgroundColor: Color              = currentBg

  def putString(x: Int, y: Int, s: String): Unit =
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
    if y >= 0 && y < height && x >= 0 && x < width then bgs(y)(x) else Color.BLACK

  def getRow(y: Int): String =
    if y >= 0 && y < height then chars(y).mkString else ""

  def clear(): Unit =
    for y <- 0 until height; x <- 0 until width do
      chars(y)(x) = ' '
      fgs(y)(x)   = Color.WHITE
      bgs(y)(x)   = Color.BLACK
