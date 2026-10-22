package com.serenity.ui.terminal

import java.awt.Cursor

/** A GPU layer standing in for the canvas's own painting: the base frame lives in the layer, which has no
  * `BufferedImage` for the window to fill carets over, so the window hands the carets and the pointer cursor to it.
  */
trait LayerPresenter:

  def baseAvailable: Boolean

  def showCarets(carets: scala.List[SwingWindow.CaretPaint]): Unit

  def setCursor(cursor: Cursor): Unit

  def dispose(): Unit
