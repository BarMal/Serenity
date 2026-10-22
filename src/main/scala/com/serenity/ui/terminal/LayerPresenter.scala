package com.serenity.ui.terminal

import java.awt.Cursor

/** A GPU layer standing in for the canvas's own painting: the base frame lives in the layer, which has no
  * `BufferedImage` for the window to fill carets over, so the window hands the carets and the pointer cursor to it.
  */
trait LayerPresenter:

  def baseAvailable: Boolean

  def showCarets(carets: scala.List[SwingWindow.CaretPaint]): Unit

  def setCursor(cursor: Cursor): Unit

  /** `callback` is called, on the event thread, each time the layer has drawn a frame. */
  def onRendered(callback: () => Unit): Unit

  /** The render API the layer ended up with, which is a software one where the GPU's could not start. */
  def renderApi: String

  def dispose(): Unit
