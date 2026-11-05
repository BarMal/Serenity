package com.serenity.ui.benchmark

/** A window's size in pixels. */
final case class WindowPx(width: Int, height: Int):

  /** Three quarters of each side: a visible change in both directions, far from any minimum the window enforces. */
  def smaller: WindowPx = WindowPx(width * 3 / 4, height * 3 / 4)
