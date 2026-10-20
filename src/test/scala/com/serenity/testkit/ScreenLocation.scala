package com.serenity.testkit

import java.awt.image.BufferedImage

/** Finds a window on a screen capture by what it painted, for specs that read the screen back. Where the window system
  * put a window is not always where `getLocationOnScreen` says: without a window manager (Xvfb in CI) the toolkit can
  * keep the position it asked for while the window sits at another, and a spec that trusts it compares the wrong part
  * of the screen (#2352).
  */
object ScreenLocation:

  /** The first pixel, reading rows from the top and each from the left, whose colour is `rgb` (alpha ignored). */
  def firstPixelOf(screen: BufferedImage, rgb: Int): Option[(Int, Int)] =
    val width  = screen.getWidth
    val pixels = screen.getRGB(0, 0, width, screen.getHeight, null, 0, width)
    Iterator
      .range(0, pixels.length)
      .find(i => (pixels(i) & 0xffffff) == (rgb & 0xffffff))
      .map(i => (i % width, i / width))
