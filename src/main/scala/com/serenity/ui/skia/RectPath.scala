package com.serenity.ui.skia

import org.jetbrains.skia.{Canvas, Paint, PathBuilder, PathDirection, Rect}

/** Rects drawn as one path: one call into Skia, not one for each. */
private[skia] object RectPath:

  /** Fills `rects`, (x, y, width, height) quads, with `paint`. A rect without size is one pixel, as a single fill of it
    * would be.
    */
  def fill(canvas: Canvas, rects: IArray[Int], paint: Paint): Unit =
    val builder = PathBuilder()
    try
      (0 until rects.length by 4).foreach { i =>
        val left = rects(i).toFloat
        val top  = rects(i + 1).toFloat
        val _ = builder.addRect(
          Rect.Companion.makeLTRB(left, top, left + rects(i + 2).max(1), top + rects(i + 3).max(1)),
          PathDirection.CLOCKWISE,
          0
        )
      }
      val path = builder.detach()
      try
        val _ = canvas.drawPath(path, paint)
      finally path.close()
    finally builder.close()
