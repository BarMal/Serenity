package com.serenity.ui.skia

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.PanelOutlineDrawing
import org.jetbrains.skia.{Canvas, ClipMode, Rect}

/** Keeps drawing off the pixels a frame holds correct, so a redrawn row's antialiased ink cannot land on a kept row's
  * (#2481).
  */
private[skia] trait SkiaExcludedPixels extends PanelOutlineDrawing:

  protected def canvas: Canvas

  override def withoutPixels(kept: List[PixelRect])(render: => Unit): Unit =
    val saved = canvas.save()
    try
      kept.foreach { area =>
        val _ = canvas.clipRect(
          Rect.Companion.makeLTRB(
            area.xPx.toFloat,
            area.yPx.toFloat,
            (area.xPx + area.widthPx).toFloat,
            (area.yPx + area.heightPx).toFloat
          ),
          ClipMode.DIFFERENCE,
          false
        )
      }
      render
    finally
      val _ = canvas.restoreToCount(saved)
