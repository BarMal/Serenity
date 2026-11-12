package com.serenity.ui.skia

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.TextShadow
import org.jetbrains.skia.{Canvas, Rect}

/** The shadow pass of a shadowed run on a Skia canvas: `draw` runs with the foreground as [[TextShadow]] has it, moved
  * by its offset and clipped to the run's own box, which is all a repaint of the run clears.
  */
private[skia] object SkiaTextShadow:

  def pass(canvas: Canvas, foreground: AtomicReference[Color], deviceScale: Double)(
    left: Int,
    top: Int,
    width: Int,
    height: Int
  )(draw: => Unit): Unit =
    val ink   = foreground.getAndUpdate(TextShadow.colorOf(_))
    val saved = canvas.save()
    try
      val offset = TextShadow.offsetOf(deviceScale).toFloat
      val _ = canvas.clipRect(Rect.Companion.makeXYWH(left.toFloat, top.toFloat, width.toFloat, height.toFloat), false)
      val _ = canvas.translate(offset, offset)
      draw
    finally
      val _ = canvas.restoreToCount(saved)
      foreground.set(ink)
