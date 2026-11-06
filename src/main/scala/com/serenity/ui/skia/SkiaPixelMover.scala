package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.{BlendMode, Canvas, IRect, Paint, Surface}

/** Moves a retained surface's own pixels for [[SkiaRenderSurface.copyPixels]].
  *
  * In device pixels on an untransformed canvas, since the copy is of whole device pixels. Every source pixel is read as
  * it was before the copy. A clip in force (inside `withRectClip`) still bounds the copy, which Java2D's raw graphics
  * would ignore; the surface's pixels are moved in place only when no clip is, since only the canvas knows of one. A
  * source that reaches past the surface (a row's rect starts a pixel left of it where a glyph overhangs) copies only
  * the part inside, to where that part belongs: a snapshot is cut to the surface, so drawing it at the rect's own
  * corner would move it sideways.
  */
final private[skia] class SkiaPixelMover(
    canvas: Canvas,
    retainedSurface: Option[Surface],
    changes: Option[ChangeRecorder],
    phases: PresentPhases,
    deviceScaleX: Double,
    deviceScaleY: Double,
    userClipDepth: AtomicInteger
):

  private val fill = Paint()

  def copy(from: PixelRect, dyPx: Int): Unit =
    retainedSurface.foreach { surface =>
      val left   = math.round(from.xPx * deviceScaleX).toInt
      val top    = math.round(from.yPx * deviceScaleY).toInt
      val width  = math.round(from.widthPx * deviceScaleX).toInt
      val height = math.round(from.heightPx * deviceScaleY).toInt
      val moved  = math.round(dyPx * deviceScaleY).toInt
      changes.foreach(_.moved(DeviceArea(left, top, width, height), moved))
      if userClipDepth.get() == 0 then
        phases.timed(PresentPhase.SurfaceMove)(
          PixelMove.inPlace(surface, DeviceArea(left, top, width, height), moved)
        )
      else
        val insideLeft   = left.max(0)
        val insideTop    = top.max(0)
        val insideRight  = (left + width).min(surface.getWidth)
        val insideBottom = (top + height).min(surface.getHeight)
        if insideRight > insideLeft && insideBottom > insideTop then
          val snap =
            surface.makeImageSnapshot(IRect.Companion.makeLTRB(insideLeft, insideTop, insideRight, insideBottom))
          val saved = canvas.save()
          try
            val _ = canvas.resetMatrix()
            fill.setBlendMode(BlendMode.SRC)
            fill.setColor(OpaqueBlack)
            val _ = canvas.drawImage(snap, insideLeft.toFloat, (insideTop + moved).toFloat, fill)
            fill.setBlendMode(BlendMode.SRC_OVER)
          finally
            val _ = canvas.restoreToCount(saved)
            snap.close()
    }

  private val OpaqueBlack = 0xff000000
