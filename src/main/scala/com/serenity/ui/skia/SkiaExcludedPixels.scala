package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.{PanelOutlineDrawing, PreservedPixels, SurfaceActivity}
import org.jetbrains.skia.{Canvas, Rect}

/** Keeps drawing off the pixels a frame holds correct, so a redrawn row's antialiased ink cannot land on a kept row's
  * (#2481). A clip on the canvas for the whole painting would send every antialiased edge it draws through Skia's
  * clip-mask path, which rounds its coverage differently from the unclipped one, so only the draws that can reach a
  * kept rect are clipped, as [[com.serenity.ui.renderer.Java2DKeptOff]] does.
  */
private[skia] trait SkiaExcludedPixels extends PanelOutlineDrawing:

  protected def canvas: Canvas

  def activity: SurfaceActivity

  /** Where the canvas has been moved to from the pixels the kept rects are in. */
  protected def translationPx: (Double, Double)

  private val keptRects = AtomicReference[List[PixelRect]](Nil)

  override def keepsOffPixels: Boolean = true

  override def withoutPixels(kept: List[PixelRect])(render: => Unit): Unit =
    val before = keptRects.getAndSet(kept)
    try render
    finally keptRects.set(before)

  /** Runs `draw`, whose ink stays within a line's height of the box `left, top, right, bottom` (in the canvas's own
    * coordinates, as [[SkiaRenderSurface]] records a run's reach), off the kept rects that ink can reach: once for each
    * rectangle of that reach the kept rects leave, as [[com.serenity.ui.renderer.Java2DKeptOff]] does. A rectangle clip
    * leaves an antialiased edge's coverage as the unclipped draw has it, which a clip that cuts a rect out does not. A
    * draw out of reach of all of them is not clipped at all.
    */
  private[skia] def awayFromKept(left: Float, top: Float, right: Float, bottom: Float)(draw: => Unit): Unit =
    val kept = keptRects.get
    if kept.isEmpty then draw
    else
      val (shiftX, shiftY) = translationPx
      val boxLeft          = math.floor(left + shiftX).toInt
      val boxTop           = math.floor(top + shiftY).toInt
      val boxRight         = math.ceil(right + shiftX).toInt
      val boxBottom        = math.ceil(bottom + shiftY).toInt
      val margin           = (boxBottom - boxTop).max(4)
      val near             = PreservedPixels.near(kept, boxLeft, boxTop, boxRight, boxBottom, margin)
      if near.isEmpty then draw
      else
        val reach =
          PixelRect(boxLeft - margin, boxTop - margin, boxRight - boxLeft + 2 * margin, boxBottom - boxTop + 2 * margin)
        val pieces = PixelRect.uncoveredWithin(reach, near)
        activity.clipSwept(pieces.size)
        pieces.foreach { piece =>
          val saved = canvas.save()
          try
            val _ = canvas.clipRect(
              Rect.Companion.makeLTRB(
                (piece.xPx - shiftX).toFloat,
                (piece.yPx - shiftY).toFloat,
                (piece.xPx + piece.widthPx - shiftX).toFloat,
                (piece.yPx + piece.heightPx - shiftY).toFloat
              ),
              false
            )
            draw
          finally
            val _ = canvas.restoreToCount(saved)
        }
