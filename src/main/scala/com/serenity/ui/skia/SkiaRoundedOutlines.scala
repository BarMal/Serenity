package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, CornerRounding, RoundedCorners}
import com.serenity.ui.renderer.PanelOutlineDrawing
import org.jetbrains.skia.{BlendMode, Canvas, ClipMode, Paint, PaintMode, PaintStrokeCap, PaintStrokeJoin, RRect}

/** [[PanelOutlineDrawing]] for [[SkiaRenderSurface]]: the square outline and clip come from the surface itself, the
  * rounded ones are drawn here on its canvas.
  */
private[skia] final class SkiaRoundedOutlines(
  flat: PanelOutlineDrawing,
  canvas: Canvas,
  metrics: CellMetrics,
  argbOf: RenderColor => Int,
  recordPaint: (Double, Double, Double, Double, Double) => Unit,
  userClipDepth: AtomicInteger
) extends PanelOutlineDrawing:

  // The surface's other shapes are not anti-aliased; rounded outlines are, so they get a paint of their own.
  private val roundedStroke = Paint()
  roundedStroke.setAntiAlias(true)
  roundedStroke.setMode(PaintMode.STROKE)
  roundedStroke.setStrokeCap(PaintStrokeCap.SQUARE)
  roundedStroke.setStrokeJoin(PaintStrokeJoin.MITER)

  def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    flat.strokeRect(x, y, width, height, color, strokeWidth)

  def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    flat.withRectClip(x, y, width, height)(render)

  /** Skia's per-corner radii run top-left, top-right, bottom-right, bottom-left. */
  private def roundedRect(
    x: Float,
    y: Float,
    width: Float,
    height: Float,
    radius: Float,
    corners: RoundedCorners
  ): RRect =
    def r(rounded: Boolean): Float = if rounded then radius else 0f
    RRect.Companion.makeComplexXYWH(
      x,
      y,
      width,
      height,
      Array(r(corners.topLeft), r(corners.topRight), r(corners.bottomRight), r(corners.bottomLeft))
    )

  override def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    corners: RoundedCorners,
    radiusPx: Int,
    color: RenderColor,
    strokeWidth: Float
  ): Unit =
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val radius = CornerRounding.effectiveRadiusPx(radiusPx, pw, ph)
    if radius == 0 || !corners.any then strokeRect(x, y, width, height, color, strokeWidth)
    else
      val px    = metrics.toPixelX(x)
      val py    = metrics.toPixelY(y)
      val inset = math.ceil(strokeWidth / 2).toInt
      recordPaint(px.toDouble, py.toDouble, pw.toDouble, ph.toDouble, 2.0 * strokeWidth + 1.0)
      roundedStroke.setBlendMode(BlendMode.SRC_OVER)
      roundedStroke.setColor(argbOf(color))
      roundedStroke.setStrokeWidth(strokeWidth)
      val _ = canvas.drawRRect(
        roundedRect(
          (px + inset).toFloat,
          (py + inset).toFloat,
          (pw - 2 * inset).toFloat,
          (ph - 2 * inset).toFloat,
          (radius - inset).max(0).toFloat,
          corners
        ),
        roundedStroke
      )

  override def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, corners: RoundedCorners, radiusPx: Int)(
    render: => Unit
  ): Unit =
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val radius = CornerRounding.effectiveRadiusPx(radiusPx, pw, ph)
    if radius == 0 || !corners.any then withRectClip(x, y, width, height)(render)
    else
      val saved = canvas.save()
      userClipDepth.incrementAndGet()
      try
        val _ = canvas.clipRRect(
          roundedRect(
            metrics.toPixelX(x).toFloat,
            metrics.toPixelY(y).toFloat,
            pw.toFloat,
            ph.toFloat,
            radius.toFloat,
            corners
          ),
          ClipMode.INTERSECT,
          true
        )
        render
      finally
        userClipDepth.decrementAndGet()
        val _ = canvas.restoreToCount(saved)
