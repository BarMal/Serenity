package com.serenity.ui.renderer

import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, CornerRounding, RoundedCorners}

/** [[PanelOutlineDrawing]] for [[Java2DRenderSurface]]: the square outline and clip come from the surface itself, the
  * rounded ones are drawn here on its graphics context.
  *
  * Java2D clips are not anti-aliased, so the rounded stroke (which is) is what hides the clip's jagged corner.
  */
private[renderer] final class Java2DRoundedOutlines(
  flat: PanelOutlineDrawing,
  g: Graphics2D,
  metrics: CellMetrics,
  paintColor: RenderColor => Color,
  touched: (Int, Int, Int, Int) => Unit,
  userClipDepth: AtomicInteger
) extends PanelOutlineDrawing:

  def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    flat.strokeRect(x, y, width, height, color, strokeWidth)

  def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    flat.withRectClip(x, y, width, height)(render)

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
      val px          = metrics.toPixelX(x)
      val py          = metrics.toPixelY(y)
      val inset       = math.ceil(strokeWidth / 2).toInt
      val savedStroke = g.getStroke
      val savedAlias  = g.getRenderingHint(RenderingHints.KEY_ANTIALIASING)
      touched(px, py, pw, ph)
      g.setColor(paintColor(color))
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setStroke(new BasicStroke(strokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER))
      g.draw(
        RoundedPath.of(
          (px + inset).toDouble,
          (py + inset).toDouble,
          (pw - 2 * inset).toDouble,
          (ph - 2 * inset).toDouble,
          (radius - inset).max(0).toDouble,
          corners
        )
      )
      g.setStroke(savedStroke)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, savedAlias)

  override def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, corners: RoundedCorners, radiusPx: Int)(
    render: => Unit
  ): Unit =
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val radius = CornerRounding.effectiveRadiusPx(radiusPx, pw, ph)
    if radius == 0 || !corners.any then withRectClip(x, y, width, height)(render)
    else
      val savedClip = g.getClip
      userClipDepth.incrementAndGet()
      try
        g.clip(
          RoundedPath.of(
            metrics.toPixelX(x).toDouble,
            metrics.toPixelY(y).toDouble,
            pw.toDouble,
            ph.toDouble,
            radius.toDouble,
            corners
          )
        )
        render
      finally
        userClipDepth.decrementAndGet()
        g.setClip(savedClip)
