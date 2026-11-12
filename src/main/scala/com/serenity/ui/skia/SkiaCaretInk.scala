package com.serenity.ui.skia

import com.serenity.ui.renderer.{CaretGeometry, CaretPaint}
import org.jetbrains.skia.{Canvas, Paint, Path, PathFillMode, Point, Rect}

/** Draws carets over a layer: a plain caret as the rect it always was, a styled one as the shapes of [[CaretGeometry]]
  * with their edges smoothed, so a slant is not stair-stepped.
  */
private[serenity] object SkiaCaretInk:

  def draw(canvas: Canvas, carets: List[CaretPaint], scale: Float): Unit =
    val ink = new Paint()
    try
      carets.foreach { caret =>
        ink.setColor(caret.color.getRGB)
        caret.body match
          case Some(body) if !caret.style.isPlain =>
            ink.setAntiAlias(true)
            CaretGeometry.polygons(body, caret.style).foreach(polygon => drawPolygon(canvas, polygon, scale, ink))
            ink.setAntiAlias(false)
          case _ =>
            canvas.drawRect(
              Rect.Companion.makeXYWH(
                caret.rect.x * scale,
                caret.rect.y * scale,
                caret.rect.width * scale,
                caret.rect.height * scale
              ),
              ink
            )
      }
    finally ink.close()

  private def drawPolygon(canvas: Canvas, polygon: CaretGeometry.Polygon, scale: Float, ink: Paint): Unit =
    val corners = polygon.points.map((x, y) => new Point((x * scale).toFloat, (y * scale).toFloat)).toArray
    val path    = Path.Companion.Polygon(corners, true, PathFillMode.WINDING, false)
    try
      val _ = canvas.drawPath(path, ink)
    finally path.close()
