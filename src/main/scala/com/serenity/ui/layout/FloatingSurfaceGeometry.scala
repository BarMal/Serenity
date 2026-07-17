package com.serenity.ui.layout

/** Logical-pixel geometry shared by floating-surface drawing and targeting. Device scale is deliberately excluded. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < x + width && pixelY >= y && pixelY < y + height

case class FloatingSurfaceGeometry(frame: LogicalPixelRect, content: LogicalPixelRect, items: Vector[LogicalPixelRect]):

  def itemAt(pixelX: Double, pixelY: Double): Option[Int] =
    items.indexWhere(_.contains(pixelX, pixelY)) match
      case -1    => None
      case index => Some(index)

object FloatingSurfaceGeometry:

  def calculate(
    frame: LayoutRect,
    metrics: CellMetrics,
    borderCells: Int,
    itemCount: Int,
    itemGapRows: Double,
    itemOffsetRows: Double = 0.0
  ): FloatingSurfaceGeometry =
    val border = math.max(0, borderCells)
    val gap    = itemGapRows.max(0.0).min(8.0)
    val framePx = LogicalPixelRect(
      frame.x * metrics.charWidth.toDouble,
      frame.y * metrics.lineHeight.toDouble,
      frame.width * metrics.charWidth.toDouble,
      frame.height * metrics.lineHeight.toDouble
    )
    val content = LogicalPixelRect(
      framePx.x + border * metrics.charWidth,
      framePx.y + border * metrics.lineHeight,
      (framePx.width - border * metrics.charWidth * 2).max(0.0),
      (framePx.height - border * metrics.lineHeight * 2).max(0.0)
    )
    val itemHeight = metrics.lineHeight.toDouble
    val itemTop    = content.y + math.max(0.0, itemOffsetRows) * itemHeight
    val items = Vector.tabulate(math.max(0, itemCount)) { index =>
      LogicalPixelRect(content.x, itemTop + index * (itemHeight + gap * itemHeight), content.width, itemHeight)
    }
    FloatingSurfaceGeometry(framePx, content, items)

  /** Number of whole terminal rows required to contain a fractional logical-row gap. */
  def requiredCellRows(gapRows: Double): Int =
    math.ceil(gapRows.max(0.0)).toInt

  def interpolate(
    from: FloatingSurfaceGeometry,
    to: FloatingSurfaceGeometry,
    progress: Double
  ): FloatingSurfaceGeometry =
    val t = progress.max(0.0).min(1.0)
    def lerp(a: LogicalPixelRect, b: LogicalPixelRect) =
      LogicalPixelRect(
        a.x + (b.x - a.x) * t,
        a.y + (b.y - a.y) * t,
        a.width + (b.width - a.width) * t,
        a.height + (b.height - a.height) * t
      )
    FloatingSurfaceGeometry(
      lerp(from.frame, to.frame),
      lerp(from.content, to.content),
      from.items.zip(to.items).map(lerp)
    )
