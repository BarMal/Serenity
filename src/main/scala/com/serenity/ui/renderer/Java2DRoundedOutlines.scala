package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, CornerRounding, PixelRect, RoundedCorners}

/** [[PanelOutlineDrawing]] for [[Java2DRenderSurface]]: the square outline and clip come from the surface itself, the
  * rounded ones are drawn here on its graphics context.
  *
  * Java2D clips are not anti-aliased, so the rounded stroke (which is) is what hides the clip's jagged corner.
  */
final private[renderer] class Java2DRoundedOutlines(
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

  override def withPixelClip(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int)(render: => Unit): Unit =
    flat.withPixelClip(xPx, yPx, widthPx, heightPx)(render)

  override def withoutPixels(kept: List[PixelRect])(render: => Unit): Unit = flat.withoutPixels(kept)(render)

  override def keepsOffPixels: Boolean = flat.keepsOffPixels

  override def fillRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    corners: RoundedCorners,
    radiusPx: Int,
    color: RenderColor
  ): Boolean =
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val radius = CornerRounding.effectiveRadiusPx(radiusPx, pw, ph)
    val rounds = radius > 0 && corners.any
    if rounds then
      val px = metrics.toPixelX(x)
      val py = metrics.toPixelY(y)
      touched(px, py, pw, ph)
      g.setColor(paintColor(color))
      val topInset    = if corners.topLeft || corners.topRight then radius else 0
      val bottomInset = if corners.bottomLeft || corners.bottomRight then radius else 0
      g.fillRect(px, py + topInset, pw, ph - topInset - bottomInset)
      g.fillRect(
        px + (if corners.topLeft then radius else 0),
        py,
        pw - (if corners.topLeft then radius else 0) - (if corners.topRight then radius else 0),
        topInset
      )
      g.fillRect(
        px + (if corners.bottomLeft then radius else 0),
        py + ph - bottomInset,
        pw - (if corners.bottomLeft then radius else 0) - (if corners.bottomRight then radius else 0),
        bottomInset
      )
      // Hard-edged, like the clip it replaces: a pixel is wholly the fill or left as it was, so drawing the same shape
      // again, or into a cached layer and compositing that, gives the same pixels. The border is anti-aliased over it.
      if corners.topLeft then fillCorner(RoundedPath.Corner.TopLeft, px, py, radius)
      if corners.topRight then fillCorner(RoundedPath.Corner.TopRight, px + pw - radius, py, radius)
      if corners.bottomRight then fillCorner(RoundedPath.Corner.BottomRight, px + pw - radius, py + ph - radius, radius)
      if corners.bottomLeft then fillCorner(RoundedPath.Corner.BottomLeft, px, py + ph - radius, radius)
    rounds

  /** Each corner's rows as runs of rectangles, found once per corner and radius by filling the corner's path the way it
    * used to be drawn. A few rectangles cost far less than rasterising a path, which every status bar and pane header
    * paid for on every frame.
    */
  private val cornerRuns = new ConcurrentHashMap[(RoundedPath.Corner, Int), Array[Int]]()

  private def fillCorner(corner: RoundedPath.Corner, x: Int, y: Int, radius: Int): Unit =
    if translatesOnly then
      val runs = cornerRuns.computeIfAbsent((corner, radius), key => Java2DRoundedOutlines.runsOf(key._1, key._2))
      Iterator.range(0, runs.length, 4).foreach { at =>
        g.fillRect(x + runs(at), y + runs(at + 1), runs(at + 2), runs(at + 3))
      }
    else g.fill(RoundedPath.cornerFill(corner, x, y, radius))

  /** Whether one logical pixel is one device pixel here, which is what the rectangles are worked out for: under a scale
    * the path fills finer than the rectangles can.
    */
  private def translatesOnly: Boolean =
    val transform = g.getTransform
    transform.getScaleX == 1.0 && transform.getScaleY == 1.0 && transform.getShearX == 0.0 &&
    transform.getShearY == 0.0

  private def withAntialiasing(draw: => Unit): Unit =
    val savedAlias   = g.getRenderingHint(RenderingHints.KEY_ANTIALIASING)
    val savedControl = g.getRenderingHint(RenderingHints.KEY_STROKE_CONTROL)
    // Normalised strokes are shifted by up to half a pixel, which would leave straight edges half covered.
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    try draw
    finally
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, savedAlias)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, savedControl)

  /** The straight edges are plain rectangles and only the rounded corners are anti-aliased arcs: a rounded path stroked
    * whole costs several times as much for the same pixels.
    */
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
    val thick  = math.ceil(strokeWidth).toInt
    if radius == 0 || !corners.any then strokeRect(x, y, width, height, color, strokeWidth)
    else
      val px = metrics.toPixelX(x)
      val py = metrics.toPixelY(y)
      touched(px, py, pw, ph)
      g.setColor(paintColor(color))
      val topEdge    = if corners.topLeft then radius else 0
      val topEnd     = if corners.topRight then radius else 0
      val bottomEdge = if corners.bottomLeft then radius else 0
      val bottomEnd  = if corners.bottomRight then radius else 0
      g.fillRect(px + topEdge, py, pw - topEdge - topEnd, thick)
      g.fillRect(px + bottomEdge, py + ph - thick, pw - bottomEdge - bottomEnd, thick)
      val leftStart  = if corners.topLeft then radius else thick
      val leftEnd    = if corners.bottomLeft then radius else thick
      val rightStart = if corners.topRight then radius else thick
      val rightEnd   = if corners.bottomRight then radius else thick
      g.fillRect(px, py + leftStart, thick, ph - leftStart - leftEnd)
      g.fillRect(px + pw - thick, py + rightStart, thick, ph - rightStart - rightEnd)
      val savedStroke = g.getStroke
      def arc(corner: RoundedPath.Corner, cornerX: Int, cornerY: Int): Unit =
        g.draw(RoundedPath.cornerArc(corner, cornerX, cornerY, radius, thick))
      withAntialiasing {
        g.setStroke(new BasicStroke(thick.toFloat, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
        if corners.topLeft then arc(RoundedPath.Corner.TopLeft, px, py)
        if corners.topRight then arc(RoundedPath.Corner.TopRight, px + pw - radius, py)
        if corners.bottomRight then arc(RoundedPath.Corner.BottomRight, px + pw - radius, py + ph - radius)
        if corners.bottomLeft then arc(RoundedPath.Corner.BottomLeft, px, py + ph - radius)
      }
      g.setStroke(savedStroke)

  override def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, corners: RoundedCorners, radiusPx: Int)(
    render: => Unit
  ): Unit =
    val pw     = width * metrics.charWidth
    val ph     = height * metrics.lineHeight
    val radius = CornerRounding.effectiveRadiusPx(radiusPx, pw, ph)
    if radius == 0 || !corners.any then render
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

private[renderer] object Java2DRoundedOutlines:

  /** The rectangles, as `x, y, width, height` four to a run, that give the pixels a hard-edged fill of
    * `RoundedPath.cornerFill(corner, 0, 0, radius)` gives. Rows with the same span are one rectangle.
    */
  def runsOf(corner: RoundedPath.Corner, radius: Int): Array[Int] =
    val image = new BufferedImage(radius, radius, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    g.setColor(Color.WHITE)
    g.fill(RoundedPath.cornerFill(corner, 0, 0, radius))
    g.dispose()
    def span(row: Int): Option[(Int, Int)] =
      val filled = (0 until radius).filter(column => (image.getRGB(column, row) >>> 24) != 0)
      filled.headOption.zip(filled.lastOption)
    val runs = (0 until radius).foldLeft(List.empty[Run]) { (done, row) =>
      span(row) match
        case None => done
        case Some((from, to)) =>
          done match
            case last :: earlier if last.continuedBy(row, from, to) => last.copy(height = last.height + 1) :: earlier
            case _                                                  => Run(from, row, to - from + 1, 1) :: done
    }
    runs.reverse.flatMap(run => List(run.x, run.y, run.width, run.height)).toArray

  final private case class Run(x: Int, y: Int, width: Int, height: Int):
    def continuedBy(row: Int, from: Int, to: Int): Boolean = x == from && width == to - from + 1 && y + height == row
