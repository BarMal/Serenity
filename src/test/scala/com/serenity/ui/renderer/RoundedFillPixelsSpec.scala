package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, RoundedCorners}
import com.serenity.{FramePixels, Java2DFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: a rounded fill's corners are the pixels a hard-edged path fill gives, however they are drawn. The
  * reference here fills each corner as a path on a plain image; the surface's own fill must match it pixel for pixel,
  * for every combination of rounded corners, at scale 1 and 2.
  */
class RoundedFillPixelsSpec extends AnyFlatSpec with Matchers:

  private val page = new Color(250, 250, 250)
  private val fill = new Color(30, 90, 160)
  private val font = new Font(Font.MONOSPACED, Font.PLAIN, 12)

  private val cellX     = 1
  private val cellY     = 1
  private val cellsWide = 9
  private val cellsHigh = 3

  private def metricsAt(scale: Int) = CellMetrics(charWidth = 7 * scale, lineHeight = 13 * scale, ascent = 10 * scale)

  private val everyCombination: List[RoundedCorners] =
    for
      topLeft     <- List(false, true)
      topRight    <- List(false, true)
      bottomRight <- List(false, true)
      bottomLeft  <- List(false, true)
    yield RoundedCorners(topLeft, topRight, bottomRight, bottomLeft)

  private def reference(
    metrics: CellMetrics,
    widthPx: Int,
    heightPx: Int,
    corners: RoundedCorners,
    radius: Int
  ): BufferedImage =
    val image = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val g     = image.createGraphics()
    g.setColor(page)
    g.fillRect(0, 0, widthPx, heightPx)
    g.setColor(fill)
    val px          = cellX * metrics.charWidth
    val py          = cellY * metrics.lineHeight
    val pw          = cellsWide * metrics.charWidth
    val ph          = cellsHigh * metrics.lineHeight
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
    if corners.topLeft then g.fill(RoundedPath.cornerFill(RoundedPath.Corner.TopLeft, px, py, radius))
    if corners.topRight then g.fill(RoundedPath.cornerFill(RoundedPath.Corner.TopRight, px + pw - radius, py, radius))
    if corners.bottomRight then
      g.fill(RoundedPath.cornerFill(RoundedPath.Corner.BottomRight, px + pw - radius, py + ph - radius, radius))
    if corners.bottomLeft then
      g.fill(RoundedPath.cornerFill(RoundedPath.Corner.BottomLeft, px, py + ph - radius, radius))
    g.dispose()
    image

  private def drawn(metrics: CellMetrics, widthPx: Int, heightPx: Int, corners: RoundedCorners, radius: Int) =
    val frame    = Java2DFrames.newFrame(widthPx, heightPx)
    val surface  = Java2DFrames.surfaceOver(frame, metrics, font, widthPx, heightPx, 1.0, false, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(page))
    outlines.fillRoundRect(cellX, cellY, cellsWide, cellsHigh, corners, radius, RenderColor.fromAwt(fill)) shouldBe true
    surface.flush()
    frame

  private def differences(actual: FramePixels, expected: BufferedImage): List[(Int, Int)] =
    for
      y <- (0 until expected.getHeight).toList
      x <- 0 until expected.getWidth
      if (actual.rgbAt(x, y) & 0xffffff) != (expected.getRGB(x, y) & 0xffffff)
    yield (x, y)

  List(1 -> List(6, 8, 12), 2 -> List(12, 16, 24)).foreach {
    case (scale, radii) =>
      radii.foreach { radius =>
        s"A rounded fill at scale $scale, radius $radius" should "match a hard-edged path fill for every corner combination" in {
          val metrics  = metricsAt(scale)
          val widthPx  = (cellX * 2 + cellsWide) * metrics.charWidth
          val heightPx = (cellY * 2 + cellsHigh) * metrics.lineHeight
          everyCombination.filter(_.any).foreach { corners =>
            val wrong = differences(
              drawn(metrics, widthPx, heightPx, corners, radius),
              reference(metrics, widthPx, heightPx, corners, radius)
            )
            withClue(s"$corners differs at ${wrong.take(8)} (${wrong.size} pixels): ")(wrong shouldBe empty)
          }
        }
      }
  }
