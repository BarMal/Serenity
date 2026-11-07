package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, RoundedCorners}
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: a panel drawn through the rounded clip and the rounded stroke, on every backend and at scale 1 and 2.
  * Scale 2 is the same layout with cells and radius twice the size. Assertions are pixel colours at points well inside
  * or outside the arc, never a tolerance between rasterisers.
  */
abstract class RoundedPanelDrawingBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend
  protected def scale: Int

  private val page   = new Color(250, 250, 250)
  private val panel  = new Color(200, 210, 220)
  private val border = new Color(20, 40, 80)

  private def metrics = CellMetrics(charWidth = 7 * scale, lineHeight = 13 * scale, ascent = 10 * scale)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private def width   = 14 * metrics.charWidth
  private def height  = 10 * metrics.lineHeight

  // The panel, in cells: x 2..12 and y 2..8.
  private val cellX      = 2
  private val cellY      = 2
  private val cellsWide  = 10
  private val cellsHigh  = 6
  private def left       = cellX * metrics.charWidth
  private def top        = cellY * metrics.lineHeight
  private def right      = left + cellsWide * metrics.charWidth - 1
  private def bottom     = top + cellsHigh * metrics.lineHeight - 1
  private def radius     = 12 * scale
  private def strokeSize = 2f * scale

  private def rgb(color: Color): Int = color.getRGB & 0xffffff

  private def drawPanel(corners: RoundedCorners, radiusPx: Int): FramePixels =
    val image    = frames.newFrame(width, height)
    val surface  = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(page))
    surface.setForegroundColor(RenderColor.fromAwt(border))
    surface.setBackgroundColor(RenderColor.fromAwt(panel))
    outlines.withRoundRectClip(cellX, cellY, cellsWide, cellsHigh, corners, radiusPx) {
      (cellY until cellY + cellsHigh).foreach(row => surface.putString(cellX, row, " " * cellsWide))
    }
    outlines.strokeRoundRect(
      cellX,
      cellY,
      cellsWide,
      cellsHigh,
      corners,
      radiusPx,
      RenderColor.fromAwt(border),
      strokeSize
    )
    image

  private def at(image: FramePixels, x: Int, y: Int): Int = image.rgbAt(x, y) & 0xffffff

  private val midX = () => (left + right) / 2
  private val midY = () => (top + bottom) / 2

  "A panel with every corner rounded" should "leave its four corner pixels as the page, and fill and stroke the rest" in {
    val image = drawPanel(RoundedCorners.All, radius)

    List((left, top), (right, top), (right, bottom), (left, bottom)).foreach {
      case (x, y) => withClue(s"corner ($x,$y): ")(at(image, x, y) shouldBe rgb(page))
    }
    val half = strokeSize.toInt / 2
    at(image, midX(), top + half) shouldBe rgb(border)
    at(image, midX(), bottom - half) shouldBe rgb(border)
    at(image, left + half, midY()) shouldBe rgb(border)
    at(image, right - half, midY()) shouldBe rgb(border)
    at(image, midX(), midY()) shouldBe rgb(panel)
  }

  it should "stroke the arc: a point on the diagonal a little inside the corner is the border colour, give or take edge blending" in {
    val image = drawPanel(RoundedCorners.All, radius)
    // The arc's centre is (radius, radius) in from the corner; the arc passes radius * (1 - cos 45) in along each axis
    // from the corner, about 0.29 of the radius, and the stroke is centred on a path half its width inside that.
    val along = (radius * 0.29).toInt + strokeSize.toInt / 2
    // At scale 1 the two-pixel stroke is narrower than a diagonal pixel is wide, so this pixel is nearly, not exactly,
    // the border; the page and the panel are far further away than the tolerance.
    val found = at(image, left + along, top + along)
    List(16, 8, 0).map(shift => ((found >> shift) & 0xff) - ((rgb(border) >> shift) & 0xff)).foreach { channel =>
      math.abs(channel) should be <= 12
    }
  }

  "A panel with no radius" should "keep square corners: the corner pixel is the border" in {
    val image = drawPanel(RoundedCorners.All, 0)

    List((left, top), (right, top), (right, bottom), (left, bottom)).foreach {
      case (x, y) => withClue(s"corner ($x,$y): ")(at(image, x, y) shouldBe rgb(border))
    }
    at(image, midX(), midY()) shouldBe rgb(panel)
  }

  "A panel with only its bottom-right corner rounded" should "round that corner and keep the others square" in {
    val image = drawPanel(
      RoundedCorners(topLeft = false, topRight = false, bottomRight = true, bottomLeft = false),
      radius
    )

    at(image, right, bottom) shouldBe rgb(page)
    List((left, top), (right, top), (left, bottom)).foreach {
      case (x, y) => withClue(s"corner ($x,$y): ")(at(image, x, y) shouldBe rgb(border))
    }
  }

  "A panel with only its top corners rounded" should "round those two, as a pane header does" in {
    val image = drawPanel(RoundedCorners.Top, radius)

    at(image, left, top) shouldBe rgb(page)
    at(image, right, top) shouldBe rgb(page)
    at(image, left, bottom) shouldBe rgb(border)
    at(image, right, bottom) shouldBe rgb(border)
  }

  private def fillPanel(corners: RoundedCorners, radiusPx: Int): (FramePixels, Boolean) =
    val image    = frames.newFrame(width, height)
    val surface  = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(page))
    val drew =
      outlines.fillRoundRect(cellX, cellY, cellsWide, cellsHigh, corners, radiusPx, RenderColor.fromAwt(panel))
    (image, drew)

  "A rounded fill" should "paint the interior and leave the four corner pixels as the page" in {
    val (image, drew) = fillPanel(RoundedCorners.All, radius)

    drew shouldBe true
    List((left, top), (right, top), (right, bottom), (left, bottom)).foreach {
      case (x, y) => withClue(s"corner ($x,$y): ")(at(image, x, y) shouldBe rgb(page))
    }
    at(image, midX(), midY()) shouldBe rgb(panel)
    at(image, midX(), top) shouldBe rgb(panel)
    at(image, left, midY()) shouldBe rgb(panel)
    at(image, right, bottom - radius) shouldBe rgb(panel)
    at(image, left + radius, bottom) shouldBe rgb(panel)
  }

  it should "round only the corners asked for" in {
    val (image, _) = fillPanel(RoundedCorners.Top, radius)

    at(image, left, top) shouldBe rgb(page)
    at(image, right, top) shouldBe rgb(page)
    at(image, left, bottom) shouldBe rgb(panel)
    at(image, right, bottom) shouldBe rgb(panel)
  }

  it should "draw nothing and say so when no corner is rounded or the radius is zero" in {
    val (squareImage, squareDrew) = fillPanel(RoundedCorners.None, radius)
    val (zeroImage, zeroDrew)     = fillPanel(RoundedCorners.All, 0)

    squareDrew shouldBe false
    zeroDrew shouldBe false
    at(squareImage, midX(), midY()) shouldBe rgb(page)
    at(zeroImage, midX(), midY()) shouldBe rgb(page)
  }

  it should "meet a stroke drawn over it with no gap: the pixel just inside the border is the panel" in {
    val image    = frames.newFrame(width, height)
    val surface  = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(page))
    outlines.fillRoundRect(cellX, cellY, cellsWide, cellsHigh, RoundedCorners.All, radius, RenderColor.fromAwt(panel))
    outlines.strokeRoundRect(
      cellX,
      cellY,
      cellsWide,
      cellsHigh,
      RoundedCorners.All,
      radius,
      RenderColor.fromAwt(border),
      strokeSize
    )
    val half = strokeSize.toInt / 2

    at(image, midX(), top + half) shouldBe rgb(border)
    at(image, midX(), top + strokeSize.toInt) shouldBe rgb(panel)
  }

  "The rounded clip" should "leave drawing after its block unclipped" in {
    val image    = frames.newFrame(width, height)
    val surface  = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(page))
    surface.setBackgroundColor(RenderColor.fromAwt(panel))
    outlines.withRoundRectClip(cellX, cellY, cellsWide, cellsHigh, RoundedCorners.All, radius)(())
    surface.putString(cellX, cellY, " " * cellsWide)

    at(image, left, top) shouldBe rgb(panel)
  }

class Java2DRoundedPanelScale1Spec extends RoundedPanelDrawingBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 1

class Java2DRoundedPanelScale2Spec extends RoundedPanelDrawingBehaviour:
  override protected def frames: FrameBackend = Java2DFrames
  override protected def scale: Int           = 2

class SkiaRoundedPanelScale1Spec extends RoundedPanelDrawingBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaRoundedPanelScale2Spec extends RoundedPanelDrawingBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
