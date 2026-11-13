package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, RoundedCorners}
import com.serenity.{FrameBackend, FramePixels, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: a rounded fill painted again over its own pixels leaves what one painting leaves. A frame that reuses
  * pixels repaints a header over the fringe its last frame put under the corners, and an anti-aliased fringe composited
  * twice is not what a frame drawn from scratch shows. The caller says what lies under the corners (the spacer column,
  * or the window), and the fill restores it before it paints.
  */
abstract class RoundedFillRepaintBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend
  protected def scale: Int

  private val window = new Color(250, 250, 250)
  private val margin = new Color(236, 232, 224)
  private val panel  = new Color(60, 90, 120)

  private def metrics = CellMetrics(charWidth = 7 * scale, lineHeight = 13 * scale, ascent = 10 * scale)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private def width   = 14 * metrics.charWidth
  private def height  = 10 * metrics.lineHeight
  private def radius  = 12 * scale

  private val underlay = CornerUnderlay.all(RenderColor.fromAwt(margin))

  private def painted(times: Int, withUnderlay: Boolean): FramePixels =
    val image    = frames.newFrame(width, height)
    val surface  = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = true, RenderCaches.create())
    val outlines = surface.panelOutlines.getOrElse(fail("expected panel outlines"))
    surface.clearViewport(RenderColor.fromAwt(window))
    surface.setBackgroundColor(RenderColor.fromAwt(margin))
    surface.fillRect(1, 1, 12, 8, ' ')
    (1 to times).foreach { _ =>
      val rounded = outlines.fillRoundRect(
        2,
        2,
        10,
        6,
        RoundedCorners.All,
        radius,
        RenderColor.fromAwt(panel),
        if withUnderlay then underlay else CornerUnderlay.absent
      )
      rounded shouldBe true
    }
    surface.flush()
    image

  private def differing(a: FramePixels, b: FramePixels): Seq[(Int, Int)] =
    for
      x <- 0 until width
      y <- 0 until height
      if (a.rgbAt(x, y) & 0xffffff) != (b.rgbAt(x, y) & 0xffffff)
    yield (x, y)

  "A rounded fill with its corners' underlay" should "leave the same pixels painted twice as painted once" in {
    val differences = differing(painted(times = 2, withUnderlay = true), painted(times = 1, withUnderlay = true))

    withClue(s"${differences.size} pixels differ, the first at ${differences.headOption}: ")(differences shouldBe empty)
  }

  it should "leave the corner's exterior as the underlay, as a painting over a cold frame does" in {
    val image = painted(times = 2, withUnderlay = true)

    val x = 2 * metrics.charWidth
    val y = 2 * metrics.lineHeight
    (image.rgbAt(x, y) & 0xffffff) shouldBe (margin.getRGB & 0xffffff)
  }

class SkiaRoundedFillRepaintScale1Spec extends RoundedFillRepaintBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 1

class SkiaRoundedFillRepaintScale2Spec extends RoundedFillRepaintBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
  override protected def scale: Int           = 2
