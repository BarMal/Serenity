package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A shadowed run paints a dimmer copy of its glyphs one logical pixel down and to the right, under the glyphs, and
  * inside the run's own box. The scenes and assertions are shared by every backend, and read colours and ink only.
  */
abstract class ShadowRunPaintBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width   = 210
  private val height  = 130
  private val black   = Color.BLACK.getRGB & 0xffffff
  private val ink     = new Color(200, 100, 50)

  private val shadowedBold = TextStyle(isBold = true, isShadowed = true)
  private val plainBold    = TextStyle(isBold = true)

  private def rendered(
    style: TextStyle,
    text: String = "MMMM",
    runWidthPx: Float = 56f,
    leftPx: Float = 14f,
    topPx: Int = 26
  ): FramePixels =
    val image   = frames.newFrame(width, height)
    val surface = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(ink))
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.enableStyle(style)
    surface.text.drawRunPx(leftPx, topPx, runWidthPx, metrics.lineHeight, metrics.ascent, text)
    surface.disableStyle(style)
    image

  private def rgb(image: FramePixels, x: Int, y: Int): Int = image.rgbAt(x, y) & 0xffffff

  private def inkedPixels(image: FramePixels): List[(Int, Int)] =
    (for y <- 0 until height; x <- 0 until width if rgb(image, x, y) != black yield (x, y)).toList

  private def red(pixel: Int): Int = (pixel >> 16) & 0xff

  "A shadowed run" should "extend its ink one pixel down and to the right and not up or to the left" in {
    val plain    = inkedPixels(rendered(plainBold))
    val shadowed = inkedPixels(rendered(shadowedBold))

    shadowed.map(_._1).min shouldBe plain.map(_._1).min
    shadowed.map(_._2).min shouldBe plain.map(_._2).min
    shadowed.map(_._1).max shouldBe (plain.map(_._1).max + 1)
    shadowed.map(_._2).max shouldBe (plain.map(_._2).max + 1)
  }

  it should "only ever add light where the plain run left the background" in {
    val plain    = rendered(plainBold)
    val shadowed = rendered(shadowedBold)
    val changed =
      for y <- 0 until height; x <- 0 until width if rgb(plain, x, y) != rgb(shadowed, x, y) yield (x, y)

    changed should not be empty
    all(changed.map((x, y) => red(rgb(shadowed, x, y)) - red(rgb(plain, x, y)))) should be >= 0
  }

  it should "paint the shadow at 40% of the foreground" in {
    val plain    = rendered(plainBold)
    val shadowed = rendered(shadowedBold)
    val shadowOnly =
      for y <- 0 until height; x <- 0 until width if rgb(plain, x, y) == black && rgb(shadowed, x, y) != black
      yield red(rgb(shadowed, x, y))
    val full = (ink.getRed * 0.4).round.toInt

    shadowOnly should not be empty
    shadowOnly.max should (be >= full - 12 and be <= full + 2)
  }

  it should "add nothing outside the run's own box, which is all a repaint of the run clears" in {
    val plain    = rendered(plainBold, text = "MMMMMMMM", runWidthPx = 56f)
    val shadowed = rendered(shadowedBold, text = "MMMMMMMM", runWidthPx = 56f)
    val outside =
      for y <- 0 until height; x <- 0 until width if !(x >= 14 && x < 70 && y >= 26 && y < 39) yield (x, y)

    all(outside.map((x, y) => rgb(shadowed, x, y) - rgb(plain, x, y))) shouldBe 0
  }

  it should "leave the runs after it unshadowed once the style is switched off" in {
    val image   = frames.newFrame(width, height)
    val surface = frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())
    surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(ink))
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))
    surface.enableStyle(shadowedBold)
    surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "MMMM")
    surface.disableStyle(shadowedBold)
    surface.enableStyle(plainBold)
    surface.text.drawRunPx(14f, 52, 56f, metrics.lineHeight, metrics.ascent, "MMMM")
    surface.disableStyle(plainBold)
    val alone = rendered(plainBold, topPx = 52)

    for y <- 52 until 65; x <- 0 until width do rgb(image, x, y) shouldBe rgb(alone, x, y)
  }
end ShadowRunPaintBehaviour

class Java2DShadowRunPaintSpec extends ShadowRunPaintBehaviour:
  override protected def frames: FrameBackend = Java2DFrames

class SkiaShadowRunPaintSpec extends ShadowRunPaintBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
